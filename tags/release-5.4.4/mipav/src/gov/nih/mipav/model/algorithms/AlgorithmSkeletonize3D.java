package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;


/**
 * This is a port of the C++ code for pfSkel: Potential Field Based 3D Skeleton Extraction written by Nicu D. Cornea,
 * Electrical and Computer Engineering Department, Rutgers, The State University of New Jersey, Piscataway, New Jersey,
 * 08854, cornea@caip.rutgers.edu. The code was downloaded from http://www.caip.rutgers.edu/~cornea/Skeletonization/
 * References: 1.) Jen-Hui Chuang, Chi-Hao Tsai, Min-Chi Ko, Skeletonization of Three-Dimensional Object Using
 * Generalized Potential Field, IEEE Transactions on Pattern Analysis and Machine Intelligence, Vol. 22., No. 11,
 * November, 2000, pp. 1241-1251. 2.) Nicu D. Cornea, Deborah Silver, and Patrick Min, Curve-Skeleton Applications,
 * Proceedings IEEE Visualization, 2005, pp. 95-102. 3.) Nicu D. Cornea, Deborah Silver, Xiaosong Yuan, and Raman
 * Balasubramanian, Computing Hierarchical Curve-Skeletons of 3D Objects, Springer-Verlag, The Visual Computer, Vol. 21,
 * No. 11, October, 2005, pp. 945-955. To run this program on an image there must be a sufficient number of planes
 * composed solely of background pixels at the end x, y, and z boundaries of the image. For the default value of 0 for
 * the distance of the electrical charges from the object boundary, there must be background planes at x = 0, x = xDim -
 * 1, y = 0, y = yDim - 1, z = 0, and z = zDim - 1. Use the utilities Add image margins or Insert slice to create a
 * version of the image with the needed number of padding pixels. 98% of the image calculation time will be in the
 * potential field calculation. So the first time this program is run on an image select the save the vector fields
 * radio button to save the x, y, and z vector fields and the level 1 skeleton. On the following runs select the load
 * the vector field from files radio button to load the x, y, and z vector fields and the level 1 skeleton. On following
 * runs vary the entry fraction of divergence points to use to change the extensiveness of the skeleton generated. As
 * more divergence points are used in the level 2 skeleton calculation, the skeleton will become more extensive. The
 * program must be run on 1 solid object. For that new features have been added to the original program. A user selected
 * threshold will separate background from object voxels. A default selected checkbox for slice by slice hole filling
 * will be used to convert the image into 1 solid object. After IDing all the objects in 3D, all but the largest object
 * will be deleted.
 */
public class AlgorithmSkeletonize3D extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final byte OBJECT_VAL = 1;

    /** 3 values defining outside pixels. */
    private static final byte OUTSIDE_1 = 2;

    /** DOCUMENT ME! */
    private static final byte OUTSIDE_2 = 3;

    /** DOCUMENT ME! */
    private static final byte OUTSIDE_3 = 4;

    /** interior voxel. */
    private static final byte INTERIOR = 70;

    /** background (exterior to the object) voxel. */
    private static final byte EXTERIOR = 0;

    /** surface voxel. */
    private static final byte SURF = 50;

    /** Boundary voxel - participates in potential field calculation. */
    private static final byte BOUNDARY = 60;

    /** Added voxels in order to thicken the object. */
    private static final byte PADDING_MIN = 80;

    /** DOCUMENT ME! */
    private static final int BOUND_SIZE = 1200000;

    /**
     * If a boundary point is at a distance greater than the PF_THRESHOLD, then it is ignored, i.e., it does not
     * influence the field at this point. Setting this to a very low value is not a good idea: Imagine the example of a
     * very long cylinder. Setting this threshold smaller than half the length of the cylinder will cause the field not
     * to flow towards the one attracting point in the middle of the cylinder. Instead it will only go towards the
     * center, creating a critical point at each slice along the cylinder.
     */
    private static final short PF_THRESHOLD = 100;

    /** DOCUMENT ME! */
    private static final int MAX_NUM_CRITPTS = 2000;

    /** DOCUMENT ME! */
    private static final double EPSILON = 1.0E-6;

    /** DOCUMENT ME! */
    private static final int NR_OF_SIGN_CHANGES = 3;

    /** DOCUMENT ME! */
    private static final double CELL_SUBDIVISION_FACTOR = 1048576.0;

    /** DOCUMENT ME! */
    private static final byte CPT_SADDLE = 1;

    /** DOCUMENT ME! */
    private static final byte CPT_ATTRACTING_NODE = 2;

    /** DOCUMENT ME! */
    private static final byte CPT_REPELLING_NODE = 3;

    /** DOCUMENT ME! */
    private static final byte CPT_UNKNOWN = 4;

    /** Exit status in followStreamLines. */
    private static final int FSL_EXS_ERROR = 0;

    /** DOCUMENT ME! */
    private static final int FSL_EXS_TOO_MANY_STEPS = 1;

    /** DOCUMENT ME! */
    private static final int FSL_EXS_CLOSE_TO_SKEL = 2;

    /** DOCUMENT ME! */
    private static final int FSL_EXS_CLOSE_TO_CP = 3;

    /** DOCUMENT ME! */
    private static final int FSL_EXS_STUCK = 4;

    /** DOCUMENT ME! */
    private static final int FSL_EXS_OUTSIDE = 5;

    /** left end point of the segment. */
    private static final int SKEL_SEG_LEFT = 0;

    /** right end point of the segment. */
    private static final int SKEL_SEG_RIGHT = 1;

    /** first point of the segment excluding the left end point. */
    private static final int SKEL_SEG_FIRST = 2;

    /** last point of the segment excluding the right end point. */
    private static final int SKEL_SEG_LAST = 3;

    /**
     * At each ... th step, we check whether we made some progress in the last ... steps (some more points were added to
     * the skeleton). If yes, we go on for another ... steps. If not, we stop.
     */
    private static final int PROGRESS_CHECK_INTERVAL = 1000;

    /**
     * Once we make MAX_NUM_STEP_INTERVALS * PROGRESS_STEP_INTERVAL, we should stop following the vector field. We are
     * probably stuck near an attracting node.
     */
    private static final int MAX_NUM_STEP_INTERVALS = 50;

    /**
     * Defines the maximum number of skeleton points that can separate an intersection with a skeleton segment from the
     * segment's end points so that the intersection is not reported. When constructing a skeleton segment, at each step
     * we test for intersection with other existing segments. If we are close to another skeleton segment, we also check
     * if we are also close to one of that segment's end points. If we are, we keep going even if the points in the 2
     * segments overlap, in the hope that we can end the current segment at the same point as the segment we are
     * intersecting, thus reducing the number of joints. Of course this does not work if we are close to an end point
     * but we are actually moving towards the other end of the segment, but in that case, the current segment will be
     * terminated once we get far enough from the end point.
     */
    private static final int SP_CLOSE_TO_SEGMENT_ENDPOINT = 5;

    /** Defines "close to ..." in terms of Manhattan distance |x1-x2| + |y1-y2| + |z1-z2|. */
    private static final double SP_SMALL_DISTANCE = 0.5;

    /**
     * This is the size of the step used to advance to the next position when following the vector field. 0.2 seems to
     * be the best value
     */
    private static final double STEP_SIZE = 0.2;

    /**
     * Minimum segment length (number of points) Only segments containing at least that many points will be included in
     * the skeleton. However, if the segment originated in a critical point, we should keep it regardless of the length.
     * Removing it, might disconnect the basic skeleton.
     */
    private static final int MIN_SEG_LENGTH = 5;

    /** DOCUMENT ME! */
    private static final int MAX_NUM_HDPTS = 5000;

    /** DOCUMENT ME! */
    private static final int SEARCH_GRID = 1;

    /** DOCUMENT ME! */
    private static final double CELL_SIZE = 1.00 / SEARCH_GRID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** coordinates of critical points. */
    private double[][] critPts = null;

    /**
     * distCharges and fieldStrength are unused if field vectors and the level 1 skeleton are read in from files
     * distance of electric charges from object boundary -40 to 40.
     */
    private int distCharges = 0;

    /** 3 eigenvectors of the Jacobian matrix evaluateed at the critical point (3 double values for each vector). */
    private double[][][] eigenvectors = null;

    /** electric field strenth (4-9). */
    private int fieldStrength = 5;

    /** x component of vector field. */
    private double[] forceX = null;

    /** y component of vector field. */
    private double[] forceY = null;

    /** z component of vector field. */
    private double[] forceZ = null;

    /** DOCUMENT ME! */
    private double[][] hdPoints = null;

    /** Minimum value for outer pixel to be an object pixel. */
    private float minObjectValue;

    /** number of critical points. */
    private int numCritPoints = 0;

    /** DOCUMENT ME! */
    private int numHDPoints = 0;

    /** If true, output all skeleton points If false, output only end points of segments. */
    private boolean outputPoints = true;

    /** fraction of the highest negative divergence points to use. */
    private float perHDPoints = 0.3f;

    /** type of critical point (1 - saddle point, 2 - attracting node, 3 - repelling point, 4 - unknown). */
    private byte[] pointType = null;

    /** real part of 3 eigenvalues of Jacobian matrix evaluated at the critical point. */
    private double[][] realEigenvalues = null;

    /** If true save the x, y, and z vector components and the level 1 skeleton. */
    private boolean saveVF = true;

    /** DOCUMENT ME! */
    private int skelNumPoints = 0;

    /** DOCUMENT ME! */
    private int skelNumSegments = 0;

    /** DOCUMENT ME! */
    private double[][] skelPoints = null;

    /** DOCUMENT ME! */
    private int[][] skelSegments = null;

    /** DOCUMENT ME! */
    private int skelSizePoints = 0;

    /** DOCUMENT ME! */
    private int skelSizeSegments = 0;

    /** skeleton points written to or read from file. */
    private double[][] skPoints = null;

    /** skeleton segments written to or read from file. */
    private int[][] skSegments = null;

    /** If true, perform slice by slice hole filling on the thresholded image. */
    private boolean sliceHoleFilling = true;

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private int totLength;

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
     * @param  saveVF            if true save xvf, yvf, zvf, and skf files
     * @param  distCharges       distance of electrical charges from object boundary
     * @param  fieldStrength     field strength (4-9)
     * @param  perHDPoints       fraction of divergence points to use (0.0 - 1.0)
     * @param  forceX            x component of vector field
     * @param  forceY            y component of vector field
     * @param  forceZ            z component of vector field
     * @param  skPoints          skeleton points read from a file
     * @param  skSegments        skeleton segments read from a file
     * @param  outputPoints      If true, output all skeleton points If false, output only end points of segments
     */
    public AlgorithmSkeletonize3D(ModelImage srcImg, float minObjectValue, boolean sliceHoleFilling, boolean saveVF,
                                  int distCharges, int fieldStrength, float perHDPoints, double[] forceX,
                                  double[] forceY, double[] forceZ, double[][] skPoints, int[][] skSegments,
                                  boolean outputPoints) {
        super(null, srcImg);
        this.minObjectValue = minObjectValue;
        this.sliceHoleFilling = sliceHoleFilling;
        this.saveVF = saveVF;
        this.distCharges = distCharges;
        this.fieldStrength = fieldStrength;
        this.perHDPoints = perHDPoints;
        this.forceX = forceX;
        this.forceY = forceY;
        this.forceZ = forceZ;
        this.skPoints = skPoints;
        this.skSegments = skSegments;
        this.outputPoints = outputPoints;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {
        int i, j;
        ModelImage gvfImage;
        boolean[] used;
        float[] xArr = new float[1];
        float[] yArr = new float[1];
        float[] zArr = new float[1];
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
        int z;
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

        /*int test = 1;
         * if (test == 1) { createCylinder(); setCompleted(true); return;}*/

        

        fireProgressStateChanged(srcImage.getImageName(), "Performing 3D skeletonization...");


        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        totLength = sliceSize * zDim;

        File file;
        RandomAccessFile raFile;
        volFloat = new float[totLength];

        try {
            srcImage.exportData(0, totLength, volFloat);
        } catch (IOException e) {
            MipavUtil.displayError("IO error on srcImage.exportData");

            setCompleted(false);

            return;
        }

        if (skPoints == null) {

            if (!checkVolumePadding()) {
                MipavUtil.displayError("Error! The object is not sufficiently padded");

                setCompleted(false);

                return;
            }
        } // if (skPoints == null)

        fireProgressStateChanged("Making volume solid");
        Preferences.debug("Making volume solid\n", Preferences.DEBUG_ALGORITHM);
        makeSolidVolume();
        /*boolean test;
         * test = false; if (test) { solidImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(),
         * "solidImage", srcImage.getUserInterface()); try {     solidImage.importData(0, vol, true); } catch
         * (IOException e) {     MipavUtil.displayError("IOException on soldImage.importData");     setCompleted(false);
         *    return; }
         *
         * new ViewJFrameImage(solidImage);  setCompleted(true); return;} // if (test)*/

        if (sliceHoleFilling) {
            fireProgressStateChanged("Slice by slice hole filling");
            Preferences.debug("Slice by slice hole filling\n", Preferences.DEBUG_ALGORITHM);
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
        Preferences.debug("Delete all but the largest object\n", Preferences.DEBUG_ALGORITHM);
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
         * "solidImage", srcImage.getUserInterface()); try {     solidImage.importData(0, vol, true); } catch
         * (IOException e) {     MipavUtil.displayError("IOException on soldImage.importData");     setCompleted(false);
         *    return; }
         *
         * new ViewJFrameImage(solidImage);  setCompleted(true); return;} // if (test)*/

        // Reset the INTERIOR voxels with at least one 6 neighbor EXTERIOR
        // to be SURF voxels
        flagVolume();

        if (skPoints == null) {

            try {
                forceX = new double[totLength];
                forceY = new double[totLength];
                forceZ = new double[totLength];
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory error when allocating force");

                setCompleted(false);

                return;
            }

            // Thicken the object with distCharges layers of extra voxels and
            // compute the potential field
            if (distCharges > 0) {
                fireProgressStateChanged("Padding the object");
                Preferences.debug("Padding the object\n", Preferences.DEBUG_ALGORITHM);

                // First layer attaches itself to the SURF voxels
                if (!expandVolume(SURF, PADDING_MIN)) {
                    MipavUtil.displayError("Error padding the object");
                    MipavUtil.displayError("Volume bounds are too tight");

                    setCompleted(false);

                    return;
                }

                // All other layers attach themselves to the previous one.
                for (i = 1; i < distCharges; i++) {

                    if (!expandVolume((byte) (PADDING_MIN + (i - 1)), (byte) (PADDING_MIN + i))) {
                        MipavUtil.displayError("Error padding the object");
                        MipavUtil.displayError("Volume bounds are too tight");

                        setCompleted(false);

                        return;
                    }
                }
            } // if (distCharges > 0)
            else if (distCharges < 0) {
                fireProgressStateChanged("Peeling the object");
                Preferences.debug("Peeling the object\n", Preferences.DEBUG_ALGORITHM);

                for (i = 0; i > distCharges; i--) {
                    peelVolume();
                }
            } // else if (distCharges <0)

            fireProgressStateChanged("Calculating potential field");
            Preferences.debug("Calculating potential field\n", Preferences.DEBUG_ALGORITHM);

            // Check volume padding - fast version
            if (!quickCheckVolumePadding()) {
                MipavUtil.displayError("Error - object touches bounding box");

                setCompleted(false);

                return;
            }

            if (!calculatePotentialField()) {

                setCompleted(false);

                return;
            }

            if (saveVF) {
                fireProgressStateChanged("Saving potential field");
                Preferences.debug("Saving potential field\n", Preferences.DEBUG_ALGORITHM);
                gvfImage = new ModelImage(ModelImage.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_xvf");

                try {
                    gvfImage.importData(0, forceX, true);
                } catch (IOException error) {

                    if (gvfImage != null) {
                        gvfImage.disposeLocal();
                        gvfImage = null;
                    }

                    MipavUtil.displayError("Error on gvfImage.importData");

                    setCompleted(false);

                    return;
                }

                try {
                    gvfImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), srcImage.getImageName() + "_xvf",
                                       FileUtility.XML, true);
                } catch (OutOfMemoryError error) {

                    if (gvfImage != null) {
                        gvfImage.disposeLocal();
                        gvfImage = null;
                    }

                    MipavUtil.displayError("Error on gvfImage.saveImage");

                    setCompleted(false);

                    return;
                }

                try {
                    gvfImage.importData(0, forceY, true);
                } catch (IOException error) {

                    if (gvfImage != null) {
                        gvfImage.disposeLocal();
                        gvfImage = null;
                    }

                    MipavUtil.displayError("Error on gvfImage.importData");

                    setCompleted(false);

                    return;
                }

                try {
                    gvfImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), srcImage.getImageName() + "_yvf",
                                       FileUtility.XML, true);
                } catch (OutOfMemoryError error) {

                    if (gvfImage != null) {
                        gvfImage.disposeLocal();
                        gvfImage = null;
                    }

                    MipavUtil.displayError("Error on gvfImage.saveImage");

                    setCompleted(false);

                    return;
                }

                try {
                    gvfImage.importData(0, forceZ, true);
                } catch (IOException error) {

                    if (gvfImage != null) {
                        gvfImage.disposeLocal();
                        gvfImage = null;
                    }

                    MipavUtil.displayError("Error on gvfImage.importData");

                    setCompleted(false);

                    return;
                }

                try {
                    gvfImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), srcImage.getImageName() + "_zvf",
                                       FileUtility.XML, true);
                } catch (OutOfMemoryError error) {

                    if (gvfImage != null) {
                        gvfImage.disposeLocal();
                        gvfImage = null;
                    }

                    MipavUtil.displayError("Error on gvfImage.saveImage");

                    setCompleted(false);

                    return;
                }

                gvfImage.disposeLocal();
                gvfImage = null;
            } // if (saveVF)

            fireProgressStateChanged("Detecting critical points\n");
            fireProgressStateChanged(92);
            Preferences.debug("Detecting critical points\n", Preferences.DEBUG_ALGORITHM);

            if (!getCriticalPoints()) {

                setCompleted(false);

                return;
            }

            // Generating the skeleton
            fireProgressStateChanged("Generating level 1 skeleton");
            fireProgressStateChanged(94);
            Preferences.debug("Generating level1 skeleton\n", Preferences.DEBUG_ALGORITHM);

            // Allocate skeleton structure
            try {
                skelSizePoints = 100000;
                skelPoints = new double[skelSizePoints][3];
                skelSizeSegments = 10000;
                skelSegments = new int[skelSizeSegments][];
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Error allocating skeleton structure");

                setCompleted(false);

                return;
            }

            if (!getLevel1Skeleton()) {

                setCompleted(false);

                return;
            }

            if (saveVF) {
                fireProgressStateChanged("Saving level 1 skeleton\n");
                Preferences.debug("Saving level 1 skeleton\n", Preferences.DEBUG_ALGORITHM);
                file = new File(srcImage.getFileInfo(0).getFileDirectory() + srcImage.getImageName() + ".skf");

                try {
                    raFile = new RandomAccessFile(file, "rw");
                    raFile.setLength(0);
                    raFile.writeInt(skelNumPoints);

                    for (i = 0; i < skelNumPoints; i++) {

                        for (j = 0; j < 3; j++) {
                            raFile.writeDouble(skelPoints[i][j]);
                        }
                    } // for (i = 0; i < skelNumPoints; i++)

                    raFile.writeInt(skelNumSegments);

                    for (i = 0; i < skelNumSegments; i++) {

                        for (j = 0; j < 4; j++) {
                            raFile.writeInt(skelSegments[i][j]);
                        }
                    } // for (i = 0; i < skelNumSegments; i++)

                    raFile.close();
                } catch (IOException e) {
                    MipavUtil.displayError("IOEXception on new RandomAccessFile");

                    setCompleted(false);

                    return;
                }
            } // if (saveVF)
        } // if (skPoints == null)
        else { // skPoints != null

            // Allocate skeleton structure
            try {
                skelSizePoints = 100000;
                skelPoints = new double[skelSizePoints][3];
                skelSizeSegments = 10000;
                skelSegments = new int[skelSizeSegments][];
                skelNumPoints = skPoints.length;

                for (i = 0; i < skelNumPoints; i++) {

                    for (j = 0; j < 3; j++) {
                        skelPoints[i][j] = skPoints[i][j];
                    }
                }

                for (i = 0; i < skelNumPoints; i++) {
                    skPoints[i] = null;
                }

                skPoints = null;
                skelNumSegments = skSegments.length;

                for (i = 0; i < skelNumSegments; i++) {
                    skelSegments[i] = new int[4];

                    for (j = 0; j < 4; j++) {
                        skelSegments[i][j] = skSegments[i][j];
                    }
                }

                for (i = 0; i < skelNumSegments; i++) {
                    skSegments[i] = null;
                }

                skSegments = null;
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Error allocating skeleton structure");

                setCompleted(false);

                return;
            }
        } // else skPoints != null

        if (perHDPoints > 0.0f) {
            fireProgressStateChanged("Getting high divergence points");
            fireProgressStateChanged(96);
            Preferences.debug("Getting high divergence points\n", Preferences.DEBUG_ALGORITHM);

            // Get top perHDPoints fraction of highest negative divergence points
            if (!getHighDivergencePoints()) {

                setCompleted(false);

                return;
            }

            Preferences.debug("Number of high divergence points = " + numHDPoints + "\n", Preferences.DEBUG_ALGORITHM);
            /*for (i = 0; i < numHDPoints; i++) {
             *  Preferences.debug(i + " = " + hdPoints[i][0] + "  " + hdPoints[i][1] + "  " +
             * hdPoints[i][2] + "\n", Preferences.DEBUG_ALGORITHM);}*/

            fireProgressStateChanged("Computing level 2 skeleton");
            fireProgressStateChanged(92);
            Preferences.debug("Computing level 2 skeleton\n", Preferences.DEBUG_ALGORITHM);

            if (!getLevel2Skeleton()) {

                setCompleted(false);

                return;
            }
        } // if (perHDPoints > 0.0f)

        fireProgressStateChanged(99);

        if (outputPoints) {
            // Write out the skeleton points

            // used is an array that specifies for each skeleton point whether or not
            // it was already used.  It is needed because intersection points belong
            // to more than one segment and might be put in a VOI more than one time
            try {
                used = new boolean[skelNumPoints];
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory error allocating used array");

                setCompleted(false);

                return;
            }

            // Output each segment
            ptNum = 0;

            for (i = 0; i < skelNumSegments; i++) {

                // Output the left end point of the segment
                segNum = 0;
                haveSegColor = false;

                if (!used[skelSegments[i][SKEL_SEG_LEFT]]) {
                    newPtVOI = new VOI(ptNum++, "point" + i + "_" + segNum + ".voi", VOI.POINT, -1);
                    segColor = newPtVOI.getColor();
                    haveSegColor = true;
                    xArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_LEFT]][0];
                    yArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_LEFT]][1];
                    zArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_LEFT]][2];
                    /*
                    newPtVOI.importCurve(xArr, yArr, zArr, (int) zArr[0]);
                    ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setFixed(true);
                    ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setLabel(String.valueOf(i)); */

                    newPtVOI.importCurve(xArr, yArr, zArr);
                    ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                    ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(String.valueOf(i));
                    srcImage.registerVOI(newPtVOI);
                    used[skelSegments[i][SKEL_SEG_LEFT]] = true;
                    segNum++;
                } // if (!used[skelSegments[i][SKEL_SEG_LEFT]])

                // Output the interior points
                for (j = skelSegments[i][SKEL_SEG_FIRST]; j <= skelSegments[i][SKEL_SEG_LAST]; j++) {

                    if ((j != skelSegments[i][SKEL_SEG_LEFT]) && (j != skelSegments[i][SKEL_SEG_RIGHT])) {

                        if (!used[j]) {
                            newPtVOI = new VOI(ptNum++, "point" + i + "_" + segNum + ".voi", VOI.POINT, -1);

                            if (haveSegColor) {
                                newPtVOI.setColor(segColor);
                            } else {
                                segColor = newPtVOI.getColor();
                                haveSegColor = true;
                            }

                            xArr[0] = (float) skelPoints[j][0];
                            yArr[0] = (float) skelPoints[j][1];
                            zArr[0] = (float) skelPoints[j][2];
                            /*
                            newPtVOI.importCurve(xArr, yArr, zArr, (int) zArr[0]);
                            ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setFixed(true);
                            ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setLabel(String.valueOf(i));*/
                            

                            newPtVOI.importCurve(xArr, yArr, zArr);
                            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(String.valueOf(i));
                            srcImage.registerVOI(newPtVOI);
                            used[j] = true;
                            segNum++;
                        }
                    }
                } // for (j = skelSegments[i][SKEL_SEG_FIRST]; j <= skelSegments[i][SKEL_SEG_LAST]; j++)

                // Output the right end point
                if (!used[skelSegments[i][SKEL_SEG_RIGHT]]) {
                    newPtVOI = new VOI(ptNum++, "point" + i + "_" + segNum + ".voi", VOI.POINT, -1);

                    if (haveSegColor) {
                        newPtVOI.setColor(segColor);
                    } else {
                        segColor = newPtVOI.getColor();
                        haveSegColor = true;
                    }

                    xArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_RIGHT]][0];
                    yArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_RIGHT]][1];
                    zArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_RIGHT]][2];
                    /*
                    newPtVOI.importCurve(xArr, yArr, zArr, (int) zArr[0]);
                    ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setFixed(true);
                    ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setLabel(String.valueOf(i)); */
                    newPtVOI.importCurve(xArr, yArr, zArr);
                    ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                    ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(String.valueOf(i));                 
                    srcImage.registerVOI(newPtVOI);
                    used[skelSegments[i][SKEL_SEG_RIGHT]] = true;
                    segNum++;
                } // if (!used[skelSegments[i][SKEL_SEG_RIGHT]])
            } // for (i = 0; i < skelNumSegments; i++)
        } // if (outputPoints)
        else {

            // Output only line segment end points
            ptNum = 0;

            for (i = 0; i < skelNumSegments; i++) {
                newPtVOI = new VOI(ptNum++, "pointL" + i + ".voi", VOI.POINT, -1);
                segColor = newPtVOI.getColor();
                xArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_LEFT]][0];
                yArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_LEFT]][1];
                zArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_LEFT]][2];
                /*
                newPtVOI.importCurve(xArr, yArr, zArr, (int) zArr[0]);
                ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setLabel(String.valueOf(i)); */
                newPtVOI.importCurve(xArr, yArr, zArr);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(String.valueOf(i));
                srcImage.registerVOI(newPtVOI);
                
                newPtVOI = new VOI(ptNum++, "pointR" + i + ".voi", VOI.POINT, -1);
                newPtVOI.setColor(segColor);
                xArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_RIGHT]][0];
                yArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_RIGHT]][1];
                zArr[0] = (float) skelPoints[skelSegments[i][SKEL_SEG_RIGHT]][2];
                /*
                newPtVOI.importCurve(xArr, yArr, zArr, (int) zArr[0]);
                ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[(int) zArr[0]].elementAt(0))).setLabel(String.valueOf(i)); */
                newPtVOI.importCurve(xArr, yArr, zArr);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(String.valueOf(i));
                srcImage.registerVOI(newPtVOI);
            } // for (i = 0; i < skelNumSegments; i++)
        } // else

        Preferences.debug("Segments output = " + skelNumSegments + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Points output = " + ptNum + "\n", Preferences.DEBUG_ALGORITHM);
        srcImage.notifyImageDisplayListeners();

        setCompleted(true);

        return;
    } // runAlgorithm

    /**
     * DOCUMENT ME!
     *
     * @param   point  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int addNewSkelSegment(int point) {
        int retval = -1;

        if (skelNumSegments >= skelSizeSegments) {
            MipavUtil.displayError((skelNumSegments + 1) + " skeleton segments detected, but only " + skelSizeSegments +
                                   " are allowed");

            return -2;
        }

        try {

            if (skelSegments[skelNumSegments] == null) {
                skelSegments[skelNumSegments] = new int[4];
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error allocating memory for skelSegments");

            return -2;
        }

        skelSegments[skelNumSegments][SKEL_SEG_LEFT] = point;
        skelSegments[skelNumSegments][SKEL_SEG_RIGHT] = point;
        skelSegments[skelNumSegments][SKEL_SEG_FIRST] = point;
        skelSegments[skelNumSegments][SKEL_SEG_LAST] = point;

        retval = skelNumSegments;
        skelNumSegments++;

        return retval;
    } // addNewSkelSegment

    /**
     * DOCUMENT ME!
     *
     * @param   pos  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int addSkeletonPoint(double[] pos) {
        int retval = skelNumPoints;

        // Copy the point's position
        skelPoints[retval][0] = pos[0];
        skelPoints[retval][1] = pos[1];
        skelPoints[retval][2] = pos[2];

        // Increase the number of skeleton points
        skelNumPoints++;

        if (skelNumPoints >= skelSizePoints) {
            MipavUtil.displayError("Too many skeleton points");

            return -2;
        }

        return retval;
    } // addSkeletonPoint

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean calculatePotentialField() {
        short[][] bound;
        short x, y, z;
        int xm1, ym1, zm1;
        boolean flagSurf, flagBound;
        int idx;
        int iidx;
        int numBound = 0;
        int zStartIndex, zEndIndex;
        int s;
        int yStartIndex, yEndIndex;
        int startIndex, endIndex;
        int v1, v2, v3;
        double r, t;
        int p;
        int[] ng = new int[26];

        try {
            bound = new short[BOUND_SIZE][3];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory during bound allocation");

            return false;
        }

        xm1 = xDim - 1;
        ym1 = yDim - 1;
        zm1 = zDim - 1;

        // Save all the boundary voxels in the array bound
        for (z = 1; z < zm1; z++) {

            for (y = 1; y < ym1; y++) {

                for (x = 1; x < xm1; x++) {
                    flagSurf = false;
                    flagBound = true;
                    idx = (z * sliceSize) + (y * xDim) + x;

                    // Case 1: treat the inner layer
                    if (vol[idx] == 0) {
                        continue;
                    }

                    // Consider six face neighbors, if anyone is zero, it is a boundary voxel
                    iidx = idx - 1;

                    if (vol[iidx] == 0) {
                        flagSurf = true;
                    }

                    if (!flagSurf || flagBound) {
                        iidx = idx + 1;

                        if (vol[iidx] == 0) {
                            flagSurf = true;
                        }

                        if (!flagSurf || flagBound) {
                            iidx = idx - xDim;

                            if (vol[iidx] == 0) {
                                flagSurf = true;
                            }

                            if (!flagSurf || flagBound) {
                                iidx = idx + xDim;

                                if (vol[iidx] == 0) {
                                    flagSurf = true;
                                }

                                if (!flagSurf || flagBound) {
                                    iidx = idx - sliceSize;

                                    if (vol[iidx] == 0) {
                                        flagSurf = true;
                                    }

                                    if (!flagSurf || flagBound) {
                                        iidx = idx + sliceSize;

                                        if (vol[iidx] == 0) {
                                            flagSurf = true;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if (flagSurf) {
                        vol[idx] = SURF;

                        if (flagBound) {

                            // If no neighbor of this voxel is already marked as boundary, then
                            // mark this one.  Or if we are taking all the boundary voxels
                            // (in this case flagBound stays true)
                            vol[idx] = BOUNDARY;
                            bound[numBound][0] = x;
                            bound[numBound][1] = y;
                            bound[numBound][2] = z;
                            numBound++;

                            if (numBound >= BOUND_SIZE) {
                                MipavUtil.displayError("Error: too many boundary points detected");

                                return false;
                            }
                        } // if (flagBound)
                    } // if (flagSurf)
                } // for (x = 1; x < xm1; x++)
            } // for (y = 1; y < ym1; y++)
        } // for (z = 1; z < zm1; z++)

        Preferences.debug("Found  " + numBound + " boundary voxels\n", Preferences.DEBUG_ALGORITHM);

        // Sort the boundary array
        sortBoundaryArray(numBound, bound);

        // Compute the potential field
        Preferences.debug("Computing potential field\n", Preferences.DEBUG_ALGORITHM);
        idx = -1;

        for (z = 0; z < zDim; z++) {
            Preferences.debug("Processing plane " + z + " out of " + (zDim - 1) + "\n", Preferences.DEBUG_ALGORITHM);
            fireProgressStateChanged(z * 90 / (zDim - 1));

            // Find the boundary voxels that will influence this point
            // Look at the z coordinate
            zStartIndex = 0;
            zEndIndex = numBound - 1;

            for (s = 0; s < numBound; s++) {

                if ((z - bound[s][2]) <= PF_THRESHOLD) {
                    zStartIndex = s;

                    break;
                }
            } // for (s = 0; s < numBound; s++)

            for (s = numBound - 1; s >= zStartIndex; s--) {

                if ((bound[s][2] - z) <= PF_THRESHOLD) {
                    zEndIndex = s;

                    break;
                }
            } // for (s = numBound-1; s>= zStartIndex; s--)

            for (y = 0; y < yDim; y++) {

                // Find the boundary voxels that will influence this point
                // Look at the y coordinate
                yStartIndex = zStartIndex;
                yEndIndex = zEndIndex;

                for (s = zStartIndex; s <= zEndIndex; s++) {

                    if ((y - bound[s][1]) <= PF_THRESHOLD) {
                        yStartIndex = s;

                        break;
                    }
                } // for (s = zStartIndex; s <= zEndIndex; s++)

                for (s = zEndIndex; s >= yStartIndex; s--) {

                    if ((bound[s][1] - y) <= PF_THRESHOLD) {
                        yEndIndex = s;

                        break;
                    }
                } // for (s = zEndIndex; s >= yStartIndex; s--)

                for (x = 0; x < xDim; x++) {
                    idx = idx + 1;

                    forceX[idx] = 0.0;
                    forceY[idx] = 0.0;
                    forceZ[idx] = 0.0;

                    if (vol[idx] == 0) {

                        // outside voxels have null force
                        continue;
                    } // if (vol[idx] == 0)

                    // Surface voxels (including those selected for the field calculation)
                    // are ignored for now.  The force there will be the average of their
                    // neighbors.  If we are to compute the force at the boundary voxels
                    // too, the force will point towards the exterior of the object
                    // (example: a 30x30x100 box)

                    if (vol[idx] == SURF) {
                        continue;
                    }

                    if (vol[idx] == BOUNDARY) {
                        continue;
                    }

                    // Find the boundary voxels that will influence this point
                    // Look at the x coordinate
                    startIndex = yStartIndex;
                    endIndex = yEndIndex;

                    for (s = yStartIndex; s <= yEndIndex; s++) {

                        if ((x - bound[s][0]) <= PF_THRESHOLD) {
                            startIndex = s;

                            break;
                        }
                    } // for (s = yStartIndex; s <= yEndIndex; s++)

                    for (s = yEndIndex; s >= startIndex; s--) {

                        if ((bound[s][0] - x) <= PF_THRESHOLD) {
                            endIndex = s;

                            break;
                        }
                    } // for (s = yEndIndex; s >= startIndex; s--)

                    if (endIndex < startIndex) {

                        // No boundary point is close enough to this point -
                        // take all the boundary points
                        startIndex = 0;
                        endIndex = numBound - 1;
                    } // if (endIndex < startIndex)

                    for (s = startIndex; s <= endIndex; s++) {
                        v1 = x - bound[s][0];
                        v2 = y - bound[s][1];
                        v3 = z - bound[s][2];

                        // euclidean metric
                        r = Math.sqrt((v1 * v1) + (v2 * v2) + (v3 * v3));

                        // r can be 0 if we are computing the force at boundary voxels too
                        // If the current point is a boundary point, some r will be zero,
                        // and that should be ignored
                        if (r != 0.0) {

                            // Raise r to the fieldStrength+1 power so that the force is
                            // 1/(dist^fieldStrength)
                            t = 1.00;

                            for (p = 0; p <= fieldStrength; p++) {
                                t = t * r;
                            }

                            r = t;

                            forceX[idx] = forceX[idx] + (v1 / r);
                            forceY[idx] = forceY[idx] + (v2 / r);
                            forceZ[idx] = forceZ[idx] + (v3 / r);
                        } // if (r != 0.0)
                    } // for (s = startIndex; s <= endIndex; s++)
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)

        // delete the bound array - don't need it anywhere
        for (s = 0; s < bound.length; s++) {
            bound[s] = null;
        }

        bound = null;

        // normalize force vectors
        for (idx = 0; idx < totLength; idx++) {

            // Only for interior voxels have forces been calculated
            if (vol[idx] == EXTERIOR) {
                continue;
            }

            r = (forceX[idx] * forceX[idx]) + (forceY[idx] * forceY[idx]) + (forceZ[idx] * forceZ[idx]);

            if (r > 0.0) {
                r = Math.sqrt(r);
                forceX[idx] = forceX[idx] / r;
                forceY[idx] = forceY[idx] / r;
                forceZ[idx] = forceZ[idx] / r;
            }
        } // for (idx = 0; idx < totLength; idx++)

        // Calculate the force at the surface
        // voxels as the average of the interior neighbors
        // face neighbors
        ng[0] = +sliceSize;
        ng[1] = -sliceSize;
        ng[2] = +xDim;
        ng[3] = -xDim;
        ng[4] = +1;
        ng[5] = -1;

        // vertex neighbors
        ng[6] = -sliceSize - xDim - 1;
        ng[7] = -sliceSize - xDim + 1;
        ng[8] = -sliceSize + xDim - 1;
        ng[9] = -sliceSize + xDim + 1;
        ng[10] = sliceSize - xDim - 1;
        ng[11] = sliceSize - xDim + 1;
        ng[12] = sliceSize + xDim - 1;
        ng[13] = sliceSize + xDim + 1;

        // edge neighbors
        ng[14] = sliceSize + xDim;
        ng[15] = sliceSize - xDim;
        ng[16] = -sliceSize + xDim;
        ng[17] = -sliceSize - xDim;
        ng[18] = sliceSize + 1;
        ng[19] = sliceSize - 1;
        ng[20] = -sliceSize + 1;
        ng[21] = -sliceSize - 1;
        ng[22] = xDim + 1;
        ng[23] = xDim - 1;
        ng[24] = -xDim + 1;
        ng[25] = -xDim - 1;

        for (z = 1; z < zm1; z++) {

            for (y = 1; y < ym1; y++) {

                for (x = 1; x < xm1; x++) {
                    idx = (z * sliceSize) + (y * xDim) + x;

                    if ((vol[idx] == SURF) || (vol[idx] == BOUNDARY)) {
                        forceX[idx] = 0.0;
                        forceY[idx] = 0.0;
                        forceZ[idx] = 0.0;

                        // Look at the neighbors and average the forces if not 0
                        v1 = 0;

                        for (s = 0; s < 26; s++) {
                            iidx = idx + ng[s]; // index of neighbor

                            // Take only neighbors that are not SURF or BOUNDARY
                            // because those neighbors have force = 0
                            if ((vol[iidx] == SURF) || (vol[iidx] == BOUNDARY)) {
                                continue;
                            }

                            // Take only interior neighbors
                            if (vol[iidx] == EXTERIOR) {
                                continue;
                            }

                            forceX[idx] = forceX[idx] + forceX[iidx];
                            forceY[idx] = forceY[idx] + forceY[iidx];
                            forceZ[idx] = forceZ[idx] + forceZ[iidx];
                            v1++;
                        } // for (s = 0; s < 26; s++)


                        // average
                        if (v1 != 0) {
                            forceX[idx] = forceX[idx] / (double) v1;
                            forceY[idx] = forceY[idx] / (double) v1;
                            forceZ[idx] = forceZ[idx] / (double) v1;
                        } // if (v1 != 0)

                        // normalize
                        r = (forceX[idx] * forceX[idx]) + (forceY[idx] * forceY[idx]) + (forceZ[idx] * forceZ[idx]);

                        if (r > 0.0) {
                            r = Math.sqrt(r);

                            forceX[idx] = forceX[idx] / r;
                            forceY[idx] = forceY[idx] / r;
                            forceZ[idx] = forceZ[idx] / r;
                        } // if (r > 0.0)
                    } // if ((vol[idx] == SURF) || (vol[idx] == BOUNDARY))
                } // for (x = 1; x < xm1; x++)
            } // for (y = 1; y < ym1; y++)
        } // for (z = 1; z < zm1; z++)

        return true;
    } // calculatePotentialField

    /**
     * DOCUMENT ME!
     *
     * @param   forces     DOCUMENT ME!
     * @param   numForces  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean changeInSign(double[][] forces, int numForces) {
        // sign change notes: If one component is 0 in all vectors, it should be considered as a change in sign or else
        // we might miss some critical points for which we have only NR_OF_SIGN_CHANGES - 1 changes in sign and the last
        // component is 0 all over the cell.  Example (2D):   *->--<-*   *->--<-* In the above example, the arrows
        // indicate the new vectors at the 4 corners of a 2D cell.  The x component changes sign, but the y component is
        // 0 in all 4 corners, and thus we might miss this critical point if we require exactly 2 sign changes.
        // Considering 0 everywhere as a change in sign will include this critical point.

        // Change in sign for at least one of the vector components leads to a sort of
        // medial surface.
        //
        // Change in sign for at least NR_OF_SIGN_CHANGES vector components
        int s, i, s2;
        int count;
        boolean change;

        if (numForces > 0) {
            count = 0;
            // check the components of these vectors.

            // x
            change = false;

            if (forces[0][0] < -EPSILON) {
                s = -1;
            } else if (forces[0][0] > EPSILON) {
                s = 1;
            } else {
                s = 0;
            }

            for (i = 1; i < numForces; i++) {

                if (forces[i][0] < -EPSILON) {
                    s2 = -1;
                } else if (forces[i][0] > EPSILON) {
                    s2 = 1;
                } else {
                    s2 = 0;
                }

                if (s2 != s) {
                    count++;
                    change = true;

                    break;
                }
            } // for (i = 1; i < numForces; i++)

            if ((!change) && (s == 0)) {

                // no change in sign but sign is 0
                count++;
            }

            if (count >= NR_OF_SIGN_CHANGES) {

                // return if at least NR_OF_SIGN_CHANGES vector components change sign
                return true;
            }

            // y
            change = false;

            if (forces[0][1] < -EPSILON) {
                s = -1;
            } else if (forces[0][1] > EPSILON) {
                s = 1;
            } else {
                s = 0;
            }

            for (i = 1; i < numForces; i++) {

                if (forces[i][1] < -EPSILON) {
                    s2 = -1;
                } else if (forces[i][1] > EPSILON) {
                    s2 = 1;
                } else {
                    s2 = 0;
                }

                if (s2 != s) {
                    count++;
                    change = true;

                    break;
                }
            } // for (i = 1; i < numForces; i++)

            if ((!change) && (s == 0)) {

                // no change in sign but sign is 0
                count++;
            }

            if (count >= NR_OF_SIGN_CHANGES) {

                // return if at least NR_OF_SIGN_CHANGES vector components change sign
                return true;
            }

            // z
            change = false;

            if (forces[0][2] < -EPSILON) {
                s = -1;
            } else if (forces[0][2] > EPSILON) {
                s = 1;
            } else {
                s = 0;
            }

            for (i = 1; i < numForces; i++) {

                if (forces[i][2] < -EPSILON) {
                    s2 = -1;
                } else if (forces[i][2] > EPSILON) {
                    s2 = 1;
                } else {
                    s2 = 0;
                }

                if (s2 != s) {
                    count++;
                    change = true;

                    break;
                }
            } // for (i = 1; i < numForces; i++)

            if ((!change) && (s == 0)) {

                // no change in sign but sign is 0
                count++;
            }

            if (count >= NR_OF_SIGN_CHANGES) {

                // return if at least NR_OF_SIGN_CHANGES vector components change sign
                return true;
            }
        } // if (numForces > 0)

        return false;
    } // changeInSign(double forces[][], int numForces)

    /**
     * DOCUMENT ME!
     *
     * @param   indices     DOCUMENT ME!
     * @param   numIndices  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean changeInSign(int[] indices, int numIndices) {

        // See notes in the other changeInSign routine
        int s, i, s2;
        int count;
        boolean change;

        // change in sign for at least NR_OF_SIGN_CHANGES vector components
        if (numIndices > 0) {
            count = 0;
            // check the components of these vectors

            // x
            change = false;

            if (forceX[indices[0]] < -EPSILON) {
                s = -1;
            } else if (forceX[indices[0]] > EPSILON) {
                s = 1;
            } else {
                s = 0;
            }

            for (i = 1; i < numIndices; i++) {

                if (forceX[indices[i]] < -EPSILON) {
                    s2 = -1;
                } else if (forceX[indices[i]] > EPSILON) {
                    s2 = 1;
                } else {
                    s2 = 0;
                }

                if (s2 != s) {
                    count++;
                    change = true;

                    break;
                }
            } // for (i = 1; i < numIndices; i++)

            if ((!change) && (s == 0)) {

                // no change in sign but sign is 0
                count++;
            }

            if (count >= NR_OF_SIGN_CHANGES) {

                // return if at least NR_OF_SIGN_CHANGES vector components change sign
                return true;
            }

            // Y
            change = false;

            if (forceY[indices[0]] < -EPSILON) {
                s = -1;
            } else if (forceY[indices[0]] > EPSILON) {
                s = 1;
            } else {
                s = 0;
            }

            for (i = 1; i < numIndices; i++) {

                if (forceY[indices[i]] < -EPSILON) {
                    s2 = -1;
                } else if (forceY[indices[i]] > EPSILON) {
                    s2 = 1;
                } else {
                    s2 = 0;
                }

                if (s2 != s) {
                    count++;
                    change = true;

                    break;
                }
            } // for (i = 1; i < numIndices; i++)

            if ((!change) && (s == 0)) {

                // no change in sign but sign is 0
                count++;
            }

            if (count >= NR_OF_SIGN_CHANGES) {

                // return if at least NR_OF_SIGN_CHANGES vector components change sign
                return true;
            }

            // Z
            change = false;

            if (forceZ[indices[0]] < -EPSILON) {
                s = -1;
            } else if (forceZ[indices[0]] > EPSILON) {
                s = 1;
            } else {
                s = 0;
            }

            for (i = 1; i < numIndices; i++) {

                if (forceZ[indices[i]] < -EPSILON) {
                    s2 = -1;
                } else if (forceZ[indices[i]] > EPSILON) {
                    s2 = 1;
                } else {
                    s2 = 0;
                }

                if (s2 != s) {
                    count++;
                    change = true;

                    break;
                }
            } // for (i = 1; i < numIndices; i++)

            if ((!change) && (s == 0)) {

                // no change in sign but sign is 0
                count++;
            }

            if (count >= NR_OF_SIGN_CHANGES) {

                // return if at least NR_OF_SIGN_CHANGES vector components change sign
                return true;
            }
        } // if (numIndices > 0)

        return false;
    } // changeInSign(int indices[], int numIndices)

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean checkVolumePadding() {

        // Make sure the volume is padded by enough empty planes in all 3 directions
        // so that placing charges at the specified distance from the boundary will
        // still leave a plane of empty voxels in each direction
        int x, y, z;
        int minX = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxY = Integer.MIN_VALUE;
        int minZ = Integer.MAX_VALUE;
        int maxZ = Integer.MIN_VALUE;
        int idx;

        for (z = 0; z < zDim; z++) {

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    idx = (z * sliceSize) + (y * xDim) + x;

                    if (volFloat[idx] >= minObjectValue) {

                        if (x < minX) {
                            minX = x;
                        }

                        if (y < minY) {
                            minY = y;
                        }

                        if (z < minZ) {
                            minZ = z;
                        }

                        if (x > maxX) {
                            maxX = x;
                        }

                        if (y > maxY) {
                            maxY = y;
                        }

                        if (z > maxZ) {
                            maxZ = z;
                        }
                    }
                }
            }
        }

        Preferences.debug("checkVolumePadding\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("minX = " + minX + " minY = " + minY + " minZ = " + minZ + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("maxX = " + maxX + " maxY = " + maxY + " maxZ = " + maxZ + "\n", Preferences.DEBUG_ALGORITHM);

        if (((minX - distCharges) <= 0) || ((minY - distCharges) <= 0) || ((minZ - distCharges) <= 0) ||
                ((maxX + distCharges) >= (xDim - 1)) || ((maxY + distCharges) >= (yDim - 1)) ||
                ((maxZ + distCharges) >= (zDim - 1))) {
            return false;
        }

        return true;

    } // checkVolumePadding

    /**
     * DOCUMENT ME!
     *
     * @param   pos          DOCUMENT ME!
     * @param   originCP     DOCUMENT ME!
     * @param   maxDistance  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int closeToCP(double[] pos, int originCP, double maxDistance) {
        int i;
        double a, b;

        // see if it's close to a critical point except the one specified in originCP
        for (i = 0; i < numCritPoints; i++) {

            // Ignore the critical point that started this streamline
            if (i == originCP) {
                continue;
            }

            if (Math.abs(critPts[i][0] - pos[0]) < maxDistance) {
                a = Math.abs(critPts[i][0] - pos[0]);

                if ((a + Math.abs(critPts[i][1] - pos[1])) < maxDistance) {
                    b = Math.abs(critPts[i][1] - pos[1]);

                    if ((a + b + Math.abs(critPts[i][2] - pos[2])) < maxDistance) {

                        // the point is "close" to a critical point
                        return i;
                    }
                }
            }
        }

        return -1;
    } // closeToCP

    /**
     * DOCUMENT ME!
     *
     * @param   pos               DOCUMENT ME!
     * @param   originSkel        DOCUMENT ME!
     * @param   nrAlreadyInSkel   DOCUMENT ME!
     * @param   maxDistance       DOCUMENT ME!
     * @param   crtSegmentLength  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int closeToSkel(double[] pos, int originSkel, int nrAlreadyInSkel, double maxDistance,
                            int crtSegmentLength) {
        int i;
        double a, b;
        boolean[] endPoint = new boolean[1];
        int seg;
        int dToLeft, dToRight;

        // See if it's close to a skeleton point, ignoring the one point specified
        // by originSkel
        for (i = 0; i < nrAlreadyInSkel; i++) {

            // ignore the point specified by originSkel
            if (i == originSkel) {
                continue;
            } // if (i == originSkel)

            // incrmental testing of the "close to" condition for higher speed
            if (Math.abs(skelPoints[i][0] - pos[0]) < maxDistance) {
                a = Math.abs(skelPoints[i][0] - pos[0]);

                if ((a + Math.abs(skelPoints[i][1] - pos[1])) < maxDistance) {
                    b = Math.abs(skelPoints[i][1] - pos[1]);

                    if ((a + b + Math.abs(skelPoints[i][2] - pos[2])) < maxDistance) {

                        // The point is "close" to a skeleton point
                        // If the skeleton point we are close to is also close to one of
                        // the segment end points, (but it's not an end point)
                        // do not report as being close to the point, wait to get to one
                        // of the segment end points
                        endPoint[0] = false;
                        seg = getSkelSegment(i, endPoint);

                        if (seg == -2) {
                            return -2;
                        }

                        if (endPoint[0]) {

                            // report the intersection if we are at an end point
                            return i;
                        } else {

                            // see how close we are to the endpoints of seg
                            dToLeft = i - skelSegments[seg][SKEL_SEG_FIRST];
                            dToRight = skelSegments[seg][SKEL_SEG_LAST] - i;

                            if ((dToLeft > SP_CLOSE_TO_SEGMENT_ENDPOINT) && (dToRight > SP_CLOSE_TO_SEGMENT_ENDPOINT)) {

                                // If we are close to an end point, report the intersection
                                return i;
                            }
                            // Otherwise, we are close to a segment given by seg, but we
                            // are also just a few points from the seg's end point and we
                            // hope to end the current segment at the same point where seg
                            // ends.  However, if the current segment's length is comparable
                            // to SP_CLOSE_TO_SEGMENT_ENDPOINT, then we should stop right
                            // here because this won't look good

                            // Comparable means not more than twice as long as
                            // SP_CLOSE_TO_SEGMENT_ENDPOINT
                            if (crtSegmentLength <= (2 * SP_CLOSE_TO_SEGMENT_ENDPOINT)) {
                                return i;
                            }
                        }
                    }
                }
            }
        } // for (i = 0; i < nrAlreeadyInSkel; i++)

        return -1;
    } // closeToSkel

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
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

    /**
     * DOCUMENT ME!
     */
    private void deleteLastSkelSegment() {

        if (skelNumSegments > 0) {
            skelNumSegments--;
        }

        return;
    } // deleteLastSkelSegment

    /**
     * DOCUMENT ME!
     *
     * @param   pos1  DOCUMENT ME!
     * @param   pos2  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double distance(double[] pos1, double[] pos2) {
        return (Math.abs(pos1[0] - pos2[0]) + Math.abs(pos1[1] - pos2[1]) + Math.abs(pos1[2] - pos2[2]));
    } // distance

    /**
     * DOCUMENT ME!
     *
     * @param   padTarget     DOCUMENT ME!
     * @param   padCharacter  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean expandVolume(byte padTarget, byte padCharacter) {
        int x, y, z;
        int idx;
        int ii;
        int iidx;

        // face neighbors
        int[] ng = new int[] { +sliceSize, -sliceSize, +xDim, -xDim, +1, -1 };

        // Look at face neighbors only because that gives the best result,
        // compared with looking at face and edge or
        // face, edge, and vertex
        for (z = 0; z < zDim; z++) {

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    idx = (z * sliceSize) + (y * xDim) + x;

                    if (vol[idx] == padTarget) {

                        // Look at all the face neighbors of this voxel
                        // and replace all the "blank" neighbors with the new value
                        for (ii = 0; ii < 6; ii++) {
                            iidx = idx + ng[ii];

                            if ((iidx < 0) || (iidx >= totLength)) {
                                return false;
                            }

                            if (vol[iidx] == EXTERIOR) {
                                vol[iidx] = padCharacter;
                            }
                        }
                    }
                }
            }
        }

        return true;
    } // expandVolume

    /**
     * DOCUMENT ME!
     *
     * @param   x         DOCUMENT ME!
     * @param   y         DOCUMENT ME!
     * @param   z         DOCUMENT ME!
     * @param   cellSize  DOCUMENT ME!
     * @param   critPt    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean findCriticalPointInFloatCell(double x, double y, double z, double cellSize, double[] critPt) {
        int xx, yy, zz;
        double[][] cv = new double[8][3];

        // interpolate vector values at each of the 8 vertices of the cell
        cv[0] = interpolation(x, y, z);
        cv[1] = interpolation(x + cellSize, y, z);
        cv[2] = interpolation(x, y + cellSize, z);
        cv[3] = interpolation(x + cellSize, y + cellSize, z);
        cv[4] = interpolation(x, y, z + cellSize);
        cv[5] = interpolation(x + cellSize, y, z + cellSize);
        cv[6] = interpolation(x, y + cellSize, z + cellSize);
        cv[7] = interpolation(x + cellSize, y + cellSize, z + cellSize);

        // If first vertex vector (corresponding to (x, y, z)) is (0, 0, 0),
        // then return (x, y, z) as the critical point
        if ((cv[0][0] < EPSILON) && (cv[0][0] > -EPSILON) && (cv[0][1] < EPSILON) && (cv[0][1] > -EPSILON) &&
                (cv[0][2] < EPSILON) && (cv[0][2] > -EPSILON)) {
            critPt[0] = x;
            critPt[1] = y;
            critPt[2] = z;

            return true;
        }

        // The cell is a candidate cell if there is a change of sign
        // in one of the vector components among all eight vertices of the cell
        if (changeInSign(cv, 8)) {

            // If cell size < 1/CELL_SUBDIVISION_FACTOR,
            // stop here and assume critical point is in the center of the cell
            if (cellSize <= (1.0 / CELL_SUBDIVISION_FACTOR)) {
                critPt[0] = x + (cellSize / 2.0);
                critPt[1] = y + (cellSize / 2.0);
                critPt[2] = z + (cellSize / 2.0);

                return true;
            } // if (cellSize <= (1.0/CELL_SUBDIVISION_FACTOR))

            // It is a candidate cell and it's not small enough
            // Divide the cell in 8 subcells
            // and try to find the critical point in one of the subcells
            for (zz = 0; zz < 2; zz++) {

                for (yy = 0; yy < 2; yy++) {

                    for (xx = 0; xx < 2; xx++) {

                        if (findCriticalPointInFloatCell(x + (xx * cellSize / 2.0), y + (yy * cellSize / 2.0),
                                                             z + (zz * cellSize / 2.0), cellSize / 2.0, critPt)) {

                            // critPt is already set
                            return true;
                        }
                    }
                }
            }
        } // if (changeInSign(cv, 8))

        return false;
    } // findCriticalPointInFloatCell

    /**
     * DOCUMENT ME!
     *
     * @param   x       DOCUMENT ME!
     * @param   y       DOCUMENT ME!
     * @param   z       DOCUMENT ME!
     * @param   inds    DOCUMENT ME!
     * @param   critPt  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean findCriticalPointInIntCell(int x, int y, int z, int[] inds, double[] critPt) {
        int xx, yy, zz;

        // If first vertex vector (corresponding to (x, y, z) with index in inds[0])
        // is (0, 0, 0), then return (x, y, z) as the critical point
        if ((forceX[inds[0]] < EPSILON) && (forceX[inds[0]] > -EPSILON) && (forceY[inds[0]] < EPSILON) &&
                (forceY[inds[0]] > -EPSILON) && (forceZ[inds[0]] < EPSILON) && (forceZ[inds[0]] > -EPSILON)) {
            critPt[0] = x;
            critPt[1] = y;
            critPt[2] = z;

            return true;
        }

        // The cell is a candidate cell if there is a change of sign
        // in one of the vector components among all the eight vertices of the cell
        if (changeInSign(inds, 8)) {

            // It is a candidate cell.
            // Divide the cell in 8 subcells
            // For each of those 8 subcells do the candidate test again
            // and try to find the critical point in one of the candidate subcells
            for (zz = 0; zz < 2; zz++) {

                for (yy = 0; yy < 2; yy++) {

                    for (xx = 0; xx < 2; xx++) {

                        if (findCriticalPointInFloatCell(x + (xx / 2.0), y + (yy / 2.0), z + (zz / 2.0), 0.50,
                                                             critPt)) {

                            // critPt is already set
                            return true;
                        }
                    }
                }
            }
        } // if (changeInSign(inds, 8))

        return false;
    } // findCriticalPointInIntCell

    /**
     * DOCUMENT ME!
     */
    private void flagVolume() {
        int i, s;
        int x, y, z, idx, idx2;

        // Only face neighbors
        int[] neighborhood = new int[] { -1, +1, -xDim, +xDim, -sliceSize, +sliceSize };

        for (i = 0; i < totLength; i++) {

            // EXTERIOR == 0
            if (vol[i] != 0) {
                vol[i] = INTERIOR;
            }
        } // for (i = 0; i < totLength; i++)

        // Look at the interior voxels.
        // If an INTERIOR voxel has an EXTERIOR neighbor,
        // then it is a SURFace voxel.
        // Look only at the neighbors defined by the neighborhood
        for (z = 1; z < (zDim - 1); z++) {

            for (y = 1; y < (yDim - 1); y++) {

                for (x = 1; x < (xDim - 1); x++) {
                    idx = (z * sliceSize) + (y * xDim) + x;

                    if (vol[idx] == INTERIOR) {

                        for (s = 0; s < 6; s++) {
                            idx2 = idx + neighborhood[s];

                            if (vol[idx2] == EXTERIOR) {
                                vol[idx] = SURF;

                                break;
                            }
                        }
                    }
                }
            }
        }

        return;
    } // flagVolume

    /**
     * DOCUMENT ME!
     *
     * @param  xPlane  DOCUMENT ME!
     */
    private void floodFillXPlane(int xPlane) {

        // Floodfill each x plane with OUTSIDE_3 values
        int idx, idxS, idxN;
        boolean anyChange = true;
        int y, z;

        // Set (y = 0, z = 0) point to OUTSIDE_3
        idx = xPlane;
        vol[idx] = OUTSIDE_3;

        while (anyChange) {
            anyChange = false;

            // Loop from left to right and top to bottom
            for (y = 0; y < yDim; y++) {

                for (z = 0; z < zDim; z++) {
                    idxS = (z * sliceSize) + (y * xDim) + idx;

                    // If the point is set to OUTSIDE_3, then set all empty neighbors
                    // to OUTSIDE_3
                    if (vol[idxS] == OUTSIDE_3) {
                        idxN = idxS + sliceSize;

                        if ((z < (zDim - 1)) &&
                                ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1) || (vol[idxN] == OUTSIDE_2))) {
                            vol[idxN] = OUTSIDE_3;
                            anyChange = true;
                        }

                        idxN = idxS - sliceSize;

                        if ((z > 0) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1) || (vol[idxN] == OUTSIDE_2))) {
                            vol[idxN] = OUTSIDE_3;
                            anyChange = true;
                        }

                        idxN = idxS + xDim;

                        if ((y < (yDim - 1)) &&
                                ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1) || (vol[idxN] == OUTSIDE_2))) {
                            vol[idxN] = OUTSIDE_3;
                            anyChange = true;
                        }

                        idxN = idxS - xDim;

                        if ((y > 0) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1) || (vol[idxN] == OUTSIDE_2))) {
                            vol[idxN] = OUTSIDE_3;
                            anyChange = true;
                        }
                    } // if (vol[idxS] == OUTSIDE_3)
                } // for (z = 0; z < zDim; z++)
            } // for (y = 0; y < yDim; y++)

            if (anyChange) {

                // Same loop but bottom to top and right to left
                anyChange = false;

                for (y = yDim - 1; y >= 0; y--) {

                    for (z = zDim - 1; z >= 0; z--) {
                        idxS = (z * sliceSize) + (y * xDim) + idx;

                        // If the point is set to OUTSIDE_3, then set all empty neighbors
                        // to OUTSIDE_3
                        if (vol[idxS] == OUTSIDE_3) {
                            idxN = idxS + sliceSize;

                            if ((z < (zDim - 1)) &&
                                    ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1) || (vol[idxN] == OUTSIDE_2))) {
                                vol[idxN] = OUTSIDE_3;
                                anyChange = true;
                            }

                            idxN = idxS - sliceSize;

                            if ((z > 0) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1) || (vol[idxN] == OUTSIDE_2))) {
                                vol[idxN] = OUTSIDE_3;
                                anyChange = true;
                            }

                            idxN = idxS + xDim;

                            if ((y < (yDim - 1)) &&
                                    ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1) || (vol[idxN] == OUTSIDE_2))) {
                                vol[idxN] = OUTSIDE_3;
                                anyChange = true;
                            }

                            idxN = idxS - xDim;

                            if ((y > 0) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1) || (vol[idxN] == OUTSIDE_2))) {
                                vol[idxN] = OUTSIDE_3;
                                anyChange = true;
                            }
                        } // if (vol[idxS] == OUTSIDE_3)
                    } // for (z = zDim - 1; z >= 0; z--)
                } // for (y = yDim - 1; y >= 0; y--)
            } // if (anyChange)
        } // while (anyChange)

        return;
    } // floodFillXPlane

    /**
     * DOCUMENT ME!
     *
     * @param  yPlane  DOCUMENT ME!
     */
    private void floodFillYPlane(int yPlane) {

        // Floodfill each y plane with OUTSIDE_2 values
        int idx, idxS, idxN;
        boolean anyChange = true;
        int x, z;

        // Set point with (x = 0, z = 0) to OUTSIDE_2
        idx = yPlane * xDim;
        vol[idx] = OUTSIDE_2;

        while (anyChange) {
            anyChange = false;

            // Loop from left to right and from top to bottom
            for (x = 0; x < xDim; x++) {

                for (z = 0; z < zDim; z++) {
                    idxS = (z * sliceSize) + idx + x;

                    // If the point is set to OUTSIDE_2, then set all empty neighbors
                    // to OUTSIDE_2
                    if (vol[idxS] == OUTSIDE_2) {
                        idxN = idxS + sliceSize;

                        if ((z < (zDim - 1)) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1))) {
                            vol[idxN] = OUTSIDE_2;
                            anyChange = true;
                        }

                        idxN = idxS - sliceSize;

                        if ((z > 0) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1))) {
                            vol[idxN] = OUTSIDE_2;
                            anyChange = true;
                        }

                        idxN = idxS + 1;

                        if ((x < (xDim - 1)) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1))) {
                            vol[idxN] = OUTSIDE_2;
                            anyChange = true;
                        }

                        idxN = idxS - 1;

                        if ((x > 0) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1))) {
                            vol[idxN] = OUTSIDE_2;
                            anyChange = true;
                        }
                    } // if (vol[idxS == OUTSIDE_2)
                } // for (z = 0; z < zDim; z++)
            } // for (x = 0; x < xDim; x++)
        } // while (anyChange)

        if (anyChange) {

            // Same loop but bottom to top and right to left
            anyChange = false;

            for (x = xDim - 1; x >= 0; x--) {

                for (z = zDim - 1; z >= 0; z--) {
                    idxS = (z * sliceSize) + idx + x;

                    // If the point is set to OUTSIDE_2, then set all empty neighbors
                    // to OUTSIDE_2
                    if (vol[idxS] == OUTSIDE_2) {
                        idxN = idxS + sliceSize;

                        if ((z < (zDim - 1)) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1))) {
                            vol[idxN] = OUTSIDE_2;
                            anyChange = true;
                        }

                        idxN = idxS - sliceSize;

                        if ((z > 0) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1))) {
                            vol[idxN] = OUTSIDE_2;
                            anyChange = true;
                        }

                        idxN = idxS + 1;

                        if ((x < (xDim - 1)) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1))) {
                            vol[idxN] = OUTSIDE_2;
                            anyChange = true;
                        }

                        idxN = idxS - 1;

                        if ((x > 0) && ((vol[idxN] == 0) || (vol[idxN] == OUTSIDE_1))) {
                            vol[idxN] = OUTSIDE_2;
                            anyChange = true;
                        }
                    } // if (vol[idxS] == OUTSIDE_2)
                } // for (z = zDim - 1; z >= 0; z--)
            } // for (x = xDim - 1; x >= 0; x--)
        } // if (anyChange)

        return;
    } // floodFillYPlane

    /**
     * DOCUMENT ME!
     *
     * @param  zPlane  DOCUMENT ME!
     */
    private void floodFillZPlane(int zPlane) {
        int idx, idxS, idxN;
        boolean anyChange = true;
        int x, y;

        // Floodfill each z plane with OUTSIDE_1 values
        // Set (x = 0, y = 0) point to OUTSIDE_1
        idx = zPlane * sliceSize;
        vol[idx] = OUTSIDE_1;

        while (anyChange) {
            anyChange = false;

            // loop from left to right and top to bottom
            for (x = 0; x < xDim; x++) {

                for (y = 0; y < yDim; y++) {
                    idxS = idx + (y * xDim) + x;

                    // If the point is set to OUTSIDE_1, then set all empty neighbors
                    // to OUTSIDE_1
                    if (vol[idxS] == OUTSIDE_1) {
                        idxN = idxS + xDim;

                        if ((y < (yDim - 1)) && (vol[idxN] == 0)) {
                            vol[idxN] = OUTSIDE_1;
                            anyChange = true;
                        }

                        idxN = idxS - xDim;

                        if ((y > 0) && (vol[idxN] == 0)) {
                            vol[idxN] = OUTSIDE_1;
                            anyChange = true;
                        }

                        idxN = idxS + 1;

                        if ((x < (xDim - 1)) && (vol[idxN] == 0)) {
                            vol[idxN] = OUTSIDE_1;
                            anyChange = true;
                        }

                        idxN = idxS - 1;

                        if ((x > 1) && (vol[idxN] == 0)) {
                            vol[idxN] = OUTSIDE_1;
                            anyChange = true;
                        }
                    } // if (vol[idxS] == OUTSIDE_1)
                } // for (y = 0; y < yDim; y++)
            } // for (x = 0; x < xDim; x++)

            if (anyChange) {

                // Same loop but bottom to top and right to left
                anyChange = false;

                for (x = xDim - 1; x >= 0; x--) {

                    for (y = yDim - 1; y >= 0; y--) {
                        idxS = idx + (y * xDim) + x;

                        // If the point is set to OUTSIDE_1, then set all empty neighbors
                        // to OUTSIDE_1
                        if (vol[idxS] == OUTSIDE_1) {
                            idxN = idxS + xDim;

                            if ((y < (yDim - 1)) && (vol[idxN] == 0)) {
                                vol[idxN] = OUTSIDE_1;
                                anyChange = true;
                            }

                            idxN = idxS - xDim;

                            if ((y > 0) && (vol[idxN] == 0)) {
                                vol[idxN] = OUTSIDE_1;
                                anyChange = true;
                            }

                            idxN = idxS + 1;

                            if ((x < (xDim - 1)) && (vol[idxN] == 0)) {
                                vol[idxN] = OUTSIDE_1;
                                anyChange = true;
                            }

                            idxN = idxS - 1;

                            if ((x > 0) && (vol[idxN] == 0)) {
                                vol[idxN] = OUTSIDE_1;
                                anyChange = true;
                            }
                        } // if (vol[idxS] == OUTSIDE_1)
                    } // for (y = yDim - 1; y >= 0; y--)
                } // for (x = xDim - 1; x >= 0; x--)
            } // if (anyChange)
        } // while (anyChange)

        return;
    } // floodFillZPlane

    /**
     * DOCUMENT ME!
     *
     * @param   originCP          DOCUMENT ME!
     * @param   lookInCP          DOCUMENT ME!
     * @param   startPt           DOCUMENT ME!
     * @param   whereTo           DOCUMENT ME!
     * @param   critPtInSkeleton  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean followStreamLines(int originCP, boolean lookInCP, double[] startPt, double[] whereTo,
                                      int[] critPtInSkeleton) {

        // Creates an entire skeleton segment
        double[] startPos = new double[3];
        double[] nextPos = new double[3];
        double[] lastAddedSkelPoint = new double[3];
        int nrAlreadyInSkel;
        int step;
        boolean stop;
        int crtSegment, newSeg, segToBeSplit;
        boolean[] endPoint = new boolean[1];
        int sp, cp, s, ocpSkelPos;
        int crtSegLength = 0;
        int saveCrtSegLen;
        int quot;
        int rem;

        int exit_status = FSL_EXS_ERROR;
        int exit_data = -1;
        boolean first_iteration = true;

        // Used to limit the search in the skeleton only to points not in the current segment
        nrAlreadyInSkel = skelNumPoints;

        startPos[0] = startPt[0];
        startPos[1] = startPt[1];
        startPos[2] = startPt[2];

        // The position of the original critical point in the skeleton array if it was already
        // there at the start of this function
        ocpSkelPos = -1;

        // Add the start point to the skeleton if not already there
        // The point can already be in the skeleton if it's a critical point
        // that was added as part of a previous segment (example: a saddle point
        // belongs to at least 2 segments)
        // The critPtInSkeleton array tells us if a critical point is already in the
        // skeleton or not and if it is, it tells us its location

        s = -1;

        if (originCP != -1) {
            s = critPtInSkeleton[originCP];
            ocpSkelPos = s;

            if (s == -1) {
                s = addSkeletonPoint(startPos);

                if (s == -2) {
                    return false;
                }

                // Update the critPtInSkeleton array
                critPtInSkeleton[originCP] = s;
            }
        } // if (originCP != -1)
        else {
            s = addSkeletonPoint(startPos);

            if (s == -2) {
                return false;
            }
        }

        lastAddedSkelPoint[0] = startPos[0];
        lastAddedSkelPoint[1] = startPos[1];
        lastAddedSkelPoint[2] = startPos[2];

        // Add a new segment to the skeleton starting and ending at this point
        crtSegment = addNewSkelSegment(s);

        if (s == -2) {
            return false;
        }

        // There is a new skeleton segment added after this point
        // Make sure you set it correctly or delete it before leaving this function
        exit_status = FSL_EXS_ERROR;
        exit_data = 0;

        first_iteration = true;
        step = 0;
        stop = false;
        saveCrtSegLen = crtSegLength;

        while (!stop) {

            // If startPos is on the bounding box or outside, remove this segment
            if ((startPos[0] <= 0) || (startPos[0] >= (xDim - 1)) || (startPos[1] <= 0) ||
                    (startPos[1] >= (yDim - 1)) || (startPos[2] <= 0) || (startPos[2] >= (zDim - 1))) {
                exit_status = FSL_EXS_OUTSIDE;
                stop = true;

                break;
            }

            step++;

            // At every multiple of PROGRESS_CHECK_INTERVAL steps,
            // take a moment to see if we are moving at all
            quot = step / PROGRESS_CHECK_INTERVAL;
            rem = step % PROGRESS_CHECK_INTERVAL;

            if (rem == 0) {

                if (crtSegLength > saveCrtSegLen) {

                    // yes we are moving - continue
                    saveCrtSegLen = crtSegLength;

                    // but not forever
                    // MAX_NUM_STEP_INTERVALS times PROGRESS_STEP_INTERVAL steps
                    // should be enough
                    if (quot > MAX_NUM_STEP_INTERVALS) {

                        // Stop if more than ... steps were performed.  Most likely we are
                        // chasing our own tail.
                        exit_status = FSL_EXS_TOO_MANY_STEPS;
                        stop = true;

                        break;
                    } // if (quot > MAX_NUM_STEP_INTERVALS)
                } // if (crtSegLength > saveCrtSegLen)
                else {

                    // we didn't add any skeleton point in the last PROGRESS_CHECK_INTERVAL
                    // steps.  We should stop
                    exit_status = FSL_EXS_TOO_MANY_STEPS;
                    stop = true;

                    break;
                }
            } // if (rem == 0)

            // Compute current segment length so far
            crtSegLength = skelSegments[crtSegment][SKEL_SEG_LAST] - skelSegments[crtSegment][SKEL_SEG_FIRST] + 1;
            // If this point is close to another point that is already part of the skeleton
            // but not in the same segment, we should stop.  The points in the same segment
            // are excluded from the search since we are only searching among the points that
            // existed in the Skel array when this function started.

            // If we are starting from a critical point and we are at the first iteration,
            // don't check if we are close to a skeleton point.  We may be close to a
            // skeleton point now, but we may be moving away from it at the next step.
            if ((originCP != -1) && (first_iteration)) {
                // don't check
            } // if ((originCP != -1) && (first_iteration))
            else {
                sp = closeToSkel(startPos, ocpSkelPos, nrAlreadyInSkel, SP_SMALL_DISTANCE, crtSegLength);

                if (sp == -2) {
                    return false;
                } else if (sp != -1) {
                    exit_status = FSL_EXS_CLOSE_TO_SKEL;
                    exit_data = sp;
                    stop = true;

                    break;
                } // else if (sp != -1)
            } // else

            // Check if we are close to a critical point (if lookInCP is true)

            // If we are starting from a critical point and we are at the first
            // iteration, don't check if we are close to a critical point.
            // We may be close to a critical point now, but we may be moving away from
            // it at the next step.
            if ((originCP != -1) && (first_iteration)) {
                // don't check
            } // if (originCP != -1) && (first_iteration))
            else if (lookInCP) {
                cp = closeToCP(startPos, originCP, SP_SMALL_DISTANCE);

                if (cp != -1) {
                    exit_status = FSL_EXS_CLOSE_TO_CP;
                    exit_data = cp;
                    stop = true;

                    break;
                } // if (cp != -1)
            } // else if (lookInCP)

            // Add startPos to the skeleton if not too close to last added point
            // for the first iteration.  startPos is equal to lastAddedSkelPoint so
            // the starting position is not added twice
            if (distance(lastAddedSkelPoint, startPos) > SP_SMALL_DISTANCE) {

                // Add current position to the skeleton
                s = addSkeletonPoint(startPos);

                if (s == -2) {
                    return false;
                }

                // Update current segment
                if (skelSegments[crtSegment][SKEL_SEG_LEFT] == skelSegments[crtSegment][SKEL_SEG_FIRST]) {

                    // If LEFT and FIRST are the same, make FIRST equal to this new position
                    skelSegments[crtSegment][SKEL_SEG_FIRST] = s;
                }

                skelSegments[crtSegment][SKEL_SEG_LAST] = s;
                skelSegments[crtSegment][SKEL_SEG_RIGHT] = s;

                // Update lastAddedSkelPoint
                lastAddedSkelPoint[0] = startPos[0];
                lastAddedSkelPoint[1] = startPos[1];
                lastAddedSkelPoint[2] = startPos[2];
            } // if (distance(lastAddedSkelPoint, startPos) > SP_SMALL_DISTANCE)

            // Move to the next position
            // For the first iteration, the next position is given by the
            // whereTo vector, if it's not null.  If it is null, then we take
            // the force field value at the start position
            if (first_iteration) {
                first_iteration = false;

                if (whereTo != null) {
                    nextPos[0] = startPos[0] + (whereTo[0] * STEP_SIZE);
                    nextPos[1] = startPos[1] + (whereTo[1] * STEP_SIZE);
                    nextPos[2] = startPos[2] + (whereTo[2] * STEP_SIZE);
                } else {
                    rk2(startPos[0], startPos[1], startPos[2], STEP_SIZE, nextPos);
                }
            } // if (first_iteration)
            else {

                // For the subsequent iterations, we use the vector field value
                // at the current position
                rk2(startPos[0], startPos[1], startPos[2], STEP_SIZE, nextPos);
            } // else not first_iteration

            // If nextPos = startPos we are stuck and should stop
            if (((nextPos[0] - startPos[0]) < EPSILON) && ((nextPos[0] - startPos[0]) > -EPSILON) &&
                    ((nextPos[1] - startPos[1]) < EPSILON) && ((nextPos[1] - startPos[1]) > -EPSILON) &&
                    ((nextPos[2] - startPos[2]) < EPSILON) && ((nextPos[2] - startPos[2]) > -EPSILON)) {
                exit_status = FSL_EXS_STUCK;
                stop = true;

                break;
            }

            // Next position becomes current position
            startPos[0] = nextPos[0];
            startPos[1] = nextPos[1];
            startPos[2] = nextPos[2];
            first_iteration = false;
        } // while (!stop)

        // Usually, if the current segment is not long enough
        // we are not going to keep it.
        // However, if the segment originated in a critical point, we should
        // keep it regardless of length.  Removing it might disconnect the skeleton.

        // Check segment length only if not originated at a critical point
        if (originCP == -1) {

            if (!segmentIsLongEnough(crtSegment)) {

                // The segment is not long enough - will be removed
                deleteLastSkelSegment();

                // Also delete the points added to the skeleton during the processing of
                // this segment
                skelNumPoints = nrAlreadyInSkel;

                return true;
            }
        } // if (originCP == -1)

        // We are going to keep the segment; let's finish the job
        switch (exit_status) {

            case FSL_EXS_TOO_MANY_STEPS:

                // nothing to do
                break;

            case FSL_EXS_CLOSE_TO_SKEL:

                // We are close to a skeleton point, belonging to another segment
                // End the current segment at the intersection point
                skelSegments[crtSegment][SKEL_SEG_RIGHT] = exit_data;

                // Find the segment that contains the skeleton point we are close to
                // (that segment will be split into 2 if we are not close to one of
                // its end points)
                endPoint[0] = false;
                segToBeSplit = getSkelSegment(exit_data, endPoint);
                if (segToBeSplit == -2) {
                    return false;
                }

                if (!endPoint[0]) {

                    // Not close to one of the end points - the segment must be split
                    // Create a new skeleton segment starting at the intersection point
                    // and ending where the original segment ended.
                    newSeg = addNewSkelSegment(exit_data);

                    if (newSeg == -2) {
                        return false;
                    }

                    // LEFT and FIRST are set to exit_data by addNewSkelSegment
                    skelSegments[newSeg][SKEL_SEG_RIGHT] = skelSegments[segToBeSplit][SKEL_SEG_RIGHT];
                    skelSegments[newSeg][SKEL_SEG_LAST] = skelSegments[segToBeSplit][SKEL_SEG_LAST];

                    // The old segment now has to terminate at the intersection point
                    skelSegments[segToBeSplit][SKEL_SEG_RIGHT] = exit_data;
                    skelSegments[segToBeSplit][SKEL_SEG_LAST] = exit_data;
                } // if (!endPoint[0])
                else {
                    // We are close to one of the end points of an existing segment
                    // The original segment doesn't have to be split
                } // else

                break;

            case FSL_EXS_CLOSE_TO_CP:
                // We are close to a critical point that is not in the skeleton yet.
                // (We know it's not in the skeleton yet because we first check if
                // we are close to a skeleton point, and only if we are not close
                // to any skeleton point do we check whether we are close to a critical
                // point)

                // Add the critical point to the skeleton
                sp = addSkeletonPoint(critPts[exit_data]);
                if (sp == -2) {
                    return false;
                }

                // Mark the critical point as being part of the skeleton
                critPtInSkeleton[exit_data] = sp;

                // End the current segment at the critical point
                skelSegments[crtSegment][SKEL_SEG_RIGHT] = sp;
                break;

            case FSL_EXS_STUCK:

                // We are stuck in a place - nothing to do, just exit
                break;

            case FSL_EXS_OUTSIDE:

                // The segment touched the bounding box.
                // The segment will be removed
                deleteLastSkelSegment();

                // Also delete the points added to the skeleton during the processing
                // of this segment
                skelNumPoints = nrAlreadyInSkel;
                break;

            case FSL_EXS_ERROR:
                MipavUtil.displayError("follow stream lines exit status error");

                return false;
        } // switch(exit_status)

        return true;
    } // followStreamLines

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean getCriticalPoints() {
        int idx;
        int x, y, z, ii;
        double vecLength;
        double[] critpt = new double[3];
        int[] inds = new int[27];
        boolean skipThisPoint = false;
        double[][] cv;
        double vdist;
        int i;
        double[][] jac;
        double[] realEigen;
        double[][] eigenvec;

        try {
            critPts = new double[MAX_NUM_CRITPTS][3];
            pointType = new byte[MAX_NUM_CRITPTS];

            // 3 eigenvalues and 3 eigenvectors of the Jacobian matrix are evaluated at the
            // critical point
            realEigenvalues = new double[MAX_NUM_CRITPTS][3];

            // 3 double values for each vector
            eigenvectors = new double[MAX_NUM_CRITPTS][3][3];

            eigenvec = new double[3][3];
            realEigen = new double[3];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error allocating critical point memory");

            return false;
        }

        numCritPoints = 0;

        for (z = 1; z < (zDim - 1); z++) {
            Preferences.debug("Processing plane " + z + " out of " + (zDim - 1) + "\n", Preferences.DEBUG_ALGORITHM);

            for (y = 1; y < (yDim - 1); y++) {

                for (x = 1; x < (xDim - 1); x++) {
                    idx = (z * sliceSize) + (y * xDim) + x;

                    // Ignore voxels that are BOUNDARY or SURF or have a neighbor that is

                    // Also, ignore voxels that are EXTERIOR or have a neighbor that is

                    // We have to check the neighbors that will be touched by the interpolation

                    // The flags array indicates the state of all the face neighbors,
                    // but we are not interested in all the face neighbors,
                    // just 3 of them, so will will not be using the flag SURF or BOUNDARY
                    // at the current point to find out if it has any external neighbors.

                    // These are the neighbors that will be touched by the interpolation
                    inds[0] = idx;
                    inds[1] = idx + 1;
                    inds[2] = idx + xDim;
                    inds[3] = idx + xDim + 1;
                    inds[4] = idx + sliceSize;
                    inds[5] = idx + sliceSize + 1;
                    inds[6] = idx + sliceSize + xDim;
                    inds[7] = idx + sliceSize + xDim + 1;

                    skipThisPoint = false;

                    for (ii = 0; ii < 8; ii++) {

                        // Ignore voxels close to the boundary
                        if (ii > 0) {

                            if ((vol[inds[ii]] == SURF) || (vol[inds[ii]] == BOUNDARY)) {
                                skipThisPoint = true;

                                break;
                            }
                        } // if (ii > 0)

                        // Ignore voxels on the boundary (neighbor to an exterior point)
                        // or in the exterior
                        if (vol[inds[ii]] == EXTERIOR) {
                            skipThisPoint = true;

                            break;
                        }
                    } // for (ii = 0; ii < 8; ii++)

                    if (skipThisPoint) {
                        continue;
                    }

                    if (findCriticalPointInIntCell(x, y, z, inds, critpt)) {
                        critPts[numCritPoints][0] = critpt[0];
                        critPts[numCritPoints][1] = critpt[1];
                        critPts[numCritPoints][2] = critpt[2];
                        pointType[numCritPoints] = CPT_UNKNOWN;
                        numCritPoints++;

                        if (numCritPoints >= MAX_NUM_CRITPTS) {
                            MipavUtil.displayError("Too many critical points found");

                            return false;
                        }
                    } // if (findCriticalPointInIntCell(x, y, z, inds, critPt))
                } // for (x = 1; x < xDim-1; x++)
            } // for (y = 1; y < yDim-1; y++)
        } // for (z = 1; z < zDim-1; z++)

        Preferences.debug("Number of critical points is: " + numCritPoints + "\n", Preferences.DEBUG_ALGORITHM);

        // Classify the critical points as: attracting nodes, repelling nodes, or
        // saddles.
        cv = new double[6][3];

        // distance to a neighbor will be 1/CELL_SUBDIVISION_FACTOR
        vdist = 1.0 / CELL_SUBDIVISION_FACTOR;
        jac = new double[3][3];

        // Since critical points are at least 1 voxel inside the bounding box,
        // we will not check that the neighbor coordinates are inside the
        // volume bounds, because they should be.
        for (i = 0; i < numCritPoints; i++) {

            // Interpolate the force vector at 6 neighbors of this point
            cv[0] = interpolation(critPts[i][0] + vdist, critPts[i][1], critPts[i][2]);
            cv[1] = interpolation(critPts[i][0] - vdist, critPts[i][1], critPts[i][2]);
            cv[2] = interpolation(critPts[i][0], critPts[i][1] + vdist, critPts[i][2]);
            cv[3] = interpolation(critPts[i][0], critPts[i][1] - vdist, critPts[i][2]);
            cv[4] = interpolation(critPts[i][0], critPts[i][1], critPts[i][2] + vdist);
            cv[5] = interpolation(critPts[i][0], critPts[i][1], critPts[i][2] - vdist);

            // Construct the Jacobian matrix
            // is central differencing ok ???
            jac[0][0] = (cv[0][0] - cv[1][0]) / (2.0 * vdist);
            jac[0][1] = (cv[2][0] - cv[3][0]) / (2.0 * vdist);
            jac[0][2] = (cv[4][0] - cv[5][0]) / (2.0 * vdist);

            jac[1][0] = (cv[0][1] - cv[1][1]) / (2.0 * vdist);
            jac[1][1] = (cv[2][1] - cv[3][1]) / (2.0 * vdist);
            jac[1][2] = (cv[4][1] - cv[5][1]) / (2.0 * vdist);

            jac[2][0] = (cv[0][2] - cv[1][2]) / (2.0 * vdist);
            jac[2][1] = (cv[2][2] - cv[3][2]) / (2.0 * vdist);
            jac[2][2] = (cv[4][2] - cv[5][2]) / (2.0 * vdist);

            // Find the eigenvalues and eigenvectors of the Jacobian
            // The columns of eigenvec represent the eigenvectors
            Eigenvalue.decompose( jac, eigenvec, realEigen );
            
            // Analyze the eigenvalues
            // If all real parts of the eigenvalues are negative, the point is an attracting node.
            if ((realEigen[0] < 0.0) && (realEigen[1] < 0.0) && (realEigen[2] < 0.0)) {
                pointType[i] = CPT_ATTRACTING_NODE;
            }
            // If all real parts of the eigenvalues are positive, the point is a repelling node.
            else if ((realEigen[0] > 0.0) && (realEigen[1] > 0.0) && (realEigen[2] > 0.0)) {
                pointType[i] = CPT_REPELLING_NODE;
            }
            // else if 2 real parts of the eigenvalues are of one sign and the other
            // has the opposite sign, the point is a saddle point
            else {
                pointType[i] = CPT_SADDLE;
            }

            realEigenvalues[i][0] = realEigen[0];
            realEigenvalues[i][1] = realEigen[1];
            realEigenvalues[i][2] = realEigen[2];

            // Normalize the eigenvectors
            vecLength = Math.sqrt((eigenvec[0][0] * eigenvec[0][0]) + (eigenvec[1][0] * eigenvec[1][0]) +
                                  (eigenvec[2][0] * eigenvec[2][0]));

            if (vecLength == 0.0) {
                vecLength = 1.0;
            }

            eigenvectors[i][0][0] = eigenvec[0][0] / vecLength;
            eigenvectors[i][0][1] = eigenvec[1][0] / vecLength;
            eigenvectors[i][0][2] = eigenvec[2][0] / vecLength;

            vecLength = Math.sqrt((eigenvec[0][1] * eigenvec[0][1]) + (eigenvec[1][1] * eigenvec[1][1]) +
                                  (eigenvec[2][1] * eigenvec[2][1]));

            if (vecLength == 0.0) {
                vecLength = 1.0;
            }

            eigenvectors[i][1][0] = eigenvec[0][1] / vecLength;
            eigenvectors[i][1][1] = eigenvec[1][1] / vecLength;
            eigenvectors[i][1][2] = eigenvec[2][1] / vecLength;

            vecLength = Math.sqrt((eigenvec[0][2] * eigenvec[0][2]) + (eigenvec[1][2] * eigenvec[1][2]) +
                                  (eigenvec[2][2] * eigenvec[2][2]));

            if (vecLength == 0.0) {
                vecLength = 1.0;
            }

            eigenvectors[i][2][0] = eigenvec[0][2] / vecLength;
            eigenvectors[i][2][1] = eigenvec[1][2] / vecLength;
            eigenvectors[i][2][2] = eigenvec[2][2] / vecLength;
        } // for (i = 0; i < numCritPoints; i++)

        return true;
    } // getCriticalPoints

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean getHighDivergencePoints() {

        // Find high divergence points of a vector field
        // --- Input: 1. Normalized 3D vector field
        //
        // divergence = dFx/dx + dFy/dy + dFz/dz
        // Output: highest perHDPoints fraction of divergence point list
        int idx;
        int i, j, k, ii, jj, kk;
        double x, y, z;
        int cntz, cntnz;
        double[] adiv = null;
        int[][] groupsPoints = null;
        int[] groupsNumPoints = null;
        int numGroups = 0;
        boolean closeToSomeGroup = false;
        double[][] newHDPoints = null;

        if (hdPoints != null) {

            for (i = 0; i < hdPoints.length; i++) {
                hdPoints[i] = null;
            }

            hdPoints = null;
        }

        numHDPoints = 0;

        try {
            adiv = new double[MAX_NUM_HDPTS]; // divergence array
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error allocating divergence array");

            return false;
        }

        try {
            hdPoints = new double[MAX_NUM_HDPTS][3];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error allocating hdPoints array");

            return false;
        }

        // Calculate divergence throughout the dataset
        double maxDiv = -999999.99;
        double minDiv = 999999.99;
        double div;

        cntz = 0;
        cntnz = 0;

        double zeroDiv = 0.1;
        double[][] v = new double[6][3];
        double vdist = CELL_SIZE / 2.0;
        double threshold;
        double minVal, tmp;
        int minPos;

        Preferences.debug("Finding high divergence points 1\n", Preferences.DEBUG_ALGORITHM);

        for (k = 1; k < (zDim - 1); k++) {

            for (j = 1; j < (yDim - 1); j++) {

                for (i = 1; i < (xDim - 1); i++) {
                    idx = (k * sliceSize) + (j * xDim) + i;

                    // If this point is EXTERIOR, BOUNDARY, or SURF, skip it.
                    if ((vol[idx] == EXTERIOR) || (vol[idx] == BOUNDARY) || (vol[idx] == SURF)) {
                        continue;
                    }

                    for (kk = 0; kk < SEARCH_GRID; kk++) {
                        z = k + (kk * CELL_SIZE);

                        for (jj = 0; jj < SEARCH_GRID; jj++) {
                            y = j + (jj * CELL_SIZE);

                            for (ii = 0; ii < SEARCH_GRID; ii++) {
                                x = i + (ii * CELL_SIZE);

                                // Interpolate force vectors around the point
                                v[0] = interpolation(x + vdist, y, z);
                                v[1] = interpolation(x - vdist, y, z);
                                v[2] = interpolation(x, y + vdist, z);
                                v[3] = interpolation(x, y - vdist, z);
                                v[4] = interpolation(x, y, z + vdist);
                                v[5] = interpolation(x, y, z - vdist);

                                div = ((v[0][0] - v[1][0]) + (v[2][1] - v[3][1]) + (v[4][2] - v[5][2])) / (2.0 * vdist);

                                if ((div > -zeroDiv) && (div < zeroDiv)) {
                                    cntz++;
                                } else {
                                    cntnz++;
                                }

                                if (div > maxDiv) {
                                    maxDiv = div;
                                }

                                if (div < minDiv) {
                                    minDiv = div;
                                }
                            } // for (ii = 0; ii < SEARCH_GRID; ii++)
                        } // for (jj = 0; jj < SEARCH_GRID; jj++)
                    } // for (kk = 0; kk < SEARCH_GRID; kk++)
                } // for (i = 1; i < xDim-1; i++)
            } // for (j = 1; j < yDim-1; j++)
        } // for (k = 1; k < zDim-1; k++)

        // case 1:
        // take perHDPoints fraction of lowest negative value
        threshold = maxDiv - minDiv;
        threshold = perHDPoints * threshold;
        threshold = minDiv + threshold;

        Preferences.debug("Finding high divergence points 2\n", Preferences.DEBUG_ALGORITHM);

        for (k = 1; k < (zDim - 1); k++) {

            for (j = 1; j < (yDim - 1); j++) {

                for (i = 1; i < (xDim - 1); i++) {
                    idx = (k * sliceSize) + (j * xDim) + i;

                    // If this point is EXTERIOR, BOUNDARY, or SURF, skip it
                    if ((vol[idx] == EXTERIOR) || (vol[idx] == BOUNDARY) || (vol[idx] == SURF)) {
                        continue;
                    }

                    for (kk = 0; kk < SEARCH_GRID; kk++) {
                        z = k + (kk * CELL_SIZE);

                        for (jj = 0; jj < SEARCH_GRID; jj++) {
                            y = j + (jj * CELL_SIZE);

                            for (ii = 0; ii < SEARCH_GRID; ii++) {
                                x = i + (ii * CELL_SIZE);

                                // Interpolate force vectors around the point
                                v[0] = interpolation(x + vdist, y, z);
                                v[1] = interpolation(x - vdist, y, z);
                                v[2] = interpolation(x, y + vdist, z);
                                v[3] = interpolation(x, y - vdist, z);
                                v[4] = interpolation(x, y, z + vdist);
                                v[5] = interpolation(x, y, z - vdist);

                                div = ((v[0][0] - v[1][0]) + (v[2][1] - v[3][1]) + (v[4][2] - v[5][2])) / (2.0 * vdist);

                                if (div <= threshold) {

                                    // Add the point to the HD list
                                    hdPoints[numHDPoints][0] = x;
                                    hdPoints[numHDPoints][1] = y;
                                    hdPoints[numHDPoints][2] = z;

                                    adiv[numHDPoints] = div;
                                    numHDPoints++;

                                    if (numHDPoints >= MAX_NUM_HDPTS) {
                                        MipavUtil.displayError("Too many divergence points detected");
                                        MipavUtil.displayError("Reached maximum number of " + MAX_NUM_HDPTS);

                                        return false;
                                    }
                                } // if (div <= threshold)

                            } // for (ii = 0; ii < SEARCH_GRID; ii++)
                        } // for (jj = 0; jj < SEARCH_GRID; jj++)
                    } // for (kk = 0; kk < SEARCH_GRID; kk++)
                } // for (i = 1; i < xDim-1; i++)
            } // for (j = 1; j < yDim-1; j++)
        } // for (k = 1; k < zDim-1; k++)

        // Sort the points on the divergence value
        for (i = 0; i < numHDPoints; i++) {
            minVal = adiv[i];
            minPos = i;

            for (j = i + 1; j < numHDPoints; j++) {

                if (adiv[j] < minVal) {
                    minVal = adiv[j];
                    minPos = j;
                }
            } // for (j = i+1; j < numHDPoints; j++)

            if (minPos != i) {

                // Exchange points and div values
                tmp = adiv[i];
                adiv[i] = adiv[minPos];
                adiv[minPos] = tmp;

                tmp = hdPoints[i][0];
                hdPoints[i][0] = hdPoints[minPos][0];
                hdPoints[minPos][0] = tmp;

                tmp = hdPoints[i][1];
                hdPoints[i][1] = hdPoints[minPos][1];
                hdPoints[minPos][1] = tmp;

                tmp = hdPoints[i][2];
                hdPoints[i][2] = hdPoints[minPos][2];
                hdPoints[minPos][2] = tmp;
            } // if (minPos != i)
        } // for (i = 0; i < numHDPoints; i++)

        // Cluster the points

        // Algorithm:
        // First point creates the first group
        // For all the other points:
        // If the point is close to an existing group
        // add the point to that group
        // else
        // the point starts a new group
        // endif
        // endfor
        // end

        // Initialize data structure
        try {
            groupsPoints = new int[numHDPoints][numHDPoints];
            groupsNumPoints = new int[numHDPoints];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory error allocating groups structure");

            return false;
        }

        closeToSomeGroup = false;

        // First point creates the first group
        groupsPoints[0][0] = 0;
        groupsNumPoints[0] = 1;
        numGroups = 1;

        for (i = 1; i < numHDPoints; i++) {
            closeToSomeGroup = false;

            for (j = 0; j < numGroups; j++) {

                if (pointIsCloseToGroup(i, j, groupsPoints, groupsNumPoints)) {

                    // Add the point to that group
                    groupsPoints[j][groupsNumPoints[j]] = i;
                    groupsNumPoints[j] = groupsNumPoints[j] + 1;
                    closeToSomeGroup = true;

                    break;
                } // if (pointIsCloseToGroup(i, j, groupsPoints, groupsNumPoints))
            } // for (j = 0; j < numGroups; j++)

            if (!closeToSomeGroup) {

                // Start a new group
                groupsPoints[numGroups][0] = i;
                groupsNumPoints[numGroups] = 1;
                numGroups++;
            } // if (!closeToSomeGroup)
        } // for (i = 1; i < numHDPoints; i++)

        // Return only the first point in each group as the high divergence points
        try {
            newHDPoints = new double[numGroups][3];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory error allocating newHDPoints");

            return false;
        }

        for (i = 0; i < numGroups; i++) {
            newHDPoints[i][0] = hdPoints[groupsPoints[i][0]][0];
            newHDPoints[i][1] = hdPoints[groupsPoints[i][0]][1];
            newHDPoints[i][2] = hdPoints[groupsPoints[i][0]][2];
        } // for (i = 0; i < numGroups; i++)

        // delete the old array
        for (i = 0; i < hdPoints.length; i++) {
            hdPoints[i] = null;
        }

        hdPoints = null;

        try {
            hdPoints = new double[numGroups][3];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory error allocating hdPoints");

            return false;
        }

        for (i = 0; i < numGroups; i++) {

            for (j = 0; j < 3; j++) {
                hdPoints[i][j] = newHDPoints[i][j];
            }
        }

        numHDPoints = numGroups;

        return true;
    } // getHighDivergencePoints

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean getLevel1Skeleton() {

        // Get basic skeleton (level 1): connects only critical points
        int i, j;
        double[] whereTo = new double[3];

        // critPtInSkeleton indicates if a critical point is already part of the
        // skeleton (is in the skelPoints array).  If -1, then the point is not yet
        // in the skelPoints array.  Otherwise, it indicates the point's position in
        // the skeleton.
        int[] critPtInSkeleton = null;

        if (numCritPoints < 1) {
            return true;
        }

        try {
            critPtInSkeleton = new int[numCritPoints];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error allocating memory for critPtInSkeleton");

            return false;
        }

        // Initialize to -1
        for (i = 0; i < numCritPoints; i++) {
            critPtInSkeleton[i] = -1;
        }

        // Follow the streamlines starting at saddles in the direction of the positive
        // eigenvector(s) until we are close to one of the skeleton points or a critical
        // point, ignoring points in the current segment
        for (i = 0; i < numCritPoints; i++) {

            // Start to follow force field at saddle points only
            if (pointType[i] != CPT_SADDLE) {
                continue;
            }

            // The point is a saddle, so we should go in the direction pointed by
            // the positive eigenvectors
            for (j = 0; j < 3; j++) {

                if (realEigenvalues[i][j] > 0.0) {

                    // direction given by the eigenvector
                    whereTo[0] = eigenvectors[i][j][0];
                    whereTo[1] = eigenvectors[i][j][1];
                    whereTo[2] = eigenvectors[i][j][2];

                    if (!followStreamLines(i, true, critPts[i], whereTo, critPtInSkeleton)) {
                        return false;
                    }

                    // the exact opposite of the eigenvector
                    whereTo[0] = -eigenvectors[i][j][0];
                    whereTo[1] = -eigenvectors[i][j][1];
                    whereTo[2] = -eigenvectors[i][j][2];

                    if (!followStreamLines(i, true, critPts[i], whereTo, critPtInSkeleton)) {
                        return false;
                    }

                } // if (realEigenvalues[i][j] > 0.0)
            } // for (j = 0; j < 3; j++)
        } // for (i = 0; i < numCritPoints; i++)

        return true;
    } // getLevel1Skeleton

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean getLevel2Skeleton() {
        int i;
        // Adds segments starting at interior high divergence points

        if (critPts != null) {

            for (i = 0; i < critPts.length; i++) {
                critPts[i] = null;
            }

            critPts = null;
        }

        numCritPoints = 0;

        // Follow the streamlines starting at each high divergence point
        // until we are close to one of the already existing skeleton points
        // ignoring the points in the current segment
        if (hdPoints != null) {

            for (i = 0; i < numHDPoints; i++) {

                if (!followStreamLines(-1, false, hdPoints[i], null, null)) {
                    return false;
                }
            } // for (i = 0; i < numHDPoints; i++)
        } // if (hdPoints != null)

        return true;
    } // getLevel2Skeleton

    /**
     * DOCUMENT ME!
     *
     * @param   point     DOCUMENT ME!
     * @param   endPoint  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int getSkelSegment(int point, boolean[] endPoint) {
        int i;

        for (i = 0; i < skelNumSegments; i++) {

            if ((point == skelSegments[i][SKEL_SEG_LEFT]) || (point == skelSegments[i][SKEL_SEG_RIGHT])) {
                endPoint[0] = true;

                return i;
            }

            if ((point >= skelSegments[i][SKEL_SEG_FIRST]) && (point <= skelSegments[i][SKEL_SEG_LAST])) {
                endPoint[0] = false;

                return i;
            }
        } // (for i = 0; i < skelNumSegments; i++)

        MipavUtil.displayError("Could not find point in getSkelSegment");

        return -2;
    } // getSkelSegment

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     * @param   z  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] interpolation(double x, double y, double z) {
        double[] forceInterp = new double[3];
        double alpha, beta, gamma;
        int ix, iy, iz;

        ix = (int) x;
        iy = (int) y;
        iz = (int) z;

        alpha = x - ix;
        beta = y - iy;
        gamma = z - iz;

        forceInterp[0] = (forceX[(iz * sliceSize) + (iy * xDim) + ix] * (1.0 - alpha) * (1.0 - beta) * (1.0 - gamma)) +
                         (forceX[((iz + 1) * sliceSize) + (iy * xDim) + ix] * (1.0 - alpha) * (1.0 - beta) * gamma) +
                         (forceX[(iz * sliceSize) + ((iy + 1) * xDim) + ix] * (1.0 - alpha) * beta * (1.0 - gamma)) +
                         (forceX[(iz * sliceSize) + (iy * xDim) + ix + 1] * alpha * (1.0 - beta) * (1.0 - gamma)) +
                         (forceX[((iz + 1) * sliceSize) + (iy * xDim) + ix + 1] * alpha * (1.0 - beta) * gamma) +
                         (forceX[(iz * sliceSize) + ((iy + 1) * xDim) + ix + 1] * alpha * beta * (1.0 - gamma)) +
                         (forceX[((iz + 1) * sliceSize) + ((iy + 1) * xDim) + ix] * (1.0 - alpha) * beta * gamma) +
                         (forceX[((iz + 1) * sliceSize) + ((iy + 1) * xDim) + ix + 1] * alpha * beta * gamma);

        forceInterp[1] = (forceY[(iz * sliceSize) + (iy * xDim) + ix] * (1.0 - alpha) * (1.0 - beta) * (1.0 - gamma)) +
                         (forceY[((iz + 1) * sliceSize) + (iy * xDim) + ix] * (1.0 - alpha) * (1.0 - beta) * gamma) +
                         (forceY[(iz * sliceSize) + ((iy + 1) * xDim) + ix] * (1.0 - alpha) * beta * (1.0 - gamma)) +
                         (forceY[(iz * sliceSize) + (iy * xDim) + ix + 1] * alpha * (1.0 - beta) * (1.0 - gamma)) +
                         (forceY[((iz + 1) * sliceSize) + (iy * xDim) + ix + 1] * alpha * (1.0 - beta) * gamma) +
                         (forceY[(iz * sliceSize) + ((iy + 1) * xDim) + ix + 1] * alpha * beta * (1.0 - gamma)) +
                         (forceY[((iz + 1) * sliceSize) + ((iy + 1) * xDim) + ix] * (1.0 - alpha) * beta * gamma) +
                         (forceY[((iz + 1) * sliceSize) + ((iy + 1) * xDim) + ix + 1] * alpha * beta * gamma);

        forceInterp[2] = (forceZ[(iz * sliceSize) + (iy * xDim) + ix] * (1.0 - alpha) * (1.0 - beta) * (1.0 - gamma)) +
                         (forceZ[((iz + 1) * sliceSize) + (iy * xDim) + ix] * (1.0 - alpha) * (1.0 - beta) * gamma) +
                         (forceZ[(iz * sliceSize) + ((iy + 1) * xDim) + ix] * (1.0 - alpha) * beta * (1.0 - gamma)) +
                         (forceZ[(iz * sliceSize) + (iy * xDim) + ix + 1] * alpha * (1.0 - beta) * (1.0 - gamma)) +
                         (forceZ[((iz + 1) * sliceSize) + (iy * xDim) + ix + 1] * alpha * (1.0 - beta) * gamma) +
                         (forceZ[(iz * sliceSize) + ((iy + 1) * xDim) + ix + 1] * alpha * beta * (1.0 - gamma)) +
                         (forceZ[((iz + 1) * sliceSize) + ((iy + 1) * xDim) + ix] * (1.0 - alpha) * beta * gamma) +
                         (forceZ[((iz + 1) * sliceSize) + ((iy + 1) * xDim) + ix + 1] * alpha * beta * gamma);

        return forceInterp;
    } // interpolation

    /**
     * DOCUMENT ME!
     */
    private void makeSolidVolume() {

        // Makes the 3D dataset a solid volume
        // Fills all the holes in the volume if there are any
        // Assigns all object voxels the value OBJECT_VAL and all outside voxels
        // 1 of 3 values
        int i;
        vol = new byte[totLength];

        // Replace every object voxel with OBJECT_VAL
        // Everything else remains zero
        for (i = 0; i < totLength; i++) {

            if (volFloat[i] >= minObjectValue) {
                vol[i] = OBJECT_VAL;
            }
        }

        volFloat = null;
        Preferences.debug("Floodfill z planes\n", Preferences.DEBUG_ALGORITHM);

        // Floodfill the outside of the object with OUTSIDE_1 in the z direction
        for (i = 0; i < zDim; i++) {
            floodFillZPlane(i);
        }

        Preferences.debug("Floodfill y planes\n", Preferences.DEBUG_ALGORITHM);

        // Floodfill the outside of the object with OUTSIDE_2 in the y direction
        for (i = 0; i < yDim; i++) {
            floodFillYPlane(i);
        }

        Preferences.debug("floodfill x planes\n", Preferences.DEBUG_ALGORITHM);

        // Floodfill the outside of the object with OUTSIDE_3 in the x direction
        for (i = 0; i < xDim; i++) {
            floodFillXPlane(i);
        }

        // Replace any voxel that's not OUTSIDE_1, OUTSIDE_2, or OUTSIDE_3 with
        // INTERIOR and every OUTSIDE_1, OUTSIDE_2, or OUTSIDE_3 with EXTERIOR
        for (i = 0; i < totLength; i++) {

            if ((vol[i] == OUTSIDE_1) || (vol[i] == OUTSIDE_2) || (vol[i] == OUTSIDE_3)) {
                vol[i] = EXTERIOR;
            } else {
                vol[i] = INTERIOR;
            }
        } // for (i = 0; i < totLength; i++)

        return;
    } // makeSolidVolume

    /**
     * DOCUMENT ME!
     */
    private void peelVolume() {
        int i;
        // Removes the first layer of voxels from the volume
        // It may disconnect the volume
        // Mark all surface voxels as SURF and then remove them

        // Mark all surface voxels as SURF
        flagVolume();

        // Remove all the surface voxels
        for (i = 0; i < totLength; i++) {

            if (vol[i] == SURF) {
                vol[i] = EXTERIOR;
            }
        }

        return;
    } // peelVolume

    /**
     * DOCUMENT ME!
     *
     * @param   pt               DOCUMENT ME!
     * @param   grp              DOCUMENT ME!
     * @param   groupsPoints     DOCUMENT ME!
     * @param   groupsNumPoints  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean pointIsCloseToGroup(int pt, int grp, int[][] groupsPoints, int[] groupsNumPoints) {
        int i;

        for (i = 0; i < groupsNumPoints[grp]; i++) {

            if ((Math.abs(hdPoints[pt][0] - hdPoints[groupsPoints[grp][i]][0]) <= 1) &&
                    (Math.abs(hdPoints[pt][1] - hdPoints[groupsPoints[grp][i]][1]) <= 1) &&
                    (Math.abs(hdPoints[pt][2] - hdPoints[groupsPoints[grp][i]][2]) <= 1)) {
                return true;
            }
        }

        return false;
    } // pointIsCloseToGroup

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean quickCheckVolumePadding() {
        int x, y, z;
        int idx;
        z = 0;

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                idx = (z * sliceSize) + (y * xDim) + x;

                if (vol[idx] != EXTERIOR) {
                    return false;
                }
            }
        }

        z = zDim - 1;

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                idx = (z * sliceSize) + (y * xDim) + x;

                if (vol[idx] != EXTERIOR) {
                    return false;
                }
            }
        }

        y = 0;

        for (z = 0; z < zDim; z++) {

            for (x = 0; x < xDim; x++) {
                idx = (z * sliceSize) + (y * xDim) + x;

                if (vol[idx] != EXTERIOR) {
                    return false;
                }
            }
        }

        y = yDim - 1;

        for (z = 0; z < zDim; z++) {

            for (x = 0; x < xDim; x++) {
                idx = (z * sliceSize) + (y * xDim) + x;

                if (vol[idx] != EXTERIOR) {
                    return false;
                }
            }
        }

        x = 0;

        for (z = 0; z < zDim; z++) {

            for (y = 0; y < yDim; y++) {
                idx = (z * sliceSize) + (y * xDim) + x;

                if (vol[idx] != EXTERIOR) {
                    return false;
                }
            }
        }

        x = xDim - 1;

        for (z = 0; z < zDim; z++) {

            for (y = 0; y < yDim; y++) {
                idx = (z * sliceSize) + (y * xDim) + x;

                if (vol[idx] != EXTERIOR) {
                    return false;
                }
            }
        }

        return true;
    } // quickCheckVolumePadding

    /**
     * DOCUMENT ME!
     *
     * @param  x        DOCUMENT ME!
     * @param  y        DOCUMENT ME!
     * @param  z        DOCUMENT ME!
     * @param  steps    DOCUMENT ME!
     * @param  nextPos  DOCUMENT ME!
     */
    private void rk2(double x, double y, double z, double steps, double[] nextPos) {
        double[] outForce;
        double len;

        outForce = interpolation(x, y, z);

        // normalize
        len = Math.sqrt((outForce[0] * outForce[0]) + (outForce[1] * outForce[1]) + (outForce[2] * outForce[2]));

        if (len > 0.0) {
            outForce[0] = outForce[0] / len;
            outForce[1] = outForce[1] / len;
            outForce[2] = outForce[2] / len;
        }

        nextPos[0] = x + (outForce[0] * steps);
        nextPos[1] = y + (outForce[1] * steps);
        nextPos[2] = z + (outForce[2] * steps);

        return;
    } // rk2

    /**
     * DOCUMENT ME!
     *
     * @param   crtSegment  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean segmentIsLongEnough(int crtSegment) {

        // A segment is long enough if it contains minimum MIN_SEG_LENGTH points
        // between its left and right end points
        if (Math.abs(skelSegments[crtSegment][SKEL_SEG_LAST] - skelSegments[crtSegment][SKEL_SEG_FIRST]) <
                MIN_SEG_LENGTH) {
            return false;
        }

        return true;
    } // segmentIsLongEnough

    /**
     * DOCUMENT ME!
     *
     * @param  numBound  DOCUMENT ME!
     * @param  bound     DOCUMENT ME!
     */
    private void sortBoundaryArray(int numBound, short[][] bound) {

        // Sort the boundary array so that we can speed up the potential field calculation:
        // zyx in that order
        // selection sort
        int st, i;
        short zvst, yvst;

        // sort by Z
        sortByZ(0, numBound - 1, bound);

        // then by Y
        st = 0;
        zvst = bound[st][2];

        for (i = 0; i < numBound; i++) {

            if (bound[i][2] != zvst) {
                sortByY(st, i - 1, bound);

                st = i;
                zvst = bound[st][2];
            } // if (bound[i][2] != zvst)
        } // for (i = 0; i < numBound; i++)

        sortByY(st, numBound - 1, bound);

        // then by X
        st = 0;
        zvst = bound[st][2];
        yvst = bound[st][1];

        for (i = 0; i < numBound; i++) {

            if ((bound[i][1] != yvst) || (bound[i][2] != zvst)) {
                sortByX(st, i - 1, bound);

                st = i;
                zvst = bound[st][2];
                yvst = bound[st][1];
            } // if ((bound[i][1] != yvst) || (bound[i][2] != zvst))
        } // for (i = 0; i < numBound; i++)

        sortByX(st, numBound - 1, bound);

        return;
    } // sortBoundaryArray

    /**
     * DOCUMENT ME!
     *
     * @param  startAt  DOCUMENT ME!
     * @param  endAt    DOCUMENT ME!
     * @param  bound    DOCUMENT ME!
     */
    private void sortByX(int startAt, int endAt, short[][] bound) {
        int i, j, minIndex, crtMin;
        short tmp;

        for (i = startAt; i <= endAt; i++) {
            minIndex = -1;
            crtMin = bound[i][0];

            for (j = i + 1; j <= endAt; j++) {

                if (bound[j][0] < crtMin) {
                    minIndex = j;
                    crtMin = bound[j][0];
                } // if (bound[j][0] < crtMin)
            } // for (j = i+1; j <= endAt; j++)

            if (minIndex != -1) {

                // swap values
                tmp = bound[i][0];
                bound[i][0] = bound[minIndex][0];
                bound[minIndex][0] = tmp;

                tmp = bound[i][1];
                bound[i][1] = bound[minIndex][1];
                bound[minIndex][1] = tmp;

                tmp = bound[i][2];
                bound[i][2] = bound[minIndex][2];
                bound[minIndex][2] = tmp;
            } // if (minIndex != -1)
        } // for (i = startAt; i <= endAt; i++)

        return;
    } // sortByX

    /**
     * DOCUMENT ME!
     *
     * @param  startAt  DOCUMENT ME!
     * @param  endAt    DOCUMENT ME!
     * @param  bound    DOCUMENT ME!
     */
    private void sortByY(int startAt, int endAt, short[][] bound) {
        int i, j, minIndex, crtMin;
        short tmp;

        for (i = startAt; i <= endAt; i++) {
            minIndex = -1;
            crtMin = bound[i][1];

            for (j = i + 1; j <= endAt; j++) {

                if (bound[j][1] < crtMin) {
                    minIndex = j;
                    crtMin = bound[j][1];
                } // if (bound[j][1] < crtMin)
            } // for (j = i+1; j <= endAt; j++)

            if (minIndex != -1) {

                // swap values
                tmp = bound[i][0];
                bound[i][0] = bound[minIndex][0];
                bound[minIndex][0] = tmp;

                tmp = bound[i][1];
                bound[i][1] = bound[minIndex][1];
                bound[minIndex][1] = tmp;

                tmp = bound[i][2];
                bound[i][2] = bound[minIndex][2];
                bound[minIndex][2] = tmp;
            } // if (minIndex != -1)
        } // for (i = startAt; i <= endAt; i++)

        return;
    } // sortByY

    /**
     * DOCUMENT ME!
     *
     * @param  startAt  DOCUMENT ME!
     * @param  endAt    DOCUMENT ME!
     * @param  bound    DOCUMENT ME!
     */
    private void sortByZ(int startAt, int endAt, short[][] bound) {
        int i, j, minIndex, crtMin;
        short tmp;

        for (i = startAt; i <= endAt; i++) {
            minIndex = -1;
            crtMin = bound[i][2];

            for (j = i + 1; j <= endAt; j++) {

                if (bound[j][2] < crtMin) {
                    minIndex = j;
                    crtMin = bound[j][2];
                }
            } // for (j = i+1; j <= endAt; j++)

            if (minIndex != -1) {

                // swap values
                tmp = bound[i][0];
                bound[i][0] = bound[minIndex][0];
                bound[minIndex][0] = tmp;

                tmp = bound[i][1];
                bound[i][1] = bound[minIndex][1];
                bound[minIndex][1] = tmp;

                tmp = bound[i][2];
                bound[i][2] = bound[minIndex][2];
                bound[minIndex][2] = tmp;
            } // if (minIndex != -1)
        } // for (i = startAt; i <= endAt; i++)

        return;
    } // sortByZ
}
