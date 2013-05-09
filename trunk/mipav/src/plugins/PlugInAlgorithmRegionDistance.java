import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;

import javax.swing.*;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;


/**
 * This shows how to extend the AlgorithmBase class.
 *
 * @version  October 19, 2007
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmRegionDistance.java $ $Revision: 72 $ $Date: 2/06/06 5:50p $
 *           PlugInAlgorithmRegionDistance is used to find the distances between geometric cell centers and cell
 *           boundaries, geometric cell centers and voi geometric centers, geometric cell centers and voi centers of
 *           mass, voi geometric centers and cell boundaries, and voi centers of mass and cell boundaries. The geometric
 *           center is a zero order moment not using intensity multiplications. The center of mass is a first order
 *           moment mutliplying position by intensity. The cells are blue and the chromosome regions of interest
 *           outlined by the vois are predominantly red or green. Images can be 2D or 3D. VOIs can be supplied by the
 *           user, but if no VOI is supplied by the user the program will automatically generate red VOIs and green VOIs
 *           in each cell. The user can input the number of red and green VOIs to be looked for in each cell. The
 *           default is 2 red VOIs and 2 green VOIs.</p>
 *
 *           <p>1.) Only the blue portion of the red, green, blue color image is used to create the blueImage.</p>
 *
 *           <p>2.) Fuzzy C means segmentation is performed on the blueImage to create a blueSegImage, in which all of
 *           the blueImage pixel intensities are assigned to 1 of 2 classes. The lower intensity value pixels
 *           corresponding to the background in blueImage are all given the intensity value 127 in blueSegImage and the
 *           higher intensity value pixels in blueImage corresponding to the nucleus are all given the intensity value
 *           254 in blueSegImage. In fuzzy c means segmentation pixels less than the threshold are excluded from the
 *           classification process and assigned the intensity value of 0 in the segmentation image. Here, threshold is
 *           set to 0.0, so no pixels are less than threshold, so all the pixels in blueImage are assigned to either 127
 *           or 254 in blueSegImage. Why 127 and 254? The fuzzy c means program assigns values of 255/(number of
 *           classes) to the classes in the segmented image. With 2 classes, the resulting values are 127 and 254.</p>
 *
 *           <p>3.) AlgorithmThresholdDual is used to set all the pixels with intensity = 254 to pixels with intensity =
 *           1 and all the pixels with intensity = 127 to pixels with intensity = 0.</p>
 *
 *           <p>4.) A slice by slice hole filling is performed on the blue segmented image.</p>
 *
 *           <p>5.) blueSegImage is smoothed with a morphological opening followed by a morphological closing. An
 *           opening consists of an erosion operation followed by a dilation operation. A closing consists of a dilation
 *           operation followed by an erosion operation.</p>
 *
 *           <p>6.) Erode blueSegImage with iters erosion iterations so as to be able to ID separate cells which are
 *           touching. iters is a user specified variable with a default of 8 for 2D and 6 for 3D.</p>
 *
 *           <p>7.) Assign positive integer IDs to each object between blueMin and 200,000 pixels in size in the 2D
 *           program or between blueMin and 2,000,000 pixels in size in the 3D program. blueSegImage now has 0 for
 *           background and a positive integer ID for each object between the minimum and maximum size. blueMin has a
 *           default of 1,000.</p>
 *
 *           <p>8.) Dilate blueSegImage with 6*iters dilation operations so as to reverse the previous erosion. iters is
 *           a user specified variable with a default of 8 for 2D and 6 for 3D.</p>
 *
 *           <p>9.) Zero all portions of the dilated objects that are zero in the blue smoothed image from the end of
 *           step 5.</p>
 *
 *           <p>10.) Set all the pixels in any blue object touching an edge to 0.</p>
 *
 *           <p>11.) Export the red portion of the image to buffer and the green portion of the image to greenBuffer.
 *           </p>
 *
 *           <p>12.) If no user supplied contour VOIs are present, the program automatically generates VOIs.</p>
 *
 *           <p>13.) Process each VOI one at a time. a.) For each VOI find the sum of the red intensity values, the sum
 *           of the green intensity values, and the pixel count for each ID object. b.) Look at all the object pixel
 *           counts and assign the ID of the object with the most pixels in the VOI to the VOI's objectID. c.) Find the
 *           center of the VOI. If redCount >= greenCount, find the red center, otherwise find the green center. Both
 *           the zero order geometric centers and the first order centers of mass are found. d.) Expand the nucleus to
 *           which the VOI belongs to include the VOI.</p>
 *
 *           <p>14.) Process IDArray objects one at a time. a.) Ellipse fit blue IDArray objects with first and second
 *           moments to calculate the length of the 3 semiaxes. b.) Find the zero order moments of each blue IDArray
 *           object. c.) Create a byteBuffer with pixels for that ID object equal to 1 and all the other pixels = 0. d.)
 *           Import byteBuffer into grayImage. e.) Run a dilation on grayImage and export the result to dilateArray. e.)
 *           At every point where dilateArray = 1 and byteBuffer = 0, set boundaryArray = to the id of that object. For
 *           every point on the boundary find the square of the distance to the center of the object and if this square
 *           distance is the lowest yet noted, put the value in lowestSquare. If this square distance is the highest yet
 *           noted, put the value in highestSquare. f.) After examining all points in the image, the program records the
 *           Rmin distance from the center to edge as the square root of lowestSquare and the Rmax distance from the
 *           center to the edge as the square root of highestSquare. g.) Put points on the boundary of each blue ID
 *           object into a second ellipsoid fitting program that uses the peripheral points to find the 3 semiaxes. h.)
 *           Nuclei volumes are found from the blue count multiplied by the x, y, and z resolutions. 15.) Determine
 *           nucleus-VOI distances a.) Determine the distance from the center of the ID object to the zero order center
 *           of the VOI. Find the distance of the line segment that passes thru these 2 points and terminates at the
 *           cell surface. b.) Find the distance from the center of the ID object to the first order center of the VOI.
 *           Find the distance of the line segment that passes thru these 2 points and terminates at the cell surface.
 *           </p>
 *
 *           <p>Scheme for automatic VOI generation: 1.) Create a red image. 2.) Median filter the red image. 3.) Obtain
 *           histogram information on the red image. Set the threshold so that the redFraction portion of the cumulative
 *           histogram is at or above threshold for the fuzzy c means. 4.) Perform a 3 level fuzzy c means segmentation
 *           on the red image. 5.) Convert the red max to 1 and the other values to 0. 6.) Remove holes from red VOIs.
 *           7.) Smooth red with a morphological opening followed by a closing. 8.) ID objects in red segmented image
 *           which have at least redMin pixels. Split redIDArray objects apart into 2 or 3 redIDArray objects if they go
 *           between 2 or 3 cells. 9.) Create a green image. 10.) Median filter the green image. 11.) Obtain histogram
 *           information on the green image. Set the threshold so that the greenFraction portion of the cumulative
 *           histogram is at or above threshold for the fuzzy c means. 12.) Perform a 3 level fuzzy c means segmentation
 *           on the green image. 13.) Convert the green max to 1 and the other values to 0. 15.) Remove holes from green
 *           VOIs. 15.) Smooth green with a morphological opening followed by a closing. 16.) ID objects in green
 *           segmented image which have at least greenMin pixels. Split greenIDArray objects apart into 2 or 3
 *           greenIDArray objects if they go between 2 or 3 cells. 17.) Sort red objects by red intensity count into a
 *           sorted red array. Have a separate sorted array for each nucleus. Put no more than redNumber red objects
 *           into the sorted red array for each nucleus. 18.) Sort green objects by green intensity count into a sorted
 *           green array. Have a separate sorted green array for each nucleus. Put no more than greenNumber green
 *           objects into the sorted green array for each nucleus. 19.) Create byteBuffer with value = 0 if no sorted
 *           object is present at that position and use a separate positive index for each red or green voi. Create an
 *           accompanying color table to set for each nucleus the largest red voi to color yellow, the other red vois to
 *           a color yellow.darker(), the largest green voi to color pink, and the other green vois to color
 *           pink.darker(). Create an accompanying name table to set the name for each voi. The largest red voi in the
 *           fifth nucleus has name 5R1. The second largest red voi in the fifth nucleus has the name 5R2. The third
 *           largest green voi in the fourth nucleus has the name 4G3. 20.) Extract VOIs from the red_green image. 21.)
 *           Set the source image VOIs to the VOIs obtained from the red_green image.</p>
 */
public class PlugInAlgorithmRegionDistance extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int blueMin = 1000;

    /** Portion of green pixels above threshold in fuzzy c means. */
    private float greenFraction = 0.15f;

    /** DOCUMENT ME! */
    private int greenMin = 100;

    /** DOCUMENT ME! */
    private int greenNumber = 2;

    /** Iterations used in erosion before IDIng = iters Iterations used in dilation after IDing = 6*iters. */
    private int iters = 6;

    /** Portion of red pixels above threshold in fuzzy c means. */
    private float redFraction = 0.25f;

    /** DOCUMENT ME! */
    private int redMin = 100;

    /** DOCUMENT ME! */
    private int redNumber = 2;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg         Source image model.
     * @param  redMin         DOCUMENT ME!
     * @param  redFraction    DOCUMENT ME!
     * @param  redNumber      DOCUMENT ME!
     * @param  greenMin       DOCUMENT ME!
     * @param  greenFraction  DOCUMENT ME!
     * @param  greenNumber    DOCUMENT ME!
     * @param  blueMin        DOCUMENT ME!
     * @param  iters          DOCUMENT ME!
     */
    public PlugInAlgorithmRegionDistance(ModelImage srcImg, int redMin, float redFraction, int redNumber, int greenMin,
                                         float greenFraction, int greenNumber, int blueMin, int iters) {
        super(null, srcImg);
        this.redMin = redMin;
        this.redFraction = redFraction;
        this.redNumber = redNumber;
        this.greenMin = greenMin;
        this.greenFraction = greenFraction;
        this.greenNumber = greenNumber;
        this.blueMin = blueMin;
        this.iters = iters;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() > 2) {
            calc3D();
        }
    } // end runAlgorithm()

    /**
     * DOCUMENT ME!
     */
    private void calc2D() {

        int length; // total number of data-elements (pixels) in image
        float[] buffer;
        AlgorithmFuzzyCMeans fcmAlgo;
        ModelImage grayImage;
        int nClasses;
        int nPyramid;
        int oneJacobiIter;
        int twoJacobiIter;
        float q;
        float oneSmooth;
        float twoSmooth;
        boolean outputGainField;
        int segmentation;
        boolean cropBackground;
        float threshold;
        int maxIter;
        float endTolerance;
        boolean wholeImage;
        float[] centroids;
        float min;
        float max;
        AlgorithmThresholdDual thresholdAlgo;
        float[] thresholds;
        float fillValue;
        AlgorithmMorphology2D openAlgo;
        AlgorithmMorphology2D closeAlgo;
        AlgorithmMorphology2D erosionAlgo;
        AlgorithmMorphology2D dilationAlgo;
        int kernel;
        float circleDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        int i, j;
        int x, y;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
        boolean found;
        AlgorithmMorphology2D idObjectsAlgo2D;
        int numObjects;
        byte[] byteBuffer;
        byte[] IDArray;
        int id;
        byte[] dilateArray;
        byte[] boundaryArray;
        AlgorithmMorphology2D dilateAlgo;
        float[] xCenter;
        float[] yCenter;
        float[] centerToNearEdge;
        float[] centerToFarEdge;
        float[] greenBuffer = null;
        ViewVOIVector VOIs = null;
        ViewVOIVector VOIs2 = null;
        int nVOIs;
        short[] shortMask;
        int index;
        int redCount;
        int greenCount;
        int[] idCount;
        int[] objectID;
        int[] objectID2;
        int objectCount;
        float[] xPosGeo;
        float[] yPosGeo;
        float[] xPosGeo2;
        float[] yPosGeo2;
        float[] xPosGrav;
        float[] yPosGrav;
        float[] xPosGrav2;
        float[] yPosGrav2;
        float colorCount[];
        float geoToCenter;
        float gravToCenter;
        float geoToEdge;
        float gravToEdge;
        ViewUserInterface UI = ViewUserInterface.getReference();
        float distSqr;
        float lowestSqr;
        float highestSqr;

        // int xEdge = -1;
        // int yEdge = -1;
        int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        FileInfoBase fileInfo;
        int numRedObjects = 0;
        int numGreenObjects = 0;
        byte[] redIDArray = null;
        byte[] greenIDArray = null;
        float[] redIntensityTotal;
        float[] greenIntensityTotal;
        float[] redXCenter;
        float[] redYCenter;
        float[] greenXCenter;
        float[] greenYCenter;
        int[] redNucleusNumber = null;
        int[][] sortedRedIndex = null;
        float[][] sortedRedIntensity;
        float[][] sortedRedXCenter;
        float[][] sortedRedYCenter;
        int[] greenNucleusNumber = null;
        int[][] sortedGreenIndex = null;
        float[][] sortedGreenIntensity;
        float[][] sortedGreenXCenter;
        float[][] sortedGreenYCenter;
        int k;
        AlgorithmVOIExtraction algoVOIExtraction;
        VOI newPtVOI;
        float[] xArr = new float[1];
        float[] yArr = new float[1];
        float[] zArr = new float[1];
        int numVOIObjects;
        Color[] colorTable = null;
        boolean removeID;
        int m;
        AlgorithmMedian algoMedian;
        int medianIters;
        int kernelSize;
        int kernelShape;
        float stdDev;
        String[] nameTable = null;
        String voiName;
        float[] blueCountTotal = null;
        AlgorithmMorphology2D fillHolesAlgo2D;
        int[] voiCount;
        int[] voiCount2;

        // int j1;
        int redFound;
        int greenFound;
        char voiType;
        float nuclearArea;

        // float equivalentRadius;
        float voiArea;
        NumberFormat nf;
        float initialdx;
        float initialdy;
        int lastEdgeX;
        int lastEdgeY;
        int newEdgeX;
        int newEdgeY;
        float incx;
        float incy;
        int numInc;
        float centerToGeoToEdge;
        float centerToGravToEdge;
        int moreRedObjects;
        int moreGreenObjects;
        int[] redObjectNucleus;
        int[] greenObjectNucleus;
        int maxNucleus;
        int maxCount;
        int secondMaxNucleus;
        int secondMaxCount;
        int thirdMaxNucleus;
        int thirdMaxCount;
        int numRedVOIObjects;
        int numGreenVOIObjects;
        int edgeObjects;
        AlgorithmHistogram algoHist;
        int bins;
        int[] histoBuffer;
        double[] lowValue;
        int totalCount;
        int countsToRetain;
        int countsFound;
        boolean adaptiveSize = false;
        int maximumSize = 0;
        boolean initiallyOneObject;
        int response;
        boolean isRed[];
        int numRedFound;
        int numGreenFound;
        int rIndex[];
        float rCount[];
        int gIndex[];
        float gCount[];
        boolean placed;
        int index2;


        nf = NumberFormat.getNumberInstance();
        nf.setMinimumFractionDigits(3);
        nf.setMaximumFractionDigits(3);

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        
        for (i = nVOIs - 1; i >=0; i--) {

            if (VOIs.VOIAt(i).getCurveType() != VOI.CONTOUR) {
                VOIs.remove(i);
            }
        }
        
        nVOIs = VOIs.size();

        if (nVOIs > 0) {
            // Increase redNumber and greenNumber based on what is found in supplied VOIs
            redNumber = 1;
            greenNumber = 1;
        }

        try {

            // image length is length in 2 dims
            length = xDim * yDim;
            buffer = new float[length];
            srcImage.exportRGBData(3, 0, length, buffer); // export blue data
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm RegionDistance reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm RegionDistance reports: out of memory", true);

            return;
        }

        fireProgressStateChanged("Processing image ...");


        fireProgressStateChanged("Creating blue image");
        grayImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_gray");
        fileInfo = grayImage.getFileInfo()[0];
        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        grayImage.setFileInfo(fileInfo, 0);

        try {
            grayImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }

        // Segment into 2 values
        fireProgressStateChanged("Performing FuzzyCMeans Segmentation on blue");

        if (nVOIs > 0) {
            fireProgressStateChanged(5);
        } else {
            fireProgressStateChanged(2);
        }

        nClasses = 2;
        nPyramid = 4;
        oneJacobiIter = 1;
        twoJacobiIter = 2;
        q = 2.0f;
        oneSmooth = 2e4f;
        twoSmooth = 2e5f;
        outputGainField = false;
        segmentation = HARD_ONLY;
        cropBackground = false;
        threshold = 0.0f;
        maxIter = 200;
        endTolerance = 0.01f;
        wholeImage = true;

        // grayImage enters ModelStorageBase.FLOAT and returns ModelStorageBase.UBYTE
        fcmAlgo = new AlgorithmFuzzyCMeans(grayImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q, oneSmooth,
                                           twoSmooth, outputGainField, segmentation, cropBackground, threshold, maxIter,
                                           endTolerance, wholeImage);
        centroids = new float[2];
        min = (float) grayImage.getMin();
        max = (float) grayImage.getMax();
        centroids[0] = min + ((max - min) / 3.0f);
        centroids[1] = min + (2.0f * (max - min) / 3.0f);
        fcmAlgo.setCentroids(centroids);
        fcmAlgo.run();
        fcmAlgo.finalize();
        fcmAlgo = null;

        // Now convert the min and max to 0 and 1
        fireProgressStateChanged("Setting segmented blue values to 0 and 1");

        if (nVOIs > 0) {
            fireProgressStateChanged(20);
        } else {
            fireProgressStateChanged(6);
        }

        grayImage.calcMinMax();
        max = (float) grayImage.getMax();
        thresholds = new float[2];
        thresholds[0] = max;
        thresholds[1] = max;
        fillValue = 0.0f;
        thresholdAlgo = new AlgorithmThresholdDual(grayImage, thresholds, fillValue, AlgorithmThresholdDual.BINARY_TYPE,
                                                   wholeImage, false);
        thresholdAlgo.run();
        thresholdAlgo.finalize();
        thresholdAlgo = null;

        // Remove all inappropriate holes in blue nuclei
        fireProgressStateChanged("Removing holes from blue nuclei");

        if (nVOIs > 0) {
            fireProgressStateChanged(23);
        } else {
            fireProgressStateChanged(7);
        }

        fillHolesAlgo2D = new AlgorithmMorphology2D(grayImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0,
                                                    wholeImage);
        fillHolesAlgo2D.run();
        fillHolesAlgo2D.finalize();
        fillHolesAlgo2D = null;

        // Smooth with a morphological opening followed by a closing
        fireProgressStateChanged("Opening blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(25);
        } else {
            fireProgressStateChanged(8);
        }

        kernel = AlgorithmMorphology2D.CONNECTED4;
        circleDiameter = 1.0f;
        method = AlgorithmMorphology2D.OPEN;
        itersDilation = 1;
        itersErosion = 1;
        numPruningPixels = 0;
        edgingType = 0;
        openAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                             numPruningPixels, edgingType, wholeImage);
        openAlgo.run();
        openAlgo.finalize();
        openAlgo = null;

        fireProgressStateChanged("Closing blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(30);
        } else {
            fireProgressStateChanged(10);
        }

        method = AlgorithmMorphology2D.CLOSE;
        closeAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                              numPruningPixels, edgingType, wholeImage);
        closeAlgo.run();
        closeAlgo.finalize();
        closeAlgo = null;

        byteBuffer = new byte[length];

        try {
            grayImage.exportData(0, length, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

        fireProgressStateChanged("Eroding blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(35);
        } else {
            fireProgressStateChanged(12);
        }

        kernel = AlgorithmMorphology2D.CONNECTED4;
        method = AlgorithmMorphology2D.ERODE;
        itersDilation = 0;
        itersErosion = iters;
        erosionAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                                numPruningPixels, edgingType, wholeImage);
        erosionAlgo.run();
        erosionAlgo.finalize();
        erosionAlgo = null;

        // Put the object IDs in IDArray and the corresponding boundaries
        // in boundaryArray
        fireProgressStateChanged("IDing objects in blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(45);
        } else {
            fireProgressStateChanged(15);
        }

        kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
        circleDiameter = 0.0f;
        method = AlgorithmMorphology2D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo2D = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, wholeImage);
        idObjectsAlgo2D.setMinMax(blueMin, 200000);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;

        grayImage.calcMinMax();
        numObjects = (int) grayImage.getMax();

        fireProgressStateChanged("Dilating IDed objects in blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(55);
        } else {
            fireProgressStateChanged(18);
        }

        kernel = AlgorithmMorphology2D.CONNECTED4;
        method = AlgorithmMorphology2D.DILATE;
        itersDilation = 6 * iters;
        itersErosion = 0;
        dilationAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                                 numPruningPixels, edgingType, wholeImage);
        dilationAlgo.run();
        dilationAlgo.finalize();
        dilationAlgo = null;

        IDArray = new byte[length];

        try {
            grayImage.exportData(0, length, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

        fireProgressStateChanged("Zeroing overgrowth of dilated objects");

        if (nVOIs > 0) {
            fireProgressStateChanged(65);
        } else {
            fireProgressStateChanged(22);
        }

        // byteBuffer contains the smoothed blue objects before erosion
        // IDArray contains the objects after erosion and dilation
        for (i = 0; i < length; i++) {

            if (byteBuffer[i] == (byte) 0) {
                IDArray[i] = (byte) 0;
            }
        }

        edgeObjects = numObjects;
        fireProgressStateChanged("Removing blue objects touching edges");

        if (nVOIs > 0) {
            fireProgressStateChanged(70);
        } else {
            fireProgressStateChanged(23);
        }

        if (numObjects == 1) {
            initiallyOneObject = true;
        }
        else {
            initiallyOneObject = false;
        }
        
        for (id = numObjects; id >= 1; id--) {
            removeID = false;

            for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                for (x = 0; x < xDim; x++) {
                    i = x + j;

                    if ((IDArray[i] == id) && ((x == 0) || (x == (xDim - 1)) || (y == 0) || (y == (yDim - 1)))) {
                        removeID = true;
                    }
                } // for (x = 0; x < xDim; x++)
            } // for (j = 0, y = 0; y < yDim; y++, j += xDim)
            
            if (initiallyOneObject && removeID) {
                response = JOptionPane.showConfirmDialog(UI.getMainFrame(), "Delete the one blue object with edge touching?", 
                           "Deletion", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
                if (response == JOptionPane.NO_OPTION) {
                    removeID = false;
                }
            } // if (initiallyOneObject && removeID)


            if (removeID) {

                for (i = 0; i < length; i++) {

                    if (IDArray[i] == (byte) id) {
                        IDArray[i] = (byte) 0;
                    } 
                    else if (IDArray[i] > id) {
                        IDArray[i]--;
                    }
                }

                numObjects--;
                
                
            } // if (removeID)
        } // for (id = numObjects; id >= 1; id--)

        if (numObjects == 0) {
            MipavUtil.displayError(edgeObjects + " blue objects touched edges");
            MipavUtil.displayError("No blue objects that do not touch edges");

            setCompleted(false);

            return;
        }


        if (greenNumber > 0) {
            greenBuffer = new float[length];
        }

        try {

            if (redNumber > 0) {
                srcImage.exportRGBData(1, 0, length, buffer); // export red data
            } // if (redNumber > 0)

            if (greenNumber > 0) {
                srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
            } // if (greenNumber > 0)
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            errorCleanUp("Algorithm RegionDistance reports: source image locked", true);

            return;
        }

        if (nVOIs == 0) {

            if (redNumber > 0) {
                fireProgressStateChanged("Creating red image");
                fireProgressStateChanged(30);

                // grayImage is ModelStorageBase.UBYTE
                grayImage.reallocate(ModelStorageBase.FLOAT);

                try {
                    grayImage.importData(0, buffer, true);
                } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Error on grayImage.importData", true);

                    return;
                }

                fireProgressStateChanged("Performing median filter on red");
                fireProgressStateChanged(31);
                medianIters = 1;
                kernelSize = 3;
                kernelShape = AlgorithmMedian.SQUARE_KERNEL;
                stdDev = 0.0f;
                wholeImage = true;
                algoMedian = new AlgorithmMedian(grayImage, medianIters, kernelSize, kernelShape, stdDev, adaptiveSize,
                                                 maximumSize, wholeImage);
                algoMedian.run();
                algoMedian.finalize();
                algoMedian = null;

                fireProgressStateChanged("Getting histogram info on red");
                fireProgressStateChanged(32);
                bins = 256;
                algoHist = new AlgorithmHistogram(grayImage, bins);
                algoHist.run();
                histoBuffer = algoHist.getHistoBuffer();
                lowValue = algoHist.getLowValue();
                algoHist.finalize();
                algoHist = null;

                totalCount = histoBuffer[0];

                for (i = 1; i < histoBuffer.length; i++) {
                    totalCount += histoBuffer[i];
                }

                countsToRetain = (int) Math.round(redFraction * totalCount);
                found = false;
                countsFound = 0;
                threshold = 1.0f;

                for (i = bins - 1; (i >= 0) && !found; i--) {
                    countsFound += histoBuffer[i];

                    if (countsFound >= countsToRetain) {
                        found = true;
                        threshold = (float) lowValue[i];
                    }
                }

                cropBackground = true;

                // Segment red into 3 values
                fireProgressStateChanged("Performing FuzzyCMeans Segmentation on red");
                fireProgressStateChanged(33);
                nClasses = 3;
                nPyramid = 4;
                oneJacobiIter = 1;
                twoJacobiIter = 2;
                q = 2.0f;
                oneSmooth = 2e4f;
                twoSmooth = 2e5f;
                outputGainField = false;
                segmentation = HARD_ONLY;
                maxIter = 200;
                endTolerance = 0.01f;
                wholeImage = true;

                // grayImage enters ModelStorageBase.FLOAT and returns ModelStorageBase.UBYTE
                fcmAlgo = new AlgorithmFuzzyCMeans(grayImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q,
                                                   oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground,
                                                   threshold, maxIter, endTolerance, wholeImage);
                centroids = new float[3];

                // Find the first value above the 0 value for min
                min = Float.MAX_VALUE;

                for (i = 0; i < length; i++) {

                    if ((buffer[i] < min) && (buffer[i] != 0.0f)) {
                        min = buffer[i];
                    }
                }

                max = (float) grayImage.getMax();
                centroids[0] = min + ((max - min) / 4.0f);
                centroids[1] = min + (2.0f * (max - min) / 4.0f);
                centroids[2] = min + (3.0f * (max - min) / 4.0f);
                fcmAlgo.setCentroids(centroids);
                fcmAlgo.run();
                fcmAlgo.finalize();
                fcmAlgo = null;

                // ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
                // new Dimension(600, 300), srcImage.getUserInterface());

                // Now convert the red min and max to 0 and 1
                fireProgressStateChanged("Setting segmented red values to 0 and 1");
                fireProgressStateChanged(35);
                grayImage.calcMinMax();
                max = (float) grayImage.getMax();
                thresholds[0] = max;
                thresholds[1] = max;
                fillValue = 0.0f;
                thresholdAlgo = new AlgorithmThresholdDual(grayImage, thresholds, fillValue,
                                                           AlgorithmThresholdDual.BINARY_TYPE, wholeImage, false);
                thresholdAlgo.run();
                thresholdAlgo.finalize();
                thresholdAlgo = null;

                // Remove all inappropriate holes in red VOIs
                fireProgressStateChanged("Removing holes from red VOIs");
                fireProgressStateChanged(37);
                fillHolesAlgo2D = new AlgorithmMorphology2D(grayImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0,
                                                            0, wholeImage);
                fillHolesAlgo2D.run();
                fillHolesAlgo2D.finalize();
                fillHolesAlgo2D = null;

                // Smooth red with a morphological opening followed by a closing
                fireProgressStateChanged("Opening red segmented image");
                fireProgressStateChanged(40);
                kernel = AlgorithmMorphology2D.CONNECTED4;
                circleDiameter = 1.0f;
                method = AlgorithmMorphology2D.OPEN;
                itersDilation = 1;
                itersErosion = 1;
                numPruningPixels = 0;
                edgingType = 0;
                openAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                     itersErosion, numPruningPixels, edgingType, wholeImage);
                openAlgo.run();
                openAlgo.finalize();
                openAlgo = null;

                fireProgressStateChanged("Closing red segmented image");
                fireProgressStateChanged(42);
                method = AlgorithmMorphology2D.CLOSE;
                closeAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                      itersErosion, numPruningPixels, edgingType, wholeImage);
                closeAlgo.run();
                closeAlgo.finalize();
                closeAlgo = null;

                // Put the red VOI IDs in redIDArray
                fireProgressStateChanged("IDing objects in red segmented image");
                fireProgressStateChanged(44);
                kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
                circleDiameter = 0.0f;
                method = AlgorithmMorphology2D.ID_OBJECTS;
                itersDilation = 0;
                itersErosion = 0;
                idObjectsAlgo2D = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                            itersErosion, numPruningPixels, edgingType, wholeImage);
                idObjectsAlgo2D.setMinMax(redMin, 200000);
                idObjectsAlgo2D.run();
                idObjectsAlgo2D.finalize();
                idObjectsAlgo2D = null;

                grayImage.calcMinMax();
                numRedObjects = (int) grayImage.getMax();
                redIDArray = new byte[length];

                try {
                    grayImage.exportData(0, length, redIDArray);
                } catch (IOException error) {
                    byteBuffer = null;
                    redIDArray = null;
                    errorCleanUp("Error on grayImage.exportData", true);

                    return;
                }

                // Split redIDArray objects apart into 2 or 3 redIDArray objects
                // if they go between 2 or 3 cells
                moreRedObjects = 0;
                redObjectNucleus = new int[numObjects + 1];

                for (i = 1; i <= numRedObjects; i++) {

                    for (j = 0; j <= numObjects; j++) {
                        redObjectNucleus[j] = 0;
                    } // for (j = 0; j <= numObjects; j++)

                    for (j = 0; j < length; j++) {

                        if (redIDArray[j] == i) {
                            redObjectNucleus[IDArray[j]]++;
                        } // if (redIDArray[j] == i)
                    } // for (j = 0; j < totLength; j++)

                    maxNucleus = 1;
                    maxCount = redObjectNucleus[1];

                    for (j = 2; j <= numObjects; j++) {

                        if (redObjectNucleus[j] > maxCount) {
                            maxNucleus = j;
                            maxCount = redObjectNucleus[j];
                        }
                    } // for (j = 2; j <= numObjects; j++)

                    secondMaxCount = 0;
                    secondMaxNucleus = 0;

                    for (j = 1; j <= numObjects; j++) {

                        if (j != maxNucleus) {

                            if (redObjectNucleus[j] > secondMaxCount) {
                                secondMaxNucleus = j;
                                secondMaxCount = redObjectNucleus[j];
                            } // if (redObjectNucleus[j] > secondMaxCount)
                        } // if (j != maxNucleus)
                    } // for (j = 1; j <= numObjects; j++)

                    if (secondMaxCount > 0) {

                        if (secondMaxCount >= redMin) {
                            moreRedObjects++;

                            for (j = 0; j < length; j++) {

                                if ((redIDArray[j] == i) && (IDArray[j] == secondMaxNucleus)) {
                                    redIDArray[j] = (byte) (numRedObjects + moreRedObjects);
                                }
                            } // for (j = 0; j < totLength; j++)

                            thirdMaxCount = 0;
                            thirdMaxNucleus = 0;

                            for (j = 1; j <= numObjects; j++) {

                                if ((j != maxNucleus) && (j != secondMaxNucleus)) {

                                    if (redObjectNucleus[j] > thirdMaxCount) {
                                        thirdMaxNucleus = j;
                                        thirdMaxCount = redObjectNucleus[j];
                                    } // if (redObjectNucleus[j] > thirdMaxCount)
                                } // if ((j != maxNucleus) && (j != secondMaxNucleus))
                            } // for (j = 1; j <= numObjects; j++)

                            if (thirdMaxCount > 0) {

                                if (thirdMaxCount >= redMin) {
                                    moreRedObjects++;

                                    for (j = 0; j < length; j++) {

                                        if ((redIDArray[j] == i) && (IDArray[j] == thirdMaxNucleus)) {
                                            redIDArray[j] = (byte) (numRedObjects + moreRedObjects);
                                        }
                                    } // for (j = 0; j < totLength; j++)
                                } // if (thirdMaxCount >= redMin)
                                else { // thirdMaxCount < redMin

                                    for (j = 0; j < length; j++) {

                                        if ((redIDArray[j] == i) && (IDArray[j] != 0) && (IDArray[j] != maxNucleus) &&
                                                (IDArray[j] != secondMaxNucleus)) {
                                            redIDArray[j] = (byte) 0;
                                        }
                                    } // for (j = 0; j < totLength; j++)
                                } // else thirdMaxCount < redMin
                            } // if (thirdMaxCount > 0)
                        } // if (secondMaxCount >= redMin)
                        else { // secondMaxCount < redMin

                            for (j = 0; j < length; j++) {

                                if ((redIDArray[j] == i) && (IDArray[j] != 0) && (IDArray[j] != maxNucleus)) {
                                    redIDArray[j] = (byte) 0;
                                }
                            } // for (j = 0; j < totLength; j++)
                        } // secondMaxCount < redMin
                    } // if (secondMaxCount > 0)
                } // for (i = 1; i <= numRedObjects; i++)

                numRedObjects = numRedObjects + moreRedObjects;
            } // if (redNumber > 0)

            if (greenNumber > 0) {
                fireProgressStateChanged("Creating green image");
                fireProgressStateChanged(46);

                // grayImage is ModelStorageBase.UBYTE
                grayImage.reallocate(ModelStorageBase.FLOAT);

                try {
                    grayImage.importData(0, greenBuffer, true);
                } catch (IOException error) {
                    greenBuffer = null;
                    errorCleanUp("Error on grayImage.importData", true);

                    return;
                }

                fireProgressStateChanged("Performing median filter on green");
                fireProgressStateChanged(47);
                medianIters = 1;
                kernelSize = 3;
                kernelShape = AlgorithmMedian.SQUARE_KERNEL;
                stdDev = 0.0f;
                wholeImage = true;
                algoMedian = new AlgorithmMedian(grayImage, medianIters, kernelSize, kernelShape, stdDev, adaptiveSize,
                                                 maximumSize, wholeImage);
                algoMedian.run();
                algoMedian.finalize();
                algoMedian = null;

                fireProgressStateChanged("Getting histogram info on green");
                fireProgressStateChanged(48);
                bins = 256;
                algoHist = new AlgorithmHistogram(grayImage, bins);
                algoHist.run();
                histoBuffer = algoHist.getHistoBuffer();
                lowValue = algoHist.getLowValue();
                algoHist.finalize();
                algoHist = null;

                totalCount = histoBuffer[0];

                for (i = 1; i < histoBuffer.length; i++) {
                    totalCount += histoBuffer[i];
                }

                countsToRetain = (int) Math.round(greenFraction * totalCount);
                found = false;
                countsFound = 0;
                threshold = 1.0f;

                for (i = bins - 1; (i >= 0) && !found; i--) {
                    countsFound += histoBuffer[i];

                    if (countsFound >= countsToRetain) {
                        found = true;
                        threshold = (float) lowValue[i];
                    }
                }

                cropBackground = true;


                // Segment green into 3 values
                fireProgressStateChanged("Performing FuzzyCMeans Segmentation on green");
                fireProgressStateChanged(49);
                nClasses = 3;
                nPyramid = 4;
                oneJacobiIter = 1;
                twoJacobiIter = 2;
                q = 2.0f;
                oneSmooth = 2e4f;
                twoSmooth = 2e5f;
                outputGainField = false;
                segmentation = HARD_ONLY;
                maxIter = 200;
                endTolerance = 0.01f;
                wholeImage = true;

                // grayImage enters ModelStorageBase.FLOAT and returns ModelStorageBase.UBYTE
                fcmAlgo = new AlgorithmFuzzyCMeans(grayImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q,
                                                   oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground,
                                                   threshold, maxIter, endTolerance, wholeImage);
                centroids = new float[3];

                // Find the first value above the 0 value for min
                min = Float.MAX_VALUE;

                for (i = 0; i < length; i++) {

                    if ((greenBuffer[i] < min) && (greenBuffer[i] != 0.0f)) {
                        min = greenBuffer[i];
                    }
                }

                max = (float) grayImage.getMax();
                centroids[0] = min + ((max - min) / 4.0f);
                centroids[1] = min + (2.0f * (max - min) / 4.0f);
                centroids[2] = min + (3.0f * (max - min) / 4.0f);
                fcmAlgo.setCentroids(centroids);
                fcmAlgo.run();
                fcmAlgo.finalize();
                fcmAlgo = null;

                // Now convert the green min and max to 0 and 1
                fireProgressStateChanged("Setting segmented green values to 0 and 1");
                fireProgressStateChanged(50);
                grayImage.calcMinMax();
                max = (float) grayImage.getMax();
                thresholds[0] = max;
                thresholds[1] = max;
                fillValue = 0.0f;
                thresholdAlgo = new AlgorithmThresholdDual(grayImage, thresholds, fillValue,
                                                           AlgorithmThresholdDual.BINARY_TYPE, wholeImage, false);
                thresholdAlgo.run();
                thresholdAlgo.finalize();
                thresholdAlgo = null;

                // Remove all inappropriate holes in green VOIs
                fireProgressStateChanged("Removing holes from green VOIs");
                fireProgressStateChanged(52);
                fillHolesAlgo2D = new AlgorithmMorphology2D(grayImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0,
                                                            0, wholeImage);
                fillHolesAlgo2D.run();
                fillHolesAlgo2D.finalize();
                fillHolesAlgo2D = null;

                // Smooth green with a morphological opening followed by a closing
                fireProgressStateChanged("Opening green segmented image");
                fireProgressStateChanged(54);
                kernel = AlgorithmMorphology2D.CONNECTED4;
                circleDiameter = 1.0f;
                method = AlgorithmMorphology2D.OPEN;
                itersDilation = 1;
                itersErosion = 1;
                numPruningPixels = 0;
                edgingType = 0;
                openAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                     itersErosion, numPruningPixels, edgingType, wholeImage);
                openAlgo.run();
                openAlgo.finalize();
                openAlgo = null;

                fireProgressStateChanged("Closing green segmented image");
                fireProgressStateChanged(56);
                method = AlgorithmMorphology2D.CLOSE;
                closeAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                      itersErosion, numPruningPixels, edgingType, wholeImage);
                closeAlgo.run();
                closeAlgo.finalize();
                closeAlgo = null;

                // Put the green VOI IDs in greenIDArray
                fireProgressStateChanged("IDing objects in green segmented image");
                fireProgressStateChanged(58);
                kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
                circleDiameter = 0.0f;
                method = AlgorithmMorphology2D.ID_OBJECTS;
                itersDilation = 0;
                itersErosion = 0;
                idObjectsAlgo2D = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                            itersErosion, numPruningPixels, edgingType, wholeImage);
                idObjectsAlgo2D.setMinMax(greenMin, 200000);
                idObjectsAlgo2D.run();
                idObjectsAlgo2D.finalize();
                idObjectsAlgo2D = null;

                grayImage.calcMinMax();
                numGreenObjects = (int) grayImage.getMax();
                greenIDArray = new byte[length];

                try {
                    grayImage.exportData(0, length, greenIDArray);
                } catch (IOException error) {
                    byteBuffer = null;
                    greenIDArray = null;
                    errorCleanUp("Error on grayImage.exportData", true);

                    return;
                }

                // Split greenIDArray objects apart into 2 or 3 greenIDArray objects
                // if they go between 2 or 3 cells
                moreGreenObjects = 0;
                greenObjectNucleus = new int[numObjects + 1];

                for (i = 1; i <= numGreenObjects; i++) {

                    for (j = 0; j <= numObjects; j++) {
                        greenObjectNucleus[j] = 0;
                    } // for (j = 0; j <= numObjects; j++)

                    for (j = 0; j < length; j++) {

                        if (greenIDArray[j] == i) {
                            greenObjectNucleus[IDArray[j]]++;
                        } // if (greenIDArray[j] == i)
                    } // for (j = 0; j < totLength; j++)

                    maxNucleus = 1;
                    maxCount = greenObjectNucleus[1];

                    for (j = 2; j <= numObjects; j++) {

                        if (greenObjectNucleus[j] > maxCount) {
                            maxNucleus = j;
                            maxCount = greenObjectNucleus[j];
                        }
                    } // for (j = 2; j <= numObjects; j++)

                    secondMaxCount = 0;
                    secondMaxNucleus = 0;

                    for (j = 1; j <= numObjects; j++) {

                        if (j != maxNucleus) {

                            if (greenObjectNucleus[j] > secondMaxCount) {
                                secondMaxNucleus = j;
                                secondMaxCount = greenObjectNucleus[j];
                            } // if (greenObjectNucleus[j] > secondMaxCount)
                        } // if (j != maxNucleus)
                    } // for (j = 1; j <= numObjects; j++)

                    if (secondMaxCount > 0) {

                        if (secondMaxCount >= greenMin) {
                            moreGreenObjects++;

                            for (j = 0; j < length; j++) {

                                if ((greenIDArray[j] == i) && (IDArray[j] == secondMaxNucleus)) {
                                    greenIDArray[j] = (byte) (numGreenObjects + moreGreenObjects);
                                }
                            } // for (j = 0; j < totLength; j++)

                            thirdMaxCount = 0;
                            thirdMaxNucleus = 0;

                            for (j = 1; j <= numObjects; j++) {

                                if ((j != maxNucleus) && (j != secondMaxNucleus)) {

                                    if (greenObjectNucleus[j] > thirdMaxCount) {
                                        thirdMaxNucleus = j;
                                        thirdMaxCount = greenObjectNucleus[j];
                                    } // if (greenObjectNucleus[j] > thirdMaxCount)
                                } // if ((j != maxNucleus) && (j != secondMaxNucleus))
                            } // for (j = 1; j <= numObjects; j++)

                            if (thirdMaxCount > 0) {

                                if (thirdMaxCount >= greenMin) {
                                    moreGreenObjects++;

                                    for (j = 0; j < length; j++) {

                                        if ((greenIDArray[j] == i) && (IDArray[j] == thirdMaxNucleus)) {
                                            greenIDArray[j] = (byte) (numGreenObjects + moreGreenObjects);
                                        }
                                    } // for (j = 0; j < totLength; j++)
                                } else { // if (thirdMaxCount >= greenMin),  thirdMaxCount < greenMin

                                    for (j = 0; j < length; j++) {

                                        if ((greenIDArray[j] == i) && (IDArray[j] != 0) && (IDArray[j] != maxNucleus) &&
                                                (IDArray[j] != secondMaxNucleus)) {
                                            greenIDArray[j] = (byte) 0;
                                        }
                                    } // for (j = 0; j < totLength; j++)
                                } // else thirdMaxCount < greenMin
                            } // if (thirdMaxCount > 0)
                        } else { // if (secondMaxCount >= greenMin),  secondMaxCount < greenMin

                            for (j = 0; j < length; j++) {

                                if ((greenIDArray[j] == i) && (IDArray[j] != 0) && (IDArray[j] != maxNucleus)) {
                                    greenIDArray[j] = (byte) 0;
                                }
                            } // for (j = 0; j < totLength; j++)
                        } // secondMaxCount < greenMin
                    } // if (secondMaxCount > 0)
                } // for (i = 1; i <= numGreenObjects; i++)

                numGreenObjects = numGreenObjects + moreGreenObjects;
            } // if (greenNumber > 0)

            if (redNumber > 0) {

                // Sort the red objects within each nucleus.
                // Create no more than redNumber red objects within each nucleus.
                fireProgressStateChanged("Sorting red objects by intensity count");
                fireProgressStateChanged(60);
                redIntensityTotal = new float[numRedObjects];
                redXCenter = new float[numRedObjects];
                redYCenter = new float[numRedObjects];
                redNucleusNumber = new int[numObjects];
                sortedRedIndex = new int[numObjects][redNumber];
                sortedRedIntensity = new float[numObjects][redNumber];
                sortedRedXCenter = new float[numObjects][redNumber];
                sortedRedYCenter = new float[numObjects][redNumber];

                for (j = 1; j <= numRedObjects; j++) {

                    for (i = 0, x = 0, y = 0; i < length; i++) {

                        if (redIDArray[i] == j) {
                            redIntensityTotal[j - 1] += buffer[i];
                            redXCenter[j - 1] += buffer[i] * x;
                            redYCenter[j - 1] += buffer[i] * y;
                        }

                        x++;

                        if (x == xDim) {
                            x = 0;
                            y++;
                        }
                    } // for (i = 0, x = 0, y = 0; i < length; i++)

                    redXCenter[j - 1] = redXCenter[j - 1] / redIntensityTotal[j - 1];
                    redYCenter[j - 1] = redYCenter[j - 1] / redIntensityTotal[j - 1];
                    i = (int) redXCenter[j - 1] + ((int) (redYCenter[j - 1]) * xDim);
                    id = IDArray[i];

                    if (id >= 1) {

                        if (redNucleusNumber[id - 1] < redNumber) {
                            redNucleusNumber[id - 1]++;
                        }

                        found = false;

                        for (i = 0; (i < redNucleusNumber[id - 1]) && !found; i++) {

                            if (redIntensityTotal[j - 1] >= sortedRedIntensity[id - 1][i]) {
                                found = true;

                                for (k = redNucleusNumber[id - 1] - 2; k >= i; k--) {
                                    sortedRedIntensity[id - 1][k + 1] = sortedRedIntensity[id - 1][k];
                                    sortedRedIndex[id - 1][k + 1] = sortedRedIndex[id - 1][k];
                                    sortedRedXCenter[id - 1][k + 1] = sortedRedXCenter[id - 1][k];
                                    sortedRedYCenter[id - 1][k + 1] = sortedRedYCenter[id - 1][k];
                                }

                                sortedRedIntensity[id - 1][i] = redIntensityTotal[j - 1];
                                sortedRedIndex[id - 1][i] = j; // from redIDArray
                                sortedRedXCenter[id - 1][i] = redXCenter[j - 1];
                                sortedRedYCenter[id - 1][i] = redYCenter[j - 1];
                            }
                        } // for (i = 0; i < redNucleusNumber[id-1]  && !found; i++)
                    } // if (id >= 1)
                } // for (j = 1; j <= numRedObjects; j++)
            } // if (redNumber > 0)

            if (greenNumber > 0) {

                // Sort the green objects within each nucleus.
                // Create no more than greenNumber green objects within each nucleus.
                fireProgressStateChanged("Sorting green objects by intensity count");
                fireProgressStateChanged(62);
                greenIntensityTotal = new float[numGreenObjects];
                greenXCenter = new float[numGreenObjects];
                greenYCenter = new float[numGreenObjects];
                greenNucleusNumber = new int[numObjects];
                sortedGreenIndex = new int[numObjects][greenNumber];
                sortedGreenIntensity = new float[numObjects][greenNumber];
                sortedGreenXCenter = new float[numObjects][greenNumber];
                sortedGreenYCenter = new float[numObjects][greenNumber];

                for (j = 1; j <= numGreenObjects; j++) {

                    for (x = 0, y = 0, i = 0; i < length; i++) {

                        if (greenIDArray[i] == j) {
                            greenIntensityTotal[j - 1] += greenBuffer[i];
                            greenXCenter[j - 1] += greenBuffer[i] * x;
                            greenYCenter[j - 1] += greenBuffer[i] * y;
                        }

                        x++;

                        if (x == xDim) {
                            x = 0;
                            y++;
                        }
                    }

                    greenXCenter[j - 1] = greenXCenter[j - 1] / greenIntensityTotal[j - 1];
                    greenYCenter[j - 1] = greenYCenter[j - 1] / greenIntensityTotal[j - 1];
                    i = (int) greenXCenter[j - 1] + ((int) (greenYCenter[j - 1]) * xDim);
                    id = IDArray[i];

                    if (id >= 1) {

                        if (greenNucleusNumber[id - 1] < greenNumber) {
                            greenNucleusNumber[id - 1]++;
                        }

                        found = false;

                        for (i = 0; (i < greenNucleusNumber[id - 1]) && !found; i++) {

                            if (greenIntensityTotal[j - 1] >= sortedGreenIntensity[id - 1][i]) {
                                found = true;

                                for (k = greenNucleusNumber[id - 1] - 2; k >= i; k--) {
                                    sortedGreenIntensity[id - 1][k + 1] = sortedGreenIntensity[id - 1][k];
                                    sortedGreenIndex[id - 1][k + 1] = sortedGreenIndex[id - 1][k];
                                    sortedGreenXCenter[id - 1][k + 1] = sortedGreenXCenter[id - 1][k];
                                    sortedGreenYCenter[id - 1][k + 1] = sortedGreenYCenter[id - 1][k];
                                }

                                sortedGreenIntensity[id - 1][i] = greenIntensityTotal[j - 1];
                                sortedGreenIndex[id - 1][i] = j; // from greenIDArray
                                sortedGreenXCenter[id - 1][i] = greenXCenter[j - 1];
                                sortedGreenYCenter[id - 1][i] = greenYCenter[j - 1];
                            }
                        } // for (i = 0; i < greenNucleusNumber[id-1]  && !found; i++)
                    } // if (id >= 1)
                } // for (j = 1; j <= numGreenObjects; j++)
            } // if (greenNumber > 0)

            Arrays.fill(byteBuffer, (byte) 0);

            numRedVOIObjects = 0;
            numGreenVOIObjects = 0;

            for (i = 0; i < numObjects; i++) {

                if (redNumber > 0) {
                    numRedVOIObjects += redNucleusNumber[i];
                }

                if (greenNumber > 0) {
                    numGreenVOIObjects += greenNucleusNumber[i];
                }
            }

            numVOIObjects = Math.max(numRedVOIObjects, numGreenVOIObjects);
            colorTable = new Color[numVOIObjects];
            nameTable = new String[numVOIObjects];

            if (redNumber > 0) {

                for (i = 0, j = 0; i < numObjects; i++) {

                    for (m = 0; m < redNumber; m++) {

                        if (redNucleusNumber[i] > m) {

                            for (k = 0; k < length; k++) {

                                if (redIDArray[k] == sortedRedIndex[i][m]) {
                                    byteBuffer[k] = (byte) (j + 1);
                                }
                            } // for (k = 0; k < length; k++)

                            if (m == 0) {
                                colorTable[j] = Color.yellow;
                            } else {
                                colorTable[j] = Color.yellow.darker();
                            }

                            nameTable[j] = new String((i + 1) + "R" + (m + 1));
                            j++;
                        } // if (redNucleusNumber[i] > m)
                    } // for (m = 0; m < redNumber; m++)
                } // for (i = 0, j = 0; i < numObjects; i++)

                try {
                    grayImage.importData(0, byteBuffer, true);
                } catch (IOException error) {
                    byteBuffer = null;
                    errorCleanUp("Error on grayImage.importData", true);

                    return;
                }

                fireProgressStateChanged("Extracting VOIs from red image");
                fireProgressStateChanged(73);
                algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
                algoVOIExtraction.setColorTable(colorTable);
                algoVOIExtraction.setNameTable(nameTable);
                algoVOIExtraction.run();
                algoVOIExtraction.finalize();
                algoVOIExtraction = null;

                srcImage.setVOIs(grayImage.getVOIs());
            } // if (redNumber > 0)

            if (greenNumber > 0) {
                Arrays.fill(byteBuffer, (byte) 0);

                for (i = 0, j = 0; i < numObjects; i++) {

                    for (m = 0; m < greenNumber; m++) {

                        if (greenNucleusNumber[i] > m) {

                            for (k = 0; k < length; k++) {

                                if (greenIDArray[k] == sortedGreenIndex[i][m]) {
                                    byteBuffer[k] = (byte) (j + 1);
                                }
                            } // for (k = 0; k < length; k++)

                            if (m == 0) {
                                colorTable[j] = Color.pink;
                            } else {
                                colorTable[j] = Color.pink.darker();
                            }

                            nameTable[j] = new String((i + 1) + "G" + (m + 1));
                            j++;
                        } // if (greenNucleusNumber[i] > m)
                    } // for (m = 0; m < greenNumber; m++)
                } // for (i = 0, j = 0; i < numObjects; i++)

                grayImage.resetVOIs();

                try {
                    grayImage.importData(0, byteBuffer, true);
                } catch (IOException error) {
                    byteBuffer = null;
                    errorCleanUp("Error on grayImage.importData", true);

                    return;
                }

                fireProgressStateChanged("Extracting VOIs from green image");
                fireProgressStateChanged(74);
                algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
                algoVOIExtraction.setColorTable(colorTable);
                algoVOIExtraction.setNameTable(nameTable);
                algoVOIExtraction.run();
                algoVOIExtraction.finalize();
                algoVOIExtraction = null;

                if (redNumber > 0) {
                    srcImage.addVOIs(grayImage.getVOIs());
                } else {
                    srcImage.setVOIs(grayImage.getVOIs());
                }
            } // if (greenNumber > 0)

            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
        } // if (nVOIs == 0)

        shortMask = new short[length];

        idCount = new int[numObjects];
        xPosGeo = new float[nVOIs];
        yPosGeo = new float[nVOIs];
        xPosGrav = new float[nVOIs];
        yPosGrav = new float[nVOIs];
        objectID = new int[nVOIs];
        isRed = new boolean[nVOIs];
        colorCount = new float[nVOIs];

        UI.clearAllDataText();
        UI.setDataText("\n");

        voiCount = new int[nVOIs];

        for (i = 0; i < nVOIs; i++) {
            fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
            fireProgressStateChanged(75 + (10 * (i + 1) / nVOIs));
            VOIs.VOIAt(i).setOnlyID((short) i);
            voiName = VOIs.VOIAt(i).getName();

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                Arrays.fill(shortMask, (short) -1);

                shortMask = srcImage.generateVOIMask(shortMask, i);
                redCount = 0;
                greenCount = 0;
                Arrays.fill(idCount, 0);

                for (j = 0; j < length; j++) {

                    if (shortMask[j] != -1) {
                        if (redNumber >= 1) {
                            redCount += buffer[j];
                        }
                        if (greenNumber >= 1) {
                            greenCount += greenBuffer[j];
                        }

                        if (IDArray[j] > 0) {
                            index = IDArray[j] - 1;
                            idCount[index]++;
                        }
                    }
                }

                objectID[i] = 1;
                objectCount = idCount[0];

                for (j = 2; j <= numObjects; j++) {

                    if (idCount[j - 1] > objectCount) {
                        objectID[i] = j;
                        objectCount = idCount[j - 1];
                    }
                } // for (j = 2; j <= numObjects; j++)

                xPosGeo[i] = 0.0f;
                yPosGeo[i] = 0.0f;
                xPosGrav[i] = 0.0f;
                yPosGrav[i] = 0.0f;

                if (redCount >= greenCount) {
                    isRed[i] = true;
                    colorCount[i] = 0.0f;

                    for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                        for (x = 0; x < xDim; x++) {
                            index = x + j;

                            if (shortMask[index] != -1) {
                                xPosGeo[i] += x;
                                yPosGeo[i] += y;
                                xPosGrav[i] += buffer[index] * x;
                                yPosGrav[i] += buffer[index] * y;
                                colorCount[i] += buffer[index];
                                voiCount[i]++;
                                IDArray[index] = (byte) objectID[i];
                            }
                        }
                    }

                    xPosGeo[i] /= voiCount[i];
                    yPosGeo[i] /= voiCount[i];
                    xPosGrav[i] /= colorCount[i];
                    yPosGrav[i] /= colorCount[i];
                } // if (redCount >= greenCount)
                else { // redCount < greenCount
                    colorCount[i] = 0.0f;

                    for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                        for (x = 0; x < xDim; x++) {
                            index = x + j;

                            if (shortMask[index] != -1) {
                                xPosGeo[i] += x;
                                yPosGeo[i] += y;
                                xPosGrav[i] += greenBuffer[index] * x;
                                yPosGrav[i] += greenBuffer[index] * y;
                                colorCount[i] += greenBuffer[index];
                                voiCount[i]++;
                                IDArray[index] = (byte) objectID[i];
                            }
                        }
                    }

                    xPosGeo[i] /= voiCount[i];
                    yPosGeo[i] /= voiCount[i];
                    xPosGrav[i] /= colorCount[i];
                    yPosGrav[i] /= colorCount[i];
                } // redCount < greenCount

                
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)
        
        rIndex = new int[3];
        rCount = new float[3];
        gIndex = new int[3];
        gCount = new float[3];
        VOIs2 = new ViewVOIVector();
        objectID2 = new int[nVOIs];
        xPosGeo2 = new float[nVOIs];
        yPosGeo2 = new float[nVOIs];
        xPosGrav2 = new float[nVOIs];
        yPosGrav2 = new float[nVOIs];
        voiCount2 = new int[nVOIs];
        index2 = 0;
        for (j = 1; j <= numObjects; j++) {
            numRedFound = 0;
            numGreenFound = 0;
            rCount[0] = 0.0f;
            rCount[1] = 0.0f;
            rCount[2] = 0.0f;
            gCount[0] = 0.0f;
            gCount[1] = 0.0f;
            gCount[2] = 0.0f;
            for (i = 0; i < nVOIs; i++) {
                if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) && (objectID[i] == j)) {
                    if (isRed[i]) {
                        if (numRedFound < 3) {
                            numRedFound++;
                            if (numRedFound > redNumber) {
                                redNumber = numRedFound;
                            }
                        }
                        placed = false;
                        for (k = 0; k <= 2 && (!placed); k++) {
                            if (colorCount[i] >= rCount[k]) {
                                placed = true; 
                                for (m = numRedFound - 2; m >= k; m--) {
                                    rIndex[m+1] = rIndex[m];
                                    rCount[m+1] = rCount[m];
                                }
                                rIndex[k] = i;
                                rCount[k] = colorCount[i];
                            }
                        }
                    }
                    else {
                        if (numGreenFound < 3) {
                            numGreenFound++;
                            if (numGreenFound > greenNumber) {
                                greenNumber = numGreenFound;
                            }
                        }
                        placed = false;
                        for (k = 0; k <= 2 && (!placed); k++) {
                            if (colorCount[i] >= gCount[k]) {
                                placed = true; 
                                for (m = numGreenFound - 2; m >= k; m--) {
                                    gIndex[m+1] = gIndex[m];
                                    gCount[m+1] = gCount[m];
                                }
                                gIndex[k] = i;
                                gCount[k] = colorCount[i];
                            }
                        }
                    }
                }
            } // for (i = 0; i < nVOIs; i++)
            for (k = 0; k < numRedFound; k++) {
                VOIs.VOIAt(rIndex[k]).setName(j + "R" + (k+1));
                if (k == 0) {
                    VOIs.VOIAt(rIndex[k]).setColor(Color.yellow);
                }
                else {
                    VOIs.VOIAt(rIndex[k]).setColor(Color.yellow.darker());
                }
                VOIs2.addElement(VOIs.VOIAt(rIndex[k]));
                objectID2[index2] = j;
                xPosGeo2[index2] = xPosGeo[rIndex[k]];
                yPosGeo2[index2] = yPosGeo[rIndex[k]];
                xPosGrav2[index2] = xPosGrav[rIndex[k]];
                yPosGrav2[index2] = yPosGrav[rIndex[k]];
                voiCount2[index2] = voiCount[rIndex[k]];
                index2++;
            }
            for (k = 0; k < numGreenFound; k++) {
                VOIs.VOIAt(gIndex[k]).setName(j + "G" + (k+1));
                if (k == 0) {
                    VOIs.VOIAt(gIndex[k]).setColor(Color.pink);
                }
                else {
                    VOIs.VOIAt(gIndex[k]).setColor(Color.pink.darker());
                }
                VOIs2.addElement(VOIs.VOIAt(gIndex[k]));
                objectID2[index2] = j;
                xPosGeo2[index2] = xPosGeo[gIndex[k]];
                yPosGeo2[index2] = yPosGeo[gIndex[k]];
                xPosGrav2[index2] = xPosGrav[gIndex[k]];
                yPosGrav2[index2] = yPosGrav[gIndex[k]];
                voiCount2[index2] = voiCount[gIndex[k]];
                index2++;
            }
        } // (j = 1; j <= numObjects; j++)
        VOIs.clear();
        nVOIs = VOIs2.size();
        for (i = 0; i < nVOIs; i++) {
            VOIs.addElement(VOIs2.VOIAt(i));
        }
        VOIs2.clear();
        VOIs2 = null;
        for (i = 0; i < nVOIs; i++) {
            objectID[i] = objectID2[i];
            xPosGeo[i] = xPosGeo2[i];
            yPosGeo[i] = yPosGeo2[i];
            xPosGrav[i] = xPosGrav2[i];
            yPosGrav[i] = yPosGrav2[i];
            voiCount[i] = voiCount2[i];
        }
        objectID2 = null;
        xPosGeo2 = null;
        yPosGeo2 = null;
        xPosGrav2 = null;
        yPosGrav2 = null;
        voiCount2 = null;
        
        for (i = 0; i < nVOIs; i++) {
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                voiName = VOIs.VOIAt(i).getName();
                newPtVOI = new VOI((short) (i + nVOIs), "point2D" + i + ".voi", VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xArr[0] = xPosGrav[i];
                yArr[0] = yPosGrav[i];
                zArr[0] = 0.0f;
                newPtVOI.importCurve(xArr, yArr, zArr);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(voiName);
                srcImage.registerVOI(newPtVOI);
            }
        }

        dilateArray = new byte[length];
        boundaryArray = new byte[length];

        xCenter = new float[numObjects];
        yCenter = new float[numObjects];
        centerToNearEdge = new float[numObjects];
        centerToFarEdge = new float[numObjects];

        kernel = AlgorithmMorphology2D.CONNECTED8;
        method = AlgorithmMorphology2D.DILATE;
        itersDilation = 1;
        blueCountTotal = new float[numObjects];

        for (id = 1; id <= numObjects; id++) {
            fireProgressStateChanged("Processing object " + id + " of " + numObjects + " in blue segmented image");

            if (nVOIs > 0) {
                fireProgressStateChanged(85 + (id * 10 / numObjects));
            } else {
                fireProgressStateChanged(25 + (id * 3 / numObjects));
            }

            Arrays.fill(byteBuffer, (byte) 0);
            xCenter[id - 1] = 0.0f;
            yCenter[id - 1] = 0.0f;
            lowestSqr = Float.MAX_VALUE;
            highestSqr = -Float.MAX_VALUE;

            for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                for (x = 0; x < xDim; x++) {
                    i = x + j;

                    if (IDArray[i] == id) {
                        byteBuffer[i] = (byte) 1;
                        xCenter[id - 1] += x;
                        yCenter[id - 1] += y;
                        blueCountTotal[id - 1]++;
                    }
                }
            }

            xCenter[id - 1] /= blueCountTotal[id - 1];
            yCenter[id - 1] /= blueCountTotal[id - 1];

            try {
                grayImage.importData(0, byteBuffer, true);
            } catch (IOException error) {
                byteBuffer = null;
                IDArray = null;
                errorCleanUp("Error on grayImage.importData", true);

                return;
            }

            dilateAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                   itersErosion, numPruningPixels, edgingType, wholeImage);
            dilateAlgo.run();
            dilateAlgo.finalize();
            dilateAlgo = null;

            try {
                grayImage.exportData(0, length, dilateArray);
            } catch (IOException error) {
                byteBuffer = null;
                IDArray = null;
                dilateArray = null;
                errorCleanUp("Error on grayImage.importData", true);

                return;
            }

            for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                for (x = 0; x < xDim; x++) {
                    i = x + j;

                    if ((dilateArray[i] == (byte) 1) && (byteBuffer[i] == (byte) 0)) {
                        boundaryArray[i] = (byte) id;
                        distSqr = ((xCenter[id - 1] - x) * (xCenter[id - 1] - x) * xRes * xRes) +
                                  ((yCenter[id - 1] - y) * (yCenter[id - 1] - y) * yRes * yRes);

                        if (distSqr < lowestSqr) {
                            lowestSqr = distSqr;
                        }

                        if (distSqr > highestSqr) {
                            highestSqr = distSqr;
                        }
                    }
                }
            }

            centerToNearEdge[id - 1] = (float) Math.sqrt(lowestSqr);
            centerToFarEdge[id - 1] = (float) Math.sqrt(highestSqr);
            Preferences.debug("Object id = " + id + "\n");
            Preferences.debug("Center of mass = (" + xCenter[id - 1] + ", " + yCenter[id - 1] + ")\n");
            Preferences.debug("Distance from center of mass to near edge = " + centerToNearEdge[id - 1] + "\n");
            Preferences.debug("Distance from center of mass to far edge = " + centerToFarEdge[id - 1] + "\n");
        } // for (id = 1; id <= numObjects; id++)

        grayImage.disposeLocal();
        grayImage = null;


        for (i = 0; i < numObjects; i++) {
            newPtVOI = new VOI((short) (i + (2 * nVOIs)), "point2D" + i + ".voi", VOI.POINT, -1.0f);
            newPtVOI.setColor(Color.magenta);
            xArr[0] = xCenter[i];
            yArr[0] = yCenter[i];
            zArr[0] = 0.0f;
            newPtVOI.importCurve(xArr, yArr, zArr);
            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel("N" + (i + 1));
            srcImage.registerVOI(newPtVOI);
        } // for (i = 0; i < numObjects; i++)

        srcImage.notifyImageDisplayListeners();

        UI.setDataText("Plugin 10/19/07 version\n");
        UI.setDataText(srcImage.getFileInfo(0).getFileName() + "\n");

        if (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
            UI.setDataText("X resolution = " + xRes + "  " + (Unit.getUnitFromLegacyNum(xUnits)).toString() + "\n");
        } else {
            UI.setDataText("X resolution = " + xRes + "\n");
        }

        if (yUnits != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
            UI.setDataText("Y resolution = " + yRes + "  " + (Unit.getUnitFromLegacyNum(yUnits)).toString() + "\n\n");
        } else {
            UI.setDataText("Y resolution = " + yRes + "\n\n");
        }

        UI.setDataText("Nucleus\tNarea\tRmin\tRmax");

        if (redNumber > 0) {
            UI.setDataText("\tR1area");
            UI.setDataText("\tR1geomax\tR1geo%\tR1geo-Np");
            UI.setDataText("\tR1gravmax\tR1grav%\tR1grav-Np");
        }

        if (redNumber > 1) {
            UI.setDataText("\tR2area");
            UI.setDataText("\tR2geomax\tR2geo%\tR2geo-Np");
            UI.setDataText("\tR2gravmax\tR2grav%\tR2grav-Np");
        }

        if (redNumber > 2) {
            UI.setDataText("\tR3area");
            UI.setDataText("\tR3geomax\tR3geo%\tR3geo-Np");
            UI.setDataText("\tR3gravmax\tR3grav%\tR3grav-Np");
        }

        if (greenNumber > 0) {
            UI.setDataText("\tG1area");
            UI.setDataText("\tG1geomax\tG1geo%\tG1geo-Np");
            UI.setDataText("\tG1gravmax\tG1grav%\tG1grav-Np");
        }

        if (greenNumber > 1) {
            UI.setDataText("\tG2area");
            UI.setDataText("\tG2geomax\tG2geo%\tG2geo-Np");
            UI.setDataText("\tG2gravmax\tG2grav%\tG2grav-Np");
        }

        if (greenNumber > 2) {
            UI.setDataText("\tG3area");
            UI.setDataText("\tG3geomax\tG3geo%\tG3geo-Np");
            UI.setDataText("\tG3gravmax\tG3grav%\tG3grav-Np");
        }

        UI.setDataText("\n");

        for (i = 0; i < numObjects; i++) {
            nuclearArea = xRes * yRes * blueCountTotal[i];
            UI.setDataText((i + 1) + "\t" + nf.format(nuclearArea));
            UI.setDataText("\t" + nf.format(centerToNearEdge[i]));

            // equivalentRadius = (float)Math.sqrt(nuclearArea/Math.PI);
            // UI.setDataText("\t" + nf.format(equivalentRadius));
            UI.setDataText("\t" + nf.format(centerToFarEdge[i]));
            redFound = 0;
            greenFound = 0;

            for (j = 0; j <= (nVOIs - 1); j++) {

                if (objectID[j] == (i + 1)) {
                    voiType = VOIs.VOIAt(j).getName().charAt(1);

                    if (voiType == 'R') {
                        redFound++;
                    }

                    if (voiType == 'G') {
                        greenFound++;

                        if ((greenFound == 1) && (redFound != redNumber)) {

                            for (k = 0; k < (redNumber - redFound); k++) {
                                UI.setDataText("\t\t\t\t\t\t\t");
                            } // for (k = 0; k < (redNumber-redFound); k++)
                        } // if ((greenFound == 1) && (redFound != redNumber))
                    }

                    voiArea = xRes * yRes * voiCount[j];
                    UI.setDataText("\t" + nf.format(voiArea));

                    geoToCenter = (float)
                                      Math.sqrt(((xPosGeo[j] - xCenter[i]) * (xPosGeo[j] - xCenter[i]) * xRes * xRes) +
                                                ((yPosGeo[j] - yCenter[i]) * (yPosGeo[j] - yCenter[i]) * yRes * yRes));

                    initialdx = xPosGeo[j] - xCenter[i];
                    initialdy = yPosGeo[j] - yCenter[i];
                    lastEdgeX = (int) Math.round(xPosGeo[j]);
                    lastEdgeY = (int) Math.round(yPosGeo[j]);

                    if (Math.abs(initialdx) >= Math.abs(initialdy)) {

                        if (initialdx >= 0.0f) {
                            incx = 0.1f;
                        } else {
                            incx = -0.1f;
                        }

                        incy = 0.1f * initialdy / Math.abs(initialdx);
                    } else {

                        if (initialdy >= 0.0f) {
                            incy = 0.1f;
                        } else {
                            incy = -0.1f;
                        }

                        incx = 0.1f * initialdx / Math.abs(initialdy);
                    }

                    numInc = 1;

                    while (true) {
                        newEdgeX = (int) Math.round(xPosGeo[j] + (numInc * incx));

                        if ((newEdgeX < 0) || (newEdgeX >= xDim)) {
                            break;
                        }

                        newEdgeY = (int) Math.round(yPosGeo[j] + (numInc * incy));

                        if ((newEdgeY < 0) || (newEdgeY >= yDim)) {
                            break;
                        }

                        numInc++;

                        if ((newEdgeX == lastEdgeX) && (newEdgeY == lastEdgeY)) {
                            continue;
                        }

                        index = newEdgeX + (xDim * newEdgeY);
                        lastEdgeX = newEdgeX;
                        lastEdgeY = newEdgeY;

                        if (IDArray[index] != (i + 1)) {
                            break;
                        }
                    } // while (true)

                    centerToGeoToEdge = (float)
                                            Math.sqrt(((lastEdgeX - xCenter[i]) * (lastEdgeX - xCenter[i]) * xRes *
                                                           xRes) +
                                                      ((lastEdgeY - yCenter[i]) * (lastEdgeY - yCenter[i]) * yRes *
                                                           yRes));
                    UI.setDataText("\t" + nf.format(centerToGeoToEdge));
                    geoToCenter = 100.0f * geoToCenter / centerToGeoToEdge;
                    UI.setDataText("\t" + nf.format(geoToCenter));
                    geoToEdge = (float)
                                    Math.sqrt(((lastEdgeX - xPosGeo[j]) * (lastEdgeX - xPosGeo[j]) * xRes * xRes) +
                                              ((lastEdgeY - yPosGeo[j]) * (lastEdgeY - yPosGeo[j]) * yRes * yRes));
                    UI.setDataText("\t" + nf.format(geoToEdge));


                    gravToCenter = (float)
                                       Math.sqrt(((xPosGrav[j] - xCenter[i]) * (xPosGrav[j] - xCenter[i]) * xRes *
                                                      xRes) +
                                                 ((yPosGrav[j] - yCenter[i]) * (yPosGrav[j] - yCenter[i]) * yRes *
                                                      yRes));

                    initialdx = xPosGrav[j] - xCenter[i];
                    initialdy = yPosGrav[j] - yCenter[i];
                    lastEdgeX = (int) Math.round(xPosGrav[j]);
                    lastEdgeY = (int) Math.round(yPosGrav[j]);

                    if (Math.abs(initialdx) >= Math.abs(initialdy)) {

                        if (initialdx >= 0.0f) {
                            incx = 0.1f;
                        } else {
                            incx = -0.1f;
                        }

                        incy = 0.1f * initialdy / Math.abs(initialdx);
                    } else {

                        if (initialdy >= 0.0f) {
                            incy = 0.1f;
                        } else {
                            incy = -0.1f;
                        }

                        incx = 0.1f * initialdx / Math.abs(initialdy);
                    }

                    numInc = 1;

                    while (true) {
                        newEdgeX = (int) Math.round(xPosGrav[j] + (numInc * incx));

                        if ((newEdgeX < 0) || (newEdgeX >= xDim)) {
                            break;
                        }

                        newEdgeY = (int) Math.round(yPosGrav[j] + (numInc * incy));

                        if ((newEdgeY < 0) || (newEdgeY >= yDim)) {
                            break;
                        }

                        numInc++;

                        if ((newEdgeX == lastEdgeX) && (newEdgeY == lastEdgeY)) {
                            continue;
                        }

                        index = newEdgeX + (xDim * newEdgeY);
                        lastEdgeX = newEdgeX;
                        lastEdgeY = newEdgeY;

                        if (IDArray[index] != (i + 1)) {
                            break;
                        }
                    } // while (true)

                    centerToGravToEdge = (float)
                                             Math.sqrt(((lastEdgeX - xCenter[i]) * (lastEdgeX - xCenter[i]) * xRes *
                                                            xRes) +
                                                       ((lastEdgeY - yCenter[i]) * (lastEdgeY - yCenter[i]) * yRes *
                                                            yRes));
                    UI.setDataText("\t" + nf.format(centerToGravToEdge));
                    gravToCenter = 100.0f * gravToCenter / centerToGravToEdge;
                    UI.setDataText("\t" + nf.format(gravToCenter));
                    gravToEdge = (float)
                                     Math.sqrt(((lastEdgeX - xPosGrav[j]) * (lastEdgeX - xPosGrav[j]) * xRes * xRes) +
                                               ((lastEdgeY - yPosGrav[j]) * (lastEdgeY - yPosGrav[j]) * yRes * yRes));
                    UI.setDataText("\t" + nf.format(gravToEdge));


                    /*lowestSqr = Float.MAX_VALUE;
                     * for (j1 = 0, y = 0; y < yDim; y++, j1 += xDim) { for (x = 0; x < xDim; x++) {     index = x + j1;
                     *  if (boundaryArray[index] == objectID[j]) {         distSqr = (xPosGeo[j] - x) * (xPosGeo[j] - x)
                     *                   xRes * xRes +                   (yPosGeo[j] - y) * (yPosGeo[j] - y) * yRes *
                     * yRes;         if (distSqr < lowestSqr) {             lowestSqr = distSqr; xEdge = x;       yEdge
                     * = y;         }     } } } geoToEdge = (float) Math.sqrt(lowestSqr); UI.setDataText("\t\t" +
                     * nf.format(geoToEdge)); geoToEdge *= 100.0f /centerToFarEdge[i]; UI.setDataText("\t" +
                     * nf.format(geoToEdge));
                     *
                     * lowestSqr = Float.MAX_VALUE; for (j1 = 0, y = 0; y < yDim; y++, j1 += xDim) { for (x = 0; x < xDim;
                     * x++) {     index = x + j1;     if (boundaryArray[index] == objectID[j]) {         distSqr =
                     * (xPosGrav[j] - x) * (xPosGrav[j] - x) *                   xRes * xRes + (yPosGrav[j] - y) *
                     * (yPosGrav[j] - y) *                   yRes * yRes;         if (distSqr < lowestSqr) { lowestSqr =
                     * distSqr;             xEdge = x;             yEdge = y;  }     } } } gravToEdge = (float)
                     * Math.sqrt(lowestSqr); UI.setDataText("\t\t" + nf.format(gravToEdge)); gravToEdge *= 100.0f
                     * /centerToFarEdge[i];UI.setDataText("\t" +
                     * nf.format(gravToEdge));*/
                } // if (objectID[j] == (i+1))
            } // for (j = 0; j <= nVOIs - 1; j++)

            UI.setDataText("\n");
        } // for (i = 0; i < numObjects; i++)

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    private void calc3D() {

        int totLength, sliceLength;
        float[] buffer;
        AlgorithmFuzzyCMeans fcmAlgo;
        ModelImage grayImage;
        int nClasses;
        int nPyramid;
        int oneJacobiIter;
        int twoJacobiIter;
        float q;
        float oneSmooth;
        float twoSmooth;
        boolean outputGainField;
        int segmentation;
        boolean cropBackground;
        float threshold;
        int maxIter;
        float endTolerance;
        boolean wholeImage;
        float[] centroids;
        float min;
        float max;
        AlgorithmThresholdDual thresholdAlgo;
        float[] thresholds;
        float fillValue;
        AlgorithmMorphology3D openAlgo;
        AlgorithmMorphology3D closeAlgo;
        AlgorithmMorphology3D erosionAlgo;
        AlgorithmMorphology3D dilationAlgo;
        int kernel;
        float sphereDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        int i, j, k;
        int x, y, z;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
        float zRes = srcImage.getResolutions(0)[2];
        boolean found;
        AlgorithmMorphology3D idObjectsAlgo3D;
        int numObjects;
        byte[] byteBuffer;
        byte[] IDArray;
        int id;
        byte[] dilateArray;
        byte[] boundaryArray;
        AlgorithmMorphology3D dilateAlgo;
        float[] xCenter;
        float[] yCenter;
        float[] zCenter;
        float[] centerToNearEdge;
        float[] centerToFarEdge;
        float[] greenBuffer = null;
        ViewVOIVector VOIs = null;
        int nVOIs;
        ViewVOIVector VOIs2 = null;
        short[] shortMask;
        int index;
        int redCount;
        int greenCount;
        int[] idCount;
        int[] objectID;
        int[] objectID2;
        int objectCount;
        float[] xPosGeo;
        float[] yPosGeo;
        float[] zPosGeo;
        float[] xPosGeo2;
        float[] yPosGeo2;
        float[] zPosGeo2;
        float[] xPosGrav;
        float[] yPosGrav;
        float[] zPosGrav;
        float[] xPosGrav2;
        float[] yPosGrav2;
        float[] zPosGrav2;
        float colorCount[];
        float geoToCenter;
        float gravToCenter;
        float geoToEdge;
        float gravToEdge;
        ViewUserInterface UI = ViewUserInterface.getReference();
        float distSqr;
        float lowestSqr;
        float highestSqr;

        // int xEdge = -1;
        // int yEdge = -1;
        // int zEdge = -1;
        int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        int zUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
        FileInfoBase fileInfo;
        int numRedObjects = 0;
        int numGreenObjects = 0;
        byte[] redIDArray = null;
        byte[] greenIDArray = null;
        float[] redIntensityTotal;
        float[] greenIntensityTotal;
        float[] redXCenter;
        float[] redYCenter;
        float[] redZCenter;
        float[] greenXCenter;
        float[] greenYCenter;
        float[] greenZCenter;
        int[] redNucleusNumber = null;
        int[][] sortedRedIndex = null;
        float[][] sortedRedIntensity;
        float[][] sortedRedXCenter;
        float[][] sortedRedYCenter;
        float[][] sortedRedZCenter;
        int[] greenNucleusNumber = null;
        int[][] sortedGreenIndex = null;
        float[][] sortedGreenIntensity;
        float[][] sortedGreenXCenter;
        float[][] sortedGreenYCenter;
        float[][] sortedGreenZCenter;
        AlgorithmVOIExtraction algoVOIExtraction;
        VOI newPtVOI;
        float[] xArr = new float[1];
        float[] yArr = new float[1];
        float[] zArr = new float[1];
        int numVOIObjects;
        Color[] colorTable = null;
        boolean removeID;
        int m;
        AlgorithmMedian algoMedian;
        int medianIters;
        int kernelSize;
        int kernelShape;
        float stdDev;
        boolean sliceBySlice;
        String[] nameTable = null;
        String voiName;
        int[] blueCountTotal = null;
        ModelImage grayImage2D;
        int[] extents2D = new int[2];
        byte[] buffer2D;
        AlgorithmMorphology2D fillHolesAlgo2D;
        AlgorithmMorphology3D fillHolesAlgo3D;
        int[] voiCount;
        int[] voiCount2;

        // int j1;
        // int k1;
        int redFound;
        int greenFound;
        char voiType;
        float nuclearVolume;

        // float equivalentRadius;
        NumberFormat nf;
        float voiVolume;
        float initialdx;
        float initialdy;
        float initialdz;
        int lastEdgeX;
        int lastEdgeY;
        int lastEdgeZ;
        int newEdgeX;
        int newEdgeY;
        int newEdgeZ;
        float incx;
        float incy;
        float incz;
        int numInc;
        float centerToGeoToEdge;
        float centerToGravToEdge;
        int moreRedObjects;
        int moreGreenObjects;
        int[] redObjectNucleus;
        int[] greenObjectNucleus;
        int maxNucleus;
        int maxCount;
        int secondMaxNucleus;
        int secondMaxCount;
        int thirdMaxNucleus;
        int thirdMaxCount;
        int numRedVOIObjects;
        int numGreenVOIObjects;
        int edgeObjects;
        AlgorithmHistogram algoHist;
        int bins;
        int[] histoBuffer;
        double[] lowValue;
        int totalCount;
        int countsToRetain;
        int countsFound;
        int offset;
        int idm1;
        int[] volume;
        boolean adaptiveSize = false;
        int maximumSize = 0;

        // centroids
        double[] cx;
        double[] cy;
        double[] cz;

        // Second order moments
        double[] ixx;
        double[] iyy;
        double[] izz;
        double[] iyz;
        double[] ixy;
        double[] izx;
        double xdiff;
        double ydiff;
        double zdiff;
        double[][] tensor = new double[3][3];
        double[] eigenvalue = new double[3];;
        double[][] eigenvector = new double[3][3];;
        double temp;
        double ellipVol;
        double normFactor;
        int n;
        double[] tempCol = new double[3];
        float[][] sAxisCen;
        float scaleMax;
        float invMax;
        Vector3f kVoxel;
        AlgorithmEllipsoidFit kF;
        Vector<Vector3f>[] volPoints;
        double[] axes;
        float[][] sAxisPer;
        float tempf;
        boolean initiallyOneObject;
        int response;
        boolean isRed[];
        int numRedFound;
        int numGreenFound;
        int rIndex[];
        float rCount[];
        int gIndex[];
        float gCount[];
        boolean placed;
        int index2;

        nf = NumberFormat.getNumberInstance();
        nf.setMinimumFractionDigits(3);
        nf.setMaximumFractionDigits(3);

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();


        for (i = nVOIs - 1; i >=0; i--) {

            if (VOIs.VOIAt(i).getCurveType() != VOI.CONTOUR) {
                VOIs.remove(i);
            }
        }
        
        nVOIs = VOIs.size();
        
        if (nVOIs > 0) {
            // Increase redNumber and greenNumber based on what is found in supplied VOIs
            redNumber = 1;
            greenNumber = 1;
        }

        fireProgressStateChanged("Processing image ...");

        try {
            sliceLength = xDim * yDim;
            totLength = sliceLength * zDim;
            buffer = new float[totLength];
            fireProgressStateChanged("Creating blue image");
            srcImage.exportRGBData(3, 0, totLength, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm RegionDistance: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm RegionDistance: Out of memory creating process buffer", true);

            return;
        }

        grayImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_gray");

        for (i = 0; i < srcImage.getExtents()[2]; i++) {
            fileInfo = grayImage.getFileInfo()[i];
            fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
            fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            grayImage.setFileInfo(fileInfo, i);
        } // for (i = 0; i < srcImage.getExtents()[2]; i++)

        try {
            grayImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }

        // Segment into 2 values
        fireProgressStateChanged("Performing FuzzyCMeans Segmentation on blue");

        if (nVOIs > 0) {
            fireProgressStateChanged(5);
        } else {
            fireProgressStateChanged(2);
        }

        nClasses = 2;
        nPyramid = 4;
        oneJacobiIter = 1;
        twoJacobiIter = 2;
        q = 2.0f;
        oneSmooth = 2e4f;
        twoSmooth = 2e5f;
        outputGainField = false;
        segmentation = HARD_ONLY;
        cropBackground = false;
        threshold = 0.0f;
        maxIter = 200;
        endTolerance = 0.01f;
        wholeImage = true;

        // grayImage enters ModelStorageBase.FLOAT and returns ModelStorageBase.UBYTE
        fcmAlgo = new AlgorithmFuzzyCMeans(grayImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q, oneSmooth,
                                           twoSmooth, outputGainField, segmentation, cropBackground, threshold, maxIter,
                                           endTolerance, wholeImage);
        centroids = new float[2];
        min = (float) grayImage.getMin();
        max = (float) grayImage.getMax();
        centroids[0] = min + ((max - min) / 3.0f);
        centroids[1] = min + (2.0f * (max - min) / 3.0f);
        fcmAlgo.setCentroids(centroids);
        fcmAlgo.run();
        fcmAlgo.finalize();
        fcmAlgo = null;
        System.gc();

        // Now convert the min and max to 0 and 1
        fireProgressStateChanged("Setting segmented blue values to 0 and 1");

        if (nVOIs > 0) {
            fireProgressStateChanged(20);
        } else {
            fireProgressStateChanged(6);
        }

        grayImage.calcMinMax();
        max = (float) grayImage.getMax();
        thresholds = new float[2];
        thresholds[0] = max;
        thresholds[1] = max;
        fillValue = 0.0f;

         /*ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
         new Dimension(600, 300));
         boolean runTest = true;
         if (runTest) {
             setCompleted(true);
             return;
         }*/

        thresholdAlgo = new AlgorithmThresholdDual(grayImage, thresholds, fillValue, AlgorithmThresholdDual.BINARY_TYPE,
                                                   wholeImage, false);
        thresholdAlgo.run();
        thresholdAlgo.finalize();
        thresholdAlgo = null;
        System.gc();
        /*ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
        new Dimension(600, 300));
        boolean runTest = true;
        if (runTest) {
            setCompleted(true);
            return;
        }*/

        // Do a slice by slice hole filling operation on the blue image
        fireProgressStateChanged("Slice by slice hole filling on blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(23);
        } else {
            fireProgressStateChanged(7);
        }

        extents2D[0] = srcImage.getExtents()[0];
        extents2D[1] = srcImage.getExtents()[1];
        buffer2D = new byte[sliceLength];
        grayImage2D = new ModelImage(ModelStorageBase.USHORT, extents2D, srcImage.getImageName() + "_gray2D");

        fileInfo = grayImage2D.getFileInfo()[0];
        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        grayImage2D.setFileInfo(fileInfo, 0);
        wholeImage = true;

        for (z = 0; z < zDim; z++) {

            try {
                grayImage.exportData(z * sliceLength, sliceLength, buffer2D);
            } catch (IOException e) {
                buffer = null;
                errorCleanUp("Algorithm RegionDistance: Error on grayImage exportData", true);

                return;
            }

            try {
                grayImage2D.importData(0, buffer2D, true);
            } catch (IOException e) {
                buffer = null;
                errorCleanUp("Algorithm RegionDistance: Error on grayImage2D importData", true);

                return;
            }

            fillHolesAlgo2D = new AlgorithmMorphology2D(grayImage2D, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0,
                                                        wholeImage);
            fillHolesAlgo2D.run();
            fillHolesAlgo2D.finalize();
            fillHolesAlgo2D = null;

            try {
                grayImage2D.exportData(0, sliceLength, buffer2D);
            } catch (IOException e) {
                buffer = null;
                errorCleanUp("Algorithm RegionDistance: Error on grayImage2D exportData", true);

                return;
            }

            try {
                grayImage.importData(z * sliceLength, buffer2D, false);
            } catch (IOException e) {
                buffer = null;
                errorCleanUp("Algorithm RegionDistance: Error on grayImage importData", true);

                return;
            }

        } // for (z = 0; z < zDim; z++)

        grayImage.calcMinMax();
        grayImage2D.disposeLocal();
        grayImage2D = null;
        buffer2D = null;
        System.gc();

        /*ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
        new Dimension(600, 300));
        boolean runTest = true;
        if (runTest) {
            setCompleted(true);
            return;
        }*/

        // Smooth with a morphological opening followed by a closing
        fireProgressStateChanged("Opening blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(25);
        } else {
            fireProgressStateChanged(8);
        }

        kernel = AlgorithmMorphology3D.CONNECTED6;
        sphereDiameter = 1.0f;
        method = AlgorithmMorphology3D.OPEN;
        itersDilation = 1;
        itersErosion = 1;
        numPruningPixels = 0;
        edgingType = 0;
        openAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation, itersErosion,
                                             numPruningPixels, edgingType, wholeImage);
        openAlgo.run();
        openAlgo.finalize();
        openAlgo = null;

        /*ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
        new Dimension(600, 300));
        boolean runTest = true;
        if (runTest) {
            setCompleted(true);
            return;
        }*/

        fireProgressStateChanged("Closing blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(30);
        } else {
            fireProgressStateChanged(10);
        }

        method = AlgorithmMorphology3D.CLOSE;
        closeAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation, itersErosion,
                                              numPruningPixels, edgingType, wholeImage);
        closeAlgo.run();
        closeAlgo.finalize();
        closeAlgo = null;
        System.gc();
        
        /*ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
        new Dimension(600, 300));
        boolean runTest = true;
        if (runTest) {
            setCompleted(true);
            return;
        }*/

        byteBuffer = new byte[totLength];

        try {
            grayImage.exportData(0, totLength, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

        fireProgressStateChanged("Eroding blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(35);
        } else {
            fireProgressStateChanged(12);
        }

        kernel = AlgorithmMorphology3D.CONNECTED6;
        method = AlgorithmMorphology3D.ERODE;
        itersDilation = 0;
        itersErosion = iters;
        erosionAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation, itersErosion,
                                                numPruningPixels, edgingType, wholeImage);
        erosionAlgo.run();
        erosionAlgo.finalize();
        erosionAlgo = null;

        /*ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
        new Dimension(600, 300));
        boolean runTest = true;
        if (runTest) {
            setCompleted(true);
            return;
        }*/

        fireProgressStateChanged("IDing objects in blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(45);
        } else {
            fireProgressStateChanged(15);
        }

        kernel = AlgorithmMorphology3D.SIZED_SPHERE;
        sphereDiameter = 0.0f;
        method = AlgorithmMorphology3D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo3D = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, wholeImage);
        idObjectsAlgo3D.setMinMax(blueMin, 20000000);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;
        System.gc();

        grayImage.calcMinMax();

        // ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
        // new Dimension(600, 300), srcImage.getUserInterface());
        numObjects = (int) grayImage.getMax();

        fireProgressStateChanged("Dilating IDed objects in blue segmented image");

        if (nVOIs > 0) {
            fireProgressStateChanged(55);
        } else {
            fireProgressStateChanged(18);
        }

        kernel = AlgorithmMorphology3D.CONNECTED6;
        method = AlgorithmMorphology3D.DILATE;
        itersDilation = 6 * iters;
        itersErosion = 0;
        dilationAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation, itersErosion,
                                                 numPruningPixels, edgingType, wholeImage);
        dilationAlgo.run();
        dilationAlgo.finalize();
        dilationAlgo = null;
        System.gc();

        IDArray = new byte[totLength];

        try {
            grayImage.exportData(0, totLength, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

        System.out.println("Number of Objects = " + numObjects);

        fireProgressStateChanged("Zeroing overgrowth of dilated objects");

        if (nVOIs > 0) {
            fireProgressStateChanged(65);
        } else {
            fireProgressStateChanged(22);
        }

        // byteBuffer contains the smoothed blue objects before erosion
        // IDArray contains the objects after erosion and dilation
        for (i = 0; i < totLength; i++) {

            if (byteBuffer[i] == (byte) 0) {
                IDArray[i] = (byte) 0;
            }
        }

        /*try {
         *      grayImage.importData(0, IDArray, true); } catch (IOException error) {     byteBuffer = null;     IDArray
         * = null;     errorCleanUp("Error on grayImage.importData", true);     return; }
         *
         * ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,     new Dimension(600, 300),
         * srcImage.getUserInterface());*/

        edgeObjects = numObjects;
        fireProgressStateChanged("Removing blue objects touching edges");

        if (nVOIs > 0) {
            fireProgressStateChanged(70);
        } else {
            fireProgressStateChanged(23);
        }

        if (numObjects == 1) {
            initiallyOneObject = true;
        }
        else {
            initiallyOneObject = false;
        }
        
        for (id = numObjects; id >= 1; id--) {
            removeID = false;

            for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                for (j = k, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        i = x + j;

                        if ((IDArray[i] == id) &&
                                ((x == 0) || (x == (xDim - 1)) || (y == 0) || (y == (yDim - 1)) || (z == 0) ||
                                     (z == (zDim - 1)))) {
                            removeID = true;
                        }
                    }
                }
            }
            if (initiallyOneObject && removeID) {
                response = JOptionPane.showConfirmDialog(UI.getMainFrame(), "Delete the one blue object with edge touching?", 
                           "Deletion", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
                if (response == JOptionPane.NO_OPTION) {
                    removeID = false;
                }
            } // if (initiallyOneObject && removeID)

            if (removeID) {

                for (i = 0; i < totLength; i++) {

                    if (IDArray[i] == id) {
                        IDArray[i] = 0;
                    }
                    else if (IDArray[i] > id) {
                        IDArray[i]--;
                    }
                }

                numObjects--;
            } // if (removeID)

        } // for (id = numObjects; id >= 1; id--)

        if (numObjects == 0) {
            MipavUtil.displayError(edgeObjects + " blue objects touched edges");
            MipavUtil.displayError("No blue objects that do not touch edges");

            setCompleted(false);

            return;
        }

        if (greenNumber > 0) {
            greenBuffer = new float[totLength];
        } // if (greenNumber > 0)

        try {

            if (redNumber > 0) {
                srcImage.exportRGBData(1, 0, totLength, buffer); // export red data
            } // if (redNumber > 0)

            if (greenNumber > 0) {
                srcImage.exportRGBData(2, 0, totLength, greenBuffer); // export green data
            } // if (greenNumber > 0)
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            errorCleanUp("Algorithm RegionDistance reports: source image locked", true);

            return;
        }

        if (nVOIs == 0) {

            if (redNumber > 0) {
                fireProgressStateChanged("Creating red image");

                // grayImage is ModelStorageBase.UBYTE
                grayImage.reallocate(ModelStorageBase.FLOAT);
                fireProgressStateChanged(30);

                try {
                    grayImage.importData(0, buffer, true);
                } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Error on grayImage.importData", true);

                    return;
                }

                fireProgressStateChanged("Performing median filter on red");
                fireProgressStateChanged(31);
                medianIters = 1;
                kernelSize = 3;
                kernelShape = AlgorithmMedian.CUBE_KERNEL;
                stdDev = 0.0f;
                sliceBySlice = false;
                wholeImage = true;
                algoMedian = new AlgorithmMedian(grayImage, medianIters, kernelSize, kernelShape, stdDev, adaptiveSize,
                                                 maximumSize, sliceBySlice, wholeImage);
                algoMedian.run();
                algoMedian.finalize();
                algoMedian = null;

                /*ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
                new Dimension(600, 300));
                boolean runTest = true;
                if (runTest) {
                    setCompleted(true);
                    return;
                }*/

                fireProgressStateChanged("Getting histogram info on red");
                fireProgressStateChanged(32);
                bins = 256;
                algoHist = new AlgorithmHistogram(grayImage, bins);
                algoHist.run();
                histoBuffer = algoHist.getHistoBuffer();
                lowValue = algoHist.getLowValue();
                algoHist.finalize();
                algoHist = null;

                totalCount = histoBuffer[0];

                for (i = 1; i < histoBuffer.length; i++) {
                    totalCount += histoBuffer[i];
                }

                countsToRetain = (int) Math.round(redFraction * totalCount);
                found = false;
                countsFound = 0;
                threshold = 1.0f;

                for (i = bins - 1; (i >= 0) && !found; i--) {
                    countsFound += histoBuffer[i];

                    if (countsFound >= countsToRetain) {
                        found = true;
                        threshold = (float) lowValue[i];
                    }
                }

                cropBackground = true;

                // Segment red into 3 values
                fireProgressStateChanged("Performing FuzzyCMeans Segmentation on red");
                fireProgressStateChanged(33);
                nClasses = 3;
                nPyramid = 4;
                oneJacobiIter = 1;
                twoJacobiIter = 2;
                q = 2.0f;
                oneSmooth = 2e4f;
                twoSmooth = 2e5f;
                outputGainField = false;
                segmentation = HARD_ONLY;
                maxIter = 200;
                endTolerance = 0.01f;
                wholeImage = true;

                // grayImage enters ModelStorageBase.FLOAT and returns ModelStorageBase.UBYTE
                fcmAlgo = new AlgorithmFuzzyCMeans(grayImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q,
                                                   oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground,
                                                   threshold, maxIter, endTolerance, wholeImage);
                centroids = new float[3];

                // Find the first value above the 0 value for min
                min = Float.MAX_VALUE;

                for (i = 0; i < totLength; i++) {

                    if ((buffer[i] < min) && (buffer[i] != 0.0f)) {
                        min = buffer[i];
                    }
                }

                max = (float) grayImage.getMax();
                centroids[0] = min + ((max - min) / 4.0f);
                centroids[1] = min + (2.0f * (max - min) / 4.0f);
                centroids[2] = min + (3.0f * (max - min) / 4.0f);
                fcmAlgo.setCentroids(centroids);
                fcmAlgo.run();
                fcmAlgo.finalize();
                fcmAlgo = null;
                System.gc();

                /*ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
                new Dimension(600, 300));
                boolean runTest = true;
                if (runTest) {
                    setCompleted(true);
                    return;
                }*/

                // Now convert the red min and max to 0 and 1
                fireProgressStateChanged("Setting segmented red values to 0 and 1");
                fireProgressStateChanged(35);
                grayImage.calcMinMax();
                max = (float) grayImage.getMax();
                thresholds[0] = max;
                thresholds[1] = max;
                fillValue = 0.0f;
                thresholdAlgo = new AlgorithmThresholdDual(grayImage, thresholds, fillValue,
                                                           AlgorithmThresholdDual.BINARY_TYPE, wholeImage, false);
                thresholdAlgo.run();
                thresholdAlgo.finalize();
                thresholdAlgo = null;

                // Remove all inappropriate holes in red VOIs
                fireProgressStateChanged("Removing holes from red VOIs");
                fireProgressStateChanged(37);
                fillHolesAlgo3D = new AlgorithmMorphology3D(grayImage, 0, 0, AlgorithmMorphology3D.FILL_HOLES, 0, 0, 0,
                                                            0, wholeImage);
                fillHolesAlgo3D.run();
                fillHolesAlgo3D.finalize();
                fillHolesAlgo3D = null;

                // Smooth red with a morphological opening followed by a closing
                fireProgressStateChanged("Opening red segmented image");
                fireProgressStateChanged(40);
                kernel = AlgorithmMorphology3D.CONNECTED6;
                sphereDiameter = 1.0f;
                method = AlgorithmMorphology3D.OPEN;
                itersDilation = 1;
                itersErosion = 1;
                numPruningPixels = 0;
                edgingType = 0;
                openAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                     itersErosion, numPruningPixels, edgingType, wholeImage);
                openAlgo.run();
                openAlgo.finalize();
                openAlgo = null;

                fireProgressStateChanged("Closing red segmented image");
                fireProgressStateChanged(42);
                method = AlgorithmMorphology3D.CLOSE;
                closeAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                      itersErosion, numPruningPixels, edgingType, wholeImage);
                closeAlgo.run();
                closeAlgo.finalize();
                closeAlgo = null;

                // Put the red VOI IDs in redIDArray
                fireProgressStateChanged("IDing objects in red segmented image");
                fireProgressStateChanged(44);
                kernel = AlgorithmMorphology3D.SIZED_SPHERE;
                sphereDiameter = 0.0f;
                method = AlgorithmMorphology3D.ID_OBJECTS;
                itersDilation = 0;
                itersErosion = 0;
                idObjectsAlgo3D = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                            itersErosion, numPruningPixels, edgingType, wholeImage);
                idObjectsAlgo3D.setMinMax(redMin, 2000000);
                idObjectsAlgo3D.run();
                idObjectsAlgo3D.finalize();
                idObjectsAlgo3D = null;
                System.gc();

                grayImage.calcMinMax();
                numRedObjects = (int) grayImage.getMax();
                redIDArray = new byte[totLength];

                /*ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
                new Dimension(600, 300));
                boolean runTest = true;
                if (runTest) {
                    setCompleted(true);
                    return;
                }*/

                try {
                    grayImage.exportData(0, totLength, redIDArray);
                } catch (IOException error) {
                    byteBuffer = null;
                    redIDArray = null;
                    errorCleanUp("Error on grayImage.exportData", true);

                    return;
                }

                // Split redIDArray objects apart into 2 or 3 redIDArray objects
                // if they go between 2 or 3 cells
                moreRedObjects = 0;
                redObjectNucleus = new int[numObjects + 1];

                for (i = 1; i <= numRedObjects; i++) {

                    for (j = 0; j <= numObjects; j++) {
                        redObjectNucleus[j] = 0;
                    } // for (j = 0; j <= numObjects; j++)

                    for (j = 0; j < totLength; j++) {

                        if (redIDArray[j] == i) {
                            redObjectNucleus[IDArray[j]]++;
                        } // if (redIDArray[j] == i)
                    } // for (j = 0; j < totLength; j++)

                    maxNucleus = 1;
                    maxCount = redObjectNucleus[1];

                    for (j = 2; j <= numObjects; j++) {

                        if (redObjectNucleus[j] > maxCount) {
                            maxNucleus = j;
                            maxCount = redObjectNucleus[j];
                        }
                    } // for (j = 2; j <= numObjects; j++)

                    secondMaxCount = 0;
                    secondMaxNucleus = 0;

                    for (j = 1; j <= numObjects; j++) {

                        if (j != maxNucleus) {

                            if (redObjectNucleus[j] > secondMaxCount) {
                                secondMaxNucleus = j;
                                secondMaxCount = redObjectNucleus[j];
                            } // if (redObjectNucleus[j] > secondMaxCount)
                        } // if (j != maxNucleus)
                    } // for (j = 1; j <= numObjects; j++)

                    if (secondMaxCount > 0) {

                        if (secondMaxCount >= redMin) {
                            moreRedObjects++;

                            for (j = 0; j < totLength; j++) {

                                if ((redIDArray[j] == i) && (IDArray[j] == secondMaxNucleus)) {
                                    redIDArray[j] = (byte) (numRedObjects + moreRedObjects);
                                }
                            } // for (j = 0; j < totLength; j++)

                            thirdMaxCount = 0;
                            thirdMaxNucleus = 0;

                            for (j = 1; j <= numObjects; j++) {

                                if ((j != maxNucleus) && (j != secondMaxNucleus)) {

                                    if (redObjectNucleus[j] > thirdMaxCount) {
                                        thirdMaxNucleus = j;
                                        thirdMaxCount = redObjectNucleus[j];
                                    } // if (redObjectNucleus[j] > thirdMaxCount)
                                } // if ((j != maxNucleus) && (j != secondMaxNucleus))
                            } // for (j = 1; j <= numObjects; j++)

                            if (thirdMaxCount > 0) {

                                if (thirdMaxCount >= redMin) {
                                    moreRedObjects++;

                                    for (j = 0; j < totLength; j++) {

                                        if ((redIDArray[j] == i) && (IDArray[j] == thirdMaxNucleus)) {
                                            redIDArray[j] = (byte) (numRedObjects + moreRedObjects);
                                        }
                                    } // for (j = 0; j < totLength; j++)
                                } else { // if (thirdMaxCount >= redMin),  thirdMaxCount < redMin

                                    for (j = 0; j < totLength; j++) {

                                        if ((redIDArray[j] == i) && (IDArray[j] != 0) && (IDArray[j] != maxNucleus) &&
                                                (IDArray[j] != secondMaxNucleus)) {
                                            redIDArray[j] = (byte) 0;
                                        }
                                    } // for (j = 0; j < totLength; j++)
                                } // else thirdMaxCount < redMin
                            } // if (thirdMaxCount > 0)
                        } else { // if (secondMaxCount >= redMin),  secondMaxCount < redMin

                            for (j = 0; j < totLength; j++) {

                                if ((redIDArray[j] == i) && (IDArray[j] != 0) && (IDArray[j] != maxNucleus)) {
                                    redIDArray[j] = (byte) 0;
                                }
                            } // for (j = 0; j < totLength; j++)
                        } // secondMaxCount < redMin
                    } // if (secondMaxCount > 0)
                } // for (i = 1; i <= numRedObjects; i++)

                numRedObjects = numRedObjects + moreRedObjects;
            } // if (redNumber > 0)

            if (greenNumber > 0) {
                fireProgressStateChanged("Creating green image");
                fireProgressStateChanged(46);

                // grayImage is ModelStorageBase.UBYTE
                grayImage.reallocate(ModelStorageBase.FLOAT);

                try {
                    grayImage.importData(0, greenBuffer, true);
                } catch (IOException error) {
                    greenBuffer = null;
                    errorCleanUp("Error on grayImage.importData", true);

                    return;
                }

                fireProgressStateChanged("Performing median filter on green");
                fireProgressStateChanged(47);
                medianIters = 1;
                kernelSize = 3;
                kernelShape = AlgorithmMedian.CUBE_KERNEL;
                stdDev = 0.0f;
                sliceBySlice = false;
                wholeImage = true;
                algoMedian = new AlgorithmMedian(grayImage, medianIters, kernelSize, kernelShape, stdDev, adaptiveSize,
                                                 maximumSize, sliceBySlice, wholeImage);
                algoMedian.run();
                algoMedian.finalize();
                algoMedian = null;
                System.gc();

                fireProgressStateChanged("Getting histogram info on green");
                fireProgressStateChanged(48);
                bins = 256;
                algoHist = new AlgorithmHistogram(grayImage, bins);
                algoHist.run();
                histoBuffer = algoHist.getHistoBuffer();
                lowValue = algoHist.getLowValue();
                algoHist.finalize();
                algoHist = null;

                totalCount = histoBuffer[0];

                for (i = 1; i < histoBuffer.length; i++) {
                    totalCount += histoBuffer[i];
                }

                countsToRetain = (int) Math.round(greenFraction * totalCount);
                found = false;
                countsFound = 0;
                threshold = 1.0f;

                for (i = bins - 1; (i >= 0) && !found; i--) {
                    countsFound += histoBuffer[i];

                    if (countsFound >= countsToRetain) {
                        found = true;
                        threshold = (float) lowValue[i];
                    }
                }

                cropBackground = true;

                // Segment green into 3 values
                fireProgressStateChanged("Performing FuzzyCMeans Segmentation on green");
                fireProgressStateChanged(49);
                nClasses = 3;
                nPyramid = 4;
                oneJacobiIter = 1;
                twoJacobiIter = 2;
                q = 2.0f;
                oneSmooth = 2e4f;
                twoSmooth = 2e5f;
                outputGainField = false;
                segmentation = HARD_ONLY;
                maxIter = 200;
                endTolerance = 0.01f;
                wholeImage = true;

                // grayImage enters ModelStorageBase.FLOAT and returns ModelStorageBase.UBYTE
                fcmAlgo = new AlgorithmFuzzyCMeans(grayImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q,
                                                   oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground,
                                                   threshold, maxIter, endTolerance, wholeImage);
                centroids = new float[3];

                // Find the first value above the 0 value for min
                min = Float.MAX_VALUE;

                for (i = 0; i < totLength; i++) {

                    if ((greenBuffer[i] < min) && (greenBuffer[i] != 0.0f)) {
                        min = greenBuffer[i];
                    }
                }

                max = (float) grayImage.getMax();
                centroids[0] = min + ((max - min) / 4.0f);
                centroids[1] = min + (2.0f * (max - min) / 4.0f);
                centroids[2] = min + (3.0f * (max - min) / 4.0f);
                fcmAlgo.setCentroids(centroids);
                fcmAlgo.run();
                fcmAlgo.finalize();
                fcmAlgo = null;
                System.gc();

                // ViewJFrameImage testFrame2 = new ViewJFrameImage(grayImage, null,
                // new Dimension(600, 300), srcImage.getUserInterface());

                // Now convert the green min and max to 0 and 1
                fireProgressStateChanged("Setting segmented green values to 0 and 1");
                fireProgressStateChanged(50);
                grayImage.calcMinMax();
                max = (float) grayImage.getMax();
                thresholds[0] = max;
                thresholds[1] = max;
                fillValue = 0.0f;
                thresholdAlgo = new AlgorithmThresholdDual(grayImage, thresholds, fillValue,
                                                           AlgorithmThresholdDual.BINARY_TYPE, wholeImage, false);
                thresholdAlgo.run();
                thresholdAlgo.finalize();
                thresholdAlgo = null;
                System.gc();

                // Remove all inappropriate holes in green VOIs
                fireProgressStateChanged("Removing holes from green VOIs");
                fireProgressStateChanged(52);
                fillHolesAlgo3D = new AlgorithmMorphology3D(grayImage, 0, 0, AlgorithmMorphology3D.FILL_HOLES, 0, 0, 0,
                                                            0, wholeImage);
                fillHolesAlgo3D.run();
                fillHolesAlgo3D.finalize();
                fillHolesAlgo3D = null;
                System.gc();

                // Smooth green with a morphological opening followed by a closing
                fireProgressStateChanged("Opening green segmented image");
                fireProgressStateChanged(54);
                kernel = AlgorithmMorphology3D.CONNECTED6;
                sphereDiameter = 1.0f;
                method = AlgorithmMorphology3D.OPEN;
                itersDilation = 1;
                itersErosion = 1;
                numPruningPixels = 0;
                edgingType = 0;
                openAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                     itersErosion, numPruningPixels, edgingType, wholeImage);
                openAlgo.run();
                openAlgo.finalize();
                openAlgo = null;

                fireProgressStateChanged("Closing green segmented image");
                fireProgressStateChanged(56);
                method = AlgorithmMorphology3D.CLOSE;
                closeAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                      itersErosion, numPruningPixels, edgingType, wholeImage);
                closeAlgo.run();
                closeAlgo.finalize();
                closeAlgo = null;
                System.gc();

                // Put the green VOI IDs in greenIDArray
                fireProgressStateChanged("IDing objects in green segmented image");
                fireProgressStateChanged(58);
                kernel = AlgorithmMorphology3D.SIZED_SPHERE;
                sphereDiameter = 0.0f;
                method = AlgorithmMorphology3D.ID_OBJECTS;
                itersDilation = 0;
                itersErosion = 0;
                idObjectsAlgo3D = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                            itersErosion, numPruningPixels, edgingType, wholeImage);
                idObjectsAlgo3D.setMinMax(greenMin, 2000000);
                idObjectsAlgo3D.run();
                idObjectsAlgo3D.finalize();
                idObjectsAlgo3D = null;
                System.gc();

                grayImage.calcMinMax();
                numGreenObjects = (int) grayImage.getMax();
                greenIDArray = new byte[totLength];

                try {
                    grayImage.exportData(0, totLength, greenIDArray);
                } catch (IOException error) {
                    byteBuffer = null;
                    greenIDArray = null;
                    errorCleanUp("Error on grayImage.exportData", true);

                    return;
                }

                // Split greenIDArray objects apart into 2 or 3 greenIDArray objects
                // if they go between 2 or 3 cells
                moreGreenObjects = 0;
                greenObjectNucleus = new int[numObjects + 1];

                for (i = 1; i <= numGreenObjects; i++) {

                    for (j = 0; j <= numObjects; j++) {
                        greenObjectNucleus[j] = 0;
                    } // for (j = 0; j <= numObjects; j++)

                    for (j = 0; j < totLength; j++) {

                        if (greenIDArray[j] == i) {
                            greenObjectNucleus[IDArray[j]]++;
                        } // if (greenIDArray[j] == i)
                    } // for (j = 0; j < totLength; j++)

                    maxNucleus = 1;
                    maxCount = greenObjectNucleus[1];

                    for (j = 2; j <= numObjects; j++) {

                        if (greenObjectNucleus[j] > maxCount) {
                            maxNucleus = j;
                            maxCount = greenObjectNucleus[j];
                        }
                    } // for (j = 2; j <= numObjects; j++)

                    secondMaxCount = 0;
                    secondMaxNucleus = 0;

                    for (j = 1; j <= numObjects; j++) {

                        if (j != maxNucleus) {

                            if (greenObjectNucleus[j] > secondMaxCount) {
                                secondMaxNucleus = j;
                                secondMaxCount = greenObjectNucleus[j];
                            } // if (greenObjectNucleus[j] > secondMaxCount)
                        } // if (j != maxNucleus)
                    } // for (j = 1; j <= numObjects; j++)

                    if (secondMaxCount > 0) {

                        if (secondMaxCount >= greenMin) {
                            moreGreenObjects++;

                            for (j = 0; j < totLength; j++) {

                                if ((greenIDArray[j] == i) && (IDArray[j] == secondMaxNucleus)) {
                                    greenIDArray[j] = (byte) (numGreenObjects + moreGreenObjects);
                                }
                            } // for (j = 0; j < totLength; j++)

                            thirdMaxCount = 0;
                            thirdMaxNucleus = 0;

                            for (j = 1; j <= numObjects; j++) {

                                if ((j != maxNucleus) && (j != secondMaxNucleus)) {

                                    if (greenObjectNucleus[j] > thirdMaxCount) {
                                        thirdMaxNucleus = j;
                                        thirdMaxCount = greenObjectNucleus[j];
                                    } // if (greenObjectNucleus[j] > thirdMaxCount)
                                } // if ((j != maxNucleus) && (j != secondMaxNucleus))
                            } // for (j = 1; j <= numObjects; j++)

                            if (thirdMaxCount > 0) {

                                if (thirdMaxCount >= greenMin) {
                                    moreGreenObjects++;

                                    for (j = 0; j < totLength; j++) {

                                        if ((greenIDArray[j] == i) && (IDArray[j] == thirdMaxNucleus)) {
                                            greenIDArray[j] = (byte) (numGreenObjects + moreGreenObjects);
                                        }
                                    } // for (j = 0; j < totLength; j++)
                                } else { // if (thirdMaxCount >= greenMin),  thirdMaxCount < greenMin

                                    for (j = 0; j < totLength; j++) {

                                        if ((greenIDArray[j] == i) && (IDArray[j] != 0) && (IDArray[j] != maxNucleus) &&
                                                (IDArray[j] != secondMaxNucleus)) {
                                            greenIDArray[j] = (byte) 0;
                                        }
                                    } // for (j = 0; j < totLength; j++)
                                } // else thirdMaxCount < greenMin
                            } // if (thirdMaxCount > 0)
                        } else { // if (secondMaxCount >= greenMin),  secondMaxCount < greenMin

                            for (j = 0; j < totLength; j++) {

                                if ((greenIDArray[j] == i) && (IDArray[j] != 0) && (IDArray[j] != maxNucleus)) {
                                    greenIDArray[j] = (byte) 0;
                                }
                            } // for (j = 0; j < totLength; j++)
                        } // secondMaxCount < greenMin
                    } // if (secondMaxCount > 0)
                } // for (i = 1; i <= numGreenObjects; i++)

                numGreenObjects = numGreenObjects + moreGreenObjects;
            } // if (greenNumber > 0)

            if (redNumber > 0) {

                // Sort the red objects within each nucleus.
                // Create no more than redNumber red objects within each nucleus.
                fireProgressStateChanged("Sorting red objects by intensity count");
                fireProgressStateChanged(60);
                redIntensityTotal = new float[numRedObjects];
                redXCenter = new float[numRedObjects];
                redYCenter = new float[numRedObjects];
                redZCenter = new float[numRedObjects];
                redNucleusNumber = new int[numObjects];
                sortedRedIndex = new int[numObjects][redNumber];
                sortedRedIntensity = new float[numObjects][redNumber];
                sortedRedXCenter = new float[numObjects][redNumber];
                sortedRedYCenter = new float[numObjects][redNumber];
                sortedRedZCenter = new float[numObjects][redNumber];

                for (j = 1; j <= numRedObjects; j++) {

                    for (i = 0, x = 0, y = 0, z = 0; i < totLength; i++) {

                        if (redIDArray[i] == j) {
                            redIntensityTotal[j - 1] += buffer[i];
                            redXCenter[j - 1] += buffer[i] * x;
                            redYCenter[j - 1] += buffer[i] * y;
                            redZCenter[j - 1] += buffer[i] * z;
                        }

                        x++;

                        if (x == xDim) {
                            x = 0;
                            y++;
                        }

                        if (y == yDim) {
                            y = 0;
                            z++;
                        }
                    } // for (i = 0, x = 0, y = 0, z = 0; i < totLength; i++)

                    redXCenter[j - 1] = redXCenter[j - 1] / redIntensityTotal[j - 1];
                    redYCenter[j - 1] = redYCenter[j - 1] / redIntensityTotal[j - 1];
                    redZCenter[j - 1] = redZCenter[j - 1] / redIntensityTotal[j - 1];
                    i = (int) redXCenter[j - 1] + ((int) (redYCenter[j - 1]) * xDim) +
                        ((int) redZCenter[j - 1] * sliceLength);
                    id = IDArray[i];

                    if (id >= 1) {

                        if (redNucleusNumber[id - 1] < redNumber) {
                            redNucleusNumber[id - 1]++;
                        }

                        found = false;

                        for (i = 0; (i < redNucleusNumber[id - 1]) && !found; i++) {

                            if (redIntensityTotal[j - 1] >= sortedRedIntensity[id - 1][i]) {
                                found = true;

                                for (k = redNucleusNumber[id - 1] - 2; k >= i; k--) {
                                    sortedRedIntensity[id - 1][k + 1] = sortedRedIntensity[id - 1][k];
                                    sortedRedIndex[id - 1][k + 1] = sortedRedIndex[id - 1][k];
                                    sortedRedXCenter[id - 1][k + 1] = sortedRedXCenter[id - 1][k];
                                    sortedRedYCenter[id - 1][k + 1] = sortedRedYCenter[id - 1][k];
                                    sortedRedZCenter[id - 1][k + 1] = sortedRedZCenter[id - 1][k];
                                }

                                sortedRedIntensity[id - 1][i] = redIntensityTotal[j - 1];
                                sortedRedIndex[id - 1][i] = j; // from redIDArray
                                sortedRedXCenter[id - 1][i] = redXCenter[j - 1];
                                sortedRedYCenter[id - 1][i] = redYCenter[j - 1];
                                sortedRedZCenter[id - 1][i] = redZCenter[j - 1];
                            }
                        } // for (i = 0; i < redNucleusNumber[id-1]  && !found; i++)
                    } // if (id >= 1)
                } // for (j = 1; j <= numRedObjects; j++)

                redIntensityTotal = null;
                redXCenter = null;
                redYCenter = null;
                redZCenter = null;

                for (i = 0; i < numObjects; i++) {
                    sortedRedIntensity[i] = null;
                    sortedRedXCenter[i] = null;
                    sortedRedYCenter[i] = null;
                    sortedRedZCenter[i] = null;
                }

                sortedRedIntensity = null;
                sortedRedXCenter = null;
                sortedRedYCenter = null;
                sortedRedZCenter = null;
            } // if (redNumber > 0)

            if (greenNumber > 0) {

                // Sort the green objects within each nucleus.
                // Create no more than greenNumber green objects within each nucleus.
                greenIntensityTotal = new float[numGreenObjects];
                greenXCenter = new float[numGreenObjects];
                greenYCenter = new float[numGreenObjects];
                greenZCenter = new float[numGreenObjects];
                greenNucleusNumber = new int[numObjects];
                sortedGreenIndex = new int[numObjects][greenNumber];
                sortedGreenIntensity = new float[numObjects][greenNumber];
                sortedGreenXCenter = new float[numObjects][greenNumber];
                sortedGreenYCenter = new float[numObjects][greenNumber];
                sortedGreenZCenter = new float[numObjects][greenNumber];

                for (j = 1; j <= numGreenObjects; j++) {
                    fireProgressStateChanged("Sorting green object " + j + " of " + numGreenObjects +
                                             " by intensity count");
                    fireProgressStateChanged(62 + (10 * j / numGreenObjects));

                    for (x = 0, y = 0, z = 0, i = 0; i < totLength; i++) {

                        if (greenIDArray[i] == j) {
                            greenIntensityTotal[j - 1] += greenBuffer[i];
                            greenXCenter[j - 1] += greenBuffer[i] * x;
                            greenYCenter[j - 1] += greenBuffer[i] * y;
                            greenZCenter[j - 1] += greenBuffer[i] * z;
                        }

                        x++;

                        if (x == xDim) {
                            x = 0;
                            y++;
                        }

                        if (y == yDim) {
                            y = 0;
                            z++;
                        }
                    }

                    greenXCenter[j - 1] = greenXCenter[j - 1] / greenIntensityTotal[j - 1];
                    greenYCenter[j - 1] = greenYCenter[j - 1] / greenIntensityTotal[j - 1];
                    greenZCenter[j - 1] = greenZCenter[j - 1] / greenIntensityTotal[j - 1];
                    i = (int) greenXCenter[j - 1] + ((int) (greenYCenter[j - 1]) * xDim) +
                        ((int) greenZCenter[j - 1] * sliceLength);
                    id = IDArray[i];

                    if (id >= 1) {

                        if (greenNucleusNumber[id - 1] < greenNumber) {
                            greenNucleusNumber[id - 1]++;
                        }

                        found = false;

                        for (i = 0; (i < greenNucleusNumber[id - 1]) && !found; i++) {

                            if (greenIntensityTotal[j - 1] >= sortedGreenIntensity[id - 1][i]) {
                                found = true;

                                for (k = greenNucleusNumber[id - 1] - 2; k >= i; k--) {
                                    sortedGreenIntensity[id - 1][k + 1] = sortedGreenIntensity[id - 1][k];
                                    sortedGreenIndex[id - 1][k + 1] = sortedGreenIndex[id - 1][k];
                                    sortedGreenXCenter[id - 1][k + 1] = sortedGreenXCenter[id - 1][k];
                                    sortedGreenYCenter[id - 1][k + 1] = sortedGreenYCenter[id - 1][k];
                                    sortedGreenZCenter[id - 1][k + 1] = sortedGreenZCenter[id - 1][k];
                                }

                                sortedGreenIntensity[id - 1][i] = greenIntensityTotal[j - 1];
                                sortedGreenIndex[id - 1][i] = j; // from greenIDArray
                                sortedGreenXCenter[id - 1][i] = greenXCenter[j - 1];
                                sortedGreenYCenter[id - 1][i] = greenYCenter[j - 1];
                                sortedGreenZCenter[id - 1][i] = greenZCenter[j - 1];
                            }
                        } // for (i = 0; i < greenNucleusNumber[id-1]  && !found; i++)
                    } // if (id >= 1)
                } // for (j = 1; j <= numGreenObjects; j++)

                greenIntensityTotal = null;
                greenXCenter = null;
                greenYCenter = null;
                greenZCenter = null;

                for (i = 0; i < numObjects; i++) {
                    sortedGreenIntensity[i] = null;
                    sortedGreenXCenter[i] = null;
                    sortedGreenYCenter[i] = null;
                    sortedGreenZCenter[i] = null;
                }

                sortedGreenIntensity = null;
                sortedGreenXCenter = null;
                sortedGreenYCenter = null;
                sortedGreenZCenter = null;
            } // if (greenNumber > 0)

            Arrays.fill(byteBuffer, (byte) 0);

            numRedVOIObjects = 0;
            numGreenVOIObjects = 0;

            for (i = 0; i < numObjects; i++) {

                if (redNumber > 0) {
                    numRedVOIObjects += redNucleusNumber[i];
                } // if (redNumber > 0)

                if (greenNumber > 0) {
                    numGreenVOIObjects += greenNucleusNumber[i];
                } // if (greenNumber > 0)
            }

            numVOIObjects = Math.max(numRedVOIObjects, numGreenVOIObjects);
            colorTable = new Color[numVOIObjects];
            nameTable = new String[numVOIObjects];

            if (redNumber > 0) {

                for (i = 0, j = 0; i < numObjects; i++) {

                    for (m = 0; m < redNumber; m++) {

                        if (redNucleusNumber[i] > m) {

                            for (k = 0; k < totLength; k++) {

                                if (redIDArray[k] == sortedRedIndex[i][m]) {
                                    byteBuffer[k] = (byte) (j + 1);
                                }
                            } // for (k = 0; k < totLength; k++)

                            if (m == 0) {
                                colorTable[j] = Color.yellow;
                            } else {
                                colorTable[j] = Color.yellow.darker();
                            }

                            nameTable[j] = new String((i + 1) + "R" + (m + 1));
                            j++;
                        } // if (redNucleusNumber[i] > m)
                    } // for (m = 0; m < redNumber; m++)
                } // for (i = 0, j = 0; i < numObjects; i++)

                for (i = 0; i < numObjects; i++) {
                    sortedRedIndex[i] = null;
                }

                sortedRedIndex = null;
                redIDArray = null;

                try {
                    grayImage.importData(0, byteBuffer, true);
                } catch (IOException error) {
                    byteBuffer = null;
                    errorCleanUp("Error on grayImage.importData", true);

                    return;
                }

                fireProgressStateChanged("Extracting VOIs from red image");
                fireProgressStateChanged(73);
                algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
                algoVOIExtraction.setColorTable(colorTable);
                algoVOIExtraction.setNameTable(nameTable);
                algoVOIExtraction.run();
                algoVOIExtraction.finalize();
                algoVOIExtraction = null;
                System.gc();

                srcImage.setVOIs(grayImage.getVOIs());
            } // if (redNumber > 0)

            if (greenNumber > 0) {
                Arrays.fill(byteBuffer, (byte) 0);

                for (i = 0, j = 0; i < numObjects; i++) {

                    for (m = 0; m < greenNumber; m++) {

                        if (greenNucleusNumber[i] > m) {

                            for (k = 0; k < totLength; k++) {

                                if (greenIDArray[k] == sortedGreenIndex[i][m]) {
                                    byteBuffer[k] = (byte) (j + 1);
                                }
                            } // for (k = 0; k < totLength; k++)

                            if (m == 0) {
                                colorTable[j] = Color.pink;
                            } else {
                                colorTable[j] = Color.pink.darker();
                            }

                            nameTable[j] = new String((i + 1) + "G" + (m + 1));
                            j++;
                        } // if (greenNucleusNumber[i] > m)
                    } // for (m = 0; m < greenNumber; m++)
                } // for (i = 0, j = 0; i < numObjects; i++)

                for (i = 0; i < numObjects; i++) {
                    sortedGreenIndex[i] = null;
                }

                sortedGreenIndex = null;
                greenIDArray = null;

                grayImage.resetVOIs();

                try {
                    grayImage.importData(0, byteBuffer, true);
                } catch (IOException error) {
                    byteBuffer = null;
                    errorCleanUp("Error on grayImage.importData", true);

                    return;
                }

                fireProgressStateChanged("Extracting VOIs from green image");
                fireProgressStateChanged(74);
                algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
                algoVOIExtraction.setColorTable(colorTable);
                algoVOIExtraction.setNameTable(nameTable);
                algoVOIExtraction.run();
                algoVOIExtraction.finalize();
                algoVOIExtraction = null;
                System.gc();

                if (redNumber > 0) {
                    srcImage.addVOIs(grayImage.getVOIs());
                } else {
                    srcImage.setVOIs(grayImage.getVOIs());
                }
            } // if (greenNumber > 0)

            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
        } // if (nVOIs == 0)

        shortMask = new short[totLength];

        // System.out.println("Image = " + srcImage);
        // System.out.println("VOIs = " + VOIs);

        idCount = new int[numObjects];
        xPosGeo = new float[nVOIs];
        yPosGeo = new float[nVOIs];
        zPosGeo = new float[nVOIs];
        xPosGrav = new float[nVOIs];
        yPosGrav = new float[nVOIs];
        zPosGrav = new float[nVOIs];
        objectID = new int[nVOIs];
        isRed = new boolean[nVOIs];
        colorCount = new float[nVOIs];

        UI.clearAllDataText();
        UI.setDataText("\n");

        voiCount = new int[nVOIs];

        for (i = 0; i < nVOIs; i++) {
            Preferences.debug("i = " + i + "\n");
            fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
            fireProgressStateChanged(75 + (10 * (i + 1) / nVOIs));
            VOIs.VOIAt(i).setOnlyID((short) i);
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                Preferences.debug("VOI contour i = " + i + "\n");
                Arrays.fill(shortMask, (short) -1);

                shortMask = srcImage.generateVOIMask(shortMask, i);
                redCount = 0;
                greenCount = 0;
                Arrays.fill(idCount, 0);

                for (j = 0; j < totLength; j++) {

                    if (shortMask[j] != -1) {
                        if (redNumber >= 1) {
                            redCount += buffer[j];
                        }
                        if (greenNumber >= 1) {
                            greenCount += greenBuffer[j];
                        }

                        if (IDArray[j] > 0) {
                            index = IDArray[j] - 1;
                            idCount[index]++;
                        }
                    }
                }

                objectID[i] = 1;
                objectCount = idCount[0];

                for (j = 2; j <= numObjects; j++) {

                    if (idCount[j - 1] > objectCount) {
                        objectID[i] = j;
                        objectCount = idCount[j - 1];
                    }
                } // for (j = 2; j <= numObjects; j++)

                xPosGeo[i] = 0.0f;
                yPosGeo[i] = 0.0f;
                zPosGeo[i] = 0.0f;
                xPosGrav[i] = 0.0f;
                yPosGrav[i] = 0.0f;
                zPosGrav[i] = 0.0f;
                if (redCount >= greenCount) {
                    isRed[i] = true;
                    colorCount[i] = 0.0f;

                    for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                        for (j = k, y = 0; y < yDim; y++, j += xDim) {

                            for (x = 0; x < xDim; x++) {
                                index = x + j;

                                if (shortMask[index] != -1) {
                                    xPosGeo[i] += x;
                                    yPosGeo[i] += y;
                                    zPosGeo[i] += z;
                                    xPosGrav[i] += buffer[index] * x;
                                    yPosGrav[i] += buffer[index] * y;
                                    zPosGrav[i] += buffer[index] * z;
                                    colorCount[i] += buffer[index];
                                    voiCount[i]++;

                                    // Expand blue nuclei to include VOI
                                    IDArray[index] = (byte) objectID[i];
                                }
                            }
                        }
                    }

                    xPosGeo[i] /= voiCount[i];
                    yPosGeo[i] /= voiCount[i];
                    zPosGeo[i] /= voiCount[i];
                    xPosGrav[i] /= colorCount[i];
                    yPosGrav[i] /= colorCount[i];
                    zPosGrav[i] /= colorCount[i];
                } // if (redCount >= greenCount)
                else { // redCount < greenCount
                    colorCount[i] = 0.0f;

                    for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                        for (j = k, y = 0; y < yDim; y++, j += xDim) {

                            for (x = 0; x < xDim; x++) {
                                index = x + j;

                                if (shortMask[index] != -1) {
                                    xPosGeo[i] += x;
                                    yPosGeo[i] += y;
                                    zPosGeo[i] += z;
                                    xPosGrav[i] += greenBuffer[index] * x;
                                    yPosGrav[i] += greenBuffer[index] * y;
                                    zPosGrav[i] += greenBuffer[index] * z;
                                    colorCount[i] += greenBuffer[index];
                                    voiCount[i]++;

                                    // Expand blue nuclei to include VOI
                                    IDArray[index] = (byte) objectID[i];
                                }
                            }
                        }
                    }

                    xPosGeo[i] /= voiCount[i];
                    yPosGeo[i] /= voiCount[i];
                    zPosGeo[i] /= voiCount[i];
                    xPosGrav[i] /= colorCount[i];
                    yPosGrav[i] /= colorCount[i];
                    zPosGrav[i] /= colorCount[i];
                } // redCount < greenCount

                
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)
        
        rIndex = new int[3];
        rCount = new float[3];
        gIndex = new int[3];
        gCount = new float[3];
        VOIs2 = new ViewVOIVector();
        objectID2 = new int[nVOIs];
        xPosGeo2 = new float[nVOIs];
        yPosGeo2 = new float[nVOIs];
        zPosGeo2 = new float[nVOIs];
        xPosGrav2 = new float[nVOIs];
        yPosGrav2 = new float[nVOIs];
        zPosGrav2 = new float[nVOIs];
        voiCount2 = new int[nVOIs];
        index2 = 0;
        for (j = 1; j <= numObjects; j++) {
            numRedFound = 0;
            numGreenFound = 0;
            rCount[0] = 0.0f;
            rCount[1] = 0.0f;
            rCount[2] = 0.0f;
            gCount[0] = 0.0f;
            gCount[1] = 0.0f;
            gCount[2] = 0.0f;
            for (i = 0; i < nVOIs; i++) {
                if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) && (objectID[i] == j)) {
                    if (isRed[i]) {
                        if (numRedFound < 3) {
                            numRedFound++;
                            if (numRedFound > redNumber) {
                                redNumber = numRedFound;
                            }
                        }
                        placed = false;
                        for (k = 0; k <= 2 && (!placed); k++) {
                            if (colorCount[i] >= rCount[k]) {
                                placed = true; 
                                for (m = numRedFound - 2; m >= k; m--) {
                                    rIndex[m+1] = rIndex[m];
                                    rCount[m+1] = rCount[m];
                                }
                                rIndex[k] = i;
                                rCount[k] = colorCount[i];
                            }
                        }
                    }
                    else {
                        if (numGreenFound < 3) {
                            numGreenFound++;
                            if (numGreenFound > greenNumber) {
                                greenNumber = numGreenFound;
                            }
                        }
                        placed = false;
                        for (k = 0; k <= 2 && (!placed); k++) {
                            if (colorCount[i] >= gCount[k]) {
                                placed = true; 
                                for (m = numGreenFound - 2; m >= k; m--) {
                                    gIndex[m+1] = gIndex[m];
                                    gCount[m+1] = gCount[m];
                                }
                                gIndex[k] = i;
                                gCount[k] = colorCount[i];
                            }
                        }
                    }
                }
            } // for (i = 0; i < nVOIs; i++)
            for (k = 0; k < numRedFound; k++) {
                VOIs.VOIAt(rIndex[k]).setName(j + "R" + (k+1));
                if (k == 0) {
                    VOIs.VOIAt(rIndex[k]).setColor(Color.yellow);
                }
                else {
                    VOIs.VOIAt(rIndex[k]).setColor(Color.yellow.darker());
                }
                VOIs2.addElement(VOIs.VOIAt(rIndex[k]));
                objectID2[index2] = j;
                xPosGeo2[index2] = xPosGeo[rIndex[k]];
                yPosGeo2[index2] = yPosGeo[rIndex[k]];
                zPosGeo2[index2] = zPosGeo[rIndex[k]];
                xPosGrav2[index2] = xPosGrav[rIndex[k]];
                yPosGrav2[index2] = yPosGrav[rIndex[k]];
                zPosGrav2[index2] = zPosGrav[rIndex[k]];
                voiCount2[index2] = voiCount[rIndex[k]];
                index2++;
            }
            for (k = 0; k < numGreenFound; k++) {
                VOIs.VOIAt(gIndex[k]).setName(j + "G" + (k+1));
                if (k == 0) {
                    VOIs.VOIAt(gIndex[k]).setColor(Color.pink);
                }
                else {
                    VOIs.VOIAt(gIndex[k]).setColor(Color.pink.darker());
                }
                VOIs2.addElement(VOIs.VOIAt(gIndex[k]));
                objectID2[index2] = j;
                xPosGeo2[index2] = xPosGeo[gIndex[k]];
                yPosGeo2[index2] = yPosGeo[gIndex[k]];
                zPosGeo2[index2] = zPosGeo[gIndex[k]];
                xPosGrav2[index2] = xPosGrav[gIndex[k]];
                yPosGrav2[index2] = yPosGrav[gIndex[k]];
                zPosGrav2[index2] = zPosGrav[gIndex[k]];
                voiCount2[index2] = voiCount[gIndex[k]];
                index2++;
            }
        } // (j = 1; j <= numObjects; j++)
        VOIs.clear();
        nVOIs = VOIs2.size();
        for (i = 0; i < nVOIs; i++) {
            VOIs.addElement(VOIs2.VOIAt(i));
        }
        VOIs2.clear();
        VOIs2 = null;
        for (i = 0; i < nVOIs; i++) {
            objectID[i] = objectID2[i];
            xPosGeo[i] = xPosGeo2[i];
            yPosGeo[i] = yPosGeo2[i];
            zPosGeo[i] = zPosGeo2[i];
            xPosGrav[i] = xPosGrav2[i];
            yPosGrav[i] = yPosGrav2[i];
            zPosGrav[i] = zPosGrav2[i];
            voiCount[i] = voiCount2[i];
        }
        objectID2 = null;
        xPosGeo2 = null;
        yPosGeo2 = null;
        zPosGeo2 = null;
        xPosGrav2 = null;
        yPosGrav2 = null;
        zPosGrav2 = null;
        voiCount2 = null;
        
        for (i = 0; i < nVOIs; i++) {
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                voiName = VOIs.VOIAt(i).getName();
                newPtVOI = new VOI((short) (i + nVOIs), "point3D" + i + ".voi", VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xArr[0] = xPosGrav[i];
                yArr[0] = yPosGrav[i];
                zArr[0] = zPosGrav[i];
                newPtVOI.importCurve(xArr, yArr, zArr);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(voiName);
                srcImage.registerVOI(newPtVOI);    
            }
        }

        // Ellipse fitting blue objects with first and second moments
        fireProgressStateChanged("Ellipse fitting with moments");
        volume = new int[numObjects];
        cx = new double[numObjects];
        cy = new double[numObjects];
        cz = new double[numObjects];
        ixx = new double[numObjects];
        iyy = new double[numObjects];
        izz = new double[numObjects];
        iyz = new double[numObjects];
        ixy = new double[numObjects];
        izx = new double[numObjects];
        sAxisCen = new float[numObjects][3];

        for (z = 0; z < zDim; z++) {
            offset = z * sliceLength;

            for (y = 0; y < yDim; y++) {
                j = offset + (y * xDim);

                for (x = 0; x < xDim; x++) {
                    i = j + x;

                    if (IDArray[i] > 0) {
                        idm1 = IDArray[i] - 1;
                        volume[idm1]++;
                        cx[idm1] += x * xRes;
                        cy[idm1] += y * yRes;
                        cz[idm1] += z * zRes;
                    } // if (IDArray[i] > 0)
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)

        for (k = 0; k < numObjects; k++) {
            cx[k] = cx[k] / volume[k];
            cy[k] = cy[k] / volume[k];
            cz[k] = cz[k] / volume[k];
        } // for (k = 0; k < numObjects; k++)

        for (z = 0; z < zDim; z++) {
            offset = z * sliceLength;

            for (y = 0; y < yDim; y++) {
                j = offset + (y * xDim);

                for (x = 0; x < xDim; x++) {
                    i = j + x;

                    if (IDArray[i] > 0) {
                        idm1 = IDArray[i] - 1;
                        xdiff = (x * xRes) - cx[idm1];
                        ydiff = (y * yRes) - cy[idm1];
                        zdiff = (z * zRes) - cz[idm1];
                        ixx[idm1] += xdiff * xdiff;
                        iyy[idm1] += ydiff * ydiff;
                        izz[idm1] += zdiff * zdiff;
                        iyz[idm1] += ydiff * zdiff;
                        ixy[idm1] += xdiff * ydiff;
                        izx[idm1] += zdiff * xdiff;
                    } // if (IDArray[i] > 0)
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)

        for (k = 0; k < numObjects; k++) {
            ixx[k] = ixx[k] / volume[k];
            iyy[k] = iyy[k] / volume[k];
            izz[k] = izz[k] / volume[k];
            iyz[k] = iyz[k] / volume[k];
            ixy[k] = ixy[k] / volume[k];
            izx[k] = izx[k] / volume[k];
            tensor[0][0] = ixx[k];
            tensor[0][1] = ixy[k];
            tensor[0][2] = izx[k];
            tensor[1][0] = ixy[k];
            tensor[1][1] = iyy[k];
            tensor[1][2] = iyz[k];
            tensor[2][0] = izx[k];
            tensor[2][1] = iyz[k];
            tensor[2][2] = izz[k];
            // In EigenvalueDecomposition the columns represent the eigenvectors
            Eigenvalue.decompose(tensor, eigenvector, eigenvalue);

            // Arrange the eigenvalues and the corresponding eigenvectors
            // in ascending order so that e0 <= e1 <= e2
            for (m = 0; m < 3; m++) {
                index = m;

                for (n = m + 1; n < 3; n++) {

                    if (eigenvalue[n] < eigenvalue[m]) {
                        index = n;
                    }
                } // for (n = m+1; n < 3; n++)

                if (index != m) {
                    temp = eigenvalue[m];
                    eigenvalue[m] = eigenvalue[index];
                    eigenvalue[index] = temp;

                    for (n = 0; n < 3; n++) {
                        tempCol[n] = eigenvector[n][m];
                        eigenvector[n][m] = eigenvector[n][index];
                        eigenvector[n][index] = tempCol[n];
                    }
                } // if (index != m)
            } // for (m = 0; m < 3; m++)

            // Semi axes are proportional to the square root of the eigenvalues
            for (m = 0; m < 3; m++) {
                eigenvalue[m] = Math.sqrt(eigenvalue[m]);
            }

            // Calculate an unnormalized volume from the eigenvalues
            ellipVol = (4.0 / 3.0) * Math.PI * eigenvalue[0] * eigenvalue[1] * eigenvalue[2];

            // Calculate a normalizing factor with the actual volume
            normFactor = Math.pow((xRes * yRes * zRes * volume[k] / ellipVol), (1.0 / 3.0));

            // Normalize to obtain the actual semi axes
            for (m = 0; m < 3; m++) {
                sAxisCen[k][m] = (float) (normFactor * eigenvalue[m]);
            }
        } // for (k = 0; k < numObjects; k++)

        boundaryArray = new byte[totLength];
        dilateArray = new byte[totLength];
        xCenter = new float[numObjects];
        yCenter = new float[numObjects];
        zCenter = new float[numObjects];
        centerToNearEdge = new float[numObjects];
        centerToFarEdge = new float[numObjects];

        kernel = AlgorithmMorphology3D.CONNECTED24;
        method = AlgorithmMorphology3D.DILATE;
        itersDilation = 1;
        blueCountTotal = new int[numObjects];
        scaleMax = xRes * (xDim - 1);
        scaleMax = Math.max(scaleMax, yRes * (yDim - 1));
        scaleMax = Math.max(scaleMax, zRes * (zDim - 1));
        invMax = 1.0f / scaleMax;
        volPoints = new Vector[numObjects];
        sAxisPer = new float[numObjects][3];

        // nucleusVoxel = new boolean[numObjects][totLength];
        for (id = 1; id <= numObjects; id++) {
            fireProgressStateChanged("Processing object " + id + " of " + numObjects + " in blue segmented image");
            fireProgressStateChanged(85 + (id * 10 / numObjects));

            Arrays.fill(byteBuffer, (byte) 0);
            xCenter[id - 1] = 0.0f;
            yCenter[id - 1] = 0.0f;
            zCenter[id - 1] = 0.0f;
            volPoints[id - 1] = new Vector<Vector3f>();
            lowestSqr = Float.MAX_VALUE;
            highestSqr = -Float.MAX_VALUE;

            for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                for (j = k, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        i = x + j;

                        if (IDArray[i] == id) {
                            byteBuffer[i] = (byte) 1;
                            xCenter[id - 1] += x;
                            yCenter[id - 1] += y;
                            zCenter[id - 1] += z;
                            blueCountTotal[id - 1]++;
                        }
                    }
                }
            }

            xCenter[id - 1] /= blueCountTotal[id - 1];
            yCenter[id - 1] /= blueCountTotal[id - 1];
            zCenter[id - 1] /= blueCountTotal[id - 1];

            try {
                grayImage.importData(0, byteBuffer, true);
            } catch (IOException error) {
                byteBuffer = null;
                IDArray = null;
                errorCleanUp("Error on grayImage.importData", true);

                return;
            }

            dilateAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                   itersErosion, numPruningPixels, edgingType, wholeImage);
            dilateAlgo.run();
            dilateAlgo.finalize();
            dilateAlgo = null;

            try {
                grayImage.exportData(0, totLength, dilateArray);
            } catch (IOException error) {
                byteBuffer = null;
                IDArray = null;
                dilateArray = null;
                errorCleanUp("Error on grayImage.exportData", true);

                return;
            }

            for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                for (j = k, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        i = x + j;

                        if ((dilateArray[i] == (byte) 1) && (byteBuffer[i] == (byte) 0)) {
                            boundaryArray[i] = (byte) id;
                            kVoxel = new Vector3f(x * xRes * invMax, y * yRes * invMax, z * zRes * invMax);
                            volPoints[id - 1].add(kVoxel);
                            distSqr = ((xCenter[id - 1] - x) * (xCenter[id - 1] - x) * xRes * xRes) +
                                      ((yCenter[id - 1] - y) * (yCenter[id - 1] - y) * yRes * yRes) +
                                      ((zCenter[id - 1] - z) * (zCenter[id - 1] - z) * zRes * zRes);

                            if (distSqr < lowestSqr) {
                                lowestSqr = distSqr;
                            }

                            if (distSqr > highestSqr) {
                                highestSqr = distSqr;
                            }
                        }
                    }
                }
            }

            centerToNearEdge[id - 1] = (float) Math.sqrt(lowestSqr);
            centerToFarEdge[id - 1] = (float) Math.sqrt(highestSqr);
            Preferences.debug("Object id = " + id + "\n");
            Preferences.debug("Center of mass = (" + xCenter[id - 1] + ", " + yCenter[id - 1] + ", " + zCenter[id - 1] +
                              ")\n");
            Preferences.debug("Distance from center of mass to near edge = " + centerToNearEdge[id - 1] + "\n");
            Preferences.debug("Distance from center of mass to far edge = " + centerToFarEdge[id - 1] + "\n");

            kF = new AlgorithmEllipsoidFit(volPoints[id - 1]);
            axes = kF.getAxes();
            sAxisPer[id - 1][0] = (float) (0.5 * axes[0] * scaleMax);
            sAxisPer[id - 1][1] = (float) (0.5 * axes[1] * scaleMax);
            sAxisPer[id - 1][2] = (float) (0.5 * axes[2] * scaleMax);

            // Arrange the ellipse lengths in ascending order
            for (m = 0; m < 3; m++) {
                index = m;

                for (n = m + 1; n < 3; n++) {

                    if (sAxisPer[id - 1][n] < sAxisPer[id - 1][index]) {
                        index = n;
                    }
                } // for (n = m+1; n < 3; n++)

                if (index != m) {
                    tempf = sAxisPer[id - 1][m];
                    sAxisPer[id - 1][m] = sAxisPer[id - 1][index];
                    sAxisPer[id - 1][index] = tempf;
                } // if (index != m)
            } // for (m = 0; m < 3; m++)
        } // for (id = 1; id <= numObjects; id++)

        grayImage.disposeLocal();
        grayImage = null;
        System.gc();


        for (i = 0; i < numObjects; i++) {
            newPtVOI = new VOI((short) (i + (2 * nVOIs)), "point3D" + i + ".voi", VOI.POINT, -1.0f);
            newPtVOI.setColor(Color.magenta);
            xArr[0] = xCenter[i];
            yArr[0] = yCenter[i];
            zArr[0] = zCenter[i];
            newPtVOI.importCurve(xArr, yArr, zArr);
            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel("N" + (i + 1));
            srcImage.registerVOI(newPtVOI);
        } // for (i = 0; i < numObjects; i++)

        srcImage.notifyImageDisplayListeners();

        UI.setDataText("Plugin 10/19/07 version\n");
        UI.setDataText(srcImage.getFileInfo(0).getFileName() + "\n");

        if (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
            UI.setDataText("X resolution = " + xRes + "  " + (Unit.getUnitFromLegacyNum(xUnits)).toString() + "\n");
        } else {
            UI.setDataText("X resolution = " + xRes + "\n");
        }

        if (yUnits != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
            UI.setDataText("Y resolution = " + yRes + "  " + (Unit.getUnitFromLegacyNum(yUnits)).toString() + "\n");
        } else {
            UI.setDataText("Y resolution = " + yRes + "\n");
        }

        if (zUnits != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
            UI.setDataText("Z resolution = " + zRes + "  " + (Unit.getUnitFromLegacyNum(zUnits)).toString() + "\n\n");
        } else {
            UI.setDataText("Z resolution = " + zRes + "\n\n");
        }

        UI.setDataText("Nucleus\tNvolume\tRmin\tRmax");
        UI.setDataText("\tsAx1Cen\tsAx2Cen\tsAx3Cen");
        UI.setDataText("\tsAx1Per\tsAx2Per\tsAx3Per");

        if (redNumber > 0) {
            UI.setDataText("\tR1volume");
            UI.setDataText("\tR1geomax\tR1geo%\tR1geo-Np");
            UI.setDataText("\tR1gravmax\tR1grav%\tR1grav-Np");
        }

        if (redNumber > 1) {
            UI.setDataText("\tR2volume");
            UI.setDataText("\tR2geomax\tR2geo%\tR2geo-Np");
            UI.setDataText("\tR2gravmax\tR2grav%\tR2grav-Np");
        }

        if (redNumber > 2) {
            UI.setDataText("\tR3volume");
            UI.setDataText("\tR3geomax\tR3geo%\tR3geo-Np");
            UI.setDataText("\tR3gravmax\tR3grav%\tR3grav-Np");
        }

        if (greenNumber > 0) {
            UI.setDataText("\tG1volume");
            UI.setDataText("\tG1geomax\tG1geo%\tG1geo-Np");
            UI.setDataText("\tG1gravmax\tG1grav%\tG1grav-Np");
        }

        if (greenNumber > 1) {
            UI.setDataText("\tG2volume");
            UI.setDataText("\tG2geomax\tG2geo%\tG2geo-Np");
            UI.setDataText("\tG2gravmax\tG2grav%\tG2grav-Np");
        }

        if (greenNumber > 2) {
            UI.setDataText("\tG3volume");
            UI.setDataText("\tG3geomax\tG3geo%\tG3geo-Np");
            UI.setDataText("\tG3gravmax\tG3grav%\tG3grav-Np");
        }

        UI.setDataText("\n");

        for (i = 0; i < numObjects; i++) {
            nuclearVolume = xRes * yRes * zRes * blueCountTotal[i];
            UI.setDataText((i + 1) + "\t" + nf.format(nuclearVolume));
            UI.setDataText("\t" + nf.format(centerToNearEdge[i]));

            // equivalentRadius = (float)Math.pow(nuclearVolume/((4.0/3.0)*Math.PI),1.0/3.0);
            // UI.setDataText("\t" + nf.format(equivalentRadius));
            UI.setDataText("\t" + nf.format(centerToFarEdge[i]));
            UI.setDataText("\t" + nf.format(sAxisCen[i][0]));
            UI.setDataText("\t" + nf.format(sAxisCen[i][1]));
            UI.setDataText("\t" + nf.format(sAxisCen[i][2]));
            UI.setDataText("\t" + nf.format(sAxisPer[i][0]));
            UI.setDataText("\t" + nf.format(sAxisPer[i][1]));
            UI.setDataText("\t" + nf.format(sAxisPer[i][2]));

            redFound = 0;
            greenFound = 0;

            for (j = 0; j <= (nVOIs - 1); j++) {

                if (objectID[j] == (i + 1)) {
                    voiType = VOIs.VOIAt(j).getName().charAt(1);
                    Preferences.debug("VOI name found = " + VOIs.VOIAt(j).getName() + "\n");
                    Preferences.debug("voiType = " + voiType + "\n");

                    if (voiType == 'R') {
                        redFound++;
                        Preferences.debug("Red found\n");
                    }

                    if (voiType == 'G') {
                        greenFound++;
                        Preferences.debug("Green found\n");

                        if ((greenFound == 1) && (redFound != redNumber)) {

                            for (k = 0; k < (redNumber - redFound); k++) {
                                UI.setDataText("\t\t\t\t\t\t\t");
                            } // for (k = 0; k < (redNumber-redFound); k++)
                        } // if ((greenFound == 1) && (redFound != redNumber))
                    }

                    voiVolume = xRes * yRes * zRes * voiCount[j];
                    UI.setDataText("\t" + nf.format(voiVolume));
                    geoToCenter = (float)
                                      Math.sqrt(((xPosGeo[j] - xCenter[i]) * (xPosGeo[j] - xCenter[i]) * xRes * xRes) +
                                                ((yPosGeo[j] - yCenter[i]) * (yPosGeo[j] - yCenter[i]) * yRes * yRes) +
                                                ((zPosGeo[j] - zCenter[i]) * (zPosGeo[j] - zCenter[i]) * zRes * zRes));

                    initialdx = xPosGeo[j] - xCenter[i];
                    initialdy = yPosGeo[j] - yCenter[i];
                    initialdz = zPosGeo[j] - zCenter[i];
                    lastEdgeX = (int) Math.round(xPosGeo[j]);
                    lastEdgeY = (int) Math.round(yPosGeo[j]);
                    lastEdgeZ = (int) Math.round(zPosGeo[j]);

                    if ((Math.abs(initialdx) >= Math.abs(initialdy)) && (Math.abs(initialdx) >= Math.abs(initialdz))) {

                        if (initialdx >= 0.0f) {
                            incx = 0.1f;
                        } else {
                            incx = -0.1f;
                        }

                        incy = 0.1f * initialdy / Math.abs(initialdx);
                        incz = 0.1f * initialdz / Math.abs(initialdx);
                    } else if ((Math.abs(initialdy) >= Math.abs(initialdx)) &&
                                   (Math.abs(initialdy) >= Math.abs(initialdz))) {

                        if (initialdy >= 0.0f) {
                            incy = 0.1f;
                        } else {
                            incy = -0.1f;
                        }

                        incx = 0.1f * initialdx / Math.abs(initialdy);
                        incz = 0.1f * initialdz / Math.abs(initialdy);
                    } else {

                        if (initialdz >= 0.0f) {
                            incz = 0.1f;
                        } else {
                            incz = -0.1f;
                        }

                        incx = 0.1f * initialdx / Math.abs(initialdz);
                        incy = 0.1f * initialdy / Math.abs(initialdz);
                    }

                    numInc = 1;

                    while (true) {
                        newEdgeX = (int) Math.round(xPosGeo[j] + (numInc * incx));

                        if ((newEdgeX < 0) || (newEdgeX >= xDim)) {
                            break;
                        }

                        newEdgeY = (int) Math.round(yPosGeo[j] + (numInc * incy));

                        if ((newEdgeY < 0) || (newEdgeY >= yDim)) {
                            break;
                        }

                        newEdgeZ = (int) Math.round(zPosGeo[j] + (numInc * incz));

                        if ((newEdgeZ < 0) || (newEdgeZ >= zDim)) {
                            break;
                        }

                        numInc++;

                        if ((newEdgeX == lastEdgeX) && (newEdgeY == lastEdgeY) && (newEdgeZ == lastEdgeZ)) {
                            continue;
                        }

                        index = newEdgeX + (xDim * newEdgeY) + (sliceLength * newEdgeZ);
                        lastEdgeX = newEdgeX;
                        lastEdgeY = newEdgeY;
                        lastEdgeZ = newEdgeZ;

                        if (IDArray[index] != (i + 1)) {
                            break;
                        }
                    } // while (true)

                    centerToGeoToEdge = (float)
                                            Math.sqrt(((lastEdgeX - xCenter[i]) * (lastEdgeX - xCenter[i]) * xRes *
                                                           xRes) +
                                                      ((lastEdgeY - yCenter[i]) * (lastEdgeY - yCenter[i]) * yRes *
                                                           yRes) +
                                                      ((lastEdgeZ - zCenter[i]) * (lastEdgeZ - zCenter[i]) * zRes *
                                                           zRes));
                    UI.setDataText("\t" + nf.format(centerToGeoToEdge));
                    geoToCenter = 100.0f * geoToCenter / centerToGeoToEdge;
                    UI.setDataText("\t" + nf.format(geoToCenter));
                    geoToEdge = (float)
                                    Math.sqrt(((lastEdgeX - xPosGeo[j]) * (lastEdgeX - xPosGeo[j]) * xRes * xRes) +
                                              ((lastEdgeY - yPosGeo[j]) * (lastEdgeY - yPosGeo[j]) * yRes * yRes) +
                                              ((lastEdgeZ - zPosGeo[j]) * (lastEdgeZ - zPosGeo[j]) * zRes * zRes));
                    UI.setDataText("\t" + nf.format(geoToEdge));

                    gravToCenter = (float)
                                       Math.sqrt(((xPosGrav[j] - xCenter[i]) * (xPosGrav[j] - xCenter[i]) * xRes *
                                                      xRes) +
                                                 ((yPosGrav[j] - yCenter[i]) * (yPosGrav[j] - yCenter[i]) * yRes *
                                                      yRes) +
                                                 ((zPosGrav[j] - zCenter[i]) * (zPosGrav[j] - zCenter[i]) * zRes *
                                                      zRes));

                    initialdx = xPosGrav[j] - xCenter[i];
                    initialdy = yPosGrav[j] - yCenter[i];
                    initialdz = zPosGrav[j] - zCenter[i];
                    lastEdgeX = (int) Math.round(xPosGrav[j]);
                    lastEdgeY = (int) Math.round(yPosGrav[j]);
                    lastEdgeZ = (int) Math.round(zPosGrav[j]);

                    if ((Math.abs(initialdx) >= Math.abs(initialdy)) && (Math.abs(initialdx) >= Math.abs(initialdz))) {

                        if (initialdx >= 0.0f) {
                            incx = 0.1f;
                        } else {
                            incx = -0.1f;
                        }

                        incy = 0.1f * initialdy / Math.abs(initialdx);
                        incz = 0.1f * initialdz / Math.abs(initialdx);
                    } else if ((Math.abs(initialdy) >= Math.abs(initialdx)) &&
                                   (Math.abs(initialdy) >= Math.abs(initialdz))) {

                        if (initialdy >= 0.0f) {
                            incy = 0.1f;
                        } else {
                            incy = -0.1f;
                        }

                        incx = 0.1f * initialdx / Math.abs(initialdy);
                        incz = 0.1f * initialdz / Math.abs(initialdy);
                    } else {

                        if (initialdz >= 0.0f) {
                            incz = 0.1f;
                        } else {
                            incz = -0.1f;
                        }

                        incx = 0.1f * initialdx / Math.abs(initialdz);
                        incy = 0.1f * initialdy / Math.abs(initialdz);
                    }

                    numInc = 1;

                    while (true) {
                        newEdgeX = (int) Math.round(xPosGrav[j] + (numInc * incx));

                        if ((newEdgeX < 0) || (newEdgeX >= xDim)) {
                            break;
                        }

                        newEdgeY = (int) Math.round(yPosGrav[j] + (numInc * incy));

                        if ((newEdgeY < 0) || (newEdgeY >= yDim)) {
                            break;
                        }

                        newEdgeZ = (int) Math.round(zPosGrav[j] + (numInc * incz));

                        if ((newEdgeZ < 0) || (newEdgeZ >= zDim)) {
                            break;
                        }

                        numInc++;

                        if ((newEdgeX == lastEdgeX) && (newEdgeY == lastEdgeY) && (newEdgeZ == lastEdgeZ)) {
                            continue;
                        }

                        index = newEdgeX + (xDim * newEdgeY) + (sliceLength * newEdgeZ);
                        lastEdgeX = newEdgeX;
                        lastEdgeY = newEdgeY;
                        lastEdgeZ = newEdgeZ;

                        if (IDArray[index] != (i + 1)) {
                            break;
                        }
                    } // while (true)

                    centerToGravToEdge = (float)
                                             Math.sqrt(((lastEdgeX - xCenter[i]) * (lastEdgeX - xCenter[i]) * xRes *
                                                            xRes) +
                                                       ((lastEdgeY - yCenter[i]) * (lastEdgeY - yCenter[i]) * yRes *
                                                            yRes) +
                                                       ((lastEdgeZ - zCenter[i]) * (lastEdgeZ - zCenter[i]) * zRes *
                                                            zRes));
                    UI.setDataText("\t" + nf.format(centerToGravToEdge));
                    gravToCenter = 100.0f * gravToCenter / centerToGravToEdge;
                    UI.setDataText("\t" + nf.format(gravToCenter));
                    gravToEdge = (float)
                                     Math.sqrt(((lastEdgeX - xPosGrav[j]) * (lastEdgeX - xPosGrav[j]) * xRes * xRes) +
                                               ((lastEdgeY - yPosGrav[j]) * (lastEdgeY - yPosGrav[j]) * yRes * yRes) +
                                               ((lastEdgeZ - zPosGrav[j]) * (lastEdgeZ - zPosGrav[j]) * zRes * zRes));
                    UI.setDataText("\t" + nf.format(gravToEdge));


                    /*lowestSqr = Float.MAX_VALUE;
                     * for (k1 = 0, z = 0; z < zDim; z++, k1 += sliceLength) { for (j1 = k1, y = 0; y < yDim; y++, j1 +=
                     * xDim) {     for (x = 0; x < xDim; x++) {         index = x + j1;         if (boundaryArray[index]
                     * == objectID[j]) {             distSqr = (xPosGeo[j] - x) * (xPosGeo[j] - x) *  xRes * xRes +
                     *      (yPosGeo[j] - y) * (yPosGeo[j] - y) * yRes * yRes + (zPosGeo[j] - z) * (zPosGeo[j] - z) *
                     * zRes * zRes;             if (distSqr < lowestSqr) {        lowestSqr = distSqr;       xEdge = x;
                     *                yEdge = y;                 zEdge = z;             }         }  } } } geoToEdge =
                     * (float) Math.sqrt(lowestSqr); UI.setDataText("\t\t" + nf.format(geoToEdge)); geoToEdge *= 100.0f
                     * / centerToFarEdge[i]; UI.setDataText("\t" + nf.format(geoToEdge));
                     *
                     * lowestSqr = Float.MAX_VALUE; for (k1 = 0, z = 0; z < zDim; z++, k1 += sliceLength) { for (j1 = k1,
                     * y = 0; y < yDim; y++, j1 += xDim) {     for (x = 0; x < xDim; x++) {         index = x + j1;   if
                     * (boundaryArray[index] == objectID[j]) {             distSqr = (xPosGrav[j] - x) * (xPosGrav[j] -
                     * x) *                       xRes * xRes +                       (yPosGrav[j] - y) * (yPosGrav[j] -
                     * y) *                       yRes * yRes +                       (zPosGrav[j] - z) * (zPosGrav[j] -
                     * z) *                       zRes * zRes;             if (distSqr < lowestSqr) { lowestSqr =
                     * distSqr;                 xEdge = x;                 yEdge = y;     zEdge = z;    }         } } }
                     * } gravToEdge = (float) Math.sqrt(lowestSqr); UI.setDataText("\t\t" + nf.format(gravToEdge));
                     * gravToEdge *= 100.0f /
                     * centerToFarEdge[i];UI.setDataText("\t" + nf.format(gravToEdge));*/
                } // if (objectID[j] == (i+1))
            } // for (j = 0; j <= nVOIs - 1; j++)

            UI.setDataText("\n");
        } // for (i = 0; i < numObjects; i++)

        /*VOIs = srcImage.getVOIs();
         * System.out.println("Image = " + srcImage); System.out.println("VOIs = " + VOIs); for (i = 0; i < nVOIs; i++)
         * { System.out.println("id = " + VOIs.VOIAt(i).getID()); System.out.println(VOIs.VOIAt(i).getName());
         * System.out.println(VOIs.VOIAt(i).getColor());}*/

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }
}
