import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;


/**
 *
 * @version  June 2, 2008
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmCenterDistance.java $ $Revision: 72 $ $Date: 2/06/06 5:50p $
 *           PlugInAlgorithmCenterDistance is used to find the distances between geometric nucleus centers and nucleus
 *           boundaries, and distances on a straight line from the geometric nucleus center to the VOI center
 *           of mass to the nucleus boundary. The geometric center is a zero order moment not using intensity
 *           multiplications. The center of mass is a first order moment mutliplying position by intensity. The nuclei
 *           are blue and the chromosome regions of interest outlined by the vois are predominantly green. Images are
 *           2D or 3D. The program will automatically generate green VOIs in each nucleus. The user can input 1 or 2 as
 *           the number of green VOIs to be looked for in each nucleus. The default is 2 green VOIs.</p>
 *
 *           <p>1.) Only the blue portion of the red, green, blue color image is used to create the blueImage.</p>
 *
 *           <p>2.) Fuzzy C means segmentation is performed on the blueImage to create a blueSegImage, in which all of
 *           the blueImage pixel intensities are assigned to 1 of 2 classes. The lower intensity value pixels
 *           corresponding to the background in blueImage are all given the intensity value 1 in blueSegImage and the
 *           higher intensity value pixels in blueImage corresponding to the nucleus are all given the intensity value
 *           2 in blueSegImage. In fuzzy c means segmentation pixels less than the threshold are excluded from the
 *           classification process and assigned the intensity value of 0 in the segmentation image. Here, threshold is
 *           set to 0.0, so no pixels are less than threshold, so all the pixels in blueImage are assigned to either 1
 *           or 2 in blueSegImage.</p>
 *
 *           <p>3.) Set all the pixels with intensity = 2 to pixels with intensity =
 *           1 and all the pixels with intensity = 1 to pixels with intensity = 0.</p>
 *
 *           <p>4.) A slice by slice hole filling is performed on the blue segmented image.</p>
 *           
 *           <p>5.) Assign positive integer IDs to each object between blueMin and 200,000 pixels in size
 *            for 2D and between blueMin and 20,000,000 pixels in size for 3D.
 *            blueSegImage now has 0 for background and a positive integer ID for each object between the
 *            minimum and maximum size. blueMin has a default of 1,000.</p>
 *
 *           <p>6.) If one or more blue objects that do not touch an edge are present, 
 *                  set all the pixels in any blue object touching an edge to 0.</p>
 *                  
 *           <p>7.) Find the bounding box of each blue object.  Expand the bounding box until
 *                  (bounding box area or volume)/(blue object area or volume) >= minBoundsRatio.
 *                  Use a midpoint algorithm with upper and lower limits to find the minimum
 *                  blue value for the object.  The initial upper value is taken as 1 less than the
 *                  minimum blue value already present in the object and the initial lower value is
 *                  taken as 1 more than the minimum blue value in the image.  The initial midValue is
 *                  taken to be halfway between the lower and upper values.  This algorithm expands the
 *                  original blue object within the box until 4 or 6 neigbor linked blue values that are >=
 *                  midValue have been added to the original blue object.  The intensity fraction equal to
 *                  the sum of the expanded blue object intensities divided by the sum of the box
 *                  blue intensities is found.  If intensityFraction < minIntensityFraction, then 
 *                  upperValue is set equal to midValue - 1 and the blue object growth that
 *                  occurred during this iteration is made a permanent part of the expanded blue object,
 *                  so that future iterations will only require partial rather than full growth of the 
 *                  blue object.  If intensityFraction >= minIntensityFraction, then lowerValue is set
 *                  equal to midValue.  A new midValue is found and the iterations continue until the
 *                  smallest blue value is found for which intensityFraction >= minIntensityFraction.
 *           
 *           <p>8.) Smooth the blue objects with an open operation followed by a close operation.
 *           
 *           <p>9.) Obtain the red portion of the image.
 *           
 *           <p>10.) Obtain a red histogram.  Set the threshold so that the redFraction
 *           portion of the cumulative histogram is at or above threshold for the fuzzy c means.
 *           
 *           <p>11.) Perform a 3 level fuzzy c means segmentation on the red image.  Values below threshold
 *           are set equal to 0 and values >= threshold are assigned values of 1, 2, and 3
 *            
 *           <p>12.) Convert the red fuzzy segmented value of 3 to 1 and the other values to 0. 
 *           
 *           <p>13.) ID objects in red segmented image which have at least redMin pixels.
 *           
 *           <p>14.) Expand blue objects to include red objects protruding out of the blue boundary. 
 *
 *           <p>15.) Export the green portion of the image to greenBuffer.
 *
 *           <p>16.) Obtain histogram information on the green image. Set the threshold so that the greenFraction
 *           portion of the cumulative histogram is at or above threshold for the fuzzy c means. 
 *           
 *           <p>17.) Perform a 3 level fuzzy c means segmentation on the green image.  Green values below threshold
 *           are set to 0, and green values >= threshold are set to values of 1, 2, and 3.
 *           
 *           <p>18.) Create an image in which all the green values originally equal to 3 in the fuzzy
 *           c means segmentation are set to 1 and all the other green values are set to 0.
 *           
 *           <p>19.) ID objects in this green segmented image which have at least greenMin pixels.
 *            
 *           <p>20.) If twoGreenLevels is true, create an image in which all green values originally equal to
 *           2 in the fuzzy c means segmentation are set to 1 and all the other green values are set to 0.
 *           
 *           <p>21.) If twoGreenLevels is true, ID objects in green segmented image which have at least
 *            greenMin pixels.
 *           
 *           <p>22.) Find blue objects to which the green objects belong.  Expand the blue objects to 
 *           include green objects protruding out of the blue boundaries.
 *           
 *           <p>23.) Merge green objects with 2 pixels having a distance <= mergingDistance.
 *            
 *           <p>24.) Sort green objects by green intensity count per unit area or volume into a sorted green array.
 *           Have a separate sorted green array for each nucleus. Put no more than greenRegionNumber green objects
 *           into the sorted green array for each nucleus. 
 *           
 *           <p>25.) Fill the byteBuffer array with zeros.  For each sorted green VOI fill the appropriate
 *           positions in byteBuffer with a positive index. Create an accompanying color table to set for each
 *           nucleus the largest green voi to color pink and the other green voi to color pink.darker(). 
 *           Create an accompanying name table to set the name for each voi. The largest green voi in the
 *           fifth nucleus has name 5G1. The second largest green voi in the fifth nucleus has the name 5G2
 *             
 *           <p>26.) Extract VOIs from the green image.
 *            
 *           <p>27.) Set the source image VOIs to the VOIs obtained from the green image. 
 *           
 *           <p>28.) Process each VOI one at a time. 
 *           a.) For each VOI find the sum of the green intensity values, and the pixel count inside each blue object. 
 *           b.) Look at all the blue object pixel counts and assign the ID of the blue object with the most pixels
 *           in the VOI to the VOI's objectID. 
 *           c.) Find the green center of mass of the VOI. 
 *           d.) Expand the nucleus to which the VOI belongs to include the VOI.</p>
 *            
 *           <p>29.)From each blue object extract a VOI.
 *            
 *           <p>30.) Smooth the VOI from the blue object as follows:  From each slice select the VOI contour 
 *           with the largest number of points.  Use trimPoints at the maximum setting on this contour.
 *           Determine the default number of points to be used in AlgorithmBSmooth by dividing the arc length
 *           of the contour by the user supplied interpolationDivisor.  Apply AlgorithmBSmooth to the contour.
 *           For this slice remove all of the original VOI contours and replace them with this 1 smoothed contour.
 *             
 *           <p>31.) Generate a mask from the smoothed VOI.  Use the mask to modify the IDArray of the blue object
 *            positions.
 *           
 *           <p>32.) Assing a dark yellow color to the VOI and add the VOI to the source image VOI vector. </p>
 *           
 *           <p>33.) // Reorder the indices for the green VOIs and their associated arrays so that the indices
 *            use the sequence:
 *            blue object 1 - large green VOI 
 *            blue object 1 - small green VOI
 *            blue object 2 - large green VOI
 *            blue object 2 - small green VOI
 *            and so on
 *
 *           <p>34.) Process blue IDArray objects one at a time. 
 *           a.) Find the zero order moments of each blue IDArray object. 
 *           b.) Create a byteBuffer with pixels for that ID object equal to 1 and all the other pixels = 0. 
 *           c.)Import byteBuffer into grayImage. 
 *           d.) Run a dilation on grayImage and export the result to dilateArray. 
 *           e.) At every point where dilateArray = 1 and byteBuffer = 0, set boundaryArray = to the id of that
 *           object. For every point on the boundary find the square of the distance to the center of the object
 *           and if this square distance is the lowest yet noted, put the value in lowestSquare. If this square
 *           distance is the highest yet noted, put the value in highestSquare. 
 *           f.) After examining all points in the image, the program records the Rmin distance from the center
 *           to edge as the square root of lowestSquare and the Rmax distance from the center to the edge as the
 *           square root of highestSquare. 
 *           g.) Nucleus area = (blue count) * (x resolution) * (y resolution). 
 *           
 *           <p>35.) Determine nucleus-VOI distances 
 *           a.) Find the distance from the center of the ID object to the first order center of the VOI.
 *           b.) Find the distance of the line segment that passes thru these 2 points and terminates at the
 *           cell surface.
 *           c.) Divide the center to VOI distance by the center to boundary distance to find the VOI's
 *           fraction of the way out to the boundary.
 *           </p>
 *           
 *           Note that the blue objects are smoothed twice.  The first smoothing before VOI extraction with
 *           a morphological open followed by a close.  The second smoothing is after VOI extraction.  Note that
 *           the smoothing can put part of a green VOI at the boundary outside of the blue object.
 */
public class PlugInAlgorithmCenterDistance extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Minimum number of pixels in a red object */
    private int redMin = 50;

    /** Portion of red pixels above threshold in fuzzy c means. */
    private float redFraction = 0.15f;
    
    /** Green merging radius in inches around peak green spot */
    /** Merge 2 green VOIs together if 2 centers are <= merging radius */
    private float mergingDistance = 0.33f;
    
    // Number of green regions per cell
    // Either 1 for 1 for all cells, 2 for 2 for all cells, or 0 for 1 or 2 for all cells
    private int greenRegionNumber = 1;
    
    /** Minimum number of pixels in a blue object */
    private int blueMin = 1000;

    /** Portion of green pixels above threshold in fuzzy c means. */
    private float greenFraction = 0.01f;

    /** Minimum number of pixels in green VOI */
    private int greenMin = 10;
    
    /** If true, in green 3 level fuzzy c means, assign max and max-1 to 1 and other values to 0.
     * If false, in green 3 level fuzzy c means, assign max to 1 and other values to 0.
     */
    private boolean twoGreenLevels;
    
    private boolean blueExpand;
    
    /** The minimum ratio of the bounding box area or volume to the original blue object area or volume*/
    /** This number must be greater than 1.0 */
    private float minBoundsRatio;
    
    /** The minimum fraction of the total intensity count in the bounding box that just meets minAreaRatio  
     *   requirements that must be present in the expanded blue object. 
     *   This number must be less than 1.0.
     */
    private float minIntensityFraction;
    
    private boolean blueSmooth;
    
    private float interpolationDivisor = 24.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg         Source image model.
     * @param  blueMin
     * @param  redMin
     * @param  redFraction
     * @param  mergingDistance         DOCUMENT ME!
     * @param  greenMin
     * @param  greenFraction
     * @param  greenRegionNumber      DOCUMENT ME!
     * @param  twoGreenLevels
     * @param  blueExpand
     * @param  minAreaRatio
     * @param  minIntensityFraction
     * @param  blueSmooth
     * @param  interpolationDivisor
     */
    public PlugInAlgorithmCenterDistance(ModelImage srcImg, int blueMin, int redMin, float redFraction, float mergingDistance, 
                                         int greenMin, float greenFraction,  int greenRegionNumber,
                                         boolean twoGreenLevels, boolean blueExpand,
                                         float minBoundsRatio, float minIntensityFraction,
                                         boolean blueSmooth, float interpolationDivisor) {
        super(null, srcImg);
        this.blueMin = blueMin;
        this.redMin = redMin;
        this.redFraction = redFraction;
        this.mergingDistance = mergingDistance;
        this.greenMin = greenMin;
        this.greenFraction = greenFraction;
        this.greenRegionNumber = greenRegionNumber;
        this.twoGreenLevels = twoGreenLevels;
        this.blueExpand = blueExpand;
        this.minBoundsRatio = minBoundsRatio;
        this.minIntensityFraction = minIntensityFraction;
        this.blueSmooth = blueSmooth;
        this.interpolationDivisor = interpolationDivisor;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
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
        ModelImage grayImage2 = null;
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
        AlgorithmMorphology2D openAlgo;
        AlgorithmMorphology2D closeAlgo;
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
        ViewVOIVector blueVOIs = null;
        int nVOIs;
        short[] shortMask;
        int index;
        int greenCount;
        int[] idCount;
        int[] objectID;
        int[] objectID2;
        int objectCount;
        float[] xPosGrav;
        float[] yPosGrav;
        float[] xPosGrav2;
        float[] yPosGrav2;
        float colorCount[];
        float gravToCenter;
        ViewUserInterface UI = ViewUserInterface.getReference();
        float distSqr;
        float lowestSqr;
        float highestSqr;

        FileInfoBase fileInfo;
        int numGreenObjects = 0;
        int numGreenObjects2 = 0;
        byte[] greenIDArray = null;
        byte[] greenIDArray2 = null;
        float[] greenIntensityTotal;
        int[] greenArea;
        float[] greenIntensityPerArea;
        int[] greenNucleusNumber = null;
        int[][] sortedGreenIndex = null;
        float[][] sortedGreenIntensity;
        int k;
        AlgorithmVOIExtraction algoVOIExtraction;
        VOI newPtVOI;
        float[] xArr = new float[1];
        float[] yArr = new float[1];
        float[] zArr = new float[1];
        int numVOIObjects;
        Color[] colorTable = null;
        boolean removeID[];
        int m;
        String[] nameTable = null;
        String voiName;
        float[] blueCountTotal = null;
        AlgorithmMorphology2D fillHolesAlgo2D;
        int[] voiCount;
        int[] voiCount2;

        int greenFound;
        float nuclearArea;

        float voiArea;
        DecimalFormat df;
        DecimalFormat dfFract;
        float initialdx;
        float initialdy;
        int lastEdgeX;
        int lastEdgeY;
        int newEdgeX;
        int newEdgeY;
        float incx;
        float incy;
        int numInc;
        float centerToGravToEdge;
        float fractionOut;
        AlgorithmHistogram algoHist;
        int bins;
        int[] histoBuffer;
        double[] lowValue;
        int totalCount;
        int countsToRetain;
        int countsFound;
        int numGreenFound;
        int gIndex[];
        float gCount[];
        boolean placed;
        int index2;
        int greenCellNumber[];
        double distX;
        double distY;
        double distanceSquared;
        double minDistanceSquared;
        int greenBelong[];
        int[] redCellNumber;
        int[] redBelong;
        int redAdded;
        boolean isMaxGreen[][] = null;
        int blueArea[];
        double blueIntensityTotal[];
        double newBlueIntensityTotal;
        int blueLeft[];
        int blueRight[];
        int blueTop[];
        int blueBottom[];
        int blueWidth;
        int blueHeight;
        float blueMinArray[];
        int blueRectArea;
        float areaRatio;
        double blueRectIntensityTotal;
        float intensityFraction;
        boolean atBounds;
        boolean areaGrowth;
        float[] xPoints = null;
        float[] yPoints = null;
        AlgorithmArcLength arcLength;
        int defaultPts;
        boolean trim;
        AlgorithmBSmooth smoothAlgo = null;
        VOI resultVOI;
        byte IDArray2[];
        double minBlueValue;
        int maxGreenBelong;
        boolean allRemoved;
        int numRemoved;
        Vector<VOIBase> contours;
        int nPoints;
        int maxPoints;
        int maxElement;
        Polygon srcGon;
        VOI newVOI;
        int lowerValue;
        int upperValue;
        int midValue;
        boolean lastRun;
        double mergingDistanceSquared = mergingDistance * mergingDistance;
        int x1;
        int y1;
        int greenLeft[];
        int greenRight[];
        int greenTop[];
        int greenBottom[];
        int numGreenTotal;
        int index1;
        int index3;

        df = new DecimalFormat("0.000E0");
        dfFract = new DecimalFormat("0.000");
        
        minBlueValue = srcImage.getMinB();

        try {

            // image length is length in 2 dims
            length = xDim * yDim;
            buffer = new float[length];
            srcImage.exportRGBData(3, 0, length, buffer); // export blue data
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm CenterDistance reports: out of memory", true);

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

        
        fireProgressStateChanged(2);

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

        // Now convert the min = 1 and max = 2 to min = 0 and and max = 1
        fireProgressStateChanged("Setting segmented blue values to 0 and 1");
        fireProgressStateChanged(6);

        byteBuffer = new byte[length];
        try {
            grayImage.exportData(0, length, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        for (i = 0; i < length; i++) {
            byteBuffer[i]--;
        }
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }

        // Remove all inappropriate holes in blue nuclei
        fireProgressStateChanged("Removing holes from blue nuclei");
        fireProgressStateChanged(7);

        fillHolesAlgo2D = new AlgorithmMorphology2D(grayImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0,
                                                    wholeImage);
        fillHolesAlgo2D.run();
        fillHolesAlgo2D.finalize();
        fillHolesAlgo2D = null;
        
        fireProgressStateChanged("IDing objects in blue segmented image");
        fireProgressStateChanged(15);
        
        numPruningPixels = 0;
        edgingType = 0;   
        
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

        
        IDArray = new byte[length];

        try {
            grayImage.exportData(0, length, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

        fireProgressStateChanged("Removing blue objects touching edges");
        fireProgressStateChanged(22);

        removeID = new boolean[numObjects];
        for (id = 1; id <= numObjects; id++) {

            for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                for (x = 0; x < xDim; x++) {
                    i = x + j;

                    if ((IDArray[i] == id) && ((x == 0) || (x == (xDim - 1)) || (y == 0) || (y == (yDim - 1)))) {
                        removeID[id-1] = true;
                    }
                } // for (x = 0; x < xDim; x++)
            } // for (j = 0, y = 0; y < yDim; y++, j += xDim)
        } // for (id = 1; id <= numObjects; id++)
        
        allRemoved = true;
        numRemoved = 0;
        for (id = 1; id <= numObjects; id++) {
            if (!removeID[id-1]) {
                allRemoved = false;
            }
            else {
                numRemoved++;
            }
        } // for (id = 1; id <= numObjects; id++)
        
        if (allRemoved) {
            Preferences.debug("All blue objects touch edges so don't remove any\n");
            for (id = 1; id <= numObjects; id++) {
                removeID[id-1] = false;   
            }
        }
        else {
            Preferences.debug("Removing " + numRemoved + " blue objects of " + numObjects + " for touching edges\n");    
        }
        
        for (id = numObjects; id >= 1; id--) {    
            if (removeID[id-1]) {
    
                for (i = 0; i < length; i++) {

                    if (IDArray[i] == (byte) id) {
                        IDArray[i] = (byte) 0;
                    } 
                }

                numObjects--;
            } // if (removeID[id-1])
        } // for (id = numObjects; id >= 1; id--)      
        
        IDArray2 = new byte[length];
        if (blueExpand) {
            fireProgressStateChanged("Obtaining bounding boxes of blue objects");
            fireProgressStateChanged(25);
            blueArea= new int[numObjects];
            blueIntensityTotal = new double[numObjects];
            blueLeft = new int[numObjects];
            blueRight = new int[numObjects];
            blueTop = new int[numObjects];
            blueBottom = new int[numObjects];
            blueMinArray = new float[numObjects];
            for (i = 0; i < numObjects; i++) {
                blueLeft[i] = Integer.MAX_VALUE;
                blueTop[i] = Integer.MAX_VALUE;
                blueMinArray[i] = Float.MAX_VALUE;
            }
            // Obtain areas and bounding rectangles of numObjects
            for (y = 0; y < yDim; y++) {
                index = y*xDim;
                for (x = 0; x < xDim; x++) {
                    i = x + index;
                    // id goes from 0 to numObjects
                    id = IDArray[i];
                    if (id >= 1) {
                        blueArea[id-1]++;
                        blueIntensityTotal[id-1] += buffer[i];
                        if (buffer[i] < blueMinArray[id-1]) {
                            blueMinArray[id-1] = buffer[i];
                        }
                        if (x < blueLeft[id-1]) {
                            blueLeft[id-1] = x;
                        }
                        if (x > blueRight[id-1]) {
                            blueRight[id-1] = x;
                        }
                        if (y < blueTop[id-1]) {
                            blueTop[id-1] = y;
                        }
                        if (y > blueBottom[id-1]) {
                            blueBottom[id-1] = y;
                        }
                    } // if (id >= 1)
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
            
            fireProgressStateChanged(26);
            for (i = 0; i < numObjects; i++) {
                Preferences.debug("Expanding object " + (i+1) + "\n");
                blueWidth = (blueRight[i] - blueLeft[i]);
                blueHeight = (blueBottom[i] - blueTop[i]);
                blueRectArea = blueWidth*blueHeight;
                areaRatio = (float)blueRectArea/(float)blueArea[i];
                atBounds = false;
                while ((areaRatio < minBoundsRatio) && (!atBounds)) { 
                    blueLeft[i] = Math.max(0, blueLeft[i] - 1);
                    blueRight[i] = Math.min(xDim-1, blueRight[i] + 1);
                    blueTop[i] = Math.max(0, blueTop[i] - 1);
                    blueBottom[i] = Math.min(yDim - 1, blueBottom[i] + 1);
                    if ((blueLeft[i] == 0) && (blueRight[i] == (xDim - 1)) &&
                        (blueTop[i] == 0) && (blueBottom[i] == (yDim - 1))) {
                        atBounds = true;
                    }
                    blueWidth = (blueRight[i] - blueLeft[i]);
                    blueHeight = (blueBottom[i] - blueTop[i]);
                    blueRectArea = blueWidth*blueHeight;
                    areaRatio = (float)blueRectArea/(float)blueArea[i];
                } // while ((areaRatio < minBoundsRatio) && (!atBounds))
                blueRectIntensityTotal = 0;
                for (y = blueTop[i]; y <= blueBottom[i]; y++) {
                    index = y * xDim;
                    for (x = blueLeft[i]; x <= blueRight[i]; x++) {
                        k = x + index;
                        blueRectIntensityTotal += buffer[k];
                    }
                } // for (y = blueTop[i]; y <= blueBottom[i]; y++)
                intensityFraction = (float)(blueIntensityTotal[i]/blueRectIntensityTotal);
                if ((intensityFraction < minIntensityFraction) && (blueMinArray[i] > minBlueValue)) {
                    lowerValue = (int)Math.round(minBlueValue + 1);
                    upperValue = (int)Math.round(blueMinArray[i] - 1);
                    Preferences.debug("lowerValue = " + lowerValue + " upperValue = " + upperValue + "\n");
                    if (lowerValue == upperValue) {
                        lastRun = true;
                    }
                    else {
                        lastRun = false;
                    }
                    midValue = (lowerValue + upperValue)/2;
                    do {
                        Preferences.debug("lowerValue = " + lowerValue + "midValue = " + midValue + 
                                " upperValue = " + upperValue + "\n");
                        blueMinArray[i] = midValue; 
                        areaGrowth = true;
                        newBlueIntensityTotal = blueIntensityTotal[i];
                        for (k = 0; k < length; k++) {
                            IDArray2[k] = IDArray[k];
                        }
                        while (areaGrowth) {
                            areaGrowth = false;
                                for (y = blueTop[i]; y <= blueBottom[i]; y++) {
                                    index = y*xDim;
                                    for (x = blueLeft[i]; x <= blueRight[i]; x++) {
                                        k = x + index;
                                        if ((IDArray2[k] == 0) && (buffer[k] >= blueMinArray[i])) {
                                           if ((x > blueLeft[i]) && (IDArray2[k-1] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               areaGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                           else if ((x < blueRight[i]) && (IDArray2[k+1] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               areaGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                           else if ((y > blueTop[i]) && (IDArray2[k - xDim] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               areaGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                           else if ((y < blueBottom[i]) && (IDArray2[k + xDim] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               areaGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                        } // if ((IDArray[k] == 0) && (buffer[k] >= blueMinArray[i]))
                                    } // for (x = blueLeft[i]; x <= blueRight[i]; x++)
                                } // for (y = blueTop[i]; y <= blueBottom[i]; y++)
                        } // while (volumeGrowth)
                        intensityFraction = (float)(newBlueIntensityTotal/blueRectIntensityTotal);
                        if (lastRun) {
                            for (k = 0; k < length; k++) {
                                IDArray[k] = IDArray2[k];
                            }
                            break;
                        }
                        if (intensityFraction >= minIntensityFraction) {
                                if (midValue != lowerValue) {
                                    lowerValue = midValue;
                                    midValue = (lowerValue + upperValue)/2;
                                }
                                else {
                                    midValue = upperValue;
                                }
                        }
                        else {
                                upperValue = Math.max(lowerValue, midValue-1); 
                                midValue = (lowerValue + upperValue)/2;
                                // So you don't have to repeat all of the growth
                                // process on every iteration
                                blueIntensityTotal[i] = newBlueIntensityTotal;
                                for (k = 0; k < length; k++) {
                                    IDArray[k] = IDArray2[k];
                                }
                        }
                        
                        if (lowerValue == upperValue) {
                            lastRun = true;
                        }
                    } while (true);
                } // if ((intensityFraction < minIntensityFraction) && (blueMinArray[i] > minBlueValue))
            } // for (i = 0; i < numObjects; i++)
        } // if (blueExpand)
        
        try {
            grayImage.importData(0, IDArray, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }  
        
        // Smooth with a morphological opening followed by a closing
        fireProgressStateChanged("Opening blue segmented image");
        fireProgressStateChanged(27);

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
        fireProgressStateChanged(28);

        method = AlgorithmMorphology2D.CLOSE;
        closeAlgo = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                              numPruningPixels, edgingType, wholeImage);
        closeAlgo.run();
        closeAlgo.finalize();
        closeAlgo = null;

        try {
            srcImage.exportRGBData(1, 0, length, buffer); // export red data
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        }
        
        try {
            grayImage.exportData(0, length, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

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
        
        wholeImage = true;
        

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
        // in which values below threshold are set to 0 and values >= threshold are set to 1, 2, and 3
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

        // Now convert the red = 3 in the fuzzy c means segmentation to 1 and the other values to 0
        fireProgressStateChanged("Setting segmented red values to 0 and 1");
        fireProgressStateChanged(35);
        try {
            grayImage.exportData(0, length, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        for (i = 0; i < length; i++) {
            if (byteBuffer[i] == 3) {
                byteBuffer[i] = 1;
            }
            else {
                byteBuffer[i] = 0;
            }
        }
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }
        
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
        int numRedObjects = (int) grayImage.getMax();
        byte [] redIDArray = new byte[length];

        try {
            grayImage.exportData(0, length, redIDArray);
        } catch (IOException error) {
            byteBuffer = null;
            redIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        // redCellNumber will contain the positive ID of the blue object that
        // most completely contains it.  redCellNumber will be 0 if it is
        // completely outside all blue objects.
        redCellNumber = new int[numRedObjects];
        
        redAdded = 0;
        for (j = 1; j <= numRedObjects; j++) {
            redBelong = new int[numObjects+1];
            for (i = 0; i < length; i++) {

                if (redIDArray[i] == j) {
                    
                    redBelong[IDArray[i]] = redBelong[IDArray[i]] + 1;
                }

                
            }

            redCellNumber[j-1] = 0;
            for (k = 1; k <= numObjects; k++) {
                if (redBelong[k] > redCellNumber[j-1]) {
                    redCellNumber[j-1] = k;
                }
            }
            
            // Expand IDArray to include red regions protruding out of the blue cell boundary
            for (i = 0; i < length; i++) {
                if ((redIDArray[i] == j) && (IDArray[i] == 0)) {
                    IDArray[i] = (byte)redCellNumber[j-1];
                    redAdded++;
                }
            }
        }
        Preferences.debug("redAdded = " + redAdded + "\n");
        
        fireProgressStateChanged("Creating green image");
        fireProgressStateChanged(46);
        
        greenBuffer = new float[length];

        try {

            srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        }         

        // grayImage is ModelStorageBase.UBYTE
        grayImage.reallocate(ModelStorageBase.FLOAT);

        try {
            grayImage.importData(0, greenBuffer, true);
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }

        

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
        
        Preferences.debug("threshold = " + threshold + "\n");


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
        // having 0 for values below threshold and 1, 2, and 3 for 3 classes >= threshold.
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

        if (twoGreenLevels) {
            grayImage2 = (ModelImage)grayImage.clone();
        }
        
        // Now convert the green = 3 in the fuzzy c means segmentation to 1 and convert all other values to 0.
        fireProgressStateChanged("Setting segmented green values to 0 and 1");
        fireProgressStateChanged(50);
        try {
            grayImage.exportData(0, length, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        for (i = 0; i < length; i++) {
            if (byteBuffer[i] == 3) {
                byteBuffer[i] = 1;
            }
            else {
                byteBuffer[i] = 0;
            }
        }
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }
        
        // Put the green objects IDs in greenIDArray
        fireProgressStateChanged("IDing objects in green segmented image");
        fireProgressStateChanged(58);
        kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
        circleDiameter = 0.0f;
        method = AlgorithmMorphology2D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo2D = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, wholeImage);
        idObjectsAlgo2D.setMinMax(greenMin, 200);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;

        grayImage.calcMinMax();
        numGreenObjects = (int) grayImage.getMax();
        Preferences.debug("numGreenObjects = " + numGreenObjects + "\n");
        greenIDArray = new byte[length];

        try {
            grayImage.exportData(0, length, greenIDArray);
        } catch (IOException error) {
            byteBuffer = null;
            greenIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        if (twoGreenLevels) {
            // Now convert the green = 2 in the fuzzy c means segmentation to 1 and convert all other values to 0.
            fireProgressStateChanged("Setting segmented green values to 0 and 1");
            fireProgressStateChanged(59);
            try {
                grayImage2.exportData(0, length, byteBuffer);
            } catch (IOException error) {
                byteBuffer = null;
                errorCleanUp("Error on grayImage2.exportData", true);

                return;
            }
            
            for (i = 0; i < length; i++) {
                if (byteBuffer[i] == 2) {
                    byteBuffer[i] = 1;
                }
                else {
                    byteBuffer[i] = 0;
                }
            }
            
            try {
                grayImage2.importData(0, byteBuffer, true);
            }
            catch (IOException error) {
                byteBuffer = null;
                errorCleanUp("Error on grayImage2.importData", true);
                return;
            }
            
            // Put the green object IDs in greenIDArray2
            fireProgressStateChanged("IDing objects in green segmented image");
            fireProgressStateChanged(60);
            kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
            circleDiameter = 0.0f;
            method = AlgorithmMorphology2D.ID_OBJECTS;
            itersDilation = 0;
            itersErosion = 0;
            idObjectsAlgo2D = new AlgorithmMorphology2D(grayImage2, kernel, circleDiameter, method, itersDilation,
                                                        itersErosion, numPruningPixels, edgingType, wholeImage);
            idObjectsAlgo2D.setMinMax(greenMin, 200);
            idObjectsAlgo2D.run();
            idObjectsAlgo2D.finalize();
            idObjectsAlgo2D = null;

            grayImage2.calcMinMax();
            numGreenObjects2 = (int) grayImage2.getMax();
            Preferences.debug("numGreenObjects2 = " + numGreenObjects2 + "\n");
            greenIDArray2 = new byte[length];

            try {
                grayImage2.exportData(0, length, greenIDArray2);
            } catch (IOException error) {
                byteBuffer = null;
                greenIDArray = null;
                greenIDArray2 = null;
                errorCleanUp("Error on grayImage.exportData", true);

                return;
            } 
            grayImage2.disposeLocal();
            grayImage2 = null;
            
            // Merge greenIDArray and greenIDArray2 into greenIDArray
            for (i = 0; i < length; i++) {
                if ((greenIDArray[i] == 0) && (greenIDArray2[i] > 0)) {
                    greenIDArray[i] = (byte)(greenIDArray2[i] + numGreenObjects);
                }
            }
        } // if (twoGreenLevels)

        fireProgressStateChanged("Finding cells to which green objects belong");
        fireProgressStateChanged(61);
        numGreenTotal = numGreenObjects+numGreenObjects2;
        greenIntensityTotal = new float[numGreenTotal];
        greenArea = new int[numGreenTotal];
        greenCellNumber = new int[numGreenTotal];
        greenNucleusNumber = new int[numObjects];
        sortedGreenIndex = new int[numObjects][greenRegionNumber];
        sortedGreenIntensity = new float[numObjects][greenRegionNumber];
        isMaxGreen = new boolean[numObjects][greenRegionNumber];
        
        for (j = 1; j <= numGreenTotal; j++) {
            greenBelong = new int[numObjects+1];
            for (i = 0; i < length; i++) {

                if (greenIDArray[i] == j) {
                    greenIntensityTotal[j - 1] += greenBuffer[i];
                    greenArea[j - 1]++;
                    greenBelong[IDArray[i]] = greenBelong[IDArray[i]] + 1;
                } // if (greenIDArray[i] == j)
            }

            greenCellNumber[j-1] = 0;
            maxGreenBelong = 0;
            for (k = 1; k <= numObjects; k++) {
                if (greenBelong[k] > maxGreenBelong) {
                    greenCellNumber[j-1] = k;
                    maxGreenBelong = greenBelong[k];
                }
            }
            
            // Expand IDArray to include green regions protruding out of the blue cell boundary
            for (i = 0; i < length; i++) {
                if ((greenIDArray[i] == j) && (IDArray[i] == 0)) {
                    IDArray[i] = (byte)greenCellNumber[j-1];
                }
            }
        }
        
        fireProgressStateChanged("Merging together close green objects");
        fireProgressStateChanged(62);
        greenLeft = new int[numGreenTotal];
        greenRight = new int[numGreenTotal];
        greenTop = new int[numGreenTotal];
        greenBottom = new int[numGreenTotal];
        for (i = 0; i < numGreenTotal; i++) {
            greenLeft[i] = Integer.MAX_VALUE;
            greenTop[i] = Integer.MAX_VALUE;
        }
        // Obtain bounding blocks of numGreenTotal green objects
        for (y = 0; y < yDim; y++) {
            index = y*xDim;
            for (x = 0; x < xDim; x++) {
                i = x + index;
                // id goes from 0 to numGreenTotal
                id = greenIDArray[i];
                if (id >= 1) {
                    if (x < greenLeft[id-1]) {
                        greenLeft[id-1] = x;
                    }
                    if (x > greenRight[id-1]) {
                        greenRight[id-1] = x;
                    }
                    if (y < greenTop[id-1]) {
                        greenTop[id-1] = y;
                    }
                    if (y > greenBottom[id-1]) {
                        greenBottom[id-1] = y;
                    }
                } // if (id >= 1)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        // Merge green regions in the same cell within merging distance together
        loop1:
        for (j = numGreenTotal; j >= 1; j--) {
            for (i = 1; i < j; i++) {
                if (greenCellNumber[j-1] == greenCellNumber[i-1]) {
                    minDistanceSquared = Double.MAX_VALUE; 
                    for (y = greenTop[j-1]; y <= greenBottom[j-1]; y++) {
                        index1 = y*xDim;
                        for (x = greenLeft[j-1]; x <= greenRight[j-1]; x++) {
                            index = index1 + x;
                            if (greenIDArray[index] == j) {
                                for (y1 = greenTop[i-1]; y1 <= greenBottom[i-1]; y1++) {
                                    index3 = y1*xDim;
                                    for (x1 = greenLeft[i-1]; x1 <= greenRight[i-1]; x1++) {
                                        index2 = index3 + x1;
                                        if (greenIDArray[index2] == i) {
                                            distX = (x1 - x) * xRes;
                                            distY = (y1 - y) * yRes;
                                            distanceSquared = distX*distX + distY*distY;
                                            if (distanceSquared < minDistanceSquared) {
                                                minDistanceSquared = distanceSquared;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if (minDistanceSquared <= mergingDistanceSquared) {
                        greenIntensityTotal[i-1] = 0.0f;
                        greenArea[i-1] = 0;
                        greenLeft[i-1] = Math.min(greenLeft[i-1], greenLeft[j-1]);
                        greenRight[i-1] = Math.max(greenRight[i-1], greenRight[j-1]);
                        greenTop[i-1] = Math.min(greenTop[i-1], greenTop[j-1]);
                        greenBottom[i-1] = Math.max(greenBottom[i-1], greenBottom[j-1]);
                        for (k = 0; k < length; k++) {
                            if (greenIDArray[k] == j) {
                                greenIDArray[k] = (byte)i;
                            }
                            if (greenIDArray[k] == i) {
                                greenIntensityTotal[i-1] += greenBuffer[k];
                                greenArea[i-1]++;
                            }
                        } // for (k = 0; k < length; k++)
                        for (m = j+1; m <= numGreenTotal; m++) {
                            for (k = 0; k < length; k++) {
                                if (greenIDArray[k] == m) {
                                    greenIDArray[k] = (byte)(m-1);
                                }
                            }
                            greenIntensityTotal[m-2] = greenIntensityTotal[m-1];
                            greenArea[m-2] = greenArea[m-1];
                            greenCellNumber[m-2] = greenCellNumber[m-1];
                            greenLeft[m-2] = greenLeft[m-1];
                            greenRight[m-2] = greenRight[m-1];
                            greenTop[m-2] = greenTop[m-1];
                            greenBottom[m-2] = greenBottom[m-1];
                        }
                        if (j <= numGreenObjects) {
                            numGreenObjects--;
                        }
                        else {
                            numGreenObjects2--;
                        }
                        numGreenTotal--;
                        continue loop1;
                    } // if (distanceSquared <= mergingDistanceSquared)
                }
            }
        }

        // Sort the green objects within each nucleus.
        // Create no more than greenRegionNumber green objects within each nucleus.
        fireProgressStateChanged("Sorting green objects by intensity count");
        fireProgressStateChanged(66);
        greenIntensityPerArea = new float[numGreenTotal];
        for (j = 1; j <= numGreenObjects; j++) {
            id = greenCellNumber[j-1];
            greenIntensityPerArea[j-1] = greenIntensityTotal[j-1]/greenArea[j-1];

            if (id >= 1) {

                if (greenNucleusNumber[id - 1] < greenRegionNumber) {
                    greenNucleusNumber[id - 1]++;
                }

                found = false;

                for (i = 0; (i < greenNucleusNumber[id - 1]) && !found; i++) {

                    if (greenIntensityPerArea[j - 1] >= sortedGreenIntensity[id - 1][i]) {
                        found = true;

                        if ((i == 0) && (greenNucleusNumber[id-1] >= 2)) {
                            sortedGreenIntensity[id - 1][1] = sortedGreenIntensity[id - 1][0];
                            sortedGreenIndex[id - 1][1] = sortedGreenIndex[id - 1][0];
                            isMaxGreen[id-1][1] = true;
                        }

                        sortedGreenIntensity[id - 1][i] = greenIntensityPerArea[j - 1];
                        sortedGreenIndex[id - 1][i] = j; // from greenIDArray
                        isMaxGreen[id-1][i] = true;
                    }
                } // for (i = 0; i < greenNucleusNumber[id-1]  && !found; i++)
            } // if (id >= 1)
        } // for (j = 1; j <= numGreenObjects; j++)
        
        for (j = numGreenObjects+1; j <= numGreenTotal; j++) {
            id = greenCellNumber[j-1];
            greenIntensityPerArea[j-1] = greenIntensityTotal[j-1]/greenArea[j-1];

            if (id >= 1) {

                if (greenNucleusNumber[id - 1] < greenRegionNumber) {
                    greenNucleusNumber[id - 1]++;
                }

                found = false;

                for (i = 0; (i < greenNucleusNumber[id - 1]) && !found; i++) {

                    if ((!isMaxGreen[id-1][i])&& (greenIntensityPerArea[j - 1] >= sortedGreenIntensity[id - 1][i])) {
                        found = true;

                        if ((i == 0) && (greenNucleusNumber[id-1] >= 2)) {
                            sortedGreenIntensity[id - 1][1] = sortedGreenIntensity[id - 1][0];
                            sortedGreenIndex[id - 1][1] = sortedGreenIndex[id - 1][0];
                        }

                        sortedGreenIntensity[id - 1][i] = greenIntensityPerArea[j - 1];
                        sortedGreenIndex[id - 1][i] = j; // from greenIDArray
                    }
                } // for (i = 0; i < greenNucleusNumber[id-1]  && !found; i++)
            } // if (id >= 1)    
        } // for (j = numGreenObjects+1; j <= numGreenTotal; j++)
        
        greenIntensityTotal = null;
        greenIntensityPerArea = null;

        for (i = 0; i < numObjects; i++) {
            sortedGreenIntensity[i] = null;
            isMaxGreen[i] = null;
        }
        isMaxGreen = null;
        sortedGreenIntensity = null;

        numVOIObjects = 0;

        for (i = 0; i < numObjects; i++) {
            numVOIObjects += greenNucleusNumber[i];
        }

        colorTable = new Color[numVOIObjects];
        nameTable = new String[numVOIObjects];

        Arrays.fill(byteBuffer, (byte) 0);

        for (i = 0, j = 0; i < numObjects; i++) {

            for (m = 0; m < greenNucleusNumber[i]; m++) {

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
            } // for (m = 0; m < greenNucleusNumber[i]; m++)
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
        fireProgressStateChanged(70);
        algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
        algoVOIExtraction.setColorTable(colorTable);
        algoVOIExtraction.setNameTable(nameTable);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;

        srcImage.setVOIs(grayImage.getVOIs());

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size(); 
        
        idCount = new int[numObjects];
        xPosGrav = new float[nVOIs];
        yPosGrav = new float[nVOIs];
        objectID = new int[nVOIs];
        colorCount = new float[nVOIs];
        shortMask = new short[length];
        voiCount = new int[nVOIs];

        for (i = 0; i < nVOIs; i++) {
            fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
            fireProgressStateChanged(72);
            VOIs.VOIAt(i).setOnlyID((short) i);
            voiName = VOIs.VOIAt(i).getName();

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                Arrays.fill(shortMask, (short) -1);

                shortMask = srcImage.generateVOIMask(shortMask, i);
                greenCount = 0;
                Arrays.fill(idCount, 0);

                for (j = 0; j < length; j++) {

                    if (shortMask[j] != -1) {
                        
                        greenCount += greenBuffer[j];

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

                xPosGrav[i] = 0.0f;
                yPosGrav[i] = 0.0f;

                
                colorCount[i] = 0.0f;

                for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        index = x + j;

                        if (shortMask[index] != -1) {
                            xPosGrav[i] += greenBuffer[index] * x;
                            yPosGrav[i] += greenBuffer[index] * y;
                            colorCount[i] += greenBuffer[index];
                            voiCount[i]++;
                            IDArray[index] = (byte) objectID[i];
                        }
                    }
                }

                xPosGrav[i] /= colorCount[i];
                yPosGrav[i] /= colorCount[i];

                
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        fireProgressStateChanged("Extracting VOIs from blue image");
        fireProgressStateChanged(78);
        
        
        trim = true;
        
        for (i = 0; i < numObjects; i++) {
            grayImage.resetVOIs();
            for (j = 0; j < length; j++) {
                IDArray2[j] = 0;
                if (IDArray[j] == (i+1)) {
                    IDArray2[j] = (byte)(i+1);    
                }
            }
            
            try {
                grayImage.importData(0, IDArray2, true);
            } catch (IOException error) {
                byteBuffer = null;
                errorCleanUp("Error on grayImage.importData", true);

                return;
            }
            
            algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
            algoVOIExtraction.run();
            algoVOIExtraction.finalize();
            algoVOIExtraction = null;
            
            blueVOIs = grayImage.getVOIs();
            if ((blueVOIs.size() != 0) && (blueVOIs.VOIAt(0).getCurves().size() != 0)) {
                if (blueSmooth) {
                    newVOI = new VOI((short) 1, "blueVOI", VOI.CONTOUR, -1.0f);
                    contours = blueVOIs.VOIAt(0).getCurves();
                    maxPoints = 0;
                    maxElement = 0;
                    for (j = 0; j < contours.size(); j++) {
                        nPoints = ((VOIContour)contours.elementAt(j)).size();
                        if (nPoints > maxPoints) {
                            maxPoints = nPoints;
                            maxElement = j;
                        }
                    }
                    ((VOIContour) (contours.elementAt(maxElement))).trimPoints(1.0, true);
                    srcGon = ((VOIContour) (contours.elementAt(maxElement))).exportPolygon();
                    xPoints = new float[srcGon.npoints + 5];
                    yPoints = new float[srcGon.npoints + 5];
        
                    xPoints[0] = srcGon.xpoints[srcGon.npoints - 2];
                    yPoints[0] = srcGon.ypoints[srcGon.npoints - 2];
        
                    xPoints[1] = srcGon.xpoints[srcGon.npoints - 1];
                    yPoints[1] = srcGon.ypoints[srcGon.npoints - 1];
        
                    for (j = 0; j < srcGon.npoints; j++) {
                        xPoints[j + 2] = srcGon.xpoints[j];
                        yPoints[j + 2] = srcGon.ypoints[j];
                    }
        
                    xPoints[srcGon.npoints + 2] = srcGon.xpoints[0];
                    yPoints[srcGon.npoints + 2] = srcGon.ypoints[0];
        
                    xPoints[srcGon.npoints + 3] = srcGon.xpoints[1];
                    yPoints[srcGon.npoints + 3] = srcGon.ypoints[1];
        
                    xPoints[srcGon.npoints + 4] = srcGon.xpoints[2];
                    yPoints[srcGon.npoints + 4] = srcGon.ypoints[2];
        
                    arcLength = new AlgorithmArcLength(xPoints, yPoints);
                    defaultPts = Math.round(arcLength.getTotalArcLength() / interpolationDivisor);
                    newVOI.removeCurves();
                    newVOI.importCurve((VOIContour)contours.elementAt(maxElement));
                    newVOI.setAllActive(true);
                    blueVOIs.VOIAt(0).setAllActive(true);
                    smoothAlgo = new AlgorithmBSmooth(grayImage, newVOI, defaultPts, trim);
                    smoothAlgo.run();
                    if (smoothAlgo.isCompleted()) {
                        // The algorithm has completed and produced a VOI
                        resultVOI = smoothAlgo.getResultVOI();
                        blueVOIs.VOIAt(0).removeCurves();
                        blueVOIs.VOIAt(0).importCurve((VOIContour)resultVOI.getCurves().elementAt(0));
                        // Change IDArray to reflect the changed boundaries of the voi
                        Arrays.fill(shortMask, (short) -1);
                        shortMask = grayImage.generateVOIMask(shortMask, 0);
                        if (shortMask != null) {
                            for (j = 0; j < length; j++) {
                                if (IDArray[j] == (i+1)) {
                                    IDArray[j] = 0;
                                }
                                if (shortMask[j] != -1) {
                                    IDArray[j] = (byte)(i + 1);
                                }
                            }
                        } // if (shortMask != null)
                        else {
                            shortMask = new short[length];
                        }
                    } // if (smoothAlgo.isCompleted())
                    smoothAlgo.finalize();
                    smoothAlgo = null;
                } // if (blueSmooth)
                blueVOIs.VOIAt(0).setColor(Color.yellow.darker());
                VOIs.add(blueVOIs.VOIAt(0));
            } // if (blueVOIs.size() != 0) && (blueVOIs.VOIAt(0).getCurves().length != 0))
        } // for (i = 0; i < numObjects; i++)
        
        srcImage.setVOIs((VOIVector)VOIs);

        // Reorder the indices for the green VOIs and their associated arrays so that
        // the indices use the sequence:
        // blue object 1 - large green VOI
        // blue object 1 - small green VOI
        // blue object 2 - large green VOI
        // blue object 2 - small green VOI
        // and so on
        gIndex = new int[2];
        gCount = new float[2];
        VOIs2 = new ViewVOIVector();
        objectID2 = new int[nVOIs];
        xPosGrav2 = new float[nVOIs];
        yPosGrav2 = new float[nVOIs];
        voiCount2 = new int[nVOIs];
        index2 = 0;
        for (j = 1; j <= numObjects; j++) {
            numGreenFound = 0;
            gCount[0] = 0.0f;
            gCount[1] = 0.0f;
            for (i = 0; i < nVOIs; i++) {
                if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) && (objectID[i] == j)) {
                    if (numGreenFound < 2) {
                        numGreenFound++;
                    }
                    placed = false;
                    for (k = 0; k <= 1 && (!placed); k++) {
                        if (colorCount[i] >= gCount[k]) {
                            placed = true; 
                            if ((k == 0) && (numGreenFound == 2)) {
                                gIndex[1] = gIndex[0];
                                gCount[1] = gCount[0];
                            }
                            gIndex[k] = i;
                            gCount[k] = colorCount[i];
                        }
                    }
                }
            } // for (i = 0; i < nVOIs; i++)
            
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
            xPosGrav[i] = xPosGrav2[i];
            yPosGrav[i] = yPosGrav2[i];
            voiCount[i] = voiCount2[i];
        }
        objectID2 = null;
        xPosGrav2 = null;
        yPosGrav2 = null;
        voiCount2 = null;
        
        for (i = 0; i < nVOIs; i++) {
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                voiName = VOIs.VOIAt(i).getName();
                newPtVOI = new VOI((short) (i + nVOIs), voiName, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xArr[0] = xPosGrav[i];
                yArr[0] = yPosGrav[i];
                zArr[0] = 0.0f;
                newPtVOI.importCurve(xArr, yArr, zArr);
                newPtVOI.getCurves().lastElement().setFixed(true);
                newPtVOI.getCurves().lastElement().setLabel(voiName);
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

            fireProgressStateChanged(85 + (id * 10 / numObjects));

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
            newPtVOI = new VOI((short) (i + (2 * nVOIs)), "Cen" + (i+1), VOI.POINT, -1.0f);
            newPtVOI.setColor(Color.white);
            xArr[0] = xCenter[i];
            yArr[0] = yCenter[i];
            zArr[0] = 0.0f;
            newPtVOI.importCurve(xArr, yArr, zArr);
            newPtVOI.getCurves().lastElement().setFixed(true);
            newPtVOI.getCurves().lastElement().setLabel("Cen" + (i + 1));
            srcImage.registerVOI(newPtVOI);
        } // for (i = 0; i < numObjects; i++)

        srcImage.notifyImageDisplayListeners();
        
        UI.getMessageFrame().addTab("PlugInAlgorithmCenterDistance");
        UI.getMessageFrame().append("PlugInAlgorithmCenterDistance", "\n");
        UI.getMessageFrame().append("PlugInAlgorithmCenterDistance", "File Name \t\tNucleus\tNArea\tRmin\tRmax" +
                                    "\tObj\tGarea\tcenterToG\tcenterToGToEdge\tfractionGOut\n");

        for (i = 0; i < numObjects; i++) {
            nuclearArea = xRes * yRes * blueCountTotal[i];
            greenFound = 0;

            for (j = 0; j <= (nVOIs - 1); j++) {

                if (objectID[j] == (i + 1)) {
                    greenFound++;
                    voiArea = xRes * yRes * voiCount[j];
                    
                    gravToCenter = (float)
                                       Math.sqrt(((xPosGrav[j] - xCenter[i]) * (xPosGrav[j] - xCenter[i]) * xRes *
                                                      xRes) +
                                                 ((yPosGrav[j] - yCenter[i]) * (yPosGrav[j] - yCenter[i]) * yRes *
                                                      yRes));

                    initialdx = xPosGrav[j] - xCenter[i];
                    initialdy = yPosGrav[j] - yCenter[i];
                    lastEdgeX = Math.round(xPosGrav[j]);
                    lastEdgeY = Math.round(yPosGrav[j]);

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
                        newEdgeX = Math.round(xPosGrav[j] + (numInc * incx));

                        if ((newEdgeX < 0) || (newEdgeX >= xDim)) {
                            break;
                        }

                        newEdgeY = Math.round(yPosGrav[j] + (numInc * incy));

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
                    fractionOut = gravToCenter/centerToGravToEdge;
        
                    UI.getMessageFrame().append("PlugInAlgorithmCenterDistance",
                    srcImage.getFileInfo(0).getFileName() + "\t" + (i+1) + "\t" + df.format(nuclearArea) +
                    "\t" + df.format(centerToNearEdge[i]) + "\t" + df.format(centerToFarEdge[i]) +
                    "\t" + (j+1) + "\t" + df.format(voiArea) + "\t" + df.format(gravToCenter) +
                    "\t" + df.format(centerToGravToEdge) + "\t\t" + dfFract.format(fractionOut) + "\n");
                } // if (objectID[j] == (i+1))
            } // for (j = 0; j <= nVOIs - 1; j++)

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
    private void calc3D() {

        int totLength, sliceLength;
        float[] buffer;
        AlgorithmFuzzyCMeans fcmAlgo;
        ModelImage grayImage;
        ModelImage grayImage2 = null;
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
        int sliceSize = xDim * yDim;
        int zDim = srcImage.getExtents()[2];
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
        float zRes = srcImage.getResolutions(0)[2];
        boolean found;
        AlgorithmMorphology3D idObjectsAlgo3D;
        AlgorithmMorphology3D openAlgo;
        AlgorithmMorphology3D closeAlgo;
        int numObjects;
        byte[] byteBuffer;
        byte[] IDArray;
        byte[] IDArray2;
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
        ViewVOIVector blueVOIs = null;
        short[] shortMask;
        int index;
        int index3;
        int index4;
        int index5;
        int greenCount;
        int[] idCount;
        int[] objectID;
        int[] objectID2;
        int objectCount;
        float[] xPosGrav;
        float[] yPosGrav;
        float[] zPosGrav;
        float[] xPosGrav2;
        float[] yPosGrav2;
        float[] zPosGrav2;
        float colorCount[];
        float gravToCenter;
        ViewUserInterface UI = ViewUserInterface.getReference();
        float distSqr;
        float lowestSqr;
        float highestSqr;

        FileInfoBase fileInfo;
        int numRedObjects = 0;
        int numGreenObjects = 0;
        int numGreenObjects2 = 0;
        byte[] redIDArray = null;
        byte[] greenIDArray = null;
        byte[] greenIDArray2 = null;
        float[] greenIntensityTotal;
        int[] greenVolume;
        float[] greenIntensityPerVolume;
        int[] greenNucleusNumber = null;
        int[][] sortedGreenIndex = null;
        float[][] sortedGreenIntensity;
        AlgorithmVOIExtraction algoVOIExtraction;
        VOI newPtVOI;
        float[] xArr = new float[1];
        float[] yArr = new float[1];
        float[] zArr = new float[1];
        int numVOIObjects;
        Color[] colorTable = null;
        boolean removeID[];
        int m;
        String[] nameTable = null;
        String voiName;
        int[] blueCountTotal = null;
        ModelImage grayImage2D;
        int[] extents2D = new int[2];
        byte[] buffer2D;
        AlgorithmMorphology2D fillHolesAlgo2D;
        int[] voiCount;
        int[] voiCount2;
        int greenFound;
        float nuclearVolume;
        DecimalFormat df;
        DecimalFormat dfFract;
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
        float centerToGravToEdge;
        float fractionOut;
        AlgorithmHistogram algoHist;
        int bins;
        int[] histoBuffer;
        double[] lowValue;
        int totalCount;
        int countsToRetain;
        int countsFound;
        int numGreenFound;
        int gIndex[];
        float gCount[];
        boolean placed;
        int index1;
        int index2;
        int[] redCellNumber;
        int[] redBelong;
        int redAdded;
        int[] greenCellNumber;
        int[] greenBelong;
        double distX;
        double distY;
        double distZ;
        double distanceSquared;
        double minDistanceSquared;
        boolean isMaxGreen[][] = null;
        int blueVolume[];
        double blueIntensityTotal[];
        double newBlueIntensityTotal;
        int blueLeft[];
        int blueRight[];
        int blueTop[];
        int blueBottom[];
        int blueFront[];
        int blueBack[];
        int blueWidth;
        int blueHeight;
        int blueDepth;
        float blueMinArray[];
        int blueBlockVolume;
        float volumeRatio;
        double blueBlockIntensityTotal;
        float intensityFraction;
        boolean atBounds;
        boolean volumeGrowth;
        Polygon srcGon;
        float[] xPoints = null;
        float[] yPoints = null;
        AlgorithmArcLength arcLength;
        int defaultPts;
        boolean trim;
        AlgorithmBSmooth smoothAlgo = null;
        VOI resultVOI;
        double minBlueValue;
        int maxGreenBelong;
        boolean allRemoved;
        int numRemoved;
        Vector<VOIBase>[] contours;
        int nPoints;
        int maxPoints;
        int maxElement;
        VOI newVOI;
        int lowerValue;
        int upperValue;
        int midValue;
        boolean lastRun;
        double mergingDistanceSquared = mergingDistance * mergingDistance;
        int x1;
        int y1;
        int z1;
        int greenLeft[];
        int greenRight[];
        int greenTop[];
        int greenBottom[];
        int greenFront[];
        int greenBack[];
        int numGreenTotal;

        df = new DecimalFormat("0.000E0");
        dfFract = new DecimalFormat("0.000");

        fireProgressStateChanged("Processing image ...");
        
        minBlueValue = srcImage.getMinB();

        try {
            sliceLength = xDim * yDim;
            totLength = sliceLength * zDim;
            buffer = new float[totLength];
            fireProgressStateChanged("Creating blue image");
            srcImage.exportRGBData(3, 0, totLength, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm CenterDistance: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm CenterDistance: Out of memory creating process buffer", true);

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

        fireProgressStateChanged(2);

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

        // Now convert the min = 1 and max = 2 to min = 0 and and max = 1
        fireProgressStateChanged("Setting segmented blue values to 0 and 1");
        fireProgressStateChanged(6);

        byteBuffer = new byte[totLength];
        try {
            grayImage.exportData(0, totLength, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        for (i = 0; i < totLength; i++) {
            byteBuffer[i]--;
        }
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }
        
        // Do a slice by slice hole filling operation on the blue image
        fireProgressStateChanged("Slice by slice hole filling on blue segmented image");
        fireProgressStateChanged(7);

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
                errorCleanUp("Algorithm CenterDistance: Error on grayImage exportData", true);

                return;
            }

            try {
                grayImage2D.importData(0, buffer2D, true);
            } catch (IOException e) {
                buffer = null;
                errorCleanUp("Algorithm CenterDistance: Error on grayImage2D importData", true);

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
                errorCleanUp("Algorithm CenterDistance: Error on grayImage2D exportData", true);

                return;
            }

            try {
                grayImage.importData(z * sliceLength, buffer2D, false);
            } catch (IOException e) {
                buffer = null;
                errorCleanUp("Algorithm CenterDistance: Error on grayImage importData", true);

                return;
            }

        } // for (z = 0; z < zDim; z++)

        grayImage.calcMinMax();
        buffer2D = null;
        System.gc();

        fireProgressStateChanged("IDing objects in blue segmented image");
        fireProgressStateChanged(15);

        kernel = AlgorithmMorphology3D.SIZED_SPHERE;
        sphereDiameter = 0.0f;
        method = AlgorithmMorphology3D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        numPruningPixels = 0;
        edgingType = 0;
        idObjectsAlgo3D = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, wholeImage);
        idObjectsAlgo3D.setMinMax(blueMin, 20000000);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;
        System.gc();

        grayImage.calcMinMax();

        numObjects = (int) grayImage.getMax();

        
        IDArray = new byte[totLength];

        try {
            grayImage.exportData(0, totLength, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

        fireProgressStateChanged("Removing blue objects touching edges");
        fireProgressStateChanged(22);
        
        
        removeID = new boolean[numObjects];
        for (id = 1; id <= numObjects; id++) {

            for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                for (j = k, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        i = x + j;

                        if ((IDArray[i] == id) &&
                                ((x == 0) || (x == (xDim - 1)) || (y == 0) || (y == (yDim - 1)) || (z == 0) ||
                                     (z == (zDim - 1)))) {
                            removeID[id-1] = true;
                        }
                    }
                }
            }
        } // for (id = 1; id <= numObjects; id++)
        
        allRemoved = true;
        numRemoved = 0;
        for (id = 1; id <= numObjects; id++) {
            if (!removeID[id-1]) {
                allRemoved = false;
            }
            else {
                numRemoved++;
            }
        } // for (id = 1; id <= numObjects; id++)
        
        if (allRemoved) {
            Preferences.debug("All blue objects touch edges so don't remove any\n");
            for (id = 1; id <= numObjects; id++) {
                removeID[id-1] = false;   
            }
        }
        else {
            Preferences.debug("Removing " + numRemoved + " blue objects of " + numObjects + " for touching edges\n");    
        }
        
        for (id = numObjects; id >= 1; id--) {    
            if (removeID[id-1]) {
    
                for (i = 0; i < totLength; i++) {

                    if (IDArray[i] == (byte) id) {
                        IDArray[i] = (byte) 0;
                    } 
                }

                numObjects--;
            } // if (removeID[id-1])
        } // for (id = numObjects; id >= 1; id--)      
        
        IDArray2 = new byte[totLength];
        if (blueExpand) {
            fireProgressStateChanged("Obtaining bounding boxes of blue objects");
            fireProgressStateChanged(25);
            blueVolume = new int[numObjects];
            blueIntensityTotal = new double[numObjects];
            blueLeft = new int[numObjects];
            blueRight = new int[numObjects];
            blueTop = new int[numObjects];
            blueBottom = new int[numObjects];
            blueFront = new int[numObjects];
            blueBack = new int[numObjects];
            blueMinArray = new float[numObjects];
            for (i = 0; i < numObjects; i++) {
                blueLeft[i] = Integer.MAX_VALUE;
                blueTop[i] = Integer.MAX_VALUE;
                blueFront[i] = Integer.MAX_VALUE;
                blueMinArray[i] = Float.MAX_VALUE;
            }
            // Obtain volumes and bounding blocks of numObjects
            for (z = 0; z < zDim; z++) {
                index1 = z*sliceSize;
                for (y = 0; y < yDim; y++) {
                    index = index1 + y*xDim;
                    for (x = 0; x < xDim; x++) {
                        i = x + index;
                        // id goes from 0 to numObjects
                        id = IDArray[i];
                        if (id >= 1) {
                            blueVolume[id-1]++;
                            blueIntensityTotal[id-1] += buffer[i];
                            if (buffer[i] < blueMinArray[id-1]) {
                                blueMinArray[id-1] = buffer[i];
                            }
                            if (x < blueLeft[id-1]) {
                                blueLeft[id-1] = x;
                            }
                            if (x > blueRight[id-1]) {
                                blueRight[id-1] = x;
                            }
                            if (y < blueTop[id-1]) {
                                blueTop[id-1] = y;
                            }
                            if (y > blueBottom[id-1]) {
                                blueBottom[id-1] = y;
                            }
                            if (z < blueFront[id-1]) {
                                blueFront[id-1] = z;
                            }
                            if (z > blueBack[id-1]) {
                                blueBack[id-1] = z;
                            }
                        } // if (id >= 1)
                    } // for (x = 0; x < xDim; x++)
                } // for (y = 0; y < yDim; y++)
            } // for (z = 0; z < zDim; z++)
            
            fireProgressStateChanged("Expanding blue objects within bounding boxes");
            fireProgressStateChanged(26);
            for (i = 0; i < numObjects; i++) {
                Preferences.debug("Expanding object " + (i+1) + "\n");
                blueWidth = (blueRight[i] - blueLeft[i]);
                blueHeight = (blueBottom[i] - blueTop[i]);
                blueDepth = (blueBack[i] - blueFront[i]);
                blueBlockVolume = blueWidth*blueHeight*blueDepth;
                volumeRatio = (float)blueBlockVolume/(float)blueVolume[i];
                atBounds = false;
                while ((volumeRatio < minBoundsRatio) && (!atBounds)) { 
                    blueLeft[i] = Math.max(0, blueLeft[i] - 1);
                    blueRight[i] = Math.min(xDim-1, blueRight[i] + 1);
                    blueTop[i] = Math.max(0, blueTop[i] - 1);
                    blueBottom[i] = Math.min(yDim - 1, blueBottom[i] + 1);
                    blueFront[i] = Math.max(0, blueFront[i] - 1);
                    blueBack[i] = Math.min(zDim - 1, blueBack[i] + 1);
                    if ((blueLeft[i] == 0) && (blueRight[i] == (xDim - 1)) &&
                        (blueTop[i] == 0) && (blueBottom[i] == (yDim - 1)) &&
                        (blueFront[i] == 0) && (blueBack[i] == (zDim - 1))) {
                        atBounds = true;
                    }
                    blueWidth = (blueRight[i] - blueLeft[i]);
                    blueHeight = (blueBottom[i] - blueTop[i]);
                    blueDepth = (blueBack[i] - blueFront[i]);
                    blueBlockVolume = blueWidth*blueHeight*blueDepth;
                    volumeRatio = (float)blueBlockVolume/(float)blueVolume[i];
                } // while ((volumeRatio < minBoundsRatio) && (!atBounds))
                blueBlockIntensityTotal = 0;
                for (z = blueFront[i]; z <= blueBack[i]; z++) {
                    index1 = z*sliceSize;
                    for (y = blueTop[i]; y <= blueBottom[i]; y++) {
                        index = index1 + y*xDim;
                        for (x = blueLeft[i]; x <= blueRight[i]; x++) {
                            k = x + index;
                            blueBlockIntensityTotal += buffer[k];
                        }
                    } // for (y = blueTop[i]; y <= blueBottom[i]; y++)
                } // for (z = blueFront[i]; z <= blueBack[i]; z++)
                intensityFraction = (float)(blueIntensityTotal[i]/blueBlockIntensityTotal);
                if ((intensityFraction < minIntensityFraction) && (blueMinArray[i] > minBlueValue)) {
                    lowerValue = (int)Math.round(minBlueValue + 1);
                    upperValue = (int)Math.round(blueMinArray[i] - 1);
                    Preferences.debug("lowerValue = " + lowerValue + " upperValue = " + upperValue + "\n");
                    if (lowerValue == upperValue) {
                        lastRun = true;
                    }
                    else {
                        lastRun = false;
                    }
                    midValue = (lowerValue + upperValue)/2;
                    do {
                        blueMinArray[i] = midValue; 
                        volumeGrowth = true;
                        newBlueIntensityTotal = blueIntensityTotal[i];
                        for (k = 0; k < totLength; k++) {
                            IDArray2[k] = IDArray[k];
                        }
                        while (volumeGrowth) {
                            volumeGrowth = false;
                            for (z = blueFront[i]; z <= blueBack[i]; z++) {
                                index1 = z*sliceSize;
                                for (y = blueTop[i]; y <= blueBottom[i]; y++) {
                                    index = index1 + y*xDim;
                                    for (x = blueLeft[i]; x <= blueRight[i]; x++) {
                                        k = x + index;
                                        if ((IDArray2[k] == 0) && (buffer[k] >= blueMinArray[i])) {
                                           if ((x > blueLeft[i]) && (IDArray2[k-1] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               volumeGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                           else if ((x < blueRight[i]) && (IDArray2[k+1] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               volumeGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                           else if ((y > blueTop[i]) && (IDArray2[k - xDim] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               volumeGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                           else if ((y < blueBottom[i]) && (IDArray2[k + xDim] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               volumeGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                           else if ((z > blueFront[i]) && (IDArray2[k - sliceSize] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               volumeGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                           else if ((z < blueBack[i]) && (IDArray2[k + sliceSize] == (i+1))) {
                                               IDArray2[k] = (byte)(i+1);
                                               volumeGrowth = true;
                                               newBlueIntensityTotal += buffer[k];
                                           }
                                        } // if ((IDArray[k] == 0) && (buffer[k] >= blueMinArray[i]))
                                    } // for (x = blueLeft[i]; x <= blueRight[i]; x++)
                                } // for (y = blueTop[i]; y <= blueBottom[i]; y++)
                            } // for (z = blueFront[i]; z <= blueBack[i]; z++)
                        } // while (volumeGrowth)
                        intensityFraction = (float)(newBlueIntensityTotal/blueBlockIntensityTotal);
                        if (lastRun) {
                            for (k = 0; k < totLength; k++) {
                                IDArray[k] = IDArray2[k];
                            }
                            break;
                        }
                        if (intensityFraction >= minIntensityFraction) {
                                if (midValue != lowerValue) {
                                    lowerValue = midValue;
                                    midValue = (lowerValue + upperValue)/2;
                                }
                                else {
                                    midValue = upperValue;
                                }
                        }
                        else {
                                upperValue = Math.max(lowerValue, midValue-1); 
                                midValue = (lowerValue + upperValue)/2;
                                // So you don't have to repeat all of the growth
                                // process on every iteration
                                blueIntensityTotal[i] = newBlueIntensityTotal;
                                for (k = 0; k < totLength; k++) {
                                    IDArray[k] = IDArray2[k];
                                }
                        }
                        
                        if (lowerValue == upperValue) {
                            lastRun = true;
                        }
                    } while (true);
                } // if ((intensityFraction < minIntensityFraction) && (blueMinArray[i] > minBlueValue))
            } // for (i = 0; i < numObjects; i++)
        } // if (blueExpand)
        
        try {
            grayImage.importData(0, IDArray, true);
        }
        catch(IOException e) {
            buffer = null;
            errorCleanUp("Algorithm CenterDistance reports: grayImage locked", true);

            return;    
        }
        
        // Smooth with a morphological opening followed by a closing
        fireProgressStateChanged("Opening blue segmented image");
        fireProgressStateChanged(27);
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

        fireProgressStateChanged("Closing blue segmented image");
        fireProgressStateChanged(28);
        method = AlgorithmMorphology3D.CLOSE;
        closeAlgo = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation, itersErosion,
                                              numPruningPixels, edgingType, wholeImage);
        closeAlgo.run();
        closeAlgo.finalize();
        closeAlgo = null;
        System.gc();
        
        try {
            grayImage.exportData(0, totLength, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        try {
            srcImage.exportRGBData(1, 0, totLength, buffer); // export red data       
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        }
            
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
        // in which values below threshold are set to 0 and values >= threshold are set to 1, 2, and 3
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

        // Now convert the red = 3 in the fuzzy c means segmentation to 1 and the other values to 0
        fireProgressStateChanged("Setting segmented red values to 0 and 1");
        fireProgressStateChanged(35);
        try {
            grayImage.exportData(0, totLength, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        for (i = 0; i < totLength; i++) {
            if (byteBuffer[i] == 3) {
                byteBuffer[i] = 1;
            }
            else {
                byteBuffer[i] = 0;
            }
        }
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }

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

        try {
            grayImage.exportData(0, totLength, redIDArray);
        } catch (IOException error) {
            byteBuffer = null;
            redIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        // redCellNumber will contain the positive ID of the blue object that
        // most completely contains it.  redCellNumber will be 0 if it is
        // completely outside all blue objects.
        redCellNumber = new int[numRedObjects];
        
        redAdded = 0;
        for (j = 1; j <= numRedObjects; j++) {
            redBelong = new int[numObjects+1];
            for (i = 0; i < totLength; i++) {

                if (redIDArray[i] == j) {
                    
                    redBelong[IDArray[i]] = redBelong[IDArray[i]] + 1;
                }

                
            }

            redCellNumber[j-1] = 0;
            for (k = 1; k <= numObjects; k++) {
                if (redBelong[k] > redCellNumber[j-1]) {
                    redCellNumber[j-1] = k;
                }
            }
            
            // Expand IDArray to include red regions protruding out of the blue cell boundary
            for (i = 0; i < totLength; i++) {
                if ((redIDArray[i] == j) && (IDArray[i] == 0)) {
                    IDArray[i] = (byte)redCellNumber[j-1];
                    redAdded++;
                }
            }
        }
        Preferences.debug("redAdded = " + redAdded + "\n");

        fireProgressStateChanged("Creating green image");
        fireProgressStateChanged(46);
        
        greenBuffer = new float[totLength];
        
        try {

            srcImage.exportRGBData(2, 0, totLength, greenBuffer); // export green data
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        }         

        // grayImage is ModelStorageBase.UBYTE
        grayImage.reallocate(ModelStorageBase.FLOAT);

        try {
            grayImage.importData(0, greenBuffer, true);
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }

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
        // having 0 for values below threshold and 1, 2, and 3 for 3 classes >= threshold.
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

        if (twoGreenLevels) {
            grayImage2 = (ModelImage)grayImage.clone();
        }
        
        // Now convert the green = 3 in the fuzzy c means segmentation to 1 and convert all other values to 0.
        fireProgressStateChanged("Setting segmented green values to 0 and 1");
        fireProgressStateChanged(50);
        try {
            grayImage.exportData(0, totLength, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        for (i = 0; i < totLength; i++) {
            if (byteBuffer[i] == 3) {
                byteBuffer[i] = 1;
            }
            else {
                byteBuffer[i] = 0;
            }
        }
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }

        // Put the green objects IDs in greenIDArray
        fireProgressStateChanged("IDing objects in green segmented image");
        fireProgressStateChanged(58);
        kernel = AlgorithmMorphology3D.SIZED_SPHERE;
        sphereDiameter = 0.0f;
        method = AlgorithmMorphology3D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo3D = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, wholeImage);
        idObjectsAlgo3D.setMinMax(greenMin, 100000);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;
        System.gc();

        grayImage.calcMinMax();
        numGreenObjects = (int) grayImage.getMax();
        Preferences.debug("numGreeenObjects = " + numGreenObjects + "\n");
        greenIDArray = new byte[totLength];

        try {
            grayImage.exportData(0, totLength, greenIDArray);
        } catch (IOException error) {
            byteBuffer = null;
            greenIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        if (twoGreenLevels) {
            // Now convert the green = 2 in the fuzzy c means segmentation to 1 and convert all other values to 0.
            fireProgressStateChanged("Setting segmented green values to 0 and 1");
            fireProgressStateChanged(59);
            try {
                grayImage2.exportData(0, totLength, byteBuffer);
            } catch (IOException error) {
                byteBuffer = null;
                errorCleanUp("Error on grayImage2.exportData", true);

                return;
            }
            
            for (i = 0; i < totLength; i++) {
                if (byteBuffer[i] == 2) {
                    byteBuffer[i] = 1;
                }
                else {
                    byteBuffer[i] = 0;
                }
            }
            
            try {
                grayImage2.importData(0, byteBuffer, true);
            }
            catch (IOException error) {
                byteBuffer = null;
                errorCleanUp("Error on grayImage2.importData", true);
                return;
            }
            
            // Put the green objects IDs in greenIDArray2
            fireProgressStateChanged("IDing objects in green segmented image");
            fireProgressStateChanged(60);
            kernel = AlgorithmMorphology3D.SIZED_SPHERE;
            sphereDiameter = 0.0f;
            method = AlgorithmMorphology3D.ID_OBJECTS;
            itersDilation = 0;
            itersErosion = 0;
            idObjectsAlgo3D = new AlgorithmMorphology3D(grayImage2, kernel, sphereDiameter, method, itersDilation,
                                                        itersErosion, numPruningPixels, edgingType, wholeImage);
            idObjectsAlgo3D.setMinMax(greenMin, 100000);
            idObjectsAlgo3D.run();
            idObjectsAlgo3D.finalize();
            idObjectsAlgo3D = null;

            grayImage2.calcMinMax();
            numGreenObjects2 = (int) grayImage2.getMax();
            Preferences.debug("numGreenObjects2 = " + numGreenObjects2 + "\n");
            greenIDArray2 = new byte[totLength];

            try {
                grayImage2.exportData(0, totLength, greenIDArray2);
            } catch (IOException error) {
                byteBuffer = null;
                greenIDArray = null;
                greenIDArray2 = null;
                errorCleanUp("Error on grayImage.exportData", true);

                return;
            } 
            grayImage2.disposeLocal();
            grayImage2 = null;
            
            // Merge greenIDArray and greenIDArray2 into greenIDArray
            for (i = 0; i < totLength; i++) {
                if ((greenIDArray[i] == 0) && (greenIDArray2[i] > 0)) {
                    greenIDArray[i] = (byte)(greenIDArray2[i] + numGreenObjects);
                }
            }
        } // if (twoGreenLevels)
        
        // Sort the green objects within each nucleus.
        // Create no more than greenRegionNumber green objects within each nucleus.
        fireProgressStateChanged("Finding cells to which green objects belong");
        fireProgressStateChanged(61);
        numGreenTotal = numGreenObjects+numGreenObjects2;
        greenIntensityTotal = new float[numGreenTotal];
        greenVolume = new int[numGreenTotal];
        greenCellNumber = new int[numGreenTotal];
        greenNucleusNumber = new int[numObjects];
        sortedGreenIndex = new int[numObjects][greenRegionNumber];
        sortedGreenIntensity = new float[numObjects][greenRegionNumber];
        isMaxGreen = new boolean[numObjects][greenRegionNumber];
        
        for (j = 1; j <= numGreenTotal; j++) {
            greenBelong = new int[numObjects+1];
            for (i = 0; i < totLength; i++) {

                if (greenIDArray[i] == j) {
                    greenIntensityTotal[j - 1] += greenBuffer[i];
                    greenVolume[j - 1]++;
                    greenBelong[IDArray[i]] = greenBelong[IDArray[i]] + 1;
                }
            }

            greenCellNumber[j-1] = 0;
            maxGreenBelong = 0;
            for (k = 1; k <= numObjects; k++) {
                if (greenBelong[k] > maxGreenBelong) {
                    greenCellNumber[j-1] = k;
                    maxGreenBelong = greenBelong[k];
                }
            }
            
            // Expand IDArray to include green regions protruding out of the blue cell boundary
            for (i = 0; i < totLength; i++) {
                if ((greenIDArray[i] == j) && (IDArray[i] == 0)) {
                    IDArray[i] = (byte)greenCellNumber[j-1];
                }
            }
        }
        
        fireProgressStateChanged("Merging together close green objects");
        fireProgressStateChanged(62);
        greenLeft = new int[numGreenTotal];
        greenRight = new int[numGreenTotal];
        greenTop = new int[numGreenTotal];
        greenBottom = new int[numGreenTotal];
        greenFront = new int[numGreenTotal];
        greenBack = new int[numGreenTotal];
        for (i = 0; i < numGreenTotal; i++) {
            greenLeft[i] = Integer.MAX_VALUE;
            greenTop[i] = Integer.MAX_VALUE;
            greenFront[i] = Integer.MAX_VALUE;
        }
        // Obtain bounding blocks of numGreenTotal green objects
        for (z = 0; z < zDim; z++) {
            index = z*sliceSize;
            for (y = 0; y < yDim; y++) {
                index1 = index + y*xDim;
                for (x = 0; x < xDim; x++) {
                    i = x + index1;
                    // id goes from 0 to numGreenTotal
                    id = greenIDArray[i];
                    if (id >= 1) {
                        if (x < greenLeft[id-1]) {
                            greenLeft[id-1] = x;
                        }
                        if (x > greenRight[id-1]) {
                            greenRight[id-1] = x;
                        }
                        if (y < greenTop[id-1]) {
                            greenTop[id-1] = y;
                        }
                        if (y > greenBottom[id-1]) {
                            greenBottom[id-1] = y;
                        }
                        if (z < greenFront[id-1]) {
                            greenFront[id-1] = z;
                        }
                        if (z > greenBack[id-1]) {
                            greenBack[id-1] = z;
                        }
                    } // if (id >= 1)
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)
        // Merge green regions in the same cell within merging distance together
        loop1:
        for (j = numGreenTotal; j >= 1; j--) {
            for (i = 1; i < j; i++) {
                if (greenCellNumber[j-1] == greenCellNumber[i-1]) {
                    minDistanceSquared = Double.MAX_VALUE;
                    for (z = greenFront[j-1]; z <= greenBack[j-1]; z++) {
                        index2 = z*sliceSize;
                        for (y = greenTop[j-1]; y <= greenBottom[j-1]; y++) {
                            index1 = index2 + y*xDim;
                            for (x = greenLeft[j-1]; x <= greenRight[j-1]; x++) {
                                index = index1 + x;
                                if (greenIDArray[index] == j) {
                                    for (z1 = greenFront[i-1]; z1 <= greenBack[i-1]; z1++) {
                                        index5 = z1*sliceSize;
                                        for (y1 = greenTop[i-1]; y1 <= greenBottom[i-1]; y1++) {
                                            index4 = index5 + y1*xDim;
                                            for (x1 = greenLeft[i-1]; x1 <= greenRight[i-1]; x1++) {
                                                index3 = index4 + x1;
                                                if (greenIDArray[index3] == i) {
                                                    distX = (x1 - x) * xRes;
                                                    distY = (y1 - y) * yRes;
                                                    distZ = (z1 - z) * zRes;
                                                    distanceSquared = distX*distX + distY*distY + distZ*distZ;
                                                    if (distanceSquared < minDistanceSquared) {
                                                        minDistanceSquared = distanceSquared;
                                                    } // if (distanceSquared < minDistanceSquared)
                                                } // if (greenIDArray[index3] == i)
                                            } // for (x1 = greenLeft[i-1]; x1 <= greenRight[i-1]; x1++)
                                        } // for (y1 = greenTop[i-1]; y1 <= greenBottom[i-1]; y1++)
                                    } // for (z1 = greenFront[i-1]; z1 <= greenBack[i-1]; z1++)
                                } // if (greenIDArray[index] == j)
                            } // for (x = greenLeft[j-1]; x <= greenRight[j-1]; x++)
                        } // for (y = greenTop[j-1]; y <= greenBottom[j-1]; y++)
                    } // for (z = greenFront[j-1]; z <= greenBack[j-1]; z++)
                    if (minDistanceSquared <= mergingDistanceSquared) {
                        greenIntensityTotal[i-1] = 0.0f;
                        greenVolume[i-1] = 0;
                        greenLeft[i-1] = Math.min(greenLeft[i-1], greenLeft[j-1]);
                        greenRight[i-1] = Math.max(greenRight[i-1], greenRight[j-1]);
                        greenTop[i-1] = Math.min(greenTop[i-1], greenTop[j-1]);
                        greenBottom[i-1] = Math.max(greenBottom[i-1], greenBottom[j-1]);
                        greenFront[i-1] = Math.min(greenFront[i-1], greenFront[j-1]);
                        greenBack[i-1] = Math.max(greenBack[i-1], greenBack[j-1]);
                        for (k = 0; k < totLength; k++) {
                            if (greenIDArray[k] == j) {
                                greenIDArray[k] = (byte)i;
                            }
                            if (greenIDArray[k] == i) {
                                greenIntensityTotal[i-1] += greenBuffer[k];
                                greenVolume[i-1]++;
                            }
                        } // for (k = 0; k < length; k++)
                        for (m = j+1; m <= numGreenTotal; m++) {
                            for (k = 0; k < totLength; k++) {
                                if (greenIDArray[k] == m) {
                                    greenIDArray[k] = (byte)(m-1);
                                }
                            }
                            greenIntensityTotal[m-2] = greenIntensityTotal[m-1];
                            greenVolume[m-2] = greenVolume[m-1];
                            greenCellNumber[m-2] = greenCellNumber[m-1];
                            greenLeft[m-2] = greenLeft[m-1];
                            greenRight[m-2] = greenRight[m-1];
                            greenTop[m-2] = greenTop[m-1];
                            greenBottom[m-2] = greenBottom[m-1];
                            greenFront[m-2] = greenFront[m-1];
                            greenBack[m-2] = greenBack[m-1];
                        }
                        if (j <= numGreenObjects) {
                            numGreenObjects--;
                        }
                        else {
                            numGreenObjects2--;
                        }
                        numGreenTotal--;
                        continue loop1;
                    } // if (distanceSquared <= mergingDistanceSquared)
                } // if (greenCellNumber[j-1] == greenCellNumber[i-1])
            } // for (i = 1; i < j; i++)
        } // for (j = numGreenTotal; j >= 1; j--)

        fireProgressStateChanged("Sorting green objects by intensity count");
        fireProgressStateChanged(66);
        greenIntensityPerVolume = new float[numGreenTotal];
        for (j = 1; j <= numGreenObjects; j++) {
            id = greenCellNumber[j-1];
            greenIntensityPerVolume[j-1] = greenIntensityTotal[j-1]/greenVolume[j-1];

            if (id >= 1) {

                if (greenNucleusNumber[id - 1] < greenRegionNumber) {
                    greenNucleusNumber[id - 1]++;
                }

                found = false;

                for (i = 0; (i < greenNucleusNumber[id - 1]) && !found; i++) {

                    if (greenIntensityPerVolume[j - 1] >= sortedGreenIntensity[id - 1][i]) {
                        found = true;

                        if ((i == 0) && (greenNucleusNumber[id-1] >= 2)) {
                            sortedGreenIntensity[id - 1][1] = sortedGreenIntensity[id - 1][0];
                            sortedGreenIndex[id - 1][1] = sortedGreenIndex[id - 1][0];
                            isMaxGreen[id - 1][1] = true;
                        }

                        sortedGreenIntensity[id - 1][i] = greenIntensityPerVolume[j - 1];
                        sortedGreenIndex[id - 1][i] = j; // from greenIDArray
                        isMaxGreen[id - 1][i] = true;
                    }
                } // for (i = 0; i < greenNucleusNumber[id-1]  && !found; i++)
            } // if (id >= 1)
        } // for (j = 1; j <= numGreenObjects; j++)
        for (j = numGreenObjects+1; j <= numGreenTotal; j++) {
            id = greenCellNumber[j-1];
            greenIntensityPerVolume[j-1] = greenIntensityTotal[j-1]/greenVolume[j-1];

            if (id >= 1) {

                if (greenNucleusNumber[id - 1] < greenRegionNumber) {
                    greenNucleusNumber[id - 1]++;
                }

                found = false;

                for (i = 0; (i < greenNucleusNumber[id - 1]) && !found; i++) {

                    if ((!isMaxGreen[id-1][i])&& (greenIntensityPerVolume[j - 1] >= sortedGreenIntensity[id - 1][i])) {
                        found = true;

                        if ((i == 0) && (greenNucleusNumber[id-1] >= 2)) {
                            sortedGreenIntensity[id - 1][1] = sortedGreenIntensity[id - 1][0];
                            sortedGreenIndex[id - 1][1] = sortedGreenIndex[id - 1][0];
                        }

                        sortedGreenIntensity[id - 1][i] = greenIntensityPerVolume[j - 1];
                        sortedGreenIndex[id - 1][i] = j; // from greenIDArray
                    }
                } // for (i = 0; i < greenNucleusNumber[id-1]  && !found; i++)
            } // if (id >= 1)    
        } // for (j = numGreenObjects+1; j <= numGreenTotal; j++)
        

        greenIntensityTotal = null;
        greenIntensityPerVolume = null;

        for (i = 0; i < numObjects; i++) {
            sortedGreenIntensity[i] = null;
            isMaxGreen[i] = null;
        }
        isMaxGreen = null;
        sortedGreenIntensity = null;

        numVOIObjects = 0;

        for (i = 0; i < numObjects; i++) {
            numVOIObjects += greenNucleusNumber[i];
        }

        colorTable = new Color[numVOIObjects];
        nameTable = new String[numVOIObjects];

        Arrays.fill(byteBuffer, (byte) 0);

        for (i = 0, j = 0; i < numObjects; i++) {

            for (m = 0; m < greenNucleusNumber[i]; m++) {

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
            } // for (m = 0; m < greenNucleusNumber[i]; m++)
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
        fireProgressStateChanged(70);
        algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
        algoVOIExtraction.setColorTable(colorTable);
        algoVOIExtraction.setNameTable(nameTable);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;
        System.gc();

        srcImage.setVOIs(grayImage.getVOIs());

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        
        idCount = new int[numObjects];
        xPosGrav = new float[nVOIs];
        yPosGrav = new float[nVOIs];
        zPosGrav = new float[nVOIs];
        objectID = new int[nVOIs];
        colorCount = new float[nVOIs];

        voiCount = new int[nVOIs];
        shortMask = new short[totLength];
        for (i = 0; i < nVOIs; i++) {
            Preferences.debug("i = " + i + "\n");
            fireProgressStateChanged("Processing green VOI " + (i + 1) + " of " + nVOIs);
            fireProgressStateChanged(72);
            VOIs.VOIAt(i).setOnlyID((short) i);
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                Preferences.debug("VOI contour i = " + i + "\n");
                Arrays.fill(shortMask, (short) -1);

                shortMask = srcImage.generateVOIMask(shortMask, i);
                greenCount = 0;
                Arrays.fill(idCount, 0);

                for (j = 0; j < totLength; j++) {

                    if (shortMask[j] != -1) {
                        greenCount += greenBuffer[j];

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
                
                xPosGrav[i] = 0.0f;
                yPosGrav[i] = 0.0f;
                zPosGrav[i] = 0.0f;
                
                colorCount[i] = 0.0f;

                for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                    for (j = k, y = 0; y < yDim; y++, j += xDim) {

                        for (x = 0; x < xDim; x++) {
                            index = x + j;

                            if (shortMask[index] != -1) {
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

                xPosGrav[i] /= colorCount[i];
                yPosGrav[i] /= colorCount[i];
                zPosGrav[i] /= colorCount[i];

                
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        fireProgressStateChanged("Extracting VOIs from blue image");
        fireProgressStateChanged(78);      
        
        blueVOIs = grayImage.getVOIs();
        trim = true;
        for (i = 0; i < numObjects; i++) {
            grayImage.resetVOIs();
            for (j = 0; j < totLength; j++) {
                IDArray2[j] = 0;
                if (IDArray[j] == (i+1)) {
                    IDArray2[j] = (byte)(i+1);    
                }
            }
            
            try {
                grayImage.importData(0, IDArray2, true);
            } catch (IOException error) {
                byteBuffer = null;
                errorCleanUp("Error on grayImage.importData", true);

                return;
            }
            
            algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
            algoVOIExtraction.run();
            algoVOIExtraction.finalize();
            algoVOIExtraction = null;
            
            blueVOIs = grayImage.getVOIs();
            
            if (blueSmooth) {
                newVOI = new VOI((short) 1, "blueVOI", VOI.CONTOUR, -1.0f);
                if ((blueVOIs.size() != 0) && (blueVOIs.VOIAt(0).getCurves().size() != 0)) {
                    contours = blueVOIs.VOIAt(0).getSortedCurves(zDim);
                    for (z = 0; z < zDim; z++) {
                        if (contours[z].size() != 0) {
                            maxPoints = 0;
                            maxElement = 0;
                            for (j = 0; j < contours[z].size(); j++) {
                                nPoints = ((VOIContour)contours[z].elementAt(j)).size();
                                if (nPoints > maxPoints) {
                                    maxPoints = nPoints;
                                    maxElement = j;
                                }
                            }
                            ((VOIContour) (contours[z].elementAt(maxElement))).trimPoints(1.0, true);
                            srcGon = ((VOIContour) (contours[z].elementAt(maxElement))).exportPolygon();
                            
                            xPoints = new float[srcGon.npoints + 5];
                            yPoints = new float[srcGon.npoints + 5];
                
                            xPoints[0] = srcGon.xpoints[srcGon.npoints - 2];
                            yPoints[0] = srcGon.ypoints[srcGon.npoints - 2];
                
                            xPoints[1] = srcGon.xpoints[srcGon.npoints - 1];
                            yPoints[1] = srcGon.ypoints[srcGon.npoints - 1];
                
                            for (j = 0; j < srcGon.npoints; j++) {
                                xPoints[j + 2] = srcGon.xpoints[j];
                                yPoints[j + 2] = srcGon.ypoints[j];
                            }
                
                            xPoints[srcGon.npoints + 2] = srcGon.xpoints[0];
                            yPoints[srcGon.npoints + 2] = srcGon.ypoints[0];
                
                            xPoints[srcGon.npoints + 3] = srcGon.xpoints[1];
                            yPoints[srcGon.npoints + 3] = srcGon.ypoints[1];
                
                            xPoints[srcGon.npoints + 4] = srcGon.xpoints[2];
                            yPoints[srcGon.npoints + 4] = srcGon.ypoints[2];
                
                            arcLength = new AlgorithmArcLength(xPoints, yPoints);
                            defaultPts = Math.round(arcLength.getTotalArcLength() / interpolationDivisor);
                            newVOI.removeCurves();
                            newVOI.importCurve((VOIContour)contours[z].elementAt(maxElement));
                            newVOI.setAllActive(true);
                            smoothAlgo = new AlgorithmBSmooth(grayImage2D, newVOI, defaultPts, trim);
                            smoothAlgo.run();
                            if (smoothAlgo.isCompleted()) {
                                // The algorithm has completed and produced a VOI
                                resultVOI = smoothAlgo.getResultVOI();
                                Vector<VOIBase> removeCurves = blueVOIs.VOIAt(0).getSliceCurves(z);
                        		for (j = 0; j < removeCurves.size(); j++ )
                        		{
                        			blueVOIs.VOIAt(0).getCurves().removeElement(removeCurves.elementAt(j));
                        		}
                                blueVOIs.VOIAt(0).importCurve((VOIContour)resultVOI.getCurves().elementAt(0));
                            } // if (smoothAlgo.isCompleted())
                            smoothAlgo.finalize();
                            smoothAlgo = null;
                        } // if (contours[z].size() != 0)
                    } // for (z = 0; z < zDim; z++)
                } // if ((blueVOIs.size() != 0) && (blueVOIs.VOIAt(0).getCurves().length != 0))
                // Change IDArray to reflect the changed boundaries of the voi
                Arrays.fill(shortMask, (short) -1);
                shortMask = grayImage.generateVOIMask(shortMask, 0);
                if (shortMask != null) {
                    for (j = 0; j < totLength; j++) {
                        if (IDArray[j] == (i+1)) {
                            IDArray[j] = 0;
                        }
                        if (shortMask[j] != -1) {
                            IDArray[j] = (byte)(i + 1);
                        }
                    }
                    blueVOIs.VOIAt(0).setColor(Color.yellow.darker());
                    VOIs.add(blueVOIs.VOIAt(0));
                } // if (shortMask != null)
                else {
                    shortMask = new short[totLength];
                }
            } // if (blueSmooth)
            else {
                blueVOIs.VOIAt(0).setColor(Color.yellow.darker());
                VOIs.add(blueVOIs.VOIAt(0));    
            }
        } // for (i = 0; i < numObjects; i++)
        grayImage2D.disposeLocal();
        grayImage2D = null;
        
        srcImage.setVOIs((VOIVector)VOIs);

        
        
        // Reorder the indices for the green VOIs and their associated arrays so that
        // the indices use the sequence:
        // blue object 1 - large green VOI
        // blue object 1 - small green VOI
        // blue object 2 - large green VOI
        // blue object 2 - small green VOI
        // and so on
        gIndex = new int[2];
        gCount = new float[2];
        VOIs2 = new ViewVOIVector();
        objectID2 = new int[nVOIs];
        xPosGrav2 = new float[nVOIs];
        yPosGrav2 = new float[nVOIs];
        zPosGrav2 = new float[nVOIs];
        voiCount2 = new int[nVOIs];
        index2 = 0;
        for (j = 1; j <= numObjects; j++) {
            numGreenFound = 0;
            gCount[0] = 0.0f;
            gCount[1] = 0.0f;
            for (i = 0; i < nVOIs; i++) {
                if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) && (objectID[i] == j)) {
                    
                    if (numGreenFound < 2) {
                        numGreenFound++;
                    }
                    placed = false;
                    for (k = 0; k <= 1 && (!placed); k++) {
                        if (colorCount[i] >= gCount[k]) {
                            placed = true; 
                            if ((k == 0) && (numGreenFound == 2)) {
                                gIndex[1] = gIndex[0];
                                gCount[1] = gCount[0];
                            }
                            gIndex[k] = i;
                            gCount[k] = colorCount[i];
                        }
                    }
                }
            } // for (i = 0; i < nVOIs; i++)
            
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
            xPosGrav[i] = xPosGrav2[i];
            yPosGrav[i] = yPosGrav2[i];
            zPosGrav[i] = zPosGrav2[i];
            voiCount[i] = voiCount2[i];
        }
        objectID2 = null;
        xPosGrav2 = null;
        yPosGrav2 = null;
        zPosGrav2 = null;
        voiCount2 = null;
        
        for (i = 0; i < nVOIs; i++) {
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                voiName = VOIs.VOIAt(i).getName();
                newPtVOI = new VOI((short) (i + nVOIs), voiName, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xArr[0] = xPosGrav[i];
                yArr[0] = yPosGrav[i];
                zArr[0] = zPosGrav[i];
                newPtVOI.importCurve(xArr, yArr, zArr);
                newPtVOI.getCurves().lastElement().setFixed(true);
                newPtVOI.getCurves().lastElement().setLabel(voiName);
                srcImage.registerVOI(newPtVOI);    
            }
        }

        

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
        
        for (id = 1; id <= numObjects; id++) {
            fireProgressStateChanged("Processing object " + id + " of " + numObjects + " in blue segmented image");
            fireProgressStateChanged(85 + (id * 10 / numObjects));

            Arrays.fill(byteBuffer, (byte) 0);
            xCenter[id - 1] = 0.0f;
            yCenter[id - 1] = 0.0f;
            zCenter[id - 1] = 0.0f;
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
        } // for (id = 1; id <= numObjects; id++)

        grayImage.disposeLocal();
        grayImage = null;
        System.gc();


        for (i = 0; i < numObjects; i++) {
            newPtVOI = new VOI((short) (i + (2 * nVOIs)), "Cen" + (i+1), VOI.POINT, -1.0f);
            newPtVOI.setColor(Color.white);
            xArr[0] = xCenter[i];
            yArr[0] = yCenter[i];
            zArr[0] = zCenter[i];
            newPtVOI.importCurve(xArr, yArr, zArr);
            newPtVOI.getCurves().lastElement().setFixed(true);
            newPtVOI.getCurves().lastElement().setLabel("Cen" + (i + 1));
            srcImage.registerVOI(newPtVOI);
        } // for (i = 0; i < numObjects; i++)

        srcImage.notifyImageDisplayListeners();
        
        UI.getMessageFrame().addTab("PlugInAlgorithmCenterDistance");
        UI.getMessageFrame().append("PlugInAlgorithmCenterDistance", "\n");
        UI.getMessageFrame().append("PlugInAlgorithmCenterDistance", "File Name \t\tNucleus\tNVolume\tRmin\tRmax" +
                                    "\tObj\tGvolume\tcenterToG\tcenterToGToEdge\tfractionGOut\n");

        for (i = 0; i < numObjects; i++) {
            nuclearVolume = xRes * yRes * zRes * blueCountTotal[i];
            
            greenFound = 0;

            for (j = 0; j <= (nVOIs - 1); j++) {

                if (objectID[j] == (i + 1)) {
                    Preferences.debug("VOI name found = " + VOIs.VOIAt(j).getName() + "\n");

                    greenFound++;

                    voiVolume = xRes * yRes * zRes * voiCount[j];
                    

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
                    lastEdgeX = Math.round(xPosGrav[j]);
                    lastEdgeY = Math.round(yPosGrav[j]);
                    lastEdgeZ = Math.round(zPosGrav[j]);

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
                        newEdgeX = Math.round(xPosGrav[j] + (numInc * incx));

                        if ((newEdgeX < 0) || (newEdgeX >= xDim)) {
                            break;
                        }

                        newEdgeY = Math.round(yPosGrav[j] + (numInc * incy));

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
                    fractionOut = gravToCenter/centerToGravToEdge;
                    
                    UI.getMessageFrame().append("PlugInAlgorithmCenterDistance",
                            srcImage.getFileInfo(0).getFileName() + "\t" + (i+1) + "\t" + df.format(nuclearVolume) +
                            "\t" + df.format(centerToNearEdge[i]) + "\t" + df.format(centerToFarEdge[i]) +
                            "\t" + (j+1) + "\t" + df.format(voiVolume) + "\t" + df.format(gravToCenter) +
                            "\t" + df.format(centerToGravToEdge) + "\t\t" + dfFract.format(fractionOut) + "\n");
                } // if (objectID[j] == (i+1))
            } // for (j = 0; j <= nVOIs - 1; j++)

        } // for (i = 0; i < numObjects; i++)

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }

    
}
