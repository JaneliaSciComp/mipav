import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;

import javax.swing.*;


/**
 * This shows how to extend the AlgorithmBase class.
 *
 * @version  April 14, 2008
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmCenterDistance.java $ $Revision: 72 $ $Date: 2/06/06 5:50p $
 *           PlugInAlgorithmCenterDistance is used to find the distances between geometric cell centers and cell
 *           boundaries, geometric cell centers and voi geometric centers, geometric cell centers and voi centers of
 *           mass, voi geometric centers and cell boundaries, and voi centers of mass and cell boundaries. The geometric
 *           center is a zero order moment not using intensity multiplications. The center of mass is a first order
 *           moment mutliplying position by intensity. The cells are blue and the chromosome regions of interest
 *           outlined by the vois are predominantly green. Images are 2D. VOIs can be supplied by the
 *           user, but if no VOI is supplied by the user the program will automatically generate green VOIs
 *           in each cell. The user can input the number of green VOIs to be looked for in each cell. The
 *           default is 1 green VOI.</p>
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
 *           
 *           <p>5.) Assign positive integer IDs to each object between blueMin and 200,000 pixels in size in the 2D
 *           program or between blueMin and 2,000,000 pixels in size in the 3D program. blueSegImage now has 0 for
 *           background and a positive integer ID for each object between the minimum and maximum size. blueMin has a
 *           default of 1,000.</p>
 *
 *           <p>6.) Set all the pixels in any blue object touching an edge to 0.</p>
 *
 *           <p>11.) Export the green portion of the image to greenBuffer.
 *           </p>
 *
 *           <p>12.) If no user supplied contour VOIs are present, the program automatically generates VOIs.</p>
 *
 *           <p>13.) Process each VOI one at a time. a.) For each VOI find the sum
 *           of the green intensity values, and the pixel count for each ID object. b.) Look at all the object pixel
 *           counts and assign the ID of the object with the most pixels in the VOI to the VOI's objectID. c.) Find the
 *           center of the VOI. Find the green center. Both
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
public class PlugInAlgorithmCenterDistance extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Green merging radius around peak green spot */
    private float mergingDistance = 0.1f;
    
    // Number of green regions per cell
    // Either 1 for 1 for all cells, 2 for 2 for all cells, or 0 for 1 or 2 for all cells
    private int greenRegionNumber = 1;
    
    /** DOCUMENT ME! */
    private int blueMin = 1000;

    /** Portion of green pixels above threshold in fuzzy c means. */
    private float greenFraction = 0.01f;

    /** DOCUMENT ME! */
    private int greenMin = 10;
    
    private boolean twoGreenLevels;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg         Source image model.
     * @param  blueMin
     * @param  mergingDistance         DOCUMENT ME!
     * @param  greenMin
     * @param  greenFraction
     * @param  greenRegionNumber      DOCUMENT ME!
     * @param  twoGreenLevels
     */
    public PlugInAlgorithmCenterDistance(ModelImage srcImg, int blueMin, float mergingDistance, 
                                         int greenMin, float greenFraction,  int greenRegionNumber,
                                         boolean twoGreenLevels) {
        super(null, srcImg);
        this.blueMin = blueMin;
        this.mergingDistance = mergingDistance;
        this.greenMin = greenMin;
        this.greenFraction = greenFraction;
        this.greenRegionNumber = greenRegionNumber;
        this.twoGreenLevels = twoGreenLevels;
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

        int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        FileInfoBase fileInfo;
        int numGreenObjects = 0;
        byte[] greenIDArray = null;
        float[] greenIntensityTotal;
        float[] greenXCenter;
        float[] greenYCenter;
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
        String[] nameTable = null;
        String voiName;
        float[] blueCountTotal = null;
        AlgorithmMorphology2D fillHolesAlgo2D;
        int[] voiCount;
        int[] voiCount2;

        int greenFound;
        float nuclearArea;

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
        int numGreenVOIObjects;
        int edgeObjects;
        AlgorithmHistogram algoHist;
        int bins;
        int[] histoBuffer;
        double[] lowValue;
        int totalCount;
        int countsToRetain;
        int countsFound;
        boolean initiallyOneObject;
        int response;
        int numGreenFound;
        int gIndex[];
        float gCount[];
        boolean placed;
        int index2;
        int greenCellNumber[];
        double distX;
        double distY;
        double distance;
        int greenBelong[];


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
        
        if (greenRegionNumber == 0) {
            greenRegionNumber = 2;
        }

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
        
        numPruningPixels = 0;
        edgingType = 0;
        
        byteBuffer = new byte[length];

        try {
            grayImage.exportData(0, length, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
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

        
        IDArray = new byte[length];

        try {
            grayImage.exportData(0, length, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

        if (nVOIs > 0) {
            fireProgressStateChanged(65);
        } else {
            fireProgressStateChanged(22);
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
        
        for (id = 1; id <= numObjects; id++) {
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
                    } else if (IDArray[i] > id) {
                        IDArray[i]--;
                    }
                }

                numObjects--;
                id--;
            } // if (removeID)
        } // for (id = 1; id <= numObjects; id++)

        if (numObjects == 0) {
            MipavUtil.displayError(edgeObjects + " blue objects touched edges");
            MipavUtil.displayError("No blue objects that do not touch edges");

            setCompleted(false);

            return;
        }


        
        greenBuffer = new float[length];

        try {

            srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        }

        if (nVOIs == 0) {

            

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
            if (twoGreenLevels) {
                thresholds[0] = max-1;
            }
            else {
                thresholds[0] = max;
            }
            thresholds[1] = max;
            fillValue = 0.0f;
            thresholdAlgo = new AlgorithmThresholdDual(grayImage, thresholds, fillValue,
                                                       AlgorithmThresholdDual.BINARY_TYPE, wholeImage, false);
            thresholdAlgo.run();
            thresholdAlgo.finalize();
            thresholdAlgo = null;

            
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
            idObjectsAlgo2D.setMinMax(greenMin, 200);
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

            // Sort the green objects within each nucleus.
            // Create no more than greenNumber green objects within each nucleus.
            fireProgressStateChanged("Sorting green objects by intensity count");
            fireProgressStateChanged(62);
            greenIntensityTotal = new float[numGreenObjects];
            greenXCenter = new float[numGreenObjects];
            greenYCenter = new float[numGreenObjects];
            greenCellNumber = new int[numGreenObjects];
            greenNucleusNumber = new int[numObjects];
            sortedGreenIndex = new int[numObjects][greenRegionNumber+1];
            sortedGreenIntensity = new float[numObjects][greenRegionNumber+1];
            sortedGreenXCenter = new float[numObjects][greenRegionNumber+1];
            sortedGreenYCenter = new float[numObjects][greenRegionNumber+1];
            
            for (j = 1; j <= numGreenObjects; j++) {
                greenBelong = new int[numObjects+1];
                for (x = 0, y = 0, i = 0; i < length; i++) {

                    if (greenIDArray[i] == j) {
                        greenIntensityTotal[j - 1] += greenBuffer[i];
                        greenXCenter[j - 1] += greenBuffer[i] * x;
                        greenYCenter[j - 1] += greenBuffer[i] * y;
                        greenBelong[IDArray[i]] = greenBelong[IDArray[i]] + 1;
                    }

                    x++;

                    if (x == xDim) {
                        x = 0;
                        y++;
                    }
                }

                greenXCenter[j - 1] = greenXCenter[j - 1] / greenIntensityTotal[j - 1];
                greenYCenter[j - 1] = greenYCenter[j - 1] / greenIntensityTotal[j - 1];
                greenCellNumber[j-1] = 0;
                for (k = 1; k <= numObjects; k++) {
                    if (greenBelong[k] > greenCellNumber[j-1]) {
                        greenCellNumber[j-1] = k;
                    }
                }
                
                // Expand IDArray to include green regions protruding out of the blue cell boundary
                for (i = 0; i < length; i++) {
                    if ((greenIDArray[i] == j) && (IDArray[i] == 0)) {
                        IDArray[i] = (byte)greenCellNumber[j-1];
                    }
                }
            }
            
            // Merge green regions in the same cell within merging distance together
            loop1:
            for (j = numGreenObjects; j >= 1; j--) {
                for (i = 1; i < j; i++) {
                    if (greenCellNumber[j-1] == greenCellNumber[i-1]) {
                        distX = greenXCenter[j-1] - greenXCenter[i-1];
                        distY = greenYCenter[j-1] - greenYCenter[i-1];
                        distance = Math.sqrt(distX*distX + distY*distY);
                        if (distance <= mergingDistance) {
                            greenIntensityTotal[i-1] = 0.0f;
                            greenXCenter[i-1] = 0.0f;
                            greenYCenter[i-1] = 0.0f;
                            for (x = 0, y = 0, k = 0; k < length; k++) {
                                if (greenIDArray[k] == j) {
                                    greenIDArray[k] = (byte)i;
                                }
                                if (greenIDArray[k] == i) {
                                    greenIntensityTotal[i-1] += greenBuffer[k];
                                    greenXCenter[i-1] += greenBuffer[k] * x;
                                    greenYCenter[i-1] += greenBuffer[k] * y;
                                }
                                
                                x++;
                                if (x == xDim) {
                                    x = 0;
                                    y++;
                                }
                            } // for (x = 0, y = 0, k = 0; k < length; k++)
                            greenXCenter[i - 1] = greenXCenter[i - 1] / greenIntensityTotal[i - 1];
                            greenYCenter[i - 1] = greenYCenter[i - 1] / greenIntensityTotal[i - 1];
                            numGreenObjects--;
                            continue loop1;
                        } // if (distance <= mergingDistance)
                    }
                }
            }

            for (j = 1; j <= numGreenObjects; j++) {
                id = greenCellNumber[j-1];

                if (id >= 1) {

                    if (greenNucleusNumber[id - 1] < greenRegionNumber) {
                        greenNucleusNumber[id - 1]++;
                    }

                    found = false;

                    for (i = 0; (i < greenNucleusNumber[id - 1]) && !found; i++) {

                        if (greenIntensityTotal[j - 1] >= sortedGreenIntensity[id - 1][i]) {
                            found = true;

                            for (k = greenNucleusNumber[id - 1] - 1; k >= i; k--) {
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

            Arrays.fill(byteBuffer, (byte) 0);

            numGreenVOIObjects = 0;

            for (i = 0; i < numObjects; i++) {

                

                numGreenVOIObjects += greenNucleusNumber[i];
            }

            numVOIObjects = numGreenVOIObjects;
            colorTable = new Color[numVOIObjects];
            nameTable = new String[numVOIObjects];

            

            Arrays.fill(byteBuffer, (byte) 0);

            for (i = 0, j = 0; i < numObjects; i++) {

                for (m = 0; m < greenRegionNumber; m++) {

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

            srcImage.setVOIs(grayImage.getVOIs());
            grayImage.resetVOIs();

            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
            
            try {
                grayImage.importData(0, IDArray, true);
            } catch (IOException error) {
                byteBuffer = null;
                errorCleanUp("Error on grayImage.importData", true);

                return;
            }

            fireProgressStateChanged("Extracting VOIs from blue image");
            fireProgressStateChanged(76);
            algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
            algoVOIExtraction.run();
            algoVOIExtraction.finalize();
            algoVOIExtraction = null;
            for (i = 0; i < grayImage.getVOIs().size(); i++) {
                grayImage.getVOIs().VOIAt(i).setColor(Color.yellow.darker());
                VOIs.add(grayImage.getVOIs().VOIAt(i));
            }
            srcImage.setVOIs((VOIVector)VOIs);
        } // if (nVOIs == 0)

        shortMask = new short[length];

        idCount = new int[numObjects];
        xPosGeo = new float[nVOIs];
        yPosGeo = new float[nVOIs];
        xPosGrav = new float[nVOIs];
        yPosGrav = new float[nVOIs];
        objectID = new int[nVOIs];
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

                xPosGeo[i] = 0.0f;
                yPosGeo[i] = 0.0f;
                xPosGrav[i] = 0.0f;
                yPosGrav[i] = 0.0f;

                
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

                
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)
        
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
            numGreenFound = 0;
            gCount[0] = 0.0f;
            gCount[1] = 0.0f;
            gCount[2] = 0.0f;
            for (i = 0; i < nVOIs; i++) {
                if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) && (objectID[i] == j)) {
                    if (numGreenFound < 2) {
                        numGreenFound++;
                    }
                    placed = false;
                    for (k = 0; k <= 1 && (!placed); k++) {
                        if (colorCount[i] >= gCount[k]) {
                            placed = true; 
                            for (m = numGreenFound - 1; m >= k; m--) {
                                gIndex[m+1] = gIndex[m];
                                gCount[m+1] = gCount[m];
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
                newPtVOI = new VOI((short) (i + nVOIs), "point2D" + i + ".voi", 1, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xArr[0] = xPosGrav[i];
                yArr[0] = yPosGrav[i];
                zArr[0] = 0.0f;
                newPtVOI.importCurve(xArr, yArr, zArr, 0);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setLabel(voiName);
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
            newPtVOI = new VOI((short) (i + (2 * nVOIs)), "point2D" + i + ".voi", 1, VOI.POINT, -1.0f);
            newPtVOI.setColor(Color.magenta);
            xArr[0] = xCenter[i];
            yArr[0] = yCenter[i];
            zArr[0] = 0.0f;
            newPtVOI.importCurve(xArr, yArr, zArr, 0);
            ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
            ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setLabel("N" + (i + 1));
            srcImage.registerVOI(newPtVOI);
        } // for (i = 0; i < numObjects; i++)

        srcImage.notifyImageDisplayListeners();

        UI.setDataText("Plugin 04/14/08 version\n");
        UI.setDataText(srcImage.getFileInfo(0).getFileName() + "\n");

        if (xUnits != FileInfoBase.UNKNOWN_MEASURE) {
            UI.setDataText("X resolution = " + xRes + "  " + FileInfoBase.getUnitsOfMeasureStr(xUnits) + "\n");
        } else {
            UI.setDataText("X resolution = " + xRes + "\n");
        }

        if (yUnits != FileInfoBase.UNKNOWN_MEASURE) {
            UI.setDataText("Y resolution = " + yRes + "  " + FileInfoBase.getUnitsOfMeasureStr(yUnits) + "\n\n");
        } else {
            UI.setDataText("Y resolution = " + yRes + "\n\n");
        }

        UI.setDataText("Nucleus\tNarea\tRmin\tRmax");

        if (greenRegionNumber > 0) {
            UI.setDataText("\tG1area");
            UI.setDataText("\tcenterToG1\tcenterToG1ToEdge");
        }

        if (greenRegionNumber > 1) {
            UI.setDataText("\tG2area");
            UI.setDataText("\tcenterToG2\tcenterToG2ToEdge");
        }

        UI.setDataText("\n");

        for (i = 0; i < numObjects; i++) {
            nuclearArea = xRes * yRes * blueCountTotal[i];
            UI.setDataText((i + 1) + "\t" + nf.format(nuclearArea));
            UI.setDataText("\t" + nf.format(centerToNearEdge[i]));

            // equivalentRadius = (float)Math.sqrt(nuclearArea/Math.PI);
            // UI.setDataText("\t" + nf.format(equivalentRadius));
            UI.setDataText("\t" + nf.format(centerToFarEdge[i]));
            greenFound = 0;

            for (j = 0; j <= (nVOIs - 1); j++) {

                if (objectID[j] == (i + 1)) {
                    greenFound++;

                    

                    voiArea = xRes * yRes * voiCount[j];
                    if (j == 1) {
                        UI.setDataText("\t");
                    }
                    UI.setDataText("\t" + nf.format(voiArea));

                    /*geoToCenter = (float)
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
                    UI.setDataText("\t" + nf.format(geoToCenter));
                    UI.setDataText("\t\t" + nf.format(centerToGeoToEdge));*/
                    


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
                    UI.setDataText("\t" + nf.format(gravToCenter));
                    UI.setDataText("\t" + nf.format(centerToGravToEdge));


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

    
}
