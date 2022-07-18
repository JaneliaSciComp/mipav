import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;



import java.io.*;


/**
 * This shows how to extend the AlgorithmBase class.
 *
 * @version  March 29, 2004
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmFISHAnalysis.java $ $Revision: 3 $ $Date: 11/18/05 1:12p $
 *           </p>
 */
public class PlugInAlgorithmFISHAnalysis extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for 3D images in which changes are placed in a predetermined destination image.
     *
     * @param  srcImg  Source image model.
     */
    public PlugInAlgorithmFISHAnalysis(ModelImage srcImg) {
        super(null, srcImg);
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
        ModelImage blueImage;
        AlgorithmFuzzyCMeans fcmAlgo;
        ModelImage[] blueSegImage;
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
        int kernel;
        float circleDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        boolean[] backgroundBuffer;
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
        float[] centerToEdge;
        int count;
        float[] redBuffer;
        float[] greenBuffer;
        ViewVOIVector VOIs = null;
        int nVOIs;
        short[] shortMask;
        int index;
        int redCount;
        int greenCount;
        int[] idCount;
        int objectID;
        int objectCount;
        float xPos, yPos;
        float colorCount;
        float regionToCenter;
        float regionToEdge;
        ViewUserInterface UI = ViewUserInterface.getReference();
        float distSqr;
        float lowestSqr;
        int xEdge = -1;
        int yEdge = -1;
        int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        String unitsString = null;
        FileInfoBase fileInfo;
        FileInfoBase fileInfo2;

        /**
         * Added by Prabhakar (Reddy) Gudla, NCI/SAIC-Frederick Date: 06/03/2005
         *
         */
        AlgorithmVOIProps algoVOIProps;
        AlgorithmVOIExtraction algoVOIExtraction;

        float[] xPosVOIs;
        float[] yPosVOIs;
        float[] distBtwVOIs;

        float[] bufferR;
        float[] bufferG;
        ModelImage greenImage;
        ModelImage redImage;
        FileInfoBase fileInfoG;
        FileInfoBase fileInfoR;
        FileInfoBase fileInfoG2;
        FileInfoBase fileInfoR2;
        ModelImage[] greenSegImage;
        ModelImage[] redSegImage;
        int rfCnt = 0;
        int gfCnt = 0;
        // end addition

        if ((xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
            unitsString = (Unit.getUnitFromLegacyNum(xUnits)).toString();
        }

        try {

            // image length is length in 2 dims
            length = xDim * yDim;
            buffer = new float[length];
            bufferR = new float[length];
            bufferG = new float[length];
            srcImage.exportRGBData(3, 0, length, buffer); // export blue data
            srcImage.exportRGBData(2, 0, length, bufferG); // export green data
            srcImage.exportRGBData(1, 0, length, bufferR); // export red data
        } catch (IOException error) {
            buffer = null;
            bufferG = null;
            bufferR = null;
            errorCleanUp("Algorithm RegionDistance reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            bufferG = null;
            bufferR = null;
            errorCleanUp("Algorithm RegionDistance reports: out of memory", true);

            return;
        }

        fireProgressStateChanged("Processing image ...");


        fireProgressStateChanged("Creating blue image");
        blueImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_blue");
        fileInfo = blueImage.getFileInfo()[0];
        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        blueImage.setFileInfo(fileInfo, 0);
        fireProgressStateChanged(5);

        fireProgressStateChanged("Creating green image");
        greenImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_green");
        fileInfoG = greenImage.getFileInfo()[0];
        fileInfoG.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfoG.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        greenImage.setFileInfo(fileInfoG, 0);
        fireProgressStateChanged(10);

        fireProgressStateChanged("Creating red image");
        redImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_red");
        fileInfoR = redImage.getFileInfo()[0];
        fileInfoR.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfoR.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        redImage.setFileInfo(fileInfoR, 0);

        try {
            blueImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Error on blueImage.importData", true);

            return;
        }

        try {
            greenImage.importData(0, bufferG, true);
        } catch (IOException error) {
            bufferG = null;
            errorCleanUp("Error on greenImage.importData", true);

            return;
        }

        try {
            redImage.importData(0, bufferR, true);
        } catch (IOException error) {
            bufferR = null;
            errorCleanUp("Error on redImage.importData", true);

            return;
        }

        buffer = null;
        bufferG = null;
        bufferR = null;

        // Segment into 2 values
        fireProgressStateChanged("Performing FuzzyCMeans Segmentation on blue");
        fireProgressStateChanged(15);
        blueSegImage = new ModelImage[1];
        blueSegImage[0] = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(),
                                         blueImage.getImageName() + "_seg");
        fileInfo2 = blueSegImage[0].getFileInfo()[0];
        fileInfo2.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo2.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        blueSegImage[0].setFileInfo(fileInfo2, 0);

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
        fcmAlgo = new AlgorithmFuzzyCMeans(blueSegImage, blueImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q,
                                           oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground,
                                           threshold, maxIter, endTolerance, wholeImage);
        centroids = new float[2];
        min = (float) blueImage.getMin();
        max = (float) blueImage.getMax();
        centroids[0] = min + ((max - min) / 3.0f);
        centroids[1] = min + (2.0f * (max - min) / 3.0f);
        fcmAlgo.setCentroids(centroids);
        fcmAlgo.run();
        fcmAlgo.finalize();
        fcmAlgo = null;
        blueImage.disposeLocal();
        blueImage = null;


        // Segment Green into 2 values
        fireProgressStateChanged("Performing FuzzyCMeans Segmentation on green");
        fireProgressStateChanged(20);
        greenSegImage = new ModelImage[1];
        greenSegImage[0] = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(),
                                          greenImage.getImageName() + "_seg");
        fileInfoG2 = blueSegImage[0].getFileInfo()[0];
        fileInfoG2.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfoG2.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        greenSegImage[0].setFileInfo(fileInfoG2, 0);

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
        fcmAlgo = new AlgorithmFuzzyCMeans(greenSegImage, greenImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter,
                                           q, oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground,
                                           threshold, maxIter, endTolerance, wholeImage);
        centroids = new float[2];
        min = (float) greenImage.getMin();
        max = (float) greenImage.getMax();
        centroids[0] = min + ((max - min) / 3.0f);
        centroids[1] = min + (2.0f * (max - min) / 3.0f);
        fcmAlgo.setCentroids(centroids);
        fcmAlgo.run();
        fcmAlgo.finalize();
        fcmAlgo = null;
        greenImage.disposeLocal();
        greenImage = null;

        // Segment Red into 2 values
        fireProgressStateChanged("Performing FuzzyCMeans Segmentation on red");
        fireProgressStateChanged(25);
        redSegImage = new ModelImage[1];
        redSegImage[0] = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(),
                                        redImage.getImageName() + "_seg");
        fileInfoR2 = redSegImage[0].getFileInfo()[0];
        fileInfoR2.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfoR2.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        redSegImage[0].setFileInfo(fileInfoR2, 0);

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
        fcmAlgo = new AlgorithmFuzzyCMeans(redSegImage, redImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q,
                                           oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground,
                                           threshold, maxIter, endTolerance, wholeImage);
        centroids = new float[2];
        min = (float) redImage.getMin();
        max = (float) redImage.getMax();
        centroids[0] = min + ((max - min) / 3.0f);
        centroids[1] = min + (2.0f * (max - min) / 3.0f);
        fcmAlgo.setCentroids(centroids);
        fcmAlgo.run();
        fcmAlgo.finalize();
        fcmAlgo = null;
        redImage.disposeLocal();
        redImage = null;

        /*
         * try { imageFrameG=         new ViewJFrameImage(greenSegImage[0], null,         new Dimension(610, 200),
         * srcImage.getUserInterface()); } catch (OutOfMemoryError error) { System.gc(); UI.setDataText("Out of memory:
         * unable to open new frame"); }
         */
        /*
         * try { imageFrameR=         new ViewJFrameImage(redSegImage[0], null,         new Dimension(610, 200),
         * srcImage.getUserInterface()); } catch (OutOfMemoryError error) { System.gc(); UI.setDataText("Out of memory:
         * unable to open new frame"); }
         */

        // extract the VOI from the segmented green image
        fireProgressStateChanged("Extracting green-labelled FISH signals");
        fireProgressStateChanged(30);
        algoVOIExtraction = new AlgorithmVOIExtraction(greenSegImage[0]);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;

        // extract the VOI from the segmented red image
        fireProgressStateChanged("Extracting red-labelled FISH signals");
        fireProgressStateChanged(35);

        algoVOIExtraction = new AlgorithmVOIExtraction(redSegImage[0]);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;

        int gVOIs = greenSegImage[0].getVOIs().size();
        int rVOIs = redSegImage[0].getVOIs().size();
        // displayError("Before VOIs addition = " + srcImage.getVOIs().size());

        VOIVector allRGVOIs = new VOIVector(gVOIs + rVOIs - 2); // this to eliminate the outer VOIs
        VOIVector RVOIs = redSegImage[0].getVOIs();

        // this removes the outer boundary VOI
        RVOIs.removeElementAt(0);

        VOIVector GVOIs = greenSegImage[0].getVOIs();

        // this removes the outer boudnary VOI
        GVOIs.removeElementAt(0);
        allRGVOIs.addAll(GVOIs);
        allRGVOIs.addAll(RVOIs);


        srcImage.setVOIs(allRGVOIs);

        // MipavUtil.displayInfo("After VOIs addition = " + srcImage.getVOIs().size());
        allRGVOIs = null;
        allRGVOIs = srcImage.getVOIs();

        for (int voisCnt = 0; voisCnt < srcImage.getVOIs().size(); voisCnt++) {
            allRGVOIs.VOIAt(voisCnt).setAllActive(true);
        }

        srcImage.ungroupVOIs();
        // MipavUtil.displayInfo("After ungrouping the added VOIs = " + srcImage.getVOIs().size());

        // let's process both the VOIs and remove duplicate VOIs returned from Green and Red
        allRGVOIs = null;
        allRGVOIs = srcImage.getVOIs();

        VOIVector newallRGVOIs = new VOIVector();

        for (int rgvoisCnt = 0; rgvoisCnt < allRGVOIs.size(); rgvoisCnt++) {

            // UI.setDataText("\n I'm here start");
            boolean flagVOI = false;
            VOI cRGVOIs = allRGVOIs.VOIAt(rgvoisCnt);
            Vector3f geomCentercRGVOIs = cRGVOIs.getGeometricCenter();
            int areacRGVOIs = (int)cRGVOIs.area();
            VOIBase cRGVOIsPoly = cRGVOIs.getCurves().elementAt(0);
            // UI.setDataText("\n Number of polygons in C = " + rgvoisCnt + ", " + cRGVOIsPoly.length);

            for (int rgvoisCnt1 = rgvoisCnt + 1; rgvoisCnt1 < allRGVOIs.size(); rgvoisCnt1++) {
                VOI nRGVOIs = allRGVOIs.VOIAt(rgvoisCnt1);
                Vector3f centmassnRGVOIs = nRGVOIs.getGeometricCenter();
                int areanRGVOIs = (int)nRGVOIs.area();
                VOIBase nRGVOIsPoly = nRGVOIs.getCurves().elementAt(0);

                // UI.setDataText("\n  Number of polygons in N = "  + rgvoisCnt1 + ", " +  nRGVOIsPoly.length);
                if (nRGVOIsPoly.contains(geomCentercRGVOIs.X, geomCentercRGVOIs.Y)) {

                    if (areanRGVOIs < areacRGVOIs) {
                        newallRGVOIs.addElement(cRGVOIs);
                        allRGVOIs.removeElementAt(rgvoisCnt1);

                        // UI.setDataText("\nAdding ii = " + rgvoisCnt);
                        flagVOI = true;
                    } else if (areanRGVOIs > areacRGVOIs) {
                        newallRGVOIs.addElement(nRGVOIs);

                        // UI.setDataText("\nAdding ie = " + rgvoisCnt1);
                        flagVOI = true;
                    }
                } else if (cRGVOIsPoly.contains(centmassnRGVOIs.X, centmassnRGVOIs.Y)) {

                    if (areacRGVOIs < areanRGVOIs) {
                        newallRGVOIs.addElement(nRGVOIs);

                        // UI.setDataText("\nAdding ei = " + rgvoisCnt1);
                        flagVOI = true;
                    } else if (areacRGVOIs > areanRGVOIs) {
                        newallRGVOIs.addElement(cRGVOIs);
                        allRGVOIs.removeElementAt(rgvoisCnt1);

                        // UI.setDataText("\nAdding ee = " + rgvoisCnt);
                        flagVOI = true;
                    }
                }
            }

            if (!flagVOI) {
                newallRGVOIs.addElement(cRGVOIs);
                // UI.setDataText("\nAdding in if = " + rgvoisCnt);
            }
            // UI.setDataText("I'm here end");
        }

        allRGVOIs = null;
        srcImage.setVOIs(newallRGVOIs);
        newallRGVOIs = null;
        allRGVOIs = srcImage.getVOIs();

        for (int voisCnt = 0; voisCnt < allRGVOIs.size(); voisCnt++) {
            allRGVOIs.VOIAt(voisCnt).setAllActive(true);
        }

        srcImage.ungroupVOIs();

        // Now convert the min and max to 0 and 1
        fireProgressStateChanged("Setting segmented blue values to 0 and 1");
        fireProgressStateChanged(40);
        blueSegImage[0].calcMinMax();
        max = (float) blueSegImage[0].getMax();
        thresholds = new float[2];
        thresholds[0] = max;
        thresholds[1] = max;
        fillValue = 0.0f;
        thresholdAlgo = new AlgorithmThresholdDual(blueSegImage[0], thresholds, fillValue,
                                                   AlgorithmThresholdDual.BINARY_TYPE, wholeImage, true);
        thresholdAlgo.run();
        thresholdAlgo.finalize();
        thresholdAlgo = null;

        // Smooth with a morphological opening followed by a closing
        fireProgressStateChanged("Opening blue segmented image");
        fireProgressStateChanged(45);
        kernel = AlgorithmMorphology2D.CONNECTED4;
        circleDiameter = 1.0f;
        method = AlgorithmMorphology2D.OPEN;
        itersDilation = 1;
        itersErosion = 1;
        numPruningPixels = 0;
        edgingType = 0;
        openAlgo = new AlgorithmMorphology2D(blueSegImage[0], kernel, circleDiameter, method, itersDilation,
                                             itersErosion, numPruningPixels, edgingType, wholeImage);
        openAlgo.run();
        openAlgo.finalize();
        openAlgo = null;

        fireProgressStateChanged("Closing blue segmented image");
        fireProgressStateChanged(50);
        method = AlgorithmMorphology2D.CLOSE;
        closeAlgo = new AlgorithmMorphology2D(blueSegImage[0], kernel, circleDiameter, method, itersDilation,
                                              itersErosion, numPruningPixels, edgingType, wholeImage);
        closeAlgo.run();
        closeAlgo.finalize();
        closeAlgo = null;

        // Remove all inappropriate holes in cells
        // If not 4 connected to a edge black pixel, then convert to white
        fireProgressStateChanged("Removing holes from blue image cells");
        fireProgressStateChanged(60);
        byteBuffer = new byte[length];

        try {
            blueSegImage[0].exportData(0, length, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on blueSegImage[0].exportData", true);

            return;
        }

        backgroundBuffer = new boolean[length];

        for (i = 0; i < length; i++) {
            backgroundBuffer[i] = false;
        }

        for (j = 0, y = 0; y < yDim; y++, j += xDim) {

            for (x = 0; x < xDim; x++) {
                i = x + j;

                if ((byteBuffer[i] == 0) && ((x == 0) || (x == (xDim - 1)) || (y == 0) || (y == (yDim - 1)))) {
                    backgroundBuffer[i] = true;
                }
            }
        }

        found = true;

        while (found) {
            found = false;

            for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                for (x = 0; x < xDim; x++) {
                    i = x + j;

                    if ((byteBuffer[i] == 0) && (!backgroundBuffer[i])) {

                        if ((x >= 1) && (backgroundBuffer[i - 1])) {
                            backgroundBuffer[i] = true;
                            found = true;
                        } else if ((x < (xDim - 1)) && (backgroundBuffer[i + 1])) {
                            backgroundBuffer[i] = true;
                            found = true;
                        } else if ((y >= 1) && (backgroundBuffer[i - xDim])) {
                            backgroundBuffer[i] = true;
                            found = true;
                        } else if ((y < (yDim - 1)) && (backgroundBuffer[i + xDim])) {
                            backgroundBuffer[i] = true;
                            found = true;
                        }
                    }
                }
            }
        } // while (found)

        for (i = 0; i < length; i++) {

            if ((byteBuffer[i] == 0.0f) && (!backgroundBuffer[i])) {
                byteBuffer[i] = 1;
            }
        }

        try {
            blueSegImage[0].importData(0, byteBuffer, true);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on blueSegImage[0].importData", true);

            return;
        }

        // Put the object IDs in IDArray and the corresponding boundaries
        // in boundaryArray
        fireProgressStateChanged("IDing objects in blue segmented image");
        fireProgressStateChanged(70);
        kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
        circleDiameter = 0.0f;
        method = AlgorithmMorphology2D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo2D = new AlgorithmMorphology2D(blueSegImage[0], kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, wholeImage);
        idObjectsAlgo2D.setMinMax(10, 200000);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;

        blueSegImage[0].calcMinMax();
        numObjects = (int) blueSegImage[0].getMax();
        IDArray = new byte[length];
        dilateArray = new byte[length];
        boundaryArray = new byte[length];

        for (i = 0; i < length; i++) {
            boundaryArray[i] = 0;
        }

        xCenter = new float[numObjects];
        yCenter = new float[numObjects];
        centerToEdge = new float[numObjects];

        try {
            blueSegImage[0].exportData(0, length, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on blueSegImage[0].exportData", true);

            return;
        }

        kernel = AlgorithmMorphology2D.CONNECTED8;
        method = AlgorithmMorphology2D.DILATE;
        itersDilation = 1;

        for (id = 1; id <= numObjects; id++) {
            fireProgressStateChanged("Processing object " + id + " of " + numObjects + " in blue segmented image");
            fireProgressStateChanged(75 + (id * 10 / numObjects));

            for (j = 0; j < length; j++) {
                byteBuffer[j] = 0;
            }

            xCenter[id - 1] = 0.0f;
            yCenter[id - 1] = 0.0f;
            lowestSqr = Float.MAX_VALUE;

            for (count = 0, j = 0, y = 0; y < yDim; y++, j += xDim) {

                for (x = 0; x < xDim; x++) {
                    i = x + j;

                    if (IDArray[i] == id) {
                        byteBuffer[i] = 1;
                        xCenter[id - 1] += x;
                        yCenter[id - 1] += y;
                        count++;
                    }
                }
            }

            xCenter[id - 1] /= count;
            yCenter[id - 1] /= count;

            try {
                blueSegImage[0].importData(0, byteBuffer, true);
            } catch (IOException error) {
                byteBuffer = null;
                IDArray = null;
                errorCleanUp("Error on blueSegImage[0].importData", true);

                return;
            }

            dilateAlgo = new AlgorithmMorphology2D(blueSegImage[0], kernel, circleDiameter, method, itersDilation,
                                                   itersErosion, numPruningPixels, edgingType, wholeImage);
            dilateAlgo.run();
            dilateAlgo.finalize();
            dilateAlgo = null;

            try {
                blueSegImage[0].exportData(0, length, dilateArray);
            } catch (IOException error) {
                byteBuffer = null;
                IDArray = null;
                dilateArray = null;
                errorCleanUp("Error on blueSegImage[0].importData", true);

                return;
            }

            for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                for (x = 0; x < xDim; x++) {
                    i = x + j;

                    if ((dilateArray[i] == 1) && (byteBuffer[i] == 0)) {
                        boundaryArray[i] = (byte) id;
                        distSqr = ((xCenter[id - 1] - x) * (xCenter[id - 1] - x) * xRes * xRes) +
                                  ((yCenter[id - 1] - y) * (yCenter[id - 1] - y) * yRes * yRes);

                        if (distSqr < lowestSqr) {
                            lowestSqr = distSqr;
                        }
                    }
                }
            }

            centerToEdge[id - 1] = (float) Math.sqrt(lowestSqr);
            Preferences.debug("Object id = " + id + "\n");
            Preferences.debug("Center of mass = (" + xCenter[id - 1] + ", " + yCenter[id - 1] + ")\n");
            Preferences.debug("Distance from center of mass to edge = " + centerToEdge[id - 1] + "\n");
        } // for (id = 1; id <= numObjects; id++)

        redBuffer = new float[length];
        greenBuffer = new float[length];

        try {
            srcImage.exportRGBData(1, 0, length, redBuffer); // export red data
            srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
        } catch (IOException error) {
            redBuffer = null;
            greenBuffer = null;
            errorCleanUp("Algorithm FISHAnalysis reports: source image locked", true);

            return;
        }

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        xPosVOIs = new float[nVOIs];
        yPosVOIs = new float[nVOIs];

        // the number of distances between all types of FISH signals will be comb(nVOIs,2);
        distBtwVOIs = new float[(factorial(nVOIs) / (factorial(nVOIs - 2) * factorial(2)))];

        shortMask = new short[length];

        for (i = 0; i < length; i++) {
            shortMask[i] = -1;
        }

        idCount = new int[numObjects];

        UI.setDataText("\n--------------------------------------------------------\n");

        for (i = 0; i < nVOIs; i++) {
            fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
            fireProgressStateChanged(80 + (10 * (i + 1) / nVOIs));

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                shortMask = srcImage.generateVOIMask(shortMask, i);
                redCount = 0;
                greenCount = 0;

                for (j = 0; j < numObjects; j++) {
                    idCount[j] = 0;
                }

                for (j = 0; j < length; j++) {

                    if (shortMask[j] == i) {
                        redCount += redBuffer[j];
                        greenCount += greenBuffer[j];

                        if (IDArray[j] > 0) {
                            index = IDArray[j] - 1;
                            idCount[index]++;
                        }
                    }
                }

                objectID = 1;
                objectCount = idCount[objectID - 1];

                for (j = 2; j <= numObjects; j++) {

                    if (idCount[j - 1] > objectCount) {
                        objectID = j;
                        objectCount = idCount[j - 1];
                    }
                } // for (j = 2; j <= numObjects; j++)

                xPos = 0.0f;
                yPos = 0.0f;
                colorCount = 0.0f;

                if (redCount >= greenCount) {

                    for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                        for (x = 0; x < xDim; x++) {
                            index = x + j;

                            if (shortMask[index] == i) {
                                xPos += redBuffer[index] * x;
                                yPos += redBuffer[index] * y;
                                colorCount += redBuffer[index];
                            }
                        }
                    }
                } else { // else redCount < greenCount

                    for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                        for (x = 0; x < xDim; x++) {
                            index = x + j;

                            if (shortMask[index] == i) {
                                xPos += greenBuffer[index] * x;
                                yPos += greenBuffer[index] * y;
                                colorCount += greenBuffer[index];
                            }
                        }
                    }
                } // else redCount < greenCount

                xPos /= colorCount;
                yPos /= colorCount;

                // store them for furture processing
                xPosVOIs[i] = xPos;
                yPosVOIs[i] = yPos;

                regionToCenter = (float)
                                     Math.sqrt(((xPos - xCenter[objectID - 1]) * (xPos - xCenter[objectID - 1]) *
                                                    xRes * xRes) +
                                               ((yPos - yCenter[objectID - 1]) * (yPos - yCenter[objectID - 1]) *
                                                    yRes * yRes));
                lowestSqr = Float.MAX_VALUE;

                for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        index = x + j;

                        if (boundaryArray[index] == objectID) {
                            distSqr = ((xPos - x) * (xPos - x) * xRes * xRes) + ((yPos - y) * (yPos - y) * yRes * yRes);

                            if (distSqr < lowestSqr) {
                                lowestSqr = distSqr;
                                xEdge = x;
                                yEdge = y;
                            }
                        }
                    }
                }

                regionToEdge = (float) Math.sqrt(lowestSqr);

                if (redCount >= greenCount) {
                    VOIs.VOIAt(i).setName("RedFISH-" + rfCnt);
                    rfCnt++;
                    UI.setDataText("VOI ID = " + i + ", Name = " + VOIs.VOIAt(i).getName() +
                                   " with red weighted voi center of mass = " + "(" + xPos + ", " + yPos + ")\n");
                    // UI.setGlobalDataText("VOI ID = " + i + " with red weighted voi center of mass = " +
                    // "(" + xPos + ", " + yPos + ")\n");
                } else {
                    VOIs.VOIAt(i).setName("GreenFISH-" + gfCnt);
                    gfCnt++;
                    UI.setDataText("VOI ID = " + i + ", Name = " + VOIs.VOIAt(i).getName() +
                                   " with green weighted voi center of mass = " + "(" + xPos + ", " + yPos + ")\n");
                    // UI.setGlobalDataText("VOI ID = " + i + " with green weighted voi center of mass = " +
                    // "(" + xPos + ", " + yPos + ")\n");
                }

                UI.setDataText("Center of cell mass = (" + xCenter[objectID - 1] + ", " + yCenter[objectID - 1] +
                               ")\n");

                // UI.setGlobalDataText("Center of cell mass = (" + xCenter[objectID-1] + ", " +
                // yCenter[objectID-1] + ")\n");
                if (unitsString != null) {
                    UI.setDataText("Distance to center of cell mass = " + regionToCenter + " " + unitsString + "\n");
                    // UI.setGlobalDataText("Distance to center of cell mass = " +
                    // regionToCenter + " " + unitsString + "\n");
                } else {
                    UI.setDataText("Distance to center of cell mass = " + regionToCenter + "\n");
                    //                    UI.setGlobalDataText("Distance to center of cell mass = " +
                    //  regionToCenter + "\n");
                }

                UI.setDataText("Edge of cell = (" + xEdge + ", " + yEdge + ")\n");

                // UI.setGlobalDataText("Edge of cell = (" + xEdge + ", " + yEdge + ")\n");
                if (unitsString != null) {
                    UI.setDataText("Distance to edge of cell = " + regionToEdge + " " + unitsString + "\n\n");
                    // UI.setGlobalDataText("Distance to edge of cell = " +
                    // regionToEdge + " " + unitsString + "\n\n");

                } else {
                    UI.setDataText("Distance to edge of cell = " + regionToEdge + "\n\n");
                    //                    UI.setGlobalDataText("Distance to edge of cell = " +              regionToEdge
                    // + "\n\n");
                }
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        UI.setDataText("--------------------------------------------------------");

        /**
         * Added by Prabhakar (Reddy) Gudla, NCI/SAIC-Frederick Date: 06/03/2005
         *
         * Process the binary segmented image (blueSegImage) into a VOI. This is help us get the statistics of the
         * segmented image, i.e. cell nucleus
         */

        /*
         * try { imageFrame=         new ViewJFrameImage(blueSegImage[0], null,         new Dimension(610, 200),
         * srcImage.getUserInterface()); } catch (OutOfMemoryError error) { System.gc(); UI.setDataText("Out of memory:
         * unable to open new frame"); }
         */

        // extract the VOI from the segmented blue image
        algoVOIExtraction = new AlgorithmVOIExtraction(blueSegImage[0]);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;
        blueSegImage[0].getVOIs().VOIAt(0).setActive(true);
        blueSegImage[0].getVOIs().VOIAt(0).setName("Nucleus");
        allRGVOIs.add(blueSegImage[0].getVOIs().VOIAt(0));
        srcImage.setVOIs(allRGVOIs);

        allRGVOIs = null;
        allRGVOIs = srcImage.getVOIs();

        for (int voisCnt = 0; voisCnt < allRGVOIs.size(); voisCnt++) {
            allRGVOIs.VOIAt(voisCnt).setAllActive(true);
        }

        srcImage.ungroupVOIs();


        // find the properties of the segmented VOI
        algoVOIProps = new AlgorithmVOIProps(blueSegImage[0], AlgorithmVOIProps.PROCESS_PER_VOI, RangeType.BETWEEN, getActiveVOIs(blueSegImage[0]));
        algoVOIProps.run();
        UI.setDataText("\n--------------------------------------------------------");
        UI.setDataText("\n Image: " + blueSegImage[0].getImageFileName());
        UI.setDataText("\n VOI: " + blueSegImage[0].getVOIs().VOIAt(0));
        UI.setDataText("\n   Number of Pixels \t = " + algoVOIProps.getNVoxels() + " Pixels");

        if (unitsString != null) {
            UI.setDataText("\n   Area \t\t = " + algoVOIProps.getArea() + " " + unitsString + "^2");
        } else {
            UI.setDataText("\n   Area \t\t = " + algoVOIProps.getArea() + " Units^2");
        }

        UI.setDataText("\n   Eccentricity (2D-Only) \t = " + algoVOIProps.getEccentricity());
        UI.setDataText("\n   Major Axis (2D-Only) \t = " + algoVOIProps.getMajorAxis());
        UI.setDataText("\n   Minor Axis (2D-Only) \t = " + algoVOIProps.getMinorAxis());
        UI.setDataText("\n   Principal Axis (2D-Only) \t = " + algoVOIProps.getPrincipalAxis() + " Degrees");
        UI.setDataText("\n   Center Of Mass \t = " + algoVOIProps.getGeometricCenter());
        UI.setDataText("\n--------------------------------------------------------");
        algoVOIProps.finalize();
        algoVOIProps = null;


        // dispose the segmented image. It's no longer needed.
        blueSegImage[0].disposeLocal();
        blueSegImage[0] = null;
        blueSegImage = null;
        greenSegImage[0].disposeLocal();
        greenSegImage[0] = null;
        greenSegImage = null;
        redSegImage[0].disposeLocal();
        redSegImage[0] = null;
        redSegImage = null;


        if (nVOIs > 1) {
            int distCnt = 0;
            UI.setDataText("\n--------------------------------------------------------");

            if (unitsString != null) {
                UI.setDataText("\nEuclidean Distances Between FISH signals (" + unitsString + ") :");
            } else {
                UI.setDataText("\nEuclidean Distances Between FISH signals : ");
            }

            for (int oCnt = 0; oCnt < nVOIs; oCnt++) {

                for (int iCnt = oCnt + 1; iCnt < nVOIs; iCnt++) {
                    distBtwVOIs[distCnt] = (float)
                                               Math.sqrt(((xPosVOIs[oCnt] - xPosVOIs[iCnt]) *
                                                              (xPosVOIs[oCnt] - xPosVOIs[iCnt]) * xRes * xRes) +
                                                         ((yPosVOIs[oCnt] - yPosVOIs[iCnt]) *
                                                              (yPosVOIs[oCnt] - yPosVOIs[iCnt]) * yRes * yRes));
                    UI.setDataText("\n" + VOIs.VOIAt(oCnt).getName() + "--" + VOIs.VOIAt(iCnt).getName() + " = " +
                                   distBtwVOIs[distCnt]);
                    distCnt++;
                }
            }

            UI.setDataText("\n--------------------------------------------------------\n");
        } // if(nVOIs > 1)

        Preferences.debug("\n\n");

        allRGVOIs = null;
        allRGVOIs = srcImage.getVOIs();

        for (int voisCnt = 0; voisCnt < (allRGVOIs.size() - 1); voisCnt++) {
            allRGVOIs.VOIAt(voisCnt).setName(VOIs.VOIAt(voisCnt).getName());
            allRGVOIs.VOIAt(voisCnt).setActive(true);
        }

        allRGVOIs.VOIAt(allRGVOIs.size() - 1).setName("Nucleus");
        allRGVOIs = null;
        VOIs = null;

        if (threadStopped) {
            finalize();

            return;
        }

        fireProgressStateChanged("Job Completed");
        fireProgressStateChanged(100);


        setCompleted(true);
        /*
         * // trying the VOI from AlgorithmMorphology2D short ts = 99; String tSt = new String("Try_VOI");
         * UI.setDataText("T1"); Polygon tryPolygon = new Polygon(); tryPolygon =
         * AlgorithmMorphology2D.genContour(blueSegImage[0], xDim, yDim, new Point(10, 20)); UI.setDataText("T2"); VOI
         * tryVOI = new VOI(ts, tSt.toString(), 1); UI.setDataText("T3"); tryVOI.setCurveType(VOI.CONTOUR);
         * UI.setDataText("T4"); tryVOI.importPolygon(tryPolygon, 1); UI.setDataText("T5"); tryVOI.setAllActive(true);
         * UI.setDataText("T6"); UI.setDataText("\nArea === " + tryVOI.area());
         *
         * //allRGVOIs.add(tryVOI); //end trying
         */

    }

    /**
     * DOCUMENT ME!
     */
    private void calc3D() {

        int totLength, sliceLength;
        float[] buffer;
        ModelImage blueImage;
        AlgorithmFuzzyCMeans fcmAlgo;
        ModelImage[] blueSegImage;
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
        int kernel;
        float sphereDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        boolean[] backgroundBuffer;
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
        float[] centerToEdge;
        int count;
        float[] redBuffer;
        float[] greenBuffer;
        ViewVOIVector VOIs = null;
        int nVOIs;
        short[] shortMask;
        int index;
        int redCount;
        int greenCount;
        int[] idCount;
        int objectID;
        int objectCount;
        float xPos, yPos, zPos;
        float colorCount;
        float regionToCenter;
        float regionToEdge;
        ViewUserInterface UI = ViewUserInterface.getReference();
        float distSqr;
        float lowestSqr;
        int xEdge = -1;
        int yEdge = -1;
        int zEdge = -1;
        int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        int zUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
        String unitsString = null;
        FileInfoBase fileInfo;
        FileInfoBase fileInfo2;

        if ((xUnits == yUnits) && (xUnits == zUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
            unitsString = (Unit.getUnitFromLegacyNum(xUnits)).toString();
        }

        fireProgressStateChanged(srcImage.getImageName(), "Processing image ...");


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

        blueImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_blue");

        for (i = 0; i < srcImage.getExtents()[2]; i++) {
            fileInfo = blueImage.getFileInfo()[i];
            fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
            fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            blueImage.setFileInfo(fileInfo, i);
        } // for (i = 0; i < srcImage.getExtents()[2]; i++)

        try {
            blueImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Error on blueImage.importData", true);

            return;
        }

        buffer = null;

        // Segment into 2 values
        fireProgressStateChanged("Performing FuzzyCMeans Segmentation on blue");
        fireProgressStateChanged(5);
        blueSegImage = new ModelImage[1];
        blueSegImage[0] = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(),
                                         blueImage.getImageName() + "_seg");

        for (i = 0; i < srcImage.getExtents()[2]; i++) {
            fileInfo2 = blueSegImage[0].getFileInfo()[i];
            fileInfo2.setResolutions(srcImage.getFileInfo()[0].getResolutions());
            fileInfo2.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            blueSegImage[0].setFileInfo(fileInfo2, i);
        } // for (i = 0; i < srcImage.getExtents()[2]; i++)

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
        fcmAlgo = new AlgorithmFuzzyCMeans(blueSegImage, blueImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q,
                                           oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground,
                                           threshold, maxIter, endTolerance, wholeImage);
        centroids = new float[2];
        min = (float) blueImage.getMin();
        max = (float) blueImage.getMax();
        centroids[0] = min + ((max - min) / 3.0f);
        centroids[1] = min + (2.0f * (max - min) / 3.0f);
        fcmAlgo.setCentroids(centroids);
        fcmAlgo.run();
        fcmAlgo.finalize();
        fcmAlgo = null;
        blueImage.disposeLocal();
        blueImage = null;

        // Now convert the min and max to 0 and 1
        fireProgressStateChanged("Setting segmented blue values to 0 and 1");
        fireProgressStateChanged(30);
        blueSegImage[0].calcMinMax();
        max = (float) blueSegImage[0].getMax();
        thresholds = new float[2];
        thresholds[0] = max;
        thresholds[1] = max;
        fillValue = 0.0f;

        // ViewJFrameImage testFrame = new ViewJFrameImage(blueSegImage[0], null,
        // new Dimension(600, 300), srcImage.getUserInterface());

        thresholdAlgo = new AlgorithmThresholdDual(blueSegImage[0], thresholds, fillValue,
                                                   AlgorithmThresholdDual.BINARY_TYPE, wholeImage, true);
        thresholdAlgo.run();
        thresholdAlgo.finalize();
        thresholdAlgo = null;

        // Smooth with a morphological opening followed by a closing
        fireProgressStateChanged("Opening blue segmented image");
        fireProgressStateChanged(40);
        kernel = AlgorithmMorphology3D.CONNECTED6;
        sphereDiameter = 1.0f;
        method = AlgorithmMorphology3D.OPEN;
        itersDilation = 1;
        itersErosion = 1;
        numPruningPixels = 0;
        edgingType = 0;
        openAlgo = new AlgorithmMorphology3D(blueSegImage[0], kernel, sphereDiameter, method, itersDilation,
                                             itersErosion, numPruningPixels, edgingType, wholeImage);
        openAlgo.run();
        openAlgo.finalize();
        openAlgo = null;

        // ViewJFrameImage testFrame2 = new ViewJFrameImage(blueSegImage[0], null,
        // new Dimension(600, 300), srcImage.getUserInterface());


        fireProgressStateChanged("Closing blue segmented image");
        fireProgressStateChanged(50);
        method = AlgorithmMorphology3D.CLOSE;
        closeAlgo = new AlgorithmMorphology3D(blueSegImage[0], kernel, sphereDiameter, method, itersDilation,
                                              itersErosion, numPruningPixels, edgingType, wholeImage);
        closeAlgo.run();
        closeAlgo.finalize();
        closeAlgo = null;

        // ViewJFrameImage testFrame = new ViewJFrameImage(blueSegImage[0], null,
        // new Dimension(600, 300), srcImage.getUserInterface());

        // Remove all inappropriate holes in cells
        // If not 6 connected to a edge black pixel, then convert to white
        fireProgressStateChanged("Removing holes from blue image cells");
        fireProgressStateChanged(60);
        byteBuffer = new byte[totLength];

        try {
            blueSegImage[0].exportData(0, totLength, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on blueSegImage[0].exportData", true);

            return;
        }

        backgroundBuffer = new boolean[totLength];

        for (i = 0; i < totLength; i++) {
            backgroundBuffer[i] = false;
        }

        for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

            for (j = k, y = 0; y < yDim; y++, j += xDim) {

                for (x = 0; x < xDim; x++) {
                    i = x + j;

                    if ((byteBuffer[i] == 0) &&
                            ((x == 0) || (x == (xDim - 1)) || (y == 0) || (y == (yDim - 1)) || (z == 0) ||
                                 (z == (zDim - 1)))) {
                        backgroundBuffer[i] = true;
                    }
                }
            }
        }

        found = true;

        while (found) {
            found = false;

            for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                for (j = k, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        i = x + j;

                        if ((byteBuffer[i] == 0) && (!backgroundBuffer[i])) {

                            if ((x >= 1) && (backgroundBuffer[i - 1])) {
                                backgroundBuffer[i] = true;
                                found = true;
                            } else if ((x < (xDim - 1)) && (backgroundBuffer[i + 1])) {
                                backgroundBuffer[i] = true;
                                found = true;
                            } else if ((y >= 1) && (backgroundBuffer[i - xDim])) {
                                backgroundBuffer[i] = true;
                                found = true;
                            } else if ((y < (yDim - 1)) && (backgroundBuffer[i + xDim])) {
                                backgroundBuffer[i] = true;
                                found = true;
                            } else if ((z >= 1) && (backgroundBuffer[i - sliceLength])) {
                                backgroundBuffer[i] = true;
                                found = true;
                            } else if ((z < (zDim - 1)) && (backgroundBuffer[i + sliceLength])) {
                                backgroundBuffer[i] = true;
                                found = true;
                            }
                        }
                    }
                }
            }
        } // while (found)

        for (i = 0; i < totLength; i++) {

            if ((byteBuffer[i] == 0.0f) && (!backgroundBuffer[i])) {
                byteBuffer[i] = 1;
            }
        }

        try {
            blueSegImage[0].importData(0, byteBuffer, true);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on blueSegImage[0].importData", true);

            return;
        }

        // ViewJFrameImage testFrame = new ViewJFrameImage(blueSegImage[0], null,
        // new Dimension(600, 300), srcImage.getUserInterface());

        fireProgressStateChanged("IDing objects in blue segmented image");
        fireProgressStateChanged(70);
        kernel = AlgorithmMorphology3D.SIZED_SPHERE;
        sphereDiameter = 0.0f;
        method = AlgorithmMorphology3D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo3D = new AlgorithmMorphology3D(blueSegImage[0], kernel, sphereDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, wholeImage);
        idObjectsAlgo3D.setMinMax(10, 2000000);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;

        blueSegImage[0].calcMinMax();


        // ViewJFrameImage testFrame = new ViewJFrameImage(blueSegImage[0], null,
        // new Dimension(600, 300), srcImage.getUserInterface());
        numObjects = (int) blueSegImage[0].getMax();

        System.out.println("111111Number of Objects = " + numObjects);

        IDArray = new byte[totLength];
        boundaryArray = new byte[totLength];

        for (i = 0; i < totLength; i++) {
            boundaryArray[i] = 0;
        }

        dilateArray = new byte[totLength];
        xCenter = new float[numObjects];
        yCenter = new float[numObjects];
        zCenter = new float[numObjects];
        centerToEdge = new float[numObjects];

        try {
            blueSegImage[0].exportData(0, totLength, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on blueSegImage[0].exportData", true);

            return;
        }

        kernel = AlgorithmMorphology3D.CONNECTED24;
        method = AlgorithmMorphology3D.DILATE;
        itersDilation = 1;

        for (id = 1; id <= numObjects; id++) {
            fireProgressStateChanged("Processing object " + id + " of " + numObjects + " in blue segmented image");
            fireProgressStateChanged(75 + (id * 10 / numObjects));

            for (j = 0; j < totLength; j++) {
                byteBuffer[j] = 0;
            }

            xCenter[id - 1] = 0.0f;
            yCenter[id - 1] = 0.0f;
            zCenter[id - 1] = 0.0f;
            lowestSqr = Float.MAX_VALUE;

            for (count = 0, k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                for (j = k, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        i = x + j;

                        if (IDArray[i] == id) {
                            byteBuffer[i] = 1;
                            xCenter[id - 1] += x;
                            yCenter[id - 1] += y;
                            zCenter[id - 1] += z;
                            count++;
                        }
                    }
                }
            }

            xCenter[id - 1] /= count;
            yCenter[id - 1] /= count;
            zCenter[id - 1] /= count;

            try {
                blueSegImage[0].importData(0, byteBuffer, true);
            } catch (IOException error) {
                byteBuffer = null;
                IDArray = null;
                errorCleanUp("Error on blueSegImage[0].importData", true);

                return;
            }

            dilateAlgo = new AlgorithmMorphology3D(blueSegImage[0], kernel, sphereDiameter, method, itersDilation,
                                                   itersErosion, numPruningPixels, edgingType, wholeImage);
            dilateAlgo.run();
            dilateAlgo.finalize();
            dilateAlgo = null;

            try {
                blueSegImage[0].exportData(0, totLength, dilateArray);
            } catch (IOException error) {
                byteBuffer = null;
                IDArray = null;
                dilateArray = null;
                errorCleanUp("Error on blueSegImage[0].importData", true);

                return;
            }

            for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                for (j = k, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        i = x + j;

                        if ((dilateArray[i] == 1) && (byteBuffer[i] == 0)) {
                            boundaryArray[i] = (byte) id;
                            distSqr = ((xCenter[id - 1] - x) * (xCenter[id - 1] - x) * xRes * xRes) +
                                      ((yCenter[id - 1] - y) * (yCenter[id - 1] - y) * yRes * yRes) +
                                      ((zCenter[id - 1] - z) * (zCenter[id - 1] - z) * zRes * zRes);

                            if (distSqr < lowestSqr) {
                                lowestSqr = distSqr;
                            }
                        }
                    }
                }
            }

            centerToEdge[id - 1] = (float) Math.sqrt(lowestSqr);
            Preferences.debug("Object id = " + id + "\n");
            Preferences.debug("Center of mass = (" + xCenter[id - 1] + ", " + yCenter[id - 1] + ", " + zCenter[id - 1] +
                              ")\n");
            Preferences.debug("Distance from center of mass to edge = " + centerToEdge[id - 1] + "\n");
        } // for (id = 1; id <= numObjects; id++)

        blueSegImage[0].disposeLocal();
        blueSegImage[0] = null;
        blueSegImage = null;

        redBuffer = new float[totLength];
        greenBuffer = new float[totLength];

        try {
            srcImage.exportRGBData(1, 0, totLength, redBuffer); // export red data
            srcImage.exportRGBData(2, 0, totLength, greenBuffer); // export green data
        } catch (IOException error) {
            redBuffer = null;
            greenBuffer = null;
            errorCleanUp("Algorithm RegionDistance reports: source image locked", true);

            return;
        }

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        shortMask = new short[totLength];

        for (i = 0; i < totLength; i++) {
            shortMask[i] = -1;
        }

        System.out.println("Number of Objects = " + numObjects);
        idCount = new int[numObjects];

        for (i = 0; i < nVOIs; i++) {
            fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
            fireProgressStateChanged(85 + (10 * (i + 1) / nVOIs));

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                shortMask = srcImage.generateVOIMask(shortMask, i);
                redCount = 0;
                greenCount = 0;

                for (j = 0; j < numObjects; j++) {
                    idCount[j] = 0;
                }

                for (j = 0; j < totLength; j++) {

                    if (shortMask[j] == i) {
                        redCount += redBuffer[j];
                        greenCount += greenBuffer[j];

                        if (IDArray[j] > 0) {
                            index = IDArray[j] - 1;
                            idCount[index]++;
                        }
                    }
                }

                objectID = 1;
                objectCount = idCount[objectID - 1];

                for (j = 2; j <= numObjects; j++) {

                    if (idCount[j - 1] > objectCount) {
                        objectID = j;
                        objectCount = idCount[j - 1];
                    }
                } // for (j = 2; j <= numObjects; j++)

                xPos = 0.0f;
                yPos = 0.0f;
                zPos = 0.0f;
                colorCount = 0.0f;

                if (redCount >= greenCount) {

                    for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                        for (j = k, y = 0; y < yDim; y++, j += xDim) {

                            for (x = 0; x < xDim; x++) {
                                index = x + j;

                                if (shortMask[index] == i) {
                                    xPos += redBuffer[index] * x;
                                    yPos += redBuffer[index] * y;
                                    zPos += redBuffer[index] * z;
                                    colorCount += redBuffer[index];
                                }
                            }
                        }
                    }
                } else { // else redCount < greenCount

                    for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                        for (j = k, y = 0; y < yDim; y++, j += xDim) {

                            for (x = 0; x < xDim; x++) {
                                index = x + j;

                                if (shortMask[index] == i) {
                                    xPos += greenBuffer[index] * x;
                                    yPos += greenBuffer[index] * y;
                                    zPos += greenBuffer[index] * z;
                                    colorCount += greenBuffer[index];
                                }
                            }
                        }
                    }
                } // else redCount < greenCount

                xPos /= colorCount;
                yPos /= colorCount;
                zPos /= colorCount;
                regionToCenter = (float)
                                     Math.sqrt(((xPos - xCenter[objectID - 1]) * (xPos - xCenter[objectID - 1]) * xRes *
                                                    xRes) +
                                               ((yPos - yCenter[objectID - 1]) * (yPos - yCenter[objectID - 1]) * yRes *
                                                    yRes) +
                                               ((zPos - zCenter[objectID - 1]) * (zPos - zCenter[objectID - 1]) *
                                                    zRes * zRes));
                lowestSqr = Float.MAX_VALUE;

                for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                    for (j = k, y = 0; y < yDim; y++, j += xDim) {

                        for (x = 0; x < xDim; x++) {
                            index = x + j;

                            if (boundaryArray[index] == objectID) {
                                distSqr = ((xPos - x) * (xPos - x) * xRes * xRes) +
                                          ((yPos - y) * (yPos - y) * yRes * yRes) +
                                          ((zPos - z) * (zPos - z) * zRes * zRes);

                                if (distSqr < lowestSqr) {
                                    lowestSqr = distSqr;
                                    xEdge = x;
                                    yEdge = y;
                                    zEdge = z;
                                }
                            }
                        }
                    }
                }

                regionToEdge = (float) Math.sqrt(lowestSqr);

                if (redCount >= greenCount) {
                    UI.setDataText("VOI ID = " + i + " with red weighted voi center of mass = " + "(" + xPos + ", " +
                                   yPos + ", " + zPos + ")\n");
                    UI.setGlobalDataText("VOI ID = " + i + " with red weighted voi center of mass = " + "(" + xPos +
                                         ", " + yPos + ", " + zPos + ")\n");
                } else {
                    UI.setDataText("VOI ID = " + i + " with green weighted voi center of mass = " + "(" + xPos + ", " +
                                   yPos + ", " + zPos + ")\n");
                    UI.setGlobalDataText("VOI ID = " + i + " with green weighted voi center of mass = " + "(" + xPos +
                                         ", " + yPos + ", " + zPos + ")\n");
                }

                UI.setDataText("Center of cell mass = (" + xCenter[objectID - 1] + ", " + yCenter[objectID - 1] + ", " +
                               zCenter[objectID - 1] + ")\n");
                UI.setGlobalDataText("Center of cell mass = (" + xCenter[objectID - 1] + ", " + yCenter[objectID - 1] +
                                     ", " + zCenter[objectID - 1] + ")\n");

                if (unitsString != null) {
                    UI.setDataText("Distance to center of cell mass = " + regionToCenter + " " + unitsString + "\n");
                    UI.setGlobalDataText("Distance to center of cell mass = " + regionToCenter + " " + unitsString +
                                         "\n");
                } else {
                    UI.setDataText("Distance to center of cell mass = " + regionToCenter + "\n");
                    UI.setGlobalDataText("Distance to center of cell mass = " + regionToCenter + "\n");
                }

                UI.setDataText("Edge of cell = (" + xEdge + ", " + yEdge + ", " + zEdge + ")\n");
                UI.setGlobalDataText("Edge of cell = (" + xEdge + ", " + yEdge + ", " + zEdge + ")\n");

                if (unitsString != null) {
                    UI.setDataText("Distance to edge of cell = " + regionToEdge + " " + unitsString + "\n\n");
                    UI.setGlobalDataText("Distance to edge of cell = " + regionToEdge + " " + unitsString + "\n\n");

                } else {
                    UI.setDataText("Distance to edge of cell = " + regionToEdge + "\n\n");
                    UI.setGlobalDataText("Distance to edge of cell = " + regionToEdge + "\n\n");
                }
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   n  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  RuntimeException  DOCUMENT ME!
     */
    private int factorial(int n) {

        if (n < 0) {
            throw new RuntimeException("Underflow error in factorial");
        } else if (n > 10) {
            throw new RuntimeException("Overflow error in factorial");
        } else if (n == 0) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }
    
    /**
     * This legacy code returns all active vois for a given source image.  PlugIns should explicitly identify VOIs they would
     * like to process using AlgorithmVOIProps, because the user may have already added other VOIs to srcImage, or VOIs
     * may be created by the algorithm in an unexpected way.  This plugin relied on <code>AlgorithmVOIProp</code>'s 
     * getActiveVOIs() code, so that code has been moved into this plugin.
     * 
     * Use of this method is discouraged, as shown by the old documentation for this method:
     * not for use. should be moved to a better location. does NOT clone the VOIs that it find to be active, and inserts
     * into a new ViewVOIVector. if no VOIs are active, the ViewVOIVector returned is <code>null</code>.
     *
     * @return  All the active VOIs for a given srcImage.
     */
    private ViewVOIVector getActiveVOIs(ModelImage srcImage) {
        ViewVOIVector voiList;

        voiList = new ViewVOIVector();

        int i;

        try {

            for (i = 0; i < srcImage.getVOIs().size(); i++) {

                if (srcImage.getVOIs().VOIAt(i).isActive()) {

                    // voi at i is the active voi
                    voiList.addElement(srcImage.getVOIs().VOIAt(i));
                }
            }
        } catch (ArrayIndexOutOfBoundsException indexException) {

            // got to the end of list and never found an active VOI.
            // return an  empty VOI list.
            return new ViewVOIVector();
        }

        return voiList;
    }
}
