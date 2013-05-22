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
* @version  May 9, 2013
* @author   William Gandler
* @see      AlgorithmBase
* PlugInAlgorithmNucleiDeformation is used to identify nuclei and output statistics for each nucleus 
*/

public class PlugInAlgorithmNucleiDeformation extends AlgorithmBase {
  //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;
    
  //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Minimum number of pixels in a nucleus */
    private int minSize = 100;
    
    /** Maximum number of pixels in a nucleus */
    private int maxSize = 1000000;
    
    /**
     * 
     * @param srcImg
     * @param minSize
     * @param maxSize
     */
    public PlugInAlgorithmNucleiDeformation(ModelImage srcImg, int minSize, int maxSize) {
        super(null, srcImg);
        this.minSize = minSize;
        this.maxSize = maxSize;
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
        int length;
        int xDim;
        int yDim;
        float buffer[];
        AlgorithmFuzzyCMeans fcmAlgo;
        ModelImage grayImage;
        FileInfoBase fileInfo;
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
        byte byteBuffer[];
        int i;
        int kernel;
        float circleDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        AlgorithmMorphology2D idObjectsAlgo2D;
        int numObjects;
        byte[] IDArray;
        boolean[] removeID;
        int id;
        int j;
        int x;
        int y;
        boolean allRemoved;
        int numRemoved;
        AlgorithmVOIExtraction algoVOIExtraction;
        VOIVector VOIs;
        int nVOIs;
        ViewUserInterface UI = ViewUserInterface.getReference();
        AlgorithmMorphology2D fillHolesAlgo2D;

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        
        try {

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // export blue data
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm NucleiDeformation reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm NucleiDeformation reports: out of memory", true);

            return;
        }

        fireProgressStateChanged("Processing image ...");


        fireProgressStateChanged("Creating  image");
        grayImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_gray");
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
        fireProgressStateChanged("Performing FuzzyCMeans Segmentation on image");

        
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
        fireProgressStateChanged("Setting segmented image values to 0 and 1");
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
        
        // Remove all inappropriate holes in nuclei
        fireProgressStateChanged("Removing holes from nuclei");
        fireProgressStateChanged(7);

        fillHolesAlgo2D = new AlgorithmMorphology2D(grayImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0,
                                                    wholeImage);
        fillHolesAlgo2D.run();
        fillHolesAlgo2D.finalize();
        fillHolesAlgo2D = null;
        
        // Filling holes set top, right, left, and bottom boundaries to zero
        // Restore original boundary values
        // This will allow objects touching boundaries to be removed
        IDArray = new byte[length];
        for (i = 0; i < length; i++) {
            IDArray[i] = byteBuffer[i];
        }
        try {
            grayImage.exportData(0, length, byteBuffer);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        for (i = 0; i < xDim; i++) {
            byteBuffer[i] = IDArray[i];
        }

        // bottom boundary
        for (i = (yDim - 1) * xDim; i < length; i++) {
            byteBuffer[i] = IDArray[i];
        }

        // left boundary
        for (i = 0; i < length; i = i + xDim) {
            byteBuffer[i] = IDArray[i];
        }

        // right boundary
        for (i = xDim; i < length; i = i + xDim) {
            byteBuffer[i - 1] = IDArray[i-1];
        }
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }
        
        fireProgressStateChanged("IDing objects in segmented image");
        fireProgressStateChanged(15);
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }

        fireProgressStateChanged("IDing objects in segmented image");
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
        idObjectsAlgo2D.setMinMax(minSize, maxSize);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;

        grayImage.calcMinMax();
        numObjects = (int) grayImage.getMax();
        Preferences.debug("numObjects = " + numObjects + "\n", Preferences.DEBUG_ALGORITHM);

        try {
            grayImage.exportData(0, length, IDArray);
        } catch (IOException error) {
            byteBuffer = null;
            IDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

        fireProgressStateChanged("Removing objects touching edges");
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
            Preferences.debug("All objects touch edges so don't remove any\n");
            for (id = 1; id <= numObjects; id++) {
                removeID[id-1] = false;   
            }
        }
        else {
            UI.setDataText("Removing " + numRemoved + " objects of " + numObjects + " for touching edges\n");
            Preferences.debug("Removing " + numRemoved + " objects of " + numObjects + " for touching edges\n");    
        }
        

        for (id = numObjects; id >= 1; id--) {    
            if (removeID[id-1]) {
    
                for (i = 0; i < length; i++) {

                    if (IDArray[i] == (byte) id) {
                        IDArray[i] = (byte) 0;
                    } 
                    else if (IDArray[i] > id) {
                        IDArray[i]--;
                    }
                }

                numObjects--;
            } // if (removeID[id-1])
        } // for (id = numObjects; id >= 1; id--) 
        
        try {
            grayImage.importData(0, IDArray, true);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }

        fireProgressStateChanged("Extracting VOIs from segmented image");
        fireProgressStateChanged(70);
        algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
        //algoVOIExtraction.setColorTable(colorTable);
        //algoVOIExtraction.setNameTable(nameTable);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;
        
        VOIs = grayImage.getVOIs();
        nVOIs = VOIs.size();
        Preferences.debug("nVOIS = " + nVOIs + "\n", Preferences.DEBUG_ALGORITHM);

        srcImage.setVOIs(VOIs);
        VOIs = srcImage.getVOIs();
        for (i = 0; i < nVOIs; i++) {
            VOIs.VOIAt(i).setFixed(true);
            VOIs.VOIAt(i).setDisplayMode(VOI.CONTOUR);
            ((VOIContour)(VOIs.VOIAt(i).getCurves().elementAt(0))).setDoGeometricCenterLabel(true);
        }
        
        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }
}