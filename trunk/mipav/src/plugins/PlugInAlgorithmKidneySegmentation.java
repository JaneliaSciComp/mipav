import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;
import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;

import javax.vecmath.*;


/**
 * This shows how to extend the AlgorithmBase class.
 *
 * @version  December 15, 2006
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmKidneySegmentation.java $ $Revision: 72 $ $Date: 2/06/06 5:50p $
 *           PlugInAlgorithmKidneySegmentation is used to generate an image containing only the image
 *           from an image of the abdominal cavity. </p>
 */
public class PlugInAlgorithmKidneySegmentation extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    

    /** Iterations used in erosion before IDIng = iters Iterations used in dilation after IDing = 6*iters. */
    private int iters = 6;

    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  resultImage    Result image model
     * @param  srcImg         Source image model.
     * @param  iters          DOCUMENT ME!
     */
    public PlugInAlgorithmKidneySegmentation(ModelImage resultImage, ModelImage srcImg, int iters) {
        super(resultImage, srcImg);
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

        constructLog();

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

        
        int i;
        int x, y;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
        
        ViewVOIVector VOIs = null;
        int nVOIs;
        
        ViewUserInterface UI = srcImage.getUserInterface();
        
        int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        NumberFormat nf;
        AlgorithmVOIProps algoVOIProps;
        float VOIMin;
        float VOIMax;
        ModelImage threshImage;
        float threshold[] = new float[2];
        AlgorithmThresholdDual algoThreshDual;
        float fillValue;
        boolean entireImage;
        boolean fillValueOutsideThresholds;
        FileInfoBase fileInfo;
        AlgorithmMorphology2D fillHolesAlgo2D;
        int kernel;
        float circleDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        AlgorithmMorphology2D erosionAlgo;
        AlgorithmMorphology2D idObjectsAlgo2D;
        VOI contourVOI;
        int numObjects;
        short shortMask[];
        short shortBuffer[];
        boolean keepObject[];
        AlgorithmMorphology2D dilationAlgo;
        AlgorithmMorphology2D closeAlgo2D;
        AlgorithmVOIExtraction algoVOIExtraction;

        nf = NumberFormat.getNumberInstance();
        nf.setMinimumFractionDigits(3);
        nf.setMaximumFractionDigits(3);

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }
        contourVOI = VOIs.VOIAt(i);

        

        fireProgressStateChanged("Processing image ...");
        contourVOI.setActive(true);
        algoVOIProps = new AlgorithmVOIProps(srcImage, AlgorithmVOIProps.PROCESS_PER_VOI, JDialogVOIStatistics.NO_RANGE);
        algoVOIProps.run();
        VOIMin = algoVOIProps.getMinIntensity();
        Preferences.debug("VOI minimum = " + VOIMin + "\n");
        VOIMax = algoVOIProps.getMaxIntensity();
        Preferences.debug("VOI maximum = " + VOIMax + "\n");
        algoVOIProps.finalize();
        algoVOIProps = null;
        
        // Use the top 50% of the values between the VOIMin and VOIMax
        threshold[0] = VOIMin + 0.5f*(VOIMax - VOIMin);
        threshold[1] = VOIMax;
        fillValue = 0.0f;
        entireImage = true;
        fillValueOutsideThresholds = true;
        fireProgressStateChanged("Thresholding image...");
        threshImage = new ModelImage(ModelStorageBase.BOOLEAN, srcImage.getExtents(), srcImage.getImageName() + "_thresh",
                srcImage.getUserInterface());

        fileInfo = threshImage.getFileInfo()[0];
        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        threshImage.setFileInfo(fileInfo, 0);
        
        algoThreshDual = new AlgorithmThresholdDual(threshImage, srcImage, threshold, fillValue,
                AlgorithmThresholdDual.BINARY_TYPE, entireImage, fillValueOutsideThresholds);
        algoThreshDual.run();
        algoThreshDual.finalize();
        algoThreshDual = null;
        
        fireProgressStateChanged("Filling holes...");
        
        fillHolesAlgo2D = new AlgorithmMorphology2D(threshImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0,
                entireImage);
        fillHolesAlgo2D.run();
        fillHolesAlgo2D.finalize();
        fillHolesAlgo2D = null;
        
        fireProgressStateChanged("Eroding image...");
        kernel = AlgorithmMorphology2D.CONNECTED4;
        method = AlgorithmMorphology2D.ERODE;
        itersDilation = 0;
        itersErosion = iters;
        circleDiameter = 1.0f;
        numPruningPixels = 0;
        edgingType = 0;
        if (iters >= 1) {
            erosionAlgo = new AlgorithmMorphology2D(threshImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                                    numPruningPixels, edgingType, entireImage);
            erosionAlgo.run();
            erosionAlgo.finalize();
            erosionAlgo = null;
        }
        
        fireProgressStateChanged("IDing objects...");
        kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
        circleDiameter = 0.0f;
        method = AlgorithmMorphology2D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo2D = new AlgorithmMorphology2D(threshImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, entireImage);
        idObjectsAlgo2D.setMinMax(10, 200000);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;
        
        threshImage.calcMinMax();
        numObjects = (int) threshImage.getMax();
        
        shortMask = new short[sliceSize];
        shortBuffer = new short[sliceSize];
        keepObject = new boolean[numObjects];
        for (i = 0; i < sliceSize; i++) {
            shortMask[i] = -1;
        }
        shortMask = srcImage.generateVOIMask(shortMask, contourVOI.getID());
        try {
            threshImage.exportData(0, sliceSize, shortBuffer);
        }
        catch (IOException error) {
            MipavUtil.displayError("Error on threshImage.exportData");
            setCompleted(false);
            return;
        }
        for (i = 0; i < sliceSize; i++) {
            if (shortMask[i] != -1) {
                if (shortBuffer[i] != 0) {
                    keepObject[shortBuffer[i]-1] = true;
                }
            }
        }
        
        for (i = 0; i < sliceSize; i++) {
            if ((shortBuffer[i] > 0) && keepObject[shortBuffer[i]-1]) {
                shortBuffer[i] = 1;
            }
            else {
                shortBuffer[i] = 0;
            }
        }
        try {
            threshImage.importData(0, shortBuffer, true);
        }
        catch (IOException error) {
            MipavUtil.displayError("Error on threshImage.importData");
            setCompleted(false);
            return;
        }
        
        fireProgressStateChanged("Dilating image...");
        kernel = AlgorithmMorphology2D.CONNECTED4;
        method = AlgorithmMorphology2D.DILATE;
        itersDilation = iters;
        itersErosion = 0;
        if (iters >= 1) {
            dilationAlgo = new AlgorithmMorphology2D(threshImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                                    numPruningPixels, edgingType, entireImage);
            dilationAlgo.run();
            dilationAlgo.finalize();
            dilationAlgo = null;
        }
        
        fireProgressStateChanged("Closing image ...");
        kernel = AlgorithmMorphology2D.CONNECTED4;
        method = AlgorithmMorphology2D.CLOSE;
        itersDilation = 1;
        itersErosion = 1;
        closeAlgo2D = new AlgorithmMorphology2D(threshImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                numPruningPixels, edgingType, entireImage);
        closeAlgo2D.run();
        closeAlgo2D.finalize();
        closeAlgo2D = null;
        
        fireProgressStateChanged("Extracting VOIs...");
        algoVOIExtraction = new AlgorithmVOIExtraction(threshImage);
        //algoVOIExtraction.setColorTable(colorTable);
        //algoVOIExtraction.setNameTable(nameTable);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;

        destImage.setVOIs(threshImage.getVOIs());
        threshImage.disposeLocal();
        threshImage = null;

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

        
        
        int i, j, k;
        int x, y, z;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int zDim = srcImage.getExtents()[2];
        int totLength = sliceSize * zDim;
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
        float zRes = srcImage.getResolutions(0)[2];
        
        ViewVOIVector VOIs = null;
        int nVOIs;
        
        ViewUserInterface UI = srcImage.getUserInterface();
        
        int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        int zUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
        
        NumberFormat nf;
        AlgorithmVOIProps algoVOIProps;
        float VOIMin;
        float VOIMax;
        float threshold[] = new float[2];
        AlgorithmThresholdDual algoThreshDual;
        float fillValue;
        boolean entireImage;
        boolean fillValueOutsideThresholds;
        ModelImage threshImage;
        FileInfoBase fileInfo;
        // 2.5D gives more complete kidney filling than 3D
        AlgorithmMorphology25D fillHolesAlgo25D;
        int kernel;
        float circleDiameter;
        float sphereDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        AlgorithmMorphology25D erosionAlgo25D;
        AlgorithmMorphology3D erosionAlgo3D;
        AlgorithmMorphology3D idObjectsAlgo3D;
        AlgorithmMorphology3D dilationAlgo3D;
        AlgorithmMorphology25D dilationAlgo25D;
        int numObjects;
        VOI contourVOI;
        VOI contourVOI2;
        short shortMask[];
        short shortBuffer[];
        boolean keepObject[];
        AlgorithmMorphology25D closeAlgo25D;
        AlgorithmMorphology3D closeAlgo3D;
        AlgorithmVOIExtraction algoVOIExtraction;
        Vector curves[];
        BitSet sliceMask;
        float floatBuffer[];
        float sliceMin;
        float sliceMax;
        int numberConnected;
        int connect1[];
        int connect2[];
        int nCurves;
        
        nf = NumberFormat.getNumberInstance();
        nf.setMinimumFractionDigits(3);
        nf.setMaximumFractionDigits(3);

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }
        contourVOI = VOIs.VOIAt(i);
        

        fireProgressStateChanged("Processing image ...");
        contourVOI.setActive(true);
        algoVOIProps = new AlgorithmVOIProps(srcImage, AlgorithmVOIProps.PROCESS_PER_VOI, JDialogVOIStatistics.NO_RANGE);
        algoVOIProps.run();
        VOIMin = algoVOIProps.getMinIntensity();
        Preferences.debug("VOI minimum = " + VOIMin + "\n");
        VOIMax = algoVOIProps.getMaxIntensity();
        Preferences.debug("VOI maximum = " + VOIMax + "\n");
        algoVOIProps.finalize();
        algoVOIProps = null;
        
        // Use the top 50% of the values between the VOIMin and VOIMax
        threshold[0] = VOIMin + 0.5f*(VOIMax - VOIMin);
        threshold[1] = VOIMax;
        fillValue = 0.0f;
        entireImage = true;
        fillValueOutsideThresholds = true;
        fireProgressStateChanged("Thresholding image ...");
        threshImage = new ModelImage(ModelStorageBase.BOOLEAN, srcImage.getExtents(), srcImage.getImageName() + "_thresh",
                srcImage.getUserInterface());

        for (i = 0; i < srcImage.getExtents()[2]; i++) {
            fileInfo = threshImage.getFileInfo()[i];
            fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
            fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            threshImage.setFileInfo(fileInfo, i);
        } // for (i = 0; i < srcImage.getExtents()[2]; i++)
        algoThreshDual = new AlgorithmThresholdDual(threshImage, srcImage, threshold, fillValue,
                                               AlgorithmThresholdDual.BINARY_TYPE, entireImage, fillValueOutsideThresholds);
        algoThreshDual.run();
        algoThreshDual.finalize();
        algoThreshDual = null;
        
        // new ViewJFrameImage(threshImage);
        
        fireProgressStateChanged("Filling holes in image ...");
        fillHolesAlgo25D = new AlgorithmMorphology25D(threshImage, 0, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0,
                entireImage);
        fillHolesAlgo25D.run();
        fillHolesAlgo25D.finalize();
        fillHolesAlgo25D = null;
        
        // new ViewJFrameImage(threshImage);
        
        fireProgressStateChanged("Eroding image ...");
        kernel = AlgorithmMorphology25D.CONNECTED4;
        method = AlgorithmMorphology25D.ERODE;
        itersDilation = 0;
        itersErosion = iters-1;
        circleDiameter = 1.0f;
        numPruningPixels = 0;
        edgingType = 0;
        if (iters > 1) {
            erosionAlgo25D = new AlgorithmMorphology25D(threshImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                                    numPruningPixels, edgingType, entireImage);
            erosionAlgo25D.run();
            erosionAlgo25D.finalize();
            erosionAlgo25D = null;
        }
        
        kernel = AlgorithmMorphology3D.CONNECTED6;
        method = AlgorithmMorphology3D.ERODE;
        sphereDiameter = 1.0f;
        itersDilation = 0;
        itersErosion = 1;
        if (iters >= 1) {
            erosionAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation, itersErosion,
                numPruningPixels, edgingType, entireImage);
            erosionAlgo3D.run();
            erosionAlgo3D.finalize();
            erosionAlgo3D = null;
        }
        
        // new ViewJFrameImage(threshImage);
        
        kernel = AlgorithmMorphology3D.SIZED_SPHERE;
        sphereDiameter = 0.0f;
        method = AlgorithmMorphology3D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, entireImage);
        idObjectsAlgo3D.setMinMax(100, 2000000);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;
        if (threadStopped) {
            finalize();

            return;
        }

        // threshImage is now of type unsigned short
        threshImage.calcMinMax();
        numObjects = (int) threshImage.getMax();
        // new ViewJFrameImage(threshImage);
        
        shortMask = new short[totLength];
        shortBuffer = new short[totLength];
        keepObject = new boolean[numObjects];
        for (i = 0; i < totLength; i++) {
            shortMask[i] = -1;
        }
        shortMask = srcImage.generateVOIMask(shortMask, contourVOI.getID());
        try {
            threshImage.exportData(0, totLength, shortBuffer);
        }
        catch (IOException error) {
            MipavUtil.displayError("Error on threshImage.exportData");
            setCompleted(false);
            return;
        }
        for (i = 0; i < totLength; i++) {
            if (shortMask[i] != -1) {
                if (shortBuffer[i] != 0) {
                    keepObject[shortBuffer[i]-1] = true;
                }
            }
        }
        
        for (i = 0; i < totLength; i++) {
            if ((shortBuffer[i] > 0) && keepObject[shortBuffer[i]-1]) {
                shortBuffer[i] = 1;
            }
            else {
                shortBuffer[i] = 0;
            }
        }
        try {
            threshImage.importData(0, shortBuffer, true);
        }
        catch (IOException error) {
            MipavUtil.displayError("Error on threshImage.importData");
            setCompleted(false);
            return;
        }
        // new ViewJFrameImage(threshImage);
        
        fireProgressStateChanged("Dilating image ...");
        
        kernel = AlgorithmMorphology3D.CONNECTED6;
        method = AlgorithmMorphology3D.DILATE;
        itersDilation = 1;
        itersErosion = 0;
        if (iters >= 1) {
            dilationAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation, itersErosion,
                numPruningPixels, edgingType, entireImage);
            dilationAlgo3D.run();
            dilationAlgo3D.finalize();
            dilationAlgo3D = null;
        }
        
        kernel = AlgorithmMorphology25D.CONNECTED4;
        method = AlgorithmMorphology25D.DILATE;
        itersDilation = iters-1;
        itersErosion = 0;
        if (iters > 1) {
            dilationAlgo25D = new AlgorithmMorphology25D(threshImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                                    numPruningPixels, edgingType, entireImage);
            dilationAlgo25D.run();
            dilationAlgo25D.finalize();
            dilationAlgo25D = null;
        }
        // ModelImage threshImage2 = (ModelImage)threshImage.clone();
        // new ViewJFrameImage(threshImage2);
        
        fireProgressStateChanged("Second hole fill ...");
        fillHolesAlgo25D = new AlgorithmMorphology25D(threshImage, 0, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0,
                entireImage);
        fillHolesAlgo25D.run();
        fillHolesAlgo25D.finalize();
        fillHolesAlgo25D = null;
        ModelImage threshImage2 = (ModelImage)threshImage.clone();
        //new ViewJFrameImage(threshImage3);
        
        fireProgressStateChanged("Closing image ...");
        kernel = AlgorithmMorphology3D.CONNECTED6;
        method = AlgorithmMorphology3D.CLOSE;
        itersDilation = 1;
        itersErosion = 1;
        closeAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation, itersErosion,
                numPruningPixels, edgingType, entireImage);
        closeAlgo3D.run();
        closeAlgo3D.finalize();
        closeAlgo3D = null;
        //new ViewJFrameImage(threshImage);
        
        /*kernel = AlgorithmMorphology25D.CONNECTED4;
        method = AlgorithmMorphology25D.CLOSE;
        itersDilation = 1;
        itersErosion = 1;
        closeAlgo25D = new AlgorithmMorphology25D(threshImage2, kernel, circleDiameter, method, itersDilation, itersErosion,
                numPruningPixels, edgingType, entireImage);
        closeAlgo25D.run();
        closeAlgo25D.finalize();
        closeAlgo25D = null;*/
        
        fireProgressStateChanged("Third hole fill ...");
        fillHolesAlgo25D = new AlgorithmMorphology25D(threshImage, 0, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0,
                entireImage);
        fillHolesAlgo25D.run();
        fillHolesAlgo25D.finalize();
        fillHolesAlgo25D = null;
        
        fireProgressStateChanged("Extracting VOIs...");
        algoVOIExtraction = new AlgorithmVOIExtraction(threshImage);
        //algoVOIExtraction.setColorTable(colorTable);
        //algoVOIExtraction.setNameTable(nameTable);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;

        destImage.setVOIs(threshImage.getVOIs());
        /*contourVOI2 = destImage.getVOIs().VOIAt(0);
        contourVOI2.setAllActive(true);
        curves = contourVOI2.getCurves();
        sliceMask = new BitSet(sliceSize);
        floatBuffer = new float[sliceSize];
        for (z = 0; z < zDim; z++) {
            // Ideally 1 contour for each kidney
            if (curves[z].size() <= 1) {
                continue;
            }
            // Create a kidney 1 set and a kidney 2 set
            // If either set has 2 or more curves, further
            // processing is necessary
            // See if any curves are within 4 pixels of each other
            // For n curves checking n*(n-1)/2 connections are checked
            numberConnected = 0;
            nCurves = curves[z].size();
            connect1 = new int[nCurves*(nCurves-1)/2];
            connect2 = new int[nCurves*(nCurves-1)/2];
            for (i = 0; i < curves[z].size() - 1; i++) {
                for (j = i+1; j < curves[z].size(); j++) {
                    
                }
            }
            sliceMask.clear();
            contourVOI.createActiveContourBinaryMask(xDim, yDim, z, sliceMask, false);
            try {
                srcImage.exportData(z*sliceSize, sliceSize, floatBuffer);
            }
            catch(IOException error) {
                MipavUtil.displayError("Error on srcImage.exportData");
                setCompleted(false);
                return;
            }
            sliceMin = Float.MAX_VALUE;
            sliceMax = -Float.MAX_VALUE;
            for (i = 0; i < sliceSize; i++) {
                if (sliceMask.get(i)) {
                    if (floatBuffer[i] < sliceMin) {
                        sliceMin = floatBuffer[i];
                    }
                    if (floatBuffer[i] > sliceMax) {
                        sliceMax = floatBuffer[i];
                    }
                }
            }
        } // for (z = 0; z < zDim; z++)*/
        
        threshImage.disposeLocal();
        threshImage = null;
        
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("KidneySegmentation(" + ")\n");
    }

}
