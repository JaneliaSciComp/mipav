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
 *           from an image of the abdominal cavity.
 *           
 *            LevelSet 2.5D seems to give a slight improvement after an 8 minute run on a 45 slice kidney image. 
 *            LevelSet 3D takes about 4.5 minutes per slice in the loop doing contourVOI.pointToContour, so this
 *            would be 3 hours, 20 minutes for all 45 slices.  LevelSet Diffusion gives bad results for both
 *            2.5D and 3D.</p>
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
    private int iters = 5;
    
    private ModelImage sliceImage = null;
    
    private ModelImage threshSliceImage = null;
    
    private int sliceSize;
    
    private VOI contourVOI2;
    
    private VOI contourVOI3;
    
    private int z;
    
    private ViewJFrameImage sliceFrame;

    
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
        int x, y;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
            sliceSize = xDim * yDim;
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
        short shortMask[];
        short shortBuffer[];
        boolean keepObject[];
        AlgorithmMorphology25D closeAlgo25D;
        AlgorithmMorphology3D closeAlgo3D;
        AlgorithmVOIExtraction algoVOIExtraction;
        Vector curves[];
        Vector curves2[];
        BitSet sliceMask;
        float floatBuffer[];
        float sliceMin;
        float sliceMax;
        int nCurves;
        float xcen;
        float xcen1;
        float xcen2;
        byte setMem[];
        int num1;
        int num2;
        VOI VOI1;
        VOI VOI2;
        int extents2D[];
        float sliceBuffer[];
        Polygon poly;
        Polygon gons[];
        
        nf = NumberFormat.getNumberInstance();
        nf.setMinimumFractionDigits(3);
        nf.setMaximumFractionDigits(3);

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        
        fireProgressStateChanged("Processing image ...");

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }
        contourVOI = VOIs.VOIAt(i);
        
        curves = contourVOI.getCurves();
        for (z = 0; z < zDim; z++) {
            if (curves[z].size() == 2) {
                break;
            }
        }
        // Find the x center of mass of each of the 2 drawn curves
        xcen1 = ((VOIContour)(curves[z].elementAt(0))).getCenterOfMass().x;
        xcen2 = ((VOIContour)(curves[z].elementAt(1))).getCenterOfMass().x;
        
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
        extents2D = new int[2];
        extents2D[0] = xDim;
        extents2D[1] = yDim;
        
        sliceImage = new ModelImage(srcImage.getType(), extents2D, "sliceImage", srcImage.getUserInterface());
        sliceFrame = new ViewJFrameImage(sliceImage);
        threshSliceImage = new ModelImage(ModelStorageBase.BOOLEAN, extents2D, "threshSliceImage", srcImage.getUserInterface());

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

        
        contourVOI2 = threshImage.getVOIs().VOIAt(0);
        contourVOI2.setName("contourVOI2");
        contourVOI2.setAllActive(true);
        curves2 = contourVOI2.getCurves();
        sliceBuffer = new float[sliceSize];
        contourVOI3 = new VOI((short)3, "contourVOI3", zDim, VOI.CONTOUR, -1.0f);
        fireProgressStateChanged("Redoing some slices...");
        for (z = 0; z < zDim; z++) {
            // Ideally 1 contour for each kidney
            if (curves2[z].size() < 1) {
                continue;
            }
            // Create a kidney 1 set and a kidney 2 set
            // If either set has 2 or more curves, further
            // processing is necessary
            nCurves = curves2[z].size();
            setMem = new byte[nCurves];
            num1 = 0;
            num2 = 0;
            for (i = 0; i < nCurves; i++) {
                xcen = ((VOIContour)(curves2[z].elementAt(i))).getCenterOfMass().x;
                if (Math.abs(xcen - xcen1) < Math.abs(xcen - xcen2)) {
                    setMem[i] = 1;
                    num1++;
                }
                else {
                    setMem[i] = 2;
                    num2++;
                }
            } // for (i = 0; i < nCurves; i++)
            if ((num1 <= 1) && (num2 <= 1)) {
                // contourVOI3.importCurve((VOIContour)(curves2[z].elementAt(i)), z); causes these curves to disappear
                // when threshImage.disposeLocal(); occurs.
                gons = contourVOI2.exportPolygons(z);
                for (i = 0; i < nCurves; i++) {
                    contourVOI3.importPolygon(gons[i],z);
                }
                continue;
            }
            else if (num1 == 1) {   
               for (i = 0; i < nCurves; i++) {
                   if (setMem[i] == 1) {
                       poly = ((VOIContour)(curves2[z].elementAt(i))).exportPolygon();
                       contourVOI3.importPolygon(poly, z);
                   }
               }
            } // else if (num1 == 1)
            else if (num2 == 1) {
                for (i = 0; i < nCurves; i++) {
                    if (setMem[i] == 2) {
                        poly = ((VOIContour)(curves2[z].elementAt(i))).exportPolygon();
                        contourVOI3.importPolygon(poly, z);
                    }
                }    
            } // else if (num2 == 1)
            try {
                srcImage.exportData(z*sliceSize, sliceSize, sliceBuffer);
            }
            catch (IOException error) {
                MipavUtil.displayError("IOException on srcImage.exportData");
                setCompleted(false);
                return;
            }
            try {
                sliceImage.importData(0, sliceBuffer, true);
            }
            catch (IOException error) {
                MipavUtil.displayError("IOException on sliceImage.importData");
                setCompleted(false);
                return;    
            }
            if (num1 > 1) {
                VOI1 = new VOI((short)1, "one.voi", 1, VOI.CONTOUR, -1.0f);
                for (i = 0; i < nCurves; i++) {
                    if (setMem[i] == 1) {
                        VOI1.importCurve((VOIContour)(curves2[z].elementAt(i)), 0);
                    } // if (setMem[i] == 1)
                } // for (i = 0; i < nCurves; i++)
                (sliceImage.getVOIs()).addVOI(VOI1);
                sliceCorrect();
                (sliceImage.getVOIs()).removeAllElements();
            } // if (num1 > 1)
            if (num2 > 1) {
                VOI2 = new VOI((short)2, "two.voi", 1, VOI.CONTOUR, -1.0f);
                for (i = 0; i < nCurves; i++) {
                    if (setMem[i] == 2) {
                        VOI2.importCurve((VOIContour)(curves2[z].elementAt(i)), 0);
                    } // if (setMem[i] == 2)
                } // for (i = 0; i < nCurves; i++) 
                (sliceImage.getVOIs()).addVOI(VOI2);
                sliceCorrect();
                (sliceImage.getVOIs()).removeAllElements();
            } // if (num2 > 1)
        } // for (z = 0; z < zDim; z++)
        (destImage.getVOIs()).removeAllElements();
        (destImage.getVOIs()).addVOI(contourVOI3);
        threshImage.disposeLocal();
        threshImage = null;
        
        setCompleted(true);
    }
    
    private void sliceCorrect() {
        VOI sliceVOI;
        AlgorithmVOIProps algoVOIProps;
        float VOIMin;
        float VOIMax;
        float threshold[] = new float[2];
        float fillValue;
        boolean entireImage;
        boolean fillValueOutsideThresholds;
        AlgorithmThresholdDual algoThreshDual;
        AlgorithmMorphology2D fillHolesAlgo2D;
        int kernel;
        float circleDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        AlgorithmMorphology2D erosionAlgo2D;
        AlgorithmMorphology2D idObjectsAlgo2D;
        int numObjects;
        short shortMask[];
        short shortBuffer[];
        boolean keepObject[];
        int i;
        AlgorithmMorphology2D dilationAlgo2D;
        AlgorithmMorphology2D closeAlgo2D;
        AlgorithmVOIExtraction algoVOIExtraction;
        Vector curves[];
        int nCurves;
        Polygon gons[];
        int sliceIter = 0;
        boolean finished = false;
        int finalIter = 4;
        
        while ((sliceIter <= finalIter) && (!finished)) {
            (threshSliceImage.getVOIs()).removeAllElements();
            
            sliceVOI = sliceImage.getVOIs().VOIAt(0);
            sliceVOI.setAllActive(true);
            algoVOIProps = new AlgorithmVOIProps(sliceImage, AlgorithmVOIProps.PROCESS_PER_VOI, JDialogVOIStatistics.NO_RANGE);
            algoVOIProps.run();
            VOIMin = algoVOIProps.getMinIntensity();
            VOIMax = algoVOIProps.getMaxIntensity();
            algoVOIProps.finalize();
            algoVOIProps = null;
            
            // Use the top 50% of the values between the VOIMin and VOIMax
            threshold[0] = VOIMin + (0.5f-sliceIter*0.05f)*(VOIMax - VOIMin);
            threshold[1] = VOIMax;
            fillValue = 0.0f;
            entireImage = true;
            fillValueOutsideThresholds = true;
            algoThreshDual = new AlgorithmThresholdDual(threshSliceImage, sliceImage, threshold, fillValue,
                    AlgorithmThresholdDual.BINARY_TYPE, entireImage, fillValueOutsideThresholds);
            algoThreshDual.run();
            algoThreshDual.finalize();
            algoThreshDual = null;
            
            fillHolesAlgo2D = new AlgorithmMorphology2D(threshSliceImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0,
                    entireImage);
            fillHolesAlgo2D.run();
            fillHolesAlgo2D.finalize();
            fillHolesAlgo2D = null;
            
            kernel = AlgorithmMorphology2D.CONNECTED4;
            method = AlgorithmMorphology2D.ERODE;
            itersDilation = 0;
            itersErosion = iters;
            circleDiameter = 1.0f;
            numPruningPixels = 0;
            edgingType = 0;
            if (iters >= 1) {
                erosionAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                                                        numPruningPixels, edgingType, entireImage);
                erosionAlgo2D.run();
                erosionAlgo2D.finalize();
                erosionAlgo2D = null;
            }
            
            kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
            circleDiameter = 0.0f;
            method = AlgorithmMorphology3D.ID_OBJECTS;
            itersDilation = 0;
            itersErosion = 0;
            idObjectsAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method, itersDilation,
                                                        itersErosion, numPruningPixels, edgingType, entireImage);
            idObjectsAlgo2D.setMinMax(10, 2000000);
            idObjectsAlgo2D.run();
            idObjectsAlgo2D.finalize();
            idObjectsAlgo2D = null;
            
             // threshSliceImage is now of type unsigned short
            threshSliceImage.calcMinMax();
            numObjects = (int) threshSliceImage.getMax();
            // new ViewJFrameImage(threshImage);
            
            shortMask = new short[sliceSize];
            shortBuffer = new short[sliceSize];
            keepObject = new boolean[numObjects];
            for (i = 0; i < sliceSize; i++) {
                shortMask[i] = -1;
            }
            shortMask = sliceImage.generateVOIMask(shortMask, 0);
            try {
                threshSliceImage.exportData(0, sliceSize, shortBuffer);
            }
            catch (IOException error) {
                MipavUtil.displayError("Error on threshSliceImage.exportData");
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
                threshSliceImage.importData(0, shortBuffer, true);
            }
            catch (IOException error) {
                MipavUtil.displayError("Error on threshSliceImage.importData");
                setCompleted(false);
                return;
            }
            
            kernel = AlgorithmMorphology2D.CONNECTED4;
            method = AlgorithmMorphology2D.DILATE;
            itersDilation = 1;
            itersErosion = 0;
            if (iters >= 1) {
                dilationAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                    numPruningPixels, edgingType, entireImage);
                dilationAlgo2D.run();
                dilationAlgo2D.finalize();
                dilationAlgo2D = null;
            }
            
            fillHolesAlgo2D = new AlgorithmMorphology2D(threshSliceImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0,
                    entireImage);
            fillHolesAlgo2D.run();
            fillHolesAlgo2D.finalize();
            fillHolesAlgo2D = null;
            
            kernel = AlgorithmMorphology2D.CONNECTED4;
            method = AlgorithmMorphology2D.CLOSE;
            itersDilation = 1;
            itersErosion = 1;
            closeAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method, itersDilation, itersErosion,
                    numPruningPixels, edgingType, entireImage);
            closeAlgo2D.run();
            closeAlgo2D.finalize();
            closeAlgo2D = null;
            
            algoVOIExtraction = new AlgorithmVOIExtraction(threshSliceImage);
            //algoVOIExtraction.setColorTable(colorTable);
            //algoVOIExtraction.setNameTable(nameTable);
            algoVOIExtraction.run();
            algoVOIExtraction.finalize();
            algoVOIExtraction = null;
            //System.out.println("threshSliceImage.getVOIs().size = " + threshSliceImage.getVOIs().size());
            
            if (threshSliceImage.getVOIs().size() > 0) {
                gons = threshSliceImage.getVOIs().VOIAt(0).exportPolygons(0);
                nCurves = threshSliceImage.getVOIs().VOIAt(0).getCurves()[0].size();
                if (nCurves == 1) {
                    finished = true;
                }
                if (finished || (sliceIter == finalIter)) {
                    for (i = 0; i < nCurves; i++) {
                        contourVOI3.importPolygon(gons[i],z);
                    }
                }
            }
            sliceIter++;
        } // while((sliceIter <= finalIter) && (!finished))
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("KidneySegmentation(" + ")\n");
    }

}
