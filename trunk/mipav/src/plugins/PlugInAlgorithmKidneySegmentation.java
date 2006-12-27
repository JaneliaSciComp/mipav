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
 * @version  December 26, 2006
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmKidneySegmentation.java $ $Revision: 72 $ $Date: 2/06/06 5:50p $
 *           PlugInAlgorithmKidneySegmentation is used to generate an image containing only the image
 *           from an image of the abdominal cavity.
 *           
 *           Program operation is as follows:
 *           1.) In 1 slice the user draws a contour in the interior of each kidney.  The contour does not need to follow 
 *           the boundary - it simply provides general location information and is used to provide a sample range of
 *           pixel values.
 *           2.) The minimum and maximum pixel value is determined from all the pixels inside the 2 contours.
 *           3.) The image is thresholded with a value halfway between VOIMin and VOIMax as the bottom threshold
 *           and VOIMax as the top threshold.
 *           4.) 2.5D hole filling is performed on the resulting binary image.  2.5D hole filling gives better results than
 *           3D hole filling.
 *           5.) The user has specified iters erosion operations.  The same iters numbers of dilations will be performed.
 *           The default for iters is 5.  iters-1 2.5D erosions are performed followed by 1 3D erosion.
 *           6.) All 3D objects between size 100 and 2,000,000 are IDed in 3D.
 *           7.) Only those objects touching 1 of the 2 user drawn contours are kept.
 *           8.) Dilations to undo the erosions are performed.  1 3D dilation is followed by iters-1 2.5D dilations.
 *           9.) A second 2.5D hole fill operation is performed.
 *           10.) A 3D closing operation is performed.
 *           11.) A third 2.5D hole fill operation is performed.
 *           12.) A VOI is extracted from the image.
 *           13.) Each kidney should only have one curve per slice.  When a kidney in a slice is represented by more than
 *           1 curve, the bottom threshold is lowered by 0.05*(VOIMax - VOIMin) and a new thresholding is performed for
 *           the slice.  These 2D VOI regrowing steps parallel the original 3D steps.  After thresholding, a hole filling,
 *           iters erosions, IDing, discarding those objects not touching the extracted VOI, iters dilations, hole filling,
 *           closing, and a VOI extraction in the slice.  If the VOI extraction gives only 1 curve, then the operation on 
 *           that kidney for that slice is finished.  If the VOI extraction gives more than 1 curve in the slice, then the
 *           threshold is lowered by another 0.05*(VOIMax - VOIMin) and the operation is repeated.  A maximum of 4 iterations
 *           is allowed.
 *           14.) If the user selects area correction, then the bottom threshold is lowered and a new thresholding is performed
 *           for a slice when the kidney area is less than 0.8 times the kidney area in both the previous and following slices.
 *           The default is area correction selected.  The steps are the same as in 13.) above.  The new thresholding is not
 *           accepted if the new area is >= 1.0 times both the previous and next kidney values.  When this happens, the increment 
 *           change in the bottom threshold is halved and the bottom threshold is increased by this new half incrment in the next
 *           iteration.  If the curve is still too big after the maximum 4 iterations, then the original curve is used.
 *           
 *           What sort of results can be expected at best?  In Computer-Aided Kidney Segmentation on Abdominal CT Images by 
 *           Daw-Tung Lin, Chung-Chih Lei, and Siu-Wan Hung, IEEE Transactions on Information Technology in Biomedicine, Vol.
 *           10., No. 1, January 2006, pp. 59-65.  states: "The results of a series of tests on 358 images from 30 patients
 *           indicate an average correlation of up to 88% between automatic and manual segmentation."  Also stated: "Kobashi
 *           and Shapiro described a knowledge-based procedure for identifying and extracting organs from normal CT imagery.
 *           The detection result was 85% grade A from testing of 75 images from 3 patients."  Finally: "Tsagaan and Shimizu 
 *           proposed a deformable model approach for automatic kidney segmentation.  They used a deformable model represented
 *           by the grey level appearance of kidney and its statistical information of the shape.  They tested 33 abdominal
 *           CT images.  The degree of correspondence between automatic segmentation and manual positioning was 86.9%.
 *           
 *            LevelSet 2.5D seems to give a slight improvement after an 8 minute run on a 45 slice kidney image. 
 *            LevelSet 3D takes about 4.5 minutes per slice in the loop doing contourVOI.pointToContour, so this
 *            would be 3 hours, 20 minutes for all 45 slices.  LevelSet Diffusion gives bad results for both
 *            2.5D and 3D.  Since the maximum alloted time for processing each image is 5 minutes, do not use
 *            levelset.</p>
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

    

    /** Iterations used in erosion before IDIng = iters Iterations used in dilation after IDing = iters. */
    private int iters = 5;
    
    /** If the curve area is not at least 0.8 the area of the previous or the following curve for
     *  the kidney on that side, then for that slice rethreshold the slice with a lowered threshold
     *  to obtain a kidney slice curve with a larger area.
     */
    private boolean areaCorrect = true;
    
    private ModelImage sliceImage = null;
    
    private ModelImage threshSliceImage = null;
    
    private int xDim;
    
    private int yDim;
    
    private int sliceSize;
    
    private VOI contourVOI2;
    
    private VOI contourVOI3L;
    
    private VOI contourVOI3R;
    
    private VOI contourVOI4L;
    
    private VOI contourVOI4R;
    
    /** The VOI that will receive the new curve resulting from rethresholding. */
    private VOI nextVOI;
    
    private int z;
    
    /** The last threshold[0] or minimum threshold for each slice was set at
     *  VOIMin + lastFraction[slice-1]*(VOIMax - VOIMin)
     */
    private float lastFractionL[];
    
    private float lastFractionR[];
    
    /** A count of the area encompassed by the curve for one kidney in each slice */
    private int area[];
    
    /** If true, then sliceCorrect found a new kidney curve for one kidney in one slice */
    private boolean finished;

    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  resultImage    Result image model
     * @param  srcImg         Source image model.
     * @param  iters          Number of iterations used in both erosions and dilations
     * @param  areaCorrect    If true, rethreshold kdney slice curves with small areas compared
     *                        to areas in 2 surrounding slices.
     */
    public PlugInAlgorithmKidneySegmentation(ModelImage resultImage, ModelImage srcImg, int iters, boolean areaCorrect) {
        super(resultImage, srcImg);
        this.iters = iters;
        this.areaCorrect = areaCorrect;
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
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        ViewVOIVector VOIs = null;
        int nVOIs;
        
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

        
        
        int i;
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        int zDim = srcImage.getExtents()[2];
        int totLength = sliceSize * zDim;
        
        ViewVOIVector VOIs = null;
        int nVOIs;
        
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
        AlgorithmMorphology3D closeAlgo3D;
        AlgorithmVOIExtraction algoVOIExtraction;
        Vector curves[];
        Vector curves2[];
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
        int vIters;
        VOI currentVOI;
        BitSet mask;
        boolean xor = false;
        boolean onlyActive = false;
        int offset;
        Vector curves3[];
        
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
        fireProgressStateChanged(10);
        threshImage = new ModelImage(ModelStorageBase.BOOLEAN, srcImage.getExtents(), srcImage.getImageName() + "_thresh",
                srcImage.getUserInterface());
        extents2D = new int[2];
        extents2D[0] = xDim;
        extents2D[1] = yDim;
        
        sliceImage = new ModelImage(srcImage.getType(), extents2D, "sliceImage", srcImage.getUserInterface());
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
        fireProgressStateChanged(20);
        fillHolesAlgo25D = new AlgorithmMorphology25D(threshImage, 0, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0,
                entireImage);
        fillHolesAlgo25D.run();
        fillHolesAlgo25D.finalize();
        fillHolesAlgo25D = null;
        
        // new ViewJFrameImage(threshImage);
        
        fireProgressStateChanged("Eroding image ...");
        fireProgressStateChanged(25);
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
        
        fireProgressStateChanged("IDing objects ...");
        fireProgressStateChanged(35);
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
        fireProgressStateChanged(45);
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
        fireProgressStateChanged(55);
        fillHolesAlgo25D = new AlgorithmMorphology25D(threshImage, 0, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0,
                entireImage);
        fillHolesAlgo25D.run();
        fillHolesAlgo25D.finalize();
        fillHolesAlgo25D = null;
        //new ViewJFrameImage(threshImage3);
        
        fireProgressStateChanged("Closing image ...");
        fireProgressStateChanged(60);
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
        fireProgressStateChanged(70);
        fillHolesAlgo25D = new AlgorithmMorphology25D(threshImage, 0, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0,
                entireImage);
        fillHolesAlgo25D.run();
        fillHolesAlgo25D.finalize();
        fillHolesAlgo25D = null;
        
        fireProgressStateChanged("Extracting VOIs...");
        fireProgressStateChanged(75);
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
        contourVOI3L = new VOI((short)3, "contourVOI3L", zDim, VOI.CONTOUR, -1.0f);
        contourVOI3R = new VOI((short)4, "contourVOI3R", zDim, VOI.CONTOUR, -1.0f);
        fireProgressStateChanged("Redoing some slices...");
        fireProgressStateChanged(85);
        lastFractionL = new float[zDim];
        lastFractionR = new float[zDim];
        for (z = 0; z < zDim; z++) {
            lastFractionL[z] = 0.5f;
            lastFractionR[z] = 0.5f;
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
                    if (setMem[i] == 1) {
                        contourVOI3L.importPolygon(gons[i],z);
                    }
                    else {
                        contourVOI3R.importPolygon(gons[i], z);
                    }
                }
                continue;
            }
            else if (num1 == 1) {   
               for (i = 0; i < nCurves; i++) {
                   if (setMem[i] == 1) {
                       poly = ((VOIContour)(curves2[z].elementAt(i))).exportPolygon();
                       contourVOI3L.importPolygon(poly, z);
                   }
               }
            } // else if (num1 == 1)
            else if (num2 == 1) {
                for (i = 0; i < nCurves; i++) {
                    if (setMem[i] == 2) {
                        poly = ((VOIContour)(curves2[z].elementAt(i))).exportPolygon();
                        contourVOI3R.importPolygon(poly, z);
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
                nextVOI = contourVOI3L;
                sliceCorrect(1, false);
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
                nextVOI = contourVOI3R;
                sliceCorrect(2, false);
                (sliceImage.getVOIs()).removeAllElements();
            } // if (num2 > 1)
        } // for (z = 0; z < zDim; z++)
        if (!areaCorrect) {
            (destImage.getVOIs()).removeAllElements();
            (destImage.getVOIs()).addVOI(contourVOI3L);
            (destImage.getVOIs()).addVOI(contourVOI3R);
        } // if (!areaCorrect)
        
        if (areaCorrect) {
            contourVOI4L = new VOI((short)5, "contourVOI4L", zDim, VOI.CONTOUR, -1.0f);
            contourVOI4R = new VOI((short)6, "contourVOI4R", zDim, VOI.CONTOUR, -1.0f);
            mask = new BitSet(totLength);
            area = new int[zDim];
             for (vIters = 1; vIters <= 2; vIters++)  {
                 if (vIters == 1) {
                     currentVOI = contourVOI3L;
                     nextVOI = contourVOI4L;
                 }
                 else {
                     currentVOI = contourVOI3R;
                     nextVOI = contourVOI4R;
                 }
                 curves3 = currentVOI.getCurves();
                 mask.clear();
                 currentVOI.createBinaryMask(mask, xDim, yDim, xor, onlyActive);
                 for (z = 0; z < zDim; z++) {
                     area[z] = 0;
                     offset = z*sliceSize;
                     for (i = 0; i < sliceSize; i++) {
                         if (mask.get(offset + i)) {
                             area[z]++;
                         }
                     }
                 } // for (z = 0; z < zDim; z++)
                 if (curves3[0].size() > 0) {
                     nextVOI.importCurve((VOIContour)(curves3[0].elementAt(0)), 0);
                 }
                 if (curves3[zDim-1].size() > 0) {
                     nextVOI.importCurve((VOIContour)(curves3[zDim-1].elementAt(0)), zDim-1);
                 }
                 for (z = 1; z < zDim - 1; z++) {
                     if ((curves3[z].size() > 0) && (area[z] < 0.8*area[z-1]) && (area[z] < 0.8*area[z+1])) {
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
                         VOI1 = new VOI((short)1, "one.voi", 1, VOI.CONTOUR, -1.0f);
                         VOI1.importCurve((VOIContour)(curves3[z].elementAt(0)), 0);
                         (sliceImage.getVOIs()).addVOI(VOI1);
                         sliceCorrect(vIters, true);
                         if (!finished) {
                             nextVOI.importCurve((VOIContour)(curves3[z].elementAt(0)), z); 
                         }
                         (sliceImage.getVOIs()).removeAllElements();   
                     } // if ((curves3.size() > 0) && (area[z] < 0.8*area[z-1]) && (area[z] < 0.8*area[z+1]))
                     else {
                         if (curves3[z].size() > 0) {
                             nextVOI.importCurve((VOIContour)(curves3[z].elementAt(0)), z); 
                         }
                     } // else
                 } // for (z = 1; z < zDim - 1; z++)
             } // for (vIters = 1; vIters <= 2; vIters++)
             (destImage.getVOIs()).removeAllElements();
             (destImage.getVOIs()).addVOI(contourVOI4L);
             (destImage.getVOIs()).addVOI(contourVOI4R);
        } // if (areaCorrect)
        
        threshImage.disposeLocal();
        threshImage = null;
        
        setCompleted(true);
    }
    
    private void sliceCorrect(int side, boolean doingArea) {
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
        int nCurves;
        Polygon gons[];
        int sliceIter = 0;
        int finalIter = 4;
        float incr = 0.05f;
        boolean tooBig = false;
        finished = false;
        BitSet sliceMask = null;
        boolean xor = false;
        boolean onlyActive = false;
        
        if (doingArea) {
            sliceMask = new BitSet(sliceSize);
        }
        
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
            
            // Initially the top 50% of the values between the VOIMin and VOIMax were used
            // On each successive decrease the lower threshold by 5% of the range between VOIMin and VOIMax.
            if (side == 1) {
                if (tooBig) {
                    lastFractionL[z] = lastFractionL[z] + incr;
                }
                else {
                    lastFractionL[z] = lastFractionL[z] - incr;
                }
                threshold[0] = VOIMin + lastFractionL[z]*(VOIMax - VOIMin);
            }
            else {
                if (tooBig) {
                    lastFractionR[z] = lastFractionR[z] + incr;
                }
                else {
                    lastFractionR[z] = lastFractionR[z] - incr;
                }
                threshold[0] = VOIMin + lastFractionR[z]*(VOIMax - VOIMin);    
            }
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
            
            if (doingArea && threshSliceImage.getVOIs().size() > 0 &&
                threshSliceImage.getVOIs().VOIAt(0).getCurves()[0].size() > 0) {
                sliceMask.clear();
                threshSliceImage.getVOIs().VOIAt(0).createBinaryMask(sliceMask, xDim, yDim, xor, onlyActive);
                area[z] = 0;
                for (i = 0; i < sliceSize; i++) {
                    if (sliceMask.get(i)) {
                        area[z]++;
                    }
                } 
            } // if (doingArea && threshSliceImage.getVOIs().size() > 0)
            
            if (threshSliceImage.getVOIs().size() > 0) {
                gons = threshSliceImage.getVOIs().VOIAt(0).exportPolygons(0);
                nCurves = threshSliceImage.getVOIs().VOIAt(0).getCurves()[0].size();
                if (!doingArea && (nCurves == 1)) {
                    finished = true;
                }
                else if (doingArea && ((area[z] >= 1.1*area[z-1]) && (area[z] >= 1.1*area[z+1]))) {
                    tooBig = true;
                    incr = 0.5f * incr;
                }
                else if (doingArea && ((area[z] >= 0.8*area[z-1]) || (area[z] >= 0.8*area[z+1]))) {
                    finished = true; 
                    tooBig = false;
                }
                if ((!tooBig) && (finished || (sliceIter == finalIter))) {
                    for (i = 0; i < nCurves; i++) {
                        nextVOI.importPolygon(gons[i], z);
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
