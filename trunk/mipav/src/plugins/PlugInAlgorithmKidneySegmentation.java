import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;


/**
 * This shows how to extend the AlgorithmBase class.
 *
 * @version  December 28, 2006
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmKidneySegmentation.java $ $Revision: 72 $ $Date: 2/06/06
 *           5:50p $ PlugInAlgorithmKidneySegmentation is used to generate an image containing only the image from an
 *           image of the abdominal cavity.</p>
 *
 *           <p>The quality of an automatic segmentation must be validated by comparing it with a manual segmentation.
 *           The Dice similiarity coefficient is used to compare the 2 segmentations. Let A = manual segmentation and B
 *           = automatic segmentation. Then the Dice similarity coefficient = (A intersection B)/(A union B) = 2 * (A
 *           intersection B)/(A + B). The DSC ranges from no overlap with DSC = 0 to complete overlap with DSC = 1. As
 *           recommended by Zijdenbos et al in the literature of image validation a good overlap occurs when DSC >
 *           0.700. For more information on the Dice Similiarity Coefficient see: "Statistical Validation of Image
 *           Segmentation Quality Based on a Spatial Overlap Index" by Kelly H. Zou, Simon K. Warfield, Aditya Bharatha,
 *           Claire M. C. Tempany, Michael R. Kaus, Steven J. Haker, William M. Wells III, Ferenc A. Jolesz, and Ron
 *           Kikinis, Acta Radiol 2004, 11: 178-189.</p>
 *
 *           <p>Program operation is as follows: 1.) In 1 slice the user draws a contour in the interior of each kidney.
 *           The contour does not need to follow the boundary - it simply provides general location information and is
 *           used to provide a sample range of pixel values. 2.) The minimum and maximum pixel value is determined from
 *           all the pixels inside the 2 contours. 3.) The image is thresholded with a value halfway between VOIMin and
 *           VOIMax as the bottom threshold and VOIMax as the top threshold. 4.) 2.5D hole filling is performed on the
 *           resulting binary image. 2.5D hole filling gives better results than 3D hole filling. 5.) The user has
 *           specified iters erosion operations. The same iters numbers of dilations will be performed. The default for
 *           iters is 5. iters-1 2.5D erosions are performed followed by 1 3D erosion. 6.) All 3D objects between size
 *           100 and 2,000,000 are IDed in 3D. 7.) Only those objects touching 1 of the 2 user drawn contours are kept.
 *           8.) Dilations to undo the erosions are performed. 1 3D dilation is followed by iters-1 2.5D dilations. 9.)
 *           A second 2.5D hole fill operation is performed. 10.) A 3D closing operation is performed. 11.) A third 2.5D
 *           hole fill operation is performed. 12.) A VOI is extracted from the image. 13.) Each kidney should only have
 *           one curve per slice. When a kidney in a slice is represented by more than 1 curve, the bottom threshold is
 *           lowered by 0.05*(VOIMax - VOIMin) and a new thresholding is performed for the slice. These 2D VOI regrowing
 *           steps parallel the original 3D steps. After thresholding, a hole filling, iters erosions, IDing, discarding
 *           those objects not touching the extracted VOI, iters dilations, hole filling, closing, and a VOI extraction
 *           in the slice. If the VOI extraction gives only 1 curve, then the operation on that kidney for that slice is
 *           finished. If the VOI extraction gives more than 1 curve in the slice, then the threshold is lowered by
 *           another 0.05*(VOIMax - VOIMin) and the operation is repeated. A maximum of 4 iterations is allowed. 14.) If
 *           the user selects area correction, then the bottom threshold is lowered and a new thresholding is performed
 *           for a slice when the kidney area is less than 0.8 times the kidney area in both the previous and following
 *           slices. The default is area correction selected. The steps are the same as in 13.) above. The new
 *           thresholding is not accepted if the new area is >= 1.0 times both the previous and next kidney values. When
 *           this happens, the increment change in the bottom threshold is halved and the bottom threshold is increased
 *           by this new half incrment in the next iteration. If the curve is still too big after the maximum 4
 *           iterations, then the original curve is used.</p>
 *
 *           <p>What sort of results can be expected at best? In Computer-Aided Kidney Segmentation on Abdominal CT
 *           Images by Daw-Tung Lin, Chung-Chih Lei, and Siu-Wan Hung, IEEE Transactions on Information Technology in
 *           Biomedicine, Vol. 10., No. 1, January 2006, pp. 59-65. states: "The results of a series of tests on 358
 *           images from 30 patients indicate an average correlation of up to 88% between automatic and manual
 *           segmentation." Also stated: "Kobashi and Shapiro described a knowledge-based procedure for identifying and
 *           extracting organs from normal CT imagery. The detection result was 85% grade A from testing of 75 images
 *           from 3 patients." Finally: "Tsagaan and Shimizu proposed a deformable model approach for automatic kidney
 *           segmentation. They used a deformable model represented by the grey level appearance of kidney and its
 *           statistical information of the shape. They tested 33 abdominal CT images. The degree of correspondence
 *           between automatic segmentation and manual positioning was 86.9%.</p>
 *
 *           <p>LevelSet 2.5D seems to give a slight improvement after an 8 minute run on a 45 slice kidney image.
 *           LevelSet 3D takes about 4.5 minutes per slice in the loop doing contourVOI.pointToContour, so this would be
 *           3 hours, 20 minutes for all 45 slices. LevelSet Diffusion gives bad results for both 2.5D and 3D. Since the
 *           maximum alloted time for processing each image is 5 minutes, do not use levelset.</p>
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

    /** A count of the area encompassed by the curve for one kidney in each slice. */
    private int[] area;

    /**
     * If the curve area is not at least 0.8 the area of the previous or the following curve for the kidney on that
     * side, then for that slice rethreshold the slice with a lowered threshold to obtain a kidney slice curve with a
     * larger area.
     */
    private boolean areaCorrect = true;

    /** DOCUMENT ME! */
    private VOI contourVOI2;

    /** DOCUMENT ME! */
    private VOI contourVOI3 = null;

    /** DOCUMENT ME! */
    private VOI contourVOI3L;

    /** DOCUMENT ME! */
    private VOI contourVOI3R;

    /** DOCUMENT ME! */
    private VOI contourVOI4 = null;

    /** DOCUMENT ME! */
    private VOI contourVOI4L;

    /** DOCUMENT ME! */
    private VOI contourVOI4R;

    /** If true, then sliceCorrect found a new kidney curve for one kidney in one slice. */
    private boolean finished;

    /** Iterations used in erosion before IDIng = iters Iterations used in dilation after IDing = iters. */
    private int iters = 5;

    /** DOCUMENT ME! */
    private float[] lastFraction;

    /**
     * The last threshold[0] or minimum threshold for each slice was set at VOIMin + lastFraction[slice-1]*(VOIMax -
     * VOIMin).
     */
    private float[] lastFractionL;

    /** DOCUMENT ME! */
    private float[] lastFractionR;

    /**
     * In zero based numbering slice containing the middle of the kidneys Default is
     * Math.floor(srcImage.getExtents()[2]/2) - 1;
     */
    private int middleSlice;

    /** The VOI that will receive the new curve resulting from rethresholding. */
    private VOI nextVOI;

    /** DOCUMENT ME! */
    private ModelImage sliceImage = null;

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private ModelImage threshSliceImage = null;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int z;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     * @param  middleSlice  DOCUMENT ME!
     *
     * @apram  middleSlice In zero based numbering the slice containing the middle of the kidneys
     *
     * @param  iters        Number of iterations used in both erosions and dilations
     * @param  areaCorrect  If true, rethreshold kdney slice curves with small areas compared to areas in 2 surrounding
     *                      slices.
     */
    public PlugInAlgorithmKidneySegmentation(ModelImage resultImage, ModelImage srcImg, int middleSlice, int iters,
                                             boolean areaCorrect) {
        super(resultImage, srcImg);
        this.middleSlice = middleSlice;
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
        float[] threshold = new float[2];
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
        short[] shortMask;
        short[] shortBuffer;
        boolean[] keepObject;
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
        algoVOIProps = new AlgorithmVOIProps(srcImage, AlgorithmVOIProps.PROCESS_PER_VOI,
                                                RangeType.NO_RANGE, getActiveVOIs(srcImage));
        algoVOIProps.run();
        VOIMin = algoVOIProps.getMinIntensity();
        Preferences.debug("VOI minimum = " + VOIMin + "\n");
        VOIMax = algoVOIProps.getMaxIntensity();
        Preferences.debug("VOI maximum = " + VOIMax + "\n");
        algoVOIProps.finalize();
        algoVOIProps = null;

        // Use the top 50% of the values between the VOIMin and VOIMax
        threshold[0] = VOIMin + (0.5f * (VOIMax - VOIMin));
        threshold[1] = VOIMax;
        fillValue = 0.0f;
        entireImage = true;
        fillValueOutsideThresholds = true;
        fireProgressStateChanged("Thresholding image...");
        threshImage = new ModelImage(ModelStorageBase.BOOLEAN, srcImage.getExtents(),
                                     srcImage.getImageName() + "_thresh");

        fileInfo = threshImage.getFileInfo()[0];
        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        threshImage.setFileInfo(fileInfo, 0);

        algoThreshDual = new AlgorithmThresholdDual(threshImage, srcImage, threshold, fillValue,
                                                    AlgorithmThresholdDual.BINARY_TYPE, entireImage,
                                                    fillValueOutsideThresholds);
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
            erosionAlgo = new AlgorithmMorphology2D(threshImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, entireImage);
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
        } catch (IOException error) {
            MipavUtil.displayError("Error on threshImage.exportData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < sliceSize; i++) {

            if (shortMask[i] != -1) {

                if (shortBuffer[i] != 0) {
                    keepObject[shortBuffer[i] - 1] = true;
                }
            }
        }

        for (i = 0; i < sliceSize; i++) {

            if ((shortBuffer[i] > 0) && keepObject[shortBuffer[i] - 1]) {
                shortBuffer[i] = 1;
            } else {
                shortBuffer[i] = 0;
            }
        }

        try {
            threshImage.importData(0, shortBuffer, true);
        } catch (IOException error) {
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
            dilationAlgo = new AlgorithmMorphology2D(threshImage, kernel, circleDiameter, method, itersDilation,
                                                     itersErosion, numPruningPixels, edgingType, entireImage);
            dilationAlgo.run();
            dilationAlgo.finalize();
            dilationAlgo = null;
        }

        fireProgressStateChanged("Closing image ...");
        kernel = AlgorithmMorphology2D.CONNECTED4;
        method = AlgorithmMorphology2D.CLOSE;
        itersDilation = 1;
        itersErosion = 1;
        closeAlgo2D = new AlgorithmMorphology2D(threshImage, kernel, circleDiameter, method, itersDilation,
                                                itersErosion, numPruningPixels, edgingType, entireImage);
        closeAlgo2D.run();
        closeAlgo2D.finalize();
        closeAlgo2D = null;

        fireProgressStateChanged("Extracting VOIs...");
        algoVOIExtraction = new AlgorithmVOIExtraction(threshImage);

        // algoVOIExtraction.setColorTable(colorTable);
        // algoVOIExtraction.setNameTable(nameTable);
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
        NumberFormat nf;
        float[] sliceBuffer;
        BitSet sliceMask;
        BitSet countUp;
        BitSet countDown;
        BitSet countLeft;
        BitSet countRight;
        int x, y;
        int index;
        float sliceMin;
        int offset;
        boolean sliceClear;
        int[] extents2D;
        float aboveMin = 900.0f;
        int kernel;
        float circleDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        int numObjects;
        boolean entireImage = true;
        AlgorithmMorphology2D idObjectsAlgo2D;
        short[] shortBuffer;
        int maxObject;
        int maxArea;
        double m10;
        double m01;
        int nPts;
        double m20;
        double m11;
        double m02;
        double xdiff;
        double ydiff;
        double root;
        double majorAxis;
        double minorAxis;
        double areaEllipse;
        double normFactor;
        //float eccentricity;
        double theta;
        double a;
        double b;
        double t;
        double xp;
        double yp;
        double xLKidCen;
        double xRKidCen;
        double yLKidCen;
        double yRKidCen;
        double costh;
        double sinth;
        Vector3f[] ptArrayL;
        Vector3f[] ptArrayR;
        double aKid;
        double bKid;
        double cosL;
        double sinL;
        double cosR;
        double sinR;
        VOI VOIEllipseL;
        VOI VOIEllipseR;
        BitSet maskL;
        BitSet maskR;
        boolean xor = false;
        boolean onlyActive = false;
        ViewVOIVector VOIs = null;
        int nVOIs;
        VOI contourVOI;
        Vector<VOIBase>[] curves;
        VOI contourVOIL;
        VOI contourVOIR;
        float xcen1;
        float xcen2;
        Polygon poly;
        int rep;
        float VOIMin;
        float VOIMax;
        float fillValue;
        boolean fillValueOutsideThresholds;
        AlgorithmThresholdDual algoThreshDual;
        ModelImage threshImage;
        FileInfoBase fileInfo;

        // 2.5D gives more complete kidney filling than 3D
        AlgorithmMorphology25D fillHolesAlgo25D;
        AlgorithmMorphology25D erosionAlgo25D;
        AlgorithmMorphology3D erosionAlgo3D;
        AlgorithmMorphology3D idObjectsAlgo3D;
        AlgorithmMorphology3D dilationAlgo3D;
        AlgorithmMorphology25D dilationAlgo25D;
        float sphereDiameter;
        boolean[] keepObject;
        AlgorithmMorphology3D closeAlgo3D;
        AlgorithmVOIExtraction algoVOIExtraction;
        Vector<VOIBase>[] curves2;
        int nCurves;
        VOI VOI1;
        Vector<VOIBase>[] curves3;
        float[] threshold = new float[2];
        float imageMin = (float) srcImage.getMin();
        int zc;
        BitSet imageMask;


        nf = NumberFormat.getNumberInstance();
        nf.setMinimumFractionDigits(3);
        nf.setMaximumFractionDigits(3);

        fireProgressStateChanged("Processing image ...");

        // Start the extraction process from the slice containing the middle of the kidneys
        sliceBuffer = new float[sliceSize];

        try {
            srcImage.exportData(middleSlice * sliceSize, sliceSize, sliceBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("IOException on srcImage.exportData");
            setCompleted(false);

            return;
        }

        sliceMin = Float.MAX_VALUE;

        for (i = 0; i < sliceSize; i++) {

            if (sliceBuffer[i] < sliceMin) {
                sliceMin = sliceBuffer[i];
            }
        }

        sliceMask = new BitSet(sliceSize);

        for (i = 0; i < sliceSize; i++) {
            sliceMask.set(i);
        }

        countUp = new BitSet(sliceSize);
        countDown = new BitSet(sliceSize);
        countLeft = new BitSet(sliceSize);
        countRight = new BitSet(sliceSize);
        y = 0;

        for (x = 0; x < xDim; x++) {
            countUp.set(x);
            countDown.set(x);

            for (i = 1; (i <= 7) && countDown.get(x); i++) {

                if (sliceBuffer[(i * xDim) + x] > (sliceMin + aboveMin)) {
                    countDown.clear(x);
                }
            }

            if (countDown.get(x)) {
                sliceMask.clear(x);
            }

            countLeft.set(x);

            for (i = 1; (i <= 3) && countLeft.get(x); i++) {

                if ((x - i) >= 0) {

                    if (sliceBuffer[x - i] > (sliceMin + aboveMin)) {
                        countLeft.clear(x);
                    }
                }
            }

            countRight.set(x);

            for (i = 1; (i <= 3) && countRight.get(x); i++) {

                if ((x + i) <= (xDim - 1)) {

                    if (sliceBuffer[x + i] > (sliceMin + aboveMin)) {
                        countRight.clear(x);
                    }
                }
            }

            if (countLeft.get(x) && countRight.get(x)) {
                sliceMask.clear(x);
            }
        } // for (x = 0; x < xDim; x++)

        y = yDim - 1;
        offset = y * xDim;

        for (x = 0; x < xDim; x++) {
            countDown.set(offset + x);
            countUp.set(offset + x);

            for (i = 1; (i <= 7) && countUp.get(offset + x); i++) {

                if (sliceBuffer[((y - i) * xDim) + x] > (sliceMin + aboveMin)) {
                    countUp.clear(offset + x);
                }
            }

            if (countUp.get(offset + x)) {
                sliceMask.clear(offset + x);
            }

            countLeft.set(offset + x);

            for (i = 1; (i <= 3) && countLeft.get(offset + x); i++) {

                if ((x - i) >= 0) {

                    if (sliceBuffer[offset + x - i] > (sliceMin + aboveMin)) {
                        countLeft.clear(offset + x);
                    }
                }
            }

            countRight.set(offset + x);

            for (i = 1; (i <= 3) && countRight.get(offset + x); i++) {

                if ((x + i) <= (xDim - 1)) {

                    if (sliceBuffer[offset + x + i] > (sliceMin + aboveMin)) {
                        countRight.clear(offset + x);
                    }
                }
            }

            if (countLeft.get(offset + x) && countRight.get(offset + x)) {
                sliceMask.clear(offset + x);
            }
        } // for (x = 0; x < xDim; x++)

        x = 0;

        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            countLeft.set(offset);
            countRight.set(offset);

            for (i = 1; (i <= 3) && countRight.get(offset); i++) {

                if (sliceBuffer[offset + i] > (sliceMin + aboveMin)) {
                    countRight.clear(offset);
                }
            }

            if (countRight.get(offset)) {
                sliceMask.clear(offset);
            }

            countUp.set(offset);

            for (i = 1; (i <= 7) && countUp.get(offset); i++) {

                if ((y - i) >= 0) {

                    if (sliceBuffer[(y - i) * xDim] > (sliceMin + aboveMin)) {
                        countUp.clear(offset);
                    }
                }
            }

            countDown.set(offset);

            for (i = 1; (i <= 7) && countDown.get(offset); i++) {

                if ((y + i) <= (yDim - 1)) {

                    if (sliceBuffer[(y + i) * xDim] > (sliceMin + aboveMin)) {
                        countDown.clear(offset);
                    }
                }
            }

            if (countUp.get(offset) && (countDown.get(offset))) {
                sliceMask.clear(offset);
            }
        } // for (y = 0; y < yDim; y++)

        x = xDim - 1;

        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            countRight.set(offset + xDim - 1);
            countLeft.set(offset + xDim - 1);

            for (i = 1; (i <= 3) && countLeft.get(offset + xDim - 1); i++) {

                if (sliceBuffer[offset + xDim - 1 - i] > (sliceMin + aboveMin)) {
                    countLeft.clear(offset + xDim - 1);
                }
            }

            if (countLeft.get(offset + xDim - 1)) {
                sliceMask.clear(offset + xDim - 1);
            }

            countUp.set(offset + xDim - 1);

            for (i = 1; (i <= 7) && countUp.get(offset + xDim - 1); i++) {

                if ((y - i) >= 0) {

                    if (sliceBuffer[((y - i) * xDim) + xDim - 1] > (sliceMin + aboveMin)) {
                        countUp.clear(offset + xDim - 1);
                    }
                }
            }

            countDown.set(offset + xDim - 1);

            for (i = 1; (i <= 7) && countDown.get(offset + xDim - 1); i++) {

                if ((y + i) <= (yDim - 1)) {

                    if (sliceBuffer[((y + i) * xDim) + xDim - 1] > (sliceMin + aboveMin)) {
                        countDown.clear(offset + xDim - 1);
                    }
                }
            }

            if (countUp.get(offset + xDim - 1) && (countDown.get(offset + xDim - 1))) {
                sliceMask.clear(offset + xDim - 1);
            }
        } // for (y = 0; y < yDim; y++)

        sliceClear = true;

        while (sliceClear) {
            sliceClear = false;

            for (y = 1; y < (yDim - 1); y++) {

                for (x = 1; x < (xDim - 1); x++) {
                    index = (y * xDim) + x;

                    if (sliceMask.get(index) &&
                            ((!sliceMask.get(index - 1)) || (!sliceMask.get(index + 1)) ||
                                 (!sliceMask.get(((y - 1) * xDim) + x)) || (!sliceMask.get(((y + 1) * xDim) + x)))) {
                        countUp.set(index);

                        for (i = 1; (i <= 7) && countUp.get(index); i++) {

                            if ((y - i) >= 0) {

                                if (sliceBuffer[((y - i) * xDim) + x] > (sliceMin + aboveMin)) {
                                    countUp.clear(index);
                                }
                            }
                        } // for (i = 1; (i <= 7) && countUp.get(index); i++)

                        countDown.set(index);

                        for (i = 1; (i <= 7) && countDown.get(index); i++) {

                            if ((y + i) <= (yDim - 1)) {

                                if (sliceBuffer[((y + i) * xDim) + x] > (sliceMin + aboveMin)) {
                                    countDown.clear(index);
                                }
                            }
                        } // for (i = 1; (i <= 7) && countDown.get(index); i++)

                        if (countUp.get(index) && (countDown.get(index))) {
                            sliceMask.clear(index);
                            sliceClear = true;
                        }

                        countLeft.set(index);

                        for (i = 1; (i <= 3) && countLeft.get(index); i++) {

                            if ((x - i) >= 0) {

                                if (sliceBuffer[(y * xDim) + (x - i)] > (sliceMin + aboveMin)) {
                                    countLeft.clear(index);
                                }
                            }
                        } // for (i = 1; (i <= 3) && countLeft.get(index); i++)

                        countRight.set(index);

                        for (i = 1; (i <= 3) && countRight.get(index); i++) {

                            if ((x + i) <= (xDim - 1)) {

                                if (sliceBuffer[(y * xDim) + (x + i)] > (sliceMin + aboveMin)) {
                                    countRight.clear(index);
                                }
                            }
                        } // for (i = 1; (i <= 3) && countRight.get(index); i++)

                        if (countLeft.get(index) && (countRight.get(index))) {
                            sliceMask.clear(index);
                            sliceClear = true;
                        }
                    } // if (sliceMask.get(index) && ((!sliceMask.get(index - 1)) || (!sliceMask.get(index + 1)) ||
                } // for (x = 1; x < xDim - 1; x++)
            } // for (y = 1; y < yDim-1; y++)
        } // while (sliceClear)

        extents2D = new int[2];
        extents2D[0] = xDim;
        extents2D[1] = yDim;

        sliceImage = new ModelImage(ModelStorageBase.UBYTE, extents2D, "sliceImage");
        shortBuffer = new short[sliceSize];

        for (i = 0; i < sliceSize; i++) {

            if (sliceMask.get(i)) {
                shortBuffer[i] = 100;
            }
        }

        try {
            sliceImage.importData(0, shortBuffer, true);
        } catch (IOException error) {
            MipavUtil.displayError("IOException on sliceImage.importData");
            setCompleted(false);

            return;
        }

        fireProgressStateChanged("IDing objects...");
        kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
        circleDiameter = 0.0f;
        method = AlgorithmMorphology2D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        numPruningPixels = 0;
        edgingType = 0;
        idObjectsAlgo2D = new AlgorithmMorphology2D(sliceImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, entireImage);
        idObjectsAlgo2D.setMinMax(10, 2000000);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;

        sliceImage.calcMinMax();
        numObjects = (int) sliceImage.getMax();
        area = new int[numObjects];

        try {
            sliceImage.exportData(0, sliceSize, shortBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("Error on sliceImage.exportData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < sliceSize; i++) {

            if (shortBuffer[i] > 0) {
                area[shortBuffer[i] - 1]++;
            }
        }

        maxObject = 1;
        maxArea = area[0];

        for (i = 2; i <= numObjects; i++) {

            if (area[i - 1] > maxArea) {
                maxArea = area[i - 1];
                maxObject = i;
            }
        }

        for (i = 0; i < sliceSize; i++) {

            if (shortBuffer[i] != maxObject) {
                shortBuffer[i] = 0;
            }
        }

        /*try {
         *  sliceImage.importData(0, shortBuffer, true); } catch (IOException error) {
         * MipavUtil.displayError("IOException on sliceImage.importData"); setCompleted(false); return;    }new
         * ViewJFrameImage(sliceImage);*/
        // Find the ellipse that best fits the abdominal cavity
        m10 = 0.0;
        m01 = 0.0;
        nPts = 0;

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                index = (y * xDim) + x;

                if (shortBuffer[index] != 0) {
                    nPts++;
                    m10 += x;
                    m01 += y;
                }
            }
        }

        m10 = m10 / nPts;
        m01 = m01 / nPts;

        m20 = 0.0;
        m11 = 0.0;
        m02 = 0.0;

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                index = (y * xDim) + x;

                if (shortBuffer[index] != 0) {
                    xdiff = x - m10;
                    ydiff = y - m01;
                    m20 += xdiff * xdiff;
                    m11 += xdiff * ydiff;
                    m02 += ydiff * ydiff;
                }
            }
        }

        m20 = (m20 / nPts);
        m11 = (m11 / nPts);
        m02 = (m02 / nPts);

        // The eigenvalues of m20 m11
        // m11 m02
        // are proportional to the square of the semiaxes
        // (m20 - e)*(m02 - e) - m11*m11 = 0;
        root = Math.sqrt(((m20 - m02) * (m20 - m02)) + (4 * m11 * m11));
        majorAxis = (float) Math.sqrt(2.0 * (m20 + m02 + root));
        minorAxis = (float) Math.sqrt(2.0 * (m20 + m02 - root));

        areaEllipse = (Math.PI / 4.0) * majorAxis * minorAxis;

        normFactor = Math.sqrt(nPts / areaEllipse);

        majorAxis = (float) (normFactor * majorAxis);
        Preferences.debug("Major axis of ellipse = " + majorAxis + "\n");
        minorAxis = (float) (normFactor * minorAxis);
        Preferences.debug("Minor axis of ellipse = " + minorAxis + "\n");
        //eccentricity = (float) Math.sqrt(1.0 - ((minorAxis * minorAxis) / (majorAxis * majorAxis)));

        // Jahne p. 507
        // Increased tilt with image rotation to verify negaitve sign in theta equation
        theta = -0.5 * Math.atan2((2.0 * m11), (m20 - m02));
        Preferences.debug("Major axis of ellipse forms an angle of " + ((180.0 / Math.PI) * theta) +
                          " degrees with the x axis\n");
        costh = Math.cos(theta);
        sinth = Math.sin(theta);

        // Double check by drawing the ellipse as a VOI
        // a = majorAxis/2, b = minorAxis/2
        // Select x', y' coordinates along major and minor axes
        // The points on the ellipse are given by
        // x' = a * cos(t)
        // y' = b * sin(t)
        // x' = cos(theta)*(x-x0) - sin(theta)(y-y0)
        // y' = sin(theta)*(x-x0) + cos(theta)(y-y0)
        // x = m10 + cos(theta)*x' + sin(theta)*y'
        // y = m01 - sin(theta)*x' + cos(theta)*y'
        a = majorAxis / 2.0;
        b = minorAxis / 2.0;

        /*ptArray = new Vector3f[360];
         * for (i = 0; i < 360; i++) { t = i*(Math.PI/180.0); xp = a * Math.cos(t); yp = b * Math.sin(t); ptArray[i] =
         * new Vector3f(); ptArray[i].x = (float)(m10 + costh*xp + sinth*yp); ptArray[i].y = (float)(m01 - sinth*xp +
         * costh*yp); ptArray[i].z = middleSlice; } VOIEllipse = new VOI((short)99, "ellipse", zDim, VOI.CONTOUR,
         * -1.0f); VOIEllipse.importCurve(ptArray, middleSlice);destImage.getVOIs().addVOI(VOIEllipse); */
        // Locate the spine at (0.5*majorAxis, 0.56*minorAxis) so the spine is on the minor axis. The minor axis goes
        // from (x',y') = (0,b) to (0,-b) (x,y) goes from (m10 + sin(theta)*b, m01 + cos(theta)*b) to (m10 -
        // sin(theta)*b, m01  - cos(theta)*b)
        /*spinePt = new Vector3f();
         * spinePt.x = (float)(m10 + 0.12*sinth*b); spinePt.y = (float)(m01 + 0.12*costh*b); spinePt.z = middleSlice;
         * spineVOI = new VOI((short)100, "spine", zDim, VOI.POINT, -1.0f); spineVOI.importPoint(spinePt,
         * middleSlice);destImage.getVOIs().addVOI(spineVOI); */
        // In Automated Segmentation Method of Kidney Using Statistical Information 2 rectangles are used to localize
        // the kidneys.  The rectangles go from 1/9 of the major axis from the end to 2/5 of the major axis to the end
        // and from the bottom of the minor axis to 2/3 above the  bottom of the minor axis So let's use (1/9 + 2/5)/2
        // from major axis end = (23/90)*major axis = (23/45)*a from major axis end as ellipse center and (1 - 1/3)/2 *
        // b = (1/3)*b below major axis as first guess for confining ellipses.   Make ellipses with short axis length of
        // 0.4*majorAxis, long axis length of 0.6*minorAxis, and 60 degrees tilt. Then experiment and adjust x left
        // kidney center = m10 - (20/45)*a*cos(theta) + (7.0/30.0)*b*sin(theta) x right kidney center = m10 +
        // (20/45)*a*cos(theta) + (7.0/30.0)*b*sin(theta) y left kidney center = m01 + (20/45)*a*sin(theta) +
        // (7.0/30.0)*b*cos(theta) y right kidney center = m01 - (20/45)*a*sin(theta) + (7.0/30.0)*b*cos(theta)
        xLKidCen = m10 - ((20.0 / 45.0) * a * costh) + ((7.0 / 30.0) * b * sinth);
        xRKidCen = m10 + ((20.0 / 45.0) * a * costh) + ((7.0 / 30.0) * b * sinth);
        yLKidCen = m01 + ((20.0 / 45.0) * a * sinth) + ((7.0 / 30.0) * b * costh);
        yRKidCen = m01 - ((20.0 / 45.0) * a * sinth) + ((7.0 / 30.0) * b * costh);
        aKid = 0.6 * b;
        bKid = 0.4 * a;
        cosL = Math.cos(theta + (Math.PI / 3.0));
        sinL = Math.sin(theta + (Math.PI / 3.0));
        cosR = Math.cos(theta - (Math.PI / 3.0));
        sinR = Math.sin(theta - (Math.PI / 3.0));
        ptArrayL = new Vector3f[360];
        ptArrayR = new Vector3f[360];

        for (i = 0; i < 360; i++) {
            t = i * (Math.PI / 180.0);
            xp = aKid * Math.cos(t);
            yp = bKid * Math.sin(t);
            ptArrayL[i] = new Vector3f();
            ptArrayL[i].X = (float) (xLKidCen + (cosL * xp) + (sinL * yp));
            ptArrayL[i].Y = (float) (yLKidCen - (sinL * xp) + (cosL * yp));
            ptArrayL[i].Z = 0;
            ptArrayR[i] = new Vector3f();
            ptArrayR[i].X = (float) (xRKidCen + (cosR * xp) + (sinR * yp));
            ptArrayR[i].Y = (float) (yRKidCen - (sinR * xp) + (cosR * yp));
            ptArrayR[i].Z = 0;
        }

        VOIEllipseL = new VOI((short) 101, "ellipseL", VOI.CONTOUR, -1.0f);
        VOIEllipseL.importCurve(ptArrayL);

        // destImage.getVOIs().addVOI(VOIEllipseL);
        VOIEllipseR = new VOI((short) 102, "ellipseR", VOI.CONTOUR, -1.0f);
        VOIEllipseR.importCurve(ptArrayR);

        // destImage.getVOIs().addVOI(VOIEllipseR);
        maskL = new BitSet(sliceSize);
        maskR = new BitSet(sliceSize);
        VOIEllipseL.createBinaryMask3D(maskL, xDim, yDim, xor, onlyActive);
        VOIEllipseR.createBinaryMask3D(maskR, xDim, yDim, xor, onlyActive);

        for (z = 0; z < zDim; z++) {

            try {
                destImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
            } catch (IOException error) {
                MipavUtil.displayError("IOException on destImage.exportData");
                setCompleted(false);

                return;
            }

            for (i = 0; i < sliceSize; i++) {

                if ((!maskL.get(i)) && (!maskR.get(i))) {
                    sliceBuffer[i] = imageMin;
                }
            }

            try {
                destImage.importData(z * sliceSize, sliceBuffer, true);
            } catch (IOException error) {
                MipavUtil.displayError("IOException on destImage.importData");
                setCompleted(false);

                return;
            }
        }

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }

        contourVOI = VOIs.VOIAt(i);

        curves = contourVOI.getSortedCurves(zDim);

        for (z = 0; z < zDim; z++) {

            if (curves[z].size() == 2) {
                break;
            }
        }

        zc = z;


        // Find the x center of mass of each of the 2 drawn curves
        xcen1 = ((VOIContour) (curves[zc].elementAt(0))).getGeometricCenter().X;
        xcen2 = ((VOIContour) (curves[zc].elementAt(1))).getGeometricCenter().X;
        contourVOIL = new VOI((short) 10, "contourL", VOI.CONTOUR, -1.0f);
        contourVOIR = new VOI((short) 11, "contourR", VOI.CONTOUR, -1.0f);

        if (xcen1 < xcen2) {
            poly = ((VOIContour) (curves[zc].elementAt(0))).exportPolygon();
            contourVOIL.importPolygon(poly, zc);
            poly = ((VOIContour) (curves[zc].elementAt(1))).exportPolygon();
            contourVOIR.importPolygon(poly, zc);
        } else {
            poly = ((VOIContour) (curves[zc].elementAt(0))).exportPolygon();
            contourVOIR.importPolygon(poly, zc);
            poly = ((VOIContour) (curves[zc].elementAt(1))).exportPolygon();
            contourVOIL.importPolygon(poly, zc);
        }

        for (rep = 0; rep <= 1; rep++) {

            if (rep == 0) {
                contourVOI = contourVOIL;
            } else {
                contourVOI = contourVOIR;
            }

            try {
                destImage.exportData(zc * sliceSize, sliceSize, sliceBuffer);
            } catch (IOException error) {
                MipavUtil.displayError("IOException on destImage.exportData");
                setCompleted(false);

                return;
            }

            imageMask = new BitSet(totLength);
            contourVOI.createBinaryMask(xDim, yDim, zc, imageMask, xor, onlyActive);
            VOIMin = Float.MAX_VALUE;
            VOIMax = -Float.MAX_VALUE;

            for (i = 0; i < sliceSize; i++) {

                if (imageMask.get((zc * sliceSize) + i)) {

                    if (sliceBuffer[i] < VOIMin) {
                        VOIMin = sliceBuffer[i];
                    }

                    if (sliceBuffer[i] > VOIMax) {
                        VOIMax = sliceBuffer[i];
                    }
                }
            }

            Preferences.debug("VOI minimum = " + VOIMin + "\n");
            Preferences.debug("VOI maximum = " + VOIMax + "\n");

            // Use the top 50% of the values between the VOIMin and VOIMax
            threshold[0] = VOIMin + (0.5f * (VOIMax - VOIMin));
            threshold[1] = VOIMax;
            fillValue = 0.0f;
            entireImage = true;
            fillValueOutsideThresholds = true;
            fireProgressStateChanged("Thresholding image ...");
            fireProgressStateChanged((rep * 50) + 5);
            threshImage = new ModelImage(ModelStorageBase.BOOLEAN, srcImage.getExtents(),
                                         srcImage.getImageName() + "_thresh");
            extents2D = new int[2];
            extents2D[0] = xDim;
            extents2D[1] = yDim;

            sliceImage = new ModelImage(srcImage.getType(), extents2D, "sliceImage");
            threshSliceImage = new ModelImage(ModelStorageBase.BOOLEAN, extents2D, "threshSliceImage");

            for (i = 0; i < srcImage.getExtents()[2]; i++) {
                fileInfo = threshImage.getFileInfo()[i];
                fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
                fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                threshImage.setFileInfo(fileInfo, i);
            } // for (i = 0; i < srcImage.getExtents()[2]; i++)

            algoThreshDual = new AlgorithmThresholdDual(threshImage, destImage, threshold, fillValue,
                                                        AlgorithmThresholdDual.BINARY_TYPE, entireImage,
                                                        fillValueOutsideThresholds);
            algoThreshDual.run();
            algoThreshDual.finalize();
            algoThreshDual = null;

            /* new ViewJFrameImage(threshImage);
             * boolean test = true; if (test) { (destImage.getVOIs()).removeAllElements();
             * (destImage.getVOIs()).addVOI(contourVOI); setCompleted(true); return;} */

            fireProgressStateChanged("Filling holes in image ...");
            fireProgressStateChanged((rep * 50) + 10);
            fillHolesAlgo25D = new AlgorithmMorphology25D(threshImage, 0, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0,
                                                          0, entireImage);
            fillHolesAlgo25D.run();
            fillHolesAlgo25D.finalize();
            fillHolesAlgo25D = null;

            // new ViewJFrameImage(threshImage);

            fireProgressStateChanged("Eroding image ...");
            fireProgressStateChanged((rep * 50) + 13);
            kernel = AlgorithmMorphology25D.CONNECTED4;
            method = AlgorithmMorphology25D.ERODE;
            itersDilation = 0;
            itersErosion = iters - 1;
            circleDiameter = 1.0f;
            numPruningPixels = 0;
            edgingType = 0;

            if (iters > 1) {
                erosionAlgo25D = new AlgorithmMorphology25D(threshImage, kernel, circleDiameter, method, itersDilation,
                                                            itersErosion, numPruningPixels, edgingType, entireImage);
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
                erosionAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation,
                                                          itersErosion, numPruningPixels, edgingType, entireImage);
                erosionAlgo3D.run();
                erosionAlgo3D.finalize();
                erosionAlgo3D = null;
            }

            // new ViewJFrameImage(threshImage);

            fireProgressStateChanged("IDing objects ...");
            fireProgressStateChanged((rep * 50) + 18);
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

            shortBuffer = new short[totLength];
            keepObject = new boolean[numObjects];

            try {
                threshImage.exportData(0, totLength, shortBuffer);
            } catch (IOException error) {
                MipavUtil.displayError("Error on threshImage.exportData");
                setCompleted(false);

                return;
            }

            for (i = 0; i < totLength; i++) {

                if (imageMask.get(i)) {

                    if (shortBuffer[i] != 0) {
                        keepObject[shortBuffer[i] - 1] = true;
                    }
                }
            }

            for (i = 0; i < totLength; i++) {

                if ((shortBuffer[i] > 0) && keepObject[shortBuffer[i] - 1]) {
                    shortBuffer[i] = 1;
                } else {
                    shortBuffer[i] = 0;
                }
            }

            try {
                threshImage.importData(0, shortBuffer, true);
            } catch (IOException error) {
                MipavUtil.displayError("Error on threshImage.importData");
                setCompleted(false);

                return;
            }
            // new ViewJFrameImage(threshImage);

            fireProgressStateChanged("Dilating image ...");
            fireProgressStateChanged((rep * 50) + 23);
            kernel = AlgorithmMorphology3D.CONNECTED6;
            method = AlgorithmMorphology3D.DILATE;
            itersDilation = 1;
            itersErosion = 0;

            if (iters >= 1) {
                dilationAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation,
                                                           itersErosion, numPruningPixels, edgingType, entireImage);
                dilationAlgo3D.run();
                dilationAlgo3D.finalize();
                dilationAlgo3D = null;
            }

            kernel = AlgorithmMorphology25D.CONNECTED4;
            method = AlgorithmMorphology25D.DILATE;
            itersDilation = iters - 1;
            itersErosion = 0;

            if (iters > 1) {
                dilationAlgo25D = new AlgorithmMorphology25D(threshImage, kernel, circleDiameter, method, itersDilation,
                                                             itersErosion, numPruningPixels, edgingType, entireImage);
                dilationAlgo25D.run();
                dilationAlgo25D.finalize();
                dilationAlgo25D = null;
            }
            // ModelImage threshImage2 = (ModelImage)threshImage.clone();
            // new ViewJFrameImage(threshImage2);

            fireProgressStateChanged("Second hole fill ...");
            fireProgressStateChanged((rep * 50) + 28);
            fillHolesAlgo25D = new AlgorithmMorphology25D(threshImage, 0, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0,
                                                          0, entireImage);
            fillHolesAlgo25D.run();
            fillHolesAlgo25D.finalize();
            fillHolesAlgo25D = null;
            // new ViewJFrameImage(threshImage3);

            fireProgressStateChanged("Closing image ...");
            fireProgressStateChanged((rep * 50) + 30);
            kernel = AlgorithmMorphology3D.CONNECTED6;
            method = AlgorithmMorphology3D.CLOSE;
            itersDilation = 1;
            itersErosion = 1;
            closeAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, entireImage);
            closeAlgo3D.run();
            closeAlgo3D.finalize();
            closeAlgo3D = null;
            // new ViewJFrameImage(threshImage);

            /*kernel = AlgorithmMorphology25D.CONNECTED4;
             * method = AlgorithmMorphology25D.CLOSE; itersDilation = 1; itersErosion = 1; closeAlgo25D = new
             * AlgorithmMorphology25D(threshImage2, kernel, circleDiameter, method, itersDilation, itersErosion,
             * numPruningPixels, edgingType, entireImage); closeAlgo25D.run(); closeAlgo25D.finalize();closeAlgo25D =
             * null;*/

            fireProgressStateChanged("Third hole fill ...");
            fireProgressStateChanged((rep * 50) + 35);
            fillHolesAlgo25D = new AlgorithmMorphology25D(threshImage, 0, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0,
                                                          0, entireImage);
            fillHolesAlgo25D.run();
            fillHolesAlgo25D.finalize();
            fillHolesAlgo25D = null;

            fireProgressStateChanged("Extracting VOIs...");
            fireProgressStateChanged((rep * 50) + 38);
            algoVOIExtraction = new AlgorithmVOIExtraction(threshImage);

            // algoVOIExtraction.setColorTable(colorTable);
            // algoVOIExtraction.setNameTable(nameTable);
            algoVOIExtraction.run();
            algoVOIExtraction.finalize();
            algoVOIExtraction = null;


            contourVOI2 = threshImage.getVOIs().VOIAt(0);
            contourVOI2.setName("contourVOI2");
            contourVOI2.setAllActive(true);
            curves2 = contourVOI2.getSortedCurves(zDim);
            sliceBuffer = new float[sliceSize];
            contourVOI3 = new VOI((short) 3, "contourVOI3", VOI.CONTOUR, -1.0f);
            fireProgressStateChanged("Redoing some slices...");
            fireProgressStateChanged((rep * 50) + 43);
            lastFraction = new float[zDim];

            for (z = 0; z < zDim; z++) {
                lastFraction[z] = 0.5f;
                nCurves = curves2[z].size();

                // Ideally 1 contour for each kidney
                if (nCurves < 1) {
                    continue;
                }
                // If 2 or more curves further processing is necessary


                if (nCurves == 1) {

                    // contourVOI3.importCurve((VOIContour)(curves2[z].elementAt(i)), z); causes these curves to
                    // disappear when threshImage.disposeLocal(); occurs.
                    poly = ((VOIContour) (curves2[z].elementAt(0))).exportPolygon();
                    contourVOI3.importPolygon(poly, z);
                }

                if (nCurves > 1) {

                    try {
                        destImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
                    } catch (IOException error) {
                        MipavUtil.displayError("IOException on destImage.exportData");
                        setCompleted(false);

                        return;
                    }

                    try {
                        sliceImage.importData(0, sliceBuffer, true);
                    } catch (IOException error) {
                        MipavUtil.displayError("IOException on sliceImage.importData");
                        setCompleted(false);

                        return;
                    }

                    VOI1 = new VOI((short) 1, "one.voi", VOI.CONTOUR, -1.0f);

                    for (i = 0; i < nCurves; i++) {
                        poly = ((VOIContour) (curves2[z].elementAt(i))).exportPolygon();
                        VOI1.importPolygon(poly, 0);
                    } // for (i = 0; i < nCurves; i++)

                    (sliceImage.getVOIs()).addVOI(VOI1);
                    nextVOI = contourVOI3;
                    sliceFix(false);
                    (sliceImage.getVOIs()).removeAllElements();
                } // if (nCurves > 1)
            } // for (z = 0; z < zDim; z++)

            if (!areaCorrect) {
                (destImage.getVOIs()).removeAllElements();

                if (rep == 0) {
                    contourVOI3L = contourVOI3;
                }

                if (rep == 1) {
                    (destImage.getVOIs()).addVOI(contourVOI3L);
                    (destImage.getVOIs()).addVOI(contourVOI3);
                }
            } // if (!areaCorrect)

            if (areaCorrect) {
                contourVOI4 = new VOI((short) 4, "contourVOI4", VOI.CONTOUR, -1.0f);
                mask = new BitSet(totLength);
                area = new int[zDim];
                nextVOI = contourVOI4;

                curves3 = contourVOI3.getSortedCurves(zDim);
                mask.clear();
                contourVOI3.createBinaryMask3D(mask, xDim, yDim, xor, onlyActive);

                for (z = 0; z < zDim; z++) {
                    area[z] = 0;
                    offset = z * sliceSize;

                    for (i = 0; i < sliceSize; i++) {

                        if (mask.get(offset + i)) {
                            area[z]++;
                        }
                    }
                } // for (z = 0; z < zDim; z++)

                if (curves3[0].size() > 0) {
                    poly = ((VOIContour) (curves3[0].elementAt(0))).exportPolygon();
                    contourVOI4.importPolygon(poly, 0);
                }

                if (curves3[zDim - 1].size() > 0) {
                    poly = ((VOIContour) (curves3[zDim - 1].elementAt(0))).exportPolygon();
                    contourVOI4.importPolygon(poly, zDim - 1);
                }

                for (z = 1; z < (zDim - 1); z++) {

                    if ((curves3[z].size() > 0) && (area[z] < (0.8 * area[z - 1])) && (area[z] < (0.8 * area[z + 1]))) {

                        try {
                            destImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
                        } catch (IOException error) {
                            MipavUtil.displayError("IOException on destImage.exportData");
                            setCompleted(false);

                            return;
                        }

                        try {
                            sliceImage.importData(0, sliceBuffer, true);
                        } catch (IOException error) {
                            MipavUtil.displayError("IOException on sliceImage.importData");
                            setCompleted(false);

                            return;
                        }

                        VOI1 = new VOI((short) 1, "one.voi", VOI.CONTOUR, -1.0f);
                        poly = ((VOIContour) (curves3[z].elementAt(0))).exportPolygon();
                        VOI1.importPolygon(poly, 0);
                        (sliceImage.getVOIs()).addVOI(VOI1);
                        sliceFix(true);

                        if (!finished) {
                            poly = ((VOIContour) (curves3[z].elementAt(0))).exportPolygon();
                            contourVOI4.importPolygon(poly, z);
                        }

                        (sliceImage.getVOIs()).removeAllElements();
                    } // if ((curves3.size() > 0) && (area[z] < 0.8*area[z-1]) && (area[z] < 0.8*area[z+1]))
                    else {

                        if (curves3[z].size() > 0) {
                            poly = ((VOIContour) (curves3[z].elementAt(0))).exportPolygon();
                            contourVOI4.importPolygon(poly, z);
                        }
                    } // else
                } // for (z = 1; z < zDim - 1; z++)

                (destImage.getVOIs()).removeAllElements();

                if (rep == 0) {
                    contourVOI4L = contourVOI4;
                }

                if (rep == 1) {
                    (destImage.getVOIs()).addVOI(contourVOI4L);
                    (destImage.getVOIs()).addVOI(contourVOI4);
                }
            } // if (areaCorrect)

            threshImage.disposeLocal();
            threshImage = null;
        } // for (rep = 0; rep <= 1; rep++)

        setCompleted(true);
    }

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void calc3D1() {
        int i;
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;

        float[] sliceBuffer;
        float sliceMin;
        float aboveMin = 900.0f;
        float[] threshold = new float[2];
        float fillValue;
        boolean entireImage;
        boolean fillValueOutsideThresholds;
        ModelImage threshImage;
        int[] extents2D = new int[2];
        AlgorithmThresholdDual algoThreshDual;
        AlgorithmMorphology2D fillHolesAlgo2D;
        AlgorithmMorphology2D idObjectsAlgo2D;
        int maxObject;
        int maxArea;
        int[] area;
        int numObjects;
        short[] shortBuffer;
        int kernel;
        float circleDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        NumberFormat nf;

        nf = NumberFormat.getNumberInstance();
        nf.setMinimumFractionDigits(3);
        nf.setMaximumFractionDigits(3);

        fireProgressStateChanged("Processing image ...");

        // Start the extraction process from the slice containing the middle of the kidneys
        sliceBuffer = new float[sliceSize];

        try {
            srcImage.exportData(middleSlice * sliceSize, sliceSize, sliceBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("IOException on srcImage.exportData");
            setCompleted(false);

            return;
        }

        sliceMin = Float.MAX_VALUE;

        for (i = 0; i < sliceSize; i++) {

            if (sliceBuffer[i] < sliceMin) {
                sliceMin = sliceBuffer[i];
            }
        }

        threshold[0] = sliceMin + aboveMin;
        threshold[1] = Float.MAX_VALUE;
        extents2D[0] = xDim;
        extents2D[1] = yDim;
        fillValue = 0.0f;
        entireImage = true;
        fillValueOutsideThresholds = true;
        fireProgressStateChanged("Thresholding image...");
        threshImage = new ModelImage(ModelStorageBase.USHORT, extents2D, srcImage.getImageName() + "_thresh");


        algoThreshDual = new AlgorithmThresholdDual(threshImage, srcImage, threshold, fillValue,
                                                    AlgorithmThresholdDual.BINARY_TYPE, entireImage,
                                                    fillValueOutsideThresholds);
        algoThreshDual.run();
        algoThreshDual.finalize();
        algoThreshDual = null;

        fireProgressStateChanged("Filling holes...");

        fillHolesAlgo2D = new AlgorithmMorphology2D(threshImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0,
                                                    entireImage);
        fillHolesAlgo2D.run();
        fillHolesAlgo2D.finalize();
        fillHolesAlgo2D = null;

        fireProgressStateChanged("IDing objects...");
        kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
        circleDiameter = 0.0f;
        method = AlgorithmMorphology2D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        numPruningPixels = 0;
        edgingType = 0;
        idObjectsAlgo2D = new AlgorithmMorphology2D(threshImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, entireImage);
        idObjectsAlgo2D.setMinMax(10, 2000000);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;

        threshImage.calcMinMax();
        numObjects = (int) threshImage.getMax();
        shortBuffer = new short[sliceSize];
        area = new int[numObjects];

        try {
            threshImage.exportData(0, sliceSize, shortBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("Error on threshImage.exportData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < sliceSize; i++) {

            if (shortBuffer[i] > 0) {
                area[shortBuffer[i] - 1]++;
            }
        }

        maxObject = 1;
        maxArea = area[0];

        for (i = 2; i <= numObjects; i++) {

            if (area[i - 1] > maxArea) {
                maxArea = area[i - 1];
                maxObject = i;
            }
        }

        for (i = 0; i < sliceSize; i++) {

            if (shortBuffer[i] != maxObject) {
                shortBuffer[i] = 0;
            }
        }

        try {
            threshImage.importData(0, shortBuffer, true);
        } catch (IOException error) {
            MipavUtil.displayError("IOException on threshImage.importData");
            setCompleted(false);

            return;
        }

        new ViewJFrameImage(threshImage);
        setCompleted(true);

    }
    /**
     * replaced calc3d(not calc 3d1)
     */
    @SuppressWarnings("unused")
    private void oldCalc3D() {


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
        float[] threshold = new float[2];
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
        short[] shortMask;
        short[] shortBuffer;
        boolean[] keepObject;
        AlgorithmMorphology3D closeAlgo3D;
        AlgorithmVOIExtraction algoVOIExtraction;
        Vector<VOIBase>[] curves;
        Vector<VOIBase>[] curves2;
        int nCurves;
        float xcen;
        float xcen1;
        float xcen2;
        byte[] setMem;
        int num1;
        int num2;
        VOI VOI1;
        VOI VOI2;
        int[] extents2D;
        float[] sliceBuffer;
        Polygon poly;
        int vIters;
        VOI currentVOI;
        BitSet mask;
        boolean xor = false;
        boolean onlyActive = false;
        int offset;
        Vector<VOIBase>[] curves3;

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

        curves = contourVOI.getSortedCurves(zDim);

        for (z = 0; z < zDim; z++) {

            if (curves[z].size() == 2) {
                break;
            }
        }

        // Find the x center of mass of each of the 2 drawn curves
        xcen1 = ((VOIContour) (curves[z].elementAt(0))).getGeometricCenter().X;
        xcen2 = ((VOIContour) (curves[z].elementAt(1))).getGeometricCenter().X;

        contourVOI.setActive(true);
        algoVOIProps = new AlgorithmVOIProps(srcImage, AlgorithmVOIProps.PROCESS_PER_VOI,
                                                RangeType.NO_RANGE, getActiveVOIs(srcImage));
        algoVOIProps.run();
        VOIMin = algoVOIProps.getMinIntensity();
        Preferences.debug("VOI minimum = " + VOIMin + "\n");
        VOIMax = algoVOIProps.getMaxIntensity();
        Preferences.debug("VOI maximum = " + VOIMax + "\n");
        algoVOIProps.finalize();
        algoVOIProps = null;

        // Use the top 50% of the values between the VOIMin and VOIMax
        threshold[0] = VOIMin + (0.5f * (VOIMax - VOIMin));
        threshold[1] = VOIMax;
        fillValue = 0.0f;
        entireImage = true;
        fillValueOutsideThresholds = true;
        fireProgressStateChanged("Thresholding image ...");
        fireProgressStateChanged(10);
        threshImage = new ModelImage(ModelStorageBase.BOOLEAN, srcImage.getExtents(),
                                     srcImage.getImageName() + "_thresh");
        extents2D = new int[2];
        extents2D[0] = xDim;
        extents2D[1] = yDim;

        sliceImage = new ModelImage(srcImage.getType(), extents2D, "sliceImage");
        threshSliceImage = new ModelImage(ModelStorageBase.BOOLEAN, extents2D, "threshSliceImage");

        for (i = 0; i < srcImage.getExtents()[2]; i++) {
            fileInfo = threshImage.getFileInfo()[i];
            fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
            fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            threshImage.setFileInfo(fileInfo, i);
        } // for (i = 0; i < srcImage.getExtents()[2]; i++)

        algoThreshDual = new AlgorithmThresholdDual(threshImage, srcImage, threshold, fillValue,
                                                    AlgorithmThresholdDual.BINARY_TYPE, entireImage,
                                                    fillValueOutsideThresholds);
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
        itersErosion = iters - 1;
        circleDiameter = 1.0f;
        numPruningPixels = 0;
        edgingType = 0;

        if (iters > 1) {
            erosionAlgo25D = new AlgorithmMorphology25D(threshImage, kernel, circleDiameter, method, itersDilation,
                                                        itersErosion, numPruningPixels, edgingType, entireImage);
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
            erosionAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation,
                                                      itersErosion, numPruningPixels, edgingType, entireImage);
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
        } catch (IOException error) {
            MipavUtil.displayError("Error on threshImage.exportData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < totLength; i++) {

            if (shortMask[i] != -1) {

                if (shortBuffer[i] != 0) {
                    keepObject[shortBuffer[i] - 1] = true;
                }
            }
        }

        for (i = 0; i < totLength; i++) {

            if ((shortBuffer[i] > 0) && keepObject[shortBuffer[i] - 1]) {
                shortBuffer[i] = 1;
            } else {
                shortBuffer[i] = 0;
            }
        }

        try {
            threshImage.importData(0, shortBuffer, true);
        } catch (IOException error) {
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
            dilationAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation,
                                                       itersErosion, numPruningPixels, edgingType, entireImage);
            dilationAlgo3D.run();
            dilationAlgo3D.finalize();
            dilationAlgo3D = null;
        }

        kernel = AlgorithmMorphology25D.CONNECTED4;
        method = AlgorithmMorphology25D.DILATE;
        itersDilation = iters - 1;
        itersErosion = 0;

        if (iters > 1) {
            dilationAlgo25D = new AlgorithmMorphology25D(threshImage, kernel, circleDiameter, method, itersDilation,
                                                         itersErosion, numPruningPixels, edgingType, entireImage);
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
        // new ViewJFrameImage(threshImage3);

        fireProgressStateChanged("Closing image ...");
        fireProgressStateChanged(60);
        kernel = AlgorithmMorphology3D.CONNECTED6;
        method = AlgorithmMorphology3D.CLOSE;
        itersDilation = 1;
        itersErosion = 1;
        closeAlgo3D = new AlgorithmMorphology3D(threshImage, kernel, sphereDiameter, method, itersDilation,
                                                itersErosion, numPruningPixels, edgingType, entireImage);
        closeAlgo3D.run();
        closeAlgo3D.finalize();
        closeAlgo3D = null;
        // new ViewJFrameImage(threshImage);

        /*kernel = AlgorithmMorphology25D.CONNECTED4;
         * method = AlgorithmMorphology25D.CLOSE; itersDilation = 1; itersErosion = 1; closeAlgo25D = new
         * AlgorithmMorphology25D(threshImage2, kernel, circleDiameter, method, itersDilation, itersErosion,
         * numPruningPixels, edgingType, entireImage); closeAlgo25D.run(); closeAlgo25D.finalize();closeAlgo25D =
         * null;*/

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

        // algoVOIExtraction.setColorTable(colorTable);
        // algoVOIExtraction.setNameTable(nameTable);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;


        contourVOI2 = threshImage.getVOIs().VOIAt(0);
        contourVOI2.setName("contourVOI2");
        contourVOI2.setAllActive(true);
        curves2 = contourVOI2.getSortedCurves(zDim);
        sliceBuffer = new float[sliceSize];
        contourVOI3L = new VOI((short) 3, "contourVOI3L", VOI.CONTOUR, -1.0f);
        contourVOI3R = new VOI((short) 4, "contourVOI3R", VOI.CONTOUR, -1.0f);
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
                xcen = ((VOIContour) (curves2[z].elementAt(i))).getGeometricCenter().X;

                if (Math.abs(xcen - xcen1) < Math.abs(xcen - xcen2)) {
                    setMem[i] = 1;
                    num1++;
                } else {
                    setMem[i] = 2;
                    num2++;
                }
            } // for (i = 0; i < nCurves; i++)

            if ((num1 <= 1) && (num2 <= 1)) {

                // contourVOI3.importCurve((VOIContour)(curves2[z].elementAt(i)), z); causes these curves to disappear
                // when threshImage.disposeLocal(); occurs.
                Vector<VOIBase> sliceCurves = contourVOI2.getSliceCurves(z);

                for (i = 0; i < nCurves; i++) {

                    if (setMem[i] == 1) {
                        contourVOI3L.importCurve(sliceCurves.elementAt(i));
                    } else {
                        contourVOI3R.importCurve(sliceCurves.elementAt(i));
                    }
                }

                continue;
            } else if (num1 == 1) {

                for (i = 0; i < nCurves; i++) {

                    if (setMem[i] == 1) {
                        poly = ((VOIContour) (curves2[z].elementAt(i))).exportPolygon();
                        contourVOI3L.importPolygon(poly, z);
                    }
                }
            } // else if (num1 == 1)
            else if (num2 == 1) {

                for (i = 0; i < nCurves; i++) {

                    if (setMem[i] == 2) {
                        poly = ((VOIContour) (curves2[z].elementAt(i))).exportPolygon();
                        contourVOI3R.importPolygon(poly, z);
                    }
                }
            } // else if (num2 == 1)

            try {
                srcImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
            } catch (IOException error) {
                MipavUtil.displayError("IOException on srcImage.exportData");
                setCompleted(false);

                return;
            }

            try {
                sliceImage.importData(0, sliceBuffer, true);
            } catch (IOException error) {
                MipavUtil.displayError("IOException on sliceImage.importData");
                setCompleted(false);

                return;
            }

            if (num1 > 1) {
                VOI1 = new VOI((short) 1, "one.voi", VOI.CONTOUR, -1.0f);

                for (i = 0; i < nCurves; i++) {

                    if (setMem[i] == 1) {
                        VOI1.importCurve((VOIContour) (curves2[z].elementAt(i)));
                    } // if (setMem[i] == 1)
                } // for (i = 0; i < nCurves; i++)

                (sliceImage.getVOIs()).addVOI(VOI1);
                nextVOI = contourVOI3L;
                sliceCorrect(1, false);
                (sliceImage.getVOIs()).removeAllElements();
            } // if (num1 > 1)

            if (num2 > 1) {
                VOI2 = new VOI((short) 2, "two.voi", VOI.CONTOUR, -1.0f);

                for (i = 0; i < nCurves; i++) {

                    if (setMem[i] == 2) {
                        VOI2.importCurve((VOIContour) (curves2[z].elementAt(i)));
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
            contourVOI4L = new VOI((short) 5, "contourVOI4L", VOI.CONTOUR, -1.0f);
            contourVOI4R = new VOI((short) 6, "contourVOI4R", VOI.CONTOUR, -1.0f);
            mask = new BitSet(totLength);
            area = new int[zDim];

            for (vIters = 1; vIters <= 2; vIters++) {

                if (vIters == 1) {
                    currentVOI = contourVOI3L;
                    nextVOI = contourVOI4L;
                } else {
                    currentVOI = contourVOI3R;
                    nextVOI = contourVOI4R;
                }

                curves3 = currentVOI.getSortedCurves(zDim);
                mask.clear();
                currentVOI.createBinaryMask3D(mask, xDim, yDim, xor, onlyActive);

                for (z = 0; z < zDim; z++) {
                    area[z] = 0;
                    offset = z * sliceSize;

                    for (i = 0; i < sliceSize; i++) {

                        if (mask.get(offset + i)) {
                            area[z]++;
                        }
                    }
                } // for (z = 0; z < zDim; z++)

                if (curves3[0].size() > 0) {
                    nextVOI.importCurve((VOIContour) (curves3[0].elementAt(0)));
                }

                if (curves3[zDim - 1].size() > 0) {
                    nextVOI.importCurve((VOIContour) (curves3[zDim - 1].elementAt(0)));
                }

                for (z = 1; z < (zDim - 1); z++) {

                    if ((curves3[z].size() > 0) && (area[z] < (0.8 * area[z - 1])) && (area[z] < (0.8 * area[z + 1]))) {

                        try {
                            srcImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
                        } catch (IOException error) {
                            MipavUtil.displayError("IOException on srcImage.exportData");
                            setCompleted(false);

                            return;
                        }

                        try {
                            sliceImage.importData(0, sliceBuffer, true);
                        } catch (IOException error) {
                            MipavUtil.displayError("IOException on sliceImage.importData");
                            setCompleted(false);

                            return;
                        }

                        VOI1 = new VOI((short) 1, "one.voi", VOI.CONTOUR, -1.0f);
                        VOI1.importCurve((VOIContour) (curves3[z].elementAt(0)));
                        (sliceImage.getVOIs()).addVOI(VOI1);
                        sliceCorrect(vIters, true);

                        if (!finished) {
                            nextVOI.importCurve((VOIContour) (curves3[z].elementAt(0)));
                        }

                        (sliceImage.getVOIs()).removeAllElements();
                    } // if ((curves3.size() > 0) && (area[z] < 0.8*area[z-1]) && (area[z] < 0.8*area[z+1]))
                    else {

                        if (curves3[z].size() > 0) {
                            nextVOI.importCurve((VOIContour) (curves3[z].elementAt(0)));
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

    /**
     * DOCUMENT ME!
     *
     * @param  side       DOCUMENT ME!
     * @param  doingArea  DOCUMENT ME!
     */
    private void sliceCorrect(int side, boolean doingArea) {
        VOI sliceVOI;
        AlgorithmVOIProps algoVOIProps;
        float VOIMin;
        float VOIMax;
        float[] threshold = new float[2];
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
        short[] shortMask;
        short[] shortBuffer;
        boolean[] keepObject;
        int i;
        AlgorithmMorphology2D dilationAlgo2D;
        AlgorithmMorphology2D closeAlgo2D;
        AlgorithmVOIExtraction algoVOIExtraction;
        int nCurves;
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
            algoVOIProps = new AlgorithmVOIProps(sliceImage, AlgorithmVOIProps.PROCESS_PER_VOI,
                                                    RangeType.NO_RANGE, getActiveVOIs(sliceImage));
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
                } else {
                    lastFractionL[z] = lastFractionL[z] - incr;
                }

                threshold[0] = VOIMin + (lastFractionL[z] * (VOIMax - VOIMin));
            } else {

                if (tooBig) {
                    lastFractionR[z] = lastFractionR[z] + incr;
                } else {
                    lastFractionR[z] = lastFractionR[z] - incr;
                }

                threshold[0] = VOIMin + (lastFractionR[z] * (VOIMax - VOIMin));
            }

            threshold[1] = VOIMax;
            fillValue = 0.0f;
            entireImage = true;
            fillValueOutsideThresholds = true;
            algoThreshDual = new AlgorithmThresholdDual(threshSliceImage, sliceImage, threshold, fillValue,
                                                        AlgorithmThresholdDual.BINARY_TYPE, entireImage,
                                                        fillValueOutsideThresholds);
            algoThreshDual.run();
            algoThreshDual.finalize();
            algoThreshDual = null;

            fillHolesAlgo2D = new AlgorithmMorphology2D(threshSliceImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0,
                                                        0, 0, entireImage);
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
                erosionAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method,
                                                          itersDilation, itersErosion, numPruningPixels, edgingType,
                                                          entireImage);
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
            } catch (IOException error) {
                MipavUtil.displayError("Error on threshSliceImage.exportData");
                setCompleted(false);

                return;
            }

            for (i = 0; i < sliceSize; i++) {

                if (shortMask[i] != -1) {

                    if (shortBuffer[i] != 0) {
                        keepObject[shortBuffer[i] - 1] = true;
                    }
                }
            }

            for (i = 0; i < sliceSize; i++) {

                if ((shortBuffer[i] > 0) && keepObject[shortBuffer[i] - 1]) {
                    shortBuffer[i] = 1;
                } else {
                    shortBuffer[i] = 0;
                }
            }

            try {
                threshSliceImage.importData(0, shortBuffer, true);
            } catch (IOException error) {
                MipavUtil.displayError("Error on threshSliceImage.importData");
                setCompleted(false);

                return;
            }

            kernel = AlgorithmMorphology2D.CONNECTED4;
            method = AlgorithmMorphology2D.DILATE;
            itersDilation = 1;
            itersErosion = 0;

            if (iters >= 1) {
                dilationAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method,
                                                           itersDilation, itersErosion, numPruningPixels, edgingType,
                                                           entireImage);
                dilationAlgo2D.run();
                dilationAlgo2D.finalize();
                dilationAlgo2D = null;
            }

            fillHolesAlgo2D = new AlgorithmMorphology2D(threshSliceImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0,
                                                        0, 0, entireImage);
            fillHolesAlgo2D.run();
            fillHolesAlgo2D.finalize();
            fillHolesAlgo2D = null;

            kernel = AlgorithmMorphology2D.CONNECTED4;
            method = AlgorithmMorphology2D.CLOSE;
            itersDilation = 1;
            itersErosion = 1;
            closeAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, entireImage);
            closeAlgo2D.run();
            closeAlgo2D.finalize();
            closeAlgo2D = null;

            algoVOIExtraction = new AlgorithmVOIExtraction(threshSliceImage);

            // algoVOIExtraction.setColorTable(colorTable);
            // algoVOIExtraction.setNameTable(nameTable);
            algoVOIExtraction.run();
            algoVOIExtraction.finalize();
            algoVOIExtraction = null;
            // System.out.println("threshSliceImage.getVOIs().size = " + threshSliceImage.getVOIs().size());

            if (doingArea && (threshSliceImage.getVOIs().size() > 0) &&
                    (threshSliceImage.getVOIs().VOIAt(0).getCurves().size() > 0)) {
                sliceMask.clear();
                threshSliceImage.getVOIs().VOIAt(0).createBinaryMask3D(sliceMask, xDim, yDim, xor, onlyActive);
                area[z] = 0;

                for (i = 0; i < sliceSize; i++) {

                    if (sliceMask.get(i)) {
                        area[z]++;
                    }
                }
            } // if (doingArea && threshSliceImage.getVOIs().size() > 0)

            if (threshSliceImage.getVOIs().size() > 0) {
                nCurves = threshSliceImage.getVOIs().VOIAt(0).getCurves().size();

                if (!doingArea && (nCurves == 1)) {
                    finished = true;
                } else if (doingArea && ((area[z] >= (1.1 * area[z - 1])) && (area[z] >= (1.1 * area[z + 1])))) {
                    tooBig = true;
                    incr = 0.5f * incr;
                } else if (doingArea && ((area[z] >= (0.8 * area[z - 1])) || (area[z] >= (0.8 * area[z + 1])))) {
                    finished = true;
                    tooBig = false;
                }

                if ((!tooBig) && (finished || (sliceIter == finalIter))) {

                    for (i = 0; i < nCurves; i++) {
                        VOIContour polygonContour = (VOIContour)threshSliceImage.getVOIs().VOIAt(0).getCurves().elementAt(i);
                        nextVOI.importPolygon(polygonContour.exportPolygon(), z);
                    }
                }
            }

            sliceIter++;
        } // while((sliceIter <= finalIter) && (!finished))
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doingArea  DOCUMENT ME!
     */
    private void sliceFix(boolean doingArea) {
        VOI sliceVOI;
        AlgorithmVOIProps algoVOIProps;
        float VOIMin;
        float VOIMax;
        float[] threshold = new float[2];
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
        short[] shortMask;
        short[] shortBuffer;
        boolean[] keepObject;
        int i;
        AlgorithmMorphology2D dilationAlgo2D;
        AlgorithmMorphology2D closeAlgo2D;
        AlgorithmVOIExtraction algoVOIExtraction;
        int nCurves;
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
            algoVOIProps = new AlgorithmVOIProps(sliceImage, AlgorithmVOIProps.PROCESS_PER_VOI,
                                                    RangeType.NO_RANGE, getActiveVOIs(sliceImage));
            algoVOIProps.run();
            VOIMin = algoVOIProps.getMinIntensity();
            VOIMax = algoVOIProps.getMaxIntensity();
            algoVOIProps.finalize();
            algoVOIProps = null;

            // Initially the top 50% of the values between the VOIMin and VOIMax were used
            // On each successive decrease the lower threshold by 5% of the range between VOIMin and VOIMax.
            if (tooBig) {
                lastFraction[z] = lastFraction[z] + incr;
            } else {
                lastFraction[z] = lastFraction[z] - incr;
            }

            threshold[0] = VOIMin + (lastFraction[z] * (VOIMax - VOIMin));

            threshold[1] = VOIMax;
            fillValue = 0.0f;
            entireImage = true;
            fillValueOutsideThresholds = true;
            algoThreshDual = new AlgorithmThresholdDual(threshSliceImage, sliceImage, threshold, fillValue,
                                                        AlgorithmThresholdDual.BINARY_TYPE, entireImage,
                                                        fillValueOutsideThresholds);
            algoThreshDual.run();
            algoThreshDual.finalize();
            algoThreshDual = null;

            fillHolesAlgo2D = new AlgorithmMorphology2D(threshSliceImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0,
                                                        0, 0, entireImage);
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
                erosionAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method,
                                                          itersDilation, itersErosion, numPruningPixels, edgingType,
                                                          entireImage);
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
            } catch (IOException error) {
                MipavUtil.displayError("Error on threshSliceImage.exportData");
                setCompleted(false);

                return;
            }

            for (i = 0; i < sliceSize; i++) {

                if (shortMask[i] != -1) {

                    if (shortBuffer[i] != 0) {
                        keepObject[shortBuffer[i] - 1] = true;
                    }
                }
            }

            for (i = 0; i < sliceSize; i++) {

                if ((shortBuffer[i] > 0) && keepObject[shortBuffer[i] - 1]) {
                    shortBuffer[i] = 1;
                } else {
                    shortBuffer[i] = 0;
                }
            }

            try {
                threshSliceImage.importData(0, shortBuffer, true);
            } catch (IOException error) {
                MipavUtil.displayError("Error on threshSliceImage.importData");
                setCompleted(false);

                return;
            }

            kernel = AlgorithmMorphology2D.CONNECTED4;
            method = AlgorithmMorphology2D.DILATE;
            itersDilation = 1;
            itersErosion = 0;

            if (iters >= 1) {
                dilationAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method,
                                                           itersDilation, itersErosion, numPruningPixels, edgingType,
                                                           entireImage);
                dilationAlgo2D.run();
                dilationAlgo2D.finalize();
                dilationAlgo2D = null;
            }

            fillHolesAlgo2D = new AlgorithmMorphology2D(threshSliceImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0,
                                                        0, 0, entireImage);
            fillHolesAlgo2D.run();
            fillHolesAlgo2D.finalize();
            fillHolesAlgo2D = null;

            kernel = AlgorithmMorphology2D.CONNECTED4;
            method = AlgorithmMorphology2D.CLOSE;
            itersDilation = 1;
            itersErosion = 1;
            closeAlgo2D = new AlgorithmMorphology2D(threshSliceImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, entireImage);
            closeAlgo2D.run();
            closeAlgo2D.finalize();
            closeAlgo2D = null;

            algoVOIExtraction = new AlgorithmVOIExtraction(threshSliceImage);

            // algoVOIExtraction.setColorTable(colorTable);
            // algoVOIExtraction.setNameTable(nameTable);
            algoVOIExtraction.run();
            algoVOIExtraction.finalize();
            algoVOIExtraction = null;
            // System.out.println("threshSliceImage.getVOIs().size = " + threshSliceImage.getVOIs().size());

            if (doingArea && (threshSliceImage.getVOIs().size() > 0) &&
                    (threshSliceImage.getVOIs().VOIAt(0).getCurves().size() > 0)) {
                sliceMask.clear();
                threshSliceImage.getVOIs().VOIAt(0).createBinaryMask3D(sliceMask, xDim, yDim, xor, onlyActive);
                area[z] = 0;

                for (i = 0; i < sliceSize; i++) {

                    if (sliceMask.get(i)) {
                        area[z]++;
                    }
                }
            } // if (doingArea && threshSliceImage.getVOIs().size() > 0)

            if (threshSliceImage.getVOIs().size() > 0) {
                nCurves = threshSliceImage.getVOIs().VOIAt(0).getCurves().size();

                if (!doingArea && (nCurves == 1)) {
                    finished = true;
                } else if (doingArea && ((area[z] >= (1.1 * area[z - 1])) && (area[z] >= (1.1 * area[z + 1])))) {
                    tooBig = true;
                    incr = 0.5f * incr;
                } else if (doingArea && ((area[z] >= (0.8 * area[z - 1])) || (area[z] >= (0.8 * area[z + 1])))) {
                    finished = true;
                    tooBig = false;
                }

                if ((!tooBig) && (finished || (sliceIter == finalIter))) {

                    for (i = 0; i < nCurves; i++) {
                        VOIContour polygonContour = (VOIContour)threshSliceImage.getVOIs().VOIAt(0).getCurves().elementAt(i);
                        nextVOI.importPolygon(polygonContour.exportPolygon(), z);
                    }
                }
            }

            sliceIter++;
        } // while((sliceIter <= finalIter) && (!finished))
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
