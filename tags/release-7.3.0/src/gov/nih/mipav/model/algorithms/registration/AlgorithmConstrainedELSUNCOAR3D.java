package gov.nih.mipav.model.algorithms.registration;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.util.*;


/**
 * This is an automatic registration method based on FLIRT. FLIRT stands for FMRIB's Linear Image Registration Tool 1.3.
 * For more information on FLIRT, visit their homepage at <a href="http://www.fmrib.ox.ac.uk/fsl/flirt/">
 * http://www.fmrib.ox.ac.uk/fsl/flirt/</a>. Their main paper is:
 *
 * <p>Jenkinson, M. and Smith, S. (2001a).<br>
 * A global optimisation method for robust affine registration of brain images.<br>
 * <i>Medical Image Analysis</i>, 5(2):143-156.<br>
 * </p>
 *
 * <p>Our algorithm works as follows:<br>
 * 1.) We find the minimum resolution of the images and blur them if neccessary.<br>
 * 2.) We transform the images into isotropic voxels.<br>
 * 3.) We subsample the images by 2, 4, and 8, depending on the resolution.<br>
 * Subsampling can be performed for x, y, and z or for only x and y. 4.) With the images that were subsampled by 8, we
 * call levelEight. This function will use the coarse sampling rate and optimize translations and global scale at the
 * given rotation. So for example, if the coarse sampling range were -30 to 30 at every 15 degrees, we would optimize at
 * rotations of (-30, -30, -30), (-30, -30, -15), (-30, -30, 0), etc. In this case there would be a total of 125 calls
 * to the optimization method.<br>
 * 5.) Still in levelEight, we now measure the cost at the fine sampling rate. We interpolate the translations and
 * global scale to come up with a good guess as to what the optimized translation would be at that point.<br>
 * 6.) We take the top 20% of the points and optimize them.<br>
 * 7.) We now have a large multi-array of costs. 20% of those have been optimized and placed back into their original
 * position in the multi-array. We look at the 9 neighbors of a point: +, =, or - one fine sample in each of the three
 * directions. If our point has a cost greater than any of these, it is not a minima. Otherwise it is. We save it in a
 * vector of minima.<br>
 * 8.) We optimize the minima over rotations as well as translations and global scale. (Previously we had not optimized
 * over rotations.) We return two vectors, one containing the minima before optimization, one containing the minima
 * after optimization.<br>
 * 9.) We now call levelFour with the images subsampled by 4 and the vectors of minima. We measure the costs of the
 * minima on the new images and sort them. We take the top numMinima in each vector (pre-optimization and
 * post-optimization) and optimize them. We put them all into one vector.<br>
 * 10.) We perturb the rotations in each dimension by zero and plus-minus fineDelta. If it's not a rigid transformation,
 * we then perturb the scaling by factors of 0.8, 0.9, 1.0, 1.1, and 1.2.<br>
 * 11.) We optimize the perturbations. We return a vector of the perturbed, optimized minima.<br>
 * 12.) We now call levelTwo with the images subsampled by 2. We measure the costs of the minima at the new images. We
 * optimize the best minimum with 7 degrees of freedom, then 9, then 12. If the user has limited the degrees of freedom
 * to 6, there will only be one optimization run, with 6 degrees of freedom. The function returns the best minimum after
 * optimization.<br>
 * 13.) We call levelOne with the un-subsampled images. At levelOne, one optimization run is performed, with the maximum
 * allowable degrees of freedom, as specified by the user (the max is 12).<br>
 * 14.) The best answer is returned from levelOne. The matrix from this answer is saved in a file and also accessed by
 * the dialog that called this algorithm.<br>
 * </p>
 *
 * <p>Only subsample if 16 or more z slices are present so that the number of z slices will not be reduced below 8.</p>
 *
 * @author  Neva Cherniavsky
 * @author  Matthew McAuliffe
 */
public class AlgorithmConstrainedELSUNCOAR3D extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int minimumZForSub = 16;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    double initialCost, maxPossibleCost;

    /** DOCUMENT ME! */
    long timeElapsed;

    /** DOCUMENT ME! */
    long timeLater;

    /** DOCUMENT ME! */
    long timeNow;

    /** DOCUMENT ME! */
    TransMatrix tMatrix = new TransMatrix(4);

    /** DOCUMENT ME! */
    private boolean allowLevel16XY = true;

    /** DOCUMENT ME! */
    private boolean allowLevel16Z = true;

    /** DOCUMENT ME! */
    private boolean allowLevel2XY = true;

    /** DOCUMENT ME! */
    private boolean allowLevel2Z = true;

    /** DOCUMENT ME! */
    private boolean allowLevel4XY = true;

    /** DOCUMENT ME! */
    private boolean allowLevel4Z = true;

    /** DOCUMENT ME! */
    private boolean allowLevel8XY = true;

    /** DOCUMENT ME! */
    private boolean allowLevel8Z = true;

    /** Final answer after registration. */
    private MatrixListItem answer;

    /** DOCUMENT ME! */
    private MatrixListItem bestGuessLevel2;

    /** Blurred input image. */
    private ModelImage blurredInput = null;

    /** Blurred reference image. */
    private ModelImage blurredRef = null;

    /**
     * Sets minimum and maximum limits as initial guess -+ unit_tolerance[i]*bracketBound.
     */
    private int bracketBound;

    /**
     * If true calculate the center of gravity (mass) and use the difference to intialize the translation. If false,
     * images are pretty much aligned then don't calculated COG.
     */
    private boolean calcCOG = true;

    /** Number of passes that will be made in the coarse sampling and fine sampling. */
    private int coarseNumX, fineNumX;

    /** DOCUMENT ME! */
    private int coarseNumY, fineNumY;

    /** DOCUMENT ME! */
    private int coarseNumZ, fineNumZ;

    /** Choice of which cost function to use. */
    private int costChoice;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** Maximum degrees of freedom when running the optimization. */
    private int DOF;

    /** If true subsample for levelEight, levelFour and levelTwo analyses. */
    private boolean doSubsample = true;
    
    /** Dummy initial values used to create a ELSUNC algorithm instance before setting initial. */
    private double[] dummy = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

    /**
     * If true this algorithm skips all subsample and goes directly to the level 1 optimization. This assumes that
     * images are fairly well aligned to begin with and therefore no sophisticated search is needed.
     */
    private boolean fastMode = false, fullAnalysisMode = true;

    /** Isotropic input image. */
    private ModelImage imageInputIso = null;

    /** Isotropic reference image. */
    private ModelImage imageRefIso = null;

    /** Isotropic weighted input image. */
    private ModelImage imageWeightInputIso = null;

    /** Isotropic weighted reference image. */
    private ModelImage imageWeightRefIso = null;

    /** DOCUMENT ME! */
    private String initialMessage = new String("Initial point is out of Bounds!\n");

    /** This image is to registered to the reference image. */
    private ModelImage inputImage;

    /**
     * This gives weights for the input image - higher weights mean a greater impact in that area on the registration.
     */
    private ModelImage inputWeight = null;

    /** Interpolation method. */
    private int interp;

    /** Multiplication factor for level 1 - will be set based on subsampling. */
    private float level1FactorXY = 1.0f;

    /** DOCUMENT ME! */
    private float level1FactorZ = 1.0f;

    /** Multiplication factor for level 2 - will be set based on subsampling. */
    private float level2FactorXY = 1.0f;

    /** DOCUMENT ME! */
    private float level2FactorZ = 1.0f;

    /** Multiplication factor for level 4 - will be set based on subsampling. */
    private float level4FactorXY = 1.0f;

    /** DOCUMENT ME! */
    private float level4FactorZ = 1.0f;

    /** Translation limits. */
    private float[][] limits;

    /** DOCUMENT ME! */
    private float[][] limits_mm;

    /** DOCUMENT ME! */
    private int maxDim = 256;

    /** Advanced optimization settings maxIter in the call to ELSUNC will be an integer multiple of baseNumIter. */
    private int maxIter, baseNumIter;

    /**
     * Flag to determine if the maximum of the minimum resolutions of the two datasets should be used. If true use the
     * maximum resolution of the two dataset. Throws away information some image information but is faster. If false the
     * algorithms uses the minimum of the resolutions when resampling the images. Can be slower but does not "lose"
     * informaton.
     */
    private boolean maxResol;

    /** Number of minima from level 8 to test at level 4. */
    private int numMinima;

    /** The inputImage will be registered to this reference image. */
    private ModelImage refImage;

    /**
     * This gives weights for the reference image - higher weights mean a greater impact in that area on the
     * registration.
     */
    private ModelImage refWeight = null;

    /** DOCUMENT ME! */
    private boolean resampleInput;

    /** DOCUMENT ME! */
    private boolean resampleRef;

    /** The voxel resolutions of the image to be registered to the reference image. */
    private float[] resInput = null;

    /** The voxel resolutions of the reference image. */
    private float[] resRef = null;

    /** Coarse and fine sampling parameters. */
    private float rotateBeginX, rotateRangeX, coarseRateX, fineRateX;

    /** DOCUMENT ME! */
    private float rotateBeginY, rotateRangeY, coarseRateY, fineRateY;

    /** DOCUMENT ME! */
    private float rotateBeginZ, rotateRangeZ, coarseRateZ, fineRateZ;

    /** Simple version of input image. */
    private ModelSimpleImage simpleInput = null;

    /** Simple version of input image, subsampled by 2. */
    private ModelSimpleImage simpleInputSub2 = null;

    /** Simple version of input image, subsampled by 4. */
    private ModelSimpleImage simpleInputSub4 = null;

    /** Simple version of input image, subsampled by 8. */
    private ModelSimpleImage simpleInputSub8 = null;

    /** Simple version of reference image. */
    private ModelSimpleImage simpleRef;

    /** Simple version of reference image, subsampled by 2. */
    private ModelSimpleImage simpleRefSub2 = null;

    /** Simple version of reference image, subsampled by 4. */
    private ModelSimpleImage simpleRefSub4 = null;

    /** Simple version of reference image, subsampled by 8. */
    private ModelSimpleImage simpleRefSub8;

    /** Simple version of weighted input image. */
    private ModelSimpleImage simpleWeightInput = null;

    /** Simple version of weighted input image, subsampled by 2. */
    private ModelSimpleImage simpleWeightInputSub2 = null;

    /** Simple version of weighted input image, subsampled by 4. */
    private ModelSimpleImage simpleWeightInputSub4 = null;

    /** Simple version of weighted input image, subsampled by 8. */
    private ModelSimpleImage simpleWeightInputSub8 = null;

    /** Simple version of weighted reference image. */
    private ModelSimpleImage simpleWeightRef = null;

    /** Simple version of weighted reference image, subsampled by 2. */
    private ModelSimpleImage simpleWeightRefSub2 = null;

    /** Simple version of weighted reference image, subsampled by 4. */
    private ModelSimpleImage simpleWeightRefSub4 = null;

    /** Simple version of weighted reference image, subsampled by 8. */
    private ModelSimpleImage simpleWeightRefSub8 = null;

    /** Transformation algorithm for creating an isotropic reference image. */
    private AlgorithmTransform transform = null;

    /** Transformation algorithm for creating an isotropic input image. */
    private AlgorithmTransform transform2 = null;

    /** Flag to determine if there are weighted images or not. */
    private boolean weighted;

    /** DOCUMENT ME! */
    private int weightedInputPixels = 0;

    /** DOCUMENT ME! */
    private int weightedInputPixelsSub2 = 0;

    /** DOCUMENT ME! */
    private int weightedInputPixelsSub4 = 0;

    /** DOCUMENT ME! */
    private int weightedInputPixelsSub8 = 0;

    /** DOCUMENT ME! */
    private int weightedRefPixels = 0;

    /** DOCUMENT ME! */
    private int weightedRefPixelsSub2 = 0;

    /** DOCUMENT ME! */
    private int weightedRefPixelsSub4 = 0;

    /** DOCUMENT ME! */
    private int weightedRefPixelsSub8 = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     *
     * @param  _imageA        Reference image (register input image to reference image).
     * @param  _imageB        Input image (register input image to reference image).
     * @param  _costChoice    Choice of cost functions, like correlation ratio or mutual information.
     * @param  _DOF           Degrees of freedom for registration
     * @param  _interp        Interpolation method used in transformations.
     * @param  _rotateBeginX  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateRangeX  DOCUMENT ME!
     * @param  _rotateBeginY  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateRangeY  DOCUMENT ME!
     * @param  _rotateBeginZ  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateRangeZ  DOCUMENT ME!
     * @param  _nCoarseX      DOCUMENT ME!
     * @param  _nCoarseY      DOCUMENT ME!
     * @param  _nCoarseZ      DOCUMENT ME!
     * @param  _transLimits   Limitations on translations.
     * @param  _maxResol      If true is the maximum of the minimum resolution of the two datasets when resampling.
     * @param  _doSubsample   If true then subsample
     * @param  _fastMode      If true then searching the parameter space is not conducted and the algorithm proceeds to
     *                        level one immediately
     * @param  _calcCOG       If true calculate the center of gravity (mass) to initialize registration.
     * @param  _bracketBound  Sets minimum and maximum limits as initial guess -+ unit_tolerance[i]*bracketBound.
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     */
    public AlgorithmConstrainedELSUNCOAR3D(ModelImage _imageA, ModelImage _imageB, int _costChoice, int _DOF, int _interp,
                                     float _rotateBeginX, float _rotateRangeX, float _rotateBeginY, float _rotateRangeY,
                                     float _rotateBeginZ, float _rotateRangeZ, int _nCoarseX, int _nCoarseY,
                                     int _nCoarseZ, float[][] _transLimits, boolean _maxResol, boolean _doSubsample,
                                     boolean _fastMode, boolean _calcCOG, int _bracketBound, int _baseNumIter,
                                     int _numMinima) {

        super(null, _imageB);
        refImage = _imageA;
        inputImage = _imageB;

        if (inputImage.isColorImage()) {
            doColor = true;
        } else {
            doColor = false;
        }

        costChoice = _costChoice;
        maxPossibleCost = Double.MAX_VALUE;
        DOF = _DOF;
        interp = _interp;
        resRef = refImage.getFileInfo(0).getResolutions();
        resInput = inputImage.getFileInfo(0).getResolutions();

        // set up rotation parameters - range, number of increments, limits, etc
        rotateBeginX = _rotateBeginX;
        rotateRangeX = _rotateRangeX;
        coarseNumX = _nCoarseX + 1;
        rotateBeginY = _rotateBeginY;
        rotateRangeY = _rotateRangeY;
        coarseNumY = _nCoarseY + 1;
        rotateBeginZ = _rotateBeginZ;
        rotateRangeZ = _rotateRangeZ;
        coarseNumZ = _nCoarseZ + 1;
        fineNumX = 4 * coarseNumX;
        fineNumY = 4 * coarseNumY;
        fineNumZ = 4 * coarseNumZ;
        coarseRateX = rotateRangeX / (float) coarseNumX;
        coarseRateY = rotateRangeY / (float) coarseNumY;
        coarseRateZ = rotateRangeZ / (float) coarseNumZ;
        fineRateX = rotateRangeX / (float) fineNumX;
        fineRateY = rotateRangeY / (float) fineNumY;
        fineRateZ = rotateRangeZ / (float) fineNumZ;
        limits = new float[2][6];
        limits[0][0] = rotateBeginX;
        limits[1][0] = rotateBeginX + rotateRangeX;
        limits[0][1] = rotateBeginY;
        limits[1][1] = rotateBeginY + rotateRangeY;
        limits[0][2] = rotateBeginZ;
        limits[1][2] = rotateBeginZ + rotateRangeZ;

        // set up translation limits
        limits_mm = new float[2][3];

        for (int i = 0; i <= 2; i++) {

            for (int j = 0; j <= 1; j++) {
                limits_mm[j][i] = _transLimits[j][i]; // JDialog passes them in in mm.
            }
        }

        // note that limits are now in pixels
        weighted = false;
        maxResol = _maxResol;
        doSubsample = _doSubsample;
        fastMode = _fastMode;

        if (fastMode) {
            fullAnalysisMode = false;
        }

        calcCOG = _calcCOG;
        bracketBound = _bracketBound;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;
    }

    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     *
     * @param  _imageA        Reference image (register input image to reference image).
     * @param  _imageB        Input image (register input image to reference image).
     * @param  _refWeight     Reference weighted image, used to give certain areas of the image greater impact on the
     *                        registration.
     * @param  _inputWeight   Input weighted image, used to give certain areas of the image greater impact on the
     *                        registration.
     * @param  _costChoice    Choice of cost functions, like correlation ratio or mutual information.
     * @param  _DOF           Degrees of freedom for registration
     * @param  _interp        Interpolation method used in transformations.
     * @param  _rotateBeginX  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateRangeX  DOCUMENT ME!
     * @param  _rotateBeginY  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateRangeY  DOCUMENT ME!
     * @param  _rotateBeginZ  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateRangeZ  DOCUMENT ME!
     * @param  _nCoarseX      DOCUMENT ME!
     * @param  _nCoarseY      DOCUMENT ME!
     * @param  _nCoarseZ      DOCUMENT ME!
     * @param  _transLimits   DOCUMENT ME!
     * @param  _maxResol      If true is the maximum of the minimum resolution of the two datasets when resampling.
     * @param  _doSubsample   If true then subsample
     * @param  _fastMode      If true then searching the parameter space is not conducted and the algorithm proceeds to
     *                        level one immediately
     * @param  _calcCOG       If true calculate the center of gravity (mass) to initialize registration.
     * @param  _bracketBound  Sets minimum and maximum limits as initial guess -+ unit_tolerance[i]*bracketBound.
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     */
    public AlgorithmConstrainedELSUNCOAR3D(ModelImage _imageA, ModelImage _imageB, ModelImage _refWeight,
                                     ModelImage _inputWeight, int _costChoice, int _DOF, int _interp,
                                     float _rotateBeginX, float _rotateRangeX, float _rotateBeginY, float _rotateRangeY,
                                     float _rotateBeginZ, float _rotateRangeZ, int _nCoarseX, int _nCoarseY,
                                     int _nCoarseZ, float[][] _transLimits, boolean _maxResol, boolean _doSubsample,
                                     boolean _fastMode, boolean _calcCOG, int _bracketBound, int _baseNumIter,
                                     int _numMinima) {
        super(null, _imageB);
        refImage = _imageA;
        inputImage = _imageB;

        if (inputImage.isColorImage()) {
            doColor = true;
        } else {
            doColor = false;
        }

        refWeight = _refWeight;
        inputWeight = _inputWeight;
        costChoice = _costChoice;
        maxPossibleCost = Double.MAX_VALUE;
        DOF = _DOF;
        interp = _interp;
        resRef = refImage.getFileInfo(0).getResolutions();
        resInput = inputImage.getFileInfo(0).getResolutions();

        // set up rotation parameters - range, number of increments, limits, etc
        rotateBeginX = _rotateBeginX;
        rotateRangeX = _rotateRangeX;
        coarseNumX = _nCoarseX;
        rotateBeginY = _rotateBeginY;
        rotateRangeY = _rotateRangeY;
        coarseNumY = _nCoarseY;
        rotateBeginZ = _rotateBeginZ;
        rotateRangeZ = _rotateRangeZ;
        coarseNumZ = _nCoarseZ;
        fineNumX = 4 * coarseNumX;
        fineNumY = 4 * coarseNumY;
        fineNumZ = 4 * coarseNumZ;
        coarseRateX = rotateRangeX / (float) coarseNumX;
        coarseRateY = rotateRangeY / (float) coarseNumY;
        coarseRateZ = rotateRangeZ / (float) coarseNumZ;
        fineRateX = rotateRangeX / (float) fineNumX;
        fineRateY = rotateRangeY / (float) fineNumY;
        fineRateZ = rotateRangeZ / (float) fineNumZ;
        limits = new float[2][6];
        limits[0][0] = rotateBeginX;
        limits[1][0] = rotateBeginX + rotateRangeX;
        limits[0][1] = rotateBeginY;
        limits[1][1] = rotateBeginY + rotateRangeY;
        limits[0][2] = rotateBeginZ;
        limits[1][2] = rotateBeginZ + rotateRangeZ;

        // set up translation limits
        limits_mm = new float[2][3];

        for (int i = 0; i <= 2; i++) {

            for (int j = 0; j <= 1; j++) {
                limits_mm[j][i] = _transLimits[j][i];
            }
        }

        weighted = true;
        maxResol = _maxResol;
        doSubsample = _doSubsample;
        fastMode = _fastMode;

        if (fastMode) {
            fullAnalysisMode = false;
        }

        bracketBound = _bracketBound;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the center of mass (gravity) of a 3D image. In image space where the upper left hand corner of the
     * image is 0,0. The x axis goes left to right, y axis goes top to bottom and z axis goes into the screen. (i.e. the
     * right hand rule). One could simply multiply by voxel resolutions.
     *
     * @param   image     the center of mass will be calculated from this image data
     * @param   wgtImage  DOCUMENT ME!
     * @param   isColor   DOCUMENT ME!
     *
     * @return  the center of mass as a 3D point
     */
    public static Vector3f calculateCenterOfMass3D(ModelSimpleImage image, ModelSimpleImage wgtImage, boolean isColor) {
        int x, y, z, c;
        int sliceSize = image.xDim * image.yDim;
        float diff;

        Vector3f cogPt = new Vector3f(0, 0, 0);
        double voxVal = 0.0, total = 0.0, wgtVal = 0.0;

        if (isColor) {

            if (wgtImage == null) {

                for (z = 0; z < image.zDim; z++) {

                    for (y = 0; y < image.yDim; y++) {

                        for (x = 0; x < image.xDim; x++) {

                            for (c = 1; c <= 3; c++) {
                                voxVal = image.data[(4 * ((z * sliceSize) + (y * image.xDim) + x)) + c];
                                cogPt.X += voxVal * x;
                                cogPt.Y += voxVal * y;
                                cogPt.Z += voxVal * z;
                                total += voxVal;
                            }
                        }
                    }
                }
            } else { // wgtImage != null

                wgtImage.calcMinMax();

                if ((wgtImage.min < 0) || (wgtImage.max > 1)) {

                    // remap data - normalize data between 0 and 1
                    if (wgtImage.min != wgtImage.max) {
                        diff = wgtImage.max - wgtImage.min;

                        for (int i = 0; i < wgtImage.data.length; i++) {
                            wgtImage.data[i] = (wgtImage.data[i] - wgtImage.min) / diff;
                        }

                        wgtImage.calcMinMax();
                    }
                }

                for (z = 0; z < image.zDim; z++) {

                    for (y = 0; y < image.yDim; y++) {

                        for (x = 0; x < image.xDim; x++) {
                            wgtVal = wgtImage.data[(z * sliceSize) + (y * image.xDim) + x];

                            for (c = 1; c <= 3; c++) {
                                voxVal = image.data[(4 * ((z * sliceSize) + (y * image.xDim) + x)) + c];
                                cogPt.X += wgtVal * voxVal * x;
                                cogPt.Y += wgtVal * voxVal * y;
                                cogPt.Z += wgtVal * voxVal * z;
                                total += wgtVal * voxVal;
                            }
                        }
                    }
                }
            }
        } // if (isColor)
        else { // grey

            if (wgtImage == null) {

                for (z = 0; z < image.zDim; z++) {

                    for (y = 0; y < image.yDim; y++) {

                        for (x = 0; x < image.xDim; x++) {
                            voxVal = image.data[(z * sliceSize) + (y * image.xDim) + x] - image.min;
                            cogPt.X += voxVal * x;
                            cogPt.Y += voxVal * y;
                            cogPt.Z += voxVal * z;
                            total += voxVal;
                        }
                    }
                }
            } else { // wgtImage != null

                wgtImage.calcMinMax();

                if ((wgtImage.min < 0) || (wgtImage.max > 1)) {

                    // remap data - normalize data between 0 and 1
                    if (wgtImage.min != wgtImage.max) {
                        diff = wgtImage.max - wgtImage.min;

                        for (int i = 0; i < wgtImage.data.length; i++) {
                            wgtImage.data[i] = (wgtImage.data[i] - wgtImage.min) / diff;
                        }

                        wgtImage.calcMinMax();
                    }
                }

                for (z = 0; z < image.zDim; z++) {

                    for (y = 0; y < image.yDim; y++) {

                        for (x = 0; x < image.xDim; x++) {
                            voxVal = image.data[(z * sliceSize) + (y * image.xDim) + x] - image.min;
                            wgtVal = wgtImage.data[(z * sliceSize) + (y * image.xDim) + x];
                            cogPt.X += wgtVal * voxVal * x;
                            cogPt.Y += wgtVal * voxVal * y;
                            cogPt.Z += wgtVal * voxVal * z;
                            total += wgtVal * voxVal;

                        }
                    }
                }
            }
        } // grey

        if (total != 0) {
            cogPt.X /= total;
            cogPt.Y /= total;
            cogPt.Z /= total;
        } else { // Do nothing at the moment which will leave the COG
        }

        return cogPt;
    }

    /**
     * Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {

        if (simpleRef != null) {
            simpleRef.disposeLocal(false);
        }

        if (simpleInput != null) {
            simpleInput.disposeLocal(false);
        }

        if (simpleRefSub2 != null) {
            simpleRefSub2.disposeLocal(false);
        }

        if (simpleInputSub2 != null) {
            simpleInputSub2.disposeLocal(false);
        }

        if (simpleRefSub4 != null) {
            simpleRefSub4.disposeLocal(false);
        }

        if (simpleInputSub4 != null) {
            simpleInputSub4.disposeLocal(false);
        }

        if (simpleRefSub8 != null) {
            simpleRefSub8.disposeLocal(false);
        }

        if (simpleInputSub8 != null) {
            simpleInputSub8.disposeLocal(false);
        }

        if (simpleWeightRef != null) {
            simpleWeightRef.disposeLocal(false);
        }

        if (simpleWeightInput != null) {
            simpleWeightInput.disposeLocal(false);
        }

        if (simpleWeightRefSub2 != null) {
            simpleWeightRefSub2.disposeLocal(false);
        }

        if (simpleWeightInputSub2 != null) {
            simpleWeightInputSub2.disposeLocal(false);
        }

        if (simpleWeightRefSub4 != null) {
            simpleWeightRefSub4.disposeLocal(false);
        }

        if (simpleWeightInputSub4 != null) {
            simpleWeightInputSub4.disposeLocal(false);
        }

        if (simpleWeightRefSub8 != null) {
            simpleWeightRefSub8.disposeLocal(false);
        }

        if (simpleWeightInputSub8 != null) {
            simpleWeightInputSub8.disposeLocal(false);
        }

        simpleRef = null;
        simpleInput = null;
        simpleRefSub2 = null;
        simpleInputSub2 = null;
        simpleRefSub4 = null;
        simpleInputSub4 = null;
        simpleRefSub8 = null;
        simpleInputSub8 = null;
        dummy = null;

        simpleWeightRef = null;
        simpleWeightInput = null;
        simpleWeightRefSub2 = null;
        simpleWeightInputSub2 = null;
        simpleWeightRefSub4 = null;
        simpleWeightInputSub4 = null;
        simpleWeightRefSub8 = null;
        simpleWeightInputSub8 = null;

        if ((blurredRef != refImage) && (blurredRef != null)) {
            blurredRef.disposeLocal();
        } else {
            blurredRef = null;
        }

        if ((blurredInput != inputImage) && (blurredInput != null)) {
            blurredInput.disposeLocal();
        } else {
            blurredInput = null;
        }

        if (imageRefIso != null) {
            imageRefIso.disposeLocal();
        } else {

            if ((transform != null) && (transform.getTransformedImage() != null)) {
                transform.getTransformedImage().disposeLocal();
                transform = null;
            }
        }

        if (imageInputIso != null) {
            imageInputIso.disposeLocal();
        } else {

            if ((transform2 != null) && (transform2.getTransformedImage() != null)) {
                transform2.getTransformedImage().disposeLocal();
                transform2 = null;
            }
        }

        if (imageInputIso != null) {
            imageInputIso.disposeLocal();
        }

        refImage = null;
        inputImage = null;

        if (transform != null) {
            transform.disposeLocal();
        }

        if (transform2 != null) {
            transform2.disposeLocal();
        }

        System.gc();
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * Accessor that returns the final cost function.
     *
     * @return  Matrix found at the end of algorithm.
     */
    public double getAnswer() {
        return answer.cost;
    }

    /**
     * Access that returns an array containing the transformation parameters.
     *
     * @return  transformation array (0-2 rot, 3-5 trans, 6-9 scale, 9-12 skew)
     */
    public double[] getTransArray() {
        return answer.initial;
    }

    /**
     * Accessor that returns the matrix calculated in this algorithm.
     *
     * @return  Matrix found at the end of algorithm.
     */
    public TransMatrix getTransform() {
        TransMatrixd tMatd = answer.matrix;
        TransMatrix tMat = new TransMatrix(tMatd.getDim(), tMatd.getID(), tMatd.isNIFTI(), tMatd.isQform());
        for (int i = 0; i < tMatd.getDim(); i++) {
        	for (int j = 0; j < tMatd.getDim(); j++) {
        		tMat.set(i, j, tMatd.get(i, j));
        	}
        }
        return tMat;
    }

    /**
     * Accessor that returns the matrix calculated in this algorithm divided by 2.
     *
     * @return  Matrix found at the end of algorithm with the compoents halved.
     */
    public TransMatrix getTransformHalf() {
        TransMatrixd tMatd = answer.halfMatrix;
        TransMatrix tMat = new TransMatrix(tMatd.getDim(), tMatd.getID(), tMatd.isNIFTI(), tMatd.isQform());
        for (int i = 0; i < tMatd.getDim(); i++) {
        	for (int j = 0; j < tMatd.getDim(); j++) {
        		tMat.set(i, j, tMatd.get(i, j));
        	}
        }
        return tMat;
    }

    /**
     * Accessor that returns the z rot and x and y trans from the matrix calculated in this algorithm.
     *
     * @return  z rotation and x and y translations from the matrix found at the end of algorithm.
     */
    public TransMatrix getTransformMigsagittal() {
        TransMatrixd tMatd = answer.midsagMatrix;
        TransMatrix tMat = new TransMatrix(tMatd.getDim(), tMatd.getID(), tMatd.isNIFTI(), tMatd.isQform());
        for (int i = 0; i < tMatd.getDim(); i++) {
        	for (int j = 0; j < tMatd.getDim(); j++) {
        		tMat.set(i, j, tMatd.get(i, j));
        	}
        }
        return tMat;
    }

    /**
     * Runs the image registration. Blurs the images based on what their minimum resolutions are. The reference image is
     * blurred if one of the input image resolutions is 50% or more bigger than the corresponding resolution in the
     * reference image; likewise, the input image is blurred if one of the reference resolutions is 50% or more bigger
     * than the corresponding resolution in the input image. Thus, it is unlikely, though not impossible, that both
     * images will be blurred. The images are then transformed into isotropic voxels. The resolutions of the two images
     * after the isotropic transformation will be the same in all dimensions. If maxResol is true, that resolution will
     * equal the maximum of the minimums of each image's resolutions: Max( Min (resolutions of ref image, resolutions of
     * input image) ). If the images are weighted, the weight images are blurred and transformed into isotropic voxels
     * in the same manner as the originals. Then the images are subsampled by 2, 4, and 8. If the images are too small
     * they will not be subsampled down to the smallest level; if they are too big, they will be subsampled to 16. The
     * same is done with the weight images if necessary. The function levelEight is called with the images subsampled by
     * 8; it returns two vectors with minima. Then the function levelFour is called with images subsampled by 4 and the
     * two vectors; it returns one vector of minima. The function levelTwo is called with images subsampled by 2 and the
     * vector; it returns an "answer" in the form of a MatrixListItem, which is a convenient way of storing the point,
     * the matrix, and the cost of the minimum. Then the function levelOne is called with the minimum; it returns a
     * final "answer", or minimum, which will then be accessed by the dialog that called this algorithm.
     */
    public void runAlgorithm() {
        //long startTime = System.currentTimeMillis();
        int i;

        if (refImage.getNDims() != 3) {
            MipavUtil.displayError("" + refImage.getNDims() + "D registration not supported.");
            disposeLocal();
            setCompleted(false);

            return;
        }

        if (inputImage.getNDims() != 3) {
            MipavUtil.displayError("" + inputImage.getNDims() + "D registration not supported.");
            disposeLocal();
            setCompleted(false);

            return;
        }

        float minSampleRef = 1.0f;
        float minSampleInput = 1.0f;
        float minSample = 1.0f;

        minSampleRef = Math.min(resRef[0], Math.min(resRef[1], resRef[2]));
        minSampleInput = Math.min(resInput[0], Math.min(resInput[1], resInput[2]));

        if (maxResol) {

            // max of the min resolutions of the two datasets
            minSample = Math.max(minSampleRef, minSampleInput);
        } else {

            // min of the min resolutions of the two datasets
            minSample = Math.min(minSampleRef, minSampleInput);
        }

        if ((resRef[0] == resRef[1]) && (resRef[0] == resRef[2]) && (resRef[0] == minSample)) {
            resampleRef = false;
        } else {
            resampleRef = true;
        }

        if ((resInput[0] == resInput[1]) && (resInput[0] == resInput[2]) && (resInput[0] == minSample)) {
            resampleInput = false;
        } else {
            resampleInput = true;
        }

        // Establish minimum allowable resolutions.  Don't want the subsampled
        // image resolutions to be so low that a five pixel translation will be
        // "out of bounds".
        double maxPixSizeX, maxPixSizeY, maxPixSizeZ;
        maxPixSizeX = (limits_mm[1][0] - limits_mm[0][0]) / 10.;
        maxPixSizeY = (limits_mm[1][1] - limits_mm[0][1]) / 10.;
        maxPixSizeZ = (limits_mm[1][2] - limits_mm[0][2]) / 10.;

        if (((resRef[0] * 2.) > maxPixSizeX) || ((resRef[1] * 2.) > maxPixSizeY)) {
            allowLevel2XY = false;
        }

        if ((resRef[2] * 2.) > maxPixSizeZ) {
            allowLevel2Z = false;
        }

        if (!allowLevel2XY || !allowLevel2Z) {
            Preferences.debug("Image resolution is too low to subsample for level 2, given " +
                              "the prescribed translation limits. \n",Preferences.DEBUG_ALGORITHM);
        }

        if (((resRef[0] * 4.) > maxPixSizeX) || ((resRef[1] * 4.) > maxPixSizeY)) {
            allowLevel4XY = false;
        }

        if ((resRef[2] * 4.) > maxPixSizeZ) {
            allowLevel4Z = false;
        }

        if (!allowLevel4XY || !allowLevel4Z) {
            Preferences.debug("Image resolution is too low to subsample for level 4, given " +
                              "the prescribed translation limits. \n",Preferences.DEBUG_ALGORITHM);
        }

        if (((resRef[0] * 8.) > maxPixSizeX) || ((resRef[1] * 8.) > maxPixSizeY)) {
            allowLevel8XY = false;
        }

        if ((resRef[2] * 8.) > maxPixSizeZ) {
            allowLevel8Z = false;
        }

        if (!allowLevel8XY || !allowLevel8Z) {
            Preferences.debug("Image resolution is too low to subsample for level 8, given " +
                              "the prescribed translation limits. \n",Preferences.DEBUG_ALGORITHM);
        }

        if (((resRef[0] * 16.) > maxPixSizeX) || ((resRef[1] * 16.) > maxPixSizeY)) {
            allowLevel16XY = false;
        }

        if ((resRef[2] * 16.) > maxPixSizeZ) {
            allowLevel16Z = false;
        }

        if (!allowLevel16XY || !allowLevel16Z) {
            Preferences.debug("Image resolution is too low to subsample for level 16, given " +
                              "the prescribed translation limits. \n",Preferences.DEBUG_ALGORITHM);
        }


        int[] extentsRefIso = null;
        float[] resRefIso = null;
        int[] extentsInputIso = null;
        float[] resInputIso = null;
        float[] sigmasRef = null;
        float[] sigmasInput = null;

        try {
            extentsRefIso = new int[3];
            resRefIso = new float[3];
            extentsInputIso = new int[3];
            resInputIso = new float[3];
            sigmasRef = new float[3];
            sigmasInput = new float[3];
        } catch (OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("Out of memory in AlgorithmOAR3D.");
            disposeLocal();
            setCompleted(false);

            return;
        }

        for (i = 0; i < extentsRefIso.length; i++) {
            extentsRefIso[i] = (int) ((refImage.getExtents()[i] - 1) / (minSample / resRef[i])) + 1;
            resRefIso[i] = minSample;
            extentsInputIso[i] = (int) ((inputImage.getExtents()[i] - 1) / (minSample / resInput[i])) + 1;
            resInputIso[i] = minSample;
        }

        boolean blurRef = false;
        boolean blurInput = false;

        for (i = 0; i < sigmasRef.length; i++) {
            sigmasRef[i] = ((resInput[i] - resRef[i]) / resRef[i]) * 0.6f; // 2.36  * sigma = x -->  FWHM  0.424 =
                                                                           // 1/2.36
            //System.out.println(" sigmasRef[" + i + " ] = " + sigmasRef[i]);

            if (sigmasRef[i] < 0.5f) {
                sigmasRef[i] = 0.0f;
            } else {
                blurRef = true;
            }

            if (i == 2) {
                sigmasRef[2] = sigmasRef[2] * (resRef[0] / resRef[2]);
            }

            sigmasInput[i] = ((resRef[i] - resInput[i]) / resInput[i]) * 0.6f; // 2.36  * sigma = x -->  FWHM  0.424 =
                                                                               // 1/2.36

            if (sigmasInput[i] < 0.5f) {
                sigmasInput[i] = 0.0f;
            } else {
                blurInput = true;
            }

            if (i == 2) {
                sigmasInput[2] = sigmasInput[2] * (resInput[0] / resInput[2]);
            }

        }

        Preferences.debug("Blurring ref = " + sigmasRef[0] + ", " + sigmasRef[1] + ", " + sigmasRef[2] + "\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Blurring inp = " + sigmasInput[0] + ", " + sigmasInput[1] + ", " + sigmasInput[2] + "\n",
        		Preferences.DEBUG_ALGORITHM);
        
        Preferences.debug(getConstructionInfo(),Preferences.DEBUG_ALGORITHM);

        if (blurRef) {

            if (doColor) {
                blurredRef = new ModelImage(ModelImage.ARGB_FLOAT, refImage.getExtents(), "BlurRef");
            } else {
                blurredRef = new ModelImage(ModelImage.FLOAT, refImage.getExtents(), "BlurRef");
            }

            // update resolutions
            FileInfoBase[] fileInfo = blurredRef.getFileInfo();

            for (i = 0; i < refImage.getExtents()[2]; i++) {
                fileInfo[i].setResolutions(refImage.getResolutions(i));
            }

            AlgorithmGaussianBlur blur = new AlgorithmGaussianBlur(blurredRef, refImage, sigmasRef, true, false);

            if (doColor) {
                blur.setRed(true);
                blur.setGreen(true);
                blur.setBlue(true);
            }

            blur.setRunningInSeparateThread(runningInSeparateThread);
            blur.run();

            if (blur.isCompleted() == false) {
                setCompleted(false);
                finalize();

                return;
            }
        } else {
            blurredRef = refImage;
        }

        if (blurInput) {

            if (doColor) {
                blurredInput = new ModelImage(ModelImage.ARGB_FLOAT, inputImage.getExtents(), "BlurInput");
            } else {
                blurredInput = new ModelImage(ModelImage.FLOAT, inputImage.getExtents(), "BlurInput");
            }

            // update resolutions
            FileInfoBase[] fileInfo = blurredInput.getFileInfo();

            for (i = 0; i < inputImage.getExtents()[2]; i++) {
                fileInfo[i].setResolutions(inputImage.getFileInfo()[i].getResolutions());
            }

            AlgorithmGaussianBlur blur2 = new AlgorithmGaussianBlur(blurredInput, inputImage, sigmasInput, true, false);

            if (doColor) {
                blur2.setRed(true);
                blur2.setGreen(true);
                blur2.setBlue(true);
            }

            blur2.setRunningInSeparateThread(runningInSeparateThread);
            blur2.run();

            if (blur2.isCompleted() == false) {
                setCompleted(false);
                finalize();

                return;
            }
        } else {
            blurredInput = inputImage;
        }

        // Resample blurred image of reference image into isotropic voxels

        if (resampleRef) {
            transform = new AlgorithmTransform(blurredRef, new TransMatrix(4), interp, resRefIso[0], resRefIso[1],
                                               resRefIso[2], extentsRefIso[0], extentsRefIso[1], extentsRefIso[2],
                                               false, true, false);
            transform.setRunningInSeparateThread(runningInSeparateThread);
            transform.run();

            if (transform.isCompleted() == false) {
                transform.finalize();
                transform = null;
                setCompleted(false);
                finalize();

                return;
            }

            if ((blurredRef != refImage) && (blurredRef != null)) {
                blurredRef.disposeLocal();
            } else {
                blurredRef = null;
            }

            System.gc();

            imageRefIso = transform.getTransformedImage();

            if (transform != null) {
                transform.finalize();
            }

            simpleRef = new ModelSimpleImage(imageRefIso.getExtents(), imageRefIso.getFileInfo(0).getResolutions(),
                                             imageRefIso);
        } // if (resampleRef)
        else {
            simpleRef = new ModelSimpleImage(blurredRef.getExtents(), blurredRef.getFileInfo(0).getResolutions(),
                                             blurredRef);
        }

        maxDim = simpleRef.xDim;

        if (simpleRef.yDim > maxDim) {
            maxDim = simpleRef.yDim;
        }

        if (simpleRef.zDim > maxDim) {
            maxDim = simpleRef.zDim;
        }

        if ((weighted) &&
                ((resampleRef) ||
                     ((refWeight.getFileInfo(0).getResolutions()[0] != refImage.getFileInfo(0).getResolutions()[0]) ||
                          (refWeight.getFileInfo(0).getResolutions()[1] !=
                               refImage.getFileInfo(0).getResolutions()[1]) ||
                          (refWeight.getFileInfo(0).getResolutions()[2] !=
                               refImage.getFileInfo(0).getResolutions()[2])))) {
            transform = new AlgorithmTransform(refWeight, new TransMatrix(4), interp, resRefIso[0], resRefIso[1],
                                               resRefIso[2], extentsRefIso[0], extentsRefIso[1], extentsRefIso[2],
                                               false, true, false);
            transform.setRunningInSeparateThread(runningInSeparateThread);
            transform.run();

            if (transform.isCompleted() == false) {
                transform.finalize();
                transform = null;
                setCompleted(false);
                finalize();

                return;
            }

            imageWeightRefIso = transform.getTransformedImage();

            if (transform != null) {
                transform.finalize();
            }

            simpleWeightRef = new ModelSimpleImage(imageWeightRefIso.getExtents(),
                                                   imageWeightRefIso.getFileInfo(0).getResolutions(),
                                                   imageWeightRefIso);

            if (imageWeightRefIso != null) {
                imageWeightRefIso.disposeLocal();
            }
        } else if (weighted) {
            simpleWeightRef = new ModelSimpleImage(refWeight.getExtents(), refWeight.getFileInfo(0).getResolutions(),
                                                   refWeight);
        }

        if (imageRefIso != null) {
            imageRefIso.disposeLocal();
        }

        if (transform != null) {

            if (transform.getTransformedImage() != null) {
                transform.getTransformedImage().disposeLocal();
            }

            transform.disposeLocal();
            transform = null;
        }

        System.gc();

        if (resampleInput) {
            transform2 = new AlgorithmTransform(blurredInput, new TransMatrix(4), interp, resInputIso[0],
                                                resInputIso[1], resInputIso[2], extentsInputIso[0], extentsInputIso[1],
                                                extentsInputIso[2], false, true, false);
            transform2.setRunningInSeparateThread(runningInSeparateThread);
            transform2.run();

            if (transform2.isCompleted() == false) {
                transform2.finalize();
                transform2 = null;
                setCompleted(false);
                finalize();

                return;
            }

            if ((blurredInput != inputImage) && (blurredInput != null)) {
                blurredInput.disposeLocal();
            } else {
                blurredInput = null;
            }

            System.gc();

            imageInputIso = transform2.getTransformedImage();

            if (transform2 != null) {
                transform2.finalize();
            }

            simpleInput = new ModelSimpleImage(imageInputIso.getExtents(),
                                               imageInputIso.getFileInfo(0).getResolutions(), imageInputIso);
        } // if (resampleInput)
        else {
            simpleInput = new ModelSimpleImage(blurredInput.getExtents(), blurredInput.getFileInfo(0).getResolutions(),
                                               blurredInput);
        }

        if (simpleInput.xDim > maxDim) {
            maxDim = simpleInput.xDim;
        }

        if (simpleInput.yDim > maxDim) {
            maxDim = simpleInput.yDim;
        }

        if (simpleInput.zDim > maxDim) {
            maxDim = simpleInput.zDim;
        }

        if ((weighted) &&
                ((resampleInput) ||
                     ((inputWeight.getFileInfo(0).getResolutions()[0] !=
                           inputImage.getFileInfo(0).getResolutions()[0]) ||
                          (inputWeight.getFileInfo(0).getResolutions()[1] !=
                               inputImage.getFileInfo(0).getResolutions()[1]) ||
                          (inputWeight.getFileInfo(0).getResolutions()[2] !=
                               inputImage.getFileInfo(0).getResolutions()[2])))) {
            transform2 = new AlgorithmTransform(inputWeight, new TransMatrix(4), interp, resInputIso[0], resInputIso[1],
                                                resInputIso[2], extentsInputIso[0], extentsInputIso[1],
                                                extentsInputIso[2], false, true, false);
            transform2.setRunningInSeparateThread(runningInSeparateThread);
            transform2.run();

            if (transform2.isCompleted() == false) {
                transform2.finalize();
                transform2 = null;
                setCompleted(false);
                finalize();

                return;
            }

            imageWeightInputIso = transform2.getTransformedImage();

            if (transform2 != null) {
                transform2.finalize();
            }

            simpleWeightInput = new ModelSimpleImage(imageWeightInputIso.getExtents(),
                                                     imageWeightInputIso.getFileInfo(0).getResolutions(),
                                                     imageWeightInputIso);

            if (imageWeightInputIso != null) {
                imageWeightInputIso.disposeLocal();
            }
        } else if (weighted) {
            simpleWeightInput = new ModelSimpleImage(inputWeight.getExtents(),
                                                     inputWeight.getFileInfo(0).getResolutions(), inputWeight);
        }

        if (imageInputIso != null) {
            imageInputIso.disposeLocal();
        }

        if (transform2 != null) {

            if (transform2.getTransformedImage() != null) {
                transform2.getTransformedImage().disposeLocal();
            }

            transform2.disposeLocal();
            transform2 = null;
        }

        System.gc();

        int subMinFactor = 75000;
        long time = System.currentTimeMillis();

        if (doColor) {
            subMinFactor *= 4;
        }

        fireProgressStateChanged("Registering images", "Beginning registration");


        if (weighted) {

            for (i = 0; i < simpleWeightRef.dataSize; i++) {

                if (simpleWeightRef.data[i] > 0) {
                    weightedRefPixels++;
                }
            }

            for (i = 0; i < simpleWeightInput.dataSize; i++) {

                if (simpleWeightInput.data[i] > 0) {
                    weightedInputPixels++;
                }
            }

            if ((weightedRefPixels > subMinFactor) && (weightedInputPixels > subMinFactor) &&
                    (simpleWeightRef.zDim >= minimumZForSub) && (simpleWeightInput.zDim >= minimumZForSub) &&
                    doSubsample && allowLevel2XY && allowLevel2Z) {
                simpleWeightRefSub2 = subsampleBy2(simpleWeightRef, false);
                simpleWeightInputSub2 = subsampleBy2(simpleWeightInput, false);
            } else if ((weightedRefPixels > subMinFactor) && (weightedInputPixels > subMinFactor) && doSubsample &&
                           allowLevel2XY) {
                simpleWeightRefSub2 = subsampleBy2XY(simpleWeightRef, false);
                simpleWeightInputSub2 = subsampleBy2XY(simpleWeightInput, false);
                allowLevel2Z = false;
            } else {
                simpleWeightRefSub2 = simpleWeightRef;
                simpleWeightInputSub2 = simpleWeightInput;
                allowLevel2XY = false;
                allowLevel2Z = false;
            }
        }

        if (weighted && fullAnalysisMode) { // If in fast mode, skip resampling because won't do levelEight, levelFour
                                            // and levelTwo

            for (i = 0; i < simpleWeightRefSub2.dataSize; i++) {

                if (simpleWeightRefSub2.data[i] > 0) {
                    weightedRefPixelsSub2++;
                }
            }

            for (i = 0; i < simpleWeightInputSub2.dataSize; i++) {

                if (simpleWeightInputSub2.data[i] > 0) {
                    weightedInputPixelsSub2++;
                }
            }

            if ((weightedRefPixelsSub2 > subMinFactor) && (weightedInputPixelsSub2 > subMinFactor) &&
                    (simpleWeightRefSub2.zDim >= minimumZForSub) && (simpleWeightInputSub2.zDim >= minimumZForSub) &&
                    doSubsample && allowLevel4XY && allowLevel4Z) {
                simpleWeightRefSub4 = subsampleBy2(simpleWeightRefSub2, false);
                simpleWeightInputSub4 = subsampleBy2(simpleWeightInputSub2, false);
            } else if ((weightedRefPixelsSub2 > subMinFactor) && (weightedInputPixelsSub2 > subMinFactor) &&
                           doSubsample && allowLevel4XY) {
                simpleWeightRefSub4 = subsampleBy2XY(simpleWeightRefSub2, false);
                simpleWeightInputSub4 = subsampleBy2XY(simpleWeightInputSub2, false);
                allowLevel4Z = false;
            } else {
                simpleWeightRefSub4 = simpleWeightRefSub2;
                simpleWeightInputSub4 = simpleWeightInputSub2;
                allowLevel4XY = false;
                allowLevel4Z = false;
            }

            for (i = 0; i < simpleWeightRefSub4.dataSize; i++) {

                if (simpleWeightRefSub4.data[i] > 0) {
                    weightedRefPixelsSub4++;
                }
            }

            for (i = 0; i < simpleWeightInputSub4.dataSize; i++) {

                if (simpleWeightInputSub4.data[i] > 0) {
                    weightedInputPixelsSub4++;
                }
            }

            if ((weightedRefPixelsSub4 > subMinFactor) && (weightedInputPixelsSub4 > subMinFactor) &&
                    (simpleWeightRefSub4.zDim >= minimumZForSub) && (simpleWeightInputSub4.zDim >= minimumZForSub) &&
                    doSubsample && allowLevel8XY && allowLevel8Z) {
                simpleWeightRefSub8 = subsampleBy2(simpleWeightRefSub4, false);
                simpleWeightInputSub8 = subsampleBy2(simpleWeightInputSub4, false);

                // For really big images subsample level 8 again!
                for (i = 0; i < simpleWeightRefSub8.dataSize; i++) {

                    if (simpleWeightRefSub8.data[i] > 0) {
                        weightedRefPixelsSub8++;
                    }
                }

                for (i = 0; i < simpleWeightInputSub8.dataSize; i++) {

                    if (simpleWeightInputSub8.data[i] > 0) {
                        weightedInputPixelsSub8++;
                    }
                }

                if ((weightedRefPixelsSub8 > subMinFactor) && (weightedInputPixelsSub8 > subMinFactor) &&
                        (simpleWeightRefSub8.zDim >= minimumZForSub) &&
                        (simpleWeightInputSub8.zDim >= minimumZForSub) && doSubsample && allowLevel16XY &&
                        allowLevel16Z) {
                    ModelSimpleImage simpleWeightRefSub16;
                    ModelSimpleImage simpleWeightInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16  ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleWeightRefSub16 = subsampleBy2(simpleWeightRefSub8, false);
                    simpleWeightInputSub16 = subsampleBy2(simpleWeightInputSub8, false);

                    simpleWeightRefSub8 = simpleWeightRefSub16;
                    simpleWeightInputSub8 = simpleWeightInputSub16;
                } else if ((weightedRefPixelsSub8 > subMinFactor) && (weightedInputPixelsSub8 > subMinFactor) &&
                               doSubsample && allowLevel16XY) {
                    ModelSimpleImage simpleWeightRefSub16;
                    ModelSimpleImage simpleWeightInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleWeightRefSub16 = subsampleBy2XY(simpleWeightRefSub8, false);
                    simpleWeightInputSub16 = subsampleBy2XY(simpleWeightInputSub8, false);

                    simpleWeightRefSub8 = simpleWeightRefSub16;
                    simpleWeightInputSub8 = simpleWeightInputSub16;
                    allowLevel16Z = false;
                } else {
                    allowLevel16XY = false;
                    allowLevel16Z = false;
                }
            } else if ((weightedRefPixelsSub4 > subMinFactor) && (weightedInputPixelsSub4 > subMinFactor) &&
                           doSubsample && allowLevel8XY) {
                simpleWeightRefSub8 = subsampleBy2XY(simpleWeightRefSub4, false);
                simpleWeightInputSub8 = subsampleBy2XY(simpleWeightInputSub4, false);
                allowLevel8Z = false;

                // For really big images subsample level 8 again!
                for (i = 0; i < simpleWeightRefSub8.dataSize; i++) {

                    if (simpleWeightRefSub8.data[i] > 0) {
                        weightedRefPixelsSub8++;
                    }
                }

                for (i = 0; i < simpleWeightInputSub8.dataSize; i++) {

                    if (simpleWeightInputSub8.data[i] > 0) {
                        weightedInputPixelsSub8++;
                    }
                }

                if ((weightedRefPixelsSub8 > subMinFactor) && (weightedInputPixelsSub8 > subMinFactor) && doSubsample &&
                        allowLevel16XY) {
                    ModelSimpleImage simpleWeightRefSub16;
                    ModelSimpleImage simpleWeightInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleWeightRefSub16 = subsampleBy2XY(simpleWeightRefSub8, false);
                    simpleWeightInputSub16 = subsampleBy2XY(simpleWeightInputSub8, false);

                    simpleWeightRefSub8 = simpleWeightRefSub16;
                    simpleWeightInputSub8 = simpleWeightInputSub16;
                } else {
                    allowLevel16XY = false;
                }
            } else {
                simpleWeightRefSub8 = simpleWeightRefSub4;
                simpleWeightInputSub8 = simpleWeightInputSub4;
                allowLevel8XY = false;
                allowLevel8Z = false;
            }

            Preferences.debug("Weighted ref subsampled 2 = " + simpleWeightRefSub2 + "Weighted ref subsampled 4 = " +
                              simpleWeightRefSub4 + "Weighted ref subsampled 8 = " + simpleWeightRefSub8 +
                              "Weighted input subsampled 2 = " + simpleWeightInputSub2 +
                              "Weighted input subsampled 4 = " + simpleWeightInputSub4 +
                              "Weighted input subsampled 8 = " + simpleWeightInputSub8,Preferences.DEBUG_ALGORITHM);

        } // end of (if weighted && fullAnalysisMode)

        if (fullAnalysisMode) {

            // Level two subsampling?
            if ((simpleRef.dataSize > subMinFactor) && (simpleInput.dataSize > subMinFactor) && allowLevel2XY &&
                    allowLevel2Z && (simpleRef.zDim >= minimumZForSub) && (simpleInput.zDim >= minimumZForSub) &&
                    doSubsample) {
                simpleRefSub2 = subsampleBy2(simpleRef, doColor);
                simpleInputSub2 = subsampleBy2(simpleInput, doColor);
                level1FactorXY = 2.0f;
                level1FactorZ = 2.0f;
            } else if ((simpleRef.dataSize > subMinFactor) && (simpleInput.dataSize > subMinFactor) && allowLevel2XY &&
                           doSubsample) {
                simpleRefSub2 = subsampleBy2XY(simpleRef, doColor);
                simpleInputSub2 = subsampleBy2XY(simpleInput, doColor);
                level1FactorXY = 2.0f;
            } else {
                Preferences.debug("Level one image not resampled because ",Preferences.DEBUG_ALGORITHM);

                if (simpleRef.dataSize <= subMinFactor) {
                    Preferences.debug("reference image data size is too small. \n",Preferences.DEBUG_ALGORITHM);
                } else {

                    if (simpleInput.dataSize <= subMinFactor) {
                        Preferences.debug("input image data size is too small. \n",Preferences.DEBUG_ALGORITHM);
                    } else {
                        Preferences.debug("resolution too low for user defined translation limits. \n",
                        		Preferences.DEBUG_ALGORITHM);
                    }
                }

                simpleRefSub2 = simpleRef;
                simpleInputSub2 = simpleInput;
            }

            // Level four subsampling?
            if ((simpleRefSub2.dataSize > subMinFactor) && (simpleInputSub2.dataSize > subMinFactor) && allowLevel4XY &&
                    allowLevel4Z && (simpleRefSub2.zDim >= minimumZForSub) &&
                    (simpleInputSub2.zDim >= minimumZForSub) && doSubsample) {
                simpleRefSub4 = subsampleBy2(simpleRefSub2, doColor);
                simpleInputSub4 = subsampleBy2(simpleInputSub2, doColor);
                level2FactorXY = 2.0f;
                level2FactorZ = 2.0f;
            } else if ((simpleRefSub2.dataSize > subMinFactor) && (simpleInputSub2.dataSize > subMinFactor) &&
                           allowLevel4XY && doSubsample) {
                simpleRefSub4 = subsampleBy2XY(simpleRefSub2, doColor);
                simpleInputSub4 = subsampleBy2XY(simpleInputSub2, doColor);
                level2FactorXY = 2.0f;
            } else {
                Preferences.debug("Level two image not resampled because ",Preferences.DEBUG_ALGORITHM);

                if (simpleRefSub2.dataSize <= subMinFactor) {
                    Preferences.debug("reference image data size is too small. \n",Preferences.DEBUG_ALGORITHM);
                } else {

                    if (simpleInputSub2.dataSize <= subMinFactor) {
                        Preferences.debug("input image data size is too small. \n",Preferences.DEBUG_ALGORITHM);
                    } else {
                        Preferences.debug("resolution too low for user defined translation limits. \n",
                        		Preferences.DEBUG_ALGORITHM);
                    }
                }

                simpleRefSub4 = simpleRefSub2;
                simpleInputSub4 = simpleInputSub2;
            }

            // Level eight subsampling?
            if ((simpleRefSub4.dataSize > subMinFactor) && (simpleInputSub4.dataSize > subMinFactor) && allowLevel8XY &&
                    allowLevel8Z && (simpleRefSub4.zDim >= minimumZForSub) &&
                    (simpleInputSub4.zDim >= minimumZForSub) && doSubsample) {
                simpleRefSub8 = subsampleBy2(simpleRefSub4, doColor);
                simpleInputSub8 = subsampleBy2(simpleInputSub4, doColor);
                level4FactorXY = 2.0f;
                level4FactorZ = 2.0f;

                // For really big images subsample level 8 again!
                if ((simpleRefSub8.dataSize > subMinFactor) && (simpleInputSub8.dataSize > subMinFactor) &&
                        allowLevel16XY && allowLevel16Z && (simpleRefSub8.zDim >= minimumZForSub) &&
                        (simpleInputSub8.zDim >= minimumZForSub)) {
                    ModelSimpleImage simpleRefSub16;
                    ModelSimpleImage simpleInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16  ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleRefSub16 = subsampleBy2(simpleRefSub8, doColor);
                    simpleInputSub16 = subsampleBy2(simpleInputSub8, doColor);

                    simpleRefSub8 = simpleRefSub16;
                    simpleInputSub8 = simpleInputSub16;
                    level4FactorXY = 4.0f;
                    level4FactorZ = 4.0f;
                } else if ((simpleRefSub8.dataSize > subMinFactor) && (simpleInputSub8.dataSize > subMinFactor) &&
                               allowLevel16XY) {
                    ModelSimpleImage simpleRefSub16;
                    ModelSimpleImage simpleInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleRefSub16 = subsampleBy2XY(simpleRefSub8, doColor);
                    simpleInputSub16 = subsampleBy2XY(simpleInputSub8, doColor);

                    simpleRefSub8 = simpleRefSub16;
                    simpleInputSub8 = simpleInputSub16;
                    level4FactorXY = 4.0f;
                }
            } else if ((simpleRefSub4.dataSize > subMinFactor) && (simpleInputSub4.dataSize > subMinFactor) &&
                           allowLevel8XY && doSubsample) {
                simpleRefSub8 = subsampleBy2XY(simpleRefSub4, doColor);
                simpleInputSub8 = subsampleBy2XY(simpleInputSub4, doColor);
                level4FactorXY = 2.0f;

                // For really big images subsample level 8 again!
                if ((simpleRefSub8.dataSize > subMinFactor) && (simpleInputSub8.dataSize > subMinFactor) &&
                        allowLevel16XY) {
                    ModelSimpleImage simpleRefSub16;
                    ModelSimpleImage simpleInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleRefSub16 = subsampleBy2XY(simpleRefSub8, doColor);
                    simpleInputSub16 = subsampleBy2XY(simpleInputSub8, doColor);

                    simpleRefSub8 = simpleRefSub16;
                    simpleInputSub8 = simpleInputSub16;
                    level4FactorXY = 4.0f;
                }
            } else {
                simpleRefSub8 = simpleRefSub4;
                simpleInputSub8 = simpleInputSub4;
            }

            Preferences.debug("Level 1 factor XY = " + level1FactorXY + "\n" + "Level 1 factor Z = " + level1FactorZ +
                              "\n" + "Level 2 factor XY = " + level2FactorXY + "\n" + "Level 2 factor Z = " +
                              level2FactorZ + "\n" + "Level 4 factor XY = " + level4FactorXY + "\n" +
                              "Level 4 factor Z = " + level4FactorZ + "\n" + "Ref subsampled 2 = " + simpleRefSub2 +
                              "Ref subsampled 4 = " + simpleRefSub4 + "Ref subsampled 8 = " + simpleRefSub8 +
                              "Input subsampled 2 = " + simpleInputSub2 + "Input subsampled 4 = " + simpleInputSub4 +
                              "Input subsampled 8 = " + simpleInputSub8,Preferences.DEBUG_ALGORITHM);

            // STARTING LEVEL 8

            // Calculate translation limits at each level since they must be updated
            // for the current image resolution.
            float multXY, multZ;
            multXY = level1FactorXY * level2FactorXY * level4FactorXY;
            multZ = level1FactorZ * level2FactorZ * level4FactorZ;
            Preferences.debug("multxy " + multXY + ".\n",Preferences.DEBUG_ALGORITHM);

            for (int j = 0; j <= 1; j++) {
                limits[j][3] = limits_mm[j][0] / (resInput[0] * multXY);
                limits[j][4] = limits_mm[j][1] / (resInput[1] * multXY);
                limits[j][5] = limits_mm[j][2] / (resInput[2] * multZ);
            }

            time = System.currentTimeMillis();
            Preferences.debug(" Starting level 8 ************************************************\n",
            		Preferences.DEBUG_ALGORITHM);

            Vector<MatrixListItem>[] minimas = levelEight(simpleRefSub8, simpleInputSub8);

            // "minimas" is an array of Vector, because it will have two Vectors - one with
            // the original minima and one with the optimized minima.
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 8 minutes = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }

            // STARTING LEVEL 4

            // Calculate translation limits at each level since they must be updated
            // for the current image resolution.
            multXY = level1FactorXY * level2FactorXY;
            multZ = level1FactorZ * level2FactorZ;
            Preferences.debug("multxy " + multXY + ".\n",Preferences.DEBUG_ALGORITHM);

            for (int j = 0; j <= 1; j++) {
                limits[j][3] = limits_mm[j][0] / (resInput[0] * multXY);
                limits[j][4] = limits_mm[j][1] / (resInput[1] * multXY);
                limits[j][5] = limits_mm[j][2] / (resInput[2] * multZ);
            }

            Preferences.debug(" Starting level 4 ************************************************\n",Preferences.DEBUG_ALGORITHM);

            Vector<MatrixListItem> minima = levelFour(simpleRefSub4, simpleInputSub4, minimas[0], minimas[1]);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 4  minutes = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }

            // STARTING LEVEL 2

            // Calculate translation limits at each level since they must be updated
            // for the current image resolution.
            multXY = level1FactorXY;
            multZ = level1FactorZ;
            Preferences.debug("multxy " + multXY + ".\n",Preferences.DEBUG_ALGORITHM);

            for (int j = 0; j <= 1; j++) {
                limits[j][3] = limits_mm[j][0] / (resInput[0] * multXY);
                limits[j][4] = limits_mm[j][1] / (resInput[1] * multXY);
                limits[j][5] = limits_mm[j][2] / (resInput[2] * multZ);
            }

            Preferences.debug(" Starting level 2 ************************************************\n",
            		Preferences.DEBUG_ALGORITHM);
            bestGuessLevel2 = levelTwo(simpleRefSub2, simpleInputSub2, minima);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 2 minutes = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }
        } // end of if (fullAnalysisMode)
        else { // if (fastMode) setup item to be to "first best guess"  = identity

            double[] initial = new double[12];
            bestGuessLevel2 = new MatrixListItem(0, new TransMatrixd(4), initial);

            double diffX = 0;
            double diffY = 0;
            double diffZ = 0;

            if (calcCOG == true) {
                Vector3f cog = calculateCenterOfMass3D(simpleInput, simpleWeightInput, doColor);
                Vector3f cogR = calculateCenterOfMass3D(simpleRef, simpleWeightRef, doColor);
                Preferences.debug("Center of mass for the subsampled input image:" + cog + "\n",Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Center of mass for the subsampled reference image:" + cogR + "\n",
                		Preferences.DEBUG_ALGORITHM);

                diffX = (cog.X - cogR.X);
                diffY = (cog.Y - cogR.Y);
                diffZ = (cog.Z - cogR.Z);
            }

            bestGuessLevel2.initial[0] = bestGuessLevel2.initial[1] = bestGuessLevel2.initial[2] = 0; // initial rotation
            bestGuessLevel2.initial[3] = diffX; // initial translations
            bestGuessLevel2.initial[4] = diffY;
            bestGuessLevel2.initial[5] = diffZ;
            bestGuessLevel2.initial[6] = bestGuessLevel2.initial[7] = bestGuessLevel2.initial[8] = 1; // initial scaling
            bestGuessLevel2.initial[9] = bestGuessLevel2.initial[10] = bestGuessLevel2.initial[11] = 0; // initial skewing
        } // end of (fastMode)

        // STARTING LEVEL 0NE - note - this is for fastMode and fullAnalysisMode

        // Calculate translation limits at each level since they must be updated
        // for the current image resolution.  Here the resolution is the original
        // image resolution.
        for (int j = 0; j <= 1; j++) {
            limits[j][3] = limits_mm[j][0] / resInput[0];
            limits[j][4] = limits_mm[j][1] / resInput[1];
            limits[j][5] = limits_mm[j][2] / resInput[2];
        }

        Preferences.debug(" Starting level 1 ************************************************\n",Preferences.DEBUG_ALGORITHM);
        maxIter = baseNumIter * 1;
        answer = levelOne(simpleRef, simpleInput, bestGuessLevel2, maxIter);
        time = System.currentTimeMillis() - time;
        Preferences.debug(" Level 1 minutes = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);

        if (threadStopped) {
            finalize();

            return;
        }

        answer.matrix.Inverse();


        disposeLocal();
        finalize();
        setCompleted(true);
        //System.err.println( "Total registration time = " + (System.currentTimeMillis() - startTime) );
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param   point    DOCUMENT ME!
     * @param   message  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected boolean testBounds3D(double[] point, String message) {
        boolean test = false;
        double margin = Math.pow(10, -4);

        for (int i = 0; i < 6; i++) {

            if ((point[i] < (limits[0][i] - margin)) || (point[i] > (limits[1][i] + margin))) {
                test = true;
                i = 6;
            }
        }

        if (test) {
            Preferences.debug(message,Preferences.DEBUG_ALGORITHM);
            Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);

            for (int m = 0; m < 6; m++) {
                Preferences.debug("point: " + point[m] + ", min: ",Preferences.DEBUG_ALGORITHM);
                Preferences.debug(limits[0][m] + ", max: " + limits[1][m] + ".\n",Preferences.DEBUG_ALGORITHM);
            }

            Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
        }

        return test;
    }

    /**
     * Takes a simple image and subsamples it by 2, interpolating so that the new values are averages.
     *
     * @param   srcImage  Image to subsample.
     * @param   isColor   DOCUMENT ME!
     *
     * @return  Subsampled image.
     */
    private static ModelSimpleImage subsampleBy2(ModelSimpleImage srcImage, boolean isColor) {
        return srcImage.subsample3dBy2(isColor);
    }

    /**
     * Takes a simple image and subsamples XY by 2, interpolating so that the new XY values are averages.
     *
     * @param   srcImage  Image to subsample.
     * @param   isColor   DOCUMENT ME!
     *
     * @return  Subsampled image.
     */
    private static ModelSimpleImage subsampleBy2XY(ModelSimpleImage srcImage, boolean isColor) {
        return srcImage.subsample3dBy2XY(isColor);
    }

    /**
     * Check if the given minimum is within the translation and rotation bounds.
     *
     * @param   item  MatrixListItem
     *
     * @return  good true if minimum is in bounds.
     */
    private boolean checkMinimum(MatrixListItem item) {
        boolean good = true;

        for (int j = 0; (j < 6) && good; j++) {

            if ((item.initial[j] < limits[0][j]) || (item.initial[j] > limits[1][j])) {
                good = false;
            }
        }

        return good;
    }

    /**
     * Creates a string with the parameters that the image was constructed with.
     *
     * @return  Construction info.
     */
    private String getConstructionInfo() {
        String s;

        s = new String("RegistrationOAR3D(" + refImage.getImageName() + ", " + inputImage.getImageName() + ", ");

        if (weighted) {
            s += refWeight.getImageName() + ", " + inputWeight.getImageName() + ", ";
        }

        switch (costChoice) {

            case AlgorithmCostFunctions.CORRELATION_RATIO:
                s += "Correlation ratio, ";
                break;

            case AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED:
                s += "Correlation ratio smoothed, ";
                break;

            case AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT:
                s += "Correlation ratio smoothed weighted, ";
                break;

            case AlgorithmCostFunctions.LEAST_SQUARES:
                s += "Least squares, ";
                break;

            case AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED:
                s += "Least squares smoothed, ";
                break;

            case AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT:
                s += "Least squares smoothed weighted, ";
                break;

            case AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_COLOR:
                s += "Least squares smoothed color, ";
                break;

            case AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT_COLOR:
                s += "Least squares smoothed weighted color, ";
                break;

            case AlgorithmCostFunctions.MUTUAL_INFORMATION:
                s += "Mutual information, ";
                break;

            case AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED:
                s += "Mutual information smoothed, ";
                break;

            case AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED_WGT:
                s += "Mutual information smoothed weighted, ";
                break;

            case AlgorithmCostFunctions.NORMALIZED_XCORRELATION:
                s += "Normalized cross correlation, ";
                break;

            case AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED:
                s += "Normalized cross correlation smoothed, ";
                break;

            case AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED_WGT:
                s += "Normalized cross correlation smoothed weight, ";
                break;

            case AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION:
                s += "Normalized mutual information, ";
                break;

            case AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED:
                s += "Normalized mutual information smoothed, ";
                break;

            case AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT:
                s += "Normalized mutual information smoothed weighted, ";
                break;

            default:
                s += "Correlation ratio, ";
                break;
        }

        s += DOF + ", ";

        switch (interp) {

            case AlgorithmTransform.TRILINEAR:
                s += "Trilinear, ";
                break;

            case AlgorithmTransform.BSPLINE3:
                s += "Bspline 3rd order, ";
                break;

            case AlgorithmTransform.BSPLINE4:
                s += "Bspline 4th order, ";
                break;

            case AlgorithmTransform.CUBIC_LAGRANGIAN:
                s += "Cubic lagrangian, ";
                break;

            case AlgorithmTransform.QUINTIC_LAGRANGIAN:
                s += "Quintic lagrangian, ";
                break;

            case AlgorithmTransform.HEPTIC_LAGRANGIAN:
                s += "Heptic lagrangian, ";
                break;

            case AlgorithmTransform.WSINC:
                s += "Windowed sinc, ";
                break;

            default:
                s += "Trilinear, ";
                break;
        }

        s += rotateBeginX + ", " + coarseRateX + ", " + fineRateX + ", " + rotateBeginY + ", " + coarseRateY + ", " +
             fineRateY + ", " + rotateBeginZ + ", " + coarseRateZ + ", " + fineRateZ + ", " + maxResol + ", " +
             fastMode + ", " + calcCOG + ")\n";

        return s;
    }

    /**
     * Gets the tolerance vector based on the degrees of freedom (the length of the tolerance is the degrees of freedom)
     * and the level of subsampling (1, 2, 4, 8).
     *
     * @param   DOF  Degrees of freedom, will be length of vector.
     *
     * @return  New tolerance vector to send to optimization.
     *
     *          <p>Based on FLIRT paper: let n=pixel dimension (in one dimension) R=brain radius, here assumed to be
     *          half of field-of-view Translation tolerance = n/2 Rotation tolerance = (180/PI)*n/(2R) (converted to
     *          degrees because AlgorithmConstELSUNC works in degrees) Scaling tolerance = n/(2R) Skewing tolerance =
     *          n/(2R)</p>
     */
    private double[] getTolerance(int DOF) {
        double[] tols = new double[DOF];
        int i;

        if (DOF == 3) {

            for (i = 0; i < 3; i++) {
                tols[i] = 0.5;
            } // translation
        } else if (DOF == 4) {
            tols[0] = 0.005; // global scaling

            for (i = 1; i < 4; i++) {
                tols[i] = 0.5;
            } // translation
        } else if (DOF >= 6) {

            for (i = 0; i < DOF; i++) {

                if ((i / 3) == 0) {
                    tols[i] = ((180. / Math.PI) / maxDim);
                } // rotation tolerances
                else if ((i / 3) == 1) { // translation tolerances
                    tols[i] = 0.5;
                } else if ((i / 3) == 2) { // scaling tolerances
                    tols[i] = 0.005;
                } else if ((i / 3) == 3) { // skewing tolerances
                    tols[i] = 0.001;
                }
            }
        }

        return tols;
    }

    /**
     * Performs a trilinear interpolation on points. Takes 3 initial points, a vector of values to set, and an array in
     * which to look at neighbors of those points. Sets the appropriate values in the vector. Does not set scale if the
     * scale parameter is <code>false</code>.
     *
     * @param  x          X rotation initial index into array.
     * @param  y          Y rotation initial index into array.
     * @param  z          Z rotation initial index into array.
     * @param  initial    Vector to set; if scale is <code>true</code>, set three translations and a scale. Otherwise
     *                    just set translations.
     * @param  tranforms  DOCUMENT ME!
     * @param  scale      <code>true</code> means set the scale in the vector.
     */
    private void interpolate(double x, double y, double z, double[] initial, double[][][][] tranforms, boolean scale) {
        int ix0, iy0, iz0, ix1, iy1, iz1;

        // convert to closest integer values to access proper parts of array
        ix0 = (int) Math.floor(x);
        iy0 = (int) Math.floor(y);
        iz0 = (int) Math.floor(z);
        ix1 = ix0 + 1;
        iy1 = iy0 + 1;
        iz1 = iz0 + 1;

        // can't be bigger than 3, size of array is 4x4x4
        if ((ix0 == (coarseNumX - 1))) {
            ix1 = ix0;
        }

        if ((iy0 == (coarseNumY - 1))) {
            iy1 = iy0;
        }

        if ((iz0 == (coarseNumZ - 1))) {
            iz1 = iz0;
        }

        if (scale) {

            // x translation
            initial[3] = (((x - ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix1][iy1][iz1][1] * (z - iz0)) +
                                          (tranforms[ix1][iy1][iz0][1] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix1][iy0][iz1][1] * (z - iz0)) +
                                              (tranforms[ix1][iy0][iz0][1] * (1 - z + iz0)))))) +
                          ((1 - x + ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix0][iy1][iz1][1] * (z - iz0)) +
                                          (tranforms[ix0][iy1][iz0][1] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix0][iy0][iz1][1] * (z - iz0)) +
                                              (tranforms[ix0][iy0][iz0][1] * (1 - z + iz0)))))));

            // y translation
            initial[4] = (((x - ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix1][iy1][iz1][2] * (z - iz0)) +
                                          (tranforms[ix1][iy1][iz0][2] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix1][iy0][iz1][2] * (z - iz0)) +
                                              (tranforms[ix1][iy0][iz0][2] * (1 - z + iz0)))))) +
                          ((1 - x + ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix0][iy1][iz1][2] * (z - iz0)) +
                                          (tranforms[ix0][iy1][iz0][2] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix0][iy0][iz1][2] * (z - iz0)) +
                                              (tranforms[ix0][iy0][iz0][2] * (1 - z + iz0)))))));

            // z translation
            initial[5] = (((x - ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix1][iy1][iz1][3] * (z - iz0)) +
                                          (tranforms[ix1][iy1][iz0][3] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix1][iy0][iz1][3] * (z - iz0)) +
                                              (tranforms[ix1][iy0][iz0][3] * (1 - z + iz0)))))) +
                          ((1 - x + ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix0][iy1][iz1][3] * (z - iz0)) +
                                          (tranforms[ix0][iy1][iz0][3] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix0][iy0][iz1][3] * (z - iz0)) +
                                              (tranforms[ix0][iy0][iz0][3] * (1 - z + iz0)))))));

            // scale
            initial[6] = (((x - ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix1][iy1][iz1][0] * (z - iz0)) +
                                          (tranforms[ix1][iy1][iz0][0] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix1][iy0][iz1][0] * (z - iz0)) +
                                              (tranforms[ix1][iy0][iz0][0] * (1 - z + iz0)))))) +
                          ((1 - x + ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix0][iy1][iz1][0] * (z - iz0)) +
                                          (tranforms[ix0][iy1][iz0][0] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix0][iy0][iz1][0] * (z - iz0)) +
                                              (tranforms[ix0][iy0][iz0][0] * (1 - z + iz0)))))));
        } else {

            // x translation
            initial[3] = (((x - ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix1][iy1][iz1][0] * (z - iz0)) +
                                          (tranforms[ix1][iy1][iz0][0] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix1][iy0][iz1][0] * (z - iz0)) +
                                              (tranforms[ix1][iy0][iz0][0] * (1 - z + iz0)))))) +
                          ((1 - x + ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix0][iy1][iz1][0] * (z - iz0)) +
                                          (tranforms[ix0][iy1][iz0][0] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix0][iy0][iz1][0] * (z - iz0)) +
                                              (tranforms[ix0][iy0][iz0][0] * (1 - z + iz0)))))));

            // y translation
            initial[4] = (((x - ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix1][iy1][iz1][1] * (z - iz0)) +
                                          (tranforms[ix1][iy1][iz0][1] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix1][iy0][iz1][1] * (z - iz0)) +
                                              (tranforms[ix1][iy0][iz0][1] * (1 - z + iz0)))))) +
                          ((1 - x + ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix0][iy1][iz1][1] * (z - iz0)) +
                                          (tranforms[ix0][iy1][iz0][1] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix0][iy0][iz1][1] * (z - iz0)) +
                                              (tranforms[ix0][iy0][iz0][1] * (1 - z + iz0)))))));

            // z translation
            initial[5] = (((x - ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix1][iy1][iz1][2] * (z - iz0)) +
                                          (tranforms[ix1][iy1][iz0][2] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix1][iy0][iz1][2] * (z - iz0)) +
                                              (tranforms[ix1][iy0][iz0][2] * (1 - z + iz0)))))) +
                          ((1 - x + ix0) *
                               (((y - iy0) *
                                     ((tranforms[ix0][iy1][iz1][2] * (z - iz0)) +
                                          (tranforms[ix0][iy1][iz0][2] * (1 - z + iz0)))) +
                                    ((1 - y + iy0) *
                                         ((tranforms[ix0][iy0][iz1][2] * (z - iz0)) +
                                              (tranforms[ix0][iy0][iz0][2] * (1 - z + iz0)))))));
        }
    }

    /**
     * Takes two images that have been subsampled by a factor of eight. Sets up the cost function with the images and
     * the weighted images, if necessary. Uses the coarse sampling rate and optimizes translations and global scale at
     * the given rotation. So for example, if the coarse sampling range were -30 to 30 at every 15 degrees, we would
     * optimize at rotations of (-30, -30, -30), (-30, -30, -15), (-30, -30, 0), etc. In this case there would be a
     * total of 125 calls to the optimization method. Measures the cost at the fine sampling rate. Interpolates the
     * translations and global scale to come up with a good guess as to what the optimized translation would be at that
     * point. Takes the top 20% of the points and optimizes them. Now have a large multi-array of costs. 20% of those
     * have been optimized and placed back into their original position in the multi-array. Removes those items that are
     * outisde the rotation begin and end limits. Looks at the 8 neighbors of a point: +, =, or - one fine sample in
     * each of the three directions. If the point has a cost greater than any of these, it is not a minima. Otherwise it
     * is. Saves it in a vector of minima. Optimizes the minima over rotations as well as translations and global scale.
     * (Previously had not optimized over rotations.) Returns two vectors, one containing the minima before
     * optimization, one containing the minima after optimization.
     *
     * @param   ref    Subsampled by 8 reference image.
     * @param   input  Subsampled by 8 input image.
     *
     * @return  List of preoptimized and optimized points.
     */
    @SuppressWarnings("unchecked")
    private Vector<MatrixListItem>[] levelEight(ModelSimpleImage ref, ModelSimpleImage input) {
        AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input, costChoice, 32, 1);

        /*
         * // To test the amount of time for a single cost evaluation at this level: timeNow =
         * System.currentTimeMillis(); initialCost = cost.cost(tMatrix); timeLater = System.currentTimeMillis();
         * timeElapsed = timeLater - timeNow; Preferences.debug(" LEVEL EIGHT \n"); Preferences.debug(" Time for single
         * cost function evaluation: " + ( (float) timeElapsed / 1000.0f) + " seconds \n");
         *
         * // Initial amount of overlap. if (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION ||
         * costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED || costChoice ==
         * AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT) { double overAmt[] = new double[] {0, 0,
         * 0}; overAmt = cost.getOverlap(); Preferences.debug(" Initial overlap amount " + (int) (overAmt[0]) + " of " +
         * (int) (overAmt[1]) + " voxels, yielding " + (int) (overAmt[2]) + " percent.\n"); }
         */

        int numberOfCoarseAngles, numberOfFineAngles;
        numberOfCoarseAngles = coarseNumX * coarseNumY * coarseNumZ;
        numberOfFineAngles = fineNumX * fineNumY * fineNumZ;

        Preferences.debug("Total number of positions at coarse sampling to be tested: " + numberOfCoarseAngles + ".\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Total number of positions at fine sampling to be tested: " + numberOfFineAngles + ".\n",
        		Preferences.DEBUG_ALGORITHM);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub8);
            cost.setInputWgtImage(simpleWeightInputSub8);
        }

        // Initialize translation, if user selected to align the centers of gravity
        double diffX = 0;
        double diffY = 0;
        double diffZ = 0;
        Vector3f cog = new Vector3f(0, 0, 0);
        Vector3f cogR = new Vector3f(0, 0, 0);

        if (calcCOG == true) {
            cog = calculateCenterOfMass3D(input, simpleWeightInputSub8, doColor);
            cogR = calculateCenterOfMass3D(ref, simpleWeightRefSub8, doColor);
            Preferences.debug("Center of mass for the subsampled input image:" + cog + "\n",Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Center of mass for the subsampled reference image:" + cogR + "\n",Preferences.DEBUG_ALGORITHM);

            diffX = (cog.X - cogR.X);
            diffY = (cog.Y - cogR.Y);
            diffZ = (cog.Z - cogR.Z);
        }

        // Optimizing over translations and global scale for coarse angle increments
        double[] initial = new double[12];
        initial[0] = initial[1] = initial[2] = 0; // initial rotation
        initial[3] = diffX; // initial translations
        initial[4] = diffY;
        initial[5] = diffZ;
        initial[6] = initial[7] = initial[8] = 1; // initial scaling
        initial[9] = initial[10] = initial[11] = 0; // initial skewing

        double[][][][] transforms = new double[coarseNumX][coarseNumY][coarseNumZ][4];

        fireProgressStateChanged("\nOptimizing at coarse samples");
        Preferences.debug("Level Eight. Optimizing at coarse samples.\n",Preferences.DEBUG_ALGORITHM);

        AlgorithmConstELSUNCOpt3D elsunc = null;
        maxIter = baseNumIter * 2;

        if (DOF > 6) {
            elsunc = new AlgorithmConstELSUNCOpt3D(this, cog, 4, cost, initial, getTolerance(4), maxIter, bracketBound);
        } else {
            elsunc = new AlgorithmConstELSUNCOpt3D(this, cog, 3, cost, initial, getTolerance(3), maxIter, bracketBound);
        }
        elsunc.setRunningInSeparateThread(runningInSeparateThread);
        elsunc.setLimits(limits);

        int iG = 0;
        Preferences.debug("Coarse increment " + iG + ".\n",Preferences.DEBUG_ALGORITHM);

        boolean[] foundMin = new boolean[numberOfCoarseAngles];

        for (int i = 0; (i < coarseNumX) && !threadStopped; i++) {
            fireProgressStateChanged((i + 1) * 10 / coarseNumX);
            initial[0] = rotateBeginX + (i * coarseRateX);

            for (int j = 0; (j < coarseNumY) && !threadStopped; j++) {
                initial[1] = rotateBeginY + (j * coarseRateY);

                for (int k = 0; (k < coarseNumZ) && !threadStopped; k++) {
                    initial[2] = rotateBeginZ + (k * coarseRateZ);
                    initial[3] = diffX;
                    initial[4] = diffY;
                    initial[5] = diffZ;

                    if (testBounds3D(initial, initialMessage)) {
                        Preferences.debug("Initial point no good.\n",Preferences.DEBUG_ALGORITHM);
                    } else {
                        elsunc.setInitialPoint(initial);
                        elsunc.run();
                        foundMin[iG] = elsunc.didSucceed();

                        if (foundMin[iG]) {
                            transforms[i][j][k] = elsunc.getPoint();
                            initial[3] = elsunc.getPoint()[0];
                            initial[4] = elsunc.getPoint()[1];
                            initial[5] = elsunc.getPoint()[2];
                        } else {
                            Preferences.debug("For coarse increment " + iG +
                                              ", minimum was not found (ELSUNC failed).\n",Preferences.DEBUG_ALGORITHM);
                        }
                    }

                    iG++;
                }
            }
        }

        if (threadStopped) {
            return null;
        }

        // Fine sampling
        MatrixListItem[][][] matrixList = new MatrixListItem[fineNumX][fineNumY][fineNumZ];
        fireProgressStateChanged("Measuring at fine samples");
        Preferences.debug("Level Eight.  Measuring at fine samples.\n",Preferences.DEBUG_ALGORITHM);

        double[] costs = new double[fineNumX * fineNumY * fineNumZ];
        int index = 0;
        iG = 0;

        double factorX, factorY, factorZ;

        for (int i = 0; (i < fineNumX) && !threadStopped; i++) {
            fireProgressStateChanged(10 + ((i + 1) * 5 / fineNumX));
            initial[0] = rotateBeginX + (i * fineRateX);
            factorX = (i * fineRateX) / coarseRateX;

            for (int j = 0; (j < fineNumY) && !threadStopped; j++) {
                initial[1] = rotateBeginY + (j * fineRateY);
                factorY = (j * fineRateY) / coarseRateY;

                for (int k = 0; (k < fineNumZ) && !threadStopped; k++) {
                    initial[2] = rotateBeginZ + (k * fineRateZ);
                    initial[7] = initial[8] = initial[6];

                    factorZ = (k * fineRateZ) / coarseRateZ;
                    interpolate(factorX, factorY, factorZ, initial, transforms, (DOF > 6));

                    if (testBounds3D(initial, initialMessage)) {
                        matrixList[i][j][k] = new MatrixListItem(maxPossibleCost);
                        Preferences.debug("Setting the cost to the maximum possible for fine increment, " + iG + ".\n",
                        		Preferences.DEBUG_ALGORITHM);
                    } else {
                        elsunc.setInitialPoint(initial);
                        elsunc.measureCost();
                        matrixList[i][j][k] = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(),
                                                                 elsunc.getFinal());
                    }

                    costs[index++] = matrixList[i][j][k].cost;
                    iG++;
                }
            }
        }

        if (threadStopped) {
            return null;
        }

        // Sorting costs and calculating 20% cut-off value.
        Arrays.sort(costs);
        if (costs[0] == costs[costs.length - 1]) {
            Preferences.debug("!Warning all " + (costs.length) + " costs values = " + costs[0] + "\n",
            		Preferences.DEBUG_ALGORITHM);
            System.err.println("!Warning all " + (costs.length) + " costs values = " + costs[0]);
        }

        double threshold = costs[0] + (0.2 * (costs[costs.length - 1] - costs[0]));

        if (threshold > costs[(int) (0.2 * costs.length)]) {
            threshold = costs[(int) (0.2 * costs.length)];
        }

        Preferences.debug("Threshold is:" + threshold + "\n",Preferences.DEBUG_ALGORITHM);

        // Optimize top 20% of points
        double[] tempInitial = new double[12];

        for (int i = 0; i <= 11; i++) {
            tempInitial[i] = initial[i];
        }

        fireProgressStateChanged("Optimizing top samples");
        Preferences.debug("Level Eight. Optimizing top samples. \n",Preferences.DEBUG_ALGORITHM);
        elsunc.setRunningInSeparateThread(runningInSeparateThread);

        for (int i = 0; (i < fineNumX) && !threadStopped; i++) {
            fireProgressStateChanged(15 + ((i + 1) * 5 / fineNumX));

            for (int j = 0; (j < fineNumY) && !threadStopped; j++) {

                for (int k = 0; (k < fineNumZ) && !threadStopped; k++) {

                    if (matrixList[i][j][k].cost < threshold) {
                        tempInitial = matrixList[i][j][k].initial;

                        if (testBounds3D(tempInitial, initialMessage)) {
                            matrixList[i][j][k] = new MatrixListItem(maxPossibleCost);
                        }

                        elsunc.setInitialPoint(tempInitial);
                        elsunc.run();
                        matrixList[i][j][k] = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(),
                                                                 elsunc.getFinal());
                    }
                }
            }
        }
        elsunc.disposeLocal();

        if (threadStopped) {
            return null;
        }

        // Examine points to see if they are minima
        Vector<MatrixListItem> minima = new Vector<MatrixListItem>();
        boolean possibleMinima[][][] = new boolean[fineNumX][fineNumY][fineNumZ];

        for (int i = 0; i < fineNumX; i++) {

            for (int j = 0; j < fineNumY; j++) {

                for (int k = 0; k < fineNumZ; k++) {
                    
                    possibleMinima[i][j][k] = true; // possible minimum

                    for (int itest = -1; (itest <= 1) && possibleMinima[i][j][k]; itest++) {

                        // as long as still possible minimum, check neighbors one degree off
                        for (int jtest = -1; (jtest <= 1) && possibleMinima[i][j][k]; jtest++) {

                            for (int ktest = -1; (ktest <= 1) && possibleMinima[i][j][k]; ktest++) {

                                if (((i + itest) >= 0) && ((i + itest) < fineNumX) && ((j + jtest) >= 0) &&
                                        ((j + jtest) < fineNumY) && ((k + ktest) >= 0) && ((k + ktest) < fineNumZ)) {

                                    if ((matrixList[i][j][k].cost > matrixList[i + itest][j + jtest][k + ktest].cost) ||
                                            (matrixList[i][j][k].cost == maxPossibleCost)) {
                                        possibleMinima[i][j][k] = false;
                                    } // not a minimum if a neighbor has a lower cost
                                }
                            }
                        }
                    }

                }
            }
        }
        
        
       boolean change = true;
       while (change) {
           change = false;
           for (int i = 0; i < fineNumX; i++) {

               for (int j = 0; j < fineNumY; j++) {

                   for (int k = 0; k < fineNumZ; k++) {

                       for (int itest = -1; (itest <= 1) && possibleMinima[i][j][k]; itest++) {

                           // as long as still possible minimum, check neighbors one degree off
                           for (int jtest = -1; (jtest <= 1) && possibleMinima[i][j][k]; jtest++) {

                               for (int ktest = -1; (ktest <= 1) && possibleMinima[i][j][k]; ktest++) {

                                   if (((i + itest) >= 0) && ((i + itest) < fineNumX) && ((j + jtest) >= 0) &&
                                           ((j + jtest) < fineNumY) && ((k + ktest) >= 0) && ((k + ktest) < fineNumZ)) {

                                       if ((matrixList[i][j][k].cost == matrixList[i + itest][j + jtest][k + ktest].cost) &&
                                               (!possibleMinima[i + itest][j + jtest][k + ktest])) {
                                           possibleMinima[i][j][k] = false;
                                           change = true;
                                       } // not a minimum if equal value neighbor is not a minimum
                                   }
                               }
                           }
                       }

                   }
               }
           }
       }
       
       for (int i = 0; i < fineNumX; i++) {

           for (int j = 0; j < fineNumY; j++) {

               for (int k = 0; k < fineNumZ; k++) {
                   if (possibleMinima[i][j][k]) {
                       minima.add(matrixList[i][j][k]);
                   }
               }
               
           }
       }

        if (threadStopped) {
            return null;
        }

        Preferences.debug("Number of minima: " + minima.size() + "\n",Preferences.DEBUG_ALGORITHM);

        Vector<MatrixListItem> optMinima = new Vector<MatrixListItem>();

        // Now freely optimizes over rotations:
        fireProgressStateChanged("Optimizing minima");
        Preferences.debug("Level Eight. Optimizing minima freely over rotations. \n",Preferences.DEBUG_ALGORITHM);

        int count = 0;
        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;
        elsunc = new AlgorithmConstELSUNCOpt3D(this, cog, degree, cost, initial, getTolerance(degree), maxIter,
                                               bracketBound);
        elsunc.setRunningInSeparateThread(runningInSeparateThread);
        elsunc.setLimits(limits);

        MatrixListItem item;

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            fireProgressStateChanged(20 + ((count + 1) / minima.size() * 5));
            tempInitial = (en.nextElement()).initial;

            if (testBounds3D(tempInitial, initialMessage)) {
                item = new MatrixListItem(maxPossibleCost);
            } else {
                elsunc.setInitialPoint(tempInitial);
                elsunc.run();
                item = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(), elsunc.getFinal());
            }

            optMinima.add(item);
            count++;
        }

        if (threadStopped) {
            return null;
        }

        Preferences.debug("Number of optimized minima: " + count + "\n",Preferences.DEBUG_ALGORITHM);
        cost.disposeLocal();
        elsunc.disposeLocal();

        return new Vector [] { minima, optMinima };
    }

    /**
     * Takes two images that have been subsampled by a factor of four, and two vectors of minima. Sets up the cost
     * function with the images and the weighted images, if necessary. Adds the level4Factor determined during
     * subsampling. Measures the costs of the minima on the images and sort them. Takes the top three in each vector
     * (pre-optimization and post-optimization) and optimizes them. Puts them all into one vector. Perturbs the
     * rotations in each dimension by zero and plus-minus fineDelta. If it's not a rigid transformation, perturbs the
     * scales by factors of 0.8, 0.9, 1.0, 1.1, and 1.2. Optimize the perturbations. Returns a vector of the perturbed,
     * optimized minima.
     *
     * @param   ref        Reference image, subsampled by 4.
     * @param   input      Input image, subsampled by 4.
     * @param   minima     Preoptimized minima.
     * @param   optMinima  Optimized minima.
     *
     * @return  A vector of perturbed, optimized minima.
     */
    private Vector<MatrixListItem> levelFour(ModelSimpleImage ref, ModelSimpleImage input, Vector<MatrixListItem> minima,
    		                 Vector<MatrixListItem> optMinima) {
        AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input, costChoice, 64, 1);

        /*
         * // To test the amount of time for a single cost evaluation at this level: timeNow =
         * System.currentTimeMillis(); initialCost = cost.cost(tMatrix); timeLater = System.currentTimeMillis();
         * timeElapsed = timeLater - timeNow; Preferences.debug(" LEVEL FOUR \n"); Preferences.debug(" Time for single
         * cost function evaluation: " + ( (float) timeElapsed / 1000.0f) + " seconds \n");
         *
         * // Test initial amount of overlap. if (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION ||
         * costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED || costChoice ==
         * AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT) { double overAmt[] = new double[] {0, 0,
         * 0}; overAmt = cost.getOverlap(); Preferences.debug(" Initial overlap amount " + (int) (overAmt[0]) + " of " +
         * (int) (overAmt[1]) + " voxels, yielding " + (int) (overAmt[2]) + " percent.\n"); }
         */

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub4);
            cost.setInputWgtImage(simpleWeightInputSub4);
        }

        Vector3f cog = new Vector3f(0, 0, 0);

        if (calcCOG == true) {
            cog = calculateCenterOfMass3D(input, simpleWeightInputSub4, doColor);
            Preferences.debug("Center of mass for the subsampled input image:" + cog + "\n",Preferences.DEBUG_ALGORITHM);
            // Preferences.debug("Center of mass for the subsampled reference image:" + cog + "\n",Preferences.DEBUG_ALGORITHM);
        }

        MatrixListItem item = null;

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = (en.nextElement());
            item.initial[3] *= level4FactorXY;
            item.initial[4] *= level4FactorXY;
            item.initial[5] *= level4FactorZ;
        }

        for (Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements();) {
            item = (en.nextElement());
            item.initial[3] *= level4FactorXY;
            item.initial[4] *= level4FactorXY;
            item.initial[5] *= level4FactorZ;
        }

        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;

        AlgorithmConstELSUNCOpt3D elsunc = new AlgorithmConstELSUNCOpt3D(this, cog, degree, cost, dummy,
                                                                         getTolerance(degree), maxIter, bracketBound);
        elsunc.setLimits(limits);

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = (en.nextElement());
            elsunc.setInitialPoint(item.initial);
            elsunc.measureCost();
            item.cost = elsunc.getCost(); // pointer, so this changes the element in the minima Vector
        }

        if (threadStopped) {
            return null;
        }

        for (Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = (en.nextElement());
            elsunc.setInitialPoint(item.initial);
            elsunc.measureCost();
            item.cost = elsunc.getCost(); // pointer, so this changes the element in the minima Vector
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(minima);
        Collections.sort(optMinima);

        int total = (numMinima < minima.size()) ? numMinima : minima.size();
        // Old code: int total = (3 < minima.size()) ? 3 : minima.size(); New code: int total = (numMinima <
        // minima.size()) ? numMinima : minima.size(); Changed so that the number of minima to test at Level Four is a
        // variable, passed in from JDialog.  It used to be set to "3".

        elsunc.setMaxIterations(3);

        Vector<MatrixListItem> newMinima = new Vector<MatrixListItem>();
        fireProgressStateChanged("Optimizing new minima");

        double currentMinimum = 2, elsuncCost;
        int minimumIndex = 0;
        MatrixListItem best4Now;

        // Recalculating minima at this level, i.e. with images subsampled at level 4.
        for (int i = 0; (i < total) && !threadStopped; i++) { // Loop "total" times.
            fireProgressStateChanged(25 + ((i + 1) * 4 / total));

            // add i-th mimium to newMinima Vector
            elsunc.setInitialPoint((minima.elementAt(i)).initial);
            elsunc.setRunningInSeparateThread(runningInSeparateThread);
            elsunc.run();

            if (threadStopped) {
                return null;
            }

            elsuncCost = elsunc.getCost();
            item = new MatrixListItem(elsuncCost, elsunc.getMatrix(), elsunc.getFinal());
            newMinima.add(item);

            if (elsuncCost < currentMinimum) {
                currentMinimum = elsuncCost;
                minimumIndex = i;
            }

            // add i-th optimized minimum to newMinima vector
            elsunc.setInitialPoint((optMinima.elementAt(i)).initial);
            elsunc.setRunningInSeparateThread(runningInSeparateThread);
            elsunc.run();

            if (threadStopped) {
                return null;
            }

            elsuncCost = elsunc.getCost();
            item = new MatrixListItem(elsuncCost, elsunc.getMatrix(), elsunc.getFinal());
            newMinima.add(item);

            if (elsuncCost < currentMinimum) {
                currentMinimum = elsuncCost;
                minimumIndex = i + total;
            }
        }

        if (threadStopped) {
            return null;
        }

        // Print out the recalculated best minima.
        best4Now = null;
        Preferences.debug("Top optimized minima from Level Eight: (already sorted and recalculated)\n",
        		Preferences.DEBUG_ALGORITHM);

        for (int i = 0; i < (2 * total); i++) {
            Preferences.debug("\n Minimum number " + (i + 1) + ": \n",Preferences.DEBUG_ALGORITHM);
            best4Now = newMinima.elementAt(i);
            Preferences.debug(best4Now.toAbridgedString(),Preferences.DEBUG_ALGORITHM);
        }

        if (minimumIndex < total) {
            Preferences.debug("New minimum is at index " + (minimumIndex + 1) + " and is " + currentMinimum + "\n",
            		Preferences.DEBUG_ALGORITHM);
        } else {
            Preferences.debug("New minimum is from optimized minima at index " + (minimumIndex - total + 1) +
                              " and is " + currentMinimum + "\n",Preferences.DEBUG_ALGORITHM);
        }

        // Resort the minima.  Shouldn't have switched much from previous sorting.
        Collections.sort(newMinima);

        // Remove items outside the rotateBegin and rotateEnd limits
        int remove = 0;

        for (int i = (2 * total) - 1; i >= 1; i--) {

            if (!checkMinimum(newMinima.elementAt(i))) {
                newMinima.removeElementAt(i);
                remove++;
            }
        }

        if (remove < ((2 * total) - 1)) {

            if (!checkMinimum(newMinima.elementAt(0))) {
                newMinima.removeElementAt(0);
                remove++;
            }
        }

        Preferences.debug("Removed " + remove + " items outside rotation limits\n",Preferences.DEBUG_ALGORITHM);

        fireProgressStateChanged("Perturbing minima");

        double fineDeltaX = fineRateX / 8.0; // ZAC changed from 2.0 to 8.0
        double fineDeltaY = fineRateY / 8.0;
        double fineDeltaZ = fineRateZ / 8.0;
        double[] initial;
        Vector<MatrixListItem> perturbList = new Vector<MatrixListItem>();
        int sign = 1;
        currentMinimum = 2;
        minimumIndex = 0;
        best4Now = null;

        Preferences.debug("Number of minima to test at levelFour: " + ((2 * total) - remove) + ".\n",
        		Preferences.DEBUG_ALGORITHM);

        // Perturb rotations.  In each of the three dimensions, add fine delta and optimize,
        // then subtract fine delta and optimize.
        for (int j = 0; (j < 7) && !threadStopped; j++) {

            for (int i = 0; (i < ((2 * total) - remove)) && !threadStopped; i++) {
                fireProgressStateChanged(29 + ((((j * 2 * total) + i + 1) * 3) / (total * 12)));

                // Current "initial" is element for this i.
                initial = (double[]) (newMinima.elementAt(i)).initial.clone();

                // Output to debug window.
                if (((i + 1) % 2) == 0) {
                    Preferences.debug("Perturbing optimized minimum " + ((i / 2) + 1),Preferences.DEBUG_ALGORITHM);
                } else {
                    Preferences.debug("Perturbing minimum " + ((i / 2) + 1),Preferences.DEBUG_ALGORITHM);
                }

                // Will we add or subtract fine delta?  Add for j=1,3,5. Subract for j=2,4,6.
                if ((j % 2) != 0) {
                    sign = 1;
                } else {
                    sign = -1;
                }

                // Apply perturbation and send message to debug window.
                if (j == 0) {
                    Preferences.debug(".\n",Preferences.DEBUG_ALGORITHM);
                } else {

                    if ((j == 1) || (j == 2)) {
                        Preferences.debug(" by adding " + (sign * fineDeltaX) + " to initial[0] (" + (int) initial[0] +
                                          ").\n",Preferences.DEBUG_ALGORITHM);
                        initial[0] += sign * fineDeltaX;
                    } else if ((j == 3) || (j == 4)) {
                        Preferences.debug(" by adding " + (sign * fineDeltaY) + " to initial[1] (" + (int) initial[1] +
                                          ").\n",Preferences.DEBUG_ALGORITHM);
                        initial[1] += sign * fineDeltaY;
                    } else {
                        Preferences.debug(" by adding " + (sign * fineDeltaZ) + " to initial[2] (" + (int) initial[2] +
                                          ").\n",Preferences.DEBUG_ALGORITHM);
                        initial[2] += sign * fineDeltaZ;
                    }
                }

                // Set elsunc fields.
                elsunc.setInitialPoint(initial);
                elsunc.setRunningInSeparateThread(runningInSeparateThread);
                elsunc.run();

                // Add results (in form of a MatrixListItem) to perturbList.
                elsuncCost = elsunc.getCost();
                item = new MatrixListItem(elsuncCost, elsunc.getMatrix(), elsunc.getFinal());
                perturbList.add(item);

                // Update currentMinimum and minimumIndex so that we can know if level8 minima tend to be the same as
                // level4
                if (elsuncCost < currentMinimum) {
                    currentMinimum = elsuncCost;
                    minimumIndex = (int) i / 2;
                }

            }
        }

        if (threadStopped) {
            return null;
        }

        Preferences.debug("Best minimum from level four is a perturbed version of " + (minimumIndex + 1) + " and is " +
                          currentMinimum + ".\n",Preferences.DEBUG_ALGORITHM);

        if (DOF > 6) {

            // Perturb scales.  Multiply scaleDelta by 0.8, 0.9, 1.0, 1.1, and 1.2.
            float scaleDelta = 0.8f;

            for (int j = 0; (j < 5) && !threadStopped; j++) {

                for (int i = 0; (i < ((2 * total) - remove)) && !threadStopped; i++) {
                    fireProgressStateChanged(32 + ((((j * 2 * total) + i + 1) * 3) / (total * 8)));
                    initial = (double[]) (newMinima.elementAt(i)).initial.clone();

                    if (j == 1) {
                        scaleDelta = 0.9f;
                    } else if (j == 2) {
                        scaleDelta = 1.0f;
                    } else if (j == 3) {
                        scaleDelta = 1.1f;
                    } else if (j == 4) {
                        scaleDelta = 1.2f;
                    }

                    Preferences.debug("Perturbing initial[6] by ",Preferences.DEBUG_ALGORITHM);
                    initial[6] *= scaleDelta;
                    Preferences.debug("Multiplying by " + scaleDelta + "\n",Preferences.DEBUG_ALGORITHM);

                    // make initial variable old initial * scaleDelta in each dimension
                    elsunc.setInitialPoint(initial);
                    elsunc.setRunningInSeparateThread(runningInSeparateThread);
                    elsunc.run();
                    item = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(), elsunc.getFinal());
                    perturbList.add(item);
                }
            }
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(perturbList);

        // Print out best value from level4.
        // Preferences.debug("Top minimum from Level Four: \n",Preferences.DEBUG_ALGORITHM);
        best4Now = perturbList.elementAt(0);
        Preferences.debug(best4Now.toString(),Preferences.DEBUG_ALGORITHM);

        fireProgressStateChanged(35);
        cost.disposeLocal();
        elsunc.disposeLocal();

        return perturbList;
    }

    /**
     * Takes the two images, no subsampling, and the best minimum so far. Sets up the cost function with the images and
     * the weighted images, if necessary. Adds the level1Factor determined during subsampling. Performs one optimization
     * run, with the maximum allowable degrees of freedom as specified by the user (the max is 12). Returns the best
     * minimum.
     *
     * @param   ref      Reference image.
     * @param   input    Input image.
     * @param   item     Best minimum so far.
     * @param   maxIter  DOCUMENT ME!
     *
     * @return  Best minimum after optimization.
     */
    private MatrixListItem levelOne(ModelSimpleImage ref, ModelSimpleImage input, MatrixListItem item, int maxIter) {
        MatrixListItem item2;
        AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input, costChoice, 256, 1);

        /*
         * // To test the amount of time for a single cost evaluation at this level: timeNow =
         * System.currentTimeMillis(); initialCost = cost.cost(tMatrix); timeLater = System.currentTimeMillis();
         * timeElapsed = timeLater - timeNow; Preferences.debug(" LEVEL ONE \n"); Preferences.debug(" Time for single
         * cost function evaluation: " + ( (float) timeElapsed / 1000.0f) + " seconds \n");
         *
         * // To test the initial amount of overlap. if (costChoice ==
         * AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION || costChoice ==
         * AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED || costChoice ==
         * AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT) { double overAmt[] = new double[] {0, 0,
         * 0}; overAmt = cost.getOverlap(); Preferences.debug(" Initial overlap amount " + (int) (overAmt[0]) + " of " +
         * (int) (overAmt[1]) + " voxels, yielding " + (int) (overAmt[2]) + " percent.\n"); }
         */

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRef);
            cost.setInputWgtImage(simpleWeightInput);
            double min = Double.MAX_VALUE;
            double max = -Double.MAX_VALUE;
            for (int i = 0; i < simpleWeightInput.data.length; i++) {
                if (simpleWeightInput.data[i] < min) {
                    min = simpleWeightInput.data[i];
                }
                if (simpleWeightInput.data[i] > max) {
                    max = simpleWeightInput.data[i];
                }
                if (Double.isNaN(simpleWeightInput.data[i])) {
                    Preferences.debug("simpleWeightInput[" + i + "] = NaN\n",Preferences.DEBUG_ALGORITHM);
                }
                if (Double.isInfinite(simpleWeightInput.data[i])) {
                    Preferences.debug("simpleWeightInput[" + i + "] = Infinite\n",Preferences.DEBUG_ALGORITHM);
                }
            }
            Preferences.debug("simpleWeightInput.min = " + min + " simpleWeightInput.max = " + max + "\n",
            		Preferences.DEBUG_ALGORITHM);
            
            min = Double.MAX_VALUE;
            max = -Double.MAX_VALUE;
            for (int i = 0; i < simpleWeightRef.data.length; i++) {
                if (simpleWeightRef.data[i] < min) {
                    min = simpleWeightRef.data[i];
                }
                if (simpleWeightRef.data[i] > max) {
                    max = simpleWeightRef.data[i];
                }
                if (Double.isNaN(simpleWeightRef.data[i])) {
                    Preferences.debug("simpleWeightRef[" + i + "] = NaN\n",Preferences.DEBUG_ALGORITHM);
                }
                if (Double.isInfinite(simpleWeightRef.data[i])) {
                    Preferences.debug("simpleWeightRef[" + i + "] = Infinite\n",Preferences.DEBUG_ALGORITHM);
                }
            }
            Preferences.debug("simpleWeightRef.min = " + min + " simpleWeightRef.max = " + max + "\n",
            		Preferences.DEBUG_ALGORITHM);
        }
        
        double min = Double.MAX_VALUE;
        double max = -Double.MAX_VALUE;
        for (int i = 0; i < input.data.length; i++) {
            if (input.data[i] < min) {
                min = input.data[i];
            }
            if (input.data[i] > max) {
                max = input.data[i];
            }
            if (Double.isNaN(input.data[i])) {
                Preferences.debug("input[" + i + "] = NaN\n",Preferences.DEBUG_ALGORITHM);
            }
            if (Double.isInfinite(input.data[i])) {
                Preferences.debug("input[" + i + "] = Infinite\n",Preferences.DEBUG_ALGORITHM);
            }
        }
        Preferences.debug("input.min = " + min + " input.max = " + max + "\n",Preferences.DEBUG_ALGORITHM);
        
        min = Double.MAX_VALUE;
        max = -Double.MAX_VALUE;
        for (int i = 0; i < ref.data.length; i++) {
            if (ref.data[i] < min) {
                min = ref.data[i];
            }
            if (ref.data[i] > max) {
                max = ref.data[i];
            }
            if (Double.isNaN(ref.data[i])) {
                Preferences.debug("ref[" + i + "] = NaN\n",Preferences.DEBUG_ALGORITHM);
            }
            if (Double.isInfinite(ref.data[i])) {
                Preferences.debug("ref[" + i + "] = Infinite\n",Preferences.DEBUG_ALGORITHM);
            }
        }
        Preferences.debug("ref.min = " + min + " ref.max = " + max + "\n",Preferences.DEBUG_ALGORITHM);

        Vector3f cog = new Vector3f(0, 0, 0);

        if (calcCOG == true) {
            cog = calculateCenterOfMass3D(input, simpleWeightInput, doColor);
        }

        item.initial[3] *= level1FactorXY;
        item.initial[4] *= level1FactorXY;
        item.initial[5] *= level1FactorZ;

        int degree = (DOF < 12) ? DOF : 12;

        fireProgressStateChanged("Starting last optimization");
        Preferences.debug("cog.X = " + cog.X + " cog.Y = " + cog.Y + " cog.Z = " + cog.Z + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("degree = " + degree + "\n",Preferences.DEBUG_ALGORITHM);
        for (int i = 0; i < item.initial.length; i++) {
            Preferences.debug("item.initial[" + i + "] = " + item.initial[i] + "\n",Preferences.DEBUG_ALGORITHM);
        }
        for (int i = 0; i < getTolerance(degree).length; i++ ) {
            Preferences.debug("getTolerance(degree)[" + i + "] = " + getTolerance(degree)[i] + "\n",
            		Preferences.DEBUG_ALGORITHM);
        }
        Preferences.debug("maxIter = " + maxIter + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("bracketBound = " + bracketBound + "\n",Preferences.DEBUG_ALGORITHM);
        for (int i = 0; i < limits.length; i++) {

            for (int j = 0; j < limits[i].length; j++) {
                Preferences.debug("limits[" + i + "][" + j + "] = " + limits[i][j] + "\n",Preferences.DEBUG_ALGORITHM);    
            }
        }
        Preferences.debug("input.xRes = " + input.xRes + " input.yRes = " + input.yRes +  
                          " input.zRes = " + input.zRes + "\n",Preferences.DEBUG_ALGORITHM);

        AlgorithmConstELSUNCOpt3D elsunc = new AlgorithmConstELSUNCOpt3D(this, cog, degree, cost, item.initial,
                                                                         getTolerance(degree), maxIter, bracketBound);

        linkProgressToAlgorithm(elsunc);
        elsunc.setProgressValues(generateProgressValues(60, 100));

        elsunc.setRunningInSeparateThread(runningInSeparateThread);
        elsunc.setLimits(limits);
        elsunc.run();

        if (threadStopped) {
            return null;
        }

        // System.out.println("Input x =  " + input.xRes  + " y =  " + input.yRes  + " z =  " + input.zRes );
        item2 = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(input.xRes), elsunc.getFinal(input.xRes));
        item2.halfMatrix = elsunc.getMatrixHalf(input.xRes);
        item2.midsagMatrix = elsunc.getMatrixMidsagittal(input.xRes);
        fireProgressStateChanged(100);
        Preferences.debug("Best answer: \n" + item2 + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("item2.initial[3] = " + item2.initial[3] + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("item2.initial[4] = " + item2.initial[4] + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("item2.initial[5] = " + item2.initial[5] + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resRef[0] = " + resRef[0] + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resRef[1] = " + resRef[1] + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resRef[2] = " + resRef[2] + "\n",Preferences.DEBUG_ALGORITHM);

        cost.disposeLocal();
        elsunc.disposeLocal();

        return item2;
    }

    /**
     * Takes two images that have been subsampled by a factor of 2 and a vector of minima. Sets up the cost function
     * with the images and the weighted images, if necessary. Adds the level2Factor determined during subsampling.
     * Measures the costs of the minima at the images. Optimizes the best minimum with 7 degrees of freedom, then 9,
     * then 12. If the user has limited the degrees of freedom to 6, there will only be one optimization run, with 6
     * degrees of freedom. Returns the best minimum after optimization.
     *
     * @param   ref     Reference image, subsampled by 2.
     * @param   input   Input image, subsampled by 2.
     * @param   minima  Minima.
     *
     * @return  The optimized minimum.
     */
    private MatrixListItem levelTwo(ModelSimpleImage ref, ModelSimpleImage input, Vector<MatrixListItem> minima) {
        AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input, costChoice, 128, 1);

        /*
         * // To test the amount of time for a single cost evaluation at this level: timeNow =
         * System.currentTimeMillis(); initialCost = cost.cost(tMatrix); timeLater = System.currentTimeMillis();
         * timeElapsed = timeLater - timeNow; Preferences.debug(" LEVEL TWO \n"); Preferences.debug(" Time for single
         * cost function evaluation: " + ( (float) timeElapsed / 1000.0f) + " seconds \n"); // Amount of overlap for
         * initial position. if (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION || costChoice ==
         * AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED || costChoice ==
         * AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT) { double overAmt[] = new double[] {0, 0,
         * 0}; overAmt = cost.getOverlap(); Preferences.debug(" Initial overlap amount " + (int) (overAmt[0]) + " of " +
         * (int) (overAmt[1]) + " voxels, yielding " + (int) (overAmt[2]) + " percent.\n"); }
         */

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub2);
            cost.setInputWgtImage(simpleWeightInputSub2);
        }

        Vector3f cog = new Vector3f(0, 0, 0);

        if (calcCOG == true) {
            cog = calculateCenterOfMass3D(input, simpleWeightInputSub2, doColor);
        }

        MatrixListItem item = null;

        // Some problem here b/c initial point is starting off way out of bounds.

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = (en.nextElement());
            item.initial[3] *= level2FactorXY;
            item.initial[4] *= level2FactorXY;
            item.initial[5] *= level2FactorZ;
        }

        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;

        AlgorithmConstELSUNCOpt3D elsunc = new AlgorithmConstELSUNCOpt3D(this, cog, degree, cost, item.initial,
                                                                         getTolerance(degree), maxIter, bracketBound);
        elsunc.setLimits(limits);

        fireProgressStateChanged("Measuring costs of minima");

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = (en.nextElement());

            if (testBounds3D(item.initial, initialMessage)) {
                item.cost = maxPossibleCost;
                Preferences.debug("Setting the cost to the maximum possible.\n",Preferences.DEBUG_ALGORITHM);
            } else {
                elsunc.setInitialPoint(item.initial);
                elsunc.measureCost();
                item.cost = elsunc.getCost(); // pointer, so this changes the element in the minima Vector
            }
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(minima);

        fireProgressStateChanged("Optimizing with " + degree + " DOF");

        linkProgressToAlgorithm(elsunc);
        elsunc.setProgressValues(generateProgressValues(35, 43));

        elsunc.setInitialPoint((minima.elementAt(0)).initial);
        elsunc.setRunningInSeparateThread(runningInSeparateThread);
        elsunc.run();

        if (threadStopped) {
            return null;
        }

        item = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(), elsunc.getFinal());

        MatrixListItem itemPtr = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(input.xRes),
                                                    elsunc.getFinal(input.xRes));
        Preferences.debug("Level 2, after " + degree + " DOF: " + itemPtr + "\n",Preferences.DEBUG_ALGORITHM);

        maxIter = baseNumIter * 2;

        if (DOF > 7) {
            degree = 9;
            fireProgressStateChanged("Optimizing with " + degree + " DOF");
            fireProgressStateChanged(43);
            elsunc.disposeLocal();
            elsunc = new AlgorithmConstELSUNCOpt3D(this, cog, degree, cost, item.initial, getTolerance(degree), maxIter,
                                                   bracketBound);
            linkProgressToAlgorithm(elsunc);
            elsunc.setProgressValues(generateProgressValues(43, 51));
            elsunc.setRunningInSeparateThread(runningInSeparateThread);
            elsunc.setLimits(limits);
            elsunc.run();

            if (threadStopped) {
                return null;
            }

            item = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(), elsunc.getFinal());
            itemPtr = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(input.xRes), elsunc.getFinal(input.xRes));
            Preferences.debug("Level 2, after " + degree + " DOF: " + itemPtr + "\n",Preferences.DEBUG_ALGORITHM);

            if (DOF > 9) {
                degree = 12;
                fireProgressStateChanged("Optimizing with " + degree + " DOF");
                fireProgressStateChanged(51);
                elsunc.disposeLocal();
                elsunc = new AlgorithmConstELSUNCOpt3D(this, cog, 12, cost, item.initial, getTolerance(12), maxIter,
                                                       bracketBound);
                linkProgressToAlgorithm(elsunc);
                elsunc.setProgressValues(generateProgressValues(51, 60));
                elsunc.setRunningInSeparateThread(runningInSeparateThread);
                elsunc.setLimits(limits);
                elsunc.run();

                if (threadStopped) {
                    return null;
                }

                item = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(), elsunc.getFinal());
                itemPtr = new MatrixListItem(elsunc.getCost(), elsunc.getMatrix(input.xRes),
                                             elsunc.getFinal(input.xRes));
                Preferences.debug("Level 2, after " + degree + " DOF: " + itemPtr + "\n",Preferences.DEBUG_ALGORITHM);
            }
        }

        fireProgressStateChanged(60);
        cost.disposeLocal();
        elsunc.disposeLocal();

        return item;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Helper class to make it easy to store the necessary information about a minimum. Stores the "point", or vector at
     * which the minimum was reached; the "cost", or value of the cost function at that minimum; and the matrix, which
     * was the true input into the cost function and represents the transformation that gives the minimum cost of
     * differences between the images. Implements Comparable, so that a list of MatrixListItems can be sorted using
     * Java's sort.
     */
    class MatrixListItem implements Comparable <MatrixListItem>{

        /** Cost of function at this minimum. */
        protected double cost;

        /** Matrix with the best transformation divided by half. Might be null. */
        protected TransMatrixd halfMatrix;

        /** Rotations, translations, scales, and skews that make up transformation. */
        protected double[] initial;

        /** Matrix that gives best transformation. */
        protected TransMatrixd matrix;

        /** Matrix with the best transformation's z rot and xy translations. Might be null. */
        protected TransMatrixd midsagMatrix;

        /**
         * Creates new minimum object, with identity transMatrix and a given cost.
         *
         * @param  _cost  Cost of this minimum.
         */
        protected MatrixListItem(double _cost) {
            this.cost = _cost;
            this.matrix = new TransMatrixd(4);
            initial = new double[12];

            for (int i = 0; i < 12; i++) {
                initial[i] = 0.0;
            }
        }

        /**
         * Creates new minimum object, setting the data and copying the point array explicitly.
         *
         * @param  _cost     Cost of this minimum.
         * @param  _matrix   Matrix that gives best transformation.
         * @param  _initial  Rotations, translations, scales, and skews that make up transformation.
         */
        protected MatrixListItem(double _cost, TransMatrixd _matrix, double[] _initial) {
            this.cost = _cost;
            this.matrix = _matrix;
            initial = new double[_initial.length];

            for (int i = 0; i < initial.length; i++) {
                initial[i] = _initial[i];
            }
        }

        /**
         * Necessary to implement so that list may be sorted. Returns -1 if this cost is less than the parameter's cost;
         * 1 if this cost is greater than the parameter's cost; and 0 if they are equal.
         *
         * @param   o  MatrixListItem to compare to.
         *
         * @return  -1 if this is less than, 1 if greater than, 0 if equal.
         */
        public int compareTo(MatrixListItem o) {

            if (cost < o.cost) {
                return -1;
            } else if (cost > o.cost) {
                return 1;
            } else {
                return 0;
            }
        }

        /**
         * Creates string of this object with just first 6DOF and cost.
         *
         * @return  Readable string representation of this object.
         */
        public String toAbridgedString() {
            String s = "";
            s += "Cost of " + cost + " at:\n";

            for (int i = 0; i < 3; i++) {
                s += " Rotations : ";
                s += initial[i] + " ";
                s += "\n";
            }

            for (int i = 3; i < 6; i++) {
                s += " Translations : ";
                s += initial[i] + " ";
                s += "\n";
            }

            return s;
        }

        /**
         * Creates readable string of this object, including cost, matrix, and point with its meanings.
         *
         * @return  Readable string representation of this object.
         */
        public String toString() {
            String s = "";
            s += "Cost of " + cost + " at:\n";
            s += matrix.toString();
            s += "\n";
            s += "Point:\n";

            for (int i = 0; i < 3; i++) {
                s += " Rotations : ";
                s += initial[i] + " ";
                s += "\n";
            }

            for (int i = 3; i < 6; i++) {
                s += " Translations : ";
                s += (initial[i] * resRef[i - 3]) + " ";
                s += "\n";
            }

            for (int i = 6; i < 9; i++) {
                s += " Zooms : ";
                s += initial[i] + " ";
                s += "\n";
            }

            for (int i = 9; i < 12; i++) {
                s += " Skews : ";
                s += initial[i] + " ";
                s += "\n";
            }

            return s;
        }

    }
}
