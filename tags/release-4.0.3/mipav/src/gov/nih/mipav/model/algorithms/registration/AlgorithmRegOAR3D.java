package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

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
public class AlgorithmRegOAR3D extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int minimumZForSub = 16;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    double initialCost;

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
     * The bracket size around the minimum in multiples of unit_tolerance in the first iteration of Powell's algorithm.
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

    /** Dummy initial values used to create a Powell's algorithm instance before setting initial. */
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

    /** DOCUMENT ME! */
    private int maxDim = 256;

    /** Advanced optimization settings maxIter in the call to Powell's will be an integer multiple of baseNumIter. */
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
    private float rotateBeginX, rotateEndX, coarseRateX, fineRateX;

    /** DOCUMENT ME! */
    private float rotateBeginY, rotateEndY, coarseRateY, fineRateY;

    /** DOCUMENT ME! */
    private float rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;

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
     * @param  _rotateEndX    End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRateX   Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRateX     Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  _rotateBeginY  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEndY    End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRateY   Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRateY     Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  _rotateBeginZ  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEndZ    End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRateZ   Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRateZ     Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  _maxResol      If true is the maximum of the minimum resolution of the two datasets when resampling.
     * @param  _doSubsample   If true then subsample
     * @param  _fastMode      If true then searching the parameter space is not conducted and the algorithm proceeds to
     *                        level one immediately
     * @param  _bracketBound  The bracket size around the minimum in multiples of unit_tolerance for the first iteration
     *                        of Powell's algorithm.
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     */
    public AlgorithmRegOAR3D(ModelImage _imageA, ModelImage _imageB, int _costChoice, int _DOF, int _interp,
                             float _rotateBeginX, float _rotateEndX, float _coarseRateX, float _fineRateX,
                             float _rotateBeginY, float _rotateEndY, float _coarseRateY, float _fineRateY,
                             float _rotateBeginZ, float _rotateEndZ, float _coarseRateZ, float _fineRateZ,
                             boolean _maxResol, boolean _doSubsample, boolean _fastMode, int _bracketBound,
                             int _baseNumIter, int _numMinima) {

        super(null, _imageB);
        refImage = _imageA;
        inputImage = _imageB;

        if (inputImage.isColorImage()) {
            doColor = true;
        } else {
            doColor = false;
        }

        costChoice = _costChoice;
        DOF = _DOF;
        interp = _interp;
        resRef = refImage.getFileInfo(0).getResolutions();
        resInput = inputImage.getFileInfo(0).getResolutions();
        rotateBeginX = _rotateBeginX;
        rotateEndX = _rotateEndX;
        coarseRateX = _coarseRateX;
        fineRateX = _fineRateX;
        coarseNumX = (int) ((_rotateEndX - rotateBeginX) / coarseRateX) + 1;
        fineNumX = (int) ((_rotateEndX - rotateBeginX) / fineRateX) + 1;
        rotateBeginY = _rotateBeginY;
        rotateEndY = _rotateEndY;
        coarseRateY = _coarseRateY;
        fineRateY = _fineRateY;
        coarseNumY = (int) ((_rotateEndY - rotateBeginY) / coarseRateY) + 1;
        fineNumY = (int) ((_rotateEndY - rotateBeginY) / fineRateY) + 1;
        rotateBeginZ = _rotateBeginZ;
        rotateEndZ = _rotateEndZ;
        coarseRateZ = _coarseRateZ;
        fineRateZ = _fineRateZ;
        coarseNumZ = (int) ((_rotateEndZ - rotateBeginZ) / coarseRateZ) + 1;
        fineNumZ = (int) ((_rotateEndZ - rotateBeginZ) / fineRateZ) + 1;
        weighted = false;
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
     * @param  _rotateEndX    End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRateX   Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRateX     Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  _rotateBeginY  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEndY    End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRateY   Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRateY     Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  _rotateBeginZ  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEndZ    End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRateZ   Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRateZ     Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  _maxResol      If true is the maximum of the minimum resolution of the two datasets when resampling.
     * @param  _doSubsample   If true then subsample
     * @param  _fastMode      If true then searching the parameter space is not conducted and the algorithm proceeds to
     *                        level one immediately
     * @param  _bracketBound  The bracket size around the minimum in multiples of unit_tolerance for the first iteration
     *                        of Powell's algorithm.
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     */
    public AlgorithmRegOAR3D(ModelImage _imageA, ModelImage _imageB, ModelImage _refWeight, ModelImage _inputWeight,
                             int _costChoice, int _DOF, int _interp, float _rotateBeginX, float _rotateEndX,
                             float _coarseRateX, float _fineRateX, float _rotateBeginY, float _rotateEndY,
                             float _coarseRateY, float _fineRateY, float _rotateBeginZ, float _rotateEndZ,
                             float _coarseRateZ, float _fineRateZ, boolean _maxResol, boolean _doSubsample,
                             boolean _fastMode, int _bracketBound, int _baseNumIter, int _numMinima) {
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
        DOF = _DOF;
        interp = _interp;
        resRef = refImage.getFileInfo(0).getResolutions();
        resInput = inputImage.getFileInfo(0).getResolutions();
        rotateBeginX = _rotateBeginX;
        rotateEndX = _rotateEndX;
        coarseRateX = _coarseRateX;
        fineRateX = _fineRateX;
        coarseNumX = (int) ((_rotateEndX - rotateBeginX) / coarseRateX) + 1;
        fineNumX = (int) ((_rotateEndX - rotateBeginX) / fineRateX) + 1;
        rotateBeginY = _rotateBeginY;
        rotateEndY = _rotateEndY;
        coarseRateY = _coarseRateY;
        fineRateY = _fineRateY;
        coarseNumY = (int) ((_rotateEndY - rotateBeginY) / coarseRateY) + 1;
        fineNumY = (int) ((_rotateEndY - rotateBeginY) / fineRateY) + 1;
        rotateBeginZ = _rotateBeginZ;
        rotateEndZ = _rotateEndZ;
        coarseRateZ = _coarseRateZ;
        fineRateZ = _fineRateZ;
        coarseNumZ = (int) ((_rotateEndZ - rotateBeginZ) / coarseRateZ) + 1;
        fineNumZ = (int) ((_rotateEndZ - rotateBeginZ) / fineRateZ) + 1;
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
    public static Point3Dd calculateCenterOfMass3D(ModelSimpleImage image, ModelSimpleImage wgtImage, boolean isColor) {
        int x, y, z, c;
        int sliceSize = image.xDim * image.yDim;
        float diff;

        Point3Dd cogPt = new Point3Dd(0, 0, 0);
        double voxVal = 0.0, total = 0.0, wgtVal = 0.0;

        if (isColor) {

            if (wgtImage == null) {

                for (z = 0; z < image.zDim; z++) {

                    for (y = 0; y < image.yDim; y++) {

                        for (x = 0; x < image.xDim; x++) {

                            for (c = 1; c <= 3; c++) {
                                voxVal = image.data[(4 * ((z * sliceSize) + (y * image.xDim) + x)) + c];
                                cogPt.x += voxVal * x;
                                cogPt.y += voxVal * y;
                                cogPt.z += voxVal * z;
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
                                cogPt.x += wgtVal * voxVal * x;
                                cogPt.y += wgtVal * voxVal * y;
                                cogPt.z += wgtVal * voxVal * z;
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
                            cogPt.x += voxVal * x;
                            cogPt.y += voxVal * y;
                            cogPt.z += voxVal * z;
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
                            cogPt.x += wgtVal * voxVal * x;
                            cogPt.y += wgtVal * voxVal * y;
                            cogPt.z += wgtVal * voxVal * z;
                            total += wgtVal * voxVal;

                        }
                    }
                }
            }
        } // grey

        if (total != 0) {
            cogPt.x /= total;
            cogPt.y /= total;
            cogPt.z /= total;
        } else { // Do nothing at the moment which will leave the COG
        }

        return cogPt;
    }

    /**
     * Working ...
     *
     * @param  image    DOCUMENT ME!
     * @param  doColor  DOCUMENT ME!
     */
    public void calculatePrincipleAxis(ModelImage image, boolean doColor) {
        int x, y, z;
        int n = 0;
        Matrix mat2 = new Matrix(3, 3); // Row,Col
        Matrix meanProduct = new Matrix(3, 3);
        Matrix mean = new Matrix(3, 1); // Column vector
        double voxVal = 0;
        double total = 0;
        double tot = 0;

        // Moments first and second order
        double mX = 0, mY = 0, mZ = 0, mXX = 0, mXY = 0, mXZ = 0, mYY = 0, mYZ = 0, mZZ = 0;

        float min = (float) image.getMin();

        int xEnd = image.getExtents()[0];
        int yEnd = image.getExtents()[1];
        int zEnd = image.getExtents()[2];

        int nLim = (int) Math.sqrt((double) xEnd * yEnd * zEnd);

        if (nLim < 1000) {
            nLim = 1000;
        }

        for (z = 0; z < zEnd; z++) {

            for (y = 0; y < yEnd; y++) {

                for (x = 0; x < xEnd; x++) {

                    if (doColor) {
                        voxVal = (double) (image.getFloatC(x, y, z, 1) + image.getFloatC(x, y, z, 2) +
                                           image.getFloatC(x, y, z, 3));
                    } else {
                        voxVal = (double) (image.getFloat(x, y, z) - min);
                    }

                    mX += voxVal * x;
                    mY += voxVal * y;
                    mZ += voxVal * z;
                    mXX += voxVal * x * x;
                    mXY += voxVal * x * y;
                    mXZ += voxVal * x * z;
                    mYY += voxVal * y * y;
                    mYZ += voxVal * y * z;
                    mZZ += voxVal * z * z;
                    tot += voxVal;
                    n++;

                    if (n > nLim) { // Lets not over run the buffers during summation
                        n = 0;
                        total += tot;
                        mat2.set(0, 0, mat2.get(0, 0) + mXX);
                        mat2.set(0, 1, mat2.get(0, 1) + mXY);
                        mat2.set(0, 2, mat2.get(0, 2) + mXZ);
                        mat2.set(1, 1, mat2.get(1, 1) + mYY);
                        mat2.set(1, 2, mat2.get(1, 2) + mYZ);
                        mat2.set(2, 2, mat2.get(2, 2) + mZZ);
                        mean.set(0, 0, mean.get(0, 0) + mX);
                        mean.set(1, 0, mean.get(1, 0) + mY);
                        mean.set(2, 0, mean.get(2, 0) + mZ);
                        tot = 0;
                        mX = 0;
                        mY = 0;
                        mZ = 0;
                        mXX = 0;
                        mXY = 0;
                        mXZ = 0;
                        mYY = 0;
                        mYZ = 0;
                        mZZ = 0;
                    }
                }
            }
        }

        total += tot;

        if (Math.abs(total) < 1e-5) {
            total = 1.0f;
        }

        mat2.set(0, 0, (mat2.get(0, 0) + mXX) / total);
        mat2.set(0, 1, (mat2.get(0, 1) + mXY) / total);
        mat2.set(0, 2, (mat2.get(0, 2) + mXZ) / total);
        mat2.set(1, 1, (mat2.get(1, 1) + mYY) / total);
        mat2.set(1, 2, (mat2.get(1, 2) + mYZ) / total);
        mat2.set(2, 2, (mat2.get(2, 2) + mZZ) / total);
        mean.set(0, 0, (mean.get(0, 0) + mX) / total);
        mean.set(1, 0, (mean.get(1, 0) + mY) / total);
        mean.set(2, 0, (mean.get(2, 0) + mZ) / total);

        // Now make it central (taking off the Center of Mass)
        for (int j = 0; j < 3; j++) {

            for (int k = 0; k < 3; k++) {
                meanProduct.set(j, k, mean.get(j, 0) * mean.get(k, 0));
            }
        }

        mat2.minusEquals(meanProduct);
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
        return answer.matrix;
    }

    /**
     * Accessor that returns the matrix calculated in this algorithm divided by 2.
     *
     * @return  Matrix found at the end of algorithm with the compoents halved.
     */
    public TransMatrix getTransformHalf() {
        return answer.halfMatrix;
    }

    /**
     * Accessor that returns the z rot and x and y trans from the matrix calculated in this algorithm.
     *
     * @return  z rotation and x and y translations from the matrix found at the end of algorithm.
     */
    public TransMatrix getTransformMigsagittal() {
        return answer.midsagMatrix;
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
    	long beforeTime = System.currentTimeMillis();
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
            System.out.println(" sigmasRef[" + i + " ] = " + sigmasRef[i]);

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

        Preferences.debug("Blurring ref = " + sigmasRef[0] + ", " + sigmasRef[1] + ", " + sigmasRef[2] + "\n");
        Preferences.debug("Blurring inp = " + sigmasInput[0] + ", " + sigmasInput[1] + ", " + sigmasInput[2] + "\n");
        
        Preferences.debug(getConstructionInfo());

        if (blurRef) {

            if (doColor) {
                blurredRef = new ModelImage(ModelImage.ARGB_FLOAT, refImage.getExtents(), "BlurRef");
            } else {
                blurredRef = new ModelImage(ModelImage.FLOAT, refImage.getExtents(), "BlurRef");
            }

            // update resolutions
            FileInfoBase[] fileInfo = blurredRef.getFileInfo();

            for (i = 0; i < refImage.getExtents()[2]; i++) {
                fileInfo[i].setResolutions(refImage.getFileInfo()[i].getResolutions());
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
                    doSubsample) {
                simpleWeightRefSub2 = subsampleBy2(simpleWeightRef, false);
                simpleWeightInputSub2 = subsampleBy2(simpleWeightInput, false);
            } else if ((weightedRefPixels > subMinFactor) && (weightedInputPixels > subMinFactor) && doSubsample) {
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
                    doSubsample) {
                simpleWeightRefSub4 = subsampleBy2(simpleWeightRefSub2, false);
                simpleWeightInputSub4 = subsampleBy2(simpleWeightInputSub2, false);
            } else if ((weightedRefPixelsSub2 > subMinFactor) && (weightedInputPixelsSub2 > subMinFactor) &&
                           doSubsample) {
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
                    doSubsample) {
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
                        (simpleWeightInputSub8.zDim >= minimumZForSub) && doSubsample) {
                    ModelSimpleImage simpleWeightRefSub16;
                    ModelSimpleImage simpleWeightInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16  ***********\n");

                    simpleWeightRefSub16 = subsampleBy2(simpleWeightRefSub8, false);
                    simpleWeightInputSub16 = subsampleBy2(simpleWeightInputSub8, false);

                    simpleWeightRefSub8 = simpleWeightRefSub16;
                    simpleWeightInputSub8 = simpleWeightInputSub16;
                } else if ((weightedRefPixelsSub8 > subMinFactor) && (weightedInputPixelsSub8 > subMinFactor) &&
                               doSubsample) {
                    ModelSimpleImage simpleWeightRefSub16;
                    ModelSimpleImage simpleWeightInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n");

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
                           doSubsample) {
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

                if ((weightedRefPixelsSub8 > subMinFactor) && (weightedInputPixelsSub8 > subMinFactor) && doSubsample) {
                    ModelSimpleImage simpleWeightRefSub16;
                    ModelSimpleImage simpleWeightInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n");

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
                              "Weighted input subsampled 8 = " + simpleWeightInputSub8);

        } // end of (if weighted && fullAnalysisMode)

        if (fullAnalysisMode) {

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
                Preferences.debug("Level one image not resampled because ");

                if (simpleRef.dataSize <= subMinFactor) {
                    Preferences.debug("reference image data size is too small. \n");
                }

                if (simpleInput.dataSize <= subMinFactor) {
                    Preferences.debug("input image data size is too small. \n");
                }

                simpleRefSub2 = simpleRef;
                simpleInputSub2 = simpleInput;
            }

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
                Preferences.debug("Level two image not resampled because ");

                if (simpleRefSub2.dataSize <= subMinFactor) {
                    Preferences.debug("reference image data size is too small. \n");
                }

                if (simpleInputSub2.dataSize <= subMinFactor) {
                    Preferences.debug("input image data size is too small. \n");
                }

                simpleRefSub4 = simpleRefSub2;
                simpleInputSub4 = simpleInputSub2;
            }

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
                    Preferences.debug("Sub sampled level 8 to 16  ***********\n");

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
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n");

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
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n");

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
                              "Input subsampled 8 = " + simpleInputSub8);

            // STARTING LEVEL 8
            time = System.currentTimeMillis();
            Preferences.debug(" Starting level 8 ************************************************\n");

            Vector[] minimas = levelEight(simpleRefSub8, simpleInputSub8, 0, 30);

            // "minimas" is an array of Vector, because it will have two Vectors - one with
            // the original minima and one with the optimized minima.
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 8 minutes = " + ((float) time / 60000.0f) + "\n");
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }

            // STARTING LEVEL 4
            Preferences.debug(" Starting level 4 ************************************************\n");

            Vector minima = levelFour(simpleRefSub4, simpleInputSub4, minimas[0], minimas[1], 30, 60);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 4  minutes = " + ((float) time / 60000.0f) + "\n");
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }

            // STARTING LEVEL 2
            Preferences.debug(" Starting level 2 ************************************************\n");
            bestGuessLevel2 = levelTwo(simpleRefSub2, simpleInputSub2, minima, 60, 90);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 2 minutes = " + ((float) time / 60000.0f) + "\n");
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }
        } // end of if (fullAnalysisMode)
        else { // if (fastMode) setup item to be to "first best guess"  = identity

            double[] initial = new double[12];
            bestGuessLevel2 = new MatrixListItem(0, new TransMatrix(4), initial);

            double diffX = 0;
            double diffY = 0;
            double diffZ = 0;

            // change this
            if (calcCOG) {
                Point3Dd cog = calculateCenterOfMass3D(simpleInput, simpleWeightInput, doColor);
                Point3Dd cogR = calculateCenterOfMass3D(simpleRef, simpleWeightRef, doColor);
                Preferences.debug("Center of mass for the subsampled input image:" + cog + "\n");
                Preferences.debug("Center of mass for the subsampled reference image:" + cogR + "\n");

                diffX = (cog.x - cogR.x);
                diffY = (cog.y - cogR.y);
                diffZ = (cog.z - cogR.z);
            }

            bestGuessLevel2.initial[0] = bestGuessLevel2.initial[1] = bestGuessLevel2.initial[2] = 0; // initial rotation
            bestGuessLevel2.initial[3] = diffX; // initial translations
            bestGuessLevel2.initial[4] = diffY;
            bestGuessLevel2.initial[5] = diffZ;
            bestGuessLevel2.initial[6] = bestGuessLevel2.initial[7] = bestGuessLevel2.initial[8] = 1; // initial scaling
            bestGuessLevel2.initial[9] = bestGuessLevel2.initial[10] = bestGuessLevel2.initial[11] = 0; // initial skewing
        } // end of (fastMode)

        // STARTING LEVEL 0NE - note - this is for fastMode and fullAnalysisMode
        Preferences.debug(" Starting level 1 ************************************************\n");
        maxIter = baseNumIter * 1;
        answer = levelOne(simpleRef, simpleInput, bestGuessLevel2, maxIter, 90, 99);
        time = System.currentTimeMillis() - time;
        Preferences.debug(" Level 1 minutes = " + ((float) time / 60000.0f) + "\n");

        if (threadStopped) {
            finalize();

            return;
        }

        answer.matrix.invert();
        fireProgressStateChanged(100);
        disposeLocal();
        finalize();
        setCompleted(true);
        long timeConsumed = System.currentTimeMillis() - beforeTime;
        System.out.println("Time consumed by registration algorithm: " + timeConsumed);
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
     *          degrees because AlgorithmPowell works in degrees) Scaling tolerance = n/(2R) Skewing tolerance = n/(2R)
     *          </p>
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
    private Vector[] levelEight(ModelSimpleImage ref, ModelSimpleImage input,
			float progressFrom, float progressTo) {
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
        Preferences.debug("Total number of positions at coarse sampling to be tested: " +
                          (coarseNumX * coarseNumY * coarseNumZ) + ".\n");
        Preferences.debug("Total number of positions at fine sampling to be tested: " +
                          (fineNumX * fineNumY * fineNumZ) + ".\n");

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub8);
            cost.setInputWgtImage(simpleWeightInputSub8);
        }

        double diffX = 0;
        double diffY = 0;
        double diffZ = 0;
        Point3Dd cog = new Point3Dd(0, 0, 0);
        Point3Dd cogR = new Point3Dd(0, 0, 0);

        // change this
        if (calcCOG) {
            cog = calculateCenterOfMass3D(input, simpleWeightInputSub8, doColor);
            cogR = calculateCenterOfMass3D(ref, simpleWeightRefSub8, doColor);
            Preferences.debug("Center of mass for the subsampled input image:" + cog + "\n");
            Preferences.debug("Center of mass for the subsampled reference image:" + cogR + "\n");

            diffX = (cog.x - cogR.x);
            diffY = (cog.y - cogR.y);
            diffZ = (cog.z - cogR.z);
        }

        double[] initial = new double[12];

        initial[0] = initial[1] = initial[2] = 0; // initial rotation
        initial[3] = diffX; // initial translations
        initial[4] = diffY;
        initial[5] = diffZ;
        initial[6] = initial[7] = initial[8] = 1; // initial scaling
        initial[9] = initial[10] = initial[11] = 0; // initial skewing

        double[][][][] transforms = new double[coarseNumX][coarseNumY][coarseNumZ][4];

        // Optimizing over translations and global scale
        AlgorithmPowellOpt3D powell = null;
        maxIter = baseNumIter * 2;

        if (DOF > 6) {
            powell = new AlgorithmPowellOpt3D(this, cog, 4, cost, initial, getTolerance(4), maxIter, bracketBound);
        } else {
            powell = new AlgorithmPowellOpt3D(this, cog, 3, cost, initial, getTolerance(3), maxIter, bracketBound);
        }

        progress = progressFrom;
        progressStep = (progressTo-progressFrom)/5;
        fireProgressStateChanged("Optimizing at coarse samples");

        for (int i = 0; (i < coarseNumX) && !threadStopped; i++) {
        	progress += 2.0 * progressStep / coarseNumX;
            fireProgressStateChanged((int)progress);

            for (int j = 0; (j < coarseNumY) && !threadStopped; j++) {

                for (int k = 0; (k < coarseNumZ) && !threadStopped; k++) {
                    initial[0] = rotateBeginX + (i * coarseRateX);
                    initial[1] = rotateBeginY + (j * coarseRateY);
                    initial[2] = rotateBeginZ + (k * coarseRateZ);

                    // find cost, record
                    powell.setInitialPoint(initial);
                    powell.setRunningInSeparateThread(runningInSeparateThread);
                    powell.run();
                    transforms[i][j][k] = powell.getPoint();
                }
            }
        }

        if (threadStopped) {
            return null;
        }

        MatrixListItem[][][] matrixList = new MatrixListItem[fineNumX][fineNumY][fineNumZ];

        fireProgressStateChanged("Measuring at fine samples");

        double[] costs = new double[fineNumX * fineNumY * fineNumZ];
        int index = 0;
        double factorX, factorY, factorZ;

        System.out.println("coarseNumX and fineNumX " + coarseNumX + " " + fineNumX);
        System.out.println("coarseNumY and fineNumY " + coarseNumY + " " + fineNumY);
        System.out.println("coarseNumZ and fineNumZ " + coarseNumZ + " " + fineNumZ);

        for (int i = 0; (i < fineNumX) && !threadStopped; i++) {
        	progress += progressStep / fineNumX;
            fireProgressStateChanged((int)progress);

            for (int j = 0; (j < fineNumY) && !threadStopped; j++) {

                for (int k = 0; (k < fineNumZ) && !threadStopped; k++) {
                    initial[0] = rotateBeginX + (i * fineRateX);
                    initial[1] = rotateBeginY + (j * fineRateY);
                    initial[2] = rotateBeginZ + (k * fineRateZ);

                    // sets up translation and global scaling factors
                    factorX = (i * fineRateX) / coarseRateX;
                    factorY = (j * fineRateY) / coarseRateY;
                    factorZ = (k * fineRateZ) / coarseRateZ;
                    interpolate(factorX, factorY, factorZ, initial, transforms, (DOF > 6));
                    initial[7] = initial[8] = initial[6];
                    powell.setInitialPoint(initial);
                    powell.measureCost();
                    matrixList[i][j][k] = new MatrixListItem(powell.getCost(), powell.getMatrix(), powell.getFinal());
                    costs[index++] = matrixList[i][j][k].cost;
                }
            }
        }

        if (threadStopped) {
            return null;
        }

        Arrays.sort(costs);

        double threshold = costs[0] + (0.2 * (costs[costs.length - 1] - costs[0]));

        if (threshold > costs[(int) (0.2 * costs.length)]) {
            threshold = costs[(int) (0.2 * costs.length)];
        }

        fireProgressStateChanged("Optimizing top samples");

        for (int i = 0; (i < fineNumX) && !threadStopped; i++) {
        	progress += progressStep / fineNumX;
            fireProgressStateChanged((int)progress);

            for (int j = 0; (j < fineNumY) && !threadStopped; j++) {

                for (int k = 0; (k < fineNumZ) && !threadStopped; k++) {

                    if (matrixList[i][j][k].cost < threshold) {
                        powell.setInitialPoint(matrixList[i][j][k].initial);
                        powell.setRunningInSeparateThread(runningInSeparateThread);
                        powell.run();
                        matrixList[i][j][k] = new MatrixListItem(powell.getCost(), powell.getMatrix(),
                                                                 powell.getFinal());
                    }
                }
            }
        }

        if (threadStopped) {
            return null;
        }

        Vector minima = new Vector();

        for (int i = 0; i < fineNumX; i++) {

            for (int j = 0; j < fineNumY; j++) {

                for (int k = 0; k < fineNumZ; k++) {
                    boolean minimum = true; // possible minimum

                    for (int itest = -1; (itest <= 1) && minimum; itest++) { // as long as still possible minimum,
                                                                             // check neighbors one degree off

                        for (int jtest = -1; (jtest <= 1) && minimum; jtest++) {

                            for (int ktest = -1; (ktest <= 1) && minimum; ktest++) {

                                if (((i + itest) >= 0) && ((i + itest) < fineNumX) && ((j + jtest) >= 0) &&
                                        ((j + jtest) < fineNumY) && ((k + ktest) >= 0) && ((k + ktest) < fineNumZ)) {

                                    if (matrixList[i][j][k].cost > matrixList[i + itest][j + jtest][k + ktest].cost) {
                                        minimum = false;
                                    } // not a minimum if a neighbor has a lower cost
                                }
                            }
                        }
                    }

                    if (minimum) {
                        minima.add(matrixList[i][j][k]);
                    }
                }
            }
        }

        if (threadStopped) {
            return null;
        }

        Preferences.debug("Number of minima: " + minima.size() + "\n");

        Vector optMinima = new Vector();
        // Now freely optimizes over rotations:

        fireProgressStateChanged("Optimizing minima");

        int count = 0;
        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;
        powell = new AlgorithmPowellOpt3D(this, cog, degree, cost, initial, getTolerance(degree), maxIter,
                                          bracketBound);

        MatrixListItem item;

        for (Enumeration en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
        	progress += progressStep / minima.size();
            fireProgressStateChanged((int)progress);
            powell.setInitialPoint(((MatrixListItem) en.nextElement()).initial);
            powell.setRunningInSeparateThread(runningInSeparateThread);
            powell.run();
            item = new MatrixListItem(powell.getCost(), powell.getMatrix(), powell.getFinal());
            optMinima.add(item);
            count++;
        }

        if (threadStopped) {
            return null;
        }
        fireProgressStateChanged((int)progressTo);
        cost.disposeLocal();
        powell.disposeLocal();

        return new Vector[] { minima, optMinima };
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
    private Vector levelFour(ModelSimpleImage ref, ModelSimpleImage input,
			Vector minima, Vector optMinima, float progressFrom,
			float progressTo) {
		AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input,
				costChoice, 64, 1);

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

        Point3Dd cog = new Point3Dd(0, 0, 0);

        if (calcCOG) {
            cog = calculateCenterOfMass3D(input, simpleWeightInputSub4, doColor);
        }

        Preferences.debug("Center of mass for the subsampled input image:" + cog + "\n");

        // Preferences.debug("Center of mass for the subsampled reference image:" + cog + "\n");
        MatrixListItem item = null;

        for (Enumeration en = minima.elements(); en.hasMoreElements();) {
            item = ((MatrixListItem) en.nextElement());
            item.initial[3] *= level4FactorXY;
            item.initial[4] *= level4FactorXY;
            item.initial[5] *= level4FactorZ;
        }

        for (Enumeration en = optMinima.elements(); en.hasMoreElements();) {
            item = ((MatrixListItem) en.nextElement());
            item.initial[3] *= level4FactorXY;
            item.initial[4] *= level4FactorXY;
            item.initial[5] *= level4FactorZ;
        }

        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;

        AlgorithmPowellOpt3D powell = new AlgorithmPowellOpt3D(this, cog, degree, cost, dummy, getTolerance(degree),
                                                               maxIter, bracketBound);

        for (Enumeration en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = ((MatrixListItem) en.nextElement());
            powell.setInitialPoint(item.initial);
            powell.measureCost();
            item.cost = powell.getCost(); // pointer, so this changes the element in the minima Vector
        }

        if (threadStopped) {
            return null;
        }

        for (Enumeration en = optMinima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = ((MatrixListItem) en.nextElement());
            powell.setInitialPoint(item.initial);
            powell.measureCost();
            item.cost = powell.getCost(); // pointer, so this changes the element in the minima Vector
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

        powell.setMaxIterations(3);

        Vector newMinima = new Vector();
        fireProgressStateChanged("Optimizing new minima");

        double currentMinimum = 2, powellCost;
        int minimumIndex = 0;
        MatrixListItem best4Now;
        progress = progressFrom;
        progressStep = (progressTo - progressFrom)/10;
        // Recalculating minima at this level, i.e. with images subsampled at level 4.
        for (int i = 0; (i < total) && !threadStopped; i++) { // Loop "total" times.
        	progress += 4.0 * progressStep / total;
            fireProgressStateChanged((int)progress);

            // add i-th mimium to newMinima Vector
            powell.setInitialPoint(((MatrixListItem) minima.elementAt(i)).initial);
            powell.setRunningInSeparateThread(runningInSeparateThread);
            powell.run();

            if (threadStopped) {
                return null;
            }

            powellCost = powell.getCost();
            item = new MatrixListItem(powellCost, powell.getMatrix(), powell.getFinal());
            newMinima.add(item);

            if (powellCost < currentMinimum) {
                currentMinimum = powellCost;
                minimumIndex = i;
            }

            // add i-th optimized minimum to newMinima vector
            powell.setInitialPoint(((MatrixListItem) optMinima.elementAt(i)).initial);
            powell.setRunningInSeparateThread(runningInSeparateThread);
            powell.run();

            if (threadStopped) {
                return null;
            }

            powellCost = powell.getCost();
            item = new MatrixListItem(powellCost, powell.getMatrix(), powell.getFinal());
            newMinima.add(item);

            if (powellCost < currentMinimum) {
                currentMinimum = powellCost;
                minimumIndex = i + total;
            }
        }

        if (threadStopped) {
            return null;
        }

        // Print out the recalculated best minima.
        best4Now = null;
        Preferences.debug("Top optimized minima from Level Eight: (already sorted and recalculated)\n");

        for (int i = 0; i < (2 * total); i++) {
            Preferences.debug("\n Minimum number " + (i + 1) + ": \n");
            best4Now = (MatrixListItem) newMinima.elementAt(i);
            Preferences.debug(best4Now.toAbridgedString());
        }

        if (minimumIndex < total) {
            Preferences.debug("New minimum is at index " + (minimumIndex + 1) + " and is " + currentMinimum + "\n");
        } else {
            Preferences.debug("New minimum is from optimized minima at index " + (minimumIndex - total + 1) +
                              " and is " + currentMinimum + "\n");
        }

        // Resort the minima.  Shouldn't have switched much from previous sorting.
        Collections.sort(newMinima);

        // Remove items outside the rotateBegin and rotateEnd limits
        int remove = 0;

        for (int i = (2 * total) - 1; i >= 1; i--) {

            if ((((MatrixListItem) newMinima.elementAt(i)).initial[0] < rotateBeginX) ||
                    (((MatrixListItem) newMinima.elementAt(i)).initial[0] > rotateEndX) ||
                    (((MatrixListItem) newMinima.elementAt(i)).initial[1] < rotateBeginY) ||
                    (((MatrixListItem) newMinima.elementAt(i)).initial[1] > rotateEndY) ||
                    (((MatrixListItem) newMinima.elementAt(i)).initial[2] < rotateBeginZ) ||
                    (((MatrixListItem) newMinima.elementAt(i)).initial[2] > rotateEndZ)) {
                newMinima.removeElementAt(i);
                remove++;
            }
        }

        if (remove < ((2 * total) - 1)) {

            if ((((MatrixListItem) newMinima.elementAt(0)).initial[0] < rotateBeginX) ||
                    (((MatrixListItem) newMinima.elementAt(0)).initial[0] > rotateEndX) ||
                    (((MatrixListItem) newMinima.elementAt(0)).initial[1] < rotateBeginY) ||
                    (((MatrixListItem) newMinima.elementAt(0)).initial[1] > rotateEndY) ||
                    (((MatrixListItem) newMinima.elementAt(0)).initial[2] < rotateBeginZ) ||
                    (((MatrixListItem) newMinima.elementAt(0)).initial[2] > rotateEndZ)) {
                newMinima.removeElementAt(0);
                remove++;
            }
        }

        Preferences.debug("Removed " + remove + " items outside rotation limits\n");

        fireProgressStateChanged("Perturbing minima");

        double fineDeltaX = fineRateX / 2.0;
        double fineDeltaY = fineRateY / 2.0;
        double fineDeltaZ = fineRateZ / 2.0;
        double[] initial;
        Vector perturbList = new Vector();
        int sign = 1;
        currentMinimum = 2;
        minimumIndex = 0;
        best4Now = null;

        Preferences.debug("Number of minima to test at levelFour: " + ((2 * total) - remove) + ".\n");

        // Perturb rotations.  In each of the three dimensions, add fine delta and optimize,
        // then subtract fine delta and optimize.
        for (int j = 0; (j < 7) && !threadStopped; j++) {

            for (int i = 0; (i < ((2 * total) - remove)) && !threadStopped; i++) {
            	progress += 3.0 * progressStep / ((2*total-remove) * 7);
                fireProgressStateChanged((int)progress);

                // Current "initial" is element for this i.
                initial = (double[]) ((MatrixListItem) newMinima.elementAt(i)).initial.clone();

                // Output to debug window.
                if (((i + 1) % 2) == 0) {
                    Preferences.debug("Perturbing optimized minimum " + ((i / 2) + 1));
                } else {
                    Preferences.debug("Perturbing minimum " + ((i / 2) + 1));
                }

                // Will we add or subtract fine delta?  Add for j=1,3,5. Subract for j=2,4,6.
                if ((j % 2) != 0) {
                    sign = 1;
                } else {
                    sign = -1;
                }

                // Apply perturbation and send message to debug window.
                if (j == 0) {
                    Preferences.debug(".\n");
                } else {

                    if ((j == 1) || (j == 2)) {
                        Preferences.debug(" by adding " + (sign * fineDeltaX) + " to initial[0] (" + (int) initial[0] +
                                          ").\n");
                        initial[0] += sign * fineDeltaX;
                    } else if ((j == 3) || (j == 4)) {
                        Preferences.debug(" by adding " + (sign * fineDeltaY) + " to initial[1] (" + (int) initial[1] +
                                          ").\n");
                        initial[1] += sign * fineDeltaY;
                    } else {
                        Preferences.debug(" by adding " + (sign * fineDeltaZ) + " to initial[2] (" + (int) initial[2] +
                                          ").\n");
                        initial[2] += sign * fineDeltaZ;
                    }
                }

                // Set powell fields.
                powell.setInitialPoint(initial);
                powell.setRunningInSeparateThread(runningInSeparateThread);
                powell.run();

                // Add results (in form of a MatrixListItem) to perturbList.
                powellCost = powell.getCost();
                item = new MatrixListItem(powellCost, powell.getMatrix(), powell.getFinal());
                perturbList.add(item);

                // Update currentMinimum and minimumIndex so that we can know if level8 minima tend to be the same as
                // level4
                if (powellCost < currentMinimum) {
                    currentMinimum = powellCost;
                    minimumIndex = (int) i / 2;
                }

            }
        }

        if (threadStopped) {
            return null;
        }

        Preferences.debug("Best minimum from level four is a perturbed version of " + (minimumIndex + 1) + " and is " +
                          currentMinimum + ".\n");

        if (DOF > 6) {

            // Perturb scales.  Multiply scaleDelta by 0.8, 0.9, 1.0, 1.1, and 1.2.
            float scaleDelta = 0.8f;

            for (int j = 0; (j < 5) && !threadStopped; j++) {

                for (int i = 0; (i < ((2 * total) - remove)) && !threadStopped; i++) {
                	progress += 3.0 * progressStep / ((2*total-remove) * 5);
                    fireProgressStateChanged((int)progress);

                    initial = (double[]) ((MatrixListItem) newMinima.elementAt(i)).initial.clone();

                    if (j == 1) {
                        scaleDelta = 0.9f;
                    } else if (j == 2) {
                        scaleDelta = 1.0f;
                    } else if (j == 3) {
                        scaleDelta = 1.1f;
                    } else if (j == 4) {
                        scaleDelta = 1.2f;
                    }

                    Preferences.debug("Perturbing initial[6] by ");
                    initial[6] *= scaleDelta;
                    Preferences.debug("Multiplying by " + scaleDelta + "\n");

                    // make initial variable old initial * scaleDelta in each dimension
                    powell.setInitialPoint(initial);
                    powell.setRunningInSeparateThread(runningInSeparateThread);
                    powell.run();
                    item = new MatrixListItem(powell.getCost(), powell.getMatrix(), powell.getFinal());
                    perturbList.add(item);
                }
            }
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(perturbList);

        // Print out best value from level4.
        // Preferences.debug("Top minimum from Level Four: \n");
        best4Now = (MatrixListItem) perturbList.elementAt(0);
        Preferences.debug(best4Now.toString());

        fireProgressStateChanged((int)progressTo);
        cost.disposeLocal();
        powell.disposeLocal();

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
    private MatrixListItem levelOne(ModelSimpleImage ref,
			ModelSimpleImage input, MatrixListItem item, int maxIter,
			float progressFrom, float progressTo) {
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
        }

        Point3Dd cog = new Point3Dd(0, 0, 0);

        if (calcCOG) {
            cog = calculateCenterOfMass3D(input, simpleWeightInput, doColor);
        }

        item.initial[3] *= level1FactorXY;
        item.initial[4] *= level1FactorXY;
        item.initial[5] *= level1FactorZ;

        int degree = (DOF < 12) ? DOF : 12;

        fireProgressStateChanged("Starting last optimization");

        AlgorithmPowellOpt3D powell = new AlgorithmPowellOpt3D(this, cog, degree, cost, item.initial,
                                                               getTolerance(degree), maxIter, bracketBound);

        linkProgressToAlgorithm(powell);
        powell.setProgressValues(generateProgressValues((int)progressFrom, (int)progressTo));
        powell.setRunningInSeparateThread(runningInSeparateThread);
        powell.run();

        if (threadStopped) {
            return null;
        }

        // System.out.println("Input x =  " + input.xRes  + " y =  " + input.yRes  + " z =  " + input.zRes );
        item2 = new MatrixListItem(powell.getCost(), powell.getMatrix(input.xRes), powell.getFinal(input.xRes));
        item2.halfMatrix = powell.getMatrixHalf(input.xRes);
        item2.midsagMatrix = powell.getMatrixMidsagittal(input.xRes);
        delinkProgressToAlgorithm(powell);

        fireProgressStateChanged((int)progressTo);
        Preferences.debug("Best answer: \n" + item2 + "\n");
        
        cost.disposeLocal();
        powell.disposeLocal();

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
    private MatrixListItem levelTwo(ModelSimpleImage ref,
			ModelSimpleImage input, Vector minima, float progressFrom,
			float progressTo) {
		AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input,
				costChoice, 128, 1);

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

        Point3Dd cog = new Point3Dd(0, 0, 0);

        if (calcCOG) {
            cog = calculateCenterOfMass3D(input, simpleWeightInputSub2, doColor);
        }

        MatrixListItem item = null;

        for (Enumeration en = minima.elements(); en.hasMoreElements();) {
            item = ((MatrixListItem) en.nextElement());
            item.initial[3] *= level2FactorXY;
            item.initial[4] *= level2FactorXY;
            item.initial[5] *= level2FactorZ;
        }

        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;

        AlgorithmPowellOpt3D powell = new AlgorithmPowellOpt3D(this, cog, degree, cost, item.initial,
                                                               getTolerance(degree), maxIter, bracketBound);

        fireProgressStateChanged("Measuring costs of minima");

        for (Enumeration en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = ((MatrixListItem) en.nextElement());
            powell.setInitialPoint(item.initial);
            powell.measureCost();
            item.cost = powell.getCost(); // pointer, so this changes the element in the minima Vector
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(minima);

        fireProgressStateChanged("Optimizing with " + degree + " DOF");
        int progressStart = (int)progressFrom;
        progressStep = (progressTo - progressFrom)/8;
        if(DOF > 9){
        	progressStep = (progressTo-progressFrom)/25;
        }else if(DOF > 7){
        	progressStep = (progressTo-progressFrom)/16;
        }
        int progressEnd = (int)(progressStart + 8 * progressStep);

        linkProgressToAlgorithm(powell);
        powell.setProgressValues(generateProgressValues(progressStart, progressEnd));
        powell.setInitialPoint(((MatrixListItem) minima.elementAt(0)).initial);
        powell.setRunningInSeparateThread(runningInSeparateThread);
        powell.run();

        if (threadStopped) {
            return null;
        }

        item = new MatrixListItem(powell.getCost(), powell.getMatrix(), powell.getFinal());

        MatrixListItem itemPtr = new MatrixListItem(powell.getCost(), powell.getMatrix(input.xRes),
                                                    powell.getFinal(input.xRes));
        delinkProgressToAlgorithm(powell);
        Preferences.debug("Level 2, after " + degree + " DOF: " + itemPtr + "\n");
        maxIter = baseNumIter * 2;

        if (DOF > 7) {
        	progressStart = progressEnd;
        	progressEnd = (int)(progressStart + 8 * progressStep);
            degree = 9;
            fireProgressStateChanged("Optimizing with " + degree + " DOF");
            powell = new AlgorithmPowellOpt3D(this, cog, degree, cost, item.initial, getTolerance(degree), maxIter,
                                              bracketBound);
            linkProgressToAlgorithm(powell);
            powell.setProgressValues(generateProgressValues(progressStart, progressEnd));
            powell.setRunningInSeparateThread(runningInSeparateThread);
            powell.run();

            if (threadStopped) {
                return null;
            }

            item = new MatrixListItem(powell.getCost(), powell.getMatrix(), powell.getFinal());
            itemPtr = new MatrixListItem(powell.getCost(), powell.getMatrix(input.xRes), powell.getFinal(input.xRes));
            Preferences.debug("Level 2, after " + degree + " DOF: " + itemPtr + "\n");
            delinkProgressToAlgorithm(powell);

            if (DOF > 9) {
            	progressStart = progressEnd;
            	progressEnd = (int)(progressStart + 9 * progressStep);
                degree = 12;
                fireProgressStateChanged("Optimizing with " + degree + " DOF");
                powell = new AlgorithmPowellOpt3D(this, cog, 12, cost, item.initial, getTolerance(12), maxIter,
                                                  bracketBound);
                linkProgressToAlgorithm(powell);
                powell.setProgressValues(generateProgressValues(progressStart, progressEnd));
                powell.setRunningInSeparateThread(runningInSeparateThread);
                powell.run();

                if (threadStopped) {
                    return null;
                }

                item = new MatrixListItem(powell.getCost(), powell.getMatrix(), powell.getFinal());
                itemPtr = new MatrixListItem(powell.getCost(), powell.getMatrix(input.xRes),
                                             powell.getFinal(input.xRes));
                delinkProgressToAlgorithm(powell);
                Preferences.debug("Level 2, after " + degree + " DOF: " + itemPtr + "\n");
            }
        }

        fireProgressStateChanged((int)(progressTo));
        cost.disposeLocal();
        powell.disposeLocal();

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
    class MatrixListItem implements Comparable {

        /** Cost of function at this minimum. */
        protected double cost;

        /** Matrix with the best transformation divided by half. Might be null. */
        protected TransMatrix halfMatrix;

        /** Rotations, translations, scales, and skews that make up transformation. */
        protected double[] initial;

        /** Matrix that gives best transformation. */
        protected TransMatrix matrix;

        /** Matrix with the best transformation's z rot and xy translations. Might be null. */
        protected TransMatrix midsagMatrix;

        /**
         * Creates new minimum object, setting the data and copying the point array explicitly.
         *
         * @param  _cost     Cost of this minimum.
         * @param  _matrix   Matrix that gives best transformation.
         * @param  _initial  Rotations, translations, scales, and skews that make up transformation.
         */
        protected MatrixListItem(double _cost, TransMatrix _matrix, double[] _initial) {
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
        public int compareTo(Object o) {

            if (cost < ((MatrixListItem) o).cost) {
                return -1;
            } else if (cost > ((MatrixListItem) o).cost) {
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
                s += initial[i] + " ";
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
