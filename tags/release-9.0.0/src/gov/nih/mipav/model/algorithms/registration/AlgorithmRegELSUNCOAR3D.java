package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.ImageRegistrationGPU;

import java.util.*;
import java.util.concurrent.CountDownLatch;

import WildMagic.LibFoundation.Mathematics.Vector3f;


// import com.mentorgen.tools.profile.runtime.Profile;

/**
 * This is an automatic registration method based on FLIRT. FLIRT stands for FMRIB's Linear Image Registration Tool 1.3.
 * For more information on FLIRT, visit their homepage at <a href="http://www.fmrib.ox.ac.uk/fsl/flirt/">
 * http://www.fmrib.ox.ac.uk/fsl/flirt/</a>. Their main paper is:
 * 
 * <p>
 * Jenkinson, M. and Smith, S. (2001a).<br>
 * A global optimisation method for robust affine registration of brain images.<br>
 * <i>Medical Image Analysis</i>, 5(2):143-156.<br>
 * </p>
 * 
 * <p>
 * Our algorithm works as follows:<br>
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
 * <p>
 * Only subsample if 16 or more z slices are present so that the number of z slices will not be reduced below 8.
 * </p>
 * 
 * @author Neva Cherniavsky
 * @author Matthew McAuliffe
 */
public class AlgorithmRegELSUNCOAR3D extends AlgorithmBase implements AlgorithmInterface {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int minimumZForSub = 16;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

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
     * If true calculate the center of gravity (mass) and use the difference to intialize the translation. If false,
     * images are pretty much aligned then don't calculated COG.
     */
    private final boolean calcCOG = true;

    /** Number of passes that will be made in the coarse sampling and fine sampling. */
    private final int coarseNumX, fineNumX;

    /** DOCUMENT ME! */
    private final int coarseNumY, fineNumY;

    /** DOCUMENT ME! */
    private final int coarseNumZ, fineNumZ;

    /** Choice of which cost function to use. */
    private int costChoice;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** Maximum degrees of freedom when running the optimization. */
    private final int DOF;

    /** If true subsample for levelEight, levelFour and levelTwo analyses. */
    private boolean doSubsample = true;
    
    private boolean doMultiThread = true;

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
    private final int interp;

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

    /** Advanced optimization settings maxIter in the call to ELSUNC will be an integer multiple of baseNumIter. */
    private int maxIter;

    final int baseNumIter;
    
    private int searchAlgorithm;

    /**
     * Flag to determine if the maximum of the minimum resolutions of the two datasets should be used. If true use the
     * maximum resolution of the two dataset. Throws away information some image information but is faster. If false the
     * algorithms uses the minimum of the resolutions when resampling the images. Can be slower but does not "lose"
     * informaton.
     */
    private final boolean maxResol;

    /** Number of minima from level 8 to test at level 4. */
    private final int numMinima;

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
    private final float rotateBeginX, rotateEndX, coarseRateX, fineRateX;

    /** DOCUMENT ME! */
    private final float rotateBeginY, rotateEndY, coarseRateY, fineRateY;

    /** DOCUMENT ME! */
    private final float rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;

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

    private ImageRegistrationGPU m_kGPUCost = null;
    
    private CountDownLatch doneSignal;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     * 
     * @param _imageA Reference image (register input image to reference image).
     * @param _imageB Input image (register input image to reference image).
     * @param _costChoice Choice of cost functions, like correlation ratio or mutual information.
     * @param _DOF Degrees of freedom for registration
     * @param _interp Interpolation method used in transformations.
     * @param _rotateBeginX Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndX End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateX Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateX Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _rotateBeginY Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndY End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateY Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateY Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _rotateBeginZ Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndZ End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateZ Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateZ Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _maxResol If true is the maximum of the minimum resolution of the two datasets when resampling.
     * @param _doSubsample If true then subsample
     * @param _fastMode If true then searching the parameter space is not conducted and the algorithm proceeds to level
     *            one immediately
     * @param _baseNumIter Limits the number of iterations of ELSUNC algorithm. maxIter in the call to ELSUNC will
     *            be an integer multiple of baseNumIter
     * @param _numMinima Number of minima from level 8 to test at level 4
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     */
    public AlgorithmRegELSUNCOAR3D(final ModelImage _imageA, final ModelImage _imageB, final int _costChoice, final int _DOF,
            final int _interp, final float _rotateBeginX, final float _rotateEndX, final float _coarseRateX,
            final float _fineRateX, final float _rotateBeginY, final float _rotateEndY, final float _coarseRateY,
            final float _fineRateY, final float _rotateBeginZ, final float _rotateEndZ, final float _coarseRateZ,
            final float _fineRateZ, final boolean _maxResol, final boolean _doSubsample, 
            final boolean _fastMode, final int _baseNumIter, final int _numMinima, final int searchAlgorithm) {

        this(_imageA, _imageB, _costChoice, _DOF, _interp,
            _rotateBeginX, _rotateEndX, _coarseRateX, _fineRateX,
            _rotateBeginY, _rotateEndY, _coarseRateY, _fineRateY,
            _rotateBeginZ, _rotateEndZ, _coarseRateZ, _fineRateZ,
            _maxResol, _doSubsample, false,
            _fastMode, _baseNumIter, _numMinima, searchAlgorithm);  
    }
    
    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     * 
     * @param _imageA Reference image (register input image to reference image).
     * @param _imageB Input image (register input image to reference image).
     * @param _costChoice Choice of cost functions, like correlation ratio or mutual information.
     * @param _DOF Degrees of freedom for registration
     * @param _interp Interpolation method used in transformations.
     * @param _rotateBeginX Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndX End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateX Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateX Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _rotateBeginY Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndY End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateY Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateY Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _rotateBeginZ Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndZ End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateZ Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateZ Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _maxResol If true is the maximum of the minimum resolution of the two datasets when resampling.
     * @param _doSubsample If true then subsample
     * @param _doMultiThread
     * @param _fastMode If true then searching the parameter space is not conducted and the algorithm proceeds to level
     *            one immediately
     * @param _baseNumIter Limits the number of iterations of ELSUNC algorithm. maxIter in the call to ELSUNC will
     *            be an integer multiple of baseNumIter
     * @param _numMinima Number of minima from level 8 to test at level 4
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     */
    public AlgorithmRegELSUNCOAR3D(final ModelImage _imageA, final ModelImage _imageB, final int _costChoice, final int _DOF,
            final int _interp, final float _rotateBeginX, final float _rotateEndX, final float _coarseRateX,
            final float _fineRateX, final float _rotateBeginY, final float _rotateEndY, final float _coarseRateY,
            final float _fineRateY, final float _rotateBeginZ, final float _rotateEndZ, final float _coarseRateZ,
            final float _fineRateZ, final boolean _maxResol, final boolean _doSubsample, final boolean _doMultiThread,
            final boolean _fastMode, final int _baseNumIter, final int _numMinima, final int searchAlgorithm) {

        this(_imageA, _imageB, null, null, _costChoice, _DOF, _interp,
            _rotateBeginX, _rotateEndX, _coarseRateX, _fineRateX,
            _rotateBeginY, _rotateEndY, _coarseRateY, _fineRateY,
            _rotateBeginZ, _rotateEndZ, _coarseRateZ, _fineRateZ,
            _maxResol, _doSubsample, _doMultiThread,
            _fastMode, _baseNumIter, _numMinima, searchAlgorithm);  
        weighted = false;
    }

    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     * 
     * @param _imageA Reference image (register input image to reference image).
     * @param _imageB Input image (register input image to reference image).
     * @param _refWeight Reference weighted image, used to give certain areas of the image greater impact on the
     *            registration.
     * @param _inputWeight Input weighted image, used to give certain areas of the image greater impact on the
     *            registration.
     * @param _costChoice Choice of cost functions, like correlation ratio or mutual information.
     * @param _DOF Degrees of freedom for registration
     * @param _interp Interpolation method used in transformations.
     * @param _rotateBeginX Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndX End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateX Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateX Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _rotateBeginY Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndY End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateY Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateY Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _rotateBeginZ Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndZ End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateZ Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateZ Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _maxResol If true is the maximum of the minimum resolution of the two datasets when resampling.
     * @param _doSubsample If true then subsample
     * @param _fastMode If true then searching the parameter space is not conducted and the algorithm proceeds to level
     *            one immediately
     * @param _baseNumIter Limits the number of iterations of ELSUNC algorithm. maxIter in the call to ELSUNC will
     *            be an integer multiple of baseNumIter
     * @param _numMinima Number of minima from level 8 to test at level 4
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     */
    public AlgorithmRegELSUNCOAR3D(final ModelImage _imageA, final ModelImage _imageB, final ModelImage _refWeight,
            final ModelImage _inputWeight, final int _costChoice, final int _DOF, final int _interp,
            final float _rotateBeginX, final float _rotateEndX, final float _coarseRateX, final float _fineRateX,
            final float _rotateBeginY, final float _rotateEndY, final float _coarseRateY, final float _fineRateY,
            final float _rotateBeginZ, final float _rotateEndZ, final float _coarseRateZ, final float _fineRateZ,
            final boolean _maxResol, final boolean _doSubsample, 
            final boolean _fastMode, final int _baseNumIter, final int _numMinima, final int searchAlgorithm) {
    	this(_imageA, _imageB, _refWeight, _inputWeight, _costChoice, _DOF, _interp,
                _rotateBeginX, _rotateEndX, _coarseRateX, _fineRateX,
                _rotateBeginY, _rotateEndY, _coarseRateY, _fineRateY,
                _rotateBeginZ, _rotateEndZ, _coarseRateZ, _fineRateZ,
                _maxResol, _doSubsample, false,
                _fastMode, _baseNumIter, _numMinima, searchAlgorithm);  
    }
    
    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     * 
     * @param _imageA Reference image (register input image to reference image).
     * @param _imageB Input image (register input image to reference image).
     * @param _refWeight Reference weighted image, used to give certain areas of the image greater impact on the
     *            registration.
     * @param _inputWeight Input weighted image, used to give certain areas of the image greater impact on the
     *            registration.
     * @param _costChoice Choice of cost functions, like correlation ratio or mutual information.
     * @param _DOF Degrees of freedom for registration
     * @param _interp Interpolation method used in transformations.
     * @param _rotateBeginX Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndX End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateX Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateX Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _rotateBeginY Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndY End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateY Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateY Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _rotateBeginZ Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEndZ End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRateZ Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRateZ Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param _maxResol If true is the maximum of the minimum resolution of the two datasets when resampling.
     * @param _doSubsample If true then subsample
     * @param _doMultiThread
     * @param _fastMode If true then searching the parameter space is not conducted and the algorithm proceeds to level
     *            one immediately
     * @param _baseNumIter Limits the number of iterations of ELSUNC algorithm. maxIter in the call to ELSUNC will
     *            be an integer multiple of baseNumIter
     * @param _numMinima Number of minima from level 8 to test at level 4
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     */
    public AlgorithmRegELSUNCOAR3D(final ModelImage _imageA, final ModelImage _imageB, final ModelImage _refWeight,
            final ModelImage _inputWeight, final int _costChoice, final int _DOF, final int _interp,
            final float _rotateBeginX, final float _rotateEndX, final float _coarseRateX, final float _fineRateX,
            final float _rotateBeginY, final float _rotateEndY, final float _coarseRateY, final float _fineRateY,
            final float _rotateBeginZ, final float _rotateEndZ, final float _coarseRateZ, final float _fineRateZ,
            final boolean _maxResol, final boolean _doSubsample, final boolean _doMultiThread,
            final boolean _fastMode, final int _baseNumIter, final int _numMinima, final int searchAlgorithm) {
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
        coarseNumX = (int) ( (_rotateEndX - rotateBeginX) / coarseRateX) + 1;
        fineNumX = (int) ( (_rotateEndX - rotateBeginX) / fineRateX) + 1;
        rotateBeginY = _rotateBeginY;
        rotateEndY = _rotateEndY;
        coarseRateY = _coarseRateY;
        fineRateY = _fineRateY;
        coarseNumY = (int) ( (_rotateEndY - rotateBeginY) / coarseRateY) + 1;
        fineNumY = (int) ( (_rotateEndY - rotateBeginY) / fineRateY) + 1;
        rotateBeginZ = _rotateBeginZ;
        rotateEndZ = _rotateEndZ;
        coarseRateZ = _coarseRateZ;
        fineRateZ = _fineRateZ;
        coarseNumZ = (int) ( (_rotateEndZ - rotateBeginZ) / coarseRateZ) + 1;
        fineNumZ = (int) ( (_rotateEndZ - rotateBeginZ) / fineRateZ) + 1;
        
        maxResol = _maxResol;
        doSubsample = _doSubsample;
        doMultiThread = _doMultiThread;
        fastMode = _fastMode;

        if (fastMode) {
            fullAnalysisMode = false;
        }
        
        weighted = true;

        baseNumIter = _baseNumIter;
        numMinima = _numMinima;
        this.searchAlgorithm = searchAlgorithm;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the center of mass (gravity) of a 3D image. In image space where the upper left hand corner of the
     * image is 0,0. The x axis goes left to right, y axis goes top to bottom and z axis goes into the screen. (i.e. the
     * right hand rule). One could simply multiply by voxel resolutions.
     * 
     * @param image the center of mass will be calculated from this image data
     * @param wgtImage DOCUMENT ME!
     * @param isColor DOCUMENT ME!
     * 
     * @return the center of mass as a 3D point
     */
    public static Vector3f calculateCenterOfMass3D(final ModelSimpleImage image, final ModelSimpleImage wgtImage,
            final boolean isColor) {
        int x, y, z, c;
        final int sliceSize = image.xDim * image.yDim;
        float diff;

        final Vector3f cogPt = new Vector3f(0, 0, 0);
        double voxVal = 0.0, total = 0.0, wgtVal = 0.0;

        if (isColor) {

            if (wgtImage == null) {

                for (z = 0; z < image.zDim; z++) {

                    for (y = 0; y < image.yDim; y++) {

                        for (x = 0; x < image.xDim; x++) {

                            for (c = 1; c <= 3; c++) {
                                voxVal = image.data[ (4 * ( (z * sliceSize) + (y * image.xDim) + x)) + c];
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

                if ( (wgtImage.min < 0) || (wgtImage.max > 1)) {

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
                            wgtVal = wgtImage.data[ (z * sliceSize) + (y * image.xDim) + x];

                            for (c = 1; c <= 3; c++) {
                                voxVal = image.data[ (4 * ( (z * sliceSize) + (y * image.xDim) + x)) + c];
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
                            voxVal = image.data[ (z * sliceSize) + (y * image.xDim) + x] - image.min;
                            cogPt.X += voxVal * x;
                            cogPt.Y += voxVal * y;
                            cogPt.Z += voxVal * z;
                            total += voxVal;
                        }
                    }
                }
            } else { // wgtImage != null

                wgtImage.calcMinMax();

                if ( (wgtImage.min < 0) || (wgtImage.max > 1)) {

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
                            voxVal = image.data[ (z * sliceSize) + (y * image.xDim) + x] - image.min;
                            wgtVal = wgtImage.data[ (z * sliceSize) + (y * image.xDim) + x];
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

        simpleWeightRef = null;
        simpleWeightInput = null;
        simpleWeightRefSub2 = null;
        simpleWeightInputSub2 = null;
        simpleWeightRefSub4 = null;
        simpleWeightInputSub4 = null;
        simpleWeightRefSub8 = null;
        simpleWeightInputSub8 = null;

        if ( (blurredRef != refImage) && (blurredRef != null)) {
            blurredRef.disposeLocal();
        } else {
            blurredRef = null;
        }

        if ( (blurredInput != inputImage) && (blurredInput != null)) {
            blurredInput.disposeLocal();
        } else {
            blurredInput = null;
        }

        if (imageRefIso != null) {
            imageRefIso.disposeLocal();
        } else {

            if ( (transform != null) && (transform.getTransformedImage() != null)) {
                transform.getTransformedImage().disposeLocal();
                transform = null;
            }
        }

        if (imageInputIso != null) {
            imageInputIso.disposeLocal();
        } else {

            if ( (transform2 != null) && (transform2.getTransformedImage() != null)) {
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
     * @return Matrix found at the end of algorithm.
     */
    public double getAnswer() {
        return answer.cost;
    }

    /**
     * Access that returns an array containing the transformation parameters.
     * 
     * @return transformation array (0-2 rot, 3-5 trans, 6-9 scale, 9-12 skew)
     */
    public double[] getTransArray() {
        return answer.initial;
    }

    /**
     * Accessor that returns the matrix calculated in this algorithm.
     * 
     * @return Matrix found at the end of algorithm.
     */
    public TransMatrix getTransform() {
        TransMatrixd tMatd = answer.matrixd;
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
     * @return Matrix found at the end of algorithm with the compoents halved.
     */
    public TransMatrix getTransformHalf() {
        TransMatrixd tMatd = answer.halfMatrixd;
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
     * @return z rotation and x and y translations from the matrix found at the end of algorithm.
     */
    public TransMatrix getTransformMigsagittal() {
        TransMatrixd tMatd = answer.midsagMatrixd;
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
        final long startTime = System.currentTimeMillis();
        int i;

        // Profile.clear();
        // Profile.start();

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
        
        refImage.makeUnitsOfMeasureIdentical();
        inputImage.makeUnitsOfMeasureIdentical();

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

        if ( (resRef[0] == resRef[1]) && (resRef[0] == resRef[2]) && (resRef[0] == minSample)) {
            resampleRef = false;
        } else {
            resampleRef = true;
        }

        if ( (resInput[0] == resInput[1]) && (resInput[0] == resInput[2]) && (resInput[0] == minSample)) {
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
        } catch (final OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("Out of memory in AlgorithmOAR3D.");
            disposeLocal();
            setCompleted(false);

            return;
        }

        for (i = 0; i < extentsRefIso.length; i++) {
            extentsRefIso[i] = (int) ( (refImage.getExtents()[i] - 1) / (minSample / resRef[i])) + 1;
            resRefIso[i] = minSample;
            extentsInputIso[i] = (int) ( (inputImage.getExtents()[i] - 1) / (minSample / resInput[i])) + 1;
            resInputIso[i] = minSample;
        }

        boolean blurRef = false;
        boolean blurInput = false;

        for (i = 0; i < sigmasRef.length; i++) {
            sigmasRef[i] = ( (resInput[i] - resRef[i]) / resRef[i]) * 0.6f; // 2.36 * sigma = x --> FWHM 0.424 =
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

            sigmasInput[i] = ( (resRef[i] - resInput[i]) / resInput[i]) * 0.6f; // 2.36 * sigma = x --> FWHM 0.424 =
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
                blurredRef = new ModelImage(ModelStorageBase.ARGB_FLOAT, refImage.getExtents(), "BlurRef");
            } else {
                blurredRef = new ModelImage(ModelStorageBase.FLOAT, refImage.getExtents(), "BlurRef");
            }

            // update resolutions
            final FileInfoBase[] fileInfo = blurredRef.getFileInfo();

            for (i = 0; i < refImage.getExtents()[2]; i++) {
                fileInfo[i].setResolutions(refImage.getFileInfo()[i].getResolutions());
            }

            final AlgorithmGaussianBlur blur = new AlgorithmGaussianBlur(blurredRef, refImage, sigmasRef, true, false);

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
                blurredInput = new ModelImage(ModelStorageBase.ARGB_FLOAT, inputImage.getExtents(), "BlurInput");
            } else {
                blurredInput = new ModelImage(ModelStorageBase.FLOAT, inputImage.getExtents(), "BlurInput");
            }

            // update resolutions
            final FileInfoBase[] fileInfo = blurredInput.getFileInfo();

            for (i = 0; i < inputImage.getExtents()[2]; i++) {
                fileInfo[i].setResolutions(inputImage.getFileInfo()[i].getResolutions());
            }

            final AlgorithmGaussianBlur blur2 = new AlgorithmGaussianBlur(blurredInput, inputImage, sigmasInput, true,
                    false);

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
                    resRefIso[2], extentsRefIso[0], extentsRefIso[1], extentsRefIso[2], false, true, false);
            transform.setRunningInSeparateThread(runningInSeparateThread);
            transform.run();

            if (transform.isCompleted() == false) {
                transform.finalize();
                transform = null;
                setCompleted(false);
                finalize();

                return;
            }

            if ( (blurredRef != refImage) && (blurredRef != null)) {
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

        if ( (weighted)
                && ( (resampleRef) || ( (refWeight.getFileInfo(0).getResolutions()[0] != refImage.getFileInfo(0)
                        .getResolutions()[0])
                        || (refWeight.getFileInfo(0).getResolutions()[1] != refImage.getFileInfo(0).getResolutions()[1]) || (refWeight
                        .getFileInfo(0).getResolutions()[2] != refImage.getFileInfo(0).getResolutions()[2])))) {
            transform = new AlgorithmTransform(refWeight, new TransMatrix(4), interp, resRefIso[0], resRefIso[1],
                    resRefIso[2], extentsRefIso[0], extentsRefIso[1], extentsRefIso[2], false, true, false);
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

            simpleWeightRef = new ModelSimpleImage(imageWeightRefIso.getExtents(), imageWeightRefIso.getFileInfo(0)
                    .getResolutions(), imageWeightRefIso);

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
                    resInputIso[1], resInputIso[2], extentsInputIso[0], extentsInputIso[1], extentsInputIso[2], false,
                    true, false);
            transform2.setRunningInSeparateThread(runningInSeparateThread);
            transform2.run();

            if (transform2.isCompleted() == false) {
                transform2.finalize();
                transform2 = null;
                setCompleted(false);
                finalize();

                return;
            }

            if ( (blurredInput != inputImage) && (blurredInput != null)) {
                blurredInput.disposeLocal();
            } else {
                blurredInput = null;
            }

            System.gc();

            imageInputIso = transform2.getTransformedImage();

            if (transform2 != null) {
                transform2.finalize();
            }

            simpleInput = new ModelSimpleImage(imageInputIso.getExtents(), imageInputIso.getFileInfo(0)
                    .getResolutions(), imageInputIso);
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

        if ( (weighted)
                && ( (resampleInput) || ( (inputWeight.getFileInfo(0).getResolutions()[0] != inputImage.getFileInfo(0)
                        .getResolutions()[0])
                        || (inputWeight.getFileInfo(0).getResolutions()[1] != inputImage.getFileInfo(0)
                                .getResolutions()[1]) || (inputWeight.getFileInfo(0).getResolutions()[2] != inputImage
                        .getFileInfo(0).getResolutions()[2])))) {
            transform2 = new AlgorithmTransform(inputWeight, new TransMatrix(4), interp, resInputIso[0],
                    resInputIso[1], resInputIso[2], extentsInputIso[0], extentsInputIso[1], extentsInputIso[2], false,
                    true, false);
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

            simpleWeightInput = new ModelSimpleImage(imageWeightInputIso.getExtents(), imageWeightInputIso.getFileInfo(
                    0).getResolutions(), imageWeightInputIso);

            if (imageWeightInputIso != null) {
                imageWeightInputIso.disposeLocal();
            }
        } else if (weighted) {
            simpleWeightInput = new ModelSimpleImage(inputWeight.getExtents(), inputWeight.getFileInfo(0)
                    .getResolutions(), inputWeight);
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

            if ( (weightedRefPixels > subMinFactor) && (weightedInputPixels > subMinFactor)
                    && (simpleWeightRef.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub)
                    && (simpleWeightInput.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub) && doSubsample) {
                simpleWeightRefSub2 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleWeightRef, false);
                simpleWeightInputSub2 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleWeightInput, false);
            } else if ( (weightedRefPixels > subMinFactor) && (weightedInputPixels > subMinFactor) && doSubsample) {
                simpleWeightRefSub2 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightRef, false);
                simpleWeightInputSub2 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightInput, false);
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

            if ( (weightedRefPixelsSub2 > subMinFactor) && (weightedInputPixelsSub2 > subMinFactor)
                    && (simpleWeightRefSub2.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub)
                    && (simpleWeightInputSub2.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub) && doSubsample) {
                simpleWeightRefSub4 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleWeightRefSub2, false);
                simpleWeightInputSub4 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleWeightInputSub2, false);
            } else if ( (weightedRefPixelsSub2 > subMinFactor) && (weightedInputPixelsSub2 > subMinFactor)
                    && doSubsample) {
                simpleWeightRefSub4 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightRefSub2, false);
                simpleWeightInputSub4 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightInputSub2, false);
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

            if ( (weightedRefPixelsSub4 > subMinFactor) && (weightedInputPixelsSub4 > subMinFactor)
                    && (simpleWeightRefSub4.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub)
                    && (simpleWeightInputSub4.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub) && doSubsample) {
                simpleWeightRefSub8 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleWeightRefSub4, false);
                simpleWeightInputSub8 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleWeightInputSub4, false);

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

                if ( (weightedRefPixelsSub8 > subMinFactor) && (weightedInputPixelsSub8 > subMinFactor)
                        && (simpleWeightRefSub8.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub)
                        && (simpleWeightInputSub8.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub) && doSubsample) {
                    ModelSimpleImage simpleWeightRefSub16;
                    ModelSimpleImage simpleWeightInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16  ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleWeightRefSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleWeightRefSub8, false);
                    simpleWeightInputSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleWeightInputSub8, false);

                    simpleWeightRefSub8 = simpleWeightRefSub16;
                    simpleWeightInputSub8 = simpleWeightInputSub16;
                } else if ( (weightedRefPixelsSub8 > subMinFactor) && (weightedInputPixelsSub8 > subMinFactor)
                        && doSubsample) {
                    ModelSimpleImage simpleWeightRefSub16;
                    ModelSimpleImage simpleWeightInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleWeightRefSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightRefSub8, false);
                    simpleWeightInputSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightInputSub8, false);

                    simpleWeightRefSub8 = simpleWeightRefSub16;
                    simpleWeightInputSub8 = simpleWeightInputSub16;
                    allowLevel16Z = false;
                } else {
                    allowLevel16XY = false;
                    allowLevel16Z = false;
                }
            } else if ( (weightedRefPixelsSub4 > subMinFactor) && (weightedInputPixelsSub4 > subMinFactor)
                    && doSubsample) {
                simpleWeightRefSub8 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightRefSub4, false);
                simpleWeightInputSub8 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightInputSub4, false);
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

                if ( (weightedRefPixelsSub8 > subMinFactor) && (weightedInputPixelsSub8 > subMinFactor) && doSubsample) {
                    ModelSimpleImage simpleWeightRefSub16;
                    ModelSimpleImage simpleWeightInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleWeightRefSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightRefSub8, false);
                    simpleWeightInputSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleWeightInputSub8, false);

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

            Preferences.debug("Weighted ref subsampled 2 = " + simpleWeightRefSub2 + "Weighted ref subsampled 4 = "
                    + simpleWeightRefSub4 + "Weighted ref subsampled 8 = " + simpleWeightRefSub8
                    + "Weighted input subsampled 2 = " + simpleWeightInputSub2 + "Weighted input subsampled 4 = "
                    + simpleWeightInputSub4 + "Weighted input subsampled 8 = " + simpleWeightInputSub8,
                    Preferences.DEBUG_ALGORITHM);

        } // end of (if weighted && fullAnalysisMode)

        if (costChoice >= AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU) {
            m_kGPUCost = ImageRegistrationGPU.create(simpleRef, simpleInput);
            if (m_kGPUCost == null) {
                MipavUtil.displayError("Not enough memory on the GPU, reverting to CPU registration");
                costChoice = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
            }
        }
        if (fullAnalysisMode) {

            if ( (simpleRef.dataSize > subMinFactor) && (simpleInput.dataSize > subMinFactor) && allowLevel2XY
                    && allowLevel2Z && (simpleRef.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub)
                    && (simpleInput.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub) && doSubsample) {
                simpleRefSub2 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleRef, doColor);
                simpleInputSub2 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleInput, doColor);
                level1FactorXY = 2.0f;
                level1FactorZ = 2.0f;
            } else if ( (simpleRef.dataSize > subMinFactor) && (simpleInput.dataSize > subMinFactor) && allowLevel2XY
                    && doSubsample) {
                simpleRefSub2 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleRef, doColor);
                simpleInputSub2 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleInput, doColor);
                level1FactorXY = 2.0f;
            } else {
                Preferences.debug("Level one image not resampled because ",Preferences.DEBUG_ALGORITHM);

                if (simpleRef.dataSize <= subMinFactor) {
                    Preferences.debug("reference image data size is too small. \n",Preferences.DEBUG_ALGORITHM);
                }

                if (simpleInput.dataSize <= subMinFactor) {
                    Preferences.debug("input image data size is too small. \n",Preferences.DEBUG_ALGORITHM);
                }

                simpleRefSub2 = simpleRef;
                simpleInputSub2 = simpleInput;
            }

            if ( (simpleRefSub2.dataSize > subMinFactor) && (simpleInputSub2.dataSize > subMinFactor) && allowLevel4XY
                    && allowLevel4Z && (simpleRefSub2.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub)
                    && (simpleInputSub2.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub) && doSubsample) {
                simpleRefSub4 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleRefSub2, doColor);
                simpleInputSub4 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleInputSub2, doColor);
                level2FactorXY = 2.0f;
                level2FactorZ = 2.0f;
            } else if ( (simpleRefSub2.dataSize > subMinFactor) && (simpleInputSub2.dataSize > subMinFactor)
                    && allowLevel4XY && doSubsample) {
                simpleRefSub4 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleRefSub2, doColor);
                simpleInputSub4 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleInputSub2, doColor);
                level2FactorXY = 2.0f;
            } else {
                Preferences.debug("Level two image not resampled because ",Preferences.DEBUG_ALGORITHM);

                if (simpleRefSub2.dataSize <= subMinFactor) {
                    Preferences.debug("reference image data size is too small. \n",Preferences.DEBUG_ALGORITHM);
                }

                if (simpleInputSub2.dataSize <= subMinFactor) {
                    Preferences.debug("input image data size is too small. \n",Preferences.DEBUG_ALGORITHM);
                }

                simpleRefSub4 = simpleRefSub2;
                simpleInputSub4 = simpleInputSub2;
            }

            if ( (simpleRefSub4.dataSize > subMinFactor) && (simpleInputSub4.dataSize > subMinFactor) && allowLevel8XY
                    && allowLevel8Z && (simpleRefSub4.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub)
                    && (simpleInputSub4.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub) && doSubsample) {
                simpleRefSub8 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleRefSub4, doColor);
                simpleInputSub8 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleInputSub4, doColor);
                level4FactorXY = 2.0f;
                level4FactorZ = 2.0f;

                // For really big images subsample level 8 again!
                if ( (simpleRefSub8.dataSize > subMinFactor) && (simpleInputSub8.dataSize > subMinFactor)
                        && allowLevel16XY && allowLevel16Z && (simpleRefSub8.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub)
                        && (simpleInputSub8.zDim >= AlgorithmRegELSUNCOAR3D.minimumZForSub)) {
                    ModelSimpleImage simpleRefSub16;
                    ModelSimpleImage simpleInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16  ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleRefSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleRefSub8, doColor);
                    simpleInputSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2(simpleInputSub8, doColor);

                    simpleRefSub8 = simpleRefSub16;
                    simpleInputSub8 = simpleInputSub16;
                    level4FactorXY = 4.0f;
                    level4FactorZ = 4.0f;
                } else if ( (simpleRefSub8.dataSize > subMinFactor) && (simpleInputSub8.dataSize > subMinFactor)
                        && allowLevel16XY) {
                    ModelSimpleImage simpleRefSub16;
                    ModelSimpleImage simpleInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleRefSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleRefSub8, doColor);
                    simpleInputSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleInputSub8, doColor);

                    simpleRefSub8 = simpleRefSub16;
                    simpleInputSub8 = simpleInputSub16;
                    level4FactorXY = 4.0f;
                }
            } else if ( (simpleRefSub4.dataSize > subMinFactor) && (simpleInputSub4.dataSize > subMinFactor)
                    && allowLevel8XY && doSubsample) {
                simpleRefSub8 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleRefSub4, doColor);
                simpleInputSub8 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleInputSub4, doColor);
                level4FactorXY = 2.0f;

                // For really big images subsample level 8 again!
                if ( (simpleRefSub8.dataSize > subMinFactor) && (simpleInputSub8.dataSize > subMinFactor)
                        && allowLevel16XY) {
                    ModelSimpleImage simpleRefSub16;
                    ModelSimpleImage simpleInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16 in XY ***********\n",Preferences.DEBUG_ALGORITHM);

                    simpleRefSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleRefSub8, doColor);
                    simpleInputSub16 = AlgorithmRegELSUNCOAR3D.subsampleBy2XY(simpleInputSub8, doColor);

                    simpleRefSub8 = simpleRefSub16;
                    simpleInputSub8 = simpleInputSub16;
                    level4FactorXY = 4.0f;
                }
            } else {
                simpleRefSub8 = simpleRefSub4;
                simpleInputSub8 = simpleInputSub4;
            }

            Preferences.debug("Level 1 factor XY = " + level1FactorXY + "\n" + "Level 1 factor Z = " + level1FactorZ
                    + "\n" + "Level 2 factor XY = " + level2FactorXY + "\n" + "Level 2 factor Z = " + level2FactorZ
                    + "\n" + "Level 4 factor XY = " + level4FactorXY + "\n" + "Level 4 factor Z = " + level4FactorZ
                    + "\n" + "Ref subsampled 2 = " + simpleRefSub2 + "Ref subsampled 4 = " + simpleRefSub4
                    + "Ref subsampled 8 = " + simpleRefSub8 + "Input subsampled 2 = " + simpleInputSub2
                    + "Input subsampled 4 = " + simpleInputSub4 + "Input subsampled 8 = " + simpleInputSub8,
                    Preferences.DEBUG_ALGORITHM);

            // STARTING LEVEL 8
            time = System.currentTimeMillis();
            Preferences.debug(" Starting level 8 ************************************************\n",Preferences.DEBUG_ALGORITHM);

            final Vector<MatrixListItem>[] minimas = levelEight(simpleRefSub8, simpleInputSub8, 0, 30);

            // "minimas" is an array of Vector, because it will have two Vectors - one with
            // the original minima and one with the optimized minima.
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 8 milliseconds = " + time + "\n",Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }

            // STARTING LEVEL 4
            Preferences.debug(" Starting level 4 ************************************************\n",Preferences.DEBUG_ALGORITHM);

            final Vector<MatrixListItem> minima = levelFour(simpleRefSub4, simpleInputSub4, minimas[0], minimas[1], 30,
                    60);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 4  milliseconds = " + time + "\n",Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }

            // STARTING LEVEL 2
            Preferences.debug(" Starting level 2 ************************************************\n",Preferences.DEBUG_ALGORITHM);
            bestGuessLevel2 = levelTwo(simpleRefSub2, simpleInputSub2, minima, 60, 90);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 2 milliseconds = " + time + "\n",Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }
        } // end of if (fullAnalysisMode)
        else { // if (fastMode) setup item to be to "first best guess" = identity

            final double[] initial = new double[12];
            bestGuessLevel2 = new MatrixListItem(0, new TransMatrix(4), initial);

            double diffX = 0;
            double diffY = 0;
            double diffZ = 0;

            // change this
            if (calcCOG) {
                final Vector3f cog = AlgorithmRegELSUNCOAR3D.calculateCenterOfMass3D(simpleInput, simpleWeightInput, doColor);
                final Vector3f cogR = AlgorithmRegELSUNCOAR3D.calculateCenterOfMass3D(simpleRef, simpleWeightRef, doColor);
                Preferences.debug("Center of mass for the subsampled input image:" + cog + "\n",Preferences.DEBUG_ALGORITHM);
                Preferences.debug("Center of mass for the subsampled reference image:" + cogR + "\n",Preferences.DEBUG_ALGORITHM);

                diffX = (cog.X - cogR.X);
                diffY = (cog.Y - cogR.Y);
                diffZ = (cog.Z - cogR.Z);
            }

            bestGuessLevel2.initial[0] = bestGuessLevel2.initial[1] = bestGuessLevel2.initial[2] = 0; // initial
            // rotation
            bestGuessLevel2.initial[3] = diffX; // initial translations
            bestGuessLevel2.initial[4] = diffY;
            bestGuessLevel2.initial[5] = diffZ;
            bestGuessLevel2.initial[6] = bestGuessLevel2.initial[7] = bestGuessLevel2.initial[8] = 1; // initial
            // scaling
            bestGuessLevel2.initial[9] = bestGuessLevel2.initial[10] = bestGuessLevel2.initial[11] = 0; // initial
            // skewing
        } // end of (fastMode)

        // STARTING LEVEL 0NE - note - this is for fastMode and fullAnalysisMode
        Preferences.debug(" Starting level 1 ************************************************\n",Preferences.DEBUG_ALGORITHM);
        maxIter = baseNumIter * 1;
        answer = levelOne(simpleRef, simpleInput, bestGuessLevel2, maxIter, 90, 100);
        time = System.currentTimeMillis() - time;
        Preferences.debug(" Level 1 milliseconds = " + time + "\n",Preferences.DEBUG_ALGORITHM);

        if (threadStopped) {
            finalize();

            return;
        }

        answer.matrixd.Inverse();
        fireProgressStateChanged(100);
        disposeLocal();
        finalize();
        setCompleted(true);
        Preferences.debug("Time consumed by OAR registration algorithm: " + (System.currentTimeMillis() - startTime),
        		Preferences.DEBUG_ALGORITHM);
        time = (System.currentTimeMillis() - startTime);
        Preferences.debug("Time consumed by OAR registration algorithm: " + (time * .001f) + " seconds",
        		Preferences.DEBUG_ALGORITHM);

        if (m_kGPUCost != null) {
            m_kGPUCost.dispose();
            m_kGPUCost = null;
        }
        // Profile.stop();
        // Profile.setFileName( "profile_out" );
        // Profile.shutdown();
    }

   

    /**
     * Takes a simple image and subsamples it by 2, interpolating so that the new values are averages.
     * 
     * @param srcImage Image to subsample.
     * @param isColor DOCUMENT ME!
     * 
     * @return Subsampled image.
     */
    public static ModelSimpleImage subsampleBy2(final ModelSimpleImage srcImage, final boolean isColor) {
        return srcImage.subsample3dBy2(isColor);
    }

    /**
     * Takes a simple image and subsamples XY by 2, interpolating so that the new XY values are averages.
     * 
     * @param srcImage Image to subsample.
     * @param isColor DOCUMENT ME!
     * 
     * @return Subsampled image.
     */
    public static ModelSimpleImage subsampleBy2XY(final ModelSimpleImage srcImage, final boolean isColor) {
        return srcImage.subsample3dBy2XY(isColor);
    }

    /**
     * Creates a string with the parameters that the image was constructed with.
     * 
     * @return Construction info.
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

            case AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU:
                s += "Normalized mutual information GPU, ";
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

        s += rotateBeginX + ", " + coarseRateX + ", " + fineRateX + ", " + rotateBeginY + ", " + coarseRateY + ", "
                + fineRateY + ", " + rotateBeginZ + ", " + coarseRateZ + ", " + fineRateZ + ", " + maxResol + ", "
                + fastMode + ", " + calcCOG + ")\n";
        return s;
    }

    /**
     * Gets the tolerance vector based on the degrees of freedom (the length of the tolerance is the degrees of freedom)
     * and the level of subsampling (1, 2, 4, 8).
     * 
     * @param DOF Degrees of freedom, will be length of vector.
     * 
     * @return New tolerance vector to send to optimization.
     * 
     * <p>
     * Based on FLIRT paper: let n=pixel dimension (in one dimension) R=brain radius, here assumed to be half of
     * field-of-view Translation tolerance = n/2 Rotation tolerance = (180/PI)*n/(2R) (converted to degrees because
     * AlgorithmELSUNC works in degrees) Scaling tolerance = n/(2R) Skewing tolerance = n/(2R)
     * </p>
     */
    private double[] getTolerance(final int DOF) {
        final double[] tols = new double[DOF];
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

                if ( (i / 3) == 0) {
                    tols[i] = ( (180. / Math.PI) / maxDim);
                } // rotation tolerances
                else if ( (i / 3) == 1) { // translation tolerances
                    tols[i] = 0.5;
                } else if ( (i / 3) == 2) { // scaling tolerances
                    tols[i] = 0.005;
                } else if ( (i / 3) == 3) { // skewing tolerances
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
     * @param x X rotation initial index into array.
     * @param y Y rotation initial index into array.
     * @param z Z rotation initial index into array.
     * @param initial Vector to set; if scale is <code>true</code>, set three translations and a scale. Otherwise
     *            just set translations.
     * @param tranforms DOCUMENT ME!
     * @param scale <code>true</code> means set the scale in the vector.
     */
    private void interpolate(final double x, final double y, final double z, final double[] initial,
            final double[][][][] tranforms, final boolean scale) {
        int ix0, iy0, iz0, ix1, iy1, iz1;

        // convert to closest integer values to access proper parts of array
        ix0 = (int) Math.floor(x);
        iy0 = (int) Math.floor(y);
        iz0 = (int) Math.floor(z);
        ix1 = ix0 + 1;
        iy1 = iy0 + 1;
        iz1 = iz0 + 1;

        // can't be bigger than 3, size of array is 4x4x4
        if ( (ix0 == (coarseNumX - 1))) {
            ix1 = ix0;
        }

        if ( (iy0 == (coarseNumY - 1))) {
            iy1 = iy0;
        }

        if ( (iz0 == (coarseNumZ - 1))) {
            iz1 = iz0;
        }

        if (scale) {

            // x translation
            initial[3] = ( ( (x - ix0) * ( ( (y - iy0) * ( (tranforms[ix1][iy1][iz1][1] * (z - iz0)) + (tranforms[ix1][iy1][iz0][1] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix1][iy0][iz1][1] * (z - iz0)) + (tranforms[ix1][iy0][iz0][1] * (1 - z + iz0)))))) + ( (1 - x + ix0) * ( ( (y - iy0) * ( (tranforms[ix0][iy1][iz1][1] * (z - iz0)) + (tranforms[ix0][iy1][iz0][1] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix0][iy0][iz1][1] * (z - iz0)) + (tranforms[ix0][iy0][iz0][1] * (1 - z + iz0)))))));

            // y translation
            initial[4] = ( ( (x - ix0) * ( ( (y - iy0) * ( (tranforms[ix1][iy1][iz1][2] * (z - iz0)) + (tranforms[ix1][iy1][iz0][2] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix1][iy0][iz1][2] * (z - iz0)) + (tranforms[ix1][iy0][iz0][2] * (1 - z + iz0)))))) + ( (1 - x + ix0) * ( ( (y - iy0) * ( (tranforms[ix0][iy1][iz1][2] * (z - iz0)) + (tranforms[ix0][iy1][iz0][2] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix0][iy0][iz1][2] * (z - iz0)) + (tranforms[ix0][iy0][iz0][2] * (1 - z + iz0)))))));

            // z translation
            initial[5] = ( ( (x - ix0) * ( ( (y - iy0) * ( (tranforms[ix1][iy1][iz1][3] * (z - iz0)) + (tranforms[ix1][iy1][iz0][3] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix1][iy0][iz1][3] * (z - iz0)) + (tranforms[ix1][iy0][iz0][3] * (1 - z + iz0)))))) + ( (1 - x + ix0) * ( ( (y - iy0) * ( (tranforms[ix0][iy1][iz1][3] * (z - iz0)) + (tranforms[ix0][iy1][iz0][3] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix0][iy0][iz1][3] * (z - iz0)) + (tranforms[ix0][iy0][iz0][3] * (1 - z + iz0)))))));

            // scale
            initial[6] = ( ( (x - ix0) * ( ( (y - iy0) * ( (tranforms[ix1][iy1][iz1][0] * (z - iz0)) + (tranforms[ix1][iy1][iz0][0] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix1][iy0][iz1][0] * (z - iz0)) + (tranforms[ix1][iy0][iz0][0] * (1 - z + iz0)))))) + ( (1 - x + ix0) * ( ( (y - iy0) * ( (tranforms[ix0][iy1][iz1][0] * (z - iz0)) + (tranforms[ix0][iy1][iz0][0] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix0][iy0][iz1][0] * (z - iz0)) + (tranforms[ix0][iy0][iz0][0] * (1 - z + iz0)))))));
        } else {

            // x translation
            initial[3] = ( ( (x - ix0) * ( ( (y - iy0) * ( (tranforms[ix1][iy1][iz1][0] * (z - iz0)) + (tranforms[ix1][iy1][iz0][0] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix1][iy0][iz1][0] * (z - iz0)) + (tranforms[ix1][iy0][iz0][0] * (1 - z + iz0)))))) + ( (1 - x + ix0) * ( ( (y - iy0) * ( (tranforms[ix0][iy1][iz1][0] * (z - iz0)) + (tranforms[ix0][iy1][iz0][0] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix0][iy0][iz1][0] * (z - iz0)) + (tranforms[ix0][iy0][iz0][0] * (1 - z + iz0)))))));

            // y translation
            initial[4] = ( ( (x - ix0) * ( ( (y - iy0) * ( (tranforms[ix1][iy1][iz1][1] * (z - iz0)) + (tranforms[ix1][iy1][iz0][1] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix1][iy0][iz1][1] * (z - iz0)) + (tranforms[ix1][iy0][iz0][1] * (1 - z + iz0)))))) + ( (1 - x + ix0) * ( ( (y - iy0) * ( (tranforms[ix0][iy1][iz1][1] * (z - iz0)) + (tranforms[ix0][iy1][iz0][1] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix0][iy0][iz1][1] * (z - iz0)) + (tranforms[ix0][iy0][iz0][1] * (1 - z + iz0)))))));

            // z translation
            initial[5] = ( ( (x - ix0) * ( ( (y - iy0) * ( (tranforms[ix1][iy1][iz1][2] * (z - iz0)) + (tranforms[ix1][iy1][iz0][2] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix1][iy0][iz1][2] * (z - iz0)) + (tranforms[ix1][iy0][iz0][2] * (1 - z + iz0)))))) + ( (1 - x + ix0) * ( ( (y - iy0) * ( (tranforms[ix0][iy1][iz1][2] * (z - iz0)) + (tranforms[ix0][iy1][iz0][2] * (1 - z + iz0)))) + ( (1 - y + iy0) * ( (tranforms[ix0][iy0][iz1][2] * (z - iz0)) + (tranforms[ix0][iy0][iz0][2] * (1 - z + iz0)))))));
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
     * outside the rotation begin and end limits. Looks at the 8 neighbors of a point: +, =, or - one fine sample in
     * each of the three directions. If the point has a cost greater than any of these, it is not a minima. Otherwise it
     * is. Saves it in a vector of minima. Optimizes the minima over rotations as well as translations and global scale.
     * (Previously had not optimized over rotations.) Returns two vectors, one containing the minima before
     * optimization, one containing the minima after optimization.
     * 
     * @param ref Subsampled by 8 reference image.
     * @param input Subsampled by 8 input image.
     * 
     * @return List of preoptimized and optimized points.
     */
    @SuppressWarnings("unchecked")
    public Vector<MatrixListItem>[] levelEight(final ModelSimpleImage ref, final ModelSimpleImage input,
            final float progressFrom, final float progressTo) {
        final AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input, costChoice, 32, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            // System.err.println( "Level 8 " );
            m_kGPUCost.initImages(ref, input, 32);
            cost.setGPUCost(m_kGPUCost);
        }

        /*
         * // To test the amount of time for a single cost evaluation at this level: timeNow =
         * System.currentTimeMillis(); initialCost = cost.cost(tMatrix); timeLater = System.currentTimeMillis();
         * timeElapsed = timeLater - timeNow; Preferences.debug(" LEVEL EIGHT \n"); Preferences.debug(" Time for single
         * cost function evaluation: " + ( (float) timeElapsed / 1000.0f) + " seconds \n");
         *  // Initial amount of overlap. if (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION ||
         * costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED || costChoice ==
         * AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT) { double overAmt[] = new double[] {0, 0,
         * 0}; overAmt = cost.getOverlap(); Preferences.debug(" Initial overlap amount " + (int) (overAmt[0]) + " of " +
         * (int) (overAmt[1]) + " voxels, yielding " + (int) (overAmt[2]) + " percent.\n"); }
         */
        Preferences.debug("Total number of positions at coarse sampling to be tested: "
                + (coarseNumX * coarseNumY * coarseNumZ) + ".\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Total number of positions at fine sampling to be tested: "
                + (fineNumX * fineNumY * fineNumZ) + ".\n",Preferences.DEBUG_ALGORITHM);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub8);
            cost.setInputWgtImage(simpleWeightInputSub8);
        }

        double diffX = 0;
        double diffY = 0;
        double diffZ = 0;
        Vector3f cog = new Vector3f(0, 0, 0);
        Vector3f cogR = new Vector3f(0, 0, 0);

        // change this
        if (calcCOG) {
            cog = AlgorithmRegELSUNCOAR3D.calculateCenterOfMass3D(input, simpleWeightInputSub8, doColor);
            cogR = AlgorithmRegELSUNCOAR3D.calculateCenterOfMass3D(ref, simpleWeightRefSub8, doColor);
            Preferences.debug("Center of mass for the subsampled input image:" + cog + "\n",Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Center of mass for the subsampled reference image:" + cogR + "\n",Preferences.DEBUG_ALGORITHM);

            diffX = (cog.X - cogR.X);
            diffY = (cog.Y - cogR.Y);
            diffZ = (cog.Z - cogR.Z);
        }

        // Optimizing over translations and global scale
        maxIter = baseNumIter * 2;
        int dofs = 3;
        if (DOF > 6) {
            // Global scale and x,y,z translations.
            dofs = 4;
        }

        final double[] initial = new double[12];
        /**
         * Initial rotation
         */
        initial[0] = initial[1] = initial[2] = 0;

        /**
         * Initial translation
         */
        initial[3] = diffX;
        initial[4] = diffY;
        initial[5] = diffZ;
        /**
         * Initial scaling
         */
        initial[6] = initial[7] = initial[8] = 1;

        /**
         * Initial skewing
         */
        initial[9] = initial[10] = initial[11] = 0;

        int index = 0;
        
        
        int numIterations = coarseNumY * coarseNumX;
        doneSignal = new CountDownLatch(numIterations);  
        AlgorithmELSUNCOpt3D[] elsuncMT = new AlgorithmELSUNCOpt3D[numIterations];
        // The following code calls ELSUNC for the level8 search. 
        // The search creates numIterations = coarseNumY * coarseNumX instances of the ELSUNC search
        // each of those searches has a constant value for rotationX and rotationY but a set of rotations in Z
        // The ELSUNC instances can run as separate threads or sequentially. If they are run as parallel threads
        // the AlgorithmELSUNCOpt3D does not use threads internally. If they are run sequentially, the AlgorithmELSUNCOpt3D
        // can use threads internally.
        for (int i = 0; (i < coarseNumX); i++) {
            for (int j = 0; (j < coarseNumY); j++) {
                
                elsuncMT[index] = new AlgorithmELSUNCOpt3D(null, cog, dofs, cost, getTolerance(dofs), maxIter,
                		searchAlgorithm);
                
                Vectornd[] initials = new Vectornd[coarseNumZ];
                for (int k = 0; (k < coarseNumZ); k++) {
                    
                    // Initial rotation
                    initial[0] = rotateBeginX + (i * coarseRateX);
                    initial[1] = rotateBeginY + (j * coarseRateY);
                    initial[2] = rotateBeginZ + (k * coarseRateZ);

                    initials[k] = new Vectornd(initial, true);
                }
                
                elsuncMT[index].setPoints(initials);  
                elsuncMT[index].addListener(this);  
                if (doMultiThread)
                {
                    ThreadUtil.mipavThreadPool.execute(elsuncMT[index]);
                }
                else
                {
                    elsuncMT[index].run();
                }
                index++;
            }
        }
        try {
            doneSignal.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        
        // At this point all the iterations of elsunc have completed, either they ran sequentially or
        // as multiple threads. This code gathers the results into the transforms[][][] data structure
        // for use in the next pass.
        index = 0;
        final double[][][][] transforms = new double[coarseNumX][coarseNumY][coarseNumZ][dofs];
        for (int i = 0; (i < coarseNumX); i++) {
            for (int j = 0; (j < coarseNumY); j++) {
                for (int k = 0; (k < coarseNumZ); k++) {
                    transforms[i][j][k] = elsuncMT[index].extractPoint(elsuncMT[index].getPoint(k));
                }
                elsuncMT[index].disposeLocal();
                index++;
            }
        }

        if (threadStopped) {
            return null;
        }
        
        AlgorithmELSUNCOpt3D elsunc = new AlgorithmELSUNCOpt3D(this, cog, dofs, cost, getTolerance(dofs), maxIter,
        		searchAlgorithm);
        
        elsunc.setMinProgressValue((int) progressFrom);
        elsunc.setMaxProgressValue((int) (progressFrom + 2 * (progressTo - progressFrom) / 3));
        elsunc.setProgress(0);
        elsunc.setProgressModulus(coarseNumX * coarseNumY * coarseNumZ * maxIter / 100);
        this.linkProgressToAlgorithm(elsunc);

        final MatrixListItem[][][] matrixList = new MatrixListItem[fineNumX][fineNumY][fineNumZ];

        fireProgressStateChanged("Measuring at fine samples");

        final double[] costs = new double[fineNumX * fineNumY * fineNumZ];
        double factorX, factorY, factorZ;

        //System.out.println("coarseNumX and fineNumX " + coarseNumX + " " + fineNumX);
        //System.out.println("coarseNumY and fineNumY " + coarseNumY + " " + fineNumY);
        //System.out.println("coarseNumZ and fineNumZ " + coarseNumZ + " " + fineNumZ);

        index = 0;
        for (int i = 0; (i < fineNumX) && !threadStopped; i++) {

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
                    costs[index] = elsunc.measureCost(initial);
                    matrixList[i][j][k] = new MatrixListItem(costs[index++], elsunc.convertToMatrix(initial), initial);
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

        index = Arrays.binarySearch(costs, threshold);
        if (index < 0) {
            index = -1 * (index + 1);
        }
        Vectornd[] initials = new Vectornd[index];
        index = 0;
        for (int i = 0; i < fineNumX; i++) {
            for (int j = 0; (j < fineNumY) && !threadStopped; j++) {
                for (int k = 0; (k < fineNumZ) && !threadStopped; k++) {
                    if (matrixList[i][j][k].cost < threshold) {
                        initials[index] = new Vectornd(matrixList[i][j][k].initial);
                        index++;
                    }
                }
            }
        }
        elsunc.setMinProgressValue((int) (progressFrom + 2 * (progressTo - progressFrom) / 3));
        elsunc.setMaxProgressValue((int) (progressFrom + 5 * (progressTo - progressFrom) / 6));
        elsunc.setProgressModulus( (initials.length * maxIter < 100) ? 1 : initials.length * maxIter / 100);
        elsunc.setProgress(0);
        elsunc.setPoints(initials);
        elsunc.run();
        this.delinkProgressToAlgorithm(elsunc);
        fireProgressStateChanged((int) (progressFrom + 5 * (progressTo - progressFrom) / 6));
        index = 0;
        for (int i = 0; (i < fineNumX); i++) {
            for (int j = 0; (j < fineNumY); j++) {
                for (int k = 0; (k < fineNumZ); k++) {
                    if (matrixList[i][j][k].cost < threshold) {
                        matrixList[i][j][k] = new MatrixListItem(elsunc.getCost(index), elsunc.getMatrix(index), elsunc
                                .getPoint(index));
                        index++;
                    }
                }
            }
        }

        if (threadStopped) {
            return null;
        }

        final Vector<MatrixListItem> minima = new Vector<MatrixListItem>();

        final boolean possibleMinima[][][] = new boolean[fineNumX][fineNumY][fineNumZ];

        for (int i = 0; i < fineNumX; i++) {

            for (int j = 0; j < fineNumY; j++) {

                for (int k = 0; k < fineNumZ; k++) {

                    possibleMinima[i][j][k] = true; // possible minimum

                    for (int itest = -1; (itest <= 1) && possibleMinima[i][j][k]; itest++) {

                        // as long as still possible minimum, check neighbors one degree off
                        for (int jtest = -1; (jtest <= 1) && possibleMinima[i][j][k]; jtest++) {

                            for (int ktest = -1; (ktest <= 1) && possibleMinima[i][j][k]; ktest++) {

                                if ( ( (i + itest) >= 0) && ( (i + itest) < fineNumX) && ( (j + jtest) >= 0)
                                        && ( (j + jtest) < fineNumY) && ( (k + ktest) >= 0)
                                        && ( (k + ktest) < fineNumZ)) {

                                    if (matrixList[i][j][k].cost > matrixList[i + itest][j + jtest][k + ktest].cost) {
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

                                    if ( ( (i + itest) >= 0) && ( (i + itest) < fineNumX) && ( (j + jtest) >= 0)
                                            && ( (j + jtest) < fineNumY) && ( (k + ktest) >= 0)
                                            && ( (k + ktest) < fineNumZ)) {

                                        if ( (matrixList[i][j][k].cost == matrixList[i + itest][j + jtest][k + ktest].cost)
                                                && ( !possibleMinima[i + itest][j + jtest][k + ktest])) {
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

        final Vector<MatrixListItem> optMinima = new Vector<MatrixListItem>();
        // Now freely optimizes over rotations:

        fireProgressStateChanged("Optimizing minima");

        final int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;
        elsunc = new AlgorithmELSUNCOpt3D(this, cog, degree, cost, getTolerance(degree), maxIter, searchAlgorithm);
        
        elsunc.setMinProgressValue((int) (progressFrom + 5 * (progressTo - progressFrom) / 6));
        elsunc.setMaxProgressValue((int) progressTo);
        this.linkProgressToAlgorithm(elsunc);
        elsunc.setProgress(0);

        initials = new Vectornd[minima.size()];
        for (int i = 0; i < minima.size(); i++) {
            initials[i] = new Vectornd(minima.get(i).initial, true);
        }
        elsunc.setPoints(initials);
        elsunc.setProgressModulus( (initials.length * maxIter < 100) ? 1 : initials.length * maxIter / 100);
        elsunc.run();
        this.delinkProgressToAlgorithm(elsunc);
        for (int i = 0; i < initials.length; i++) {
            optMinima.add(new MatrixListItem(elsunc.getCost(i), elsunc.getMatrix(i), elsunc.getPoint(i)));
        }
        if (threadStopped) {
            return null;
        }
        fireProgressStateChanged((int) progressTo);
        cost.disposeLocal();
        elsunc.disposeLocal();
        return new Vector[] {minima, optMinima};
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
     * @param ref Reference image, subsampled by 4.
     * @param input Input image, subsampled by 4.
     * @param minima Preoptimized minima.
     * @param optMinima Optimized minima.
     * 
     * @return A vector of perturbed, optimized minima.
     */
    public Vector<MatrixListItem> levelFour(final ModelSimpleImage ref, final ModelSimpleImage input,
            final Vector<MatrixListItem> minima, final Vector<MatrixListItem> optMinima, final float progressFrom,
            final float progressTo) {
        final AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input, costChoice, 64, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            // System.err.println( "Level 4 " );
            m_kGPUCost.initImages(ref, input, 64);
            cost.setGPUCost(m_kGPUCost);
        }
        /*
         * // To test the amount of time for a single cost evaluation at this level: timeNow =
         * System.currentTimeMillis(); initialCost = cost.cost(tMatrix); timeLater = System.currentTimeMillis();
         * timeElapsed = timeLater - timeNow; Preferences.debug(" LEVEL FOUR \n"); Preferences.debug(" Time for single
         * cost function evaluation: " + ( (float) timeElapsed / 1000.0f) + " seconds \n");
         *  // Test initial amount of overlap. if (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION ||
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

        if (calcCOG) {
            cog = AlgorithmRegELSUNCOAR3D.calculateCenterOfMass3D(input, simpleWeightInputSub4, doColor);
        }

        Preferences.debug("Center of mass for the subsampled input image:" + cog + "\n",Preferences.DEBUG_ALGORITHM);

        // Preferences.debug("Center of mass for the subsampled reference image:" + cog + "\n",Preferences.DEBUG_ALGORITHM);
        MatrixListItem item = null;

        for (final Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[3] *= level4FactorXY;
            item.initial[4] *= level4FactorXY;
            item.initial[5] *= level4FactorZ;
        }

        for (final Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[3] *= level4FactorXY;
            item.initial[4] *= level4FactorXY;
            item.initial[5] *= level4FactorZ;
        }

        final int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;

        final AlgorithmELSUNCOpt3D elsunc = new AlgorithmELSUNCOpt3D(this, cog, degree, cost, getTolerance(degree),
                maxIter, searchAlgorithm);
        
        elsunc.setMinProgressValue((int) progressFrom);
        elsunc.setMaxProgressValue((int) (progressFrom + (progressTo - progressFrom) / 5));
        for (final Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = en.nextElement();
            item.cost = elsunc.measureCost(item.initial);
        }

        if (threadStopped) {
            return null;
        }

        for (final Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = en.nextElement();
            item.cost = elsunc.measureCost(item.initial);
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(minima);
        Collections.sort(optMinima);

        final int total = (numMinima < minima.size()) ? numMinima : minima.size();

        Vectornd[] initials = new Vectornd[2 * total];
        for (int i = 0; i < total; i++) {
            initials[i] = new Vectornd(minima.elementAt(i).initial);
            initials[i + total] = new Vectornd(optMinima.elementAt(i).initial);
        }
        elsunc.setMaxIterations(3);
        elsunc.setPoints(initials);
        this.linkProgressToAlgorithm(elsunc);
        elsunc.setProgress(0);
        elsunc.setProgressModulus( (initials.length * 3 < 100) ? 1 : initials.length * 3 / 100);
        elsunc.run();

        if (threadStopped) {
            return null;
        }

        final Vector<MatrixListItem> newMinima = new Vector<MatrixListItem>();
        for (int i = 0; i < initials.length; i++) {
            newMinima.add(new MatrixListItem(elsunc.getCost(i), elsunc.getMatrix(i), elsunc.getPoint(i)));
        }

        // Resort the minima. Shouldn't have switched much from previous sorting.
        Collections.sort(newMinima);

        // Remove items outside the rotateBegin and rotateEnd limits
        for (int i = newMinima.size() - 1; i >= 1; i--) {
            if ( ( (newMinima.elementAt(i)).initial[0] < rotateBeginX)
                    || ( (newMinima.elementAt(i)).initial[0] > rotateEndX)
                    || ( (newMinima.elementAt(i)).initial[1] < rotateBeginY)
                    || ( (newMinima.elementAt(i)).initial[1] > rotateEndY)
                    || ( (newMinima.elementAt(i)).initial[2] < rotateBeginZ)
                    || ( (newMinima.elementAt(i)).initial[2] > rotateEndZ)) {
                newMinima.removeElementAt(i);
            }
        }

        if (newMinima.size() > 1) {

            if ( ( (newMinima.elementAt(0)).initial[0] < rotateBeginX)
                    || ( (newMinima.elementAt(0)).initial[0] > rotateEndX)
                    || ( (newMinima.elementAt(0)).initial[1] < rotateBeginY)
                    || ( (newMinima.elementAt(0)).initial[1] > rotateEndY)
                    || ( (newMinima.elementAt(0)).initial[2] < rotateBeginZ)
                    || ( (newMinima.elementAt(0)).initial[2] > rotateEndZ)) {
                newMinima.removeElementAt(0);
            }
        }

        Preferences.debug("Removed " + (total - newMinima.size()) + " items outside rotation limits\n",Preferences.DEBUG_ALGORITHM);

        fireProgressStateChanged("Perturbing minima");

        final double fineDeltaX = fineRateX / 2.0;
        final double fineDeltaY = fineRateY / 2.0;
        final double fineDeltaZ = fineRateZ / 2.0;
        double[] initial;
        final Vector<MatrixListItem> perturbList = new Vector<MatrixListItem>();

        int sign = 1;

        /**
         * Add minimas perturnList
         */
        perturbList.addAll(newMinima);

        /*
         * Perturb rotations by +findDelta, -fineDelta in each of the three dimensions.
         */
        initials = null;
        if (DOF > 6) {
            initials = new Vectornd[10 * newMinima.size()];
        } else {
            initials = new Vectornd[6 * newMinima.size()];
        }
        int index = 0;
        for (int j = 1; (j < 7) && !threadStopped; j++) {

            for (int i = 0; (i < newMinima.size()) && !threadStopped; i++) {
                progress += 3.0 * progressStep / (newMinima.size() * 7);
                fireProgressStateChanged((int) progress);

                // Current "initial" is element for this i.
                initial = (newMinima.elementAt(i)).initial.clone();

                // Will we add or subtract fine delta? Add for j=1,3,5. Subract for j=2,4,6.
                if ( (j % 2) != 0) {
                    sign = 1;
                } else {
                    sign = -1;
                }

                // Apply perturbation and send message to debug window.
                if ( (j == 1) || (j == 2)) {
                    Preferences.debug(" by adding " + (sign * fineDeltaX) + " to initial[0] (" + (int) initial[0]
                            + ").\n",Preferences.DEBUG_ALGORITHM);
                    initial[0] += sign * fineDeltaX;
                } else if ( (j == 3) || (j == 4)) {
                    Preferences.debug(" by adding " + (sign * fineDeltaY) + " to initial[1] (" + (int) initial[1]
                            + ").\n",Preferences.DEBUG_ALGORITHM);
                    initial[1] += sign * fineDeltaY;
                } else {
                    Preferences.debug(" by adding " + (sign * fineDeltaZ) + " to initial[2] (" + (int) initial[2]
                            + ").\n",Preferences.DEBUG_ALGORITHM);
                    initial[2] += sign * fineDeltaZ;
                }

                initials[index++] = new Vectornd(initial);
            }
        }

        /**
         * Perturb scales by 0.8, 0.9, 1.0, 1.1, and 1.2.
         */
        if (DOF > 6) {

            float scaleDelta = 0.8f;

            for (int j = 0; (j < 4) && !threadStopped; j++) {

                for (int i = 0; (i < newMinima.size()) && !threadStopped; i++) {
                    progress += 3.0 * progressStep / (newMinima.size() * 5);
                    fireProgressStateChanged((int) progress);

                    initial = (newMinima.elementAt(i)).initial.clone();

                    if (j == 1) {
                        scaleDelta = 0.9f;
                    } else if (j == 2) {
                        scaleDelta = 1.1f;
                    } else if (j == 3) {
                        scaleDelta = 1.2f;
                    }

                    Preferences.debug("Perturbing initial[6] by ",Preferences.DEBUG_ALGORITHM);
                    initial[6] *= scaleDelta;
                    Preferences.debug("Multiplying by " + scaleDelta + "\n",Preferences.DEBUG_ALGORITHM);
                    initials[index++] = new Vectornd(initial);

                }
            }
        }

        elsunc.setMinProgressValue((int) (progressFrom + (progressTo - progressFrom) / 5));
        elsunc.setMaxProgressValue((int) progressTo);
        elsunc.setProgressModulus( (initials.length * elsunc.getMaxIterations() < 100) ? 1 : initials.length
                * elsunc.getMaxIterations() / 100);
        elsunc.setProgress(0);
        elsunc.setPoints(initials);
        elsunc.run();
        this.delinkProgressToAlgorithm(elsunc);
        for (int i = 0; i < initials.length; i++) {
            perturbList.add(new MatrixListItem(elsunc.getCost(i), elsunc.getMatrix(i), elsunc.getPoint(i)));
        }
        if (threadStopped) {
            return null;
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(perturbList);

        fireProgressStateChanged((int) progressTo);
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
     * @param ref Reference image.
     * @param input Input image.
     * @param item Best minimum so far.
     * @param maxIter DOCUMENT ME!
     * 
     * @return Best minimum after optimization.
     */
    public MatrixListItem levelOne(final ModelSimpleImage ref, final ModelSimpleImage input, final MatrixListItem item,
            final int maxIter, final float progressFrom, final float progressTo) {
        fireProgressStateChanged((int) progressFrom);
        MatrixListItem item2;

        final AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input, costChoice, 256, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            // System.err.println( "Level 1 " );
            m_kGPUCost.initImages(ref, input, 256);
            cost.setGPUCost(m_kGPUCost);
        }
        /*
         * // To test the amount of time for a single cost evaluation at this level: timeNow =
         * System.currentTimeMillis(); initialCost = cost.cost(tMatrix); timeLater = System.currentTimeMillis();
         * timeElapsed = timeLater - timeNow; Preferences.debug(" LEVEL ONE \n"); Preferences.debug(" Time for single
         * cost function evaluation: " + ( (float) timeElapsed / 1000.0f) + " seconds \n");
         *  // To test the initial amount of overlap. if (costChoice ==
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

        Vector3f cog = new Vector3f(0, 0, 0);

        if (calcCOG) {
            cog = AlgorithmRegELSUNCOAR3D.calculateCenterOfMass3D(input, simpleWeightInput, doColor);
        }

        /**
         * Scale the translation
         */
        item.initial[3] *= level1FactorXY;
        item.initial[4] *= level1FactorXY;
        item.initial[5] *= level1FactorZ;

        // item = new MatrixListItem(0, new TransMatrix(4), new
        // double[]{16.129471936295587,38.70284813817548,16.084311925686084,8.996883430244447,40.88645020065903,-0.2864091562806768,1.0000740313432002,0.9996596963470643,0.9988705261391,-3.1953502507784087E-4,2.473314739475689E-4,0.0});
        final int degree = (DOF < 12) ? DOF : 12;

        fireProgressStateChanged("Starting last optimization");

        final AlgorithmELSUNCOpt3D elsunc = new AlgorithmELSUNCOpt3D(this, cog, degree, cost, getTolerance(degree),
                maxIter, searchAlgorithm);

        final Vectornd[] initialPoints = new Vectornd[1];
        initialPoints[0] = new Vectornd(item.initial);
        elsunc.setPoints(initialPoints);
    	
        elsunc.setRunningInSeparateThread(runningInSeparateThread);
        elsunc.run();

        if (threadStopped) {
            return null;
        }

        // System.out.println("Input x = " + input.xRes + " y = " + input.yRes + " z = " + input.zRes );
        item2 = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0, input.xRes), elsunc.getPoint(0, input.xRes));
        item2.halfMatrixd = elsunc.getMatrixHalf(0, input.xRes);
        item2.midsagMatrixd = elsunc.getMatrixMidsagittal(0, input.xRes);

        fireProgressStateChanged((int) progressTo);
        Preferences.debug("Best answer: \n" + item2 + "\n",Preferences.DEBUG_ALGORITHM);

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
     * @param ref Reference image, subsampled by 2.
     * @param input Input image, subsampled by 2.
     * @param minima Minima.
     * 
     * @return The optimized minimum.
     */
    public MatrixListItem levelTwo(final ModelSimpleImage ref, final ModelSimpleImage input,
            final Vector<MatrixListItem> minima, final float progressFrom, final float progressTo) {
        fireProgressStateChanged((int) progressFrom);
        final AlgorithmCostFunctions cost = new AlgorithmCostFunctions(ref, input, costChoice, 128, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            // System.err.println( "Level 2 " );
            m_kGPUCost.initImages(ref, input, 128);
            cost.setGPUCost(m_kGPUCost);
        }
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

        if (calcCOG) {
            cog = AlgorithmRegELSUNCOAR3D.calculateCenterOfMass3D(input, simpleWeightInputSub2, doColor);
        }

        MatrixListItem item = null;

        for (final Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[3] *= level2FactorXY;
            item.initial[4] *= level2FactorXY;
            item.initial[5] *= level2FactorZ;
        }

        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;

        AlgorithmELSUNCOpt3D elsunc = new AlgorithmELSUNCOpt3D(this, cog, degree, cost, getTolerance(degree),
                maxIter, searchAlgorithm);
        
        fireProgressStateChanged("Measuring costs of minima");

        for (final Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = en.nextElement();
            item.cost = elsunc.measureCost(item.initial);
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(minima);

        fireProgressStateChanged("Optimizing with " + degree + " DOF");

        final Vectornd[] initialPoints = new Vectornd[1];
        initialPoints[0] = new Vectornd(minima.elementAt(0).initial);
        elsunc.setPoints(initialPoints);
        elsunc.setRunningInSeparateThread(runningInSeparateThread);
        elsunc.run();
        if (DOF > 9) {
            fireProgressStateChanged((int) (progressFrom + (progressTo - progressFrom) / 3));
        } else if (DOF > 7) {
            fireProgressStateChanged((int) (progressFrom + (progressTo - progressFrom) / 2));
        }
        /**
         * Disconnect ELSUNC class with Progress bar in order to release memory of ELSUNC class.
         */
        if (threadStopped) {
            return null;
        }

        item = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0), elsunc.getPoint(0));

        MatrixListItem itemPtr = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0, input.xRes), elsunc
                .getPoint(0, input.xRes));

        elsunc.disposeLocal();
        Preferences.debug("Level 2, after " + degree + " DOF: " + itemPtr + "\n",Preferences.DEBUG_ALGORITHM);
        maxIter = baseNumIter * 2;

        if (DOF > 7) {
            degree = 9;
            fireProgressStateChanged("Optimizing with " + degree + " DOF");
            elsunc = new AlgorithmELSUNCOpt3D(this, cog, degree, cost, getTolerance(degree), maxIter, searchAlgorithm);
            initialPoints[0] = new Vectornd(item.initial);
            elsunc.setPoints(initialPoints);
            
            elsunc.run();
            if (DOF > 9) {
                fireProgressStateChanged((int) (progressFrom + 2 * (progressTo - progressFrom) / 3));
            } else {
                fireProgressStateChanged((int) progressTo);
            }

            if (threadStopped) {
                return null;
            }

            item = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0), elsunc.getPoint(0));
            itemPtr = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0, input.xRes), elsunc.getPoint(0,
                    input.xRes));
            elsunc.disposeLocal();
            Preferences.debug("Level 2, after " + degree + " DOF: " + itemPtr + "\n",Preferences.DEBUG_ALGORITHM);

            if (DOF > 9) {
                degree = 12;
                fireProgressStateChanged("Optimizing with " + degree + " DOF");
                elsunc = new AlgorithmELSUNCOpt3D(this, cog, 12, cost, getTolerance(12), maxIter, searchAlgorithm);
                initialPoints[0] = new Vectornd(item.initial);
                elsunc.setPoints(initialPoints);
                elsunc.run();

                fireProgressStateChanged((int) (progressTo));
                if (threadStopped) {
                    return null;
                }

                item = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0), elsunc.getPoint(0));
                itemPtr = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0, input.xRes), elsunc.getPoint(0,
                        input.xRes));
                elsunc.disposeLocal();
                Preferences.debug("Level 2, after " + degree + " DOF: " + itemPtr + "\n",Preferences.DEBUG_ALGORITHM);
            }
        }

        fireProgressStateChanged((int) (progressTo));
        cost.disposeLocal();

        return item;
    }

    public float getLevel1FactorXY() {
        return level1FactorXY;
    }

    public void setLevel1FactorXY(final float level1FactorXY) {
        this.level1FactorXY = level1FactorXY;
    }

    public float getLevel1FactorZ() {
        return level1FactorZ;
    }

    public void setLevel1FactorZ(final float level1FactorZ) {
        this.level1FactorZ = level1FactorZ;
    }

    public float getLevel2FactorXY() {
        return level2FactorXY;
    }

    public void setLevel2FactorXY(final float level2FactorXY) {
        this.level2FactorXY = level2FactorXY;
    }

    public float getLevel2FactorZ() {
        return level2FactorZ;
    }

    public void setLevel2FactorZ(final float level2FactorZ) {
        this.level2FactorZ = level2FactorZ;
    }

    public float getLevel4FactorXY() {
        return level4FactorXY;
    }

    public void setLevel4FactorXY(final float level4FactorXY) {
        this.level4FactorXY = level4FactorXY;
    }

    public float getLevel4FactorZ() {
        return level4FactorZ;
    }

    public void setLevel4FactorZ(final float level4FactorZ) {
        this.level4FactorZ = level4FactorZ;
    }

    @Override
    public void algorithmPerformed(AlgorithmBase algorithm) {
        // This is used in the levelEight when the multiple instance of ELSUNC used to search the
        // different coarse rotations are multi-threaded. This is how the algorithms let the CountDownLatch
        // know when all the multi-threaded search have completed.
        if ( doneSignal != null )
        {
            //System.err.println( "AlgorithmPerformed " + doneSignal.getCount() );
            doneSignal.countDown();
        }
    }

}
