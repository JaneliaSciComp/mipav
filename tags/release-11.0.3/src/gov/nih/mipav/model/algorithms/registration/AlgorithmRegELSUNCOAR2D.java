package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.ImageRegistrationGPU;

import java.awt.Dimension;
import java.io.IOException;
import java.util.*;

import WildMagic.LibFoundation.Mathematics.*;


// import com.mentorgen.tools.profile.runtime.Profile;

/**
 * This is an automatic registration method based on FLIRT. FLIRT stands for FMRIB's Linear Image Registration Tool. For
 * more information on FLIRT, visit their homepage at <a href="http://www.fmrib.ox.ac.uk/fsl/flirt/">
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
 * 4.) With the images that were subsampled by 8, we call levelEight. This function will use the coarse sampling rate
 * and optimize translations and global scale at the given rotation. So for example, if the coarse sampling range were
 * -30 to 30 at every 15 degrees, we would optimize at rotations of -30, -15, 0, 15, 30.<br>
 * 5.) Still in levelEight, we now measure the cost at the fine sampling rate. We interpolate the translations and
 * global scale to come up with a good guess as to what the optimized translation would be at that point.<br>
 * 6.) We take the top 20% of the points and optimize them.<br>
 * 7.) We now have a large multi-array of costs. 20% of those have been optimized and placed back into their original
 * position in the multi-array. We look at the 2 neighbors of a point: + and - one fine sample. If our point has a cost
 * greater than any of these, it is not a minima. Otherwise it is. We save it in a vector of minima.<br>
 * 8.) We optimize the minima over rotation as well as translations and global scale. (Previously we had not optimized
 * over rotation.) We return two vectors, one containing the minima before optimization, one containing the minima after
 * optimization.<br>
 * 9.) We now call levelFour with the images subsampled by 4 and the vectors of minima. We measure the costs of the
 * minima on the new images and sort them. We take the top numMinima in each vector (pre-optimization and
 * post-optimization) and optimize them. We put them all into one vector.<br>
 * 10.) We perturb the rotation by zero and plus-minus fineDelta. If it's not a rigid transformation, we then perturb
 * the global scaling by factors of 0.8, 0.9, 1.0, 1.1, and 1.2.<br>
 * 11.) We optimize the perturbations. We return a vector of the perturbed, optimized minima.<br>
 * 12.) We now call levelTwo with the images subsampled by 2. We measure the costs of the minima at the new images. We
 * optimize the best minimum with 4 degrees of freedom, then 5, then 6. If the user has limited the degrees of freedom
 * to 3, there will only be one optimization run, with 3 degrees of freedom. The function returns the best minimum after
 * optimization.<br>
 * 13.) We call levelOne with the un-subsampled images. At levelOne, one optimization run is performed, with the maximum
 * allowable degrees of freedom, as specified by the user (the max is 6).<br>
 * 14.) The best answer is returned from levelOne. The matrix from this answer is saved in a file and also accessed by
 * the dialog that called this algorithm.<br>
 * </p>
 * 
 * <p>
 * Note that when 6 degrees of freedom is used the rotation is set equal to 0 because diffX sets (0,2), diffY sets
 * (1,2), scaleX sets (0,0), scaleY sets (1,1), skewX sets (0,1), and skewY sets (1,0) so all 6 elements are set.
 * </p>
 * 
 * @author Neva Cherniavsky
 * @author Matthew McAuliffe
 */
public class AlgorithmRegELSUNCOAR2D extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean allowLevel16 = true;

    /** DOCUMENT ME! */
    private boolean allowLevel2 = true;

    /** DOCUMENT ME! */
    private boolean allowLevel4 = true;

    /** DOCUMENT ME! */
    private boolean allowLevel8 = true;

    /** Final answer after registration. */
    private MatrixListItem answer;

    /** these numbers hard coded for constructors that don't include them. */
    private int baseNumIter = 2;

    /** Blurred input image. */
    private ModelImage blurredInput;

    /** Blurred reference image. */
    private ModelImage blurredRef;

    /** Number of passes that will be made in the coarse sampling and fine sampling. */
    private final int coarseNum, fineNum;

    /** Choice of which cost function to use. */
    private int costChoice;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** Maximum degrees of freedom when running the optimization. */
    private int DOF;

    /** If true subsample. */
    private final boolean doSubsample;
    private boolean doJTEM;
    
    private boolean doMultiThread;
    
    private int searchAlgorithm;

    /** Isotropic input image. */
    private ModelImage imageInputIso;

    /** Isotropic reference image. */
    private ModelImage imageRefIso;

    /** Isotropic weighted input image. */
    private ModelImage imageWeightInputIso;

    /** Isotropic weighted reference image. */
    private ModelImage imageWeightRefIso;

    /** This image is to registered to the reference image. */
    private ModelImage inputImage;

    /**
     * This gives weights for the input image - higher weights mean a greater impact in that area on the registration.
     */
    private ModelImage inputWeight = null;

    /** Interpolation method. */
    private final int interp;

    /** Multiplication factor for level 1 - will be set based on subsampling. */
    private float level1Factor = 1.0f;

    /** Multiplication factor for level 2 - will be set based on subsampling. */
    private float level2Factor = 1.0f;

    /** Multiplication factor for level 4 - will be set based on subsampling. */
    private float level4Factor = 1.0f;

    /**
     * setBruteForce. Tells the algorithm to do a brute-force optimization, where it will iterate of the the input
     * rotation, xscale, yscale, and translation ranges calculating the cost function at each point and returing the
     * minimum. No optimization with the brute-force approach.
     */
    /** If true, calculate the brute-force solution:. */
    private boolean m_bBruteForce = false;

    /** The range of rotations to try in brute-force mode:. */
    private float m_fRotationRange = 0f;

    /** The range of scales in x to try in brute-force mode:. */
    private float m_fXScaleRange = 0f;

    /** The range of scales in y to try in brute-force mode:. */
    private float m_fYScaleRange = 0f;

    /** The number of steps to divide scale ranges:. */
    private int m_iScaleSteps;

    /** The range of x,y translations to try in brute-force mode:. */
    private int m_iTranslationRange = 0;

    /** DOCUMENT ME! */
    private int maxDim = 256;

    /**
     * Limits number of iterations in ELSUNC optimization. maxIter in the call to ELSUNC will be an integer multiple
     * of baseNumIter
     */
    private int maxIter;

    /** Number of minima from level 8 to test at level 4. */
    private int numMinima = 3;

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
    private final float[] resInput;

    /** The voxel resolutions of the reference image. */
    private final float[] resRef;

    /** Flag used to indicate if the registration is rigid (rotation and translation only; DOF = 3. */
    private boolean rigidFlag = false;

    /** Coarse and fine sampling parameters. */
    private final float rotateBegin, rotateEnd, coarseRate, fineRate;

    /** Simple version of input image. */
    private ModelSimpleImage simpleInput;

    /** Simple version of input image, subsampled by 2. */
    private ModelSimpleImage simpleInputSub2;

    /** Simple version of input image, subsampled by 4. */
    private ModelSimpleImage simpleInputSub4;

    /** Simple version of input image, subsampled by 8. */
    private ModelSimpleImage simpleInputSub8;

    /** Simple version of reference image. */
    private ModelSimpleImage simpleRef;

    /** Simple version of reference image, subsampled by 2. */
    private ModelSimpleImage simpleRefSub2;

    /** Simple version of reference image, subsampled by 4. */
    private ModelSimpleImage simpleRefSub4;

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

    private AlgorithmCostFunctions2D cost;

    private ImageRegistrationGPU m_kGPUCost = null;

    /**
     * Used to store all paths for levelEigth, levelFour, levelTwo and levelOne.
     */
    @SuppressWarnings("unchecked")
    private final Vector<Vector<Vector3f>>[] paths = new Vector[6];

    /**
     * The optimal path.
     */
    private Vector<Vector3f> optimalPath = new Vector<Vector3f>();

    private static final Vector3f[] originalPath = {new Vector3f(18.0f, 27.201007843017578f, 35.089054107666016f),
            new Vector3f(18.0f, 26.926931381225586f, 35.089054107666016f),
            new Vector3f(18.0f, 26.926931381225586f, 37.107540130615234f),
            new Vector3f(18.0f, 26.987857818603516f, 37.107540130615234f),
            new Vector3f(18.0f, 26.987857818603516f, 37.013275146484375f),
            new Vector3f(18.0f, 26.987857818603516f, 37.013275146484375f),
            new Vector3f(19.229249954223633f, 26.987857818603516f, 37.013275146484375f),
            new Vector3f(19.229249954223633f, 26.926382064819336f, 37.013275146484375f),
            new Vector3f(19.229249954223633f, 26.926382064819336f, 39.34950256347656f),
            new Vector3f(19.83618927001953f, 26.926382064819336f, 39.34950256347656f),
            new Vector3f(19.83618927001953f, 27.25579261779785f, 39.34950256347656f),
            new Vector3f(19.83618927001953f, 27.25579261779785f, 40.23905563354492f),
            new Vector3f(19.83618927001953f, 27.25579261779785f, 40.23905563354492f),
            new Vector3f(19.99574089050293f, 27.25579261779785f, 40.23905563354492f),
            new Vector3f(19.99574089050293f, 27.316665649414062f, 40.23905563354492f),
            new Vector3f(9.99574089050293f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f),
            new Vector3f(20.01641845703125f, 27.316665649414062f, 40.429630279541016f)};

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
     * @param _rotateBegin Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEnd End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRate Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRate Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param doSubsample If true subsample
     * @param doMultiThread
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     * 
     * <p>
     * Constructor without weighting and without advanced settings (num iter).
     * </p>
     */
    public AlgorithmRegELSUNCOAR2D(final ModelImage _imageA, final ModelImage _imageB, final int _costChoice, final int _DOF,
            final int _interp, final float _rotateBegin, final float _rotateEnd, final float _coarseRate,
            final float _fineRate, final boolean doSubsample, final boolean doMultiThread, final int searchAlgorithm) {
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

        if (DOF == 3) {
            rigidFlag = true;
        }

        if (DOF == 6) {
            DOF = 7; // use 2 shears
        }

        interp = _interp;
        resRef = refImage.getFileInfo(0).getResolutions();
        resInput = inputImage.getFileInfo(0).getResolutions();
        rotateBegin = _rotateBegin;
        rotateEnd = _rotateEnd;
        coarseRate = _coarseRate;
        fineRate = _fineRate;
        coarseNum = (int) ( (rotateEnd - rotateBegin) / coarseRate) + 1;
        fineNum = (int) ( (rotateEnd - rotateBegin) / fineRate) + 1;
        weighted = false;
        this.doSubsample = doSubsample;
        this.doMultiThread = doMultiThread;
        this.searchAlgorithm = searchAlgorithm;
    }
    
    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     * 
     * @param _imageA Reference image (register input image to reference image).
     * @param _imageB Input image (register input image to reference image).
     * @param _costChoice Choice of cost functions, like correlation ratio or mutual information.
     * @param _DOF Degrees of freedom for registration
     * @param _interp Interpolation method used in transformations.
     * @param _rotateBegin Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEnd End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRate Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRate Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param doSubsample If true subsample
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     * 
     * <p>
     * Constructor without weighting and without advanced settings (num iter).
     * </p>
     */
    public AlgorithmRegELSUNCOAR2D(final ModelImage _imageA, final ModelImage _imageB, final int _costChoice, final int _DOF,
            final int _interp, final float _rotateBegin, final float _rotateEnd, final float _coarseRate,
            final float _fineRate, final boolean doSubsample, final int searchAlgorithm) {
    	this(_imageA, _imageB, _costChoice, _DOF, _interp, _rotateBegin, _rotateEnd, _coarseRate, _fineRate,
    			doSubsample, false, searchAlgorithm);
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
     * @param _rotateBegin Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEnd End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRate Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRate Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param doSubsample If true subsample
     * @param doMultiThread
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     * 
     * <p>
     * Constructor with weighting and without advanced settings (num iter).
     * </p>
     */
    public AlgorithmRegELSUNCOAR2D(final ModelImage _imageA, final ModelImage _imageB, final ModelImage _refWeight,
            final ModelImage _inputWeight, final int _costChoice, final int _DOF, final int _interp,
            final float _rotateBegin, final float _rotateEnd, final float _coarseRate, final float _fineRate,
            final boolean doSubsample, final boolean doMultiThread, final int searchAlgorithm) {
    	this(_imageA, _imageB, _costChoice, _DOF, _interp, _rotateBegin, _rotateEnd, _coarseRate, _fineRate,
    			doSubsample, doMultiThread, searchAlgorithm);

    	weighted = true;
        refWeight = _refWeight;
        inputWeight = _inputWeight;

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
     * @param _rotateBegin Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEnd End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRate Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRate Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param doSubsample If true subsample
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     * 
     * <p>
     * Constructor with weighting and without advanced settings (num iter).
     * </p>
     */
    public AlgorithmRegELSUNCOAR2D(final ModelImage _imageA, final ModelImage _imageB, final ModelImage _refWeight,
            final ModelImage _inputWeight, final int _costChoice, final int _DOF, final int _interp,
            final float _rotateBegin, final float _rotateEnd, final float _coarseRate, final float _fineRate,
            final boolean doSubsample, final int searchAlgorithm) {
    	this(_imageA, _imageB, _refWeight, _inputWeight, _costChoice, _DOF, _interp, _rotateBegin,
    			_rotateEnd, _coarseRate, _fineRate, doSubsample, false, searchAlgorithm);
    }

    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     * 
     * @param _imageA Reference image (register input image to reference image).
     * @param _imageB Input image (register input image to reference image).
     * @param _costChoice Choice of cost functions, like correlation ratio or mutual information.
     * @param _DOF Degrees of freedom for registration
     * @param _interp Interpolation method used in transformations.
     * @param _rotateBegin Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEnd End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRate Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRate Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param doSubsample If true subsample
     * @param doMultiThread
     * @param _baseNumIter Limits the number of iterations of ELSUNC algorithm. maxIter in the call to ELSUNC will
     *            be an integer multiple of baseNumIter
     * @param _numMinima Number of minima from level 8 to test at level 4
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     * 
     * <p>
     * Constructor without weighting and with advanced settings (num iter) set.
     * </p>
     */
    public AlgorithmRegELSUNCOAR2D(final ModelImage _imageA, final ModelImage _imageB, final int _costChoice, final int _DOF,
            final int _interp, final float _rotateBegin, final float _rotateEnd, final float _coarseRate,
            final float _fineRate, final boolean doSubsample, final boolean doMultiThread,
            final int _baseNumIter,
            final int _numMinima, final int searchAlgorithm) {
    	this(_imageA, _imageB, _costChoice, _DOF, _interp, _rotateBegin, _rotateEnd, _coarseRate, _fineRate,
    			doSubsample, doMultiThread, searchAlgorithm);

        baseNumIter = _baseNumIter;
        numMinima = _numMinima;

    }
    
    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     * 
     * @param _imageA Reference image (register input image to reference image).
     * @param _imageB Input image (register input image to reference image).
     * @param _costChoice Choice of cost functions, like correlation ratio or mutual information.
     * @param _DOF Degrees of freedom for registration
     * @param _interp Interpolation method used in transformations.
     * @param _rotateBegin Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEnd End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRate Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRate Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param doSubsample If true subsample
     * @param doMultiThread
     * @param _baseNumIter Limits the number of iterations of ELSUNC algorithm. maxIter in the call to ELSUNC will
     *            be an integer multiple of baseNumIter
     * @param _numMinima Number of minima from level 8 to test at level 4
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     * 
     * <p>
     * Constructor without weighting and with advanced settings (num iter) set.
     * </p>
     */
    public AlgorithmRegELSUNCOAR2D(final ModelImage _imageA, final ModelImage _imageB, final int _costChoice, final int _DOF,
            final int _interp, final float _rotateBegin, final float _rotateEnd, final float _coarseRate,
            final float _fineRate, final boolean doSubsample, final int _baseNumIter, final int _numMinima,
            final int searchAlgorithm) {
    	this(_imageA, _imageB, _costChoice, _DOF, _interp, _rotateBegin, _rotateEnd, _coarseRate, _fineRate,
    			doSubsample, false, _baseNumIter, _numMinima, searchAlgorithm);
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
     * @param _rotateBegin Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEnd End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRate Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRate Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param doSubsample If true subsample
     * @param doMultiThread
     * @param _baseNumIter Limits the number of iterations of ELSUNC algorithm. maxIter in the call to ELSUNC will
     *            be an integer multiple of baseNumIter
     * @param _numMinima Number of minima from level 8 to test at level 4
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     * 
     * <p>
     * Constructor with weighting and with advanced settings (num iter) set.
     * </p>
     */
    public AlgorithmRegELSUNCOAR2D(final ModelImage _imageA, final ModelImage _imageB, final ModelImage _refWeight,
            final ModelImage _inputWeight, final int _costChoice, final int _DOF, final int _interp,
            final float _rotateBegin, final float _rotateEnd, final float _coarseRate, final float _fineRate,
            final boolean doSubsample, final boolean doMultiThread, 
            final int _baseNumIter, final int _numMinima, final int searchAlgorithm) {
    	this(_imageA, _imageB, _costChoice, _DOF, _interp, _rotateBegin, _rotateEnd, _coarseRate, _fineRate,
    			doSubsample, doMultiThread, searchAlgorithm);

    	weighted = true;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;
        refWeight = _refWeight;
        inputWeight = _inputWeight;

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
     * @param _rotateBegin Beginning of coarse sampling range (i.e., -60 degrees).
     * @param _rotateEnd End of coarse sampling range (i.e., 60 degrees).
     * @param _coarseRate Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param _fineRate Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param doSubsample If true subsample
     * @param doMultiThread
     * @param _baseNumIter Limits the number of iterations of ELSUNC algorithm. maxIter in the call to ELSUNC will
     *            be an integer multiple of baseNumIter
     * @param _numMinima Number of minima from level 8 to test at level 4
     * @param searchAlgorithm ESLUNC, LEVENBERG_MARQUARDT, or NL2SOL.
     * 
     * <p>
     * Constructor with weighting and with advanced settings (num iter) set.
     * </p>
     */
    public AlgorithmRegELSUNCOAR2D(final ModelImage _imageA, final ModelImage _imageB, final ModelImage _refWeight,
            final ModelImage _inputWeight, final int _costChoice, final int _DOF, final int _interp,
            final float _rotateBegin, final float _rotateEnd, final float _coarseRate, final float _fineRate,
            final boolean doSubsample, final int _baseNumIter, final int _numMinima, final int searchAlgorithm) {
    	this(_imageA, _imageB, _refWeight, _inputWeight, _costChoice, _DOF, _interp, _rotateBegin, _rotateEnd,
    			_coarseRate, _fineRate, doSubsample, false, _baseNumIter, _numMinima, searchAlgorithm);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the center of mass (gravity) of a 2D image. In image space where the upper left hand corner of the
     * image is 0,0. The x axis goes left to right, y axis goes top to bottom. (i.e. the right hand rule). One could
     * simply multiply by voxel resolutions.
     * 
     * @param image DOCUMENT ME!
     * @param wgtImage DOCUMENT ME!
     * @param isColor DOCUMENT ME!
     * 
     * @return the center of mass as a 2D point
     */
    public Vector2f calculateCenterOfMass2D(final ModelSimpleImage image, final ModelSimpleImage wgtImage,
            final boolean isColor) {
        int x, y, c;
        float diff;

        final Vector2f cogPt = new Vector2f(0, 0);

        double voxVal = 0.0, total = 0.0, wgtVal = 0.0;

        if (isColor) {

            if (wgtImage == null) {

                for (y = 0; y < image.yDim; y++) {

                    for (x = 0; x < image.xDim; x++) {

                        for (c = 1; c <= 3; c++) {
                            voxVal = image.data[ (4 * ( (y * image.xDim) + x)) + c];
                            cogPt.X += voxVal * x;
                            cogPt.Y += voxVal * y;
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

                for (y = 0; y < image.yDim; y++) {

                    for (x = 0; x < image.xDim; x++) {
                        wgtVal = wgtImage.data[ (y * image.xDim) + x];

                        for (c = 1; c <= 3; c++) {
                            voxVal = image.data[ (4 * ( (y * image.xDim) + x)) + c];
                            cogPt.X += wgtVal * voxVal * x;
                            cogPt.Y += wgtVal * voxVal * y;
                            total += wgtVal * voxVal;
                        }
                    }
                }
            }
        } // if (isColor)
        else { // black and white

            if (wgtImage == null) {

                for (y = 0; y < image.yDim; y++) {

                    for (x = 0; x < image.xDim; x++) {
                        voxVal = image.data[ (y * image.xDim) + x] - image.min;
                        cogPt.X += voxVal * x;
                        cogPt.Y += voxVal * y;
                        total += voxVal;
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

                for (y = 0; y < image.yDim; y++) {

                    for (x = 0; x < image.xDim; x++) {
                        voxVal = image.data[ (y * image.xDim) + x] - image.min;
                        wgtVal = wgtImage.data[ (y * image.xDim) + x];
                        cogPt.X += wgtVal * voxVal * x;
                        cogPt.Y += wgtVal * voxVal * y;
                        total += wgtVal * voxVal;

                    }
                }
            }
        } // else black and white

        if (total != 0) {
            cogPt.X /= total;
            cogPt.Y /= total;
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

    public void drawLine(final int xdim, final int ydim, final int zdim, final float[] image, final float value,
            final Vector<Point3D> path) {
        if (path == null) {
            return;
        }
        for (int i = 0; i < path.size(); i++) {
            final Point3D p3i = path.elementAt(i);
            if (image[p3i.z * xdim * ydim + p3i.y * xdim + p3i.x] > 300) {
                image[p3i.z * xdim * ydim + p3i.y * xdim + p3i.x] = value - 100;
            } else {
                image[p3i.z * xdim * ydim + p3i.y * xdim + p3i.x] = value;
            }
        }
    }

    public Vector<Vector3f> param2Coordinates(final double startX, final double endX, final double stepX,
            final double startY, final double endY, final double stepY, final double startZ, final double endZ,
            final double stepZ, final Vector<Vector3f> path) {
        if (path == null) {
            return null;
        }

        final Vector<Vector3f> coordPath = new Vector<Vector3f>(path.size());
        for (int i = 0; i < path.size(); i++) {
            final Vector3f p3d = path.elementAt(i);
            final Vector3f p3f = new Vector3f();
            p3f.X = (float) ( (p3d.Y - startX) / stepX);
            p3f.Y = (float) ( (p3d.Z - startY) / stepY);
            p3f.Z = (float) ( (p3d.X - startZ) / stepZ);
            coordPath.add(p3f);
        }
        return coordPath;
    }

    public Vector<Point3D> findPointsOfLine(final Vector<Vector3f> realPath) {
        if (realPath == null) {
            return null;
        }

        final Vector<Point3D> imagePath = new Vector<Point3D>(realPath.size() * 2);
        for (int i = 0; i < realPath.size() - 1; i++) {
            final Vector3f p3fFrom = realPath.elementAt(i);
            final Vector3f p3fTo = realPath.elementAt(i + 1);
            if (p3fFrom.equals(p3fTo)) {
                continue;
            }
            midpointLine(p3fFrom, p3fTo, imagePath);
        }
        Point3D previousPoint = imagePath.elementAt(0);

        for (int i = 1; i < imagePath.size(); i++) {
            final Point3D currentPoint = imagePath.elementAt(i);
            if (currentPoint.equals(previousPoint)) {
                imagePath.remove(i--);
            } else {
                previousPoint = currentPoint;
            }
        }
        return imagePath;
    }

    public void midpointLine(final Vector3f p0, final Vector3f p1, final Vector<Point3D> imagePath) {
        if (Math.abs(p1.X - p0.X) >= Math.abs(p1.Y - p0.Y) && Math.abs(p1.X - p0.X) >= Math.abs(p1.Z - p0.Z)) {
            final float a1 = (p1.Y - p0.Y) / (p1.X - p0.X);
            final float b1 = p0.Y - a1 * p0.X;
            final float a2 = (p1.Z - p0.Z) / (p1.X - p0.X);
            final float b2 = p0.Z - a2 * p0.X;
            int x = (int) (p0.X + 0.5);
            float fy = p0.Y;
            float fz = p0.Z;
            int y = (int) p0.Y;
            int z = (int) p0.Z;

            while (x < p1.X) {
                final float d1 = line(a1, b1, x, y + 0.5f);
                if (d1 < 0) {
                    y++;
                }
                final float d2 = line(a2, b2, x, z + 0.5f);
                if (d2 < 0) {
                    z++;
                }
                final Point3D p3i = new Point3D(x, y, z);
                imagePath.add(p3i);
                fy += a1;
                fz += a2;
                x++;
                y = (int) fy;
                z = (int) fz;
            }
        } else if (Math.abs(p1.Y - p0.Y) >= Math.abs(p1.X - p0.X) && Math.abs(p1.Y - p0.Y) >= Math.abs(p1.Z - p0.Z)) {
            final float a1 = (p1.X - p0.X) / (p1.Y - p0.Y);
            final float b1 = p0.X - a1 * p0.Y;
            final float a2 = (p1.Z - p0.Z) / (p1.Y - p0.Y);
            final float b2 = p0.Z - a2 * p0.Y;
            int y = (int) (p0.Y + 0.5);
            float fx = p0.X;
            float fz = p0.Z;
            int x = (int) p0.X;
            int z = (int) p0.Z;

            while (y < p1.Y) {
                final float d1 = line(a1, b1, y, x + 0.5f);
                if (d1 < 0) {
                    x++;
                }
                final float d2 = line(a2, b2, y, z + 0.5f);
                if (d2 < 0) {
                    z++;
                }
                final Point3D p3i = new Point3D(x, y, z);
                imagePath.add(p3i);
                fx += a1;
                fz += a2;
                y++;
                x = (int) fx;
                z = (int) fz;
            }
        } else {
            final float a1 = (p1.X - p0.X) / (p1.Z - p0.Z);
            final float b1 = p0.X - a1 * p0.Z;
            final float a2 = (p1.Y - p0.Y) / (p1.Z - p0.Z);
            final float b2 = p0.Z - a2 * p0.Y;
            int z = (int) (p0.Z + 0.5);
            float fy = p0.Y;
            float fx = p0.X;
            int y = (int) p0.Y;
            int x = (int) p0.X;

            while (z < p1.Z) {
                final float d1 = line(a1, b1, z, x + 0.5f);
                if (d1 < 0) {
                    x++;
                }
                final float d2 = line(a2, b2, z, y + 0.5f);
                if (d2 < 0) {
                    y++;
                }
                final Point3D p3i = new Point3D(x, y, z);
                imagePath.add(p3i);
                fx += a1;
                fy += a2;
                z++;
                y = (int) fy;
                x = (int) fx;
            }
        }

    }

    public float line(final float a, final float b, final int x, final float y) {
        return y - a * x - b;
    }

    /**
     * Runs the image registration. Blurs the images based on what their minimum resolutions are. The reference image is
     * blurred if one of the input image resolutions is 50% or more bigger than the corresponding resolution in the
     * reference image; likewise, the input image is blurred if one of the reference image resolutions is 50% or more
     * bigger than the corresponding resolution in the input image. Thus, it is unlikely, though not impossible, that
     * both images will be blurred. The images are then transformed into isotropic voxels. The resolutions of the two
     * images after the isotropic transformation will be the same in all dimensions. That resolution will equal the
     * minimum of the minimums of each image's resolutions: Min( Min (resolutions of ref image, resolutions of input
     * image) ). If the images are weighted, the weight images are blurred and transformed into isotropic voxels in the
     * same manner as the originals. Then the images are subsampled by 2, 4, and 8. If the images are too small they
     * will not be subsampled down to the smallest level; if they are too big, they will be subsampled to 16. The same
     * is done with the weight images if necessary. The function levelEight is called with the images subsampled by 8;
     * it returns two vectors with minima. Then the function levelFour is called with images subsampled by 4 and the two
     * vectors; it returns one vector of minima. The function levelTwo is called with images subsampled by 2 and the
     * vector; it returns an "answer" in the form of a MatrixListItem, which is a convenient way of storing the point,
     * the matrix, and the cost of the minimum. Then the function levelOne is called with the minimum; it returns a
     * final "answer", or minimum, which will then be accessed by the dialog that called this algorithm.
     */
    public void runAlgorithm() {
        // -javaagent:E:\MagicConsulting\mipav\src\lib\profile.jar
        // -Dprofile.properties=E:\MagicConsulting\mipav\src\lib\profile.properties
        // Profile.clear();
        // Profile.start();

        final long startTime = System.currentTimeMillis();
        int i;

        if (refImage.getNDims() != 2) {
            MipavUtil.displayError("" + refImage.getNDims() + "D registration not supported.");
            disposeLocal();

            return;
        }

        if (inputImage.getNDims() != 2) {
            MipavUtil.displayError("" + inputImage.getNDims() + "D registration not supported.");
            disposeLocal();

            return;
        }

        float minSampleRef = 1.0f;
        float minSampleInput = 1.0f;
        float minSample = 1.0f;

        minSampleRef = Math.min(resRef[0], resRef[1]);
        minSampleInput = Math.min(resInput[0], resInput[1]);

        minSample = Math.min(minSampleRef, minSampleInput); // min of the min resolutions of the two datasets

        if ( (resRef[0] == resRef[1]) && (resRef[0] == minSample)) {
            resampleRef = false;
        } else {
            resampleRef = true;
        }

        if ( (resInput[0] == resInput[1]) && (resInput[0] == minSample)) {
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
            extentsRefIso = new int[2];
            resRefIso = new float[2];
            extentsInputIso = new int[2];
            resInputIso = new float[2];
            sigmasRef = new float[2];
            sigmasInput = new float[2];
        } catch (final OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("Out of memory in AlgorithmOAR2D.");
            disposeLocal();

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
            sigmasRef[i] = ( (resInput[i] - resRef[i]) / resRef[i]) * 0.424f; // / 2.36 sigma = x --> FWHM 0.424 =
            // 1/2.36

            if (sigmasRef[i] < 0.5f) {
                sigmasRef[i] = 0.0f;
            } else {
                blurRef = true;
            }

            sigmasInput[i] = ( (resRef[i] - resInput[i]) / resInput[i]) * 0.424f;

            if (sigmasInput[i] < 0.5f) {
                sigmasInput[i] = 0.0f;
            } else {
                blurInput = true;
            }
        }

        Preferences.debug(getConstructionInfo(),Preferences.DEBUG_ALGORITHM);

        if (blurRef) {

            if (doColor) {
                blurredRef = new ModelImage(ModelStorageBase.ARGB_FLOAT, refImage.getExtents(), "BlurRef");
            } else {
                blurredRef = new ModelImage(ModelStorageBase.FLOAT, refImage.getExtents(), "BlurRef");
            }

            // update resolutions
            final FileInfoBase[] fileInfo = blurredRef.getFileInfo();
            fileInfo[0].setResolutions(refImage.getFileInfo()[0].getResolutions());

            final AlgorithmGaussianBlur blur = new AlgorithmGaussianBlur(blurredRef, refImage, sigmasRef, true, false);

            if (doColor) {
                blur.setRed(true);
                blur.setGreen(true);
                blur.setBlue(true);
            }

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

            final FileInfoBase[] fileInfo = blurredRef.getFileInfo();
            fileInfo[0].setResolutions(refImage.getFileInfo()[0].getResolutions());

            final AlgorithmGaussianBlur blur2 = new AlgorithmGaussianBlur(blurredInput, inputImage, sigmasInput, true,
                    false);

            if (doColor) {
                blur2.setRed(true);
                blur2.setGreen(true);
                blur2.setBlue(true);
            }

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
            transform = new AlgorithmTransform(blurredRef, new TransMatrix(3), interp, resRefIso[0], resRefIso[1],
                    extentsRefIso[0], extentsRefIso[1], false, true, false);
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

            // System.gc();

            imageRefIso = transform.getTransformedImage();

            if (transform != null) {
                transform.finalize();
            }

            simpleRef = new ModelSimpleImage(imageRefIso.getExtents(), imageRefIso.getFileInfo(0).getResolutions(),
                    imageRefIso);
        } // if (resampleRef)
        else {
            simpleRef = new ModelSimpleImage(refImage.getExtents(), refImage.getFileInfo(0).getResolutions(), refImage);
        }

        maxDim = simpleRef.xDim;

        if (simpleRef.yDim > maxDim) {
            maxDim = simpleRef.yDim;
        }

        if ( (weighted)
                && ( (resampleRef) || ( (refWeight.getFileInfo(0).getResolutions()[0] != refImage.getFileInfo(0)
                        .getResolutions()[0]) || (refWeight.getFileInfo(0).getResolutions()[1] != refImage.getFileInfo(
                        0).getResolutions()[1])))) {
            transform = new AlgorithmTransform(refWeight, new TransMatrix(3), interp, resRefIso[0], resRefIso[1],
                    extentsRefIso[0], extentsRefIso[1], false, true, false);
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

        // System.gc();

        if (resampleInput) {
            transform2 = new AlgorithmTransform(blurredInput, new TransMatrix(3), interp, resInputIso[0],
                    resInputIso[1], extentsInputIso[0], extentsInputIso[1], false, true, false);
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

            // System.gc();

            imageInputIso = transform2.getTransformedImage();

            if (transform2 != null) {
                transform2.finalize();
            }

            simpleInput = new ModelSimpleImage(imageInputIso.getExtents(), imageInputIso.getFileInfo(0)
                    .getResolutions(), imageInputIso);
        } // if (resampleInput)
        else {
            simpleInput = new ModelSimpleImage(inputImage.getExtents(), inputImage.getFileInfo(0).getResolutions(),
                    inputImage);

        }

        if (simpleInput.xDim > maxDim) {
            maxDim = simpleInput.xDim;
        }

        if (simpleInput.yDim > maxDim) {
            maxDim = simpleInput.yDim;
        }

        if ( (weighted)
                && ( (resampleInput) || ( (inputWeight.getFileInfo(0).getResolutions()[0] != inputImage.getFileInfo(0)
                        .getResolutions()[0]) || (inputWeight.getFileInfo(0).getResolutions()[1] != inputImage
                        .getFileInfo(0).getResolutions()[1])))) {
            transform2 = new AlgorithmTransform(inputWeight, new TransMatrix(3), interp, resInputIso[0],
                    resInputIso[1], extentsInputIso[0], extentsInputIso[1], false, true, false);
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

        // System.gc();

        int subMinFactor = 15000;

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

            if (DOF >= 3) {

                if ( (weightedRefPixels > subMinFactor) && (weightedInputPixels > subMinFactor) && doSubsample) {
                    simpleWeightRefSub2 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleWeightRef, false);
                    simpleWeightInputSub2 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleWeightInput, false);
                } else {
                    simpleWeightRefSub2 = simpleWeightRef;
                    simpleWeightInputSub2 = simpleWeightInput;
                    allowLevel2 = false;
                }

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

                if ( (weightedRefPixelsSub2 > subMinFactor) && (weightedInputPixelsSub2 > subMinFactor) && doSubsample) {
                    simpleWeightRefSub4 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleWeightRefSub2, false);
                    simpleWeightInputSub4 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleWeightInputSub2, false);
                } else {
                    simpleWeightRefSub4 = simpleWeightRefSub2;
                    simpleWeightInputSub4 = simpleWeightInputSub2;
                    allowLevel4 = false;
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

                if ( (weightedRefPixelsSub4 > subMinFactor) && (weightedInputPixelsSub4 > subMinFactor) && doSubsample) {
                    simpleWeightRefSub8 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleWeightRefSub4, false);
                    simpleWeightInputSub8 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleWeightInputSub4, false);

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
                            && doSubsample) {
                        ModelSimpleImage simpleWeightRefSub16;
                        ModelSimpleImage simpleWeightInputSub16;
                        Preferences.debug("Sub sampled level 8 to 16  *********** \n",Preferences.DEBUG_ALGORITHM);

                        simpleWeightRefSub16 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleWeightRefSub8, false);
                        simpleWeightInputSub16 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleWeightInputSub8, false);

                        simpleWeightRefSub8 = simpleWeightRefSub16;
                        simpleWeightInputSub8 = simpleWeightInputSub16;
                    } else {
                        allowLevel16 = false;
                    }
                } else {
                    simpleWeightRefSub8 = simpleWeightRefSub4;
                    simpleWeightInputSub8 = simpleWeightInputSub4;
                    allowLevel8 = false;
                }

                Preferences.debug("Weighted ref subsampled 2 = " + simpleWeightRefSub2 + "Weighted ref subsampled 4 = "
                        + simpleWeightRefSub4 + "Weighted ref subsampled 8 = " + simpleWeightRefSub8
                        + "Weighted input subsampled 2 = " + simpleWeightInputSub2 + "Weighted input subsampled 4 = "
                        + simpleWeightInputSub4 + "Weighted input subsampled 8 = " + simpleWeightInputSub8,
                        Preferences.DEBUG_ALGORITHM);
            } // if (DOF >= 3)

        }

        if (DOF >= 3) {

            if (doColor) {
                subMinFactor *= 4;
            }

            if ( (simpleRef.dataSize > subMinFactor) && (simpleInput.dataSize > subMinFactor) && allowLevel2
                    && doSubsample) {
                simpleRefSub2 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleRef, doColor);
                simpleInputSub2 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleInput, doColor);
                level1Factor = 2.0f;
            } else {
                simpleRefSub2 = simpleRef;
                simpleInputSub2 = simpleInput;
            }

            if ( (simpleRefSub2.dataSize > subMinFactor) && (simpleInputSub2.dataSize > subMinFactor) && allowLevel4
                    && doSubsample) {
                simpleRefSub4 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleRefSub2, doColor);
                simpleInputSub4 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleInputSub2, doColor);
                level2Factor = 2.0f;
            } else {
                simpleRefSub4 = simpleRefSub2;
                simpleInputSub4 = simpleInputSub2;
            }

            if ( (simpleRefSub4.dataSize > subMinFactor) && (simpleInputSub4.dataSize > subMinFactor) && allowLevel8
                    && doSubsample) {
                simpleRefSub8 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleRefSub4, doColor);
                simpleInputSub8 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleInputSub4, doColor);
                level4Factor = 2.0f;

                // For really big images subsample level 8 again!
                if ( (simpleRefSub8.dataSize > subMinFactor) && (simpleInputSub8.dataSize > subMinFactor)
                        && allowLevel16) {
                    ModelSimpleImage simpleRefSub16;
                    ModelSimpleImage simpleInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16  *********** \n",Preferences.DEBUG_ALGORITHM);

                    simpleRefSub16 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleRefSub8, doColor);
                    simpleInputSub16 = AlgorithmRegELSUNCOAR2D.subsampleBy2(simpleInputSub8, doColor);

                    simpleRefSub8 = simpleRefSub16;
                    simpleInputSub8 = simpleInputSub16;
                    level4Factor = 4.0f;
                }
            } else {
                simpleRefSub8 = simpleRefSub4;
                simpleInputSub8 = simpleInputSub4;
            }

            Preferences.debug("Level 1 factor = " + level1Factor + "\n" + "Level 2 factor = " + level2Factor + "\n"
                    + "Level 4 factor = " + level4Factor + "\n" + "Ref subsampled 2 = " + simpleRefSub2
                    + "Ref subsampled 4 = " + simpleRefSub4 + "Ref subsampled 8 = " + simpleRefSub8
                    + "Input subsampled 2 = " + simpleInputSub2 + "Input subsampled 4 = " + simpleInputSub4
                    + "Input subsampled 8 = " + simpleInputSub8,Preferences.DEBUG_ALGORITHM);
        } // if (DOF >= 3)

        fireProgressStateChanged("Registering images", "Beginning registration");

        if (costChoice >= AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU) {
            m_kGPUCost = ImageRegistrationGPU.create(simpleRef, simpleInput);
            if (m_kGPUCost == null) {
                MipavUtil.displayError("Not enough memory on the GPU, reverting to CPU registration");
                costChoice = AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
            }
        }
        long time = System.currentTimeMillis();

        /* If the algorithm is to run in "Brute Force" mode, do so now: */
        if (m_bBruteForce == true) {
            algorithmBruteForce();

            return;
        }
        if (DOF >= 3) {
            Preferences.debug(" Starting level 8 ************************************************\n",
            		Preferences.DEBUG_ALGORITHM);

            final Vector<MatrixListItem>[] minimas = levelEight(simpleRefSub8, simpleInputSub8);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 8 min = " + (time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }

            Preferences.debug(" Starting level 4 ************************************************\n",
            		Preferences.DEBUG_ALGORITHM);

            final Vector<MatrixListItem> minima = levelFour(simpleRefSub4, simpleInputSub4, minimas[0], minimas[1]);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 4  min = " + (time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }

            Preferences.debug(" Starting level 2 ************************************************\n",
            		Preferences.DEBUG_ALGORITHM);

            final MatrixListItem item = levelTwo(simpleRefSub2, simpleInputSub2, minima);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 2 min = " + (time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
            time = System.currentTimeMillis();

            if (threadStopped) {
                finalize();

                return;
            }

            Preferences.debug(" Starting level 1 ************************************************\n",
            		Preferences.DEBUG_ALGORITHM);
            answer = levelOne(simpleRef, simpleInput, item);
            // createTerrain();
        } // if (DOF >= 3)
        else if ( DOF == 1 ){
            Preferences.debug(" Starting level 1 ************************************************\n",
            		Preferences.DEBUG_ALGORITHM);
            answer = levelOne2DRotation(simpleRef, simpleInput);
        }
        else {
            Preferences.debug(" Starting level 1 ************************************************\n",
            		Preferences.DEBUG_ALGORITHM);
            answer = levelOne2D(simpleRef, simpleInput);
        }

        time = System.currentTimeMillis() - time;
        Preferences.debug(" Level 1 min = " + (time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
        time = System.currentTimeMillis();

        if (threadStopped) {
            finalize();

            return;
        }

        answer.matrixd.Inverse();

        disposeLocal();
        finalize();
        setCompleted(true);
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

    public double getCost() {
        return answer.cost;
    }

    public double getRotation() {
        return answer.initial[0];
    }

    @SuppressWarnings("unused")
    private void createTerrain() {
        searchOptimalPath();
        print(optimalPath, "Optimal Path: ");
        final long startTime = System.nanoTime();
        cost = new AlgorithmCostFunctions2D(simpleRef, simpleInput, costChoice, 256, 1);
        maxIter = baseNumIter * 2;
        final Vector2f cog = calculateCenterOfMass2D(simpleInput, simpleWeightInput, doColor);
        final double[] initial = new double[7];

        initial[0] = 0; // initial rotation
        initial[1] = 0; // initial translations
        initial[2] = 0;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing
        final float xFrom = 17.2f;
        final float xTo = 42.81f;
        final float xStep = 0.2f;
        final float yFrom = 27.2f;
        final float yTo = 52.81f;
        final float yStep = 0.2f;
        final float zFrom = 7.2f;
        final float zTo = 32.81f;
        final float zStep = 0.2f;
        final Vector<Vector3f> realPath = param2Coordinates(xFrom, xTo, xStep, yFrom, yTo, yStep, zFrom, zTo, zStep,
                optimalPath);
        final Vector<Point3D> imagePath = findPointsOfLine(realPath);
        print(imagePath, "Final Path:");
        final Vector<Vector3f> originalPath2 = new Vector<Vector3f>(30);
        for (final Vector3f element : AlgorithmRegELSUNCOAR2D.originalPath) {
            originalPath2.add(element);
        }
        final Vector<Vector3f> origRealPath = param2Coordinates(xFrom, xTo, xStep, yFrom, yTo, yStep, zFrom, zTo,
                zStep, originalPath2);
        final Vector<Point3D> origImagePath = findPointsOfLine(origRealPath);
        print(origImagePath, "Original Final Path:");
        final AlgorithmELSUNCOpt2D elsunc = new AlgorithmELSUNCOpt2D(this, cog, DOF, cost, getTolerance(DOF),
                maxIter, rigidFlag, searchAlgorithm);
        final float[] terrain = elsunc.createTerrain(xFrom, xTo, xStep, yFrom, yTo, yStep, zFrom, zTo, zStep);
        final int xdim = (int) ( (xTo - xFrom) / xStep);
        final int ydim = (int) ( (yTo - yFrom) / yStep);
        final int zdim = (int) ( (zTo - zFrom) / zStep);
        drawLine(xdim, ydim, zdim, terrain, 300f, origImagePath);
        drawLine(xdim, ydim, zdim, terrain, 500f, imagePath);
        final ModelImage resultImage = new ModelImage(ModelStorageBase.FLOAT, new int[] {xdim, ydim, zdim},
                "LevelOne_Terrain");
        try {
            resultImage.importData(0, terrain, true);
        } catch (final IOException e) {
            e.printStackTrace();
        }

        final FileInfoBase[] fileInfos = resultImage.getFileInfo();
        for (final FileInfoBase element : fileInfos) {
            element.setResolutions(new float[] {xStep, yStep, zStep});
            element.setOrigin(new float[] {xFrom, yFrom, zFrom});
        }
        try {
            new ViewJFrameImage(resultImage, null, new Dimension(xdim, ydim));
        } catch (final OutOfMemoryError error) {
            gov.nih.mipav.view.MipavUtil.displayError("Out of memory: unable to open new frame");
        }

        System.out.println("Time consumed by createTerrain(): " + (System.nanoTime() - startTime));
    }

    private void searchOptimalPath() {
        if (paths == null) {
            return;
        }
        optimalPath = new Vector<Vector3f>(100);
        optimalPath.addAll(paths[5].get(0));
        optimalPath.addAll(paths[4].get(0));
        int index = indexOf(paths[3], optimalPath.elementAt(0));
        if (index < 0) {
            System.out.println("######3: Something is wrong, please double check! #######");
            // return;
        } else {
            optimalPath.addAll(paths[3].get(index));
        }
        index = indexOf(paths[2], optimalPath.elementAt(0));
        if (index < 0) {
            System.out.println("######2: Something is wrong, please double check! #######");
            // return;
        } else {
            optimalPath.addAll(paths[2].get(index));
        }
        index = indexOf(paths[1], optimalPath.elementAt(0));
        if (index < 0) {
            index = indexOf(paths[0], optimalPath.elementAt(0));
            if (index < 0) {
                System.out.println("######1: Something is wrong, please double check! #######");
                // return;
            } else {
                optimalPath.addAll(paths[0].get(index));
            }
        } else {
            optimalPath.addAll(paths[1].get(index));
        }
    }

    private int indexOf(final Vector<Vector<Vector3f>> aPaths, final Vector3f tp3d) {
        for (int i = 0; i < aPaths.size(); i++) {
            final Vector<Vector3f> aPath = aPaths.get(i);
            if (tp3d.equals(aPath.get(aPath.size() - 1))) {
                return i;
            }
        }
        return -1;
    }

    @SuppressWarnings("unchecked")
    public void print(final Vector data, final String message) {
        if (data == null || data.size() == 0) {
            return;
        }
        System.out.println(message + "**************");
        for (int i = 0; i < data.size(); i++) {
            final Object obj = data.get(i);
            final StringBuffer sb = new StringBuffer("");
            if (obj instanceof MatrixListItem) {
                final MatrixListItem item = (MatrixListItem) data.get(i);
                for (final double element : item.initial) {
                    sb.append(element);
                    sb.append(",");
                }
            } else if (obj instanceof Vector3f) {
                final Vector3f p3d = (Vector3f) data.get(i);
                sb.append(p3d.X);
                sb.append(",");
                sb.append(p3d.Y);
                sb.append(",");
                sb.append(p3d.Z);
            } else if (obj instanceof Point3D) {
                final Point3D p3i = (Point3D) data.get(i);
                sb.append(p3i.x);
                sb.append(",");
                sb.append(p3i.y);
                sb.append(",");
                sb.append(p3i.z);
            }
            System.out.println(sb.toString());
        }
    }

    public void print(final MatrixListItem item, final String message) {
        if (item == null) {
            return;
        }
        System.out.println(message + "**************");
        final StringBuffer sb = new StringBuffer("");
        for (final double element : item.initial) {
            sb.append(element);
            sb.append(",");
        }
        System.out.println(sb.toString());
    }

    /**
     * setBruteForce. Tells the algorithm to do a brute-force optimization, where it will iterate of the the input
     * rotation, xscale, yscale, and translation ranges calculating the cost function at each point and returing the
     * minimum. No optimization with the brute-force approach.
     * 
     * @param bOn DOCUMENT ME!
     * @param fRotationRange DOCUMENT ME!
     * @param fXScaleRange DOCUMENT ME!
     * @param fYScaleRange DOCUMENT ME!
     * @param iScaleSteps DOCUMENT ME!
     * @param iTranslationRange DOCUMENT ME!
     */
    public void setBruteForce(final boolean bOn, final float fRotationRange, final float fXScaleRange,
            final float fYScaleRange, final int iScaleSteps, final int iTranslationRange) {
        m_bBruteForce = bOn;
        m_fRotationRange = fRotationRange;
        m_fXScaleRange = fXScaleRange;
        m_fYScaleRange = fYScaleRange;
        m_iTranslationRange = iTranslationRange;
        m_iScaleSteps = iScaleSteps;
    }
    
    public void setJTEM(boolean bOn)
    {
    	doJTEM = bOn;
    }

    /**
     * Takes a simple image and subsamples it by 2, interpolating so that the new values are averages.
     * 
     * @param srcImage Image to subsample.
     * @param isColor DOCUMENT ME!
     * 
     * @return Subsampled image.
     */
    private static ModelSimpleImage subsampleBy2(final ModelSimpleImage srcImage, final boolean isColor) {
        return srcImage.subSample2dBy2(isColor);
    }

    /**
     * Compute the brute-force solution. Iterates over a range of angles, scales and translations, calculating the cost
     * function for each new position, returning the one with minimum cost:
     */
    private void algorithmBruteForce() {

        /* Sets the cost function: */
        final AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(simpleRef, simpleInput, costChoice, 32, 1);

        /*
         * Currently, brute force is activated from the Mosaic window, so the registration is always a weighted
         * registration:
         */
        cost.setRefWgtImage(simpleWeightRef);
        cost.setInputWgtImage(simpleWeightInput);

        /* Set the center of gravity to be the center of the input image: */
        final Vector2f cog = new Vector2f(simpleInput.xDim / 2, simpleInput.yDim / 2);

        /* Initialize the transform to the identity: */
        final double[] initial = new double[7];
        initial[0] = 0; // initial rotation
        initial[1] = 0; // initial translations
        initial[2] = 0;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        AlgorithmELSUNCOpt2D elsunc;
        maxIter = baseNumIter;
        elsunc = new AlgorithmELSUNCOpt2D(this, cog, 7, cost, getTolerance(7), maxIter, false, searchAlgorithm);

        final Vector<MatrixListItem> minima = new Vector<MatrixListItem>();

        /* Count the number of times the cost function is called: */
        int iBruteCount = 1;
        int iTotal = (int) ( ( (m_iTranslationRange * 2) + 1) * ( (m_iTranslationRange * 2) + 1) * ( (m_fRotationRange * 2) + 1));

        if (m_fXScaleRange != 0) {
            iTotal *= (m_iScaleSteps + 1);
        }

        if (m_fYScaleRange != 0) {
            iTotal *= (m_iScaleSteps + 1);
        }

        if (iTotal > iBruteCount) {
            Preferences.debug("Computing " + iTotal + " cost functions...\n", Preferences.DEBUG_ALGORITHM);
        }

        /* The scale increment for x and y: */
        final float fXScaleIncrement = (m_fXScaleRange * 2) / (m_iScaleSteps);
        final float fYScaleIncrement = (m_fYScaleRange * 2) / (m_iScaleSteps);

        /* initial angle: */
        float angle = -m_fRotationRange;

        /* Initial scales: */
        float xScale = -m_fXScaleRange;
        float yScale = -m_fYScaleRange;

        /* Initial translations: */
        int dx = -m_iTranslationRange;
        int dy = -m_iTranslationRange;
        boolean bIncrementNext = false;
        boolean bDone = false;

        /*
         * Test to see if any parameters are set, if all zero, return the identity transform:
         */
        double dMinCost = Float.MAX_VALUE;
        double dCost = 0;

        if ( (m_fRotationRange == 0) && (m_iTranslationRange == 0)
                && ( ( (m_fXScaleRange == 0) && (m_fYScaleRange == 0)) || (m_iScaleSteps == 0))) {
            bDone = true;

            /* initial must already be set to identity: */
            dCost = elsunc.measureCost(initial);
            minima.add(new MatrixListItem(dCost, elsunc.convertToMatrix(initial), initial));
            dMinCost = dCost;
        }

        /*
         * Otherwise loop until all the permutations of rotations, scales, and translations have been tested:
         */
        /*
         * Note, this could have been several nested loops, however since any of the loops may be executed 0 times (and
         * therefor the inner loops never executed) the nested loops are flattened into one. The progression is as
         * follows: first angle is incremented, when angle reaches the max value, then the translation in x is
         * incremented by one and angle is reset to the initial value. Angle is incremented again until it reaches max
         * and then translation in x is incremented a second time. This repeats until the translation in x is at max,
         * when this occurs dx is reset and dy is incremented. And so on until all ranges (rotation, x,y translation,
         * x,y scale have been maxed...
         */
        while (bDone == false) {

            /* Set progress bar: */
            fireProgressStateChanged((int) (100 * (float) iBruteCount / iTotal));

            /* test current permutation and store results: */
            initial[0] = angle;
            initial[1] = dx;
            initial[2] = dy;
            initial[3] = 1 / (1.0 + (xScale / 100f));
            initial[4] = 1 / (1.0 + (yScale / 100f));

            dCost = elsunc.measureCost(initial);
            minima.add(new MatrixListItem(dCost, elsunc.convertToMatrix(initial), initial));

            if (dCost < dMinCost) {
                dMinCost = dCost;
            }

            Preferences.debug("... testing cost " + iBruteCount + " angle = " + angle + " dx = " + dx + " dy = " + dy
                    + " scale x = " + xScale + " scale y = " + yScale + " ... cost = " + dCost
                    + " ... min cost sor far = " + dMinCost + "\n", Preferences.DEBUG_ALGORITHM);
            iBruteCount++;

            /* increment transform values: */
            bIncrementNext = true;

            /* Increment angle, if we are optimizing over angles: */
            if (m_fRotationRange != 0) {
                bIncrementNext = false;
                bDone = false;
                angle += 1;

                if (angle > m_fRotationRange) {
                    angle = -m_fRotationRange;
                    bIncrementNext = true;
                    bDone = true;
                }
            }

            /*
             * If we are optimizing over dx and angle is at max, increment dx by one:
             */
            if ( (m_iTranslationRange != 0) && bIncrementNext) {
                bIncrementNext = false;
                bDone = false;
                dx += 1;

                if (dx > m_iTranslationRange) {
                    dx = -m_iTranslationRange;
                    bIncrementNext = true;
                    bDone = true;
                }
            }

            /*
             * If we are optimizing over dy and angle and dx are at max, increment dy by one:
             */
            if ( (m_iTranslationRange != 0) && bIncrementNext) {
                bIncrementNext = false;
                bDone = false;
                dy += 1;

                if (dy > m_iTranslationRange) {
                    dy = -m_iTranslationRange;
                    bIncrementNext = true;
                    bDone = true;
                }
            }

            /*
             * If we are optimizing over scale x and angle, dx, dy are at max, increment scale x by xscale increment:
             */
            if ( (m_iScaleSteps != 0) && (m_fXScaleRange != 0) && bIncrementNext) {
                bIncrementNext = false;
                bDone = false;
                xScale += fXScaleIncrement;

                if (xScale > m_fXScaleRange) {
                    xScale = -m_fXScaleRange;
                    bIncrementNext = true;
                    bDone = true;
                }
            }

            /*
             * If we are optimizing over scale y and angle, dx, dy, and scale x are at max, increment scale y by yscale
             * increment:
             */
            if ( (m_iScaleSteps != 0) && (m_fYScaleRange != 0) && bIncrementNext) {
                bIncrementNext = false;
                bDone = false;
                yScale += fYScaleIncrement;

                if (yScale > m_fYScaleRange) {
                    yScale = -m_fYScaleRange;
                    bIncrementNext = true;
                    bDone = true;
                }
            }
        }

        if (iTotal < iBruteCount) {
            Preferences.debug("... done testing costs \n", Preferences.DEBUG_ALGORITHM);
        }

        /* Sort the list of costs, take the minimum and return: */
        Collections.sort(minima);
        answer = minima.firstElement();
        answer.matrixd.Inverse();
        Preferences.debug("Min transform:\n" + answer.matrix + " cost = " + answer.cost + " min cost = " + dMinCost
                + "\n", Preferences.DEBUG_ALGORITHM);

        cost.disposeLocal();
        elsunc.disposeLocal();
        disposeLocal();
        finalize();
        setCompleted(true);
    }

    /**
     * Creates a string with the parameters that the image was constructed with.
     * 
     * @return Construction info.
     */
    private String getConstructionInfo() {
        String s;

        s = new String("RegistrationOAR2D(" + refImage.getImageName() + ", " + inputImage.getImageName() + ", ");

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

            case AlgorithmTransform.BILINEAR:
                s += "Bilinear, ";
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
                s += "Bilinear, ";
                break;
        }

        s += rotateBegin + ", " + rotateEnd + ", " + coarseRate + ", " + fineRate + ")\n";

        return s;
    }

    /**
     * Gets the tolerance vector based on the degrees of freedom (the length of the tolerance is the degrees of
     * freedom).
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

        if (DOF == 1) {
            tols[0] = ( (180. / Math.PI) / maxDim); // rotation
        } else if (DOF == 2) {
            tols[0] = tols[1] = 0.5; // translations
        } else if ( (DOF == 3) && (rigidFlag == true)) {
            tols[0] = ( (180. / Math.PI) / maxDim); // rotation
            tols[1] = tols[2] = 0.5; // translations
        } else if ( (DOF == 3) && (rigidFlag == false)) {
            tols[0] = 0.005; // global scaling
            tols[1] = tols[2] = 0.5; // translations
        } else if (DOF == 4) {
            tols[0] = ( (180. / Math.PI) / maxDim); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = 0.005; // (1.0/maxDim); // scaling tolerances global
        } else if (DOF == 5) {
            tols[0] = ( (180. / Math.PI) / maxDim); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = tols[4] = 0.005; // (1.0/maxDim); // scaling tolerance x
        } else if (DOF == 7) {
            tols[0] = ( (180. / Math.PI) / maxDim); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = tols[4] = 0.005; // (1.0/maxDim); // scaling tolerance x

            // tols[5] = tols[6] = (1.0/maxDim); // skew tolerance x
            tols[5] = tols[6] = 0.001; // skew tolerance x
        }

        return tols;
    }

    /**
     * Performs a bilinear interpolation on points. Takes an initial point, a vector of values to set, and an array in
     * which to look at neighbors of that point. Sets the appropriate values in the vector. Does not set scale if the
     * scale parameter is <code>false</code>.
     * 
     * @param x Initial index into array.
     * @param initial Vector to set; if scale is <code>true</code>, set two translations and a scale. Otherwise just
     *            set translations.
     * @param tranforms DOCUMENT ME!
     * @param scale <code>true</code> means set the scale in the vector.
     */
    private void interpolate(final double x, final double[] initial, final double[][] tranforms, final boolean scale) {
        int ix0, ix1;

        // convert to closest integer values to access proper parts of array
        ix0 = (int) Math.floor(x);
        ix1 = ix0 + 1;

        // can't be bigger than 3
        if ( (ix0 == (coarseNum - 1))) {
            ix1 = ix0;
        }

        if (scale) {

            // x translation
            initial[1] = ( (x - ix0) * tranforms[ix1][1]) + ( (1 - x + ix0) * tranforms[ix0][1]);

            // y translation
            initial[2] = ( (x - ix0) * tranforms[ix1][2]) + ( (1 - x + ix0) * tranforms[ix0][2]);

            // scale
            initial[3] = ( (x - ix0) * tranforms[ix1][0]) + ( (1 - x + ix0) * tranforms[ix0][0]);
        } else {

            // x translation
            initial[1] = ( (x - ix0) * tranforms[ix1][0]) + ( (1 - x + ix0) * tranforms[ix0][0]);

            // y translation
            initial[2] = ( (x - ix0) * tranforms[ix1][1]) + ( (1 - x + ix0) * tranforms[ix0][1]);
        }
    }

    /**
     * Takes two images that have been subsampled by a factor of eight. Sets up the cost function with the images and
     * the weighted images, if necessary. Uses the coarse sampling rate and optimizes translations and global scale at
     * the given rotation. So for example, if the coarse sampling range were -30 to 30 at every 15 degrees, we would
     * optimize at rotations of -30, -15, 0, 15, 30. Measures the cost at the fine sampling rate. Interpolates the
     * translations and global scale to come up with a good guess as to what the optimized translation would be at that
     * point. Takes the top 20% of the points and optimizes them. Now have a large multi-array of costs. 20% of those
     * have been optimized and placed back into their original position in the multi-array. Looks at the 2 neighbors of
     * a point: + and - one fine sample. If the point has a cost greater than any of these, it is not a minima.
     * Otherwise it is. Saves it in a vector of minima. Optimizes the minima over rotation as well as translations and
     * global scale. (Previously had not optimized over rotation.) Returns two vectors, one containing the minima before
     * optimization, one containing the minima after optimization.
     * 
     * @param ref Subsampled by 8 reference image.
     * @param input Subsampled by 8 input image.
     * 
     * @return List of preoptimized and optimized points.
     */
    @SuppressWarnings("unchecked")
    public Vector<MatrixListItem>[] levelEight(final ModelSimpleImage ref, final ModelSimpleImage input) {
        double factor;
        final AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 32, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            m_kGPUCost.initImages(ref, input, 32);
            cost.setGPUCost(m_kGPUCost);
        }
        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub8);
            cost.setInputWgtImage(simpleWeightInputSub8);
        }

        final Vector2f cog = calculateCenterOfMass2D(input, simpleWeightInputSub8, doColor);
        final Vector2f cogR = calculateCenterOfMass2D(ref, simpleWeightRefSub8, doColor);

        Preferences.debug(" COG input image = " + cog + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug(" COG ref   image = " + cogR + "\n",Preferences.DEBUG_ALGORITHM);

        final double diffX = (cog.X - cogR.X);
        final double diffY = (cog.Y - cogR.Y);

        final double[] initial = new double[7];

        initial[0] = 0; // initial rotation
        initial[1] = diffX; // initial translations
        initial[2] = diffY;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        final double[][] transforms = new double[coarseNum][3];

        // Optimizing over translations and global scaling in AlgorithmELSUNCOpt2D for DOF = 3
        // Optimizing only over translations in AlgorithmELSUNCOpt2D for DOF = 2
        AlgorithmELSUNCOpt2D elsunc;
        maxIter = baseNumIter;

        int dofs = 2;
        if (DOF > 3) {
            dofs = 3;
        }
        elsunc = new AlgorithmELSUNCOpt2D(this, cog, dofs, cost, getTolerance(dofs), maxIter, false, searchAlgorithm);

        // Should we even try to coarse since 2D images at level 8 are pretty small and not computionally taxing ?
        fireProgressStateChanged("Optimizing at coarse samples");

        Vectornd[] initials = new Vectornd[coarseNum];
        for (int i = 0; (i < coarseNum) && !threadStopped; i++) {
            fireProgressStateChanged( (i + 1) * 10 / coarseNum);

            initial[0] = rotateBegin + (i * coarseRate);
            initials[i] = new Vectornd(initial, true);
        }

        elsunc.setPoints(initials);
        elsunc.run();
        for (int i = 0; i < coarseNum; i++) {
            transforms[i] = elsunc.extractPoint(elsunc.getPoint(i));
        }

        if (threadStopped) {
            return null;
        }

        final MatrixListItem[] matrixList = new MatrixListItem[fineNum];

        fireProgressStateChanged("Measuring at fine samples");

        final double[] costs = new double[fineNum];
        int index = 0;

        for (int i = 0; (i < fineNum) && !threadStopped; i++) {
            fireProgressStateChanged(10 + ( (i + 1) * 5 / fineNum));

            initial[0] = rotateBegin + (i * fineRate);

            // sets up translation and global scaling factors
            factor = (rotateBegin - rotateBegin + (i * fineRate)) / coarseRate;
            interpolate(factor, initial, transforms, (DOF > 3));
            initial[4] = initial[3];

            costs[index] = elsunc.measureCost(initial);
            matrixList[i] = new MatrixListItem(costs[index++], elsunc.convertToMatrix(initial), initial);
        }

        if (threadStopped) {
            return null;
        }

        fireProgressStateChanged(15);

        Arrays.sort(costs);

        double threshold = costs[0] + (0.2 * (costs[costs.length - 1] - costs[0]));

        if (threshold > costs[(int) (0.2 * costs.length)]) {
            threshold = costs[(int) (0.2 * costs.length)];
        }

        fireProgressStateChanged("Optimizing top samples");

        elsunc.setPathRecorded(true);
        paths[0] = new Vector<Vector<Vector3f>>(10);
        index = Arrays.binarySearch(costs, threshold);
        if (index < 0) {
            index = -1 * (index + 1);
        }
        initials = new Vectornd[index];
        index = 0;
        for (int i = 0; (i < fineNum) && !threadStopped; i++) {
            fireProgressStateChanged(15 + ( (i + 1) * 5 / fineNum));

            if (matrixList[i].cost < threshold) {
                initials[index++] = new Vectornd(matrixList[i].initial);
            }
        }
        elsunc.setPoints(initials);
        elsunc.run();
        index = 0;
        for (int i = 0; i < fineNum; i++) {
            if (matrixList[i].cost < threshold) {
                matrixList[i] = new MatrixListItem(elsunc.getCost(index), elsunc.getMatrix(index), elsunc
                        .getPoint(index));
                index++;
            }
        }

        if (threadStopped) {
            return null;
        }

        final Vector<MatrixListItem> minima = new Vector<MatrixListItem>();

        for (int i = 0; i < fineNum; i++) {
            boolean minimum = true; // possible minimum

            for (int itest = -1; (itest <= 1) && minimum; itest += 2) { // as long as still possible minimum, check
                // neighbors one degree off

                if ( ( (i + itest) >= 0) && ( (i + itest) < fineNum)) {

                    if (matrixList[i].cost > matrixList[i + itest].cost) {
                        minimum = false;
                    } // not a minimum if a neighbor has a lower cost
                }
            }

            if (minimum) {
                minima.add(matrixList[i]);
            }
        }

        if (threadStopped) {
            return null;
        }

        Preferences.debug("Number of minima: " + minima.size() + "\n",Preferences.DEBUG_ALGORITHM);

        final Vector<MatrixListItem> optMinima = new Vector<MatrixListItem>();
        // Now freely optimizes over rotation:

        fireProgressStateChanged("Optimizing minima");

        final int degree = (DOF < 4) ? DOF : 4;
        maxIter = baseNumIter * 2;
        elsunc = new AlgorithmELSUNCOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter, rigidFlag,
        		searchAlgorithm);

        paths[1] = new Vector<Vector<Vector3f>>(10);
        elsunc.setPathRecorded(true);

        MatrixListItem item;
        initials = new Vectornd[minima.size()];
        index = 0;
        for (final MatrixListItem matrixListItem : minima) {
            initials[index++] = new Vectornd(matrixListItem.initial, true);
        }

        elsunc.setPoints(initials);
        elsunc.run();
        for (int i = 0; i < initials.length; i++) {
            item = new MatrixListItem(elsunc.getCost(i), elsunc.getMatrix(i), elsunc.getPoint(i));
            optMinima.add(item);
        }

        if (threadStopped) {
            return null;
        }

        fireProgressStateChanged(25);

        cost.disposeLocal();
        elsunc.disposeLocal();

        return new Vector[] {minima, optMinima};
    }

    /**
     * Takes two images that have been subsampled by a factor of four, and two vectors of minima. Sets up the cost
     * function with the images and the weighted images, if necessary. Adds the level4Factor determined during
     * subsampling. Measures the costs of the minima on the images and sort them. Takes the top three in each vector
     * (pre-optimization and post-optimization) and optimizes them. Puts them all into one vector. Perturbs the rotation
     * by zero and plus-minus fineDelta. If it's not a rigid transformation, perturbs the global scaling by factors of
     * 0.8, 0.9, 1.0, 1.1, and 1.2. Optimize the perturbations. Returns a vector of the perturbed, optimized minima.
     * 
     * @param ref Reference image, subsampled by 4.
     * @param input Input image, subsampled by 4.
     * @param minima Preoptimized minima.
     * @param optMinima Optimized minima.
     * 
     * @return A vector of perturbed, optimized minima.
     */
    public Vector<MatrixListItem> levelFour(final ModelSimpleImage ref, final ModelSimpleImage input,
            final Vector<MatrixListItem> minima, final Vector<MatrixListItem> optMinima) {
        final AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 64, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            m_kGPUCost.initImages(ref, input, 64);
            cost.setGPUCost(m_kGPUCost);
        }
        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub4);
            cost.setInputWgtImage(simpleWeightInputSub4);
        }

        final Vector2f cog = calculateCenterOfMass2D(input, simpleWeightInputSub4, doColor);
        MatrixListItem item = null;

        // fix translations based on image resolutions!
        for (final Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[1] *= level4Factor;
            item.initial[2] *= level4Factor;
        }

        for (final Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[1] *= level4Factor;
            item.initial[2] *= level4Factor;
        }

        final int degree = (DOF < 4) ? DOF : 4;

        maxIter = baseNumIter * 2;

        final AlgorithmELSUNCOpt2D elsunc = new AlgorithmELSUNCOpt2D(this, cog, degree, cost, getTolerance(degree),
                maxIter, rigidFlag, searchAlgorithm);

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

        final Vector<MatrixListItem> newMinima = new Vector<MatrixListItem>();

        fireProgressStateChanged("Optimizing new minima");

        // Now changed so that the number of minima to test at Level Four is a variable,
        // passed in from JDialog. It used to be set to "3".
        final int total = (numMinima < minima.size()) ? numMinima : minima.size();
        elsunc.setMaxIterations(baseNumIter);
        paths[2] = new Vector<Vector<Vector3f>>(10);
        elsunc.setPathRecorded(true);
        Vectornd[] initials = new Vectornd[2 * total];
        for (int i = 0; i < total; i++) {
            initials[i] = new Vectornd(minima.elementAt(i).initial);
            initials[i + total] = new Vectornd(optMinima.elementAt(i).initial);
        }
        elsunc.setPoints(initials);
        elsunc.run();

        if (threadStopped) {
            return null;
        }
        for (int i = 0; i < initials.length; i++) {
            item = new MatrixListItem(elsunc.getCost(i), elsunc.getMatrix(i), elsunc.getPoint(i));
            newMinima.add(item);
        }

        Collections.sort(newMinima);

        final double fineDelta = fineRate / 2.0;
        double[] initial;
        final Vector<MatrixListItem> perturbList = new Vector<MatrixListItem>();

        fireProgressStateChanged("Perturbing minima");
        paths[3] = new Vector<Vector<Vector3f>>(10);
        elsunc.setPathRecorded(true);
        if (DOF > 3) {
            initials = new Vectornd[newMinima.size() * 7];
        } else {
            initials = new Vectornd[newMinima.size() * 3];
        }
        // Perturb rotation. Add fine delta and optimize, then subtract fine delta and optimize.
        int index = 0;
        for (int j = 0; (j < 3) && !threadStopped; j++) {

            for (int i = 0; (i < (2 * total)) && !threadStopped; i++) {
                fireProgressStateChanged(29 + ( ( ( (j * 2 * total) + i + 1) * 3) / (total * 12)));

                initial = (newMinima.elementAt(i)).initial.clone();
                Preferences.debug("Perturbing initial[0] by ",Preferences.DEBUG_ALGORITHM);

                if (j == 0) {
                    Preferences.debug("No fineDelta added or subtracted\n",Preferences.DEBUG_ALGORITHM);
                }

                if (j == 1) {
                    initial[0] += fineDelta;
                    Preferences.debug("adding " + fineDelta + "\n",Preferences.DEBUG_ALGORITHM);
                }

                if (j == 2) {
                    initial[0] -= fineDelta;
                    Preferences.debug("subtracting " + fineDelta + "\n",Preferences.DEBUG_ALGORITHM);
                }

                // 1.) unchanged initial
                // 2.) make initial variable old initial + fineDelta
                // 3.) Make initial variable old initial - fineDelta
                initials[index++] = new Vectornd(initial, true);

            }
        }
        if (DOF > 3) {
            float scaleDelta = 0.8f;

            for (int j = 0; (j < 4) && !threadStopped; j++) {

                for (int i = 0; (i < (2 * total)) && !threadStopped; i++) {
                    fireProgressStateChanged(32 + ( ( ( (2 * j * total) + i + 1) * 3) / (total * 8)));

                    initial = (newMinima.elementAt(i)).initial.clone();

                    if (j == 1) {
                        scaleDelta = 0.9f;
                    } else if (j == 2) {
                        scaleDelta = 1.1f;
                    } else if (j == 3) {
                        scaleDelta = 1.2f;
                    }

                    Preferences.debug("Perturbing initial[3] by ",Preferences.DEBUG_ALGORITHM);
                    initial[3] *= scaleDelta;
                    Preferences.debug("Multiplying by " + scaleDelta + "\n",Preferences.DEBUG_ALGORITHM);

                    // make initial variable old initial * scaleDelta in each dimension
                    initials[index++] = new Vectornd(initial, true);
                }
            }
        }

        elsunc.setPoints(initials);
        elsunc.run();

        for (int i = 0; i < initials.length; i++) {
            item = new MatrixListItem(elsunc.getCost(i), elsunc.getMatrix(i), elsunc.getPoint(i));
            perturbList.add(item);
        }

        if (threadStopped) {
            return null;
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(perturbList);

        fireProgressStateChanged(35);

        cost.disposeLocal();
        elsunc.disposeLocal();

        return perturbList;
    }

    /**
     * Takes the two images, no subsampling, and the best minimum so far. Sets up the cost function with the images and
     * the weighted images, if necessary. Adds the level1Factor determined during subsampling. Performs one optimization
     * run, with the maximum allowable degrees of freedom as specified by the user (the max is 7). Returns the best
     * minimum.
     * 
     * @param ref Reference image.
     * @param input Input image.
     * @param item Best minimum so far.
     * 
     * @return Best minimum after optimization.
     */
    public MatrixListItem levelOne(final ModelSimpleImage ref, final ModelSimpleImage input, final MatrixListItem item) {
        // System.err.println( "level 1" );
        // Profile.clear();
        // Profile.start();

        int degree;
        AlgorithmELSUNCOpt2D elsunc;
        MatrixListItem item2;
        final AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 256, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            m_kGPUCost.initImages(ref, input, 256);
            cost.setGPUCost(m_kGPUCost);
        }
        if (weighted) {
            cost.setRefWgtImage(simpleWeightRef);
            cost.setInputWgtImage(simpleWeightInput);
        }

        final Vector2f cog = calculateCenterOfMass2D(input, simpleWeightInput, doColor);

        item.initial[1] *= level1Factor;
        item.initial[2] *= level1Factor;

        degree = DOF;

        fireProgressStateChanged("Starting last optimization");

        maxIter = baseNumIter * 2;
        elsunc = new AlgorithmELSUNCOpt2D(this, cog, degree, cost, getTolerance(DOF), maxIter, rigidFlag, searchAlgorithm);

        elsunc.setPathRecorded(true);
        paths[5] = new Vector<Vector<Vector3f>>(1);
      
        linkProgressToAlgorithm(elsunc);
        elsunc.setProgressValues(generateProgressValues(60, 100));
        final Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(item.initial);
        elsunc.setPoints(initials);
        elsunc.run();

        delinkProgressToAlgorithm(elsunc);
        if (threadStopped) {
            return null;
        }

        item2 = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0, input.xRes), elsunc.getPoint(0));
        Preferences.debug("Best answer: \n" + item2 + "\n",Preferences.DEBUG_ALGORITHM);

        fireProgressStateChanged(100);

        cost.disposeLocal();
        elsunc.disposeLocal();

        // Profile.stop();
        // Profile.setFileName( "profile_out" );
        // Profile.shutdown();

        return item2;
    }

    /**
     * Only used for translations only = 2 degrees of freedom levelEight, levelFour, and levelTwo are skipped Takes the
     * two images, no subsampling. Sets up the cost function with the images and the weighted images, Performs one
     * optimization run, with 2 degrees of freedom Returns the best minimum.
     * 
     * @param ref Reference image.
     * @param input Input image.
     * 
     * @return Best minimum after optimization.
     */
    private MatrixListItem levelOne2D(final ModelSimpleImage ref, final ModelSimpleImage input) {
        int degree;
        AlgorithmELSUNCOpt2D elsunc;
        MatrixListItem item2;
        final AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 256, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            m_kGPUCost.initImages(ref, input, 256);
            cost.setGPUCost(m_kGPUCost);
        }
        if (weighted) {
            cost.setRefWgtImage(simpleWeightRef);
            cost.setInputWgtImage(simpleWeightInput);
        }

        final Vector2f cog = calculateCenterOfMass2D(input, simpleWeightInput, doColor);
        final Vector2f cogR = calculateCenterOfMass2D(ref, simpleWeightRef, doColor);

        Preferences.debug(" COG input image = " + cog + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug(" COG ref   image = " + cogR + "\n",Preferences.DEBUG_ALGORITHM);

        final double diffX = (cog.X - cogR.X);
        final double diffY = (cog.Y - cogR.Y);

        final double[] initial = new double[7];

        initial[0] = 0; // initial rotation
        initial[1] = diffX; // initial translations
        initial[2] = diffY;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        degree = 2;

        fireProgressStateChanged("Starting last optimization");

        maxIter = baseNumIter * 2;
        elsunc = new AlgorithmELSUNCOpt2D(this, cog, degree, cost, getTolerance(DOF), maxIter, rigidFlag, searchAlgorithm);
        
        linkProgressToAlgorithm(elsunc);
        elsunc.setProgressValues(generateProgressValues(60, 100));
        final Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(initial);
        elsunc.setPoints(initials);
        elsunc.run();
        delinkProgressToAlgorithm(elsunc);
        if (threadStopped) {
            return null;
        }

        item2 = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0, input.xRes), elsunc.getPoint(0));

        Preferences.debug("Best answer: \n" + item2 + "\n",Preferences.DEBUG_ALGORITHM);

        fireProgressStateChanged(100);

        cost.disposeLocal();
        elsunc.disposeLocal();

        return item2;
    }

    /**
     * Only used for rotations only = 1 degrees of freedom levelEight, levelFour, and levelTwo are skipped Takes the
     * two images, no subsampling. Sets up the cost function with the images and the weighted images, Performs one
     * optimization run, with 2 degrees of freedom Returns the best minimum.
     * 
     * @param ref Reference image.
     * @param input Input image.
     * 
     * @return Best minimum after optimization.
     */
    private MatrixListItem levelOne2DRotation(final ModelSimpleImage ref, final ModelSimpleImage input) {
        int degree;
        AlgorithmELSUNCOpt2D elsunc;
        final AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 256, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            m_kGPUCost.initImages(ref, input, 256);
            cost.setGPUCost(m_kGPUCost);
        }
        if (weighted) {
            cost.setRefWgtImage(simpleWeightRef);
            cost.setInputWgtImage(simpleWeightInput);
        }

        final Vector2f cog = new Vector2f( input.xDim / 2, input.yDim / 2 );
        final double[] initial = new double[7];

        initial[0] = 0; // initial rotation
        initial[1] = 0; // initial translations
        initial[2] = 0;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        degree = DOF;

        maxIter = baseNumIter * 2;
        elsunc = new AlgorithmELSUNCOpt2D(this, cog, degree, cost, getTolerance(DOF), maxIter, rigidFlag, searchAlgorithm);
        
        //fireProgressStateChanged("Optimizing at coarse samples");
        Vectornd[] initials = new Vectornd[coarseNum + 1];
        initials[0] = new Vectornd(initial, true);
        for (int i = 0; (i < coarseNum) && !threadStopped; i++) {
            fireProgressStateChanged( (i + 1) * 10 / coarseNum);

            initial[0] = rotateBegin + (i * coarseRate);
            initials[i + 1] = new Vectornd(initial, true);
        }

        //elsunc.setPoints(initials);

        //Vectornd[] initials = new Vectornd[1];
        //initials[0] = new Vectornd(initial, true);

        elsunc.setPoints(initials);
        elsunc.run();
        
        int minIndex = 0;
        double minCost = Float.MAX_VALUE;
        MatrixListItem item2 =  new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0, input.xRes), elsunc.getPoint(0));
    	
        for ( int i = 0; i < initials.length; i++ )
        {
        	 item2 =  new MatrixListItem(elsunc.getCost(i), elsunc.getMatrix(i, input.xRes), elsunc.getPoint(i));
        	 if ( item2.getCost() < minCost )
        	 {
        		 minCost = item2.getCost();
        		 minIndex = i;
        	 }        	
        }
        item2 =  new MatrixListItem(elsunc.getCost(minIndex), elsunc.getMatrix(minIndex, input.xRes), elsunc.getPoint(minIndex));
        
        Preferences.debug("Best answer: \n" + item2 + "\n",Preferences.DEBUG_ALGORITHM);

        fireProgressStateChanged(100);

        cost.disposeLocal();
        elsunc.disposeLocal();
        return item2;
    }

    /**
     * Takes two images that have been subsampled by a factor of 2 and a vector of minima. Sets up the cost function
     * with the images and the weighted images, if necessary. Adds the level2Factor determined during subsampling.
     * Measures the costs of the minima at the images. Optimizes the best minimum with 4 degrees of freedom, then 5,
     * then 7. If the user has limited the degrees of freedom to 3, there will only be one optimization run, with 3
     * degrees of freedom. Returns the best minimum after optimization.
     * 
     * @param ref Reference image, subsampled by 2.
     * @param input Input image, subsampled by 2.
     * @param minima Minima.
     * 
     * @return The optimized minimum.
     */
    public MatrixListItem levelTwo(final ModelSimpleImage ref, final ModelSimpleImage input,
            final Vector<MatrixListItem> minima) {
        final AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 128, 1);
        if ( (m_kGPUCost != null)
                && ( (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU) || (costChoice == AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU_LM))) {
            m_kGPUCost.initImages(ref, input, 128);
            cost.setGPUCost(m_kGPUCost);
        }
        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub2);
            cost.setInputWgtImage(simpleWeightInputSub2);
        }

        final Vector2f cog = calculateCenterOfMass2D(input, simpleWeightInputSub2, doColor);
        MatrixListItem item = null;

        for (final Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[1] *= level2Factor;
            item.initial[2] *= level2Factor;
        }

        int degree = (DOF < 4) ? DOF : 4;
        maxIter = baseNumIter;

        AlgorithmELSUNCOpt2D elsunc = new AlgorithmELSUNCOpt2D(this, cog, degree, cost, getTolerance(degree),
                maxIter, rigidFlag, searchAlgorithm);

        fireProgressStateChanged("Measuring costs of minima");

        for (final Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.cost = elsunc.measureCost(item.initial);
        }

        Collections.sort(minima);

        fireProgressStateChanged("Optimizing with " + degree + " DOF");

        elsunc = new AlgorithmELSUNCOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter, rigidFlag, searchAlgorithm);

        elsunc.setPathRecorded(true);
        paths[4] = new Vector<Vector<Vector3f>>(1);
       
        linkProgressToAlgorithm(elsunc);
        elsunc.setProgressValues(generateProgressValues(35, 43));

        final Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(minima.elementAt(0).initial);
        elsunc.setPoints(initials);
        // Preferences.debug("item for level2 elsunc run = " + (MatrixListItem) minima.elementAt(0) + "\n",
        //Preferences.DEBUG_ALGORITHM);
        elsunc.run();
        delinkProgressToAlgorithm(elsunc);

        if (threadStopped) {
            return null;
        }

        item = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0), elsunc.getPoint(0));
        Preferences.debug("Level 2, after " + degree + " DOF: " + item + "\n",Preferences.DEBUG_ALGORITHM);

        if (DOF > 4) {
            degree = 5;

            fireProgressStateChanged("Optimizing with " + degree + " DOF");
            fireProgressStateChanged(43);

            elsunc = new AlgorithmELSUNCOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter,
            		rigidFlag, searchAlgorithm);

            linkProgressToAlgorithm(elsunc);
            elsunc.setProgressValues(generateProgressValues(43, 51));
            elsunc.setPoints(initials);
            elsunc.run();
            delinkProgressToAlgorithm(elsunc);

            if (threadStopped) {
                return null;
            }

            item = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0), elsunc.getPoint(0));
            Preferences.debug("Level 2, after " + degree + " DOF: " + item + "\n",Preferences.DEBUG_ALGORITHM);

            if (DOF > 5) {
                degree = (DOF < 7) ? DOF : 7;
                elsunc = new AlgorithmELSUNCOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter,
                		rigidFlag, searchAlgorithm);

                fireProgressStateChanged("Optimizing with 7 DOF");

                fireProgressStateChanged(51);

                linkProgressToAlgorithm(elsunc);
                elsunc.setProgressValues(generateProgressValues(51, 60));
                elsunc.setPoints(initials);

                elsunc.run();
                delinkProgressToAlgorithm(elsunc);

                if (threadStopped) {
                    return null;
                }

                item = new MatrixListItem(elsunc.getCost(0), elsunc.getMatrix(0), elsunc.getPoint(0));
                Preferences.debug("Level 2, after " + degree + " DOF: " + item + "\n",Preferences.DEBUG_ALGORITHM);
            }
        }

        fireProgressStateChanged(60);

        cost.disposeLocal();
        elsunc.disposeLocal();

        return item;
    }

    public float getLevel1Factor() {
        return level1Factor;
    }

    public void setLevel1Factor(final float level1Factor) {
        this.level1Factor = level1Factor;
    }

    public float getLevel2Factor() {
        return level2Factor;
    }

    public void setLevel2Factor(final float level2Factor) {
        this.level2Factor = level2Factor;
    }

    public float getLevel4Factor() {
        return level4Factor;
    }

    public void setLevel4Factor(final float level4Factor) {
        this.level4Factor = level4Factor;
    }

    public int getMaxDim() {
        return maxDim;
    }

    public void setMaxDim(final int maxDim) {
        this.maxDim = maxDim;
    }
}
