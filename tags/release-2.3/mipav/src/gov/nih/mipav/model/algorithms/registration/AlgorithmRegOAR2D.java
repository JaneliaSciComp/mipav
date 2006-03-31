package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;

import java.util.*;


/**
 *   This is an automatic registration method based on FLIRT.  FLIRT stands
 *   for FMRIB's Linear Image Registration Tool.  For more information on FLIRT,
 *   visit their homepage at <a href="http://www.fmrib.ox.ac.uk/fsl/flirt/">
 *   http://www.fmrib.ox.ac.uk/fsl/flirt/</a>.  Their main paper is:
 *   <p>
 *   Jenkinson, M. and Smith, S. (2001a).<br>
 *   A global optimisation method for robust affine registration of brain images.<br>
 *   <i>Medical Image Analysis</i>, 5(2):143-156.<br>
 *   <p>
 *   Our algorithm works as follows:<br>
 *   1.)  We find the minimum resolution of the images and blur them if neccessary.<br>
 *   2.)  We transform the images into isotropic voxels.<br>
 *   3.)  We subsample the images by 2, 4, and 8, depending on the resolution.<br>
 *   4.)  With the images that were subsampled by 8, we call levelEight.  This function
 *        will use the coarse sampling rate and optimize translations and global scale at
 *        the given rotation.  So for example, if the coarse sampling range were -30 to 30
 *        at every 15 degrees, we would optimize at rotations of -30, -15, 0, 15, 30.<br>
 *   5.)  Still in levelEight, we now measure the cost at the fine sampling rate.  We interpolate
 *        the translations and global scale to come up with a good guess as to what the optimized
 *        translation would be at that point.<br>
 *   6.)  We take the top 20% of the points and optimize them. <br>
 *   7.)  We now have a large multi-array of costs.  20% of those have been optimized and placed back
 *        into their original position in the multi-array.  We look at the 2 neighbors of a point:
 *        + and - one fine sample.  If our point has a cost greater than any of these, it is not
 *        a minima.  Otherwise it is.  We save it in a vector of minima.<br>
 *   8.)  We optimize the minima over rotation as well as translations and global scale.  (Previously
 *        we had not optimized over rotation.)  We return two vectors, one containing the minima
 *        before optimization, one containing the minima after optimization.<br>
 *   9.)  We now call levelFour with the images subsampled by 4 and the vectors of minima.  We measure the
 *        costs of the minima on the new images and sort them.  We take the top numMinima in each vector
 *        (pre-optimization and post-optimization) and optimize them.  We put them all into one vector.<br>
 *   10.) We perturb the rotation by zero and plus-minus fineDelta.  If it's not a rigid transformation,
 *        we then perturb the global scaling by factors of 0.8, 0.9, 1.0, 1.1, and 1.2.  <br>
 *   11.) We optimize the perturbations.  We return a vector of the perturbed, optimized minima.<br>
 *   12.) We now call levelTwo with the images subsampled by 2.  We measure the costs of the minima
 *        at the new images.  We optimize the best minimum with 4 degrees of freedom, then 5, then 6.
 *        If the user has limited the degrees of freedom to 3, there will only be one optimization run,
 *        with 3 degrees of freedom.  The function returns the best minimum after optimization.<br>
 *   13.) We call levelOne with the un-subsampled images.  At levelOne, one optimization run is performed,
 *        with the maximum allowable degrees of freedom, as specified by the user (the max is 6).<br>
 *   14.) The best answer is returned from levelOne.  The matrix from this answer is saved in a file and also
 *        accessed by the dialog that called this algorithm.<br>
 *
 *   Note that when 6 degrees of freedom is used the rotation is set equal to 0 because diffX sets (0,2),
 *   diffY sets (1,2), scaleX sets (0,0), scaleY sets (1,1), skewX sets (0,1), and skewY sets (1,0) so
 *   all 6 elements are set.
 *   @author Neva Cherniavsky
 *	@author Matthew McAuliffe
 */
public class AlgorithmRegOAR2D extends AlgorithmBase {

    /** The inputImage will be registered to this reference image. */
    private ModelImage refImage;

    /** This image is to registered to the reference image. */
    private ModelImage inputImage;

    /** This gives weights for the reference image - higher weights mean a greater impact in that
     area on the registration. */
    private ModelImage refWeight;

    /** This gives weights for the input image - higher weights mean a greater impact in that
     area on the registration. */
    private ModelImage inputWeight;

    /**   The voxel resolutions of the reference image. */
    private float[] resRef;

    /** The voxel resolutions of the image to be registered to the reference image. */
    private float[] resInput;

    /** Choice of which cost function to use. */
    private int costChoice;

    /** Maximum degrees of freedom when running the optimization. */
    private int DOF;

    /** Interpolation method. */
    private int interp;

    /** Coarse and fine sampling parameters. */
    private float rotateBegin, rotateEnd, coarseRate, fineRate;

    /** Number of passes that will be made in the coarse sampling and fine sampling. */
    private int coarseNum, fineNum;

    // If true subsample
    private boolean doSubsample;

    /** Multiplication factor for level 1 - will be set based on subsampling. */
    private float level1Factor = 1.0f;

    /** Multiplication factor for level 2 - will be set based on subsampling. */
    private float level2Factor = 1.0f;

    /** Multiplication factor for level 4 - will be set based on subsampling. */
    private float level4Factor = 1.0f;

    /** Final answer after registration. */
    private MatrixListItem answer;

    /** Transformation algorithm for creating an isotropic reference image. */
    private AlgorithmTransform transform = null;

    /** Transformation algorithm for creating an isotropic input image. */
    private AlgorithmTransform transform2 = null;

    /** Isotropic reference image. */
    private ModelImage imageRefIso;

    /** Isotropic input image. */
    private ModelImage imageInputIso;

    /** Isotropic weighted reference image. */
    private ModelImage imageWeightRefIso;

    /** Isotropic weighted input image. */
    private ModelImage imageWeightInputIso;

    /** Blurred reference image. */
    private ModelImage blurredRef;

    /** Blurred input image. */
    private ModelImage blurredInput;

    /** Simple version of reference image. */
    private ModelSimpleImage simpleRef;

    /** Simple version of input image. */
    private ModelSimpleImage simpleInput;

    /** Simple version of reference image, subsampled by 2. */
    private ModelSimpleImage simpleRefSub2;

    /** Simple version of input image, subsampled by 2. */
    private ModelSimpleImage simpleInputSub2;

    /** Simple version of reference image, subsampled by 4. */
    private ModelSimpleImage simpleRefSub4;

    /** Simple version of input image, subsampled by 4. */
    private ModelSimpleImage simpleInputSub4;

    /** Simple version of reference image, subsampled by 8. */
    private ModelSimpleImage simpleRefSub8;

    /** Simple version of input image, subsampled by 8. */
    private ModelSimpleImage simpleInputSub8;

    /** Simple version of weighted reference image. */
    private ModelSimpleImage simpleWeightRef = null;

    /** Simple version of weighted input image. */
    private ModelSimpleImage simpleWeightInput = null;

    /** Simple version of weighted reference image, subsampled by 2. */
    private ModelSimpleImage simpleWeightRefSub2 = null;

    /** Simple version of weighted input image, subsampled by 2. */
    private ModelSimpleImage simpleWeightInputSub2 = null;

    /** Simple version of weighted reference image, subsampled by 4. */
    private ModelSimpleImage simpleWeightRefSub4 = null;

    /** Simple version of weighted input image, subsampled by 4. */
    private ModelSimpleImage simpleWeightInputSub4 = null;

    /** Simple version of weighted reference image, subsampled by 8. */
    private ModelSimpleImage simpleWeightRefSub8 = null;

    /** Simple version of weighted input image, subsampled by 8. */
    private ModelSimpleImage simpleWeightInputSub8 = null;

    /** Dummy initial values used to create a Powell's algorithm instance before setting initial. */
    private double[] dummy = { 0, 0, 0, 0, 0, 0, 0 };

    /** Flag to determine if there are weighted images or not. */
    private boolean weighted;

    /** Flag used to indicate if the registration is rigid (rotation and translation only; DOF = 3 */
    private boolean rigidFlag = false;

    private int weightedRefPixels = 0;
    private int weightedInputPixels = 0;
    private int weightedRefPixelsSub2 = 0;
    private int weightedInputPixelsSub2 = 0;
    private int weightedRefPixelsSub4 = 0;
    private int weightedInputPixelsSub4 = 0;
    private int weightedRefPixelsSub8 = 0;
    private int weightedInputPixelsSub8 = 0;
    private boolean allowLevel2 = true;
    private boolean allowLevel4 = true;
    private boolean allowLevel8 = true;
    private boolean allowLevel16 = true;
    private boolean resampleRef;
    private boolean resampleInput;
    private boolean doColor;

    /** Limits number of iterations in Powell optimization.
     * maxIter in the call to Powell's will be an integer multiple of baseNumIter
     */
    private int maxIter;

    /**
     * these numbers hard coded for constructors that don't include them
     */
    private int baseNumIter = 2;

    /* The bracket size around the minimum in multiples of unit_tolerance
     * in the first iteration of Powell's algorithm
     */
    private int bracketBound = 20;

    /** Number of minima from level 8 to test at level 4 */
    private int numMinima = 3;

    private int maxDim = 256;

    /**
     *	Creates new automatic linear registration algorithm and sets necessary variables.
     *	@param _imageA		Reference image (register input image to reference image).
     *	@param _imageB		Input image (register input image to reference image).
     *	@param _costChoice	Choice of cost functions, like correlation ratio or mutual information.
     *    @param _DOF         Degrees of freedom for registration
     *	@param _interp		Interpolation method used in transformations.
     *	@param _rotateBegin	Beginning of coarse sampling range (i.e., -60 degrees).
     *	@param _rotateEnd	End of coarse sampling range (i.e., 60 degrees).
     *	@param _coarseRate	Point at which coarse samples should be taken (i.e., every 45 degrees).
     *	@param _fineRate	Point at which fine samples should be taken (i.e., every 15 degrees).
     *    @param doSubsample  If true subsample
     *    @param _bracketBound The bracket size around the minimum in multiples of unit_tolerance
     *                       for the first iteration of Powell's algorithm.
     *    @param _baseNumIter Limits the number of iterations of Powell's algorithm.
     *                        maxIter in the call to Powell's will be an integer multiple
     *                        of baseNumIter
     *    @param _numMinima Number of minima from level 8 to test at level 4
     *
     *	Constructor without weighting and with advanced settings (bracket, num iter) set.
     */
    public AlgorithmRegOAR2D( ModelImage _imageA, ModelImage _imageB, int _costChoice, int _DOF, int _interp,
            float _rotateBegin, float _rotateEnd, float _coarseRate, float _fineRate,
            boolean doSubsample, int _bracketBound, int _baseNumIter,
            int _numMinima ) {
        super( null, _imageB );
        refImage = _imageA;
        inputImage = _imageB;
        if ( inputImage.isColorImage() ) {
            doColor = true;
        } else {
            doColor = false;
        }
        costChoice = _costChoice;
        DOF = _DOF;
        if ( DOF == 3 ) {
            rigidFlag = true;
        }
        if ( DOF == 6 ) {
            DOF = 7; // use 2 shears
        }

        interp = _interp;
        resRef = refImage.getFileInfo( 0 ).getResolutions();
        resInput = inputImage.getFileInfo( 0 ).getResolutions();
        rotateBegin = _rotateBegin;
        rotateEnd = _rotateEnd;
        coarseRate = _coarseRate;
        fineRate = _fineRate;
        coarseNum = (int) ( ( rotateEnd - rotateBegin ) / coarseRate ) + 1;
        fineNum = (int) ( ( rotateEnd - rotateBegin ) / fineRate ) + 1;
        weighted = false;
        this.doSubsample = doSubsample;
        bracketBound = _bracketBound;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;

    }

    /**
     *	Creates new automatic linear registration algorithm and sets necessary variables.
     *	@param _imageA		Reference image (register input image to reference image).
     *	@param _imageB		Input image (register input image to reference image).
     *	@param _refWeight	Reference weighted image, used to give certain areas of the image greater impact on the registration.
     *	@param _inputWeight	Input weighted image, used to give certain areas of the image greater impact on the registration.
     *	@param _costChoice	Choice of cost functions, like correlation ratio or mutual information.
     *    @param _DOF         Degrees of freedom for registration
     *	@param _interp		Interpolation method used in transformations.
     *	@param _rotateBegin	Beginning of coarse sampling range (i.e., -60 degrees).
     *	@param _rotateEnd	End of coarse sampling range (i.e., 60 degrees).
     *	@param _coarseRate	Point at which coarse samples should be taken (i.e., every 45 degrees).
     *	@param _fineRate	Point at which fine samples should be taken (i.e., every 15 degrees).
     *    @param doSubsample  If true subsample
     *    @param _bracketBound The bracket size around the minimum in multiples of unit_tolerance
     *                       for the first iteration of Powell's algorithm.
     *    @param _baseNumIter Limits the number of iterations of Powell's algorithm.
     *                        maxIter in the call to Powell's will be an integer multiple
     *                        of baseNumIter
     *    @param _numMinima Number of minima from level 8 to test at level 4
     *
     *	Constructor with weighting and with advanced settings (bracket, num iter) set.
     */
    public AlgorithmRegOAR2D( ModelImage _imageA, ModelImage _imageB, ModelImage _refWeight, ModelImage _inputWeight,
            int _costChoice, int _DOF, int _interp,
            float _rotateBegin, float _rotateEnd, float _coarseRate,
            float _fineRate, boolean doSubsample,
            int _bracketBound, int _baseNumIter, int _numMinima ) {
        super( null, _imageB );
        refImage = _imageA;
        inputImage = _imageB;
        if ( inputImage.isColorImage() ) {
            doColor = true;
        } else {
            doColor = false;
        }
        refWeight = _refWeight;
        inputWeight = _inputWeight;
        costChoice = _costChoice;
        DOF = _DOF;
        if ( DOF == 3 ) {
            rigidFlag = true;
        }
        if ( DOF == 6 ) {
            DOF = 7; // use 2 shears
        }

        interp = _interp;
        resRef = refImage.getFileInfo( 0 ).getResolutions();
        resInput = inputImage.getFileInfo( 0 ).getResolutions();
        rotateBegin = _rotateBegin;
        rotateEnd = _rotateEnd;
        coarseRate = _coarseRate;
        fineRate = _fineRate;
        coarseNum = (int) ( ( rotateEnd - rotateBegin ) / coarseRate ) + 1;
        fineNum = (int) ( ( rotateEnd - rotateBegin ) / fineRate ) + 1;
        weighted = true;
        this.doSubsample = doSubsample;
        bracketBound = _bracketBound;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;

    }

    /**
     *	Creates new automatic linear registration algorithm and sets necessary variables.
     *	@param _imageA		Reference image (register input image to reference image).
     *	@param _imageB		Input image (register input image to reference image).
     *	@param _costChoice	Choice of cost functions, like correlation ratio or mutual information.
     *    @param _DOF         Degrees of freedom for registration
     *	@param _interp		Interpolation method used in transformations.
     *	@param _rotateBegin	Beginning of coarse sampling range (i.e., -60 degrees).
     *	@param _rotateEnd	End of coarse sampling range (i.e., 60 degrees).
     *	@param _coarseRate	Point at which coarse samples should be taken (i.e., every 45 degrees).
     *	@param _fineRate	Point at which fine samples should be taken (i.e., every 15 degrees).
     *    @param doSubsample  If true subsample
     *
     *	Constructor without weighting and without advanced settings (bracket, num iter).
     */
    public AlgorithmRegOAR2D( ModelImage _imageA, ModelImage _imageB, int _costChoice, int _DOF, int _interp,
            float _rotateBegin, float _rotateEnd, float _coarseRate, float _fineRate,
            boolean doSubsample ) {
        super( null, _imageB );
        refImage = _imageA;
        inputImage = _imageB;
        if ( inputImage.isColorImage() ) {
            doColor = true;
        } else {
            doColor = false;
        }
        costChoice = _costChoice;
        DOF = _DOF;
        if ( DOF == 3 ) {
            rigidFlag = true;
        }
        if ( DOF == 6 ) {
            DOF = 7; // use 2 shears
        }

        interp = _interp;
        resRef = refImage.getFileInfo( 0 ).getResolutions();
        resInput = inputImage.getFileInfo( 0 ).getResolutions();
        rotateBegin = _rotateBegin;
        rotateEnd = _rotateEnd;
        coarseRate = _coarseRate;
        fineRate = _fineRate;
        coarseNum = (int) ( ( rotateEnd - rotateBegin ) / coarseRate ) + 1;
        fineNum = (int) ( ( rotateEnd - rotateBegin ) / fineRate ) + 1;
        weighted = false;
        this.doSubsample = doSubsample;

    }

    /**
     *	Creates new automatic linear registration algorithm and sets necessary variables.
     *	@param _imageA		Reference image (register input image to reference image).
     *	@param _imageB		Input image (register input image to reference image).
     *	@param _refWeight	Reference weighted image, used to give certain areas of the image greater impact on the registration.
     *	@param _inputWeight	Input weighted image, used to give certain areas of the image greater impact on the registration.
     *	@param _costChoice	Choice of cost functions, like correlation ratio or mutual information.
     *    @param _DOF         Degrees of freedom for registration
     *	@param _interp		Interpolation method used in transformations.
     *	@param _rotateBegin	Beginning of coarse sampling range (i.e., -60 degrees).
     *	@param _rotateEnd	End of coarse sampling range (i.e., 60 degrees).
     *	@param _coarseRate	Point at which coarse samples should be taken (i.e., every 45 degrees).
     *	@param _fineRate	Point at which fine samples should be taken (i.e., every 15 degrees).
     *   @param doSubsample  If true subsample
     *
     *	Constructor with weighting and without advanced settings (bracket, num iter).
     */
    public AlgorithmRegOAR2D( ModelImage _imageA, ModelImage _imageB, ModelImage _refWeight, ModelImage _inputWeight,
            int _costChoice, int _DOF, int _interp,
            float _rotateBegin, float _rotateEnd, float _coarseRate,
            float _fineRate, boolean doSubsample ) {
        super( null, _imageB );
        refImage = _imageA;
        inputImage = _imageB;
        if ( inputImage.isColorImage() ) {
            doColor = true;
        } else {
            doColor = false;
        }
        refWeight = _refWeight;
        inputWeight = _inputWeight;
        costChoice = _costChoice;
        DOF = _DOF;
        if ( DOF == 3 ) {
            rigidFlag = true;
        }
        if ( DOF == 6 ) {
            DOF = 7; // use 2 shears
        }

        interp = _interp;
        resRef = refImage.getFileInfo( 0 ).getResolutions();
        resInput = inputImage.getFileInfo( 0 ).getResolutions();
        rotateBegin = _rotateBegin;
        rotateEnd = _rotateEnd;
        coarseRate = _coarseRate;
        fineRate = _fineRate;
        coarseNum = (int) ( ( rotateEnd - rotateBegin ) / coarseRate ) + 1;
        fineNum = (int) ( ( rotateEnd - rotateBegin ) / fineRate ) + 1;
        weighted = true;
        this.doSubsample = doSubsample;

    }

    /**
     *   Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     *	Runs the image registration.  Blurs the images based on what their
     *   minimum resolutions are.  The reference image is blurred if one of the input image
     *   resolutions is 50% or more bigger than the corresponding resolution in the
     *   reference image; likewise, the input image is blurred if one of the reference image resolutions
     *   is 50% or more bigger than the corresponding resolution in the input
     *   image.  Thus, it is unlikely, though not impossible, that both images will be
     *   blurred.  The images are then transformed into isotropic voxels.  The resolutions
     *   of the two images after the isotropic transformation will be the same in all
     *   dimensions.  That resolution will equal the minimum of the minimums of each
     *   image's resolutions: Min( Min (resolutions of ref image, resolutions of input image) ).
     *   If the images are weighted, the weight images are blurred and transformed into
     *   isotropic voxels in the same manner as the originals.  Then the images are
     *   subsampled by 2, 4, and 8.  If the images are too small they will not be subsampled
     *   down to the smallest level; if they are too big, they will be subsampled to 16.
     *   The same is done with the weight images if necessary.  The function levelEight
     *   is called with the images subsampled by 8; it returns two vectors with minima.  Then
     *   the function levelFour is called with images subsampled by 4 and the two vectors; it
     *   returns one vector of minima.  The function levelTwo is called with images subsampled
     *   by 2 and the vector; it returns an "answer" in the form of a MatrixListItem, which is
     *   a convenient way of storing the point, the matrix, and the cost of the minimum.  Then
     *   the function levelOne is called with the minimum; it returns a final "answer", or
     *   minimum, which will then be accessed by the dialog that called this algorithm.
     */
    public void runAlgorithm() {
        int i;
        if ( refImage.getNDims() != 2 ) {
            MipavUtil.displayError( "" + refImage.getNDims() + "D registration not supported." );
            disposeLocal();
            return;
        }
        if ( inputImage.getNDims() != 2 ) {
            MipavUtil.displayError( "" + inputImage.getNDims() + "D registration not supported." );
            disposeLocal();
            return;
        }

        float minSampleRef = 1.0f;
        float minSampleInput = 1.0f;
        float minSample = 1.0f;

        minSampleRef = Math.min( resRef[0], resRef[1] );
        minSampleInput = Math.min( resInput[0], resInput[1] );

        minSample = Math.min( minSampleRef, minSampleInput ); // min of the min resolutions of the two datasets

        if ( ( resRef[0] == resRef[1] ) && ( resRef[0] == minSample ) ) {
            resampleRef = false;
        } else {
            resampleRef = true;
        }

        if ( ( resInput[0] == resInput[1] ) && ( resInput[0] == minSample ) ) {
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
        } catch ( OutOfMemoryError e ) {
            System.gc();
            MipavUtil.displayError( "Out of memory in AlgorithmOAR2D." );
            disposeLocal();
            return;
        }

        for ( i = 0; i < extentsRefIso.length; i++ ) {
            extentsRefIso[i] = (int) ( ( refImage.getExtents()[i] - 1 ) / ( minSample / resRef[i] ) ) + 1;
            resRefIso[i] = minSample;
            extentsInputIso[i] = (int) ( ( inputImage.getExtents()[i] - 1 ) / ( minSample / resInput[i] ) ) + 1;
            resInputIso[i] = minSample;
        }

        boolean blurRef = false;
        boolean blurInput = false;
        for ( i = 0; i < sigmasRef.length; i++ ) {
            sigmasRef[i] = ( ( resInput[i] - resRef[i] ) / resRef[i] ) * 0.424f; /// 2.36 sigma = x -->  FWHM  0.424 = 1/2.36
            if ( sigmasRef[i] < 0.5f ) {
                sigmasRef[i] = 0.0f;
            } else {
                blurRef = true;
            }
            sigmasInput[i] = ( ( resRef[i] - resInput[i] ) / resInput[i] ) * 0.424f;
            if ( sigmasInput[i] < 0.5f ) {
                sigmasInput[i] = 0.0f;
            } else {
                blurInput = true;
            }
        }

        constructLog();
        Preferences.debug( getConstructionInfo() );

        if ( blurRef ) {
            if ( doColor ) {
                blurredRef = new ModelImage( ModelImage.ARGB_FLOAT, refImage.getExtents(), "BlurRef",
                        refImage.getUserInterface() );
            } else {
                blurredRef = new ModelImage( ModelImage.FLOAT, refImage.getExtents(), "BlurRef",
                        refImage.getUserInterface() );
            }
            // update resolutions
            FileInfoBase[] fileInfo = blurredRef.getFileInfo();
            fileInfo[0].setResolutions( refImage.getFileInfo()[0].getResolutions() );

            AlgorithmGaussianBlur blur = new AlgorithmGaussianBlur( blurredRef, refImage, sigmasRef, true, false );
            if ( doColor ) {
                blur.setRed( true );
                blur.setGreen( true );
                blur.setBlue( true );
            }
            blur.run();
            if ( blur.isCompleted() == false ) {
                completed = false;
                finalize();
                return;
            }
        } else {
            blurredRef = refImage;
        }

        if ( blurInput ) {
            if ( doColor ) {
                blurredInput = new ModelImage( ModelImage.ARGB_FLOAT, inputImage.getExtents(), "BlurInput",
                        inputImage.getUserInterface() );
            } else {
                blurredInput = new ModelImage( ModelImage.FLOAT, inputImage.getExtents(), "BlurInput",
                        inputImage.getUserInterface() );
            }
            FileInfoBase[] fileInfo = blurredRef.getFileInfo();
            fileInfo[0].setResolutions( refImage.getFileInfo()[0].getResolutions() );
            AlgorithmGaussianBlur blur2 = new AlgorithmGaussianBlur( blurredInput, inputImage, sigmasInput, true, false );
            if ( doColor ) {
                blur2.setRed( true );
                blur2.setGreen( true );
                blur2.setBlue( true );
            }
            blur2.run();
            if ( blur2.isCompleted() == false ) {
                completed = false;
                finalize();
                return;
            }
        } else {
            blurredInput = inputImage;
        }

        // Resample blurred image of reference image into isotropic voxels
        if ( resampleRef ) {
            transform = new AlgorithmTransform( blurredRef, new TransMatrix( 3 ), interp, resRefIso[0], resRefIso[1],
                    extentsRefIso[0], extentsRefIso[1], false, true, false );
            transform.run();

            if ( transform.isCompleted() == false ) {
                transform.finalize();
                transform = null;
                completed = false;
                finalize();
                return;
            }

            if ( blurredRef != refImage && blurredRef != null ) {
                blurredRef.disposeLocal();
            } else {
                blurredRef = null;
            }
            System.gc();

            imageRefIso = transform.getTransformedImage();
            if ( transform != null ) {
                transform.finalize();
            }
            simpleRef = new ModelSimpleImage( imageRefIso.getExtents(), imageRefIso.getFileInfo( 0 ).getResolutions(),
                    imageRefIso );
        } // if (resampleRef)
        else {
            simpleRef = new ModelSimpleImage( refImage.getExtents(), refImage.getFileInfo( 0 ).getResolutions(),
                    refImage );
        }

        maxDim = simpleRef.xDim;
        if ( simpleRef.yDim > maxDim ) {
            maxDim = simpleRef.yDim;
        }

        if ( ( weighted )
                && ( ( resampleRef )
                        || ( ( refWeight.getFileInfo( 0 ).getResolutions()[0]
                                != refImage.getFileInfo( 0 ).getResolutions()[0] )
                                        || ( refWeight.getFileInfo( 0 ).getResolutions()[1]
                                                != refImage.getFileInfo( 0 ).getResolutions()[1] ) ) ) ) {
            transform = new AlgorithmTransform( refWeight, new TransMatrix( 3 ), interp, resRefIso[0], resRefIso[1],
                    extentsRefIso[0], extentsRefIso[1], false, true, false );
            transform.run();
            if ( transform.isCompleted() == false ) {
                transform.finalize();
                transform = null;
                completed = false;
                finalize();
                return;
            }

            imageWeightRefIso = transform.getTransformedImage();
            if ( transform != null ) {
                transform.finalize();
            }
            simpleWeightRef = new ModelSimpleImage( imageWeightRefIso.getExtents(),
                    imageWeightRefIso.getFileInfo( 0 ).getResolutions(), imageWeightRefIso );
            if ( imageWeightRefIso != null ) {
                imageWeightRefIso.disposeLocal();
            }

        } else if ( weighted ) {
            simpleWeightRef = new ModelSimpleImage( refWeight.getExtents(), refWeight.getFileInfo( 0 ).getResolutions(),
                    refWeight );
        }

        if ( imageRefIso != null ) {
            imageRefIso.disposeLocal();
        }
        if ( transform != null ) {
            if ( transform.getTransformedImage() != null ) {
                transform.getTransformedImage().disposeLocal();
            }
            transform.disposeLocal();
            transform = null;
        }
        System.gc();

        if ( resampleInput ) {
            transform2 = new AlgorithmTransform( blurredInput, new TransMatrix( 3 ), interp, resInputIso[0],
                    resInputIso[1], extentsInputIso[0], extentsInputIso[1], false, true, false );
            transform2.run();
            if ( transform2.isCompleted() == false ) {
                transform2.finalize();
                transform2 = null;
                completed = false;
                finalize();
                return;
            }

            if ( blurredInput != inputImage && blurredInput != null ) {
                blurredInput.disposeLocal();
            } else {
                blurredInput = null;
            }
            System.gc();

            imageInputIso = transform2.getTransformedImage();
            if ( transform2 != null ) {
                transform2.finalize();
            }
            simpleInput = new ModelSimpleImage( imageInputIso.getExtents(),
                    imageInputIso.getFileInfo( 0 ).getResolutions(), imageInputIso );
        } // if (resampleInput)
        else {
            simpleInput = new ModelSimpleImage( inputImage.getExtents(), inputImage.getFileInfo( 0 ).getResolutions(),
                    inputImage );

        }
        if ( simpleInput.xDim > maxDim ) {
            maxDim = simpleInput.xDim;
        }
        if ( simpleInput.yDim > maxDim ) {
            maxDim = simpleInput.yDim;
        }

        if ( ( weighted )
                && ( ( resampleInput )
                        || ( ( inputWeight.getFileInfo( 0 ).getResolutions()[0]
                                != inputImage.getFileInfo( 0 ).getResolutions()[0] )
                                        || ( inputWeight.getFileInfo( 0 ).getResolutions()[1]
                                                != inputImage.getFileInfo( 0 ).getResolutions()[1] ) ) ) ) {
            transform2 = new AlgorithmTransform( inputWeight, new TransMatrix( 3 ), interp, resInputIso[0],
                    resInputIso[1], extentsInputIso[0], extentsInputIso[1], false, true, false );
            transform2.run();
            if ( transform2.isCompleted() == false ) {
                transform2.finalize();
                transform2 = null;
                completed = false;
                finalize();
                return;
            }

            imageWeightInputIso = transform2.getTransformedImage();
            if ( transform2 != null ) {
                transform2.finalize();
            }
            simpleWeightInput = new ModelSimpleImage( imageWeightInputIso.getExtents(),
                    imageWeightInputIso.getFileInfo( 0 ).getResolutions(), imageWeightInputIso );
            if ( imageWeightInputIso != null ) {
                imageWeightInputIso.disposeLocal();
            }
        } else if ( weighted ) {
            simpleWeightInput = new ModelSimpleImage( inputWeight.getExtents(),
                    inputWeight.getFileInfo( 0 ).getResolutions(), inputWeight );
        }

        if ( imageInputIso != null ) {
            imageInputIso.disposeLocal();
        }
        if ( transform2 != null ) {
            if ( transform2.getTransformedImage() != null ) {
                transform2.getTransformedImage().disposeLocal();
            }
            transform2.disposeLocal();
            transform2 = null;
        }
        System.gc();

        int subMinFactor = 15000;

        if ( weighted ) {
            for ( i = 0; i < simpleWeightRef.dataSize; i++ ) {
                if ( simpleWeightRef.data[i] > 0 ) {
                    weightedRefPixels++;
                }
            }

            for ( i = 0; i < simpleWeightInput.dataSize; i++ ) {
                if ( simpleWeightInput.data[i] > 0 ) {
                    weightedInputPixels++;
                }
            }

            if ( DOF >= 3 ) {
                if ( weightedRefPixels > subMinFactor && weightedInputPixels > subMinFactor && doSubsample ) {
                    simpleWeightRefSub2 = subsampleBy2( simpleWeightRef, false );
                    simpleWeightInputSub2 = subsampleBy2( simpleWeightInput, false );
                } else {
                    simpleWeightRefSub2 = simpleWeightRef;
                    simpleWeightInputSub2 = simpleWeightInput;
                    allowLevel2 = false;
                }

                for ( i = 0; i < simpleWeightRefSub2.dataSize; i++ ) {
                    if ( simpleWeightRefSub2.data[i] > 0 ) {
                        weightedRefPixelsSub2++;
                    }
                }

                for ( i = 0; i < simpleWeightInputSub2.dataSize; i++ ) {
                    if ( simpleWeightInputSub2.data[i] > 0 ) {
                        weightedInputPixelsSub2++;
                    }
                }

                if ( weightedRefPixelsSub2 > subMinFactor && weightedInputPixelsSub2 > subMinFactor && doSubsample ) {
                    simpleWeightRefSub4 = subsampleBy2( simpleWeightRefSub2, false );
                    simpleWeightInputSub4 = subsampleBy2( simpleWeightInputSub2, false );
                } else {
                    simpleWeightRefSub4 = simpleWeightRefSub2;
                    simpleWeightInputSub4 = simpleWeightInputSub2;
                    allowLevel4 = false;
                }

                for ( i = 0; i < simpleWeightRefSub4.dataSize; i++ ) {
                    if ( simpleWeightRefSub4.data[i] > 0 ) {
                        weightedRefPixelsSub4++;
                    }
                }

                for ( i = 0; i < simpleWeightInputSub4.dataSize; i++ ) {
                    if ( simpleWeightInputSub4.data[i] > 0 ) {
                        weightedInputPixelsSub4++;
                    }
                }

                if ( weightedRefPixelsSub4 > subMinFactor && weightedInputPixelsSub4 > subMinFactor && doSubsample ) {
                    simpleWeightRefSub8 = subsampleBy2( simpleWeightRefSub4, false );
                    simpleWeightInputSub8 = subsampleBy2( simpleWeightInputSub4, false );

                    // For really big images subsample level 8 again!
                    for ( i = 0; i < simpleWeightRefSub8.dataSize; i++ ) {
                        if ( simpleWeightRefSub8.data[i] > 0 ) {
                            weightedRefPixelsSub8++;
                        }
                    }

                    for ( i = 0; i < simpleWeightInputSub8.dataSize; i++ ) {
                        if ( simpleWeightInputSub8.data[i] > 0 ) {
                            weightedInputPixelsSub8++;
                        }
                    }

                    if ( weightedRefPixelsSub8 > subMinFactor && weightedInputPixelsSub8 > subMinFactor && doSubsample ) {
                        ModelSimpleImage simpleWeightRefSub16;
                        ModelSimpleImage simpleWeightInputSub16;
                        Preferences.debug( "Sub sampled level 8 to 16  *********** \n" );

                        simpleWeightRefSub16 = subsampleBy2( simpleWeightRefSub8, false );
                        simpleWeightInputSub16 = subsampleBy2( simpleWeightInputSub8, false );

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
                Preferences.debug(
                        "Weighted ref subsampled 2 = " + simpleWeightRefSub2 + "Weighted ref subsampled 4 = "
                        + simpleWeightRefSub4 + "Weighted ref subsampled 8 = " + simpleWeightRefSub8
                        + "Weighted input subsampled 2 = " + simpleWeightInputSub2 + "Weighted input subsampled 4 = "
                        + simpleWeightInputSub4 + "Weighted input subsampled 8 = " + simpleWeightInputSub8 );
            } // if (DOF >= 3)

        }

        if ( DOF >= 3 ) {
            if ( doColor ) {
                subMinFactor *= 4;
            }

            if ( simpleRef.dataSize > subMinFactor && simpleInput.dataSize > subMinFactor && allowLevel2 && doSubsample ) {
                simpleRefSub2 = subsampleBy2( simpleRef, doColor );
                simpleInputSub2 = subsampleBy2( simpleInput, doColor );
                level1Factor = 2.0f;
            } else {
                simpleRefSub2 = simpleRef;
                simpleInputSub2 = simpleInput;
            }

            if ( simpleRefSub2.dataSize > subMinFactor && simpleInputSub2.dataSize > subMinFactor && allowLevel4
                    && doSubsample ) {
                simpleRefSub4 = subsampleBy2( simpleRefSub2, doColor );
                simpleInputSub4 = subsampleBy2( simpleInputSub2, doColor );
                level2Factor = 2.0f;
            } else {
                simpleRefSub4 = simpleRefSub2;
                simpleInputSub4 = simpleInputSub2;
            }

            if ( simpleRefSub4.dataSize > subMinFactor && simpleInputSub4.dataSize > subMinFactor && allowLevel8
                    && doSubsample ) {
                simpleRefSub8 = subsampleBy2( simpleRefSub4, doColor );
                simpleInputSub8 = subsampleBy2( simpleInputSub4, doColor );
                level4Factor = 2.0f;

                // For really big images subsample level 8 again!
                if ( simpleRefSub8.dataSize > subMinFactor && simpleInputSub8.dataSize > subMinFactor && allowLevel16 ) {
                    ModelSimpleImage simpleRefSub16;
                    ModelSimpleImage simpleInputSub16;
                    Preferences.debug( "Sub sampled level 8 to 16  *********** \n" );

                    simpleRefSub16 = subsampleBy2( simpleRefSub8, doColor );
                    simpleInputSub16 = subsampleBy2( simpleInputSub8, doColor );

                    simpleRefSub8 = simpleRefSub16;
                    simpleInputSub8 = simpleInputSub16;
                    level4Factor = 4.0f;
                }
            } else {
                simpleRefSub8 = simpleRefSub4;
                simpleInputSub8 = simpleInputSub4;
            }
            Preferences.debug(
                    "Level 1 factor = " + level1Factor + "\n" + "Level 2 factor = " + level2Factor + "\n"
                    + "Level 4 factor = " + level4Factor + "\n" + "Ref subsampled 2 = " + simpleRefSub2
                    + "Ref subsampled 4 = " + simpleRefSub4 + "Ref subsampled 8 = " + simpleRefSub8
                    + "Input subsampled 2 = " + simpleInputSub2 + "Input subsampled 4 = " + simpleInputSub4
                    + "Input subsampled 8 = " + simpleInputSub8 );
        } // if (DOF >= 3)

        buildProgressBar( "Registering images", "Beginning registration", 0, 100 );
        initProgressBar();
        long time = System.currentTimeMillis();
        if ( DOF >= 3 ) {
            Preferences.debug( " Starting level 8 ************************************************\n" );

            Vector[] minimas = levelEight( simpleRefSub8, simpleInputSub8 );
            time = System.currentTimeMillis() - time;
            Preferences.debug( " Level 8 min = " + ( (float) time / 60000.0f ) + "\n" );
            time = System.currentTimeMillis();

            if ( threadStopped ) {
                finalize();
                return;
            }

            Preferences.debug( " Starting level 4 ************************************************\n" );
            Vector minima = levelFour( simpleRefSub4, simpleInputSub4, minimas[0], minimas[1] );
            time = System.currentTimeMillis() - time;
            Preferences.debug( " Level 4  min = " + ( (float) time / 60000.0f ) + "\n" );
            time = System.currentTimeMillis();

            if ( threadStopped ) {
                finalize();
                return;
            }

            Preferences.debug( " Starting level 2 ************************************************\n" );
            MatrixListItem item = levelTwo( simpleRefSub2, simpleInputSub2, minima );
            time = System.currentTimeMillis() - time;
            Preferences.debug( " Level 2 min = " + ( (float) time / 60000.0f ) + "\n" );
            time = System.currentTimeMillis();

            if ( threadStopped ) {
                finalize();
                return;
            }

            Preferences.debug( " Starting level 1 ************************************************\n" );
            answer = levelOne( simpleRef, simpleInput, item );
        } // if (DOF >= 3)
        else {
            Preferences.debug( " Starting level 1 ************************************************\n" );
            answer = levelOne2D( simpleRef, simpleInput );
        }
        time = System.currentTimeMillis() - time;
        Preferences.debug( " Level 1 min = " + ( (float) time / 60000.0f ) + "\n" );
        time = System.currentTimeMillis();

        if ( threadStopped ) {
            finalize();
            return;
        }

        answer.matrix.invert();

        disposeLocal();
        finalize();
        completed = true;
    }

    /**
     *	Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {

        if ( simpleRef != null ) {
            simpleRef.disposeLocal( false );
        }
        if ( simpleInput != null ) {
            simpleInput.disposeLocal( false );
        }
        if ( simpleRefSub2 != null ) {
            simpleRefSub2.disposeLocal( false );
        }
        if ( simpleInputSub2 != null ) {
            simpleInputSub2.disposeLocal( false );
        }
        if ( simpleRefSub4 != null ) {
            simpleRefSub4.disposeLocal( false );
        }
        if ( simpleInputSub4 != null ) {
            simpleInputSub4.disposeLocal( false );
        }
        if ( simpleRefSub8 != null ) {
            simpleRefSub8.disposeLocal( false );
        }
        if ( simpleInputSub8 != null ) {
            simpleInputSub8.disposeLocal( false );
        }

        if ( simpleWeightRef != null ) {
            simpleWeightRef.disposeLocal( false );
        }
        if ( simpleWeightInput != null ) {
            simpleWeightInput.disposeLocal( false );
        }
        if ( simpleWeightRefSub2 != null ) {
            simpleWeightRefSub2.disposeLocal( false );
        }
        if ( simpleWeightInputSub2 != null ) {
            simpleWeightInputSub2.disposeLocal( false );
        }
        if ( simpleWeightRefSub4 != null ) {
            simpleWeightRefSub4.disposeLocal( false );
        }
        if ( simpleWeightInputSub4 != null ) {
            simpleWeightInputSub4.disposeLocal( false );
        }
        if ( simpleWeightRefSub8 != null ) {
            simpleWeightRefSub8.disposeLocal( false );
        }
        if ( simpleWeightInputSub8 != null ) {
            simpleWeightInputSub8.disposeLocal( false );
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

        if ( blurredRef != refImage && blurredRef != null ) {
            blurredRef.disposeLocal();
        } else {
            blurredRef = null;
        }

        if ( blurredInput != inputImage && blurredInput != null ) {
            blurredInput.disposeLocal();
        } else {
            blurredInput = null;
        }

        if ( imageRefIso != null ) {
            imageRefIso.disposeLocal();
        } else {
            if ( transform != null && transform.getTransformedImage() != null ) {
                transform.getTransformedImage().disposeLocal();
                transform = null;
            }
        }

        if ( imageInputIso != null ) {
            imageInputIso.disposeLocal();
        } else {
            if ( transform2 != null && transform2.getTransformedImage() != null ) {
                transform2.getTransformedImage().disposeLocal();
                transform2 = null;
            }
        }
        if ( imageInputIso != null ) {
            imageInputIso.disposeLocal();
        }

        refImage = null;
        inputImage = null;
        if ( transform != null ) {
            transform.disposeLocal();
        }
        if ( transform2 != null ) {
            transform2.disposeLocal();
        }

        System.gc();

    }

    /**
     *   Constructs a string of the contruction parameters and outputs
     *   the string to the messsage frame if the logging procedure is turned on.
     */
    private void constructLog() {
        historyString = getConstructionInfo();
    }

    /**
     *   Creates a string with the parameters that the image was constructed with.
     *   @return Construction info.
     */
    private String getConstructionInfo() {
        String s;

        s = new String( "RegistrationOAR2D(" + refImage.getImageName() + ", " + inputImage.getImageName() + ", " );
        if ( weighted ) {
            s += refWeight.getImageName() + ", " + inputWeight.getImageName() + ", ";
        }
        switch ( costChoice ) {
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
        switch ( interp ) {
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
     *	Accessor that returns the matrix calculated in this algorithm.
     *	@return Matrix found at the end of algorithm.
     */
    public TransMatrix getTransform() {
        return answer.matrix;
    }

    /**
     *	Takes two images that have been subsampled by a factor of eight.
     *   Sets up the cost function with the images and the weighted images,
     *   if necessary.
     *	Uses the coarse sampling rate and optimizes translations and global
     *   scale at the given rotation.  So for example, if the coarse sampling
     *   range were -30 to 30 at every 15 degrees, we would optimize at rotations
     *   of -30, -15, 0, 15, 30.  Measures the cost at the fine sampling rate.
     *   Interpolates the translations and global scale to come up with a good
     *   guess as to what the optimized translation would be at that point.
     *   Takes the top 20% of the points and optimizes them.
     *   Now have a large multi-array of costs.  20% of those have been optimized
     *   and placed back into their original position in the multi-array.  Looks
     *   at the 2 neighbors of a point: + and  - one fine sample.  If the point
     *   has a cost greater than any of these, it is not a minima.  Otherwise it is.
     *   Saves it in a vector of minima.
     *   Optimizes the minima over rotation as well as translations and global scale.
     *   (Previously had not optimized over rotation.)  Returns two vectors, one
     *   containing the minima before optimization, one containing the minima after
     *   optimization.
     *	@param ref		Subsampled by 8 reference image.
     *	@param input	Subsampled by 8 input image.
     *	@return			List of preoptimized and optimized points.
     */
    private Vector[] levelEight( ModelSimpleImage ref, ModelSimpleImage input ) {
        double factor;
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D( ref, input, costChoice, 32, 1 );
        if ( weighted ) {
            cost.setRefWgtImage( simpleWeightRefSub8 );
            cost.setInputWgtImage( simpleWeightInputSub8 );
        }
        Point2Dd cog = calculateCenterOfMass2D( input, simpleWeightInputSub8, doColor );
        Point2Dd cogR = calculateCenterOfMass2D( ref, simpleWeightRefSub8, doColor );

        Preferences.debug( " COG input image = " + cog + "\n" );
        Preferences.debug( " COG ref   image = " + cogR + "\n" );

        double diffX = ( cog.x - cogR.x );
        double diffY = ( cog.y - cogR.y );

        double[] initial = new double[7];

        initial[0] = 0; // initial rotation
        initial[1] = diffX; // initial translations
        initial[2] = diffY;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        double[][] transforms = new double[coarseNum][3];

        //Optimizing over translations and global scaling in AlgorithmPowellOpt2D for DOF = 3
        //Optimizing only over translations in AlgorithmPowellOpt2D for DOF = 2
        AlgorithmPowellOpt2D powell;
        maxIter = baseNumIter;
        if ( DOF > 3 ) {
            powell = new AlgorithmPowellOpt2D( this, cog, 3, cost, initial, getTolerance( 3 ), maxIter, false,
                    bracketBound );
        } else {
            powell = new AlgorithmPowellOpt2D( this, cog, 2, cost, initial, getTolerance( 2 ), maxIter, false,
                    bracketBound );
        }
        // Should we even try to coarse since 2D images at level 8 are pretty small and not computionally taxing ?
        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Optimizing at coarse samples" );
        }
        for ( int i = 0; i < coarseNum && !threadStopped; i++ ) {
            if ( isProgressBarVisible() ) {
                progressBar.updateValue( ( i + 1 ) * 10 / coarseNum, activeImage );
            }
            initial[0] = rotateBegin + i * coarseRate;
            // find cost, record
            powell.setInitialPoint( initial );
            powell.run();
            transforms[i] = powell.getPoint();
        }
        if ( threadStopped ) {
            return null;
        }

        MatrixListItem[] matrixList = new MatrixListItem[fineNum];

        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Measuring at fine samples" );
        }
        double[] costs = new double[fineNum];
        int index = 0;
        for ( int i = 0; i < fineNum && !threadStopped; i++ ) {
            if ( isProgressBarVisible() ) {
                progressBar.updateValue( 10 + ( ( i + 1 ) * 5 / fineNum ), activeImage );
            }
            initial[0] = rotateBegin + i * fineRate;
            // sets up translation and global scaling factors
            factor = ( rotateBegin - rotateBegin + i * fineRate ) / coarseRate;
            interpolate( factor, initial, transforms, ( DOF > 3 ) );
            initial[4] = initial[3];

            powell.setInitialPoint( initial );
            powell.measureCost();
            matrixList[i] = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
            costs[index++] = matrixList[i].cost;
        }
        if ( threadStopped ) {
            return null;
        }

        if ( isProgressBarVisible() ) {
            progressBar.updateValue( 15, activeImage );
        }
        Arrays.sort( costs );
        double threshold = costs[0] + 0.2 * ( costs[costs.length - 1] - costs[0] );
        if ( threshold > costs[ (int) ( 0.2 * costs.length )] ) {
            threshold = costs[ (int) ( 0.2 * costs.length )];
        }
        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Optimizing top samples" );
        }
        for ( int i = 0; i < fineNum && !threadStopped; i++ ) {
            if ( isProgressBarVisible() ) {
                progressBar.updateValue( 15 + ( ( i + 1 ) * 5 / fineNum ), activeImage );
            }
            if ( matrixList[i].cost < threshold ) {
                powell.setInitialPoint( matrixList[i].initial );
                powell.run();
                matrixList[i] = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
            }
        }
        if ( threadStopped ) {
            return null;
        }

        Vector minima = new Vector();
        for ( int i = 0; i < fineNum; i++ ) {
            boolean minimum = true; // possible minimum
            for ( int itest = -1; itest <= 1 && minimum; itest += 2 ) { // as long as still possible minimum, check neighbors one degree off
                if ( ( i + itest >= 0 ) && ( i + itest < fineNum ) ) {
                    if ( matrixList[i].cost > matrixList[i + itest].cost ) {
                        minimum = false;
                    } // not a minimum if a neighbor has a lower cost
                }
            }
            if ( minimum ) {
                minima.add( matrixList[i] );
            }
        }
        if ( threadStopped ) {
            return null;
        }

        Preferences.debug( "Number of minima: " + minima.size() + "\n" );
        Vector optMinima = new Vector();
        // Now freely optimizes over rotation:

        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Optimizing minima" );
        }
        int count = 0;

        int degree = ( DOF < 4 ) ? DOF : 4;
        maxIter = baseNumIter * 2;
        powell = new AlgorithmPowellOpt2D( this, cog, degree, cost, initial, getTolerance( degree ), maxIter, rigidFlag,
                bracketBound );
        MatrixListItem item;
        for ( Enumeration en = minima.elements(); en.hasMoreElements() && !threadStopped; ) {
            if ( isProgressBarVisible() ) {
                progressBar.updateValue( 20 + ( ( count + 1 ) * 5 / minima.size() ), activeImage );
            }
            powell.setInitialPoint( ( (MatrixListItem) en.nextElement() ).initial );
            powell.run();
            item = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
            Preferences.debug( " item.cost = " + item.cost + "\n" );
            optMinima.add( item );
            count++;
        }
        if ( threadStopped ) {
            return null;
        }

        if ( isProgressBarVisible() ) {
            progressBar.updateValue( 25, activeImage );
        }
        cost.disposeLocal();
        powell.disposeLocal();
        return new Vector[] { minima, optMinima };
    }

    /**
     *   Takes two images that have been subsampled by a factor of four, and
     *   two vectors of minima.  Sets up the cost function with the images
     *   and the weighted images, if necessary.  Adds the level4Factor determined
     *   during subsampling.  Measures the costs of the minima on the
     *   images and sort them.  Takes the top three in each vector
     *   (pre-optimization and post-optimization) and optimizes them.  Puts
     *   them all into one vector.  Perturbs the rotation by zero and plus-minus fineDelta.
     *   If it's not a rigid transformation, perturbs the global scaling by
     *   factors of 0.8, 0.9, 1.0, 1.1, and 1.2.
     *   Optimize the perturbations.  Returns a vector of the perturbed,
     *   optimized minima.
     *   @param ref       Reference image, subsampled by 4.
     *   @param input     Input image, subsampled by 4.
     *   @param minima    Preoptimized minima.
     *   @param optMinima Optimized minima.
     *   @return          A vector of perturbed, optimized minima.
     */
    private Vector levelFour( ModelSimpleImage ref, ModelSimpleImage input, Vector minima, Vector optMinima ) {
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D( ref, input, costChoice, 64, 1 );
        if ( weighted ) {
            cost.setRefWgtImage( simpleWeightRefSub4 );
            cost.setInputWgtImage( simpleWeightInputSub4 );
        }
        Point2Dd cog = calculateCenterOfMass2D( input, simpleWeightInputSub4, doColor );
        MatrixListItem item = null;

        //fix translations based on image resolutions!
        for ( Enumeration en = minima.elements(); en.hasMoreElements(); ) {
            item = ( (MatrixListItem) en.nextElement() );
            item.initial[1] *= level4Factor;
            item.initial[2] *= level4Factor;
        }

        for ( Enumeration en = optMinima.elements(); en.hasMoreElements(); ) {
            item = ( (MatrixListItem) en.nextElement() );
            item.initial[1] *= level4Factor;
            item.initial[2] *= level4Factor;
        }

        int degree = ( DOF < 4 ) ? DOF : 4;

        maxIter = baseNumIter * 2;
        AlgorithmPowellOpt2D powell = new AlgorithmPowellOpt2D( this, cog, degree, cost, dummy, getTolerance( degree ),
                maxIter, rigidFlag, bracketBound );

        for ( Enumeration en = minima.elements(); en.hasMoreElements() && !threadStopped; ) {
            item = ( (MatrixListItem) en.nextElement() );
            powell.setInitialPoint( item.initial );
            powell.measureCost();
            item.cost = powell.getCost(); // pointer, so this changes the element in the minima Vector
        }
        if ( threadStopped ) {
            return null;
        }

        for ( Enumeration en = optMinima.elements(); en.hasMoreElements() && !threadStopped; ) {
            item = ( (MatrixListItem) en.nextElement() );
            powell.setInitialPoint( item.initial );
            powell.measureCost();
            item.cost = powell.getCost(); // pointer, so this changes the element in the minima Vector
        }
        if ( threadStopped ) {
            return null;
        }

        Collections.sort( minima );
        Collections.sort( optMinima );
        Vector newMinima = new Vector();
        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Optimizing new minima" );
        }

        // Now changed so that the number of minima to test at Level Four is a variable,
        // passed in from JDialog.  It used to be set to "3".
        int total = ( numMinima < minima.size() ) ? numMinima : minima.size();
        powell.setMaxIterations( baseNumIter );

        for ( int i = 0; i < total && !threadStopped; i++ ) {
            if ( isProgressBarVisible() ) {
                progressBar.updateValue( 25 + ( ( i + 1 ) * 4 / total ), activeImage );
            }
            powell.setInitialPoint( ( (MatrixListItem) minima.elementAt( i ) ).initial );
            powell.run();
            if ( threadStopped ) {
                return null;
            }
            item = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
            newMinima.add( item );
            powell.setInitialPoint( ( (MatrixListItem) optMinima.elementAt( i ) ).initial );
            powell.run();
            if ( threadStopped ) {
                return null;
            }
            item = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
            newMinima.add( item );
        }
        if ( threadStopped ) {
            return null;
        }

        Collections.sort( newMinima );
        double fineDelta = fineRate / 2.0;
        double[] initial;
        Vector perturbList = new Vector();
        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Perturbing minima" );
        }
        // Perturb rotation.  Add fine delta and optimize, then subtract fine delta and optimize.
        for ( int j = 0; j < 3 && !threadStopped; j++ ) {
            for ( int i = 0; i < 2 * total && !threadStopped; i++ ) {
                if ( isProgressBarVisible() ) {
                    progressBar.updateValue( 29 + ( ( ( j * 2 * total + i + 1 ) * 3 ) / ( total * 12 ) ), activeImage );
                }
                initial = (double[]) ( (MatrixListItem) newMinima.elementAt( i ) ).initial.clone();
                Preferences.debug( "Perturbing initial[0] by " );
                if ( j == 0 ) {
                    Preferences.debug( "No fineDelta added or subtracted\n" );
                }
                if ( j == 1 ) {
                    initial[0] += fineDelta;
                    Preferences.debug( "adding " + fineDelta + "\n" );
                }
                if ( j == 2 ) {
                    initial[0] -= fineDelta;
                    Preferences.debug( "subtracting " + fineDelta + "\n" );
                }
                // 1.) unchanged initial
                // 2.) make initial variable old initial + fineDelta
                // 3.) Make initial variable old initial - fineDelta
                powell.setInitialPoint( initial );
                powell.run();
                item = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
                perturbList.add( item );
            }
        }
        if ( threadStopped ) {
            return null;
        }

        if ( DOF > 3 ) {
            float scaleDelta = 0.8f;
            for ( int j = 0; j < 5 && !threadStopped; j++ ) {
                for ( int i = 0; i < 2 * total && !threadStopped; i++ ) {
                    if ( isProgressBarVisible() ) {
                        progressBar.updateValue( 32 + ( ( ( 2 * j * total + i + 1 ) * 3 ) / ( total * 8 ) ), activeImage );
                    }
                    initial = (double[]) ( (MatrixListItem) newMinima.elementAt( i ) ).initial.clone();
                    if ( j == 1 ) {
                        scaleDelta = 0.9f;
                    } else if ( j == 2 ) {
                        scaleDelta = 1.0f;
                    } else if ( j == 3 ) {
                        scaleDelta = 1.1f;
                    } else if ( j == 4 ) {
                        scaleDelta = 1.2f;
                    }
                    Preferences.debug( "Perturbing initial[3] by " );
                    initial[3] *= scaleDelta;
                    Preferences.debug( "Multiplying by " + scaleDelta + "\n" );
                    // make initial variable old initial * scaleDelta in each dimension
                    powell.setInitialPoint( initial );
                    powell.run();
                    item = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
                    perturbList.add( item );
                }
            }
        }
        if ( threadStopped ) {
            return null;
        }

        Collections.sort( perturbList );
        if ( isProgressBarVisible() ) {
            progressBar.updateValue( 35, activeImage );
        }
        cost.disposeLocal();
        powell.disposeLocal();
        return perturbList;
    }

    /**
     *   Takes two images that have been subsampled by a factor of 2 and
     *   a vector of minima.  Sets up the cost function with the images and
     *   the weighted images, if necessary.  Adds the level2Factor determined
     *   during subsampling.  Measures the costs of the minima at
     *   the images.  Optimizes the best minimum with 4 degrees of
     *   freedom, then 5, then 7.  If the user has limited the degrees
     *   of freedom to 3, there will only be one optimization run,
     *   with 3 degrees of freedom.  Returns the best minimum
     *   after optimization.
     *   @param ref      Reference image, subsampled by 2.
     *   @param input    Input image, subsampled by 2.
     *   @param minima   Minima.
     *   @return         The optimized minimum.
     */
    private MatrixListItem levelTwo( ModelSimpleImage ref, ModelSimpleImage input, Vector minima ) {
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D( ref, input, costChoice, 128, 1 );
        if ( weighted ) {
            cost.setRefWgtImage( simpleWeightRefSub2 );
            cost.setInputWgtImage( simpleWeightInputSub2 );
        }
        Point2Dd cog = calculateCenterOfMass2D( input, simpleWeightInputSub2, doColor );
        MatrixListItem item = null;

        for ( Enumeration en = minima.elements(); en.hasMoreElements(); ) {
            item = ( (MatrixListItem) en.nextElement() );
            item.initial[1] *= level2Factor;
            item.initial[2] *= level2Factor;
        }

        int degree = ( DOF < 4 ) ? DOF : 4;
        maxIter = baseNumIter;
        AlgorithmPowellOpt2D powell = new AlgorithmPowellOpt2D( this, cog, degree, cost, item.initial,
                getTolerance( degree ), maxIter, rigidFlag, bracketBound );

        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Measuring costs of minima" );
        }
        for ( Enumeration en = minima.elements(); en.hasMoreElements(); ) {
            item = ( (MatrixListItem) en.nextElement() );
            powell.setInitialPoint( item.initial );
            powell.measureCost();
            item.cost = powell.getCost(); // pointer, so this changes the element in the minima Vector
        }
        Collections.sort( minima );

        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Optimizing with " + degree + " DOF" );
        }
        powell = new AlgorithmPowellOpt2D( this, cog, degree, cost, item.initial, getTolerance( degree ), maxIter,
                rigidFlag, bracketBound );
        if ( isProgressBarVisible() ) {
            powell.setProgressBar( progressBar, 35, 8 );
        }
        powell.setInitialPoint( ( (MatrixListItem) minima.elementAt( 0 ) ).initial );
        Preferences.debug( "item for level2 powell run = " + (MatrixListItem) minima.elementAt( 0 ) + "\n" );
        powell.run();
        if ( threadStopped ) {
            return null;
        }
        item = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
        Preferences.debug( "Level 2, after " + degree + " DOF: " + item + "\n" );

        if ( DOF > 4 ) {
            degree = 5;
            if ( isProgressBarVisible() ) {
                progressBar.setMessage( "Optimizing with " + degree + " DOF" );
            }
            if ( isProgressBarVisible() ) {
                progressBar.updateValue( 43, activeImage );
            }
            powell = new AlgorithmPowellOpt2D( this, cog, degree, cost, item.initial, getTolerance( degree ), maxIter,
                    rigidFlag, bracketBound );
            if ( isProgressBarVisible() ) {
                powell.setProgressBar( progressBar, 43, 8 );
            }
            powell.run();
            if ( threadStopped ) {
                return null;
            }
            item = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
            Preferences.debug( "Level 2, after " + degree + " DOF: " + item + "\n" );

            if ( DOF > 5 ) {
                degree = ( DOF < 7 ) ? DOF : 7;
                powell = new AlgorithmPowellOpt2D( this, cog, degree, cost, item.initial, getTolerance( degree ),
                        maxIter, rigidFlag, bracketBound );
                if ( isProgressBarVisible() ) {
                    progressBar.setMessage( "Optimizing with 7 DOF" );
                }
                if ( isProgressBarVisible() ) {
                    progressBar.updateValue( 51, activeImage );
                }
                if ( isProgressBarVisible() ) {
                    powell.setProgressBar( progressBar, 51, 9 );
                }
                powell.run();
                if ( threadStopped ) {
                    return null;
                }
                item = new MatrixListItem( powell.getCost(), powell.getMatrix(), powell.getFinal() );
                Preferences.debug( "Level 2, after " + degree + " DOF: " + item + "\n" );
            }
        }
        if ( isProgressBarVisible() ) {
            progressBar.updateValue( 60, activeImage );
        }
        cost.disposeLocal();
        powell.disposeLocal();
        return item;
    }

    /**
     *   Takes the two images, no subsampling, and the best minimum so far.
     *   Sets up the cost function with the images and the weighted images,
     *   if necessary.  Adds the level1Factor determined during subsampling.
     *   Performs one optimization run, with the maximum allowable degrees
     *   of freedom as specified by the user (the max is 7).  Returns the
     *   best minimum.
     *   @param ref      Reference image.
     *   @param input    Input image.
     *   @param item     Best minimum so far.
     *   @return         Best minimum after optimization.
     */
    private MatrixListItem levelOne( ModelSimpleImage ref, ModelSimpleImage input, MatrixListItem item ) {
        int degree;
        AlgorithmPowellOpt2D powell;
        MatrixListItem item2;
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D( ref, input, costChoice, 256, 1 );
        if ( weighted ) {
            cost.setRefWgtImage( simpleWeightRef );
            cost.setInputWgtImage( simpleWeightInput );
        }
        Point2Dd cog = calculateCenterOfMass2D( input, simpleWeightInput, doColor );

        item.initial[1] *= level1Factor;
        item.initial[2] *= level1Factor;

        degree = DOF;

        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Starting last optimization" );
        }

        maxIter = baseNumIter * 2;
        powell = new AlgorithmPowellOpt2D( this, cog, degree, cost, item.initial, getTolerance( DOF ), maxIter,
                rigidFlag, bracketBound );

        if ( isProgressBarVisible() ) {
            powell.setProgressBar( progressBar, 60, 40 );
        }

        powell.run();
        if ( threadStopped ) {
            return null;
        }
        item2 = new MatrixListItem( powell.getCost(), powell.getMatrix( input.xRes ), powell.getFinal() );
        Preferences.debug( "Best answer: \n" + item2 + "\n" );
        if ( isProgressBarVisible() ) {
            progressBar.updateValue( 100, activeImage );
        }
        if ( isProgressBarVisible() ) {
            progressBar.dispose();
        }
        cost.disposeLocal();
        powell.disposeLocal();
        return item2;
    }

    /**
     *   Only used for translations only = 2 degrees of freedom
     *   levelEight, levelFour, and levelTwo are skipped
     *   Takes the two images, no subsampling.
     *   Sets up the cost function with the images and the weighted images,
     *   Performs one optimization run, with 2 degrees of freedom
     *   Returns the best minimum.
     *   @param ref      Reference image.
     *   @param input    Input image.
     *   @return         Best minimum after optimization.
     */
    private MatrixListItem levelOne2D( ModelSimpleImage ref, ModelSimpleImage input ) {
        int degree;
        AlgorithmPowellOpt2D powell;
        MatrixListItem item2;
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D( ref, input, costChoice, 256, 1 );
        if ( weighted ) {
            cost.setRefWgtImage( simpleWeightRef );
            cost.setInputWgtImage( simpleWeightInput );
        }
        Point2Dd cog = calculateCenterOfMass2D( input, simpleWeightInput, doColor );
        Point2Dd cogR = calculateCenterOfMass2D( ref, simpleWeightRef, doColor );

        Preferences.debug( " COG input image = " + cog + "\n" );
        Preferences.debug( " COG ref   image = " + cogR + "\n" );

        double diffX = ( cog.x - cogR.x );
        double diffY = ( cog.y - cogR.y );

        double[] initial = new double[7];

        initial[0] = 0; // initial rotation
        initial[1] = diffX; // initial translations
        initial[2] = diffY;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        degree = 2;

        if ( isProgressBarVisible() ) {
            progressBar.setMessage( "Starting last optimization" );
        }

        maxIter = baseNumIter * 2;
        powell = new AlgorithmPowellOpt2D( this, cog, degree, cost, initial, getTolerance( DOF ), maxIter, rigidFlag,
                bracketBound );

        if ( isProgressBarVisible() ) {
            powell.setProgressBar( progressBar, 60, 40 );
        }

        powell.run();
        if ( threadStopped ) {
            return null;
        }

        item2 = new MatrixListItem( powell.getCost(), powell.getMatrix( input.xRes ), powell.getFinal() );

        Preferences.debug( "Best answer: \n" + item2 + "\n" );

        if ( isProgressBarVisible() ) {
            progressBar.updateValue( 100, activeImage );
        }
        if ( isProgressBarVisible() ) {
            progressBar.dispose();
        }
        cost.disposeLocal();
        powell.disposeLocal();
        return item2;
    }

    /**
     *   Calculates the center of mass (gravity) of a 2D image. In image space where
     *   the upper left hand corner of the image is 0,0. The x axis goes left to right,
     *   y axis goes top to bottom.
     *   (i.e. the right hand rule). One could simply multiply by voxel resolutions.
     *   @param image
     *   @param wgtImage
     *   @return the center of mass as a 2D point
     */
    public Point2Dd calculateCenterOfMass2D( ModelSimpleImage image,
            ModelSimpleImage wgtImage,
            boolean isColor ) {
        int x, y, c;
        float diff;

        Point2Dd cogPt = new Point2Dd( 0, 0 );

        double voxVal = 0.0, total = 0.0, wgtVal = 0.0;

        if ( isColor ) {
            if ( wgtImage == null ) {
                for ( y = 0; y < image.yDim; y++ ) {
                    for ( x = 0; x < image.xDim; x++ ) {
                        for ( c = 1; c <= 3; c++ ) {
                            voxVal = image.data[4 * ( y * image.xDim + x ) + c];
                            cogPt.x += voxVal * x;
                            cogPt.y += voxVal * y;
                            total += voxVal;
                        }
                    }
                }
            } else { // wgtImage != null

                wgtImage.calcMinMax();
                if ( ( wgtImage.min < 0 ) || ( wgtImage.max > 1 ) ) {
                    // remap data - normalize data between 0 and 1
                    if ( wgtImage.min != wgtImage.max ) {
                        diff = wgtImage.max - wgtImage.min;
                        for ( int i = 0; i < wgtImage.data.length; i++ ) {
                            wgtImage.data[i] = ( wgtImage.data[i] - wgtImage.min ) / diff;
                        }
                        wgtImage.calcMinMax();
                    }
                }

                for ( y = 0; y < image.yDim; y++ ) {
                    for ( x = 0; x < image.xDim; x++ ) {
                        wgtVal = wgtImage.data[y * image.xDim + x];
                        for ( c = 1; c <= 3; c++ ) {
                            voxVal = image.data[4 * ( y * image.xDim + x ) + c];
                            cogPt.x += wgtVal * voxVal * x;
                            cogPt.y += wgtVal * voxVal * y;
                            total += wgtVal * voxVal;
                        }
                    }
                }
            }
        } // if (isColor)
        else { // black and white
            if ( wgtImage == null ) {
                for ( y = 0; y < image.yDim; y++ ) {
                    for ( x = 0; x < image.xDim; x++ ) {
                        voxVal = image.data[y * image.xDim + x] - image.min;
                        cogPt.x += voxVal * x;
                        cogPt.y += voxVal * y;
                        total += voxVal;
                    }
                }
            } else { // wgtImage != null

                wgtImage.calcMinMax();
                if ( ( wgtImage.min < 0 ) || ( wgtImage.max > 1 ) ) {
                    // remap data - normalize data between 0 and 1
                    if ( wgtImage.min != wgtImage.max ) {
                        diff = wgtImage.max - wgtImage.min;
                        for ( int i = 0; i < wgtImage.data.length; i++ ) {
                            wgtImage.data[i] = ( wgtImage.data[i] - wgtImage.min ) / diff;
                        }
                        wgtImage.calcMinMax();
                    }
                }

                for ( y = 0; y < image.yDim; y++ ) {
                    for ( x = 0; x < image.xDim; x++ ) {
                        voxVal = image.data[y * image.xDim + x] - image.min;
                        wgtVal = wgtImage.data[y * image.xDim + x];
                        cogPt.x += wgtVal * voxVal * x;
                        cogPt.y += wgtVal * voxVal * y;
                        total += wgtVal * voxVal;

                    }
                }
            }
        } // else black and white

        if ( total != 0 ) {
            cogPt.x /= total;
            cogPt.y /= total;
        }
        return cogPt;
    }

    /**
     *   Takes a simple image and subsamples it by 2, interpolating
     *   so that the new values are averages.
     *   @param srcImage Image to subsample.
     *   @param isColor
     *   @return         Subsampled image.
     */
    private static ModelSimpleImage subsampleBy2( ModelSimpleImage srcImage, boolean isColor ) {
        return srcImage.subSample2dBy2( isColor );
    }

    /**
     *   Performs a bilinear interpolation on points.  Takes an initial point,
     *   a vector of values to set, and an array in which to look at neighbors
     *   of that point.  Sets the appropriate values in the vector.  Does not
     *   set scale if the scale parameter is <code>false</code>.
     *   @param x        Initial index into array.
     *   @param initial  Vector to set; if scale is <code>true</code>, set two
     *                   translations and a scale.  Otherwise just set translations.
     *   @param transforms
     *   @param scale    <code>true</code> means set the scale in the vector.
     */
    private void interpolate( double x, double[] initial, double[][] tranforms, boolean scale ) {
        int ix0, ix1;

        // convert to closest integer values to access proper parts of array
        ix0 = (int) Math.floor( x );
        ix1 = ix0 + 1;

        // can't be bigger than 3
        if ( ( ix0 == coarseNum - 1 ) ) {
            ix1 = ix0;
        }

        if ( scale ) {
            // x translation
            initial[1] = ( x - ix0 ) * tranforms[ix1][1] + ( 1 - x + ix0 ) * tranforms[ix0][1];
            // y translation
            initial[2] = ( x - ix0 ) * tranforms[ix1][2] + ( 1 - x + ix0 ) * tranforms[ix0][2];
            // scale
            initial[3] = ( x - ix0 ) * tranforms[ix1][0] + ( 1 - x + ix0 ) * tranforms[ix0][0];
        } else {
            // x translation
            initial[1] = ( x - ix0 ) * tranforms[ix1][0] + ( 1 - x + ix0 ) * tranforms[ix0][0];
            // y translation
            initial[2] = ( x - ix0 ) * tranforms[ix1][1] + ( 1 - x + ix0 ) * tranforms[ix0][1];
        }
    }

    /**
     *   Gets the tolerance vector based on the degrees of freedom
     *   (the length of the tolerance is the degrees of freedom)
     *   @param DOF   Degrees of freedom, will be length of vector.
     *   @param dims	Image dimensions in x, y and z.
     *   @return      New tolerance vector to send to optimization.
     *
     *  Based on FLIRT paper:
     *  let n=pixel dimension (in one dimension)
     *      R=brain radius, here assumed to be half of field-of-view
     *  Translation tolerance = n/2
     *  Rotation tolerance = (180/PI)*n/(2R)  (converted to degrees because AlgorithmPowell works in degrees)
     *  Scaling tolerance = n/(2R)
     *  Skewing tolerance = n/(2R)
     */
    private double[] getTolerance( int DOF ) {
        double[] tols = new double[DOF];

        if ( DOF == 2 ) {
            tols[0] = tols[1] = 0.5; // translations
        } else if ( DOF == 3 && rigidFlag == true ) {
            tols[0] = ( ( 180. / Math.PI ) / maxDim ); // rotation
            tols[1] = tols[2] = 0.5; // translations
        } else if ( DOF == 3 && rigidFlag == false ) {
            tols[0] = 0.005; // global scaling
            tols[1] = tols[2] = 0.5; // translations
        } else if ( DOF == 4 ) {
            tols[0] = ( ( 180. / Math.PI ) / maxDim ); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = 0.005; // (1.0/maxDim); // scaling tolerances global
        } else if ( DOF == 5 ) {
            tols[0] = ( ( 180. / Math.PI ) / maxDim ); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = tols[4] = 0.005; // (1.0/maxDim); // scaling tolerance x
        } else if ( DOF == 7 ) {
            tols[0] = ( ( 180. / Math.PI ) / maxDim ); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = tols[4] = 0.005; //(1.0/maxDim); // scaling tolerance x
            //tols[5] = tols[6] = (1.0/maxDim); // skew tolerance x
            tols[5] = tols[6] = 0.001; // skew tolerance x
        }

        return tols;
    }

    /**
     *   Helper class to make it easy to store the necessary information
     *   about a minimum.  Stores the "point", or vector at which the
     *   minimum was reached; the "cost", or value of the cost function
     *   at that minimum; and the matrix, which was the true input into
     *   the cost function and represents the transformation that gives
     *   the minimum cost of differences between the images.
     *   Implements Comparable, so that a list of MatrixListItems can be
     *   sorted using Java's sort.
     */
    class MatrixListItem
        implements Comparable {

        /** Cost of function at this minimum. */
        protected double cost;

        /** Matrix that gives best transformation. */
        protected TransMatrix matrix;

        /** Rotations, translations, scales, and skews that make up transformation. */
        protected double[] initial;

        /**
         *   Creates new minimum object, setting the data and copying the
         *   point array explicitly.
         *   @param _cost    Cost of this minimum.
         *   @param _matrix  Matrix that gives best transformation.
         *   @param _initial Rotations, translations, scales, and skews that
         *                   make up transformation.
         */
        protected MatrixListItem( double _cost, TransMatrix _matrix, double[] _initial ) {
            this.cost = _cost;
            this.matrix = _matrix;
            initial = new double[_initial.length];

            for ( int i = 0; i < initial.length; i++ ) {
                initial[i] = _initial[i];
            }
        }

        /**
         *   Necessary to implement so that list may be sorted.
         *   Returns -1 if this cost is less than the parameter's cost;
         *   1 if this cost is greater than the parameter's cost;
         *   and 0 if they are equal.
         *   @param o    MatrixListItem to compare to.
         *   @return     -1 if this is less than, 1 if greater than, 0 if equal.
         */
        public int compareTo( Object o ) {
            if ( cost < ( (MatrixListItem) o ).cost ) {
                return -1;
            } else if ( cost > ( (MatrixListItem) o ).cost ) {
                return 1;
            } else {
                return 0;
            }
        }

        /**
         *   Creates readable string of this object, including cost,
         *   matrix, and point with its meanings.
         *   @return Readable string representation of this object.
         */
        public String toString() {
            String s = "";
            s += "Cost of " + cost + " at:\n";
            s += matrix.toString();
            s += "\n";
            s += "Point:\n";

            s += " Rotation : ";
            s += initial[0];
            s += "\n";

            for ( int i = 1; i < 3; i++ ) {
                s += " Translations : ";
                s += initial[i] + " ";
                s += "\n";
            }
            for ( int i = 3; i < 5; i++ ) {
                s += " Zooms : ";
                s += initial[i] + " ";
                s += "\n";
            }
            for ( int i = 5; i < 7; i++ ) {
                s += " Skews : ";
                s += initial[i] + " ";
                s += "\n";
            }

            return s;

        }

    }

}
