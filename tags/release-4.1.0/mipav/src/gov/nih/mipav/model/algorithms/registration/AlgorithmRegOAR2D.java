package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions2D;
import gov.nih.mipav.model.algorithms.AlgorithmPowellOpt2D;
import gov.nih.mipav.model.algorithms.AlgorithmPowellOptBase;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.Vectornd;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.Point2Dd;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Dimension;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Vector;

import javax.vecmath.Point3d;
import javax.vecmath.Point3f;
import javax.vecmath.Point3i;


/**
 * This is an automatic registration method based on FLIRT. FLIRT stands for FMRIB's Linear Image Registration Tool. For
 * more information on FLIRT, visit their homepage at <a href="http://www.fmrib.ox.ac.uk/fsl/flirt/">
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
 * <p>Note that when 6 degrees of freedom is used the rotation is set equal to 0 because diffX sets (0,2), diffY sets
 * (1,2), scaleX sets (0,0), scaleY sets (1,1), skewX sets (0,1), and skewY sets (1,0) so all 6 elements are set.</p>
 *
 * @author  Neva Cherniavsky
 * @author  Matthew McAuliffe
 */
public class AlgorithmRegOAR2D extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

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

    /**
     * The bracket size around the minimum in multiples of unit_tolerance in the first iteration of Powell's algorithm.
     */
    private int bracketBound = 20;

    /** Number of passes that will be made in the coarse sampling and fine sampling. */
    private int coarseNum, fineNum;

    /** Choice of which cost function to use. */
    private int costChoice;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** Maximum degrees of freedom when running the optimization. */
    private int DOF;

    /** If true subsample. */
    private boolean doSubsample;

    /** Dummy initial values used to create a Powell's algorithm instance before setting initial. */
    private double[] dummy = { 0, 0, 0, 0, 0, 0, 0 };

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
    private ModelImage inputWeight;

    /** Interpolation method. */
    private int interp;

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
     * Limits number of iterations in Powell optimization. maxIter in the call to Powell's will be an integer multiple
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
    private ModelImage refWeight;

    /** DOCUMENT ME! */
    private boolean resampleInput;

    /** DOCUMENT ME! */
    private boolean resampleRef;

    /** The voxel resolutions of the image to be registered to the reference image. */
    private float[] resInput;

    /** The voxel resolutions of the reference image. */
    private float[] resRef;

    /** Flag used to indicate if the registration is rigid (rotation and translation only; DOF = 3. */
    private boolean rigidFlag = false;

    /** Coarse and fine sampling parameters. */
    private float rotateBegin, rotateEnd, coarseRate, fineRate;

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

    /**
     * Used to store all paths for levelEigth, levelFour, levelTwo and levelOne.
     */
    private Vector<Vector<Point3d>>[] paths = new Vector[6];
    
    /**
     * The optimal path.
     */
    private Vector<Point3d> optimalPath = new Vector<Point3d>();
    
    private boolean pathRecorded = false;
    
    private static final Point3d[] originalPath = {
    	new Point3d(18.0,27.201007843017578,35.089054107666016),
    	new Point3d(18.0,26.926931381225586,35.089054107666016),
    	new Point3d(18.0,26.926931381225586,37.107540130615234),
    	new Point3d(18.0,26.987857818603516,37.107540130615234),
    	new Point3d(18.0,26.987857818603516,37.013275146484375),
    	new Point3d(18.0,26.987857818603516,37.013275146484375),
    	new Point3d(19.229249954223633,26.987857818603516,37.013275146484375),
    	new Point3d(19.229249954223633,26.926382064819336,37.013275146484375),
    	new Point3d(19.229249954223633,26.926382064819336,39.34950256347656),
    	new Point3d(19.83618927001953,26.926382064819336,39.34950256347656),
    	new Point3d(19.83618927001953,27.25579261779785,39.34950256347656),
    	new Point3d(19.83618927001953,27.25579261779785,40.23905563354492),
    	new Point3d(19.83618927001953,27.25579261779785,40.23905563354492),
    	new Point3d(19.99574089050293,27.25579261779785,40.23905563354492),
    	new Point3d(19.99574089050293,27.316665649414062,40.23905563354492),
    	new Point3d(9.99574089050293,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016),
    	new Point3d(20.01641845703125,27.316665649414062,40.429630279541016)};
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     *
     * @param  _imageA       Reference image (register input image to reference image).
     * @param  _imageB       Input image (register input image to reference image).
     * @param  _costChoice   Choice of cost functions, like correlation ratio or mutual information.
     * @param  _DOF          Degrees of freedom for registration
     * @param  _interp       Interpolation method used in transformations.
     * @param  _rotateBegin  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEnd    End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRate   Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRate     Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  doSubsample   If true subsample
     *
     *                       <p>Constructor without weighting and without advanced settings (bracket, num iter).</p>
     */
    public AlgorithmRegOAR2D(ModelImage _imageA, ModelImage _imageB, int _costChoice, int _DOF, int _interp,
                             float _rotateBegin, float _rotateEnd, float _coarseRate, float _fineRate,
                             boolean doSubsample) {
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
        coarseNum = (int) ((rotateEnd - rotateBegin) / coarseRate) + 1;
        fineNum = (int) ((rotateEnd - rotateBegin) / fineRate) + 1;
        weighted = false;
        this.doSubsample = doSubsample;

    }

    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     *
     * @param  _imageA       Reference image (register input image to reference image).
     * @param  _imageB       Input image (register input image to reference image).
     * @param  _refWeight    Reference weighted image, used to give certain areas of the image greater impact on the
     *                       registration.
     * @param  _inputWeight  Input weighted image, used to give certain areas of the image greater impact on the
     *                       registration.
     * @param  _costChoice   Choice of cost functions, like correlation ratio or mutual information.
     * @param  _DOF          Degrees of freedom for registration
     * @param  _interp       Interpolation method used in transformations.
     * @param  _rotateBegin  Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEnd    End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRate   Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRate     Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  doSubsample   If true subsample
     *
     *                       <p>Constructor with weighting and without advanced settings (bracket, num iter).</p>
     */
    public AlgorithmRegOAR2D(ModelImage _imageA, ModelImage _imageB, ModelImage _refWeight, ModelImage _inputWeight,
                             int _costChoice, int _DOF, int _interp, float _rotateBegin, float _rotateEnd,
                             float _coarseRate, float _fineRate, boolean doSubsample) {
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
        coarseNum = (int) ((rotateEnd - rotateBegin) / coarseRate) + 1;
        fineNum = (int) ((rotateEnd - rotateBegin) / fineRate) + 1;
        weighted = true;
        this.doSubsample = doSubsample;

    }


    /**
     * Creates new automatic linear registration algorithm and sets necessary variables.
     *
     * @param  _imageA        Reference image (register input image to reference image).
     * @param  _imageB        Input image (register input image to reference image).
     * @param  _costChoice    Choice of cost functions, like correlation ratio or mutual information.
     * @param  _DOF           Degrees of freedom for registration
     * @param  _interp        Interpolation method used in transformations.
     * @param  _rotateBegin   Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEnd     End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRate    Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRate      Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  doSubsample    If true subsample
     * @param  _bracketBound  The bracket size around the minimum in multiples of unit_tolerance for the first iteration
     *                        of Powell's algorithm.
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     *
     *                        <p>Constructor without weighting and with advanced settings (bracket, num iter) set.</p>
     */
    public AlgorithmRegOAR2D(ModelImage _imageA, ModelImage _imageB, int _costChoice, int _DOF, int _interp,
                             float _rotateBegin, float _rotateEnd, float _coarseRate, float _fineRate,
                             boolean doSubsample, int _bracketBound, int _baseNumIter, int _numMinima) {
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
        coarseNum = (int) ((rotateEnd - rotateBegin) / coarseRate) + 1;
        fineNum = (int) ((rotateEnd - rotateBegin) / fineRate) + 1;
        weighted = false;
        this.doSubsample = doSubsample;
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
     * @param  _rotateBegin   Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEnd     End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRate    Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRate      Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  doSubsample    If true subsample
     * @param  _bracketBound  The bracket size around the minimum in multiples of unit_tolerance for the first iteration
     *                        of Powell's algorithm.
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     *
     *                        <p>Constructor with weighting and with advanced settings (bracket, num iter) set.</p>
     */
    public AlgorithmRegOAR2D(ModelImage _imageA, ModelImage _imageB, ModelImage _refWeight, ModelImage _inputWeight,
                             int _costChoice, int _DOF, int _interp, float _rotateBegin, float _rotateEnd,
                             float _coarseRate, float _fineRate, boolean doSubsample, int _bracketBound,
                             int _baseNumIter, int _numMinima) {
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
        coarseNum = (int) ((rotateEnd - rotateBegin) / coarseRate) + 1;
        fineNum = (int) ((rotateEnd - rotateBegin) / fineRate) + 1;
        weighted = true;
        this.doSubsample = doSubsample;
        bracketBound = _bracketBound;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the center of mass (gravity) of a 2D image. In image space where the upper left hand corner of the
     * image is 0,0. The x axis goes left to right, y axis goes top to bottom. (i.e. the right hand rule). One could
     * simply multiply by voxel resolutions.
     *
     * @param   image     DOCUMENT ME!
     * @param   wgtImage  DOCUMENT ME!
     * @param   isColor   DOCUMENT ME!
     *
     * @return  the center of mass as a 2D point
     */
    public Point2Dd calculateCenterOfMass2D(ModelSimpleImage image, ModelSimpleImage wgtImage, boolean isColor) {
        int x, y, c;
        float diff;

        Point2Dd cogPt = new Point2Dd(0, 0);

        double voxVal = 0.0, total = 0.0, wgtVal = 0.0;

        if (isColor) {

            if (wgtImage == null) {

                for (y = 0; y < image.yDim; y++) {

                    for (x = 0; x < image.xDim; x++) {

                        for (c = 1; c <= 3; c++) {
                            voxVal = image.data[(4 * ((y * image.xDim) + x)) + c];
                            cogPt.x += voxVal * x;
                            cogPt.y += voxVal * y;
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

                for (y = 0; y < image.yDim; y++) {

                    for (x = 0; x < image.xDim; x++) {
                        wgtVal = wgtImage.data[(y * image.xDim) + x];

                        for (c = 1; c <= 3; c++) {
                            voxVal = image.data[(4 * ((y * image.xDim) + x)) + c];
                            cogPt.x += wgtVal * voxVal * x;
                            cogPt.y += wgtVal * voxVal * y;
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
                        voxVal = image.data[(y * image.xDim) + x] - image.min;
                        cogPt.x += voxVal * x;
                        cogPt.y += voxVal * y;
                        total += voxVal;
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

                for (y = 0; y < image.yDim; y++) {

                    for (x = 0; x < image.xDim; x++) {
                        voxVal = image.data[(y * image.xDim) + x] - image.min;
                        wgtVal = wgtImage.data[(y * image.xDim) + x];
                        cogPt.x += wgtVal * voxVal * x;
                        cogPt.y += wgtVal * voxVal * y;
                        total += wgtVal * voxVal;

                    }
                }
            }
        } // else black and white

        if (total != 0) {
            cogPt.x /= total;
            cogPt.y /= total;
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
     * Accessor that returns the matrix calculated in this algorithm.
     *
     * @return  Matrix found at the end of algorithm.
     */
    public TransMatrix getTransform() {
        return answer.matrix;
    }

    public void drawLine(int xdim, int ydim, int zdim, float[] image, float value, Vector<Point3i> path){
    	if(path == null)
    		return;
    	for(int i = 0; i < path.size(); i++){
    		Point3i p3i = path.elementAt(i);
    		if(image[p3i.z*xdim*ydim+p3i.y*xdim+p3i.x] >300){
    			image[p3i.z*xdim*ydim+p3i.y*xdim+p3i.x] = value-100;
    		}else{
    			image[p3i.z*xdim*ydim+p3i.y*xdim+p3i.x] = value;
    		}
    	}
    }
    
    public Vector<Point3f> param2Coordinates(double startX, double endX, double stepX,
    		double startY, double endY, double stepY, 
    		double startZ, double endZ, double stepZ, Vector<Point3d> path){
    	if(path == null){
    		return null;
    	}
    	
    	Vector<Point3f> coordPath = new Vector<Point3f>(path.size());
    	for(int i = 0; i < path.size(); i++){
    		Point3d p3d = path.elementAt(i);
    		Point3f p3f = new Point3f();
    		p3f.x = (float)((p3d.y-startX)/stepX);
    		p3f.y = (float)((p3d.z-startY)/stepY);
    		p3f.z = (float)((p3d.x-startZ)/stepZ);
    		coordPath.add(p3f);
    	}
    	return coordPath;
    }
    
    public Vector<Point3i> findPointsOfLine(Vector<Point3f> realPath){
    	if(realPath == null){
    		return null;
    	}
    	
    	Vector<Point3i> imagePath = new Vector<Point3i>(realPath.size()*2);
    	for(int i = 0; i < realPath.size()-1; i++){
    		Point3f p3fFrom = realPath.elementAt(i);
    		Point3f p3fTo = realPath.elementAt(i+1);
    		if(p3fFrom.equals(p3fTo))
    			continue;
    		midpointLine(p3fFrom, p3fTo, imagePath);    		
    	}
    	Point3i previousPoint = imagePath.elementAt(0);
    	
    	for(int i = 1; i < imagePath.size(); i++){
    		Point3i currentPoint = imagePath.elementAt(i);
    		if(currentPoint.equals(previousPoint)){
    			imagePath.remove(i--);
    		}else{
    			previousPoint = currentPoint;
    		}
    	}
    	return imagePath;
    }
    
    public void midpointLine(Point3f p0, Point3f p1, Vector<Point3i> imagePath){
    	if(Math.abs(p1.x-p0.x)>=Math.abs(p1.y-p0.y) && Math.abs(p1.x-p0.x)>=Math.abs(p1.z-p0.z)){
			float a1 = (p1.y - p0.y) / (p1.x - p0.x);
			float b1 = p0.y - a1 * p0.x;
			float a2 = (p1.z - p0.z) / (p1.x - p0.x);
			float b2 = p0.z - a2 * p0.x;
			int x = (int) (p0.x + 0.5);
			float fy = p0.y;
			float fz = p0.z;
			int y = (int) p0.y;
			int z = (int) p0.z;

			while (x < p1.x) {
				float d1 = line(a1, b1, x, y + 0.5f);
				if (d1 < 0) {
					y++;
				}
				float d2 = line(a2, b2, x, z + 0.5f);
				if (d2 < 0) {
					z++;
				}
				Point3i p3i = new Point3i(x, y, z);
				imagePath.add(p3i);
				fy += a1;
				fz += a2;
				x++;
				y = (int) fy;
				z = (int) fz;
			}
		}else if(Math.abs(p1.y-p0.y)>=Math.abs(p1.x-p0.x) && Math.abs(p1.y-p0.y)>=Math.abs(p1.z-p0.z)){
			float a1 = (p1.x - p0.x) / (p1.y - p0.y);
			float b1 = p0.x - a1 * p0.y;
			float a2 = (p1.z - p0.z) / (p1.y - p0.y);
			float b2 = p0.z - a2 * p0.y;
			int y = (int) (p0.y + 0.5);
			float fx = p0.x;
			float fz = p0.z;
			int x = (int) p0.x;
			int z = (int) p0.z;

			while (y < p1.y) {
				float d1 = line(a1, b1, y, x + 0.5f);
				if (d1 < 0) {
					x++;
				}
				float d2 = line(a2, b2, y, z + 0.5f);
				if (d2 < 0) {
					z++;
				}
				Point3i p3i = new Point3i(x, y, z);
				imagePath.add(p3i);
				fx += a1;
				fz += a2;
				y++;
				x = (int) fx;
				z = (int) fz;
			}
		}else{
			float a1 = (p1.x - p0.x) / (p1.z - p0.z);
			float b1 = p0.x - a1 * p0.z;
			float a2 = (p1.y - p0.y) / (p1.z - p0.z);
			float b2 = p0.z - a2 * p0.y;
			int z = (int) (p0.z + 0.5);
			float fy = p0.y;
			float fx = p0.x;
			int y = (int) p0.y;
			int x = (int) p0.x;

			while (z < p1.z) {
				float d1 = line(a1, b1, z, x + 0.5f);
				if (d1 < 0) {
					x++;
				}
				float d2 = line(a2, b2, z, y + 0.5f);
				if (d2 < 0) {
					y++;
				}
				Point3i p3i = new Point3i(x, y, z);
				imagePath.add(p3i);
				fx += a1;
				fy += a2;
				z++;
				y = (int) fy;
				x = (int) fx;
			}
		}
    	
    }
    
    public float line(float a, float b, int x, float y){
    	return y-a*x-b;
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

        if ((resRef[0] == resRef[1]) && (resRef[0] == minSample)) {
            resampleRef = false;
        } else {
            resampleRef = true;
        }

        if ((resInput[0] == resInput[1]) && (resInput[0] == minSample)) {
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
        } catch (OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("Out of memory in AlgorithmOAR2D.");
            disposeLocal();

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
            sigmasRef[i] = ((resInput[i] - resRef[i]) / resRef[i]) * 0.424f; /// 2.36 sigma = x -->  FWHM  0.424 = 1/2.36

            if (sigmasRef[i] < 0.5f) {
                sigmasRef[i] = 0.0f;
            } else {
                blurRef = true;
            }

            sigmasInput[i] = ((resRef[i] - resInput[i]) / resInput[i]) * 0.424f;

            if (sigmasInput[i] < 0.5f) {
                sigmasInput[i] = 0.0f;
            } else {
                blurInput = true;
            }
        }

        
        Preferences.debug(getConstructionInfo());

        if (blurRef) {

            if (doColor) {
                blurredRef = new ModelImage(ModelImage.ARGB_FLOAT, refImage.getExtents(), "BlurRef");
            } else {
                blurredRef = new ModelImage(ModelImage.FLOAT, refImage.getExtents(), "BlurRef");
            }

            // update resolutions
            FileInfoBase[] fileInfo = blurredRef.getFileInfo();
            fileInfo[0].setResolutions(refImage.getFileInfo()[0].getResolutions());

            AlgorithmGaussianBlur blur = new AlgorithmGaussianBlur(blurredRef, refImage, sigmasRef, true, false);

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
                blurredInput = new ModelImage(ModelImage.ARGB_FLOAT, inputImage.getExtents(), "BlurInput");
            } else {
                blurredInput = new ModelImage(ModelImage.FLOAT, inputImage.getExtents(), "BlurInput");
            }

            FileInfoBase[] fileInfo = blurredRef.getFileInfo();
            fileInfo[0].setResolutions(refImage.getFileInfo()[0].getResolutions());

            AlgorithmGaussianBlur blur2 = new AlgorithmGaussianBlur(blurredInput, inputImage, sigmasInput, true, false);

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
            simpleRef = new ModelSimpleImage(refImage.getExtents(), refImage.getFileInfo(0).getResolutions(), refImage);
        }

        maxDim = simpleRef.xDim;

        if (simpleRef.yDim > maxDim) {
            maxDim = simpleRef.yDim;
        }

        if ((weighted) &&
                ((resampleRef) ||
                     ((refWeight.getFileInfo(0).getResolutions()[0] != refImage.getFileInfo(0).getResolutions()[0]) ||
                          (refWeight.getFileInfo(0).getResolutions()[1] !=
                               refImage.getFileInfo(0).getResolutions()[1])))) {
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
            transform2 = new AlgorithmTransform(blurredInput, new TransMatrix(3), interp, resInputIso[0],
                                                resInputIso[1], extentsInputIso[0], extentsInputIso[1], false, true,
                                                false);
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
            simpleInput = new ModelSimpleImage(inputImage.getExtents(), inputImage.getFileInfo(0).getResolutions(),
                                               inputImage);

        }

        if (simpleInput.xDim > maxDim) {
            maxDim = simpleInput.xDim;
        }

        if (simpleInput.yDim > maxDim) {
            maxDim = simpleInput.yDim;
        }

        if ((weighted) &&
                ((resampleInput) ||
                     ((inputWeight.getFileInfo(0).getResolutions()[0] !=
                           inputImage.getFileInfo(0).getResolutions()[0]) ||
                          (inputWeight.getFileInfo(0).getResolutions()[1] !=
                               inputImage.getFileInfo(0).getResolutions()[1])))) {
            transform2 = new AlgorithmTransform(inputWeight, new TransMatrix(3), interp, resInputIso[0], resInputIso[1],
                                                extentsInputIso[0], extentsInputIso[1], false, true, false);
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

                if ((weightedRefPixels > subMinFactor) && (weightedInputPixels > subMinFactor) && doSubsample) {
                    simpleWeightRefSub2 = subsampleBy2(simpleWeightRef, false);
                    simpleWeightInputSub2 = subsampleBy2(simpleWeightInput, false);
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

                if ((weightedRefPixelsSub2 > subMinFactor) && (weightedInputPixelsSub2 > subMinFactor) && doSubsample) {
                    simpleWeightRefSub4 = subsampleBy2(simpleWeightRefSub2, false);
                    simpleWeightInputSub4 = subsampleBy2(simpleWeightInputSub2, false);
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

                if ((weightedRefPixelsSub4 > subMinFactor) && (weightedInputPixelsSub4 > subMinFactor) && doSubsample) {
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
                            doSubsample) {
                        ModelSimpleImage simpleWeightRefSub16;
                        ModelSimpleImage simpleWeightInputSub16;
                        Preferences.debug("Sub sampled level 8 to 16  *********** \n");

                        simpleWeightRefSub16 = subsampleBy2(simpleWeightRefSub8, false);
                        simpleWeightInputSub16 = subsampleBy2(simpleWeightInputSub8, false);

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

                Preferences.debug("Weighted ref subsampled 2 = " + simpleWeightRefSub2 +
                                  "Weighted ref subsampled 4 = " + simpleWeightRefSub4 +
                                  "Weighted ref subsampled 8 = " + simpleWeightRefSub8 +
                                  "Weighted input subsampled 2 = " + simpleWeightInputSub2 +
                                  "Weighted input subsampled 4 = " + simpleWeightInputSub4 +
                                  "Weighted input subsampled 8 = " + simpleWeightInputSub8);
            } // if (DOF >= 3)

        }

        if (DOF >= 3) {

            if (doColor) {
                subMinFactor *= 4;
            }

            if ((simpleRef.dataSize > subMinFactor) && (simpleInput.dataSize > subMinFactor) && allowLevel2 &&
                    doSubsample) {
                simpleRefSub2 = subsampleBy2(simpleRef, doColor);
                simpleInputSub2 = subsampleBy2(simpleInput, doColor);
                level1Factor = 2.0f;
            } else {
                simpleRefSub2 = simpleRef;
                simpleInputSub2 = simpleInput;
            }

            if ((simpleRefSub2.dataSize > subMinFactor) && (simpleInputSub2.dataSize > subMinFactor) && allowLevel4 &&
                    doSubsample) {
                simpleRefSub4 = subsampleBy2(simpleRefSub2, doColor);
                simpleInputSub4 = subsampleBy2(simpleInputSub2, doColor);
                level2Factor = 2.0f;
            } else {
                simpleRefSub4 = simpleRefSub2;
                simpleInputSub4 = simpleInputSub2;
            }

            if ((simpleRefSub4.dataSize > subMinFactor) && (simpleInputSub4.dataSize > subMinFactor) && allowLevel8 &&
                    doSubsample) {
                simpleRefSub8 = subsampleBy2(simpleRefSub4, doColor);
                simpleInputSub8 = subsampleBy2(simpleInputSub4, doColor);
                level4Factor = 2.0f;

                // For really big images subsample level 8 again!
                if ((simpleRefSub8.dataSize > subMinFactor) && (simpleInputSub8.dataSize > subMinFactor) &&
                        allowLevel16) {
                    ModelSimpleImage simpleRefSub16;
                    ModelSimpleImage simpleInputSub16;
                    Preferences.debug("Sub sampled level 8 to 16  *********** \n");

                    simpleRefSub16 = subsampleBy2(simpleRefSub8, doColor);
                    simpleInputSub16 = subsampleBy2(simpleInputSub8, doColor);

                    simpleRefSub8 = simpleRefSub16;
                    simpleInputSub8 = simpleInputSub16;
                    level4Factor = 4.0f;
                }
            } else {
                simpleRefSub8 = simpleRefSub4;
                simpleInputSub8 = simpleInputSub4;
            }

            Preferences.debug("Level 1 factor = " + level1Factor + "\n" + "Level 2 factor = " + level2Factor + "\n" +
                              "Level 4 factor = " + level4Factor + "\n" + "Ref subsampled 2 = " + simpleRefSub2 +
                              "Ref subsampled 4 = " + simpleRefSub4 + "Ref subsampled 8 = " + simpleRefSub8 +
                              "Input subsampled 2 = " + simpleInputSub2 + "Input subsampled 4 = " + simpleInputSub4 +
                              "Input subsampled 8 = " + simpleInputSub8);
        } // if (DOF >= 3)

        fireProgressStateChanged("Registering images", "Beginning registration");


        long time = System.currentTimeMillis();

        /* If the algorithm is to run in "Brute Force" mode, do so now: */
        if (m_bBruteForce == true) {
            algorithmBruteForce();

            return;
        }

        if (DOF >= 3) {
			Preferences
					.debug(" Starting level 8 ************************************************\n");

			Vector[] minimas = levelEight(simpleRefSub8, simpleInputSub8);
			time = System.currentTimeMillis() - time;
			Preferences.debug(" Level 8 min = " + ((float) time / 60000.0f)
					+ "\n");
			time = System.currentTimeMillis();

			if (threadStopped) {
				finalize();

				return;
			}

			Preferences
					.debug(" Starting level 4 ************************************************\n");

			Vector minima = levelFour(simpleRefSub4, simpleInputSub4,
					minimas[0], minimas[1]);
			time = System.currentTimeMillis() - time;
			Preferences.debug(" Level 4  min = " + ((float) time / 60000.0f)
					+ "\n");
			time = System.currentTimeMillis();

			if (threadStopped) {
				finalize();

				return;
			}

			Preferences
					.debug(" Starting level 2 ************************************************\n");

			MatrixListItem item = levelTwo(simpleRefSub2, simpleInputSub2,
					minima);
			time = System.currentTimeMillis() - time;
			Preferences.debug(" Level 2 min = " + ((float) time / 60000.0f)
					+ "\n");
			time = System.currentTimeMillis();

			if (threadStopped) {
				finalize();

				return;
			}

			Preferences.debug(" Starting level 1 ************************************************\n");
			answer = levelOne(simpleRef, simpleInput, item);
//			createTerrain();
		} // if (DOF >= 3)
        else {
            Preferences.debug(" Starting level 1 ************************************************\n");
            answer = levelOne2D(simpleRef, simpleInput);
        }

        time = System.currentTimeMillis() - time;
        Preferences.debug(" Level 1 min = " + ((float) time / 60000.0f) + "\n");
        time = System.currentTimeMillis();

        if (threadStopped) {
            finalize();

            return;
        }

        answer.matrix.invert();

        disposeLocal();
        finalize();
        setCompleted(true);
    }

    private void createTerrain(){
		searchOptimalPath();
		print(optimalPath, "Optimal Path: ");
		long startTime = System.nanoTime();
		AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(
				simpleRef, simpleInput, costChoice, 256, 1);
		maxIter = baseNumIter * 2;
		Point2Dd cog = calculateCenterOfMass2D(simpleInput,
				simpleWeightInput, doColor);
		double[] initial = new double[7];

		initial[0] = 0; // initial rotation
		initial[1] = 0; // initial translations
		initial[2] = 0;
		initial[3] = initial[4] = 1; // initial scaling
		initial[5] = initial[6] = 0; // initial skewing
		float xFrom = 17.2f;
		float xTo = 42.81f;
		float xStep = 0.2f;
		float yFrom = 27.2f;
		float yTo = 52.81f;
		float yStep = 0.2f;
		float zFrom = 7.2f;
		float zTo = 32.81f;
		float zStep = 0.2f;
		Vector<Point3f> realPath = param2Coordinates(xFrom, xTo, xStep,
				yFrom, yTo, yStep, zFrom, zTo, zStep, optimalPath);
		Vector<Point3i> imagePath = findPointsOfLine(realPath);
		print(imagePath, "Final Path:");
		Vector<Point3d> originalPath2 = new Vector<Point3d>(30);
		for(int j = 0; j < originalPath.length; j++){
			originalPath2.add(originalPath[j]);
		}
		Vector<Point3f> origRealPath = param2Coordinates(xFrom, xTo, xStep,
				yFrom, yTo, yStep, zFrom, zTo, zStep, originalPath2);
		Vector<Point3i> origImagePath = findPointsOfLine(origRealPath);
		print(origImagePath, "Original Final Path:");
		AlgorithmPowellOptBase powell = new AlgorithmPowellOpt2D(this, cog,
				DOF, cost, getTolerance(DOF), maxIter, rigidFlag,
				bracketBound);
		float[] terrain = powell.createTerrain(xFrom, xTo, xStep, yFrom,
				yTo, yStep, zFrom, zTo, zStep);
		final int xdim = (int) ((xTo - xFrom) / xStep);
		final int ydim = (int) ((yTo - yFrom) / yStep);
		final int zdim = (int) ((zTo - zFrom) / zStep);
		drawLine(xdim, ydim, zdim, terrain, 300f, origImagePath);
		drawLine(xdim, ydim, zdim, terrain, 500f, imagePath);
		ModelImage resultImage = new ModelImage(ModelImage.FLOAT,
				new int[] { xdim, ydim, zdim }, "LevelOne_Terrain");
		try {
			resultImage.importData(0, terrain, true);
		} catch (IOException e) {
			e.printStackTrace();
		}

		FileInfoBase[] fileInfos = resultImage.getFileInfo();
		for(int j = 0; j < fileInfos.length; j++){
			fileInfos[j].setResolutions(new float[] {xStep, yStep, zStep});
			fileInfos[j].setOrigin(new float[]{xFrom, yFrom, zFrom});
		}
		try {
			new ViewJFrameImage(resultImage, null,
					new Dimension(xdim, ydim));
		} catch (OutOfMemoryError error) {
			gov.nih.mipav.view.MipavUtil
					.displayError("Out of memory: unable to open new frame");
		}

		System.out.println("Time consumed by createTerrain(): "
				+ (System.nanoTime() - startTime));
    }
    
    private void searchOptimalPath(){
    	if(paths == null){
    		return;
    	}
    	optimalPath = new Vector<Point3d>(100);
    	optimalPath.addAll(paths[5].get(0));
    	optimalPath.addAll(paths[4].get(0));
    	int index = indexOf(paths[3], optimalPath.elementAt(0));
    	if(index < 0){
    		System.out.println("######3: Something is wrong, please double check! #######");
//    		return;
    	}else{
    		optimalPath.addAll(paths[3].get(index));
    	}
    	index = indexOf(paths[2], optimalPath.elementAt(0));
    	if(index < 0){
    		System.out.println("######2: Something is wrong, please double check! #######");
//    		return;
    	}else{
    		optimalPath.addAll(paths[2].get(index));
    	}
    	index = indexOf(paths[1], optimalPath.elementAt(0));
    	if(index < 0){
        	index = indexOf(paths[0], optimalPath.elementAt(0));
        	if(index < 0){
        		System.out.println("######1: Something is wrong, please double check! #######");
//        		return;
        	}else{
        		optimalPath.addAll(paths[0].get(index));
        	}
    	}else{
    		optimalPath.addAll(paths[1].get(index));
    	}
    }
    
    private int indexOf(Vector<Vector<Point3d>> aPaths, Point3d tp3d){
    	for(int i = 0; i < aPaths.size(); i++){
    		Vector<Point3d> aPath = aPaths.get(i);
    		if(tp3d.equals(aPath.get(aPath.size()-1))){
    			return i;
    		}
    	}
    	return -1;
    }
    public void print(Vector data, String message){
    	if(data == null || data.size() == 0){
    		return;
    	}
    	System.out.println(message + "**************");
    	for(int i = 0; i < data.size(); i++){
    		Object obj = data.get(i);
    		StringBuffer sb = new StringBuffer("");
    		if (obj instanceof MatrixListItem) {
				MatrixListItem item = (MatrixListItem) data.get(i);
				for (int j = 0; j < item.initial.length; j++) {
					sb.append(item.initial[j]);
					sb.append(",");
				}
			}else if(obj instanceof Point3d){
				Point3d p3d = (Point3d) data.get(i);
				sb.append(p3d.x);
				sb.append(",");
				sb.append(p3d.y);
				sb.append(",");
				sb.append(p3d.z);
			}else if(obj instanceof Point3i){
				Point3i p3i = (Point3i) data.get(i);
				sb.append(p3i.x);
				sb.append(",");
				sb.append(p3i.y);
				sb.append(",");
				sb.append(p3i.z);
			}
    		System.out.println(sb.toString());
    	}
    }
    
    public void print(MatrixListItem item, String message){
    	if(item == null){
    		return;
    	}
    	System.out.println(message + "**************");
		StringBuffer sb = new StringBuffer("");
		for(int j = 0; j < item.initial.length; j++){
			sb.append(item.initial[j]);
			sb.append(",");
		}
		System.out.println(sb.toString());
    }
    /**
     * setBruteForce. Tells the algorithm to do a brute-force optimization, where it will iterate of the the input
     * rotation, xscale, yscale, and translation ranges calculating the cost function at each point and returing the
     * minimum. No optimization with the brute-force approach.
     *
     * @param  bOn                DOCUMENT ME!
     * @param  fRotationRange     DOCUMENT ME!
     * @param  fXScaleRange       DOCUMENT ME!
     * @param  fYScaleRange       DOCUMENT ME!
     * @param  iScaleSteps        DOCUMENT ME!
     * @param  iTranslationRange  DOCUMENT ME!
     */
    public void setBruteForce(boolean bOn, float fRotationRange, float fXScaleRange, float fYScaleRange,
                              int iScaleSteps, int iTranslationRange) {
        m_bBruteForce = bOn;
        m_fRotationRange = fRotationRange;
        m_fXScaleRange = fXScaleRange;
        m_fYScaleRange = fYScaleRange;
        m_iTranslationRange = iTranslationRange;
        m_iScaleSteps = iScaleSteps;
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
        return srcImage.subSample2dBy2(isColor);
    }

    /**
     * Compute the brute-force solution. Iterates over a range of angles, scales and translations, calculating the cost
     * function for each new position, returning the one with minimum cost:
     */
    private void algorithmBruteForce() {

        /* Sets the cost function: */
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(simpleRef, simpleInput, costChoice, 32, 1);

        /* Currently, brute force is activated from the Mosaic window, so the
         * registration is always a weighted registration: */
        cost.setRefWgtImage(simpleWeightRef);
        cost.setInputWgtImage(simpleWeightInput);

        /* Set the center of gravity to be the center of the input image: */
        Point2Dd cog = new Point2Dd(simpleInput.xDim / 2, simpleInput.yDim / 2);

        /* Initialize the transform to the identity: */
        double[] initial = new double[7];
        initial[0] = 0; // initial rotation
        initial[1] = 0; // initial translations
        initial[2] = 0;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        AlgorithmPowellOptBase powell;
        maxIter = baseNumIter;
        powell = new AlgorithmPowellOpt2D(this, cog, 7, cost, getTolerance(7), maxIter, false, bracketBound);

        Vector<MatrixListItem> minima = new Vector<MatrixListItem>();

        /* Count the number of times the cost function is called: */
        int iBruteCount = 1;
        int iTotal = (int) (((m_iTranslationRange * 2) + 1) * ((m_iTranslationRange * 2) + 1) *
                                ((m_fRotationRange * 2) + 1));

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
        float fXScaleIncrement = (m_fXScaleRange * 2) / (m_iScaleSteps);
        float fYScaleIncrement = (m_fYScaleRange * 2) / (m_iScaleSteps);

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

        /* Test to see if any parameters are set, if all zero, return the
         * identity transform: */
        double dMinCost = Float.MAX_VALUE;
        double dCost = 0;

        if ((m_fRotationRange == 0) && (m_iTranslationRange == 0) &&
                (((m_fXScaleRange == 0) && (m_fYScaleRange == 0)) || (m_iScaleSteps == 0))) {
            bDone = true;

            /* initial must already be set to identity: */
            dCost = powell.measureCost(initial);
            minima.add(new MatrixListItem(dCost, powell.convertToMatrix(initial), initial));
            dMinCost = dCost;
        }

        /* Otherwise loop until all the permutations of rotations, scales, and
         * translations have been tested: */
        /* Note, this could have been several nested loops, however since any
         * of the loops may be executed 0 times (and therefor the inner loops never executed) the nested loops are
         * flattened into one. The progression is as follows: first angle is incremented, when angle reaches the max
         * value, then the translation in x is incremented by one and angle is reset to the initial value. Angle is
         * incremented again until it reaches max and then translation in x is incremented a second time. This repeats
         * until the translation in x is at max, when this occurs dx is reset and dy is incremented. And so on until all
         * ranges (rotation, x,y translation, x,y scale have been
         * maxed... */
        while (bDone == false) {

            /* Set progress bar: */
            fireProgressStateChanged((int) (100 * (float) iBruteCount / iTotal));

            /* test current permutation and store results: */
            initial[0] = angle;
            initial[1] = dx;
            initial[2] = dy;
            initial[3] = 1 / (1.0 + (xScale / 100f));
            initial[4] = 1 / (1.0 + (yScale / 100f));

            dCost = powell.measureCost(initial);
            minima.add(new MatrixListItem(dCost, powell.convertToMatrix(initial), initial));

            if (dCost < dMinCost) {
                dMinCost = dCost;
            }

            Preferences.debug("... testing cost " + iBruteCount + " angle = " + angle + " dx = " + dx + " dy = " + dy +
                              " scale x = " + xScale + " scale y = " + yScale + " ... cost = " + dCost +
                              " ... min cost sor far = " + dMinCost + "\n", Preferences.DEBUG_ALGORITHM);
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

            /* If we are optimizing over dx and angle is at max, increment dx
             * by one: */
            if ((m_iTranslationRange != 0) && bIncrementNext) {
                bIncrementNext = false;
                bDone = false;
                dx += 1;

                if (dx > m_iTranslationRange) {
                    dx = -m_iTranslationRange;
                    bIncrementNext = true;
                    bDone = true;
                }
            }

            /* If we are optimizing over dy and angle and dx are at max,
             * increment dy by one: */
            if ((m_iTranslationRange != 0) && bIncrementNext) {
                bIncrementNext = false;
                bDone = false;
                dy += 1;

                if (dy > m_iTranslationRange) {
                    dy = -m_iTranslationRange;
                    bIncrementNext = true;
                    bDone = true;
                }
            }

            /* If we are optimizing over scale x and angle, dx, dy are at max,
             * increment scale x by xscale increment: */
            if ((m_iScaleSteps != 0) && (m_fXScaleRange != 0) && bIncrementNext) {
                bIncrementNext = false;
                bDone = false;
                xScale += fXScaleIncrement;

                if (xScale > m_fXScaleRange) {
                    xScale = -m_fXScaleRange;
                    bIncrementNext = true;
                    bDone = true;
                }
            }

            /* If we are optimizing over scale y and angle, dx, dy, and scale
             * x are at max, increment scale y by yscale increment: */
            if ((m_iScaleSteps != 0) && (m_fYScaleRange != 0) && bIncrementNext) {
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
        answer = (MatrixListItem) minima.firstElement();
        answer.matrix.invert();
        Preferences.debug("Min transform:\n" + answer.matrix + " cost = " + answer.cost + " min cost = " + dMinCost +
                          "\n", Preferences.DEBUG_ALGORITHM);

        cost.disposeLocal();
        powell.disposeLocal();
        disposeLocal();
        finalize();
        setCompleted(true);
    }
   
    /**
     * Creates a string with the parameters that the image was constructed with.
     *
     * @return  Construction info.
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

        if (DOF == 2) {
            tols[0] = tols[1] = 0.5; // translations
        } else if ((DOF == 3) && (rigidFlag == true)) {
            tols[0] = ((180. / Math.PI) / maxDim); // rotation
            tols[1] = tols[2] = 0.5; // translations
        } else if ((DOF == 3) && (rigidFlag == false)) {
            tols[0] = 0.005; // global scaling
            tols[1] = tols[2] = 0.5; // translations
        } else if (DOF == 4) {
            tols[0] = ((180. / Math.PI) / maxDim); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = 0.005; // (1.0/maxDim); // scaling tolerances global
        } else if (DOF == 5) {
            tols[0] = ((180. / Math.PI) / maxDim); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = tols[4] = 0.005; // (1.0/maxDim); // scaling tolerance x
        } else if (DOF == 7) {
            tols[0] = ((180. / Math.PI) / maxDim); // rotation tolerances
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
     * @param  x          Initial index into array.
     * @param  initial    Vector to set; if scale is <code>true</code>, set two translations and a scale. Otherwise just
     *                    set translations.
     * @param  tranforms  DOCUMENT ME!
     * @param  scale      <code>true</code> means set the scale in the vector.
     */
    private void interpolate(double x, double[] initial, double[][] tranforms, boolean scale) {
        int ix0, ix1;

        // convert to closest integer values to access proper parts of array
        ix0 = (int) Math.floor(x);
        ix1 = ix0 + 1;

        // can't be bigger than 3
        if ((ix0 == (coarseNum - 1))) {
            ix1 = ix0;
        }

        if (scale) {

            // x translation
            initial[1] = ((x - ix0) * tranforms[ix1][1]) + ((1 - x + ix0) * tranforms[ix0][1]);

            // y translation
            initial[2] = ((x - ix0) * tranforms[ix1][2]) + ((1 - x + ix0) * tranforms[ix0][2]);

            // scale
            initial[3] = ((x - ix0) * tranforms[ix1][0]) + ((1 - x + ix0) * tranforms[ix0][0]);
        } else {

            // x translation
            initial[1] = ((x - ix0) * tranforms[ix1][0]) + ((1 - x + ix0) * tranforms[ix0][0]);

            // y translation
            initial[2] = ((x - ix0) * tranforms[ix1][1]) + ((1 - x + ix0) * tranforms[ix0][1]);
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
     * @param   ref    Subsampled by 8 reference image.
     * @param   input  Subsampled by 8 input image.
     *
     * @return  List of preoptimized and optimized points.
     */
    public Vector<MatrixListItem>[] levelEight(ModelSimpleImage ref, ModelSimpleImage input) {
        double factor;
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 32, 1);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub8);
            cost.setInputWgtImage(simpleWeightInputSub8);
        }

        Point2Dd cog = calculateCenterOfMass2D(input, simpleWeightInputSub8, doColor);
        Point2Dd cogR = calculateCenterOfMass2D(ref, simpleWeightRefSub8, doColor);

        Preferences.debug(" COG input image = " + cog + "\n");
        Preferences.debug(" COG ref   image = " + cogR + "\n");

        double diffX = (cog.x - cogR.x);
        double diffY = (cog.y - cogR.y);

        double[] initial = new double[7];

        initial[0] = 0; // initial rotation
        initial[1] = diffX; // initial translations
        initial[2] = diffY;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        double[][] transforms = new double[coarseNum][3];

        // Optimizing over translations and global scaling in AlgorithmPowellOpt2D for DOF = 3
        // Optimizing only over translations in AlgorithmPowellOpt2D for DOF = 2
        AlgorithmPowellOptBase powell;
        maxIter = baseNumIter;

        int dofs = 2;
        if (DOF > 3) {
            dofs = 3;
        }
        powell = new AlgorithmPowellOpt2D(this, cog, dofs, cost, getTolerance(dofs), maxIter, false, bracketBound);
        powell.setMultiThreadingEnabled(multiThreadingEnabled);

        // Should we even try to coarse since 2D images at level 8 are pretty small and not computionally taxing ?
        fireProgressStateChanged("Optimizing at coarse samples");

        Vectornd[] initials = new Vectornd[coarseNum];
        for (int i = 0; (i < coarseNum) && !threadStopped; i++) {
            fireProgressStateChanged((i + 1) * 10 / coarseNum);

            initial[0] = rotateBegin + (i * coarseRate);
            initials[i] = new Vectornd(initial, true);
        }

        powell.setPoints(initials);
        powell.run();
        for(int i = 0; i < coarseNum; i++){
            transforms[i] = powell.extractPoint(powell.getPoint(i));
        }

        if (threadStopped) {
            return null;
        }

        MatrixListItem[] matrixList = new MatrixListItem[fineNum];

        fireProgressStateChanged("Measuring at fine samples");

        double[] costs = new double[fineNum];
        int index = 0;

        for (int i = 0; (i < fineNum) && !threadStopped; i++) {
            fireProgressStateChanged(10 + ((i + 1) * 5 / fineNum));

            initial[0] = rotateBegin + (i * fineRate);

            // sets up translation and global scaling factors
            factor = (rotateBegin - rotateBegin + (i * fineRate)) / coarseRate;
            interpolate(factor, initial, transforms, (DOF > 3));
            initial[4] = initial[3];

            costs[index] = powell.measureCost(initial);
            matrixList[i] = new MatrixListItem(costs[index++], powell.convertToMatrix(initial), initial);
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

        powell.setPathRecorded(true);
        paths[0] = new Vector<Vector<Point3d>>(10);
        index = Arrays.binarySearch(costs, threshold);
        if(index < 0){
            index = -1 * (index + 1);
        }
        initials = new Vectornd[index];
        index = 0;
        for (int i = 0; (i < fineNum) && !threadStopped; i++) {
            fireProgressStateChanged(15 + ((i + 1) * 5 / fineNum));

            if (matrixList[i].cost < threshold) {
                initials[index++] = new Vectornd(matrixList[i].initial);
            }
        }
        powell.setPoints(initials);
        powell.run();
        index = 0;
        for(int i = 0; i < fineNum; i++){
            if (matrixList[i].cost < threshold) {
                matrixList[i] = new MatrixListItem(powell.getCost(index), powell.getMatrix(index), powell.getPoint(index));
                index++;
            }
//          paths[0].add(powell.getPath());
//          powell.clearPath();
        }


        if (threadStopped) {
            return null;
        }

        Vector<MatrixListItem> minima = new Vector<MatrixListItem>();

        for (int i = 0; i < fineNum; i++) {
            boolean minimum = true; // possible minimum

            for (int itest = -1; (itest <= 1) && minimum; itest += 2) { // as long as still possible minimum, check
                                                                        // neighbors one degree off

                if (((i + itest) >= 0) && ((i + itest) < fineNum)) {

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

        Preferences.debug("Number of minima: " + minima.size() + "\n");

        Vector<MatrixListItem> optMinima = new Vector<MatrixListItem>();
        // Now freely optimizes over rotation:

        fireProgressStateChanged("Optimizing minima");

        int degree = (DOF < 4) ? DOF : 4;
        maxIter = baseNumIter * 2;
        powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter, rigidFlag,
                                          bracketBound);
        powell.setMultiThreadingEnabled(multiThreadingEnabled);

        paths[1] = new Vector<Vector<Point3d>>(10);
        powell.setPathRecorded(true);
//        long startTime = System.nanoTime();
//        powell.createTerrain(-50f, 50f, 1f, -50f, 50f, 1f, -30f, 30f, 1f, "LevelEight_Terrain");
//        System.out.println("Time consumed by createTerrain(): " + (System.nanoTime()-startTime));

        MatrixListItem item;
        initials = new Vectornd[minima.size()];
        index = 0;
        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            initials[index++] = new Vectornd(en.nextElement().initial, true);
        }

        powell.setPoints(initials);
        powell.run();
        for(int i = 0; i < initials.length; i++){
            item = new MatrixListItem(powell.getCost(i), powell.getMatrix(i), powell.getPoint(i));
            optMinima.add(item);
//            paths[1].add(powell.getPath());
//            powell.clearPath();
        }

        if (threadStopped) {
            return null;
        }

        fireProgressStateChanged(25);

        cost.disposeLocal();
        powell.disposeLocal();

        return new Vector[] { minima, optMinima };
    }

    /**
     * Takes two images that have been subsampled by a factor of four, and two vectors of minima. Sets up the cost
     * function with the images and the weighted images, if necessary. Adds the level4Factor determined during
     * subsampling. Measures the costs of the minima on the images and sort them. Takes the top three in each vector
     * (pre-optimization and post-optimization) and optimizes them. Puts them all into one vector. Perturbs the rotation
     * by zero and plus-minus fineDelta. If it's not a rigid transformation, perturbs the global scaling by factors of
     * 0.8, 0.9, 1.0, 1.1, and 1.2. Optimize the perturbations. Returns a vector of the perturbed, optimized minima.
     *
     * @param   ref        Reference image, subsampled by 4.
     * @param   input      Input image, subsampled by 4.
     * @param   minima     Preoptimized minima.
     * @param   optMinima  Optimized minima.
     *
     * @return  A vector of perturbed, optimized minima.
     */
    public Vector<MatrixListItem> levelFour(ModelSimpleImage ref, ModelSimpleImage input, Vector<MatrixListItem> minima, Vector<MatrixListItem> optMinima) {
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 64, 1);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub4);
            cost.setInputWgtImage(simpleWeightInputSub4);
        }

        Point2Dd cog = calculateCenterOfMass2D(input, simpleWeightInputSub4, doColor);
        MatrixListItem item = null;

        // fix translations based on image resolutions!
        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[1] *= level4Factor;
            item.initial[2] *= level4Factor;
        }

        for (Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[1] *= level4Factor;
            item.initial[2] *= level4Factor;
        }

        int degree = (DOF < 4) ? DOF : 4;

        maxIter = baseNumIter * 2;

        AlgorithmPowellOptBase powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree),
                                                               maxIter, rigidFlag, bracketBound);
        powell.setMultiThreadingEnabled(multiThreadingEnabled);

//        long startTime = System.nanoTime();
//        powell.createTerrain(-50f, 50f, 1f, -50f, 50f, 1f, -30f, 30f, 1f, "LevelFour_Terrain");
//        System.out.println("Time consumed by createTerrain(): " + (System.nanoTime()-startTime));
        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = en.nextElement();
            item.cost = powell.measureCost(item.initial);
        }

        if (threadStopped) {
            return null;
        }

        for (Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements() && !threadStopped;) {
            item = en.nextElement();
            item.cost = powell.measureCost(item.initial);
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(minima);
        Collections.sort(optMinima);

        Vector<MatrixListItem> newMinima = new Vector<MatrixListItem>();

        fireProgressStateChanged("Optimizing new minima");

        // Now changed so that the number of minima to test at Level Four is a variable,
        // passed in from JDialog.  It used to be set to "3".
        int total = (numMinima < minima.size()) ? numMinima : minima.size();
        powell.setMaxIterations(baseNumIter);
        paths[2] = new Vector<Vector<Point3d>>(10);
        powell.setPathRecorded(true);
        Vectornd[] initials = new Vectornd[2*total];
        for (int i = 0; i < total; i++) {
            initials[i] = new Vectornd(minima.elementAt(i).initial);
            initials[i+total] = new Vectornd(optMinima.elementAt(i).initial);
        }
        powell.setPoints(initials);
        powell.run();
//        paths[2].add(powell.getPath());
//        powell.clearPath();

        if (threadStopped) {
            return null;
        }
        for(int i = 0;i < initials.length; i++){
            item = new MatrixListItem(powell.getCost(i), powell.getMatrix(i), powell.getPoint(i));
            newMinima.add(item);
        }

        Collections.sort(newMinima);

        double fineDelta = fineRate / 2.0;
        double[] initial;
        Vector<MatrixListItem> perturbList = new Vector<MatrixListItem>();

        fireProgressStateChanged("Perturbing minima");
        paths[3] = new Vector<Vector<Point3d>>(10);
        powell.setPathRecorded(true);
        if(DOF > 3){
            initials = new Vectornd[newMinima.size()*7];
        }else{
            initials = new Vectornd[newMinima.size()*3];
        }
        // Perturb rotation.  Add fine delta and optimize, then subtract fine delta and optimize.
        int index = 0;
        for (int j = 0; (j < 3) && !threadStopped; j++) {

            for (int i = 0; (i < (2 * total)) && !threadStopped; i++) {
                fireProgressStateChanged(29 + ((((j * 2 * total) + i + 1) * 3) / (total * 12)));

                initial = (double[]) ((MatrixListItem) newMinima.elementAt(i)).initial.clone();
                Preferences.debug("Perturbing initial[0] by ");

                if (j == 0) {
                    Preferences.debug("No fineDelta added or subtracted\n");
                }

                if (j == 1) {
                    initial[0] += fineDelta;
                    Preferences.debug("adding " + fineDelta + "\n");
                }

                if (j == 2) {
                    initial[0] -= fineDelta;
                    Preferences.debug("subtracting " + fineDelta + "\n");
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
                    fireProgressStateChanged(32 + ((((2 * j * total) + i + 1) * 3) / (total * 8)));

                    initial = (double[]) ((MatrixListItem) newMinima.elementAt(i)).initial.clone();

                    if (j == 1) {
                        scaleDelta = 0.9f;
                    } else if (j == 2) {
                        scaleDelta = 1.1f;
                    } else if (j == 3) {
                        scaleDelta = 1.2f;
                    } 

                    Preferences.debug("Perturbing initial[3] by ");
                    initial[3] *= scaleDelta;
                    Preferences.debug("Multiplying by " + scaleDelta + "\n");

                    // make initial variable old initial * scaleDelta in each dimension
                    initials[index++ ] = new Vectornd(initial, true);
                }
            }
        }

        powell.setPoints(initials);
        powell.run();
        
        for (int i = 0; i < initials.length; i++) {
            item = new MatrixListItem(powell.getCost(i), powell.getMatrix(i), powell.getPoint(i));
            perturbList.add(item);
//            paths[3].add(powell.getPath());
//            powell.clearPath();
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
        powell.disposeLocal();

        return perturbList;
    }

    /**
     * Takes the two images, no subsampling, and the best minimum so far. Sets up the cost function with the images and
     * the weighted images, if necessary. Adds the level1Factor determined during subsampling. Performs one optimization
     * run, with the maximum allowable degrees of freedom as specified by the user (the max is 7). Returns the best
     * minimum.
     *
     * @param   ref    Reference image.
     * @param   input  Input image.
     * @param   item   Best minimum so far.
     *
     * @return  Best minimum after optimization.
     */
    public MatrixListItem levelOne(ModelSimpleImage ref, ModelSimpleImage input, MatrixListItem item) {
    	int degree;
        AlgorithmPowellOptBase powell;
        MatrixListItem item2;
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 256, 1);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRef);
            cost.setInputWgtImage(simpleWeightInput);
        }

        Point2Dd cog = calculateCenterOfMass2D(input, simpleWeightInput, doColor);

        item.initial[1] *= level1Factor;
        item.initial[2] *= level1Factor;

        degree = DOF;

        fireProgressStateChanged("Starting last optimization");

        maxIter = baseNumIter * 2;
        powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(DOF), maxIter, rigidFlag,
                                          bracketBound);
        powell.setMultiThreadingEnabled(false);

        powell.setPathRecorded(true);
        paths[5] = new Vector<Vector<Point3d>>(1);
//      long startTime = System.nanoTime();
//      powell.createTerrain(-50f, 50f, 1f, -50f, 50f, 1f, -30f, 30f, 1f, "LevelOne_Terrain");
//      System.out.println("Time consumed by createTerrain(): " + (System.nanoTime()-startTime));
        linkProgressToAlgorithm(powell);
        powell.setProgressValues(generateProgressValues(60, 100));
        Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(item.initial);
        powell.setPoints(initials);
        powell.run();
//        paths[5].add(powell.getPath());
//        powell.clearPath();

        delinkProgressToAlgorithm(powell);
        if (threadStopped) {
            return null;
        }

        item2 = new MatrixListItem(powell.getCost(0), powell.getMatrix(0, input.xRes), powell.getPoint(0));
        Preferences.debug("Best answer: \n" + item2 + "\n");

        fireProgressStateChanged(100);

        cost.disposeLocal();
        powell.disposeLocal();

        return item2;
    }

    /**
     * Only used for translations only = 2 degrees of freedom levelEight, levelFour, and levelTwo are skipped Takes the
     * two images, no subsampling. Sets up the cost function with the images and the weighted images, Performs one
     * optimization run, with 2 degrees of freedom Returns the best minimum.
     *
     * @param   ref    Reference image.
     * @param   input  Input image.
     *
     * @return  Best minimum after optimization.
     */
    private MatrixListItem levelOne2D(ModelSimpleImage ref, ModelSimpleImage input) {
        int degree;
        AlgorithmPowellOptBase powell;
        MatrixListItem item2;
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 256, 1);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRef);
            cost.setInputWgtImage(simpleWeightInput);
        }

        Point2Dd cog = calculateCenterOfMass2D(input, simpleWeightInput, doColor);
        Point2Dd cogR = calculateCenterOfMass2D(ref, simpleWeightRef, doColor);

        Preferences.debug(" COG input image = " + cog + "\n");
        Preferences.debug(" COG ref   image = " + cogR + "\n");

        double diffX = (cog.x - cogR.x);
        double diffY = (cog.y - cogR.y);

        double[] initial = new double[7];

        initial[0] = 0; // initial rotation
        initial[1] = diffX; // initial translations
        initial[2] = diffY;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        degree = 2;

        fireProgressStateChanged("Starting last optimization");

        maxIter = baseNumIter * 2;
        powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(DOF), maxIter, rigidFlag,
                                          bracketBound);
        powell.setMultiThreadingEnabled(false);

        linkProgressToAlgorithm(powell);
        powell.setProgressValues(generateProgressValues(60, 100));
        Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(initial);
        powell.setPoints(initials);
        powell.run();
        delinkProgressToAlgorithm(powell);
        if (threadStopped) {
            return null;
        }

        item2 = new MatrixListItem(powell.getCost(0), powell.getMatrix(0, input.xRes), powell.getPoint(0));

        Preferences.debug("Best answer: \n" + item2 + "\n");

        fireProgressStateChanged(100);

        cost.disposeLocal();
        powell.disposeLocal();

        return item2;
    }

    /**
     * Takes two images that have been subsampled by a factor of 2 and a vector of minima. Sets up the cost function
     * with the images and the weighted images, if necessary. Adds the level2Factor determined during subsampling.
     * Measures the costs of the minima at the images. Optimizes the best minimum with 4 degrees of freedom, then 5,
     * then 7. If the user has limited the degrees of freedom to 3, there will only be one optimization run, with 3
     * degrees of freedom. Returns the best minimum after optimization.
     *
     * @param   ref     Reference image, subsampled by 2.
     * @param   input   Input image, subsampled by 2.
     * @param   minima  Minima.
     *
     * @return  The optimized minimum.
     */
    public MatrixListItem levelTwo(ModelSimpleImage ref, ModelSimpleImage input, Vector<MatrixListItem> minima) {
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 128, 1);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRefSub2);
            cost.setInputWgtImage(simpleWeightInputSub2);
        }

        Point2Dd cog = calculateCenterOfMass2D(input, simpleWeightInputSub2, doColor);
        MatrixListItem item = null;

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[1] *= level2Factor;
            item.initial[2] *= level2Factor;
        }

        int degree = (DOF < 4) ? DOF : 4;
        maxIter = baseNumIter;

        AlgorithmPowellOptBase powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter, rigidFlag, bracketBound);
        powell.setMultiThreadingEnabled(false);

        fireProgressStateChanged("Measuring costs of minima");

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.cost = powell.measureCost(item.initial);
        }

        Collections.sort(minima);

        fireProgressStateChanged("Optimizing with " + degree + " DOF");

        powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter,
                                          rigidFlag, bracketBound);
        powell.setMultiThreadingEnabled(false);

        powell.setPathRecorded(true);
        paths[4] = new Vector<Vector<Point3d>>(1);
//        long startTime = System.nanoTime();
//        powell.createTerrain(-50f, 50f, 1f, -50f, 50f, 1f, -30f, 30f, 1f, "LevelTwo_Terrain");
//        System.out.println("Time consumed by createTerrain(): " + (System.nanoTime()-startTime));
        linkProgressToAlgorithm(powell);
        powell.setProgressValues(generateProgressValues(35, 43));
        
        Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(minima.elementAt(0).initial);
        powell.setPoints(initials);
//        Preferences.debug("item for level2 powell run = " + (MatrixListItem) minima.elementAt(0) + "\n");
        powell.run();
        delinkProgressToAlgorithm(powell);
//        paths[4].add(powell.getPath());
//        powell.clearPath();

        if (threadStopped) {
            return null;
        }

        item = new MatrixListItem(powell.getCost(0), powell.getMatrix(0), powell.getPoint(0));
        Preferences.debug("Level 2, after " + degree + " DOF: " + item + "\n");

        if (DOF > 4) {
            degree = 5;

            fireProgressStateChanged("Optimizing with " + degree + " DOF");
            fireProgressStateChanged(43);

            powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter,
                                              rigidFlag, bracketBound);
            powell.setMultiThreadingEnabled(false);

            linkProgressToAlgorithm(powell);
            powell.setProgressValues(generateProgressValues(43, 51));
            powell.setPoints(initials);
            powell.run();
            delinkProgressToAlgorithm(powell);

            if (threadStopped) {
                return null;
            }

            item = new MatrixListItem(powell.getCost(0), powell.getMatrix(0), powell.getPoint(0));
            Preferences.debug("Level 2, after " + degree + " DOF: " + item + "\n");

            if (DOF > 5) {
                degree = (DOF < 7) ? DOF : 7;
                powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter,
                                                  rigidFlag, bracketBound);
                powell.setMultiThreadingEnabled(false);


                fireProgressStateChanged("Optimizing with 7 DOF");


                fireProgressStateChanged(51);

             
                linkProgressToAlgorithm(powell);
                powell.setProgressValues(generateProgressValues(51, 60));
                powell.setPoints(initials);

                powell.run();
                delinkProgressToAlgorithm(powell);

                if (threadStopped) {
                    return null;
                }

                item = new MatrixListItem(powell.getCost(0), powell.getMatrix(0), powell.getPoint(0));
                Preferences.debug("Level 2, after " + degree + " DOF: " + item + "\n");
            }
        }

        fireProgressStateChanged(60);

        cost.disposeLocal();
        powell.disposeLocal();

        return item;
    }

    public float getLevel1Factor() {
        return level1Factor;
    }

    public void setLevel1Factor(float level1Factor) {
        this.level1Factor = level1Factor;
    }

    public float getLevel2Factor() {
        return level2Factor;
    }

    public void setLevel2Factor(float level2Factor) {
        this.level2Factor = level2Factor;
    }

    public float getLevel4Factor() {
        return level4Factor;
    }

    public void setLevel4Factor(float level4Factor) {
        this.level4Factor = level4Factor;
    }

    public int getMaxDim() {
        return maxDim;
    }

    public void setMaxDim(int maxDim) {
        this.maxDim = maxDim;
    }
}
