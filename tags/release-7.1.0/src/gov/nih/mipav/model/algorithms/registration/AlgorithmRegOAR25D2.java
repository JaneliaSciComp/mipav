package gov.nih.mipav.model.algorithms.registration;

import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions2D;
import gov.nih.mipav.model.algorithms.AlgorithmPowellOpt2D;
import gov.nih.mipav.model.algorithms.AlgorithmPowellOptBase;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.Vectornd;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.TransMatrixd;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.awt.Color;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Vector;


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
 * <p>Internal registration is performed within one 3D image.</p>
 *
 * <p>In adjacent mode the first slice above the reference slice is registered to the reference slice, then the second
 * slice above the reference slice is registered to the first slice above the reference slice, and so on until the last
 * slice is registered the next to last slice. Then, the first slice below the reference slice is registered to the
 * reference slice, the second slice below the reference slice is registered to first slice below the reference slice,
 * and so on until the first slice is registered to the second slice. In reference mode every slice is simply registered
 * to the reference slice.</p>
 *
 * <p>Our algorithm works as follows:<br>
 * 1.) We find the minimum resolution of the images.<br>
 * 2.) We transform the images into isotropic voxels.<br>
 * 3.) We subsample the images by 2, 4, and 8 or 16, depending on the resolution.<br>
 * We loop thru a number of iterations equal to the number of slices - 1 in adjacent mode or equal to the number of
 * slices in reference mode with one slice as the input slice and one slice as the reference slice. For each iteration:
 * 4.) With the slices that were subsampled by 8 or 16, we call levelEight. This function will use the coarse sampling
 * rate and optimize translations and global scale at the given rotation. So for example, if the coarse sampling range
 * were -30 to 30 at every 15 degrees, we would optimize at rotations of -30, -15, 0, 15, 30.<br>
 * 5.) Still in levelEight, we now measure the cost at the fine sampling rate. We interpolate the translations and
 * global scale to come up with a good guess as to what the optimized translation would be at that point.<br>
 * 6.) We take the top 20% of the points and optimize them.<br>
 * 7.) We now have a large multi-array of costs. 20% of those have been optimized and placed back into their original
 * position in the multi-array. We look at the 3 neighbors of a point: +, =, or - one fine sample. If our point has a
 * cost greater than any of these, it is not a minima. Otherwise it is. We save it in a vector of minima.<br>
 * 8.) We optimize the minima over rotation as well as translations and global scale. (Previously we had not optimized
 * over rotation.) We return two vectors, one containing the minima before optimization, one containing the minima after
 * optimization.<br>
 * 9.) We now call levelFour with the slices subsampled by 4 and the vectors of minima. We measure the costs of the
 * minima on the new slices and sort them. We take the top numMinima in each vector (pre-optimization and
 * post-optimization) and optimize them. We put them all into one vector.<br>
 * 10.) We perturb the rotation by zero and by plus-minus fineDelta. If it's not a rigid transformation, we then perturb
 * the global scaling by factors of 0.8, 0.9, 1.0, 1.1, and 1.2.<br>
 * 11.) We optimize the perturbations. We return a vector of the perturbed, optimized minima.<br>
 * 12.) We now call levelTwo with the slices subsampled by 2. We measure the costs of the minima at the new slices. We
 * optimize the best minimum with 4 degrees of freedom, then 5, then 6. If the user has limited the degrees of freedom
 * to 3, there will only be one optimization run, with 3 degrees of freedom. The function returns the best minimum after
 * optimization.<br>
 * 13.) We call levelOne with the un-subsampled slices. At levelOne, one optimization run is performed, with the maximum
 * allowable degrees of freedom, as specified by the user (the max is 6).<br>
 * 14.) The best answer is returned from levelOne. The inverse of the matrix from this answer is used to register the
 * input slice to the reference slice. The registered input slice data is imported into inputImage.<br>
 * </p>
 *
 * <p>Note that when 6 degrees of freedom is used the rotation is set equal to 0 because diffX sets (0,2), diffY sets
 * (1,2), scaleX sets (0,0), scaleY sets (1,1), skewX sets (0,1), and skewY sets (1,0) so all 6 elements are set.</p>
 *
 * @author  Matthew McAuliffe
 * @author  Neva Cherniavsky
 * @author  Benjamin Link - Added code to use less memory. (April 2003)
 */
public class AlgorithmRegOAR25D2 extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean allowLevel16 = true;

    /**
     * Flags are true if weighted image is not present or if weighted image subsampling occurred, false if weighted
     * image subsampling did not occur.
     */
    private boolean allowLevel2 = true;

    /** DOCUMENT ME! */
    private boolean allowLevel4 = true;

    /** DOCUMENT ME! */
    private boolean allowLevel8 = true;

    /** Final answer after registration. */
    private MatrixListItem answer;

    /** DOCUMENT ME! */
    private float[] buffer;

    /** DOCUMENT ME! */
    private float[] bufferA;

    /** DOCUMENT ME! */
    private float[] bufferIW;

    /** DOCUMENT ME! */
    private float[] bufferW;

    /** Number of passes that will be made in the coarse sampling and fine sampling. */
    private int coarseNum, fineNum;

    /** 1 for black and white, 4 for color. */
    private int colorFactor;

    /** Choice of which cost function to use. */
    private int costChoice;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** Maximum degrees of freedom when running the optimization. */
    private int DOF;

    /**
     * Produce 2 output graphs - 1 for rotation and 1 for 2 translations. Only can be used for DOF == 3 and register to
     * reference slice
     */
    private boolean doGraph;

    /** if true subsample. */
    private boolean doSubsample;

    /** whether or not to use center of gravity for first translation. */
    private boolean ignoreCOG = false;

    /** DOCUMENT ME! */
    private ModelImage imageWeightIso = null;

    /** Image used to import a slice from inputImage. */
    private ModelImage input1;

    /** Image used to import a slice from inputImage2. */
    private ModelImage input2;

    /** Image used to import a slice from inputImage3. */
    private ModelImage input3;

    /** This is the image in which internal registration will be performed. */
    private ModelImage inputImage;

    /**
     * Other images which will not determine the registration, but which will undergo the same transformations as input
     * image. Useful if registering a color image based on only 1 of the colors
     */
    private ModelImage inputImage2 = null;

    /** DOCUMENT ME! */
    private ModelImage inputImage3 = null;

    /** Image used to import a slice from inputWeight. */
    private ModelImage inputw_1;

    /**
     * This gives weights for the input image - higher weights mean a greater impact in that area on the registration.
     */
    private ModelImage inputWeight;

    /** Interpolation method used in transformations. */
    private int interp;

    /** Interpolation method used in output. */
    private int interp2;

    /** The voxel resolutions of the input image. */
    private float[] iResols;

    /** new to this version: must keep isoImage around if image needed to be transformed. */
    private ModelImage isoImage = null;

    /** Multiplication factor for level 1 - will be set based on subsampling. */
    private float level1Factor = 1.0f;

    /** Multiplication factor for level 2 - will be set based on subsampling. */
    private float level2Factor = 1.0f;

    /** Multiplication factor for level 4 - will be set based on subsampling. */
    private float level4Factor = 1.0f;

    /** DOCUMENT ME! */
    private float maxDim = 256;

    /**
     * Limits number of iterations in Powell optimization. maxIter in the call to Powell's will be an integer multiple
     * of baseNumIter
     */
    private int maxIter, baseNumIter;

    /** Number of minima from level 8 to test at level 4. */
    private int numMinima;

    /** DOCUMENT ME! */
    private ModelImage output_1;

    /** DOCUMENT ME! */
    private ModelImage output_2;

    /** DOCUMENT ME! */
    private ModelImage output_3;

    /** DOCUMENT ME! */
    private ModelImage outsidePreReferenceSlice = null;

    /** DOCUMENT ME! */
    private ModelImage outsideReferenceSlice = null;

    /** Indicates the image slice that all images are to be registered to, if regToAdjImage is false. */

    /** Indicates the first slice used as a reference slice, if regToAdjImage is true. */
    private int refImageNo;

    /**
     * If true use adjacent image for registration. If false use image slice reference number to indicate the image
     * slice to register all images to.
     */
    private boolean regToAdjImage;

    /** true if resolutions unequal, false if resolutions equal. */
    private boolean resample;

    /** true if weight image must be resampled. */
    private boolean resampleW;

    /** Flag used to indicate if the registration is ridgid (rotation and translation - DOF = 3. */
    private boolean rigidFlag = false;

    /** Arrays used for producing graphs for DOF == 3 and register to reference image. */
    private float[] rot = null;

    /** Coarse and fine sampling parameters. */
    private float rotateBegin, rotateEnd, coarseRate, fineRate;

    /** Simple version of an image slice of the input image. */
    private ModelSimpleImage simpleInput_1;

    /** Simple version of an image slice of the input image, subsampled by 16. */
    private ModelSimpleImage simpleInputSub16_1;

    /** Simple version of an image slice of the input image, subsampled by 2. */
    private ModelSimpleImage simpleInputSub2_1;

    /** Simple version of an image slice of the input image, subsampled by 4. */
    private ModelSimpleImage simpleInputSub4_1;

    /** Simple version of an image slice of the input image, subsampled by 8. */
    private ModelSimpleImage simpleInputSub8_1;

    /** Simple version of input image. */
    private ModelSimpleImage simpleRef_1;

    /** Simple version of input image, subsampled by 16. */
    private ModelSimpleImage simpleRefSub16_1;

    /** Simple version of input image, subsampled by 2. */
    private ModelSimpleImage simpleRefSub2_1;

    /** Simple version of input image, subsampled by 4. */
    private ModelSimpleImage simpleRefSub4_1;

    /** Simple version of input image, subsampled by 8. */
    private ModelSimpleImage simpleRefSub8_1;

    /** DOCUMENT ME! */
    private ModelSimpleImage simpleWeightI_1;

    /** DOCUMENT ME! */
    private ModelSimpleImage simpleWeightISub16_1;

    /** DOCUMENT ME! */
    private ModelSimpleImage simpleWeightISub2_1;

    /** DOCUMENT ME! */
    private ModelSimpleImage simpleWeightISub4_1;

    /** DOCUMENT ME! */
    private ModelSimpleImage simpleWeightISub8_1;

    /** Simple version of weighted input image. */
    private ModelSimpleImage simpleWeightR_1;

    /** Simple version of weighted input image, subsampled by 16. */
    private ModelSimpleImage simpleWeightRSub16_1;

    /** Simple version of weighted input image, subsampled by 2. */
    private ModelSimpleImage simpleWeightRSub2_1;

    /** Simple version of weighted input image, subsampled by 4. */
    private ModelSimpleImage simpleWeightRSub4_1;

    /** Simple version of weighted input image, subsampled by 8. */
    private ModelSimpleImage simpleWeightRSub8_1;


    /** DOCUMENT ME! */
    private double[] sliceCosts = null;

    /** DOCUMENT ME! */
    private float[][] trans = null;

    /** if true transform VOIs. */
    private boolean transformVOIs;

    /** DOCUMENT ME! */
    private boolean useOutsideReferenceSlice = false;

    /** Flag to determine if there are weighted images or not. */
    private boolean weighted;

    /** DOCUMENT ME! */
    private ModelImage weightSliceImage = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new automatic internal registration algorithm and sets necessary variables.
     *
     * @param  _image         Input image
     * @param  _costChoice    Choice of cost functions, like correlation ratio or mutual information.
     * @param  _DOF           Degrees of freedom for registration
     * @param  _interp        Interpolation method used in transformations.
     * @param  _interp2       Interpolation method used in output
     * @param  mode           If true, register to adjacent. If false, register to reference.
     * @param  refImageNum    If register to reference, the slice all other slices are registered to. If register to
     *                        adjacent, the first slice used as a reference.
     * @param  _rotateBegin   Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEnd     End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRate    Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRate      Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  doGraph        If true produce 2 output graphs - one for rotation and one for 2 translations
     * @param  doSubsample    If true subsample
     * @param  transformVOIs  If true, transform VOIs
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     */
    public AlgorithmRegOAR25D2(ModelImage _image, int _costChoice, int _DOF, int _interp, int _interp2, boolean mode,
                               int refImageNum, float _rotateBegin, float _rotateEnd, float _coarseRate,
                               float _fineRate, boolean doGraph, boolean doSubsample, boolean transformVOIs,
                               int _baseNumIter, int _numMinima) {
        super(null, _image);
        inputImage = _image;

        if (inputImage.isColorImage()) {
            doColor = true;
            colorFactor = 4;
        } else {
            doColor = false;
            colorFactor = 1;
        }

        regToAdjImage = mode;
        refImageNo = refImageNum;
        costChoice = _costChoice;
        DOF = _DOF;

        if (DOF == 3) {
            rigidFlag = true;
        }

        if (DOF == 6) {
            DOF = 7; // use 2 shears
        }

        answer = null;

        interp = _interp;
        interp2 = _interp2;
        rotateBegin = _rotateBegin;
        rotateEnd = _rotateEnd;
        coarseRate = _coarseRate;
        fineRate = _fineRate;
        coarseNum = (int) ((_rotateEnd - rotateBegin) / coarseRate) + 1;
        fineNum = (int) ((_rotateEnd - rotateBegin) / fineRate) + 1;
        weighted = false;
        iResols = inputImage.getFileInfo(0).getResolutions();
        this.doGraph = doGraph;
        this.doSubsample = doSubsample;
        this.transformVOIs = transformVOIs;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;

        maxDim = inputImage.getExtents()[0];

        if (inputImage.getExtents()[1] > maxDim) {
            maxDim = inputImage.getExtents()[1];
        }

        sliceCosts = new double[inputImage.getExtents()[2]];
    }

    /**
     * Creates new automatic internal registration algorithm and sets necessary variables.
     *
     * @param  _image         Input image
     * @param  _inputWeight   Input weighted image, used to give certain areas of the image greater impact on the
     *                        registration.
     * @param  _costChoice    Choice of cost functions, like correlation ratio or mutual information.
     * @param  _DOF           DOCUMENT ME!
     * @param  _interp        Interpolation method used in transformations.
     * @param  _interp2       Interpolation method used in output
     * @param  mode           If true, register to adjacent. If false, register to reference.
     * @param  refImageNum    If register to reference, the slice all other slices are registered to. If register to
     *                        adjacent, the first slice used as a reference.
     * @param  _rotateBegin   Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEnd     End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRate    Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRate      Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  doGraph        If true produce 2 output graphs - 1 for rotation and one for 2 translations
     * @param  doSubsample    If true subsample
     * @param  transformVOIs  If true, transform VOIs
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     */
    public AlgorithmRegOAR25D2(ModelImage _image, ModelImage _inputWeight, int _costChoice, int _DOF, int _interp,
                               int _interp2, boolean mode, int refImageNum, float _rotateBegin, float _rotateEnd,
                               float _coarseRate, float _fineRate, boolean doGraph, boolean doSubsample,
                               boolean transformVOIs, int _baseNumIter, int _numMinima) {

        super(null, _image);

        inputImage = _image;

        if (inputImage.isColorImage()) {
            doColor = true;
            colorFactor = 4;
        } else {
            doColor = false;
            colorFactor = 1;
        }

        regToAdjImage = mode;
        refImageNo = refImageNum;
        inputWeight = _inputWeight;
        costChoice = _costChoice;
        DOF = _DOF;

        if (DOF == 3) {
            rigidFlag = true;
        }

        if (DOF == 6) {
            DOF = 7; // use 2 shears
        }

        answer = null;

        interp = _interp;
        interp2 = _interp2;
        rotateBegin = _rotateBegin;
        rotateEnd = _rotateEnd;
        coarseRate = _coarseRate;
        fineRate = _fineRate;
        coarseNum = (int) ((_rotateEnd - rotateBegin) / coarseRate) + 1;
        fineNum = (int) ((_rotateEnd - rotateBegin) / fineRate) + 1;
        weighted = true;
        iResols = inputImage.getFileInfo(0).getResolutions();
        this.doGraph = doGraph;
        this.doSubsample = doSubsample;
        this.transformVOIs = transformVOIs;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;

        maxDim = inputImage.getExtents()[0];

        if (inputImage.getExtents()[1] > maxDim) {
            maxDim = inputImage.getExtents()[1];
        }

        sliceCosts = new double[inputImage.getExtents()[2]];
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
    public Vector2f calculateCenterOfMass2D(ModelSimpleImage image, ModelSimpleImage wgtImage, boolean isColor) {
        int x, y, c;
        float diff;

        Vector2f cogPt = new Vector2f(0, 0);

        double voxVal = 0.0, total = 0.0, wgtVal = 0.0;

        if (isColor) {

            if (wgtImage == null) {

                for (y = 0; y < image.yDim; y++) {

                    for (x = 0; x < image.xDim; x++) {

                        for (c = 1; c <= 3; c++) {
                            voxVal = image.data[(4 * ((y * image.xDim) + x)) + c];
                            cogPt.X += voxVal * x;
                            cogPt.Y += voxVal * y;
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
                        voxVal = image.data[(y * image.xDim) + x] - image.min;
                        cogPt.X += voxVal * x;
                        cogPt.Y += voxVal * y;
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

        // System.err.println("calling disposeLocal in algoRegOAR25D2");

        if (simpleInput_1 != null) {
            simpleInput_1.disposeLocal(false);
        }

        if (simpleInputSub2_1 != null) {
            simpleInputSub2_1.disposeLocal(false);
        }

        if (simpleInputSub4_1 != null) {
            simpleInputSub4_1.disposeLocal(false);
        }

        if (simpleInputSub8_1 != null) {
            simpleInputSub8_1.disposeLocal(false);
        }

        if (simpleInputSub16_1 != null) {
            simpleInputSub16_1.disposeLocal(false);
        }

        if (simpleRef_1 != null) {
            simpleRef_1.disposeLocal(false);
        }

        if (simpleRefSub2_1 != null) {
            simpleRefSub2_1.disposeLocal(false);
        }

        if (simpleRefSub4_1 != null) {
            simpleRefSub4_1.disposeLocal(false);
        }

        if (simpleRefSub8_1 != null) {
            simpleRefSub8_1.disposeLocal(false);
        }

        if (simpleRefSub16_1 != null) {
            simpleRefSub16_1.disposeLocal(false);
        }

        if (simpleWeightR_1 != null) {
            simpleWeightR_1.disposeLocal(false);
        }

        if (simpleWeightRSub2_1 != null) {
            simpleWeightRSub2_1.disposeLocal(false);
        }

        if (simpleWeightRSub4_1 != null) {
            simpleWeightRSub4_1.disposeLocal(false);
        }

        if (simpleWeightRSub8_1 != null) {
            simpleWeightRSub8_1.disposeLocal(false);
        }

        if (simpleWeightRSub16_1 != null) {
            simpleWeightRSub16_1.disposeLocal(false);
        }

        if (simpleWeightI_1 != null) {
            simpleWeightI_1.disposeLocal(false);
        }

        if (simpleWeightISub2_1 != null) {
            simpleWeightISub2_1.disposeLocal(false);
        }

        if (simpleWeightISub4_1 != null) {
            simpleWeightISub4_1.disposeLocal(false);
        }

        if (simpleWeightISub8_1 != null) {
            simpleWeightISub8_1.disposeLocal(false);
        }

        if (simpleWeightISub16_1 != null) {
            simpleWeightISub16_1.disposeLocal(false);
        }

        if (input1 != null) {
            input1.disposeLocal();
        }
        
        if (input2 != null) {
            input2.disposeLocal();
        }
        
        if (input3 != null) {
            input3.disposeLocal();
        }
        
        if (inputImage2 != null) {
            inputImage2.disposeLocal();
        }
        
        if (inputImage3 != null) {
            inputImage3.disposeLocal();
        }

        if (inputw_1 != null) {
            inputw_1.disposeLocal();
        }
        
        if (inputWeight != null) {
            inputWeight.disposeLocal();
        }

        if (output_1 != null) {
            output_1.disposeLocal();
        }
        
        if (output_2 != null) {
            output_2.disposeLocal();
        }
        
        if (output_3 != null) {
            output_3.disposeLocal();
        }

        if (weightSliceImage != null) {
            weightSliceImage.disposeLocal();
        }

        if (isoImage != null) {
            isoImage.disposeLocal();
        }

        if (imageWeightIso != null) {
            imageWeightIso.disposeLocal();
        }
        
        if (outsidePreReferenceSlice != null) {
            outsidePreReferenceSlice.disposeLocal();
        }
        
        if (outsideReferenceSlice != null) {
            outsideReferenceSlice.disposeLocal();
        }

        simpleInput_1 = null;
        simpleInputSub2_1 = null;
        simpleInputSub4_1 = null;
        simpleInputSub8_1 = null;
        simpleInputSub16_1 = null;

        simpleRef_1 = null;
        simpleRefSub2_1 = null;
        simpleRefSub4_1 = null;
        simpleRefSub8_1 = null;
        simpleRefSub16_1 = null;

        simpleWeightR_1 = null;
        simpleWeightRSub2_1 = null;
        simpleWeightRSub4_1 = null;
        simpleWeightRSub8_1 = null;
        simpleWeightRSub16_1 = null;

        simpleWeightI_1 = null;
        simpleWeightISub2_1 = null;
        simpleWeightISub4_1 = null;
        simpleWeightISub8_1 = null;
        simpleWeightISub16_1 = null;

        input1 = null;
        input2 = null;
        input3 = null;
        inputImage2 = null;
        inputImage3 = null;
        inputw_1 = null;
        inputWeight = null;
        output_1 = null;
        output_2 = null;
        output_3 = null;
        weightSliceImage = null;
        isoImage = null;
        outsidePreReferenceSlice = null;
        outsideReferenceSlice = null;

        inputImage = null;
        buffer = null;
        bufferIW = null;
        bufferW = null;
        bufferA = null;
        iResols = null;
        rot = null;
        sliceCosts = null;
        if (trans != null) {
            for (int i = 0; i < trans.length; i++) {
                trans[i] = null;
            }
            trans = null;
        }

        answer = null;

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
     * accessor for costs.
     *
     * @return  double[] costs
     */
    public double[] getCosts() {
        return this.sliceCosts;
    }

    /**
     * accessor for rot.
     *
     * @return  rot
     */
    public float[] getRot() {
        return rot;
    }

    /**
     * accessor for trans.
     *
     * @return  trans
     */
    public float[][] getTrans() {
        return trans;
    }

    /**
     * accessor to get the internally registered image.
     *
     * @return  inputImage
     */
    public ModelImage getTransformedImage() {
        return inputImage;
    }

    /**
     * Runs the image registration. If the resolutions are unequal, the image is transformed into isotropic pixels. The
     * resolutions of the two images after the xy isotropic transformation will be the same in the x and y dimensions.
     * That resolution will equal the minimum resolution. If the image is weighted, the weight image is transformed into
     * isotropic pixels in the same manner as the original. Then the image is subsampled by 2, 4, and 8 or 16. If the
     * image is too small it will not be subsampled down to the smallest level; if it is too big, it will be subsampled
     * to 16. The same is done with the weight image if necessary. The program loops thru levelEight, levelFour,
     * levelTwo, and levelOne with one slice as the reference slice and one slice as the input slice. The function
     * levelEight is called with the slices subsampled by 8 or 16; it returns two vectors with minima. Then the function
     * levelFour is called with slices subsampled by 4 and the two vectors; it returns one vector of minima. The
     * function levelTwo is called with slices subsampled by 2 and the vector; it returns an "answer" in the form of a
     * MatrixListItem, which is a convenient way of storing the point, the matrix, and the cost of the minimum. Then the
     * function levelOne is called with the minimum; it returns a final "answer", or minimum, which is used to register
     * the input slice to the reference slice. The registered input slice data is imported into inputImage.
     */
    public void runAlgorithm() {
        int i, j;
        int iNumber;
        int rNumber;
        boolean seriesUp;
        int[] extents = new int[2];
        int[] iExtents = new int[2];
        float[] resols = new float[2];
        float[] distance = new float[2];
        AlgorithmTransform transform = null;
        int[] extentsIso = null;
        float[] resIso = null;
        int sliceSize;
        //double[][] OARmat;
        VOIVector VOIs = null;
        VOIVector VOI2s = null;
        int nVOIs = 0;
        VOI[] newVOI = null;
        short[] id = null;
        String[] name = null;
        int[] curveType = null;
        float[] presetHue = null;
        float[] hsb = null;

        if (inputImage.getNDims() != 3) {
            MipavUtil.displayError("" + inputImage.getNDims() + "D registration not supported.");
            disposeLocal();
            notifyListeners(this);

            return;
        }

        
        Preferences.debug(getConstructionInfo(),Preferences.DEBUG_ALGORITHM);

        fireProgressStateChanged("Registering images", "Beginning registration");


        if (doGraph) {

            if (DOF == 3) {
                rot = new float[inputImage.getExtents()[2]];
                rot[refImageNo] = 0.0f;
            }

            trans = new float[2][inputImage.getExtents()[2]];
            trans[0][refImageNo] = 0.0f;
            trans[1][refImageNo] = 0.0f;
        } // if (doGraph)

        if (transformVOIs) {
            VOIs = inputImage.getVOIs();
            nVOIs = VOIs.size();
            newVOI = new VOI[nVOIs];
            VOI2s = new VOIVector();
            id = new short[nVOIs];
            name = new String[nVOIs];
            curveType = new int[nVOIs];
            presetHue = new float[nVOIs];

            for (i = 0; i < nVOIs; i++) {
                id[i] = VOIs.VOIAt(i).getID();
                name[i] = VOIs.VOIAt(i).getName();
                curveType[i] = VOIs.VOIAt(i).getCurveType();
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                presetHue[i] = hsb[0];
            }
        }

        float minSample = Math.min(iResols[0], iResols[1]);

        try {
            extentsIso = new int[3];
            resIso = new float[3];
            buffer = new float[colorFactor * inputImage.getSliceSize()];
        } catch (OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("Out of memory in AlgorithmOAR25D creating extentsIso and resIso and buffer");
            disposeLocal();
            notifyListeners(this);

            return;
        }

        for (i = 0; i < 2; i++) {
            extentsIso[i] = (int) Math.round(((inputImage.getExtents()[i] - 1) / (minSample / iResols[i])) + 1);
            resIso[i] = minSample;
        }

        extentsIso[2] = inputImage.getExtents()[2];
        resIso[2] = iResols[2];

        if (weighted) {

            try {
                bufferIW = new float[inputWeight.getSliceSize()];
            } catch (OutOfMemoryError e) {
                System.gc();
                MipavUtil.displayError("Out of memory in AlgorithmOAR25D creating bufferIW");
                disposeLocal();
                notifyListeners(this);

                return;
            }

            try {
                bufferW = new float[extentsIso[0] * extentsIso[1]];
            } catch (OutOfMemoryError e) {
                System.gc();
                MipavUtil.displayError("Out of memory in AlgorithmOAR25D creating bufferW");
                disposeLocal();
                notifyListeners(this);

                return;
            }

        } // if (weighted)

        if (iResols[0] == iResols[1]) {
            resample = false;

            // must set extents & resolutions here
            extents[0] = inputImage.getExtents()[0];
            extents[1] = inputImage.getExtents()[1];
            resols[0] = inputImage.getFileInfo()[0].getResolutions()[0];
            resols[1] = inputImage.getFileInfo()[0].getResolutions()[1];
        } else {
            resample = true;

            // 2.5D interpolation
            fireProgressStateChanged("Interpolating input image to obtain equal x and y resolutions");
            transform = new AlgorithmTransform(inputImage, new TransMatrix(3), interp, resIso[0], resIso[1],
                                               extentsIso[0], extentsIso[1], false, true, false);
            transform.run();

            if (transform.isCompleted() == false) {
                setCompleted(false);
                finalize();
                notifyListeners(this);

                return;
            }

            isoImage = transform.getTransformedImage();

            if (transform != null) {
                transform.finalize();
            }

            // must set extents & resolutions here
            extents[0] = isoImage.getExtents()[0];
            extents[1] = isoImage.getExtents()[1];
            resols[0] = isoImage.getFileInfo()[0].getResolutions()[0];
            resols[1] = isoImage.getFileInfo()[0].getResolutions()[1];

        } // res[0] != res[1]

        iExtents[0] = inputImage.getExtents()[0];
        iExtents[1] = inputImage.getExtents()[1];
        sliceSize = iExtents[0] * iExtents[1];

        // still use simpleInput_1 (1 slice at a time)
        simpleInput_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image

        // check if outside reference slice and resampling are both used
        if (useOutsideReferenceSlice && resample) {
            transform = new AlgorithmTransform(outsidePreReferenceSlice, new TransMatrix(3), interp, resIso[0],
                                               resIso[1], extentsIso[0], extentsIso[1], false, true, false);
            transform.run();

            if (transform.isCompleted() == false) {
                setCompleted(false);
                finalize();
                notifyListeners(this);

                return;
            }

            outsideReferenceSlice = transform.getTransformedImage();

            if (transform != null) {
                transform.finalize();
            }
        } // if (useOutsideReferenceSlice && resample)
        else if (useOutsideReferenceSlice) {
            outsideReferenceSlice = outsidePreReferenceSlice;
        }

        simpleRef_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image

        if (doColor) {
            input1 = new ModelImage(ModelImage.ARGB_FLOAT, iExtents, "slice2D");

            if (inputImage2 != null) {
                input2 = new ModelImage(ModelImage.ARGB_FLOAT, iExtents, "slice2DI2");
            }

            if (inputImage3 != null) {
                input3 = new ModelImage(ModelImage.ARGB_FLOAT, iExtents, "slice2DI3");
            }
        } else {
            input1 = new ModelImage(ModelImage.FLOAT, iExtents, "slice2D");

            if (inputImage2 != null) {
                input2 = new ModelImage(ModelImage.FLOAT, iExtents, "slice2DI2");
            }

            if (inputImage3 != null) {
                input3 = new ModelImage(ModelImage.FLOAT, iExtents, "slice2DI3");
            }
        }

        input1.getFileInfo()[0].setResolutions(iResols);

        if (inputImage2 != null) {
            input2.getFileInfo()[0].setResolutions(iResols);
        }

        if (inputImage3 != null) {
            input3.getFileInfo()[0].setResolutions(iResols);
        }

        if (weighted) {
            inputw_1 = new ModelImage(ModelImage.FLOAT, iExtents, "slice2DW");
            inputw_1.getFileInfo()[0].setResolutions(iResols);
        }

        if (resample && regToAdjImage) {

            try {
                bufferA = new float[colorFactor * extentsIso[0] * extentsIso[1]];
            } catch (OutOfMemoryError e) {
                System.gc();
                MipavUtil.displayError("Out of memory in AlgorithmOAR25D creating bufferA");
                disposeLocal();
                notifyListeners(this);

                return;
            }

        } // if (resample && regToAdjImage)

        int subMinFactor = 15000;

        if (weighted) {

            if (resample || (inputWeight.getFileInfo(0).getResolutions()[0] != iResols[0]) ||
                    (inputWeight.getFileInfo(0).getResolutions()[1] != iResols[1])) {

                // 2.5 interpolation
                resampleW = true;
                fireProgressStateChanged("Performing interpolation on input weight image");
                transform = new AlgorithmTransform(inputWeight, new TransMatrix(3), interp, resIso[0], resIso[1],
                                                   extentsIso[0], extentsIso[1], false, true, false);
                transform.run();

                if (transform.isCompleted() == false) {
                    setCompleted(false);
                    finalize();
                    notifyListeners(this);

                    return;
                }

                imageWeightIso = transform.getTransformedImage();

                if (transform != null) {
                    transform.finalize();
                }

                // build extents and resolutions from transformed weight image
                extents[0] = imageWeightIso.getExtents()[0];
                extents[1] = imageWeightIso.getExtents()[1];
                resols[0] = imageWeightIso.getFileInfo()[0].getResolutions()[0];
                resols[1] = imageWeightIso.getFileInfo()[0].getResolutions()[1];
            } else {
                resampleW = false;
                extents[0] = inputWeight.getExtents()[0];
                extents[1] = inputWeight.getExtents()[1];
                resols[0] = inputWeight.getFileInfo()[0].getResolutions()[0];
                resols[1] = inputWeight.getFileInfo()[0].getResolutions()[1];
            }

            simpleWeightR_1 = new ModelSimpleImage(extents, resols); // 2D simple image
            simpleWeightI_1 = new ModelSimpleImage(extents, resols); // 2D simple image

            if (DOF >= 3) {
                distance[0] = extents[0] * resols[0];
                distance[1] = extents[1] * resols[1];

                if ((simpleWeightR_1.dataSize > subMinFactor) && doSubsample) {

                    // dont need simpleWeightSub2 (only single slices)
                    extents[0] /= 2;
                    extents[1] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    simpleWeightRSub2_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    simpleWeightISub2_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                } else {
                    simpleWeightRSub2_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    simpleWeightISub2_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    allowLevel2 = false;
                    allowLevel4 = false;
                    allowLevel8 = false;
                    allowLevel16 = false;
                }

                if ((simpleWeightRSub2_1.dataSize > subMinFactor) && doSubsample) {
                    extents[0] /= 2;
                    extents[1] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    simpleWeightRSub4_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    simpleWeightISub4_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                } else {
                    simpleWeightRSub4_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    simpleWeightISub4_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    allowLevel4 = false;
                    allowLevel8 = false;
                    allowLevel16 = false;
                }

                if ((simpleWeightRSub4_1.dataSize > subMinFactor) && doSubsample) {

                    extents[0] = extents[0] / 2;
                    extents[1] = extents[1] / 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    simpleWeightRSub8_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    simpleWeightISub8_1 = new ModelSimpleImage(extents, resols); // 2D simple image

                    if (simpleWeightRSub8_1.dataSize > subMinFactor) {
                        extents[0] = extents[0] / 2;
                        extents[1] = extents[1] / 2;
                        resols[0] = distance[0] / extents[0];
                        resols[1] = distance[1] / extents[1];
                        simpleWeightRSub16_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                        simpleWeightISub16_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    } else {
                        allowLevel16 = false;
                    }
                } else {
                    simpleWeightRSub8_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    simpleWeightISub8_1 = new ModelSimpleImage(extents, resols); // 2D simple image
                    allowLevel8 = false;
                    allowLevel16 = false;
                }
            } // if (DOF >= 3)
        } // if (weighted)

        if (DOF >= 3) {

            if (doColor) {
                subMinFactor *= 4;
            }

            extents[0] = simpleInput_1.xDim;
            extents[1] = simpleInput_1.yDim;
            resols[0] = simpleInput_1.xRes;
            resols[1] = simpleInput_1.yRes;
            distance[0] = extents[0] * resols[0];
            distance[1] = extents[1] * resols[1];

            if ((simpleInput_1.dataSize > subMinFactor) && allowLevel2 && doSubsample) {
                extents[0] /= 2;
                extents[1] /= 2;
                resols[0] = distance[0] / extents[0];
                resols[1] = distance[1] / extents[1];
                simpleInputSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                simpleRefSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image

                level1Factor = 2.0f;
            } else {
                simpleInputSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                simpleRefSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                allowLevel2 = false;
                allowLevel4 = false;
                allowLevel8 = false;
                allowLevel16 = false;
            }

            if ((simpleInputSub2_1.dataSize > subMinFactor) && allowLevel4 && doSubsample) {
                extents[0] /= 2;
                extents[1] /= 2;
                resols[0] = distance[0] / extents[0];
                resols[1] = distance[1] / extents[1];
                simpleInputSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                simpleRefSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                level2Factor = 2.0f;
            } else {
                simpleInputSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                simpleRefSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                allowLevel4 = false;
                allowLevel8 = false;
                allowLevel16 = false;
            }

            if ((simpleInputSub4_1.dataSize > subMinFactor) && allowLevel8 && doSubsample) {
                extents[0] /= 2;
                extents[1] /= 2;
                resols[0] = distance[0] / extents[0];
                resols[1] = distance[1] / extents[1];
                simpleInputSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                simpleRefSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image

                level4Factor = 2.0f;

                if ((simpleInputSub8_1.dataSize > subMinFactor) && allowLevel16) {
                    level4Factor = 4.0f;
                    extents[0] /= 2;
                    extents[1] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    simpleInputSub16_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                    simpleRefSub16_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                } else {
                    allowLevel16 = false;
                }
            } else {
                simpleInputSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                simpleRefSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 2D simple image
                allowLevel8 = false;
                allowLevel16 = false;
            }

            Preferences.debug("Level 1 factor = " + level1Factor + "\n" + "Level 2 factor = " + level2Factor + "\n" +
                              "Level 4 factor = " + level4Factor + "\n",Preferences.DEBUG_ALGORITHM);
        } // if (DOF >= 3)

        long time;

        rNumber = refImageNo;

        if ((refImageNo == (inputImage.getExtents()[2] - 1)) && !useOutsideReferenceSlice) {
            iNumber = refImageNo - 1;
            seriesUp = false;
        } else if (useOutsideReferenceSlice) {
            iNumber = 0;
            seriesUp = true;
        } else {
            iNumber = refImageNo + 1;
            seriesUp = true;
        }

        // get the reference slice from the image and then get the subsampled versions
        try {

            // if image was transformed, we use the isoImage not inputImage
            if (useOutsideReferenceSlice) {
                outsideReferenceSlice.exportData(0, simpleRef_1.data.length, simpleRef_1.data);
            } else if (isoImage != null) {
                isoImage.exportData(rNumber * simpleRef_1.data.length, simpleRef_1.data.length, simpleRef_1.data);
            } else {
                inputImage.exportData(rNumber * simpleRef_1.data.length, simpleRef_1.data.length, simpleRef_1.data);
            }

            if (DOF >= 3) {

                // get subsample by 2'd slice (if allowLevel2)
                // otherwise just copy the ref slice into simpleRefSub2_1
                if (allowLevel2) {
                    subSample2DimBy2(simpleRef_1, simpleRefSub2_1, doColor);
                } else {
                    copyFloatData(simpleRef_1, simpleRefSub2_1);
                }

                if (allowLevel4) {
                    subSample2DimBy2(simpleRefSub2_1, simpleRefSub4_1, doColor);
                } else {
                    copyFloatData(simpleRefSub2_1, simpleRefSub4_1);
                }

                if (allowLevel8) {
                    subSample2DimBy2(simpleRefSub4_1, simpleRefSub8_1, doColor);
                } else {
                    copyFloatData(simpleRefSub4_1, simpleRefSub8_1);
                }

                if (allowLevel16) {
                    subSample2DimBy2(simpleRefSub8_1, simpleRefSub16_1, doColor);
                }
            } // if (DOF >= 3)

            if (weighted && !useOutsideReferenceSlice) {

                if (imageWeightIso != null) {
                    imageWeightIso.exportData(rNumber * simpleWeightR_1.data.length, simpleWeightR_1.data.length,
                                              simpleWeightR_1.data);
                } else {
                    inputWeight.exportData(rNumber * simpleWeightR_1.data.length, simpleWeightR_1.data.length,
                                           simpleWeightR_1.data);
                }

                if (DOF >= 3) {

                    // get subsample by 2'd slice (if allowLevel2)
                    // otherwise just copy the ref slice into simpleRefSub2_1
                    if (allowLevel2) {
                        subSample2DimBy2(simpleWeightR_1, simpleWeightRSub2_1, false);
                    } else {
                        copyFloatData(simpleWeightR_1, simpleWeightRSub2_1);
                    }

                    if (allowLevel4) {
                        subSample2DimBy2(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                    } else {
                        copyFloatData(simpleWeightRSub2_1, simpleWeightRSub4_1);
                    }

                    if (allowLevel8) {
                        subSample2DimBy2(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                    } else {
                        copyFloatData(simpleWeightRSub4_1, simpleWeightRSub8_1);
                    }

                    if (allowLevel16) {
                        subSample2DimBy2(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                    }
                } // if (DOF >= 3)
            }

        } catch (IOException ex) {
            System.err.println("Caught IOException in RegOAR25D2");
            ex.toString();
        }

        int endIndex = inputImage.getExtents()[2] - 1;

        if (useOutsideReferenceSlice) {
            endIndex++;
        }

        // System.err.println("end index is: " + endIndex);
        for (int m = 0; m < endIndex; m++) {
            fireProgressStateChanged("Registering image " + iNumber);

            fireProgressStateChanged((int) (m / (float) (inputImage.getExtents()[2] - 1) * 100));

            Preferences.debug(" ***********************Starting Image " + iNumber + "  **************************\n",
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug(" **************************************************************************\n\n",
            		Preferences.DEBUG_ALGORITHM);

            try {

                if (isoImage != null) {
                    isoImage.exportData(iNumber * simpleInput_1.data.length, simpleInput_1.data.length,
                                        simpleInput_1.data);
                } else {
                    inputImage.exportData(iNumber * simpleInput_1.data.length, simpleInput_1.data.length,
                                          simpleInput_1.data);
                }

                if (DOF >= 3) {

                    if (allowLevel2) {
                        subSample2DimBy2(simpleInput_1, simpleInputSub2_1, doColor);
                    } else {
                        copyFloatData(simpleInput_1, simpleInputSub2_1);
                    }

                    if (allowLevel4) {
                        subSample2DimBy2(simpleInputSub2_1, simpleInputSub4_1, doColor);
                    } else {
                        copyFloatData(simpleInputSub2_1, simpleInputSub4_1);
                    }

                    if (allowLevel8) {
                        subSample2DimBy2(simpleInputSub4_1, simpleInputSub8_1, doColor);
                    } else {
                        copyFloatData(simpleInputSub4_1, simpleInputSub8_1);
                    }

                    if (allowLevel16) {
                        subSample2DimBy2(simpleInputSub8_1, simpleInputSub16_1, doColor);
                    }
                } // if (DOF >= 3)

                simpleRef_1.calcMinMax();
                simpleInput_1.calcMinMax();

                if (DOF >= 3) {
                    simpleRefSub2_1.calcMinMax();
                    simpleInputSub2_1.calcMinMax();
                    simpleRefSub4_1.calcMinMax();
                    simpleInputSub4_1.calcMinMax();
                    simpleRefSub8_1.calcMinMax();
                    simpleInputSub8_1.calcMinMax();

                    if (allowLevel16) {
                        simpleRefSub16_1.calcMinMax();
                        simpleInputSub16_1.calcMinMax();
                    }
                } // if (DOF >= 3)

                if (weighted) {

                    if (imageWeightIso != null) {
                        imageWeightIso.exportData(rNumber * simpleWeightI_1.data.length, simpleWeightI_1.data.length,
                                                  simpleWeightI_1.data);
                    } else {
                        inputWeight.exportData(rNumber * simpleWeightI_1.data.length, simpleWeightI_1.data.length,
                                               simpleWeightI_1.data);
                    }

                    if (DOF >= 3) {

                        if (allowLevel2) {
                            subSample2DimBy2(simpleWeightI_1, simpleWeightISub2_1, false);
                        } else {
                            copyFloatData(simpleWeightI_1, simpleWeightISub2_1);
                        }

                        if (allowLevel4) {
                            subSample2DimBy2(simpleWeightISub2_1, simpleWeightISub4_1, false);
                        } else {
                            copyFloatData(simpleWeightISub2_1, simpleWeightISub4_1);
                        }

                        if (allowLevel8) {
                            subSample2DimBy2(simpleWeightISub4_1, simpleWeightISub8_1, false);
                        } else {
                            copyFloatData(simpleWeightISub4_1, simpleWeightISub8_1);
                        }

                        if (allowLevel16) {
                            subSample2DimBy2(simpleWeightISub8_1, simpleWeightISub16_1, false);
                        }
                    } // if (DOF >= 3)
                }
            } catch (IOException ex) {
                System.err.println("Caught IOException in RegOAR25D2");
                ex.toString();
                ex.printStackTrace();
            }

            time = System.currentTimeMillis();

            if (DOF >= 3) {
                Preferences.debug(" Starting level 8 ************************************************\n",
                		Preferences.DEBUG_ALGORITHM);

                Vector<MatrixListItem>[] minimas;

                if (allowLevel16) {
                    minimas = levelEight(simpleRefSub16_1, simpleInputSub16_1);
                } else {
                    minimas = levelEight(simpleRefSub8_1, simpleInputSub8_1);
                }

                time = System.currentTimeMillis() - time;
                Preferences.debug(" Level 8 min = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
                time = System.currentTimeMillis();

                if (threadStopped) {
                    notifyListeners(this);
                    finalize();

                    return;
                }

                Preferences.debug(" Starting level 4 ************************************************\n",
                		Preferences.DEBUG_ALGORITHM);

                Vector<MatrixListItem> minima = levelFour(simpleRefSub4_1, simpleInputSub4_1, minimas[0], minimas[1]);
                time = System.currentTimeMillis() - time;
                Preferences.debug(" Level 4  min = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
                time = System.currentTimeMillis();

                if (threadStopped) {
                    notifyListeners(this);
                    finalize();

                    return;
                }

                Preferences.debug(" Starting level 2 ************************************************\n",Preferences.DEBUG_ALGORITHM);

                MatrixListItem item = levelTwo(simpleRefSub2_1, simpleInputSub2_1, minima);
                time = System.currentTimeMillis() - time;
                Preferences.debug(" Level 2 min = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
                time = System.currentTimeMillis();

                if (threadStopped) {
                    notifyListeners(this);
                    finalize();

                    return;
                }

                Preferences.debug(" Starting level 1 ************************************************\n",Preferences.DEBUG_ALGORITHM);
                answer = levelOne(simpleRef_1, simpleInput_1, item, iNumber);
            } // if (DOF >= 3)
            else {
                Preferences.debug(" Starting level 1 ************************************************\n",Preferences.DEBUG_ALGORITHM);
                answer = levelOne2D(simpleRef_1, simpleInput_1);
            }

            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 1 min = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);

            if (threadStopped) {
                notifyListeners(this);
                finalize();

                return;
            }

            try {
                inputImage.exportData(iNumber * colorFactor * sliceSize, colorFactor * sliceSize, buffer); // locks and releases and lock
            } catch (IOException error) {
                displayError("AlgorithmOAR25D: Source image is locked");

                setCompleted(false);
                notifyListeners(this);

                return;
            }

            try {

                // Note that if the inputImage has unequal resolutions, then simpleInput_1 has
                // undergone 1 interpolation, so taking the data directly from the inputImage
                // avoids an extra interpolation in this case
                input1.importData(0, buffer, true);
            } catch (IOException error) {
                displayError("AlgorithmOAR25D: IOException on input1.importData");

                setCompleted(false);
                notifyListeners(this);

                return;
            }

            if (transformVOIs) {
                VOI2s.removeAllElements();

                for (i = 0; i < nVOIs; i++) {
                    newVOI[i] = new VOI(id[i], name[i], curveType[i], presetHue[i]);
                    Vector<VOIBase> sliceCurves = VOIs.VOIAt(i).getSliceCurves(iNumber);

                    for (j = 0; j < sliceCurves.size(); j++) {
                        newVOI[i].importPolygon(((VOIContour)sliceCurves.elementAt(j)).exportPolygon(), 0);
                    }

                    VOI2s.addElement(newVOI[i]);
                }

                input1.setVOIs(VOI2s);
            } // if (transformVOIs)

            if (inputImage2 != null) {

                try {
                    inputImage2.exportData(iNumber * colorFactor * sliceSize, colorFactor * sliceSize, buffer); // locks and releases and lock
                } catch (IOException error) {
                    displayError("AlgorithmOAR25D: inputImage2 is locked");

                    setCompleted(false);
                    notifyListeners(this);

                    return;
                }

                try {
                    input2.importData(0, buffer, true);
                } catch (IOException error) {
                    displayError("AlgorithmOAR25D: IOException on input2.importData");

                    setCompleted(false);
                    notifyListeners(this);

                    return;
                }
            } // if (inputImage2 != null)

            if (inputImage3 != null) {

                try {
                    inputImage3.exportData(iNumber * colorFactor * sliceSize, colorFactor * sliceSize, buffer); // locks and releases and lock
                } catch (IOException error) {
                    displayError("AlgorithmOAR25D: inputImage3 is locked");

                    setCompleted(false);
                    notifyListeners(this);

                    return;
                }

                try {
                    input3.importData(0, buffer, true);
                } catch (IOException error) {
                    displayError("AlgorithmOAR25D: IOException on input3.importData");

                    setCompleted(false);
                    notifyListeners(this);

                    return;
                }
            } // if (inputImage3 != null)

            if (doGraph) {

                if (DOF == 3) {
                    rot[iNumber] = (float) (answer.initial[0]);
                }

                //OARmat = answer.matrix.getMatrix();
                TransMatrixd tMatd = answer.matrixd;
                TransMatrix tMat = new TransMatrix(tMatd.getDim(), tMatd.getID(), tMatd.isNIFTI(), tMatd.isQform());
                for (i = 0; i < tMatd.getDim(); i++) {
                    for (j = 0; j < tMatd.getDim(); j++) {
                        tMat.set(i, j, tMatd.get(i, j));
                    }
                }
                trans[0][iNumber] = tMat.get(0, 2);
                trans[1][iNumber] = tMat.get(1, 2);
            } // if (doGraph)

            answer.matrixd.Inverse();
            TransMatrixd tMatd = answer.matrixd;
            TransMatrix tMat = new TransMatrix(tMatd.getDim(), tMatd.getID(), tMatd.isNIFTI(), tMatd.isQform());
            for (i = 0; i < tMatd.getDim(); i++) {
                for (j = 0; j < tMatd.getDim(); j++) {
                    tMat.set(i, j, tMatd.get(i, j));
                }
            }
            transform = new AlgorithmTransform(input1, tMat, interp2, iResols[0], iResols[1], iExtents[0],
                                               iExtents[1], transformVOIs, true, false);
            transform.run();

            if (output_1 != null) {
                output_1.disposeLocal();
            }

            output_1 = transform.getTransformedImage();

            if (transform != null) {
                transform.finalize();
            }

            if (inputImage2 != null) {
                transform = new AlgorithmTransform(input2, tMat, interp2, iResols[0], iResols[1], iExtents[0],
                                                   iExtents[1], false, true, false);
                transform.run();

                if (output_2 != null) {
                    output_2.disposeLocal();
                }

                output_2 = transform.getTransformedImage();

                if (transform != null) {
                    transform.finalize();
                }
            } // if (inputImage2 != null)

            if (inputImage3 != null) {
                transform = new AlgorithmTransform(input3, tMat, interp2, iResols[0], iResols[1], iExtents[0],
                                                   iExtents[1], false, true, false);
                transform.run();

                if (output_3 != null) {
                    output_3.disposeLocal();
                }

                output_3 = transform.getTransformedImage();

                if (transform != null) {
                    transform.finalize();
                }
            } // if (inputImage3 != null)

            try {
                output_1.exportData(0, colorFactor * iExtents[0] * iExtents[1], buffer);
            } catch (IOException error) {
                displayError("AlgorithmOAR25D: IOException on output_1.exportData");

                setCompleted(false);
                notifyListeners(this);

                return;
            }

            if (transform != null) {

                if (transform.getTransformedImage() != null) {
                    transform.getTransformedImage().disposeLocal();
                }

                transform.disposeLocal();
                transform = null;
            }

            System.gc();

            try {
                inputImage.importData(iNumber * colorFactor * iExtents[0] * iExtents[1], buffer, false);
            } catch (IOException error) {
                displayError("AlgorithmOAR25D: IOException on inputImage.importData");

                setCompleted(false);
                notifyListeners(this);

                return;
            }

            if (transformVOIs) {
                VOI2s.removeAllElements();
                VOI2s = output_1.getVOIs();

                for (i = 0; i < nVOIs; i++) {
                    Vector<VOIBase> sliceCurves = VOI2s.VOIAt(i).getCurves();
                    Vector<VOIBase> removeCurves = VOIs.VOIAt(i).getSliceCurves(iNumber);
                    for (j = 0; j < removeCurves.size(); j++ ) {
                        VOIs.VOIAt(i).removeCurve(removeCurves.elementAt(j));	
                    }

                    for (j = 0; j < sliceCurves.size(); j++) {
                        VOIs.VOIAt(i).importPolygon(((VOIContour)sliceCurves.elementAt(j)).exportPolygon(), iNumber);
                    }
                }
            } // if (transformVOIs)

            if (inputImage2 != null) {

                try {
                    output_2.exportData(0, colorFactor * iExtents[0] * iExtents[1], buffer);
                } catch (IOException error) {
                    displayError("AlgorithmOAR25D: IOException on output_2.exportData");

                    setCompleted(false);
                    notifyListeners(this);

                    return;
                }

                try {
                    inputImage2.importData(iNumber * colorFactor * iExtents[0] * iExtents[1], buffer, false);
                } catch (IOException error) {
                    displayError("AlgorithmOAR25D: IOException on inputImage2.importData");

                    setCompleted(false);
                    notifyListeners(this);

                    return;
                }
            } // if (inputImage2 != null)

            if (inputImage3 != null) {

                try {
                    output_3.exportData(0, colorFactor * iExtents[0] * iExtents[1], buffer);
                } catch (IOException error) {
                    displayError("AlgorithmOAR25D: IOException on output_3.exportData");

                    setCompleted(false);
                    notifyListeners(this);

                    return;
                }

                try {
                    inputImage3.importData(iNumber * colorFactor * iExtents[0] * iExtents[1], buffer, false);
                } catch (IOException error) {
                    displayError("AlgorithmOAR25D: IOException on inputImage3.importData");

                    setCompleted(false);
                    notifyListeners(this);

                    return;
                }
            } // if (inputImage3 != null)

            if ((iNumber == (inputImage.getExtents()[2] - 1)) && regToAdjImage) {

                try {

                    if (isoImage != null) {
                        isoImage.exportData(refImageNo * simpleRef_1.data.length, simpleRef_1.data.length,
                                            simpleRef_1.data);
                    } else {
                        inputImage.exportData(refImageNo * simpleRef_1.data.length, simpleRef_1.data.length,
                                              simpleRef_1.data);
                    }

                    if (DOF >= 3) {

                        if (allowLevel2) {
                            subSample2DimBy2(simpleRef_1, simpleRefSub2_1, doColor);
                        } else {
                            copyFloatData(simpleRef_1, simpleRefSub2_1);
                        }

                        if (allowLevel4) {
                            subSample2DimBy2(simpleRefSub2_1, simpleRefSub4_1, doColor);
                        } else {
                            copyFloatData(simpleRefSub2_1, simpleRefSub4_1);
                        }

                        if (allowLevel8) {
                            subSample2DimBy2(simpleRefSub4_1, simpleRefSub8_1, doColor);
                        } else {
                            copyFloatData(simpleRefSub4_1, simpleRefSub8_1);
                        }

                        if (allowLevel16) {
                            subSample2DimBy2(simpleRefSub8_1, simpleRefSub16_1, doColor);
                        }
                    } // if (DOF >= 3)

                    if (weighted) {

                        if (imageWeightIso != null) {
                            imageWeightIso.exportData(refImageNo * simpleWeightR_1.data.length,
                                                      simpleWeightR_1.data.length, simpleWeightR_1.data);

                        } else {
                            inputWeight.exportData(refImageNo * simpleWeightR_1.data.length,
                                                   simpleWeightR_1.data.length, simpleWeightR_1.data);
                        }

                        if (DOF >= 3) {

                            if (allowLevel2) {
                                subSample2DimBy2(simpleWeightR_1, simpleWeightRSub2_1, false);
                            } else {
                                copyFloatData(simpleWeightR_1, simpleWeightRSub2_1);
                            }

                            if (allowLevel4) {
                                subSample2DimBy2(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                            } else {
                                copyFloatData(simpleWeightRSub2_1, simpleWeightRSub4_1);
                            }

                            if (allowLevel8) {
                                subSample2DimBy2(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                            } else {
                                copyFloatData(simpleWeightRSub4_1, simpleWeightRSub8_1);
                            }

                            if (allowLevel16) {
                                subSample2DimBy2(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                            }
                        } // if (DOF >= 3)
                    }
                } catch (IOException ex) {
                    System.err.println("Caught IOException in RegOAR25D2");

                }

            } // if ((iNumber == inputImage.getExtents()[2] - 1) && regToAdjImage)
            else if (resample && regToAdjImage) {
                transform = new AlgorithmTransform(input1, answer.matrix, interp2, resIso[0], resIso[1], extentsIso[0],
                                                   extentsIso[1], false, true, false);

                transform.run();

                if (output_1 != null) {
                    output_1.disposeLocal();
                }

                output_1 = transform.getTransformedImage();

                if (transform != null) {
                    transform.finalize();
                }

                try {
                    output_1.exportData(0, colorFactor * extentsIso[0] * extentsIso[1], bufferA);
                } catch (IOException error) {
                    displayError("AlgorithmOAR25D: IOException on output_1.exportData to bufferA");

                    setCompleted(false);
                    notifyListeners(this);

                    return;
                }

                if (transform != null) {

                    if (transform.getTransformedImage() != null) {
                        transform.getTransformedImage().disposeLocal();
                    }

                    transform.disposeLocal();
                    transform = null;
                }

                System.gc();

                for (i = 0; i < bufferA.length; i++) {
                    simpleRef_1.data[i] = bufferA[i];
                }

                if (DOF >= 3) {

                    if (level1Factor == 2.0f) {
                        subSample2DimBy2(simpleRef_1, simpleRefSub2_1, doColor);
                    }

                    if (level2Factor == 2.0f) {
                        subSample2DimBy2(simpleRefSub2_1, simpleRefSub4_1, doColor);
                    }

                    if (level4Factor >= 2.0f) {
                        subSample2DimBy2(simpleRefSub4_1, simpleRefSub8_1, doColor);
                    }

                    if (level4Factor == 4.0f) {
                        subSample2DimBy2(simpleRefSub8_1, simpleRefSub16_1, doColor);
                    }
                } // if (DOF >= 3)

                if (weighted) {

                    try {
                        inputWeight.exportData(iNumber * inputWeight.getSliceSize(), inputWeight.getSliceSize(),
                                               bufferIW);
                    } catch (IOException error) {
                        displayError("AlgorithmOAR25D: inputWeight image is locked");

                        setCompleted(false);
                        notifyListeners(this);

                        return;
                    }

                    try {

                        // Note that if the inputWeight has unequal resolutions, then simpleWeightI_1 has
                        // undergone 1 interpolation, so taking the data directly from the inputWeight
                        // avoids an extra interpolation in this case
                        inputw_1.importData(0, bufferIW, true);
                    } catch (IOException error) {
                        displayError("AlgorithmOAR25D: IOException on inputW_1.importData");

                        setCompleted(false);
                        notifyListeners(this);

                        return;
                    }

                    transform = new AlgorithmTransform(inputw_1, tMat, interp2, resIso[0], resIso[1],
                                                       extentsIso[0], extentsIso[1], false, true, false);
                    transform.run();

                    if (output_1 != null) {
                        output_1.disposeLocal();
                    }

                    output_1 = transform.getTransformedImage();

                    if (transform != null) {
                        transform.finalize();
                    }

                    try {
                        output_1.exportData(0, extentsIso[0] * extentsIso[1], bufferW);
                    } catch (IOException error) {
                        displayError("AlgorithmOAR25D: IOException on output_1.exportData");

                        setCompleted(false);
                        notifyListeners(this);

                        return;
                    }

                    if (transform != null) {

                        if (transform.getTransformedImage() != null) {
                            transform.getTransformedImage().disposeLocal();
                        }

                        transform.disposeLocal();
                        transform = null;
                    }

                    System.gc();

                    for (i = 0; i < bufferW.length; i++) {
                        simpleWeightR_1.data[i] = bufferW[i];
                    }

                    if (DOF >= 3) {

                        if (level1Factor == 2.0f) {
                            subSample2DimBy2(simpleWeightR_1, simpleWeightRSub2_1, false);
                        }

                        if (level2Factor == 2.0f) {
                            subSample2DimBy2(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                        }

                        if (level4Factor >= 2.0f) {
                            subSample2DimBy2(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                        }

                        if (level4Factor == 4.0f) {
                            subSample2DimBy2(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                        }
                    } // if (DOF >= 3)
                } // if (weighted)
            } // if (resample && regToAdjImage)
            else if (regToAdjImage) {

                for (i = 0; i < buffer.length; i++) {
                    simpleRef_1.data[i] = buffer[i];
                }

                if (DOF >= 3) {

                    if (level1Factor == 2.0f) {
                        subSample2DimBy2(simpleRef_1, simpleRefSub2_1, doColor);
                    }

                    if (level2Factor == 2.0f) {
                        subSample2DimBy2(simpleRefSub2_1, simpleRefSub4_1, doColor);
                    }

                    if (level4Factor >= 2.0f) {
                        subSample2DimBy2(simpleRefSub4_1, simpleRefSub8_1, doColor);
                    }

                    if (level4Factor == 4.0f) {
                        subSample2DimBy2(simpleRefSub8_1, simpleRefSub16_1, doColor);
                    }
                } // if (DOF >= 3)

                if (weighted) {

                    try {
                        inputWeight.exportData(iNumber * inputWeight.getSliceSize(), inputWeight.getSliceSize(),
                                               bufferIW);
                    } catch (IOException error) {
                        displayError("AlgorithmOAR25D: inputWeight image is locked");

                        setCompleted(false);
                        notifyListeners(this);

                        return;
                    }

                    if (resampleW) {

                        try {

                            // Note that if the inputWeight has unequal resolutions, then simpleWeightI_1 has
                            // undergone 1 interpolation, so taking the data directly from the inputWeight
                            // avoids an extra interpolation in this case
                            inputw_1.importData(0, bufferIW, true);
                        } catch (IOException error) {
                            displayError("AlgorithmOAR25D: IOException on inputW_1.importData");

                            setCompleted(false);
                            notifyListeners(this);

                            return;
                        }

                        transform = new AlgorithmTransform(inputw_1, tMat, interp2, resIso[0], resIso[1],
                                                           extentsIso[0], extentsIso[1], false, true, false);
                        transform.run();

                        if (output_1 != null) {
                            output_1.disposeLocal();
                        }

                        output_1 = transform.getTransformedImage();

                        if (transform != null) {
                            transform.finalize();
                        }

                        try {
                            output_1.exportData(0, extentsIso[0] * extentsIso[1], bufferW);
                        } catch (IOException error) {
                            displayError("AlgorithmOAR25D: IOException on output_1.exportData");

                            setCompleted(false);
                            notifyListeners(this);

                            return;
                        }

                        if (transform != null) {

                            if (transform.getTransformedImage() != null) {
                                transform.getTransformedImage().disposeLocal();
                            }

                            transform.disposeLocal();
                            transform = null;
                        }

                        System.gc();

                        for (i = 0; i < bufferW.length; i++) {
                            simpleWeightR_1.data[i] = bufferW[i];
                        }
                    } // if (resampleW)
                    else { // weight image is not resampled

                        for (i = 0; i < bufferIW.length; i++) {
                            simpleWeightR_1.data[i] = bufferIW[i];
                        }
                    }

                    if (DOF >= 3) {

                        if (level1Factor == 2.0f) {
                            subSample2DimBy2(simpleWeightR_1, simpleWeightRSub2_1, false);
                        }

                        if (level2Factor == 2.0f) {
                            subSample2DimBy2(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                        }

                        if (level4Factor >= 2.0f) {
                            subSample2DimBy2(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                        }

                        if (level4Factor == 4.0f) {
                            subSample2DimBy2(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                        }
                    } // if (DOF >= 3)
                } // if (weighted)
            } // else if (regToAdjImage)

            if (seriesUp) {

                if (regToAdjImage) {
                    rNumber++;
                }

                iNumber++;

                if ((iNumber == inputImage.getExtents()[2]) && !useOutsideReferenceSlice) {
                    rNumber = refImageNo;
                    iNumber = refImageNo - 1;
                    seriesUp = false;
                }
            } // if (seriesUp)
            else { // not seriesUp

                if (regToAdjImage) {
                    rNumber--;
                }

                iNumber--;
            } // else not seriesUp

            System.gc();
            System.gc();
        } // for (int m = 0; m < inputImage.getExtents()[2]-1; m++)

        inputImage.calcMinMax();

        if (transformVOIs) {
            inputImage.setVOIs(VOIs);
        }

        setCompleted(true);
    }

    /**
     * inputImage2 is not used to determine the registration, but each slice of inputImage2 undergoes the same
     * transformation as inputImage. This is useful for color images where you wish the registration to be determined by
     * only 1 of the colors
     *
     * @param  inputImage2  DOCUMENT ME!
     */
    public void setInputImage2(ModelImage inputImage2) {
        this.inputImage2 = inputImage2;
    }

    /**
     * inputImage3 is not used to determine the registration, but each slice of inputImage3 undergoes the same
     * transformation as inputImage. This is useful for color images where you wish the registration to be determined by
     * only 1 of the colors
     *
     * @param  inputImage3  DOCUMENT ME!
     */
    public void setInputImage3(ModelImage inputImage3) {
        this.inputImage3 = inputImage3;
    }

    /**
     * allows the user to pass in an OUTSIDE reference slice.
     *
     * @param   refSlice  2-Dim image for reference
     *
     * @return  DOCUMENT ME!
     */
    public boolean setReferenceSlice(ModelImage refSlice) {

        if ((refSlice.getExtents()[0] != inputImage.getExtents()[0]) ||
                (refSlice.getExtents()[1] != inputImage.getExtents()[1])) {
            return false;
        }

        this.useOutsideReferenceSlice = true;
        this.regToAdjImage = false;
        this.outsidePreReferenceSlice = refSlice;

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   srcImage     DOCUMENT ME!
     * @param   resultImage  DOCUMENT ME!
     * @param   isColor      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static ModelSimpleImage subSample2DimBy2(ModelSimpleImage srcImage, ModelSimpleImage resultImage,
                                                     boolean isColor) {

        return srcImage.subSample2dBy2(resultImage, isColor);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  srcImage     DOCUMENT ME!
     * @param  resultImage  DOCUMENT ME!
     */
    private void copyFloatData(ModelSimpleImage srcImage, ModelSimpleImage resultImage) {

        for (int i = 0; i < resultImage.data.length; i++) {
            resultImage.data[i] = srcImage.data[i];
        }
    }

    /**
     * Creates a string with the parameters that the image was constructed with.
     *
     * @return  Construction info.
     */
    private String getConstructionInfo() {
        String s;

        s = new String("RegistrationOAR25D(" + inputImage.getImageName() + ", ");
        s += String.valueOf(refImageNo) + ", ";

        if (regToAdjImage) {
            s += "Adjacent mode, ";
        } else {
            s += "Reference mode, ";
        }

        if (weighted) {
            s += inputWeight.getImageName() + ", ";
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

        s += "Output interpolation = ";

        switch (interp2) {

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

    /*private double[] getTolerance(int DOF, int dims[], float res, float factor) {
     * double[] tols = new double[DOF]; double[] rad = new double[3]; int i; for (i=0; i<3; i++) {
     * rad[i]=(dims[i]*res)/2; // field of view divided by two } double mean_rad = (rad[0]+rad[1])/2.0; //2.5 D, only
     * rotation about z
     *
     * if (DOF == 2) { tols[0] = tols[1] = (res/2.0) * factor; // translations } else if (DOF == 3 && rigidFlag == true) {
     * tols[0] = ((180./Math.PI)*res/(2*mean_rad)) * factor; // rotation tols[1] = tols[2] = (res/2.0) * factor; //
     * translations } else if (DOF == 3 && rigidFlag == false) { tols[0] =  (res/(2*mean_rad)) * factor; // global
     * scaling tols[1] = (tols[2] = res/2.0) * factor; // translations } else if (DOF == 4) { tols[0] =
     * ((180./Math.PI)*res/(2*rad[0])) * factor; // rotation tolerances tols[1] = (tols[2] = res/2.0) * factor; //
     * translation tolerance x and y tols[3] =  (res/(2*mean_rad)) * factor; // scaling tolerances global } else if (DOF
     * == 5) { tols[0] = ((180./Math.PI)*res/(2*rad[0])) * factor; // rotation tolerances tols[1] = tols[2] = (res/2.0)
     * factor; // translation tolerance x and y tols[3] = tols[4] =  (res/(2*mean_rad)) * factor; // scaling tolerance x
     * } else if (DOF == 6) { tols[0] = tols[1] = (res/2.0) * factor; // translation tolerance x and y tols[2] = tols[3]
     * = (res/(2*mean_rad)) * factor; // scaling tolerance x tols[4] = tols[5] = (res/(2*mean_rad)) * factor; // skew
     * tolerance x } return tols; }*/

    /**
     * Gets the tolerance vector based on the degrees of freedom (the length of the tolerance is the degrees of freedom)
     * and.
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
            tols[3] = (1.0 / maxDim); // scaling tolerances global
        } else if (DOF == 5) {
            tols[0] = ((180. / Math.PI) / maxDim); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = tols[4] = 0.005; // (1.0/maxDim); // scaling tolerance x
        } else if (DOF == 7) {
            tols[0] = ((180. / Math.PI) / maxDim); // rotation tolerances
            tols[1] = tols[2] = 0.5; // translation tolerance x and y
            tols[3] = tols[4] = 0.005; // (1.0/maxDim); // scaling tolerance x
            tols[5] = (0.0001); // 1.0/maxDim); // skew tolerance
            tols[6] = (0.0001); // 1.0/maxDim); // skew tolerance
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
     * Takes two slices that have been subsampled by a factor of 8 or 16. Sets up the cost function with the slices and
     * the weighted slices, if necessary. Uses the coarse sampling rate and optimize translations and global scale at
     * the given rotation. So for example, if the coarse sampling range were -30 to 30 at every 15 degrees, we would
     * optimize at rotations of -30, -15, 0, 15, 30. Measures the cost at the fine sampling rate. Interpolates the
     * translations and global scale to come up with a good guess as to what the optimized translation would be at that
     * point. Takes the top 20% of the points and optimizes them. Now have a large multi-array of costs. 20% of those
     * have been optimized and placed back into their original position in the multi-array. Looks at the 3 neighbors of
     * a point: +, =, or - one fine sample. If the point has a cost greater than any of these, it is not a minima.
     * Otherwise it is. Saves it in a vector of minima. Optimizes the minima over rotation as well as translations and
     * global scale. (Previously had not optimized over rotation.) Returns two vectors, one containing the minima before
     * optimization, one containing the minima after optimization.
     *
     * @param   ref    Subsampled by 8 or 16 reference slice.
     * @param   input  Subsampled by 8 or 16 input slice.
     *
     * @return  List of preoptimized and optimized points.
     */
    @SuppressWarnings("unchecked")
    private Vector<MatrixListItem>[] levelEight(ModelSimpleImage ref, ModelSimpleImage input) {

        double factor;
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 32, 1);

        if (weighted) {

            if (allowLevel16) {
                cost.setRefWgtImage(simpleWeightRSub16_1);
                cost.setInputWgtImage(simpleWeightISub16_1);
            } else {
                cost.setRefWgtImage(simpleWeightRSub8_1);
                cost.setInputWgtImage(simpleWeightISub8_1);
            }
        }

        Vector2f cog;
        Vector2f cogR;

        if (allowLevel16) {
            cog = calculateCenterOfMass2D(input, simpleWeightISub16_1, doColor);
            cogR = calculateCenterOfMass2D(ref, simpleWeightRSub16_1, doColor);
        } else {
            cog = calculateCenterOfMass2D(input, simpleWeightISub8_1, doColor);
            cogR = calculateCenterOfMass2D(ref, simpleWeightRSub8_1, doColor);
        }

        Preferences.debug("COG of Ref   = " + cogR + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("COG of Input = " + cog + "\n",Preferences.DEBUG_ALGORITHM);

        double diffX = (cog.X - cogR.X);
        double diffY = (cog.Y - cogR.Y);

        if (ignoreCOG) {
            diffX = 0;
            diffY = 0;
            cog = cogR;
        }

        // Prepare data for call to getTolerance
        // if all subsampling done =2*2*2=8 (if x-large img=4*2*2=16)
        // There is no need to get separate resolutions for each direction since the
        // images have been resampled to be isotropic (and have matching resolutions).
        // Optimizing over translations and global scaling. DOF = 3;
        AlgorithmPowellOptBase powell;
        maxIter = baseNumIter;
        int nDims = 2;
        if (DOF > 3) {
            nDims = 3;
        }
        powell = new AlgorithmPowellOpt2D(this, cog, nDims, cost, getTolerance(nDims), maxIter, false);

        Vectornd[] initials = new Vectornd[coarseNum];
        
        double[] initial = new double[7];
        initial[0] = 0; // initial rotation
        initial[1] = diffX; // initial translations
        initial[2] = diffY;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing
        for (int i = 0; (i < coarseNum) && !threadStopped; i++) {
            initial[0] = rotateBegin + (i * coarseRate);
            initials[i] = new Vectornd(initial, true);
        }
        powell.setPoints(initials);
        powell.run();

        double[][] transforms = new double[coarseNum][3];
        for(int i = 0; i < coarseNum; i++){
            transforms[i] = powell.getPoint(i);
        }

        if (threadStopped) {
            return null;
        }

        MatrixListItem[] matrixList = new MatrixListItem[fineNum];

        double[] costs = new double[fineNum];
        int index = 0;

        for (int i = 0; (i < fineNum) && !threadStopped; i++) {
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

        Arrays.sort(costs);

        double threshold = costs[0] + (0.2 * (costs[costs.length - 1] - costs[0]));

        if (threshold > costs[(int) (0.2 * costs.length)]) {
            threshold = costs[(int) (0.2 * costs.length)];
        }

        initials = new Vectornd[fineNum];
        for (int i = 0; (i < fineNum) && !threadStopped; i++) {
            if (matrixList[i].cost < threshold) {
                initials[i] = new Vectornd(matrixList[i].initial);
            }
        }
        powell.setPoints(initials);
        powell.run();
        for(int i = 0; i < fineNum; i++){
            if (matrixList[i].cost < threshold) {
                matrixList[i] = new MatrixListItem(powell.getCost(i), powell.getMatrix(i), powell.getPoint(i));
            }
        }

        if (threadStopped) {
            return null;
        }

        Vector<MatrixListItem> minima = new Vector<MatrixListItem>();
        boolean possibleMinima[] = new boolean[fineNum];

        for (int i = 0; i < fineNum; i++) {
            possibleMinima[i] = true; // possible minimum

            // as long as still possible minimum, check neighbors one degree off
            for (int itest = -1; (itest <= 1) && possibleMinima[i]; itest += 2) {

                if (((i + itest) >= 0) && ((i + itest) < fineNum)) {

                    if (matrixList[i].cost > matrixList[i + itest].cost) {
                        possibleMinima[i] = false;
                    } // not a minimum if a neighbor has a lower cost
                }
            }
        }
        
        boolean change = true;
        while (change) {
            change = false;
            for (int i = 0; i < fineNum; i++) {
                for (int itest = -1; (itest <= 1) && possibleMinima[i]; itest+=2) {
                    if (((i + itest) >= 0) && ((i + itest) < fineNum)) {
                        if ((matrixList[i].cost == matrixList[i + itest].cost) &&
                                (!possibleMinima[i + itest])) {
                            possibleMinima[i] = false;
                            change = true;
                        } // not a minimum if equal value neighbor is not a minimum
                    }
                }
            }
        }
        
        for (int i = 0; i < fineNum; i++) {
            if (possibleMinima[i]) {
                minima.add(matrixList[i]);
            }
        }

        if (threadStopped) {
            return null;
        }

        // Preferences.debug("Number of minima: " + minima.size() + "\n",Preferences.DEBUG_ALGORITHM);
        Vector<MatrixListItem> optMinima = new Vector<MatrixListItem>();

        // Now freely optimizes over rotation:
        int count = 0;
        int degree = (DOF < 4) ? DOF : 4;

        if (powell != null) {
            powell.finalize();
        }

        maxIter = baseNumIter;
        powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter, rigidFlag);

        MatrixListItem item;
        initials = new Vectornd[minima.size()];
        index = 0;
        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            initials[index++] = new Vectornd(en.nextElement().initial, true);
        }
        powell.setPoints(initials);
        powell.run();
        for(int i = 0; i < minima.size(); i++){
            item = new MatrixListItem(powell.getCost(i), powell.getMatrix(i), powell.getPoint(i));
            optMinima.add(item);
            count++;
        }

        if (threadStopped) {
            return null;
        }

        cost.disposeLocal();
        powell.disposeLocal();

        return new Vector[] { minima, optMinima };
    }

    /**
     * Takes two slices that have been subsampled by a factor of four, and two vectors of minima. Sets up the cost
     * function with the slices and the weighted slices, if necessary. Adds the level4Factor determined during
     * subsampling. Measures the costs of the minima on the images and sort them. Takes the top three in each vector
     * (pre-optimization and post-optimization) and optimizes them. Puts them all into one vector. Perturbs the rotation
     * by zero and by plus-minus fineDelta. If it's not a rigid transformation, perturbs the scales by 0, plus-minus .1,
     * then plus-minus .2. Optimize the perturbations. Returns a vector of the perturbed, optimized minima.
     *
     * @param   ref        Reference slice, subsampled by 4.
     * @param   input      Input slice, subsampled by 4.
     * @param   minima     Preoptimized minima.
     * @param   optMinima  DOCUMENT ME!
     *
     * @parem   optMinima Optimized minima.
     *
     * @return  A vector of perturbed, optimized minima.
     */
    private Vector<MatrixListItem> levelFour(ModelSimpleImage ref, ModelSimpleImage input, 
            Vector<MatrixListItem> minima, Vector<MatrixListItem> optMinima) {
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 64, 1);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRSub4_1);
            cost.setInputWgtImage(simpleWeightISub4_1);
        }

        Vector2f cog = calculateCenterOfMass2D(input, simpleWeightISub4_1, doColor);

        // Preferences.debug("COG of input = " + cog + "\n",Preferences.DEBUG_ALGORITHM);
        MatrixListItem item = null;

        // fix translations based on image resolutions!
        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item =  en.nextElement();
            item.initial[1] *= level4Factor;
            item.initial[2] *= level4Factor;
        }

        for (Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[1] *= level4Factor;
            item.initial[2] *= level4Factor;
        }

        int degree = (DOF < 4) ? DOF : 4;
        maxIter = baseNumIter;

        AlgorithmPowellOptBase powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree),
                                                               maxIter, rigidFlag);

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item.cost = powell.measureCost(en.nextElement().initial);
        }

        if (threadStopped) {
            return null;
        }

        for (Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements() && !threadStopped;) {
            item.cost = powell.measureCost(en.nextElement().initial);
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(minima);
        Collections.sort(optMinima);

        Vector<MatrixListItem> newMinima = new Vector<MatrixListItem>();

        // Old code:
        // int total = (3 < minima.size()) ? 3 : minima.size();
        // Now changed so that the number of minima to test at Level Four is a variable,
        // passed in from JDialog.  It used to be set to "3".
        int total = (numMinima < minima.size()) ? numMinima : minima.size();
        Preferences.debug("Number of minima to test at levelFour: " + total + ".\n",Preferences.DEBUG_ALGORITHM);
        powell.setMaxIterations(7);
        Vectornd[] initials = new Vectornd[total*2];
        for (int i = 0; (i < total) && !threadStopped; i++) {
            initials[i] = new Vectornd(minima.elementAt(i).initial);
            initials[i+total] = new Vectornd(optMinima.elementAt(i).initial);
        }
        powell.setPoints(initials);
        powell.run();

        if (threadStopped) {
            return null;
        }

        for(int i = 0; i < 2*total; i++){
            item = new MatrixListItem(powell.getCost(i), powell.getMatrix(i), powell.getPoint(i));
            newMinima.add(item);
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(newMinima);

        double fineDelta = fineRate / 2.0;
        double[] initial;
        Vector<MatrixListItem> perturbList = new Vector<MatrixListItem>();

        if(DOF > 3){
            initials = new Vectornd[newMinima.size()*7];
        }else{
            initials = new Vectornd[newMinima.size()*3];
        }
        // Perturb rotation.  Add fine delta and optimize, then subtract fine delta and optimize.
        int index = 0;
        for (int j = 0; (j <= 2) && !threadStopped; j++) {

            for (int i = 0; (i < 2*total) && !threadStopped; i++) {
                initial = (double[]) newMinima.elementAt(i).initial.clone();

                // Preferences.debug("Perturbing initial[0] by ",Preferences.DEBUG_ALGORITHM);
                if (j == 0) { // Preferences.debug("No fineDelta added or subtracted\n",Preferences.DEBUG_ALGORITHM);
                }

                if (j == 1) {
                    initial[0] += fineDelta;
                    // Preferences.debug("adding " + fineDelta + "\n",Preferences.DEBUG_ALGORITHM);
                }

                if (j == 2) {
                    initial[0] -= fineDelta;
                    // Preferences.debug("subtracting " + fineDelta + "\n",Preferences.DEBUG_ALGORITHM);
                }

                // 1.) unchanged initial
                // 2.) make initial variable old initial + fineDelta
                // 3.) Make initial variable old initial - fineDelta
                initials[index++] = new Vectornd(initial);
            }
        }

        if (DOF > 3) {
            float scaleDelta = 0.8f;
            for (int j = 0; (j < 4) && !threadStopped; j++) {

                for (int i = 0; (i < (2 * total)) && !threadStopped; i++) {
                    initial = (double[]) newMinima.elementAt(i).initial.clone();

                    if (j == 1) {
                        scaleDelta = 0.9f;
                    } else if (j == 2) {
                        scaleDelta = 1.1f;
                    } else if (j == 3) {
                        scaleDelta = 1.2f;
                    } 

                    // Preferences.debug("Perturbing initial[3] by ",Preferences.DEBUG_ALGORITHM);
                    initial[3] *= scaleDelta;

                    // Preferences.debug("Multiplying by " + scaleDelta + "\n",Preferences.DEBUG_ALGORITHM);
                    // make initial variable old initial * scaleDelta in each dimension
                    initials[index++] = new Vectornd(initial);
                }
            }
        }
        powell.setPoints(initials);
        powell.run();
        
        for(int i = 0; i < initials.length; i++){
            item = new MatrixListItem(powell.getCost(i), powell.getMatrix(i), powell.getPoint(i));
            perturbList.add(item);
        }

        if (threadStopped) {
            return null;
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(perturbList);
        cost.disposeLocal();
        powell.disposeLocal();

        return perturbList;
    }

    /**
     * Takes the two slices, no subsampling, and the best minimum so far. Sets up the cost function with the slices and
     * the weighted slices, if necessary. Adds the level1Factor determined during subsampling. Performs one optimization
     * run, with the maximum allowable degrees of freedom as specified by the user (the max is 7). Returns the best
     * minimum.
     *
     * @param   ref    Reference slice.
     * @param   input  Input slice.
     * @param   item   Best minimum so far.
     * @param   frame  DOCUMENT ME!
     *
     * @return  Best minimum after optimization.
     */
    private MatrixListItem levelOne(ModelSimpleImage ref, ModelSimpleImage input, MatrixListItem item, int frame) {
        int degree;
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 256, 1);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightR_1);
            cost.setInputWgtImage(simpleWeightI_1);
        }

        Vector2f cog = calculateCenterOfMass2D(input, simpleWeightI_1, doColor);

        // initial[1] == diffX and initial[2] == diffY
        item.initial[1] *= level1Factor;
        item.initial[2] *= level1Factor;

        degree = DOF;

        maxIter = 4 * baseNumIter;

        AlgorithmPowellOptBase powell = new AlgorithmPowellOpt2D(this, cog, degree, cost,
                getTolerance(degree), maxIter, rigidFlag);
        Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(item.initial);
        powell.setPoints(initials);
        powell.run();

        if (threadStopped) {
            return null;
        }

        MatrixListItem item2 = new MatrixListItem(powell.getCost(0), powell.getMatrix(0, input.xRes), powell.getPoint(0));
        Preferences.debug("Best answer: \n" + item2 + "\n",Preferences.DEBUG_ALGORITHM);

        sliceCosts[frame] = item2.getCost();

        cost.disposeLocal();
        powell.finalize();

        return item2;
    }

    /**
     * This routine is used only with DOF = 2, translations only. LevelEight, levelFour, and levelTwo are skipped Takes
     * the two slices, no subsampling. Sets up the cost function with the slices and the weighted slices. Adds the
     * level1Factor determined during subsampling. Performs one optimization run, with 2 degrees of freedom
     *
     * @param   ref    Reference slice.
     * @param   input  Input slice.
     *
     * @return  Best minimum after optimization.
     */
    private MatrixListItem levelOne2D(ModelSimpleImage ref, ModelSimpleImage input) {
        int degree;
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 256, 1);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightR_1);
            cost.setInputWgtImage(simpleWeightI_1);
        }

        Vector2f cog = calculateCenterOfMass2D(input, simpleWeightI_1, doColor);
        Vector2f cogR = calculateCenterOfMass2D(ref, simpleWeightR_1, doColor);

        Preferences.debug("COG of Ref   = " + cogR + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("COG of Input = " + cog + "\n",Preferences.DEBUG_ALGORITHM);

        double diffX = (cog.X - cogR.X);
        double diffY = (cog.Y - cogR.Y);
        double[] initial = new double[7];

        initial[0] = 0; // initial rotation
        initial[1] = diffX; // initial translations
        initial[2] = diffY;
        initial[3] = initial[4] = 1; // initial scaling
        initial[5] = initial[6] = 0; // initial skewing

        degree = 2;

        maxIter = 4 * baseNumIter;

        AlgorithmPowellOptBase powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree),
                                                               maxIter, rigidFlag);
        Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(initial);

        powell.setPoints(initials);
        powell.run();

        if (threadStopped) {
            return null;
        }

        MatrixListItem item2 = new MatrixListItem(powell.getCost(0), powell.getMatrix(0, input.xRes), powell.getPoint(0));
        Preferences.debug("Best answer: \n" + item2 + "\n",Preferences.DEBUG_ALGORITHM);
        cost.disposeLocal();
        powell.finalize();

        return item2;
    }

    /**
     * Takes two slices that have been subsampled by a factor of 2 and a vector of minima. Sets up the cost function
     * with the images and the weighted images, if necessary. Adds the level2Factor determined during subsampling.
     * Measures the costs of the minima at the images. Optimizes the best minimum with 4 degrees of freedom, then 5,
     * then 7. If the user has limited the degrees of freedom to 3, there will only be one optimization run, with 3
     * degrees of freedom. Returns the best minimum after optimization.
     *
     * @param   ref     Reference slice, subsampled by 2.
     * @param   input   Input slice, subsampled by 2.
     * @param   minima  Minima.
     *
     * @return  The optimized minimum.
     */
    private MatrixListItem levelTwo(ModelSimpleImage ref, ModelSimpleImage input, Vector<MatrixListItem> minima) {
        AlgorithmCostFunctions2D cost = new AlgorithmCostFunctions2D(ref, input, costChoice, 128, 1);

        if (weighted) {
            cost.setRefWgtImage(simpleWeightRSub2_1);
            cost.setInputWgtImage(simpleWeightISub2_1);
        }

        Vector2f cog = calculateCenterOfMass2D(input, simpleWeightISub2_1, doColor);
        MatrixListItem item = null;

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[1] *= level2Factor;
            item.initial[2] *= level2Factor;
        }

        int degree = (DOF < 4) ? DOF : 4;
        maxIter = baseNumIter;

        AlgorithmPowellOptBase powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter, rigidFlag);

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item.cost = powell.measureCost(en.nextElement().initial);
        }

        Collections.sort(minima);

        if (powell != null) {
            powell.finalize();
        }

        maxIter = baseNumIter;
        powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter, rigidFlag);
        Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(minima.elementAt(0).initial);
        powell.setPoints(initials);

        powell.run();

        if (threadStopped) {
            return null;
        }

        item = new MatrixListItem(powell.getCost(0), powell.getMatrix(0), powell.getPoint(0));
        // Preferences.debug("Level 2, after " + degree + " DOF: " + item + "\n",Preferences.DEBUG_ALGORITHM);

        if (DOF > 4) {
            degree = 5;

            if (powell != null) {
                powell.finalize();
            }

            maxIter = baseNumIter;
            powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter,
                                              rigidFlag);
            powell.setPoints(initials);
            powell.run();

            if (threadStopped) {
                return null;
            }

            item = new MatrixListItem(powell.getCost(0), powell.getMatrix(0), powell.getPoint(0));
            // Preferences.debug("Level 2, after " + degree + " DOF: " + item + "\n",Preferences.DEBUG_ALGORITHM);

            if (DOF > 5) {
                degree = (DOF < 7) ? DOF : 7;

                if (powell != null) {
                    powell.finalize();
                }

                maxIter = baseNumIter;
                powell = new AlgorithmPowellOpt2D(this, cog, degree, cost, getTolerance(degree), maxIter,
                                                  rigidFlag);

                powell.setPoints(initials);
                powell.run();

                if (threadStopped) {
                    return null;
                }

                item = new MatrixListItem(powell.getCost(0), powell.getMatrix(0), powell.getPoint(0));
                // Preferences.debug("Level 2, after " + degree + " DOF: " + item + "\n",Preferences.DEBUG_ALGORITHM);
            }
        }

        cost.disposeLocal();
        powell.disposeLocal();

        return item;
    }
}
