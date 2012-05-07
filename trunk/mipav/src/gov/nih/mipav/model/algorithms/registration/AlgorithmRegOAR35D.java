package gov.nih.mipav.model.algorithms.registration;

import Jama.Matrix;
import WildMagic.LibFoundation.Mathematics.Vector3f;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


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
 * <p>Internal registration is performed within one 4D image.</p>
 *
 * <p>In adjacent mode the first volume above the reference volume is registered to the reference volume, then the
 * second volume above the reference volume is registered to the first volume above the reference volume, and so on
 * until the last volume is registered the next to last volume. Then, the first volume below the reference volume is
 * registered to the reference volume, the second volume below the reference volume is registered to first volume below
 * the reference volume, and so on until the first volume is registered to the second volume. In reference mode every
 * volume is simply registered to the reference volume. In average mode every volume is simply registered to the average
 * of all the volumes.</p>
 *
 * <p>Our algorithm works as follows:<br>
 * 1.) We find the minimum resolution of the images.<br>
 * 2.) We transform the images into isotropic voxels.<br>
 * 3.) We subsample the images by 2, 4, and 8 or 16, depending on the resolution.<br>
 * Subsampling can be in x, y, and z or in only x and y. We loop thru a number of iterations equal to the number of
 * volumes - 1 for adjacent mode or equal to the number of volumes in average or reference mode with one volume as the
 * input volume and one volume as the reference volume. <br>
 * For each iteration: <br>
 * 4.) With the volumes that were subsampled by
 * 8 or 16, we call levelEight. This function will use the coarse sampling rate and optimize translations and global
 * scale at the given rotation. So for example, if the coarse sampling range were -30 to 30 at every 15 degrees, we
 * would optimize at rotations of -30, -15, 0, 15, 30.<br>
 * 5.) Still in levelEight, we now measure the cost at the fine sampling rate. We interpolate the translations and
 * global scale to come up with a good guess as to what the optimized translation would be at that point.<br>
 * 6.) We take the top 20% of the points and optimize them.<br>
 * 7.) We now have a large multi-array of costs. 20% of those have been optimized and placed back into their original
 * position in the multi-array. We look at the 9 neighbors of a point: +, =, or - one fine sample. If our point has a
 * cost greater than any of these, it is not a minima. Otherwise it is. We save it in a vector of minima.<br>
 * 8.) We optimize the minima over rotation as well as translations and global scale. (Previously we had not optimized
 * over rotation.) We return two vectors, one containing the minima before optimization, one containing the minima after
 * optimization.<br>
 * 9.) We now call levelFour with the volumes subsampled by 4 and the vectors of minima. We measure the costs of the
 * minima on the new volumes and sort them. We take the top numMinima in each vector (pre-optimization and
 * post-optimization) and optimize them. We put them all into one vector.<br>
 * 10.) We perturb the rotation by zero and by plus-minus fineDelta. If it's not a rigid transformation, we then perturb
 * the global scaling by factors of 0.8, 0.9, 1.0, 1.1, and 1.2.<br>
 * 11.) We optimize the perturbations. We return a vector of the perturbed, optimized minima.<br>
 * 12.) We now call levelTwo with the volumes subsampled by 2. We measure the costs of the minima at the new volumes. We
 * optimize the best minimum with 7 degrees of freedom, then 9, then 12. If the user has limited the degrees of freedom
 * to 6, there will only be one optimization run, with 6 degrees of freedom. The function returns the best minimum after
 * optimization.<br>
 * 13.) We call levelOne with the un-subsampled volumes. At levelOne, one optimization run is performed, with the
 * maximum allowable degrees of freedom, as specified by the user (the max is 12).<br>
 * 14.) The best answer is returned from levelOne. The inverse of the matrix from this answer is used to register the
 * input volume to the reference volume. The registered input volume data is imported into inputImage.<br>
 * </p>
 *
 * <p>Only subsample if 16 or more z slices are present, so that the number of z slices will not be reduced below 8.</p>
 *
 * @author  Matthew McAuliffe
 * @author  Neva Cherniavsky
 * @author  Benjamin Link - Added code to use less memory. (April 2003)
 */
public class AlgorithmRegOAR35D extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int ADJACENT = 1;

    /** DOCUMENT ME! */
    private static final int AVERAGE = 2;

    /** DOCUMENT ME! */
    private static final int minimumZForSub = 16;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean allowLevel16XY = true;

    /** DOCUMENT ME! */
    private boolean allowLevel16Z = true;

    /**
     * Flags are true if weighted image is not present or if weighted image subsampling occurred, false if weighted
     * image subsampling did not occur.
     */
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
    private float[] buffer;

    /** DOCUMENT ME! */
    private float[] bufferA;

    /** DOCUMENT ME! */
    private float[] bufferIW;

    /** DOCUMENT ME! */
    private float[] bufferW;

    /**
     * If true calculate the center of gravity (mass) and use the difference to intialize the translation. If false,
     * images are pretty much aligned and don't calculate COG.
     */
    private boolean calcCOG = true;

    /** Number of passes that will be made in the coarse sampling and fine sampling. */
    private int coarseNum, fineNum;

    /** 1 for black and white, 4 for color. */
    private int colorFactor;

    /** Choice of which cost function to use. */
    private int costChoice;

    /** DOCUMENT ME! */
    private boolean doColor;
    
    private TransMatrix [] VolumesToReferenceTransformations;

    /** Maximum degrees of freedom when running the optimization. */
    private int DOF;

    /**
     * Produce 2 output graphs - 1 for 3 rotations and 1 for 3 translations. Only can be used for DOF == 6 and register
     * to average or reference volume
     */
    private boolean doGraph;

    /** if true subsample. */
    private boolean doSubsample;

    /**
     * If true this algorithm skips all subsample and goes directly to the level 1 optimization. This assumes that
     * images are fairly well aligned to begin with and therefore no sophisticated search is needed.
     */
    private boolean fastMode = false;

    /** DOCUMENT ME! */
    private ModelImage imageWeightIso = null;

    /** Image used to import a volume from inputImage. */
    private ModelImage input1;

    /** This is the image in which internal registration will be performed. */
    private ModelImage inputImage;
    

    /** Image used to import a volume from inputWeight. */
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

    /**
     * Limits number of iterations in Powell optimization. maxIter in the call to Powell's will be an integer multiple
     * of baseNumIter
     */
    private int maxIter, baseNumIter;

    /** DOCUMENT ME! */
    private int mode; // 1 = adjacent, 2 = average, 3 = reference

    /** Number of minima from level 8 to test at level 4. */
    private int numMinima;

    /** DOCUMENT ME! */
    private ModelImage output_1;

    /** DOCUMENT ME! */
    private ModelImage outsidePreReferenceVolume = null;

    /** DOCUMENT ME! */
    private ModelImage outsideReferenceVolume = null;

    /** Indicates the image volume that all images are to be registered to, if mode == REFERENCE. */

    /** Indicates the first volume used as a reference volume, if mode == ADJACENT. */
    private int refImageNo;

    /**
     * If true use adjacent image for registration. If false use the average volume or use the image volume reference
     * number to indicate the image volume to register all images to.
     */
    private boolean regToAdjImage;

    /** true if resolutions unequal, false if resolutions equal. */
    private boolean resample;

    /** true if weight image is resampled. */
    private boolean resampleW;

    /** Arrays used for producing graphs for DOF == 6 and register to average or reference image. */
    private float[][] rot = null;

    /** Coarse and fine sampling parameters. */
    private float rotateBegin, rotateEnd, coarseRate, fineRate;

    /** Simple version of an image volume of the input image. */
    private ModelSimpleImage simpleInput_1;

    /** Simple version of an image volume of the input image, subsampled by 16. */
    private ModelSimpleImage simpleInputSub16_1;

    /** Simple version of an image volume of the input image, subsampled by 2. */
    private ModelSimpleImage simpleInputSub2_1;

    /** Simple version of an image volume of the input image, subsampled by 4. */
    private ModelSimpleImage simpleInputSub4_1;

    /** Simple version of an image volume of the input image, subsampled by 8. */
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
    private float[][] trans = null;

    /** DOCUMENT ME! */
    private boolean useOutsideReferenceVolume = false;

    /** Flag to determine if there are weighted images or not. */
    private boolean weighted;

    /** DOCUMENT ME! */
    private ModelImage weightVolumeImage = null;
    
    /** Turns the full version of Powell's algorithm on/off. When off the JTEM line minimization used.  */
    private boolean doJTEM = false;
    
    private boolean doMultiThread = false;

    private int[] TransMatsInumber;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new automatic internal registration algorithm and sets necessary variables.
     *
     * @param  _image         Input image
     * @param  _costChoice    Choice of cost functions, like correlation ratio or mutual information.
     * @param  _DOF           Degrees of freedom for registration
     * @param  _interp        Interpolation method used in transformations.
     * @param  _interp2       Interpolation method used in output
     * @param  mode           1 = adjacent, 2 = average, 3 = reference
     * @param  refImageNum    If mode == REFERENCE, the volume all other volumes are registered to. If mode == ADJACENT,
     *                        the first volume used as a reference.
     * @param  _rotateBegin   Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEnd     End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRate    Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRate      Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  doGraph        If true produce 2 output graphs - one for 3 rotations and one for 3 translations
     * @param  doSubsample    If true subsample
     * @param  fastMode       If true skip subsample and go directly to level 1 optimization
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     */
    public AlgorithmRegOAR35D(ModelImage _image, int _costChoice, int _DOF, int _interp, int _interp2, int mode,
                              int refImageNum, float _rotateBegin, float _rotateEnd, float _coarseRate, float _fineRate,
                              boolean doGraph, boolean doSubsample, boolean fastMode,
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

        this.mode = mode;

        if (mode == ADJACENT) {
            regToAdjImage = true;
        } else {
            regToAdjImage = false;
        }

        refImageNo = refImageNum;
        costChoice = _costChoice;
        DOF = _DOF;

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
        this.fastMode = fastMode;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;

        maxDim = inputImage.getExtents()[0];

        if (inputImage.getExtents()[1] > maxDim) {
            maxDim = inputImage.getExtents()[1];
        }

        if (inputImage.getExtents()[2] > maxDim) {
            maxDim = inputImage.getExtents()[2];
        }
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
     * @param  mode           1 = adjacent, 2 = average, 3 = average
     * @param  refImageNum    If mode == REFERENCE, the volume all other volumes are registered to. If mode == ADJACENT,
     *                        the first volume used as a reference.
     * @param  _rotateBegin   Beginning of coarse sampling range (i.e., -60 degrees).
     * @param  _rotateEnd     End of coarse sampling range (i.e., 60 degrees).
     * @param  _coarseRate    Point at which coarse samples should be taken (i.e., every 45 degrees).
     * @param  _fineRate      Point at which fine samples should be taken (i.e., every 15 degrees).
     * @param  doGraph        If true produce 2 output graphs - 1 for 3 rotations and one for 3 translations
     * @param  doSubsample    If true subsample
     * @param  fastMode       If true skip subsample and go directly to level 1 optimization
     * @param  _baseNumIter   Limits the number of iterations of Powell's algorithm. maxIter in the call to Powell's
     *                        will be an integer multiple of baseNumIter
     * @param  _numMinima     Number of minima from level 8 to test at level 4
     */
    public AlgorithmRegOAR35D(ModelImage _image, ModelImage _inputWeight, int _costChoice, int _DOF, int _interp,
                              int _interp2, int mode, int refImageNum, float _rotateBegin, float _rotateEnd,
                              float _coarseRate, float _fineRate, boolean doGraph, boolean doSubsample,
                              boolean fastMode, int _baseNumIter, int _numMinima) {

        super(null, _image);

        inputImage = _image;

        if (inputImage.isColorImage()) {
            doColor = true;
            colorFactor = 4;
        } else {
            doColor = false;
            colorFactor = 1;
        }

        this.mode = mode;

        if (mode == ADJACENT) {
            regToAdjImage = true;
        } else {
            regToAdjImage = false;
        }

        refImageNo = refImageNum;
        inputWeight = _inputWeight;
        costChoice = _costChoice;
        DOF = _DOF;

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
        this.fastMode = fastMode;
        baseNumIter = _baseNumIter;
        numMinima = _numMinima;

        maxDim = inputImage.getExtents()[0];

        if (inputImage.getExtents()[1] > maxDim) {
            maxDim = inputImage.getExtents()[1];
        }

        if (inputImage.getExtents()[2] > maxDim) {
            maxDim = inputImage.getExtents()[2];
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Turns the full version of Powell's algorithm on/off.
     * @param bOn
     */
    public void setJTEM(boolean bOn)
    {
    	doJTEM = bOn;
    }

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

        if (inputw_1 != null) {
            inputw_1.disposeLocal();
        }

        if (output_1 != null) {
            output_1.disposeLocal();
        }

        if (weightVolumeImage != null) {
            weightVolumeImage.disposeLocal();
        }

        if (isoImage != null) {
            isoImage.disposeLocal();
        }

        if (imageWeightIso != null) {
            imageWeightIso.disposeLocal();
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
        inputw_1 = null;
        output_1 = null;
        weightVolumeImage = null;

        //May comment this out at some point
        //inputImage = null;
        buffer = null;
        bufferIW = null;
        bufferW = null;
        bufferA = null;

        //answer = null;

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
     * accessor for rot.
     *
     * @return  rot
     */
    public float[][] getRot() {
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
     * Sorts the array of transformation matricies and returns the sorted array.
     * If the sub-volumes are sorted to a sub-volume within the 4D image, then the matrix
     * for the reference volume is the identity matrix.
     * @return Array of TransMatrix, one for each sub-volume in the 4D image.
     */
    public TransMatrix[] getArrayTransMatrix(){
        boolean useIndexNumBreak = false;
        int indexNumBreak = 0;
        int counter = 0;
        ArrayList<TransMatrix> arrayList1 = new ArrayList<TransMatrix>() ;
        ArrayList<TransMatrix> arrayList2 = new ArrayList<TransMatrix>() ;
        
        if (TransMatsInumber[0] != 1){
            if (TransMatsInumber[0]>TransMatsInumber[1]){
                useIndexNumBreak = true;
                for (int i = 0; i< inputImage.getExtents()[3]-1;i++){
                    if (i < TransMatsInumber[i]){
                        indexNumBreak++;
                        }
                }
            }
            //Sorts trans matrices based on volume number
            for (int i = 0; i< inputImage.getExtents()[3]-1;i++){
                if (i < TransMatsInumber[i]){
                    if (useIndexNumBreak == false){
                        arrayList1.add(i,VolumesToReferenceTransformations[i]);
                    }
                    else{
                        arrayList1.add(i,VolumesToReferenceTransformations[(indexNumBreak-1)-i]);
                    }
                }               
                else if (i > TransMatsInumber[i]){
                    arrayList2.add(counter,VolumesToReferenceTransformations[(inputImage.getExtents()[3]-2)-counter]);
                    counter++;
                }
            }       
            arrayList2.addAll(arrayList1);
            TransMatrix transIndentity = new TransMatrix(4,4);
            //Creates identity matrix
            for(int i = 0; i<4;i++){
                for(int j = 0; j<4;j++){
                    if (i==j){
                        transIndentity.set(i, j, 1.0); 
                    }
                    else{
                        transIndentity.set(i, j, 0.0); 
                    }
                }
            }

            TransMatrix[] sortedTransArray = new TransMatrix[inputImage.getExtents()[3]];
            int volCounter = 0;
            for(int i=0; i< arrayList2.size()+1; i++){
                //Adds identity matrix corresponding to reference volume number
                if (i==refImageNo){
                    sortedTransArray[i] =  transIndentity; 
                    volCounter = 1;
                }
                else{
                    sortedTransArray[i] = arrayList2.get(i-volCounter);
                }
            }

            
           return sortedTransArray;
        }
        else{       
            return VolumesToReferenceTransformations;
        }
    }
    
    
    public TransMatrix getTransform() {
        return answer.matrix;
    }
    
    public double getAnswer() {
        return answer.cost;
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
     * resolutions of the two images after the xyz isotropic transformation will be the same in the x, y, and z
     * dimensions. That resolution will equal the minimum resolution. If the image is weighted, the weight image is
     * transformed into isotropic pixels in the same manner as the original. Then the image is subsampled by 2, 4, and 8
     * or 16. If the image is too small it will not be subsampled down to the smallest level; if it is too big, it will
     * be subsampled to 8 or 16. The same is done with the weight image if necessary. The program loops thru levelEight,
     * levelFour, levelTwo, and levelOne with one volume as the reference volume and one volume as the input volume. The
     * function levelEight is called with the volumes subsampled by 8 or 16; it returns two vectors with minima. Then
     * the function levelFour is called with volumes subsampled by 4 and the two vectors; it returns one vector of
     * minima. The function levelTwo is called with volumes subsampled by 2 and the vector; it returns an "answer" in
     * the form of a MatrixListItem, which is a convenient way of storing the point, the matrix, and the cost of the
     * minimum. Then the function levelOne is called with the minimum; it returns a final "answer", or minimum, which is
     * used to register the input volume to the reference volume. The registered input volume data is imported into
     * inputImage.
     */
    public void runAlgorithm() {
        int i;
        int iNumber;
        int rNumber;
        boolean seriesUp;
        int[] extents = new int[3];
        int[] iExtents = new int[3];
        float[] resols = new float[3];
        float[] distance = new float[3];
        AlgorithmTransform transform = null;
        int[] extentsIso = null;
        float[] resIso = null;
        int volumeSize;
        MatrixListItem item;
        


        if (inputImage.getNDims() != 4) {
            MipavUtil.displayError("" + inputImage.getNDims() + "D registration not supported.");
            disposeLocal();

            return;
        }

        doMultiThread = Preferences.isMultiThreadingEnabled()  &&
		(ThreadUtil.getAvailableCores() > 1);
        
        Preferences.debug(getConstructionInfo(),Preferences.DEBUG_ALGORITHM);

        fireProgressStateChanged("Registering images", "Beginning registration");


        if (doGraph) {
            rot = new float[3][inputImage.getExtents()[3]];
            rot[0][refImageNo] = 0.0f;
            rot[1][refImageNo] = 0.0f;
            rot[2][refImageNo] = 0.0f;
            trans = new float[3][inputImage.getExtents()[3]];
            trans[0][refImageNo] = 0.0f;
            trans[1][refImageNo] = 0.0f;
            trans[2][refImageNo] = 0.0f;
        } // if (doGraph)

        float minSample = Math.min(iResols[0], Math.min(iResols[1], iResols[2]));

        try {
            extentsIso = new int[3];
            resIso = new float[3];
            buffer = new float[colorFactor * inputImage.getSliceSize() * inputImage.getExtents()[2]];
        } catch (OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("Out of memory in AlgorithmOAR35D creating extentsIso and resIso and buffer");
            disposeLocal();

            return;
        }

        for (i = 0; i < 3; i++) {
            extentsIso[i] = (int) Math.round(((inputImage.getExtents()[i] - 1) / (minSample / iResols[i])) + 1);
            resIso[i] = minSample;
        }

        if (weighted) {

            try {
                bufferIW = new float[inputWeight.getSliceSize() * inputWeight.getExtents()[2]];
            } catch (OutOfMemoryError e) {
                System.gc();
                MipavUtil.displayError("Out of memory in AlgorithmOAR35D creating bufferIW");
                disposeLocal();

                return;
            }

            try {
                bufferW = new float[extentsIso[0] * extentsIso[1] * extentsIso[2]];
            } catch (OutOfMemoryError e) {
                System.gc();
                MipavUtil.displayError("Out of memory in AlgorithmOAR35D creating bufferW");
                disposeLocal();

                return;
            }

        } // if (weighted)

        if ((iResols[0] == iResols[1]) && (iResols[0] == iResols[2])) {
            resample = false;

            // must set extents & resolutions here
            extents[0] = inputImage.getExtents()[0];
            extents[1] = inputImage.getExtents()[1];
            extents[2] = inputImage.getExtents()[2];
            resols[0] = inputImage.getFileInfo()[0].getResolutions()[0];
            resols[1] = inputImage.getFileInfo()[0].getResolutions()[1];
            resols[2] = inputImage.getFileInfo()[0].getResolutions()[2];
        } else {
            resample = true;

            // 3.5D interpolation
            fireProgressStateChanged("Interpolating input image to obtain equal x, y, and z resolutions");
            transform = new AlgorithmTransform(inputImage, new TransMatrix(4), interp, resIso[0], resIso[1], resIso[2],
                                               extentsIso[0], extentsIso[1], extentsIso[2], false, true, false);
            transform.run();
            

            if (transform.isCompleted() == false) {
                setCompleted(false);
                finalize();

                return;
            }

            isoImage = transform.getTransformedImage();
            

            

            if (transform != null) {
                transform.finalize();
            }

            // must set extents & resolutions here
            extents[0] = isoImage.getExtents()[0];
            extents[1] = isoImage.getExtents()[1];
            extents[2] = isoImage.getExtents()[2];
            resols[0] = isoImage.getFileInfo()[0].getResolutions()[0];
            resols[1] = isoImage.getFileInfo()[0].getResolutions()[1];
            resols[2] = isoImage.getFileInfo()[0].getResolutions()[2];

        } // iResols[0] != iResols[1]

        iExtents[0] = inputImage.getExtents()[0];
        iExtents[1] = inputImage.getExtents()[1];
        iExtents[2] = inputImage.getExtents()[2];
        volumeSize = iExtents[0] * iExtents[1] * iExtents[2];

        // still use simpleInput_1 (1 volume at a time)
        simpleInput_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image

        // check if outside reference volume and resampling are both used
        if (useOutsideReferenceVolume && resample) {
            transform = new AlgorithmTransform(outsidePreReferenceVolume, new TransMatrix(4), interp, resIso[0],
                                               resIso[1], resIso[2], extentsIso[0], extentsIso[1], extentsIso[2], false,
                                               true, false);
            transform.run();

            if (transform.isCompleted() == false) {
                setCompleted(false);
                finalize();

                return;
            }

            outsideReferenceVolume = transform.getTransformedImage();

            if (transform != null) {
                transform.finalize();
            }
        } // if (useOutsideReferenceVolume && resample)
        else if (useOutsideReferenceVolume) {
            outsideReferenceVolume = outsidePreReferenceVolume;
        }

        if (mode == AVERAGE) {
            float[] averageBuffer;
            float[] averageBuffer2;
            int vSize = extents[0] * extents[1] * extents[2];
            useOutsideReferenceVolume = true;
            ModelImage averageImage;

            try {
                averageBuffer = new float[colorFactor * vSize];
                averageBuffer2 = new float[colorFactor * vSize];
            } catch (OutOfMemoryError e) {
                System.gc();
                MipavUtil.displayError("Out of memory in AlgorithmOAR35D creating averageBuffer and averageBuffer2");
                disposeLocal();

                return;
            }

            for (i = 0; i < averageBuffer2.length; i++) {
                averageBuffer2[i] = 0.0f;
            }
            
            if (isoImage !=  null) {
                averageImage = isoImage;
            }
            else {
                averageImage = inputImage;
            }

            for (i = 0; i < averageImage.getExtents()[3]; i++) {

                try {
                    averageImage.exportData(i * colorFactor * vSize, colorFactor * vSize, averageBuffer);
                } catch (IOException ex) {
                    System.gc();
                    MipavUtil.displayError("IOException = " + ex + " on averageImage.exportData");
                    disposeLocal();

                    return;
                }

                for (i = 0; i < averageBuffer.length; i++) {
                    averageBuffer2[i] += averageBuffer[i];
                }
            } // for (i = 0; i < averageImage.getExtents()[3]; i++)

            averageBuffer = null;

            for (i = 0; i < averageBuffer2.length; i++) {
                averageBuffer2[i] /= averageImage.getExtents()[3];
            }

            outsideReferenceVolume = new ModelImage(averageImage.getType(), extents, "average_volume");
            outsideReferenceVolume.getFileInfo()[0].setResolutions(resols);

            try {
                outsideReferenceVolume.importData(0, averageBuffer2, true);
            } catch (IOException ex) {
                System.gc();
                MipavUtil.displayError("IOException = " + ex + " on outsideReferenceVolume.importData");
                disposeLocal();

                return;
            }

            averageBuffer2 = null;
        } // if (mode == AVERAGE)

        simpleRef_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image

        if (doColor) {
            input1 = new ModelImage(ModelImage.ARGB_FLOAT, iExtents, "volume3D");
        } else {
            input1 = new ModelImage(ModelImage.FLOAT, iExtents, "volume3D");
        }

        input1.getFileInfo()[0].setResolutions(iResols);

        if (weighted) {
            inputw_1 = new ModelImage(ModelImage.FLOAT, iExtents, "volume3DW");
            inputw_1.getFileInfo()[0].setResolutions(iResols);
        }

        if (resample && regToAdjImage) {

            try {
                bufferA = new float[colorFactor * extentsIso[0] * extentsIso[1] * extentsIso[2]];
            } catch (OutOfMemoryError e) {
                System.gc();
                MipavUtil.displayError("Out of memory in AlgorithmOAR35D creating bufferA");
                disposeLocal();

                return;
            }

        } // if (resample && regToAdjImage)

        int subMinFactor = 75000;

        if (weighted) {

            if (resample || (inputWeight.getFileInfo(0).getResolutions()[0] != iResols[0]) ||
                    (inputWeight.getFileInfo(0).getResolutions()[1] != iResols[1]) ||
                    (inputWeight.getFileInfo(0).getResolutions()[2] != iResols[2])) {

                // 3.5 interpolation
                resampleW = true;
                fireProgressStateChanged("Performing interpolation on input weight image");
                transform = new AlgorithmTransform(inputWeight, new TransMatrix(4), interp, resIso[0], resIso[1],
                                                   resIso[2], extentsIso[0], extentsIso[1], extentsIso[2], false, true,
                                                   false);
                transform.run();

                if (transform.isCompleted() == false) {
                    setCompleted(false);
                    finalize();

                    return;
                }

                imageWeightIso = transform.getTransformedImage();

                if (transform != null) {
                    transform.finalize();
                }

                // build extents and resolutions from transformed weight image
                extents[0] = imageWeightIso.getExtents()[0];
                extents[1] = imageWeightIso.getExtents()[1];
                extents[2] = imageWeightIso.getExtents()[2];
                resols[0] = imageWeightIso.getFileInfo()[0].getResolutions()[0];
                resols[1] = imageWeightIso.getFileInfo()[0].getResolutions()[1];
                resols[2] = imageWeightIso.getFileInfo()[0].getResolutions()[2];
            } else {
                resampleW = false;
                extents[0] = inputWeight.getExtents()[0];
                extents[1] = inputWeight.getExtents()[1];
                extents[2] = inputWeight.getExtents()[2];
                resols[0] = inputWeight.getFileInfo()[0].getResolutions()[0];
                resols[1] = inputWeight.getFileInfo()[0].getResolutions()[1];
                resols[2] = inputWeight.getFileInfo()[0].getResolutions()[2];
            }

            distance[0] = extents[0] * resols[0];
            distance[1] = extents[1] * resols[1];
            distance[2] = extents[2] * resols[2];
            simpleWeightR_1 = new ModelSimpleImage(extents, resols); // 3D simple image
            simpleWeightI_1 = new ModelSimpleImage(extents, resols); // 3D simple image

            if (!fastMode) {

                if ((simpleWeightR_1.dataSize > subMinFactor) && doSubsample &&
                        (simpleWeightR_1.zDim >= minimumZForSub)) {

                    // dont need simpleWeightSub2 (only single volumes)
                    extents[0] /= 2;
                    extents[1] /= 2;
                    extents[2] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    resols[2] = distance[2] / extents[2];
                    simpleWeightRSub2_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    simpleWeightISub2_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                } else if ((simpleWeightR_1.dataSize > subMinFactor) && doSubsample) {

                    // dont need simpleWeightSub2 (only single volumes)
                    extents[0] /= 2;
                    extents[1] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    simpleWeightRSub2_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    simpleWeightISub2_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    allowLevel2Z = false;
                    allowLevel4Z = false;
                    allowLevel8Z = false;
                    allowLevel16Z = false;
                } else {
                    simpleWeightRSub2_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    simpleWeightISub2_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    allowLevel2XY = false;
                    allowLevel2Z = false;
                    allowLevel4XY = false;
                    allowLevel4Z = false;
                    allowLevel8XY = false;
                    allowLevel8Z = false;
                    allowLevel16XY = false;
                    allowLevel16Z = false;
                }

                if ((simpleWeightRSub2_1.dataSize > subMinFactor) && doSubsample &&
                        (simpleWeightRSub2_1.zDim >= minimumZForSub)) {
                    extents[0] /= 2;
                    extents[1] /= 2;
                    extents[2] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    resols[2] = distance[2] / extents[2];
                    simpleWeightRSub4_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    simpleWeightISub4_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                } else if ((simpleWeightRSub2_1.dataSize > subMinFactor) && doSubsample) {
                    extents[0] /= 2;
                    extents[1] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    simpleWeightRSub4_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    simpleWeightISub4_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    allowLevel4Z = false;
                    allowLevel8Z = false;
                    allowLevel16Z = false;
                } else {
                    simpleWeightRSub4_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    simpleWeightISub4_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    allowLevel4XY = false;
                    allowLevel4Z = false;
                    allowLevel8XY = false;
                    allowLevel8Z = false;
                    allowLevel16XY = false;
                    allowLevel16Z = false;
                }

                if ((simpleWeightRSub4_1.dataSize > subMinFactor) && doSubsample &&
                        (simpleWeightRSub4_1.zDim >= minimumZForSub)) {

                    extents[0] = extents[0] / 2;
                    extents[1] = extents[1] / 2;
                    extents[2] = extents[2] / 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    resols[2] = distance[2] / extents[2];
                    simpleWeightRSub8_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    simpleWeightISub8_1 = new ModelSimpleImage(extents, resols); // 3D simple image

                    if ((simpleWeightRSub8_1.dataSize > subMinFactor) && (simpleWeightRSub8_1.zDim >= minimumZForSub)) {
                        extents[0] = extents[0] / 2;
                        extents[1] = extents[1] / 2;
                        extents[2] = extents[2] / 2;
                        resols[0] = distance[0] / extents[0];
                        resols[1] = distance[1] / extents[1];
                        resols[2] = distance[2] / extents[2];
                        simpleWeightRSub16_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                        simpleWeightISub16_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    } else if (simpleWeightRSub8_1.dataSize > subMinFactor) {
                        extents[0] = extents[0] / 2;
                        extents[1] = extents[1] / 2;
                        resols[0] = distance[0] / extents[0];
                        resols[1] = distance[1] / extents[1];
                        simpleWeightRSub16_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                        simpleWeightISub16_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                        allowLevel16Z = false;
                    } else {
                        allowLevel16XY = false;
                        allowLevel16Z = false;
                    }
                } else if ((simpleWeightRSub4_1.dataSize > subMinFactor) && doSubsample) {
                    allowLevel8Z = false;
                    allowLevel16Z = false;

                    extents[0] = extents[0] / 2;
                    extents[1] = extents[1] / 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    simpleWeightRSub8_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    simpleWeightISub8_1 = new ModelSimpleImage(extents, resols); // 3D simple image

                    if (simpleWeightRSub8_1.dataSize > subMinFactor) {
                        extents[0] = extents[0] / 2;
                        extents[1] = extents[1] / 2;
                        resols[0] = distance[0] / extents[0];
                        resols[1] = distance[1] / extents[1];
                        simpleWeightRSub16_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                        simpleWeightISub16_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    } else {
                        allowLevel16XY = false;
                    }
                } else {
                    simpleWeightRSub8_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    simpleWeightISub8_1 = new ModelSimpleImage(extents, resols); // 3D simple image
                    allowLevel8XY = false;
                    allowLevel8Z = false;
                    allowLevel16XY = false;
                    allowLevel16Z = false;
                }
            } // if (!fastMode)
        } // if (weighted)

        if (!fastMode) {

            if (doColor) {
                subMinFactor *= 4;
            }

            extents[0] = simpleInput_1.xDim;
            extents[1] = simpleInput_1.yDim;
            extents[2] = simpleInput_1.zDim;
            resols[0] = simpleInput_1.xRes;
            resols[1] = simpleInput_1.yRes;
            resols[2] = simpleInput_1.zRes;
            distance[0] = extents[0] * resols[0];
            distance[1] = extents[1] * resols[1];
            distance[2] = extents[2] * resols[2];

            if ((simpleInput_1.dataSize > subMinFactor) && allowLevel2XY && allowLevel2Z && doSubsample &&
                    (simpleInput_1.zDim >= minimumZForSub)) {
                extents[0] /= 2;
                extents[1] /= 2;
                extents[2] /= 2;
                resols[0] = distance[0] / extents[0];
                resols[1] = distance[1] / extents[1];
                resols[2] = distance[2] / extents[2];
                simpleInputSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                simpleRefSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image

                level1FactorXY = 2.0f;
                level1FactorZ = 2.0f;
            } else if ((simpleInput_1.dataSize > subMinFactor) && allowLevel2XY && doSubsample) {
                extents[0] /= 2;
                extents[1] /= 2;
                resols[0] = distance[0] / extents[0];
                resols[1] = distance[1] / extents[1];
                simpleInputSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                simpleRefSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image

                level1FactorXY = 2.0f;
                allowLevel2Z = false;
                allowLevel4Z = false;
                allowLevel8Z = false;
                allowLevel16Z = false;
            } else {
                simpleInputSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                simpleRefSub2_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                allowLevel2XY = false;
                allowLevel2Z = false;
                allowLevel4XY = false;
                allowLevel4Z = false;
                allowLevel8XY = false;
                allowLevel8Z = false;
                allowLevel16XY = false;
                allowLevel16Z = false;
            }

            if ((simpleInputSub2_1.dataSize > subMinFactor) && allowLevel4XY && allowLevel4Z && doSubsample &&
                    (simpleInputSub2_1.zDim >= minimumZForSub)) {
                extents[0] /= 2;
                extents[1] /= 2;
                extents[2] /= 2;
                resols[0] = distance[0] / extents[0];
                resols[1] = distance[1] / extents[1];
                resols[2] = distance[2] / extents[2];
                simpleInputSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                simpleRefSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                level2FactorXY = 2.0f;
                level2FactorZ = 2.0f;
            } else if ((simpleInputSub2_1.dataSize > subMinFactor) && allowLevel4XY && doSubsample) {
                extents[0] /= 2;
                extents[1] /= 2;
                resols[0] = distance[0] / extents[0];
                resols[1] = distance[1] / extents[1];
                simpleInputSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                simpleRefSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                level2FactorXY = 2.0f;
                allowLevel4Z = false;
                allowLevel8Z = false;
                allowLevel16Z = false;
            } else {
                simpleInputSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                simpleRefSub4_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                allowLevel4XY = false;
                allowLevel4Z = false;
                allowLevel8XY = false;
                allowLevel8Z = false;
                allowLevel16XY = false;
                allowLevel16Z = false;
            }

            if ((simpleInputSub4_1.dataSize > subMinFactor) && allowLevel8XY && allowLevel8Z && doSubsample &&
                    (simpleInputSub4_1.zDim >= minimumZForSub)) {
                extents[0] /= 2;
                extents[1] /= 2;
                extents[2] /= 2;
                resols[0] = distance[0] / extents[0];
                resols[1] = distance[1] / extents[1];
                resols[2] = distance[2] / extents[2];
                simpleInputSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                simpleRefSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image

                level4FactorXY = 2.0f;
                level4FactorZ = 2.0f;

                if ((simpleInputSub8_1.dataSize > subMinFactor) && allowLevel16XY && allowLevel16Z &&
                        (simpleInputSub8_1.zDim >= minimumZForSub)) {
                    level4FactorXY = 4.0f;
                    level4FactorZ = 4.0f;
                    extents[0] /= 2;
                    extents[1] /= 2;
                    extents[2] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    resols[2] = distance[2] / extents[2];
                    simpleInputSub16_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                    simpleRefSub16_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                } else if ((simpleInputSub8_1.dataSize > subMinFactor) && allowLevel16XY) {
                    level4FactorXY = 4.0f;
                    extents[0] /= 2;
                    extents[1] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    simpleInputSub16_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                    simpleRefSub16_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                    allowLevel16Z = false;
                } else {
                    allowLevel16XY = false;
                    allowLevel16Z = false;
                }
            } else if ((simpleInputSub4_1.dataSize > subMinFactor) && allowLevel8XY && doSubsample) {
                allowLevel8Z = false;
                allowLevel16Z = false;
                extents[0] /= 2;
                extents[1] /= 2;
                resols[0] = distance[0] / extents[0];
                resols[1] = distance[1] / extents[1];
                simpleInputSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                simpleRefSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image

                level4FactorXY = 2.0f;

                if ((simpleInputSub8_1.dataSize > subMinFactor) && allowLevel16XY) {
                    level4FactorXY = 4.0f;
                    extents[0] /= 2;
                    extents[1] /= 2;
                    resols[0] = distance[0] / extents[0];
                    resols[1] = distance[1] / extents[1];
                    simpleInputSub16_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                    simpleRefSub16_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                } else {
                    allowLevel16XY = false;
                }
            } else {
                simpleInputSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                simpleRefSub8_1 = new ModelSimpleImage(extents, resols, doColor); // 3D simple image
                allowLevel8XY = false;
                allowLevel8Z = false;
                allowLevel16XY = false;
                allowLevel16Z = false;
            }

            Preferences.debug("Level 1 factor XY = " + level1FactorXY + "\n" + "Level 1 factor Z = " + level1FactorZ +
                              "\n" + "Level 2 factor XY = " + level2FactorXY + "\n" + "Level 2 factor Z = " +
                              level2FactorZ + "\n" + "level 4 factor XY = " + level4FactorXY + "\n" +
                              "Level 4 factor Z = " + level4FactorZ + "\n",Preferences.DEBUG_ALGORITHM);
        } // if (!fastMode)

        long time;

        rNumber = refImageNo;

        if ((refImageNo == (inputImage.getExtents()[3] - 1)) && !useOutsideReferenceVolume) {
            iNumber = refImageNo - 1;
            seriesUp = false;
        } else if (useOutsideReferenceVolume) {
            iNumber = 0;
            seriesUp = true;
        } else {
            iNumber = refImageNo + 1;
            seriesUp = true;
        }

        // get the reference volume from the image and then get the subsampled versions
        try {

            // if image was transformed, we use the isoImage not inputImage
            if (useOutsideReferenceVolume) {
                outsideReferenceVolume.exportData(0, simpleRef_1.data.length, simpleRef_1.data);
            } else if (isoImage != null) {
                isoImage.exportData(rNumber * simpleRef_1.data.length, simpleRef_1.data.length, simpleRef_1.data);
            } else {
                inputImage.exportData(rNumber * simpleRef_1.data.length, simpleRef_1.data.length, simpleRef_1.data);
            }

            if (!fastMode) {

                // get subsample by 3'd volume (if allowLevel2)
                // otherwise just copy the ref volume into simpleRefSub2_1
                if (allowLevel2XY && allowLevel2Z) {
                    subSampleBy2(simpleRef_1, simpleRefSub2_1, doColor);
                } else if (allowLevel2XY) {
                    subSampleBy2XY(simpleRef_1, simpleRefSub2_1, doColor);
                } else {
                    copyFloatData(simpleRef_1, simpleRefSub2_1);
                }

                if (allowLevel4XY && allowLevel4Z) {
                    subSampleBy2(simpleRefSub2_1, simpleRefSub4_1, doColor);
                } else if (allowLevel4XY) {
                    subSampleBy2XY(simpleRefSub2_1, simpleRefSub4_1, doColor);
                } else {
                    copyFloatData(simpleRefSub2_1, simpleRefSub4_1);
                }

                if (allowLevel8XY && allowLevel8Z) {
                    subSampleBy2(simpleRefSub4_1, simpleRefSub8_1, doColor);
                } else if (allowLevel8XY) {
                    subSampleBy2XY(simpleRefSub4_1, simpleRefSub8_1, doColor);
                } else {
                    copyFloatData(simpleRefSub4_1, simpleRefSub8_1);
                }

                if (allowLevel16XY && allowLevel16Z) {
                    subSampleBy2(simpleRefSub8_1, simpleRefSub16_1, doColor);
                } else if (allowLevel16XY) {
                    subSampleBy2XY(simpleRefSub8_1, simpleRefSub16_1, doColor);
                }
            } // if (!fastMode)

            if (weighted && !useOutsideReferenceVolume) {

                if (imageWeightIso != null) {
                    imageWeightIso.exportData(rNumber * simpleWeightR_1.data.length, simpleWeightR_1.data.length,
                                              simpleWeightR_1.data);
                } else {
                    inputWeight.exportData(rNumber * simpleWeightR_1.data.length, simpleWeightR_1.data.length,
                                           simpleWeightR_1.data);
                }

                if (!fastMode) {

                    // get subsample by 3'd volume (if allowLevel2)
                    // otherwise just copy the ref volume into simpleRefSub2_1
                    if (allowLevel2XY && allowLevel2Z) {
                        subSampleBy2(simpleWeightR_1, simpleWeightRSub2_1, false);
                    } else if (allowLevel2XY) {
                        subSampleBy2XY(simpleWeightR_1, simpleWeightRSub2_1, false);
                    } else {
                        copyFloatData(simpleWeightR_1, simpleWeightRSub2_1);
                    }

                    if (allowLevel4XY && allowLevel4Z) {
                        subSampleBy2(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                    } else if (allowLevel4XY) {
                        subSampleBy2XY(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                    } else {
                        copyFloatData(simpleWeightRSub2_1, simpleWeightRSub4_1);
                    }

                    if (allowLevel8XY && allowLevel8Z) {
                        subSampleBy2(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                    } else if (allowLevel8XY) {
                        subSampleBy2XY(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                    } else {
                        copyFloatData(simpleWeightRSub4_1, simpleWeightRSub8_1);
                    }

                    if (allowLevel16XY && allowLevel16Z) {
                        subSampleBy2(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                    } else if (allowLevel16XY) {
                        subSampleBy2XY(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                    }
                } // if (!fastMode)
            } // if (weighted && !useOutsideReferenceVolume)

        } catch (IOException ex) {
            System.err.println("Caught IOException in RegOAR35D");
            ex.toString();
        }

        int endIndex = inputImage.getExtents()[3] - 1;
        
        VolumesToReferenceTransformations= new TransMatrix[inputImage.getExtents()[3]-1];
        TransMatsInumber = new int[inputImage.getExtents()[3]-1];

        if (useOutsideReferenceVolume) {
            endIndex++;
        }

        for (int m = 0; m < endIndex; m++) {
            fireProgressStateChanged("Registering image " + (iNumber));
            fireProgressStateChanged((int) (m / (float) (endIndex) * 100));

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

                if (!fastMode) {

                    if (allowLevel2XY && allowLevel2Z) {
                        subSampleBy2(simpleInput_1, simpleInputSub2_1, doColor);
                    } else if (allowLevel2XY) {
                        subSampleBy2XY(simpleInput_1, simpleInputSub2_1, doColor);
                    } else {
                        copyFloatData(simpleInput_1, simpleInputSub2_1);
                    }

                    if (allowLevel4XY && allowLevel4Z) {
                        subSampleBy2(simpleInputSub2_1, simpleInputSub4_1, doColor);
                    } else if (allowLevel4XY) {
                        subSampleBy2XY(simpleInputSub2_1, simpleInputSub4_1, doColor);
                    } else {
                        copyFloatData(simpleInputSub2_1, simpleInputSub4_1);
                    }

                    if (allowLevel8XY && allowLevel8Z) {
                        subSampleBy2(simpleInputSub4_1, simpleInputSub8_1, doColor);
                    } else if (allowLevel8XY) {
                        subSampleBy2XY(simpleInputSub4_1, simpleInputSub8_1, doColor);
                    } else {
                        copyFloatData(simpleInputSub4_1, simpleInputSub8_1);
                    }

                    if (allowLevel16XY && allowLevel16Z) {
                        subSampleBy2(simpleInputSub8_1, simpleInputSub16_1, doColor);
                    } else if (allowLevel16XY) {
                        subSampleBy2XY(simpleInputSub8_1, simpleInputSub16_1, doColor);
                    }
                } // if (!fastMode)

                simpleRef_1.calcMinMax();
                simpleInput_1.calcMinMax();

                if (!fastMode) {
                    simpleRefSub2_1.calcMinMax();
                    simpleInputSub2_1.calcMinMax();
                    simpleRefSub4_1.calcMinMax();
                    simpleInputSub4_1.calcMinMax();
                    simpleRefSub8_1.calcMinMax();
                    simpleInputSub8_1.calcMinMax();

                    if (allowLevel16XY) {
                        simpleRefSub16_1.calcMinMax();
                        simpleInputSub16_1.calcMinMax();
                    }
                } // if (!fastMode)

                if (weighted) {

                    if (imageWeightIso != null) {
                        imageWeightIso.exportData(rNumber * simpleWeightI_1.data.length, simpleWeightI_1.data.length,
                                                  simpleWeightI_1.data);
                    } else {
                        inputWeight.exportData(rNumber * simpleWeightI_1.data.length, simpleWeightI_1.data.length,
                                               simpleWeightI_1.data);
                    }

                    if (!fastMode) {

                        if (allowLevel2XY && allowLevel2Z) {
                            subSampleBy2(simpleWeightI_1, simpleWeightISub2_1, false);
                        } else if (allowLevel2XY) {
                            subSampleBy2XY(simpleWeightI_1, simpleWeightISub2_1, false);
                        } else {
                            copyFloatData(simpleWeightI_1, simpleWeightISub2_1);
                        }

                        if (allowLevel4XY && allowLevel4Z) {
                            subSampleBy2(simpleWeightISub2_1, simpleWeightISub4_1, false);
                        } else if (allowLevel4XY) {
                            subSampleBy2XY(simpleWeightISub2_1, simpleWeightISub4_1, false);
                        } else {
                            copyFloatData(simpleWeightISub2_1, simpleWeightISub4_1);
                        }

                        if (allowLevel8XY && allowLevel8Z) {
                            subSampleBy2(simpleWeightISub4_1, simpleWeightISub8_1, false);
                        } else if (allowLevel8XY) {
                            subSampleBy2XY(simpleWeightISub4_1, simpleWeightISub8_1, false);
                        } else {
                            copyFloatData(simpleWeightISub4_1, simpleWeightISub8_1);
                        }

                        if (allowLevel16XY && allowLevel16Z) {
                            subSampleBy2(simpleWeightISub8_1, simpleWeightISub16_1, false);
                        } else if (allowLevel16XY) {
                            subSampleBy2XY(simpleWeightISub8_1, simpleWeightISub16_1, false);
                        }
                    } // if (!fastMode)
                }
            } catch (IOException ex) {
                System.err.println("Caught IOException in RegOAR35D");
                ex.toString();
                ex.printStackTrace();
            }

            if (!fastMode) {
                time = System.currentTimeMillis();
                Preferences.debug(" Starting level 8 ************************************************\n",
                		Preferences.DEBUG_ALGORITHM);

                Vector<MatrixListItem>[] minimas;

                if (allowLevel16XY) {
                    minimas = levelEight(simpleRefSub16_1, simpleInputSub16_1);
                } else {
                    minimas = levelEight(simpleRefSub8_1, simpleInputSub8_1);
                }

                time = System.currentTimeMillis() - time;
                Preferences.debug(" Level 8 min = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);
                time = System.currentTimeMillis();

                if (threadStopped) {
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
                    finalize();

                    return;
                }

                Preferences.debug(" Starting level 2 ************************************************\n",
                		Preferences.DEBUG_ALGORITHM);
                item = levelTwo(simpleRefSub2_1, simpleInputSub2_1, minima);
                time = System.currentTimeMillis() - time;
                Preferences.debug(" Level 2 min = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);

                if (threadStopped) {
                    finalize();

                    return;
                }
            } // if (!fastMode)
            else { // fastMode

                double[] initial = new double[12];
                item = new MatrixListItem(0, new TransMatrix(4), initial);

                double diffX = 0;
                double diffY = 0;
                double diffZ = 0;

                if (calcCOG == true) {
                    Vector3f cog = calculateCenterOfMass3D(simpleInput_1, simpleWeightI_1, doColor);
                    Vector3f cogR = calculateCenterOfMass3D(simpleRef_1, simpleWeightR_1, doColor);
                    Preferences.debug("Center of mass for the subsampled input image:" + cog.X + ", " +
                                       cog.Y + ", " + cog.Z + "\n",Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Center of mass for the subsampled reference image:" + cogR.X
                                       + ", " + cogR.Y + ", " + cogR.Z + "\n",Preferences.DEBUG_ALGORITHM);

                    diffX = (cog.X - cogR.X);
                    diffY = (cog.Y - cogR.Y);
                    diffZ = (cog.Z - cogR.Z);
                }

                item.initial[0] = item.initial[1] = item.initial[2] = 0; // initial rotation
                item.initial[3] = diffX; // initial translations
                item.initial[4] = diffY;
                item.initial[5] = diffZ;
                item.initial[6] = item.initial[7] = item.initial[8] = 1; // initial scaling
                item.initial[9] = item.initial[10] = item.initial[11] = 0; // initial skewing
            } // fastMode

            time = System.currentTimeMillis();
            Preferences.debug(" Starting level 1 ************************************************\n",Preferences.DEBUG_ALGORITHM);
            answer = levelOne(simpleRef_1, simpleInput_1, item, baseNumIter);
            

            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 1 min = " + ((float) time / 60000.0f) + "\n",Preferences.DEBUG_ALGORITHM);

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                inputImage.exportData(iNumber * colorFactor * volumeSize, colorFactor * volumeSize, buffer); // locks and releases and lock
            } catch (IOException error) {
                displayError("AlgorithmOAR35D: Source image is locked");

                setCompleted(false);

                return;
            }

            try {

                // Note that if the inputImage has unequal resolutions, then simpleInput_1 has
                // undergone 1 interpolation, so taking the data directly from the inputImage
                // avoids an extra interpolation in this case
                input1.importData(0, buffer, true);
            } catch (IOException error) {
                displayError("AlgorithmOAR35D: IOException on input1.importData");

                setCompleted(false);

                return;
            }

            if (doGraph) {
                rot[0][iNumber] = (float) (answer.initial[0]);
                rot[1][iNumber] = (float) (answer.initial[1]);
                rot[2][iNumber] = (float) (answer.initial[2]);
                //OARmat = answer.matrix.getMatrix();
                trans[0][iNumber] = answer.matrix.Get(0, 3);
                trans[1][iNumber] = answer.matrix.Get(1, 3);
                trans[2][iNumber] = answer.matrix.Get(2, 3);
            } // if (doGraph)

            answer.matrix.Inverse();
            transform = new AlgorithmTransform(input1, answer.matrix, interp2, iResols[0], iResols[1], iResols[2],
                                               iExtents[0], iExtents[1], iExtents[2], false, true, false);
            transform.run();
            
            final TransMatrix finalMatrix = getTransform();
            TransMatsInumber[m] = iNumber;
            VolumesToReferenceTransformations[m] = finalMatrix;

            if (output_1 != null) {
                output_1.disposeLocal();
            }

            output_1 = transform.getTransformedImage();

            if (transform != null) {
                transform.finalize();
            }

            try {
                output_1.exportData(0, colorFactor * iExtents[0] * iExtents[1] * iExtents[2], buffer);
            } catch (IOException error) {
                displayError("AlgorithmOAR35D: IOException on output_1.exportData");

                setCompleted(false);

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
                inputImage.importData(iNumber * colorFactor * iExtents[0] * iExtents[1] * iExtents[2], buffer, false);
            } catch (IOException error) {
                displayError("AlgorithmOAR35D: IOException on inputImage.importData");

                setCompleted(false);

                return;
            }

            if ((iNumber == (inputImage.getExtents()[3] - 1)) && regToAdjImage) {

                try {

                    if (isoImage != null) {
                        isoImage.exportData(refImageNo * simpleRef_1.data.length, simpleRef_1.data.length,
                                            simpleRef_1.data);
                    } else {
                        inputImage.exportData(refImageNo * simpleRef_1.data.length, simpleRef_1.data.length,
                                              simpleRef_1.data);
                    }

                    if (!fastMode) {

                        if (allowLevel2XY && allowLevel2Z) {
                            subSampleBy2(simpleRef_1, simpleRefSub2_1, doColor);
                        } else if (allowLevel2XY) {
                            subSampleBy2XY(simpleRef_1, simpleRefSub2_1, doColor);
                        } else {
                            copyFloatData(simpleRef_1, simpleRefSub2_1);
                        }

                        if (allowLevel4XY && allowLevel4Z) {
                            subSampleBy2(simpleRefSub2_1, simpleRefSub4_1, doColor);
                        } else if (allowLevel4XY) {
                            subSampleBy2XY(simpleRefSub2_1, simpleRefSub4_1, doColor);
                        } else {
                            copyFloatData(simpleRefSub2_1, simpleRefSub4_1);
                        }

                        if (allowLevel8XY && allowLevel8Z) {
                            subSampleBy2(simpleRefSub4_1, simpleRefSub8_1, doColor);
                        } else if (allowLevel8XY) {
                            subSampleBy2XY(simpleRefSub4_1, simpleRefSub8_1, doColor);
                        } else {
                            copyFloatData(simpleRefSub4_1, simpleRefSub8_1);
                        }

                        if (allowLevel16XY && allowLevel16Z) {
                            subSampleBy2(simpleRefSub8_1, simpleRefSub16_1, doColor);
                        } else if (allowLevel16XY) {
                            subSampleBy2XY(simpleRefSub8_1, simpleRefSub16_1, doColor);
                        }
                    } // if (!fastMode)

                    if (weighted) {

                        if (imageWeightIso != null) {
                            imageWeightIso.exportData(refImageNo * simpleWeightR_1.data.length,
                                                      simpleWeightR_1.data.length, simpleWeightR_1.data);

                        } else {
                            inputWeight.exportData(refImageNo * simpleWeightR_1.data.length,
                                                   simpleWeightR_1.data.length, simpleWeightR_1.data);
                        }

                        if (!fastMode) {

                            if (allowLevel2XY && allowLevel2Z) {
                                subSampleBy2(simpleWeightR_1, simpleWeightRSub2_1, false);
                            } else if (allowLevel2XY) {
                                subSampleBy2XY(simpleWeightR_1, simpleWeightRSub2_1, false);
                            } else {
                                copyFloatData(simpleWeightR_1, simpleWeightRSub2_1);
                            }

                            if (allowLevel4XY && allowLevel4Z) {
                                subSampleBy2(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                            } else if (allowLevel4XY) {
                                subSampleBy2XY(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                            } else {
                                copyFloatData(simpleWeightRSub2_1, simpleWeightRSub4_1);
                            }

                            if (allowLevel8XY && allowLevel8Z) {
                                subSampleBy2(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                            } else if (allowLevel8XY) {
                                subSampleBy2XY(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                            } else {
                                copyFloatData(simpleWeightRSub4_1, simpleWeightRSub8_1);
                            }

                            if (allowLevel16XY && allowLevel16Z) {
                                subSampleBy2(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                            } else if (allowLevel16XY) {
                                subSampleBy2XY(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                            }
                        } // if (!fastMode)
                    }
                } catch (IOException ex) {
                    System.err.println("Caught IOException in RegOAR35D");

                }
            
            } // if ((iNumber == inputImage.getExtents()[3] - 1) && regToAdjImage)
            
            else if (resample && regToAdjImage) {
                transform = new AlgorithmTransform(input1, answer.matrix, interp2, resIso[0], resIso[1], resIso[2],
                                                   extentsIso[0], extentsIso[1], extentsIso[2], false, true, false);

                transform.run();
                
                final TransMatrix finalMatrix1 = getTransform();
                TransMatsInumber[m] = iNumber;
                VolumesToReferenceTransformations[m] = finalMatrix1;

                if (output_1 != null) {
                    output_1.disposeLocal();
                }

                output_1 = transform.getTransformedImage();

                if (transform != null) {
                    transform.finalize();
                }

                try {
                    output_1.exportData(0, colorFactor * extentsIso[0] * extentsIso[1] * extentsIso[2], bufferA);
                } catch (IOException error) {
                    displayError("AlgorithmOAR35D: IOException on output_1.exportData to bufferA");

                    setCompleted(false);

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

                if (!fastMode) {

                    if ((level1FactorXY == 2.0f) && (level1FactorZ == 2.0f)) {
                        subSampleBy2(simpleRef_1, simpleRefSub2_1, doColor);
                    } else if (level1FactorXY == 2.0f) {
                        subSampleBy2XY(simpleRef_1, simpleRefSub2_1, doColor);
                    }

                    if ((level2FactorXY == 2.0f) && (level2FactorZ == 2.0f)) {
                        subSampleBy2(simpleRefSub2_1, simpleRefSub4_1, doColor);
                    } else if (level2FactorXY == 2.0f) {
                        subSampleBy2XY(simpleRefSub2_1, simpleRefSub4_1, doColor);
                    }

                    if ((level4FactorXY >= 2.0f) && (level4FactorZ >= 2.0f)) {
                        subSampleBy2(simpleRefSub4_1, simpleRefSub8_1, doColor);
                    } else if (level4FactorXY >= 2.0f) {
                        subSampleBy2XY(simpleRefSub4_1, simpleRefSub8_1, doColor);
                    }

                    if ((level4FactorXY == 4.0f) && (level4FactorZ == 4.0f)) {
                        subSampleBy2(simpleRefSub8_1, simpleRefSub16_1, doColor);
                    } else if (level4FactorXY == 4.0f) {
                        subSampleBy2XY(simpleRefSub8_1, simpleRefSub16_1, doColor);
                    }
                } // if (!fastMode)

                if (weighted) {

                    try {
                        inputWeight.exportData(iNumber * inputWeight.getSliceSize() * inputWeight.getExtents()[2],
                                               inputWeight.getSliceSize() * inputWeight.getExtents()[2], bufferIW);
                    } catch (IOException error) {
                        displayError("AlgorithmOAR35D: inputWeight image is locked");

                        setCompleted(false);

                        return;
                    }

                    try {

                        // Note that if the inputWeight has unequal resolutions, then simpleWeightI_1 has
                        // undergone 1 interpolation, so taking the data directly from the inputWeight
                        // avoids an extra interpolation in this case
                        inputw_1.importData(0, bufferIW, true);
                    } catch (IOException error) {
                        displayError("AlgorithmOAR35D: IOException on inputW_1.importData");

                        setCompleted(false);

                        return;
                    }

                    transform = new AlgorithmTransform(inputw_1, answer.matrix, interp2, resIso[0], resIso[1],
                                                       resIso[2], extentsIso[0], extentsIso[1], extentsIso[2], false,
                                                       true, false);
                    transform.run();
                    
                    final TransMatrix finalMatrix2 = getTransform();
                    TransMatsInumber[m] = iNumber;
                    VolumesToReferenceTransformations[m] = finalMatrix2;

                    if (output_1 != null) {
                        output_1.disposeLocal();
                    }

                    output_1 = transform.getTransformedImage();

                    if (transform != null) {
                        transform.finalize();
                    }

                    try {
                        output_1.exportData(0, extentsIso[0] * extentsIso[1] * extentsIso[2], bufferW);
                    } catch (IOException error) {
                        displayError("AlgorithmOAR35D: IOException on output_1.exportData");

                        setCompleted(false);

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

                    if (!fastMode) {

                        if ((level1FactorXY == 2.0f) && (level1FactorZ == 2.0f)) {
                            subSampleBy2(simpleWeightR_1, simpleWeightRSub2_1, false);
                        } else if (level1FactorXY == 2.0f) {
                            subSampleBy2XY(simpleWeightR_1, simpleWeightRSub2_1, false);
                        }

                        if ((level2FactorXY == 2.0f) && (level2FactorZ == 2.0f)) {
                            subSampleBy2(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                        } else if (level2FactorXY == 2.0f) {
                            subSampleBy2XY(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                        }

                        if ((level4FactorXY >= 2.0f) && (level4FactorZ >= 2.0f)) {
                            subSampleBy2(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                        } else if (level4FactorXY >= 2.0f) {
                            subSampleBy2XY(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                        }

                        if ((level4FactorXY == 4.0f) && (level4FactorZ == 4.0f)) {
                            subSampleBy2(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                        } else if (level4FactorXY == 4.0f) {
                            subSampleBy2XY(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                        }
                    } // if (!fastMode)
                } // if (weighted)
            } // if (resample && regToAdjImage)
            else if (regToAdjImage) {

                for (i = 0; i < buffer.length; i++) {
                    simpleRef_1.data[i] = buffer[i];
                }

                if (!fastMode) {

                    if ((level1FactorXY == 2.0f) && (level1FactorZ == 2.0f)) {
                        subSampleBy2(simpleRef_1, simpleRefSub2_1, doColor);
                    } else if (level1FactorXY == 2.0f) {
                        subSampleBy2XY(simpleRef_1, simpleRefSub2_1, doColor);
                    }

                    if ((level2FactorXY == 2.0f) && (level2FactorZ == 2.0f)) {
                        subSampleBy2(simpleRefSub2_1, simpleRefSub4_1, doColor);
                    } else if (level2FactorXY == 2.0f) {
                        subSampleBy2XY(simpleRefSub2_1, simpleRefSub4_1, doColor);
                    }

                    if ((level4FactorXY >= 2.0f) && (level4FactorZ >= 2.0f)) {
                        subSampleBy2(simpleRefSub4_1, simpleRefSub8_1, doColor);
                    } else if (level4FactorXY >= 2.0f) {
                        subSampleBy2XY(simpleRefSub4_1, simpleRefSub8_1, doColor);
                    }

                    if ((level4FactorXY == 4.0f) && (level4FactorZ == 4.0f)) {
                        subSampleBy2(simpleRefSub8_1, simpleRefSub16_1, doColor);
                    } else if (level4FactorXY == 4.0f) {
                        subSampleBy2XY(simpleRefSub8_1, simpleRefSub16_1, doColor);
                    }
                } // if (!fastMode)

                if (weighted) {

                    try {
                        inputWeight.exportData(iNumber * inputWeight.getSliceSize() * inputWeight.getExtents()[2],
                                               inputWeight.getSliceSize() * inputWeight.getExtents()[2], bufferIW);
                    } catch (IOException error) {
                        displayError("AlgorithmOAR35D: inputWeight image is locked");

                        setCompleted(false);

                        return;
                    }

                    if (resampleW) {

                        try {

                            // Note that if the inputWeight has unequal resolutions, then simpleWeightI_1 has
                            // undergone 1 interpolation, so taking the data directly from the inputWeight
                            // avoids an extra interpolation in this case
                            inputw_1.importData(0, bufferIW, true);
                        } catch (IOException error) {
                            displayError("AlgorithmOAR35D: IOException on inputW_1.importData");

                            setCompleted(false);

                            return;
                        }

                        transform = new AlgorithmTransform(inputw_1, answer.matrix, interp2, resIso[0], resIso[1],
                                                           resIso[2], extentsIso[0], extentsIso[1], extentsIso[2],
                                                           false, true, false);
                        transform.run();
                        final TransMatrix finalMatrix3 = getTransform();
                        TransMatsInumber[m] = iNumber;
                        VolumesToReferenceTransformations[m] = finalMatrix3;

                        if (output_1 != null) {
                            output_1.disposeLocal();
                        }

                        output_1 = transform.getTransformedImage();

                        if (transform != null) {
                            transform.finalize();
                        }

                        try {
                            output_1.exportData(0, extentsIso[0] * extentsIso[1] * extentsIso[2], bufferW);
                        } catch (IOException error) {
                            displayError("AlgorithmOAR35D: IOException on output_1.exportData");

                            setCompleted(false);

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
                    else { // weight image need not be resampled

                        for (i = 0; i < bufferIW.length; i++) {
                            simpleWeightR_1.data[i] = bufferIW[i];
                        }
                    }

                    if (!fastMode) {

                        if ((level1FactorXY == 2.0f) && (level1FactorZ == 2.0f)) {
                            subSampleBy2(simpleWeightR_1, simpleWeightRSub2_1, false);
                        } else if (level1FactorXY == 2.0f) {
                            subSampleBy2XY(simpleWeightR_1, simpleWeightRSub2_1, false);
                        }

                        if ((level2FactorXY == 2.0f) && (level2FactorZ == 2.0f)) {
                            subSampleBy2(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                        } else if (level2FactorXY == 2.0f) {
                            subSampleBy2XY(simpleWeightRSub2_1, simpleWeightRSub4_1, false);
                        }

                        if ((level4FactorXY >= 2.0f) && (level4FactorZ >= 2.0f)) {
                            subSampleBy2(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                        } else if (level4FactorXY >= 2.0f) {
                            subSampleBy2XY(simpleWeightRSub4_1, simpleWeightRSub8_1, false);
                        }

                        if ((level4FactorXY == 4.0f) && (level4FactorZ == 4.0f)) {
                            subSampleBy2(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                        } else if (level4FactorXY == 4.0f) {
                            subSampleBy2XY(simpleWeightRSub8_1, simpleWeightRSub16_1, false);
                        }
                    } // if (!fastMode)
                } // if (weighted)
            } // else if (regToAdjImage)

            if (seriesUp) {

                if (regToAdjImage) {
                    rNumber++;
                }

                iNumber++;

                if ((iNumber == inputImage.getExtents()[3]) && !useOutsideReferenceVolume) {
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
        } // for (int m = 0; m < inputImage.getExtents()[3]-1; m++)
        
        

        inputImage.calcMinMax();


        boolean testImages = false;
        
        if (testImages) {
            generateTestImages();
            return;
        }
        disposeLocal();
        finalize();
        setCompleted(true);
    }
    
    /**
     * A test of the diffeomorphic demons is to transform a circle image to a letter C image.
     *
     */
    public void generateTestImages() {
        int x, y;
        int xDist;
        int yDist;
        int yDistSquared;
        int radius = 128;
        int yoff;
        int radiusSquared = radius * radius;
        int extents[] = new int[2];
        int innerRad = 64;
        int innerRadSquared = innerRad * innerRad;
        int halfCOpen = (int)Math.round(innerRad/2.5);
        extents[0] = 512;
        extents[1] = 512;
        int sliceSize = extents[0] * extents[1];
        float buffer[] = new float[sliceSize];
        for (y = 0; y < extents[1]; y ++) {
            yoff = y * extents[0];
            yDist = y - 256;
            yDistSquared = yDist * yDist;
            for (x = 0; x < extents[0]; x++) {
                xDist = x - 256;
                if ((xDist*xDist + yDistSquared) <= radiusSquared) {
                    buffer[x + yoff] = 100.0f;
                }
            }
        }


    } 

    /**
     * allows the user to pass in an OUTSIDE reference volume.
     *
     * @param   refVolume  3-Dim image for reference
     *
     * @return  DOCUMENT ME!
     */
    public boolean setReferenceVolume(ModelImage refVolume) {

        if ((refVolume.getExtents()[0] != inputImage.getExtents()[0]) ||
                (refVolume.getExtents()[1] != inputImage.getExtents()[1]) ||
                (refVolume.getExtents()[2] != inputImage.getExtents()[2])) {
            return false;
        }

        this.useOutsideReferenceVolume = true;
        this.regToAdjImage = false;
        this.outsidePreReferenceVolume = refVolume;

        return true;
    }

    /**
     * Takes a simple image and subsamples XY by 2, interpolating so that the new XY values are averages.
     *
     * @param   srcImage     Image to subsample.
     * @param   resultImage  DOCUMENT ME!
     * @param   isColor      DOCUMENT ME!
     *
     * @return  Subsampled image.
     */
    private static ModelSimpleImage subSampleBy2XY(ModelSimpleImage srcImage, ModelSimpleImage resultImage,
                                                   boolean isColor) {

        return srcImage.subSample3dBy2XY(resultImage, isColor);
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
        s = new String("");

        s += String.valueOf(refImageNo) + ", ";

        if (mode == ADJACENT) {
            s += "Adjacent mode, ";
        } else if (mode == AVERAGE) {
            s += "Average mode, ";
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

        s += "Output interpolation = ";

        switch (interp2) {

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

        s += rotateBegin + ", " + rotateEnd + ", " + coarseRate + ", " + fineRate + ")\n";

        s = "RegistrationOAR35D(" + inputImage.getImageName() + ", " + s;

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
    @SuppressWarnings("unused")
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
        if ((ix0 == (coarseNum - 1))) {
            ix1 = ix0;
        }

        if ((iy0 == (coarseNum - 1))) {
            iy1 = iy0;
        }

        if ((iz0 == (coarseNum - 1))) {
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
     * have been optimized and placed back into their original position in the multi-array. Looks at the 8 neighbors of
     * a point: +, =, or - one fine sample in each of the three directions. If the point has a cost greater than any of
     * these, it is not a minima. Otherwise it is. Saves it in a vector of minima. Optimizes the minima over rotations
     * as well as translations and global scale. (Previously had not optimized over rotations.) Returns two vectors, one
     * containing the minima before optimization, one containing the minima after optimization.
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
        Preferences.debug("Total number of positions at coarse sampling to be tested: " +
                          (coarseNum * coarseNum * coarseNum) + ".\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Total number of positions at fine sampling to be tested: " + (fineNum * fineNum * fineNum) +
                          ".\n",Preferences.DEBUG_ALGORITHM);

        if (weighted) {

            if (allowLevel16XY) {
                cost.setRefWgtImage(simpleWeightRSub16_1);
                cost.setInputWgtImage(simpleWeightISub16_1);
            } else {
                cost.setRefWgtImage(simpleWeightRSub8_1);
                cost.setInputWgtImage(simpleWeightISub8_1);
            }
        }

        double diffX = 0;
        double diffY = 0;
        double diffZ = 0;
        Vector3f cog = new Vector3f(0, 0, 0);
        Vector3f cogR = new Vector3f(0, 0, 0);

        if (calcCOG == true) {

            if (allowLevel16XY) {
                cog = calculateCenterOfMass3D(input, simpleWeightISub16_1, doColor);
                cogR = calculateCenterOfMass3D(ref, simpleWeightRSub16_1, doColor);
            } else {
                cog = calculateCenterOfMass3D(input, simpleWeightISub8_1, doColor);
                cogR = calculateCenterOfMass3D(ref, simpleWeightRSub8_1, doColor);
            }

            Preferences.debug("Center of mass for the subsampled input image:" + cog.X + 
                               ", " +  cog.Y + ", " + cog.Z + "\n",Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Center of mass for the subsampled reference image:" + cogR.X +
                               ", " + cogR.Y + ", " + cogR.Z + "\n",Preferences.DEBUG_ALGORITHM);

            diffX = (cog.X - cogR.X);
            diffY = (cog.Y - cogR.Y);
            diffZ = (cog.Z - cogR.Z);
        }

        double[][][][] preTransforms;
        if (DOF > 6) {
            preTransforms = new double[coarseNum][coarseNum][coarseNum][7];
        }
        else {
        	preTransforms = new double[coarseNum][coarseNum][coarseNum][6];	
        }
        double[][][][] transforms = new double[coarseNum][coarseNum][coarseNum][4];

        // Optimizing over translations and global scale
        AlgorithmPowellOptBase powell = null;
        maxIter = baseNumIter * 4;
        int nDims = 3;
        if (DOF > 6) {
            nDims = 4;
        }
        powell = new AlgorithmPowellOpt3D(this, cog, nDims, cost, getTolerance(nDims), maxIter);
        powell.setUseJTEM(doJTEM);
    	powell.setParallelPowell(doMultiThread);
        if ( doJTEM )
        {
        	powell.setParallelPowell(false);
        }

        double[] initial = new double[12];

        initial[0] = initial[1] = initial[2] = 0; // initial rotation
        initial[3] = diffX; // initial translations
        initial[4] = diffY;
        initial[5] = diffZ;
        initial[6] = initial[7] = initial[8] = 1; // initial scaling
        initial[9] = initial[10] = initial[11] = 0; // initial skewing

        Vectornd[] initials = new Vectornd[coarseNum*coarseNum*coarseNum];
        int index = 0;
        for (int i = 0; i < coarseNum; i++) {
            for (int j = 0; j < coarseNum; j++) {
                for (int k = 0; k < coarseNum; k++) {
                    initial[0] = rotateBegin + (i * coarseRate);
                    initial[1] = rotateBegin + (j * coarseRate);
                    initial[2] = rotateBegin + (k * coarseRate);

                    initials[index++] = new Vectornd(initial, true);
                }
            }
        }

        powell.setPoints(initials);
        powell.run();

        if (threadStopped) {
            return null;
        }
        
        index = 0;
        for (int i = 0; i < coarseNum; i++) {
            for (int j = 0; j < coarseNum; j++) {
                for (int k = 0; k < coarseNum; k++) {
                    preTransforms[i][j][k] = powell.getPoint(index++);
                    if (DOF > 6) {
                    	transforms[i][j][k][0] = preTransforms[i][j][k][6];
                    	transforms[i][j][k][1] = preTransforms[i][j][k][3];
                    	transforms[i][j][k][2] = preTransforms[i][j][k][4];
                    	transforms[i][j][k][3] = preTransforms[i][j][k][5];
                    }
                    else {
                    	transforms[i][j][k][0] = preTransforms[i][j][k][3];
                    	transforms[i][j][k][1] = preTransforms[i][j][k][4];
                    	transforms[i][j][k][2] = preTransforms[i][j][k][5];
                    }
                }
            }
        }

        MatrixListItem[][][] matrixList = new MatrixListItem[fineNum][fineNum][fineNum];

        double[] costs = new double[fineNum * fineNum * fineNum];
        double factorX, factorY, factorZ;

        index = 0;
        for (int i = 0; (i < fineNum) && !threadStopped; i++) {

            for (int j = 0; (j < fineNum) && !threadStopped; j++) {

                for (int k = 0; (k < fineNum) && !threadStopped; k++) {
                    initial[0] = rotateBegin + (i * fineRate);
                    initial[1] = rotateBegin + (j * fineRate);
                    initial[2] = rotateBegin + (k * fineRate);

                    // sets up translation and global scaling factors
                    factorX = (i * fineRate) / coarseRate;
                    factorY = (j * fineRate) / coarseRate;
                    factorZ = (k * fineRate) / coarseRate;
                    interpolate(factorX, factorY, factorZ, initial, transforms, (DOF > 6));
                    initial[7] = initial[8] = initial[6];
                    costs[index] = powell.measureCost(initial);
                    matrixList[i][j][k] = new MatrixListItem(costs[index++], powell.convertToMatrix(initial), initial);
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
        index = Arrays.binarySearch(costs, threshold);
        if (index < 0) {
            index = -1 * (index + 1);
        }
        initials = new Vectornd[index];
        //initials = new Vectornd[fineNum*fineNum*fineNum];
        index = 0;
        for (int i = 0; i < fineNum; i++) {
            for (int j = 0; j < fineNum; j++) {
                for (int k = 0; k < fineNum; k++) {

                    if (matrixList[i][j][k].cost < threshold) {
                        initials[index] = new Vectornd(matrixList[i][j][k].initial);
                        index++;
                    }
                }
            }
        }
        powell.setPoints(initials);
        powell.run();

        if (threadStopped) {
            return null;
        }

        index = 0;
        for (int i = 0; i < fineNum ; i++) {
            for (int j = 0; j < fineNum; j++) {
                for (int k = 0; k < fineNum; k++) {

                    if (matrixList[i][j][k].cost < threshold) {
                        matrixList[i][j][k] = new MatrixListItem(powell.getCost(index), powell.getMatrix(index), powell.getPoint(index));
                        index++;
                    }
                }
            }
        }
        

        if (threadStopped) {
            return null;
        }

        Vector<MatrixListItem> minima = new Vector<MatrixListItem>();
        
        boolean possibleMinima[][][] = new boolean[fineNum][fineNum][fineNum];

        for (int i = 0; i < fineNum; i++) {

            for (int j = 0; j < fineNum; j++) {

                for (int k = 0; k < fineNum; k++) {
                    
                    possibleMinima[i][j][k] = true; // possible minimum

                    for (int itest = -1; (itest <= 1) && possibleMinima[i][j][k]; itest++) {

                        // as long as still possible minimum, check neighbors one degree off
                        for (int jtest = -1; (jtest <= 1) && possibleMinima[i][j][k]; jtest++) {

                            for (int ktest = -1; (ktest <= 1) && possibleMinima[i][j][k]; ktest++) {

                                if (((i + itest) >= 0) && ((i + itest) < fineNum) && ((j + jtest) >= 0) &&
                                        ((j + jtest) < fineNum) && ((k + ktest) >= 0) && ((k + ktest) < fineNum)) {

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
            for (int i = 0; i < fineNum; i++) {

                for (int j = 0; j < fineNum; j++) {

                    for (int k = 0; k < fineNum; k++) {

                        for (int itest = -1; (itest <= 1) && possibleMinima[i][j][k]; itest++) {

                            // as long as still possible minimum, check neighbors one degree off
                            for (int jtest = -1; (jtest <= 1) && possibleMinima[i][j][k]; jtest++) {

                                for (int ktest = -1; (ktest <= 1) && possibleMinima[i][j][k]; ktest++) {

                                    if (((i + itest) >= 0) && ((i + itest) < fineNum) && ((j + jtest) >= 0) &&
                                            ((j + jtest) < fineNum) && ((k + ktest) >= 0) && ((k + ktest) < fineNum)) {

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
       
        for (int i = 0; i < fineNum; i++) {

            for (int j = 0; j < fineNum; j++) {

                for (int k = 0; k < fineNum; k++) {
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

        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 7;
        powell = new AlgorithmPowellOpt3D(this, cog, degree, cost, getTolerance(degree), maxIter);
        powell.setUseJTEM(doJTEM);
    	powell.setParallelPowell(doMultiThread);
        if ( doJTEM )
        {
        	powell.setParallelPowell(false);
        }

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
        }

        if (threadStopped) {
            return null;
        }

        cost.disposeLocal();
        powell.disposeLocal();

        return new Vector[] {minima, optMinima };
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
    private Vector<MatrixListItem> levelFour(ModelSimpleImage ref, ModelSimpleImage input, 
            Vector<MatrixListItem> minima, Vector<MatrixListItem> optMinima) {
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
            cost.setRefWgtImage(simpleWeightRSub4_1);
            cost.setInputWgtImage(simpleWeightISub4_1);
        }

        Vector3f cog = new Vector3f(0, 0, 0);

        if (calcCOG == true) {
            cog = calculateCenterOfMass3D(input, simpleWeightISub4_1, doColor);
        }

        Preferences.debug("Center of mass for the subsampled input image:" + cog.X + ", "
                           + cog.Y + ", " + cog.Z + "\n",Preferences.DEBUG_ALGORITHM);

        // Preferences.debug("Center of mass for the subsampled reference image:" + cog + "\n",Preferences.DEBUG_ALGORITHM);
        MatrixListItem item = null;

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[3] *= level4FactorXY;
            item.initial[4] *= level4FactorXY;
            item.initial[5] *= level4FactorZ;
        }

        for (Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[3] *= level4FactorXY;
            item.initial[4] *= level4FactorXY;
            item.initial[5] *= level4FactorZ;
        }

        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 2;

        AlgorithmPowellOptBase powell = new AlgorithmPowellOpt3D(this, cog, degree, cost, getTolerance(degree),
                                                               maxIter);
        powell.setUseJTEM(doJTEM);
    	powell.setParallelPowell(doMultiThread);
        if ( doJTEM )
        {
        	powell.setParallelPowell(false);
        }

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item.cost = powell.measureCost(en.nextElement().initial);
        }

        for (Enumeration<MatrixListItem> en = optMinima.elements(); en.hasMoreElements() && !threadStopped;) {
            item.cost = powell.measureCost(en.nextElement().initial);
        }

        Collections.sort(minima);
        Collections.sort(optMinima);

        int total = (numMinima < minima.size()) ? numMinima : minima.size();
        // Old code: int total = (3 < minima.size()) ? 3 : minima.size(); New code: int total = (numMinima <
        // minima.size()) ? numMinima : minima.size(); Changed so that the number of minima to test at Level Four is a
        // variable, passed in from JDialog.  It used to be set to "3".

        powell.setMaxIterations(7);

        Vector<MatrixListItem> newMinima = new Vector<MatrixListItem>();

        // Recalculating minima at this level, i.e. with images subsampled at level 4.
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

        for(int i = 0; i < initials.length; i++){
            item = new MatrixListItem(powell.getCost(i), powell.getMatrix(i), powell.getPoint(i));
            newMinima.add(item);
        }

        // Resort the minima.  Shouldn't have switched much from previous sorting.
        Collections.sort(newMinima);

        double fineDelta = fineRate / 2.0;
        double[] initial;
        Vector<MatrixListItem> perturbList = new Vector<MatrixListItem>();
        int sign = 1;

        Preferences.debug("Number of minima to test at levelFour: " + total + ".\n",Preferences.DEBUG_ALGORITHM);

        // Perturb rotations.  In each of the three dimensions, add fine delta and optimize,
        // then subtract fine delta and optimize.
        perturbList.addAll(newMinima);
        if(DOF > 6){
            initials = new Vectornd[newMinima.size() * 10];
        }else{
            initials = new Vectornd[newMinima.size()*6];
        }
        
        int index = 0;
        for (int j = 1; j < 7; j++) {

            for (int i = 0; i < (2 * total); i++) {

                // Current "initial" is element for this i.
                initial = (double[]) newMinima.elementAt(i).initial.clone();

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
                    Preferences.debug(" by adding " + (sign * fineDelta) + " to initial[" + ((int) (j - 1) / 2) +
                                      "] (" + (int) initial[(int) (j - 1) / 2] + ").\n",Preferences.DEBUG_ALGORITHM);
                    initial[(int) (j - 1) / 2] += sign * fineDelta;
                }

                initials[index++] = new Vectornd(initial);

            }
        }

        if (DOF > 6) {

            // Perturb scales.  Multiply scaleDelta by 0.8, 0.9, 1.0, 1.1, and 1.2.
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

                    Preferences.debug("Perturbing initial[6] by ",Preferences.DEBUG_ALGORITHM);
                    initial[6] *= scaleDelta;
                    Preferences.debug("Multiplying by " + scaleDelta + "\n",Preferences.DEBUG_ALGORITHM);

                    initials[index++] = new Vectornd(initial);
                }
            }
        }
        powell.setPoints(initials);
        powell.run();

        if (threadStopped) {
            return null;
        }

        for(int i = 0; i < initials.length; i++){
            item = new MatrixListItem(powell.getCost(i), powell.getMatrix(i), powell.getPoint(i));
            perturbList.add(item);
        }

        Collections.sort(perturbList);

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
            cost.setRefWgtImage(simpleWeightR_1);
            cost.setInputWgtImage(simpleWeightI_1);
        }

        Vector3f cog = new Vector3f(0, 0, 0);

        if (calcCOG == true) {
            cog = calculateCenterOfMass3D(input, simpleWeightI_1, doColor);
        }

        item.initial[3] *= level1FactorXY;
        item.initial[4] *= level1FactorXY;
        item.initial[5] *= level1FactorZ;

        int degree = (DOF < 12) ? DOF : 12;

        AlgorithmPowellOptBase powell = new AlgorithmPowellOpt3D(this, cog, degree, cost,
                getTolerance(degree), maxIter);
        powell.setUseJTEM(doJTEM);
    	powell.setParallelPowell(doMultiThread);
        if ( doJTEM )
        {
        	powell.setParallelPowell(false);
        }
        Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(item.initial);
        powell.setPoints(initials);
        powell.run();

        if (threadStopped) {
            return null;
        }

        item2 = new MatrixListItem(powell.getCost(0), powell.getMatrix(0, input.xRes), powell.getPoint(0, input.xRes));
        Preferences.debug("Best answer: \n" + item2 + "\n",Preferences.DEBUG_ALGORITHM);
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
            cost.setRefWgtImage(simpleWeightRSub2_1);
            cost.setInputWgtImage(simpleWeightISub2_1);
        }

        Vector3f cog = new Vector3f(0, 0, 0);

        if (calcCOG == true) {
            cog = calculateCenterOfMass3D(input, simpleWeightISub2_1, doColor);
        }

        MatrixListItem item = null;

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements();) {
            item = en.nextElement();
            item.initial[3] *= level2FactorXY;
            item.initial[4] *= level2FactorXY;
            item.initial[5] *= level2FactorZ;
        }

        // Prepare data for call to getTolerance
        int[] dims = new int[3];
        dims[0] = ref.xDim;
        dims[1] = ref.yDim;
        dims[2] = ref.zDim;

        int degree = (DOF < 7) ? DOF : 7;
        maxIter = baseNumIter * 4;

        AlgorithmPowellOptBase powell = new AlgorithmPowellOpt3D(this, cog, degree, cost,
                getTolerance(degree), maxIter);
        powell.setUseJTEM(doJTEM);
    	powell.setParallelPowell(doMultiThread);
        if ( doJTEM )
        {
        	powell.setParallelPowell(false);
        }

        for (Enumeration<MatrixListItem> en = minima.elements(); en.hasMoreElements() && !threadStopped;) {
            item.cost = powell.measureCost(en.nextElement().initial);
        }

        if (threadStopped) {
            return null;
        }

        Collections.sort(minima);

        Vectornd[] initials = new Vectornd[1];
        initials[0] = new Vectornd(minima.elementAt(0).initial);
        powell.setPoints(initials);
        powell.run();

        if (threadStopped) {
            return null;
        }

        item = new MatrixListItem(powell.getCost(0), powell.getMatrix(0), powell.getPoint(0));

        maxIter = baseNumIter * 4;

        if (DOF > 7) {
            degree = 9;
            powell = new AlgorithmPowellOpt3D(this, cog, degree, cost, getTolerance(degree), maxIter);
            powell.setUseJTEM(doJTEM);
        	powell.setParallelPowell(doMultiThread);
            if ( doJTEM )
            {
            	powell.setParallelPowell(false);
            }
            powell.setPoints(initials);
            powell.run();

            if (threadStopped) {
                return null;
            }

            item = new MatrixListItem(powell.getCost(0), powell.getMatrix(0), powell.getPoint(0));

            if (DOF > 9) {
                degree = 12;
                powell = new AlgorithmPowellOpt3D(this, cog, 12, cost, getTolerance(12), maxIter);
                powell.setUseJTEM(doJTEM);
            	powell.setParallelPowell(doMultiThread);
                if ( doJTEM )
                {
                	powell.setParallelPowell(false);
                }
                powell.setPoints(initials);
                powell.run();

                if (threadStopped) {
                    return null;
                }

                item = new MatrixListItem(powell.getCost(0), powell.getMatrix(0), powell.getPoint(0));
            }
        }

        cost.disposeLocal();
        powell.disposeLocal();

        return item;
    }

    /**
     * Takes a simple image and subsamples it by 2, interpolating so that the new values are averages.
     *
     * @param  srcImage     Image to subsample.
     * @param  resultImage  DOCUMENT ME!
     * @param  isColor      DOCUMENT ME!
     */
    private void subSampleBy2(ModelSimpleImage srcImage, ModelSimpleImage resultImage, boolean isColor) {
        int c;

        int sliceSize = resultImage.xDim * resultImage.yDim;
        int rowSize = resultImage.xDim;
        int slice = srcImage.xDim * srcImage.yDim;
        int row = srcImage.xDim;
        int zStop, yStop, xStop;

        if (srcImage.nDims == 3) {
            zStop = srcImage.zDim;
        } else {
            zStop = 3;
        }

        yStop = srcImage.yDim;
        xStop = srcImage.xDim;

        int nextRow, nextCol, nextSlice;

        for (int z = 0, bz = 1; bz < zStop; z++, bz += 2) {

            for (int y = 0, by = 1; by < yStop; y++, by += 2) {

                for (int x = 0, bx = 1; bx < xStop; x++, bx += 2) {
                    int currentSlice = bz * slice;
                    int previousSlice = (bz - 1) * slice;

                    if (bz >= (srcImage.zDim - 1)) {
                        nextSlice = (srcImage.zDim - 1) * slice;
                    } else {
                        nextSlice = (bz + 1) * slice;
                    }

                    int currentRow = by * row;
                    int previousRow = (by - 1) * row;

                    if (by >= (srcImage.yDim - 1)) {
                        nextRow = (srcImage.yDim - 1) * row;
                    } else {
                        nextRow = (by + 1) * row;
                    }

                    int currentCol = bx;
                    int previousCol = (bx - 1);

                    if (bx >= (srcImage.xDim - 1)) {
                        nextCol = srcImage.xDim - 1;
                    } else {
                        nextCol = (bx + 1);
                    }

                    if (isColor) {

                        for (c = 0; c <= 3; c++) {
                            resultImage.data[(4 * ((z * sliceSize) + (y * rowSize) + x)) + c] = (float) ((0.0924 *
                                                                                                              (srcImage.data[(4 *
                                                                                                                                  (currentSlice +
                                                                                                                                       currentRow +
                                                                                                                                       currentCol)) +
                                                                                                                                 c])) +
                                                                                                         (0.0560 *
                                                                                                              (srcImage.data[(4 *
                                                                                                                                  (currentSlice +
                                                                                                                                       currentRow +
                                                                                                                                       nextCol)) +
                                                                                                                                 c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           currentRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           nextRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           previousRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           currentRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           currentRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c])) +
                                                                                                         (0.0339 *
                                                                                                              (srcImage.data[(4 *
                                                                                                                                  (currentSlice +
                                                                                                                                       nextRow +
                                                                                                                                       nextCol)) +
                                                                                                                                 c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           previousRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           nextRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           previousRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           currentRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           currentRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           currentRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           currentRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           nextRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           nextRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           previousRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           previousRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c])) +
                                                                                                         (0.0206 *
                                                                                                              (srcImage.data[(4 *
                                                                                                                                  (nextSlice +
                                                                                                                                       nextRow +
                                                                                                                                       nextCol)) +
                                                                                                                                 c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           nextRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           previousRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           previousRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           nextRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           nextRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           previousRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           previousRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c])));
                        } // for (c = 0; c <= 3; c++)
                    } // if (isColor)
                    else { // black and white
                        resultImage.data[(z * sliceSize) + (y * rowSize) + x] = (float) ((0.0924 *
                                                                                              (srcImage.data[currentSlice +
                                                                                                                 currentRow +
                                                                                                                 currentCol])) +
                                                                                         (0.0560 *
                                                                                              (srcImage.data[currentSlice +
                                                                                                                 currentRow +
                                                                                                                 nextCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     currentRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     nextRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     previousRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     currentRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     currentRow +
                                                                                                                     currentCol])) +
                                                                                         (0.0339 *
                                                                                              (srcImage.data[currentSlice +
                                                                                                                 nextRow +
                                                                                                                 nextCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     previousRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     nextRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     previousRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     currentRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     currentRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     currentRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     currentRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     nextRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     nextRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     previousRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     previousRow +
                                                                                                                     currentCol])) +
                                                                                         (0.0206 *
                                                                                              (srcImage.data[nextSlice +
                                                                                                                 nextRow +
                                                                                                                 nextCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     nextRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     previousRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     previousRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     nextRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     nextRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     previousRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     previousRow +
                                                                                                                     previousCol])));
                    }
                }
            }
        }
        


        resultImage.calcMinMax();

        return;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Helper class to make it easy to store the necessary information about a minimum. Stores the "point", or vector at
     * which the minimum was reached; the "cost", or value of the cost function at that minimum; and the matrix, which
     * was the true input into the cost function and represents the transformation that gives the minimum cost of
     * differences between the images. Implements Comparable, so that a list of MatrixListItems can be sorted using
     * Java's sort.
     */
    class MatrixListItem implements Comparable<MatrixListItem> {

        /** Cost of function at this minimum. */
        protected double cost;

        /** Rotations, translations, scales, and skews that make up transformation. */
        protected double[] initial;

        /** Matrix that gives best transformation. */
        protected TransMatrix matrix;

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
