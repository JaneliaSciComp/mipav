package gov.nih.mipav.model.algorithms.registration;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This is an automatic time series registration method based on MCFLIRT. MCFLIRT stands for Motion Correction - FMRIB's
 * Linear Image Registration Tool. For more information on MCFLIRT, visit their homepage at <a
 * href="http://www.fmrib.ox.ac.uk/fsl/mcflirt/index.html">http://www.fmrib.ox.ac.uk/fsl/mcflirt/index.html</a>. Their
 * main paper is:
 *
 * <p>Jenkinson, M., Bannister, P., Brady, M., and Smith, S. (2002a).<br>
 * Improved optimisation for the robust and accurate linear registration and motion correction of brain images.<br>
 * <i>NeuroImage.</i></p>
 *
 * <p>Algorithm TSOAR</p>
 *
 * <ol>
 *   <li>Take 4D time series image volume, choose the center volume or average, depending on what the user entered.</li>
 *   <li>Rescale reference volume to isotropic voxels.</li>
 *   <li>Subsample the volume to a max subsampling of 8.</li>
 *   <li>Take the subsampled by 8 image (note - it might be subsampled less if the subsampling makes the image too
 *     small).</li>
 *   <li>Do this at each level, with varying tolerances:
 *
 *     <ol>
 *       <li>for each volume in time series, starting at refvol + 1 and going up, then starting at refvol - 1 and going
 *         down:</li>
 *       <li>isotropic rescale current volume</li>
 *       <li>subsample current volume appropriately</li>
 *       <li>create cost function with reference volume and current volume</li>
 *       <li>optimize with certain tolerances</li>
 *     </ol>
 *   </li>
 *   <li>Do the previous with the image subsampled at 4, then again subsampled at 2, and finally at original size and
 *     with a new cost function of normalized cross correlation sinc if normalized cross correlation were the initial
 *     cost function. The original MCFLIRT scheme had 1 round at subsampling 8 followed by 3 rounds at subsampling 4.
 *   </li>
 *   <li>The answer is stored as a list of matrices, giving the transformation from one volume to the next in the
 *     series.</li>
 * </ol>
 *
 * @author  Neva Cherniavsky
 */
public class AlgorithmRegTSOAR extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Minimum Z dimension for subsampling. */
    private static final int minimumZForSub = 16;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Array of answers - transforms to get each volume registered to ref volume. */
    private MatrixListItem[] answer;

    /** Center of gravity of the reference volume. */
    private Vector3f cog;

    /** Choice of which cost function to use. */
    private int costChoice;

    /** Indicates if source image is color. */
    private boolean doColor;

    /** Maximum degrees of freedom when running the optimization. */
    private int DOF;

    /** if true output graphs for rotations and translations Only used for 6 DOF. */
    private boolean doGraph;

    /** If true subsample. */
    private boolean doSubsample;

    /** Input image. */
    private ModelImage imageInput = null;

    /** Reference image. */
    private ModelImage imageRef = null;

    /** Isotropic reference image. */
    private ModelImage imageRefIso = null;

    /** Interpolation method. */
    private int interp;

    /** Multiplication factor for level 1 - will be set based on subsampling. */
    private float level1Factor = 1.0f;

    /** Multiplication factor for level 2 - will be set based on subsampling. */
    private float level2Factor = 1.0f;

    /** Multiplication factor for level 4 - will be set based on subsampling. */
    private float level4Factor = 1.0f;

    /** Use NORMALIZED_CROSS_CORRELATION_SINC on nonsubsampled images in final iterations. */
    private boolean normalizedCrossCorrelationSinc;

    /** Number of volumes to register. */
    private int nVolumes;

    /** Flag indicating if the volumes should be registered to a reference volume. */
    private boolean reference;

    /** Index of volume to use as reference. */
    private int refNum;

    /** Flag indicating if the volume needs to be resampled into isotropic voxels. */
    private boolean resample;

    /** Used for output graphs. */
    private float[][] rot = null;

    /** Speed can be an issue. Therefore, if true preform final registration at the subsample level 2 */
    private boolean setFinalRegistrationAtLevel2;

    /** Simple version of reference image. */
    private ModelSimpleImage simpleRef;

    /** Simple version of reference image, subsampled by 2. */
    private ModelSimpleImage simpleRefSub2 = null;

    /** Simple version of reference image, subsampled by 4. */
    private ModelSimpleImage simpleRefSub4 = null;

    /** Simple version of reference image, subsampled by 8. */
    private ModelSimpleImage simpleRefSub8;

    /** DOCUMENT ME! */
    private float[][] trans = null;

    /** Transformation algorithm for creating an isotropic reference image. */
    private AlgorithmTransform transform = null;

    /** Extents of each volume. */
    private int[] volumeExtents;

    /** The extents of the isotropic volume. */
    private int[] volumeExtentsIso = null;

    /** The voxel resolutions of each volume. */
    private float[] volumeResolutions;

    /** The resolutions of the isotropic volume. */
    private float[] volumeResolutionsIso = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new algorithm to register the time series.
     *
     * @param  image                            The time series.
     * @param  _costChoice                      Choice of cost functions, like correlation ratio or mutual information.
     * @param  _DOF                             Degrees of freedom for registration.
     * @param  _interp                          Interpolation method used in transformations.
     * @param  _reference                       <code>true</code> means register to reference volume, <code>false</code>
     *                                          means register to average volume.
     * @param  _refNum                          the reference volume number
     * @param  _normalizedCrossCorrelationSinc  if true use NORMALIZED_CROSS_CORRELATION SINC on nonsubsampled images in
     *                                          final iterations
     * @param  doGraph                          if true output rotations and translations graphs
     * @param  doSubsample                      if true subsample
     * @param  doFinalAt2                       if true preform final registration at the subsample level 2
     */
    public AlgorithmRegTSOAR(ModelImage image, int _costChoice, int _DOF, int _interp, boolean _reference, int _refNum,
                             boolean _normalizedCrossCorrelationSinc, boolean doGraph, boolean doSubsample,
                             boolean doFinalAt2) {
        super(null, image);

        if (srcImage.isColorImage()) {
            doColor = true;
        } else {
            doColor = false;
        }

        DOF = _DOF;
        interp = _interp;
        reference = _reference;
        refNum = _refNum;
        costChoice = _costChoice;
        normalizedCrossCorrelationSinc = _normalizedCrossCorrelationSinc;
        this.doGraph = doGraph;
        this.doSubsample = doSubsample;
        setFinalRegistrationAtLevel2 = doFinalAt2;

        if (srcImage.getNDims() != 4) {
            MipavUtil.displayError("" + srcImage.getNDims() + "D registration not supported.");
            disposeLocal();
            setCompleted(false);

            return;
        }

        nVolumes = srcImage.getExtents()[3];

        try {
            volumeResolutions = new float[3];
            volumeResolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
            volumeResolutions[1] = srcImage.getFileInfo(0).getResolutions()[1];
            volumeResolutions[2] = srcImage.getFileInfo(0).getResolutions()[2];

            volumeExtents = new int[3];
            volumeExtents[0] = srcImage.getExtents()[0];
            volumeExtents[1] = srcImage.getExtents()[1];
            volumeExtents[2] = srcImage.getExtents()[2];

            if (doGraph) {
                rot = new float[3][srcImage.getExtents()[3]];

                if (reference == true) {
                    rot[0][refNum] = 0.0f;
                    rot[1][refNum] = 0.0f;
                    rot[2][refNum] = 0.0f;
                }

                trans = new float[3][srcImage.getExtents()[3]];

                if (reference == true) {
                    trans[0][refNum] = 0.0f;
                    trans[1][refNum] = 0.0f;
                    trans[2][refNum] = 0.0f;
                }
            } // if (doGraph)

            imageRef = new ModelImage(srcImage.getType(), volumeExtents, "Reference image");
            imageInput = new ModelImage(srcImage.getType(), volumeExtents, "Input image");
            for (int z = 0; z < volumeExtents[2]; z++) {
                imageRef.getFileInfo()[z].setResolutions(volumeResolutions);
                imageInput.getFileInfo()[z].setResolutions(volumeResolutions);
            }

            float[] buffer = new float[imageRef.getSize()];

            if (reference == true) {
                srcImage.exportData(refNum * buffer.length, buffer.length, buffer);
            } else {
                average(buffer);
            }

            imageRef.importData(0, buffer, true);

            simpleRef = new ModelSimpleImage(volumeExtents, volumeResolutions, imageRef);
            cog = AlgorithmRegOAR3D.calculateCenterOfMass3D(simpleRef, null, doColor);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTSOAR: I/O error while constructing algorithm.");
            disposeLocal();
            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("AlgorithmTSOAR: Memory error while constructing algorithm.");
            disposeLocal();
            setCompleted(false);

            return;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {

        if (imageRefIso != null) {
            imageRefIso.disposeLocal();
        }

        imageRefIso = null;

        if (imageRef != null) {
            imageRef.disposeLocal();
        }

        imageRef = null;

        if (imageInput != null) {
            imageInput.disposeLocal();
        }

        imageInput = null;

        if (simpleRef != null) {
            simpleRef.disposeLocal(false);
        }

        simpleRef = null;

        if (simpleRefSub2 != null) {
            simpleRefSub2.disposeLocal(false);
        }

        simpleRefSub2 = null;

        if (simpleRefSub4 != null) {
            simpleRefSub4.disposeLocal(false);
        }

        simpleRefSub4 = null;

        if (simpleRefSub8 != null) {
            simpleRefSub8.disposeLocal(false);
        }

        simpleRefSub8 = null;

        if (transform != null) {

            if (transform.getTransformedImage() != null) {
                transform.getTransformedImage().disposeLocal();
            }

            transform.disposeLocal();
            transform = null;
        }

        volumeExtents = null;
        volumeResolutions = null;
        volumeExtentsIso = null;
        volumeResolutionsIso = null;
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
     * Makes a new image from the transformation matrices obtained through registration.
     *
     * @param   interpolation  Interpolation to use when transforming images.
     *
     * @return  4D time series that has been registered volume-by-volume.
     */
    public ModelImage getTransformedImage(int interpolation) {
        ModelImage result = (ModelImage) srcImage.clone();
        ModelImage transformed = null;

        fireProgressStateChanged("Registration", "Getting registered image");
        

        float[] transformBuffer = new float[imageInput.getSize()];

        for (int i = 0; (i < nVolumes) && !threadStopped; i++) {
            fireProgressStateChanged((i + 1) * 100 / nVolumes);

            try {
                srcImage.exportData(i * transformBuffer.length, transformBuffer.length, transformBuffer);
                imageInput.importData(0, transformBuffer, true);
            } catch (IOException error) {
                MipavUtil.displayError("I/O Error while registering.");
                

                return null;
            }

            if ((i != refNum) || (reference == false)) {

                // answer[i].matrix.print();
                // System.out.println("_______________________________" + imageInput);
                transform = new AlgorithmTransform(imageInput, answer[i].matrix, interpolation, volumeResolutions[0],
                                                   volumeResolutions[1], volumeResolutions[2], volumeExtents[0],
                                                   volumeExtents[1], volumeExtents[2], false, true, false);
                transform.run();

                if (transform.isCompleted() == false) {
                    MipavUtil.displayError("Error while transforming.");
                    

                    return null;
                }

                transformed = transform.getTransformedImage();
            } else {
                transformed = imageInput;
            }

            try {
                transformed.exportData(0, transformBuffer.length, transformBuffer);
                result.importData(i * transformBuffer.length, transformBuffer, true);

                if (transform != null) {

                    if (transform.getTransformedImage() != null) {
                        transform.getTransformedImage().disposeLocal();
                    }

                    transform.disposeLocal();
                    transform = null;
                }
            } catch (IOException error) {
                MipavUtil.displayError("Error while creating registered image in AlgorithmTSOAR.");
                

                return null;
            }
        }

        if (transform != null) {

            if (transform.getTransformedImage() != null) {
                transform.getTransformedImage().disposeLocal();
            }

            transform.disposeLocal();
            transform = null;
        }

        

        if (threadStopped) {
            return null;
        }

        return result;

    }

    /**
     * Runs the image registration.
     *
     * <ol>
     *   <li>Take 4D time series image volume, choose the center volume or average, depending on what the user
     *     entered.</li>
     *   <li>Rescale reference volume to isotropic voxels.</li>
     *   <li>Subsample the volume to a max subsampling of 8.</li>
     *   <li>Note: if the image is too small to subsample to 8, then the "subsampled by 8" image might actually only be
     *     subsampled by 4, and the "subsampled by 4" image will only be subsampled by 2, etc.</li>
     *   <li>Call register() on the subsampled by 8 image, then 3 times on the subsampled by 4 image. The tolerances
     *     will be different on the first two calls with the subsampled by 4 image, and on the last call, the cost
     *     function will be different if cross correlation was the initial cost function.</li>
     *   <li>The answer is stored as a list of matrices, giving the transformation from one volume to the next in the
     *     series.</li>
     * </ol>
     */
    public void runAlgorithm() {
        fireProgressStateChanged("Registering images", "Beginning registration");
        

        float minSample = 1.0f;

        minSample = Math.min(volumeResolutions[0], Math.min(volumeResolutions[1], volumeResolutions[2]));

        if ((volumeResolutions[0] == volumeResolutions[1]) && (volumeResolutions[0] == volumeResolutions[2])) {
            resample = false;
        } else {
            resample = true;
        }

        try {
            volumeExtentsIso = new int[3];
            volumeResolutionsIso = new float[3];
        } catch (OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("Out of memory in AlgorithmTSOAR.");
            disposeLocal();
            setCompleted(false);

            return;
        }

        for (int i = 0; i < volumeExtentsIso.length; i++) {
            volumeExtentsIso[i] = (int) (srcImage.getExtents()[i] / (minSample / volumeResolutions[i]));
            volumeResolutionsIso[i] = minSample;
        }

        if (resample) {
            transform = new AlgorithmTransform(imageRef, new TransMatrix(4), interp, volumeResolutionsIso[0],
                                               volumeResolutionsIso[1], volumeResolutionsIso[2], volumeExtentsIso[0],
                                               volumeExtentsIso[1], volumeExtentsIso[2], false, true, false);
            transform.setRunningInSeparateThread(runningInSeparateThread);
            transform.run();

            if (transform.isCompleted() == false) {
                setCompleted(false);
                finalize();

                return;
            }

            imageRefIso = transform.getTransformedImage();

            if (imageRef != null) {
                imageRef.disposeLocal();
            }

            imageRef = null;

        } // if (resample)
        else {
            imageRefIso = imageRef;
        }

        simpleRef = new ModelSimpleImage(imageRefIso.getExtents(), imageRefIso.getFileInfo(0).getResolutions(),
                                         imageRefIso);

        if (transform != null) {

            if (transform.getTransformedImage() != null) {
                transform.getTransformedImage().disposeLocal();
            }

            transform.disposeLocal();
            transform = null;
        }

        if (imageRefIso != null) {
            imageRefIso.disposeLocal();
        }

        imageRefIso = null;

        int subMinFactor = 75000;

        if (doColor) {
            subMinFactor *= 4;
        }

        /* If only one subsampling occurs, only simpleRefSub8 is subsampled.
         * If 2 subsamplings occur simpleRefSub8 is subsampled twice and simpleRefSub4 is subsampled once. */

        if ((simpleRef.dataSize > subMinFactor) && (simpleRef.zDim >= minimumZForSub) && doSubsample) {
            level4Factor = 2.0f;

            int testSize = simpleRef.dataSize / 8;
            int testZ = simpleRef.zDim / 2;

            if ((testSize > subMinFactor) && (testZ >= minimumZForSub)) {
                level2Factor = 2.0f;
                testSize = testSize / 8;
                testZ = testZ / 2;

                if ((testSize > subMinFactor) && (testZ >= minimumZForSub)) {
                    level1Factor = 2.0f;
                }
            }
        }

        if (level1Factor > 1) {
            simpleRefSub2 = simpleRef.subsample3dBy2(doColor);
        } else {
            simpleRefSub2 = simpleRef;
        }

        if (level2Factor > 1) {
            simpleRefSub4 = simpleRefSub2.subsample3dBy2(doColor);
        } else {
            simpleRefSub4 = simpleRefSub2;
        }

        if (level4Factor > 1) {
            simpleRefSub8 = simpleRefSub4.subsample3dBy2(doColor);
        } else {
            simpleRefSub8 = simpleRefSub4;
        }

        Preferences.debug("Level 1 factor = " + level1Factor + "\n" + "Level 2 factor = " + level2Factor + "\n" +
                          "Level 4 factor = " + level4Factor + "\n" + "Ref subsampled 2 = " + simpleRefSub2 +
                          "Ref subsampled 4 = " + simpleRefSub4 + "Ref subsampled 8 = " + simpleRefSub8);

        if (threadStopped) {
            
            setCompleted(false);
            finalize();

            return;
        }

        long time = System.currentTimeMillis();
        Preferences.debug(" Subsampling at 8 ************************************************\n");

        MatrixListItem[] results = register(simpleRefSub8, 8, null);
        time = System.currentTimeMillis() - time;
        Preferences.debug(" Level 8 min = " + ((float) time / 60000.0f) + "\n");
        time = System.currentTimeMillis();

        if (threadStopped) {
            setCompleted(false);
            

            return;
        }

        for (int i = 0; i < results.length; i++) {

            if (results[i] != null) {
                Preferences.debug("Result: " + results[i]);
            }
        }

        Preferences.debug(" Subsampling at 4 ************************************************\n");
        results = register(simpleRefSub4, 4, results);
        time = System.currentTimeMillis() - time;
        Preferences.debug(" Level 4  min = " + ((float) time / 60000.0f) + "\n");
        time = System.currentTimeMillis();

        if (threadStopped) {
            setCompleted(false);
            

            return;
        }

        for (int i = 0; i < results.length; i++) {

            if (results[i] != null) {
                Preferences.debug("Result: " + results[i]);
            }
        }

        Preferences.debug(" Subsampling at 2   ************************************************\n");
        results = register(simpleRefSub2, 2, results);
        time = System.currentTimeMillis() - time;
        Preferences.debug(" Level 2  min = " + ((float) time / 60000.0f) + "\n");
        time = System.currentTimeMillis();

        if (threadStopped) {
            setCompleted(false);
            

            return;
        }

        for (int i = 0; i < results.length; i++) {

            if (results[i] != null) {
                Preferences.debug("Result: " + results[i]);
            }
        }

        // Add flag to by pass registration 1 to 1.
        if (setFinalRegistrationAtLevel2 == false) {
            Preferences.debug(" No subsampling  ************************************************\n");
            answer = register(simpleRef, 1, results);
            time = System.currentTimeMillis() - time;
            Preferences.debug(" Level 1  min = " + ((float) time / 60000.0f) + "\n");

            if (threadStopped) {
                setCompleted(false);
                

                return;
            }
        } else {
            answer = results;
        }

        for (int i = 0; i < answer.length; i++) {

            if (doGraph && (answer[i] != null)) {
                rot[0][i] = (float) (answer[i].initial[0]);
                rot[1][i] = (float) (answer[i].initial[1]);
                rot[2][i] = (float) (answer[i].initial[2]);
                trans[0][i] = (float) (answer[i].matrix.Get(0, 3));
                trans[1][i] = (float) (answer[i].matrix.Get(1, 3));
                trans[2][i] = (float) (answer[i].matrix.Get(2, 3));
            } // if (doGraph)

            if ((i != refNum) || (reference == false)) {

                if (answer[i] != null) {
                    answer[i].matrix.Inverse();
                }
            }
        }

        fireProgressStateChanged(100);
        
        setCompleted(true);
    }

    /**
     * Takes a 4D image buffer and finds the average image volume from the 4D series.
     *
     * @param  image  Image buffer to put data in.
     */
    private void average(float[] image) {
        float sum;
        int loc;
        int i, j, k, m, c;

        if (doColor) {

            for (i = 0; i < srcImage.getExtents()[0]; i++) {

                for (j = 0; j < srcImage.getExtents()[1]; j++) {

                    for (k = 0; k < srcImage.getExtents()[2]; k++) {

                        for (c = 0; c < 4; c++) {
                            sum = 0;

                            for (m = 0; m < srcImage.getExtents()[3]; m++) {
                                sum += srcImage.getFloatC(i, j, k, m, c);
                            }

                            loc = (4 *
                                       ((k * srcImage.getExtents()[1] * srcImage.getExtents()[0]) +
                                            (j * srcImage.getExtents()[0]) + i)) + c;
                            image[loc] = sum / srcImage.getExtents()[3];
                        }
                    }
                }
            }
        } // if (doColor)
        else { // black and white

            for (i = 0; i < srcImage.getExtents()[0]; i++) {

                for (j = 0; j < srcImage.getExtents()[1]; j++) {

                    for (k = 0; k < srcImage.getExtents()[2]; k++) {
                        sum = 0;

                        for (m = 0; m < srcImage.getExtents()[3]; m++) {
                            sum += srcImage.getFloat(i, j, k, m);
                        }

                        loc = (k * srcImage.getExtents()[1] * srcImage.getExtents()[0]) +
                              (j * srcImage.getExtents()[0]) + i;
                        image[loc] = sum / srcImage.getExtents()[3];
                    }
                }
            }
        } // else black and white
    }

    /**
     * Creates a string with the parameters that the image was constructed with.
     *
     * @return  Construction info.
     */
    private String getConstructionInfo() {
        String s;

        s = new String("");

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
                s += "Normalized cross correlation smoothed weighted, ";
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

            // case AlgorithmTransform.NEAREST_NEIGHBOR:   s += "Nearest neighbor, ";   break;
            default:
                s += "Trilinear, ";
                break;
        }

        s += ")\n";

        s = "RegistrationTSOAR(" + srcImage.getImageName() + ", " + s;

        return s;
    }

    /**
     * Gets the tolerance vector based on the degrees of freedom (the length of the tolerance is the degrees of freedom)
     * and the level of subsampling (1, 2, 4, 8).
     *
     * @param   DOF    Degrees of freedom, will be length of vector.
     * @param   level  Level of subsampling (1, 2, 4, 8).
     *
     * @return  New tolerance vector to send to optimization.
     */
    private double[] getTolerance(int DOF, float level) {
        double[] tols = new double[DOF];

        if (DOF == 3) {
            tols[0] = tols[1] = tols[2] = 0.2 * level; // translation
        } else if (DOF == 4) {
            tols[0] = 0.002 * level; // global scaling
            tols[1] = tols[2] = tols[3] = 0.2 * level; // translation
        } else if (DOF >= 6) {

            for (int i = 0; i < DOF; i++) {

                if ((i / 3) == 0) {
                    tols[i] = 0.05 * level;
                } // rotation tolerances
                else if ((i / 3) == 1) {
                    tols[i] = 0.2 * level;
                } // translation tolerances
                else if ((i / 3) == 2) {
                    tols[i] = 0.002 * level;
                } // scaling tolerances
                else if ((i / 3) == 3) {
                    tols[i] = 0.001 * level;
                } // skewing tolerances
            }
        }

        return tols;
    }

    /**
     * Registers the reference volume to all the other volumes in the 4D time series. If the reference volume is the
     * center volume, doesn't register the center volume (because they are the same). Reslices the input volume to
     * isotropic voxels and subsamples it if necessary. Sets up the tolerances based on the given level. At level 1,
     * changes the cost function.
     *
     * @param   refImage  Reference image, already resliced and subsampled.
     * @param   level     Level is 8, 4, 2, or 1. Tolerances are different given the level.
     * @param   previous  List of previous transformation matrices. Null for level 8, otherwise the result of a previous
     *                    call to this method.
     *
     * @return  List of transformation matrices, [0] to reference, [1] to reference, etc.
     */
    private MatrixListItem[] register(ModelSimpleImage refImage, int level, MatrixListItem[] previous) {

        // going from refNum up, then refNum down.  does this mean the transformation matrices are
        // being applied improperly in getTransformedImage() ??
        AlgorithmCostFunctions cost;
        AlgorithmPowellOptBase powell;
        double[] initial = new double[] { 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 };
        MatrixListItem[] results = new MatrixListItem[nVolumes];
        float[] buffer = new float[imageInput.getSize()];
        ModelSimpleImage imageInputIso;
        Vector3f cogR;

        int count = 1;
        int offset = 0;
        int start = 0;
        float factor = 1;

        int maxIter = 4; // PowellOpt3D parameter
        int bracketBound = 20; // PowellOpt3D parameter

        if (level == 8) {
            offset = 0;
            factor = .8f;
            fireProgressStateChanged("Registering at subsample 8");
        } else if (level == 4) {
            offset = 25;
            factor = .8f;
            fireProgressStateChanged("Registering at subsample 4");
        } else if (level == 2) {
            offset = 50;
            factor = .1f;
            fireProgressStateChanged("Registering at subsample 2");
        } else if (level == 1) {
            offset = 75;
            factor = .1f;

            if (normalizedCrossCorrelationSinc) {
                costChoice = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SINC;
                fireProgressStateChanged("No subsampling, new cost function");
            } else {
                fireProgressStateChanged("Registering with no subsampling");
            }
        }

        if (reference == true) {
            start = refNum + 1;
        } else {
            start = refNum;
        }

        for (int i = start; (i < nVolumes) && !threadStopped; i++) {

            if (reference == true) {
                fireProgressStateChanged(offset + (count * 25 / (nVolumes - 1)));
            } else {
                fireProgressStateChanged(offset + (count * 25 / nVolumes));
            }

            count++;

            try {
                srcImage.exportData(i * buffer.length, buffer.length, buffer);
                imageInput.importData(0, buffer, true);
            } catch (IOException error) {
                MipavUtil.displayError("I/O Error while registering.");
                setCompleted(false);
                finalize();

                return null;
            }

            if (resample) {
                transform = new AlgorithmTransform(imageInput, new TransMatrix(4), interp, volumeResolutionsIso[0],
                                                   volumeResolutionsIso[1], volumeResolutionsIso[2],
                                                   volumeExtentsIso[0], volumeExtentsIso[1], volumeExtentsIso[2], false,
                                                   true, false);
                transform.run();

                if (transform.isCompleted() == false) {
                    setCompleted(false);
                    finalize();

                    return null;
                }

                imageInputIso = new ModelSimpleImage(volumeExtentsIso, volumeResolutionsIso,
                                                     transform.getTransformedImage());

                if (transform != null) {

                    if (transform.getTransformedImage() != null) {
                        transform.getTransformedImage().disposeLocal();
                    }

                    transform.disposeLocal();
                    transform = null;
                }
            } // if (resample)
            else {
                imageInputIso = new ModelSimpleImage(volumeExtentsIso, volumeResolutionsIso, imageInput);
            }

            if ((level1Factor > 1) && (level > 1)) {
                imageInputIso = imageInputIso.subsample3dBy2(doColor);
            }

            if ((level2Factor > 1) && (level > 2)) {
                imageInputIso = imageInputIso.subsample3dBy2(doColor);
            }

            if ((level4Factor > 1) && (level > 4)) {
                imageInputIso = imageInputIso.subsample3dBy2(doColor);
            }

            cost = new AlgorithmCostFunctions(refImage, imageInputIso, costChoice, 256 / level, 1);
            cog = AlgorithmRegOAR3D.calculateCenterOfMass3D(imageInputIso, null, doColor);

            if (level == 8) {
                cogR = AlgorithmRegOAR3D.calculateCenterOfMass3D(refImage, null, doColor);

                initial[3] = cog.X - cogR.X;
                initial[4] = cog.Y - cogR.Y;
                initial[5] = cog.Z - cogR.Z;
            }

            if (previous != null) {
                initial = previous[i].initial;

                if (((level == 4) && (level4Factor == 2.0f)) || ((level == 2) && (level2Factor == 2.0f)) ||
                        ((level == 1) && (level1Factor == 2.0f))) {
                    initial[3] *= 2.0f;
                    initial[4] *= 2.0f;
                    initial[5] *= 2.0f;
                }

            } // if (previous != null)

            powell = new AlgorithmPowellOpt3D(this, cog, DOF, cost, getTolerance(DOF, factor), maxIter,
                                              bracketBound);
            powell.setRunningInSeparateThread(runningInSeparateThread);
            Vectornd[] initials = new Vectornd[1];
            initials[0] = new Vectornd(initial);
            powell.setPoints(initials);
            powell.run();
            if ((level == 1) || (setFinalRegistrationAtLevel2 && (level == 2))) {
                results[i] = new MatrixListItem(powell.getCost(0), powell.getMatrix(0, refImage.xRes),
                                                powell.getPoint(0, refImage.xRes));
            }
            else {
                results[i] = new MatrixListItem(powell.getCost(0), powell.getMatrix(0),
                                                powell.getPoint(0));
            }
            cost.disposeLocal();
            powell.disposeLocal();
        }

        for (int i = refNum - 1; (i >= 0) && !threadStopped; i--) {

            if (reference == true) {
                fireProgressStateChanged(offset + (count * 25 / (nVolumes - 1)));
            } else {
                fireProgressStateChanged(offset + (count * 25 / nVolumes));
            }

            count++;

            try {
                srcImage.exportData(i * buffer.length, buffer.length, buffer);
                imageInput.importData(0, buffer, true);
            } catch (IOException error) {
                MipavUtil.displayError("I/O Error while registering.");
                setCompleted(false);
                finalize();

                return null;
            }

            if (resample) {
                transform = new AlgorithmTransform(imageInput, new TransMatrix(4), interp, volumeResolutionsIso[0],
                                                   volumeResolutionsIso[1], volumeResolutionsIso[2],
                                                   volumeExtentsIso[0], volumeExtentsIso[1], volumeExtentsIso[2], false,
                                                   true, false);
                transform.run();

                if (transform.isCompleted() == false) {
                    setCompleted(false);
                    finalize();

                    return null;
                }

                imageInputIso = new ModelSimpleImage(volumeExtentsIso, volumeResolutionsIso,
                                                     transform.getTransformedImage());

                if (transform != null) {

                    if (transform.getTransformedImage() != null) {
                        transform.getTransformedImage().disposeLocal();
                    }

                    transform.disposeLocal();
                    transform = null;
                }
            } // if (resample)
            else {
                imageInputIso = new ModelSimpleImage(volumeExtentsIso, volumeResolutionsIso, imageInput);
            }

            if ((level1Factor > 1) && (level > 1)) {
                imageInputIso = imageInputIso.subsample3dBy2(doColor);
            }

            if ((level2Factor > 1) && (level > 2)) {
                imageInputIso = imageInputIso.subsample3dBy2(doColor);
            }

            if ((level4Factor > 1) && (level > 4)) {
                imageInputIso = imageInputIso.subsample3dBy2(doColor);
            }

            cost = new AlgorithmCostFunctions(refImage, imageInputIso, costChoice, 256 / level, 1);
            cog = AlgorithmRegOAR3D.calculateCenterOfMass3D(imageInputIso, null, doColor);

            if (level == 8) {
                cogR = AlgorithmRegOAR3D.calculateCenterOfMass3D(refImage, null, doColor);
                
                initial[3] = cog.X - cogR.X;
                initial[4] = cog.Y - cogR.Y;
                initial[5] = cog.Z - cogR.Z;
            }

            if (previous != null) {
                initial = previous[i].initial;

                if (((level == 4) && (level4Factor == 2.0f)) || ((level == 2) && (level2Factor == 2.0f)) ||
                        ((level == 1) && (level1Factor == 2.0f))) {
                    initial[3] *= 2.0f;
                    initial[4] *= 2.0f;
                    initial[5] *= 2.0f;
                }
            } // if (previous != null)

            powell = new AlgorithmPowellOpt3D(this, cog, DOF, cost, getTolerance(DOF, factor), maxIter,
                                              bracketBound);
            powell.setRunningInSeparateThread(runningInSeparateThread);
            Vectornd[] initials = new Vectornd[1];
            initials[0] = new Vectornd(initial);
            powell.setPoints(initials);
            powell.run();
            if ((level == 1) || (setFinalRegistrationAtLevel2 && (level == 2))) {
                results[i] = new MatrixListItem(powell.getCost(0), powell.getMatrix(0, refImage.xRes),
                                                powell.getPoint(0, refImage.xRes));
            }
            else {
                results[i] = new MatrixListItem(powell.getCost(0), powell.getMatrix(0),
                                                powell.getPoint(0));
            }
            cost.disposeLocal();
            powell.disposeLocal();
        }

        if (threadStopped) {
            return null;
        }

        return results;
    }
}
