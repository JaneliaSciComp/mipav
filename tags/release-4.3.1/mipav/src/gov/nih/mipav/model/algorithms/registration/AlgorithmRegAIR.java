package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmRegAIR extends AlgorithmBase {
    // This software is a port of portions of version 5.21 of the Automated Image Registration (AIR) package found at
    // the AIR WWW site (currently http://bishopw.loni.ucla.edu/AIR5/. References: 1.) Woods RP, Grafton ST, Holmes CJ,
    // Cherry SR, Mazziotta JC.     Automated image registration: I. General methods and intrasubject,     intramodality
    // validation.  Journal of Computer Assisted     Tomography 1998; 22:141-154. 2.) Woods RP, Grafton ST, Watson JDG,
    // Sicotte NL, Mazziotta JC.     Automated image registration: II. Intersubject validation     of linear and
    // nonlinear methods.  Journal of Computer Assisted     Tomography 1998; 22:155-165.

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * If you are working with two dimensional data, be sure to specify a voxel z size that is greater than or equal to
     * the voxel x and y sizes. Otherwise, interpolation to the irrelevant voxel z size will occur in order to generate
     * cubic voxels.
     */

    /** 2D models - 3 parameters. */
    public static final int RIGID_BODY2D = 23;

    /** 2D models - 4 parameters. */
    public static final int GLOBAL_RESCALING2D = 24;

    /** 2D models - 5 parameters. */
    public static final int FIXED_DETERMINANT2D = 25;

    /** 2D models - 6 parameters. */
    public static final int AFFINE2D = 26;

    /** 3D models - 6 parameters. */
    public static final int RIGID_BODY3D = 6;

    /** 3D models - 7 parameters. */
    public static final int GLOBAL_RESCALING3D = 7;

    /** 3D models - 9 parameters. */
    public static final int TRADITIONAL3D = 9;

    /** 3D models - 12 parameters. */
    public static final int AFFINE3D = 12;

    /** DOCUMENT ME! */
    private static final int SF = 81; /* Default initial sampling interval */

    /** DOCUMENT ME! */
    private static final int SF2 = 1; /* Default final sampling interval
                                       *Cannot be greater than SF */

    /** DOCUMENT ME! */
    private static final int SFF = 3; /* Default sampling interval decrement ratio
                                       *Must be greater than 1 */

    /** DOCUMENT ME! */
    private static final int NOPROGTRY = 5; /* Default iterations without improvement in sd */

    /** DOCUMENT ME! */
    private static final int PARTITIONS = 1; /* 1 for MRI and MRI or PET and PET
                                              * 256 for MRI and PET or forcorrelation ratio cost function */

    /** DOCUMENT ME! */
    private static final boolean POSITIVE_DEFINITE_REQUIRED = true;

    /** DOCUMENT ME! */
    private static final int COORDS = 4; // number of coordinates

    /** DOCUMENT ME! */
    private static final int COEFFS = 4; // number of coefficients

    /** DOCUMENT ME! */
    private static final int MAX_PARAMS = 16; // Maximum number of parameters in any implemented

    // model including rescaling

    /** DOCUMENT ME! */
    private static final int UVDERIVSN6 = 1;

    /** DOCUMENT ME! */
    private static final int UVDERIVSN12 = 2;

    /** DOCUMENT ME! */
    private static final int QVDERIVSN6 = 3;

    /** DOCUMENT ME! */
    private static final int QVDERIVSN12 = 4;

    /** DOCUMENT ME! */
    private static final int UVDERIVSLS6 = 5;

    /** DOCUMENT ME! */
    private static final int UVDERIVSLS12 = 6;

    /** DOCUMENT ME! */
    private static final int QVDERIVSLS6 = 7;

    /** DOCUMENT ME! */
    private static final int QVDERIVSLS12 = 8;

    /** DOCUMENT ME! */
    private static final int UVDERIVSRS6 = 9;

    /** DOCUMENT ME! */
    private static final int UVDERIVSRS12 = 10;

    /** DOCUMENT ME! */
    private static final int QVDERIVSRS6 = 11;

    /** DOCUMENT ME! */
    private static final int QVDERIVSRS12 = 12;

    /** DOCUMENT ME! */
    private static final int UVDERIVSCOR6 = 13;

    /** DOCUMENT ME! */
    private static final int UVDERIVSCOR12 = 14;

    /** DOCUMENT ME! */
    private static final int QVDERIVSCOR6 = 15;

    /** DOCUMENT ME! */
    private static final int QVDERIVSCOR12 = 16;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int costFxn; // 1 for standard deviation of ratio image

    /**
     * 2 for least squares 3 for least squares with intensity rescaling 4 for correlation ratio Only standard deviation
     * of ratio image or correlation ratio can be used with intermodality registration - 1 PET and 1 MRI.
     */
    private boolean cubicInterpolation; /* false turns off cubicInterpolation, true turns on cubicInterpolation */

    /** DOCUMENT ME! */
    private float[] dataOut;

    /** DOCUMENT ME! */
    private double[] dcf = null;

    /** DOCUMENT ME! */
    private double[] dcff = null;

    /** DOCUMENT ME! */
    private double[] dcfr = null;

    /** DOCUMENT ME! */
    private double[] delta = null;

    /** DOCUMENT ME! */
    private double[][][] der = null;

    /** DOCUMENT ME! */
    private double[][][] des = null;

    /** DOCUMENT ME! */
    private double[][] e = new double[4][4];

    /** DOCUMENT ME! */
    private double[][] ecf = null;

    /** DOCUMENT ME! */
    private double[][] ecf2 = null;

    /** DOCUMENT ME! */
    private double[][] ecff = null;

    /** DOCUMENT ME! */
    private double[][] ecfr = null;

    /** DOCUMENT ME! */
    private double[][][][] eer = null;

    /** DOCUMENT ME! */
    private double[][][][] ees = null;

    /** DOCUMENT ME! */
    private boolean entireSource;

    /** DOCUMENT ME! */
    private boolean entireTarget;

    /** DOCUMENT ME! */
    private double[][] er = null;

    /** DOCUMENT ME! */
    private int error;

    /** DOCUMENT ME! */
    private double[][] es = null;

    /** DOCUMENT ME! */
    private double[] gersch = null;

    /** DOCUMENT ME! */
    private boolean haveSourceMask;

    /** DOCUMENT ME! */
    private boolean haveTargetMask;

    /** cubicInterpolation means that images are interpolated to cubic voxels. */
    private boolean interaction; // if true assume interaction of spatial parameter derivatives

    /** DOCUMENT ME! */
    private int interpolation;

    /** DOCUMENT ME! */
    private int iterations; /* number of iterations at a given sampling density */

    /** DOCUMENT ME! */
    private int[] kpvt = null;

    /** DOCUMENT ME! */
    private int model;

    /** DOCUMENT ME! */
    private boolean[] outMsk;

    /** DOCUMENT ME! */
    private float[] pixel5; // target, possibly interpolated

    /** DOCUMENT ME! */
    private boolean[] pixel5Mask;

    /** DOCUMENT ME! */
    private float[] pixel6; // source, possibly interpolated

    /** DOCUMENT ME! */
    private boolean[] pixel6Mask;

    /** DOCUMENT ME! */
    private float precision; /* cost function change must be less than precision for convergence */

    /** DOCUMENT ME! */
    private int[] q1 = null;

    /** DOCUMENT ME! */
    private double[] q2 = null;

    /** DOCUMENT ME! */
    private double[] q3 = null;

    /** DOCUMENT ME! */
    private double[][] q4 = null;

    /** DOCUMENT ME! */
    private double[][] q5 = null;

    /** DOCUMENT ME! */
    private double[][][] q6 = null;

    /** DOCUMENT ME! */
    private double[][][] q7 = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private float[] resultResolutions = new float[3];

    /** DOCUMENT ME! */
    private float[] sigmas = new float[3];

    /** DOCUMENT ME! */
    private float[] sourceArray;

    /** DOCUMENT ME! */
    private float[] sourceCubicMaskFloat;

    /** DOCUMENT ME! */
    private float sourceGaussX;

    /** DOCUMENT ME! */
    private float sourceGaussY;

    /** DOCUMENT ME! */
    private float sourceGaussZ;

    /** DOCUMENT ME! */
    private ModelImage sourceImage;

    /** DOCUMENT ME! */
    private boolean[] sourceMask;

    /** DOCUMENT ME! */
    private float[] sourceMaskFloat;

    /** DOCUMENT ME! */
    private int sourcePartitions;

    /** DOCUMENT ME! */
    private float sourceThreshold;

    /** DOCUMENT ME! */
    private float[] targetArray;

    /** DOCUMENT ME! */
    private float[] targetCubicMaskFloat;

    /** DOCUMENT ME! */
    private float targetGaussX;

    /** DOCUMENT ME! */
    private float targetGaussY;

    /** DOCUMENT ME! */
    private float targetGaussZ;

    /** DOCUMENT ME! */
    private ModelImage targetImage;

    /** DOCUMENT ME! */
    private boolean[] targetMask;

    /** DOCUMENT ME! */
    private float[] targetMaskFloat;

    /** DOCUMENT ME! */
    private int targetPartitions;

    /** DOCUMENT ME! */
    private float targetThreshold;

    /** DOCUMENT ME! */
    private int[] targetUnits = new int[3];

    /** DOCUMENT ME! */
    private BitSet tMask = null; // target image mask

    /** DOCUMENT ME! */
    private double[] tps = new double[MAX_PARAMS]; // Model parameters

    /** DOCUMENT ME! */
    private double[] tpsBest = null; /* Best values of tps at current sampling factor */

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmRegAIR - constructor.
     *
     * @param  resultImage         DOCUMENT ME!
     * @param  targetImage         DOCUMENT ME!
     * @param  entireTarget        if false only selected VOI in target image is used
     * @param  sourceImage         image which is resliced to match the target image
     * @param  entireSource        if false only selected VOI in source image is used
     * @param  transformation      models using different numbers of parameters
     * @param  interpolation       nearest neighbor, bilinear or trilinear, or windowed sinc
     * @param  sourceThreshold     DOCUMENT ME!
     * @param  targetThreshold     DOCUMENT ME!
     * @param  sourceGaussX        DOCUMENT ME!
     * @param  sourceGaussY        DOCUMENT ME!
     * @param  sourceGaussZ        DOCUMENT ME!
     * @param  targetGaussX        DOCUMENT ME!
     * @param  targetGaussY        DOCUMENT ME!
     * @param  targetGaussZ        DOCUMENT ME!
     * @param  sourcePartitions    DOCUMENT ME!
     * @param  targetPartitions    DOCUMENT ME!
     * @param  costFxn             1 for standard deviation of ratio image, 2 for least squares 3 for least squares with
     *                             intensity rescaling 4 for correlation ratio
     * @param  cubicInterpolation  if true images are interpolated to cubic voxels
     * @param  interaction         if true spatial parameter derivatives interact
     * @param  precision           cost function change must be less than precision for convergence
     * @param  iterations          maximum number of iterations at a given sampling density
     */
    public AlgorithmRegAIR(ModelImage resultImage, ModelImage targetImage, boolean entireTarget, ModelImage sourceImage,
                           boolean entireSource, int transformation, int interpolation, float sourceThreshold,
                           float targetThreshold, float sourceGaussX, float sourceGaussY, float sourceGaussZ,
                           float targetGaussX, float targetGaussY, float targetGaussZ, int sourcePartitions,
                           int targetPartitions, int costFxn, boolean cubicInterpolation, boolean interaction,
                           float precision, int iterations) {
        this.resultImage = resultImage;
        this.targetImage = targetImage;
        this.entireTarget = entireTarget;
        this.sourceImage = sourceImage;
        this.entireSource = entireSource;
        model = transformation;
        this.interpolation = interpolation;
        this.sourceThreshold = sourceThreshold;
        this.targetThreshold = targetThreshold;
        this.sourceGaussX = sourceGaussX;
        this.sourceGaussY = sourceGaussY;
        this.sourceGaussZ = sourceGaussZ;
        this.targetGaussX = targetGaussX;
        this.targetGaussY = targetGaussY;
        this.targetGaussZ = targetGaussZ;
        this.sourcePartitions = sourcePartitions;
        this.targetPartitions = targetPartitions;
        this.costFxn = costFxn;
        this.cubicInterpolation = cubicInterpolation;
        this.interaction = interaction;
        this.precision = precision;
        this.iterations = iterations;
        haveTargetMask = false;
        haveSourceMask = false;

        if (entireSource == false) {
            mask = sourceImage.generateVOIMask();
        }

        if (entireTarget == false) {
            tMask = targetImage.generateVOIMask();
        }

    }

    /**
     * AlgorithmRegAIR - constructor.
     *
     * @param  resultImage         DOCUMENT ME!
     * @param  targetImage         DOCUMENT ME!
     * @param  targetMask          mask for selected VOI in target image
     * @param  sourceImage         image which is resliced to match the target image
     * @param  sourceMask          mask for selected VOI in source image
     * @param  transformation      models using different numbers of parameters
     * @param  interpolation       nearest neighbor, bilinear or trilinear, or windowed sinc
     * @param  sourceThreshold     DOCUMENT ME!
     * @param  targetThreshold     DOCUMENT ME!
     * @param  sourceGaussX        DOCUMENT ME!
     * @param  sourceGaussY        DOCUMENT ME!
     * @param  sourceGaussZ        DOCUMENT ME!
     * @param  targetGaussX        DOCUMENT ME!
     * @param  targetGaussY        DOCUMENT ME!
     * @param  targetGaussZ        DOCUMENT ME!
     * @param  sourcePartitions    DOCUMENT ME!
     * @param  targetPartitions    DOCUMENT ME!
     * @param  costFxn             1 for standard deviation of ratio image, 2 for least squares 3 for least squares with
     *                             intensity rescaling 4 for correlation ratio
     * @param  cubicInterpolation  if true images are interpolated to cubic voxels
     * @param  interaction         if true spatial parameter derivatives interact
     * @param  precision           cost function change must be less than precision for convergence
     * @param  iterations          maximum number of iterations at a given sampling density
     */
    public AlgorithmRegAIR(ModelImage resultImage, ModelImage targetImage, boolean[] targetMask, ModelImage sourceImage,
                           boolean[] sourceMask, int transformation, int interpolation, float sourceThreshold,
                           float targetThreshold, float sourceGaussX, float sourceGaussY, float sourceGaussZ,
                           float targetGaussX, float targetGaussY, float targetGaussZ, int sourcePartitions,
                           int targetPartitions, int costFxn, boolean cubicInterpolation, boolean interaction,
                           float precision, int iterations) {
        this.resultImage = resultImage;
        this.targetImage = targetImage;
        this.targetMask = targetMask;
        this.sourceImage = sourceImage;
        this.sourceMask = sourceMask;
        model = transformation;
        this.interpolation = interpolation;
        this.sourceThreshold = sourceThreshold;
        this.targetThreshold = targetThreshold;
        this.sourceGaussX = sourceGaussX;
        this.sourceGaussY = sourceGaussY;
        this.sourceGaussZ = sourceGaussZ;
        this.targetGaussX = targetGaussX;
        this.targetGaussY = targetGaussY;
        this.targetGaussZ = targetGaussZ;
        this.sourcePartitions = sourcePartitions;
        this.targetPartitions = targetPartitions;
        this.costFxn = costFxn;
        this.cubicInterpolation = cubicInterpolation;
        this.interaction = interaction;
        this.precision = precision;
        this.iterations = iterations;
        haveTargetMask = true;
        haveSourceMask = true;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        int i, j, k;
        sourceArray = null;
        targetArray = null;
        sourceMask = null;
        targetMask = null;
        tps = null;
        sigmas = null;
        dcf = null;
        dcff = null;
        gersch = null;

        if (ecf != null) {

            for (i = 0; i < ecf.length; i++) {
                ecf[i] = null;
            }

            ecf = null;
        } // if (ecf != null)

        if (ecff != null) {

            for (i = 0; i < ecff.length; i++) {
                ecff[i] = null;
            }

            ecff = null;
        } // if (ecff != null)

        if (ecfr != null) {

            for (i = 0; i < ecfr.length; i++) {
                ecfr[i] = null;
            }

            ecfr = null;
        } // if (ecfr != null)

        if (ecf2 != null) {

            for (i = 0; i < ecf2.length; i++) {
                ecf2[i] = null;
            }

            ecf2 = null;
        } // if (ecf2 != null)

        if (es != null) {

            for (i = 0; i < es.length; i++) {
                es[i] = null;
            }

            es = null;
        } // if (es != null)

        if (er != null) {

            for (i = 0; i < er.length; i++) {
                er[i] = null;
            }

            er = null;
        } // if (er != null)

        if (des != null) {

            for (i = 0; i < des.length; i++) {

                for (j = 0; j < des[i].length; j++) {
                    des[i][j] = null;
                }
            }

            for (i = 0; i < des.length; i++) {
                des[i] = null;
            }

            des = null;
        } // if (des != null)

        if (der != null) {

            for (i = 0; i < der.length; i++) {

                for (j = 0; j < der[i].length; j++) {
                    der[i][j] = null;
                }
            }

            for (i = 0; i < der.length; i++) {
                der[i] = null;
            }

            der = null;
        } // if (der != null)

        if (ees != null) {

            for (i = 0; i < ees.length; i++) {

                for (j = 0; j < ees[i].length; j++) {

                    for (k = 0; k < ees[i][j].length; k++) {
                        ees[i][j][k] = null;
                    }
                }
            }

            for (i = 0; i < ees.length; i++) {

                for (j = 0; j < ees[i].length; j++) {
                    ees[i][j] = null;
                }
            }

            for (i = 0; i < ees.length; i++) {
                ees[i] = null;
            }

            ees = null;
        } // if (ees != null)

        if (eer != null) {

            for (i = 0; i < eer.length; i++) {

                for (j = 0; j < eer[i].length; j++) {

                    for (k = 0; k < eer[i][j].length; k++) {
                        eer[i][j][k] = null;
                    }
                }
            }

            for (i = 0; i < eer.length; i++) {

                for (j = 0; j < eer[i].length; j++) {
                    eer[i][j] = null;
                }
            }

            for (i = 0; i < eer.length; i++) {
                eer[i] = null;
            }

            eer = null;
        } // if (eer != null)

        q1 = null;
        q2 = null;
        q3 = null;

        if (q4 != null) {

            for (i = 0; i < q4.length; i++) {
                q4[i] = null;
            }

            q4 = null;
        }

        if (q5 != null) {

            for (i = 0; i < q5.length; i++) {
                q5[i] = null;
            }

            q5 = null;
        } // if (q5 != null)

        if (q6 != null) {

            for (i = 0; i < q6.length; i++) {

                for (j = 0; j < q6[i].length; j++) {
                    q6[i][j] = null;
                }
            }

            for (i = 0; i < q6.length; i++) {
                q6[i] = null;
            }

            q6 = null;
        } // if (q6 != null)

        if (q7 != null) {

            for (i = 0; i < q7.length; i++) {

                for (j = 0; j < q7[i].length; j++) {
                    q7[i][j] = null;
                }
            }

            for (i = 0; i < q7.length; i++) {
                q7[i] = null;
            }

            q7 = null;
        } // if (q7 != null)

        delta = null;
        tpsBest = null;
        kpvt = null;

        if (e != null) {

            for (i = 0; i < e.length; i++) {
                e[i] = null;
            }

            e = null;
        }

        System.gc();
        super.finalize();
    }

    /**
     * run - starts the program.
     */
    public void runAlgorithm() {

        int sourceXDim;
        int sourceYDim;
        int sourceZDim;
        int sourceSliceSize;
        int sourceVolSize;
        int targetXDim;
        int targetYDim;
        int targetZDim;
        int targetSliceSize;
        int targetVolSize;
        float sourceVoxelX;
        float sourceVoxelY;
        float sourceVoxelZ;
        float targetVoxelX;
        float targetVoxelY;
        float targetVoxelZ;
        int i, j, k;

        int sampleFactor; /* initial sampling interval */
        int sampleFactor2; /* final sampling interval */
        int sfFactor; /* default sampling interval decrement ratio */

        int noProgTries; /* number of iterations without sd improvement before forced
                          *termination */
        int scales; /* number of scaling parameters (currently 0 or 1)*/
        int parameters = 12; /* number of formal parameters in specified model */
        int param_int = 12; /* number of parameters in derivative routine derivs */
        boolean memory; /* tells align routine whether to allocate extra
                         *memory needed for RIU cost function */
        int derivsN;
        float pixelSize;
        AlgorithmGaussianBlur gaussianBlurAlgo;
        boolean inPlane = false;
        int direction;
        int partitions;

        int coords; // number of coordinates
        int coeffs; // number of coefficients
        float sourceCubicVoxelX = 0;
        float sourceCubicVoxelY = 0;
        float sourceCubicVoxelZ = 0;
        float targetCubicVoxelX = 0;
        float targetCubicVoxelY = 0;
        float targetCubicVoxelZ = 0;
        int sourceCubicXDim = 0;
        int sourceCubicYDim = 0;
        int sourceCubicZDim = 0;
        int targetCubicXDim = 0;
        int targetCubicYDim = 0;
        int targetCubicZDim = 0;
        int sourceCubicVolSize = 0;
        int targetCubicVolSize = 0;
        int iters; /*number current iteration at current sampling density*/
        int tries; /* number of iterations since sd last improved */
        boolean success;
        int s, t;
        int jj;
        double pixel_size_s;
        float xoom1, yoom1, zoom1;
        int sourceUnitsX, sourceUnitsY, sourceUnitsZ;
        int targetUnitsX, targetUnitsY, targetUnitsZ;
        boolean doConvert;
        int resultZDim;
        boolean posdefreq; /* Positive definite Hessian required if true */
        double cfBest = Double.MAX_VALUE; /* Best value of cost function at current sampling */
        double max5 = 0.0; // maximum target in mask valid area
        double max6 = 0.0; // maximum source in mask valid area
        double deltacf;
        double cf = Double.MAX_VALUE;

        fireProgressStateChanged(sourceImage.getImageName(), "Registering source image...");

        int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        int yScreen = Toolkit.getDefaultToolkit().getScreenSize().height;
        
        

        

        switch (model) {

            case RIGID_BODY2D:
                parameters = 3;
                param_int = 6;
                inPlane = true;
                break;

            case GLOBAL_RESCALING2D:
                parameters = 4;
                param_int = 6;
                inPlane = true;
                break;

            case FIXED_DETERMINANT2D:
                parameters = 5;
                param_int = 6;
                inPlane = true;
                break;

            case AFFINE2D:
                parameters = 6;
                param_int = 6;
                inPlane = true;
                break;

            case RIGID_BODY3D:
                parameters = 6;
                param_int = 12;
                inPlane = false;
                break;

            case GLOBAL_RESCALING3D:
                parameters = 7;
                param_int = 12;
                inPlane = false;
                break;

            case TRADITIONAL3D:
                parameters = 9;
                param_int = 12;
                inPlane = false;
                break;

            case AFFINE3D:
                parameters = 12;
                param_int = 12;
                inPlane = false;
                break;
        }

        sourceXDim = sourceImage.getExtents()[0];
        sourceYDim = sourceImage.getExtents()[1];
        sourceSliceSize = sourceXDim * sourceYDim;

        if (inPlane) {
            sourceZDim = 1;
        } else {
            sourceZDim = sourceImage.getExtents()[2];
        }

        sourceVolSize = sourceSliceSize * sourceZDim;
        sourceArray = new float[sourceVolSize];

        try {
            sourceImage.exportData(0, sourceVolSize, sourceArray); // locks and releases and lock
        } catch (IOException error) {
            finalize();
            displayError("AlgorithmRegAIR: IOException on sourceImage.exportData");
            
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            finalize();
            displayError("AlgorithmRegAIR: Out of memory on sourceImage.exportData");
            
            setCompleted(false);

            return;
        }

        sourceVoxelX = sourceImage.getFileInfo()[0].getResolutions()[0];
        sourceVoxelY = sourceImage.getFileInfo()[0].getResolutions()[1];

        if (!inPlane) {
            sourceVoxelZ = sourceImage.getFileInfo()[0].getResolutions()[2];
        } else {
            sourceVoxelZ = sourceVoxelX;
        }

        if (!haveSourceMask) {
            sourceMask = new boolean[sourceVolSize];

            if (entireSource == false) {

                // mask = sourceImage.generateVOIMask();
                for (i = 0; i < sourceVolSize; i++) {

                    if (mask.get(i)) {
                        sourceMask[i] = true;
                    } else {
                        sourceMask[i] = false;
                    }
                }

                mask = null;
            } // if (entireSource == false)
            else {

                for (i = 0; i < sourceVolSize; i++) {
                    sourceMask[i] = true;
                }
            }
        } // if (!haveSourceMask)

        targetXDim = targetImage.getExtents()[0];
        targetYDim = targetImage.getExtents()[1];
        targetSliceSize = targetXDim * targetYDim;

        if (inPlane) {
            targetZDim = 1;
        } else {
            targetZDim = targetImage.getExtents()[2];
        }

        targetVolSize = targetSliceSize * targetZDim;
        targetArray = new float[targetVolSize];

        try {
            targetImage.exportData(0, targetVolSize, targetArray); // locks and releases and lock
        } catch (IOException error) {
            finalize();
            displayError("AlgorithmRegAIR: IOException on targetImage.exportData");
            
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            finalize();
            displayError("AlgorithmRegAIR: Out of memory on targetImage.exportData");
            
            setCompleted(false);

            return;
        }

        targetVoxelX = targetImage.getFileInfo()[0].getResolutions()[0];
        targetVoxelY = targetImage.getFileInfo()[0].getResolutions()[1];

        if (!inPlane) {
            targetVoxelZ = targetImage.getFileInfo()[0].getResolutions()[2];
        } else {
            targetVoxelZ = targetVoxelX;
        }

        doConvert = false;
        sourceUnitsX = sourceImage.getFileInfo()[0].getUnitsOfMeasure()[0];
        sourceUnitsY = sourceImage.getFileInfo()[0].getUnitsOfMeasure()[1];
        sourceUnitsZ = sourceImage.getFileInfo()[0].getUnitsOfMeasure()[2];
        targetUnitsX = targetImage.getFileInfo()[0].getUnitsOfMeasure()[0];
        targetUnitsY = targetImage.getFileInfo()[0].getUnitsOfMeasure()[1];
        targetUnitsZ = targetImage.getFileInfo()[0].getUnitsOfMeasure()[2];
        targetUnits[0] = targetUnitsX;
        targetUnits[1] = targetUnitsY;
        targetUnits[2] = targetUnitsZ;

        if ((targetImage.getNDims() > 2) &&
                ((targetUnitsX != targetUnitsY) || (targetUnitsX != targetUnitsZ) || (targetUnitsX != sourceUnitsX) ||
                     (targetUnitsX != sourceUnitsY) || (targetUnitsX != sourceUnitsZ))) {
            doConvert = true;
        } else if ((targetImage.getNDims() == 2) &&
                       ((targetUnitsX != targetUnitsY) || (targetUnitsX != sourceUnitsX) ||
                            (targetUnitsX != sourceUnitsY))) {
            doConvert = true;
        }

        if (doConvert) {

            // The units of measure were not identical.  Convert to millimeters
            if ((sourceUnitsX == FileInfoBase.INCHES) || (sourceUnitsX == FileInfoBase.CENTIMETERS) ||
                    (sourceUnitsX == FileInfoBase.ANGSTROMS) || (sourceUnitsX == FileInfoBase.MICROMETERS) ||
                    (sourceUnitsX == FileInfoBase.MILLIMETERS) || (sourceUnitsX == FileInfoBase.METERS) ||
                    (sourceUnitsX == FileInfoBase.KILOMETERS) || (sourceUnitsX == FileInfoBase.MILES) ||
                    (sourceUnitsX == FileInfoBase.NANOMETERS)) {

                if (sourceVoxelX == 0.0f) {
                    MipavUtil.displayWarning("source x resolution was recorded as zero " +
                                             " It is being changed to 1.0");
                    sourceVoxelX = 1.0f;
                } else if (sourceVoxelX < 0.0f) {
                    MipavUtil.displayWarning("source X resolution was recorded as " + sourceVoxelX +
                                             " It is being changed to " + -sourceVoxelX);
                    sourceVoxelX = -sourceVoxelX;
                }

                // Be ready for conversions between different units.
                if (sourceUnitsX == FileInfoBase.MILLIMETERS) { // leave unchanged
                } else if (sourceUnitsX == FileInfoBase.INCHES) {
                    sourceVoxelX = 25.4f * sourceVoxelX;
                } else if (sourceUnitsX == FileInfoBase.CENTIMETERS) {
                    sourceVoxelX = 10.0f * sourceVoxelX;
                } else if (sourceUnitsX == FileInfoBase.ANGSTROMS) {
                    sourceVoxelX = 1.0e-7f * sourceVoxelX;
                } else if (sourceUnitsX == FileInfoBase.NANOMETERS) {
                    sourceVoxelX = 1.0e-6f * sourceVoxelX;
                } else if (sourceUnitsX == FileInfoBase.MICROMETERS) {
                    sourceVoxelX = 1.0e-3f * sourceVoxelX;
                } else if (sourceUnitsX == FileInfoBase.METERS) {
                    sourceVoxelX = 1.0e3f * sourceVoxelX;
                } else if (sourceUnitsX == FileInfoBase.KILOMETERS) {
                    sourceVoxelX = 1.0e6f * sourceVoxelX;
                } else if (sourceUnitsX == FileInfoBase.MILES) {
                    sourceVoxelX = 1.6093e6f * sourceVoxelX;
                }
            }

            if ((sourceUnitsY == FileInfoBase.INCHES) || (sourceUnitsY == FileInfoBase.CENTIMETERS) ||
                    (sourceUnitsY == FileInfoBase.ANGSTROMS) || (sourceUnitsY == FileInfoBase.MICROMETERS) ||
                    (sourceUnitsY == FileInfoBase.MILLIMETERS) || (sourceUnitsY == FileInfoBase.METERS) ||
                    (sourceUnitsY == FileInfoBase.KILOMETERS) || (sourceUnitsY == FileInfoBase.MILES) ||
                    (sourceUnitsY == FileInfoBase.NANOMETERS)) {

                if (sourceVoxelY == 0.0f) {
                    MipavUtil.displayWarning("source y resolution was recorded as zero " +
                                             " It is being changed to 1.0");
                    sourceVoxelY = 1.0f;
                } else if (sourceVoxelY < 0.0f) {
                    MipavUtil.displayWarning("source Y resolution was recorded as " + sourceVoxelY +
                                             " It is being changed to " + -sourceVoxelY);
                    sourceVoxelY = -sourceVoxelY;
                }

                // Be ready for conversions between different units.
                if (sourceUnitsY == FileInfoBase.MILLIMETERS) { // leave unchanged
                } else if (sourceUnitsY == FileInfoBase.INCHES) {
                    sourceVoxelY = 25.4f * sourceVoxelY;
                } else if (sourceUnitsY == FileInfoBase.CENTIMETERS) {
                    sourceVoxelY = 10.0f * sourceVoxelY;
                } else if (sourceUnitsY == FileInfoBase.ANGSTROMS) {
                    sourceVoxelY = 1.0e-7f * sourceVoxelY;
                } else if (sourceUnitsY == FileInfoBase.NANOMETERS) {
                    sourceVoxelY = 1.0e-6f * sourceVoxelY;
                } else if (sourceUnitsY == FileInfoBase.MICROMETERS) {
                    sourceVoxelY = 1.0e-3f * sourceVoxelY;
                } else if (sourceUnitsY == FileInfoBase.METERS) {
                    sourceVoxelY = 1.0e3f * sourceVoxelY;
                } else if (sourceUnitsY == FileInfoBase.KILOMETERS) {
                    sourceVoxelY = 1.0e6f * sourceVoxelY;
                } else if (sourceUnitsY == FileInfoBase.MILES) {
                    sourceVoxelY = 1.6093e6f * sourceVoxelY;
                }
            }

            if (inPlane) {
                sourceVoxelZ = sourceVoxelX;
            } else if ((sourceUnitsZ == FileInfoBase.INCHES) || (sourceUnitsZ == FileInfoBase.CENTIMETERS) ||
                           (sourceUnitsZ == FileInfoBase.ANGSTROMS) || (sourceUnitsZ == FileInfoBase.MICROMETERS) ||
                           (sourceUnitsZ == FileInfoBase.MILLIMETERS) || (sourceUnitsZ == FileInfoBase.METERS) ||
                           (sourceUnitsZ == FileInfoBase.KILOMETERS) || (sourceUnitsZ == FileInfoBase.MILES) ||
                           (sourceUnitsZ == FileInfoBase.NANOMETERS)) {

                if (sourceVoxelZ == 0.0f) {
                    MipavUtil.displayWarning("source z resolution was recorded as zero " +
                                             " It is being changed to 1.0");
                    sourceVoxelZ = 1.0f;
                } else if (sourceVoxelZ < 0.0f) {
                    MipavUtil.displayWarning("source Z resolution was recorded as " + sourceVoxelZ +
                                             " It is being changed to " + -sourceVoxelZ);
                    sourceVoxelZ = -sourceVoxelZ;
                }

                // Be ready for conversions between different units.
                if (sourceUnitsZ == FileInfoBase.MILLIMETERS) { // leave unchanged
                } else if (sourceUnitsZ == FileInfoBase.INCHES) {
                    sourceVoxelZ = 25.4f * sourceVoxelZ;
                } else if (sourceUnitsZ == FileInfoBase.CENTIMETERS) {
                    sourceVoxelZ = 10.0f * sourceVoxelZ;
                } else if (sourceUnitsZ == FileInfoBase.ANGSTROMS) {
                    sourceVoxelZ = 1.0e-7f * sourceVoxelZ;
                } else if (sourceUnitsZ == FileInfoBase.NANOMETERS) {
                    sourceVoxelZ = 1.0e-6f * sourceVoxelZ;
                } else if (sourceUnitsZ == FileInfoBase.MICROMETERS) {
                    sourceVoxelZ = 1.0e-3f * sourceVoxelZ;
                } else if (sourceUnitsZ == FileInfoBase.METERS) {
                    sourceVoxelZ = 1.0e3f * sourceVoxelZ;
                } else if (sourceUnitsZ == FileInfoBase.KILOMETERS) {
                    sourceVoxelZ = 1.0e6f * sourceVoxelZ;
                } else if (sourceUnitsZ == FileInfoBase.MILES) {
                    sourceVoxelZ = 1.6093e6f * sourceVoxelZ;
                }
            }

            if ((targetUnitsX == FileInfoBase.INCHES) || (targetUnitsX == FileInfoBase.CENTIMETERS) ||
                    (targetUnitsX == FileInfoBase.ANGSTROMS) || (targetUnitsX == FileInfoBase.MICROMETERS) ||
                    (targetUnitsX == FileInfoBase.MILLIMETERS) || (targetUnitsX == FileInfoBase.METERS) ||
                    (targetUnitsX == FileInfoBase.KILOMETERS) || (targetUnitsX == FileInfoBase.MILES) ||
                    (targetUnitsX == FileInfoBase.NANOMETERS)) {

                targetUnits[0] = FileInfoBase.MILLIMETERS;

                if (targetVoxelX == 0.0f) {
                    MipavUtil.displayWarning("target x resolution was recorded as zero " +
                                             " It is being changed to 1.0");
                    targetVoxelX = 1.0f;
                } else if (targetVoxelX < 0.0f) {
                    MipavUtil.displayWarning("target X resolution was recorded as " + targetVoxelX +
                                             " It is being changed to " + -targetVoxelX);
                    targetVoxelX = -targetVoxelX;
                }

                // Be ready for conversions between different units.
                if (targetUnitsX == FileInfoBase.MILLIMETERS) { // leave unchanged
                } else if (targetUnitsX == FileInfoBase.INCHES) {
                    targetVoxelX = 25.4f * targetVoxelX;
                } else if (targetUnitsX == FileInfoBase.CENTIMETERS) {
                    targetVoxelX = 10.0f * targetVoxelX;
                } else if (targetUnitsX == FileInfoBase.ANGSTROMS) {
                    targetVoxelX = 1.0e-7f * targetVoxelX;
                } else if (targetUnitsX == FileInfoBase.NANOMETERS) {
                    targetVoxelX = 1.0e-6f * targetVoxelX;
                } else if (targetUnitsX == FileInfoBase.MICROMETERS) {
                    targetVoxelX = 1.0e-3f * targetVoxelX;
                } else if (targetUnitsX == FileInfoBase.METERS) {
                    targetVoxelX = 1.0e3f * targetVoxelX;
                } else if (targetUnitsX == FileInfoBase.KILOMETERS) {
                    targetVoxelX = 1.0e6f * targetVoxelX;
                } else if (targetUnitsX == FileInfoBase.MILES) {
                    targetVoxelX = 1.6093e6f * targetVoxelX;
                }
            }

            if ((targetUnitsY == FileInfoBase.INCHES) || (targetUnitsY == FileInfoBase.CENTIMETERS) ||
                    (targetUnitsY == FileInfoBase.ANGSTROMS) || (targetUnitsY == FileInfoBase.MICROMETERS) ||
                    (targetUnitsY == FileInfoBase.MILLIMETERS) || (targetUnitsY == FileInfoBase.METERS) ||
                    (targetUnitsY == FileInfoBase.KILOMETERS) || (targetUnitsY == FileInfoBase.MILES) ||
                    (targetUnitsY == FileInfoBase.NANOMETERS)) {

                targetUnits[1] = FileInfoBase.MILLIMETERS;

                if (targetVoxelY == 0.0f) {
                    MipavUtil.displayWarning("target y resolution was recorded as zero " +
                                             " It is being changed to 1.0");
                    targetVoxelY = 1.0f;
                } else if (targetVoxelY < 0.0f) {
                    MipavUtil.displayWarning("target Y resolution was recorded as " + targetVoxelY +
                                             " It is being changed to " + -targetVoxelY);
                    targetVoxelY = -targetVoxelY;
                }

                // Be ready for conversions between different units.
                if (targetUnitsY == FileInfoBase.MILLIMETERS) { // leave unchanged
                } else if (targetUnitsY == FileInfoBase.INCHES) {
                    targetVoxelY = 25.4f * targetVoxelY;
                } else if (targetUnitsY == FileInfoBase.CENTIMETERS) {
                    targetVoxelY = 10.0f * targetVoxelY;
                } else if (targetUnitsY == FileInfoBase.ANGSTROMS) {
                    targetVoxelY = 1.0e-7f * targetVoxelY;
                } else if (targetUnitsY == FileInfoBase.NANOMETERS) {
                    targetVoxelY = 1.0e-6f * targetVoxelY;
                } else if (targetUnitsY == FileInfoBase.MICROMETERS) {
                    targetVoxelY = 1.0e-3f * targetVoxelY;
                } else if (targetUnitsY == FileInfoBase.METERS) {
                    targetVoxelY = 1.0e3f * targetVoxelY;
                } else if (targetUnitsY == FileInfoBase.KILOMETERS) {
                    targetVoxelY = 1.0e6f * targetVoxelY;
                } else if (targetUnitsY == FileInfoBase.MILES) {
                    targetVoxelY = 1.6093e6f * targetVoxelY;
                }
            }

            if (inPlane) {
                targetVoxelZ = targetVoxelX;
            } else if ((targetUnitsZ == FileInfoBase.INCHES) || (targetUnitsZ == FileInfoBase.CENTIMETERS) ||
                           (targetUnitsZ == FileInfoBase.ANGSTROMS) || (targetUnitsZ == FileInfoBase.MICROMETERS) ||
                           (targetUnitsZ == FileInfoBase.MILLIMETERS) || (targetUnitsZ == FileInfoBase.METERS) ||
                           (targetUnitsZ == FileInfoBase.KILOMETERS) || (targetUnitsZ == FileInfoBase.MILES) ||
                           (targetUnitsZ == FileInfoBase.NANOMETERS)) {

                targetUnits[2] = FileInfoBase.MILLIMETERS;

                if (targetVoxelZ == 0.0f) {
                    MipavUtil.displayWarning("target z resolution was recorded as zero " +
                                             " It is being changed to 1.0");
                    targetVoxelZ = 1.0f;
                } else if (targetVoxelZ < 0.0f) {
                    MipavUtil.displayWarning("target Z resolution was recorded as " + targetVoxelZ +
                                             " It is being changed to " + -targetVoxelZ);
                    targetVoxelZ = -targetVoxelZ;
                }

                // Be ready for conversions between different units.
                if (targetUnitsZ == FileInfoBase.MILLIMETERS) { // leave unchanged
                } else if (targetUnitsZ == FileInfoBase.INCHES) {
                    targetVoxelZ = 25.4f * targetVoxelZ;
                } else if (targetUnitsZ == FileInfoBase.CENTIMETERS) {
                    targetVoxelZ = 10.0f * targetVoxelZ;
                } else if (targetUnitsZ == FileInfoBase.ANGSTROMS) {
                    targetVoxelZ = 1.0e-7f * targetVoxelZ;
                } else if (targetUnitsZ == FileInfoBase.NANOMETERS) {
                    targetVoxelZ = 1.0e-6f * targetVoxelZ;
                } else if (targetUnitsZ == FileInfoBase.MICROMETERS) {
                    targetVoxelZ = 1.0e-3f * targetVoxelZ;
                } else if (targetUnitsZ == FileInfoBase.METERS) {
                    targetVoxelZ = 1.0e3f * targetVoxelZ;
                } else if (targetUnitsZ == FileInfoBase.KILOMETERS) {
                    targetVoxelZ = 1.0e6f * targetVoxelZ;
                } else if (targetUnitsZ == FileInfoBase.MILES) {
                    targetVoxelZ = 1.6093e6f * targetVoxelZ;
                }
            }
        } // if (doConvert)

        if (!haveTargetMask) {
            targetMask = new boolean[targetVolSize];

            if (entireTarget == false) {

                // mask = targetImage.generateVOIMask();
                for (i = 0; i < targetVolSize; i++) {

                    if (tMask.get(i)) {
                        targetMask[i] = true;
                    } else {
                        targetMask[i] = false;
                    }
                }

                tMask = null;
            } // if (entireTarget == false)
            else {

                for (i = 0; i < targetVolSize; i++) {
                    targetMask[i] = true;
                }
            }
        } // if (!haveTargetMask)

        sampleFactor = SF; // initial sampling interval

        if (sampleFactor < 1) {
            finalize();
            MipavUtil.displayError("sampleFactor cannot be less than 1");
            
            setCompleted(false);

            return;
        }

        sampleFactor2 = SF2; // final sampling interval

        if (sampleFactor2 > sampleFactor) {
            finalize();
            MipavUtil.displayError("final sampling cannot be > than initial sampling");
            
            setCompleted(false);

            return;
        }

        sfFactor = SFF; // default sampling interval decrement ratio

        if (sampleFactor == sampleFactor2) {
            sfFactor = 2;
        }

        if (sfFactor <= 1) {
            finalize();
            MipavUtil.displayError("sampling decrement ratio must be > 1");
            
            setCompleted(false);

            return;
        }

        noProgTries = NOPROGTRY; // number of iterations without sd improvement before forced

        // termination
        posdefreq = POSITIVE_DEFINITE_REQUIRED;

        if (costFxn == 3) {
            scales = 1;
        } else {
            scales = 0; // number of rescaling parameters
        }

        coords = COORDS;
        coeffs = COEFFS;

        if ((costFxn == 1) || (costFxn == 4)) {
            memory = true;
        } else {
            memory = false;
        }

        if (costFxn == 1) {

            if (interaction) {

                if (param_int == 6) {
                    derivsN = UVDERIVSN6;
                } else { // param_int == 12
                    derivsN = UVDERIVSN12;
                }
            } else {

                if (param_int == 6) {
                    derivsN = QVDERIVSN6;
                } else { // param_int == 12
                    derivsN = QVDERIVSN12;
                }
            }
        } // if (costFxn == 1)
        else if (costFxn == 2) {

            if (interaction) {

                if (param_int == 6) {
                    derivsN = UVDERIVSLS6;
                } else { // param_int == 12
                    derivsN = UVDERIVSLS12;
                }
            } else {

                if (param_int == 6) {
                    derivsN = QVDERIVSLS6;
                } else { // param_int == 12
                    derivsN = QVDERIVSLS12;
                }
            }
        } // else if (costFxn == 2)
        else if (costFxn == 3) { // costFxn == 3

            if (interaction) {

                if (param_int == 6) {
                    derivsN = UVDERIVSRS6;
                } else { // param_int == 12
                    derivsN = UVDERIVSRS12;
                }
            } else {

                if (param_int == 6) {
                    derivsN = QVDERIVSRS6;
                } else { // param_int == 12
                    derivsN = QVDERIVSRS12;
                }
            }
        } // else if (costFxn == 3)
        else { // costFxn == 4

            if (interaction) {

                if (param_int == 6) {
                    derivsN = UVDERIVSCOR6;
                } else {
                    derivsN = UVDERIVSCOR12;
                }
            } else {

                if (param_int == 6) {
                    derivsN = QVDERIVSCOR6;
                } else {
                    derivsN = QVDERIVSCOR12;
                }
            }
        }

        param_int++; /* This allows allocation of space for the scaling parameter */

        /* Complete initialization */
        tps[parameters] = 1.0;

        switch (model) {

            case TRADITIONAL3D:
                tps[7] = 1;
                tps[8] = 1;

            /*fallthrough*/ case GLOBAL_RESCALING3D:
                tps[6] = 1;

            /*fallthrough*/ case RIGID_BODY3D:
                tps[0] = 0;
                tps[1] = 0;
                tps[2] = 0;
                tps[3] = 0;
                tps[4] = 0;
                tps[5] = 0;
                break;

            case AFFINE3D:

                /* Complete default initialization for 12 parameter model */

                /* The default consists of center alignment without rotation or rescaling */

                /* The adjustment for cubicInterpolation is applied later after cubicInterpolation status is specified
                 */
                tps[0] = targetVoxelX / sourceVoxelX;
                tps[1] = 0.0;
                tps[2] = 0.0;
                tps[3] = ((sourceXDim - 1.0) - ((targetXDim - 1.0) * (targetVoxelX / sourceVoxelX))) / 2.0;

                tps[4] = 0.0;
                tps[5] = targetVoxelY / sourceVoxelY;
                tps[6] = 0.0;
                tps[7] = ((sourceYDim - 1.0) - ((targetYDim - 1.0) * (targetVoxelY / sourceVoxelY))) / 2.0;

                tps[8] = 0.0;
                tps[9] = 0.0;
                tps[10] = targetVoxelZ / sourceVoxelZ;
                tps[11] = ((sourceZDim - 1.0) - ((targetZDim - 1.0) * (targetVoxelZ / sourceVoxelZ))) / 2.0;
                break;

            case GLOBAL_RESCALING2D:
                tps[3] = 1;

            /*fallthrough*/ case RIGID_BODY2D:
                tps[0] = 0;
                tps[1] = 0;
                tps[2] = 0;
                break;

            case FIXED_DETERMINANT2D:
                tps[0] = targetVoxelX / sourceVoxelX;
                tps[1] = 0;
                tps[2] = ((sourceXDim - 1.0) - ((targetXDim - 1.0) * (targetVoxelX / sourceVoxelX))) / 2.0;
                tps[3] = 0;
                tps[4] = ((sourceYDim - 1.0) - ((targetYDim - 1.0) * (targetVoxelY / sourceVoxelY))) / 2.0;
                break;

            case AFFINE2D:
                tps[0] = targetVoxelX / sourceVoxelX;
                tps[1] = 0;
                tps[2] = ((sourceXDim - 1.0) - ((targetXDim - 1.0) * (targetVoxelX / sourceVoxelX))) / 2.0;
                tps[3] = 0;
                tps[4] = targetVoxelY / sourceVoxelY;
                tps[5] = ((sourceYDim - 1.0) - ((targetYDim - 1.0) * (targetVoxelY / sourceVoxelY))) / 2.0;
                break;
        } // switch (model)

        if ((model == FIXED_DETERMINANT2D) && (cubicInterpolation)) {
            pixelSize = targetVoxelX;

            if (targetVoxelY < pixelSize) {
                pixelSize = targetVoxelY;
            }

            tps[0] *= pixelSize / targetVoxelX;
            tps[1] *= pixelSize / targetVoxelY;
            tps[3] *= pixelSize / targetVoxelX;
        }

        if ((model == AFFINE2D) && (cubicInterpolation)) {
            pixelSize = targetVoxelX;

            if (targetVoxelY < pixelSize) {
                pixelSize = targetVoxelY;
            }

            tps[0] *= pixelSize / targetVoxelX;
            tps[1] *= pixelSize / targetVoxelY;
            tps[3] *= pixelSize / targetVoxelX;
            tps[4] *= pixelSize / targetVoxelY;
        }

        if ((model == AFFINE3D) && (cubicInterpolation)) {
            pixelSize = targetVoxelX;

            if (targetVoxelY < pixelSize) {
                pixelSize = targetVoxelY;
            }

            if (targetVoxelZ < pixelSize) {
                pixelSize = targetVoxelZ;
            }

            tps[0] *= pixelSize / targetVoxelX;
            tps[1] *= pixelSize / targetVoxelY;
            tps[2] *= pixelSize / targetVoxelZ;
            tps[4] *= pixelSize / targetVoxelX;
            tps[5] *= pixelSize / targetVoxelY;
            tps[6] *= pixelSize / targetVoxelZ;
            tps[8] *= pixelSize / targetVoxelX;
            tps[9] *= pixelSize / targetVoxelY;
            tps[10] *= pixelSize / targetVoxelZ;
        }

        if (threadStopped) {
            finalize();
            
            setCompleted(false);

            return;
        }

        /* Smooth if requested */
        if ((sourceGaussX != 0.0f) || (sourceGaussY != 0.0f) || (sourceGaussZ != 0.0f)) {

            try {
                fireProgressStateChanged("Blurring source Image");
                fireProgressStateChanged(5);
                sigmas[0] = sourceGaussX;
                sigmas[1] = sourceGaussY;
                sigmas[2] = sourceGaussZ;

                // Make algorithm
                gaussianBlurAlgo = new AlgorithmGaussianBlur(sourceImage, sigmas, entireSource, inPlane);
                gaussianBlurAlgo.run();
            } catch (OutOfMemoryError x) {
                finalize();
                MipavUtil.displayError("Algorithm Registration Air: unable to allocate enough memory for sourceGaussianImage");
                
                setCompleted(false);

                return;
            }

            try {
                sourceImage.exportData(0, sourceVolSize, sourceArray); // locks and releases and lock
            } catch (IOException error) {
                finalize();
                displayError("AlgorithmRegAIR: sourceImage is locked after Gaussian blur");
                
                setCompleted(false);

                return;
            }
        } // if ((sourceGaussX != 0.0f) || (sourceGaussY != 0.0f) || (sourceGaussZ != 0.0f))

        if (threadStopped) {
            finalize();
            
            setCompleted(false);

            return;
        }

        if ((targetGaussX != 0.0f) || (targetGaussY != 0.0f) || (targetGaussZ != 0.0f)) {

            try {
                fireProgressStateChanged("Blurring target Image");
                fireProgressStateChanged(10);
                sigmas[0] = targetGaussX;
                sigmas[1] = targetGaussY;
                sigmas[2] = targetGaussZ;

                // Make algorithm
                gaussianBlurAlgo = new AlgorithmGaussianBlur(targetImage, sigmas, entireTarget, inPlane);
                gaussianBlurAlgo.run();
            } catch (OutOfMemoryError x) {
                finalize();
                MipavUtil.displayError("Algorithm Registration Air: unable to allocate enough memory for targetGaussianImage");
                
                setCompleted(false);

                return;
            }

            try {
                targetImage.exportData(0, targetVolSize, targetArray); // locks and releases and lock
            } catch (IOException error) {
                finalize();
                displayError("AlgorithmRegAIR: targetImage is locked after Gaussian blur");
                
                setCompleted(false);

                return;
            }
        } // if ((targetGaussX != 0.0f) || (targetGaussY != 0.0f) || (targetGaussZ != 0.0f))

        sigmas = null;

        /* perform alignment */

        /* Set flags, etc. */
        if ((sourcePartitions > 0) && (targetPartitions > 0)) {
            direction = 0;
        } else if (targetPartitions > 0) {
            direction = 1;
        } else if (sourcePartitions > 0) {
            direction = -1;
        } else {
            finalize();
            MipavUtil.displayError("Algorithm Registration Air: No partitions have been allocated");
            
            setCompleted(false);

            return;
        }

        /* Allocate matrices */

        try {

            es = new double[coeffs][coords]; // 3 coordinates for each of 4 points
            er = new double[coeffs][coords];
            des = new double[coeffs][coords][parameters]; // these being the origin

            // and relative
            der = new double[coeffs][coords][parameters]; // these being the origin

            // and relative
            // positions of the 3 unit vectors
            ees = new double[coeffs][coords][parameters][parameters];

            // positions of the 3 unit vectors
            eer = new double[coeffs][coords][parameters][parameters];

            dcf = new double[parameters + scales];
            dcff = new double[parameters + scales];
            dcfr = new double[parameters + scales];

            ecf = new double[parameters + scales][parameters + scales];
            ecf2 = new double[parameters + scales][parameters + scales];
            ecff = new double[parameters + scales][parameters + scales];
            ecfr = new double[parameters + scales][parameters + scales];

            delta = new double[parameters + scales];
            tpsBest = new double[parameters + scales];
            gersch = new double[parameters + scales];
            kpvt = new int[parameters + scales];

            if (memory) {
                partitions = Math.max(sourcePartitions, targetPartitions);
                q1 = new int[partitions];
                q2 = new double[partitions];
                q3 = new double[partitions];
                q4 = new double[partitions][param_int];
                q5 = new double[partitions][param_int];
                q6 = new double[partitions][param_int][param_int];
                q7 = new double[partitions][param_int][param_int];
            }

        } catch (OutOfMemoryError x) {
            finalize();
            MipavUtil.displayError("Algorithm Registration Air: unable to allocate enough memory for matrices");
            
            setCompleted(false);

            return;
        }

        if (threadStopped) {
            finalize();
            
            setCompleted(false);

            return;
        }

        if (direction > -1) {

            if (!cubicInterpolation) {

                /* Pretend to make a copy of the target file */
                pixel5 = targetArray;
                pixel5Mask = targetMask;
                targetCubicXDim = targetXDim;
                targetCubicYDim = targetYDim;
                targetCubicZDim = targetZDim;
                targetCubicVoxelX = targetVoxelX;
                targetCubicVoxelY = targetVoxelY;
                targetCubicVoxelZ = targetVoxelZ;
            } else {

                /* Interpolate the target file to cubic voxels */
                targetCubicVoxelX = Math.min(targetVoxelZ, Math.min(targetVoxelX, targetVoxelY));
                targetCubicVoxelY = targetCubicVoxelX;
                targetCubicVoxelZ = targetCubicVoxelX;
                targetCubicXDim = (int) Math.floor(((targetXDim - 1) * (targetVoxelX / targetCubicVoxelX)) + 1 +
                                                   Float.MIN_VALUE);
                targetCubicYDim = (int) Math.floor(((targetYDim - 1) * (targetVoxelY / targetCubicVoxelY)) + 1 +
                                                   Float.MIN_VALUE);

                if (!inPlane) {
                    targetCubicZDim = (int) Math.floor(((targetZDim - 1) * (targetVoxelZ / targetCubicVoxelZ)) + 1 +
                                                       Float.MIN_VALUE);
                } else {
                    targetCubicZDim = targetZDim;
                }

                for (j = 0; j < 4; j++) {

                    for (i = 0; i < 4; i++) {
                        e[j][i] = 0.0;
                    }
                }

                e[0][0] = targetCubicVoxelX / targetVoxelX;
                e[1][1] = targetCubicVoxelY / targetVoxelY;
                e[2][2] = targetCubicVoxelZ / targetVoxelZ;
                e[3][3] = 1.0;
                fireProgressStateChanged("Interpolating target image to cubic voxels");
                fireProgressStateChanged(15);
                pixel5 = reslicer(targetArray, targetXDim, targetYDim, targetZDim, targetCubicXDim, targetCubicYDim,
                                  targetCubicZDim, e);

                targetCubicVolSize = targetCubicXDim * targetCubicYDim * targetCubicZDim;

                pixel5Mask = new boolean[targetCubicVolSize];

                if (entireTarget == false) {
                    targetMaskFloat = new float[targetVolSize];

                    for (i = 0; i < targetVolSize; i++) {

                        if (targetMask[i]) {
                            targetMaskFloat[i] = 1.0f;
                        } else {
                            targetMaskFloat[i] = 0.0f;
                        }
                    }

                    targetCubicMaskFloat = nnreslicer(targetMaskFloat, targetXDim, targetYDim, targetZDim,
                                                      targetCubicXDim, targetCubicYDim, targetCubicZDim, e);

                    for (i = 0; i < targetCubicVolSize; i++) {

                        if (targetCubicMaskFloat[i] != 0.0f) {
                            pixel5Mask[i] = true;
                        } else {
                            pixel5Mask[i] = false;
                        }
                    }

                    targetMaskFloat = null;
                    targetCubicMaskFloat = null;
                } // if (entireTarget == false)
                else {

                    for (i = 0; i < targetCubicVolSize; i++) {
                        pixel5Mask[i] = true;
                    }
                }
            }

            max5 = -Double.MAX_VALUE;

            for (i = 0; i < targetCubicVolSize; i++) {

                if ((pixel5Mask[i]) && (pixel5[i] > max5)) {
                    max5 = pixel5[i];
                }
            }
        } // if (direction > -1)

        if (direction < 1) {

            if (!cubicInterpolation) {

                /* Pretend to make a copy of the source file */
                pixel6 = sourceArray;
                pixel6Mask = sourceMask;
                sourceCubicXDim = sourceXDim;
                sourceCubicYDim = sourceYDim;
                sourceCubicZDim = sourceZDim;
                sourceCubicVoxelX = sourceVoxelX;
                sourceCubicVoxelY = sourceVoxelY;
                sourceCubicVoxelZ = sourceVoxelZ;
            } else {

                /* Interpolate the standard file to cubic voxels */
                sourceCubicVoxelX = Math.min(sourceVoxelZ, Math.min(sourceVoxelX, sourceVoxelY));
                sourceCubicVoxelY = sourceCubicVoxelX;
                sourceCubicVoxelZ = sourceCubicVoxelX;
                sourceCubicXDim = (int) Math.floor(((sourceXDim - 1) * (sourceVoxelX / sourceCubicVoxelX)) + 1 +
                                                   Float.MIN_VALUE);
                sourceCubicYDim = (int) Math.floor(((sourceYDim - 1) * (sourceVoxelY / sourceCubicVoxelY)) + 1 +
                                                   Float.MIN_VALUE);

                if (!inPlane) {
                    sourceCubicZDim = (int) Math.floor(((sourceZDim - 1) * (sourceVoxelZ / sourceCubicVoxelZ)) + 1 +
                                                       Float.MIN_VALUE);
                } else {
                    sourceCubicZDim = sourceZDim;
                }

                for (j = 0; j < 4; j++) {

                    for (i = 0; i < 4; i++) {
                        e[j][i] = 0.0;
                    }
                }

                e[0][0] = sourceCubicVoxelX / sourceVoxelX;
                e[1][1] = sourceCubicVoxelY / sourceVoxelY;
                e[2][2] = sourceCubicVoxelZ / sourceVoxelZ;
                e[3][3] = 1.0;
                fireProgressStateChanged("Interpolating source image to cubic voxels");
                fireProgressStateChanged(20);
                pixel6 = reslicer(sourceArray, sourceXDim, sourceYDim, sourceZDim, sourceCubicXDim, sourceCubicYDim,
                                  sourceCubicZDim, e);

                sourceCubicVolSize = sourceCubicXDim * sourceCubicYDim * sourceCubicZDim;

                pixel6Mask = new boolean[sourceCubicVolSize];

                if (entireSource == false) {
                    sourceMaskFloat = new float[sourceVolSize];

                    for (i = 0; i < sourceVolSize; i++) {

                        if (sourceMask[i]) {
                            sourceMaskFloat[i] = 1.0f;
                        } else {
                            sourceMaskFloat[i] = 0.0f;
                        }
                    }

                    sourceCubicMaskFloat = nnreslicer(sourceMaskFloat, sourceXDim, sourceYDim, sourceZDim,
                                                      sourceCubicXDim, sourceCubicYDim, sourceCubicZDim, e);

                    for (i = 0; i < sourceCubicVolSize; i++) {

                        if (sourceCubicMaskFloat[i] != 0.0f) {
                            pixel6Mask[i] = true;
                        } else {
                            pixel6Mask[i] = false;
                        }
                    }

                    sourceMaskFloat = null;
                    sourceCubicMaskFloat = null;
                } // if (entireSource == false)
                else {

                    for (i = 0; i < sourceCubicVolSize; i++) {
                        pixel6Mask[i] = true;
                    }
                }
            }

            max6 = -Double.MAX_VALUE;

            for (i = 0; i < sourceCubicVolSize; i++) {

                if ((pixel6Mask[i]) && (pixel6[i] > max6)) {
                    max6 = pixel6[i];
                }
            }
        } // if (direction < 1)

        switch (model) {

            case RIGID_BODY2D:
                uv2D3(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetXDim,
                      targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case GLOBAL_RESCALING2D:
                uv2D4(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetXDim,
                      targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case FIXED_DETERMINANT2D:
                uv2D5(tps, es, des, ees, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetVoxelX, targetVoxelY,
                      targetVoxelZ, cubicInterpolation);
                break;

            case AFFINE2D:
                uv2D6(tps, es, des, ees);
                break;

            case RIGID_BODY3D:
                uv3D6(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                      targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case GLOBAL_RESCALING3D:
                uv3D7(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                      targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case TRADITIONAL3D:
                uv3D9(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                      targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case AFFINE3D:
                uv3D12(tps, es, des, ees);
                break;
        } // switch(model)

        for (j = 0; j < coords; j++) {
            Preferences.debug("START ");

            for (jj = 0; jj < coeffs; jj++) {
                Preferences.debug(es[jj][j] + " ");
            }

            Preferences.debug("\n");
        }

        if (scales != 0) {
            Preferences.debug("scale = " + tps[parameters]);
        }

        Preferences.debug("\n");

        for (i = 0; i < (parameters + scales); i++) {
            tpsBest[i] = tps[i];
        }

        /* Iterate until the final sample factor is reached */
        fireProgressStateChanged(30);

        while (sampleFactor >= sampleFactor2) {
            iters = 0;
            tries = 0;
            deltacf = Double.MAX_VALUE;
            cfBest = Double.MAX_VALUE;

            do { // while((Math.abs(deltacf) > precision) && (!threadStopped))

                /* Calculate the standard deviation, mean, and derivatives */
                if (sampleFactor == 27) {
                    fireProgressStateChanged(40);
                } else if (sampleFactor == 9) {
                    fireProgressStateChanged(50);
                } else if (sampleFactor == 3) {
                    fireProgressStateChanged(60);
                } else if (sampleFactor == 1) {
                    fireProgressStateChanged(70);
                }

                fireProgressStateChanged("Iteration # " + (iters + 1) + " at sample factor = " + sampleFactor);

                if (direction == 0) {

                    error = 0;

                    switch (model) {

                        case RIGID_BODY2D:
                            uv2D3(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                                  targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                            break;

                        case GLOBAL_RESCALING2D:
                            uv2D4(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                                  targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                            break;

                        case FIXED_DETERMINANT2D:
                            uv2D5(tps, es, des, ees, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetVoxelX,
                                  targetVoxelY, targetVoxelZ, cubicInterpolation);
                            break;

                        case AFFINE2D:
                            uv2D6(tps, es, des, ees);
                            break;

                        case RIGID_BODY3D:
                            uv3D6(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                                  sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                                  targetVoxelZ, cubicInterpolation);
                            break;

                        case GLOBAL_RESCALING3D:
                            uv3D7(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                                  sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                                  targetVoxelZ, cubicInterpolation);
                            break;

                        case TRADITIONAL3D:
                            uv3D9(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                                  sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                                  targetVoxelZ, cubicInterpolation);
                            break;

                        case AFFINE3D:
                            uv3D12(tps, es, des, ees);
                            break;
                    } // switch(model)

                    switch (derivsN) {

                        case UVDERIVSN6:
                            cf = uvderivsN6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim, sourceYDim,
                                            pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim, targetCubicZDim,
                                            targetThreshold, dcff, ecff, q1, q2, q3, q4, q5, q6, q7, targetPartitions,
                                            max5);
                            break;

                        case UVDERIVSN12:
                            cf = uvderivsN12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                             targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff, q1, q2, q3,
                                             q4, q5, q6, q7, targetPartitions, max5);
                            break;

                        case QVDERIVSN6:
                            cf = qvderivsN6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim, sourceYDim,
                                            pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim, targetCubicZDim,
                                            targetThreshold, dcff, ecff, q1, q2, q3, q4, q5, q7, targetPartitions,
                                            max5);
                            break;

                        case QVDERIVSN12:
                            cf = qvderivsN12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                             targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff, q1, q2, q3,
                                             q4, q5, q7, targetPartitions, max5);
                            break;

                        case UVDERIVSLS6:
                            cf = uvderivsLS6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                             targetCubicZDim, targetThreshold, dcff, ecff);
                            break;

                        case UVDERIVSLS12:
                            cf = uvderivsLS12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                              targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff);
                            break;

                        case QVDERIVSLS6:
                            cf = qvderivsLS6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                             targetCubicZDim, targetThreshold, dcff, ecff);
                            break;

                        case QVDERIVSLS12:
                            cf = qvderivsLS12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                              targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff);
                            break;

                        case UVDERIVSRS6:
                            cf = uvderivsRS6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                             targetCubicZDim, targetThreshold, dcff, ecff, tps[parameters], true);
                            break;

                        case UVDERIVSRS12:
                            cf = uvderivsRS12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                              targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff,
                                              tps[parameters], true);
                            break;

                        case QVDERIVSRS6:
                            cf = qvderivsRS6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                             targetCubicZDim, targetThreshold, dcff, ecff, tps[parameters], true);
                            break;

                        case QVDERIVSRS12:
                            cf = qvderivsRS12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                              targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff,
                                              tps[parameters], true);
                            break;

                        case UVDERIVSCOR6:
                            cf = uvderivsCOR6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                              targetCubicZDim, targetThreshold, dcff, ecff, q1, q2, q3, q4, q5, q6, q7,
                                              targetPartitions, max5);
                            break;

                        case UVDERIVSCOR12:
                            cf = uvderivsCOR12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                               sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                               targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff, q1, q2,
                                               q3, q4, q5, q6, q7, targetPartitions, max5);
                            break;

                        case QVDERIVSCOR6:
                            cf = qvderivsCOR6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                              targetCubicZDim, targetThreshold, dcff, ecff, q1, q2, q3, q4, q5, q7,
                                              targetPartitions, max5);
                            break;

                        case QVDERIVSCOR12:
                            cf = qvderivsCOR12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                               sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                               targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff, q1, q2,
                                               q3, q4, q5, q7, targetPartitions, max5);
                            break;
                    } // switch(derivsN)

                    success = uvrm(parameters, es, er, des, der, ees, eer, cubicInterpolation, targetVoxelX,
                                   targetVoxelY, targetVoxelZ, sourceVoxelX, sourceVoxelY, sourceVoxelZ);

                    if (!success) {
                        finalize();
                        MipavUtil.displayError("AlgorithmRegAIR: Unsuccessful uvrm");
                        
                        setCompleted(false);

                        return;
                    }

                    switch (derivsN) {

                        case UVDERIVSN6:
                            cf += uvderivsN6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                             targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                             sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3, q4, q5, q6, q7,
                                             sourcePartitions, max6);
                            break;

                        case UVDERIVSN12:
                            cf += uvderivsN12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                              sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3,
                                              q4, q5, q6, q7, sourcePartitions, max6);
                            break;

                        case QVDERIVSN6:
                            cf += qvderivsN6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                             targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                             sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3, q4, q5, q7,
                                             sourcePartitions, max6);
                            break;

                        case QVDERIVSN12:
                            cf += qvderivsN12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                              sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3,
                                              q4, q5, q7, sourcePartitions, max6);
                            break;

                        case UVDERIVSLS6:
                            cf += uvderivsLS6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                              sourceCubicZDim, sourceThreshold, dcfr, ecfr);
                            break;

                        case UVDERIVSLS12:
                            cf += uvderivsLS12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                               targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                               sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr);
                            break;

                        case QVDERIVSLS6:
                            cf += qvderivsLS6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                              sourceCubicZDim, sourceThreshold, dcfr, ecfr);
                            break;

                        case QVDERIVSLS12:
                            cf += qvderivsLS12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                               targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                               sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr);
                            break;

                        case UVDERIVSRS6:
                            cf += uvderivsRS6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                              sourceCubicZDim, sourceThreshold, dcfr, ecfr, tps[parameters], false);
                            break;

                        case UVDERIVSRS12:
                            cf += uvderivsRS12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                               targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                               sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr,
                                               tps[parameters], false);
                            break;

                        case QVDERIVSRS6:
                            cf += qvderivsRS6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                              sourceCubicZDim, sourceThreshold, dcfr, ecfr, tps[parameters], false);
                            break;

                        case QVDERIVSRS12:
                            cf += qvderivsRS12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                               targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                               sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr,
                                               tps[parameters], false);
                            break;

                        case UVDERIVSCOR6:
                            cf += uvderivsCOR6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                               targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                               sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3, q4, q5, q6, q7,
                                               sourcePartitions, max6);
                            break;

                        case UVDERIVSCOR12:
                            cf += uvderivsCOR12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                                targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                                sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2,
                                                q3, q4, q5, q6, q7, sourcePartitions, max6);
                            break;

                        case QVDERIVSCOR6:
                            cf += qvderivsCOR6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                               targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                               sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3, q4, q5, q7,
                                               sourcePartitions, max6);
                            break;

                        case QVDERIVSCOR12:
                            cf += qvderivsCOR12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                                targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                                sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2,
                                                q3, q4, q5, q7, sourcePartitions, max6);
                            break;
                    } // switch(derivsN)

                    if (error == 2) {
                        break;
                    } /* No pixels above threshold that stayed in bounds */

                    cf /= (2.0 - error);

                    for (t = 0; t < (parameters + scales); t++) {

                        dcf[t] = (-dcff[t] - dcfr[t]) / (2.0 - error);

                        for (s = 0; s <= t; s++) {
                            ecf[t][s] = (ecff[t][s] + ecfr[t][s]) / (2.0 - error);
                        }
                    }
                } // if (direction == 0)
                else if (direction == -1) {
                    error = 0;

                    switch (model) {

                        case RIGID_BODY2D:
                            uv2D3(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                                  targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                            break;

                        case GLOBAL_RESCALING2D:
                            uv2D4(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                                  targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                            break;

                        case FIXED_DETERMINANT2D:
                            uv2D5(tps, es, des, ees, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetVoxelX,
                                  targetVoxelY, targetVoxelZ, cubicInterpolation);
                            break;

                        case AFFINE2D:
                            uv2D6(tps, es, des, ees);
                            break;

                        case RIGID_BODY3D:
                            uv3D6(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                                  sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                                  targetVoxelZ, cubicInterpolation);
                            break;

                        case GLOBAL_RESCALING3D:
                            uv3D7(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                                  sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                                  targetVoxelZ, cubicInterpolation);
                            break;

                        case TRADITIONAL3D:
                            uv3D9(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                                  sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                                  targetVoxelZ, cubicInterpolation);
                            break;

                        case AFFINE3D:
                            uv3D12(tps, es, des, ees);
                            break;
                    } // switch(model)

                    success = uvrm(parameters, es, er, des, der, ees, eer, cubicInterpolation, targetVoxelX,
                                   targetVoxelY, targetVoxelZ, sourceVoxelX, sourceVoxelY, sourceVoxelZ);

                    if (!success) {
                        finalize();
                        MipavUtil.displayError("AlgorithmRegAIR: Unsuccessful uvrm");
                        
                        setCompleted(false);

                        return;
                    }

                    switch (derivsN) {

                        case UVDERIVSN6:
                            cf = uvderivsN6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim, targetYDim,
                                            pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim, sourceCubicZDim,
                                            sourceThreshold, dcfr, ecfr, q1, q2, q3, q4, q5, q6, q7, sourcePartitions,
                                            max6);
                            break;

                        case UVDERIVSN12:
                            cf = uvderivsN12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                             targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                             sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3,
                                             q4, q5, q6, q7, sourcePartitions, max6);
                            break;

                        case QVDERIVSN6:
                            cf = qvderivsN6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim, targetYDim,
                                            pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim, sourceCubicZDim,
                                            sourceThreshold, dcfr, ecfr, q1, q2, q3, q4, q5, q7, sourcePartitions,
                                            max6);
                            break;

                        case QVDERIVSN12:
                            cf = qvderivsN12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                             targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                             sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3,
                                             q4, q5, q7, sourcePartitions, max6);
                            break;

                        case UVDERIVSLS6:
                            cf = uvderivsLS6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                             targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                             sourceCubicZDim, sourceThreshold, dcfr, ecfr);
                            break;

                        case UVDERIVSLS12:
                            cf = uvderivsLS12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                              sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr);
                            break;

                        case QVDERIVSLS6:
                            cf = qvderivsLS6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                             targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                             sourceCubicZDim, sourceThreshold, dcfr, ecfr);
                            break;

                        case QVDERIVSLS12:
                            cf = qvderivsLS12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                              sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr);
                            break;

                        case UVDERIVSRS6:
                            cf = uvderivsRS6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                             targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                             sourceCubicZDim, sourceThreshold, dcfr, ecfr, tps[parameters], false);
                            break;

                        case UVDERIVSRS12:
                            cf = uvderivsRS12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                              sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr,
                                              tps[parameters], false);
                            break;

                        case QVDERIVSRS6:
                            cf = qvderivsRS6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                             targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                             sourceCubicZDim, sourceThreshold, dcfr, ecfr, tps[parameters], false);
                            break;

                        case QVDERIVSRS12:
                            cf = qvderivsRS12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                              sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr,
                                              tps[parameters], false);
                            break;

                        case UVDERIVSCOR6:
                            cf = uvderivsCOR6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                              sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3, q4, q5, q6, q7,
                                              sourcePartitions, max6);
                            break;

                        case UVDERIVSCOR12:
                            cf = uvderivsCOR12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                               targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                               sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2,
                                               q3, q4, q5, q6, q7, sourcePartitions, max6);
                            break;

                        case QVDERIVSCOR6:
                            cf = qvderivsCOR6(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                              targetYDim, pixel6, pixel6Mask, sourceCubicXDim, sourceCubicYDim,
                                              sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2, q3, q4, q5, q7,
                                              sourcePartitions, max6);
                            break;

                        case QVDERIVSCOR12:
                            cf = qvderivsCOR12(parameters, er, der, eer, sampleFactor, targetArray, targetXDim,
                                               targetYDim, targetZDim, pixel6, pixel6Mask, sourceCubicXDim,
                                               sourceCubicYDim, sourceCubicZDim, sourceThreshold, dcfr, ecfr, q1, q2,
                                               q3, q4, q5, q7, sourcePartitions, max6);
                            break;
                    } // switch(derivsN)

                    if (error != 0) {
                        break;
                    } /* No pixels above threshold stayed in bounds */

                    for (t = 0; t < (parameters + scales); t++) {
                        dcf[t] = -dcfr[t];

                        for (s = 0; s <= t; s++) {
                            ecf[t][s] = ecfr[t][s];
                        }
                    }
                } // else if (direction == -1)
                else { // direction == 1
                    error = 0;

                    switch (model) {

                        case RIGID_BODY2D:
                            uv2D3(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                                  targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                            break;

                        case GLOBAL_RESCALING2D:
                            uv2D4(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                                  targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                            break;

                        case FIXED_DETERMINANT2D:
                            uv2D5(tps, es, des, ees, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetVoxelX,
                                  targetVoxelY, targetVoxelZ, cubicInterpolation);
                            break;

                        case AFFINE2D:
                            uv2D6(tps, es, des, ees);
                            break;

                        case RIGID_BODY3D:
                            uv3D6(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                                  sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                                  targetVoxelZ, cubicInterpolation);
                            break;

                        case GLOBAL_RESCALING3D:
                            uv3D7(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                                  sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                                  targetVoxelZ, cubicInterpolation);
                            break;

                        case TRADITIONAL3D:
                            uv3D9(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                                  sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                                  targetVoxelZ, cubicInterpolation);
                            break;

                        case AFFINE3D:
                            uv3D12(tps, es, des, ees);
                            break;
                    } // switch(model)

                    switch (derivsN) {

                        case UVDERIVSN6:
                            cf = uvderivsN6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim, sourceYDim,
                                            pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim, targetCubicZDim,
                                            targetThreshold, dcff, ecff, q1, q2, q3, q4, q5, q6, q7, targetPartitions,
                                            max5);
                            break;

                        case UVDERIVSN12:
                            cf = uvderivsN12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                             targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff, q1, q2, q3,
                                             q4, q5, q6, q7, targetPartitions, max5);
                            break;

                        case QVDERIVSN6:
                            cf = qvderivsN6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim, sourceYDim,
                                            pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim, targetCubicZDim,
                                            targetThreshold, dcff, ecff, q1, q2, q3, q4, q5, q7, targetPartitions,
                                            max5);
                            break;

                        case QVDERIVSN12:
                            cf = qvderivsN12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                             targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff, q1, q2, q3,
                                             q4, q5, q7, targetPartitions, max5);
                            break;

                        case UVDERIVSLS6:
                            cf = uvderivsLS6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                             targetCubicZDim, targetThreshold, dcff, ecff);
                            break;

                        case UVDERIVSLS12:
                            cf = uvderivsLS12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                              targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff);
                            break;

                        case QVDERIVSLS6:
                            cf = qvderivsLS6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                             targetCubicZDim, targetThreshold, dcff, ecff);
                            break;

                        case QVDERIVSLS12:
                            cf = qvderivsLS12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                              targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff);
                            break;

                        case UVDERIVSRS6:
                            cf = uvderivsRS6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                             targetCubicZDim, targetThreshold, dcff, ecff, tps[parameters], true);
                            break;

                        case UVDERIVSRS12:
                            cf = uvderivsRS12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                              targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff,
                                              tps[parameters], true);
                            break;

                        case QVDERIVSRS6:
                            cf = qvderivsRS6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                             sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                             targetCubicZDim, targetThreshold, dcff, ecff, tps[parameters], true);
                            break;

                        case QVDERIVSRS12:
                            cf = qvderivsRS12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                              targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff,
                                              tps[parameters], true);
                            break;

                        case UVDERIVSCOR6:
                            cf = uvderivsCOR6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                              targetCubicZDim, targetThreshold, dcff, ecff, q1, q2, q3, q4, q5, q6, q7,
                                              targetPartitions, max5);
                            break;

                        case UVDERIVSCOR12:
                            cf = uvderivsCOR12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                               sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                               targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff, q1, q2,
                                               q3, q4, q5, q6, q7, targetPartitions, max5);
                            break;

                        case QVDERIVSCOR6:
                            cf = qvderivsCOR6(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                              sourceYDim, pixel5, pixel5Mask, targetCubicXDim, targetCubicYDim,
                                              targetCubicZDim, targetThreshold, dcff, ecff, q1, q2, q3, q4, q5, q7,
                                              targetPartitions, max5);

                            break;

                        case QVDERIVSCOR12:
                            cf = qvderivsCOR12(parameters, es, des, ees, sampleFactor, sourceArray, sourceXDim,
                                               sourceYDim, sourceZDim, pixel5, pixel5Mask, targetCubicXDim,
                                               targetCubicYDim, targetCubicZDim, targetThreshold, dcff, ecff, q1, q2,
                                               q3, q4, q5, q7, targetPartitions, max5);
                            break;
                    } // switch(derivsN)

                    if (error != 0) {
                        break;
                    } /* No pixels above threshold stayed in bounds */

                    for (t = 0; t < (parameters + scales); t++) {
                        dcf[t] = -dcff[t];

                        for (s = 0; s <= t; s++) {
                            ecf[t][s] = ecff[t][s];
                        }
                    }
                } // else direction == 1

                if (cf < cfBest) {
                    cfBest = cf;

                    for (jj = 0; jj < (parameters + scales); jj++) {
                        tpsBest[jj] = tps[jj];
                    }

                    tries = 0;
                } else {
                    tries++;
                }

                if (tries > noProgTries) {
                    break;
                }

                iters++;

                if (iters > iterations) {
                    break;
                }

                for (t = 0; t < (parameters + scales); t++) {
                    delta[t] = dcf[t];

                    for (s = 0; s <= t; s++) {
                        ecf2[t][s] = ecf[t][s];
                    }
                }

                /* Now we solve the system of equations Hessian*delta=gradient*/

                if (dpofa(ecf2, parameters + scales) == (parameters + scales)) {
                    dposl(ecf2, parameters + scales, delta);
                } else {

                    /* Hessian is not positive definite */

                    if (posdefreq) {

                        /* If possible, sample more densely */
                        if ((sampleFactor / sfFactor) >= sampleFactor2) {
                            System.out.println("Increasing sampling density due to non-positive definite Hessian\n");

                            break;
                        } else {

                            /* Terminate with a warning */
                            finalize();
                            MipavUtil.displayError("\nWARNING: REGISTRATION TERMINATED DUE TO A HESSIAN MATRIX THAT WAS NOT POSITIVE DEFINITE\n");
                            MipavUtil.displayError("INSPECTION OF RESULTS IS ADVISED\n");
                            
                            setCompleted(false);

                            return;
                        }
                    } // if (posdefreq)

                    /* Reload the Hessian for revised modified Cholesky factorization */
                    for (t = 0; t < (parameters + scales); t++) {

                        for (s = 0; s <= t; s++) {
                            ecf2[t][s] = ecf[t][s];
                        }
                    }

                    /* We use modchol and dmodposl which do not require a positive definite Hessian */
                    modchol(ecf2, parameters + scales, kpvt, gersch);
                    Preferences.debug("Using revised modified Cholesky factorization\n");
                    dmodposl(ecf2, parameters + scales, delta, kpvt);
                } // else Hessian not positive definite

                for (t = 0; t < (parameters + scales); t++) {
                    tps[t] += delta[t];
                }

                deltacf = sdpred(delta, dcf, ecf, parameters + scales);

                switch (model) {

                    case RIGID_BODY2D:
                        uv2D3(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                              targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                        break;

                    case GLOBAL_RESCALING2D:
                        uv2D4(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                              targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                        break;

                    case FIXED_DETERMINANT2D:
                        uv2D5(tps, es, des, ees, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetVoxelX, targetVoxelY,
                              targetVoxelZ, cubicInterpolation);
                        break;

                    case AFFINE2D:
                        uv2D6(tps, es, des, ees);
                        break;

                    case RIGID_BODY3D:
                        uv3D6(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                              sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                              targetVoxelZ, cubicInterpolation);
                        break;

                    case GLOBAL_RESCALING3D:
                        uv3D7(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                              sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                              targetVoxelZ, cubicInterpolation);
                        break;

                    case TRADITIONAL3D:
                        uv3D9(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                              sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY,
                              targetVoxelZ, cubicInterpolation);
                        break;

                    case AFFINE3D:
                        uv3D12(tps, es, des, ees);
                        break;
                } // switch(model)

                Preferences.debug("cost function = " + cf + "\n");
                Preferences.debug("predicted cost function change = " + deltacf + "\n");
                Preferences.debug("iterations completed = " + iters + "\n");
                Preferences.debug("sampling = " + sampleFactor + "\n");
                Preferences.debug("\n");

                for (j = 0; j < coords; j++) {
                    Preferences.debug("NEW ");

                    for (jj = 0; jj < coeffs; jj++) {
                        Preferences.debug(es[jj][j] + " ");
                    }

                    Preferences.debug("\n");
                }

                if (scales != 0) {
                    Preferences.debug("scale = " + tps[parameters] + "\n");
                }

                Preferences.debug("\n");
            } while ((Math.abs(deltacf) > precision) && (!threadStopped));

            if (threadStopped) {
                finalize();
                
                setCompleted(false);

                return;
            }

            for (jj = 0; jj < (parameters + scales); jj++) {
                tps[jj] = tpsBest[jj];
            }

            Preferences.debug("\n");
            Preferences.debug("BEST VALUES AT SAMPLEFACTOR = " + sampleFactor + "\n");
            Preferences.debug("cost function = " + cfBest + "\n");

            switch (model) {

                case RIGID_BODY2D:
                    uv2D3(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                          targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                    break;

                case GLOBAL_RESCALING2D:
                    uv2D4(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                          targetXDim, targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                    break;

                case FIXED_DETERMINANT2D:
                    uv2D5(tps, es, des, ees, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetVoxelX, targetVoxelY,
                          targetVoxelZ, cubicInterpolation);
                    break;

                case AFFINE2D:
                    uv2D6(tps, es, des, ees);
                    break;

                case RIGID_BODY3D:
                    uv3D6(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                          sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY, targetVoxelZ,
                          cubicInterpolation);
                    break;

                case GLOBAL_RESCALING3D:
                    uv3D7(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                          sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY, targetVoxelZ,
                          cubicInterpolation);
                    break;

                case TRADITIONAL3D:
                    uv3D9(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY,
                          sourceVoxelZ, targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY, targetVoxelZ,
                          cubicInterpolation);
                    break;

                case AFFINE3D:
                    uv3D12(tps, es, des, ees);
                    break;
            } // switch(model)

            Preferences.debug("\n");

            for (j = 0; j < coords; j++) {
                Preferences.debug("BEST ");

                for (jj = 0; jj < coeffs; jj++) {
                    Preferences.debug(es[jj][j] + " ");
                }

                Preferences.debug("\n");
            }

            if (scales != 0) {
                Preferences.debug("scale = " + tps[parameters] + "\n");
            }

            Preferences.debug("\n");

            /* Calculate next sample factor for next iteration */
            sampleFactor = sampleFactor / sfFactor;
        } // while (sampleFactor >= sampleFactor2)

        /* Print out final results */
        Preferences.debug("cost function = " + cfBest + "\n");

        /* Convert optimal parameters into transformation matrix once again */
        switch (model) {

            case RIGID_BODY2D:
                uv2D3(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetXDim,
                      targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case GLOBAL_RESCALING2D:
                uv2D4(tps, es, des, ees, sourceXDim, sourceYDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetXDim,
                      targetYDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case FIXED_DETERMINANT2D:
                uv2D5(tps, es, des, ees, sourceVoxelX, sourceVoxelY, sourceVoxelZ, targetVoxelX, targetVoxelY,
                      targetVoxelZ, cubicInterpolation);
                break;

            case AFFINE2D:
                uv2D6(tps, es, des, ees);
                break;

            case RIGID_BODY3D:
                uv3D6(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                      targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case GLOBAL_RESCALING3D:
                uv3D7(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                      targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case TRADITIONAL3D:
                uv3D9(tps, es, des, ees, sourceXDim, sourceYDim, sourceZDim, sourceVoxelX, sourceVoxelY, sourceVoxelZ,
                      targetXDim, targetYDim, targetZDim, targetVoxelX, targetVoxelY, targetVoxelZ, cubicInterpolation);
                break;

            case AFFINE3D:
                uv3D12(tps, es, des, ees);
                break;
        } // switch(model)

        /* Clean up */
        if (ecf != null) {

            for (i = 0; i < ecf.length; i++) {
                ecf[i] = null;
            }

            ecf = null;
        } // if (ecf != null)

        if (ecf2 != null) {

            for (i = 0; i < ecf2.length; i++) {
                ecf2[i] = null;
            }

            ecf2 = null;
        } // if (ecf2 != null)

        if (er != null) {

            for (i = 0; i < er.length; i++) {
                er[i] = null;
            }

            er = null;
        } // if (er != null)

        if (des != null) {

            for (i = 0; i < des.length; i++) {

                for (j = 0; j < des[i].length; j++) {
                    des[i][j] = null;
                }
            }

            for (i = 0; i < des.length; i++) {
                des[i] = null;
            }

            des = null;
        } // if (des != null)

        if (der != null) {

            for (i = 0; i < der.length; i++) {

                for (j = 0; j < der[i].length; j++) {
                    der[i][j] = null;
                }
            }

            for (i = 0; i < der.length; i++) {
                der[i] = null;
            }

            der = null;
        } // if (der != null)

        if (ees != null) {

            for (i = 0; i < ees.length; i++) {

                for (j = 0; j < ees[i].length; j++) {

                    for (k = 0; k < ees[i][j].length; k++) {
                        ees[i][j][k] = null;
                    }
                }
            }

            for (i = 0; i < ees.length; i++) {

                for (j = 0; j < ees[i].length; j++) {
                    ees[i][j] = null;
                }
            }

            for (i = 0; i < ees.length; i++) {
                ees[i] = null;
            }

            ees = null;
        } // if (ees != null)

        if (eer != null) {

            for (i = 0; i < eer.length; i++) {

                for (j = 0; j < eer[i].length; j++) {

                    for (k = 0; k < eer[i][j].length; k++) {
                        eer[i][j][k] = null;
                    }
                }
            }

            for (i = 0; i < eer.length; i++) {

                for (j = 0; j < eer[i].length; j++) {
                    eer[i][j] = null;
                }
            }

            for (i = 0; i < eer.length; i++) {
                eer[i] = null;
            }

            eer = null;
        } // if (eer != null)

        dcff = null;
        dcfr = null;
        dcf = null;

        if (ecff != null) {

            for (i = 0; i < ecff.length; i++) {
                ecff[i] = null;
            }

            ecff = null;
        } // if (ecff != null)

        if (ecfr != null) {

            for (i = 0; i < ecfr.length; i++) {
                ecfr[i] = null;
            }

            ecfr = null;
        } // if (ecfr != null)

        q1 = null;
        q2 = null;
        q3 = null;

        if (q4 != null) {

            for (i = 0; i < q4.length; i++) {
                q4[i] = null;
            }

            q4 = null;
        }

        if (q5 != null) {

            for (i = 0; i < q5.length; i++) {
                q5[i] = null;
            }

            q5 = null;
        } // if (q5 != null)

        if (q6 != null) {

            for (i = 0; i < q6.length; i++) {

                for (j = 0; j < q6[i].length; j++) {
                    q6[i][j] = null;
                }
            }

            for (i = 0; i < q6.length; i++) {
                q6[i] = null;
            }

            q6 = null;
        } // if (q6 != null)

        if (q7 != null) {

            for (i = 0; i < q7.length; i++) {

                for (j = 0; j < q7[i].length; j++) {
                    q7[i][j] = null;
                }
            }

            for (i = 0; i < q7.length; i++) {
                q7[i] = null;
            }

            q7 = null;
        } // if (q7 != null)

        delta = null;
        tpsBest = null;
        kpvt = null;
        gersch = null;

        if (e != null) {

            for (i = 0; i < e.length; i++) {
                e[i] = null;
            }

            e = null;
        }

        tps = null;
        targetArray = null;
        targetMask = null;
        System.gc();

        if (es == null) {
            finalize();
            MipavUtil.displayError("AlgorithmRegAIR: Error es is null");
            
            setCompleted(false);

            return;
        }

        if (threadStopped) {
            finalize();
            
            setCompleted(false);

            return;
        }

        if (cubicInterpolation) {
            pixel_size_s = targetVoxelX;

            if (targetVoxelY < pixel_size_s) {
                pixel_size_s = targetVoxelY;
            }

            if (targetVoxelZ < pixel_size_s) {
                pixel_size_s = targetVoxelZ;
            }

            xoom1 = (float) (targetVoxelX / pixel_size_s);
            targetXDim = (int) (((targetXDim - 1) * xoom1) + 1);
            targetVoxelX = (float) pixel_size_s;

            yoom1 = (float) (targetVoxelY / pixel_size_s);
            targetYDim = (int) (((targetYDim - 1) * yoom1) + 1);
            targetVoxelY = (float) pixel_size_s;

            zoom1 = (float) (targetVoxelZ / pixel_size_s);
            targetZDim = (int) (((targetZDim - 1) * zoom1) + 1);
            targetVoxelZ = (float) pixel_size_s;
        } // if (cubicInterpolation)

        fireProgressStateChanged(80);
        fireProgressStateChanged("Reslicing source image to result image");

        if ((interpolation == AlgorithmTransform.BILINEAR) || (interpolation == AlgorithmTransform.TRILINEAR)) {
            dataOut = reslicer(sourceArray, sourceXDim, sourceYDim, sourceZDim, targetXDim, targetYDim, targetZDim, es);
        } else if (interpolation == AlgorithmTransform.NEAREST_NEIGHBOR) {
            dataOut = nnreslicer(sourceArray, sourceXDim, sourceYDim, sourceZDim, targetXDim, targetYDim, targetZDim,
                                 es);
        } else if (interpolation == AlgorithmTransform.WSINC) {

            if (inPlane) {
                dataOut = sincer(sourceArray, sourceXDim, sourceYDim, sourceZDim, targetXDim, targetYDim, targetZDim,
                                 es, 3, 3, 1);
            } else {
                dataOut = sincer(sourceArray, sourceXDim, sourceYDim, sourceZDim, targetXDim, targetYDim, targetZDim,
                                 es, 3, 3, 3);
            }
        }

        if (es != null) {

            for (i = 0; i < es.length; i++) {
                es[i] = null;
            }

            es = null;
        } // if (es != null)

        sourceArray = null;

        try {
            resultImage.importData(0, dataOut, true);
        } catch (IOException error) {
            finalize();
            MipavUtil.displayError("AlgorithmRegAIR: IOException on resultImage" + ".importData(0,dataOut,true)");
            
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            finalize();
            MipavUtil.displayError("AlgorithmRegAIR: Out of memory on resultImage" + ".importData(0,dataOut,true)");
            
            setCompleted(false);

            return;
        }

        dataOut = null;

        resultResolutions[0] = targetVoxelX;
        resultResolutions[1] = targetVoxelY;
        resultResolutions[2] = targetVoxelZ;

        if (resultImage.getNDims() == 2) {
            resultZDim = 1;
        } else {
            resultZDim = resultImage.getExtents()[2];
        }

        for (i = 0; i < resultZDim; i++) {
            resultImage.getFileInfo()[i].setResolutions(resultResolutions);
            resultImage.getFileInfo()[i].setMin(resultImage.getMin());
            resultImage.getFileInfo()[i].setMax(resultImage.getMax());
            resultImage.getFileInfo()[i].setUnitsOfMeasure(targetUnits);
        }

        resultResolutions = null;
        targetUnits = null;
        
        setCompleted(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e                   DOCUMENT ME!
     * @param  de                  DOCUMENT ME!
     * @param  ee                  DOCUMENT ME!
     * @param  tps                 DOCUMENT ME!
     * @param  xoom1               DOCUMENT ME!
     * @param  xoom2               DOCUMENT ME!
     * @param  yoom1               DOCUMENT ME!
     * @param  yoom2               DOCUMENT ME!
     * @param  zoom1               DOCUMENT ME!
     * @param  zoom2               DOCUMENT ME!
     * @param  pixel_size1         DOCUMENT ME!
     * @param  pixel_size2         DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  z_dim1              DOCUMENT ME!
     * @param  z_dim2              DOCUMENT ME!
     * @param  cubicInterpolation  DOCUMENT ME!
     */
    void gscale_rotatef(double[] e, double[][] de, double[][][] ee, double[] tps, float xoom1, float xoom2, float yoom1,
                        float yoom2, float zoom1, float zoom2, double pixel_size1, double pixel_size2, int x_dim1,
                        int x_dim2, int y_dim1, int y_dim2, int z_dim1, int z_dim2, boolean cubicInterpolation) {

        double[] x = new double[5];
        double[] y = new double[5];
        double[] z = new double[5];
        double[][] dx = new double[5][7];
        double[][] dy = new double[5][7];
        double[][] dz = new double[5][7];
        double[][][] ex = new double[5][7][7];
        double[][][] ey = new double[5][7][7];
        double[][][] ez = new double[5][7][7];
        double pitch, roll, yaw, p, q, r, gscale;
        double sinyaw, cosyaw, sinpitch, cospitch, sinroll, cosroll;

        pitch = tps[0];
        roll = tps[1];
        yaw = tps[2];
        p = tps[3];
        q = tps[4];
        r = tps[5];
        gscale = tps[6];

        sinyaw = Math.sin(yaw);
        cosyaw = Math.cos(yaw);
        sinpitch = Math.sin(pitch);
        cospitch = Math.cos(pitch);
        sinroll = Math.sin(roll);
        cosroll = Math.cos(roll);

        if (!cubicInterpolation) {
            x[0] = ((2 * e[0]) + 1 - x_dim1) * xoom1 * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + 1 - y_dim1) * yoom1 * (pixel_size1 / pixel_size2);
            z[0] = ((2 * e[2]) + 1 - z_dim1) * zoom1 * (pixel_size1 / pixel_size2);
        } else {
            x[0] = ((2 * e[0]) + ((1 - x_dim1) * xoom1)) * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + ((1 - y_dim1) * yoom1)) * (pixel_size1 / pixel_size2);
            z[0] = ((2 * e[2]) + ((1 - z_dim1) * zoom1)) * (pixel_size1 / pixel_size2);
        }

        /*Apply scaling*/

        x[1] = x[0] * gscale;
        dx[1][6] = x[0];

        /*ex[1][6][6]=0;*/

        y[1] = y[0] * gscale;
        dy[1][6] = y[0];

        /*ey[1][6][6]=0;*/

        z[1] = z[0] * gscale;
        dz[1][6] = z[0];

        /*ez[1][6][6]=0;*/

        /*Apply yaw*/

        x[2] = (x[1] * cosyaw) + (y[1] * sinyaw);
        dx[2][2] = (-x[1] * sinyaw) + (y[1] * cosyaw);
        ex[2][2][2] = (-x[1] * cosyaw) - (y[1] * sinyaw);

        dx[2][6] = (dx[1][6] * cosyaw) + (dy[1][6] * sinyaw);
        ex[2][6][2] = (-dx[1][6] * sinyaw) + (dy[1][6] * cosyaw);

        /*ex[2][6][6]=ex[1][6][6]*cosyaw+ey[1][6][6]*sinyaw;*/


        y[2] = (y[1] * cosyaw) - (x[1] * sinyaw);

        dy[2][2] = (-x[1] * cosyaw) - (y[1] * sinyaw);
        ey[2][2][2] = (x[1] * sinyaw) - (y[1] * cosyaw);

        dy[2][6] = (dy[1][6] * cosyaw) - (dx[1][6] * sinyaw);
        ey[2][6][2] = (-dy[1][6] * sinyaw) - (dx[1][6] * cosyaw);

        /*ey[2][6][6]=ey[1][6][6]*cosyaw-ex[1][6][6]*sinyaw;*/


        /*z[2]=z[1]*/

        /*Apply pitch*/

        /*x[3]=x[2];*/

        /*derivatives of y[3] with respect to parameter 1 which is not yet applied are*/

        /* computed as zero since these values are references subsequently*/

        /* This is not done for x[3] and z[3] because they are futher modified*/

        /* to generate x[4] and z[4] which have nonzero derivatives with respect to*/

        /* parameter 1 */

        y[3] = (y[2] * cospitch) + (z[1] * sinpitch);

        dy[3][0] = (-y[2] * sinpitch) + (z[1] * cospitch);

        ey[3][0][0] = (-y[2] * cospitch) - (z[1] * sinpitch);

        dy[3][1] = 0; /*parameter 1 not yet applied*/
        ey[3][1][0] = 0;
        ey[3][1][1] = 0;

        dy[3][2] = dy[2][2] * cospitch /*+dz[1][2]*sinpitch*/;

        ey[3][2][0] = -dy[2][2] * sinpitch;
        ey[3][2][1] = 0; /*parameter 1 not yet applied*/
        ey[3][2][2] = ey[2][2][2] * cospitch;

        dy[3][6] = (dy[2][6] * cospitch) + (dz[1][6] * sinpitch);

        ey[3][6][0] = (-dy[2][6] * sinpitch) + (dz[1][6] * cospitch);
        ey[3][6][1] = 0; /*parameter 1 not yet applied*/
        ey[3][6][2] = ey[2][6][2] * cospitch /*+ez[1][6][2]*sinpitch*/;
        ey[3][6][6] = 0; /*ey[2][6][6]*cospitch+ez[1][6][6]*cospitch*/

        z[3] = (z[1] * cospitch) - (y[2] * sinpitch);

        dz[3][0] = (-z[1] * sinpitch) - (y[2] * cospitch);

        ez[3][0][0] = (-z[1] * cospitch) + (y[2] * sinpitch);

        dz[3][2] = /*dz[1][2]*cospitch*/ -dy[2][2] * sinpitch;

        ez[3][2][0] = -dy[2][2] * cospitch;
        ez[3][2][2] = -ey[2][2][2] * sinpitch;

        dz[3][6] = (dz[1][6] * cospitch) - (dy[2][6] * sinpitch);

        ez[3][6][0] = (-dz[1][6] * sinpitch) - (dy[2][6] * cospitch);
        ez[3][6][2] = /*ez[1][6][2]*cospitch*/ -ey[2][6][2] * sinpitch;
        ez[3][6][6] = 0; /*ez[1][6][6]*cospitch-ey[2][6][6]*sinpitch*/

        /* Apply roll */

        x[4] = (x[2] * cosroll) + (z[3] * sinroll);
        dx[4][0] = /*dx[2][0]*cosroll+*/ dz[3][0] * sinroll;
        ex[4][0][0] = ez[3][0][0] * sinroll;
        dx[4][1] = (-x[2] * sinroll) + (z[3] * cosroll);
        ex[4][1][0] = /*-dx[2][0]*sinroll+*/ dz[3][0] * cosroll;
        ex[4][1][1] = (-x[2] * cosroll) - (z[3] * sinroll);
        dx[4][2] = (dx[2][2] * cosroll) + (dz[3][2] * sinroll);
        ex[4][2][0] = /*ex[2][2][0]*cosroll+*/ ez[3][2][0] * sinroll;
        ex[4][2][1] = (-dx[2][2] * sinroll) + (dz[3][2] * cosroll);
        ex[4][2][2] = (ex[2][2][2] * cosroll) + (ez[3][2][2] * sinroll);
        dx[4][6] = (dx[2][6] * cosroll) + (dz[3][6] * sinroll);
        ex[4][6][0] = /*ex[2][6][0]*cosroll+*/ ez[3][6][0] * sinroll;
        ex[4][6][1] = (-dx[2][6] * sinroll) + (dz[3][6] * cosroll);
        ex[4][6][2] = (ex[2][6][2] * cosroll) + (ez[3][6][2] * sinroll);
        ex[4][6][6] = 0; /*ex[2][6][6]*cosroll+ez[3][6][6]*sinroll;*/

        /*y[4]=y[3];*/

        z[4] = (z[3] * cosroll) - (x[2] * sinroll);
        dz[4][0] = dz[3][0] * cosroll /*-dx[2][0]*sinroll*/;
        ez[4][0][0] = ez[3][0][0] * cosroll;
        dz[4][1] = (-z[3] * sinroll) - (x[2] * cosroll);
        ez[4][1][0] = -dz[3][0] * sinroll /*-dx[2][0]*cosroll*/;
        ez[4][1][1] = (-z[3] * cosroll) + (x[2] * sinroll);
        dz[4][2] = (dz[3][2] * cosroll) - (dx[2][2] * sinroll);
        ez[4][2][0] = ez[3][2][0] * cosroll /*-ex[2][2][0]*sinroll*/;
        ez[4][2][1] = (-dz[3][2] * sinroll) - (dx[2][2] * cosroll);
        ez[4][2][2] = (ez[3][2][2] * cosroll) - (ex[2][2][2] * sinroll);
        dz[4][6] = (dz[3][6] * cosroll) - (dx[2][6] * sinroll);
        ez[4][6][0] = ez[3][6][0] * cosroll /*-ex[2][6][0]*sinroll*/;
        ez[4][6][1] = (-dz[3][6] * sinroll) - (dx[2][6] * cosroll);
        ez[4][6][2] = (ez[3][6][2] * cosroll) - (ex[2][6][2] * sinroll);
        ez[4][6][6] = 0; /*ez[3][6][6]*cosroll-ex[2][6][6]*sinroll;*/

        e[0] = (x[4] - p + ((x_dim2 - 1) * xoom2)) / (2 * xoom2);

        de[0][0] = dx[4][0] / (2 * xoom2);
        ee[0][0][0] = ex[4][0][0] / (2 * xoom2);
        de[0][1] = dx[4][1] / (2 * xoom2);
        ee[0][1][0] = ex[4][1][0] / (2 * xoom2);
        ee[0][1][1] = ex[4][1][1] / (2 * xoom2);
        de[0][2] = dx[4][2] / (2 * xoom2);
        ee[0][2][0] = ex[4][2][0] / (2 * xoom2);
        ee[0][2][1] = ex[4][2][1] / (2 * xoom2);
        ee[0][2][2] = ex[4][2][2] / (2 * xoom2);
        de[0][3] = -1.0 / (2 * xoom2);
        ee[0][3][0] = 0;
        ee[0][3][1] = 0;
        ee[0][3][2] = 0;
        ee[0][3][3] = 0;
        de[0][4] = 0;
        ee[0][4][0] = 0;
        ee[0][4][1] = 0;
        ee[0][4][2] = 0;
        ee[0][4][3] = 0;
        ee[0][4][4] = 0;
        de[0][5] = 0;
        ee[0][5][0] = 0;
        ee[0][5][1] = 0;
        ee[0][5][2] = 0;
        ee[0][5][3] = 0;
        ee[0][5][4] = 0;
        ee[0][5][5] = 0;
        de[0][6] = dx[4][6] / 2;
        ee[0][6][0] = ex[4][6][0] / (2 * xoom2);
        ee[0][6][1] = ex[4][6][1] / (2 * xoom2);
        ee[0][6][2] = ex[4][6][2] / (2 * xoom2);
        ee[0][6][3] = 0;
        ee[0][6][4] = 0;
        ee[0][6][5] = 0;
        ee[0][6][6] = ex[4][6][6] / (2 * xoom2);

        e[1] = (y[3] - q + ((y_dim2 - 1) * yoom2)) / (2 * yoom2);

        de[1][0] = dy[3][0] / (2 * yoom2);
        ee[1][0][0] = ey[3][0][0] / (2 * yoom2);
        de[1][1] = dy[3][1] / (2 * yoom2);
        ee[1][1][0] = ey[3][1][0] / (2 * yoom2);
        ee[1][1][1] = ey[3][1][1] / (2 * yoom2);
        de[1][2] = dy[3][2] / (2 * yoom2);
        ee[1][2][0] = ey[3][2][0] / (2 * yoom2);
        ee[1][2][1] = ey[3][2][1] / (2 * yoom2);
        ee[1][2][2] = ey[3][2][2] / (2 * yoom2);
        de[1][3] = 0;
        ee[1][3][0] = 0;
        ee[1][3][1] = 0;
        ee[1][3][2] = 0;
        ee[1][3][3] = 0;
        de[1][4] = -1.0 / (2 * yoom2);
        ee[1][4][0] = 0;
        ee[1][4][1] = 0;
        ee[1][4][2] = 0;
        ee[1][4][3] = 0;
        ee[1][4][4] = 0;
        de[1][5] = 0;
        ee[1][5][0] = 0;
        ee[1][5][1] = 0;
        ee[1][5][2] = 0;
        ee[1][5][3] = 0;
        ee[1][5][4] = 0;
        ee[1][5][5] = 0;
        de[1][6] = dy[3][6] / (2 * yoom2);
        ee[1][6][0] = ey[3][6][0] / (2 * yoom2);
        ee[1][6][1] = ey[3][6][1] / (2 * yoom2);
        ee[1][6][2] = ey[3][6][2] / (2 * yoom2);
        ee[1][6][3] = 0;
        ee[1][6][4] = 0;
        ee[1][6][5] = 0;
        ee[1][6][6] = ey[3][6][6] / (2 * yoom2);

        e[2] = (z[4] - r + ((z_dim2 - 1) * zoom2)) / (2 * zoom2);

        de[2][0] = dz[4][0] / (2 * zoom2);
        ee[2][0][0] = ez[4][0][0] / (2 * zoom2);
        de[2][1] = dz[4][1] / (2 * zoom2);
        ee[2][1][0] = ez[4][1][0] / (2 * zoom2);
        ee[2][1][1] = ez[4][1][1] / (2 * zoom2);
        de[2][2] = dz[4][2] / (2 * zoom2);
        ee[2][2][0] = ez[4][2][0] / (2 * zoom2);
        ee[2][2][1] = ez[4][2][1] / (2 * zoom2);
        ee[2][2][2] = ez[4][2][2] / (2 * zoom2);
        de[2][3] = 0;
        ee[2][3][0] = 0;
        ee[2][3][1] = 0;
        ee[2][3][2] = 0;
        ee[2][3][3] = 0;
        de[2][4] = 0;
        ee[2][4][0] = 0;
        ee[2][4][1] = 0;
        ee[2][4][2] = 0;
        ee[2][4][3] = 0;
        ee[2][4][4] = 0;
        de[2][5] = -1.0 / (2 * zoom2);
        ee[2][5][0] = 0;
        ee[2][5][1] = 0;
        ee[2][5][2] = 0;
        ee[2][5][3] = 0;
        ee[2][5][4] = 0;
        ee[2][5][5] = 0;
        de[2][6] = dz[4][6] / (2 * zoom2);
        ee[2][6][0] = ez[4][6][0] / (2 * zoom2);
        ee[2][6][1] = ez[4][6][1] / (2 * zoom2);
        ee[2][6][2] = ez[4][6][2] / (2 * zoom2);
        ee[2][6][3] = 0;
        ee[2][6][4] = 0;
        ee[2][6][5] = 0;
        ee[2][6][6] = ez[4][6][6] / (2 * zoom2);

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              source parameters([4][3])
     * @param   des             derivatives of each source parameter with respect to each element
     * @param   ees             second derivatives as above
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2XDim      DOCUMENT ME!
     * @param   pixel2YDim      DOCUMENT ME!
     * @param   pixel2ZDim      DOCUMENT ME!
     * @param   pixel5          interpolated values of target file
     * @param   pixel5Mask      mask of interpolated values of target file
     * @param   pixel5XDim      DOCUMENT ME!
     * @param   pixel5YDim      DOCUMENT ME!
     * @param   pixel5ZDim      DOCUMENT ME!
     * @param   threshold5      DOCUMENT ME!
     * @param   dcff            first derivatives of sd with respect to parameters
     * @param   ecff            second derivatives of sd with respect to parameters
     * @param   count           number of pixels in partitions that are above threshold
     * @param   mean            ratio of pixel in source file/corresponding pixel in target file
     * @param   square          intermediate computation value
     * @param   dmean           partial derivatives of mean with respect to parameters
     * @param   dsquare         partial derivatives of square
     * @param   esquare         second partial derivatives of square
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  provides verification of compatibility of calling routine This routine computes the
     *                          first and second derivatives of the correlation ratio with respect to all external
     *                          parameters.
     *
     *                          <p>The matrix indices have been adjusted to allow for a more orderly relationship
     *                          between the matrix and its elements</p>
     *
     *                          <p>Returns the correlation ratio</p>
     *
     * @return  DOCUMENT ME!
     */

    double qvderivsCOR12(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                         float[] pixel2, int pixel2XDim, int pixel2YDim, int pixel2ZDim, float[] pixel5,
                         boolean[] pixel5Mask, int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5,
                         double[] dcff, double[][] ecff, int[] count, double[] mean, double[] square, double[][] dmean,
                         double[][] dsquare, double[][][] esquare, int partitions, double maxActualValue) {

        double cf = 0.0;
        double[] dcf = new double[12];
        double[][] ecf = new double[12][12];
        double[] dxyz = new double[4];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        int counttotal;
        double meantotal;

        int jj; /*index of current partition*/
        int pix1; /*int value of current pixel in standard file*/
        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double value; /*intermediate value*/
        double[] dvalue = new double[12]; /*partial derivatives of value with respect to parameters*/
        double sd2;
        double[] dsd2 = new double[12];
        double[][] esd2 = new double[12][12];

        int i, j, k;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_dim1, y_dim1, z_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        double e00, e01, e02, e10, e11, e12, e20, e21, e22, e30, e31, e32;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4;
        int counts;
        double means;
        double sds;
        double[] dsds = new double[12];
        double[][] esds = new double[12][12];
        double temp;
        float pix4Min;

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < 12; t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        z_dim1 = pixel2ZDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];

        pix4Min = Float.MAX_VALUE;

        for (i = 0; i < pixel2.length; i++) {

            if (pixel2[i] < pix4Min) {
                pix4Min = pixel2[i];
            }
        }

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + (k * e20) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + (k * e21) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            z_i = (i * e02) + (j * e12) + (k * e22) + e32;

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

            /* Notice that dx[0], dx[1], dx[2], dx[3] are equal to dy[4], dy[5], dy[6], dy[7]
             * and to dz[8], dz[9], dz[10], dz[11].  Consequently, these are all treated as dxyz[0], dxyz[1], dxyz[2],
             * and dxyz[3]. */
            dxyz[0] = (double) i;
            dxyz[1] = (double) j;
            dxyz[2] = (double) k;
            dxyz[3] = 1.0;

            /*Get the coordinates of the 8 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);
            z_up = (int) Math.ceil(z_i);
            z_down = (int) Math.floor(z_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            if (z_up == z_down) {

                if (z_up < z_max1) {
                    z_up++;
                } else {
                    z_down--;
                }
            }

            c = z_i - z_down;
            f = z_up - z_i;

            /*Get the values of these 8 voxels*/

            n0 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_up];
            n4 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_down];
            n5 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_up];
            n6 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_down];
            n7 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - threshold5) / (maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e * f) + (n1 * a * e * f) + (n2 * d * b * f) + (n3 * a * b * f) + (n4 * d * e * c) +
                   (n5 * a * e * c) + (n6 * d * b * c) + (n7 * a * b * c);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = (((e * f) * (n1 - n0)) + ((b * f) * (n3 - n2)) + ((c * e) * (n5 - n4)) + ((b * c) * (n7 - n6)));
            dypix4 = (((d * f) * (n2 - n0)) + ((a * f) * (n3 - n1)) + ((c * d) * (n6 - n4)) + ((a * c) * (n7 - n5)));
            dzpix4 = (((d * e) * (n4 - n0)) + ((a * e) * (n5 - n1)) + ((b * d) * (n6 - n2)) + ((a * b) * (n7 - n3)));

            /* Calculate values needed to compute standard deviation */

            /*Calculate the value of the reslice voxel above the minimum */
            value = pix4 - pix4Min;
            mean[pix1] += value;
            square[pix1] += value * value;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 4; t++) {
                dvalue[t] = dxpix4 * dxyz[t];
                dmean[pix1][t] += dvalue[t];
                dsquare[pix1][t] += 2 * value * dvalue[t];
                dvalue[t + 4] = dypix4 * dxyz[t];
                dmean[pix1][t + 4] += dvalue[t + 4];
                dsquare[pix1][t + 4] += 2 * value * dvalue[t + 4];
                dvalue[t + 8] = dzpix4 * dxyz[t];
                dmean[pix1][t + 8] += dvalue[t + 8];
                dsquare[pix1][t + 8] += 2 * value * dvalue[t + 8];
            }

            /* Second derivatives */
            for (t = 0; t < 12; t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dvalue[s] * dvalue[t]);
                }
            }
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        /*Initialize values*/
        meantotal = 0;
        counttotal = 0;
        cf = 0;

        for (t = 0; t < 12; t++) {
            dcf[t] = 0;

            for (s = 0; s < 12; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Calculate normalized standard deviation for each partition*/


        for (jj = 0; jj < partitions; jj++) {

            /*Avoid subsequent NaN errors*/
            if (count[jj] <= 1) {
                continue;
            }

            counts = count[jj];

            means = mean[jj] / counts;

            for (t = 0; t < 12; t++) {
                dmean[jj][t] /= counts;
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0.0) {
                continue;
            }

            for (t = 0; t < 12; t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] - (2 * counts * (dmean[jj][s] * dmean[jj][t]))) / (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < 12; t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < 12; t++) {

                for (s = 0; s <= t; s++) {

                    esd2[t][s] = (means * dsd2[s]) + (2 * sd2 * dmean[jj][s]);
                    esd2[t][s] /= (2 * means * means * means * sd2 * sd2);
                    esd2[t][s] *= (-dsds[t]);
                    esd2[t][s] += esds[t][s] / (2 * means * means * sd2);
                    esd2[t][s] -= (dsd2[s] * dmean[jj][t]) / means;
                    esd2[t][s] += sd2 * dmean[jj][t] * dmean[jj][s] / (means * means);
                }
            }

            /*Add weighted normalized standard deviation  of this partitions*/

            /* to cumulative total for all partitions*/

            cf += sd2 * counts;
            meantotal += means * counts;

            for (t = 0; t < 12; t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Renormalize cumulated values*/

        cf /= counttotal;
        meantotal = meantotal / counttotal;

        for (t = 0; t < 12; t++) {
            dcf[t] = dcf[t] / counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = ecf[t][s] / counttotal;
            }
        }

        /* Fill in redundant matrix elements*/
        for (t = 0; t < 12; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[2][0][t];
            dcff[t] += dcf[3] * des[3][0][t];
            dcff[t] += dcf[4] * des[0][1][t];
            dcff[t] += dcf[5] * des[1][1][t];
            dcff[t] += dcf[6] * des[2][1][t];
            dcff[t] += dcf[7] * des[3][1][t];
            dcff[t] += dcf[8] * des[0][2][t];
            dcff[t] += dcf[9] * des[1][2][t];
            dcff[t] += dcf[10] * des[2][2][t];
            dcff[t] += dcf[11] * des[3][2][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[2][0][s];
                temp += ecf[0][3] * des[3][0][s];
                temp += ecf[0][4] * des[0][1][s];
                temp += ecf[0][5] * des[1][1][s];
                temp += ecf[0][6] * des[2][1][s];
                temp += ecf[0][7] * des[3][1][s];
                temp += ecf[0][8] * des[0][2][s];
                temp += ecf[0][9] * des[1][2][s];
                temp += ecf[0][10] * des[2][2][s];
                temp += ecf[0][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[2][0][s];
                temp += ecf[1][3] * des[3][0][s];
                temp += ecf[1][4] * des[0][1][s];
                temp += ecf[1][5] * des[1][1][s];
                temp += ecf[1][6] * des[2][1][s];
                temp += ecf[1][7] * des[3][1][s];
                temp += ecf[1][8] * des[0][2][s];
                temp += ecf[1][9] * des[1][2][s];
                temp += ecf[1][10] * des[2][2][s];
                temp += ecf[1][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[2][0][s];
                temp += ecf[2][3] * des[3][0][s];
                temp += ecf[2][4] * des[0][1][s];
                temp += ecf[2][5] * des[1][1][s];
                temp += ecf[2][6] * des[2][1][s];
                temp += ecf[2][7] * des[3][1][s];
                temp += ecf[2][8] * des[0][2][s];
                temp += ecf[2][9] * des[1][2][s];
                temp += ecf[2][10] * des[2][2][s];
                temp += ecf[2][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][0][t]) + (dcf[2] * ees[2][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[2][0][s];
                temp += ecf[3][3] * des[3][0][s];
                temp += ecf[3][4] * des[0][1][s];
                temp += ecf[3][5] * des[1][1][s];
                temp += ecf[3][6] * des[2][1][s];
                temp += ecf[3][7] * des[3][1][s];
                temp += ecf[3][8] * des[0][2][s];
                temp += ecf[3][9] * des[1][2][s];
                temp += ecf[3][10] * des[2][2][s];
                temp += ecf[3][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[3] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[2][0][s];
                temp += ecf[4][3] * des[3][0][s];
                temp += ecf[4][4] * des[0][1][s];
                temp += ecf[4][5] * des[1][1][s];
                temp += ecf[4][6] * des[2][1][s];
                temp += ecf[4][7] * des[3][1][s];
                temp += ecf[4][8] * des[0][2][s];
                temp += ecf[4][9] * des[1][2][s];
                temp += ecf[4][10] * des[2][2][s];
                temp += ecf[4][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[4] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[2][0][s];
                temp += ecf[5][3] * des[3][0][s];
                temp += ecf[5][4] * des[0][1][s];
                temp += ecf[5][5] * des[1][1][s];
                temp += ecf[5][6] * des[2][1][s];
                temp += ecf[5][7] * des[3][1][s];
                temp += ecf[5][8] * des[0][2][s];
                temp += ecf[5][9] * des[1][2][s];
                temp += ecf[5][10] * des[2][2][s];
                temp += ecf[5][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[5] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[6][0] * des[0][0][s];
                temp += ecf[6][1] * des[1][0][s];
                temp += ecf[6][2] * des[2][0][s];
                temp += ecf[6][3] * des[3][0][s];
                temp += ecf[6][4] * des[0][1][s];
                temp += ecf[6][5] * des[1][1][s];
                temp += ecf[6][6] * des[2][1][s];
                temp += ecf[6][7] * des[3][1][s];
                temp += ecf[6][8] * des[0][2][s];
                temp += ecf[6][9] * des[1][2][s];
                temp += ecf[6][10] * des[2][2][s];
                temp += ecf[6][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][1][t]) + (dcf[6] * ees[2][1][t][s]);
                temp = 0;
                temp += ecf[7][0] * des[0][0][s];
                temp += ecf[7][1] * des[1][0][s];
                temp += ecf[7][2] * des[2][0][s];
                temp += ecf[7][3] * des[3][0][s];
                temp += ecf[7][4] * des[0][1][s];
                temp += ecf[7][5] * des[1][1][s];
                temp += ecf[7][6] * des[2][1][s];
                temp += ecf[7][7] * des[3][1][s];
                temp += ecf[7][8] * des[0][2][s];
                temp += ecf[7][9] * des[1][2][s];
                temp += ecf[7][10] * des[2][2][s];
                temp += ecf[7][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[7] * ees[3][1][t][s]);
                temp = 0;
                temp += ecf[8][0] * des[0][0][s];
                temp += ecf[8][1] * des[1][0][s];
                temp += ecf[8][2] * des[2][0][s];
                temp += ecf[8][3] * des[3][0][s];
                temp += ecf[8][4] * des[0][1][s];
                temp += ecf[8][5] * des[1][1][s];
                temp += ecf[8][6] * des[2][1][s];
                temp += ecf[8][7] * des[3][1][s];
                temp += ecf[8][8] * des[0][2][s];
                temp += ecf[8][9] * des[1][2][s];
                temp += ecf[8][10] * des[2][2][s];
                temp += ecf[8][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][2][t]) + (dcf[8] * ees[0][2][t][s]);
                temp = 0;
                temp += ecf[9][0] * des[0][0][s];
                temp += ecf[9][1] * des[1][0][s];
                temp += ecf[9][2] * des[2][0][s];
                temp += ecf[9][3] * des[3][0][s];
                temp += ecf[9][4] * des[0][1][s];
                temp += ecf[9][5] * des[1][1][s];
                temp += ecf[9][6] * des[2][1][s];
                temp += ecf[9][7] * des[3][1][s];
                temp += ecf[9][8] * des[0][2][s];
                temp += ecf[9][9] * des[1][2][s];
                temp += ecf[9][10] * des[2][2][s];
                temp += ecf[9][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][2][t]) + (dcf[9] * ees[1][2][t][s]);
                temp = 0;
                temp += ecf[10][0] * des[0][0][s];
                temp += ecf[10][1] * des[1][0][s];
                temp += ecf[10][2] * des[2][0][s];
                temp += ecf[10][3] * des[3][0][s];
                temp += ecf[10][4] * des[0][1][s];
                temp += ecf[10][5] * des[1][1][s];
                temp += ecf[10][6] * des[2][1][s];
                temp += ecf[10][7] * des[3][1][s];
                temp += ecf[10][8] * des[0][2][s];
                temp += ecf[10][9] * des[1][2][s];
                temp += ecf[10][10] * des[2][2][s];
                temp += ecf[10][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][2][t]) + (dcf[10] * ees[2][2][t][s]);
                temp = 0;
                temp += ecf[11][0] * des[0][0][s];
                temp += ecf[11][1] * des[1][0][s];
                temp += ecf[11][2] * des[2][0][s];
                temp += ecf[11][3] * des[3][0][s];
                temp += ecf[11][4] * des[0][1][s];
                temp += ecf[11][5] * des[1][1][s];
                temp += ecf[11][6] * des[2][1][s];
                temp += ecf[11][7] * des[3][1][s];
                temp += ecf[11][8] * des[0][2][s];
                temp += ecf[11][9] * des[1][2][s];
                temp += ecf[11][10] * des[2][2][s];
                temp += ecf[11][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][2][t]) + (dcf[11] * ees[3][2][t][s]);
            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              source parameters([4][3])
     * @param   des             derivatives of each source parameter with respect to each element
     * @param   ees             second derivatives as above
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2XDim      DOCUMENT ME!
     * @param   pixel2YDim      DOCUMENT ME!
     * @param   pixel5          interpolated values of target file
     * @param   pixel5Mask      mask of interpolated values of target file
     * @param   pixel5XDim      DOCUMENT ME!
     * @param   pixel5YDim      DOCUMENT ME!
     * @param   pixel5ZDim      DOCUMENT ME!
     * @param   threshold5      DOCUMENT ME!
     * @param   dcff            first derivatives of sd with respect to parameters
     * @param   ecff            second derivatives of sd with respect to parameters
     * @param   count           number of pixels in partitions that are above threshold
     * @param   mean            ratio of pixel in source file/corresponding pixel in target file
     * @param   square          intermediate computation value
     * @param   dmean           partial derivatives of mean with respect to parameters
     * @param   dsquare         partial derivatives of square
     * @param   esquare         second partial derivatives of square
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  This routine computes the first and second derivatives of the correlation ratio with
     *                          respect to all external parameters.
     *
     *                          <p>The matrix indices have been adjusted to allow for a more orderly relationship
     *                          between the matrix and its elements</p>
     *
     *                          <p>Returns the correlation ratio</p>
     *
     * @return  DOCUMENT ME!
     */

    double qvderivsCOR6(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                        float[] pixel2, int pixel2XDim, int pixel2YDim, float[] pixel5, boolean[] pixel5Mask,
                        int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5, double[] dcff,
                        double[][] ecff, int[] count, double[] mean, double[] square, double[][] dmean,
                        double[][] dsquare, double[][][] esquare, int partitions, double maxActualValue) {

        double cf = 0.0;
        double[] dcf = new double[6];
        double[][] ecf = new double[6][6];
        double[] dxy = new double[3];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        int counttotal;
        double meantotal;

        int jj; /*index of current partition*/
        int pix1; /*int value of current pixel in standard file*/
        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double value; /*intermediate value*/
        double[] dvalue = new double[6]; /*partial derivatives of value with respect to parameters*/
        double sd2;
        double[] dsd2 = new double[6];
        double[][] esd2 = new double[6][6];

        int i, j, k;
        int s, t;
        int x_max1, y_max1;
        int x_dim1, y_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        double e00, e01, e10, e11, e30, e31;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4;
        int counts;
        double means;
        double sds;
        double[] dsds = new double[6];
        double[][] esds = new double[6][6];
        double temp;
        float pix4Min;

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < 6; t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e10 = es[1][0];
        e11 = es[1][1];
        e30 = es[3][0];
        e31 = es[3][1];

        pix4Min = Float.MAX_VALUE;

        for (i = 0; i < pixel2.length; i++) {

            if (pixel2[i] < pix4Min) {
                pix4Min = pixel2[i];
            }
        }

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            /* Note that dx[0], dx[1], and dx[2] are equal to dy[3], dy[4], and dy[5] respectively.
             * Consequently, these are all treated as dxy[0], dxy[1], and dxy[2] */

            dxy[0] = (double) i;
            dxy[1] = (double) j;
            dxy[2] = 1.0;

            /* Second derivatives are all zero */

            /*Get the coordinates of the 4 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            /*Get the values of these 4 voxels*/

            n0 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - threshold5) / (maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            /* Calculate values needed to compute standard deviation */

            /*Calculate the value of the reslice pixel above the minimum*/
            value = pix4 - pix4Min;
            mean[pix1] += value;
            square[pix1] += value * value;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 3; t++) {
                dvalue[t] = dxpix4 * dxy[t];
                dmean[pix1][t] += dvalue[t];
                dsquare[pix1][t] += 2 * value * dvalue[t];
                dvalue[t + 3] = dypix4 * dxy[t];
                dmean[pix1][t + 3] += dvalue[t + 3];
                dsquare[pix1][t + 3] += 2 * value * dvalue[t + 3];
            }

            /* Second derivatives */
            for (t = 0; t < 6; t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dvalue[s] * dvalue[t]);
                }
            }
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        /*Initialize values*/
        meantotal = 0;
        counttotal = 0;
        cf = 0;

        for (t = 0; t < 6; t++) {
            dcf[t] = 0;

            for (s = 0; s < 6; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Calculate normalized standard deviation for each partition*/


        for (jj = 0; jj < partitions; jj++) {

            /*Avoid subsequent NaN errors*/
            if (count[jj] <= 1) {
                continue;
            }

            counts = count[jj];

            means = mean[jj] / counts;

            for (t = 0; t < 6; t++) {
                dmean[jj][t] /= counts;
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0.0) {
                continue;
            }

            for (t = 0; t < 6; t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] - (2 * counts * (dmean[jj][s] * dmean[jj][t]))) / (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < 6; t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < 6; t++) {

                for (s = 0; s <= t; s++) {

                    esd2[t][s] = (means * dsd2[s]) + (2 * sd2 * dmean[jj][s]);
                    esd2[t][s] /= (2 * means * means * means * sd2 * sd2);
                    esd2[t][s] *= (-dsds[t]);
                    esd2[t][s] += esds[t][s] / (2 * means * means * sd2);
                    esd2[t][s] -= (dsd2[s] * dmean[jj][t]) / means;
                    esd2[t][s] += sd2 * dmean[jj][t] * dmean[jj][s] / (means * means);
                }
            }

            /*Add weighted normalized standard deviation  of this partitions*/

            /* to cumulative total for all partitions*/

            cf += sd2 * counts;
            meantotal += means * counts;

            for (t = 0; t < 6; t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Renormalize cumulated values*/

        cf /= counttotal;
        meantotal = meantotal / counttotal;

        for (t = 0; t < 6; t++) {
            dcf[t] = dcf[t] / counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = ecf[t][s] / counttotal;
            }
        }

        /* Fill in redundant matrix elements*/
        for (t = 0; t < 6; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[3][0][t];
            dcff[t] += dcf[3] * des[0][1][t];
            dcff[t] += dcf[4] * des[1][1][t];
            dcff[t] += dcf[5] * des[3][1][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[3][0][s];
                temp += ecf[0][3] * des[0][1][s];
                temp += ecf[0][4] * des[1][1][s];
                temp += ecf[0][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[3][0][s];
                temp += ecf[1][3] * des[0][1][s];
                temp += ecf[1][4] * des[1][1][s];
                temp += ecf[1][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[3][0][s];
                temp += ecf[2][3] * des[0][1][s];
                temp += ecf[2][4] * des[1][1][s];
                temp += ecf[2][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[2] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[3][0][s];
                temp += ecf[3][3] * des[0][1][s];
                temp += ecf[3][4] * des[1][1][s];
                temp += ecf[3][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[3] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[3][0][s];
                temp += ecf[4][3] * des[0][1][s];
                temp += ecf[4][4] * des[1][1][s];
                temp += ecf[4][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[4] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[3][0][s];
                temp += ecf[5][3] * des[0][1][s];
                temp += ecf[5][4] * des[1][1][s];
                temp += ecf[5][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[5] * ees[3][1][t][s]);

            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            source parameters([4][3])
     * @param   des           derivatives of each source parameter with respect to each element
     * @param   ees           second derivatives as above
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2XDim    DOCUMENT ME!
     * @param   pixel2YDim    DOCUMENT ME!
     * @param   pixel2ZDim    DOCUMENT ME!
     * @param   pixel5        interpolated values of target file
     * @param   pixel5Mask    mask of interpolated values of target file
     * @param   pixel5XDim    DOCUMENT ME!
     * @param   pixel5YDim    DOCUMENT ME!
     * @param   pixel5ZDim    DOCUMENT ME!
     * @param   threshold5    DOCUMENT ME!
     * @param   dcff          first derivatives of sd with respect to parameters
     * @param   ecff          second derivatives of sd with respect to parameters
     *
     *                        <p>This routine computes the first and second derivatives of the sum of squares with
     *                        respect to all external parameters.</p>
     *
     *                        <p>The matrix indices have been adjusted to allow for a more orderly relationship between
     *                        the matrix and its elements</p>
     *
     * @return  DOCUMENT ME!
     */

    double qvderivsLS12(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                        float[] pixel2, int pixel2XDim, int pixel2YDim, int pixel2ZDim, float[] pixel5,
                        boolean[] pixel5Mask, int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5,
                        double[] dcff, double[][] ecff) {

        double cf = 0.0;
        double[] dcf = new double[12];
        double[][] ecf = new double[12][12];
        double[] dxyz = new double[4];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double[][] dydx = new double[4][4];
        double[][] dxdx = new double[4][4];
        int counttotal;

        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double ratio; /*intermediate value*/

        int i, j, k;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_dim1, y_dim1, z_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        double e00, e01, e02, e10, e11, e12, e20, e21, e22, e30, e31, e32;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4;
        double temp;

        /*Initialize values*/

        counttotal = 0;
        ratio = 0;

        for (t = 0; t < 12; t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        z_dim1 = pixel2ZDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + (k * e20) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + (k * e21) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            z_i = (i * e02) + (j * e12) + (k * e22) + e32;

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

            /* Notice that dx[0], dx[1], dx[2], dx[3] are equal to dy[4], dy[5], dy[6], dy[7]
             * and to dz[8], dz[9], dz[10], dz[11].  Consequently, these are all treated as dxyz[0], dxyz[1], dxyz[2],
             * and dxyz[3]. */
            dxyz[0] = (double) i;
            dxyz[1] = (double) j;
            dxyz[2] = (double) k;
            dxyz[3] = 1.0;

            /*Get the coordinates of the 8 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);
            z_up = (int) Math.ceil(z_i);
            z_down = (int) Math.floor(z_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            if (z_up == z_down) {

                if (z_up < z_max1) {
                    z_up++;
                } else {
                    z_down--;
                }
            }

            c = z_i - z_down;
            f = z_up - z_i;

            /*Get the values of these 8 voxels*/

            n0 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_up];
            n4 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_down];
            n5 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_up];
            n6 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_down];
            n7 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e * f) + (n1 * a * e * f) + (n2 * d * b * f) + (n3 * a * b * f) + (n4 * d * e * c) +
                   (n5 * a * e * c) + (n6 * d * b * c) + (n7 * a * b * c);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = (((e * f) * (n1 - n0)) + ((b * f) * (n3 - n2)) + ((c * e) * (n5 - n4)) + ((b * c) * (n7 - n6)));
            dypix4 = (((d * f) * (n2 - n0)) + ((a * f) * (n3 - n1)) + ((c * d) * (n6 - n4)) + ((a * c) * (n7 - n5)));
            dzpix4 = (((d * e) * (n4 - n0)) + ((a * e) * (n5 - n1)) + ((b * d) * (n6 - n2)) + ((a * b) * (n7 - n3)));

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * pix3) + (pix3 * pix3);

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 4; t++) {
                dcf[t] += 2.0 * (pix4 - pix3) * dxpix4 * dxyz[t];
                dcf[t + 4] += 2.0 * (pix4 - pix3) * dypix4 * dxyz[t];
                dcf[t + 8] += 2.0 * (pix4 - pix3) * dzpix4 * dxyz[t];
            }

            /* Second derivatives */
            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    dxdx[t][s] = dydx[t][s] = 2.0 * dxyz[t] * dxyz[s];
                }

                for (; s < 4; s++) {
                    dydx[t][s] = 2.0 * dxyz[t] * dxyz[s];
                }
            }

            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += dxdx[t][s] * dxpix4 * dxpix4;
                }
            }

            for (; t < 8; t++) {

                for (s = 0; s < 4; s++) {
                    ecf[t][s] += dydx[t - 4][s] * (dxpix4 * dypix4);
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 4][s - 4] * dypix4 * dypix4;
                }
            }

            for (; t < 12; t++) {

                for (s = 0; s < 4; s++) {
                    ecf[t][s] += dydx[t - 8][s] * (dxpix4 * dzpix4);
                }

                for (; s < 8; s++) {
                    ecf[t][s] += dydx[t - 8][s - 4] * (dypix4 * dzpix4);
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 8][s - 8] * dzpix4 * dzpix4;
                }
            }

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < 12; t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        /*Fill in redundant matrix elements*/
        for (t = 0; t < 12; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[2][0][t];
            dcff[t] += dcf[3] * des[3][0][t];
            dcff[t] += dcf[4] * des[0][1][t];
            dcff[t] += dcf[5] * des[1][1][t];
            dcff[t] += dcf[6] * des[2][1][t];
            dcff[t] += dcf[7] * des[3][1][t];
            dcff[t] += dcf[8] * des[0][2][t];
            dcff[t] += dcf[9] * des[1][2][t];
            dcff[t] += dcf[10] * des[2][2][t];
            dcff[t] += dcf[11] * des[3][2][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[2][0][s];
                temp += ecf[0][3] * des[3][0][s];
                temp += ecf[0][4] * des[0][1][s];
                temp += ecf[0][5] * des[1][1][s];
                temp += ecf[0][6] * des[2][1][s];
                temp += ecf[0][7] * des[3][1][s];
                temp += ecf[0][8] * des[0][2][s];
                temp += ecf[0][9] * des[1][2][s];
                temp += ecf[0][10] * des[2][2][s];
                temp += ecf[0][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[2][0][s];
                temp += ecf[1][3] * des[3][0][s];
                temp += ecf[1][4] * des[0][1][s];
                temp += ecf[1][5] * des[1][1][s];
                temp += ecf[1][6] * des[2][1][s];
                temp += ecf[1][7] * des[3][1][s];
                temp += ecf[1][8] * des[0][2][s];
                temp += ecf[1][9] * des[1][2][s];
                temp += ecf[1][10] * des[2][2][s];
                temp += ecf[1][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[2][0][s];
                temp += ecf[2][3] * des[3][0][s];
                temp += ecf[2][4] * des[0][1][s];
                temp += ecf[2][5] * des[1][1][s];
                temp += ecf[2][6] * des[2][1][s];
                temp += ecf[2][7] * des[3][1][s];
                temp += ecf[2][8] * des[0][2][s];
                temp += ecf[2][9] * des[1][2][s];
                temp += ecf[2][10] * des[2][2][s];
                temp += ecf[2][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][0][t]) + (dcf[2] * ees[2][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[2][0][s];
                temp += ecf[3][3] * des[3][0][s];
                temp += ecf[3][4] * des[0][1][s];
                temp += ecf[3][5] * des[1][1][s];
                temp += ecf[3][6] * des[2][1][s];
                temp += ecf[3][7] * des[3][1][s];
                temp += ecf[3][8] * des[0][2][s];
                temp += ecf[3][9] * des[1][2][s];
                temp += ecf[3][10] * des[2][2][s];
                temp += ecf[3][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[3] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[2][0][s];
                temp += ecf[4][3] * des[3][0][s];
                temp += ecf[4][4] * des[0][1][s];
                temp += ecf[4][5] * des[1][1][s];
                temp += ecf[4][6] * des[2][1][s];
                temp += ecf[4][7] * des[3][1][s];
                temp += ecf[4][8] * des[0][2][s];
                temp += ecf[4][9] * des[1][2][s];
                temp += ecf[4][10] * des[2][2][s];
                temp += ecf[4][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[4] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[2][0][s];
                temp += ecf[5][3] * des[3][0][s];
                temp += ecf[5][4] * des[0][1][s];
                temp += ecf[5][5] * des[1][1][s];
                temp += ecf[5][6] * des[2][1][s];
                temp += ecf[5][7] * des[3][1][s];
                temp += ecf[5][8] * des[0][2][s];
                temp += ecf[5][9] * des[1][2][s];
                temp += ecf[5][10] * des[2][2][s];
                temp += ecf[5][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[5] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[6][0] * des[0][0][s];
                temp += ecf[6][1] * des[1][0][s];
                temp += ecf[6][2] * des[2][0][s];
                temp += ecf[6][3] * des[3][0][s];
                temp += ecf[6][4] * des[0][1][s];
                temp += ecf[6][5] * des[1][1][s];
                temp += ecf[6][6] * des[2][1][s];
                temp += ecf[6][7] * des[3][1][s];
                temp += ecf[6][8] * des[0][2][s];
                temp += ecf[6][9] * des[1][2][s];
                temp += ecf[6][10] * des[2][2][s];
                temp += ecf[6][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][1][t]) + (dcf[6] * ees[2][1][t][s]);
                temp = 0;
                temp += ecf[7][0] * des[0][0][s];
                temp += ecf[7][1] * des[1][0][s];
                temp += ecf[7][2] * des[2][0][s];
                temp += ecf[7][3] * des[3][0][s];
                temp += ecf[7][4] * des[0][1][s];
                temp += ecf[7][5] * des[1][1][s];
                temp += ecf[7][6] * des[2][1][s];
                temp += ecf[7][7] * des[3][1][s];
                temp += ecf[7][8] * des[0][2][s];
                temp += ecf[7][9] * des[1][2][s];
                temp += ecf[7][10] * des[2][2][s];
                temp += ecf[7][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[7] * ees[3][1][t][s]);
                temp = 0;
                temp += ecf[8][0] * des[0][0][s];
                temp += ecf[8][1] * des[1][0][s];
                temp += ecf[8][2] * des[2][0][s];
                temp += ecf[8][3] * des[3][0][s];
                temp += ecf[8][4] * des[0][1][s];
                temp += ecf[8][5] * des[1][1][s];
                temp += ecf[8][6] * des[2][1][s];
                temp += ecf[8][7] * des[3][1][s];
                temp += ecf[8][8] * des[0][2][s];
                temp += ecf[8][9] * des[1][2][s];
                temp += ecf[8][10] * des[2][2][s];
                temp += ecf[8][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][2][t]) + (dcf[8] * ees[0][2][t][s]);
                temp = 0;
                temp += ecf[9][0] * des[0][0][s];
                temp += ecf[9][1] * des[1][0][s];
                temp += ecf[9][2] * des[2][0][s];
                temp += ecf[9][3] * des[3][0][s];
                temp += ecf[9][4] * des[0][1][s];
                temp += ecf[9][5] * des[1][1][s];
                temp += ecf[9][6] * des[2][1][s];
                temp += ecf[9][7] * des[3][1][s];
                temp += ecf[9][8] * des[0][2][s];
                temp += ecf[9][9] * des[1][2][s];
                temp += ecf[9][10] * des[2][2][s];
                temp += ecf[9][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][2][t]) + (dcf[9] * ees[1][2][t][s]);
                temp = 0;
                temp += ecf[10][0] * des[0][0][s];
                temp += ecf[10][1] * des[1][0][s];
                temp += ecf[10][2] * des[2][0][s];
                temp += ecf[10][3] * des[3][0][s];
                temp += ecf[10][4] * des[0][1][s];
                temp += ecf[10][5] * des[1][1][s];
                temp += ecf[10][6] * des[2][1][s];
                temp += ecf[10][7] * des[3][1][s];
                temp += ecf[10][8] * des[0][2][s];
                temp += ecf[10][9] * des[1][2][s];
                temp += ecf[10][10] * des[2][2][s];
                temp += ecf[10][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][2][t]) + (dcf[10] * ees[2][2][t][s]);
                temp = 0;
                temp += ecf[11][0] * des[0][0][s];
                temp += ecf[11][1] * des[1][0][s];
                temp += ecf[11][2] * des[2][0][s];
                temp += ecf[11][3] * des[3][0][s];
                temp += ecf[11][4] * des[0][1][s];
                temp += ecf[11][5] * des[1][1][s];
                temp += ecf[11][6] * des[2][1][s];
                temp += ecf[11][7] * des[3][1][s];
                temp += ecf[11][8] * des[0][2][s];
                temp += ecf[11][9] * des[1][2][s];
                temp += ecf[11][10] * des[2][2][s];
                temp += ecf[11][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][2][t]) + (dcf[11] * ees[3][2][t][s]);
            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            source parameters([4][3])
     * @param   des           derivatives of each source parameter with respect to each element
     * @param   ees           second derivatives as above
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2XDim    DOCUMENT ME!
     * @param   pixel2YDim    DOCUMENT ME!
     * @param   pixel5        interpolated values of target file
     * @param   pixel5Mask    mask of interpolated values of target file
     * @param   pixel5XDim    DOCUMENT ME!
     * @param   pixel5YDim    DOCUMENT ME!
     * @param   pixel5ZDim    DOCUMENT ME!
     * @param   threshold5    DOCUMENT ME!
     * @param   dcff          first derivatives of sd with respect to parameters
     * @param   ecff          second derivatives of sd with respect to parameters
     *
     *                        <p>This routine computes the first and second derivatives of the sum of squares with
     *                        respect to all external parameters.</p>
     *
     *                        <p>The matrix indices have been adjusted to allow for a more orderly relationship between
     *                        the matrix and its elements</p>
     *
     *                        <p>Returns the sum of squares</p>
     *
     * @return  DOCUMENT ME!
     */

    double qvderivsLS6(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                       float[] pixel2, int pixel2XDim, int pixel2YDim, float[] pixel5, boolean[] pixel5Mask,
                       int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5, double[] dcff,
                       double[][] ecff) {

        double cf = 0.0;
        double[] dcf = new double[6];
        double[][] ecf = new double[6][6];
        double[] dxy = new double[3];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double[][] dydx = new double[3][3];
        double[][] dxdx = new double[3][3];
        int counttotal;

        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double ratio; /*intermediate value*/

        int i, j, k;
        int s, t;
        int x_max1, y_max1;
        int x_dim1, y_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        double e00, e01, e10, e11, e30, e31;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4;
        double temp;

        /*Initialize values*/

        counttotal = 0;
        ratio = 0;

        for (t = 0; t < 6; t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e10 = es[1][0];
        e11 = es[1][1];
        e30 = es[3][0];
        e31 = es[3][1];

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            /* Note that dx[0], dx[1], and dx[2] are equal to dy[3], dy[4], and dy[5] respectively.
             * Consequently, these are all treated as dxy[0], dxy[1], and dxy[2] */

            dxy[0] = (double) i;
            dxy[1] = (double) j;
            dxy[2] = 1.0;

            /* Second derivatives are all zero */

            /*Get the coordinates of the 4 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            /*Get the values of these 4 voxels*/

            n0 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * pix3) + (pix3 * pix3);

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 3; t++) {
                dcf[t] += 2.0 * (pix4 - pix3) * dxpix4 * dxy[t];
                dcf[t + 3] += 2.0 * (pix4 - pix3) * dypix4 * dxy[t];
            }

            /* Second derivatives */
            for (t = 0; t < 3; t++) {

                for (s = 0; s <= t; s++) {
                    dxdx[t][s] = dydx[t][s] = 2.0 * dxy[t] * dxy[s];
                }

                for (; s < 3; s++) {
                    dydx[t][s] = 2.0 * dxy[t] * dxy[s];
                }
            }

            for (t = 0; t < 3; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += dxdx[t][s] * dxpix4 * dxpix4;
                }
            }

            for (; t < 6; t++) {

                for (s = 0; s < 3; s++) {
                    ecf[t][s] += dydx[t - 3][s] * (dxpix4 * dypix4);
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 3][s - 3] * dypix4 * dypix4;
                }
            }

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < 6; t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        /*Fill in redundant matrix elements*/
        for (t = 0; t < 6; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[3][0][t];
            dcff[t] += dcf[3] * des[0][1][t];
            dcff[t] += dcf[4] * des[1][1][t];
            dcff[t] += dcf[5] * des[3][1][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[3][0][s];
                temp += ecf[0][3] * des[0][1][s];
                temp += ecf[0][4] * des[1][1][s];
                temp += ecf[0][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[3][0][s];
                temp += ecf[1][3] * des[0][1][s];
                temp += ecf[1][4] * des[1][1][s];
                temp += ecf[1][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[3][0][s];
                temp += ecf[2][3] * des[0][1][s];
                temp += ecf[2][4] * des[1][1][s];
                temp += ecf[2][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[2] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[3][0][s];
                temp += ecf[3][3] * des[0][1][s];
                temp += ecf[3][4] * des[1][1][s];
                temp += ecf[3][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[3] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[3][0][s];
                temp += ecf[4][3] * des[0][1][s];
                temp += ecf[4][4] * des[1][1][s];
                temp += ecf[4][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[4] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[3][0][s];
                temp += ecf[5][3] * des[0][1][s];
                temp += ecf[5][4] * des[1][1][s];
                temp += ecf[5][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[5] * ees[3][1][t][s]);

            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              source parameters([4][3])
     * @param   des             derivatives of each source parameter with respect to each element
     * @param   ees             second derivatives as above
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2XDim      DOCUMENT ME!
     * @param   pixel2YDim      DOCUMENT ME!
     * @param   pixel2ZDim      DOCUMENT ME!
     * @param   pixel5          interpolated values of target file
     * @param   pixel5Mask      mask of interpolated values of target file
     * @param   pixel5XDim      DOCUMENT ME!
     * @param   pixel5YDim      DOCUMENT ME!
     * @param   pixel5ZDim      DOCUMENT ME!
     * @param   threshold5      DOCUMENT ME!
     * @param   dcff            first derivatives of sd with respect to parameters
     * @param   ecff            second derivatives of sd with respect to parameters
     * @param   count           number of pixels in partitions that are above threshold
     * @param   mean            ratio of pixel in source file/corresponding pixel in target file
     * @param   square          intermediate computation value
     * @param   dmean           partial derivatives of mean with respect to parameters
     * @param   dsquare         partial derivatives of square
     * @param   esquare         second partial derivatives of square
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  provides verification of compatibility of calling routine This routine computes the
     *                          first and second derivatives of the normalized standard deviation with respect to all
     *                          external parameters.
     *
     *                          <p>The matrix indices have been adjusted to allow for a more orderly relationship
     *                          between the matrix and its elements</p>
     *
     *                          <p>Returns the normalized standard deviation</p>
     *
     * @return  DOCUMENT ME!
     */

    double qvderivsN12(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                       float[] pixel2, int pixel2XDim, int pixel2YDim, int pixel2ZDim, float[] pixel5,
                       boolean[] pixel5Mask, int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5,
                       double[] dcff, double[][] ecff, int[] count, double[] mean, double[] square, double[][] dmean,
                       double[][] dsquare, double[][][] esquare, int partitions, double maxActualValue) {

        double cf = 0.0;
        double[] dcf = new double[12];
        double[][] ecf = new double[12][12];
        double[] dxyz = new double[4];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        int counttotal;
        double meantotal;

        int jj; /*index of current partition*/
        int pix1; /*int value of current pixel in standard file*/
        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double ratio; /*intermediate value*/
        double[] dratio = new double[12]; /*partial derivatives of ratio with respect to parameters*/
        double sd2;
        double[] dsd2 = new double[12];
        double[][] esd2 = new double[12][12];

        int i, j, k;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_dim1, y_dim1, z_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        double e00, e01, e02, e10, e11, e12, e20, e21, e22, e30, e31, e32;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4;
        int counts;
        double means;
        double sds;
        double[] dsds = new double[12];
        double[][] esds = new double[12][12];
        double temp;

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < 12; t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        z_dim1 = pixel2ZDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + (k * e20) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + (k * e21) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            z_i = (i * e02) + (j * e12) + (k * e22) + e32;

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

            /* Notice that dx[0], dx[1], dx[2], dx[3] are equal to dy[4], dy[5], dy[6], dy[7]
             * and to dz[8], dz[9], dz[10], dz[11].  Consequently, these are all treated as dxyz[0], dxyz[1], dxyz[2],
             * and dxyz[3]. */
            dxyz[0] = (double) i;
            dxyz[1] = (double) j;
            dxyz[2] = (double) k;
            dxyz[3] = 1.0;

            /*Get the coordinates of the 8 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);
            z_up = (int) Math.ceil(z_i);
            z_down = (int) Math.floor(z_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            if (z_up == z_down) {

                if (z_up < z_max1) {
                    z_up++;
                } else {
                    z_down--;
                }
            }

            c = z_i - z_down;
            f = z_up - z_i;

            /*Get the values of these 8 voxels*/

            n0 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_up];
            n4 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_down];
            n5 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_up];
            n6 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_down];
            n7 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - threshold5) / (maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e * f) + (n1 * a * e * f) + (n2 * d * b * f) + (n3 * a * b * f) + (n4 * d * e * c) +
                   (n5 * a * e * c) + (n6 * d * b * c) + (n7 * a * b * c);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = (((e * f) * (n1 - n0)) + ((b * f) * (n3 - n2)) + ((c * e) * (n5 - n4)) + ((b * c) * (n7 - n6)));
            dypix4 = (((d * f) * (n2 - n0)) + ((a * f) * (n3 - n1)) + ((c * d) * (n6 - n4)) + ((a * c) * (n7 - n5)));
            dzpix4 = (((d * e) * (n4 - n0)) + ((a * e) * (n5 - n1)) + ((b * d) * (n6 - n2)) + ((a * b) * (n7 - n3)));

            /* Calculate values needed to compute standard deviation */

            /*Calculate the ratio of the reslice value to the standard value*/
            ratio = pix4 / pix3;
            mean[pix1] += ratio;
            square[pix1] += ratio * ratio;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 4; t++) {
                dratio[t] = dxpix4 * dxyz[t] / pix3;
                dmean[pix1][t] += dratio[t];
                dsquare[pix1][t] += 2 * ratio * dratio[t];
                dratio[t + 4] = dypix4 * dxyz[t] / pix3;
                dmean[pix1][t + 4] += dratio[t + 4];
                dsquare[pix1][t + 4] += 2 * ratio * dratio[t + 4];
                dratio[t + 8] = dzpix4 * dxyz[t] / pix3;
                dmean[pix1][t + 8] += dratio[t + 8];
                dsquare[pix1][t + 8] += 2 * ratio * dratio[t + 8];
            }

            /* Second derivatives */
            for (t = 0; t < 12; t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dratio[s] * dratio[t]);
                }
            }
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        /*Initialize values*/
        meantotal = 0;
        counttotal = 0;
        cf = 0;

        for (t = 0; t < 12; t++) {
            dcf[t] = 0;

            for (s = 0; s < 12; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Calculate normalized standard deviation for each partition*/


        for (jj = 0; jj < partitions; jj++) {

            /*Avoid subsequent NaN errors*/
            if (count[jj] <= 1) {
                continue;
            }

            counts = count[jj];

            means = mean[jj] / counts;

            for (t = 0; t < 12; t++) {
                dmean[jj][t] /= counts;
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0.0) {
                continue;
            }

            for (t = 0; t < 12; t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] - (2 * counts * (dmean[jj][s] * dmean[jj][t]))) / (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < 12; t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < 12; t++) {

                for (s = 0; s <= t; s++) {

                    esd2[t][s] = (means * dsd2[s]) + (2 * sd2 * dmean[jj][s]);
                    esd2[t][s] /= (2 * means * means * means * sd2 * sd2);
                    esd2[t][s] *= (-dsds[t]);
                    esd2[t][s] += esds[t][s] / (2 * means * means * sd2);
                    esd2[t][s] -= (dsd2[s] * dmean[jj][t]) / means;
                    esd2[t][s] += sd2 * dmean[jj][t] * dmean[jj][s] / (means * means);
                }
            }

            /*Add weighted normalized standard deviation  of this partitions*/

            /* to cumulative total for all partitions*/

            cf += sd2 * counts;
            meantotal += means * counts;

            for (t = 0; t < 12; t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Renormalize cumulated values*/

        cf /= counttotal;
        meantotal = meantotal / counttotal;

        for (t = 0; t < 12; t++) {
            dcf[t] = dcf[t] / counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = ecf[t][s] / counttotal;
            }
        }

        /* Fill in redundant matrix elements*/
        for (t = 0; t < 12; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[2][0][t];
            dcff[t] += dcf[3] * des[3][0][t];
            dcff[t] += dcf[4] * des[0][1][t];
            dcff[t] += dcf[5] * des[1][1][t];
            dcff[t] += dcf[6] * des[2][1][t];
            dcff[t] += dcf[7] * des[3][1][t];
            dcff[t] += dcf[8] * des[0][2][t];
            dcff[t] += dcf[9] * des[1][2][t];
            dcff[t] += dcf[10] * des[2][2][t];
            dcff[t] += dcf[11] * des[3][2][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[2][0][s];
                temp += ecf[0][3] * des[3][0][s];
                temp += ecf[0][4] * des[0][1][s];
                temp += ecf[0][5] * des[1][1][s];
                temp += ecf[0][6] * des[2][1][s];
                temp += ecf[0][7] * des[3][1][s];
                temp += ecf[0][8] * des[0][2][s];
                temp += ecf[0][9] * des[1][2][s];
                temp += ecf[0][10] * des[2][2][s];
                temp += ecf[0][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[2][0][s];
                temp += ecf[1][3] * des[3][0][s];
                temp += ecf[1][4] * des[0][1][s];
                temp += ecf[1][5] * des[1][1][s];
                temp += ecf[1][6] * des[2][1][s];
                temp += ecf[1][7] * des[3][1][s];
                temp += ecf[1][8] * des[0][2][s];
                temp += ecf[1][9] * des[1][2][s];
                temp += ecf[1][10] * des[2][2][s];
                temp += ecf[1][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[2][0][s];
                temp += ecf[2][3] * des[3][0][s];
                temp += ecf[2][4] * des[0][1][s];
                temp += ecf[2][5] * des[1][1][s];
                temp += ecf[2][6] * des[2][1][s];
                temp += ecf[2][7] * des[3][1][s];
                temp += ecf[2][8] * des[0][2][s];
                temp += ecf[2][9] * des[1][2][s];
                temp += ecf[2][10] * des[2][2][s];
                temp += ecf[2][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][0][t]) + (dcf[2] * ees[2][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[2][0][s];
                temp += ecf[3][3] * des[3][0][s];
                temp += ecf[3][4] * des[0][1][s];
                temp += ecf[3][5] * des[1][1][s];
                temp += ecf[3][6] * des[2][1][s];
                temp += ecf[3][7] * des[3][1][s];
                temp += ecf[3][8] * des[0][2][s];
                temp += ecf[3][9] * des[1][2][s];
                temp += ecf[3][10] * des[2][2][s];
                temp += ecf[3][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[3] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[2][0][s];
                temp += ecf[4][3] * des[3][0][s];
                temp += ecf[4][4] * des[0][1][s];
                temp += ecf[4][5] * des[1][1][s];
                temp += ecf[4][6] * des[2][1][s];
                temp += ecf[4][7] * des[3][1][s];
                temp += ecf[4][8] * des[0][2][s];
                temp += ecf[4][9] * des[1][2][s];
                temp += ecf[4][10] * des[2][2][s];
                temp += ecf[4][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[4] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[2][0][s];
                temp += ecf[5][3] * des[3][0][s];
                temp += ecf[5][4] * des[0][1][s];
                temp += ecf[5][5] * des[1][1][s];
                temp += ecf[5][6] * des[2][1][s];
                temp += ecf[5][7] * des[3][1][s];
                temp += ecf[5][8] * des[0][2][s];
                temp += ecf[5][9] * des[1][2][s];
                temp += ecf[5][10] * des[2][2][s];
                temp += ecf[5][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[5] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[6][0] * des[0][0][s];
                temp += ecf[6][1] * des[1][0][s];
                temp += ecf[6][2] * des[2][0][s];
                temp += ecf[6][3] * des[3][0][s];
                temp += ecf[6][4] * des[0][1][s];
                temp += ecf[6][5] * des[1][1][s];
                temp += ecf[6][6] * des[2][1][s];
                temp += ecf[6][7] * des[3][1][s];
                temp += ecf[6][8] * des[0][2][s];
                temp += ecf[6][9] * des[1][2][s];
                temp += ecf[6][10] * des[2][2][s];
                temp += ecf[6][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][1][t]) + (dcf[6] * ees[2][1][t][s]);
                temp = 0;
                temp += ecf[7][0] * des[0][0][s];
                temp += ecf[7][1] * des[1][0][s];
                temp += ecf[7][2] * des[2][0][s];
                temp += ecf[7][3] * des[3][0][s];
                temp += ecf[7][4] * des[0][1][s];
                temp += ecf[7][5] * des[1][1][s];
                temp += ecf[7][6] * des[2][1][s];
                temp += ecf[7][7] * des[3][1][s];
                temp += ecf[7][8] * des[0][2][s];
                temp += ecf[7][9] * des[1][2][s];
                temp += ecf[7][10] * des[2][2][s];
                temp += ecf[7][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[7] * ees[3][1][t][s]);
                temp = 0;
                temp += ecf[8][0] * des[0][0][s];
                temp += ecf[8][1] * des[1][0][s];
                temp += ecf[8][2] * des[2][0][s];
                temp += ecf[8][3] * des[3][0][s];
                temp += ecf[8][4] * des[0][1][s];
                temp += ecf[8][5] * des[1][1][s];
                temp += ecf[8][6] * des[2][1][s];
                temp += ecf[8][7] * des[3][1][s];
                temp += ecf[8][8] * des[0][2][s];
                temp += ecf[8][9] * des[1][2][s];
                temp += ecf[8][10] * des[2][2][s];
                temp += ecf[8][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][2][t]) + (dcf[8] * ees[0][2][t][s]);
                temp = 0;
                temp += ecf[9][0] * des[0][0][s];
                temp += ecf[9][1] * des[1][0][s];
                temp += ecf[9][2] * des[2][0][s];
                temp += ecf[9][3] * des[3][0][s];
                temp += ecf[9][4] * des[0][1][s];
                temp += ecf[9][5] * des[1][1][s];
                temp += ecf[9][6] * des[2][1][s];
                temp += ecf[9][7] * des[3][1][s];
                temp += ecf[9][8] * des[0][2][s];
                temp += ecf[9][9] * des[1][2][s];
                temp += ecf[9][10] * des[2][2][s];
                temp += ecf[9][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][2][t]) + (dcf[9] * ees[1][2][t][s]);
                temp = 0;
                temp += ecf[10][0] * des[0][0][s];
                temp += ecf[10][1] * des[1][0][s];
                temp += ecf[10][2] * des[2][0][s];
                temp += ecf[10][3] * des[3][0][s];
                temp += ecf[10][4] * des[0][1][s];
                temp += ecf[10][5] * des[1][1][s];
                temp += ecf[10][6] * des[2][1][s];
                temp += ecf[10][7] * des[3][1][s];
                temp += ecf[10][8] * des[0][2][s];
                temp += ecf[10][9] * des[1][2][s];
                temp += ecf[10][10] * des[2][2][s];
                temp += ecf[10][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][2][t]) + (dcf[10] * ees[2][2][t][s]);
                temp = 0;
                temp += ecf[11][0] * des[0][0][s];
                temp += ecf[11][1] * des[1][0][s];
                temp += ecf[11][2] * des[2][0][s];
                temp += ecf[11][3] * des[3][0][s];
                temp += ecf[11][4] * des[0][1][s];
                temp += ecf[11][5] * des[1][1][s];
                temp += ecf[11][6] * des[2][1][s];
                temp += ecf[11][7] * des[3][1][s];
                temp += ecf[11][8] * des[0][2][s];
                temp += ecf[11][9] * des[1][2][s];
                temp += ecf[11][10] * des[2][2][s];
                temp += ecf[11][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][2][t]) + (dcf[11] * ees[3][2][t][s]);
            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              source parameters([4][3])
     * @param   des             derivatives of each source parameter with respect to each element
     * @param   ees             second derivatives as above
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2XDim      DOCUMENT ME!
     * @param   pixel2YDim      DOCUMENT ME!
     * @param   pixel5          interpolated values of target file
     * @param   pixel5Mask      mask of interpolated values of target file
     * @param   pixel5XDim      DOCUMENT ME!
     * @param   pixel5YDim      DOCUMENT ME!
     * @param   pixel5ZDim      DOCUMENT ME!
     * @param   threshold5      DOCUMENT ME!
     * @param   dcff            first derivatives of sd with respect to parameters
     * @param   ecff            second derivatives of sd with respect to parameters
     * @param   count           number of pixels in partitions that are above threshold
     * @param   mean            ratio of pixel in source file/corresponding pixel in target file
     * @param   square          intermediate computation value
     * @param   dmean           partial derivatives of mean with respect to parameters
     * @param   dsquare         partial derivatives of square
     * @param   esquare         second partial derivatives of square
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  This routine computes the first and second derivatives of the normalized standard
     *                          deviation with respect to all external parameters.
     *
     *                          <p>The matrix indices have been adjusted to allow for a more orderly relationship
     *                          between the matrix and its elements</p>
     *
     *                          <p>Returns the normalized standard deviation</p>
     *
     * @return  DOCUMENT ME!
     */

    double qvderivsN6(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                      float[] pixel2, int pixel2XDim, int pixel2YDim, float[] pixel5, boolean[] pixel5Mask,
                      int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5, double[] dcff, double[][] ecff,
                      int[] count, double[] mean, double[] square, double[][] dmean, double[][] dsquare,
                      double[][][] esquare, int partitions, double maxActualValue) {

        double cf = 0.0;
        double[] dcf = new double[6];
        double[][] ecf = new double[6][6];
        double[] dxy = new double[3];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        int counttotal;
        double meantotal;

        int jj; /*index of current partition*/
        int pix1; /*int value of current pixel in standard file*/
        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double ratio; /*intermediate value*/
        double[] dratio = new double[6]; /*partial derivatives of ratio with respect to parameters*/
        double sd2;
        double[] dsd2 = new double[6];
        double[][] esd2 = new double[6][6];

        int i, j, k;
        int s, t;
        int x_max1, y_max1;
        int x_dim1, y_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        double e00, e01, e10, e11, e30, e31;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4;
        int counts;
        double means;
        double sds;
        double[] dsds = new double[6];
        double[][] esds = new double[6][6];
        double temp;

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < 6; t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e10 = es[1][0];
        e11 = es[1][1];
        e30 = es[3][0];
        e31 = es[3][1];

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            /* Note that dx[0], dx[1], and dx[2] are equal to dy[3], dy[4], and dy[5] respectively.
             * Consequently, these are all treated as dxy[0], dxy[1], and dxy[2] */

            dxy[0] = (double) i;
            dxy[1] = (double) j;
            dxy[2] = 1.0;

            /* Second derivatives are all zero */

            /*Get the coordinates of the 4 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            /*Get the values of these 4 voxels*/

            n0 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - threshold5) / (maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            /* Calculate values needed to compute standard deviation */

            /*Calculate the ratio of the reslice value to the standard value*/
            ratio = pix4 / pix3;
            mean[pix1] += ratio;
            square[pix1] += ratio * ratio;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 3; t++) {
                dratio[t] = dxpix4 * dxy[t] / pix3;
                dmean[pix1][t] += dratio[t];
                dsquare[pix1][t] += 2 * ratio * dratio[t];
                dratio[t + 3] = dypix4 * dxy[t] / pix3;
                dmean[pix1][t + 3] += dratio[t + 3];
                dsquare[pix1][t + 3] += 2 * ratio * dratio[t + 3];
            }

            /* Second derivatives */
            for (t = 0; t < 6; t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dratio[s] * dratio[t]);
                }
            }
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        /*Initialize values*/
        meantotal = 0;
        counttotal = 0;
        cf = 0;

        for (t = 0; t < 6; t++) {
            dcf[t] = 0;

            for (s = 0; s < 6; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Calculate normalized standard deviation for each partition*/


        for (jj = 0; jj < partitions; jj++) {

            /*Avoid subsequent NaN errors*/
            if (count[jj] <= 1) {
                continue;
            }

            counts = count[jj];

            means = mean[jj] / counts;

            for (t = 0; t < 6; t++) {
                dmean[jj][t] /= counts;
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0.0) {
                continue;
            }

            for (t = 0; t < 6; t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] - (2 * counts * (dmean[jj][s] * dmean[jj][t]))) / (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < 6; t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < 6; t++) {

                for (s = 0; s <= t; s++) {

                    esd2[t][s] = (means * dsd2[s]) + (2 * sd2 * dmean[jj][s]);
                    esd2[t][s] /= (2 * means * means * means * sd2 * sd2);
                    esd2[t][s] *= (-dsds[t]);
                    esd2[t][s] += esds[t][s] / (2 * means * means * sd2);
                    esd2[t][s] -= (dsd2[s] * dmean[jj][t]) / means;
                    esd2[t][s] += sd2 * dmean[jj][t] * dmean[jj][s] / (means * means);
                }
            }

            /*Add weighted normalized standard deviation  of this partitions*/

            /* to cumulative total for all partitions*/

            cf += sd2 * counts;
            meantotal += means * counts;

            for (t = 0; t < 6; t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Renormalize cumulated values*/

        cf /= counttotal;
        meantotal = meantotal / counttotal;

        for (t = 0; t < 6; t++) {
            dcf[t] = dcf[t] / counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = ecf[t][s] / counttotal;
            }
        }

        /* Fill in redundant matrix elements*/
        for (t = 0; t < 6; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[3][0][t];
            dcff[t] += dcf[3] * des[0][1][t];
            dcff[t] += dcf[4] * des[1][1][t];
            dcff[t] += dcf[5] * des[3][1][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[3][0][s];
                temp += ecf[0][3] * des[0][1][s];
                temp += ecf[0][4] * des[1][1][s];
                temp += ecf[0][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[3][0][s];
                temp += ecf[1][3] * des[0][1][s];
                temp += ecf[1][4] * des[1][1][s];
                temp += ecf[1][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[3][0][s];
                temp += ecf[2][3] * des[0][1][s];
                temp += ecf[2][4] * des[1][1][s];
                temp += ecf[2][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[2] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[3][0][s];
                temp += ecf[3][3] * des[0][1][s];
                temp += ecf[3][4] * des[1][1][s];
                temp += ecf[3][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[3] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[3][0][s];
                temp += ecf[4][3] * des[0][1][s];
                temp += ecf[4][4] * des[1][1][s];
                temp += ecf[4][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[4] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[3][0][s];
                temp += ecf[5][3] * des[0][1][s];
                temp += ecf[5][4] * des[1][1][s];
                temp += ecf[5][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[5] * ees[3][1][t][s]);

            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            source parameters([4][3])
     * @param   des           derivatives of each source parameter with respect to each element
     * @param   ees           second derivatives as above
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2XDim    DOCUMENT ME!
     * @param   pixel2YDim    DOCUMENT ME!
     * @param   pixel2ZDim    DOCUMENT ME!
     * @param   pixel5        interpolated values of target file
     * @param   pixel5Mask    mask of interpolated values of target file
     * @param   pixel5XDim    DOCUMENT ME!
     * @param   pixel5YDim    DOCUMENT ME!
     * @param   pixel5ZDim    DOCUMENT ME!
     * @param   threshold5    DOCUMENT ME!
     * @param   dcff          first derivatives of sd with respect to parameters
     * @param   ecff          second derivatives of sd with respect to parameters
     * @param   scale         DOCUMENT ME!
     * @param   forward       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */

    double qvderivsRS12(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                        float[] pixel2, int pixel2XDim, int pixel2YDim, int pixel2ZDim, float[] pixel5,
                        boolean[] pixel5Mask, int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5,
                        double[] dcff, double[][] ecff, double scale, boolean forward) {

        long counttotal = 0;
        double cf = 0.0;
        double[] dcf = new double[13];
        double[][] ecf = new double[13][13];
        double[] dxyz = new double[4];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double[][] dydx = new double[4][4];
        double[][] dxdx = new double[4][4];
        double dscale;
        double escale;

        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double spix3;

        double e00, e01, e02, e10, e11, e12, e20, e21, e22, e30, e31, e32;

        int i, j, k;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_dim1, y_dim1, z_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4;
        double temp;

        /*Initialize values*/

        if (!forward) {
            scale = 1.0 / scale;
        }

        for (t = 0; t < 13; t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        z_dim1 = pixel2ZDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + (k * e20) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + (k * e21) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            z_i = (i * e02) + (j * e12) + (k * e22) + e32;

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

            /* Notice that dx[0], dx[1], dx[2], dx[3] are equal to dy[4], dy[5], dy[6], dy[7]
             * and to dz[8], dz[9], dz[10], dz[11].  Consequently, these are all treated as dxyz[0], dxyz[1], dxyz[2],
             * and dxyz[3]. */
            dxyz[0] = (double) i;
            dxyz[1] = (double) j;
            dxyz[2] = (double) k;
            dxyz[3] = 1.0;

            /*Get the coordinates of the 8 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);
            z_up = (int) Math.ceil(z_i);
            z_down = (int) Math.floor(z_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            if (z_up == z_down) {

                if (z_up < z_max1) {
                    z_up++;
                } else {
                    z_down--;
                }
            }

            c = z_i - z_down;
            f = z_up - z_i;

            /*Get the values of these 8 voxels*/

            n0 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_up];
            n4 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_down];
            n5 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_up];
            n6 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_down];
            n7 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e * f) + (n1 * a * e * f) + (n2 * d * b * f) + (n3 * a * b * f) + (n4 * d * e * c) +
                   (n5 * a * e * c) + (n6 * d * b * c) + (n7 * a * b * c);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = (((e * f) * (n1 - n0)) + ((b * f) * (n3 - n2)) + ((c * e) * (n5 - n4)) + ((b * c) * (n7 - n6)));
            dypix4 = (((d * f) * (n2 - n0)) + ((a * f) * (n3 - n1)) + ((c * d) * (n6 - n4)) + ((a * c) * (n7 - n5)));
            dzpix4 = (((d * e) * (n4 - n0)) + ((a * e) * (n5 - n1)) + ((b * d) * (n6 - n2)) + ((a * b) * (n7 - n3)));

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * spix3) + (spix3 * spix3);

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 4; t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dxpix4 * dxyz[t];
                dcf[t + 4] += 2.0 * (pix4 - spix3) * dypix4 * dxyz[t];
                dcf[t + 8] += 2.0 * (pix4 - spix3) * dzpix4 * dxyz[t];
            }

            dcf[12] += 2.0 * (pix4 - spix3) * (-pix3);

            /* Second derivatives */
            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    dxdx[t][s] = dydx[t][s] = 2.0 * dxyz[t] * dxyz[s];
                }

                for (; s < 4; s++) {
                    dydx[t][s] = 2.0 * dxyz[t] * dxyz[s];
                }
            }

            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += dxdx[t][s] * dxpix4 * dxpix4;
                }
            }

            for (; t < 8; t++) {

                for (s = 0; s < 4; s++) {
                    ecf[t][s] += dydx[t - 4][s] * (dxpix4 * dypix4);
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 4][s - 4] * dypix4 * dypix4;
                }
            }

            for (; t < 12; t++) {

                for (s = 0; s < 4; s++) {
                    ecf[t][s] += dydx[t - 8][s] * (dxpix4 * dzpix4);
                }

                for (; s < 8; s++) {
                    ecf[t][s] += dydx[t - 8][s - 4] * (dypix4 * dzpix4);
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 8][s - 8] * dzpix4 * dzpix4;
                }
            }

            for (s = 0; s < 4; s++) {
                ecf[12][s] += -2 * pix3 * dxpix4 * dxyz[s];
            }

            for (; s < 8; s++) {
                ecf[12][s] += -2 * pix3 * dypix4 * dxyz[s - 4];
            }

            for (; s < 12; s++) {
                ecf[12][s] += -2 * pix3 * dzpix4 * dxyz[s - 8];
            }

            ecf[12][12] += 2 * pix3 * pix3;

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < 13; t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        /*Fill in redundant matrix elements*/
        for (t = 0; t < 13; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        if (forward) {
            dscale = 1;
            escale = 0;
        } else {
            dscale = -(scale * scale);
            escale = 2.0 * (scale * scale * scale);
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[2][0][t];
            dcff[t] += dcf[3] * des[3][0][t];
            dcff[t] += dcf[4] * des[0][1][t];
            dcff[t] += dcf[5] * des[1][1][t];
            dcff[t] += dcf[6] * des[2][1][t];
            dcff[t] += dcf[7] * des[3][1][t];
            dcff[t] += dcf[8] * des[0][2][t];
            dcff[t] += dcf[9] * des[1][2][t];
            dcff[t] += dcf[10] * des[2][2][t];
            dcff[t] += dcf[11] * des[3][2][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[2][0][s];
                temp += ecf[0][3] * des[3][0][s];
                temp += ecf[0][4] * des[0][1][s];
                temp += ecf[0][5] * des[1][1][s];
                temp += ecf[0][6] * des[2][1][s];
                temp += ecf[0][7] * des[3][1][s];
                temp += ecf[0][8] * des[0][2][s];
                temp += ecf[0][9] * des[1][2][s];
                temp += ecf[0][10] * des[2][2][s];
                temp += ecf[0][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[2][0][s];
                temp += ecf[1][3] * des[3][0][s];
                temp += ecf[1][4] * des[0][1][s];
                temp += ecf[1][5] * des[1][1][s];
                temp += ecf[1][6] * des[2][1][s];
                temp += ecf[1][7] * des[3][1][s];
                temp += ecf[1][8] * des[0][2][s];
                temp += ecf[1][9] * des[1][2][s];
                temp += ecf[1][10] * des[2][2][s];
                temp += ecf[1][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[2][0][s];
                temp += ecf[2][3] * des[3][0][s];
                temp += ecf[2][4] * des[0][1][s];
                temp += ecf[2][5] * des[1][1][s];
                temp += ecf[2][6] * des[2][1][s];
                temp += ecf[2][7] * des[3][1][s];
                temp += ecf[2][8] * des[0][2][s];
                temp += ecf[2][9] * des[1][2][s];
                temp += ecf[2][10] * des[2][2][s];
                temp += ecf[2][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][0][t]) + (dcf[2] * ees[2][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[2][0][s];
                temp += ecf[3][3] * des[3][0][s];
                temp += ecf[3][4] * des[0][1][s];
                temp += ecf[3][5] * des[1][1][s];
                temp += ecf[3][6] * des[2][1][s];
                temp += ecf[3][7] * des[3][1][s];
                temp += ecf[3][8] * des[0][2][s];
                temp += ecf[3][9] * des[1][2][s];
                temp += ecf[3][10] * des[2][2][s];
                temp += ecf[3][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[3] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[2][0][s];
                temp += ecf[4][3] * des[3][0][s];
                temp += ecf[4][4] * des[0][1][s];
                temp += ecf[4][5] * des[1][1][s];
                temp += ecf[4][6] * des[2][1][s];
                temp += ecf[4][7] * des[3][1][s];
                temp += ecf[4][8] * des[0][2][s];
                temp += ecf[4][9] * des[1][2][s];
                temp += ecf[4][10] * des[2][2][s];
                temp += ecf[4][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[4] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[2][0][s];
                temp += ecf[5][3] * des[3][0][s];
                temp += ecf[5][4] * des[0][1][s];
                temp += ecf[5][5] * des[1][1][s];
                temp += ecf[5][6] * des[2][1][s];
                temp += ecf[5][7] * des[3][1][s];
                temp += ecf[5][8] * des[0][2][s];
                temp += ecf[5][9] * des[1][2][s];
                temp += ecf[5][10] * des[2][2][s];
                temp += ecf[5][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[5] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[6][0] * des[0][0][s];
                temp += ecf[6][1] * des[1][0][s];
                temp += ecf[6][2] * des[2][0][s];
                temp += ecf[6][3] * des[3][0][s];
                temp += ecf[6][4] * des[0][1][s];
                temp += ecf[6][5] * des[1][1][s];
                temp += ecf[6][6] * des[2][1][s];
                temp += ecf[6][7] * des[3][1][s];
                temp += ecf[6][8] * des[0][2][s];
                temp += ecf[6][9] * des[1][2][s];
                temp += ecf[6][10] * des[2][2][s];
                temp += ecf[6][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][1][t]) + (dcf[6] * ees[2][1][t][s]);
                temp = 0;
                temp += ecf[7][0] * des[0][0][s];
                temp += ecf[7][1] * des[1][0][s];
                temp += ecf[7][2] * des[2][0][s];
                temp += ecf[7][3] * des[3][0][s];
                temp += ecf[7][4] * des[0][1][s];
                temp += ecf[7][5] * des[1][1][s];
                temp += ecf[7][6] * des[2][1][s];
                temp += ecf[7][7] * des[3][1][s];
                temp += ecf[7][8] * des[0][2][s];
                temp += ecf[7][9] * des[1][2][s];
                temp += ecf[7][10] * des[2][2][s];
                temp += ecf[7][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[7] * ees[3][1][t][s]);
                temp = 0;
                temp += ecf[8][0] * des[0][0][s];
                temp += ecf[8][1] * des[1][0][s];
                temp += ecf[8][2] * des[2][0][s];
                temp += ecf[8][3] * des[3][0][s];
                temp += ecf[8][4] * des[0][1][s];
                temp += ecf[8][5] * des[1][1][s];
                temp += ecf[8][6] * des[2][1][s];
                temp += ecf[8][7] * des[3][1][s];
                temp += ecf[8][8] * des[0][2][s];
                temp += ecf[8][9] * des[1][2][s];
                temp += ecf[8][10] * des[2][2][s];
                temp += ecf[8][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][2][t]) + (dcf[8] * ees[0][2][t][s]);
                temp = 0;
                temp += ecf[9][0] * des[0][0][s];
                temp += ecf[9][1] * des[1][0][s];
                temp += ecf[9][2] * des[2][0][s];
                temp += ecf[9][3] * des[3][0][s];
                temp += ecf[9][4] * des[0][1][s];
                temp += ecf[9][5] * des[1][1][s];
                temp += ecf[9][6] * des[2][1][s];
                temp += ecf[9][7] * des[3][1][s];
                temp += ecf[9][8] * des[0][2][s];
                temp += ecf[9][9] * des[1][2][s];
                temp += ecf[9][10] * des[2][2][s];
                temp += ecf[9][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][2][t]) + (dcf[9] * ees[1][2][t][s]);
                temp = 0;
                temp += ecf[10][0] * des[0][0][s];
                temp += ecf[10][1] * des[1][0][s];
                temp += ecf[10][2] * des[2][0][s];
                temp += ecf[10][3] * des[3][0][s];
                temp += ecf[10][4] * des[0][1][s];
                temp += ecf[10][5] * des[1][1][s];
                temp += ecf[10][6] * des[2][1][s];
                temp += ecf[10][7] * des[3][1][s];
                temp += ecf[10][8] * des[0][2][s];
                temp += ecf[10][9] * des[1][2][s];
                temp += ecf[10][10] * des[2][2][s];
                temp += ecf[10][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][2][t]) + (dcf[10] * ees[2][2][t][s]);
                temp = 0;
                temp += ecf[11][0] * des[0][0][s];
                temp += ecf[11][1] * des[1][0][s];
                temp += ecf[11][2] * des[2][0][s];
                temp += ecf[11][3] * des[3][0][s];
                temp += ecf[11][4] * des[0][1][s];
                temp += ecf[11][5] * des[1][1][s];
                temp += ecf[11][6] * des[2][1][s];
                temp += ecf[11][7] * des[3][1][s];
                temp += ecf[11][8] * des[0][2][s];
                temp += ecf[11][9] * des[1][2][s];
                temp += ecf[11][10] * des[2][2][s];
                temp += ecf[11][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][2][t]) + (dcf[11] * ees[3][2][t][s]);
            }
        }

        dcff[parameters] = dcf[12] * dscale;

        for (s = 0; s < parameters; s++) {
            temp = 0;
            temp += ecf[12][0] * des[0][0][s];
            temp += ecf[12][1] * des[1][0][s];
            temp += ecf[12][2] * des[2][0][s];
            temp += ecf[12][3] * des[3][0][s];
            temp += ecf[12][4] * des[0][1][s];
            temp += ecf[12][5] * des[1][1][s];
            temp += ecf[12][6] * des[2][1][s];
            temp += ecf[12][7] * des[3][1][s];
            temp += ecf[12][8] * des[0][2][s];
            temp += ecf[12][9] * des[1][2][s];
            temp += ecf[12][10] * des[2][2][s];
            temp += ecf[12][11] * des[3][2][s];
            ecff[parameters][s] = temp * dscale;
        }

        ecff[parameters][parameters] = (dscale * dscale * ecf[12][12]) + (dcf[12] * escale);

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            source parameters([4][3])
     * @param   des           derivatives of each source parameter with respect to each element
     * @param   ees           second derivatives as above
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2XDim    DOCUMENT ME!
     * @param   pixel2YDim    DOCUMENT ME!
     * @param   pixel5        interpolated values of target file
     * @param   pixel5Mask    mask of interpolated values of target file
     * @param   pixel5XDim    DOCUMENT ME!
     * @param   pixel5YDim    DOCUMENT ME!
     * @param   pixel5ZDim    DOCUMENT ME!
     * @param   threshold5    DOCUMENT ME!
     * @param   dcff          first derivatives of sd with respect to parameters
     * @param   ecff          second derivatives of sd with respect to parameters
     * @param   scale         DOCUMENT ME!
     * @param   forward
     *                        <p>This routine computes the first and second derivatives of the sum of squares with
     *                        respect to all external parameters.</p>
     *
     *                        <p>A rescaling parameter is included to allow for data where intensities require
     *                        adjustment</p>
     *
     *                        <p>The matrix indices have been adjusted to allow for a more orderly relationship between
     *                        the matrix and its elements</p>
     *
     *                        <p>Returns the sum of squares</p>
     *
     * @return  DOCUMENT ME!
     */

    double qvderivsRS6(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                       float[] pixel2, int pixel2XDim, int pixel2YDim, float[] pixel5, boolean[] pixel5Mask,
                       int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5, double[] dcff, double[][] ecff,
                       double scale, boolean forward) {

        long counttotal = 0;
        double cf = 0.0;
        double[] dcf = new double[7];
        double[][] ecf = new double[7][7];
        double[] dxy = new double[3];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double[][] dydx = new double[3][3];
        double[][] dxdx = new double[3][3];
        double dscale;
        double escale;

        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double spix3;

        double e00, e01, e10, e11, e30, e31;

        int i, j, k;
        int s, t;
        int x_max1, y_max1;
        int x_dim1, y_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4;
        double temp;

        /*Initialize values*/

        if (!forward) {
            scale = 1.0 / scale;
        }

        for (t = 0; t < 7; t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        e00 = es[0][0];
        e01 = es[0][1];
        e10 = es[1][0];
        e11 = es[1][1];
        e30 = es[3][0];
        e31 = es[3][1];

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            /* Note that dx[0], dx[1], and dx[2] are equal to dy[3], dy[4], and dy[5] respectively.
             * Consequently, these are all treated as dxy[0], dxy[1], and dxy[2] */

            dxy[0] = (double) i;
            dxy[1] = (double) j;
            dxy[2] = 1.0;

            /* Second derivatives are all zero */

            /*Get the coordinates of the 4 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            /*Get the values of these 4 voxels*/

            n0 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * spix3) + (spix3 * spix3);

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 3; t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dxpix4 * dxy[t];
                dcf[t + 3] += 2.0 * (pix4 - spix3) * dypix4 * dxy[t];
            }

            dcf[6] += 2.0 * (pix4 - spix3) * (-pix3);

            /* Second derivatives */
            for (t = 0; t < 3; t++) {

                for (s = 0; s <= t; s++) {
                    dxdx[t][s] = dydx[t][s] = 2.0 * dxy[t] * dxy[s];
                }

                for (; s < 3; s++) {
                    dydx[t][s] = 2.0 * dxy[t] * dxy[s];
                }
            }

            for (t = 0; t < 3; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += dxdx[t][s] * dxpix4 * dxpix4;
                }
            }

            for (; t < 6; t++) {

                for (s = 0; s < 3; s++) {
                    ecf[t][s] += dydx[t - 3][s] * (dxpix4 * dypix4);
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 3][s - 3] * dypix4 * dypix4;
                }
            }

            for (s = 0; s < 3; s++) {
                ecf[6][s] += -2 * pix3 * dxpix4 * dxy[s];
            }

            for (; s < 6; s++) {
                ecf[6][s] += -2 * pix3 * dypix4 * dxy[s - 3];
            }

            ecf[6][6] += 2 * pix3 * pix3;

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < 7; t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        /*Fill in redundant matrix elements*/
        for (t = 0; t < 7; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        if (forward) {
            dscale = 1;
            escale = 0;
        } else {
            dscale = -(scale * scale);
            escale = 2.0 * (scale * scale * scale);
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[3][0][t];
            dcff[t] += dcf[3] * des[0][1][t];
            dcff[t] += dcf[4] * des[1][1][t];
            dcff[t] += dcf[5] * des[3][1][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[3][0][s];
                temp += ecf[0][3] * des[0][1][s];
                temp += ecf[0][4] * des[1][1][s];
                temp += ecf[0][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[3][0][s];
                temp += ecf[1][3] * des[0][1][s];
                temp += ecf[1][4] * des[1][1][s];
                temp += ecf[1][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[3][0][s];
                temp += ecf[2][3] * des[0][1][s];
                temp += ecf[2][4] * des[1][1][s];
                temp += ecf[2][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[2] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[3][0][s];
                temp += ecf[3][3] * des[0][1][s];
                temp += ecf[3][4] * des[1][1][s];
                temp += ecf[3][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[3] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[3][0][s];
                temp += ecf[4][3] * des[0][1][s];
                temp += ecf[4][4] * des[1][1][s];
                temp += ecf[4][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[4] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[3][0][s];
                temp += ecf[5][3] * des[0][1][s];
                temp += ecf[5][4] * des[1][1][s];
                temp += ecf[5][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[5] * ees[3][1][t][s]);
            }
        }

        dcff[parameters] = dcf[6] * dscale;

        for (s = 0; s < parameters; s++) {
            temp = 0;
            temp += ecf[6][0] * des[0][0][s];
            temp += ecf[6][1] * des[1][0][s];
            temp += ecf[6][2] * des[3][0][s];
            temp += ecf[6][3] * des[0][1][s];
            temp += ecf[6][4] * des[1][1][s];
            temp += ecf[6][5] * des[3][1][s];
            ecff[parameters][s] = temp * dscale;
        }

        ecff[parameters][parameters] = (dscale * dscale * ecf[6][6]) + (dcf[6] * escale);

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   volume  source array to be resliced
     * @param   x_dim1  x dimension of source array
     * @param   y_dim1  y dimension of source array
     * @param   z_dim1  z dimension of source array
     * @param   x_dim2  x dimension of output array
     * @param   y_dim2  y dimension of output array
     * @param   z_dim2  z dimension of output array
     * @param   es      rescaling parameters
     *
     * @return  resliced data array if successful, null if unsuccessful
     *
     *          <p>This routine will reslice a file based on the unit vectors es and achieves greater efficiency than
     *          reslicer15() based on not using perspective transformations. Trilinear interpolation is used.</p>
     */

    float[] reslicer(float[] volume, int x_dim1, int y_dim1, int z_dim1, int x_dim2, int y_dim2, int z_dim2,
                     double[][] es) {

        int i, j, k;
        int i2, j2, k2;
        int i3, j3, k3;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;
        double a, b, c, d, e, f;
        double total;
        double x_i, x_j, x_k;
        double y_i, y_j, y_k;
        double z_i, z_j, z_k;
        double e00, e01, e02, e10, e11, e12, e20, e21, e22, e30, e31, e32;
        float[] new_volume;
        int slice1;
        int vol1;
        int slice2;

        slice1 = x_dim1 * y_dim1;
        vol1 = z_dim1 * slice1;
        slice2 = x_dim2 * y_dim2;

        if ((es[0][3] != 0) || (es[1][3] != 0) || (es[2][3] != 0) || (es[3][3] != 1)) {
            MipavUtil.displayError("Routine reslicer() inappropriately called to perform perspective transformation\n");

            return null;
        }

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];

        try {
            new_volume = new float[x_dim2 * y_dim2 * z_dim2];
        } catch (OutOfMemoryError er) {
            MipavUtil.displayError("unable to allocate memory to reslice file\n");

            return null;
        }

        for (i = 0; i < new_volume.length; i++) {
            new_volume[i] = 0.0f;
        }

        for (k = 0, k3 = 0, x_k = e30, y_k = e31, z_k = e32; k < z_dim2;
                 k++, k3 += slice2, x_k += e20, y_k += e21, z_k += e22) {

            for (j = 0, j3 = k3, x_j = x_k, y_j = y_k, z_j = z_k; j < y_dim2;
                     j++, j3 += x_dim2, x_j += e10, y_j += e11, z_j += e12) {

                for (i = 0, i3 = j3, x_i = x_j, y_i = y_j, z_i = z_j; i < x_dim2;
                         i++, i3++, x_i += e00, y_i += e01, z_i += e02) {

                    if ((x_i >= -0.5) && (x_i <= (x_dim1 - 0.5))) {

                        if ((y_i >= -0.5) && (y_i <= (y_dim1 - 0.5))) {

                            if ((z_i >= -0.5) && (z_i <= (z_dim1 - 0.5))) {
                                x_up = (int) Math.ceil(x_i);
                                x_down = (int) Math.floor(x_i);
                                y_up = (int) Math.ceil(y_i);
                                y_down = (int) Math.floor(y_i);
                                z_up = (int) Math.ceil(z_i);
                                z_down = (int) Math.floor(z_i);

                                if ((x_up == 0) && (x_down == -1)) {
                                    a = 1;
                                    d = 0;
                                } else if ((x_up == x_dim1) && (x_down == (x_dim1 - 1))) {
                                    a = 0;
                                    d = 1;
                                } else if (x_up == x_down) {
                                    a = 0;
                                    d = 1;
                                } else {
                                    a = x_i - x_down;
                                    d = x_up - x_i;
                                }

                                if ((y_up == 0) && (y_down == -1)) {
                                    b = 1;
                                    e = 0;
                                } else if ((y_up == y_dim1) && (y_down == (y_dim1 - 1))) {
                                    b = 0;
                                    e = 1;
                                } else if (y_up == y_down) {
                                    b = 0;
                                    e = 1;
                                } else {
                                    b = y_i - y_down;
                                    e = y_up - y_i;
                                }

                                if ((z_up == 0) && (z_down == -1)) {
                                    c = 1;
                                    f = 0;
                                } else if ((z_up == z_dim1) && (z_down == (z_dim1 - 1))) {
                                    c = 0;
                                    f = 1;
                                } else if (z_up == z_down) {
                                    c = 0;
                                    f = 1;
                                } else {
                                    c = z_i - z_down;
                                    f = z_up - z_i;
                                }

                                k2 = z_down * slice1;
                                j2 = k2 + (y_down * x_dim1);
                                i2 = j2 + x_down;
                                total = 0;

                                if ((i2 >= 0) && (i2 < vol1)) {
                                    total = volume[i2] * d * e * f;
                                }

                                if (x_up != x_down) {
                                    i2++;

                                    if ((i2 >= 0) && (i2 < vol1)) {
                                        total += volume[i2] * a * e * f;
                                    }
                                }

                                if (y_up != y_down) {
                                    j2 += x_dim1;
                                    i2 = j2 + x_down;

                                    if ((i2 >= 0) && (i2 < vol1)) {
                                        total += volume[i2] * d * b * f;
                                    }

                                    if (x_up != x_down) {
                                        i2++;

                                        if ((i2 >= 0) && (i2 < vol1)) {
                                            total += volume[i2] * a * b * f;
                                        }
                                    }
                                }

                                if (z_up != z_down) {
                                    k2 += slice1;
                                    j2 = k2 + (y_down * x_dim1);
                                    i2 = j2 + x_down;

                                    if ((i2 >= 0) && (i2 < vol1)) {
                                        total += volume[i2] * d * e * c;
                                    }

                                    if (x_up != x_down) {
                                        i2++;

                                        if ((i2 >= 0) && (i2 < vol1)) {
                                            total += volume[i2] * a * e * c;
                                        }
                                    }

                                    if (y_up != y_down) {
                                        j2 += x_dim1;
                                        i2 = j2 + x_down;

                                        if ((i2 >= 0) && (i2 < vol1)) {
                                            total += volume[i2] * d * b * c;
                                        }

                                        if (x_up != x_down) {
                                            i2++;

                                            if ((i2 >= 0) && (i2 < vol1)) {
                                                total += volume[i2] * a * b * c;
                                            }
                                        }
                                    }
                                }

                                new_volume[i3] = (float) total;
                            }
                        }
                    }
                }
            }
        }

        return new_volume;
    }

    /* Copyright 1995 Roger P. Woods, M.D. */

    /* Modified 12/12/95 */

    /**
     * DOCUMENT ME!
     *
     * @param  e                   DOCUMENT ME!
     * @param  de                  DOCUMENT ME!
     * @param  ee                  DOCUMENT ME!
     * @param  tps                 DOCUMENT ME!
     * @param  xoom1               DOCUMENT ME!
     * @param  xoom2               DOCUMENT ME!
     * @param  yoom1               DOCUMENT ME!
     * @param  yoom2               DOCUMENT ME!
     * @param  zoom1               DOCUMENT ME!
     * @param  zoom2               DOCUMENT ME!
     * @param  pixel_size1         DOCUMENT ME!
     * @param  pixel_size2         DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  z_dim1              DOCUMENT ME!
     * @param  z_dim2              DOCUMENT ME!
     * @param  cubicInterpolation  DOCUMENT ME!
     */
    void rigid_rotatef(double[] e, double[][] de, double[][][] ee, double[] tps, float xoom1, float xoom2, float yoom1,
                       float yoom2, float zoom1, float zoom2, double pixel_size1, double pixel_size2, int x_dim1,
                       int x_dim2, int y_dim1, int y_dim2, int z_dim1, int z_dim2, boolean cubicInterpolation) {

        double[] x = new double[5];
        double[] y = new double[5];
        double[] z = new double[5];
        double[][] dx = new double[5][6];
        double[][] dy = new double[5][6];
        double[][] dz = new double[5][6];
        double[][][] ex = new double[5][6][6];
        double[][][] ey = new double[5][6][6];
        double[][][] ez = new double[5][6][6];

        double pitch, roll, yaw, p, q, r;
        double sinyaw, cosyaw, sinpitch, cospitch, sinroll, cosroll;

        pitch = tps[0];
        roll = tps[1];
        yaw = tps[2];
        p = tps[3];
        q = tps[4];
        r = tps[5];

        sinyaw = Math.sin(yaw);
        cosyaw = Math.cos(yaw);
        sinpitch = Math.sin(pitch);
        cospitch = Math.cos(pitch);
        sinroll = Math.sin(roll);
        cosroll = Math.cos(roll);

        if (!cubicInterpolation) {
            x[0] = ((2 * e[0]) + 1 - x_dim1) * xoom1 * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + 1 - y_dim1) * yoom1 * (pixel_size1 / pixel_size2);
            z[0] = ((2 * e[2]) + 1 - z_dim1) * zoom1 * (pixel_size1 / pixel_size2);
        } else {
            x[0] = ((2 * e[0]) + ((1 - x_dim1) * xoom1)) * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + ((1 - y_dim1) * yoom1)) * (pixel_size1 / pixel_size2);
            z[0] = ((2 * e[2]) + ((1 - z_dim1) * zoom1)) * (pixel_size1 / pixel_size2);
        }

        /*This model doesn't apply scaling*/

        x[1] = x[0];
        y[1] = y[0];
        z[1] = z[0];

        /*Apply yaw*/

        x[2] = (x[1] * cosyaw) + (y[1] * sinyaw);
        dx[2][2] = (-x[1] * sinyaw) + (y[1] * cosyaw);
        ex[2][2][2] = (-x[1] * cosyaw) - (y[1] * sinyaw);

        y[2] = (y[1] * cosyaw) - (x[1] * sinyaw);

        dy[2][2] = (-x[1] * cosyaw) - (y[1] * sinyaw);
        ey[2][2][2] = (x[1] * sinyaw) - (y[1] * cosyaw);

        /*z[2]=z[1]*/

        /*Apply pitch*/

        /*x[3]=x[2];*/

        /*derivatives of y[3] with respect to parameter 1 which is not yet applied are*/

        /* computed as zero since these values are references subsequently*/

        /* This is not done for x[3] and z[3] because they are futher modified*/

        /* to generate x[4] and z[4] which have nonzero derivatives with respect to*/

        /* parameter 1 */

        y[3] = (y[2] * cospitch) + (z[1] * sinpitch);

        dy[3][0] = (-y[2] * sinpitch) + (z[1] * cospitch);

        ey[3][0][0] = (-y[2] * cospitch) - (z[1] * sinpitch);

        dy[3][1] = 0; /*parameter 1 not yet applied*/
        ey[3][1][0] = 0;
        ey[3][1][1] = 0;

        dy[3][2] = dy[2][2] * cospitch /*+dz[1][2]*sinpitch*/;

        ey[3][2][0] = -dy[2][2] * sinpitch;
        ey[3][2][1] = 0; /*parameter 1 not yet applied*/
        ey[3][2][2] = ey[2][2][2] * cospitch;

        z[3] = (z[1] * cospitch) - (y[2] * sinpitch);

        dz[3][0] = (-z[1] * sinpitch) - (y[2] * cospitch);

        ez[3][0][0] = (-z[1] * cospitch) + (y[2] * sinpitch);

        dz[3][2] = /*dz[1][2]*cospitch*/ -dy[2][2] * sinpitch;

        ez[3][2][0] = -dy[2][2] * cospitch;
        ez[3][2][2] = -ey[2][2][2] * sinpitch;

        /* Apply roll */

        x[4] = (x[2] * cosroll) + (z[3] * sinroll);
        dx[4][0] = /*dx[2][0]*cosroll+*/ dz[3][0] * sinroll;
        ex[4][0][0] = ez[3][0][0] * sinroll;
        dx[4][1] = (-x[2] * sinroll) + (z[3] * cosroll);
        ex[4][1][0] = /*-dx[2][0]*sinroll+*/ dz[3][0] * cosroll;
        ex[4][1][1] = (-x[2] * cosroll) - (z[3] * sinroll);
        dx[4][2] = (dx[2][2] * cosroll) + (dz[3][2] * sinroll);
        ex[4][2][0] = /*ex[2][2][0]*cosroll+*/ ez[3][2][0] * sinroll;
        ex[4][2][1] = (-dx[2][2] * sinroll) + (dz[3][2] * cosroll);
        ex[4][2][2] = (ex[2][2][2] * cosroll) + (ez[3][2][2] * sinroll);

        /*y[4]=y[3];*/

        z[4] = (z[3] * cosroll) - (x[2] * sinroll);
        dz[4][0] = dz[3][0] * cosroll /*-dx[2][0]*sinroll*/;
        ez[4][0][0] = ez[3][0][0] * cosroll;
        dz[4][1] = (-z[3] * sinroll) - (x[2] * cosroll);
        ez[4][1][0] = -dz[3][0] * sinroll /*-dx[2][0]*cosroll*/;
        ez[4][1][1] = (-z[3] * cosroll) + (x[2] * sinroll);
        dz[4][2] = (dz[3][2] * cosroll) - (dx[2][2] * sinroll);
        ez[4][2][0] = ez[3][2][0] * cosroll /*-ex[2][2][0]*sinroll*/;
        ez[4][2][1] = (-dz[3][2] * sinroll) - (dx[2][2] * cosroll);
        ez[4][2][2] = (ez[3][2][2] * cosroll) - (ex[2][2][2] * sinroll);

        e[0] = (x[4] - p + ((x_dim2 - 1) * xoom2)) / (2 * xoom2);

        de[0][0] = dx[4][0] / (2 * xoom2);
        ee[0][0][0] = ex[4][0][0] / (2 * xoom2);
        de[0][1] = dx[4][1] / (2 * xoom2);
        ee[0][1][0] = ex[4][1][0] / (2 * xoom2);
        ee[0][1][1] = ex[4][1][1] / (2 * xoom2);
        de[0][2] = dx[4][2] / (2 * xoom2);
        ee[0][2][0] = ex[4][2][0] / (2 * xoom2);
        ee[0][2][1] = ex[4][2][1] / (2 * xoom2);
        ee[0][2][2] = ex[4][2][2] / (2 * xoom2);
        de[0][3] = -1.0 / (2 * xoom2);
        ee[0][3][0] = 0;
        ee[0][3][1] = 0;
        ee[0][3][2] = 0;
        ee[0][3][3] = 0;
        de[0][4] = 0;
        ee[0][4][0] = 0;
        ee[0][4][1] = 0;
        ee[0][4][2] = 0;
        ee[0][4][3] = 0;
        ee[0][4][4] = 0;
        de[0][5] = 0;
        ee[0][5][0] = 0;
        ee[0][5][1] = 0;
        ee[0][5][2] = 0;
        ee[0][5][3] = 0;
        ee[0][5][4] = 0;
        ee[0][5][5] = 0;

        e[1] = (y[3] - q + ((y_dim2 - 1) * yoom2)) / (2 * yoom2);

        de[1][0] = dy[3][0] / (2 * yoom2);
        ee[1][0][0] = ey[3][0][0] / (2 * yoom2);
        de[1][1] = dy[3][1] / (2 * yoom2);
        ee[1][1][0] = ey[3][1][0] / (2 * yoom2);
        ee[1][1][1] = ey[3][1][1] / (2 * yoom2);
        de[1][2] = dy[3][2] / (2 * yoom2);
        ee[1][2][0] = ey[3][2][0] / (2 * yoom2);
        ee[1][2][1] = ey[3][2][1] / (2 * yoom2);
        ee[1][2][2] = ey[3][2][2] / (2 * yoom2);
        de[1][3] = 0;
        ee[1][3][0] = 0;
        ee[1][3][1] = 0;
        ee[1][3][2] = 0;
        ee[1][3][3] = 0;
        de[1][4] = -1.0 / (2 * yoom2);
        ee[1][4][0] = 0;
        ee[1][4][1] = 0;
        ee[1][4][2] = 0;
        ee[1][4][3] = 0;
        ee[1][4][4] = 0;
        de[1][5] = 0;
        ee[1][5][0] = 0;
        ee[1][5][1] = 0;
        ee[1][5][2] = 0;
        ee[1][5][3] = 0;
        ee[1][5][4] = 0;
        ee[1][5][5] = 0;

        e[2] = (z[4] - r + ((z_dim2 - 1) * zoom2)) / (2 * zoom2);

        de[2][0] = dz[4][0] / (2 * zoom2);
        ee[2][0][0] = ez[4][0][0] / (2 * zoom2);
        de[2][1] = dz[4][1] / (2 * zoom2);
        ee[2][1][0] = ez[4][1][0] / (2 * zoom2);
        ee[2][1][1] = ez[4][1][1] / (2 * zoom2);
        de[2][2] = dz[4][2] / (2 * zoom2);
        ee[2][2][0] = ez[4][2][0] / (2 * zoom2);
        ee[2][2][1] = ez[4][2][1] / (2 * zoom2);
        ee[2][2][2] = ez[4][2][2] / (2 * zoom2);
        de[2][3] = 0;
        ee[2][3][0] = 0;
        ee[2][3][1] = 0;
        ee[2][3][2] = 0;
        ee[2][3][3] = 0;
        de[2][4] = 0;
        ee[2][4][0] = 0;
        ee[2][4][1] = 0;
        ee[2][4][2] = 0;
        ee[2][4][3] = 0;
        ee[2][4][4] = 0;
        de[2][5] = -1.0 / (2 * zoom2);
        ee[2][5][0] = 0;
        ee[2][5][1] = 0;
        ee[2][5][2] = 0;
        ee[2][5][3] = 0;
        ee[2][5][4] = 0;
        ee[2][5][5] = 0;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e                   DOCUMENT ME!
     * @param  de                  DOCUMENT ME!
     * @param  ee                  DOCUMENT ME!
     * @param  tps                 DOCUMENT ME!
     * @param  xoom1               DOCUMENT ME!
     * @param  xoom2               DOCUMENT ME!
     * @param  yoom1               DOCUMENT ME!
     * @param  yoom2               DOCUMENT ME!
     * @param  zoom1               DOCUMENT ME!
     * @param  zoom2               DOCUMENT ME!
     * @param  pixel_size1         DOCUMENT ME!
     * @param  pixel_size2         DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  z_dim1              DOCUMENT ME!
     * @param  z_dim2              DOCUMENT ME!
     * @param  cubicInterpolation  DOCUMENT ME!
     */
    void tala_rotatef(double[] e, double[][] de, double[][][] ee, double[] tps, float xoom1, float xoom2, float yoom1,
                      float yoom2, float zoom1, float zoom2, double pixel_size1, double pixel_size2, int x_dim1,
                      int x_dim2, int y_dim1, int y_dim2, int z_dim1, int z_dim2, boolean cubicInterpolation) {

        double[] x = new double[5];
        double[] y = new double[5];
        double[] z = new double[5];
        double[][] dx = new double[5][9];
        double[][] dy = new double[5][9];
        double[][] dz = new double[5][9];
        double[][][] ex = new double[5][9][9];
        double[][][] ey = new double[5][9][9];
        double[][][] ez = new double[5][9][9];
        double pitch, roll, yaw, p, q, r, talx, taly, talz;
        double sinyaw, cosyaw, sinpitch, cospitch, sinroll, cosroll;

        pitch = tps[0];
        roll = tps[1];
        yaw = tps[2];
        p = tps[3];
        q = tps[4];
        r = tps[5];
        talx = tps[6];
        taly = tps[7];
        talz = tps[8];

        sinyaw = Math.sin(yaw);
        cosyaw = Math.cos(yaw);
        sinpitch = Math.sin(pitch);
        cospitch = Math.cos(pitch);
        sinroll = Math.sin(roll);
        cosroll = Math.cos(roll);

        if (!cubicInterpolation) {
            x[0] = ((2 * e[0]) + 1 - x_dim1) * xoom1 * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + 1 - y_dim1) * yoom1 * (pixel_size1 / pixel_size2);
            z[0] = ((2 * e[2]) + 1 - z_dim1) * zoom1 * (pixel_size1 / pixel_size2);
        } else {
            x[0] = ((2 * e[0]) + ((1 - x_dim1) * xoom1)) * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + ((1 - y_dim1) * yoom1)) * (pixel_size1 / pixel_size2);
            z[0] = ((2 * e[2]) + ((1 - z_dim1) * zoom1)) * (pixel_size1 / pixel_size2);
        }

        /*Apply scaling*/

        x[1] = x[0] * talx;
        dx[1][6] = x[0];

        y[1] = y[0] * taly;
        dy[1][7] = y[0];

        z[1] = z[0] * talz;
        dz[1][8] = z[0];

        /*Apply yaw*/

        x[2] = (x[1] * cosyaw) + (y[1] * sinyaw);
        dx[2][2] = (-x[1] * sinyaw) + (y[1] * cosyaw);
        ex[2][2][2] = (-x[1] * cosyaw) - (y[1] * sinyaw);

        dx[2][6] = dx[1][6] * cosyaw /*+dy[1][6]*sinyaw*/;
        ex[2][6][2] = -dx[1][6] * sinyaw;

        /*ex[2][6][6]=ex[1][6][6]*cosyaw;*/

        dx[2][7] = /*dx[1][7]*cosyaw+*/ dy[1][7] * sinyaw;
        ex[2][7][2] = dy[1][7] * cosyaw;

        /*ex[2][7][6]=ey[1][7][6]*sinyaw;*/

        /*ex[2][7][7]=ey[1][7][7]*sinyaw;*/

        /*dx[2][8]=0;*/

        /*ex[2][8][]=0;*/


        y[2] = (y[1] * cosyaw) - (x[1] * sinyaw);

        dy[2][2] = (-x[1] * cosyaw) - (y[1] * sinyaw);
        ey[2][2][2] = (x[1] * sinyaw) - (y[1] * cosyaw);

        dy[2][6] = /*dy[1][6]*cosyaw*/ -dx[1][6] * sinyaw;
        ey[2][6][2] = -dx[1][6] * cosyaw;

        /*ey[2][6][6]=-ex[1][6][6]*sinyaw;*/

        dy[2][7] = dy[1][7] * cosyaw /*-dx[1][7]*sinyaw*/;
        ey[2][7][2] = -dy[1][7] * sinyaw;

        /*ey[2][7][6]=ey[1][7][6]*cosyaw;*/

        /*ey[2][7][7]=ey[1][7][7]*cosyaw;*/

        /*ey[2][7][8]=ey[1][7][8]*cosyaw;*/

        /*dy[2][8]=0*/

        /*ey[2][8][]=0*/

        /*z[2]=z[1]*/

        /*Apply pitch*/

        /*x[3]=x[2];*/
        y[3] = (y[2] * cospitch) + (z[1] * sinpitch);

        dy[3][0] = (-y[2] * sinpitch) + (z[1] * cospitch);

        ey[3][0][0] = (-y[2] * cospitch) - (z[1] * sinpitch);

        dy[3][1] = 0; /*parameter 1 not yet applied*/
        ey[3][1][0] = 0;
        ey[3][1][1] = 0;

        dy[3][2] = dy[2][2] * cospitch /*+dz[1][2]*sinpitch*/;

        ey[3][2][0] = -dy[2][2] * sinpitch;
        ey[3][2][1] = 0; /*parameter 1 not yet applied*/
        ey[3][2][2] = ey[2][2][2] * cospitch;

        dy[3][6] = dy[2][6] * cospitch /*+dz[1][6]*sinpitch*/;

        ey[3][6][0] = -dy[2][6] * sinpitch;
        ey[3][6][1] = 0; /*parameter 1 not yet applied*/
        ey[3][6][2] = ey[2][6][2] * cospitch;
        ey[3][6][6] = 0; /*ey[2][6][6]*cospitch*/

        dy[3][7] = dy[2][7] * cospitch /*+dz[1][7]*sinpitch*/;

        ey[3][7][0] = -dy[2][7] * sinpitch;
        ey[3][7][1] = 0; /*parameter 1 not yet applied*/
        ey[3][7][2] = ey[2][7][2] * cospitch;
        ey[3][7][6] = 0; /*ey[2][7][6]*cospitch*/
        ey[3][7][7] = 0; /*ey[2][7][7]*cospitch*/

        dy[3][8] = /*dy[2][8]*cospitch+*/ dz[1][8] * sinpitch;
        ey[3][8][0] = dz[1][8] * cospitch;
        ey[3][8][1] = 0; /*ez[1][8][1]*sinpitch*/
        ey[3][8][2] = 0; /*ez[1][8][2]*sinpitch*/
        ey[3][8][6] = 0; /*ez[1][8][6]*sinpitch*/
        ey[3][8][7] = 0; /*ez[1][8][7]*sinpitch*/
        ey[3][8][8] = 0; /*ez[1][8][8]*sinpitch*/

        z[3] = (z[1] * cospitch) - (y[2] * sinpitch);

        dz[3][0] = (-z[1] * sinpitch) - (y[2] * cospitch);

        ez[3][0][0] = (-z[1] * cospitch) + (y[2] * sinpitch);

        dz[3][2] = /*dz[1][2]*cospitch*/ -dy[2][2] * sinpitch;

        ez[3][2][0] = -dy[2][2] * cospitch;
        ez[3][2][2] = -ey[2][2][2] * sinpitch;

        dz[3][6] = /*dz[1][6]*cospitch*/ -dy[2][6] * sinpitch;

        ez[3][6][0] = -dy[2][6] * cospitch;
        ez[3][6][2] = -ey[2][6][2] * sinpitch;

        /*ez[3][6][6]=-ey[2][6][6]*sinpitch;*/

        dz[3][7] = /*dz[1][7]*cospitch*/ -dy[2][7] * sinpitch;

        ez[3][7][0] = -dy[2][7] * cospitch;
        ez[3][7][2] = -ey[2][7][2] * sinpitch;

        /*ez[3][7][6]=-ey[2][7][6]*sinpitch;*/

        /*ez[3][7][7]=-ey[2][7][7]*sinpitch;*/

        dz[3][8] = dz[1][8] * cospitch /*-dy[2][8]*sinpitch*/;

        ez[3][8][0] = -dz[1][8] * sinpitch;

        /*ez[3][8][1]=-ez[1][8][1]*cospitch;*/

        /*ez[3][8][2]=-ez[1][8][2]*cospitch;*/

        /*ez[3][8][6]=-ez[1][8][6]*cospitch;*/

        /*ez[3][8][7]=-ez[1][8][7]*cospitch;*/

        /*ez[3][8][8]=-ez[1][8][8]*cospitch;*/


        /* Apply roll */

        x[4] = (x[2] * cosroll) + (z[3] * sinroll);
        dx[4][0] = /*dx[2][0]*cosroll+*/ dz[3][0] * sinroll;
        ex[4][0][0] = ez[3][0][0] * sinroll;
        dx[4][1] = (-x[2] * sinroll) + (z[3] * cosroll);
        ex[4][1][0] = /*-dx[2][0]*sinroll+*/ dz[3][0] * cosroll;
        ex[4][1][1] = (-x[2] * cosroll) - (z[3] * sinroll);
        dx[4][2] = (dx[2][2] * cosroll) + (dz[3][2] * sinroll);
        ex[4][2][0] = /*ex[2][2][0]*cosroll+*/ ez[3][2][0] * sinroll;
        ex[4][2][1] = (-dx[2][2] * sinroll) + (dz[3][2] * cosroll);
        ex[4][2][2] = (ex[2][2][2] * cosroll) + (ez[3][2][2] * sinroll);
        dx[4][6] = (dx[2][6] * cosroll) + (dz[3][6] * sinroll);
        ex[4][6][0] = /*ex[2][6][0]*cosroll+*/ ez[3][6][0] * sinroll;
        ex[4][6][1] = (-dx[2][6] * sinroll) + (dz[3][6] * cosroll);
        ex[4][6][2] = (ex[2][6][2] * cosroll) + (ez[3][6][2] * sinroll);
        ex[4][6][6] = 0; /*ex[2][6][6]*cosroll+ez[3][6][6]*sinroll;*/
        dx[4][7] = (dx[2][7] * cosroll) + (dz[3][7] * sinroll);
        ex[4][7][0] = /*ex[2][7][0]*cosroll+*/ ez[3][7][0] * sinroll;
        ex[4][7][1] = (-dx[2][7] * sinroll) + (dz[3][7] * cosroll);
        ex[4][7][2] = (ex[2][7][2] * cosroll) + (ez[3][7][2] * sinroll);
        ex[4][7][6] = 0; /*ex[2][7][6]*cosroll+ez[3][7][6]*sinroll*/
        ex[4][7][7] = 0; /*ex[2][7][7]*cosroll+ez[3][7][7]*sinroll*/
        dx[4][8] = /*dx[2][8]*cosroll+*/ dz[3][8] * sinroll;
        ex[4][8][0] = ez[3][8][0] * sinroll;
        ex[4][8][1] = dz[3][8] * cosroll;
        ex[4][8][2] = 0; /*ez[3][8][2]*sinroll*/
        ex[4][8][6] = 0; /*ez[3][8][6]*sinroll*/
        ex[4][8][7] = 0; /*ez[3][8][7]*sinroll*/
        ex[4][8][8] = 0; /*ez[3][8][8]*sinroll*/

        /*y[4]=y[3];*/

        z[4] = (z[3] * cosroll) - (x[2] * sinroll);
        dz[4][0] = dz[3][0] * cosroll /*-dx[2][0]*sinroll*/;
        ez[4][0][0] = ez[3][0][0] * cosroll;
        dz[4][1] = (-z[3] * sinroll) - (x[2] * cosroll);
        ez[4][1][0] = -dz[3][0] * sinroll /*-dx[2][0]*cosroll*/;
        ez[4][1][1] = (-z[3] * cosroll) + (x[2] * sinroll);
        dz[4][2] = (dz[3][2] * cosroll) - (dx[2][2] * sinroll);
        ez[4][2][0] = ez[3][2][0] * cosroll /*-ex[2][2][0]*sinroll*/;
        ez[4][2][1] = (-dz[3][2] * sinroll) - (dx[2][2] * cosroll);
        ez[4][2][2] = (ez[3][2][2] * cosroll) - (ex[2][2][2] * sinroll);
        dz[4][6] = (dz[3][6] * cosroll) - (dx[2][6] * sinroll);
        ez[4][6][0] = ez[3][6][0] * cosroll /*-ex[2][6][0]*sinroll*/;
        ez[4][6][1] = (-dz[3][6] * sinroll) - (dx[2][6] * cosroll);
        ez[4][6][2] = (ez[3][6][2] * cosroll) - (ex[2][6][2] * sinroll);
        ez[4][6][6] = 0; /*ez[3][6][6]*cosroll-ex[2][6][6]*sinroll;*/
        dz[4][7] = (dz[3][7] * cosroll) - (dx[2][7] * sinroll);
        ez[4][7][0] = ez[3][7][0] * cosroll /*-ex[2][7][0]*sinroll*/;
        ez[4][7][1] = (-dz[3][7] * sinroll) - (dx[2][7] * cosroll);
        ez[4][7][2] = (ez[3][7][2] * cosroll) - (ex[2][7][2] * sinroll);
        ez[4][7][6] = 0; /*ez[3][7][6]*cosroll-ex[2][7][6]*sinroll;*/
        ez[4][7][7] = 0; /*ez[3][7][7]*cosroll-ex[2][7][7]*sinroll;*/
        dz[4][8] = dz[3][8] * cosroll /*-dx[2][8]*sinroll*/;
        ez[4][8][0] = ez[3][8][0] * cosroll;
        ez[4][8][1] = -dz[3][8] * sinroll;
        ez[4][8][2] = 0; /*ez[3][8][2]*cosroll;*/
        ez[4][8][6] = 0; /*ez[3][8][6]*cosroll;*/
        ez[4][8][7] = 0; /*ez[3][8][7]*cosroll;*/
        ez[4][8][8] = 0; /*ez[3][8][8]*cosroll;*/

        e[0] = (x[4] - p + ((x_dim2 - 1) * xoom2)) / (2 * xoom2);

        de[0][0] = dx[4][0] / (2 * xoom2);
        ee[0][0][0] = ex[4][0][0] / (2 * xoom2);
        de[0][1] = dx[4][1] / (2 * xoom2);
        ee[0][1][0] = ex[4][1][0] / (2 * xoom2);
        ee[0][1][1] = ex[4][1][1] / (2 * xoom2);
        de[0][2] = dx[4][2] / (2 * xoom2);
        ee[0][2][0] = ex[4][2][0] / (2 * xoom2);
        ee[0][2][1] = ex[4][2][1] / (2 * xoom2);
        ee[0][2][2] = ex[4][2][2] / (2 * xoom2);
        de[0][3] = -1.0 / (2 * xoom2);
        ee[0][3][0] = 0;
        ee[0][3][1] = 0;
        ee[0][3][2] = 0;
        ee[0][3][3] = 0;
        de[0][4] = 0;
        ee[0][4][0] = 0;
        ee[0][4][1] = 0;
        ee[0][4][2] = 0;
        ee[0][4][3] = 0;
        ee[0][4][4] = 0;
        de[0][5] = 0;
        ee[0][5][0] = 0;
        ee[0][5][1] = 0;
        ee[0][5][2] = 0;
        ee[0][5][3] = 0;
        ee[0][5][4] = 0;
        ee[0][5][5] = 0;
        de[0][6] = dx[4][6] / (2 * xoom2);
        ee[0][6][0] = ex[4][6][0] / (2 * xoom2);
        ee[0][6][1] = ex[4][6][1] / (2 * xoom2);
        ee[0][6][2] = ex[4][6][2] / (2 * xoom2);
        ee[0][6][3] = 0;
        ee[0][6][4] = 0;
        ee[0][6][5] = 0;
        ee[0][6][6] = ex[4][6][6] / (2 * xoom2);
        de[0][7] = dx[4][7] / (2 * xoom2);
        ee[0][7][0] = ex[4][7][0] / (2 * xoom2);
        ee[0][7][1] = ex[4][7][1] / (2 * xoom2);
        ee[0][7][2] = ex[4][7][2] / (2 * xoom2);
        ee[0][7][3] = 0;
        ee[0][7][4] = 0;
        ee[0][7][5] = 0;
        ee[0][7][6] = ex[4][7][6] / (2 * xoom2);
        ee[0][7][7] = ex[4][7][7] / (2 * xoom2);
        de[0][8] = dx[4][8] / (2 * xoom2);
        ee[0][8][0] = ex[4][8][0] / (2 * xoom2);
        ee[0][8][1] = ex[4][8][1] / (2 * xoom2);
        ee[0][8][2] = ex[4][8][2] / (2 * xoom2);
        ee[0][8][3] = 0;
        ee[0][8][4] = 0;
        ee[0][8][5] = 0;
        ee[0][8][6] = ex[4][8][6] / (2 * xoom2);
        ee[0][8][7] = ex[4][8][7] / (2 * xoom2);
        ee[0][8][8] = ex[4][8][8] / (2 * xoom2);

        e[1] = (y[3] - q + ((y_dim2 - 1) * yoom2)) / (2 * yoom2);

        de[1][0] = dy[3][0] / (2 * yoom2);
        ee[1][0][0] = ey[3][0][0] / (2 * yoom2);
        de[1][1] = dy[3][1] / (2 * yoom2);
        ee[1][1][0] = ey[3][1][0] / (2 * yoom2);
        ee[1][1][1] = ey[3][1][1] / (2 * yoom2);
        de[1][2] = dy[3][2] / (2 * yoom2);
        ee[1][2][0] = ey[3][2][0] / (2 * yoom2);
        ee[1][2][1] = ey[3][2][1] / (2 * yoom2);
        ee[1][2][2] = ey[3][2][2] / (2 * yoom2);
        de[1][3] = 0;
        ee[1][3][0] = 0;
        ee[1][3][1] = 0;
        ee[1][3][2] = 0;
        ee[1][3][3] = 0;
        de[1][4] = -1.0 / (2 * yoom2);
        ee[1][4][0] = 0;
        ee[1][4][1] = 0;
        ee[1][4][2] = 0;
        ee[1][4][3] = 0;
        ee[1][4][4] = 0;
        de[1][5] = 0;
        ee[1][5][0] = 0;
        ee[1][5][1] = 0;
        ee[1][5][2] = 0;
        ee[1][5][3] = 0;
        ee[1][5][4] = 0;
        ee[1][5][5] = 0;
        de[1][6] = dy[3][6] / (2 * yoom2);
        ee[1][6][0] = ey[3][6][0] / (2 * yoom2);
        ee[1][6][1] = ey[3][6][1] / (2 * yoom2);
        ee[1][6][2] = ey[3][6][2] / (2 * yoom2);
        ee[1][6][3] = 0;
        ee[1][6][4] = 0;
        ee[1][6][5] = 0;
        ee[1][6][6] = ey[3][6][6] / (2 * yoom2);
        de[1][7] = dy[3][7] / (2 * yoom2);
        ee[1][7][0] = ey[3][7][0] / (2 * yoom2);
        ee[1][7][1] = ey[3][7][1] / (2 * yoom2);
        ee[1][7][2] = ey[3][7][2] / (2 * yoom2);
        ee[1][7][3] = 0;
        ee[1][7][4] = 0;
        ee[1][7][5] = 0;
        ee[1][7][6] = ey[3][7][6] / (2 * yoom2);
        ee[1][7][7] = ey[3][7][7] / (2 * yoom2);
        de[1][8] = dy[3][8] / (2 * yoom2);
        ee[1][8][0] = ey[3][8][0] / (2 * yoom2);
        ee[1][8][1] = ey[3][8][1] / (2 * yoom2);
        ee[1][8][2] = ey[3][8][2] / (2 * yoom2);
        ee[1][8][3] = 0;
        ee[1][8][4] = 0;
        ee[1][8][5] = 0;
        ee[1][8][6] = ey[3][8][6] / (2 * yoom2);
        ee[1][8][7] = ey[3][8][7] / (2 * yoom2);
        ee[1][8][8] = ey[3][8][8] / (2 * yoom2);

        e[2] = (z[4] - r + ((z_dim2 - 1) * zoom2)) / (2 * zoom2);

        de[2][0] = dz[4][0] / (2 * zoom2);
        ee[2][0][0] = ez[4][0][0] / (2 * zoom2);
        de[2][1] = dz[4][1] / (2 * zoom2);
        ee[2][1][0] = ez[4][1][0] / (2 * zoom2);
        ee[2][1][1] = ez[4][1][1] / (2 * zoom2);
        de[2][2] = dz[4][2] / (2 * zoom2);
        ee[2][2][0] = ez[4][2][0] / (2 * zoom2);
        ee[2][2][1] = ez[4][2][1] / (2 * zoom2);
        ee[2][2][2] = ez[4][2][2] / (2 * zoom2);
        de[2][3] = 0;
        ee[2][3][0] = 0;
        ee[2][3][1] = 0;
        ee[2][3][2] = 0;
        ee[2][3][3] = 0;
        de[2][4] = 0;
        ee[2][4][0] = 0;
        ee[2][4][1] = 0;
        ee[2][4][2] = 0;
        ee[2][4][3] = 0;
        ee[2][4][4] = 0;
        de[2][5] = -1.0 / (2 * zoom2);
        ee[2][5][0] = 0;
        ee[2][5][1] = 0;
        ee[2][5][2] = 0;
        ee[2][5][3] = 0;
        ee[2][5][4] = 0;
        ee[2][5][5] = 0;
        de[2][6] = dz[4][6] / (2 * zoom2);
        ee[2][6][0] = ez[4][6][0] / (2 * zoom2);
        ee[2][6][1] = ez[4][6][1] / (2 * zoom2);
        ee[2][6][2] = ez[4][6][2] / (2 * zoom2);
        ee[2][6][3] = 0;
        ee[2][6][4] = 0;
        ee[2][6][5] = 0;
        ee[2][6][6] = ez[4][6][6] / (2 * zoom2);
        de[2][7] = dz[4][7] / (2 * zoom2);
        ee[2][7][0] = ez[4][7][0] / (2 * zoom2);
        ee[2][7][1] = ez[4][7][1] / (2 * zoom2);
        ee[2][7][2] = ez[4][7][2] / (2 * zoom2);
        ee[2][7][3] = 0;
        ee[2][7][4] = 0;
        ee[2][7][5] = 0;
        ee[2][7][6] = ez[4][7][6] / (2 * zoom2);
        ee[2][7][7] = ez[4][7][7] / (2 * zoom2);
        de[2][8] = dz[4][8] / (2 * zoom2);
        ee[2][8][0] = ez[4][8][0] / (2 * zoom2);
        ee[2][8][1] = ez[4][8][1] / (2 * zoom2);
        ee[2][8][2] = ez[4][8][2] / (2 * zoom2);
        ee[2][8][3] = 0;
        ee[2][8][4] = 0;
        ee[2][8][5] = 0;
        ee[2][8][6] = ez[4][8][6] / (2 * zoom2);
        ee[2][8][7] = ez[4][8][7] / (2 * zoom2);
        ee[2][8][8] = ez[4][8][8] / (2 * zoom2);

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tps                 DOCUMENT ME!
     * @param  e                   corresponding forward unit vectors
     * @param  de                  1st derivatives of unit vectors
     * @param  ee                  2nd derivatives of unit vectors
     * @param  x_size2             DOCUMENT ME!
     * @param  y_size2             DOCUMENT ME!
     * @param  z_size2             DOCUMENT ME!
     * @param  x_size1             DOCUMENT ME!
     * @param  y_size1             DOCUMENT ME!
     * @param  z_size1             DOCUMENT ME!
     * @param  cubicInterpolation  In-plane affine rescaling model with fixed determinant
     */

    void uv2D5(double[] tps, double[][] e, double[][][] de, double[][][][] ee, float x_size2, float y_size2,
               float z_size2, float x_size1, float y_size1, float z_size1, boolean cubicInterpolation) {

        int s, t;
        double determinant;
        double pixel_size_s;

        pixel_size_s = x_size1;

        if (y_size1 < pixel_size_s) {
            pixel_size_s = y_size1;
        }

        if (z_size1 < pixel_size_s) {
            pixel_size_s = z_size1;
        }

        e[0][0] = tps[0];
        e[0][1] = tps[3];
        e[0][2] = 0;

        e[1][0] = tps[1];
        e[1][2] = 0;

        e[2][0] = 0;
        e[2][1] = 0;
        e[2][2] = 1;

        e[3][0] = tps[2];
        e[3][1] = tps[4];
        e[3][2] = 0;

        /*And the constant row*/
        e[0][3] = 0;
        e[1][3] = 0;
        e[2][3] = 0;
        e[3][3] = 1;

        if (!cubicInterpolation) {
            determinant = (x_size1) * (y_size1) / ((x_size2) * (y_size2));
        } else {
            determinant = (pixel_size_s) * (pixel_size_s) / ((x_size2) * (y_size2));
        }

        e[1][1] = (determinant + (tps[1] * tps[3])) / tps[0];

        for (t = 0; t < 5; t++) {
            de[0][0][t] = 0;
            de[0][1][t] = 0;
            de[0][2][t] = 0;
            de[0][3][t] = 0;
            de[1][0][t] = 0;
            de[1][1][t] = 0;
            de[1][2][t] = 0;
            de[1][3][t] = 0;
            de[2][0][t] = 0;
            de[2][1][t] = 0;
            de[2][2][t] = 0;
            de[2][3][t] = 0;
            de[3][0][t] = 0;
            de[3][1][t] = 0;
            de[3][2][t] = 0;
            de[3][3][t] = 0;

            for (s = 0; s <= t; s++) {
                ee[0][0][t][s] = 0;
                ee[0][1][t][s] = 0;
                ee[0][2][t][s] = 0;
                ee[0][3][t][s] = 0;
                ee[1][0][t][s] = 0;
                ee[1][1][t][s] = 0;
                ee[1][2][t][s] = 0;
                ee[1][3][t][s] = 0;
                ee[2][0][t][s] = 0;
                ee[2][1][t][s] = 0;
                ee[2][2][t][s] = 0;
                ee[2][3][t][s] = 0;
                ee[3][0][t][s] = 0;
                ee[3][1][t][s] = 0;
                ee[3][2][t][s] = 0;
                ee[3][3][t][s] = 0;
            }
        }

        de[0][0][0] = 1;
        de[1][0][1] = 1;
        de[3][0][2] = 1;
        de[0][1][3] = 1;
        de[3][1][4] = 1;
        de[1][1][0] = -(determinant + (tps[1] * tps[3])) / (tps[0] * tps[0]);
        de[1][1][1] = tps[3] / tps[0];
        de[1][1][3] = tps[1] / tps[0];

        ee[1][1][0][0] = (determinant + (tps[1] * tps[3])) * 2.0 / (tps[0] * tps[0] * tps[0]);
        ee[1][1][1][0] = -tps[3] / (tps[0] * tps[0]);
        ee[1][1][3][0] = -tps[1] / (tps[0] * tps[0]);
        ee[1][1][3][1] = 1.0 / tps[0];

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tps  DOCUMENT ME!
     * @param  ef   DOCUMENT ME!
     * @param  def  DOCUMENT ME!
     * @param  eef
     *              <p>will calculate first and second derivatives with respect to the paramaters for a full affine 12
     *              parameter affine model.</p>
     *
     *              <p>Note that a full 4 x 4 matrix is assumed.</p>
     */


    void uv3D12(double[] tps, double[][] ef, double[][][] def, double[][][][] eef) {

        int m, n, o, p;

        /*Define forward transform*/

        ef[0][0] = tps[0];
        ef[1][0] = tps[1];
        ef[2][0] = tps[2];
        ef[3][0] = tps[3];
        ef[0][1] = tps[4];
        ef[1][1] = tps[5];
        ef[2][1] = tps[6];
        ef[3][1] = tps[7];
        ef[0][2] = tps[8];
        ef[1][2] = tps[9];
        ef[2][2] = tps[10];
        ef[3][2] = tps[11];
        ef[0][3] = 0;
        ef[1][3] = 0;
        ef[2][3] = 0;
        ef[3][3] = 1;

        /*All second partial derivatives are zero for forward transform*/
        for (m = 0; m < 4; m++) {

            for (n = 0; n < 4; n++) {

                for (o = 0; o < 12; o++) {
                    def[m][n][o] = 0;

                    for (p = 0; p <= o; p++) {
                        eef[m][n][o][p] = 0;
                    }
                }
            }
        }

        /*First partial derivatives are zero except where the parameter*/

        /* is the factor being differentiated in which case it is one*/
        def[0][0][0] = 1;
        def[1][0][1] = 1;
        def[2][0][2] = 1;
        def[3][0][3] = 1;
        def[0][1][4] = 1;
        def[1][1][5] = 1;
        def[2][1][6] = 1;
        def[3][1][7] = 1;
        def[0][2][8] = 1;
        def[1][2][9] = 1;
        def[2][2][10] = 1;
        def[3][2][11] = 1;

    }

    /**
     * DOCUMENT ME!
     *
     * @param  tps                 ptich,roll,yaw,p,q,r in radians and half-pixels
     * @param  e                   corresponding forward unit vectors
     * @param  de                  1st derivatives of unit vectors with respect to pitch,roll,yaw,p,q,r
     * @param  ee                  2nd derivatives of unit vectors with respect to pitch,roll,yaw,p,q,r
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  z_dim2              DOCUMENT ME!
     * @param  x_size2             DOCUMENT ME!
     * @param  y_size2             DOCUMENT ME!
     * @param  z_size2             DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  z_dim1              DOCUMENT ME!
     * @param  x_size1             DOCUMENT ME!
     * @param  y_size1             DOCUMENT ME!
     * @param  z_size1             DOCUMENT ME!
     * @param  cubicInterpolation  will take pitch,roll,yaw,p,q, and r and convert them to the corresponding forward
     *                             unit vectors & derivatives
     */


    void uv3D6(double[] tps, double[][] e, double[][][] de, double[][][][] ee, int x_dim2, int y_dim2, int z_dim2,
               float x_size2, float y_size2, float z_size2, int x_dim1, int y_dim1, int z_dim1, float x_size1,
               float y_size1, float z_size1, boolean cubicInterpolation) {

        float xoom1, xoom2;
        float yoom1, yoom2;
        float zoom1, zoom2;
        double pixel_size1, pixel_size2;
        int s, t;

        pixel_size1 = x_size1;

        if (y_size1 < pixel_size1) {
            pixel_size1 = y_size1;
        }

        if (z_size1 < pixel_size1) {
            pixel_size1 = z_size1;
        }

        xoom1 = (float) (x_size1 / pixel_size1);
        yoom1 = (float) (y_size1 / pixel_size1);
        zoom1 = (float) (z_size1 / pixel_size1);

        pixel_size2 = x_size2;

        if (y_size2 < pixel_size2) {
            pixel_size2 = y_size2;
        }

        if (z_size2 < pixel_size2) {
            pixel_size2 = z_size2;
        }

        xoom2 = (float) (x_size2 / pixel_size2);
        yoom2 = (float) (y_size2 / pixel_size2);
        zoom2 = (float) (z_size2 / pixel_size2);

        e[0][0] = 1;
        e[0][1] = 0;
        e[0][2] = 0;

        e[1][0] = 0;
        e[1][1] = 1;
        e[1][2] = 0;

        e[2][0] = 0;
        e[2][1] = 0;
        e[2][2] = 1;

        e[3][0] = 0;
        e[3][1] = 0;
        e[3][2] = 0;

        /*And the constant row*/
        e[0][3] = 0;
        e[1][3] = 0;
        e[2][3] = 0;
        e[3][3] = 1;

        rigid_rotatef(e[0], de[0], ee[0], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                      x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);
        rigid_rotatef(e[1], de[1], ee[1], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                      x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);
        rigid_rotatef(e[2], de[2], ee[2], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                      x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);
        rigid_rotatef(e[3], de[3], ee[3], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                      x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);

        e[2][0] = e[2][0] - e[3][0];
        e[1][0] = e[1][0] - e[3][0];
        e[0][0] = e[0][0] - e[3][0];

        e[2][1] = e[2][1] - e[3][1];
        e[1][1] = e[1][1] - e[3][1];
        e[0][1] = e[0][1] - e[3][1];

        e[2][2] = e[2][2] - e[3][2];
        e[1][2] = e[1][2] - e[3][2];
        e[0][2] = e[0][2] - e[3][2];

        for (t = 0; t < 6; t++) {
            de[2][0][t] = de[2][0][t] - de[3][0][t];
            de[1][0][t] = de[1][0][t] - de[3][0][t];
            de[0][0][t] = de[0][0][t] - de[3][0][t];

            de[2][1][t] = de[2][1][t] - de[3][1][t];
            de[1][1][t] = de[1][1][t] - de[3][1][t];
            de[0][1][t] = de[0][1][t] - de[3][1][t];

            de[2][2][t] = de[2][2][t] - de[3][2][t];
            de[1][2][t] = de[1][2][t] - de[3][2][t];
            de[0][2][t] = de[0][2][t] - de[3][2][t];

            de[0][3][t] = 0;
            de[1][3][t] = 0;
            de[2][3][t] = 0;
            de[3][3][t] = 0;

            for (s = 0; s <= t; s++) {

                ee[2][0][t][s] = ee[2][0][t][s] - ee[3][0][t][s];
                ee[1][0][t][s] = ee[1][0][t][s] - ee[3][0][t][s];
                ee[0][0][t][s] = ee[0][0][t][s] - ee[3][0][t][s];

                ee[2][1][t][s] = ee[2][1][t][s] - ee[3][1][t][s];
                ee[1][1][t][s] = ee[1][1][t][s] - ee[3][1][t][s];
                ee[0][1][t][s] = ee[0][1][t][s] - ee[3][1][t][s];

                ee[2][2][t][s] = ee[2][2][t][s] - ee[3][2][t][s];
                ee[1][2][t][s] = ee[1][2][t][s] - ee[3][2][t][s];
                ee[0][2][t][s] = ee[0][2][t][s] - ee[3][2][t][s];

                ee[0][3][t][s] = 0;
                ee[1][3][t][s] = 0;
                ee[2][3][t][s] = 0;
                ee[3][3][t][s] = 0;
            }

        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tps                 pitch,roll,yaw,p,q,r,talax,ralay,talaz in radians, half-pixels, and dimensionless
     *                             scalings
     * @param  e                   corresponding forward unit vectors
     * @param  de                  1st derivatives of unit vectors with respect to
     *                             pitch,roll,yaw,p,q,r,talax,talay,talaz
     * @param  ee                  2nd derivatives of unit vectors with respect to
     *                             pitch,roll,yaw,p,q,r,talax,talay,talaz
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  z_dim2              DOCUMENT ME!
     * @param  x_size2             DOCUMENT ME!
     * @param  y_size2             DOCUMENT ME!
     * @param  z_size2             DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  z_dim1              DOCUMENT ME!
     * @param  x_size1             DOCUMENT ME!
     * @param  y_size1             DOCUMENT ME!
     * @param  z_size1             DOCUMENT ME!
     * @param  cubicInterpolation  will take pitch,roll,yaw,p,q,r,talax,talay, and talaz and convert them to the
     *                             corresponding forward unit vectors & derivatives
     */


    void uv3D9(double[] tps, double[][] e, double[][][] de, double[][][][] ee, int x_dim2, int y_dim2, int z_dim2,
               float x_size2, float y_size2, float z_size2, int x_dim1, int y_dim1, int z_dim1, float x_size1,
               float y_size1, float z_size1, boolean cubicInterpolation) {

        float xoom1, xoom2;
        float yoom1, yoom2;
        float zoom1, zoom2;
        double pixel_size1, pixel_size2;
        int s, t;

        pixel_size1 = x_size1;

        if (y_size1 < pixel_size1) {
            pixel_size1 = y_size1;
        }

        if (z_size1 < pixel_size1) {
            pixel_size1 = z_size1;
        }

        xoom1 = (float) (x_size1 / pixel_size1);
        yoom1 = (float) (y_size1 / pixel_size1);
        zoom1 = (float) (z_size1 / pixel_size1);

        pixel_size2 = x_size2;

        if (y_size2 < pixel_size2) {
            pixel_size2 = y_size2;
        }

        if (z_size2 < pixel_size2) {
            pixel_size2 = z_size2;
        }

        xoom2 = (float) (x_size2 / pixel_size2);
        yoom2 = (float) (y_size2 / pixel_size2);
        zoom2 = (float) (z_size2 / pixel_size2);

        e[0][0] = 1;
        e[0][1] = 0;
        e[0][2] = 0;

        e[1][0] = 0;
        e[1][1] = 1;
        e[1][2] = 0;

        e[2][0] = 0;
        e[2][1] = 0;
        e[2][2] = 1;

        e[3][0] = 0;
        e[3][1] = 0;
        e[3][2] = 0;

        /*And the constant row*/
        e[0][3] = 0;
        e[1][3] = 0;
        e[2][3] = 0;
        e[3][3] = 1;

        tala_rotatef(e[0], de[0], ee[0], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                     x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);
        tala_rotatef(e[1], de[1], ee[1], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                     x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);
        tala_rotatef(e[2], de[2], ee[2], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                     x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);
        tala_rotatef(e[3], de[3], ee[3], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                     x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);

        e[2][0] = e[2][0] - e[3][0];
        e[1][0] = e[1][0] - e[3][0];
        e[0][0] = e[0][0] - e[3][0];

        e[2][1] = e[2][1] - e[3][1];
        e[1][1] = e[1][1] - e[3][1];
        e[0][1] = e[0][1] - e[3][1];

        e[2][2] = e[2][2] - e[3][2];
        e[1][2] = e[1][2] - e[3][2];
        e[0][2] = e[0][2] - e[3][2];

        for (t = 0; t < 9; t++) {
            de[2][0][t] = de[2][0][t] - de[3][0][t];
            de[1][0][t] = de[1][0][t] - de[3][0][t];
            de[0][0][t] = de[0][0][t] - de[3][0][t];

            de[2][1][t] = de[2][1][t] - de[3][1][t];
            de[1][1][t] = de[1][1][t] - de[3][1][t];
            de[0][1][t] = de[0][1][t] - de[3][1][t];

            de[2][2][t] = de[2][2][t] - de[3][2][t];
            de[1][2][t] = de[1][2][t] - de[3][2][t];
            de[0][2][t] = de[0][2][t] - de[3][2][t];

            de[0][3][t] = 0;
            de[1][3][t] = 0;
            de[2][3][t] = 0;
            de[3][3][t] = 0;

            for (s = 0; s <= t; s++) {

                ee[2][0][t][s] = ee[2][0][t][s] - ee[3][0][t][s];
                ee[1][0][t][s] = ee[1][0][t][s] - ee[3][0][t][s];
                ee[0][0][t][s] = ee[0][0][t][s] - ee[3][0][t][s];

                ee[2][1][t][s] = ee[2][1][t][s] - ee[3][1][t][s];
                ee[1][1][t][s] = ee[1][1][t][s] - ee[3][1][t][s];
                ee[0][1][t][s] = ee[0][1][t][s] - ee[3][1][t][s];

                ee[2][2][t][s] = ee[2][2][t][s] - ee[3][2][t][s];
                ee[1][2][t][s] = ee[1][2][t][s] - ee[3][2][t][s];
                ee[0][2][t][s] = ee[0][2][t][s] - ee[3][2][t][s];

                ee[0][3][t][s] = 0;
                ee[1][3][t][s] = 0;
                ee[2][3][t][s] = 0;
                ee[3][3][t][s] = 0;
            }

        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              source parameters([4][3])
     * @param   des             derivatives of each source parameter with respect to each element
     * @param   ees             second derivatives as above
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2XDim      DOCUMENT ME!
     * @param   pixel2YDim      DOCUMENT ME!
     * @param   pixel2ZDim      DOCUMENT ME!
     * @param   pixel5          interpolated values of target file
     * @param   pixel5Mask      mask of interpolated values of target file
     * @param   pixel5XDim      DOCUMENT ME!
     * @param   pixel5YDim      DOCUMENT ME!
     * @param   pixel5ZDim      DOCUMENT ME!
     * @param   threshold5      DOCUMENT ME!
     * @param   dcff            first derivatives of sd with respect to parameters
     * @param   ecff            second derivatives of sd with respect to parameters
     * @param   count           number of pixels in partitions that are above threshold
     * @param   mean            ratio of pixel in source file/corresponding pixel in target file
     * @param   square          intermediate computation value
     * @param   dmean           partial derivatives of mean with respect to parameters
     * @param   dsquare         partial derivatives of square
     * @param   emean           second partial derivatives of mean with respect to parameters
     * @param   esquare         second partial derivatives of square
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  This routine computes the first and second derivatives of the correlation raio with
     *                          respect to all external parameters.
     *
     *                          <p>The matrix indices have been adjusted to allow for a more orderly relationship
     *                          between the matrix and its elements</p>
     *
     *                          <p>Returns the correlation ratio</p>
     *
     * @return  DOCUMENT ME!
     */

    double uvderivsCOR12(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                         float[] pixel2, int pixel2XDim, int pixel2YDim, int pixel2ZDim, float[] pixel5,
                         boolean[] pixel5Mask, int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5,
                         double[] dcff, double[][] ecff, int[] count, double[] mean, double[] square, double[][] dmean,
                         double[][] dsquare, double[][][] emean, double[][][] esquare, int partitions,
                         double maxActualValue) {

        double cf = 0.0;
        double[] dcf = new double[12];
        double[][] ecf = new double[12][12];
        double[] dxyz = new double[4];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double evaluets;
        int counttotal;
        double meantotal;

        int jj; /*index of current partition*/
        int pix1; /*int value of current pixel in standard file*/
        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double value; /*intermediate value*/
        double[] dvalue = new double[12]; /*partial derivatives of value with respect to parameters*/

        double sd2;
        double[] dsd2 = new double[12];
        double[][] esd2 = new double[12][12];

        int i, j, k;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_dim1, y_dim1, z_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        double e00, e01, e02, e10, e11, e12, e20, e21, e22, e30, e31, e32;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4, expix4, eypix4, ezpix4;
        int counts;
        double means;
        double sds;
        double[] dsds = new double[12];
        double[][] esds = new double[12][12];
        double temp;
        float pix4Min;

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < 12; t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] = 0;
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        z_dim1 = pixel2ZDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];

        pix4Min = Float.MAX_VALUE;

        for (i = 0; i < pixel2.length; i++) {

            if (pixel2[i] < pix4Min) {
                pix4Min = pixel2[i];
            }
        }

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + (k * e20) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + (k * e21) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            z_i = (i * e02) + (j * e12) + (k * e22) + e32;

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

            /* Notice that dx[0], dx[1], dx[2], dx[3] are equal to dy[4], dy[5], dy[6], dy[7]
             * and to dz[8], dz[9], dz[10], dz[11].  Consequently, these are all treated as dxyz[0], dxyz[1], dxyz[2],
             * and dxyz[3]. */
            dxyz[0] = (double) i;
            dxyz[1] = (double) j;
            dxyz[2] = (double) k;
            dxyz[3] = 1.0;

            /*Get the coordinates of the 8 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);
            z_up = (int) Math.ceil(z_i);
            z_down = (int) Math.floor(z_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            if (z_up == z_down) {

                if (z_up < z_max1) {
                    z_up++;
                } else {
                    z_down--;
                }
            }

            c = z_i - z_down;
            f = z_up - z_i;

            /*Get the values of these 8 voxels*/

            n0 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_up];
            n4 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_down];
            n5 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_up];
            n6 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_down];
            n7 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - threshold5) / (maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e * f) + (n1 * a * e * f) + (n2 * d * b * f) + (n3 * a * b * f) + (n4 * d * e * c) +
                   (n5 * a * e * c) + (n6 * d * b * c) + (n7 * a * b * c);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = (((e * f) * (n1 - n0)) + ((b * f) * (n3 - n2)) + ((c * e) * (n5 - n4)) + ((b * c) * (n7 - n6)));
            dypix4 = (((d * f) * (n2 - n0)) + ((a * f) * (n3 - n1)) + ((c * d) * (n6 - n4)) + ((a * c) * (n7 - n5)));
            dzpix4 = (((d * e) * (n4 - n0)) + ((a * e) * (n5 - n1)) + ((b * d) * (n6 - n2)) + ((a * b) * (n7 - n3)));

            expix4 = (((n7 + n1 - n5 - n3) * a) + ((n6 + n0 - n4 - n2) * d));
            eypix4 = (((n7 + n2 - n6 - n3) * b) + ((n5 + n0 - n4 - n1) * e));
            ezpix4 = (((n7 + n4 - n6 - n5) * c) + ((n3 + n0 - n2 - n1) * f));

            /*Calculate the value of the reslice voxel above the minimum */
            value = pix4 - pix4Min;
            mean[pix1] += value;
            square[pix1] += value * value;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */

            for (t = 0; t < 4; t++) {
                dvalue[t] = dxpix4 * dxyz[t];
                dmean[pix1][t] += dvalue[t];
                dsquare[pix1][t] += 2 * value * dvalue[t];
                dvalue[t + 4] = dypix4 * dxyz[t];
                dmean[pix1][t + 4] += dvalue[t + 4];
                dsquare[pix1][t + 4] += 2 * value * dvalue[t + 4];
                dvalue[t + 8] = dzpix4 * dxyz[t];
                dmean[pix1][t + 8] += dvalue[t + 8];
                dsquare[pix1][t + 8] += 2 * value * dvalue[t + 8];
            }

            /* Second derivatives */

            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dvalue[s] * dvalue[t]);
                }
            }

            for (; t < 8; t++) {

                for (s = 0; s < 4; s++) {
                    evaluets = ezpix4 * dxyz[t - 4] * dxyz[s];
                    emean[pix1][t][s] += evaluets;
                    esquare[pix1][t][s] += 2 * ((dvalue[s] * dvalue[t]) + (value * evaluets));
                }

                for (; s <= t; s++) {
                    esquare[pix1][t][s] += 2 * (dvalue[s] * dvalue[t]);
                }
            }

            for (; t < 12; t++) {

                for (s = 0; s < 4; s++) {
                    evaluets = eypix4 * dxyz[t - 8] * dxyz[s];
                    emean[pix1][t][s] += evaluets;
                    esquare[pix1][t][s] += 2 * ((dvalue[s] * dvalue[t]) + (value * evaluets));
                }

                for (; s < 8; s++) {
                    evaluets = expix4 * dxyz[t - 8] * dxyz[s - 4];
                    emean[pix1][t][s] += evaluets;
                    esquare[pix1][t][s] += 2 * ((dvalue[s] * dvalue[t]) + (value * evaluets));
                }

                for (; s <= t; s++) {
                    esquare[pix1][t][s] += 2 * (dvalue[s] * dvalue[t]);
                }
            }
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        /*Initialize values*/
        meantotal = 0;
        counttotal = 0;
        cf = 0;

        for (t = 0; t < 12; t++) {
            dcf[t] = 0;

            for (s = 0; s < 12; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Calculate normalized standard deviation for each partition*/


        for (jj = 0; jj < partitions; jj++) {

            /*Avoid subsequent NaN errors*/
            if (count[jj] <= 1) {
                continue;
            }

            counts = count[jj];

            means = mean[jj] / counts;

            for (t = 0; t < 12; t++) {
                dmean[jj][t] /= counts;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] /= counts;
                }
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0.0) {
                continue;
            }

            for (t = 0; t < 12; t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] -
                                  (2 * counts * ((means * emean[jj][t][s]) + (dmean[jj][s] * dmean[jj][t])))) /
                                     (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < 12; t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < 12; t++) {

                for (s = 0; s <= t; s++) {

                    esd2[t][s] = (means * dsd2[s]) + (2 * sd2 * dmean[jj][s]);
                    esd2[t][s] /= (2 * means * means * means * sd2 * sd2);
                    esd2[t][s] *= (-dsds[t]);
                    esd2[t][s] += esds[t][s] / (2 * means * means * sd2);
                    esd2[t][s] -= ((sd2 * emean[jj][t][s]) + (dsd2[s] * dmean[jj][t])) / means;
                    esd2[t][s] += sd2 * dmean[jj][t] * dmean[jj][s] / (means * means);
                }
            }

            /*Add weighted normalized standard deviation  of this partitions*/

            /* to cumulative total for all partitions*/

            cf += sd2 * counts;
            meantotal += means * counts;

            for (t = 0; t < 12; t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Renormalize cumulated values*/

        cf /= counttotal;
        meantotal = meantotal / counttotal;

        for (t = 0; t < 12; t++) {
            dcf[t] = dcf[t] / counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = ecf[t][s] / counttotal;
            }
        }

        /* Fill in redundant matrix elements*/
        for (t = 0; t < 12; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[2][0][t];
            dcff[t] += dcf[3] * des[3][0][t];
            dcff[t] += dcf[4] * des[0][1][t];
            dcff[t] += dcf[5] * des[1][1][t];
            dcff[t] += dcf[6] * des[2][1][t];
            dcff[t] += dcf[7] * des[3][1][t];
            dcff[t] += dcf[8] * des[0][2][t];
            dcff[t] += dcf[9] * des[1][2][t];
            dcff[t] += dcf[10] * des[2][2][t];
            dcff[t] += dcf[11] * des[3][2][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[2][0][s];
                temp += ecf[0][3] * des[3][0][s];
                temp += ecf[0][4] * des[0][1][s];
                temp += ecf[0][5] * des[1][1][s];
                temp += ecf[0][6] * des[2][1][s];
                temp += ecf[0][7] * des[3][1][s];
                temp += ecf[0][8] * des[0][2][s];
                temp += ecf[0][9] * des[1][2][s];
                temp += ecf[0][10] * des[2][2][s];
                temp += ecf[0][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[2][0][s];
                temp += ecf[1][3] * des[3][0][s];
                temp += ecf[1][4] * des[0][1][s];
                temp += ecf[1][5] * des[1][1][s];
                temp += ecf[1][6] * des[2][1][s];
                temp += ecf[1][7] * des[3][1][s];
                temp += ecf[1][8] * des[0][2][s];
                temp += ecf[1][9] * des[1][2][s];
                temp += ecf[1][10] * des[2][2][s];
                temp += ecf[1][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[2][0][s];
                temp += ecf[2][3] * des[3][0][s];
                temp += ecf[2][4] * des[0][1][s];
                temp += ecf[2][5] * des[1][1][s];
                temp += ecf[2][6] * des[2][1][s];
                temp += ecf[2][7] * des[3][1][s];
                temp += ecf[2][8] * des[0][2][s];
                temp += ecf[2][9] * des[1][2][s];
                temp += ecf[2][10] * des[2][2][s];
                temp += ecf[2][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][0][t]) + (dcf[2] * ees[2][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[2][0][s];
                temp += ecf[3][3] * des[3][0][s];
                temp += ecf[3][4] * des[0][1][s];
                temp += ecf[3][5] * des[1][1][s];
                temp += ecf[3][6] * des[2][1][s];
                temp += ecf[3][7] * des[3][1][s];
                temp += ecf[3][8] * des[0][2][s];
                temp += ecf[3][9] * des[1][2][s];
                temp += ecf[3][10] * des[2][2][s];
                temp += ecf[3][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[3] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[2][0][s];
                temp += ecf[4][3] * des[3][0][s];
                temp += ecf[4][4] * des[0][1][s];
                temp += ecf[4][5] * des[1][1][s];
                temp += ecf[4][6] * des[2][1][s];
                temp += ecf[4][7] * des[3][1][s];
                temp += ecf[4][8] * des[0][2][s];
                temp += ecf[4][9] * des[1][2][s];
                temp += ecf[4][10] * des[2][2][s];
                temp += ecf[4][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[4] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[2][0][s];
                temp += ecf[5][3] * des[3][0][s];
                temp += ecf[5][4] * des[0][1][s];
                temp += ecf[5][5] * des[1][1][s];
                temp += ecf[5][6] * des[2][1][s];
                temp += ecf[5][7] * des[3][1][s];
                temp += ecf[5][8] * des[0][2][s];
                temp += ecf[5][9] * des[1][2][s];
                temp += ecf[5][10] * des[2][2][s];
                temp += ecf[5][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[5] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[6][0] * des[0][0][s];
                temp += ecf[6][1] * des[1][0][s];
                temp += ecf[6][2] * des[2][0][s];
                temp += ecf[6][3] * des[3][0][s];
                temp += ecf[6][4] * des[0][1][s];
                temp += ecf[6][5] * des[1][1][s];
                temp += ecf[6][6] * des[2][1][s];
                temp += ecf[6][7] * des[3][1][s];
                temp += ecf[6][8] * des[0][2][s];
                temp += ecf[6][9] * des[1][2][s];
                temp += ecf[6][10] * des[2][2][s];
                temp += ecf[6][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][1][t]) + (dcf[6] * ees[2][1][t][s]);
                temp = 0;
                temp += ecf[7][0] * des[0][0][s];
                temp += ecf[7][1] * des[1][0][s];
                temp += ecf[7][2] * des[2][0][s];
                temp += ecf[7][3] * des[3][0][s];
                temp += ecf[7][4] * des[0][1][s];
                temp += ecf[7][5] * des[1][1][s];
                temp += ecf[7][6] * des[2][1][s];
                temp += ecf[7][7] * des[3][1][s];
                temp += ecf[7][8] * des[0][2][s];
                temp += ecf[7][9] * des[1][2][s];
                temp += ecf[7][10] * des[2][2][s];
                temp += ecf[7][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[7] * ees[3][1][t][s]);
                temp = 0;
                temp += ecf[8][0] * des[0][0][s];
                temp += ecf[8][1] * des[1][0][s];
                temp += ecf[8][2] * des[2][0][s];
                temp += ecf[8][3] * des[3][0][s];
                temp += ecf[8][4] * des[0][1][s];
                temp += ecf[8][5] * des[1][1][s];
                temp += ecf[8][6] * des[2][1][s];
                temp += ecf[8][7] * des[3][1][s];
                temp += ecf[8][8] * des[0][2][s];
                temp += ecf[8][9] * des[1][2][s];
                temp += ecf[8][10] * des[2][2][s];
                temp += ecf[8][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][2][t]) + (dcf[8] * ees[0][2][t][s]);
                temp = 0;
                temp += ecf[9][0] * des[0][0][s];
                temp += ecf[9][1] * des[1][0][s];
                temp += ecf[9][2] * des[2][0][s];
                temp += ecf[9][3] * des[3][0][s];
                temp += ecf[9][4] * des[0][1][s];
                temp += ecf[9][5] * des[1][1][s];
                temp += ecf[9][6] * des[2][1][s];
                temp += ecf[9][7] * des[3][1][s];
                temp += ecf[9][8] * des[0][2][s];
                temp += ecf[9][9] * des[1][2][s];
                temp += ecf[9][10] * des[2][2][s];
                temp += ecf[9][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][2][t]) + (dcf[9] * ees[1][2][t][s]);
                temp = 0;
                temp += ecf[10][0] * des[0][0][s];
                temp += ecf[10][1] * des[1][0][s];
                temp += ecf[10][2] * des[2][0][s];
                temp += ecf[10][3] * des[3][0][s];
                temp += ecf[10][4] * des[0][1][s];
                temp += ecf[10][5] * des[1][1][s];
                temp += ecf[10][6] * des[2][1][s];
                temp += ecf[10][7] * des[3][1][s];
                temp += ecf[10][8] * des[0][2][s];
                temp += ecf[10][9] * des[1][2][s];
                temp += ecf[10][10] * des[2][2][s];
                temp += ecf[10][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][2][t]) + (dcf[10] * ees[2][2][t][s]);
                temp = 0;
                temp += ecf[11][0] * des[0][0][s];
                temp += ecf[11][1] * des[1][0][s];
                temp += ecf[11][2] * des[2][0][s];
                temp += ecf[11][3] * des[3][0][s];
                temp += ecf[11][4] * des[0][1][s];
                temp += ecf[11][5] * des[1][1][s];
                temp += ecf[11][6] * des[2][1][s];
                temp += ecf[11][7] * des[3][1][s];
                temp += ecf[11][8] * des[0][2][s];
                temp += ecf[11][9] * des[1][2][s];
                temp += ecf[11][10] * des[2][2][s];
                temp += ecf[11][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][2][t]) + (dcf[11] * ees[3][2][t][s]);
            }
        }

        return cf;
    }

    /**
     * double uvderivsCOR6().
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              source parameters([4][3])
     * @param   des             derivatives of each source parameter with respect to each element
     * @param   ees             second derivatives as above
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2XDim      DOCUMENT ME!
     * @param   pixel2YDim      DOCUMENT ME!
     * @param   pixel5          interpolated values of target file
     * @param   pixel5Mask      mask of interpolated values of target file
     * @param   pixel5XDim      DOCUMENT ME!
     * @param   pixel5YDim      DOCUMENT ME!
     * @param   pixel5ZDim      DOCUMENT ME!
     * @param   threshold5      DOCUMENT ME!
     * @param   dcff            first derivatives of sd with respect to parameters
     * @param   ecff            second derivatives of sd with respect to parameters
     * @param   count           number of pixels in partitions that are above threshold
     * @param   mean            ratio of pixel in source file/corresponding pixel in target file
     * @param   square          intermediate computation value
     * @param   dmean           partial derivatives of mean with respect to parameters
     * @param   dsquare         partial derivatives of square
     * @param   emean           second partial derivatives of mean with respect to parameters
     * @param   esquare         second partial derivatives of square
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  DOCUMENT ME! This routine computes the first and second derivatives of the correlation
     *                          ratio with respect to all external parameters.
     *
     *                          <p>The matrix indices have been adjusted to allow for a more orderly relationship
     *                          between the matrix and its elements</p>
     *
     *                          <p>Returns the correlation ratio</p>
     *
     * @return  DOCUMENT ME!
     */

    double uvderivsCOR6(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                        float[] pixel2, int pixel2XDim, int pixel2YDim, float[] pixel5, boolean[] pixel5Mask,
                        int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5, double[] dcff,
                        double[][] ecff, int[] count, double[] mean, double[] square, double[][] dmean,
                        double[][] dsquare, double[][][] emean, double[][][] esquare, int partitions,
                        double maxActualValue) {

        double cf = 0.0;
        double[] dcf = new double[6];
        double[][] ecf = new double[6][6];
        double[] dxy = new double[3];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double evaluets;
        int counttotal;
        double meantotal;

        int jj; /*index of current partition*/
        int pix1; /*int value of current pixel in target file*/
        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in target file*/
        double pix4; /*calculated value of current pixel in target file*/
        double value; /*intermediate value*/
        double[] dvalue = new double[6]; /*partial derivatives of value with respect to parameters*/

        double sd2;
        double[] dsd2 = new double[6];
        double[][] esd2 = new double[6][6];

        int i, j, k;
        int s, t;
        int x_max1, y_max1;
        int x_dim1, y_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        double e00, e01, e10, e11, e30, e31;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4, ezpix4;
        int counts;
        double means;
        double sds;
        double[] dsds = new double[6];
        double[][] esds = new double[6][6];
        double temp;
        float pix4Min;

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < 6; t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] = 0;
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e10 = es[1][0];
        e11 = es[1][1];
        e30 = es[3][0];
        e31 = es[3][1];

        pix4Min = Float.MAX_VALUE;

        for (i = 0; i < pixel2.length; i++) {

            if (pixel2[i] < pix4Min) {
                pix4Min = pixel2[i];
            }
        }

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            /* Note that dx[0], dx[1], and dx[2] are equal to dy[3], dy[4], and dy[5] respectively.
             * Consequently, these are all treated as dxy[0], dxy[1], and dxy[2] */

            dxy[0] = (double) i;
            dxy[1] = (double) j;
            dxy[2] = 1.0;

            /* Second derivatives are all zero */

            /*Get the coordinates of the 4 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            /*Get the values of these 4 voxels*/

            n0 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - threshold5) / (maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            ezpix4 = n3 + n0 - n2 - n1;

            /*Calculate the value of the reslice pixel above the minimum */
            value = pix4 - pix4Min;
            mean[pix1] += value;
            square[pix1] += value * value;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */

            for (t = 0; t < 3; t++) {
                dvalue[t] = dxpix4 * dxy[t];
                dmean[pix1][t] += dvalue[t];
                dsquare[pix1][t] += 2 * value * dvalue[t];
                dvalue[t + 3] = dypix4 * dxy[t];
                dmean[pix1][t + 3] += dvalue[t + 3];
                dsquare[pix1][t + 3] += 2 * value * dvalue[t + 3];
            }

            /* Second derivatives */
            for (t = 0; t < 6; t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dvalue[s] * dvalue[t]);
                }
            }

            for (t = 3; t < 6; t++) {

                for (s = 0; s < 3; s++) {
                    evaluets = ezpix4 * dxy[t - 3] * dxy[s];
                    emean[pix1][t][s] += evaluets;
                    esquare[pix1][t][s] += 2.0 * (value * evaluets);
                }

            }
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        /*Initialize values*/
        meantotal = 0;
        counttotal = 0;
        cf = 0;

        for (t = 0; t < 6; t++) {
            dcf[t] = 0;

            for (s = 0; s < 6; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Calculate normalized standard deviation for each partition*/


        for (jj = 0; jj < partitions; jj++) {

            /*Avoid subsequent NaN errors*/
            if (count[jj] <= 1) {
                continue;
            }

            counts = count[jj];

            means = mean[jj] / counts;

            for (t = 0; t < 6; t++) {
                dmean[jj][t] /= counts;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] /= counts;
                }
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0) {
                continue;
            }

            for (t = 0; t < 6; t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] -
                                  (2 * counts * ((means * emean[jj][t][s]) + (dmean[jj][s] * dmean[jj][t])))) /
                                     (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < 6; t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < 6; t++) {

                for (s = 0; s <= t; s++) {

                    esd2[t][s] = (means * dsd2[s]) + (2 * sd2 * dmean[jj][s]);
                    esd2[t][s] /= (2 * means * means * means * sd2 * sd2);
                    esd2[t][s] *= (-dsds[t]);
                    esd2[t][s] += esds[t][s] / (2 * means * means * sd2);
                    esd2[t][s] -= ((sd2 * emean[jj][t][s]) + (dsd2[s] * dmean[jj][t])) / means;
                    esd2[t][s] += sd2 * dmean[jj][t] * dmean[jj][s] / (means * means);
                }
            }

            /*Add weighted normalized standard deviation  of this partitions*/

            /* to cumulative total for all partitions*/

            cf += sd2 * counts;
            meantotal += means * counts;

            for (t = 0; t < 6; t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Renormalize cumulated values*/

        cf /= counttotal;
        meantotal = meantotal / counttotal;

        for (t = 0; t < 6; t++) {
            dcf[t] = dcf[t] / counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = ecf[t][s] / counttotal;
            }
        }

        /* Fill in redundant matrix elements*/
        for (t = 0; t < 6; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[3][0][t];
            dcff[t] += dcf[3] * des[0][1][t];
            dcff[t] += dcf[4] * des[1][1][t];
            dcff[t] += dcf[5] * des[3][1][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[3][0][s];
                temp += ecf[0][3] * des[0][1][s];
                temp += ecf[0][4] * des[1][1][s];
                temp += ecf[0][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[3][0][s];
                temp += ecf[1][3] * des[0][1][s];
                temp += ecf[1][4] * des[1][1][s];
                temp += ecf[1][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[3][0][s];
                temp += ecf[2][3] * des[0][1][s];
                temp += ecf[2][4] * des[1][1][s];
                temp += ecf[2][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[2] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[3][0][s];
                temp += ecf[3][3] * des[0][1][s];
                temp += ecf[3][4] * des[1][1][s];
                temp += ecf[3][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[3] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[3][0][s];
                temp += ecf[4][3] * des[0][1][s];
                temp += ecf[4][4] * des[1][1][s];
                temp += ecf[4][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[4] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[3][0][s];
                temp += ecf[5][3] * des[0][1][s];
                temp += ecf[5][4] * des[1][1][s];
                temp += ecf[5][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[5] * ees[3][1][t][s]);

            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            source parameters([4][3])
     * @param   des           derivatives of each source parameter with respect to each element
     * @param   ees           second derivatives as above
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2XDim    DOCUMENT ME!
     * @param   pixel2YDim    DOCUMENT ME!
     * @param   pixel2ZDim    DOCUMENT ME!
     * @param   pixel5        interpolated values of target file
     * @param   pixel5Mask    mask of interpolated values of target file
     * @param   pixel5XDim    DOCUMENT ME!
     * @param   pixel5YDim    DOCUMENT ME!
     * @param   pixel5ZDim    DOCUMENT ME!
     * @param   threshold5    DOCUMENT ME!
     * @param   dcff          first derivatives of sd with respect to parameters
     * @param   ecff          second derivatives of sd with respect to parameters This routine computes the first and
     *                        second derivatives of the sum of squares with respect to all external parameters.
     *
     *                        <p>The matrix indices have been adjusted to allow for a more orderly relationship between
     *                        the matrix and its elements</p>
     *
     *                        <p>Returns the sum of squares</p>
     *
     * @return  DOCUMENT ME!
     */

    double uvderivsLS12(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                        float[] pixel2, int pixel2XDim, int pixel2YDim, int pixel2ZDim, float[] pixel5,
                        boolean[] pixel5Mask, int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5,
                        double[] dcff, double[][] ecff) {

        double cf = 0.0;
        double[] dcf = new double[12];
        double[][] ecf = new double[12][12];
        double[] dxyz = new double[4];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double[][] dydx = new double[4][4];
        double[][] dxdx = new double[4][4];
        int counttotal;

        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double ratio; /*intermediate value*/

        int i, j, k;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_dim1, y_dim1, z_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        double e00, e01, e02, e10, e11, e12, e20, e21, e22, e30, e31, e32;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4, expix4, eypix4, ezpix4;
        double temp;

        /*Initialize values*/

        counttotal = 0;
        ratio = 0;

        for (t = 0; t < 12; t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        x_dim1 = pixel2XDim;
        ;
        y_dim1 = pixel2YDim;
        z_dim1 = pixel2ZDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + (k * e20) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + (k * e21) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            z_i = (i * e02) + (j * e12) + (k * e22) + e32;

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

            /* Notice that dx[0], dx[1], dx[2], dx[3] are equal to dy[4], dy[5], dy[6], dy[7]
             * and to dz[8], dz[9], dz[10], dz[11].  Consequently, these are all treated as dxyz[0], dxyz[1], dxyz[2],
             * and dxyz[3]. */
            dxyz[0] = (double) i;
            dxyz[1] = (double) j;
            dxyz[2] = (double) k;
            dxyz[3] = 1.0;

            /*Get the coordinates of the 8 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);
            z_up = (int) Math.ceil(z_i);
            z_down = (int) Math.floor(z_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            if (z_up == z_down) {

                if (z_up < z_max1) {
                    z_up++;
                } else {
                    z_down--;
                }
            }

            c = z_i - z_down;
            f = z_up - z_i;

            /*Get the values of these 8 voxels*/

            n0 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_up];
            n4 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_down];
            n5 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_up];
            n6 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_down];
            n7 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e * f) + (n1 * a * e * f) + (n2 * d * b * f) + (n3 * a * b * f) + (n4 * d * e * c) +
                   (n5 * a * e * c) + (n6 * d * b * c) + (n7 * a * b * c);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = (((e * f) * (n1 - n0)) + ((b * f) * (n3 - n2)) + ((c * e) * (n5 - n4)) + ((b * c) * (n7 - n6)));
            dypix4 = (((d * f) * (n2 - n0)) + ((a * f) * (n3 - n1)) + ((c * d) * (n6 - n4)) + ((a * c) * (n7 - n5)));
            dzpix4 = (((d * e) * (n4 - n0)) + ((a * e) * (n5 - n1)) + ((b * d) * (n6 - n2)) + ((a * b) * (n7 - n3)));

            expix4 = (((n7 + n1 - n5 - n3) * a) + ((n6 + n0 - n4 - n2) * d));
            eypix4 = (((n7 + n2 - n6 - n3) * b) + ((n5 + n0 - n4 - n1) * e));
            ezpix4 = (((n7 + n4 - n6 - n5) * c) + ((n3 + n0 - n2 - n1) * f));

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * pix3) + (pix3 * pix3);

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 4; t++) {
                dcf[t] += 2.0 * (pix4 - pix3) * dxpix4 * dxyz[t];
                dcf[t + 4] += 2.0 * (pix4 - pix3) * dypix4 * dxyz[t];
                dcf[t + 8] += 2.0 * (pix4 - pix3) * dzpix4 * dxyz[t];
            }

            /* Second derivatives */
            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    dxdx[t][s] = dydx[t][s] = 2.0 * dxyz[t] * dxyz[s];
                }

                for (; s < 4; s++) {
                    dydx[t][s] = 2.0 * dxyz[t] * dxyz[s];
                }
            }

            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += dxdx[t][s] * dxpix4 * dxpix4;
                }
            }

            for (; t < 8; t++) {

                for (s = 0; s < 4; s++) {
                    ecf[t][s] += dydx[t - 4][s] * ((dxpix4 * dypix4) + ((pix4 - pix3) * ezpix4));
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 4][s - 4] * dypix4 * dypix4;
                }
            }

            for (; t < 12; t++) {

                for (s = 0; s < 4; s++) {
                    ecf[t][s] += dydx[t - 8][s] * ((dxpix4 * dzpix4) + ((pix4 - pix3) * eypix4));
                }

                for (; s < 8; s++) {
                    ecf[t][s] += dydx[t - 8][s - 4] * ((dypix4 * dzpix4) + ((pix4 - pix3) * expix4));
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 8][s - 8] * dzpix4 * dzpix4;
                }
            }

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < 12; t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        /*Fill in redundant matrix elements*/
        for (t = 0; t < 12; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[2][0][t];
            dcff[t] += dcf[3] * des[3][0][t];
            dcff[t] += dcf[4] * des[0][1][t];
            dcff[t] += dcf[5] * des[1][1][t];
            dcff[t] += dcf[6] * des[2][1][t];
            dcff[t] += dcf[7] * des[3][1][t];
            dcff[t] += dcf[8] * des[0][2][t];
            dcff[t] += dcf[9] * des[1][2][t];
            dcff[t] += dcf[10] * des[2][2][t];
            dcff[t] += dcf[11] * des[3][2][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[2][0][s];
                temp += ecf[0][3] * des[3][0][s];
                temp += ecf[0][4] * des[0][1][s];
                temp += ecf[0][5] * des[1][1][s];
                temp += ecf[0][6] * des[2][1][s];
                temp += ecf[0][7] * des[3][1][s];
                temp += ecf[0][8] * des[0][2][s];
                temp += ecf[0][9] * des[1][2][s];
                temp += ecf[0][10] * des[2][2][s];
                temp += ecf[0][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[2][0][s];
                temp += ecf[1][3] * des[3][0][s];
                temp += ecf[1][4] * des[0][1][s];
                temp += ecf[1][5] * des[1][1][s];
                temp += ecf[1][6] * des[2][1][s];
                temp += ecf[1][7] * des[3][1][s];
                temp += ecf[1][8] * des[0][2][s];
                temp += ecf[1][9] * des[1][2][s];
                temp += ecf[1][10] * des[2][2][s];
                temp += ecf[1][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[2][0][s];
                temp += ecf[2][3] * des[3][0][s];
                temp += ecf[2][4] * des[0][1][s];
                temp += ecf[2][5] * des[1][1][s];
                temp += ecf[2][6] * des[2][1][s];
                temp += ecf[2][7] * des[3][1][s];
                temp += ecf[2][8] * des[0][2][s];
                temp += ecf[2][9] * des[1][2][s];
                temp += ecf[2][10] * des[2][2][s];
                temp += ecf[2][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][0][t]) + (dcf[2] * ees[2][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[2][0][s];
                temp += ecf[3][3] * des[3][0][s];
                temp += ecf[3][4] * des[0][1][s];
                temp += ecf[3][5] * des[1][1][s];
                temp += ecf[3][6] * des[2][1][s];
                temp += ecf[3][7] * des[3][1][s];
                temp += ecf[3][8] * des[0][2][s];
                temp += ecf[3][9] * des[1][2][s];
                temp += ecf[3][10] * des[2][2][s];
                temp += ecf[3][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[3] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[2][0][s];
                temp += ecf[4][3] * des[3][0][s];
                temp += ecf[4][4] * des[0][1][s];
                temp += ecf[4][5] * des[1][1][s];
                temp += ecf[4][6] * des[2][1][s];
                temp += ecf[4][7] * des[3][1][s];
                temp += ecf[4][8] * des[0][2][s];
                temp += ecf[4][9] * des[1][2][s];
                temp += ecf[4][10] * des[2][2][s];
                temp += ecf[4][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[4] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[2][0][s];
                temp += ecf[5][3] * des[3][0][s];
                temp += ecf[5][4] * des[0][1][s];
                temp += ecf[5][5] * des[1][1][s];
                temp += ecf[5][6] * des[2][1][s];
                temp += ecf[5][7] * des[3][1][s];
                temp += ecf[5][8] * des[0][2][s];
                temp += ecf[5][9] * des[1][2][s];
                temp += ecf[5][10] * des[2][2][s];
                temp += ecf[5][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[5] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[6][0] * des[0][0][s];
                temp += ecf[6][1] * des[1][0][s];
                temp += ecf[6][2] * des[2][0][s];
                temp += ecf[6][3] * des[3][0][s];
                temp += ecf[6][4] * des[0][1][s];
                temp += ecf[6][5] * des[1][1][s];
                temp += ecf[6][6] * des[2][1][s];
                temp += ecf[6][7] * des[3][1][s];
                temp += ecf[6][8] * des[0][2][s];
                temp += ecf[6][9] * des[1][2][s];
                temp += ecf[6][10] * des[2][2][s];
                temp += ecf[6][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][1][t]) + (dcf[6] * ees[2][1][t][s]);
                temp = 0;
                temp += ecf[7][0] * des[0][0][s];
                temp += ecf[7][1] * des[1][0][s];
                temp += ecf[7][2] * des[2][0][s];
                temp += ecf[7][3] * des[3][0][s];
                temp += ecf[7][4] * des[0][1][s];
                temp += ecf[7][5] * des[1][1][s];
                temp += ecf[7][6] * des[2][1][s];
                temp += ecf[7][7] * des[3][1][s];
                temp += ecf[7][8] * des[0][2][s];
                temp += ecf[7][9] * des[1][2][s];
                temp += ecf[7][10] * des[2][2][s];
                temp += ecf[7][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[7] * ees[3][1][t][s]);
                temp = 0;
                temp += ecf[8][0] * des[0][0][s];
                temp += ecf[8][1] * des[1][0][s];
                temp += ecf[8][2] * des[2][0][s];
                temp += ecf[8][3] * des[3][0][s];
                temp += ecf[8][4] * des[0][1][s];
                temp += ecf[8][5] * des[1][1][s];
                temp += ecf[8][6] * des[2][1][s];
                temp += ecf[8][7] * des[3][1][s];
                temp += ecf[8][8] * des[0][2][s];
                temp += ecf[8][9] * des[1][2][s];
                temp += ecf[8][10] * des[2][2][s];
                temp += ecf[8][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][2][t]) + (dcf[8] * ees[0][2][t][s]);
                temp = 0;
                temp += ecf[9][0] * des[0][0][s];
                temp += ecf[9][1] * des[1][0][s];
                temp += ecf[9][2] * des[2][0][s];
                temp += ecf[9][3] * des[3][0][s];
                temp += ecf[9][4] * des[0][1][s];
                temp += ecf[9][5] * des[1][1][s];
                temp += ecf[9][6] * des[2][1][s];
                temp += ecf[9][7] * des[3][1][s];
                temp += ecf[9][8] * des[0][2][s];
                temp += ecf[9][9] * des[1][2][s];
                temp += ecf[9][10] * des[2][2][s];
                temp += ecf[9][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][2][t]) + (dcf[9] * ees[1][2][t][s]);
                temp = 0;
                temp += ecf[10][0] * des[0][0][s];
                temp += ecf[10][1] * des[1][0][s];
                temp += ecf[10][2] * des[2][0][s];
                temp += ecf[10][3] * des[3][0][s];
                temp += ecf[10][4] * des[0][1][s];
                temp += ecf[10][5] * des[1][1][s];
                temp += ecf[10][6] * des[2][1][s];
                temp += ecf[10][7] * des[3][1][s];
                temp += ecf[10][8] * des[0][2][s];
                temp += ecf[10][9] * des[1][2][s];
                temp += ecf[10][10] * des[2][2][s];
                temp += ecf[10][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][2][t]) + (dcf[10] * ees[2][2][t][s]);
                temp = 0;
                temp += ecf[11][0] * des[0][0][s];
                temp += ecf[11][1] * des[1][0][s];
                temp += ecf[11][2] * des[2][0][s];
                temp += ecf[11][3] * des[3][0][s];
                temp += ecf[11][4] * des[0][1][s];
                temp += ecf[11][5] * des[1][1][s];
                temp += ecf[11][6] * des[2][1][s];
                temp += ecf[11][7] * des[3][1][s];
                temp += ecf[11][8] * des[0][2][s];
                temp += ecf[11][9] * des[1][2][s];
                temp += ecf[11][10] * des[2][2][s];
                temp += ecf[11][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][2][t]) + (dcf[11] * ees[3][2][t][s]);
            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            source parameters([4][3])
     * @param   des           derivatives of each source parameter with respect to each element
     * @param   ees           second derivatives as above
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2XDim    DOCUMENT ME!
     * @param   pixel2YDim    DOCUMENT ME!
     * @param   pixel5        interpolated values of target file
     * @param   pixel5Mask    mask of interpolated values of target file
     * @param   pixel5XDim    DOCUMENT ME!
     * @param   pixel5YDim    DOCUMENT ME!
     * @param   pixel5ZDim    DOCUMENT ME!
     * @param   threshold5    DOCUMENT ME!
     * @param   dcff          first derivatives of sd with respect to parameters
     * @param   ecff          second derivatives of sd with respect to parameters This routine computes the first and
     *                        second derivatives of the sum of squares with respect to all external parameters.
     *
     *                        <p>The matrix indices have been adjusted to allow for a more orderly relationship between
     *                        the matrix and its elements</p>
     *
     *                        <p>Returns the sum of squares</p>
     *
     * @return  DOCUMENT ME!
     */

    double uvderivsLS6(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                       float[] pixel2, int pixel2XDim, int pixel2YDim, float[] pixel5, boolean[] pixel5Mask,
                       int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5, double[] dcff,
                       double[][] ecff) {

        double cf = 0.0;
        double[] dcf = new double[6];
        double[][] ecf = new double[6][6];
        double[] dxy = new double[3];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        int counttotal;
        double[][] dydx = new double[3][3];
        double[][] dxdx = new double[3][3];

        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double ratio; /*intermediate value*/

        int i, j, k;
        int s, t;
        int x_max1, y_max1;
        int x_dim1, y_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        double e00, e01, e10, e11, e30, e31;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4, ezpix4;
        double temp;

        /*Initialize values*/

        counttotal = 0;
        ratio = 0;

        for (t = 0; t < 6; t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e10 = es[1][0];
        e11 = es[1][1];
        e30 = es[3][0];
        e31 = es[3][1];

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            /* Note that dx[0], dx[1], and dx[2] are equal to dy[3], dy[4], and dy[5] respectively.
             * Consequently, these are all treated as dxy[0], dxy[1], and dxy[2] */

            dxy[0] = (double) i;
            dxy[1] = (double) j;
            dxy[2] = 1.0;

            /* Second derivatives are all zero */

            /*Get the coordinates of the 4 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            /*Get the values of these 4 voxels*/

            n0 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            ezpix4 = n3 + n0 - n2 - n1;

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * pix3) + (pix3 * pix3);

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 3; t++) {
                dcf[t] += 2.0 * (pix4 - pix3) * dxpix4 * dxy[t];
                dcf[t + 3] += 2.0 * (pix4 - pix3) * dypix4 * dxy[t];
            }

            /* Second derivatives */
            for (t = 0; t < 3; t++) {

                for (s = 0; s <= t; s++) {
                    dxdx[t][s] = dydx[t][s] = 2.0 * dxy[t] * dxy[s];
                }

                for (; s < 3; s++) {
                    dydx[t][s] = 2.0 * dxy[t] * dxy[s];
                }
            }

            for (t = 0; t < 3; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += dxdx[t][s] * dxpix4 * dxpix4;
                }
            }

            for (; t < 6; t++) {

                for (s = 0; s < 3; s++) {
                    ecf[t][s] += dydx[t - 3][s] * ((dxpix4 * dypix4) + ((pix4 - pix3) * ezpix4));
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 3][s - 3] * dypix4 * dypix4;
                }
            }

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < 6; t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        /*Fill in redundant matrix elements*/
        for (t = 0; t < 6; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[3][0][t];
            dcff[t] += dcf[3] * des[0][1][t];
            dcff[t] += dcf[4] * des[1][1][t];
            dcff[t] += dcf[5] * des[3][1][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[3][0][s];
                temp += ecf[0][3] * des[0][1][s];
                temp += ecf[0][4] * des[1][1][s];
                temp += ecf[0][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[3][0][s];
                temp += ecf[1][3] * des[0][1][s];
                temp += ecf[1][4] * des[1][1][s];
                temp += ecf[1][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[3][0][s];
                temp += ecf[2][3] * des[0][1][s];
                temp += ecf[2][4] * des[1][1][s];
                temp += ecf[2][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[2] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[3][0][s];
                temp += ecf[3][3] * des[0][1][s];
                temp += ecf[3][4] * des[1][1][s];
                temp += ecf[3][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[3] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[3][0][s];
                temp += ecf[4][3] * des[0][1][s];
                temp += ecf[4][4] * des[1][1][s];
                temp += ecf[4][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[4] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[3][0][s];
                temp += ecf[5][3] * des[0][1][s];
                temp += ecf[5][4] * des[1][1][s];
                temp += ecf[5][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[5] * ees[3][1][t][s]);

            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              source parameters([4][3])
     * @param   des             derivatives of each source parameter with respect to each element
     * @param   ees             second derivatives as above
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2XDim      DOCUMENT ME!
     * @param   pixel2YDim      DOCUMENT ME!
     * @param   pixel2ZDim      DOCUMENT ME!
     * @param   pixel5          interpolated values of target file
     * @param   pixel5Mask      mask of interpolated values of target file
     * @param   pixel5XDim      DOCUMENT ME!
     * @param   pixel5YDim      DOCUMENT ME!
     * @param   pixel5ZDim      DOCUMENT ME!
     * @param   threshold5      DOCUMENT ME!
     * @param   dcff            first derivatives of sd with respect to parameters
     * @param   ecff            second derivatives of sd with respect to parameters
     * @param   count           number of pixels in partitions that are above threshold
     * @param   mean            ratio of pixel in source file/corresponding pixel in target file
     * @param   square          intermediate computation value
     * @param   dmean           partial derivatives of mean with respect to parameters
     * @param   dsquare         partial derivatives of square
     * @param   emean           second partial derivatives of mean with respect to parameters
     * @param   esquare         second partial derivatives of square
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  This routine computes the first and second derivatives of the normalized standard
     *                          deviation with respect to all external parameters.
     *
     *                          <p>The matrix indices have been adjusted to allow for a more orderly relationship
     *                          between the matrix and its elements</p>
     *
     *                          <p>Returns the normalized standard deviation</p>
     *
     * @return  DOCUMENT ME!
     */

    double uvderivsN12(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                       float[] pixel2, int pixel2XDim, int pixel2YDim, int pixel2ZDim, float[] pixel5,
                       boolean[] pixel5Mask, int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5,
                       double[] dcff, double[][] ecff, int[] count, double[] mean, double[] square, double[][] dmean,
                       double[][] dsquare, double[][][] emean, double[][][] esquare, int partitions,
                       double maxActualValue) {

        double cf = 0.0;
        double[] dcf = new double[12];
        double[][] ecf = new double[12][12];
        double[] dxyz = new double[4];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double eratiots;
        int counttotal;
        double meantotal;

        int jj; /*index of current partition*/
        int pix1; /*int value of current pixel in standard file*/
        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double ratio; /*intermediate value*/
        double[] dratio = new double[12]; /*partial derivatives of ratio with respect to parameters*/

        double sd2;
        double[] dsd2 = new double[12];
        double[][] esd2 = new double[12][12];

        int i, j, k;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_dim1, y_dim1, z_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        double e00, e01, e02, e10, e11, e12, e20, e21, e22, e30, e31, e32;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4, expix4, eypix4, ezpix4;
        int counts;
        double means;
        double sds;
        double[] dsds = new double[12];
        double[][] esds = new double[12][12];
        double temp;

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < 12; t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] = 0;
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        z_dim1 = pixel2ZDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + (k * e20) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + (k * e21) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            z_i = (i * e02) + (j * e12) + (k * e22) + e32;

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

            /* Notice that dx[0], dx[1], dx[2], dx[3] are equal to dy[4], dy[5], dy[6], dy[7]
             * and to dz[8], dz[9], dz[10], dz[11].  Consequently, these are all treated as dxyz[0], dxyz[1], dxyz[2],
             * and dxyz[3]. */
            dxyz[0] = (double) i;
            dxyz[1] = (double) j;
            dxyz[2] = (double) k;
            dxyz[3] = 1.0;

            /*Get the coordinates of the 8 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);
            z_up = (int) Math.ceil(z_i);
            z_down = (int) Math.floor(z_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            if (z_up == z_down) {

                if (z_up < z_max1) {
                    z_up++;
                } else {
                    z_down--;
                }
            }

            c = z_i - z_down;
            f = z_up - z_i;

            /*Get the values of these 8 voxels*/

            n0 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_up];
            n4 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_down];
            n5 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_up];
            n6 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_down];
            n7 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - threshold5) / (maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e * f) + (n1 * a * e * f) + (n2 * d * b * f) + (n3 * a * b * f) + (n4 * d * e * c) +
                   (n5 * a * e * c) + (n6 * d * b * c) + (n7 * a * b * c);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = (((e * f) * (n1 - n0)) + ((b * f) * (n3 - n2)) + ((c * e) * (n5 - n4)) + ((b * c) * (n7 - n6)));
            dypix4 = (((d * f) * (n2 - n0)) + ((a * f) * (n3 - n1)) + ((c * d) * (n6 - n4)) + ((a * c) * (n7 - n5)));
            dzpix4 = (((d * e) * (n4 - n0)) + ((a * e) * (n5 - n1)) + ((b * d) * (n6 - n2)) + ((a * b) * (n7 - n3)));

            expix4 = (((n7 + n1 - n5 - n3) * a) + ((n6 + n0 - n4 - n2) * d));
            eypix4 = (((n7 + n2 - n6 - n3) * b) + ((n5 + n0 - n4 - n1) * e));
            ezpix4 = (((n7 + n4 - n6 - n5) * c) + ((n3 + n0 - n2 - n1) * f));

            /*Calculate the ratio of the reslice value to the standard value*/
            ratio = pix4 / pix3;
            mean[pix1] += ratio;
            square[pix1] += ratio * ratio;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */

            for (t = 0; t < 4; t++) {
                dratio[t] = dxpix4 * dxyz[t] / pix3;
                dmean[pix1][t] += dratio[t];
                dsquare[pix1][t] += 2 * ratio * dratio[t];
                dratio[t + 4] = dypix4 * dxyz[t] / pix3;
                dmean[pix1][t + 4] += dratio[t + 4];
                dsquare[pix1][t + 4] += 2 * ratio * dratio[t + 4];
                dratio[t + 8] = dzpix4 * dxyz[t] / pix3;
                dmean[pix1][t + 8] += dratio[t + 8];
                dsquare[pix1][t + 8] += 2 * ratio * dratio[t + 8];
            }

            /* Second derivatives */

            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dratio[s] * dratio[t]);
                }
            }

            for (; t < 8; t++) {

                for (s = 0; s < 4; s++) {
                    eratiots = ezpix4 * dxyz[t - 4] * dxyz[s] / pix3;
                    emean[pix1][t][s] += eratiots;
                    esquare[pix1][t][s] += 2 * ((dratio[s] * dratio[t]) + (ratio * eratiots));
                }

                for (; s <= t; s++) {
                    esquare[pix1][t][s] += 2 * (dratio[s] * dratio[t]);
                }
            }

            for (; t < 12; t++) {

                for (s = 0; s < 4; s++) {
                    eratiots = eypix4 * dxyz[t - 8] * dxyz[s] / pix3;
                    emean[pix1][t][s] += eratiots;
                    esquare[pix1][t][s] += 2 * ((dratio[s] * dratio[t]) + (ratio * eratiots));
                }

                for (; s < 8; s++) {
                    eratiots = expix4 * dxyz[t - 8] * dxyz[s - 4] / pix3;
                    emean[pix1][t][s] += eratiots;
                    esquare[pix1][t][s] += 2 * ((dratio[s] * dratio[t]) + (ratio * eratiots));
                }

                for (; s <= t; s++) {
                    esquare[pix1][t][s] += 2 * (dratio[s] * dratio[t]);
                }
            }
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        /*Initialize values*/
        meantotal = 0;
        counttotal = 0;
        cf = 0;

        for (t = 0; t < 12; t++) {
            dcf[t] = 0;

            for (s = 0; s < 12; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Calculate normalized standard deviation for each partition*/


        for (jj = 0; jj < partitions; jj++) {

            /*Avoid subsequent NaN errors*/
            if (count[jj] <= 1) {
                continue;
            }

            counts = count[jj];

            means = mean[jj] / counts;

            for (t = 0; t < 12; t++) {
                dmean[jj][t] /= counts;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] /= counts;
                }
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0.0) {
                continue;
            }

            for (t = 0; t < 12; t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] -
                                  (2 * counts * ((means * emean[jj][t][s]) + (dmean[jj][s] * dmean[jj][t])))) /
                                     (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < 12; t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < 12; t++) {

                for (s = 0; s <= t; s++) {

                    esd2[t][s] = (means * dsd2[s]) + (2 * sd2 * dmean[jj][s]);
                    esd2[t][s] /= (2 * means * means * means * sd2 * sd2);
                    esd2[t][s] *= (-dsds[t]);
                    esd2[t][s] += esds[t][s] / (2 * means * means * sd2);
                    esd2[t][s] -= ((sd2 * emean[jj][t][s]) + (dsd2[s] * dmean[jj][t])) / means;
                    esd2[t][s] += sd2 * dmean[jj][t] * dmean[jj][s] / (means * means);
                }
            }

            /*Add weighted normalized standard deviation  of this partitions*/

            /* to cumulative total for all partitions*/

            cf += sd2 * counts;
            meantotal += means * counts;

            for (t = 0; t < 12; t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Renormalize cumulated values*/

        cf /= counttotal;
        meantotal = meantotal / counttotal;

        for (t = 0; t < 12; t++) {
            dcf[t] = dcf[t] / counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = ecf[t][s] / counttotal;
            }
        }

        /* Fill in redundant matrix elements*/
        for (t = 0; t < 12; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[2][0][t];
            dcff[t] += dcf[3] * des[3][0][t];
            dcff[t] += dcf[4] * des[0][1][t];
            dcff[t] += dcf[5] * des[1][1][t];
            dcff[t] += dcf[6] * des[2][1][t];
            dcff[t] += dcf[7] * des[3][1][t];
            dcff[t] += dcf[8] * des[0][2][t];
            dcff[t] += dcf[9] * des[1][2][t];
            dcff[t] += dcf[10] * des[2][2][t];
            dcff[t] += dcf[11] * des[3][2][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[2][0][s];
                temp += ecf[0][3] * des[3][0][s];
                temp += ecf[0][4] * des[0][1][s];
                temp += ecf[0][5] * des[1][1][s];
                temp += ecf[0][6] * des[2][1][s];
                temp += ecf[0][7] * des[3][1][s];
                temp += ecf[0][8] * des[0][2][s];
                temp += ecf[0][9] * des[1][2][s];
                temp += ecf[0][10] * des[2][2][s];
                temp += ecf[0][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[2][0][s];
                temp += ecf[1][3] * des[3][0][s];
                temp += ecf[1][4] * des[0][1][s];
                temp += ecf[1][5] * des[1][1][s];
                temp += ecf[1][6] * des[2][1][s];
                temp += ecf[1][7] * des[3][1][s];
                temp += ecf[1][8] * des[0][2][s];
                temp += ecf[1][9] * des[1][2][s];
                temp += ecf[1][10] * des[2][2][s];
                temp += ecf[1][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[2][0][s];
                temp += ecf[2][3] * des[3][0][s];
                temp += ecf[2][4] * des[0][1][s];
                temp += ecf[2][5] * des[1][1][s];
                temp += ecf[2][6] * des[2][1][s];
                temp += ecf[2][7] * des[3][1][s];
                temp += ecf[2][8] * des[0][2][s];
                temp += ecf[2][9] * des[1][2][s];
                temp += ecf[2][10] * des[2][2][s];
                temp += ecf[2][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][0][t]) + (dcf[2] * ees[2][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[2][0][s];
                temp += ecf[3][3] * des[3][0][s];
                temp += ecf[3][4] * des[0][1][s];
                temp += ecf[3][5] * des[1][1][s];
                temp += ecf[3][6] * des[2][1][s];
                temp += ecf[3][7] * des[3][1][s];
                temp += ecf[3][8] * des[0][2][s];
                temp += ecf[3][9] * des[1][2][s];
                temp += ecf[3][10] * des[2][2][s];
                temp += ecf[3][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[3] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[2][0][s];
                temp += ecf[4][3] * des[3][0][s];
                temp += ecf[4][4] * des[0][1][s];
                temp += ecf[4][5] * des[1][1][s];
                temp += ecf[4][6] * des[2][1][s];
                temp += ecf[4][7] * des[3][1][s];
                temp += ecf[4][8] * des[0][2][s];
                temp += ecf[4][9] * des[1][2][s];
                temp += ecf[4][10] * des[2][2][s];
                temp += ecf[4][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[4] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[2][0][s];
                temp += ecf[5][3] * des[3][0][s];
                temp += ecf[5][4] * des[0][1][s];
                temp += ecf[5][5] * des[1][1][s];
                temp += ecf[5][6] * des[2][1][s];
                temp += ecf[5][7] * des[3][1][s];
                temp += ecf[5][8] * des[0][2][s];
                temp += ecf[5][9] * des[1][2][s];
                temp += ecf[5][10] * des[2][2][s];
                temp += ecf[5][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[5] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[6][0] * des[0][0][s];
                temp += ecf[6][1] * des[1][0][s];
                temp += ecf[6][2] * des[2][0][s];
                temp += ecf[6][3] * des[3][0][s];
                temp += ecf[6][4] * des[0][1][s];
                temp += ecf[6][5] * des[1][1][s];
                temp += ecf[6][6] * des[2][1][s];
                temp += ecf[6][7] * des[3][1][s];
                temp += ecf[6][8] * des[0][2][s];
                temp += ecf[6][9] * des[1][2][s];
                temp += ecf[6][10] * des[2][2][s];
                temp += ecf[6][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][1][t]) + (dcf[6] * ees[2][1][t][s]);
                temp = 0;
                temp += ecf[7][0] * des[0][0][s];
                temp += ecf[7][1] * des[1][0][s];
                temp += ecf[7][2] * des[2][0][s];
                temp += ecf[7][3] * des[3][0][s];
                temp += ecf[7][4] * des[0][1][s];
                temp += ecf[7][5] * des[1][1][s];
                temp += ecf[7][6] * des[2][1][s];
                temp += ecf[7][7] * des[3][1][s];
                temp += ecf[7][8] * des[0][2][s];
                temp += ecf[7][9] * des[1][2][s];
                temp += ecf[7][10] * des[2][2][s];
                temp += ecf[7][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[7] * ees[3][1][t][s]);
                temp = 0;
                temp += ecf[8][0] * des[0][0][s];
                temp += ecf[8][1] * des[1][0][s];
                temp += ecf[8][2] * des[2][0][s];
                temp += ecf[8][3] * des[3][0][s];
                temp += ecf[8][4] * des[0][1][s];
                temp += ecf[8][5] * des[1][1][s];
                temp += ecf[8][6] * des[2][1][s];
                temp += ecf[8][7] * des[3][1][s];
                temp += ecf[8][8] * des[0][2][s];
                temp += ecf[8][9] * des[1][2][s];
                temp += ecf[8][10] * des[2][2][s];
                temp += ecf[8][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][2][t]) + (dcf[8] * ees[0][2][t][s]);
                temp = 0;
                temp += ecf[9][0] * des[0][0][s];
                temp += ecf[9][1] * des[1][0][s];
                temp += ecf[9][2] * des[2][0][s];
                temp += ecf[9][3] * des[3][0][s];
                temp += ecf[9][4] * des[0][1][s];
                temp += ecf[9][5] * des[1][1][s];
                temp += ecf[9][6] * des[2][1][s];
                temp += ecf[9][7] * des[3][1][s];
                temp += ecf[9][8] * des[0][2][s];
                temp += ecf[9][9] * des[1][2][s];
                temp += ecf[9][10] * des[2][2][s];
                temp += ecf[9][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][2][t]) + (dcf[9] * ees[1][2][t][s]);
                temp = 0;
                temp += ecf[10][0] * des[0][0][s];
                temp += ecf[10][1] * des[1][0][s];
                temp += ecf[10][2] * des[2][0][s];
                temp += ecf[10][3] * des[3][0][s];
                temp += ecf[10][4] * des[0][1][s];
                temp += ecf[10][5] * des[1][1][s];
                temp += ecf[10][6] * des[2][1][s];
                temp += ecf[10][7] * des[3][1][s];
                temp += ecf[10][8] * des[0][2][s];
                temp += ecf[10][9] * des[1][2][s];
                temp += ecf[10][10] * des[2][2][s];
                temp += ecf[10][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][2][t]) + (dcf[10] * ees[2][2][t][s]);
                temp = 0;
                temp += ecf[11][0] * des[0][0][s];
                temp += ecf[11][1] * des[1][0][s];
                temp += ecf[11][2] * des[2][0][s];
                temp += ecf[11][3] * des[3][0][s];
                temp += ecf[11][4] * des[0][1][s];
                temp += ecf[11][5] * des[1][1][s];
                temp += ecf[11][6] * des[2][1][s];
                temp += ecf[11][7] * des[3][1][s];
                temp += ecf[11][8] * des[0][2][s];
                temp += ecf[11][9] * des[1][2][s];
                temp += ecf[11][10] * des[2][2][s];
                temp += ecf[11][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][2][t]) + (dcf[11] * ees[3][2][t][s]);
            }
        }

        return cf;
    }

    /**
     * double uvderivsN6().
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              source parameters([4][3])
     * @param   des             derivatives of each source parameter with respect to each element
     * @param   ees             second derivatives as above
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2XDim      DOCUMENT ME!
     * @param   pixel2YDim      DOCUMENT ME!
     * @param   pixel5          interpolated values of target file
     * @param   pixel5Mask      mask of interpolated values of target file
     * @param   pixel5XDim      DOCUMENT ME!
     * @param   pixel5YDim      DOCUMENT ME!
     * @param   pixel5ZDim      DOCUMENT ME!
     * @param   threshold5      DOCUMENT ME!
     * @param   dcff            first derivatives of sd with respect to parameters
     * @param   ecff            second derivatives of sd with respect to parameters
     * @param   count           number of pixels in partitions that are above threshold
     * @param   mean            ratio of pixel in source file/corresponding pixel in target file
     * @param   square          intermediate computation value
     * @param   dmean           partial derivatives of mean with respect to parameters
     * @param   dsquare         partial derivatives of square
     * @param   emean           second partial derivatives of mean with respect to parameters
     * @param   esquare         second partial derivatives of square
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  DOCUMENT ME! This routine computes the first and second derivatives of the normalized
     *                          standard deviation with respect to all external parameters.
     *
     *                          <p>The matrix indices have been adjusted to allow for a more orderly relationship
     *                          between the matrix and its elements</p>
     *
     *                          <p>Returns the normalized standard deviation</p>
     *
     * @return  DOCUMENT ME!
     */

    double uvderivsN6(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                      float[] pixel2, int pixel2XDim, int pixel2YDim, float[] pixel5, boolean[] pixel5Mask,
                      int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5, double[] dcff, double[][] ecff,
                      int[] count, double[] mean, double[] square, double[][] dmean, double[][] dsquare,
                      double[][][] emean, double[][][] esquare, int partitions, double maxActualValue) {

        double cf = 0.0;
        double[] dcf = new double[6];
        double[][] ecf = new double[6][6];
        double[] dxy = new double[3];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double eratiots;
        int counttotal;
        double meantotal;

        int jj; /*index of current partition*/
        int pix1; /*int value of current pixel in target file*/
        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in target file*/
        double pix4; /*calculated value of current pixel in target file*/
        double ratio; /*intermediate value*/
        double[] dratio = new double[6]; /*partial derivatives of ratio with respect to parameters*/

        double sd2;
        double[] dsd2 = new double[6];
        double[][] esd2 = new double[6][6];

        int i, j, k;
        int s, t;
        int x_max1, y_max1;
        int x_dim1, y_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        double e00, e01, e10, e11, e30, e31;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4, ezpix4;
        int counts;
        double means;
        double sds;
        double[] dsds = new double[6];
        double[][] esds = new double[6][6];
        double temp;

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < 6; t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] = 0;
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        e00 = es[0][0];
        e01 = es[0][1];
        e10 = es[1][0];
        e11 = es[1][1];
        e30 = es[3][0];
        e31 = es[3][1];

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            /* Note that dx[0], dx[1], and dx[2] are equal to dy[3], dy[4], and dy[5] respectively.
             * Consequently, these are all treated as dxy[0], dxy[1], and dxy[2] */

            dxy[0] = (double) i;
            dxy[1] = (double) j;
            dxy[2] = 1.0;

            /* Second derivatives are all zero */

            /*Get the coordinates of the 4 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            /*Get the values of these 4 voxels*/

            n0 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - threshold5) / (maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            ezpix4 = n3 + n0 - n2 - n1;

            /*Calculate the ratio of the reslice value to the standard value*/
            ratio = pix4 / pix3;
            mean[pix1] += ratio;
            square[pix1] += ratio * ratio;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */

            for (t = 0; t < 3; t++) {
                dratio[t] = dxpix4 * dxy[t] / pix3;
                dmean[pix1][t] += dratio[t];
                dsquare[pix1][t] += 2 * ratio * dratio[t];
                dratio[t + 3] = dypix4 * dxy[t] / pix3;
                dmean[pix1][t + 3] += dratio[t + 3];
                dsquare[pix1][t + 3] += 2 * ratio * dratio[t + 3];
            }

            /* Second derivatives */
            for (t = 0; t < 6; t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dratio[s] * dratio[t]);
                }
            }

            for (t = 3; t < 6; t++) {

                for (s = 0; s < 3; s++) {
                    eratiots = ezpix4 * dxy[t - 3] * dxy[s] / pix3;
                    emean[pix1][t][s] += eratiots;
                    esquare[pix1][t][s] += 2.0 * (ratio * eratiots);
                }

            }
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        /*Initialize values*/
        meantotal = 0;
        counttotal = 0;
        cf = 0;

        for (t = 0; t < 6; t++) {
            dcf[t] = 0;

            for (s = 0; s < 6; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Calculate normalized standard deviation for each partition*/


        for (jj = 0; jj < partitions; jj++) {

            /*Avoid subsequent NaN errors*/
            if (count[jj] <= 1) {
                continue;
            }

            counts = count[jj];

            means = mean[jj] / counts;

            for (t = 0; t < 6; t++) {
                dmean[jj][t] /= counts;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] /= counts;
                }
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0) {
                continue;
            }

            for (t = 0; t < 6; t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] -
                                  (2 * counts * ((means * emean[jj][t][s]) + (dmean[jj][s] * dmean[jj][t])))) /
                                     (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < 6; t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < 6; t++) {

                for (s = 0; s <= t; s++) {

                    esd2[t][s] = (means * dsd2[s]) + (2 * sd2 * dmean[jj][s]);
                    esd2[t][s] /= (2 * means * means * means * sd2 * sd2);
                    esd2[t][s] *= (-dsds[t]);
                    esd2[t][s] += esds[t][s] / (2 * means * means * sd2);
                    esd2[t][s] -= ((sd2 * emean[jj][t][s]) + (dsd2[s] * dmean[jj][t])) / means;
                    esd2[t][s] += sd2 * dmean[jj][t] * dmean[jj][s] / (means * means);
                }
            }

            /*Add weighted normalized standard deviation  of this partitions*/

            /* to cumulative total for all partitions*/

            cf += sd2 * counts;
            meantotal += means * counts;

            for (t = 0; t < 6; t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Renormalize cumulated values*/

        cf /= counttotal;
        meantotal = meantotal / counttotal;

        for (t = 0; t < 6; t++) {
            dcf[t] = dcf[t] / counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = ecf[t][s] / counttotal;
            }
        }

        /* Fill in redundant matrix elements*/
        for (t = 0; t < 6; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[3][0][t];
            dcff[t] += dcf[3] * des[0][1][t];
            dcff[t] += dcf[4] * des[1][1][t];
            dcff[t] += dcf[5] * des[3][1][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[3][0][s];
                temp += ecf[0][3] * des[0][1][s];
                temp += ecf[0][4] * des[1][1][s];
                temp += ecf[0][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[3][0][s];
                temp += ecf[1][3] * des[0][1][s];
                temp += ecf[1][4] * des[1][1][s];
                temp += ecf[1][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[3][0][s];
                temp += ecf[2][3] * des[0][1][s];
                temp += ecf[2][4] * des[1][1][s];
                temp += ecf[2][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[2] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[3][0][s];
                temp += ecf[3][3] * des[0][1][s];
                temp += ecf[3][4] * des[1][1][s];
                temp += ecf[3][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[3] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[3][0][s];
                temp += ecf[4][3] * des[0][1][s];
                temp += ecf[4][4] * des[1][1][s];
                temp += ecf[4][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[4] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[3][0][s];
                temp += ecf[5][3] * des[0][1][s];
                temp += ecf[5][4] * des[1][1][s];
                temp += ecf[5][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[5] * ees[3][1][t][s]);

            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            source parameters([4][3])
     * @param   des           derivatives of each source parameter with respect to each element
     * @param   ees           second derivatives as above
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2XDim    DOCUMENT ME!
     * @param   pixel2YDim    DOCUMENT ME!
     * @param   pixel2ZDim    DOCUMENT ME!
     * @param   pixel5        interpolated values of target file
     * @param   pixel5Mask    mask of interpolated values of target file
     * @param   pixel5XDim    DOCUMENT ME!
     * @param   pixel5YDim    DOCUMENT ME!
     * @param   pixel5ZDim    DOCUMENT ME!
     * @param   threshold5    DOCUMENT ME!
     * @param   dcff          first derivatives of sd with respect to parameters
     * @param   ecff          second derivatives of sd with respect to parameters
     * @param   scale         DOCUMENT ME!
     * @param   forward
     *                        <p>This routine computes the first and second derivatives of the sum of squares with
     *                        respect to all external parameters.</p>
     *
     *                        <p>A rescaling parameter is included to allow for data where intensities require
     *                        adjustment</p>
     *
     *                        <p>The matrix indices have been adjusted to allow for a more orderly relationship between
     *                        the matrix and its elements</p>
     *
     *                        <p>Returns the sum of squares</p>
     *
     * @return  DOCUMENT ME!
     */

    double uvderivsRS12(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                        float[] pixel2, int pixel2XDim, int pixel2YDim, int pixel2ZDim, float[] pixel5,
                        boolean[] pixel5Mask, int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5,
                        double[] dcff, double[][] ecff, double scale, boolean forward) {

        long counttotal = 0;
        double cf = 0.0;
        double[] dcf = new double[13];
        double[][] ecf = new double[13][13];
        double[] dxyz = new double[4];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double[][] dydx = new double[4][4];
        double[][] dxdx = new double[4][4];
        double dscale;
        double escale;

        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double spix3;

        double e00, e01, e02, e10, e11, e12, e20, e21, e22, e30, e31, e32;

        int i, j, k;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_dim1, y_dim1, z_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4, expix4, eypix4, ezpix4;
        double temp;

        /*Initialize values*/

        if (!forward) {
            scale = 1.0 / scale;
        }

        for (t = 0; t < 13; t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        z_dim1 = pixel2ZDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + (k * e20) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + (k * e21) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            z_i = (i * e02) + (j * e12) + (k * e22) + e32;

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

            /* Notice that dx[0], dx[1], dx[2], dx[3] are equal to dy[4], dy[5], dy[6], dy[7]
             * and to dz[8], dz[9], dz[10], dz[11].  Consequently, these are all treated as dxyz[0], dxyz[1], dxyz[2],
             * and dxyz[3]. */
            dxyz[0] = (double) i;
            dxyz[1] = (double) j;
            dxyz[2] = (double) k;
            dxyz[3] = 1.0;

            /*Get the coordinates of the 8 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);
            z_up = (int) Math.ceil(z_i);
            z_down = (int) Math.floor(z_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            if (z_up == z_down) {

                if (z_up < z_max1) {
                    z_up++;
                } else {
                    z_down--;
                }
            }

            c = z_i - z_down;
            f = z_up - z_i;

            /*Get the values of these 8 voxels*/

            n0 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(z_down * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(z_down * slice1) + (y_up * pixel2XDim) + x_up];
            n4 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_down];
            n5 = pixel2[(z_up * slice1) + (y_down * pixel2XDim) + x_up];
            n6 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_down];
            n7 = pixel2[(z_up * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e * f) + (n1 * a * e * f) + (n2 * d * b * f) + (n3 * a * b * f) + (n4 * d * e * c) +
                   (n5 * a * e * c) + (n6 * d * b * c) + (n7 * a * b * c);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = (((e * f) * (n1 - n0)) + ((b * f) * (n3 - n2)) + ((c * e) * (n5 - n4)) + ((b * c) * (n7 - n6)));
            dypix4 = (((d * f) * (n2 - n0)) + ((a * f) * (n3 - n1)) + ((c * d) * (n6 - n4)) + ((a * c) * (n7 - n5)));
            dzpix4 = (((d * e) * (n4 - n0)) + ((a * e) * (n5 - n1)) + ((b * d) * (n6 - n2)) + ((a * b) * (n7 - n3)));

            expix4 = (((n7 + n1 - n5 - n3) * a) + ((n6 + n0 - n4 - n2) * d));
            eypix4 = (((n7 + n2 - n6 - n3) * b) + ((n5 + n0 - n4 - n1) * e));
            ezpix4 = (((n7 + n4 - n6 - n5) * c) + ((n3 + n0 - n2 - n1) * f));

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * spix3) + (spix3 * spix3);

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */
            for (t = 0; t < 4; t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dxpix4 * dxyz[t];
                dcf[t + 4] += 2.0 * (pix4 - spix3) * dypix4 * dxyz[t];
                dcf[t + 8] += 2.0 * (pix4 - spix3) * dzpix4 * dxyz[t];
            }

            dcf[12] += 2.0 * (pix4 - spix3) * (-pix3);

            /* Second derivatives */

            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    dxdx[t][s] = dydx[t][s] = 2.0 * dxyz[t] * dxyz[s];
                }

                for (; s < 4; s++) {
                    dydx[t][s] = 2.0 * dxyz[t] * dxyz[s];
                }
            }

            for (t = 0; t < 4; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += 2.0 * dxdx[t][s] * dxpix4 * dxpix4;
                }
            }

            for (; t < 8; t++) {

                for (s = 0; s < 4; s++) {
                    ecf[t][s] += dydx[t - 4][s] * ((dxpix4 * dypix4) + ((pix4 - spix3) * ezpix4));
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 4][s - 4] * dypix4 * dypix4;
                }
            }

            for (; t < 12; t++) {

                for (s = 0; s < 4; s++) {
                    ecf[t][s] += dydx[t - 8][s] * ((dxpix4 * dzpix4) + ((pix4 - spix3) * eypix4));
                }

                for (; s < 8; s++) {
                    ecf[t][s] += dydx[t - 8][s - 4] * ((dypix4 * dzpix4) + ((pix4 - spix3) * expix4));
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 8][s - 8] * dzpix4 * dzpix4;
                }
            }

            for (s = 0; s < 4; s++) {
                ecf[12][s] += -2 * pix3 * dxpix4 * dxyz[s];
            }

            for (; s < 8; s++) {
                ecf[12][s] += -2 * pix3 * dypix4 * dxyz[s - 4];
            }

            for (; s < 12; s++) {
                ecf[12][s] += -2 * pix3 * dzpix4 * dxyz[s - 8];
            }

            ecf[12][12] += 2 * pix3 * pix3;

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < 13; t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        /*Fill in redundant matrix elements*/
        for (t = 0; t < 13; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        if (forward) {
            dscale = 1;
            escale = 0;
        } else {
            dscale = -(scale * scale);
            escale = 2.0 * (scale * scale * scale);
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[2][0][t];
            dcff[t] += dcf[3] * des[3][0][t];
            dcff[t] += dcf[4] * des[0][1][t];
            dcff[t] += dcf[5] * des[1][1][t];
            dcff[t] += dcf[6] * des[2][1][t];
            dcff[t] += dcf[7] * des[3][1][t];
            dcff[t] += dcf[8] * des[0][2][t];
            dcff[t] += dcf[9] * des[1][2][t];
            dcff[t] += dcf[10] * des[2][2][t];
            dcff[t] += dcf[11] * des[3][2][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[2][0][s];
                temp += ecf[0][3] * des[3][0][s];
                temp += ecf[0][4] * des[0][1][s];
                temp += ecf[0][5] * des[1][1][s];
                temp += ecf[0][6] * des[2][1][s];
                temp += ecf[0][7] * des[3][1][s];
                temp += ecf[0][8] * des[0][2][s];
                temp += ecf[0][9] * des[1][2][s];
                temp += ecf[0][10] * des[2][2][s];
                temp += ecf[0][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[2][0][s];
                temp += ecf[1][3] * des[3][0][s];
                temp += ecf[1][4] * des[0][1][s];
                temp += ecf[1][5] * des[1][1][s];
                temp += ecf[1][6] * des[2][1][s];
                temp += ecf[1][7] * des[3][1][s];
                temp += ecf[1][8] * des[0][2][s];
                temp += ecf[1][9] * des[1][2][s];
                temp += ecf[1][10] * des[2][2][s];
                temp += ecf[1][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[2][0][s];
                temp += ecf[2][3] * des[3][0][s];
                temp += ecf[2][4] * des[0][1][s];
                temp += ecf[2][5] * des[1][1][s];
                temp += ecf[2][6] * des[2][1][s];
                temp += ecf[2][7] * des[3][1][s];
                temp += ecf[2][8] * des[0][2][s];
                temp += ecf[2][9] * des[1][2][s];
                temp += ecf[2][10] * des[2][2][s];
                temp += ecf[2][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][0][t]) + (dcf[2] * ees[2][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[2][0][s];
                temp += ecf[3][3] * des[3][0][s];
                temp += ecf[3][4] * des[0][1][s];
                temp += ecf[3][5] * des[1][1][s];
                temp += ecf[3][6] * des[2][1][s];
                temp += ecf[3][7] * des[3][1][s];
                temp += ecf[3][8] * des[0][2][s];
                temp += ecf[3][9] * des[1][2][s];
                temp += ecf[3][10] * des[2][2][s];
                temp += ecf[3][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[3] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[2][0][s];
                temp += ecf[4][3] * des[3][0][s];
                temp += ecf[4][4] * des[0][1][s];
                temp += ecf[4][5] * des[1][1][s];
                temp += ecf[4][6] * des[2][1][s];
                temp += ecf[4][7] * des[3][1][s];
                temp += ecf[4][8] * des[0][2][s];
                temp += ecf[4][9] * des[1][2][s];
                temp += ecf[4][10] * des[2][2][s];
                temp += ecf[4][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[4] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[2][0][s];
                temp += ecf[5][3] * des[3][0][s];
                temp += ecf[5][4] * des[0][1][s];
                temp += ecf[5][5] * des[1][1][s];
                temp += ecf[5][6] * des[2][1][s];
                temp += ecf[5][7] * des[3][1][s];
                temp += ecf[5][8] * des[0][2][s];
                temp += ecf[5][9] * des[1][2][s];
                temp += ecf[5][10] * des[2][2][s];
                temp += ecf[5][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[5] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[6][0] * des[0][0][s];
                temp += ecf[6][1] * des[1][0][s];
                temp += ecf[6][2] * des[2][0][s];
                temp += ecf[6][3] * des[3][0][s];
                temp += ecf[6][4] * des[0][1][s];
                temp += ecf[6][5] * des[1][1][s];
                temp += ecf[6][6] * des[2][1][s];
                temp += ecf[6][7] * des[3][1][s];
                temp += ecf[6][8] * des[0][2][s];
                temp += ecf[6][9] * des[1][2][s];
                temp += ecf[6][10] * des[2][2][s];
                temp += ecf[6][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][1][t]) + (dcf[6] * ees[2][1][t][s]);
                temp = 0;
                temp += ecf[7][0] * des[0][0][s];
                temp += ecf[7][1] * des[1][0][s];
                temp += ecf[7][2] * des[2][0][s];
                temp += ecf[7][3] * des[3][0][s];
                temp += ecf[7][4] * des[0][1][s];
                temp += ecf[7][5] * des[1][1][s];
                temp += ecf[7][6] * des[2][1][s];
                temp += ecf[7][7] * des[3][1][s];
                temp += ecf[7][8] * des[0][2][s];
                temp += ecf[7][9] * des[1][2][s];
                temp += ecf[7][10] * des[2][2][s];
                temp += ecf[7][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[7] * ees[3][1][t][s]);
                temp = 0;
                temp += ecf[8][0] * des[0][0][s];
                temp += ecf[8][1] * des[1][0][s];
                temp += ecf[8][2] * des[2][0][s];
                temp += ecf[8][3] * des[3][0][s];
                temp += ecf[8][4] * des[0][1][s];
                temp += ecf[8][5] * des[1][1][s];
                temp += ecf[8][6] * des[2][1][s];
                temp += ecf[8][7] * des[3][1][s];
                temp += ecf[8][8] * des[0][2][s];
                temp += ecf[8][9] * des[1][2][s];
                temp += ecf[8][10] * des[2][2][s];
                temp += ecf[8][11] * des[3][2][s];
                ecff[t][s] += (temp * des[0][2][t]) + (dcf[8] * ees[0][2][t][s]);
                temp = 0;
                temp += ecf[9][0] * des[0][0][s];
                temp += ecf[9][1] * des[1][0][s];
                temp += ecf[9][2] * des[2][0][s];
                temp += ecf[9][3] * des[3][0][s];
                temp += ecf[9][4] * des[0][1][s];
                temp += ecf[9][5] * des[1][1][s];
                temp += ecf[9][6] * des[2][1][s];
                temp += ecf[9][7] * des[3][1][s];
                temp += ecf[9][8] * des[0][2][s];
                temp += ecf[9][9] * des[1][2][s];
                temp += ecf[9][10] * des[2][2][s];
                temp += ecf[9][11] * des[3][2][s];
                ecff[t][s] += (temp * des[1][2][t]) + (dcf[9] * ees[1][2][t][s]);
                temp = 0;
                temp += ecf[10][0] * des[0][0][s];
                temp += ecf[10][1] * des[1][0][s];
                temp += ecf[10][2] * des[2][0][s];
                temp += ecf[10][3] * des[3][0][s];
                temp += ecf[10][4] * des[0][1][s];
                temp += ecf[10][5] * des[1][1][s];
                temp += ecf[10][6] * des[2][1][s];
                temp += ecf[10][7] * des[3][1][s];
                temp += ecf[10][8] * des[0][2][s];
                temp += ecf[10][9] * des[1][2][s];
                temp += ecf[10][10] * des[2][2][s];
                temp += ecf[10][11] * des[3][2][s];
                ecff[t][s] += (temp * des[2][2][t]) + (dcf[10] * ees[2][2][t][s]);
                temp = 0;
                temp += ecf[11][0] * des[0][0][s];
                temp += ecf[11][1] * des[1][0][s];
                temp += ecf[11][2] * des[2][0][s];
                temp += ecf[11][3] * des[3][0][s];
                temp += ecf[11][4] * des[0][1][s];
                temp += ecf[11][5] * des[1][1][s];
                temp += ecf[11][6] * des[2][1][s];
                temp += ecf[11][7] * des[3][1][s];
                temp += ecf[11][8] * des[0][2][s];
                temp += ecf[11][9] * des[1][2][s];
                temp += ecf[11][10] * des[2][2][s];
                temp += ecf[11][11] * des[3][2][s];
                ecff[t][s] += (temp * des[3][2][t]) + (dcf[11] * ees[3][2][t][s]);
            }
        }

        dcff[parameters] = dcf[12] * dscale;

        for (s = 0; s < parameters; s++) {
            temp = 0;
            temp += ecf[12][0] * des[0][0][s];
            temp += ecf[12][1] * des[1][0][s];
            temp += ecf[12][2] * des[2][0][s];
            temp += ecf[12][3] * des[3][0][s];
            temp += ecf[12][4] * des[0][1][s];
            temp += ecf[12][5] * des[1][1][s];
            temp += ecf[12][6] * des[2][1][s];
            temp += ecf[12][7] * des[3][1][s];
            temp += ecf[12][8] * des[0][2][s];
            temp += ecf[12][9] * des[1][2][s];
            temp += ecf[12][10] * des[2][2][s];
            temp += ecf[12][11] * des[3][2][s];
            ecff[parameters][s] = temp * dscale;
        }

        ecff[parameters][parameters] = (dscale * dscale * ecf[12][12]) + (dcf[12] * escale);

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            source parameters([4][3])
     * @param   des           derivatives of each source parameter with respect to each element
     * @param   ees           second derivatives as above
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2XDim    DOCUMENT ME!
     * @param   pixel2YDim    DOCUMENT ME!
     * @param   pixel5        interpolated values of target file
     * @param   pixel5Mask    mask of interpolated values of target file
     * @param   pixel5XDim    DOCUMENT ME!
     * @param   pixel5YDim    DOCUMENT ME!
     * @param   pixel5ZDim    DOCUMENT ME!
     * @param   threshold5    DOCUMENT ME!
     * @param   dcff          first derivatives of sd with respect to parameters
     * @param   ecff          second derivatives of sd with respect to parameters
     * @param   scale         DOCUMENT ME!
     * @param   forward
     *                        <p>This routine computes the first and second derivatives of the sum of squares with
     *                        respect to all external parameters.</p>
     *
     *                        <p>A rescaling parameter is included to allow for data where intensities require
     *                        adjustment</p>
     *
     *                        <p>The matrix indices have been adjusted to allow for a more orderly relationship between
     *                        the matrix and its elements</p>
     *
     *                        <p>Returns the sum of squares</p>
     *
     * @return  DOCUMENT ME!
     */

    double uvderivsRS6(int parameters, double[][] es, double[][][] des, double[][][][] ees, int sampleFactor,
                       float[] pixel2, int pixel2XDim, int pixel2YDim, float[] pixel5, boolean[] pixel5Mask,
                       int pixel5XDim, int pixel5YDim, int pixel5ZDim, float threshold5, double[] dcff, double[][] ecff,
                       double scale, boolean forward) {

        long counttotal = 0;
        double cf = 0.0;
        double[] dcf = new double[7];
        double[][] ecf = new double[7][7];
        double[] dxy = new double[3];
        int rTerm;
        int slice1;
        int slice2;
        int remainder2;
        double[][] dydx = new double[3][3];
        double[][] dxdx = new double[3][3];
        double dscale;
        double escale;

        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        double spix3;

        int i, j, k;
        int s, t;
        int x_max1, y_max1;
        int x_dim1, y_dim1;
        int x_dim2, y_dim2, z_dim2;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        double e00, e01, e10, e11, e30, e31;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4, ezpix4;
        double temp;

        if (!forward) {
            scale = 1.0 / scale;
        }

        for (t = 0; t < 7; t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        e00 = es[0][0];
        e01 = es[0][1];
        e10 = es[1][0];
        e11 = es[1][1];
        e30 = es[3][0];
        e31 = es[3][1];

        x_dim1 = pixel2XDim;
        y_dim1 = pixel2YDim;
        slice1 = x_dim1 * y_dim1;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;

        x_dim2 = pixel5XDim;
        y_dim2 = pixel5YDim;
        z_dim2 = pixel5ZDim;
        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            /*Verify that pixel5>threshold*/
            if ((pixel5[r] <= threshold5) || (!pixel5Mask[r])) {
                continue;
            }

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            x_i = (i * e00) + (j * e10) + e30;

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            y_i = (i * e01) + (j * e11) + e31;

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            /* Note that dx[0], dx[1], and dx[2] are equal to dy[3], dy[4], and dy[5] respectively.
             * Consequently, these are all treated as dxy[0], dxy[1], and dxy[2] */

            dxy[0] = (double) i;
            dxy[1] = (double) j;
            dxy[2] = 1.0;

            /* Second derivatives are all zero */

            /*Get the coordinates of the 4 voxels surrounding the designated pixel*/

            /* in the reslice file*/

            x_up = (int) Math.ceil(x_i);
            x_down = (int) Math.floor(x_i);
            y_up = (int) Math.ceil(y_i);
            y_down = (int) Math.floor(y_i);

            if (x_up == x_down) {

                if (x_up < x_max1) {
                    x_up++;
                } else {
                    x_down--;
                }
            }

            a = x_i - x_down;
            d = x_up - x_i;

            if (y_up == y_down) {

                if (y_up < y_max1) {
                    y_up++;
                } else {
                    y_down--;
                }
            }

            b = y_i - y_down;
            e = y_up - y_i;

            /*Get the values of these 4 voxels*/

            n0 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_down];
            n1 = pixel2[(k * slice1) + (y_down * pixel2XDim) + x_up];
            n2 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_down];
            n3 = pixel2[(k * slice1) + (y_up * pixel2XDim) + x_up];

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            ezpix4 = n3 + n0 - n2 - n1;

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * spix3) + (spix3 * spix3);

            /*Calculate derivatives that are nonzero*/

            /* First derivatives*/
            for (t = 0; t < 3; t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dxpix4 * dxy[t];
                dcf[t + 3] += 2.0 * (pix4 - spix3) * dypix4 * dxy[t];
            }

            dcf[6] += 2.0 * (pix4 - spix3) * (-pix3);

            /* Second derivatives */
            for (t = 0; t < 3; t++) {

                for (s = 0; s <= t; s++) {
                    dxdx[t][s] = dydx[t][s] = 2.0 * dxy[t] * dxy[s];
                }

                for (; s < 3; s++) {
                    dydx[t][s] = 2.0 * dxy[t] * dxy[s];
                }
            }

            for (t = 0; t < 3; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += dxdx[t][s] * dxpix4 * dxpix4;
                }
            }

            for (; t < 6; t++) {

                for (s = 0; s < 3; s++) {
                    ecf[t][s] += dydx[t - 3][s] * ((dxpix4 * dypix4) + ((pix4 - spix3) * ezpix4));
                }

                for (; s <= t; s++) {
                    ecf[t][s] += dxdx[t - 3][s - 3] * dypix4 * dypix4;
                }
            }

            for (s = 0; s < 3; s++) {
                ecf[6][s] += -2 * pix3 * dxpix4 * dxy[s];
            }

            for (; s < 6; s++) {
                ecf[6][s] += -2 * pix3 * dypix4 * dxy[s - 3];
            }

            ecf[6][6] += 2 * pix3 * pix3;

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < 7; t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        /*Fill in redundant matrix elements*/
        for (t = 0; t < 7; t++) {

            for (s = 0; s <= t; s++) {
                ecf[s][t] = ecf[t][s];
            }
        }

        if (forward) {
            dscale = 1;
            escale = 0;
        } else {
            dscale = -(scale * scale);
            escale = 2.0 * (scale * scale * scale);
        }

        /* Calculate derivatives with respect to external parameters*/
        for (t = 0; t < parameters; t++) {
            dcff[t] = 0;
            dcff[t] += dcf[0] * des[0][0][t];
            dcff[t] += dcf[1] * des[1][0][t];
            dcff[t] += dcf[2] * des[3][0][t];
            dcff[t] += dcf[3] * des[0][1][t];
            dcff[t] += dcf[4] * des[1][1][t];
            dcff[t] += dcf[5] * des[3][1][t];

            for (s = 0; s <= t; s++) {
                ecff[t][s] = 0;
                temp = 0;
                temp += ecf[0][0] * des[0][0][s];
                temp += ecf[0][1] * des[1][0][s];
                temp += ecf[0][2] * des[3][0][s];
                temp += ecf[0][3] * des[0][1][s];
                temp += ecf[0][4] * des[1][1][s];
                temp += ecf[0][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][0][t]) + (dcf[0] * ees[0][0][t][s]);
                temp = 0;
                temp += ecf[1][0] * des[0][0][s];
                temp += ecf[1][1] * des[1][0][s];
                temp += ecf[1][2] * des[3][0][s];
                temp += ecf[1][3] * des[0][1][s];
                temp += ecf[1][4] * des[1][1][s];
                temp += ecf[1][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][0][t]) + (dcf[1] * ees[1][0][t][s]);
                temp = 0;
                temp += ecf[2][0] * des[0][0][s];
                temp += ecf[2][1] * des[1][0][s];
                temp += ecf[2][2] * des[3][0][s];
                temp += ecf[2][3] * des[0][1][s];
                temp += ecf[2][4] * des[1][1][s];
                temp += ecf[2][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][0][t]) + (dcf[2] * ees[3][0][t][s]);
                temp = 0;
                temp += ecf[3][0] * des[0][0][s];
                temp += ecf[3][1] * des[1][0][s];
                temp += ecf[3][2] * des[3][0][s];
                temp += ecf[3][3] * des[0][1][s];
                temp += ecf[3][4] * des[1][1][s];
                temp += ecf[3][5] * des[3][1][s];
                ecff[t][s] += (temp * des[0][1][t]) + (dcf[3] * ees[0][1][t][s]);
                temp = 0;
                temp += ecf[4][0] * des[0][0][s];
                temp += ecf[4][1] * des[1][0][s];
                temp += ecf[4][2] * des[3][0][s];
                temp += ecf[4][3] * des[0][1][s];
                temp += ecf[4][4] * des[1][1][s];
                temp += ecf[4][5] * des[3][1][s];
                ecff[t][s] += (temp * des[1][1][t]) + (dcf[4] * ees[1][1][t][s]);
                temp = 0;
                temp += ecf[5][0] * des[0][0][s];
                temp += ecf[5][1] * des[1][0][s];
                temp += ecf[5][2] * des[3][0][s];
                temp += ecf[5][3] * des[0][1][s];
                temp += ecf[5][4] * des[1][1][s];
                temp += ecf[5][5] * des[3][1][s];
                ecff[t][s] += (temp * des[3][1][t]) + (dcf[5] * ees[3][1][t][s]);
            }
        }

        dcff[parameters] = dcf[6] * dscale;

        for (s = 0; s < parameters; s++) {
            temp = 0;
            temp += ecf[6][0] * des[0][0][s];
            temp += ecf[6][1] * des[1][0][s];
            temp += ecf[6][2] * des[3][0][s];
            temp += ecf[6][3] * des[0][1][s];
            temp += ecf[6][4] * des[1][1][s];
            temp += ecf[6][5] * des[3][1][s];
            ecff[parameters][s] = temp * dscale;
        }

        ecff[parameters][parameters] = (dscale * dscale * ecf[6][6]) + (dcf[6] * escale);

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  a     on input the output from DEGCO or GEFFA on return the inverse of original matrix
     * @param  n     the order of matrix a
     * @param  ipvt  the pivot vector from DGECO or DGEFA
     * @param  work  work vector, contents destroyed This routine will invert a matrix using factors computed by DGECO
     *               or DGEFA
     */


    private void dgedi(double[][] a, int n, int[] ipvt, double[] work) {

        double t;
        int i, j, k, l;
        double temp;

        /*Compute inverse(U)*/

        for (k = 0; k < n; k++) {
            a[k][k] = 1.0 / a[k][k];

            for (i = 0; i < k; i++) {
                a[k][i] *= (-a[k][k]);
            }

            for (j = k + 1; j < n; j++) {
                t = a[j][k];
                a[j][k] = 0.0;

                for (i = 0; i < (k + 1); i++) {
                    a[j][i] += t * a[k][i];
                }
            }
        }

        /*form inverse(U)*inverse(L)*/

        if (n > 1) {

            for (k = n - 1; k-- != 0;) {

                for (i = k + 1; i < n; i++) {
                    work[i] = a[k][i];
                    a[k][i] = 0.0;
                }

                for (j = k + 1; j < n; j++) {

                    for (i = 0; i < n; i++) {
                        a[k][i] += work[j] * a[j][i];
                    }
                }

                l = ipvt[k];

                if (l != k) {

                    for (i = 0; i < n; i++) {
                        temp = a[k][i];
                        a[k][i] = a[l][i];
                        a[l][i] = temp;
                    }
                }
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   a     On entry the matrix to be factored On return an upper triangular matrix and the multipliers which
     *                were used to obtain it. The factorization can be written A=L*U where L is a product of permutation
     *                and unit lower triangular matrices and U is upper triangular.
     * @param   n     the order of the matrix A
     * @param   ipvt  an integer vector of pivot indices
     *
     * @return  an integer with normal value of n, with value =K<n if U(K,K) == 0.0. This is not an error condition for
     *          this routine, but it does inidcate that DGESL or DGEDI will divide by zero if called. Flags zeros on
     *          diagonal. Returns Integer.MAX_VALUE if n == 0. This routine will factor a double precision matrix by
     *          Gaussian elimination.
     */

    private int dgefa(double[][] a, int n, int[] ipvt) {

        double t;
        int i, j, k, l;
        int info;
        int max;
        double temp, temp2;

        info = n;

        if (n == 0) {
            return Integer.MAX_VALUE;
        }

        for (k = 0; k < (n - 1); k++) {

            /*Find l=pivot index*/
            if ((n - k) == 0) {
                return Integer.MAX_VALUE;
            }

            max = 0;
            temp = -1.0;

            for (i = 0; i < (n - k); i++) {
                temp2 = Math.abs(a[k][k + i]);

                if (temp2 > temp) {
                    temp = temp2;
                    max = i;
                }
            }

            l = max + k;
            ipvt[k] = l;

            /*Zero pivot implies this column already triangularized*/
            if (a[k][l] != 0.0) {

                /*Interchange if necessary*/
                if (l != k) {
                    t = a[k][l];
                    a[k][l] = a[k][k];
                    a[k][k] = t;
                }

                /*Compute multipliers*/
                t = -1.0 / a[k][k];

                for (i = 0; i < (n - k - 1); i++) {
                    a[k][k + 1 + i] *= t;
                }

                /*Row elimination with column indexing*/
                for (j = k + 1; j < n; j++) {
                    t = a[j][l];

                    if (l != k) {
                        a[j][l] = a[j][k];
                        a[j][k] = t;
                    }

                    for (i = 0; i < (n - k - 1); i++) {
                        a[j][k + 1 + i] += t * a[k][k + 1 + i];
                    }
                }
            } // if(a[k][l]!=0.0)
            else {
                info = k;
            }
        } // for(k=0;k<n-1;k++)

        ipvt[k] = k;

        if (a[k][k] == 0.0) {
            info = k;
        }

        return info;
    }

    /**
     * Copyright 1995-2001 Roger P. Woods, M.D. Modified 5/10/01 void dmodposl() This routine will solve a*x=b using
     * factors computed by MODCHOL On entry
     *
     * @param  a     the output from DGECO or DGEFA
     * @param  n     the order of the matrix A
     * @param  b     the right hand side vector (replaced by soln vector)
     * @param  ipvt  the pivot vector from MODCHOL On return b the solution vector x
     */
    private void dmodposl(double[][] a, int n, double[] b, int[] ipvt) {
        int i; /*  Solve TRANS(R)*y=b  */

        {
            int k;

            for (k = 0; k < n; k++) {
                double t; /* Undo pivoting */

                {
                    int l = ipvt[k];
                    double t2 = b[l];

                    if (l != k) {
                        b[l] = b[k];
                        b[k] = t2;
                    }
                }

                t = 0.0;

                for (i = 0; i < k; i++) {
                    t += a[k][i] * b[i];
                }

                b[k] = (b[k] - t) / a[k][k];
            }
        }

        /*Solve R*x=y */
        if (n != 0) {
            int k;

            for (k = n; (k--) != 0;) {

                b[k] /= a[k][k];

                for (i = 0; i < k; i++) {
                    b[i] += (-b[k]) * a[k][i];
                } /* Undo pivoting */

                {
                    int l = ipvt[k];

                    if (l != k) {

                        double t = b[l];

                        b[l] = b[k];
                        b[k] = t;
                    }
                }
            }
        }
    }

    /* Copyright 1994 Roger P. Woods, M.D. */

    /* Modified 5/7/94 */


    /**
     * .* * DOCUMENT ME!
     *
     * @param   a  On entry the matrix to be factored. Only the diagonal and upper triangle are used On return an upper
     *             triangle matrix R so that A = TRANS(R)*R where TRANS(R) is the transpose. The strict lower triangle
     *             is unaltered If info!=-1, the factorization is not complete
     * @param   n  the order of matrix A
     *
     * @return  DOCUMENT ME!
     */
    private int dpofa(double[][] a, int n) {

        double t;
        double s;
        int i, j, k;
        double temp;

        for (j = 0; j < n; j++) {
            s = 0.0;

            for (k = 0; k < j; k++) {
                temp = 0.0;

                for (i = 0; i < k; i++) {
                    temp += a[k][i] * a[j][i];
                }

                t = a[j][k] - temp;
                t /= a[k][k];
                a[j][k] = t;
                s += t * t;
            }

            s = a[j][j] - s;

            if (s <= 0.0) {
                return j;
            }

            a[j][j] = Math.sqrt(s);
        }

        return j;
    }

    /* Copyright 1995-2001 Roger P. Woods, M.D. */

    /* Modified 5/2/01 */

    /**
     * DOCUMENT ME!
     *
     * @param  a  On entry input matrix
     * @param  n  the order of matrix A
     * @param  b  On entry the right hand side vector On return the solution vector x
     */


    private void dposl(double[][] a, int n, double[] b) {

        double t;
        int i;
        int k;

        /*  Solve TRANS(R)*y=b  */
        for (k = 0; k < n; k++) {
            t = 0.0;

            for (i = 0; i < k; i++) {
                t += a[k][i] * b[i];
            }

            b[k] = (b[k] - t) / a[k][k];
        }

        /*Solve R*x=y */
        if (n != 0) {

            for (k = n; (k--) != 0;) {
                b[k] /= a[k][k];
                t = -b[k];

                for (i = 0; i < k; i++) {
                    b[i] += t * a[k][i];
                }
            }
        }
    }

    /* Copyright 2000-2001 Roger P. Woods, M.D. */

    /* Modified 5/22/01 */

    /*
     * @param a @param n @param ipvt @param g This implements the revised modified Cholesky factorization algorithm
     * described by Schnabel and Eskow.
     *
     * If A is not positive definite, it solves a nearby matrix that is.
     *
     * Schnabel RB, Eskow E, A revised modified Cholesky factorization algorithm,  Siam J Optim., 1999;9(4):1135-1148.
     *
     * Schnabel RB, Eskrow E. A new modified Cholesky factorization. Siam J Sci Stat Comput, 1990;11(6):1136-1158
     *
     * Note that for a sufficiently positive definite matrix, the method is effectively Algorithm 4.2.4 in the 2nd edition
     * of Golub and van Loan's Matrix Computations
     *
     * Returns 0 if matrix was sufficiently positive definite Returns 1 if some other nearby matrix had to be factored
     */

    /**
     * DOCUMENT ME!
     *
     * @param   er  4 by 4 matrix to be inverted
     *
     * @return  true if successful, false if unsuccessful
     */

    private boolean gael(double[][] er) {

        int[] ipvt = new int[4];
        int info;

        double[] work = new double[4];

        info = dgefa(er, 4, ipvt);

        if (info != 4) {
            MipavUtil.displayError("SINGULAR_GAEL_ERROR");

            return false;
        }

        dgedi(er, 4, ipvt, work);

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e                   DOCUMENT ME!
     * @param  de                  DOCUMENT ME!
     * @param  ee                  DOCUMENT ME!
     * @param  tps                 DOCUMENT ME!
     * @param  xoom1               DOCUMENT ME!
     * @param  xoom2               DOCUMENT ME!
     * @param  yoom1               DOCUMENT ME!
     * @param  yoom2               DOCUMENT ME!
     * @param  pixel_size1         DOCUMENT ME!
     * @param  pixel_size2         DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  cubicInterpolation  DOCUMENT ME!
     */
    private void gscale2D_rotatef(double[] e, double[][] de, double[][][] ee, double[] tps, float xoom1, float xoom2,
                                  float yoom1, float yoom2, double pixel_size1, double pixel_size2, int x_dim1,
                                  int x_dim2, int y_dim1, int y_dim2, boolean cubicInterpolation) {

        double[] x = new double[5];
        double[] y = new double[5];
        double[][] dx = new double[5][4];
        double[][] dy = new double[5][4];
        double[][][] ex = new double[5][4][4];
        double[][][] ey = new double[5][4][4];
        double yaw, p, q, gscale;
        double sinyaw, cosyaw;

        yaw = tps[0];
        p = tps[1];
        q = tps[2];
        gscale = tps[3];

        sinyaw = Math.sin(yaw);
        cosyaw = Math.cos(yaw);

        if (!cubicInterpolation) {
            x[0] = ((2 * e[0]) + 1 - x_dim1) * xoom1 * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + 1 - y_dim1) * yoom1 * (pixel_size1 / pixel_size2);
        } else {
            x[0] = ((2 * e[0]) + ((1 - x_dim1) * xoom1)) * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + ((1 - y_dim1) * yoom1)) * (pixel_size1 / pixel_size2);
        }

        /*Apply scaling*/

        x[1] = x[0] * gscale;
        dx[1][3] = x[0];
        ex[1][3][3] = 0;

        y[1] = y[0] * gscale;
        dy[1][3] = y[0];
        ey[1][3][3] = 0;

        /*Apply yaw*/

        x[4] = (x[1] * cosyaw) + (y[1] * sinyaw);
        dx[4][0] = (-x[1] * sinyaw) + (y[1] * cosyaw);
        ex[4][0][0] = (-x[1] * cosyaw) - (y[1] * sinyaw);

        dx[4][3] = (dx[1][3] * cosyaw) + (dy[1][3] * sinyaw);
        ex[4][3][0] = (-dx[1][3] * sinyaw) + (dy[1][3] * cosyaw);
        ex[4][3][3] = 0; /*ex[1][3][3]*cosyaw+ey[1][3][3]*sinyaw;*/

        y[3] = (y[1] * cosyaw) - (x[1] * sinyaw);

        dy[3][0] = (-x[1] * cosyaw) - (y[1] * sinyaw);
        ey[3][0][0] = (x[1] * sinyaw) - (y[1] * cosyaw);

        dy[3][3] = (dy[1][3] * cosyaw) - (dx[1][3] * sinyaw);
        ey[3][3][0] = (-dy[1][3] * sinyaw) - (dx[1][3] * cosyaw);
        ey[3][3][3] = 0; /*ey[1][3][3]*cosyaw-ex[1][3][3]*sinyaw;*/

        e[0] = (x[4] - p + ((x_dim2 - 1) * xoom2)) / (2 * xoom2);

        de[0][0] = dx[4][0] / (2 * xoom2);
        ee[0][0][0] = ex[4][0][0] / (2 * xoom2);
        de[0][1] = -1.0 / (2 * xoom2);
        ee[0][1][0] = 0;
        ee[0][1][1] = 0;
        de[0][2] = 0;
        ee[0][2][0] = 0;
        ee[0][2][1] = 0;
        ee[0][2][2] = 0;
        de[0][3] = dx[4][3] / (2 * xoom2);
        ee[0][3][0] = ex[4][3][0] / (2 * xoom2);
        ee[0][3][1] = 0;
        ee[0][3][2] = 0;
        ee[0][3][3] = ex[4][3][3] / (2 * xoom2);

        e[1] = (y[3] - q + ((y_dim2 - 1) * yoom2)) / (2 * yoom2);

        de[1][0] = dy[3][0] / (2 * yoom2);
        ee[1][0][0] = ey[3][0][0] / (2 * yoom2);
        de[1][1] = 0;
        ee[1][1][0] = 0;
        ee[1][1][1] = 0;
        de[1][2] = -1.0 / (2 * yoom2);
        ee[1][2][0] = 0;
        ee[1][2][1] = 0;
        ee[1][2][2] = 0;
        de[1][3] = dy[3][3] / (2 * yoom2);
        ee[1][3][0] = ey[3][3][0] / (2 * yoom2);
        ee[1][3][1] = 0;
        ee[1][3][2] = 0;
        ee[1][3][3] = ey[3][3][3] / (2 * yoom2);

        /*e[2]=e[2]*/

        de[2][0] = 0;
        ee[2][0][0] = 0;
        de[2][1] = 0;
        ee[2][1][0] = 0;
        ee[2][1][1] = 0;
        de[2][2] = 0;
        ee[2][2][0] = 0;
        ee[2][2][1] = 0;
        ee[2][2][2] = 0;
        de[2][3] = 0;
        ee[2][3][0] = 0;
        ee[2][3][1] = 0;
        ee[2][3][2] = 0;
        ee[2][3][3] = 0;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  c  contains result Multiplies two 4 by 4 matrices
     */


    private void matmul(double[][] a, double[][] b, double[][] c) {

        c[0][0] = (a[0][0] * b[0][0]) + (a[1][0] * b[0][1]) + (a[2][0] * b[0][2]) + (a[3][0] * b[0][3]);
        c[1][0] = (a[0][0] * b[1][0]) + (a[1][0] * b[1][1]) + (a[2][0] * b[1][2]) + (a[3][0] * b[1][3]);
        c[2][0] = (a[0][0] * b[2][0]) + (a[1][0] * b[2][1]) + (a[2][0] * b[2][2]) + (a[3][0] * b[2][3]);
        c[3][0] = (a[0][0] * b[3][0]) + (a[1][0] * b[3][1]) + (a[2][0] * b[3][2]) + (a[3][0] * b[3][3]);

        c[0][1] = (a[0][1] * b[0][0]) + (a[1][1] * b[0][1]) + (a[2][1] * b[0][2]) + (a[3][1] * b[0][3]);
        c[1][1] = (a[0][1] * b[1][0]) + (a[1][1] * b[1][1]) + (a[2][1] * b[1][2]) + (a[3][1] * b[1][3]);
        c[2][1] = (a[0][1] * b[2][0]) + (a[1][1] * b[2][1]) + (a[2][1] * b[2][2]) + (a[3][1] * b[2][3]);
        c[3][1] = (a[0][1] * b[3][0]) + (a[1][1] * b[3][1]) + (a[2][1] * b[3][2]) + (a[3][1] * b[3][3]);

        c[0][2] = (a[0][2] * b[0][0]) + (a[1][2] * b[0][1]) + (a[2][2] * b[0][2]) + (a[3][2] * b[0][3]);
        c[1][2] = (a[0][2] * b[1][0]) + (a[1][2] * b[1][1]) + (a[2][2] * b[1][2]) + (a[3][2] * b[1][3]);
        c[2][2] = (a[0][2] * b[2][0]) + (a[1][2] * b[2][1]) + (a[2][2] * b[2][2]) + (a[3][2] * b[2][3]);
        c[3][2] = (a[0][2] * b[3][0]) + (a[1][2] * b[3][1]) + (a[2][2] * b[3][2]) + (a[3][2] * b[3][3]);

        c[0][3] = (a[0][3] * b[0][0]) + (a[1][3] * b[0][1]) + (a[2][3] * b[0][2]) + (a[3][3] * b[0][3]);
        c[1][3] = (a[0][3] * b[1][0]) + (a[1][3] * b[1][1]) + (a[2][3] * b[1][2]) + (a[3][3] * b[1][3]);
        c[2][3] = (a[0][3] * b[2][0]) + (a[1][3] * b[2][1]) + (a[2][3] * b[2][2]) + (a[3][3] * b[2][3]);
        c[3][3] = (a[0][3] * b[3][0]) + (a[1][3] * b[3][1]) + (a[2][3] * b[3][2]) + (a[3][3] * b[3][3]);

        return;
    }

    /* Copyright 1995-2001 Roger P. Woods, M.D. */

    /* Modified 5/10/01 */

    /* With special thanks to Kate Fissell for identifying a bug in the original*/


    /**
     * DOCUMENT ME!
     *
     * @param   a     DOCUMENT ME!
     * @param   n     DOCUMENT ME!
     * @param   ipvt  DOCUMENT ME!
     * @param   g     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int modchol(double[][] a, int n, int[] ipvt, double[] g) {
        double gamma = 0;
        double mu = 0.1;

        /* epsilon = the largest relative spacing = 2**(1-53), where 53
         * is the number of significant bits in IEEE 754. */
        double epsilon = Math.pow(2.0, -52.0);
        double tau = Math.pow(epsilon, 1.0 / 3.0);
        double tauhat = tau * tau;

        {
            int i;

            for (i = 0; i < n; i++) {

                double temp = Math.abs(a[i][i]);

                if (temp > gamma) {
                    gamma = temp;
                }
            }
        }

        {
            int j;

            /* Phase I, A is potentially positive definite */

            for (j = 0; j < n; j++) {

                int imax = j;

                {
                    double min = a[j][j];
                    double max = min;

                    {
                        int i;

                        for (i = j + 1; i < n; i++) {

                            double temp = a[i][i];

                            if (temp < min) {
                                min = temp;
                            } else if (temp > max) {
                                max = temp;
                                imax = i;
                            }
                        }
                    }

                    if (max < (tauhat * gamma)) {
                        break;
                    }

                    if (min < (-mu * max)) {
                        break;
                    }
                }

                /* Pivot on maximum diagonal of remaining submatrix */

                ipvt[j] = imax;

                if (imax != j) {

                    {
                        double temp = a[j][j];

                        a[j][j] = a[imax][imax];
                        a[imax][imax] = temp;
                    }

                    {
                        int i;

                        for (i = 0; i < j; i++) {

                            double temp = a[j][i];
                            a[j][i] = a[imax][i];
                            a[imax][i] = temp;
                        }
                    }

                    {
                        int i;

                        for (i = imax + 1; i < n; i++) {

                            double temp = a[i][imax];
                            a[i][imax] = a[i][j];
                            a[i][j] = temp;
                        }
                    }

                    {
                        int i;

                        for (i = j + 1; i < imax; i++) {

                            double temp = a[i][j];
                            a[i][j] = a[imax][i];
                            a[imax][i] = temp;
                        }
                    }
                }

                {

                    if (j < (n - 1)) {

                        double min = a[j + 1][j + 1] - (a[j + 1][j] * a[j + 1][j] / a[j][j]);

                        {
                            int i;

                            for (i = j + 2; i < n; i++) {

                                double temp = a[i][i] - (a[i][j] * a[i][j] / a[j][j]);

                                if (temp < min) {
                                    min = temp;
                                }
                            }
                        }

                        if (min < (-mu * gamma)) {
                            break;
                        }
                    }

                    /* Perform jth iteration of factorization */
                    a[j][j] = Math.sqrt(a[j][j]);

                    {
                        int i;

                        for (i = j + 1; i < n; i++) {

                            a[i][j] /= a[j][j];

                            {
                                int k;

                                for (k = j + 1; k <= i; k++) {
                                    a[i][k] -= a[i][j] * a[k][j];
                                }
                            }
                        }
                    }
                }
            }

            if (j == n) {
                return 0;
            }

            /* Phase two, a not positive definite */
            if (j == (n - 1)) {

                double delta = tau * (-a[j][j]) / (1 - tau);
                double temp = tauhat * gamma;

                ipvt[j] = j;

                if (temp > delta) {
                    delta = temp;
                }

                a[j][j] = Math.sqrt(delta);

                return 1;
            } else {
                int k = j;
                double deltaprev = 0; /* Calculate the lower Gerschgorin Bounds of a[k]*/

                {
                    int i;

                    for (i = k; i < n; i++) {

                        g[i] = a[i][i];

                        {
                            int j1;

                            for (j1 = k; j1 < i; j1++) {

                                g[i] -= Math.abs(a[i][j1]);
                            }
                        }

                        {
                            int j1;

                            for (j1 = i + 1; j1 < n; j1++) {

                                g[i] -= Math.abs(a[j1][i]);
                            }
                        }
                    }
                }

                {

                    /* Modified Cholesky decomposition */
                    int j1;

                    for (j1 = k; (j1 + 2) < n; j1++) {

                        int imax = j1;
                        double max = g[j1]; /* Pivot on maximum lower Gerschgorin bound estimate */

                        {
                            int i;

                            for (i = j1 + 1; i < n; i++) {

                                if (g[i] > max) {
                                    max = g[i];
                                    imax = i;
                                }
                            }
                        }

                        ipvt[j1] = imax;

                        if (imax != j1) {

                            {
                                double temp = a[j1][j1];

                                a[j1][j1] = a[imax][imax];
                                a[imax][imax] = temp;
                            }

                            {
                                int i;

                                for (i = 0; i < j1; i++) {

                                    double temp = a[j1][i];
                                    a[j1][i] = a[imax][i];
                                    a[imax][i] = temp;
                                }
                            }

                            {
                                int i;

                                for (i = imax + 1; i < n; i++) {

                                    double temp = a[i][imax];
                                    a[i][imax] = a[i][j1];
                                    a[i][j1] = temp;
                                }
                            }

                            {
                                int i;

                                for (i = j1 + 1; i < imax; i++) {

                                    double temp = a[i][j1];
                                    a[i][j1] = a[imax][i];
                                    a[imax][i] = temp;
                                }
                            }
                        } /* Calculate E[j1][j1] and add to diagonal */

                        {
                            double normj = 0;
                            double delta;

                            {
                                int i;

                                for (i = j1 + 1; i < n; i++) {

                                    normj += Math.abs(a[i][j1]);
                                }
                            }

                            delta = normj;

                            if ((tauhat * gamma) > delta) {
                                delta = tauhat * gamma;
                            }

                            delta -= a[j1][j1];

                            if (delta < 0) {
                                delta = 0;
                            }

                            if (delta < deltaprev) {
                                delta = deltaprev;
                            }

                            if (delta > 0) {
                                a[j1][j1] += delta;
                                deltaprev = delta;
                            }

                            /* Update Gerschgorin bound estimates */
                            if (a[j1][j1] != normj) {

                                double temp = 1 - (normj / a[j1][j1]);

                                {
                                    int i;

                                    for (i = j1 + 1; i < n; i++) {

                                        g[i] += Math.abs(a[i][j1]) * temp;
                                    }
                                }
                            }
                        }

                        /* Perform j1th iteration of factorization */
                        a[j1][j1] = Math.sqrt(a[j1][j1]);

                        {
                            int i;

                            for (i = j1 + 1; i < n; i++) {

                                a[i][j1] /= a[j1][j1];

                                {
                                    int k1;

                                    for (k1 = j1 + 1; k1 <= i; k1++) {

                                        a[i][k1] -= a[i][j1] * a[k1][j1];
                                    }
                                }
                            }
                        }
                    }
                } /* Final 2x2 submatrix */

                {
                    double lambdahi = a[n - 2][n - 2] - a[n - 1][n - 1];
                    double lambdalo;

                    ipvt[n - 2] = n - 2;
                    ipvt[n - 1] = n - 1;

                    lambdahi = lambdahi * lambdahi;
                    lambdahi += 4 * a[n - 1][n - 2] * a[n - 1][n - 2];
                    lambdahi = Math.sqrt(lambdahi);
                    lambdalo = -lambdahi;
                    lambdahi += (a[n - 2][n - 2] + a[n - 1][n - 1]);
                    lambdalo += (a[n - 2][n - 2] + a[n - 1][n - 1]);
                    lambdahi /= 2;
                    lambdalo /= 2;

                    {
                        double delta = tau * (lambdahi - lambdalo) / (1 - tau);

                        if ((tauhat * gamma) > delta) {
                            delta = tauhat * gamma;
                        }

                        delta -= lambdalo;

                        if (delta < 0) {
                            delta = 0;
                        }

                        if (delta < deltaprev) {
                            delta = deltaprev;
                        }

                        if (delta > 0) {
                            a[n - 2][n - 2] += delta;
                            a[n - 1][n - 1] += delta;
                            deltaprev = delta;
                        }

                        a[n - 2][n - 2] = Math.sqrt(a[n - 2][n - 2]);
                        a[n - 1][n - 2] /= a[n - 2][n - 2];
                        a[n - 1][n - 1] = Math.sqrt(a[n - 1][n - 1] - (a[n - 1][n - 2] * a[n - 1][n - 2]));
                    }
                }

                return 1;
            }
        }
    }

    /* Copyright 1995-2001 Roger P. Woods, M.D. */

    /* Modified 5/27/01 */


    /**
     * DOCUMENT ME!
     *
     * @param   volume  source array to be resliced
     * @param   x_dim1  x dimension of source array
     * @param   y_dim1  y dimension of source array
     * @param   z_dim1  z dimension of source array
     * @param   x_dim2  x dimension of output array
     * @param   y_dim2  y dimension of output array
     * @param   z_dim2  z dimension of output array
     * @param   es      rescaling parameters
     *
     * @return  resliced data array if successful, null if unsuccessful
     *
     *          <p>This routine will reslice a file based on the unit vectors es It uses nearest neighbor interpolation
     *          and can do perspective transformations</p>
     */

    private float[] nnreslicer(float[] volume, int x_dim1, int y_dim1, int z_dim1, int x_dim2, int y_dim2, int z_dim2,
                               double[][] es) {

        int i, j, k;
        int i3, j3, k3;
        int x_max1, y_max1, z_max1;
        int x_up;
        int y_up;
        int z_up;
        double x_i, x_j, x_k, x_p;
        double y_i, y_j, y_k, y_p;
        double z_i, z_j, z_k, z_p;
        double t_i, t_j, t_k;
        double e00, e01, e02, e03, e10, e11, e12, e13, e20, e21, e22, e23, e30, e31, e32, e33;
        float[] new_volume;
        int slice1;
        int slice2;

        slice1 = x_dim1 * y_dim1;
        slice2 = x_dim2 * y_dim2;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e03 = es[0][3];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e13 = es[1][3];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e23 = es[2][3];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];
        e33 = es[3][3];

        try {
            new_volume = new float[x_dim2 * y_dim2 * z_dim2];
        } catch (OutOfMemoryError er) {
            MipavUtil.displayError("unable to allocate memory to reslice file\n");

            return null;
        }

        for (i = 0; i < new_volume.length; i++) {
            new_volume[i] = 0.0f;
        }

        for (k = 0, k3 = 0, x_k = e30, y_k = e31, z_k = e32, t_k = e33; k < z_dim2;
                 k++, k3 += slice2, x_k += e20, y_k += e21, z_k += e22, t_k += e23) {

            for (j = 0, j3 = k3, x_j = x_k, y_j = y_k, z_j = z_k, t_j = t_k; j < y_dim2;
                     j++, j3 += x_dim2, x_j += e10, y_j += e11, z_j += e12, t_j += e13) {

                for (i = 0, i3 = j3, x_i = x_j, y_i = y_j, z_i = z_j, t_i = t_j; i < x_dim2;
                         i++, i3++, x_i += e00, y_i += e01, z_i += e02, t_i += e03) {
                    x_p = x_i / t_i;
                    x_up = (int) Math.floor(x_p + .5);

                    if ((x_up >= 0) && (x_up <= x_max1)) {
                        y_p = y_i / t_i;
                        y_up = (int) Math.floor(y_p + .5);

                        if ((y_up >= 0) && (y_up <= y_max1)) {
                            z_p = z_i / t_i;
                            z_up = (int) Math.floor(z_p + .5);

                            if ((z_up >= 0) && (z_up <= z_max1)) {
                                new_volume[i3] = volume[(z_up * slice1) + (y_up * x_dim1) + x_up];
                            }
                        }
                    }
                }
            }
        }

        return new_volume;
    }

    /* Copyright 1995-6 Roger P. Woods, M.D. */

    /* Modified 12/31/96 */

    /**
     * DOCUMENT ME!
     *
     * @param  e                   DOCUMENT ME!
     * @param  de                  DOCUMENT ME!
     * @param  ee                  DOCUMENT ME!
     * @param  tps                 DOCUMENT ME!
     * @param  xoom1               DOCUMENT ME!
     * @param  xoom2               DOCUMENT ME!
     * @param  yoom1               DOCUMENT ME!
     * @param  yoom2               DOCUMENT ME!
     * @param  pixel_size1         DOCUMENT ME!
     * @param  pixel_size2         DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  cubicInterpolation  DOCUMENT ME!
     */
    private void rigid_2D_rotatef(double[] e, double[][] de, double[][][] ee, double[] tps, float xoom1, float xoom2,
                                  float yoom1, float yoom2, double pixel_size1, double pixel_size2, int x_dim1,
                                  int x_dim2, int y_dim1, int y_dim2, boolean cubicInterpolation) {

        double[] x = new double[5];
        double[] y = new double[5];
        double[][] dx = new double[5][3];
        double[][] dy = new double[5][3];
        double[][][] ex = new double[5][3][3];
        double[][][] ey = new double[5][3][3];
        double yaw, p, q;
        double sinyaw, cosyaw;

        yaw = tps[0];
        p = tps[1];
        q = tps[2];

        sinyaw = Math.sin(yaw);
        cosyaw = Math.cos(yaw);

        if (!cubicInterpolation) {
            x[0] = ((2 * e[0]) + 1 - x_dim1) * xoom1 * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + 1 - y_dim1) * yoom1 * (pixel_size1 / pixel_size2);
        } else {
            x[0] = ((2 * e[0]) + ((1 - x_dim1) * xoom1)) * (pixel_size1 / pixel_size2);
            y[0] = ((2 * e[1]) + ((1 - y_dim1) * yoom1)) * (pixel_size1 / pixel_size2);
        }

        /*Apply scaling*/

        x[1] = x[0];
        y[1] = y[0];

        /*Apply yaw*/

        x[4] = (x[1] * cosyaw) + (y[1] * sinyaw);
        dx[4][0] = (-x[1] * sinyaw) + (y[1] * cosyaw);
        ex[4][0][0] = (-x[1] * cosyaw) - (y[1] * sinyaw);

        y[3] = (y[1] * cosyaw) - (x[1] * sinyaw);

        dy[3][0] = (-x[1] * cosyaw) - (y[1] * sinyaw);
        ey[3][0][0] = (x[1] * sinyaw) - (y[1] * cosyaw);

        e[0] = (x[4] - p + ((x_dim2 - 1) * xoom2)) / (2 * xoom2);

        de[0][0] = dx[4][0] / (2 * xoom2);
        ee[0][0][0] = ex[4][0][0] / (2 * xoom2);
        de[0][1] = -1.0 / (2 * xoom2);
        ee[0][1][0] = 0;
        ee[0][1][1] = 0;
        de[0][2] = 0;
        ee[0][2][0] = 0;
        ee[0][2][1] = 0;
        ee[0][2][2] = 0;

        e[1] = (y[3] - q + ((y_dim2 - 1) * yoom2)) / (2 * yoom2);

        de[1][0] = dy[3][0] / (2 * yoom2);
        ee[1][0][0] = ey[3][0][0] / (2 * yoom2);
        de[1][1] = 0;
        ee[1][1][0] = 0;
        ee[1][1][1] = 0;
        de[1][2] = -1.0 / (2 * yoom2);
        ee[1][2][0] = 0;
        ee[1][2][1] = 0;
        ee[1][2][2] = 0;

        return;
    }


    /**
     * DOCUMENT ME!
     *
     * @param   delta       DOCUMENT ME!
     * @param   dsd         First derivs have previously been multiplied by -1
     * @param   esd         DOCUMENT ME!
     * @param   parameters  DOCUMENT ME!
     *
     * @return  the value of the predicted sd
     *
     *          <p>This routine estimates the predicted standard deviation based on first and second derivatives. Since
     *          it is derivative based, it leaves out a constant additive factor and is therefore only useful in
     *          estimating changes in sd. not the true value of sd</p>
     */


    private double sdpred(double[] delta, double[] dsd, double[][] esd, int parameters) {

        int ii, jj;
        double total;

        total = 0.0;

        /* Exploit symmetry of esd */
        for (jj = 0; jj < parameters; jj++) {
            total += -delta[jj] * dsd[jj];

            for (ii = 0; ii < jj; ii++) {
                total += delta[jj] * esd[jj][ii] * delta[ii];
            }

            total += delta[jj] * esd[jj][jj] * delta[jj] / 2;
        }

        return total;
    }

    /* Copyright 1995 Roger P. Woods, M.D. */

    /* Modified 12/12/95 */

    /**
     * DOCUMENT ME!
     *
     * @param   volume  source array to be resliced
     * @param   x_dim1  x dimension of source array
     * @param   y_dim1  y dimension of source array
     * @param   z_dim1  z dimension of source array
     * @param   x_dim2  x dimension of output array
     * @param   y_dim2  y dimension of output array
     * @param   z_dim2  z dimension of output array
     * @param   es      rescaling parameters
     * @param   xkern   x half-window width
     * @param   ykern   y half-window width
     * @param   zkern   z half-window width
     *
     * @return  resliced data array if successful, null if unsuccessful
     *
     *          <p>This routine will reslice a file based on the unit vectors es sinc interpolation is used</p>
     */


    private float[] sincer(float[] volume, int x_dim1, int y_dim1, int z_dim1, int x_dim2, int y_dim2, int z_dim2,
                           double[][] es, int xkern, int ykern, int zkern) {

        int i, j, k;
        int i4, j4, k4;
        int i2, j2, k2;
        int i3, j3, k3;
        int x_max1, y_max1, z_max1;
        double total;
        double x_i, x_j, x_k, x_p;
        double y_i, y_j, y_k, y_p;
        double z_i, z_j, z_k, z_p;
        double t_i, t_j, t_k;
        double e00, e01, e02, e03, e10, e11, e12, e13, e20, e21, e22, e23, e30, e31, e32, e33;
        double sincx, sincy, sincz;
        float[] new_volume;
        int slice1;
        int slice2;

        slice1 = x_dim1 * y_dim1;
        slice2 = x_dim2 * y_dim2;

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;

        e00 = es[0][0];
        e01 = es[0][1];
        e02 = es[0][2];
        e03 = es[0][3];
        e10 = es[1][0];
        e11 = es[1][1];
        e12 = es[1][2];
        e13 = es[1][3];
        e20 = es[2][0];
        e21 = es[2][1];
        e22 = es[2][2];
        e23 = es[2][3];
        e30 = es[3][0];
        e31 = es[3][1];
        e32 = es[3][2];
        e33 = es[3][3];

        try {
            new_volume = new float[x_dim2 * y_dim2 * z_dim2];
        } catch (OutOfMemoryError er) {
            MipavUtil.displayError("unable to allocate memory to reslice file\n");

            return null;
        }

        for (i = 0; i < new_volume.length; i++) {
            new_volume[i] = 0.0f;
        }

        for (k = 0, k3 = 0, x_k = e30, y_k = e31, z_k = e32, t_k = e33; k < z_dim2;
                 k++, k3 += slice2, x_k += e20, y_k += e21, z_k += e22, t_k += e23) {

            for (j = 0, j3 = k3, x_j = x_k, y_j = y_k, z_j = z_k, t_j = t_k; j < y_dim2;
                     j++, j3 += x_dim2, x_j += e10, y_j += e11, z_j += e12, t_j += e13) {

                for (i = 0, i3 = j3, x_i = x_j, y_i = y_j, z_i = z_j, t_i = t_j; i < x_dim2;
                         i++, i3++, x_i += e00, y_i += e01, z_i += e02, t_i += e03) {
                    x_p = x_i / t_i;

                    if ((x_p >= -0.5) && (x_p <= x_max1)) {
                        y_p = y_i / t_i;

                        if ((y_p >= -0.5) && (y_p <= y_max1)) {
                            z_p = z_i / t_i;

                            if ((z_p >= -0.5) && (z_p <= z_max1)) {
                                total = 0;

                                for (k4 = (int) Math.ceil(z_p), k2 = k4 * slice1;
                                         (k4 < z_dim1) && (k4 < ((int) Math.ceil(z_p) + zkern)); k4++, k2 += slice1) {
                                    sincz = wsinc((z_p - k4), zkern);

                                    for (j4 = (int) Math.ceil(y_p), j2 = k2 + (j4 * x_dim1);
                                             (j4 < y_dim1) && (j4 < ((int) Math.ceil(y_p) + ykern));
                                             j4++, j2 += x_dim1) {
                                        sincy = wsinc((y_p - j4), ykern);

                                        for (i4 = (int) Math.ceil(x_p), i2 = j2 + i4;
                                                 (i4 < x_dim1) && (i4 < ((int) Math.ceil(x_p) + xkern)); i4++, i2++) {
                                            sincx = wsinc((x_p - i4), xkern);
                                            total += volume[i2] * sincx * sincy * sincz;
                                        }

                                        for (i4 = (int) Math.ceil(x_p) - 1, i2 = j2 + i4;
                                                 (i4 >= 0) && (i4 > ((int) Math.ceil(x_p) - 1 - xkern)); i4--, i2--) {
                                            sincx = wsinc((x_p - i4), xkern);
                                            total += volume[i2] * sincx * sincy * sincz;
                                        }
                                    }

                                    for (j4 = (int) Math.ceil(y_p) - 1, j2 = k2 + (j4 * x_dim1);
                                             (j4 >= 0) && (j4 > ((int) Math.ceil(y_p) - 1 - ykern));
                                             j4--, j2 -= x_dim1) {
                                        sincy = wsinc((y_p - j4), ykern);

                                        for (i4 = (int) Math.ceil(x_p), i2 = j2 + i4;
                                                 (i4 < x_dim1) && (i4 < ((int) Math.ceil(x_p) + xkern)); i4++, i2++) {
                                            sincx = wsinc((x_p - i4), xkern);
                                            total += volume[i2] * sincx * sincy * sincz;
                                        }

                                        for (i4 = (int) Math.ceil(x_p) - 1, i2 = j2 + i4;
                                                 (i4 >= 0) && (i4 > ((int) Math.ceil(x_p) - 1 - xkern)); i4--, i2--) {
                                            sincx = wsinc((x_p - i4), xkern);
                                            total += volume[i2] * sincx * sincy * sincz;
                                        }
                                    }

                                }

                                for (k4 = (int) Math.ceil(z_p) - 1, k2 = k4 * slice1;
                                         (k4 >= 0) && (k4 > ((int) Math.ceil(z_p) - 1 - zkern)); k4--, k2 -= slice1) {
                                    sincz = wsinc((z_p - k4), zkern);

                                    for (j4 = (int) Math.ceil(y_p), j2 = k2 + (j4 * x_dim1);
                                             (j4 < y_dim1) && (j4 < ((int) Math.ceil(y_p) + ykern));
                                             j4++, j2 += x_dim1) {
                                        sincy = wsinc((y_p - j4), ykern);

                                        for (i4 = (int) Math.ceil(x_p), i2 = j2 + i4;
                                                 (i4 < x_dim1) && (i4 < ((int) Math.ceil(x_p) + xkern)); i4++, i2++) {
                                            sincx = wsinc((x_p - i4), xkern);
                                            total += volume[i2] * sincx * sincy * sincz;
                                        }

                                        for (i4 = (int) Math.ceil(x_p) - 1, i2 = j2 + i4;
                                                 (i4 >= 0) && (i4 > ((int) Math.ceil(x_p) - 1 - xkern)); i4--, i2--) {
                                            sincx = wsinc((x_p - i4), xkern);
                                            total += volume[i2] * sincx * sincy * sincz;
                                        }
                                    }

                                    for (j4 = (int) Math.ceil(y_p) - 1, j2 = k2 + (j4 * x_dim1);
                                             (j4 >= 0) && (j4 > ((int) Math.ceil(y_p) - 1 - ykern));
                                             j4--, j2 -= x_dim1) {
                                        sincy = wsinc((y_p - j4), ykern);

                                        for (i4 = (int) Math.ceil(x_p), i2 = j2 + i4;
                                                 (i4 < x_dim1) && (i4 < ((int) Math.ceil(x_p) + xkern)); i4++, i2++) {
                                            sincx = wsinc((x_p - i4), xkern);
                                            total += volume[i2] * sincx * sincy * sincz;
                                        }

                                        for (i4 = (int) Math.ceil(x_p) - 1, i2 = j2 + i4;
                                                 (i4 >= 0) && (i4 > ((int) Math.ceil(x_p) - 1 - xkern)); i4--, i2--) {
                                            sincx = wsinc((x_p - i4), xkern);
                                            total += volume[i2] * sincx * sincy * sincz;
                                        }
                                    }

                                }

                                new_volume[i3] = (float) total;
                            }
                        }
                    }
                }
            }
        }

        return new_volume;
    }

    /* Copyright 1995 Roger P. Woods, M.D. */

    /* Modified 11/28/95 */

    /**
     * In-plane rigid-body model
     *
     * @param  tps                 yaw, p, q in radians, and half-pixels
     * @param  e                   corresponding forward unit vectors
     * @param  de                  1st derivatives of unit vectors with respect to yaw,p,q
     * @param  ee                  2nd derivatives of unit vectors with respect to yaw,p,q
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  x_size2             DOCUMENT ME!
     * @param  y_size2             DOCUMENT ME!
     * @param  z_size2             DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  x_size1             DOCUMENT ME!
     * @param  y_size1             DOCUMENT ME!
     * @param  z_size1             DOCUMENT ME!
     * @param  cubicInterpolation
     *                             <p>will take yaw,p, and q and convert them to the corresponding forward unit vectors
     *                             & derivatives</p>
     */


    private void uv2D3(double[] tps, double[][] e, double[][][] de, double[][][][] ee, int x_dim2, int y_dim2,
                       float x_size2, float y_size2, float z_size2, int x_dim1, int y_dim1, float x_size1,
                       float y_size1, float z_size1, boolean cubicInterpolation) {

        float xoom1, xoom2;
        float yoom1, yoom2;
        double pixel_size1, pixel_size2;
        int s, t;

        pixel_size1 = x_size1;

        if (y_size1 < pixel_size1) {
            pixel_size1 = y_size1;
        }

        if (z_size1 < pixel_size1) {
            pixel_size1 = z_size1;
        }

        xoom1 = (float) (x_size1 / pixel_size1);
        yoom1 = (float) (y_size1 / pixel_size1);

        pixel_size2 = x_size2;

        if (y_size2 < pixel_size2) {
            pixel_size2 = y_size2;
        }

        if (z_size2 < pixel_size2) {
            pixel_size2 = z_size2;
        }

        xoom2 = (float) (x_size2 / pixel_size2);
        yoom2 = (float) (y_size2 / pixel_size2);

        e[0][0] = 1;
        e[0][1] = 0;
        e[0][2] = 0; /*constant*/

        e[1][0] = 0;
        e[1][1] = 1;
        e[1][2] = 0; /*constant*/

        e[2][0] = 0; /*constant*/
        e[2][1] = 0; /*constant*/
        e[2][2] = 1; /*constant*/

        e[3][0] = 0;
        e[3][1] = 0;
        e[3][2] = 0; /*constant*/

        /*And the constant row*/
        e[0][3] = 0;
        e[1][3] = 0;
        e[2][3] = 0;
        e[3][3] = 1;

        rigid_2D_rotatef(e[0], de[0], ee[0], tps, xoom1, xoom2, yoom1, yoom2, pixel_size1, pixel_size2, x_dim1, x_dim2,
                         y_dim1, y_dim2, cubicInterpolation);
        rigid_2D_rotatef(e[1], de[1], ee[1], tps, xoom1, xoom2, yoom1, yoom2, pixel_size1, pixel_size2, x_dim1, x_dim2,
                         y_dim1, y_dim2, cubicInterpolation);
        rigid_2D_rotatef(e[3], de[3], ee[3], tps, xoom1, xoom2, yoom1, yoom2, pixel_size1, pixel_size2, x_dim1, x_dim2,
                         y_dim1, y_dim2, cubicInterpolation);

        e[2][0] = 0;
        e[1][0] = e[1][0] - e[3][0];
        e[0][0] = e[0][0] - e[3][0];

        e[2][1] = 0;
        e[1][1] = e[1][1] - e[3][1];
        e[0][1] = e[0][1] - e[3][1];

        e[3][2] = 0;

        e[2][2] = 1;
        e[1][2] = 0;
        e[0][2] = 0;

        for (t = 0; t < 3; t++) {
            de[2][0][t] = 0;
            de[1][0][t] = de[1][0][t] - de[3][0][t];
            de[0][0][t] = de[0][0][t] - de[3][0][t];

            de[2][1][t] = 0;
            de[1][1][t] = de[1][1][t] - de[3][1][t];
            de[0][1][t] = de[0][1][t] - de[3][1][t];

            de[3][2][t] = 0;

            de[2][2][t] = 0;
            de[1][2][t] = 0;
            de[0][2][t] = 0;

            de[0][3][t] = 0;
            de[1][3][t] = 0;
            de[2][3][t] = 0;
            de[3][3][t] = 0;

            for (s = 0; s < 3; s++) {

                ee[2][0][t][s] = 0;
                ee[1][0][t][s] = ee[1][0][t][s] - ee[3][0][t][s];
                ee[0][0][t][s] = ee[0][0][t][s] - ee[3][0][t][s];

                ee[2][1][t][s] = 0;
                ee[1][1][t][s] = ee[1][1][t][s] - ee[3][1][t][s];
                ee[0][1][t][s] = ee[0][1][t][s] - ee[3][1][t][s];

                ee[3][2][t][s] = 0;

                ee[2][2][t][s] = 0;
                ee[1][2][t][s] = 0;
                ee[0][2][t][s] = 0;

                ee[0][3][t][s] = 0;
                ee[1][3][t][s] = 0;
                ee[2][3][t][s] = 0;
                ee[3][3][t][s] = 0;
            }

        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tps                 yaw,p,q,gscale in radians, half-pixels, and dimensionless scaling
     * @param  e                   corresponding forward unit vectors
     * @param  de                  1st derivatives of unit vectors with respect to yaw,p,q,gscale
     * @param  ee                  2nd derivatives of unit vectors with respect to yaw,p,q,gscale
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  x_size2             DOCUMENT ME!
     * @param  y_size2             DOCUMENT ME!
     * @param  z_size2             DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  x_size1             DOCUMENT ME!
     * @param  y_size1             DOCUMENT ME!
     * @param  z_size1             DOCUMENT ME!
     * @param  cubicInterpolation  In-plane global rescaling model
     *
     *                             <p>will take yaw,p,q,and gscale and convert them to the corresponding forward unit
     *                             vectors & derivatives</p>
     */


    private void uv2D4(double[] tps, double[][] e, double[][][] de, double[][][][] ee, int x_dim2, int y_dim2,
                       float x_size2, float y_size2, float z_size2, int x_dim1, int y_dim1, float x_size1,
                       float y_size1, float z_size1, boolean cubicInterpolation) {

        float xoom1, xoom2;
        float yoom1, yoom2;
        double pixel_size1, pixel_size2;
        int s, t;

        pixel_size1 = x_size1;

        if (y_size1 < pixel_size1) {
            pixel_size1 = y_size1;
        }

        if (z_size1 < pixel_size1) {
            pixel_size1 = z_size1;
        }

        xoom1 = (float) (x_size1 / pixel_size1);
        yoom1 = (float) (y_size1 / pixel_size1);

        pixel_size2 = x_size2;

        if (y_size2 < pixel_size2) {
            pixel_size2 = y_size2;
        }

        if (z_size2 < pixel_size2) {
            pixel_size2 = z_size2;
        }

        xoom2 = (float) (x_size2 / pixel_size2);
        yoom2 = (float) (y_size2 / pixel_size2);

        e[0][0] = 1;
        e[0][1] = 0;
        e[0][2] = 0;

        e[1][0] = 0;
        e[1][1] = 1;
        e[1][2] = 0;

        e[2][0] = 0;
        e[2][1] = 0;
        e[2][2] = 1;

        e[3][0] = 0;
        e[3][1] = 0;
        e[3][2] = 0;

        /*And the constant row*/
        e[0][3] = 0;
        e[1][3] = 0;
        e[2][3] = 0;
        e[3][3] = 1;

        gscale2D_rotatef(e[0], de[0], ee[0], tps, xoom1, xoom2, yoom1, yoom2, pixel_size1, pixel_size2, x_dim1, x_dim2,
                         y_dim1, y_dim2, cubicInterpolation);
        gscale2D_rotatef(e[1], de[1], ee[1], tps, xoom1, xoom2, yoom1, yoom2, pixel_size1, pixel_size2, x_dim1, x_dim2,
                         y_dim1, y_dim2, cubicInterpolation);
        gscale2D_rotatef(e[2], de[2], ee[2], tps, xoom1, xoom2, yoom1, yoom2, pixel_size1, pixel_size2, x_dim1, x_dim2,
                         y_dim1, y_dim2, cubicInterpolation);
        gscale2D_rotatef(e[3], de[3], ee[3], tps, xoom1, xoom2, yoom1, yoom2, pixel_size1, pixel_size2, x_dim1, x_dim2,
                         y_dim1, y_dim2, cubicInterpolation);

        e[2][0] = e[2][0] - e[3][0];
        e[1][0] = e[1][0] - e[3][0];
        e[0][0] = e[0][0] - e[3][0];

        e[2][1] = e[2][1] - e[3][1];
        e[1][1] = e[1][1] - e[3][1];
        e[0][1] = e[0][1] - e[3][1];

        e[2][2] = e[2][2] - e[3][2];
        e[1][2] = e[1][2] - e[3][2];
        e[0][2] = e[0][2] - e[3][2];

        for (t = 0; t < 4; t++) {
            de[2][0][t] = de[2][0][t] - de[3][0][t];
            de[1][0][t] = de[1][0][t] - de[3][0][t];
            de[0][0][t] = de[0][0][t] - de[3][0][t];

            de[2][1][t] = de[2][1][t] - de[3][1][t];
            de[1][1][t] = de[1][1][t] - de[3][1][t];
            de[0][1][t] = de[0][1][t] - de[3][1][t];

            de[2][2][t] = de[2][2][t] - de[3][2][t];
            de[1][2][t] = de[1][2][t] - de[3][2][t];
            de[0][2][t] = de[0][2][t] - de[3][2][t];

            de[0][3][t] = 0;
            de[1][3][t] = 0;
            de[2][3][t] = 0;
            de[3][3][t] = 0;

            for (s = 0; s <= t; s++) {

                ee[2][0][t][s] = ee[2][0][t][s] - ee[3][0][t][s];
                ee[1][0][t][s] = ee[1][0][t][s] - ee[3][0][t][s];
                ee[0][0][t][s] = ee[0][0][t][s] - ee[3][0][t][s];

                ee[2][1][t][s] = ee[2][1][t][s] - ee[3][1][t][s];
                ee[1][1][t][s] = ee[1][1][t][s] - ee[3][1][t][s];
                ee[0][1][t][s] = ee[0][1][t][s] - ee[3][1][t][s];

                ee[2][2][t][s] = ee[2][2][t][s] - ee[3][2][t][s];
                ee[1][2][t][s] = ee[1][2][t][s] - ee[3][2][t][s];
                ee[0][2][t][s] = ee[0][2][t][s] - ee[3][2][t][s];

                ee[0][3][t][s] = 0;
                ee[1][3][t][s] = 0;
                ee[2][3][t][s] = 0;
                ee[3][3][t][s] = 0;
            }

        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tps  DOCUMENT ME!
     * @param  ef   DOCUMENT ME!
     * @param  def  DOCUMENT ME!
     * @param  eef
     *              <p>will calculate first and second derivatives with respect to the parameters for an in-plane affine
     *              6 parameter model.</p>
     *
     *              <p>Note that a full 4 x 4 matrix is assumed.</p>
     */


    private void uv2D6(double[] tps, double[][] ef, double[][][] def, double[][][][] eef) {
        int m, n, o, p;

        /*Define forward transform*/

        ef[0][0] = tps[0];
        ef[1][0] = tps[1];
        ef[2][0] = 0;
        ef[3][0] = tps[2];
        ef[0][1] = tps[3];
        ef[1][1] = tps[4];
        ef[2][1] = 0;
        ef[3][1] = tps[5];
        ef[0][2] = 0;
        ef[1][2] = 0;
        ef[2][2] = 1;
        ef[3][2] = 0;
        ef[0][3] = 0;
        ef[1][3] = 0;
        ef[2][3] = 0;
        ef[3][3] = 1;

        /*All second partial derivatives are zero for forward transform*/
        for (m = 0; m < 4; m++) {

            for (n = 0; n < 4; n++) {

                for (o = 0; o < 6; o++) {
                    def[m][n][o] = 0;

                    for (p = 0; p <= o; p++) {
                        eef[m][n][o][p] = 0;
                    }
                }
            }
        }

        /*First partial derivatives are zero except where the parameter*/

        /* is the factor being differentiated in which case it is one*/
        def[0][0][0] = 1;
        def[1][0][1] = 1;

        def[3][0][2] = 1;
        def[0][1][3] = 1;
        def[1][1][4] = 1;

        def[3][1][5] = 1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tps                 pitch,roll,yaw,p,q,r,gscale in radians,half-pixels, and dimensionless
     * @param  e                   corresponding forward unit vectors
     * @param  de                  1st derivatives of unit vectors with respect to ptich, roll, yaw, p, q, r, gscale
     * @param  ee                  wnd derivatives fo unit vectors with respect to pitch, roll, yaw, p, q, r, gscale
     * @param  x_dim2              DOCUMENT ME!
     * @param  y_dim2              DOCUMENT ME!
     * @param  z_dim2              DOCUMENT ME!
     * @param  x_size2             DOCUMENT ME!
     * @param  y_size2             DOCUMENT ME!
     * @param  z_size2             DOCUMENT ME!
     * @param  x_dim1              DOCUMENT ME!
     * @param  y_dim1              DOCUMENT ME!
     * @param  z_dim1              DOCUMENT ME!
     * @param  x_size1             DOCUMENT ME!
     * @param  y_size1             DOCUMENT ME!
     * @param  z_size1             DOCUMENT ME!
     * @param  cubicInterpolation  will take pitch,roll,yaw,p,q,r,and gscale and convert them to the corresponding
     *                             forward unit vectors & derivatives
     */

    private void uv3D7(double[] tps, double[][] e, double[][][] de, double[][][][] ee, int x_dim2, int y_dim2,
                       int z_dim2, float x_size2, float y_size2, float z_size2, int x_dim1, int y_dim1, int z_dim1,
                       float x_size1, float y_size1, float z_size1, boolean cubicInterpolation) {

        float xoom1, xoom2;
        float yoom1, yoom2;
        float zoom1, zoom2;
        double pixel_size1, pixel_size2;
        int s, t;

        pixel_size1 = x_size1;

        if (y_size1 < pixel_size1) {
            pixel_size1 = y_size1;
        }

        if (z_size1 < pixel_size1) {
            pixel_size1 = z_size1;
        }

        xoom1 = (float) (x_size1 / pixel_size1);
        yoom1 = (float) (y_size1 / pixel_size1);
        zoom1 = (float) (z_size1 / pixel_size1);

        pixel_size2 = x_size2;

        if (y_size2 < pixel_size2) {
            pixel_size2 = y_size2;
        }

        if (z_size2 < pixel_size2) {
            pixel_size2 = z_size2;
        }

        xoom2 = (float) (x_size2 / pixel_size2);
        yoom2 = (float) (y_size2 / pixel_size2);
        zoom2 = (float) (z_size2 / pixel_size2);

        e[0][0] = 1;
        e[0][1] = 0;
        e[0][2] = 0;

        e[1][0] = 0;
        e[1][1] = 1;
        e[1][2] = 0;

        e[2][0] = 0;
        e[2][1] = 0;
        e[2][2] = 1;

        e[3][0] = 0;
        e[3][1] = 0;
        e[3][2] = 0;

        /*And the constant row*/
        e[0][3] = 0;
        e[1][3] = 0;
        e[2][3] = 0;
        e[3][3] = 1;

        gscale_rotatef(e[0], de[0], ee[0], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                       x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);
        gscale_rotatef(e[1], de[1], ee[1], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                       x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);
        gscale_rotatef(e[2], de[2], ee[2], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                       x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);
        gscale_rotatef(e[3], de[3], ee[3], tps, xoom1, xoom2, yoom1, yoom2, zoom1, zoom2, pixel_size1, pixel_size2,
                       x_dim1, x_dim2, y_dim1, y_dim2, z_dim1, z_dim2, cubicInterpolation);

        e[2][0] = e[2][0] - e[3][0];
        e[1][0] = e[1][0] - e[3][0];
        e[0][0] = e[0][0] - e[3][0];

        e[2][1] = e[2][1] - e[3][1];
        e[1][1] = e[1][1] - e[3][1];
        e[0][1] = e[0][1] - e[3][1];

        e[2][2] = e[2][2] - e[3][2];
        e[1][2] = e[1][2] - e[3][2];
        e[0][2] = e[0][2] - e[3][2];

        for (t = 0; t < 7; t++) {
            de[2][0][t] = de[2][0][t] - de[3][0][t];
            de[1][0][t] = de[1][0][t] - de[3][0][t];
            de[0][0][t] = de[0][0][t] - de[3][0][t];

            de[2][1][t] = de[2][1][t] - de[3][1][t];
            de[1][1][t] = de[1][1][t] - de[3][1][t];
            de[0][1][t] = de[0][1][t] - de[3][1][t];

            de[2][2][t] = de[2][2][t] - de[3][2][t];
            de[1][2][t] = de[1][2][t] - de[3][2][t];
            de[0][2][t] = de[0][2][t] - de[3][2][t];

            de[0][3][t] = 0;
            de[1][3][t] = 0;
            de[2][3][t] = 0;
            de[3][3][t] = 0;

            for (s = 0; s <= t; s++) {

                ee[2][0][t][s] = ee[2][0][t][s] - ee[3][0][t][s];
                ee[1][0][t][s] = ee[1][0][t][s] - ee[3][0][t][s];
                ee[0][0][t][s] = ee[0][0][t][s] - ee[3][0][t][s];

                ee[2][1][t][s] = ee[2][1][t][s] - ee[3][1][t][s];
                ee[1][1][t][s] = ee[1][1][t][s] - ee[3][1][t][s];
                ee[0][1][t][s] = ee[0][1][t][s] - ee[3][1][t][s];

                ee[2][2][t][s] = ee[2][2][t][s] - ee[3][2][t][s];
                ee[1][2][t][s] = ee[1][2][t][s] - ee[3][2][t][s];
                ee[0][2][t][s] = ee[0][2][t][s] - ee[3][2][t][s];

                ee[0][3][t][s] = 0;
                ee[1][3][t][s] = 0;
                ee[2][3][t][s] = 0;
                ee[3][3][t][s] = 0;
            }

        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters          number of formal parameters used in specified model
     * @param   ef                  Input forward transform
     * @param   er                  Output reverse transform
     * @param   def                 Input 1st derivatives of forward transform
     * @param   der                 Output 1st derivatives of reverse transform
     * @param   eef                 Input 2nd derivatives of forward transform
     * @param   eer                 Output second derivatives of reverse transform
     * @param   cubicInterpolation  if true create cubic voxels
     * @param   x_size1             DOCUMENT ME!
     * @param   y_size1             DOCUMENT ME!
     * @param   z_size1             DOCUMENT ME!
     * @param   x_size2             DOCUMENT ME!
     * @param   y_size2             DOCUMENT ME!
     * @param   z_size2             DOCUMENT ME!
     *
     * @return  true if successful, false if unsuccessful
     *
     *          <p>Inverts matrix and calculates derivatives (first and second) of inversion</p>
     *
     *          <p>It expects ef and er to be 4 x 4 matrices</p>
     */


    private boolean uvrm(int parameters, double[][] ef, double[][] er, double[][][] def, double[][][] der,
                         double[][][][] eef, double[][][][] eer, boolean cubicInterpolation, float x_size1,
                         float y_size1, float z_size1, float x_size2, float y_size2, float z_size2) {

        int mm, nn, oo;

        int i, j; /*Transform element index*/
        int s, t;

        int dv, dw; /*Derivative index*/

        double[][] dfv = new double[4][4];
        double[][] dfw = new double[4][4];
        double[][] drv = new double[4][4];
        double[][] drw = new double[4][4];
        double[][] drvw = new double[4][4];
        double[][] drwv = new double[4][4];
        double[][] efvw = new double[4][4];
        double[][] err = new double[4][4];

        double pixel_size1, pixel_size2;

        /*Copy parameters to be inverted into matrix er*/
        for (i = 0; i < 4; i++) {

            for (j = 0; j < 4; j++) {
                er[i][j] = ef[i][j];
            }
        }

        /* Invert spatially */
        if (!gael(er)) {
            return false;
        }

        /*Routine matmul doesn't like pointers, so...*/
        for (i = 0; i < 4; i++) {

            for (j = 0; j < 4; j++) {
                err[i][j] = er[i][j];
            }
        }

        /*Compute first derivatives of inverse with respect to each parameter*/
        for (dv = 0; dv < parameters; dv++) {

            for (i = 0; i < 4; i++) {

                for (j = 0; j < 4; j++) {
                    dfv[i][j] = def[i][j][dv];
                }
            }

            /*Note that routine matmul assumes that array is fully contiguous and orderly*/
            matmul(dfv, err, drv); /* dfv*err=drv */
            matmul(err, drv, drv); /* err*drv=drv=err*dfv*err */

            for (i = 0; i < 4; i++) {

                for (j = 0; j < 4; j++) {
                    der[i][j][dv] = -drv[i][j]; /* der=-err*dfv*err */
                }
            }
        }

        /*Compute second partial derivatives of inverse with respect to each pair of parameters*/
        for (dv = 0; dv < parameters; dv++) {

            for (dw = 0; dw <= dv; dw++) {

                for (i = 0; i < 4; i++) {

                    for (j = 0; j < 4; j++) {
                        dfv[i][j] = def[i][j][dv];
                        dfw[i][j] = def[i][j][dw];
                        efvw[i][j] = eef[i][j][dv][dw];
                    }
                }

                matmul(err, dfv, drv); /* err*dfv=drv */
                matmul(err, dfw, drw); /* err*dfw=drw */
                matmul(drw, drv, drwv); /* drw*drv=drwv=err*dfw*err*dfv */
                matmul(drv, drw, drvw); /* drv*drw=drvw=err*dfv*err*dfw */
                matmul(drwv, err, drwv); /* drwv*err=drwv=err*dfv*err*dfw*err */
                matmul(drvw, err, drvw); /* drvw*err=drvw=err*dfw*err*dfv*err */

                matmul(efvw, err, efvw); /* efvw*err=efvw */
                matmul(err, efvw, efvw); /* err*efvw=efvw=err*efvw*err */

                for (i = 0; i < 4; i++) {

                    for (j = 0; j < 4; j++) {
                        eer[i][j][dv][dw] = drvw[i][j] + drwv[i][j] - efvw[i][j];

                        /* eer= err*dfv*err*dfw*err+err*dfw*err*dfv*err-err*efvw*err */
                    }
                }
            }
        }

        /*Adjust all values to account for zooming*/

        if (cubicInterpolation) {

            pixel_size1 = x_size1;

            if (y_size1 < pixel_size1) {
                pixel_size1 = y_size1;
            }

            if (z_size1 < pixel_size1) {
                pixel_size1 = z_size1;
            }

            pixel_size2 = x_size2;

            if (y_size2 < pixel_size2) {
                pixel_size2 = y_size2;
            }

            if (z_size2 < pixel_size2) {
                pixel_size2 = z_size2;
            }

            for (mm = 0; mm < 4; mm++) {
                er[0][mm] /= (x_size2 / pixel_size2);
                er[1][mm] /= (y_size2 / pixel_size2);
                er[2][mm] /= (z_size2 / pixel_size2);

                for (nn = 0; nn < parameters; nn++) {
                    der[0][mm][nn] /= (x_size2 / pixel_size2);
                    der[1][mm][nn] /= (y_size2 / pixel_size2);
                    der[2][mm][nn] /= (z_size2 / pixel_size2);

                    for (oo = 0; oo <= nn; oo++) {
                        eer[0][mm][nn][oo] /= (x_size2 / pixel_size2);
                        eer[1][mm][nn][oo] /= (y_size2 / pixel_size2);
                        eer[2][mm][nn][oo] /= (z_size2 / pixel_size2);
                    }
                }
            }

            for (mm = 0; mm < 4; mm++) {
                er[mm][0] /= (x_size1 / pixel_size1);
                er[mm][1] /= (y_size1 / pixel_size1);
                er[mm][2] /= (z_size1 / pixel_size1);

                for (nn = 0; nn < parameters; nn++) {
                    der[mm][0][nn] /= (x_size1 / pixel_size1);
                    der[mm][1][nn] /= (y_size1 / pixel_size1);
                    der[mm][2][nn] /= (z_size1 / pixel_size1);

                    for (oo = 0; oo <= nn; oo++) {
                        eer[mm][0][nn][oo] /= (x_size1 / pixel_size1);
                        eer[mm][1][nn][oo] /= (y_size1 / pixel_size1);
                        eer[mm][2][nn][oo] /= (z_size1 / pixel_size1);
                    }
                }
            }

        } // if (cubicInterpolation)

        /* Divide everything by er[3][3] per homogenous coordinate protocol  */ {

            for (j = 0; j < 4; j++) {

                for (i = 0; i < 4; i++) {

                    if (!((i == 3) && (j == 3))) {
                        er[j][i] /= er[3][3];

                        for (t = 0; t < parameters; t++) {
                            der[j][i][t] -= er[j][i] * der[3][3][t];
                            der[j][i][t] /= er[3][3];

                            for (s = 0; s <= t; s++) {
                                eer[j][i][t][s] -= er[j][i] * eer[3][3][t][s];
                                eer[j][i][t][s] -= der[j][i][s] * der[3][3][t];
                                eer[j][i][t][s] -= der[j][i][t] * der[3][3][s];
                                eer[j][i][t][s] /= er[3][3];
                            }
                        }
                    } else {

                        /* Included only as a safeguard */
                        er[3][3] = 1;

                        for (t = 0; t < parameters; t++) {
                            der[3][3][t] = 0;

                            for (s = 0; s <= t; s++) {
                                eer[3][3][t][s] = 0;
                            }
                        }
                    }
                }
            }
        }

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   alpha  DOCUMENT ME!
     * @param   kern   DOCUMENT ME!
     *
     * @return  s
     *
     *          <p>Computes windowed sinc</p>
     */

    private double wsinc(double alpha, int kern) {

        double s;

        if (alpha == 0) {
            s = 1;
        } else {
            s = 0.5 * (1.0 + Math.cos(Math.PI * alpha / kern)) * Math.sin(Math.PI * alpha) / (Math.PI * alpha);
        }

        return s;
    }

}
