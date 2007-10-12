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
 * This software is a port of portions of version 5.21 of the Automated Image Registration (AIR) package found at the
 * AIR WWW site (currently <a href="http://bishopw.loni.ucla.edu/AIR5/">http://bishopw.loni.ucla.edu/AIR5/</a>.
 *
 * <p>References:<br>
 * 1.) Woods RP, Grafton ST, Holmes CJ, Cherry SR, Mazziotta JC. Automated image registration: I. General methods and
 * intrasubject, intramodality validation. Journal of Computer Assisted Tomography 1998; 22:141-154.<br>
 * 2.) Woods RP, Grafton ST, Watson JDG, Sicotte NL, Mazziotta JC. Automated image registration: II. Intersubject
 * validation of linear and nonlinear methods. Journal of Computer Assisted Tomography 1998; 22:155-165.<br>
 * </p>
 *
 * <p>The nonlinear registration algorithm is designed so that second order nonlinear transformations are initialized
 * with results of an affine registration, and successively higher models are sequentially initialized with the best
 * results obtained at lower orders.</p>
 *
 * <p>From reference 2: For nonlinear models the usual AIR procedure for removing biases associated with defining one
 * image as the reference image and the other as the reslice image cannot be used because the nonlinear transformations
 * are not analytically invertible. To compensate, the nonlinear algorithm includes in its cost function not only those
 * voxels that are supratrheshold in the reference file but also those that are subthreshold in the reference file but
 * that are suprathreshold in the corresponding voxels of the interpolated reslice image. Without this compensation,
 * optimization of the cost function tends to enlarge the reslice image until it completely covers all the edges of the
 * reference image.</p>
 *
 * @author  Porting done by William Gandler
 */
public class AlgorithmRegNonlinear extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** 2D models - first order linear 6 parameters. */
    public static final int LINEAR2D6 = 21;

    /** 2D models - second order nonlinear 12 parameters. */
    public static final int NL2D12 = 22;

    /** 2D models - third order nonlinear 20 parameters. */
    public static final int NL2D20 = 23;

    /** 2D models - fourth order nonlinear 30 parameters. */
    public static final int NL2D30 = 24;

    /** 2D models - fifth order nonlinear 42 parameters. */
    public static final int NL2D42 = 25;

    /** 2D models - sixth order nonlinear 56 parameters. */
    public static final int NL2D56 = 26;

    /** 2D models - seventh order nonlinear 72 parameters. */
    public static final int NL2D72 = 27;

    /** 2D models - eighth order nonlinear 90 parameters. */
    public static final int NL2D90 = 28;

    /** 2D models - ninth order nonlinear 110 parameters. */
    public static final int NL2D110 = 29;

    /** 2D models - tenth order nonlinear 132 parameters. */
    public static final int NL2D132 = 30;

    /** 2D models - eleventh order nonlinear 156 parameters. */
    public static final int NL2D156 = 31;

    /** 2D models - twelfth order nonlinear 182 parameters. */
    public static final int NL2D182 = 32;

    /** 3D models - first order linear 12 parameters. */
    public static final int LINEAR3D12 = 1;

    /** 3D models - second order nonlinear 30 parameters. */
    public static final int NL3D30 = 2;

    /** 3D models - third order nonlinear 60 parameters. */
    public static final int NL3D60 = 3;

    /** 3D models - fourth order nonlinear 105 parameters. */
    public static final int NL3D105 = 4;

    /** 3D models - fifth order nonlinear 168 parameters. */
    public static final int NL3D168 = 5;

    /** 3D models - sixth order nonlinear 252 parameters. */
    public static final int NL3D252 = 6;

    /** 3D models - seventh order nonlinear 360 parameters. */
    public static final int NL3D360 = 7;

    /** 3D models - eighth order nonlinear 495 parameters. */
    public static final int NL3D495 = 8;

    /** 3D models - ninth order nonlinear 660 parameters. */
    public static final int NL3D660 = 9;

    /** 3D models - tenth order nonlinear 858 parameters. */
    public static final int NL3D858 = 10;

    /** 3D models - eleventh order nonlinear 1092 parameters. */
    public static final int NL3D1092 = 11;

    /** 3D models - twelfth order nonlinear 1365 parameters. */
    public static final int NL3D1365 = 12;

    /** Default initial sampling interval. */
    private static final int SF = 81;

    /** Default final sampling interval. Cannot be greater than SF */
    private static final int SF2 = 1;

    /** Default sampling interval decrement ratio. Must be greater than 1 */
    private static final int SFF = 3;

    /** Default iterations without improvement in sd. */
    private static final int NOPROGTRY = 2;

    /** DOCUMENT ME! */
    private static final boolean POSITIVE_DEFINITE_REQUIRED = true;

    /** DOCUMENT ME! */
    private static final int UVDERIV2DWARP = 1;

    /** DOCUMENT ME! */
    private static final int QVDERIV2DWARP = 2;

    /** DOCUMENT ME! */
    private static final int UVDERIV3DWARP = 3;

    /** DOCUMENT ME! */
    private static final int QVDERIV3DWARP = 4;

    /** DOCUMENT ME! */
    private static final int UVDERIVCOR2DWARP = 5;

    /** DOCUMENT ME! */
    private static final int QVDERIVCOR2DWARP = 6;

    /** DOCUMENT ME! */
    private static final int UVDERIVCOR3DWARP = 7;

    /** DOCUMENT ME! */
    private static final int QVDERIVCOR3DWARP = 8;

    /** DOCUMENT ME! */
    public static final int SCALED_LEAST_SQUARES = 1;

    /** DOCUMENT ME! */
    public static final int CORRELATION_RATIO = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int bufferFactor = 1; // 1 for black and white, 4 for color

    /** DOCUMENT ME! */
    private boolean clipOutput = true;

    /** DOCUMENT ME! */
    private int costFxn; /* 1 = scaled least squares, 2 = correlation ratio */

    /** DOCUMENT ME! */
    private int[] d2 = { 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 91 };

    /** DOCUMENT ME! */
    private int[] d3 = { 1, 4, 10, 20, 35, 56, 84, 120, 165, 220, 286, 364, 455, 455 };

    /** DOCUMENT ME! */
    private float[] dataOut;

    /** DOCUMENT ME! */
    private double[] dcf = null; // first derivatives of sd with respect to parameters

    /** DOCUMENT ME! */
    private double[] delta = null;

    /** DOCUMENT ME! */
    private boolean doAdjacent;

    /** DOCUMENT ME! */
    private boolean doParam;

    /** DOCUMENT ME! */
    private double[] dx = null;

    /** DOCUMENT ME! */
    private double[][] ecf = null; // second derivatives of sd with respect to parameters

    /** DOCUMENT ME! */
    private double[][] ecf2 = null; // copy of esd

    /** DOCUMENT ME! */
    private boolean entireSource;

    /** DOCUMENT ME! */
    private boolean entireTarget;

    /** DOCUMENT ME! */
    private int error;

    /** DOCUMENT ME! */
    private double[][] es = null;

    /** DOCUMENT ME! */
    private double[][] esInitial = null;

    /** DOCUMENT ME! */
    private int fmodel; // final model

    /** DOCUMENT ME! */
    private double[] gersch = null;

    /** DOCUMENT ME! */
    private boolean interaction = true; // spatial parameter derivatives interact

    /** DOCUMENT ME! */
    private int interpolation;

    /** DOCUMENT ME! */
    private int iterations; /* number of iterations at a given sampling density */

    /** DOCUMENT ME! */
    private int[] kpvt = null;

    /** DOCUMENT ME! */
    private ModelImage maskImage;

    /** DOCUMENT ME! */
    private float precision; /* cost function change must be less than precision for convergence */

    /** DOCUMENT ME! */
    private RandomAccessFile raFile;

    /** DOCUMENT ME! */
    private int referenceSlice;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private float[] resultResolutions = new float[3];

    /** DOCUMENT ME! */
    private int sampleFactor2; /* final sampling interval */

    /** DOCUMENT ME! */
    private boolean series2D = false;

    /** DOCUMENT ME! */
    private float[] sigmas = new float[3];

    /** DOCUMENT ME! */
    private ModelImage sImage;

    /** DOCUMENT ME! */
    private float[] sourceArray;

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
    private float sourceThreshold;

    /** DOCUMENT ME! */
    private float[] targetArray;

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
    private float targetThreshold;

    /** DOCUMENT ME! */
    private int[] targetUnits = new int[3];

    /** DOCUMENT ME! */
    private int targetXDim;

    /** DOCUMENT ME! */
    private int targetYDim;

    /** DOCUMENT ME! */
    private int targetZDim;

    /** DOCUMENT ME! */
    private BitSet tMask = null;

    /** DOCUMENT ME! */
    private double[] tpsBest = null; // Best values of par at current sampling factor

    /** private double[] tps;. */
    private double[] tpsOld;

    /** DOCUMENT ME! */
    private boolean transformVOI = false; // transform source VOI

    /** DOCUMENT ME! */
    private boolean writeParameters;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmRegNonlinear - constructor.
     *
     * @param  sourceImage        DOCUMENT ME!
     * @param  targetXDim         DOCUMENT ME!
     * @param  targetYDim         DOCUMENT ME!
     * @param  targetZDim         DOCUMENT ME!
     * @param  resultResolutions  DOCUMENT ME!
     * @param  targetUnits        DOCUMENT ME!
     * @param  fmodel             DOCUMENT ME!
     * @param  es                 DOCUMENT ME!
     * @param  interpolation      DOCUMENT ME!
     * @param  clipOutput         DOCUMENT ME!
     * @param  transformVOI       DOCUMENT ME!
     */
    public AlgorithmRegNonlinear(ModelImage sourceImage, int targetXDim, int targetYDim, int targetZDim,
                                 float[] resultResolutions, int[] targetUnits, int fmodel, double[][] es,
                                 int interpolation, boolean clipOutput, boolean transformVOI) {
        this.sourceImage = sourceImage;
        this.targetXDim = targetXDim;
        this.targetYDim = targetYDim;
        this.targetZDim = targetZDim;
        this.resultResolutions = resultResolutions;
        this.targetUnits = targetUnits;
        this.fmodel = fmodel;
        this.es = es;
        this.interpolation = interpolation;
        this.clipOutput = clipOutput;
        this.transformVOI = transformVOI;
        this.doParam = true;
    }

    /**
     * AlgorithmRegNonlinear - constructor.
     *
     * @param  resultImage      DOCUMENT ME!
     * @param  targetImage      DOCUMENT ME!
     * @param  entireTarget     if false only selected VOI in target image is used
     * @param  sourceImage      image which is resliced to match the target image
     * @param  entireSource     if false only selected VOI in source image is used
     * @param  transformation   models using different numbers of parameters
     * @param  interpolation    nearest neighbor, bilinear or trilinear, or windowed sinc
     * @param  clipOutput       clips output to input range if windowed sinc used
     * @param  interaction      if true spatial parameter derivatives interact
     * @param  transformVOI     transform source VOI
     * @param  sourceThreshold  DOCUMENT ME!
     * @param  targetThreshold  DOCUMENT ME!
     * @param  sourceGaussX     DOCUMENT ME!
     * @param  sourceGaussY     DOCUMENT ME!
     * @param  sourceGaussZ     DOCUMENT ME!
     * @param  targetGaussX     DOCUMENT ME!
     * @param  targetGaussY     DOCUMENT ME!
     * @param  targetGaussZ     DOCUMENT ME!
     * @param  sampleFactor2    final sampling interval
     * @param  costFxn          1 = scaled least squares, 2 = correlation ratio
     * @param  precision        cost function change must be less than precision for convergence
     * @param  iterations       maximum number of iterations at a given sampling density
     * @param  writeParameters  DOCUMENT ME!
     * @param  esInitial        values of affine transformation matrix
     */
    public AlgorithmRegNonlinear(ModelImage resultImage, ModelImage targetImage, boolean entireTarget,
                                 ModelImage sourceImage, boolean entireSource, int transformation, int interpolation,
                                 boolean clipOutput, boolean interaction, boolean transformVOI, float sourceThreshold,
                                 float targetThreshold, float sourceGaussX, float sourceGaussY, float sourceGaussZ,
                                 float targetGaussX, float targetGaussY, float targetGaussZ, int sampleFactor2,
                                 int costFxn, float precision, int iterations, boolean writeParameters,
                                 double[][] esInitial) {
        this.resultImage = resultImage;
        this.targetImage = targetImage;
        this.entireTarget = entireTarget;
        this.sourceImage = sourceImage;
        this.entireSource = entireSource;
        fmodel = transformation;
        this.interpolation = interpolation;
        this.clipOutput = clipOutput;
        this.interaction = interaction;
        this.transformVOI = transformVOI;
        this.sourceThreshold = sourceThreshold;
        this.targetThreshold = targetThreshold;
        this.sourceGaussX = sourceGaussX;
        this.sourceGaussY = sourceGaussY;
        this.sourceGaussZ = sourceGaussZ;
        this.targetGaussX = targetGaussX;
        this.targetGaussY = targetGaussY;
        this.targetGaussZ = targetGaussZ;
        this.sampleFactor2 = sampleFactor2;
        this.costFxn = costFxn;
        this.precision = precision;
        this.iterations = iterations;
        this.writeParameters = writeParameters;
        this.doParam = false;
        this.esInitial = esInitial;

        if (entireSource == false) {
            mask = sourceImage.generateVOIMask();
        }

        if (entireTarget == false) {
            tMask = targetImage.generateVOIMask();
        }
    }

    /**
     * AlgorithmRegNonlinear - constructor This constructor used for runs aligning slices within a given 3D image to a
     * chosen reference slice number If doAdjacent is true: First referenceSlice + 1 is registered to referenceSlice,
     * then referenceSlice + 2 is registered to referenceSlice + 1, and so on until the last slice is registered to the
     * slice just before the last slice. The referenceSlice - 1 is registered to referenceSlice, referenceSlice - 2 is
     * registered to referenceSlice - 1, and so on until slice 0 is registered to slice 1. If doAdjacent is false:
     * Register all slices to the referenceSlice
     *
     * @param  resultImage      DOCUMENT ME!
     * @param  referenceSlice   slice to which the other slices are aligned
     * @param  doAdjacent       if true register to adjacent slice if false register to reference slice
     * @param  entireTarget     if false only selected VOI in target image is used
     * @param  sourceImage      image which is resliced to match the target image
     * @param  entireSource     if false only selected VOI in source image is used
     * @param  transformation   models using different numbers of parameters
     * @param  interpolation    nearest neighbor, bilinear or trilinear, or windowed sinc
     * @param  clipOutput       clips output to input range if windowed sinc used
     * @param  interaction      if true spatial parameter derivatives interact
     * @param  transformVOI     transform source VOI
     * @param  sourceThreshold  DOCUMENT ME!
     * @param  targetThreshold  DOCUMENT ME!
     * @param  sourceGaussX     DOCUMENT ME!
     * @param  sourceGaussY     DOCUMENT ME!
     * @param  sourceGaussZ     DOCUMENT ME!
     * @param  targetGaussX     DOCUMENT ME!
     * @param  targetGaussY     DOCUMENT ME!
     * @param  targetGaussZ     DOCUMENT ME!
     * @param  sampleFactor2    final sampling interval
     * @param  costFxn          1 = scaled least squares, 2 = correlation ratio
     * @param  precision        cost function change must be less than precision for convergence
     * @param  iterations       maximum number of iterations at a given sampling density
     * @param  writeParameters  DOCUMENT ME!
     */
    public AlgorithmRegNonlinear(ModelImage resultImage, int referenceSlice, boolean doAdjacent, boolean entireTarget,
                                 ModelImage sourceImage, boolean entireSource, int transformation, int interpolation,
                                 boolean clipOutput, boolean interaction, boolean transformVOI, float sourceThreshold,
                                 float targetThreshold, float sourceGaussX, float sourceGaussY, float sourceGaussZ,
                                 float targetGaussX, float targetGaussY, float targetGaussZ, int sampleFactor2,
                                 int costFxn, float precision, int iterations, boolean writeParameters) {
        this.resultImage = resultImage;
        this.referenceSlice = referenceSlice;
        this.doAdjacent = doAdjacent;
        this.entireTarget = entireTarget;
        this.sourceImage = sourceImage;
        this.entireSource = entireSource;
        fmodel = transformation;
        this.interpolation = interpolation;
        this.clipOutput = clipOutput;
        this.interaction = interaction;
        this.transformVOI = transformVOI;
        this.sourceThreshold = sourceThreshold;
        this.targetThreshold = targetThreshold;
        this.sourceGaussX = sourceGaussX;
        this.sourceGaussY = sourceGaussY;
        this.sourceGaussZ = sourceGaussZ;
        this.targetGaussX = targetGaussX;
        this.targetGaussY = targetGaussY;
        this.targetGaussZ = targetGaussZ;
        this.sampleFactor2 = sampleFactor2;
        this.costFxn = costFxn;
        this.precision = precision;
        this.iterations = iterations;
        this.writeParameters = writeParameters;
        this.doParam = false;
        this.series2D = true;

        if (entireSource == false) {
            mask = sourceImage.generateVOIMask();
        }

        if (entireTarget == false) {
            tMask = targetImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void disposeLocal() {
        int i;
        sourceArray = null;
        targetArray = null;
        sigmas = null;

        if (ecf != null) {

            for (i = 0; i < ecf.length; i++) {
                ecf[i] = null;
            }

            ecf = null;
        }

        if (ecf2 != null) {

            for (i = 0; i < ecf2.length; i++) {
                ecf2[i] = null;
            }

            ecf2 = null;
        }

        dcf = null;
        delta = null;
        tpsOld = null;

        // tps = null;
        tpsBest = null;
        kpvt = null;
        dx = null;
        gersch = null;
        d2 = null;
        d3 = null;
        resultResolutions = null;
        targetUnits = null;
        dataOut = null;

        if (es != null) {

            for (i = 0; i < es.length; i++) {
                es[i] = null;
            }

            es = null;
        }

        if (series2D) {

            if (sImage != null) {
                sImage.disposeLocal();
            }

            sImage = null;

            if (targetImage != null) {
                targetImage.disposeLocal();
            }

            targetImage = null;
        } // if (series2D)
    }

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        disposeLocal();
        System.gc();
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getTransformedImage() {
        return resultImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  es
     */
    public double[][] getTransformNL() {
        return es;
    }

    /**
     * DOCUMENT ME!
     */
    public void paramRun() {
        boolean inPlane;
        int sourceXDim, sourceYDim, sourceZDim;
        int sourceSliceSize;
        int sourceVolSize;
        int[] resultExtents;
        int resultZDim;
        int i;

        fireProgressStateChanged(sourceImage.getImageName(), "Reslicing source image");


        if (sourceImage.getNDims() == 2) {
            inPlane = true;
            resultExtents = new int[2];
            resultExtents[0] = targetXDim;
            resultExtents[1] = targetYDim;
        } else {
            inPlane = false;
            resultExtents = new int[3];
            resultExtents[0] = targetXDim;
            resultExtents[1] = targetYDim;
            resultExtents[2] = targetZDim;
        }

        if (sourceImage.isColorImage()) {
            resultImage = new ModelImage(sourceImage.getType(), resultExtents,
                                         sourceImage.getImageName() + "_registered");
            bufferFactor = 4;
        } else {
            resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents,
                                         sourceImage.getImageName() + "_registered");
            bufferFactor = 1;
        }

        sourceXDim = sourceImage.getExtents()[0];
        sourceYDim = sourceImage.getExtents()[1];
        sourceSliceSize = sourceXDim * sourceYDim;

        if (inPlane) {
            sourceZDim = 1;
        } else {
            sourceZDim = sourceImage.getExtents()[2];
        }

        sourceVolSize = bufferFactor * sourceSliceSize * sourceZDim;
        sourceArray = new float[sourceVolSize];

        try {
            sourceImage.exportData(0, sourceVolSize, sourceArray); // locks and releases and lock
        } catch (IOException error) {
            finalize();
            displayError("AlgorithmRegNonlinear: IOException on sourceImage.exportData");

            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            finalize();
            displayError("AlgorithmRegNonlinear: Out of memory on sourceImage.exportData");

            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Reslicing source image");

        if (inPlane) { // 2D

            if (sourceImage.isColorImage()) {
                dataOut = rWarp2DC(sourceArray, sourceXDim, sourceYDim, (float) sourceImage.getMin(),
                                   (float) sourceImage.getMax(), targetXDim, targetYDim, targetZDim, es, fmodel - 20,
                                   interpolation);
            } else {
                dataOut = rWarp2D(sourceArray, sourceXDim, sourceYDim, (float) sourceImage.getMin(),
                                  (float) sourceImage.getMax(), targetXDim, targetYDim, targetZDim, es, fmodel - 20,
                                  interpolation);
            }

        } else { // 3D
            dataOut = rWarp3D(sourceArray, sourceXDim, sourceYDim, sourceZDim, (float) sourceImage.getMin(),
                              (float) sourceImage.getMax(), targetXDim, targetYDim, targetZDim, es, fmodel,
                              interpolation);
        }

        try {
            resultImage.importData(0, dataOut, true);
        } catch (IOException error) {
            finalize();
            MipavUtil.displayError("AlgorithmRegNonlinear: IOException on resultImage" + ".importData(0,dataOut,true)");

            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            finalize();
            MipavUtil.displayError("AlgorithmRegNonlinear: Out of memory on resultImage" +
                                   ".importData(0,dataOut,true)");

            setCompleted(false);

            return;
        }

        dataOut = null;

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

        if (transformVOI) {
            maskImage = sourceImage.generateShortImage(1);

            try {
                maskImage.exportData(0, sourceXDim * sourceYDim, sourceArray); // locks and releases lock
            } catch (IOException error) {
                finalize();
                MipavUtil.displayError("Algorithm RegNonlinear: maskImage locked");

                setCompleted(false);

                return;
            }

            if (inPlane) { // 2D
                dataOut = rWarp2D(sourceArray, sourceXDim, sourceYDim, (float) sourceImage.getMin(),
                                  (float) sourceImage.getMax(), targetXDim, targetYDim, targetZDim, es, fmodel - 20,
                                  AlgorithmTransform.NEAREST_NEIGHBOR);

            } else { // 3D
                dataOut = rWarp3D(sourceArray, sourceXDim, sourceYDim, sourceZDim, (float) sourceImage.getMin(),
                                  (float) sourceImage.getMax(), targetXDim, targetYDim, targetZDim, es, fmodel,
                                  AlgorithmTransform.NEAREST_NEIGHBOR);
            }

            maskImage.disposeLocal();
            maskImage = new ModelImage(ModelImage.SHORT, resultImage.getExtents(), null);

            try {
                maskImage.importData(0, dataOut, true);
            } catch (IOException error) {
                finalize();
                MipavUtil.displayError("AlgorithmRegNonlinear: IOException on maskImage" +
                                       ".importData(0,dataOut,true)");

                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                finalize();
                MipavUtil.displayError("AlgorithmRegNonlinear: Out of memory on maskImage" +
                                       ".importData(0,dataOut,true)");

                setCompleted(false);

                return;
            }

            dataOut = null;

            AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(maskImage);
            VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
            VOIExtAlgo.run();
            resultImage.setVOIs(maskImage.getVOIs());
            maskImage.disposeLocal();
            maskImage = null;
        } // if (transformVOI)

        sourceArray = null;

        resultResolutions = null;
        targetUnits = null;

        return;
    }

    /**
     * run - starts the program.
     */
    public void runAlgorithm() {
        int model;
        int sourceXDim;
        int sourceYDim;
        int sourceZDim;
        int sourceSliceSize;
        int sourceVolSize;
        int targetSliceSize;
        int targetVolSize;
        float sourceVoxelX;
        float sourceVoxelY;
        float sourceVoxelZ;
        float targetVoxelX;
        float targetVoxelY;
        float targetVoxelZ;
        int seriesNumber;

        int i, j, k, jj;
        // BitSet  mask;

        int sfFactor; /* default sampling interval decrement ratio */
        int sampleFactor; /* initial sampling interval */
        int noProgTries; /* number of iterations without sd improvement before forced
                          *termination */

        AlgorithmGaussianBlur gaussianBlurAlgo;

        boolean posdefreq;
        int sourceUnitsX, sourceUnitsY, sourceUnitsZ;
        int targetUnitsX, targetUnitsY, targetUnitsZ;
        boolean doConvert;
        int resultZDim;
        boolean inPlane;
        int extraZeros;
        int coords;
        int coeffp;
        int fcoeffp;
        int uvderivsN;
        double scale;
        int parameters;
        double cf = Double.MAX_VALUE; /* standard deviation/mean */
        double cfBest = Double.MAX_VALUE; /* best sd at current sampling factor */
        int iters; /* number current iteration */
        int tries; /* number of iterations since sd last improved */
        double deltacf;
        int s, t;
        String fileDir;
        String fileName;
        File file;
        boolean endianess;
        int series;
        int sNumber = 0;
        int tNumber = 0;
        boolean seriesUp = true;
        int[] targetExtents = new int[2];
        ViewVOIVector VOIs = null; // sourceImage
        ViewVOIVector VOIsr = null; // resultImage
        ViewVOIVector VOIsm = null; // maskImage
        int nVOI = 0;
        int nVOIr = 0;
        int nVOIm = 0;
        VOI voi, outVOI;
        float sMinValue, sMaxValue;
        double maxActualValue; // maximum in targetArray allowed by targetMask
        double minActualValue;
        int[] q1 = new int[1];
        double[] q2 = new double[1];
        double[] q3 = new double[1];
        double[][] q4 = new double[1][1];
        double[][] q5 = new double[1][1];
        double[][][] q6 = new double[1][1][1];
        double[][][] q7 = new double[1][1][1];
        Color voiColor;

        

        if (doParam) {
            paramRun();

            setCompleted(true);

            return;
        }

        fireProgressStateChanged(sourceImage.getImageName(), "Registering source image");


        if ((sourceImage.getNDims() == 2) || (series2D)) {
            inPlane = true;
        } else {
            inPlane = false;
        }

        if (series2D) {
            seriesNumber = sourceImage.getExtents()[2] - 1;
        } else {
            seriesNumber = 1;
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

        sourceVoxelX = sourceImage.getFileInfo()[0].getResolutions()[0];
        sourceVoxelY = sourceImage.getFileInfo()[0].getResolutions()[1];

        if (!inPlane) {
            sourceVoxelZ = sourceImage.getFileInfo()[0].getResolutions()[2];
        } else {
            sourceVoxelZ = sourceVoxelX;
        }

        if (series2D) {
            targetXDim = sourceXDim;
            targetYDim = sourceYDim;
            targetZDim = 1;
        } else { // not series2D
            targetXDim = targetImage.getExtents()[0];
            targetYDim = targetImage.getExtents()[1];

            if (inPlane) {
                targetZDim = 1;
            } else {
                targetZDim = targetImage.getExtents()[2];
            }
        } // not series2D

        targetSliceSize = targetXDim * targetYDim;
        targetVolSize = targetSliceSize * targetZDim;
        targetArray = new float[targetVolSize];

        if (series2D) {
            targetVoxelX = sourceVoxelX;
            targetVoxelY = sourceVoxelY;
            targetVoxelZ = sourceVoxelZ;
        } else { // not series2D
            targetVoxelX = targetImage.getFileInfo()[0].getResolutions()[0];
            targetVoxelY = targetImage.getFileInfo()[0].getResolutions()[1];

            if (!inPlane) {
                targetVoxelZ = targetImage.getFileInfo()[0].getResolutions()[2];
            } else {
                targetVoxelZ = targetVoxelX;
            }
        } // else not series2D

        doConvert = false;
        sourceUnitsX = sourceImage.getFileInfo()[0].getUnitsOfMeasure()[0];
        sourceUnitsY = sourceImage.getFileInfo()[0].getUnitsOfMeasure()[1];
        sourceUnitsZ = sourceImage.getFileInfo()[0].getUnitsOfMeasure()[2];

        if (series2D) {
            targetUnitsX = sourceUnitsX;
            targetUnitsY = sourceUnitsY;
            targetUnitsZ = sourceUnitsZ;
        } else {
            targetUnitsX = targetImage.getFileInfo()[0].getUnitsOfMeasure()[0];
            targetUnitsY = targetImage.getFileInfo()[0].getUnitsOfMeasure()[1];
            targetUnitsZ = targetImage.getFileInfo()[0].getUnitsOfMeasure()[2];
        }

        targetUnits[0] = targetUnitsX;
        targetUnits[1] = targetUnitsY;
        targetUnits[2] = targetUnitsZ;

        if ((!inPlane) &&
                ((targetUnitsX != targetUnitsY) || (targetUnitsX != targetUnitsZ) || (targetUnitsX != sourceUnitsX) ||
                     (targetUnitsX != sourceUnitsY) || (targetUnitsX != sourceUnitsZ))) {
            doConvert = true;
        } else if ((inPlane) &&
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

        if (series2D) {

            // Move the reference slice unchanged into the result image
            try {
                sourceImage.exportData(referenceSlice * sourceSliceSize, sourceSliceSize, sourceArray); // locks and releases and lock
            } catch (IOException error) {
                finalize();
                MipavUtil.displayError("AlgorithmRegNonlinear: IOException on sourceImage.exportData" +
                                       "referenceSlice*sourceSliceSize,sourceSliceSize,sourceArray)");

                setCompleted(false);

                return;
            }

            try {
                resultImage.importData(referenceSlice * sourceSliceSize, sourceArray, false);
            } catch (IOException error) {
                finalize();
                MipavUtil.displayError("AlgorithmRegNonlinear: IOException on resultImage" +
                                       ".importData(referenceSlice*sourceSliceSize,sourceArray,false)");

                setCompleted(false);

                return;
            }

            tNumber = referenceSlice;

            if (tNumber == (sourceImage.getExtents()[2] - 1)) {
                sNumber = tNumber - 1;
                seriesUp = false;
            } else {
                sNumber = tNumber + 1;
                seriesUp = true;
            }

            targetExtents[0] = sourceXDim;
            targetExtents[1] = sourceYDim;
            targetImage = new ModelImage(ModelStorageBase.FLOAT, targetExtents, "target");

            sImage = new ModelImage(ModelStorageBase.FLOAT, targetExtents, "source");

            if (transformVOI) {
                VOIs = sourceImage.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {
                    voi = VOIs.VOIAt(i);
                    voiColor = voi.getColor();
                    outVOI = new VOI((short) resultImage.getVOIs().size(), resultImage.getImageName(), 1,
                                     voi.getCurveType(), -1.0f);
                    outVOI.setColor(voiColor);
                    outVOI.importNewVOI(referenceSlice, referenceSlice, voi, sourceImage.getExtents()[2], true);
                    resultImage.getVOIs().addElement(outVOI);
                }
            } // if (transformVOI)

            if (!doAdjacent) {

                try {
                    sourceImage.exportData(referenceSlice * targetSliceSize, targetSliceSize, targetArray); // locks and releases and lock
                } catch (IOException error) {
                    finalize();
                    MipavUtil.displayError("AlgorithmRegNonlinear: IOException on sourceImage.exportData" +
                                           "referenceSlice*targetSliceSize,targetSliceSize,targetArray)");

                    setCompleted(false);

                    return;
                }

                if (transformVOI) {
                    targetImage.unregisterAllVOIs();

                    for (i = 0; i < nVOI; i++) {
                        voi = VOIs.VOIAt(i);
                        voiColor = voi.getColor();
                        outVOI = new VOI((short) targetImage.getVOIs().size(), targetImage.getImageName(), 1,
                                         voi.getCurveType(), -1.0f);
                        outVOI.setColor(voiColor);
                        outVOI.importNewVOI(0, referenceSlice, voi, 1, true);
                        targetImage.getVOIs().addElement(outVOI);
                    }
                } // if (transformVOI)

                try {
                    targetImage.importData(0, targetArray, true);
                } catch (IOException error) {
                    finalize();
                    MipavUtil.displayError("AlgorithmRegNonlinear: IOException on targetImage" +
                                           ".importData(0,targetArray,true)");

                    setCompleted(false);

                    return;
                }
            } // if (!doAdjacent)

        } // if (series2D)
        else { // not series2D
            sImage = sourceImage;
        } // else not series2D

        for (series = 0; series < seriesNumber; series++) {

            if (series2D) {

                fireProgressStateChanged("Registering slice number " + String.valueOf(sNumber));
                fireProgressStateChanged(100 * series / seriesNumber);

                if (doAdjacent) {

                    if (tNumber == referenceSlice) {

                        try {
                            sourceImage.exportData(referenceSlice * targetSliceSize, targetSliceSize, targetArray); // locks and releases and lock
                        } catch (IOException error) {
                            finalize();
                            MipavUtil.displayError("AlgorithmRegNonlinear: IOException on sourceImage.exportData" +
                                                   "referenceSlice*targetSliceSize,targetSliceSize,targetArray)");

                            setCompleted(false);

                            return;
                        }

                        if (transformVOI) {
                            targetImage.unregisterAllVOIs();

                            for (i = 0; i < nVOI; i++) {
                                voi = VOIs.VOIAt(i);
                                voiColor = voi.getColor();
                                outVOI = new VOI((short) targetImage.getVOIs().size(), targetImage.getImageName(), 1,
                                                 voi.getCurveType(), -1.0f);
                                outVOI.setColor(voiColor);
                                outVOI.importNewVOI(0, referenceSlice, voi, 1, true);
                                targetImage.getVOIs().addElement(outVOI);
                            }
                        } // if (transformVOI)
                    } // if (tNumber == referenceSlice)
                    else {

                        try {
                            resultImage.exportData(tNumber * targetSliceSize, targetSliceSize, targetArray);
                        } catch (IOException error) {
                            finalize();
                            MipavUtil.displayError("AlgorithmRegNonlinear: IOException on resultImage.exportData" +
                                                   "tNumber*targetSliceSize,targetSliceSize,targetArray)");

                            setCompleted(false);

                            return;
                        }

                        if (transformVOI) {
                            targetImage.unregisterAllVOIs();

                            for (i = 0; i < nVOIr; i++) {
                                voi = VOIsr.VOIAt(i);
                                voiColor = voi.getColor();
                                outVOI = new VOI((short) targetImage.getVOIs().size(), targetImage.getImageName(), 1,
                                                 voi.getCurveType(), -1.0f);
                                outVOI.setColor(voiColor);
                                outVOI.importNewVOI(0, tNumber, voi, 1, true);
                                targetImage.getVOIs().addElement(outVOI);
                            }
                        } // if (transformVOI)
                    } // else tNumber != referenceSlice

                    try {
                        targetImage.importData(0, targetArray, true);
                    } catch (IOException error) {
                        finalize();
                        MipavUtil.displayError("AlgorithmRegNonlinear: IOException on targetImage" +
                                               ".importData(0,targetArray,true)");

                        setCompleted(false);

                        return;
                    }
                } // if (doAdjacent)

                try {
                    sourceImage.exportData(sNumber * sourceSliceSize, sourceSliceSize, sourceArray); // locks and releases and lock
                } catch (IOException error) {
                    finalize();
                    MipavUtil.displayError("AlgorithmRegNonlinear: IOException on sourceImage.exportData" +
                                           "sNumber*sourceSliceSize,sourceSliceSize,sourceArray)");

                    setCompleted(false);

                    return;
                }

                if (transformVOI) {
                    sImage.unregisterAllVOIs();

                    for (i = 0; i < nVOI; i++) {
                        voi = VOIs.VOIAt(i);
                        voiColor = voi.getColor();
                        outVOI = new VOI((short) sImage.getVOIs().size(), sImage.getImageName(), 1, voi.getCurveType(),
                                         -1.0f);
                        outVOI.setColor(voiColor);
                        outVOI.importNewVOI(0, sNumber, voi, 1, true);
                        sImage.getVOIs().addElement(outVOI);
                    }
                } // if (transformVOI)

                try {
                    sImage.importData(0, sourceArray, true);
                } catch (IOException error) {
                    finalize();
                    MipavUtil.displayError("AlgorithmRegNonlinear: IOException on sImage" +
                                           ".importData(0,sourceArray,true)");

                    setCompleted(false);

                    return;
                }
            } // if (series2D)
            else { // not series2D

                try {
                    sImage.exportData(0, sourceVolSize, sourceArray); // locks and releases and lock
                } catch (IOException error) {
                    finalize();
                    displayError("AlgorithmRegNonlinear: IOException on sImage.exportData");

                    setCompleted(false);

                    return;
                }

                try {
                    targetImage.exportData(0, targetVolSize, targetArray); // locks and releases and lock
                } catch (IOException error) {
                    finalize();
                    displayError("AlgorithmRegNonlinear: IOException on targetImage.exportData");

                    setCompleted(false);

                    return;
                }
            } // else not series2D

            sourceMask = new boolean[sourceVolSize];

            if (entireSource == false) {

                // mask = sImage.generateVOIMask();
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

            sampleFactor = SF; // initial sampling interval

            if (sampleFactor < 1) {
                finalize();
                MipavUtil.displayError("sampleFactor cannot be less than 1");

                setCompleted(false);

                return;
            }

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

            if (inPlane) {
                model = LINEAR2D6;
                extraZeros = 3;
                coords = 2;
                coeffp = 3;
                fcoeffp = d2[fmodel - 20];

                if (costFxn == SCALED_LEAST_SQUARES) {

                    if (interaction) {
                        uvderivsN = UVDERIV2DWARP;
                    } else {
                        uvderivsN = QVDERIV2DWARP;
                    }
                } // if (costFxn == SCALED_LEAST_SQUARES)
                else {

                    if (interaction) {
                        uvderivsN = UVDERIVCOR2DWARP;
                    } else {
                        uvderivsN = QVDERIVCOR2DWARP;
                    }
                }
            } else {
                model = LINEAR3D12;
                extraZeros = 6;
                coords = 3;
                coeffp = 4;
                fcoeffp = d3[fmodel];

                if (costFxn == SCALED_LEAST_SQUARES) {

                    if (interaction) {
                        uvderivsN = UVDERIV3DWARP;
                    } else {
                        uvderivsN = QVDERIV3DWARP;
                    }
                } // if (costFxn == SCALED_LEAST_SQUARES)
                else {

                    if (interaction) {
                        uvderivsN = UVDERIVCOR3DWARP;
                    } else {
                        uvderivsN = QVDERIVCOR3DWARP;
                    }
                }
            }

            /* Allocate memory for parameters including scaling */
            // tps = new double[coords*fcoeffp+1];
            // for (i = 0; i < coords*coeffp; i++) {
            // tps[i] = 0.0;
            // }
            // tps[coords*coeffp] = 1.0;
            es = new double[coords][coeffp];

            for (j = 0; j < coords; j++) {

                for (k = 0; k < coeffp; k++) {
                    es[j][k] = 0.0;
                }
            }

            scale = 1.0;

            if (costFxn == SCALED_LEAST_SQUARES) {
                tpsOld = new double[(coords * fcoeffp) + 1];

                for (i = 0; i < ((coords * fcoeffp) + 1); i++) {
                    tpsOld[i] = 0.0;
                }
            } else {
                tpsOld = new double[coords * fcoeffp];

                for (i = 0; i < (coords * fcoeffp); i++) {
                    tpsOld[i] = 0.0;
                }
            }

            // tps[0] = ((sourceXDim-1.0) - (targetXDim-1.0)*(targetVoxelX/sourceVoxelX))/2.0;
            // tps[1] = targetVoxelX/sourceVoxelX;
            // tps[coeffp] = ((sourceYDim-1.0) - (targetYDim-1.0)*(targetVoxelY/sourceVoxelY))/2.0;
            // tps[coeffp+2] = targetVoxelY/sourceVoxelY;
            // if (coords > 2) {
            // tps[2*coeffp] = ((sourceZDim-1.0) -(targetZDim-1.0)*(targetVoxelZ/sourceVoxelZ))/2.0;
            // tps[2*coeffp+3] = targetVoxelZ/sourceVoxelZ;
            // }
            if (esInitial == null) {
                es[0][0] = ((sourceXDim - 1.0) - ((targetXDim - 1.0) * (targetVoxelX / sourceVoxelX))) / 2.0;
                es[0][1] = targetVoxelX / sourceVoxelX;
                es[1][0] = ((sourceYDim - 1.0) - ((targetYDim - 1.0) * (targetVoxelY / sourceVoxelY))) / 2.0;
                es[1][2] = targetVoxelY / sourceVoxelY;

                if (coords > 2) {
                    es[2][0] = ((sourceZDim - 1.0) - ((targetZDim - 1.0) * (targetVoxelZ / sourceVoxelZ))) / 2.0;
                    es[2][3] = targetVoxelZ / sourceVoxelZ;
                }
            } // if (esInitial == null)
            else {

                for (i = 0; i < es.length; i++) {

                    for (j = 0; j < es[i].length; j++) {
                        es[i][j] = esInitial[i][j];
                    }
                }
            }

            /* Make record of old tps */

            /*for (i = 0; i <= coords*coeffp; i++) {
             * tpsOld[i] = tps[i]; }*/
            for (i = 0, j = 0; j < coords; j++) {

                for (k = 0; k < coeffp; k++) {
                    tpsOld[i++] = es[j][k];
                }
            }

            if (costFxn == SCALED_LEAST_SQUARES) {
                tpsOld[i] = scale;
            }

            if (threadStopped) {
                finalize();

                setCompleted(false);

                return;
            }

            /* Smooth if requested */
            if ((sourceGaussX != 0.0f) || (sourceGaussY != 0.0f) || (sourceGaussZ != 0.0f)) {

                try {

                    if ((!series2D)) {
                        fireProgressStateChanged("Blurring source Image");
                        fireProgressStateChanged(5);
                    } // if ((!series2D) && )

                    sigmas[0] = sourceGaussX;
                    sigmas[1] = sourceGaussY;
                    sigmas[2] = sourceGaussZ;

                    // Make algorithm
                    gaussianBlurAlgo = new AlgorithmGaussianBlur(sImage, sigmas, true, inPlane);
                    gaussianBlurAlgo.run();
                } catch (OutOfMemoryError x) {
                    finalize();
                    MipavUtil.displayError("Algorithm Registration Nonlinear: unable to allocate enough memory for sourceGaussianImage");

                    setCompleted(false);

                    return;
                }

                try {
                    sImage.exportData(0, sourceVolSize, sourceArray); // locks and releases and lock
                } catch (IOException error) {
                    finalize();
                    displayError("AlgorithmRegNonlinear: sourceImage is locked after Gaussian blur");

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

                    if ((!series2D)) {
                        fireProgressStateChanged("Blurring target Image");
                        fireProgressStateChanged(10);
                    }

                    sigmas[0] = targetGaussX;
                    sigmas[1] = targetGaussY;
                    sigmas[2] = targetGaussZ;

                    // Make algorithm
                    gaussianBlurAlgo = new AlgorithmGaussianBlur(targetImage, sigmas, entireTarget, inPlane);
                    gaussianBlurAlgo.run();
                } catch (OutOfMemoryError x) {
                    finalize();
                    MipavUtil.displayError("Algorithm Registration Nonlinear: unable to allocate enough memory for targetGaussianImage");

                    setCompleted(false);

                    return;
                }

                try {
                    targetImage.exportData(0, targetVolSize, targetArray); // locks and releases and lock
                } catch (IOException error) {
                    finalize();
                    displayError("AlgorithmRegNonlinear: targetImage is locked after Gaussian blur");

                    setCompleted(false);

                    return;
                }
            } // if ((targetGaussX != 0.0f) || (targetGaussY != 0.0f) || (targetGaussZ != 0.0f))

            sigmas = null;

            maxActualValue = -Double.MAX_VALUE;
            minActualValue = Double.MAX_VALUE;

            for (i = 0; i < targetVolSize; i++) {

                if ((targetMask[i]) && (targetArray[i] > maxActualValue)) {
                    maxActualValue = targetArray[i];
                }

                if ((targetMask[i]) && (targetArray[i] < minActualValue)) {
                    minActualValue = targetArray[i];
                }
            }

            if (costFxn == CORRELATION_RATIO) {

                try {
                    q1 = new int[256];
                    q2 = new double[256];
                    q3 = new double[256];
                    q4 = new double[256][coords * fcoeffp];
                    q5 = new double[256][coords * fcoeffp];
                    q6 = new double[256][coords * fcoeffp][coords * fcoeffp];
                    q7 = new double[256][coords * fcoeffp][coords * fcoeffp];
                } catch (OutOfMemoryError x) {
                    finalize();
                    MipavUtil.displayError("AlgorithmRegistration Nonlinear: Unable to " + "allocate q1 thru q7");

                    setCompleted(false);

                    return;
                }
            } // if (costFxn == CORRELATION_RATIO)

            while (model <= fmodel) {

                if ((!series2D)) {

                    switch (model) {

                        case LINEAR2D6:
                        case LINEAR3D12:
                            fireProgressStateChanged("Calculating first order parameters");
                            break;

                        case NL2D12:
                        case NL3D30:
                            fireProgressStateChanged("Calculating second order parameters");
                            fireProgressStateChanged(60);
                            break;

                        case NL2D20:
                        case NL3D60:
                            fireProgressStateChanged("Calculating third order parameters");
                            break;

                        case NL2D30:
                        case NL3D105:
                            fireProgressStateChanged("Calculating fourth order parameters");
                            break;

                        case NL2D42:
                        case NL3D168:
                            fireProgressStateChanged("Calculating fifth order parameters");
                            break;

                        case NL2D56:
                        case NL3D252:
                            fireProgressStateChanged("Calculating sixth order parameters");
                            break;

                        case NL2D72:
                        case NL3D360:
                            fireProgressStateChanged("Calculating seventh order parameters");
                            break;

                        case NL2D90:
                        case NL3D495:
                            fireProgressStateChanged("Calculating eighth order parameters");
                            break;

                        case NL2D110:
                        case NL3D660:
                            fireProgressStateChanged("Calculating ninth order parameters");
                            break;

                        case NL2D132:
                        case NL3D858:
                            fireProgressStateChanged("Calculating tenth order parameters");
                            break;

                        case NL2D156:
                        case NL3D1092:
                            fireProgressStateChanged("Calculating eleventh order parameters");
                            break;

                        case NL2D182:
                        case NL3D1365:
                            fireProgressStateChanged("Calculating twelfth order parameters");
                            break;
                    } // switch(model)
                } // if ((!series2D) && )

                // Perform alignment
                if (costFxn == SCALED_LEAST_SQUARES) {
                    parameters = (coords * coeffp) + 1; /* Spatial parameters plus rescaling parameter */
                } else {
                    parameters = coords * coeffp;
                }

                /* Allocate matrices */
                try {
                    ecf = new double[parameters][parameters];
                    ecf2 = new double[parameters][parameters];
                    dcf = new double[parameters];
                    delta = new double[parameters];
                    tpsBest = new double[parameters];
                    kpvt = new int[parameters];
                    dx = new double[coeffp];
                    gersch = new double[parameters];
                } catch (OutOfMemoryError x) {
                    finalize();
                    MipavUtil.displayError("Algorithm Registration Nonlinear: unable to allocate enough memory for matrices");

                    setCompleted(false);

                    return;
                }

                /*for (j = 0; j < coords; j++) {
                 * for (k = 0; k < coeffp; k++) { es[j][k] = tps[k+j*coeffp]; } } scale = tps[coords*coeffp];*/

                for (j = 0; j < coords; j++) {
                    Preferences.debug("START ");

                    for (jj = 0; jj < coeffp; jj++) {
                        Preferences.debug(es[j][jj] + " ");
                    }

                    Preferences.debug("\n");
                }

                if (costFxn == SCALED_LEAST_SQUARES) {
                    Preferences.debug("scale=" + scale + "\n");
                    Preferences.debug("\n");
                }

                /*for (jj = 0; jj < parameters; jj++) {
                 * tpsBest[jj] = tps[jj]; }*/
                for (i = 0, j = 0; j < coords; j++) {

                    for (k = 0; k < coeffp; k++) {
                        tpsBest[i++] = es[j][k];
                    }
                }

                if (costFxn == SCALED_LEAST_SQUARES) {
                    tpsBest[i] = scale;
                }

                /* Iterate until the final sample factor is reached */
                while (sampleFactor >= sampleFactor2) {
                    iters = 0;
                    tries = 0;
                    deltacf = Double.MAX_VALUE;
                    cfBest = Double.MAX_VALUE;

                    do { // while ((Math.abs(deltacf) > precision) && (!threadStopped))

                        error = 0;

                        switch (uvderivsN) {

                            case UVDERIV2DWARP:
                                cf = uvderiv2Dwarp(parameters - 1, es, sampleFactor, sourceArray, sourceMask,
                                                   sourceXDim, sourceYDim, sourceThreshold, targetArray, targetMask,
                                                   targetXDim, targetYDim, targetZDim, targetThreshold, dcf, ecf, dx,
                                                   scale);
                                break;

                            case UVDERIV3DWARP:
                                cf = uvderiv3Dwarp(parameters - 1, es, sampleFactor, sourceArray, sourceMask,
                                                   sourceXDim, sourceYDim, sourceZDim, sourceThreshold, targetArray,
                                                   targetMask, targetXDim, targetYDim, targetZDim, targetThreshold, dcf,
                                                   ecf, dx, scale);
                                break;

                            case QVDERIV2DWARP:
                                cf = qvderiv2Dwarp(parameters - 1, es, sampleFactor, sourceArray, sourceMask,
                                                   sourceXDim, sourceYDim, sourceThreshold, targetArray, targetMask,
                                                   targetXDim, targetYDim, targetZDim, targetThreshold, dcf, ecf, dx,
                                                   scale);
                                break;

                            case QVDERIV3DWARP:
                                cf = qvderiv3Dwarp(parameters - 1, es, sampleFactor, sourceArray, sourceMask,
                                                   sourceXDim, sourceYDim, sourceZDim, sourceThreshold, targetArray,
                                                   targetMask, targetXDim, targetYDim, targetZDim, targetThreshold, dcf,
                                                   ecf, dx, scale);
                                break;

                            case UVDERIVCOR2DWARP:
                                cf = uvderivCOR2Dwarp(parameters, es, sampleFactor, sourceArray, sourceMask, sourceXDim,
                                                      sourceYDim, sourceThreshold, targetArray, targetMask, targetXDim,
                                                      targetYDim, targetZDim, targetThreshold, dcf, ecf, dx, scale, q1,
                                                      q2, q3, q4, q5, q6, q7, 256, maxActualValue, minActualValue);
                                break;

                            case QVDERIVCOR2DWARP:
                                cf = qvderivCOR2Dwarp(parameters, es, sampleFactor, sourceArray, sourceMask, sourceXDim,
                                                      sourceYDim, sourceThreshold, targetArray, targetMask, targetXDim,
                                                      targetYDim, targetZDim, targetThreshold, dcf, ecf, dx, scale, q1,
                                                      q2, q3, q4, q5, q7, 256, maxActualValue, minActualValue);
                                break;

                            case UVDERIVCOR3DWARP:
                                cf = uvderivCOR3Dwarp(parameters, es, sampleFactor, sourceArray, sourceMask, sourceXDim,
                                                      sourceYDim, sourceZDim, sourceThreshold, targetArray, targetMask,
                                                      targetXDim, targetYDim, targetZDim, targetThreshold, dcf, ecf, dx,
                                                      scale, q1, q2, q3, q4, q5, q6, q7, 256, maxActualValue,
                                                      minActualValue);
                                break;

                            case QVDERIVCOR3DWARP:
                                cf = qvderivCOR3Dwarp(parameters, es, sampleFactor, sourceArray, sourceMask, sourceXDim,
                                                      sourceYDim, sourceZDim, sourceThreshold, targetArray, targetMask,
                                                      targetXDim, targetYDim, targetZDim, targetThreshold, dcf, ecf, dx,
                                                      scale, q1, q2, q3, q4, q5, q7, 256, maxActualValue,
                                                      minActualValue);
                                break;
                        } // switch(uvderivsN)

                        if (error != 0) {
                            break;
                        }

                        for (t = 0; t < parameters; t++) {
                            dcf[t] *= -1;
                        }

                        if (cf < cfBest) {
                            cfBest = cf;

                            /*for (jj = 0; jj < parameters; jj++) {
                             * tpsBest[jj] = tps[jj]; }*/
                            for (j = 0; j < coords; j++) {

                                for (k = 0; k < coeffp; k++) {
                                    tpsBest[k + (j * coeffp)] = es[j][k];
                                }
                            }

                            if (costFxn == SCALED_LEAST_SQUARES) {
                                tpsBest[coords * coeffp] = scale;
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

                        for (t = 0; t < parameters; t++) {
                            delta[t] = dcf[t];

                            for (s = 0; s <= t; s++) {
                                ecf2[t][s] = ecf[t][s];
                            }
                        }

                        /* Now we solve the system of equations Hessian*delta = gradient */

                        if (dpofa(ecf2, parameters) == parameters) {
                            dposl(ecf2, parameters, delta);
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
                            for (t = 0; t < parameters; t++) {

                                for (s = 0; s <= t; s++) {
                                    ecf2[t][s] = ecf[t][s];
                                }
                            }

                            /* Now use modchol and dmodposl which do not require a positive definite Hessian */
                            modchol(ecf2, parameters, kpvt, gersch);
                            Preferences.debug("Using revised modified Cholesky factorization\n");
                            dmodposl(ecf2, parameters, delta, kpvt);
                        } // else Hessian is not positive definite

                        /*for (jj = 0; jj < parameters; jj++) {
                         * tps[jj]+=delta[jj]; }*/
                        for (i = 0, j = 0; j < coords; j++) {

                            for (k = 0; k < coeffp; k++) {
                                es[j][k] += delta[i++];
                            }
                        }

                        if (costFxn == SCALED_LEAST_SQUARES) {
                            scale += delta[i];
                        }

                        deltacf = sdpred(delta, dcf, ecf, parameters);

                        Preferences.debug("cost function = " + cf + "\n");
                        Preferences.debug("Predicted cost function change = " + deltacf + "\n");
                        Preferences.debug("iterations completed: " + iters + " sampling: " + sampleFactor +
                                          " parameters: " + parameters + "\n");

                        Preferences.debug("\n");

                        for (j = 0; j < coords; j++) {
                            Preferences.debug("NEW ");

                            for (jj = 0; jj < coeffp; jj++) {
                                Preferences.debug(es[j][jj] + " ");
                            }

                            Preferences.debug("\n");
                        }

                        if (costFxn == SCALED_LEAST_SQUARES) {
                            Preferences.debug("scale = " + scale + "\n");
                            Preferences.debug("\n");
                        }
                    } while ((Math.abs(deltacf) > precision) && (!threadStopped));

                    if (threadStopped) {
                        finalize();

                        setCompleted(false);

                        return;
                    }

                    /*for (jj = 0; jj < parameters; jj++) {
                     * tps[jj] = tpsBest[jj]; }*/
                    for (i = 0, j = 0; j < coords; j++) {

                        for (k = 0; k < coeffp; k++) {
                            es[j][k] = tpsBest[i++];
                        }
                    }

                    if (costFxn == SCALED_LEAST_SQUARES) {
                        scale = tpsBest[i];
                    }

                    Preferences.debug("\n");
                    Preferences.debug("BEST VALUES AT SAMPLEFACTOR= " + sampleFactor + ":\n");
                    Preferences.debug("cost function = " + cfBest + "\n");
                    Preferences.debug("\n");

                    for (j = 0; j < coords; j++) {
                        Preferences.debug("BEST ");

                        for (jj = 0; jj < coeffp; jj++) {
                            Preferences.debug(es[j][jj] + " ");
                        }

                        Preferences.debug("\n");
                    }

                    if (costFxn == SCALED_LEAST_SQUARES) {
                        Preferences.debug("scale = " + scale + "\n");
                        Preferences.debug("\n");
                    }

                    /* Calculate next sample factor for next iteration */
                    sampleFactor = sampleFactor / sfFactor;
                } // while (sampleFactor >= sampleFactor2)

                Preferences.debug("cost function = " + cfBest + "\n");

                if (ecf != null) {

                    for (i = 0; i < ecf.length; i++) {
                        ecf[i] = null;
                    }

                    ecf = null;
                }

                if (ecf2 != null) {

                    for (i = 0; i < ecf2.length; i++) {
                        ecf2[i] = null;
                    }

                    ecf2 = null;
                }

                dcf = null;
                delta = null;
                tpsBest = null;
                kpvt = null;
                dx = null;
                gersch = null;
                System.gc();
                model++;

                if (model <= fmodel) {

                    for (j = 0, k = 0; j < coords; j++) {

                        for (i = 0; i < coeffp; i++, k++) {

                            // tpsOld[k] = tps[j*coeffp+i];
                            tpsOld[k] = es[j][i];
                        }

                        for (i = 0; i < extraZeros; i++, k++) {
                            tpsOld[k] = 0.0;
                        }
                    }

                    // tpsOld[k] = tps[coords*coeffp];
                    if (costFxn == SCALED_LEAST_SQUARES) {
                        tpsOld[k] = scale;
                    }

                    if (model < 21) {
                        extraZeros = d3[model + 1] - d3[model];
                        coeffp = d3[model];
                    } else {
                        extraZeros = d2[model - 20 + 1] - d2[model - 20];
                        coeffp = d2[model - 20];
                    }

                    /* Free memory allocated by previous model */
                    if (es != null) {

                        for (i = 0; i < es.length; i++) {
                            es[i] = null;
                        }

                        es = null;
                    }

                    es = new double[coords][coeffp];

                    // Copy reformatted parameters back into tps
                    /*for (kk = 0;kk <= coords*coeffp; kk++) {
                     * tps[kk] = tpsOld[kk]; }*/
                    for (i = 0, j = 0; j < coords; j++) {

                        for (k = 0; k < coeffp; k++) {
                            es[j][k] = tpsOld[i++];
                        }
                    }

                    if (costFxn == SCALED_LEAST_SQUARES) {
                        scale = tpsOld[i];
                    }
                } // if (model <= fmodel)
            } // while (model <= fmodel)

            // tps = null;
            targetMask = null;

            if (threadStopped) {
                finalize();

                setCompleted(false);

                return;
            }

            if ((!series2D)) {
                fireProgressStateChanged("Reslicing source image");
                fireProgressStateChanged(80);
            }

            if (inPlane) { // 2D
                sMinValue = Float.MAX_VALUE;
                sMaxValue = -Float.MAX_VALUE;

                for (i = 0; i < sourceArray.length; i++) {

                    if (sourceArray[i] > sMaxValue) {
                        sMaxValue = sourceArray[i];
                    }

                    if (sourceArray[i] < sMinValue) {
                        sMinValue = sourceArray[i];
                    }
                }

                dataOut = rWarp2D(sourceArray, sourceXDim, sourceYDim, sMinValue, sMaxValue, targetXDim, targetYDim,
                                  targetZDim, es, fmodel - 20, interpolation);

            } else { // 3D
                dataOut = rWarp3D(sourceArray, sourceXDim, sourceYDim, sourceZDim, (float) sourceImage.getMin(),
                                  (float) sourceImage.getMax(), targetXDim, targetYDim, targetZDim, es, fmodel,
                                  interpolation);
            }

            if (series2D) {

                try {
                    resultImage.importData(sNumber * sourceSliceSize, dataOut, false);
                } catch (IOException error) {
                    finalize();
                    MipavUtil.displayError("AlgorithmRegNonlinear: IOException on resultImage" +
                                           ".importData(sNumber*sourceSliceSize,dataOut,false)");

                    setCompleted(false);

                    return;
                }
            } // if (series2D)
            else { // not series2D

                try {
                    resultImage.importData(0, dataOut, true);
                } catch (IOException error) {
                    finalize();
                    MipavUtil.displayError("AlgorithmRegNonlinear: IOException on resultImage" +
                                           ".importData(0,dataOut,true)");

                    setCompleted(false);

                    return;
                }
            } // else not series2D

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

            if (transformVOI) {
                maskImage = sImage.generateShortImage(1);

                try {
                    maskImage.exportData(0, sourceVolSize, sourceArray); // locks and releases lock
                } catch (IOException error) {
                    finalize();
                    MipavUtil.displayError("Algorithm RegNonlinear: maskImage locked");

                    setCompleted(false);

                    return;
                }

                if ((!series2D)) {
                    fireProgressStateChanged("Reslicing mask image");
                    fireProgressStateChanged(85);
                }

                sMinValue = Float.MAX_VALUE;
                sMaxValue = -Float.MAX_VALUE;

                for (i = 0; i < sourceVolSize; i++) {

                    if (sourceArray[i] > sMaxValue) {
                        sMaxValue = sourceArray[i];
                    }

                    if (sourceArray[i] < sMinValue) {
                        sMinValue = sourceArray[i];
                    }
                }

                if (inPlane) { // 2D
                    dataOut = rWarp2D(sourceArray, sourceXDim, sourceYDim, sMinValue, sMaxValue, targetXDim, targetYDim,
                                      targetZDim, es, fmodel - 20, AlgorithmTransform.NEAREST_NEIGHBOR);

                } else { // 3D
                    dataOut = rWarp3D(sourceArray, sourceXDim, sourceYDim, sourceZDim, sMinValue, sMaxValue, targetXDim,
                                      targetYDim, targetZDim, es, fmodel, AlgorithmTransform.NEAREST_NEIGHBOR);
                }

                maskImage.disposeLocal();
                maskImage = new ModelImage(ModelImage.SHORT, resultImage.getExtents(), null);

                try {
                    maskImage.importData(0, dataOut, true);
                } catch (IOException error) {
                    finalize();
                    MipavUtil.displayError("AlgorithmRegNonlinear: IOException on maskImage" +
                                           ".importData(0,dataOut,true)");

                    setCompleted(false);

                    return;
                } catch (OutOfMemoryError e) {
                    finalize();
                    MipavUtil.displayError("AlgorithmRegNonlinear: Out of memory on maskImage" +
                                           ".importData(0,dataOut,true)");

                    setCompleted(false);

                    return;
                }

                dataOut = null;

                if ((!series2D)) {
                    fireProgressStateChanged("Performing VOI extraction algorithm");
                    fireProgressStateChanged(90);
                }

                AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(maskImage);
                VOIExtAlgo.run();

                if (series2D) {
                    VOIsm = maskImage.getVOIs();
                    nVOIm = VOIsm.size();

                    for (i = 0; i < nVOIm; i++) {
                        voi = VOIsm.VOIAt(i);
                        voiColor = voi.getColor();
                        outVOI = new VOI((short) resultImage.getVOIs().size(), resultImage.getImageName(), 1,
                                         voi.getCurveType(), -1.0f);
                        outVOI.setColor(voiColor);
                        outVOI.importNewVOI(sNumber, 0, voi, resultZDim, true);
                        resultImage.getVOIs().addElement(outVOI);
                    }
                } else {
                    resultImage.setVOIs(maskImage.getVOIs());
                }

                maskImage.disposeLocal();
                maskImage = null;
            } // if (transformVOI)

            if (writeParameters) {
                fileDir = sourceImage.getFileInfo(0).getFileDirectory();

                if (series2D) {
                    fileName = sourceImage.getImageName() + String.valueOf(sNumber) + ".par";
                } else {
                    fileName = sourceImage.getImageName() + ".par";
                }

                file = new File(fileDir + fileName);

                try {
                    raFile = new RandomAccessFile(file, "rw");
                } catch (FileNotFoundException e) {
                    finalize();
                    MipavUtil.displayError("File Not Found Exception with " + fileDir + fileName);
                    MipavUtil.displayError("Unsuccessful attempt to create parameter file");

                    setCompleted(true);

                    return;
                }

                endianess = true; // big-endianess

                try {
                    writeInt(targetXDim, endianess);
                    writeInt(targetYDim, endianess);
                    writeInt(targetZDim, endianess);
                    writeFloat(resultResolutions[0], endianess);
                    writeFloat(resultResolutions[1], endianess);
                    writeFloat(resultResolutions[2], endianess);
                    writeInt(targetUnits[0], endianess);
                    writeInt(targetUnits[1], endianess);
                    writeInt(targetUnits[2], endianess);
                    writeInt(fmodel, endianess);

                    for (i = 0; i < coords; i++) {

                        for (j = 0; j < coeffp; j++) {
                            writeDouble(es[i][j], endianess);
                        }
                    }
                } catch (IOException e) {
                    finalize();
                    MipavUtil.displayError("Error writing to parameter file " + fileDir + fileName);

                    setCompleted(true);

                    return;
                }

                try {
                    raFile.close();
                } catch (IOException e) {
                    finalize();
                    MipavUtil.displayError("Error closing parameter file " + fileDir + fileName);

                    setCompleted(true);

                    return;
                }
            } // if (writeParameters)

            if (series2D) {

                if (seriesUp) {
                    tNumber++;
                    sNumber++;

                    if (tNumber == (sourceImage.getExtents()[2] - 1)) {
                        tNumber = referenceSlice;
                        sNumber = tNumber - 1;
                        seriesUp = false;
                    }
                } else { // not seriesUp
                    tNumber--;
                    sNumber--;
                } // else not seriesUp
            } // if series2D
        } // for (series = 0 ; series < seriesNumber; series++)

        sourceArray = null;
        targetArray = null;

        if (series2D) {
            resultImage.calcMinMax();

            if (sImage != null) {
                sImage.disposeLocal();
            }

            sImage = null;

            if (targetImage != null) {
                targetImage.disposeLocal();
            }

            targetImage = null;
        } // if (series2D)

        resultResolutions = null;
        targetUnits = null;

        setCompleted(true);

        return;
    }

    /**
     * Writes a double as eight bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeDouble(double data, boolean endianess) throws IOException {

        long tmpLong;

        tmpLong = Double.doubleToLongBits(data);
        writeLong(tmpLong, endianess);
    }

    /**
     * Writes a float as four bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeFloat(float data, boolean endianess) throws IOException {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        writeInt(tmpInt, endianess);
    }

    /**
     * Writes an int as four bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeInt(int data, boolean endianess) throws IOException {
        byte[] byteBuffer4 = new byte[4];

        if (endianess == true) {
            byteBuffer4[0] = (byte) (data >>> 24);
            byteBuffer4[1] = (byte) (data >>> 16);
            byteBuffer4[2] = (byte) (data >>> 8);
            byteBuffer4[3] = (byte) (data & 0xff);
        } else {
            byteBuffer4[0] = (byte) (data & 0xff);
            byteBuffer4[1] = (byte) (data >>> 8);
            byteBuffer4[2] = (byte) (data >>> 16);
            byteBuffer4[3] = (byte) (data >>> 24);
        }

        raFile.write(byteBuffer4);
    }

    /**
     * Writes a long as eight bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeLong(long data, boolean endianess) throws IOException {
        byte[] byteBuffer8 = new byte[8];

        if (endianess == true) {
            byteBuffer8[0] = (byte) (data >>> 56);
            byteBuffer8[1] = (byte) (data >>> 48);
            byteBuffer8[2] = (byte) (data >>> 40);
            byteBuffer8[3] = (byte) (data >>> 32);
            byteBuffer8[4] = (byte) (data >>> 24);
            byteBuffer8[5] = (byte) (data >>> 16);
            byteBuffer8[6] = (byte) (data >>> 8);
            byteBuffer8[7] = (byte) (data & 0xff);
        } else {
            byteBuffer8[0] = (byte) (data & 0xff);
            byteBuffer8[1] = (byte) (data >>> 8);
            byteBuffer8[2] = (byte) (data >>> 16);
            byteBuffer8[3] = (byte) (data >>> 24);
            byteBuffer8[4] = (byte) (data >>> 32);
            byteBuffer8[5] = (byte) (data >>> 40);
            byteBuffer8[6] = (byte) (data >>> 48);
            byteBuffer8[7] = (byte) (data >>> 56);
        }

        raFile.write(byteBuffer8);
    }

    /* From material Copyright 1995-2001 Roger P. Woods, M.D. */

    /* Modified 5/16/01 */

    /**
     * From material Copyright 1995-2001 Roger P. Woods, M.D. Modified 5/10/01 void dmodposl() This routine will solve
     * a*x=b using factors computed by MODCHOL On entry
     *
     * @param  a     the output from DGECO or DGEFA
     * @param  n     the order of the matrix A
     * @param  b     the right hand side vector (replaced by soln vector)
     * @param  ipvt  the pivot vector from MODCHOL On return b the solution vector x
     */
    private void dmodposl(double[][] a, int n, double[] b, int[] ipvt) {
        int i, k, l; /*  Solve TRANS(R)*y=b  */

        {

            for (k = 0; k < n; k++) {
                double t; /* Undo pivoting */

                {
                    l = ipvt[k];

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

            for (k = n; (k--) != 0;) {

                b[k] /= a[k][k];

                for (i = 0; i < k; i++) {
                    b[i] += (-b[k]) * a[k][i];
                } /* Undo pivoting */

                {
                    l = ipvt[k];

                    if (l != k) {

                        double t = b[l];

                        b[l] = b[k];
                        b[k] = t;
                    }
                }
            }
        }
    }


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

    /* From material Copyright 1995-2001 Roger P. Woods, M.D. */

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

    /* From material Copyright 2000-2001 Roger P. Woods, M.D. */

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
     * @param  dx     array to be set
     * @param  order  DOCUMENT ME!
     * @param  i1     DOCUMENT ME!
     * @param  j1     DOCUMENT ME!
     */
    private void dx2DSet(double[] dx, int order, double i1, double j1) {
        double i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12;
        double j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12;
        dx[0] = 1.0;

        if (order != 0) {
            dx[1] = i1;
            dx[2] = j1;

            if (order > 1) {
                i2 = i1 * i1;
                j2 = j1 * j1;
                dx[3] = i2;
                dx[4] = i1 * j1;
                dx[5] = j2;

                if (order > 2) {
                    i3 = i2 * i1;
                    j3 = j2 * j1;
                    dx[6] = i3;
                    dx[7] = i2 * j1;
                    dx[8] = i1 * j2;
                    dx[9] = j3;

                    if (order > 3) {
                        i4 = i3 * i1;
                        j4 = j3 * j1;
                        dx[10] = i4;
                        dx[11] = i3 * j1;
                        dx[12] = i2 * j2;
                        dx[13] = i1 * j3;
                        dx[14] = j4;

                        if (order > 4) {
                            i5 = i4 * i1;
                            j5 = j4 * j1;
                            dx[15] = i5;
                            dx[16] = i4 * j1;
                            dx[17] = i3 * j2;
                            dx[18] = i2 * j3;
                            dx[19] = i1 * j4;
                            dx[20] = j5;

                            if (order > 5) {
                                i6 = i5 * i1;
                                j6 = j5 * j1;
                                dx[21] = i6;
                                dx[22] = i5 * j1;
                                dx[23] = i4 * j2;
                                dx[24] = i3 * j3;
                                dx[25] = i2 * j4;
                                dx[26] = i1 * j5;
                                dx[27] = j6;

                                if (order > 6) {
                                    i7 = i6 * i1;
                                    j7 = j6 * j1;
                                    dx[28] = i7;
                                    dx[29] = i6 * j1;
                                    dx[30] = i5 * j2;
                                    dx[31] = i4 * j3;
                                    dx[32] = i3 * j4;
                                    dx[33] = i2 * j5;
                                    dx[34] = i1 * j6;
                                    dx[35] = j7;

                                    if (order > 7) {
                                        i8 = i7 * i1;
                                        j8 = j7 * j1;
                                        dx[36] = i8;
                                        dx[37] = i7 * j1;
                                        dx[38] = i6 * j2;
                                        dx[39] = i5 * j3;
                                        dx[40] = i4 * j4;
                                        dx[41] = i3 * j5;
                                        dx[42] = i2 * j6;
                                        dx[43] = i1 * j7;
                                        dx[44] = j8;

                                        if (order > 8) {
                                            i9 = i8 * i1;
                                            j9 = j8 * j1;
                                            dx[45] = i9;
                                            dx[46] = i8 * j1;
                                            dx[47] = i7 * j2;
                                            dx[48] = i6 * j3;
                                            dx[49] = i5 * j4;
                                            dx[50] = i4 * j5;
                                            dx[51] = i3 * j6;
                                            dx[52] = i2 * j7;
                                            dx[53] = i1 * j8;
                                            dx[54] = j9;

                                            if (order > 9) {
                                                i10 = i9 * i1;
                                                j10 = j9 * j1;
                                                dx[55] = i10;
                                                dx[56] = i9 * j1;
                                                dx[57] = i8 * j2;
                                                dx[58] = i7 * j3;
                                                dx[59] = i6 * j4;
                                                dx[60] = i5 * j5;
                                                dx[61] = i4 * j6;
                                                dx[62] = i3 * j7;
                                                dx[63] = i2 * j8;
                                                dx[64] = i1 * j9;
                                                dx[65] = j10;

                                                if (order > 10) {
                                                    i11 = i10 * i1;
                                                    j11 = j10 * j1;
                                                    dx[66] = i11;
                                                    dx[67] = i10 * j1;
                                                    dx[68] = i9 * j2;
                                                    dx[69] = i8 * j3;
                                                    dx[70] = i7 * j4;
                                                    dx[71] = i6 * j5;
                                                    dx[72] = i5 * j6;
                                                    dx[73] = i4 * j7;
                                                    dx[74] = i3 * j8;
                                                    dx[75] = i2 * j9;
                                                    dx[76] = i1 * j10;
                                                    dx[77] = j11;

                                                    if (order > 11) {
                                                        i12 = i11 * i1;
                                                        j12 = j11 * j1;
                                                        dx[78] = i12;
                                                        dx[79] = i11 * j1;
                                                        dx[80] = i10 * j2;
                                                        dx[81] = i9 * j3;
                                                        dx[82] = i8 * j4;
                                                        dx[83] = i7 * j5;
                                                        dx[84] = i6 * j6;
                                                        dx[85] = i5 * j7;
                                                        dx[86] = i4 * j8;
                                                        dx[87] = i3 * j9;
                                                        dx[88] = i2 * j10;
                                                        dx[89] = i1 * j11;
                                                        dx[90] = j12;
                                                    } /*End if(order>11)*/
                                                } /*End if(order>10)*/
                                            } /*End if(order>9)*/
                                        } /*End if(order>8)*/
                                    } /*End if(order>7)*/
                                } /*End if(order>6)*/
                            } /*End if(order>5)*/
                        } /*End if(order>4)*/
                    } /*End if(order>3)*/
                } /*End if(order>2)*/
            } /*End if(order>1)*/
        } /*End if(order>0)*/
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dx     array to be set
     * @param  order  DOCUMENT ME!
     * @param  i1     DOCUMENT ME!
     * @param  j1     DOCUMENT ME!
     * @param  k1     DOCUMENT ME!
     */
    private void dx3DSet(double[] dx, int order, double i1, double j1, double k1) {
        double i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12;
        double j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12;
        double k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12;

        dx[0] = 1.0;

        if (order != 0) {
            dx[1] = i1;
            dx[2] = j1;
            dx[3] = k1;

            if (order > 1) {

                i2 = i1 * i1;
                j2 = j1 * j1;
                k2 = k1 * k1;

                dx[4] = i2;
                dx[5] = i1 * j1;
                dx[6] = j2;
                dx[7] = i1 * k1;
                dx[8] = j1 * k1;
                dx[9] = k2;

                if (order > 2) {

                    i3 = i2 * i1;
                    j3 = j2 * j1;
                    k3 = k2 * k1;

                    dx[10] = i3;
                    dx[11] = i2 * j1;
                    dx[12] = i1 * j2;
                    dx[13] = j3;
                    dx[14] = i2 * k1;
                    dx[15] = i1 * j1 * k1;
                    dx[16] = j2 * k1;
                    dx[17] = i1 * k2;
                    dx[18] = j1 * k2;
                    dx[19] = k3;

                    if (order > 3) {

                        i4 = i3 * i1;
                        j4 = j3 * j1;
                        k4 = k3 * k1;

                        dx[20] = i4;
                        dx[21] = i3 * j1;
                        dx[22] = i2 * j2;
                        dx[23] = i1 * j3;
                        dx[24] = j4;
                        dx[25] = i3 * k1;
                        dx[26] = i2 * j1 * k1;
                        dx[27] = i1 * j2 * k1;
                        dx[28] = j3 * k1;
                        dx[29] = i2 * k2;
                        dx[30] = i1 * j1 * k2;
                        dx[31] = j2 * k2;
                        dx[32] = i1 * k3;
                        dx[33] = j1 * k3;
                        dx[34] = k4;

                        if (order > 4) {

                            i5 = i4 * i1;
                            j5 = j4 * j1;
                            k5 = k4 * k1;

                            dx[35] = i5;
                            dx[36] = i4 * j1;
                            dx[37] = i3 * j2;
                            dx[38] = i2 * j3;
                            dx[39] = i1 * j4;
                            dx[40] = j5;
                            dx[41] = i4 * k1;
                            dx[42] = i3 * j1 * k1;
                            dx[43] = i2 * j2 * k1;
                            dx[44] = i1 * j3 * k1;
                            dx[45] = j4 * k1;
                            dx[46] = i3 * k2;
                            dx[47] = i2 * j1 * k2;
                            dx[48] = i1 * j2 * k2;
                            dx[49] = j3 * k2;
                            dx[50] = i2 * k3;
                            dx[51] = i1 * j1 * k3;
                            dx[52] = j2 * k3;
                            dx[53] = i1 * k4;
                            dx[54] = j1 * k4;
                            dx[55] = k5;

                            if (order > 5) {

                                i6 = i5 * i1;
                                j6 = j5 * j1;
                                k6 = k5 * k1;

                                dx[56] = i6;
                                dx[57] = i5 * j1;
                                dx[58] = i4 * j2;
                                dx[59] = i3 * j3;
                                dx[60] = i2 * j4;
                                dx[61] = i1 * j5;
                                dx[62] = j6;
                                dx[63] = i5 * k1;
                                dx[64] = i4 * j1 * k1;
                                dx[65] = i3 * j2 * k1;
                                dx[66] = i2 * j3 * k1;
                                dx[67] = i1 * j4 * k1;
                                dx[68] = j5 * k1;
                                dx[69] = i4 * k2;
                                dx[70] = i3 * j1 * k2;
                                dx[71] = i2 * j2 * k2;
                                dx[72] = i1 * j3 * k2;
                                dx[73] = j4 * k2;
                                dx[74] = i3 * k3;
                                dx[75] = i2 * j1 * k3;
                                dx[76] = i1 * j2 * k3;
                                dx[77] = j3 * k3;
                                dx[78] = i2 * k4;
                                dx[79] = i1 * j1 * k4;
                                dx[80] = j2 * k4;
                                dx[81] = i1 * k5;
                                dx[82] = j1 * k5;
                                dx[83] = k6;

                                if (order > 6) {

                                    i7 = i6 * i1;
                                    j7 = j6 * j1;
                                    k7 = k6 * k1;

                                    dx[84] = i7;
                                    dx[85] = i6 * j1;
                                    dx[86] = i5 * j2;
                                    dx[87] = i4 * j3;
                                    dx[88] = i3 * j4;
                                    dx[89] = i2 * j5;
                                    dx[90] = i1 * j6;
                                    dx[91] = j7;
                                    dx[92] = i6 * k1;
                                    dx[93] = i5 * j1 * k1;
                                    dx[94] = i4 * j2 * k1;
                                    dx[95] = i3 * j3 * k1;
                                    dx[96] = i2 * j4 * k1;
                                    dx[97] = i1 * j5 * k1;
                                    dx[98] = j6 * k1;
                                    dx[99] = i5 * k2;
                                    dx[100] = i4 * j1 * k2;
                                    dx[101] = i3 * j2 * k2;
                                    dx[102] = i2 * j3 * k2;
                                    dx[103] = i1 * j4 * k2;
                                    dx[104] = j5 * k2;
                                    dx[105] = i4 * k3;
                                    dx[106] = i3 * j1 * k3;
                                    dx[107] = i2 * j2 * k3;
                                    dx[108] = i1 * j3 * k3;
                                    dx[109] = j4 * k3;
                                    dx[110] = i3 * k4;
                                    dx[111] = i2 * j1 * k4;
                                    dx[112] = i1 * j2 * k4;
                                    dx[113] = j3 * k4;
                                    dx[114] = i2 * k5;
                                    dx[115] = i1 * j1 * k5;
                                    dx[116] = j2 * k5;
                                    dx[117] = i1 * k6;
                                    dx[118] = j1 * k6;
                                    dx[119] = k7;

                                    if (order > 7) {

                                        i8 = i7 * i1;
                                        j8 = j7 * j1;
                                        k8 = k7 * k1;

                                        dx[120] = i8;
                                        dx[121] = i7 * j1;
                                        dx[122] = i6 * j2;
                                        dx[123] = i5 * j3;
                                        dx[124] = i4 * j4;
                                        dx[125] = i3 * j5;
                                        dx[126] = i2 * j6;
                                        dx[127] = i1 * j7;
                                        dx[128] = j8;
                                        dx[129] = i7 * k1;
                                        dx[130] = i6 * j1 * k1;
                                        dx[131] = i5 * j2 * k1;
                                        dx[132] = i4 * j3 * k1;
                                        dx[133] = i3 * j4 * k1;
                                        dx[134] = i2 * j5 * k1;
                                        dx[135] = i1 * j6 * k1;
                                        dx[136] = j7 * k1;
                                        dx[137] = i6 * k2;
                                        dx[138] = i5 * j1 * k2;
                                        dx[139] = i4 * j2 * k2;
                                        dx[140] = i3 * j3 * k2;
                                        dx[141] = i2 * j4 * k2;
                                        dx[142] = i1 * j5 * k2;
                                        dx[143] = j6 * k2;
                                        dx[144] = i5 * k3;
                                        dx[145] = i4 * j1 * k3;
                                        dx[146] = i3 * j2 * k3;
                                        dx[147] = i2 * j3 * k3;
                                        dx[148] = i1 * j4 * k3;
                                        dx[149] = j5 * k3;
                                        dx[150] = i4 * k4;
                                        dx[151] = i3 * j1 * k4;
                                        dx[152] = i2 * j2 * k4;
                                        dx[153] = i1 * j3 * k4;
                                        dx[154] = j4 * k4;
                                        dx[155] = i3 * k5;
                                        dx[156] = i2 * j1 * k5;
                                        dx[157] = i1 * j2 * k5;
                                        dx[158] = j3 * k5;
                                        dx[159] = i2 * k6;
                                        dx[160] = i1 * j1 * k6;
                                        dx[161] = j2 * k6;
                                        dx[162] = i1 * k7;
                                        dx[163] = j1 * k7;
                                        dx[164] = k8;

                                        if (order > 8) {

                                            i9 = i8 * i1;
                                            j9 = j8 * j1;
                                            k9 = k8 * k1;

                                            dx[165] = i9;
                                            dx[166] = i8 * j1;
                                            dx[167] = i7 * j2;
                                            dx[168] = i6 * j3;
                                            dx[169] = i5 * j4;
                                            dx[170] = i4 * j5;
                                            dx[171] = i3 * j6;
                                            dx[172] = i2 * j7;
                                            dx[173] = i1 * j8;
                                            dx[174] = j9;
                                            dx[175] = i8 * k1;
                                            dx[176] = i7 * j1 * k1;
                                            dx[177] = i6 * j2 * k1;
                                            dx[178] = i5 * j3 * k1;
                                            dx[179] = i4 * j4 * k1;
                                            dx[180] = i3 * j5 * k1;
                                            dx[181] = i2 * j6 * k1;
                                            dx[182] = i1 * j7 * k1;
                                            dx[183] = j8 * k1;
                                            dx[184] = i7 * k2;
                                            dx[185] = i6 * j1 * k2;
                                            dx[186] = i5 * j2 * k2;
                                            dx[187] = i4 * j3 * k2;
                                            dx[188] = i3 * j4 * k2;
                                            dx[189] = i2 * j5 * k2;
                                            dx[190] = i1 * j6 * k2;
                                            dx[191] = j7 * k2;
                                            dx[192] = i6 * k3;
                                            dx[193] = i5 * j1 * k3;
                                            dx[194] = i4 * j2 * k3;
                                            dx[195] = i3 * j3 * k3;
                                            dx[196] = i2 * j4 * k3;
                                            dx[197] = i1 * j5 * k3;
                                            dx[198] = j6 * k3;
                                            dx[199] = i5 * k4;
                                            dx[200] = i4 * j1 * k4;
                                            dx[201] = i3 * j2 * k4;
                                            dx[202] = i2 * j3 * k4;
                                            dx[203] = i1 * j4 * k4;
                                            dx[204] = j5 * k4;
                                            dx[205] = i4 * k5;
                                            dx[206] = i3 * j1 * k5;
                                            dx[207] = i2 * j2 * k5;
                                            dx[208] = i1 * j3 * k5;
                                            dx[209] = j4 * k5;
                                            dx[210] = i3 * k6;
                                            dx[211] = i2 * j1 * k6;
                                            dx[212] = i1 * j2 * k6;
                                            dx[213] = j3 * k6;
                                            dx[214] = i2 * k7;
                                            dx[215] = i1 * j1 * k7;
                                            dx[216] = j2 * k7;
                                            dx[217] = i1 * k8;
                                            dx[218] = j1 * k8;
                                            dx[219] = k9;

                                            if (order > 9) {

                                                i10 = i9 * i1;
                                                j10 = j9 * j1;
                                                k10 = k9 * k1;

                                                dx[220] = i10;
                                                dx[221] = i9 * j1;
                                                dx[222] = i8 * j2;
                                                dx[223] = i7 * j3;
                                                dx[224] = i6 * j4;
                                                dx[225] = i5 * j5;
                                                dx[226] = i4 * j6;
                                                dx[227] = i3 * j7;
                                                dx[228] = i2 * j8;
                                                dx[229] = i1 * j9;
                                                dx[230] = j10;
                                                dx[231] = i9 * k1;
                                                dx[232] = i8 * j1 * k1;
                                                dx[233] = i7 * j2 * k1;
                                                dx[234] = i6 * j3 * k1;
                                                dx[235] = i5 * j4 * k1;
                                                dx[236] = i4 * j5 * k1;
                                                dx[237] = i3 * j6 * k1;
                                                dx[238] = i2 * j7 * k1;
                                                dx[239] = i1 * j8 * k1;
                                                dx[240] = j9 * k1;
                                                dx[241] = i8 * k2;
                                                dx[242] = i7 * j1 * k2;
                                                dx[243] = i6 * j2 * k2;
                                                dx[244] = i5 * j3 * k2;
                                                dx[245] = i4 * j4 * k2;
                                                dx[246] = i3 * j5 * k2;
                                                dx[247] = i2 * j6 * k2;
                                                dx[248] = i1 * j7 * k2;
                                                dx[249] = j8 * k2;
                                                dx[250] = i7 * k3;
                                                dx[251] = i6 * j1 * k3;
                                                dx[252] = i5 * j2 * k3;
                                                dx[253] = i4 * j3 * k3;
                                                dx[254] = i3 * j4 * k3;
                                                dx[255] = i2 * j5 * k3;
                                                dx[256] = i1 * j6 * k3;
                                                dx[257] = j7 * k3;
                                                dx[258] = i6 * k4;
                                                dx[259] = i5 * j1 * k4;
                                                dx[260] = i4 * j2 * k4;
                                                dx[261] = i3 * j3 * k4;
                                                dx[262] = i2 * j4 * k4;
                                                dx[263] = i1 * j5 * k4;
                                                dx[264] = j6 * k4;
                                                dx[265] = i5 * k5;
                                                dx[266] = i4 * j1 * k5;
                                                dx[267] = i3 * j2 * k5;
                                                dx[268] = i2 * j3 * k5;
                                                dx[269] = i1 * j4 * k5;
                                                dx[270] = j5 * k5;
                                                dx[271] = i4 * k6;
                                                dx[272] = i3 * j1 * k6;
                                                dx[273] = i2 * j2 * k6;
                                                dx[274] = i1 * j3 * k6;
                                                dx[275] = j4 * k6;
                                                dx[276] = i3 * k7;
                                                dx[277] = i2 * j1 * k7;
                                                dx[278] = i1 * j2 * k7;
                                                dx[279] = j3 * k7;
                                                dx[280] = i2 * k8;
                                                dx[281] = i1 * j1 * k8;
                                                dx[282] = j2 * k8;
                                                dx[283] = i1 * k9;
                                                dx[284] = j1 * k9;
                                                dx[285] = k10;

                                                if (order > 10) {

                                                    i11 = i10 * i1;
                                                    j11 = j10 * j1;
                                                    k11 = k10 * k1;

                                                    dx[286] = i11;
                                                    dx[287] = i10 * j1;
                                                    dx[288] = i9 * j2;
                                                    dx[289] = i8 * j3;
                                                    dx[290] = i7 * j4;
                                                    dx[291] = i6 * j5;
                                                    dx[292] = i5 * j6;
                                                    dx[293] = i4 * j7;
                                                    dx[294] = i3 * j8;
                                                    dx[295] = i2 * j9;
                                                    dx[296] = i1 * j10;
                                                    dx[297] = j11;
                                                    dx[298] = i10 * k1;
                                                    dx[299] = i9 * j1 * k1;
                                                    dx[300] = i8 * j2 * k1;
                                                    dx[301] = i7 * j3 * k1;
                                                    dx[302] = i6 * j4 * k1;
                                                    dx[303] = i5 * j5 * k1;
                                                    dx[304] = i4 * j6 * k1;
                                                    dx[305] = i3 * j7 * k1;
                                                    dx[306] = i2 * j8 * k1;
                                                    dx[307] = i1 * j9 * k1;
                                                    dx[308] = j10 * k1;
                                                    dx[309] = i9 * k2;
                                                    dx[310] = i8 * j1 * k2;
                                                    dx[311] = i7 * j2 * k2;
                                                    dx[312] = i6 * j3 * k2;
                                                    dx[313] = i5 * j4 * k2;
                                                    dx[314] = i4 * j5 * k2;
                                                    dx[315] = i3 * j6 * k2;
                                                    dx[316] = i2 * j7 * k2;
                                                    dx[317] = i1 * j8 * k2;
                                                    dx[318] = j9 * k2;
                                                    dx[319] = i8 * k3;
                                                    dx[320] = i7 * j1 * k3;
                                                    dx[321] = i6 * j2 * k3;
                                                    dx[322] = i5 * j3 * k3;
                                                    dx[323] = i4 * j4 * k3;
                                                    dx[324] = i3 * j5 * k3;
                                                    dx[325] = i2 * j6 * k3;
                                                    dx[326] = i1 * j7 * k3;
                                                    dx[327] = j8 * k3;
                                                    dx[328] = i7 * k4;
                                                    dx[329] = i6 * j1 * k4;
                                                    dx[330] = i5 * j2 * k4;
                                                    dx[331] = i4 * j3 * k4;
                                                    dx[332] = i3 * j4 * k4;
                                                    dx[333] = i2 * j5 * k4;
                                                    dx[334] = i1 * j6 * k4;
                                                    dx[335] = j7 * k4;
                                                    dx[336] = i6 * k5;
                                                    dx[337] = i5 * j1 * k5;
                                                    dx[338] = i4 * j2 * k5;
                                                    dx[339] = i3 * j3 * k5;
                                                    dx[340] = i2 * j4 * k5;
                                                    dx[341] = i1 * j5 * k5;
                                                    dx[342] = j6 * k5;
                                                    dx[343] = i5 * k6;
                                                    dx[344] = i4 * j1 * k6;
                                                    dx[345] = i3 * j2 * k6;
                                                    dx[346] = i2 * j3 * k6;
                                                    dx[347] = i1 * j4 * k6;
                                                    dx[348] = j5 * k6;
                                                    dx[349] = i4 * k7;
                                                    dx[350] = i3 * j1 * k7;
                                                    dx[351] = i2 * j2 * k7;
                                                    dx[352] = i1 * j3 * k7;
                                                    dx[353] = j4 * k7;
                                                    dx[354] = i3 * k8;
                                                    dx[355] = i2 * j1 * k8;
                                                    dx[356] = i1 * j2 * k8;
                                                    dx[357] = j3 * k8;
                                                    dx[358] = i2 * k9;
                                                    dx[359] = i1 * j1 * k9;
                                                    dx[360] = j2 * k9;
                                                    dx[361] = i1 * k10;
                                                    dx[362] = j1 * k10;
                                                    dx[363] = k11;

                                                    if (order > 11) {

                                                        i12 = i11 * i1;
                                                        j12 = j11 * j1;
                                                        k12 = k11 * k1;

                                                        dx[364] = i12;
                                                        dx[365] = i11 * j1;
                                                        dx[366] = i10 * j2;
                                                        dx[367] = i9 * j3;
                                                        dx[368] = i8 * j4;
                                                        dx[369] = i7 * j5;
                                                        dx[370] = i6 * j6;
                                                        dx[371] = i5 * j7;
                                                        dx[372] = i4 * j8;
                                                        dx[373] = i3 * j9;
                                                        dx[374] = i2 * j10;
                                                        dx[375] = i1 * j11;
                                                        dx[376] = j12;
                                                        dx[377] = i11 * k1;
                                                        dx[378] = i10 * j1 * k1;
                                                        dx[379] = i9 * j2 * k1;
                                                        dx[380] = i8 * j3 * k1;
                                                        dx[381] = i7 * j4 * k1;
                                                        dx[382] = i6 * j5 * k1;
                                                        dx[383] = i5 * j6 * k1;
                                                        dx[384] = i4 * j7 * k1;
                                                        dx[385] = i3 * j8 * k1;
                                                        dx[386] = i2 * j9 * k1;
                                                        dx[387] = i1 * j10 * k1;
                                                        dx[388] = j11 * k1;
                                                        dx[389] = i10 * k2;
                                                        dx[390] = i9 * j1 * k2;
                                                        dx[391] = i8 * j2 * k2;
                                                        dx[392] = i7 * j3 * k2;
                                                        dx[393] = i6 * j4 * k2;
                                                        dx[394] = i5 * j5 * k2;
                                                        dx[395] = i4 * j6 * k2;
                                                        dx[396] = i3 * j7 * k2;
                                                        dx[397] = i2 * j8 * k2;
                                                        dx[398] = i1 * j9 * k2;
                                                        dx[399] = j10 * k2;
                                                        dx[400] = i9 * k3;
                                                        dx[401] = i8 * j1 * k3;
                                                        dx[402] = i7 * j2 * k3;
                                                        dx[403] = i6 * j3 * k3;
                                                        dx[404] = i5 * j4 * k3;
                                                        dx[405] = i4 * j5 * k3;
                                                        dx[406] = i3 * j6 * k3;
                                                        dx[407] = i2 * j7 * k3;
                                                        dx[408] = i1 * j8 * k3;
                                                        dx[409] = j9 * k3;
                                                        dx[410] = i8 * k4;
                                                        dx[411] = i7 * j1 * k4;
                                                        dx[412] = i6 * j2 * k4;
                                                        dx[413] = i5 * j3 * k4;
                                                        dx[414] = i4 * j4 * k4;
                                                        dx[415] = i3 * j5 * k4;
                                                        dx[416] = i2 * j6 * k4;
                                                        dx[417] = i1 * j7 * k4;
                                                        dx[418] = j8 * k4;
                                                        dx[419] = i7 * k5;
                                                        dx[420] = i6 * j1 * k5;
                                                        dx[421] = i5 * j2 * k5;
                                                        dx[422] = i4 * j3 * k5;
                                                        dx[423] = i3 * j4 * k5;
                                                        dx[424] = i2 * j5 * k5;
                                                        dx[425] = i1 * j6 * k5;
                                                        dx[426] = j7 * k5;
                                                        dx[427] = i6 * k6;
                                                        dx[428] = i5 * j1 * k6;
                                                        dx[429] = i4 * j2 * k6;
                                                        dx[430] = i3 * j3 * k6;
                                                        dx[431] = i2 * j4 * k6;
                                                        dx[432] = i1 * j5 * k6;
                                                        dx[433] = j6 * k6;
                                                        dx[434] = i5 * k7;
                                                        dx[435] = i4 * j1 * k7;
                                                        dx[436] = i3 * j2 * k7;
                                                        dx[437] = i2 * j3 * k7;
                                                        dx[438] = i1 * j4 * k7;
                                                        dx[439] = j5 * k7;
                                                        dx[440] = i4 * k8;
                                                        dx[441] = i3 * j1 * k8;
                                                        dx[442] = i2 * j2 * k8;
                                                        dx[443] = i1 * j3 * k8;
                                                        dx[444] = j4 * k8;
                                                        dx[445] = i3 * k9;
                                                        dx[446] = i2 * j1 * k9;
                                                        dx[447] = i1 * j2 * k9;
                                                        dx[448] = j3 * k9;
                                                        dx[449] = i2 * k10;
                                                        dx[450] = i1 * j1 * k10;
                                                        dx[451] = j2 * k10;
                                                        dx[452] = i1 * k11;
                                                        dx[453] = j1 * k11;
                                                        dx[454] = k12;
                                                    } /*End if(order>11)*/
                                                } /*End if(order>10)*/
                                            } /*End if(order>9)*/
                                        } /*End if(order>8)*/
                                    } /*End if(order>7)*/
                                } /*End if(order>6)*/
                            } /*End if(order>5)*/
                        } /*End if(order>4)*/
                    } /*End if(order>3)*/
                } /*End if(order>2)*/
            } /*End if(order>1)*/
        } /*End if(order>0)*/
    }


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

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            reslice parameters
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2Mask    DOCUMENT ME!
     * @param   x_dim1        x dimension of reslice file
     * @param   y_dim1        y dimension of reslice file
     * @param   threshold2    DOCUMENT ME!
     * @param   pixel5        pixel values of standard file
     * @param   pixel5Mask    DOCUMENT ME!
     * @param   x_dim2        x dimension of standard file
     * @param   y_dim2        y dimension of standard file
     * @param   z_dim2        z dimension of standard file
     * @param   threshold5    DOCUMENT ME!
     * @param   dcf           first derivatives of sd with respect to parameters
     * @param   ecf           second derivatives of sd with respect to parameters
     * @param   dx            storage for second partial derivatives of ratio with respect to parameters
     * @param   scale         DOCUMENT ME!
     *
     * @return  ratio the sum of squares
     *
     *          <p>This routine computes the first and second derivatives of the normalized cost function with respect
     *          to all external parameters.</p>
     *
     *          <p>The matrix indices have been adjusted to allow for a more orderly relationship between the matrix and
     *          its elements</p>
     *
     *          <p>Note that derivatives with respect to external parameters are ignored</p>
     */

    private double qvderiv2Dwarp(int parameters, double[][] es, int sampleFactor, float[] pixel2, boolean[] pixel2Mask,
                                 int x_dim1, int y_dim1, float threshold2, float[] pixel5, boolean[] pixel5Mask,
                                 int x_dim2, int y_dim2, int z_dim2, float threshold5, double[] dcf, double[][] ecf,
                                 double[] dx, double scale) {
        int counttotal = 0;
        double cf = 0.0; /* intermediate value */

        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double spix3;
        double pix4; /*calculated value of current pixel in standard file*/
        int slice1;
        int slice2;
        int remainder2;
        int rTerm;
        double i1;
        double j1;
        int n0p, n1p, n2p, n3p;

        int i, j, k;
        int ii;
        int s, t;
        int x_max1, y_max1;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        int order;
        int coeffp;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4;

        order = 12;
        coeffp = 91;

        if (parameters < 182) {
            order = 11;
            coeffp = 78;
        }

        if (parameters < 156) {
            order = 10;
            coeffp = 66;
        }

        if (parameters < 132) {
            order = 9;
            coeffp = 55;
        }

        if (parameters < 110) {
            order = 8;
            coeffp = 45;
        }

        if (parameters < 90) {
            order = 7;
            coeffp = 36;
        }

        if (parameters < 72) {
            order = 6;
            coeffp = 28;
        }

        if (parameters < 56) {
            order = 5;
            coeffp = 21;
        }

        if (parameters < 42) {
            order = 4;
            coeffp = 15;
        }

        if (parameters < 30) {
            order = 3;
            coeffp = 10;
        }

        if (parameters < 20) {
            order = 2;
            coeffp = 6;
        }

        if (parameters < 12) {
            order = 1;
            coeffp = 3;
        }

        for (t = 0; t < coeffp; t++) {
            dx[t] = 0;
        }

        for (t = 0; t <= (2 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        slice1 = x_dim1 * y_dim1;

        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            if (!pixel5Mask[r]) {
                continue;
            }

            /*We don't yet verify that pixel5>threshold to allow for less biased function*/

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            i1 = (double) i;
            j1 = (double) j;

            /*Calculate coordinates (x_i,y_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            dx2DSet(dx, order, i1, j1);

            /* Find the x and y coordinates */

            x_i = 0.0;
            y_i = 0.0;

            for (ii = 0; ii < coeffp; ii++) {
                x_i += es[0][ii] * dx[ii];
                y_i += es[1][ii] * dx[ii];
            }

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

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

            n0p = (k * slice1) + (y_down * x_dim1) + x_down;
            n0 = pixel2[n0p];
            n1p = (k * slice1) + (y_down * x_dim1) + x_up;
            n1 = pixel2[n1p];
            n2p = (k * slice1) + (y_up * x_dim1) + x_down;
            n2 = pixel2[n2p];
            n3p = (k * slice1) + (y_up * x_dim1) + x_up;
            n3 = pixel2[n3p];

            /*Verify that something is above threshold here*/
            if ((pix3 <= threshold5) && ((n0 <= threshold2) || (!pixel2Mask[n0p])) &&
                    ((n1 <= threshold2) || (!pixel2Mask[n1p])) && ((n2 <= threshold2) || (!pixel2Mask[n2p])) &&
                    ((n3 <= threshold2) || (!pixel2Mask[n3p]))) {
                continue;
            }

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * spix3) + (spix3 * spix3);

            /*Calculate derivatives that are nonzero*/

            for (t = 0; t < coeffp; t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dxpix4 * dx[t];
            }

            for (; t < (2 * coeffp); t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dypix4 * dx[t - coeffp];
            }

            dcf[t] += -2.0 * pix3 * (pix4 - spix3);

            for (t = 0; t < coeffp; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += 2.0 * dxpix4 * dxpix4 * dx[t] * dx[s];
                }
            }

            for (; t < (2 * coeffp); t++) {

                for (s = 0; s < coeffp; s++) {
                    ecf[t][s] += 2.0 * dx[t - coeffp] * dx[s] * (dxpix4 * dypix4);
                }

                for (; s <= t; s++) {
                    ecf[t][s] += 2.0 * dypix4 * dypix4 * dx[t - coeffp] * dx[s - coeffp];
                }
            }

            for (s = 0; s < coeffp; s++) {
                ecf[t][s] += -2.0 * pix3 * dxpix4 * dx[s];
            }

            for (; s < (2 * coeffp); s++) {
                ecf[t][s] += -2.0 * pix3 * dypix4 * dx[s - coeffp];
            }

            ecf[t][s] += 2.0 * pix3 * pix3;

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t <= (2 * coeffp); t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        return cf;

    }

    /* From matertial Copyright 1995-2001 Roger P. Woods, M.D. */

    /* Modified 5/16/01 */

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            reslice parameters
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2Mask    DOCUMENT ME!
     * @param   x_dim1        x dimension of reslice file
     * @param   y_dim1        y dimension of reslice file
     * @param   z_dim1        z dimension of reslice file
     * @param   threshold2    DOCUMENT ME!
     * @param   pixel5        pixel values of standard file
     * @param   pixel5Mask    DOCUMENT ME!
     * @param   x_dim2        x dimension of standard file
     * @param   y_dim2        y dimension of standard file
     * @param   z_dim2        z dimension of standard file
     * @param   threshold5    DOCUMENT ME!
     * @param   dcf           first derivatives of sd with respect to parameters
     * @param   ecf           second derivatives of sd with respect to parameters
     * @param   dx            storage for second partial derivatives of ratio with respect to parameters
     * @param   scale         DOCUMENT ME!
     *
     * @return  ratio the sum of squares
     *
     *          <p>This routine computes the first and second derivatives of the normalized cost function with respect
     *          to all external parameters.</p>
     *
     *          <p>The matrix indices have been adjusted to allow for a more orderly relationship between the matrix and
     *          its elements</p>
     *
     *          <p>Note that derivatives with respect to external parameters are ignored</p>
     */

    private double qvderiv3Dwarp(int parameters, double[][] es, int sampleFactor, float[] pixel2, boolean[] pixel2Mask,
                                 int x_dim1, int y_dim1, int z_dim1, float threshold2, float[] pixel5,
                                 boolean[] pixel5Mask, int x_dim2, int y_dim2, int z_dim2, float threshold5,
                                 double[] dcf, double[][] ecf, double[] dx, double scale) {

        int counttotal = 0;
        double cf = 0.0;

        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double spix3;
        double pix4; /*calculated value of current pixel in standard file*/
        int slice1;
        int slice2;
        int remainder2;
        int rTerm;
        double i1;
        double j1;
        double k1;
        int n0p, n1p, n2p, n3p, n4p, n5p, n6p, n7p;

        int i, j, k;
        int ii;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        int order;
        int coeffp;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4;

        order = 12;
        coeffp = 455;

        if (parameters < 1365) {
            order = 11;
            coeffp = 364;
        }

        if (parameters < 1092) {
            order = 10;
            coeffp = 286;
        }

        if (parameters < 858) {
            order = 9;
            coeffp = 220;
        }

        if (parameters < 660) {
            order = 8;
            coeffp = 165;
        }

        if (parameters < 495) {
            order = 7;
            coeffp = 120;
        }

        if (parameters < 360) {
            order = 6;
            coeffp = 84;
        }

        if (parameters < 252) {
            order = 5;
            coeffp = 56;
        }

        if (parameters < 168) {
            order = 4;
            coeffp = 35;
        }

        if (parameters < 105) {
            order = 3;
            coeffp = 20;
        }

        if (parameters < 60) {
            order = 2;
            coeffp = 10;
        }

        if (parameters < 30) {
            order = 1;
            coeffp = 4;
        }

        for (t = 0; t < coeffp; t++) {
            dx[t] = 0;
        }

        for (t = 0; t <= (3 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;
        slice1 = x_dim1 * y_dim1;

        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            if (!pixel5Mask[r]) {
                continue;
            }

            /*We don't yet verify that pixel5>threshold to allow for less biased function*/

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            i1 = (double) i;
            j1 = (double) j;
            k1 = (double) k;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            dx3DSet(dx, order, i1, j1, k1);

            /* Find the x, y , and z coordinates */
            x_i = 0.0;
            y_i = 0.0;
            z_i = 0.0;

            for (ii = 0; ii < coeffp; ii++) {
                x_i += es[0][ii] * dx[ii];
                y_i += es[1][ii] * dx[ii];
                z_i += es[2][ii] * dx[ii];
            }

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

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

            n0p = (z_down * slice1) + (y_down * x_dim1) + x_down;
            n0 = pixel2[n0p];
            n1p = (z_down * slice1) + (y_down * x_dim1) + x_up;
            n1 = pixel2[n1p];
            n2p = (z_down * slice1) + (y_up * x_dim1) + x_down;
            n2 = pixel2[n2p];
            n3p = (z_down * slice1) + (y_up * x_dim1) + x_up;
            n3 = pixel2[n3p];
            n4p = (z_up * slice1) + (y_down * x_dim1) + x_down;
            n4 = pixel2[n4p];
            n5p = (z_up * slice1) + (y_down * x_dim1) + x_up;
            n5 = pixel2[n5p];
            n6p = (z_up * slice1) + (y_up * x_dim1) + x_down;
            n6 = pixel2[n6p];
            n7p = (z_up * slice1) + (y_up * x_dim1) + x_up;
            n7 = pixel2[n7p];

            /*Verify that something is above threshold here*/
            if ((pix3 <= threshold5) && ((n0 <= threshold2) || (!pixel2Mask[n0p])) &&
                    ((n1 <= threshold2) || (!pixel2Mask[n1p])) && ((n2 <= threshold2) || (!pixel2Mask[n2p])) &&
                    ((n3 <= threshold2) || (!pixel2Mask[n3p])) && ((n4 <= threshold2) || (!pixel2Mask[n4p])) &&
                    ((n5 <= threshold2) || (!pixel2Mask[n5p])) && ((n6 <= threshold2) || (!pixel2Mask[n6p])) &&
                    ((n7 <= threshold2) || (!pixel2Mask[n7p]))) {
                continue;
            }

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

            for (t = 0; t < coeffp; t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dxpix4 * dx[t];
            }

            for (; t < (2 * coeffp); t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dypix4 * dx[t - coeffp];
            }

            for (; t < (3 * coeffp); t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dzpix4 * dx[t - (2 * coeffp)];
            }

            dcf[t] += -2.0 * pix3 * (pix4 - spix3);

            for (t = 0; t < coeffp; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += 2.0 * dxpix4 * dxpix4 * dx[t] * dx[s];
                }
            }

            for (; t < (2 * coeffp); t++) {

                for (s = 0; s < coeffp; s++) {
                    ecf[t][s] += 2.0 * dx[t - coeffp] * dx[s] * (dxpix4 * dypix4);
                }

                for (; s <= t; s++) {
                    ecf[t][s] += 2.0 * dypix4 * dypix4 * dx[t - coeffp] * dx[s - coeffp];
                }
            }

            for (; t < (3 * coeffp); t++) {

                for (s = 0; s < coeffp; s++) {
                    ecf[t][s] += 2.0 * dx[t - (2 * coeffp)] * dx[s] * (dxpix4 * dzpix4);
                }

                for (; s < (2 * coeffp); s++) {
                    ecf[t][s] += 2.0 * dx[t - (2 * coeffp)] * dx[s - coeffp] * (dypix4 * dzpix4);
                }

                for (; s <= t; s++) {
                    ecf[t][s] += 2.0 * dzpix4 * dzpix4 * dx[t - (2 * coeffp)] * dx[s - (2 * coeffp)];
                }
            }

            for (s = 0; s < coeffp; s++) {
                ecf[t][s] += -2.0 * pix3 * dxpix4 * dx[s];
            }

            for (; s < (2 * coeffp); s++) {
                ecf[t][s] += -2.0 * pix3 * dypix4 * dx[s - coeffp];
            }

            for (; s < (3 * coeffp); s++) {
                ecf[t][s] += -2.0 * pix3 * dzpix4 * dx[s - (2 * coeffp)];
            }

            ecf[t][s] += 2.0 * pix3 * pix3;

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t <= (3 * coeffp); t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        return cf;
    }

    /* From material Copyright 1995-2001 Roger P. Woods, M.D. */

    /* Modified 5/27/01 */

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              reslice parameters
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2Mask      DOCUMENT ME!
     * @param   x_dim1          x dimension of reslice file
     * @param   y_dim1          y dimension of reslice file
     * @param   threshold2      DOCUMENT ME!
     * @param   pixel5          pixel values of standard file
     * @param   pixel5Mask      DOCUMENT ME!
     * @param   x_dim2          x dimension of standard file
     * @param   y_dim2          y dimension of standard file
     * @param   z_dim2          z dimension of standard file
     * @param   threshold5      DOCUMENT ME!
     * @param   dcf             first derivatives of sd with respect to parameters
     * @param   ecf             second derivatives of sd with respect to parameters
     * @param   dx              storage for second partial derivatives of ratio with respect to parameters
     * @param   scale           DOCUMENT ME!
     * @param   count           DOCUMENT ME!
     * @param   mean            DOCUMENT ME!
     * @param   square          DOCUMENT ME!
     * @param   dmean           DOCUMENT ME!
     * @param   dsquare         DOCUMENT ME!
     * @param   esquare         DOCUMENT ME!
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  DOCUMENT ME!
     * @param   minActualValue  DOCUMENT ME!
     *
     * @return  the correlation ratio
     *
     *          <p>This routine computes the first and second derivatives of the correlation ratio with respect to all
     *          external parameters.</p>
     *
     *          <p>The matrix indices have been adjusted to allow for a more orderly relationship between the matrix and
     *          its elements</p>
     *
     *          <p>Note that derivatives with respect to external parameters are ignored</p>
     */


    private double qvderivCOR2Dwarp(int parameters, double[][] es, int sampleFactor, float[] pixel2,
                                    boolean[] pixel2Mask, int x_dim1, int y_dim1, float threshold2, float[] pixel5,
                                    boolean[] pixel5Mask, int x_dim2, int y_dim2, int z_dim2, float threshold5,
                                    double[] dcf, double[][] ecf, double[] dx, double scale, int[] count, double[] mean,
                                    double[] square, double[][] dmean, double[][] dsquare, double[][][] esquare,
                                    int partitions, double maxActualValue, double minActualValue) {

        int counttotal = 0;
        double cf = 0.0; /* intermediate value */

        double a, b, d, e; /*interpolation weighting factors*/

        int pix1; /* int vale of current pixel in target file */
        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        int slice1;
        int slice2;
        int remainder2;
        int rTerm;
        double i1;
        double j1;
        int n0p, n1p, n2p, n3p;

        int i, j, k;
        int ii;
        int jj;
        int s, t;
        int x_max1, y_max1;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        int order;
        int coeffp;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4, ezpix4;
        float pix4Min;
        double value; /* intermediate value */
        double[] dvalue; /* partial derivatives of value with respect to parameters */
        double[] dsds;
        double[][] esds;
        double sd2;
        double[] dsd2;
        double[][] esd2;
        double evaluets;
        double meantotal;
        int counts;
        double means;
        double sds;

        order = 12;
        coeffp = 91;

        if (parameters < 182) {
            order = 11;
            coeffp = 78;
        }

        if (parameters < 156) {
            order = 10;
            coeffp = 66;
        }

        if (parameters < 132) {
            order = 9;
            coeffp = 55;
        }

        if (parameters < 110) {
            order = 8;
            coeffp = 45;
        }

        if (parameters < 90) {
            order = 7;
            coeffp = 36;
        }

        if (parameters < 72) {
            order = 6;
            coeffp = 28;
        }

        if (parameters < 56) {
            order = 5;
            coeffp = 21;
        }

        if (parameters < 42) {
            order = 4;
            coeffp = 15;
        }

        if (parameters < 30) {
            order = 3;
            coeffp = 10;
        }

        if (parameters < 20) {
            order = 2;
            coeffp = 6;
        }

        if (parameters < 12) {
            order = 1;
            coeffp = 3;
        }

        dvalue = new double[2 * coeffp];
        dsds = new double[2 * coeffp];
        esds = new double[2 * coeffp][2 * coeffp];
        dsd2 = new double[2 * coeffp];
        esd2 = new double[2 * coeffp][2 * coeffp];

        for (t = 0; t < coeffp; t++) {
            dx[t] = 0;
        }

        for (t = 0; t < (2 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < (2 * coeffp); t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        slice1 = x_dim1 * y_dim1;

        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

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

            if (!pixel5Mask[r]) {
                continue;
            }

            /*We don't yet verify that pixel5>threshold to allow for less biased function*/

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            i1 = (double) i;
            j1 = (double) j;

            /*Calculate coordinates (x_i,y_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            dx2DSet(dx, order, i1, j1);

            /* Find the x and y coordinates */

            x_i = 0.0;
            y_i = 0.0;

            for (ii = 0; ii < coeffp; ii++) {
                x_i += es[0][ii] * dx[ii];
                y_i += es[1][ii] * dx[ii];
            }

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

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

            n0p = (k * slice1) + (y_down * x_dim1) + x_down;
            n0 = pixel2[n0p];
            n1p = (k * slice1) + (y_down * x_dim1) + x_up;
            n1 = pixel2[n1p];
            n2p = (k * slice1) + (y_up * x_dim1) + x_down;
            n2 = pixel2[n2p];
            n3p = (k * slice1) + (y_up * x_dim1) + x_up;
            n3 = pixel2[n3p];

            /*Verify that something is above threshold here*/
            if ((pix3 <= threshold5) && ((n0 <= threshold2) || (!pixel2Mask[n0p])) &&
                    ((n1 <= threshold2) || (!pixel2Mask[n1p])) && ((n2 <= threshold2) || (!pixel2Mask[n2p])) &&
                    ((n3 <= threshold2) || (!pixel2Mask[n3p]))) {
                continue;
            }
            // if (pix3 <= threshold5) continue;

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - minActualValue) / (maxActualValue - minActualValue));
            // pix1=(int)((partitions-1)*(pix3-threshold5)/(maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            ezpix4 = (n3 + n0 - n2 - n1);

            /*Calculate the value of the reslice pixel above the minimum */
            value = pix4 - pix4Min;
            mean[pix1] += value;
            square[pix1] += value * value;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */

            for (t = 0; t < coeffp; t++) {
                dvalue[t] = dxpix4 * dx[t];
                dmean[pix1][t] += dvalue[t];
                dsquare[pix1][t] += 2 * value * dvalue[t];
                dvalue[t + coeffp] = dypix4 * dx[t];
                dmean[pix1][t + coeffp] += dvalue[t + coeffp];
                dsquare[pix1][t + coeffp] += 2 * value * dvalue[t + coeffp];
            }

            /* Second derivatives */
            for (t = 0; t < (2 * coeffp); t++) {

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

        for (t = 0; t < (2 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s < (2 * coeffp); s++) {
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

            for (t = 0; t < (2 * coeffp); t++) {
                dmean[jj][t] /= counts;
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0) {
                continue;
            }

            for (t = 0; t < (2 * coeffp); t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] - (2 * counts * (dmean[jj][s] * dmean[jj][t]))) / (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < (2 * coeffp); t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < (2 * coeffp); t++) {

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

            for (t = 0; t < (2 * coeffp); t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < (2 * coeffp); t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        return cf;
    }

    /* From material Copyright 1995-2001 Roger P. Woods, M.D. */

    /* Modified 5/16/01 */

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              reslice parameters
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2Mask      DOCUMENT ME!
     * @param   x_dim1          x dimension of reslice file
     * @param   y_dim1          y dimension of reslice file
     * @param   z_dim1          z dimension of reslice file
     * @param   threshold2      DOCUMENT ME!
     * @param   pixel5          pixel values of standard file
     * @param   pixel5Mask      DOCUMENT ME!
     * @param   x_dim2          x dimension of standard file
     * @param   y_dim2          y dimension of standard file
     * @param   z_dim2          z dimension of standard file
     * @param   threshold5      DOCUMENT ME!
     * @param   dcf             first derivatives of sd with respect to parameters
     * @param   ecf             second derivatives of sd with respect to parameters
     * @param   dx              storage for second partial derivatives of ratio with respect to parameters
     * @param   scale           DOCUMENT ME!
     * @param   count           DOCUMENT ME!
     * @param   mean            DOCUMENT ME!
     * @param   square          DOCUMENT ME!
     * @param   dmean           DOCUMENT ME!
     * @param   dsquare         DOCUMENT ME!
     * @param   esquare         DOCUMENT ME!
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  DOCUMENT ME!
     * @param   minActualValue  DOCUMENT ME!
     *
     * @return  ratio the correlation ratio
     *
     *          <p>This routine computes the first and second derivatives of the correlation ratio with respect to all
     *          external parameters.</p>
     *
     *          <p>The matrix indices have been adjusted to allow for a more orderly relationship between the matrix and
     *          its elements</p>
     *
     *          <p>Note that derivatives with respect to external parameters are ignored</p>
     */

    private double qvderivCOR3Dwarp(int parameters, double[][] es, int sampleFactor, float[] pixel2,
                                    boolean[] pixel2Mask, int x_dim1, int y_dim1, int z_dim1, float threshold2,
                                    float[] pixel5, boolean[] pixel5Mask, int x_dim2, int y_dim2, int z_dim2,
                                    float threshold5, double[] dcf, double[][] ecf, double[] dx, double scale,
                                    int[] count, double[] mean, double[] square, double[][] dmean, double[][] dsquare,
                                    double[][][] esquare, int partitions, double maxActualValue,
                                    double minActualValue) {

        int counttotal = 0;
        double cf = 0.0;

        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double spix3;
        double pix4; /*calculated value of current pixel in standard file*/

        int slice1;
        int slice2;
        int remainder2;
        int rTerm;
        double i1;
        double j1;
        double k1;
        int n0p, n1p, n2p, n3p, n4p, n5p, n6p, n7p;

        int i, j, k;
        int ii;
        int jj;
        int pix1; /* int value of current pixel in standard file */
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        int order;
        int coeffp;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4, expix4, eypix4, ezpix4;
        float pix4Min;
        double value; /* intermediate value */
        double[] dvalue; /* partial derivatives of value with respect to parameters */
        double[] dsds;
        double[][] esds;
        double sd2;
        double[] dsd2;
        double[][] esd2;
        double evaluets;
        double meantotal;
        int counts;
        double means;
        double sds;

        order = 12;
        coeffp = 455;

        if (parameters < 1365) {
            order = 11;
            coeffp = 364;
        }

        if (parameters < 1092) {
            order = 10;
            coeffp = 286;
        }

        if (parameters < 858) {
            order = 9;
            coeffp = 220;
        }

        if (parameters < 660) {
            order = 8;
            coeffp = 165;
        }

        if (parameters < 495) {
            order = 7;
            coeffp = 120;
        }

        if (parameters < 360) {
            order = 6;
            coeffp = 84;
        }

        if (parameters < 252) {
            order = 5;
            coeffp = 56;
        }

        if (parameters < 168) {
            order = 4;
            coeffp = 35;
        }

        if (parameters < 105) {
            order = 3;
            coeffp = 20;
        }

        if (parameters < 60) {
            order = 2;
            coeffp = 10;
        }

        if (parameters < 30) {
            order = 1;
            coeffp = 4;
        }

        dvalue = new double[3 * coeffp];
        dsds = new double[3 * coeffp];
        esds = new double[3 * coeffp][3 * coeffp];
        dsd2 = new double[3 * coeffp];
        esd2 = new double[3 * coeffp][3 * coeffp];

        for (t = 0; t < coeffp; t++) {
            dx[t] = 0;
        }

        for (t = 0; t < (3 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < (3 * coeffp); t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;
        slice1 = x_dim1 * y_dim1;

        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

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

            if (!pixel5Mask[r]) {
                continue;
            }

            /*We don't yet verify that pixel5>threshold to allow for less biased function*/

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            i1 = (double) i;
            j1 = (double) j;
            k1 = (double) k;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            dx3DSet(dx, order, i1, j1, k1);

            /* Find the x, y , and z coordinates */
            x_i = 0.0;
            y_i = 0.0;
            z_i = 0.0;

            for (ii = 0; ii < coeffp; ii++) {
                x_i += es[0][ii] * dx[ii];
                y_i += es[1][ii] * dx[ii];
                z_i += es[2][ii] * dx[ii];
            }

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

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

            n0p = (z_down * slice1) + (y_down * x_dim1) + x_down;
            n0 = pixel2[n0p];
            n1p = (z_down * slice1) + (y_down * x_dim1) + x_up;
            n1 = pixel2[n1p];
            n2p = (z_down * slice1) + (y_up * x_dim1) + x_down;
            n2 = pixel2[n2p];
            n3p = (z_down * slice1) + (y_up * x_dim1) + x_up;
            n3 = pixel2[n3p];
            n4p = (z_up * slice1) + (y_down * x_dim1) + x_down;
            n4 = pixel2[n4p];
            n5p = (z_up * slice1) + (y_down * x_dim1) + x_up;
            n5 = pixel2[n5p];
            n6p = (z_up * slice1) + (y_up * x_dim1) + x_down;
            n6 = pixel2[n6p];
            n7p = (z_up * slice1) + (y_up * x_dim1) + x_up;
            n7 = pixel2[n7p];

            /*Verify that something is above threshold here*/
            if ((pix3 <= threshold5) && ((n0 <= threshold2) || (!pixel2Mask[n0p])) &&
                    ((n1 <= threshold2) || (!pixel2Mask[n1p])) && ((n2 <= threshold2) || (!pixel2Mask[n2p])) &&
                    ((n3 <= threshold2) || (!pixel2Mask[n3p])) && ((n4 <= threshold2) || (!pixel2Mask[n4p])) &&
                    ((n5 <= threshold2) || (!pixel2Mask[n5p])) && ((n6 <= threshold2) || (!pixel2Mask[n6p])) &&
                    ((n7 <= threshold2) || (!pixel2Mask[n7p]))) {
                continue;
            }

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - minActualValue) / (maxActualValue - minActualValue));

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

            for (t = 0; t < coeffp; t++) {
                dvalue[t] = dxpix4 * dx[t];
                dmean[pix1][t] += dvalue[t];
                dsquare[pix1][t] += 2 * value * dvalue[t];
                dvalue[t + coeffp] = dypix4 * dx[t];
                dmean[pix1][t + coeffp] += dvalue[t + coeffp];
                dsquare[pix1][t + coeffp] += 2 * value * dvalue[t + coeffp];
                dvalue[t + (2 * coeffp)] = dzpix4 * dx[t];
                dmean[pix1][t + (2 * coeffp)] += dvalue[t + (2 * coeffp)];
                dsquare[pix1][t + (2 * coeffp)] += 2 * value * dvalue[t + (2 * coeffp)];
            }

            /* Second derivatives */

            for (t = 0; t < (3 * coeffp); t++) {

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

        for (t = 0; t < (3 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s < (3 * coeffp); s++) {
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

            for (t = 0; t < (3 * coeffp); t++) {
                dmean[jj][t] /= counts;
            }

            sds = square[jj] - ((means * means) * counts);
            sds /= (counts - 1);

            /*Avoid subsequent NaN errors*/
            if (sds == 0.0) {
                continue;
            }

            for (t = 0; t < (3 * coeffp); t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] - (2 * counts * (dmean[jj][s] * dmean[jj][t]))) / (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < (3 * coeffp); t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < (3 * coeffp); t++) {

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

            for (t = 0; t < (3 * coeffp); t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < (3 * coeffp); t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        return cf;
    }

    /* From material Copyright 1995-2001 Roger P. Woods, M.D. */

    /* Modified 5/16/01 */

    /**
     * DOCUMENT ME!
     *
     * @param   volume         source array to be resliced
     * @param   x_dim1         x dimension of source array
     * @param   y_dim1         y dimension of source array
     * @param   minLimit       DOCUMENT ME!
     * @param   maxLimit       DOCUMENT ME!
     * @param   x_dim2         x dimension of output array
     * @param   y_dim2         y dimension of output array
     * @param   z_dim2         z dimension of output array
     * @param   es             rescaling parameters
     * @param   order          DOCUMENT ME!
     * @param   interpolation  DOCUMENT ME!
     *
     * @return  resliced data array if successful, null if unsuccessful
     *
     *          <p>This routine will reslice a file based on the unit vectors es Trilinear interpolation used</p>
     */


    private float[] rWarp2D(float[] volume, int x_dim1, int y_dim1, float minLimit, float maxLimit, int x_dim2,
                            int y_dim2, int z_dim2, double[][] es, int order, int interpolation) {
        float[] new_volume;
        double x_max1;
        double y_max1;
        int coeffp;
        double[] dx;
        int slice2;
        int i, j, k;
        int i3, j3, k3;
        double i1;
        double j1;
        double x_p, y_p;
        int ii;
        int slice1;
        int x_up, x_down, y_up, y_down;
        double a, b, d, e;
        double total;
        int i2, j2, k2;
        int i4, j4;
        double sincx, sincy;

        // x_max1 = x_dim1 - 1.0;
        // y_max1 = y_dim1 - 1.0;
        x_max1 = x_dim1 - 0.5;
        y_max1 = y_dim1 - 0.5;
        slice1 = x_dim1 * y_dim1;
        slice2 = x_dim2 * y_dim2;

        try {
            new_volume = new float[x_dim2 * y_dim2 * z_dim2];
        } catch (OutOfMemoryError er) {
            MipavUtil.displayError("unable to allocate memory to reslice file\n");

            return null;
        }

        for (i = 0; i < new_volume.length; i++) {
            new_volume[i] = 0.0f;
        }

        coeffp = (order + 1) * (order + 2) / 2;

        try {
            dx = new double[coeffp];
        } catch (OutOfMemoryError er) {
            MipavUtil.displayError("Unable to allocate memory to dx in rWarp2D\n");

            return null;
        }

        /* Reslice */
        for (k = 0, k3 = 0; k < z_dim2; k++, k3 += slice2) {

            for (j = 0, j3 = k3; j < y_dim2; j++, j3 += x_dim2) {
                j1 = (double) j;

                for (i = 0, i3 = j3; i < x_dim2; i++, i3++) {
                    i1 = (double) i;
                    dx2DSet(dx, order, i1, j1);

                    /* Find the x and y coordinates */
                    x_p = 0.0;
                    y_p = 0.0;

                    for (ii = 0; ii < coeffp; ii++) {
                        x_p += es[0][ii] * dx[ii];
                        y_p += es[1][ii] * dx[ii];
                    }

                    if ((x_p < -0.5) || (x_p >= x_max1)) {
                        continue;
                    }

                    if ((y_p < -0.5) || (y_p >= y_max1)) {
                        continue;
                    }

                    if (interpolation == AlgorithmTransform.BILINEAR) {
                        x_up = (int) Math.ceil(x_p);
                        x_down = (int) Math.floor(x_p);
                        y_up = (int) Math.ceil(y_p);
                        y_down = (int) Math.floor(y_p);

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
                            a = x_p - x_down;
                            d = x_up - x_p;
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
                            b = y_p - y_down;
                            e = y_up - y_p;
                        }

                        /* Interpolate */

                        k2 = k * slice1;
                        j2 = k2 + (y_down * x_dim1);
                        i2 = j2 + x_down;
                        total = 0.0;

                        if ((i2 >= 0) && (i2 < slice1)) {
                            total = volume[i2] * d * e;
                        }

                        if (x_up != x_down) {
                            i2++;

                            if ((i2 >= 0) && (i2 < slice1)) {
                                total += volume[i2] * a * e;
                            }
                        }

                        if (y_up != y_down) {
                            j2 += x_dim1;
                            i2 = j2 + x_down;

                            if ((i2 >= 0) && (i2 < slice1)) {
                                total += volume[i2] * d * b;
                            }

                            if (x_up != x_down) {
                                i2++;

                                if ((i2 >= 0) && (i2 < slice1)) {
                                    total += volume[i2] * a * b;
                                }
                            }
                        }

                        new_volume[i3] = (float) total;
                    } // if (interpolation == AlgorithmTransform.BILINEAR)
                    else if (interpolation == AlgorithmTransform.NEAREST_NEIGHBOR) {

                        /* Find nearest neighbor value */

                        x_up = (int) Math.floor(x_p + .5);
                        y_up = (int) Math.floor(y_p + .5);
                        new_volume[i3] = volume[(k * slice1) + (y_up * x_dim1) + x_up];

                    } // else if (interpolation == AlgorithmTransform.NEAREST_NEIGHBOR)
                    else if (interpolation == AlgorithmTransform.WSINC) {
                        int xkern = 3;
                        int ykern = 3;

                        x_down = (int) Math.floor(x_p);

                        if (x_down >= xkern) {
                            x_down -= (xkern - 1);
                        } else {
                            x_down = 0;
                        }

                        x_up = (int) Math.ceil(x_p) + xkern;

                        if (x_dim1 < x_up) {
                            x_up = x_dim1;
                        }

                        y_down = (int) Math.floor(y_p);

                        if (y_down >= ykern) {
                            y_down -= (ykern - 1);
                        } else {
                            y_down = 0;
                        }

                        y_up = (int) Math.ceil(y_p) + ykern;

                        if (y_dim1 < y_up) {
                            y_up = y_dim1;
                        }

                        /* Find interpolated value */
                        total = 0.0;
                        k2 = k * slice1;
                        j2 = k2 + (y_down * x_dim1);
                        j4 = y_down;

                        for (; j4 < y_up; j4++, j2 += x_dim1) {
                            sincy = wsinc((y_p - j4), ykern);
                            i2 = j2 + x_down;
                            i4 = x_down;

                            for (; i4 < x_up; i4++, i2++) {
                                sincx = wsinc((x_p - i4), xkern);
                                total += volume[i2] * sincx * sincy;
                            }
                        }

                        new_volume[i3] = (float) total;

                        if (clipOutput) {

                            if (new_volume[i3] < minLimit) {
                                new_volume[i3] = minLimit;
                            }

                            if (new_volume[i3] > maxLimit) {
                                new_volume[i3] = maxLimit;
                            }
                        } // if (clipOutput)
                    } // else if (interpolation == AlgorithmTransform.WSINC)
                } // for (i=0,i3=j3p;i<x_dim2;i++,i3p++)
            } // for (j=0,j3p=k3p;j<y_dim2;j++,j3p+=x_dim2)
        } // for (k=0,k3p=0;k<z_dim2;k++,k3p+=slice2)

        dx = null;

        return new_volume;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   volume         source array to be resliced
     * @param   x_dim1         x dimension of source array
     * @param   y_dim1         y dimension of source array
     * @param   minLimit       DOCUMENT ME!
     * @param   maxLimit       DOCUMENT ME!
     * @param   x_dim2         x dimension of output array
     * @param   y_dim2         y dimension of output array
     * @param   z_dim2         z dimension of output array
     * @param   es             rescaling parameters
     * @param   order          DOCUMENT ME!
     * @param   interpolation  DOCUMENT ME!
     *
     * @return  resliced data array if successful, null if unsuccessful
     *
     *          <p>This routine will reslice a color image file based on the unit vectors es Trilinear interpolation
     *          used</p>
     */


    private float[] rWarp2DC(float[] volume, int x_dim1, int y_dim1, float minLimit, float maxLimit, int x_dim2,
                             int y_dim2, int z_dim2, double[][] es, int order, int interpolation) {
        float[] new_volume;
        double x_max1;
        double y_max1;
        int coeffp;
        double[] dx;
        int slice2;
        int i, j, k;
        int i3, j3, k3;
        double i1;
        double j1;
        double x_p, y_p;
        int ii;
        int slice1;
        int x_up, x_down, y_up, y_down;
        double a, b, d, e;
        int i2, j2, k2;
        int i4, j4;
        double sincx, sincy;
        int c;
        double totalR, totalG, totalB;

        // x_max1 = x_dim1 - 1.0;
        // y_max1 = y_dim1 - 1.0;
        x_max1 = x_dim1 - 0.5;
        y_max1 = y_dim1 - 0.5;
        slice1 = x_dim1 * y_dim1;
        slice2 = x_dim2 * y_dim2;

        try {
            new_volume = new float[4 * x_dim2 * y_dim2 * z_dim2];
        } catch (OutOfMemoryError er) {
            MipavUtil.displayError("unable to allocate memory to reslice file\n");

            return null;
        }

        for (i = 0; i < new_volume.length; i++) {
            new_volume[i] = 0.0f;
        }

        coeffp = (order + 1) * (order + 2) / 2;

        try {
            dx = new double[coeffp];
        } catch (OutOfMemoryError er) {
            MipavUtil.displayError("Unable to allocate memory to dx in rWarp2D\n");

            return null;
        }

        /* Reslice */
        for (k = 0, k3 = 0; k < z_dim2; k++, k3 += slice2) {

            for (j = 0, j3 = k3; j < y_dim2; j++, j3 += x_dim2) {
                j1 = (double) j;

                for (i = 0, i3 = j3; i < x_dim2; i++, i3++) {
                    i1 = (double) i;
                    dx2DSet(dx, order, i1, j1);

                    /* Find the x and y coordinates */
                    x_p = 0.0;
                    y_p = 0.0;

                    for (ii = 0; ii < coeffp; ii++) {
                        x_p += es[0][ii] * dx[ii];
                        y_p += es[1][ii] * dx[ii];
                    }

                    if ((x_p < -0.5) || (x_p >= x_max1)) {
                        continue;
                    }

                    if ((y_p < -0.5) || (y_p >= y_max1)) {
                        continue;
                    }

                    if (interpolation == AlgorithmTransform.BILINEAR) {
                        x_up = (int) Math.ceil(x_p);
                        x_down = (int) Math.floor(x_p);
                        y_up = (int) Math.ceil(y_p);
                        y_down = (int) Math.floor(y_p);

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
                            a = x_p - x_down;
                            d = x_up - x_p;
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
                            b = y_p - y_down;
                            e = y_up - y_p;
                        }

                        /* Interpolate */

                        k2 = k * slice1;
                        j2 = k2 + (y_down * x_dim1);
                        i2 = j2 + x_down;
                        totalR = 0.0;
                        totalG = 0.0;
                        totalB = 0.0;

                        if ((i2 >= 0) && (i2 < slice1)) {
                            totalR = volume[(4 * i2) + 1] * d * e;
                            totalG = volume[(4 * i2) + 2] * d * e;
                            totalB = volume[(4 * i2) + 3] * d * e;
                        }

                        if (x_up != x_down) {
                            i2++;

                            if ((i2 >= 0) && (i2 < slice1)) {
                                totalR += volume[(4 * i2) + 1] * a * e;
                                totalG += volume[(4 * i2) + 2] * a * e;
                                totalB += volume[(4 * i2) + 3] * a * e;
                            }
                        }

                        if (y_up != y_down) {
                            j2 += x_dim1;
                            i2 = j2 + x_down;

                            if ((i2 >= 0) && (i2 < slice1)) {
                                totalR += volume[(4 * i2) + 1] * d * b;
                                totalG += volume[(4 * i2) + 2] * d * b;
                                totalB += volume[(4 * i2) + 3] * d * b;
                            }

                            if (x_up != x_down) {
                                i2++;

                                if ((i2 >= 0) && (i2 < slice1)) {
                                    totalR += volume[(4 * i2) + 1] * a * b;
                                    totalG += volume[(4 * i2) + 2] * a * b;
                                    totalB += volume[(4 * i2) + 3] * a * b;
                                }
                            }
                        }

                        new_volume[(4 * i3) + 1] = (float) totalR;
                        new_volume[(4 * i3) + 2] = (float) totalG;
                        new_volume[(4 * i3) + 3] = (float) totalB;
                    } // if (interpolation == AlgorithmTransform.BILINEAR)
                    else if (interpolation == AlgorithmTransform.NEAREST_NEIGHBOR) {

                        /* Find nearest neighbor value */

                        x_up = (int) Math.floor(x_p + .5);
                        y_up = (int) Math.floor(y_p + .5);

                        for (c = 1; c <= 3; c++) {
                            new_volume[(4 * i3) + c] = volume[(4 * ((k * slice1) + (y_up * x_dim1) + x_up)) + c];
                        }

                    } // else if (interpolation == AlgorithmTransform.NEAREST_NEIGHBOR)
                    else if (interpolation == AlgorithmTransform.WSINC) {
                        int xkern = 3;
                        int ykern = 3;

                        x_down = (int) Math.floor(x_p);

                        if (x_down >= xkern) {
                            x_down -= (xkern - 1);
                        } else {
                            x_down = 0;
                        }

                        x_up = (int) Math.ceil(x_p) + xkern;

                        if (x_dim1 < x_up) {
                            x_up = x_dim1;
                        }

                        y_down = (int) Math.floor(y_p);

                        if (y_down >= ykern) {
                            y_down -= (ykern - 1);
                        } else {
                            y_down = 0;
                        }

                        y_up = (int) Math.ceil(y_p) + ykern;

                        if (y_dim1 < y_up) {
                            y_up = y_dim1;
                        }

                        /* Find interpolated value */
                        totalR = 0.0;
                        totalG = 0.0;
                        totalB = 0.0;
                        k2 = k * slice1;
                        j2 = k2 + (y_down * x_dim1);
                        j4 = y_down;

                        for (; j4 < y_up; j4++, j2 += x_dim1) {
                            sincy = wsinc((y_p - j4), ykern);
                            i2 = j2 + x_down;
                            i4 = x_down;

                            for (; i4 < x_up; i4++, i2++) {
                                sincx = wsinc((x_p - i4), xkern);
                                totalR += volume[(4 * i2) + 1] * sincx * sincy;
                                totalG += volume[(4 * i2) + 2] * sincx * sincy;
                                totalB += volume[(4 * i2) + 3] * sincx * sincy;
                            }
                        }

                        new_volume[(4 * i3) + 1] = (float) totalR;
                        new_volume[(4 * i3) + 2] = (float) totalG;
                        new_volume[(4 * i3) + 3] = (float) totalB;

                        if (clipOutput) {

                            if (new_volume[(4 * i3) + 1] < minLimit) {
                                new_volume[(4 * i3) + 1] = minLimit;
                            }

                            if (new_volume[(4 * i3) + 1] > maxLimit) {
                                new_volume[(4 * i3) + 1] = maxLimit;
                            }

                            if (new_volume[(4 * i3) + 2] < minLimit) {
                                new_volume[(4 * i3) + 2] = minLimit;
                            }

                            if (new_volume[(4 * i3) + 2] > maxLimit) {
                                new_volume[(4 * i3) + 2] = maxLimit;
                            }

                            if (new_volume[(4 * i3) + 3] < minLimit) {
                                new_volume[(4 * i3) + 3] = minLimit;
                            }

                            if (new_volume[(4 * i3) + 3] > maxLimit) {
                                new_volume[(4 * i3) + 3] = maxLimit;
                            }
                        } // if (clipOutput)
                    } // else if (interpolation == AlgorithmTransform.WSINC)
                } // for (i=0,i3=j3p;i<x_dim2;i++,i3p++)
            } // for (j=0,j3p=k3p;j<y_dim2;j++,j3p+=x_dim2)
        } // for (k=0,k3p=0;k<z_dim2;k++,k3p+=slice2)

        dx = null;

        return new_volume;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   volume         source array to be resliced
     * @param   x_dim1         x dimension of source array
     * @param   y_dim1         y dimension of source array
     * @param   z_dim1         z dimension of source array
     * @param   minLimit       DOCUMENT ME!
     * @param   maxLimit       DOCUMENT ME!
     * @param   x_dim2         x dimension of output array
     * @param   y_dim2         y dimension of output array
     * @param   z_dim2         z dimension of output array
     * @param   es             rescaling parameters
     * @param   order          DOCUMENT ME!
     * @param   interpolation  DOCUMENT ME!
     *
     * @return  resliced data array if successful, null if unsuccessful
     *
     *          <p>This routine will reslice a file based on the unit vectors es Trilinear interpolation used</p>
     */


    private float[] rWarp3D(float[] volume, int x_dim1, int y_dim1, int z_dim1, float minLimit, float maxLimit,
                            int x_dim2, int y_dim2, int z_dim2, double[][] es, int order, int interpolation) {
        float[] new_volume;
        double x_max1;
        double y_max1;
        double z_max1;
        int coeffp;
        double[] dx;
        int slice2;
        int i, j, k;
        int i3, j3, k3;
        double i1;
        double j1;
        double k1;
        double x_p, y_p, z_p;
        int ii;
        int slice1;
        int vol1;
        int x_up, x_down, y_up, y_down, z_up, z_down;
        double a, b, c, d, e, f;
        double total;
        int i2, j2, k2;
        int i4, j4, k4;
        double sincz, sincyz, sincxyz;

        // x_max1 = x_dim1 - 1.0;
        // y_max1 = y_dim1 - 1.0;
        // z_max1 = z_dim1 - 1.0;
        x_max1 = x_dim1 - 0.5;
        y_max1 = y_dim1 - 0.5;
        z_max1 = z_dim1 - 0.5;
        slice1 = x_dim1 * y_dim1;
        vol1 = slice1 * z_dim1;
        slice2 = x_dim2 * y_dim2;

        try {
            new_volume = new float[x_dim2 * y_dim2 * z_dim2];
        } catch (OutOfMemoryError er) {
            MipavUtil.displayError("unable to allocate memory to reslice file\n");

            return null;
        }

        for (i = 0; i < new_volume.length; i++) {
            new_volume[i] = 0.0f;
        }

        coeffp = (order + 1) * (order + 2) * (order + 3) / 6;

        try {
            dx = new double[coeffp];
        } catch (OutOfMemoryError er) {
            MipavUtil.displayError("Unable to allocate memory to dx in rWarp3D\n");

            return null;
        }

        /* Reslice */
        for (k = 0, k3 = 0; k < z_dim2; k++, k3 += slice2) {
            k1 = (double) k;

            for (j = 0, j3 = k3; j < y_dim2; j++, j3 += x_dim2) {
                j1 = (double) j;

                for (i = 0, i3 = j3; i < x_dim2; i++, i3++) {
                    i1 = (double) i;
                    dx3DSet(dx, order, i1, j1, k1);

                    /* Find the x, y, and z coordinates */
                    x_p = 0.0;
                    y_p = 0.0;
                    z_p = 0.0;

                    for (ii = 0; ii < coeffp; ii++) {
                        x_p += es[0][ii] * dx[ii];
                        y_p += es[1][ii] * dx[ii];
                        z_p += es[2][ii] * dx[ii];
                    }

                    if ((x_p < -0.5) || (x_p >= x_max1)) {
                        continue;
                    }

                    if ((y_p < -0.5) || (y_p >= y_max1)) {
                        continue;
                    }

                    if ((z_p < -0.5) || (z_p >= z_max1)) {
                        continue;
                    }

                    if (interpolation == AlgorithmTransform.TRILINEAR) {
                        x_up = (int) Math.ceil(x_p);
                        x_down = (int) Math.floor(x_p);
                        y_up = (int) Math.ceil(y_p);
                        y_down = (int) Math.floor(y_p);
                        z_up = (int) Math.ceil(z_p);
                        z_down = (int) Math.floor(z_p);

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
                            a = x_p - x_down;
                            d = x_up - x_p;
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
                            b = y_p - y_down;
                            e = y_up - y_p;
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
                            c = z_p - z_down;
                            f = z_up - z_p;
                        }

                        k2 = z_down * slice1;
                        j2 = k2 + (y_down * x_dim1);
                        i2 = j2 + x_down;
                        total = 0.0f;

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
                    } // if (interpolation == AlgorithmTransform.TRILINEAR)
                    else if (interpolation == AlgorithmTransform.NEAREST_NEIGHBOR) {

                        /* Find nearest neighbor value */

                        x_up = (int) Math.floor(x_p + .5);
                        y_up = (int) Math.floor(y_p + .5);
                        z_up = (int) Math.floor(z_p + .5);
                        new_volume[i3] = volume[(z_up * slice1) + (y_up * x_dim1) + x_up];
                    } // else if (interpolation == AlgorithmTransform.NEAREST_NEIGHBOR)
                    else if (interpolation == AlgorithmTransform.WSINC) {
                        int xkern = 3;
                        int ykern = 3;
                        int zkern = 3;
                        x_down = (int) Math.floor(x_p);

                        if (x_down >= xkern) {
                            x_down -= (xkern - 1);
                        } else {
                            x_down = 0;
                        }

                        x_up = (int) Math.ceil(x_p) + xkern;

                        if (x_dim1 < x_up) {
                            x_up = x_dim1;
                        }

                        y_down = (int) Math.floor(y_p);

                        if (y_down >= ykern) {
                            y_down -= (ykern - 1);
                        } else {
                            y_down = 0;
                        }

                        y_up = (int) Math.ceil(y_p) + ykern;

                        if (y_dim1 < y_up) {
                            y_up = y_dim1;
                        }

                        z_down = (int) Math.floor(z_p);

                        if (z_down >= zkern) {
                            z_down -= (zkern - 1);
                        } else {
                            z_down = 0;
                        }

                        z_up = (int) Math.ceil(z_p) + zkern;

                        if (z_dim1 < z_up) {
                            z_up = z_dim1;
                        }

                        /* Find interpolated value */
                        total = 0.0;

                        k2 = z_down * slice1;
                        k4 = z_down;

                        for (; k4 < z_up; k4++, k2 += slice1) {

                            sincz = wsinc((z_p - k4), zkern);

                            j2 = k2 + (y_down * x_dim1);
                            j4 = y_down;

                            for (; j4 < y_up; j4++, j2 += x_dim1) {

                                sincyz = wsinc((y_p - j4), ykern) * sincz;

                                i2 = j2 + x_down;
                                i4 = x_down;

                                for (; i4 < x_up; i4++, i2++) {

                                    sincxyz = wsinc((x_p - i4), xkern) * sincyz;

                                    total += volume[i2] * sincxyz;
                                }
                            }
                        }

                        new_volume[i3] = (float) total;

                        if (clipOutput) {

                            if (new_volume[i3] < minLimit) {
                                new_volume[i3] = minLimit;
                            }

                            if (new_volume[i3] > maxLimit) {
                                new_volume[i3] = maxLimit;
                            }
                        } // if (clipOutput)
                    } // else if (interpolation == AlgorithmTransform.WSINC)
                } // for (i=0,i3=j3;i<x_dim2;i++,i3++)
            } // for (j=0,j3=k3;j<y_dim2;j++,j3+=x_dim2)
        } // for (k=0,k3=0;k<z_dim2;k++,k3+=slice2)

        return new_volume;
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

    /* From material Copyright 1995-2001 Roger P. Woods, M.D. */

    /* Modified 5/10/01 */

    /* With special thanks to Kate Fissell for identifying a bug in the original*/

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            reslice parameters
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2Mask    DOCUMENT ME!
     * @param   x_dim1        x dimension of reslice file
     * @param   y_dim1        y dimension of reslice file
     * @param   threshold2    DOCUMENT ME!
     * @param   pixel5        pixel values of standard file
     * @param   pixel5Mask    DOCUMENT ME!
     * @param   x_dim2        x dimension of standard file
     * @param   y_dim2        y dimension of standard file
     * @param   z_dim2        z dimension of standard file
     * @param   threshold5    DOCUMENT ME!
     * @param   dcf           first derivatives of sd with respect to parameters
     * @param   ecf           second derivatives of sd with respect to parameters
     * @param   dx            storage for second partial derivatives of ratio with respect to parameters
     * @param   scale         DOCUMENT ME!
     *
     * @return  ratio the sum of squares
     *
     *          <p>This routine computes the first and second derivatives of the normalized const function with respect
     *          to all external parameters.</p>
     *
     *          <p>The matrix indices have been adjusted to allow for a more orderly relationship between the matrix and
     *          its elements</p>
     *
     *          <p>Note that derivatives with respect to external parameters are ignored</p>
     */


    private double uvderiv2Dwarp(int parameters, double[][] es, int sampleFactor, float[] pixel2, boolean[] pixel2Mask,
                                 int x_dim1, int y_dim1, float threshold2, float[] pixel5, boolean[] pixel5Mask,
                                 int x_dim2, int y_dim2, int z_dim2, float threshold5, double[] dcf, double[][] ecf,
                                 double[] dx, double scale) {

        int counttotal = 0;
        double cf = 0.0; /* intermediate value */

        double a, b, d, e; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double spix3;
        double pix4; /*calculated value of current pixel in standard file*/
        int slice1;
        int slice2;
        int remainder2;
        int rTerm;
        double i1;
        double j1;
        int n0p, n1p, n2p, n3p;

        int i, j, k;
        int ii;
        int s, t;
        int x_max1, y_max1;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        int order;
        int coeffp;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4, ezpix4;

        order = 12;
        coeffp = 91;

        if (parameters < 182) {
            order = 11;
            coeffp = 78;
        }

        if (parameters < 156) {
            order = 10;
            coeffp = 66;
        }

        if (parameters < 132) {
            order = 9;
            coeffp = 55;
        }

        if (parameters < 110) {
            order = 8;
            coeffp = 45;
        }

        if (parameters < 90) {
            order = 7;
            coeffp = 36;
        }

        if (parameters < 72) {
            order = 6;
            coeffp = 28;
        }

        if (parameters < 56) {
            order = 5;
            coeffp = 21;
        }

        if (parameters < 42) {
            order = 4;
            coeffp = 15;
        }

        if (parameters < 30) {
            order = 3;
            coeffp = 10;
        }

        if (parameters < 20) {
            order = 2;
            coeffp = 6;
        }

        if (parameters < 12) {
            order = 1;
            coeffp = 3;
        }

        for (t = 0; t < coeffp; t++) {
            dx[t] = 0;
        }

        for (t = 0; t <= (2 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        slice1 = x_dim1 * y_dim1;

        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            if (!pixel5Mask[r]) {
                continue;
            }

            /*We don't yet verify that pixel5>threshold to allow for less biased function*/

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            i1 = (double) i;
            j1 = (double) j;

            /*Calculate coordinates (x_i,y_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            dx2DSet(dx, order, i1, j1);

            /* Find the x and y coordinates */

            x_i = 0.0;
            y_i = 0.0;

            for (ii = 0; ii < coeffp; ii++) {
                x_i += es[0][ii] * dx[ii];
                y_i += es[1][ii] * dx[ii];
            }

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

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

            n0p = (k * slice1) + (y_down * x_dim1) + x_down;
            n0 = pixel2[n0p];
            n1p = (k * slice1) + (y_down * x_dim1) + x_up;
            n1 = pixel2[n1p];
            n2p = (k * slice1) + (y_up * x_dim1) + x_down;
            n2 = pixel2[n2p];
            n3p = (k * slice1) + (y_up * x_dim1) + x_up;
            n3 = pixel2[n3p];

            /*Verify that something is above threshold here*/
            if ((pix3 <= threshold5) && ((n0 <= threshold2) || (!pixel2Mask[n0p])) &&
                    ((n1 <= threshold2) || (!pixel2Mask[n1p])) && ((n2 <= threshold2) || (!pixel2Mask[n2p])) &&
                    ((n3 <= threshold2) || (!pixel2Mask[n3p]))) {
                continue;
            }

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            ezpix4 = (n3 + n0 - n2 - n1);

            /*Calculate the square of the difference*/
            cf += (pix4 * pix4) - (2.0 * pix4 * spix3) + (spix3 * spix3);

            /*Calculate derivatives that are nonzero*/

            for (t = 0; t < coeffp; t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dxpix4 * dx[t];
            }

            for (; t < (2 * coeffp); t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dypix4 * dx[t - coeffp];
            }

            dcf[t] += -2.0 * pix3 * (pix4 - spix3);

            for (t = 0; t < coeffp; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += 2.0 * dxpix4 * dxpix4 * dx[t] * dx[s];
                }
            }

            for (; t < (2 * coeffp); t++) {

                for (s = 0; s < coeffp; s++) {
                    ecf[t][s] += 2.0 * dx[t - coeffp] * dx[s] * ((dxpix4 * dypix4) + ((pix4 - spix3) * ezpix4));
                }

                for (; s <= t; s++) {
                    ecf[t][s] += 2.0 * dypix4 * dypix4 * dx[t - coeffp] * dx[s - coeffp];
                }
            }

            for (s = 0; s < coeffp; s++) {
                ecf[t][s] += -2.0 * pix3 * dxpix4 * dx[s];
            }

            for (; s < (2 * coeffp); s++) {
                ecf[t][s] += -2.0 * pix3 * dypix4 * dx[s - coeffp];
            }

            ecf[t][s] += 2.0 * pix3 * pix3;

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t <= (2 * coeffp); t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters    number of formal parameters in chosen model
     * @param   es            reslice parameters
     * @param   sampleFactor  current sampling factor
     * @param   pixel2        original pixel values of file to reslice
     * @param   pixel2Mask    DOCUMENT ME!
     * @param   x_dim1        x dimension of reslice file
     * @param   y_dim1        y dimension of reslice file
     * @param   z_dim1        z dimension of reslice file
     * @param   threshold2    DOCUMENT ME!
     * @param   pixel5        pixel values of standard file
     * @param   pixel5Mask    DOCUMENT ME!
     * @param   x_dim2        x dimension of standard file
     * @param   y_dim2        y dimension of standard file
     * @param   z_dim2        z dimension of standard file
     * @param   threshold5    DOCUMENT ME!
     * @param   dcf           first derivatives of sd with respect to parameters
     * @param   ecf           second derivatives of sd with respect to parameters
     * @param   dx            storage for second partial derivatives of ratio with respect to parameters
     * @param   scale         DOCUMENT ME!
     *
     * @return  ratio the sum of squares
     *
     *          <p>This routine computes the first and second derivatives of the normalized cost function with respect
     *          to all external parameters.</p>
     *
     *          <p>The matrix indices have been adjusted to allow for a more orderly relationship between the matrix and
     *          its elements</p>
     *
     *          <p>Note that derivatives with respect to external parameters are ignored</p>
     */

    private double uvderiv3Dwarp(int parameters, double[][] es, int sampleFactor, float[] pixel2, boolean[] pixel2Mask,
                                 int x_dim1, int y_dim1, int z_dim1, float threshold2, float[] pixel5,
                                 boolean[] pixel5Mask, int x_dim2, int y_dim2, int z_dim2, float threshold5,
                                 double[] dcf, double[][] ecf, double[] dx, double scale) {

        int counttotal = 0;
        double cf = 0.0;

        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double spix3;
        double pix4; /*calculated value of current pixel in standard file*/

        int slice1;
        int slice2;
        int remainder2;
        int rTerm;
        double i1;
        double j1;
        double k1;
        int n0p, n1p, n2p, n3p, n4p, n5p, n6p, n7p;

        int i, j, k;
        int ii;
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        int order;
        int coeffp;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4, expix4, eypix4, ezpix4;

        order = 12;
        coeffp = 455;

        if (parameters < 1365) {
            order = 11;
            coeffp = 364;
        }

        if (parameters < 1092) {
            order = 10;
            coeffp = 286;
        }

        if (parameters < 858) {
            order = 9;
            coeffp = 220;
        }

        if (parameters < 660) {
            order = 8;
            coeffp = 165;
        }

        if (parameters < 495) {
            order = 7;
            coeffp = 120;
        }

        if (parameters < 360) {
            order = 6;
            coeffp = 84;
        }

        if (parameters < 252) {
            order = 5;
            coeffp = 56;
        }

        if (parameters < 168) {
            order = 4;
            coeffp = 35;
        }

        if (parameters < 105) {
            order = 3;
            coeffp = 20;
        }

        if (parameters < 60) {
            order = 2;
            coeffp = 10;
        }

        if (parameters < 30) {
            order = 1;
            coeffp = 4;
        }

        for (t = 0; t < coeffp; t++) {
            dx[t] = 0;
        }

        for (t = 0; t <= (3 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;
        slice1 = x_dim1 * y_dim1;

        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

        /*Examine pixels of standard file at samplefactor interval*/

        /*Note that it is assumed here that pixel5[z_dim][y_dim][x_dim] refers to the*/

        /* same pixel as *(pixel5[0][0]+z_dim*y_dim*x_dim), i.e. that all the pixels */

        /* are represented in a contiguous block of memory--see the routine*/

        /* "create_volume.c" for an illustration of how this is assured*/

        /*ARRAY STRUCTURE ASSUMPTIONS MADE HERE*/

        for (r = 0; r < rTerm; r += sampleFactor) {

            if (!pixel5Mask[r]) {
                continue;
            }

            /*We don't yet verify that pixel5>threshold to allow for less biased function*/

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            i1 = (double) i;
            j1 = (double) j;
            k1 = (double) k;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            dx3DSet(dx, order, i1, j1, k1);

            /* Find the x, y , and z coordinates */
            x_i = 0.0;
            y_i = 0.0;
            z_i = 0.0;

            for (ii = 0; ii < coeffp; ii++) {
                x_i += es[0][ii] * dx[ii];
                y_i += es[1][ii] * dx[ii];
                z_i += es[2][ii] * dx[ii];
            }

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

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

            n0p = (z_down * slice1) + (y_down * x_dim1) + x_down;
            n0 = pixel2[n0p];
            n1p = (z_down * slice1) + (y_down * x_dim1) + x_up;
            n1 = pixel2[n1p];
            n2p = (z_down * slice1) + (y_up * x_dim1) + x_down;
            n2 = pixel2[n2p];
            n3p = (z_down * slice1) + (y_up * x_dim1) + x_up;
            n3 = pixel2[n3p];
            n4p = (z_up * slice1) + (y_down * x_dim1) + x_down;
            n4 = pixel2[n4p];
            n5p = (z_up * slice1) + (y_down * x_dim1) + x_up;
            n5 = pixel2[n5p];
            n6p = (z_up * slice1) + (y_up * x_dim1) + x_down;
            n6 = pixel2[n6p];
            n7p = (z_up * slice1) + (y_up * x_dim1) + x_up;
            n7 = pixel2[n7p];

            /*Verify that something is above threshold here*/
            if ((pix3 <= threshold5) && ((n0 <= threshold2) || (!pixel2Mask[n0p])) &&
                    ((n1 <= threshold2) || (!pixel2Mask[n1p])) && ((n2 <= threshold2) || (!pixel2Mask[n2p])) &&
                    ((n3 <= threshold2) || (!pixel2Mask[n3p])) && ((n4 <= threshold2) || (!pixel2Mask[n4p])) &&
                    ((n5 <= threshold2) || (!pixel2Mask[n5p])) && ((n6 <= threshold2) || (!pixel2Mask[n6p])) &&
                    ((n7 <= threshold2) || (!pixel2Mask[n7p]))) {
                continue;
            }

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

            for (t = 0; t < coeffp; t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dxpix4 * dx[t];
            }

            for (; t < (2 * coeffp); t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dypix4 * dx[t - coeffp];
            }

            for (; t < (3 * coeffp); t++) {
                dcf[t] += 2.0 * (pix4 - spix3) * dzpix4 * dx[t - (2 * coeffp)];
            }

            dcf[t] += -2.0 * pix3 * (pix4 - spix3);

            for (t = 0; t < coeffp; t++) {

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += 2.0 * dxpix4 * dxpix4 * dx[t] * dx[s];
                }
            }

            for (; t < (2 * coeffp); t++) {

                for (s = 0; s < coeffp; s++) {
                    ecf[t][s] += 2.0 * dx[t - coeffp] * dx[s] * ((dxpix4 * dypix4) + ((pix4 - spix3) * ezpix4));
                }

                for (; s <= t; s++) {
                    ecf[t][s] += 2.0 * dypix4 * dypix4 * dx[t - coeffp] * dx[s - coeffp];
                }
            }

            for (; t < (3 * coeffp); t++) {

                for (s = 0; s < coeffp; s++) {
                    ecf[t][s] += 2.0 * dx[t - (2 * coeffp)] * dx[s] * ((dxpix4 * dzpix4) + ((pix4 - spix3) * eypix4));
                }

                for (; s < (2 * coeffp); s++) {
                    ecf[t][s] += 2.0 * dx[t - (2 * coeffp)] * dx[s - coeffp] *
                                     ((dypix4 * dzpix4) + ((pix4 - spix3) * expix4));
                }

                for (; s <= t; s++) {
                    ecf[t][s] += 2.0 * dzpix4 * dzpix4 * dx[t - (2 * coeffp)] * dx[s - (2 * coeffp)];
                }
            }

            for (s = 0; s < coeffp; s++) {
                ecf[t][s] += -2.0 * pix3 * dxpix4 * dx[s];
            }

            for (; s < (2 * coeffp); s++) {
                ecf[t][s] += -2.0 * pix3 * dypix4 * dx[s - coeffp];
            }

            for (; s < (3 * coeffp); s++) {
                ecf[t][s] += -2.0 * pix3 * dzpix4 * dx[s - (2 * coeffp)];
            }

            ecf[t][s] += 2.0 * pix3 * pix3;

            counttotal++;
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t <= (3 * coeffp); t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              reslice parameters
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2Mask      DOCUMENT ME!
     * @param   x_dim1          x dimension of reslice file
     * @param   y_dim1          y dimension of reslice file
     * @param   threshold2      DOCUMENT ME!
     * @param   pixel5          pixel values of standard file
     * @param   pixel5Mask      DOCUMENT ME!
     * @param   x_dim2          x dimension of standard file
     * @param   y_dim2          y dimension of standard file
     * @param   z_dim2          z dimension of standard file
     * @param   threshold5      DOCUMENT ME!
     * @param   dcf             first derivatives of sd with respect to parameters
     * @param   ecf             second derivatives of sd with respect to parameters
     * @param   dx              storage for second partial derivatives of ratio with respect to parameters
     * @param   scale           DOCUMENT ME!
     * @param   count           DOCUMENT ME!
     * @param   mean            DOCUMENT ME!
     * @param   square          DOCUMENT ME!
     * @param   dmean           DOCUMENT ME!
     * @param   dsquare         DOCUMENT ME!
     * @param   emean           DOCUMENT ME!
     * @param   esquare         DOCUMENT ME!
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  DOCUMENT ME!
     * @param   minActualValue  DOCUMENT ME!
     *
     * @return  the correlation ratio
     *
     *          <p>This routine computes the first and second derivatives of the correlation ratio with respect to all
     *          external parameters.</p>
     *
     *          <p>The matrix indices have been adjusted to allow for a more orderly relationship between the matrix and
     *          its elements</p>
     *
     *          <p>Note that derivatives with respect to external parameters are ignored</p>
     */


    private double uvderivCOR2Dwarp(int parameters, double[][] es, int sampleFactor, float[] pixel2,
                                    boolean[] pixel2Mask, int x_dim1, int y_dim1, float threshold2, float[] pixel5,
                                    boolean[] pixel5Mask, int x_dim2, int y_dim2, int z_dim2, float threshold5,
                                    double[] dcf, double[][] ecf, double[] dx, double scale, int[] count, double[] mean,
                                    double[] square, double[][] dmean, double[][] dsquare, double[][][] emean,
                                    double[][][] esquare, int partitions, double maxActualValue,
                                    double minActualValue) {

        int counttotal = 0;
        double cf = 0.0; /* intermediate value */

        double a, b, d, e; /*interpolation weighting factors*/

        int pix1; /* int vale of current pixel in target file */
        float pix3; /*value of current pixel in standard file*/
        double pix4; /*calculated value of current pixel in standard file*/
        int slice1;
        int slice2;
        int remainder2;
        int rTerm;
        double i1;
        double j1;
        int n0p, n1p, n2p, n3p;

        int i, j, k;
        int ii;
        int jj;
        int s, t;
        int x_max1, y_max1;
        int x_up;
        int x_down;
        int y_up;
        int y_down;

        double x_i;
        double y_i;

        int order;
        int coeffp;

        int r;

        float n0, n1, n2, n3;
        double dxpix4, dypix4, ezpix4;
        float pix4Min;
        double value; /* intermediate value */
        double[] dvalue; /* partial derivatives of value with respect to parameters */
        double[] dsds;
        double[][] esds;
        double sd2;
        double[] dsd2;
        double[][] esd2;
        double evaluets;
        double meantotal;
        int counts;
        double means;
        double sds;

        order = 12;
        coeffp = 91;

        if (parameters < 182) {
            order = 11;
            coeffp = 78;
        }

        if (parameters < 156) {
            order = 10;
            coeffp = 66;
        }

        if (parameters < 132) {
            order = 9;
            coeffp = 55;
        }

        if (parameters < 110) {
            order = 8;
            coeffp = 45;
        }

        if (parameters < 90) {
            order = 7;
            coeffp = 36;
        }

        if (parameters < 72) {
            order = 6;
            coeffp = 28;
        }

        if (parameters < 56) {
            order = 5;
            coeffp = 21;
        }

        if (parameters < 42) {
            order = 4;
            coeffp = 15;
        }

        if (parameters < 30) {
            order = 3;
            coeffp = 10;
        }

        if (parameters < 20) {
            order = 2;
            coeffp = 6;
        }

        if (parameters < 12) {
            order = 1;
            coeffp = 3;
        }

        dvalue = new double[2 * coeffp];
        dsds = new double[2 * coeffp];
        esds = new double[2 * coeffp][2 * coeffp];
        dsd2 = new double[2 * coeffp];
        esd2 = new double[2 * coeffp][2 * coeffp];

        for (t = 0; t < coeffp; t++) {
            dx[t] = 0;
        }

        for (t = 0; t < (2 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < (2 * coeffp); t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] = 0;
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        slice1 = x_dim1 * y_dim1;

        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

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

            if (!pixel5Mask[r]) {
                continue;
            }

            /*We don't yet verify that pixel5>threshold to allow for less biased function*/

            pix3 = pixel5[r];

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            i1 = (double) i;
            j1 = (double) j;

            /*Calculate coordinates (x_i,y_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            dx2DSet(dx, order, i1, j1);

            /* Find the x and y coordinates */

            x_i = 0.0;
            y_i = 0.0;

            for (ii = 0; ii < coeffp; ii++) {
                x_i += es[0][ii] * dx[ii];
                y_i += es[1][ii] * dx[ii];
            }

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

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

            n0p = (k * slice1) + (y_down * x_dim1) + x_down;
            n0 = pixel2[n0p];
            n1p = (k * slice1) + (y_down * x_dim1) + x_up;
            n1 = pixel2[n1p];
            n2p = (k * slice1) + (y_up * x_dim1) + x_down;
            n2 = pixel2[n2p];
            n3p = (k * slice1) + (y_up * x_dim1) + x_up;
            n3 = pixel2[n3p];

            /*Verify that something is above threshold here*/
            if ((pix3 <= threshold5) && ((n0 <= threshold2) || (!pixel2Mask[n0p])) &&
                    ((n1 <= threshold2) || (!pixel2Mask[n1p])) && ((n2 <= threshold2) || (!pixel2Mask[n2p])) &&
                    ((n3 <= threshold2) || (!pixel2Mask[n3p]))) {
                continue;
            }
            // if (pix3 <= threshold5) continue;

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - minActualValue) / (maxActualValue - minActualValue));
            // pix1=(int)((partitions-1)*(pix3-threshold5)/(maxActualValue - threshold5));

            /*Calculate the trilinear interpolated voxel value*/

            pix4 = (n0 * d * e) + (n1 * a * e) + (n2 * d * b) + (n3 * a * b);

            /*Some intermediate values needed to calculate derivatives efficiently*/

            dxpix4 = ((e * (n1 - n0)) + (b * (n3 - n2)));
            dypix4 = ((d * (n2 - n0)) + (a * (n3 - n1)));

            ezpix4 = (n3 + n0 - n2 - n1);

            /*Calculate the value of the reslice pixel above the minimum */
            value = pix4 - pix4Min;
            mean[pix1] += value;
            square[pix1] += value * value;
            count[pix1]++;

            /*Calculate derivatives that are nonzero*/

            /* First derivatives */

            for (t = 0; t < coeffp; t++) {
                dvalue[t] = dxpix4 * dx[t];
                dmean[pix1][t] += dvalue[t];
                dsquare[pix1][t] += 2 * value * dvalue[t];
                dvalue[t + coeffp] = dypix4 * dx[t];
                dmean[pix1][t + coeffp] += dvalue[t + coeffp];
                dsquare[pix1][t + coeffp] += 2 * value * dvalue[t + coeffp];
            }

            /* Second derivatives */
            for (t = 0; t < (2 * coeffp); t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dvalue[s] * dvalue[t]);
                }
            }

            for (t = coeffp; t < (2 * coeffp); t++) {

                for (s = 0; s < coeffp; s++) {
                    evaluets = ezpix4 * dx[t - coeffp] * dx[s];
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

        for (t = 0; t < (2 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s < (2 * coeffp); s++) {
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

            for (t = 0; t < (2 * coeffp); t++) {
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

            for (t = 0; t < (2 * coeffp); t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] -
                                  (2 * counts * ((means * emean[jj][t][s]) + (dmean[jj][s] * dmean[jj][t])))) /
                                     (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < (2 * coeffp); t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < (2 * coeffp); t++) {

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

            for (t = 0; t < (2 * coeffp); t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < (2 * coeffp); t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        return cf;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parameters      number of formal parameters in chosen model
     * @param   es              reslice parameters
     * @param   sampleFactor    current sampling factor
     * @param   pixel2          original pixel values of file to reslice
     * @param   pixel2Mask      DOCUMENT ME!
     * @param   x_dim1          x dimension of reslice file
     * @param   y_dim1          y dimension of reslice file
     * @param   z_dim1          z dimension of reslice file
     * @param   threshold2      DOCUMENT ME!
     * @param   pixel5          pixel values of standard file
     * @param   pixel5Mask      DOCUMENT ME!
     * @param   x_dim2          x dimension of standard file
     * @param   y_dim2          y dimension of standard file
     * @param   z_dim2          z dimension of standard file
     * @param   threshold5      DOCUMENT ME!
     * @param   dcf             first derivatives of sd with respect to parameters
     * @param   ecf             second derivatives of sd with respect to parameters
     * @param   dx              storage for second partial derivatives of ratio with respect to parameters
     * @param   scale           DOCUMENT ME!
     * @param   count           DOCUMENT ME!
     * @param   mean            DOCUMENT ME!
     * @param   square          DOCUMENT ME!
     * @param   dmean           DOCUMENT ME!
     * @param   dsquare         DOCUMENT ME!
     * @param   emean           DOCUMENT ME!
     * @param   esquare         DOCUMENT ME!
     * @param   partitions      DOCUMENT ME!
     * @param   maxActualValue  DOCUMENT ME!
     * @param   minActualValue  DOCUMENT ME!
     *
     * @return  ratio the correlation ratio
     *
     *          <p>This routine computes the first and second derivatives of the correlation ratio with respect to all
     *          external parameters.</p>
     *
     *          <p>The matrix indices have been adjusted to allow for a more orderly relationship between the matrix and
     *          its elements</p>
     *
     *          <p>Note that derivatives with respect to external parameters are ignored</p>
     */

    private double uvderivCOR3Dwarp(int parameters, double[][] es, int sampleFactor, float[] pixel2,
                                    boolean[] pixel2Mask, int x_dim1, int y_dim1, int z_dim1, float threshold2,
                                    float[] pixel5, boolean[] pixel5Mask, int x_dim2, int y_dim2, int z_dim2,
                                    float threshold5, double[] dcf, double[][] ecf, double[] dx, double scale,
                                    int[] count, double[] mean, double[] square, double[][] dmean, double[][] dsquare,
                                    double[][][] emean, double[][][] esquare, int partitions, double maxActualValue,
                                    double minActualValue) {

        int counttotal = 0;
        double cf = 0.0;

        double a, b, c, d, e, f; /*interpolation weighting factors*/

        float pix3; /*value of current pixel in standard file*/
        double spix3;
        double pix4; /*calculated value of current pixel in standard file*/

        int slice1;
        int slice2;
        int remainder2;
        int rTerm;
        double i1;
        double j1;
        double k1;
        int n0p, n1p, n2p, n3p, n4p, n5p, n6p, n7p;

        int i, j, k;
        int ii;
        int jj;
        int pix1; /* int value of current pixel in standard file */
        int s, t;
        int x_max1, y_max1, z_max1;
        int x_up;
        int x_down;
        int y_up;
        int y_down;
        int z_up;
        int z_down;

        double x_i;
        double y_i;
        double z_i;

        int order;
        int coeffp;

        int r;

        float n0, n1, n2, n3, n4, n5, n6, n7;
        double dxpix4, dypix4, dzpix4, expix4, eypix4, ezpix4;
        float pix4Min;
        double value; /* intermediate value */
        double[] dvalue; /* partial derivatives of value with respect to parameters */
        double[] dsds;
        double[][] esds;
        double sd2;
        double[] dsd2;
        double[][] esd2;
        double evaluets;
        double meantotal;
        int counts;
        double means;
        double sds;

        order = 12;
        coeffp = 455;

        if (parameters < 1365) {
            order = 11;
            coeffp = 364;
        }

        if (parameters < 1092) {
            order = 10;
            coeffp = 286;
        }

        if (parameters < 858) {
            order = 9;
            coeffp = 220;
        }

        if (parameters < 660) {
            order = 8;
            coeffp = 165;
        }

        if (parameters < 495) {
            order = 7;
            coeffp = 120;
        }

        if (parameters < 360) {
            order = 6;
            coeffp = 84;
        }

        if (parameters < 252) {
            order = 5;
            coeffp = 56;
        }

        if (parameters < 168) {
            order = 4;
            coeffp = 35;
        }

        if (parameters < 105) {
            order = 3;
            coeffp = 20;
        }

        if (parameters < 60) {
            order = 2;
            coeffp = 10;
        }

        if (parameters < 30) {
            order = 1;
            coeffp = 4;
        }

        dvalue = new double[3 * coeffp];
        dsds = new double[3 * coeffp];
        esds = new double[3 * coeffp][3 * coeffp];
        dsd2 = new double[3 * coeffp];
        esd2 = new double[3 * coeffp][3 * coeffp];

        for (t = 0; t < coeffp; t++) {
            dx[t] = 0;
        }

        for (t = 0; t < (3 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s <= t; s++) {
                ecf[t][s] = 0;
            }
        }

        /*Initialize values*/
        for (jj = 0; jj < partitions; jj++) {
            count[jj] = 0;
            mean[jj] = 0;
            square[jj] = 0;

            for (t = 0; t < (3 * coeffp); t++) {

                dmean[jj][t] = 0;
                dsquare[jj][t] = 0;

                for (s = 0; s <= t; s++) {
                    emean[jj][t][s] = 0;
                    esquare[jj][t][s] = 0;
                }
            }
        }

        x_max1 = x_dim1 - 1;
        y_max1 = y_dim1 - 1;
        z_max1 = z_dim1 - 1;
        slice1 = x_dim1 * y_dim1;

        slice2 = x_dim2 * y_dim2;
        rTerm = slice2 * z_dim2;

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

            if (!pixel5Mask[r]) {
                continue;
            }

            /*We don't yet verify that pixel5>threshold to allow for less biased function*/

            pix3 = pixel5[r];
            spix3 = pix3 * scale;

            /*Calculate coordinates (i,j,k) of pixel r in standard file*/
            remainder2 = r % slice2;
            k = r / slice2;
            j = remainder2 / x_dim2;
            i = remainder2 % x_dim2;

            i1 = (double) i;
            j1 = (double) j;
            k1 = (double) k;

            /*Calculate coordinates (x_i,y_i,z_i) of corresponding pixel in reslice file*/

            /*Skip further computations if pixel is out of bounds*/

            dx3DSet(dx, order, i1, j1, k1);

            /* Find the x, y , and z coordinates */
            x_i = 0.0;
            y_i = 0.0;
            z_i = 0.0;

            for (ii = 0; ii < coeffp; ii++) {
                x_i += es[0][ii] * dx[ii];
                y_i += es[1][ii] * dx[ii];
                z_i += es[2][ii] * dx[ii];
            }

            if ((x_i < 0) || (x_i > x_max1)) {
                continue;
            }

            if ((y_i < 0) || (y_i > y_max1)) {
                continue;
            }

            if ((z_i < 0) || (z_i > z_max1)) {
                continue;
            }

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

            n0p = (z_down * slice1) + (y_down * x_dim1) + x_down;
            n0 = pixel2[n0p];
            n1p = (z_down * slice1) + (y_down * x_dim1) + x_up;
            n1 = pixel2[n1p];
            n2p = (z_down * slice1) + (y_up * x_dim1) + x_down;
            n2 = pixel2[n2p];
            n3p = (z_down * slice1) + (y_up * x_dim1) + x_up;
            n3 = pixel2[n3p];
            n4p = (z_up * slice1) + (y_down * x_dim1) + x_down;
            n4 = pixel2[n4p];
            n5p = (z_up * slice1) + (y_down * x_dim1) + x_up;
            n5 = pixel2[n5p];
            n6p = (z_up * slice1) + (y_up * x_dim1) + x_down;
            n6 = pixel2[n6p];
            n7p = (z_up * slice1) + (y_up * x_dim1) + x_up;
            n7 = pixel2[n7p];

            /*Verify that something is above threshold here*/
            if ((pix3 <= threshold5) && ((n0 <= threshold2) || (!pixel2Mask[n0p])) &&
                    ((n1 <= threshold2) || (!pixel2Mask[n1p])) && ((n2 <= threshold2) || (!pixel2Mask[n2p])) &&
                    ((n3 <= threshold2) || (!pixel2Mask[n3p])) && ((n4 <= threshold2) || (!pixel2Mask[n4p])) &&
                    ((n5 <= threshold2) || (!pixel2Mask[n5p])) && ((n6 <= threshold2) || (!pixel2Mask[n6p])) &&
                    ((n7 <= threshold2) || (!pixel2Mask[n7p]))) {
                continue;
            }

            /*Calculate the partition of the current standard pixel*/

            /*Note that if partitions==1, all pixels are in the same partition*/
            pix1 = (int) ((partitions - 1) * (pix3 - minActualValue) / (maxActualValue - minActualValue));

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

            for (t = 0; t < coeffp; t++) {
                dvalue[t] = dxpix4 * dx[t];
                dmean[pix1][t] += dvalue[t];
                dsquare[pix1][t] += 2 * value * dvalue[t];
                dvalue[t + coeffp] = dypix4 * dx[t];
                dmean[pix1][t + coeffp] += dvalue[t + coeffp];
                dsquare[pix1][t + coeffp] += 2 * value * dvalue[t + coeffp];
                dvalue[t + (2 * coeffp)] = dzpix4 * dx[t];
                dmean[pix1][t + (2 * coeffp)] += dvalue[t + (2 * coeffp)];
                dsquare[pix1][t + (2 * coeffp)] += 2 * value * dvalue[t + (2 * coeffp)];
            }

            /* Second derivatives */

            for (t = 0; t < coeffp; t++) {

                for (s = 0; s <= t; s++) {
                    esquare[pix1][t][s] += 2.0 * (dvalue[s] * dvalue[t]);
                }
            }

            for (; t < (2 * coeffp); t++) {

                for (s = 0; s < coeffp; s++) {
                    evaluets = ezpix4 * dx[t - coeffp] * dx[s];
                    emean[pix1][t][s] += evaluets;
                    esquare[pix1][t][s] += 2 * ((dvalue[s] * dvalue[t]) + (value * evaluets));
                }

                for (; s <= t; s++) {
                    esquare[pix1][t][s] += 2 * (dvalue[s] * dvalue[t]);
                }
            }

            for (; t < (3 * coeffp); t++) {

                for (s = 0; s < coeffp; s++) {
                    evaluets = eypix4 * dx[t - (2 * coeffp)] * dx[s];
                    emean[pix1][t][s] += evaluets;
                    esquare[pix1][t][s] += 2 * ((dvalue[s] * dvalue[t]) + (value * evaluets));
                }

                for (; s < (2 * coeffp); s++) {
                    evaluets = expix4 * dx[t - (2 * coeffp)] * dx[s - coeffp];
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

        for (t = 0; t < (3 * coeffp); t++) {
            dcf[t] = 0;

            for (s = 0; s < (3 * coeffp); s++) {
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

            for (t = 0; t < (3 * coeffp); t++) {
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

            for (t = 0; t < (3 * coeffp); t++) {
                dsds[t] = (dsquare[jj][t] - (2 * means * counts * dmean[jj][t])) / (counts - 1);

                for (s = 0; s <= t; s++) {
                    esds[t][s] = (esquare[jj][t][s] -
                                  (2 * counts * ((means * emean[jj][t][s]) + (dmean[jj][s] * dmean[jj][t])))) /
                                     (counts - 1);
                }
            }

            /*Calculate the normalized standard deviation for this partition*/

            sd2 = Math.sqrt(sds) / means;

            for (t = 0; t < (3 * coeffp); t++) {
                dsd2[t] = (dsds[t] / (2 * sd2 * means * means)) - (dmean[jj][t] * sd2 / means);
            }

            for (t = 0; t < (3 * coeffp); t++) {

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

            for (t = 0; t < (3 * coeffp); t++) {
                dcf[t] += dsd2[t] * counts;

                for (s = 0; s <= t; s++) {
                    ecf[t][s] += esd2[t][s] * counts;
                }
            }

            counttotal += count[jj];
        }

        /*Calculate the weighted, normalized standard deviation and its derivatives*/

        if (counttotal == 0) {
            error++;

            return 0.0;
        }

        /*Normalize by the number of voxels*/

        cf /= counttotal;

        for (t = 0; t < (3 * coeffp); t++) {
            dcf[t] /= counttotal;

            for (s = 0; s <= t; s++) {
                ecf[t][s] /= counttotal;
            }
        }

        return cf;
    }

    /**
     * From material Copyright 1995 Roger P. Woods, M.D. Modified 11/28/95 Computes windowed sinc
     *
     * @param   alpha  DOCUMENT ME!
     * @param   kern   DOCUMENT ME!
     *
     * @return  s
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
