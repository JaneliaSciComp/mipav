package gov.nih.mipav.model.algorithms;


import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * Runs ELSUNC for a 3D image.
 * 
 * <hr>
 * 
 * <p>Based on ELSUNC allowed by the author with acknowledgement:</p>
 * 
 * <p>Gauss-Newton Based Algorithms For Constrained Nonlinear Least Squares Problems by Per Lindstrom and Per-Ake Wedin,
 * Institute of Information Processing, University of Umea, S-901 87 Umea, Sweden This can be downleaded from
 * http://www.cs.umu.se/~perl/reports/alg.ps.gz</p>
 * 
 * @version 0.1 March 19, 2012
 * @author William Gandler
 */
public class AlgorithmConstELSUNCOpt3D extends AlgorithmBase {

    FitOAR3DConstrainedModel dModel;

    /** The initial bracket size for first iteration of ELSUNC. */
    private int bracketBound;

    /** Cost function called to measure cost - 1D. */
    private AlgorithmOptimizeFunctionBase costFunction;

    /** Final point when optimization is complete. */
    private double[] finalPoint;

    /** The transformation matrix from the origin of the input image. */
    private TransMatrixd fromOrigin;

    /** The cost of the function at the best minimum. */
    private double functionAtBest;

    private double minFunctionAtBest;

    /** The maximum number of iterations the optimization allows. */
    private int maxIterations;

    /** Progress bar that may be set for long optimization runs. */
    private ViewJProgressBar myProgressBar = null;

    /** Degress of freedom. */
    private int nDims;

    /** Parent algorithm that called this optimization. */
    private AlgorithmBase parent;

    /** Point that is currently being optimized. */
    private double[] point;

    /** Where progress is when sent in. */
    private int progressBegin;

    /** The max the progress can go to. */
    private int progressMax;

    /** Point that was initially passed into function. */
    private double[] start;

    /** Indicates whether the ELSUNC algorithm succeeded in finding a minimum. */
    private boolean success = true;

    /** Array of tolerances for each dimension. */
    private double[] OARTolerance;

    /** The transformation matrix to the origin of the input image. */
    protected TransMatrixd toOrigin;

    /** Array of translation and rotation limits for each dimension. */
    private float[][] trLimits;

    private int status;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new algorithm with the given centers of mass (needed for setting the transformations), the given
     * cost function (which was constructed with the proper images), the initial point we're looking at, some tolerance
     * within that point to look for the minimum, and the maximum number of iterations.
     * 
     * @param parent Algorithm that called this optimization.
     * @param com Center of Mass of the input image.
     * @param degreeOfFreedom Degree of freedom for transformation (must be 3, 4, 6, 7, 9, or 12).
     * @param costFunc Cost function to use.
     * @param initial Initial point to start from, length of 12.
     * @param tols Tolerance for each dimension (tols.length == degreeOfFreedom).
     * @param maxIter Maximum number of iterations.
     * @param bracketBound DOCUMENT ME!
     */
    public AlgorithmConstELSUNCOpt3D(AlgorithmBase parent, Vector3f com, int degreeOfFreedom,
            AlgorithmOptimizeFunctionBase costFunc, double[] initial, double[] tols, int maxIter, int bracketBound) {
        nDims = degreeOfFreedom;
        costFunction = costFunc;
        OARTolerance = tols;
        maxIterations = maxIter;
        this.bracketBound = bracketBound;
        this.parent = parent;
        point = new double[nDims];
        start = initial;

        if (degreeOfFreedom <= 12) {
            toOrigin = new TransMatrixd(4);
            toOrigin.setTranslate(com.X, com.Y, com.Z);

            fromOrigin = new TransMatrixd(4);
            fromOrigin.setTranslate( -com.X, -com.Y, -com.Z);
        }

        finalPoint = new double[start.length];

        // set up initial point and limits
        if (degreeOfFreedom == 3) {
            point[0] = initial[3]; // translation x
            point[1] = initial[4]; // translation y
            point[2] = initial[5]; // translation z
            trLimits = new float[2][3];

            for (int i = 0; i < 3; i++) { // translation x, y, z; i=0, 1, 2
                trLimits[0][i] = -Float.MAX_VALUE;
                trLimits[1][i] = Float.MAX_VALUE;
            }
        } else if (degreeOfFreedom == 4) {
            point[0] = initial[6]; // global scaling factor
            point[1] = initial[3]; // translation x
            point[2] = initial[4]; // translation y
            point[3] = initial[5]; // translation z
            trLimits = new float[2][4];
            trLimits[0][0] = -(float) Math.pow(10, 10); // global scale is unbound
            trLimits[1][0] = (float) Math.pow(10, 10); // global scale is unbound

            for (int i = 0; i < 3; i++) { // translation x, y, z; i=0, 1, 2
                trLimits[0][i] = -Float.MAX_VALUE;
                trLimits[1][i] = Float.MAX_VALUE;
            }
        } else if (degreeOfFreedom >= 6) {
            trLimits = new float[2][degreeOfFreedom];

            for (int i = 0; i < degreeOfFreedom; i++) {
                point[i] = initial[i]; // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
                // = 6 + 1 scale = 7 + 3 scales = 9 + 3 skews = 12
                trLimits[0][i] = -Float.MAX_VALUE;
                trLimits[1][i] = Float.MAX_VALUE;
            }
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Sets everything to null and prepares this class for destruction.
     */
    public void disposeLocal() {
        costFunction = null;
        start = null;
        point = null;
        OARTolerance = null;
        finalPoint = null;
        toOrigin = null;
        fromOrigin = null;
    }

    /**
     * Helper method to take the "point" we've been working with as a vector, and convert it into a transformation
     * matrix. The length of the vector should be equal to the global variable nDims, which in turn was initialized by
     * the degrees of freedom originally sent to this algorithm. Therefore, if there are 3 degrees of freedom, we set
     * only the translations; 4 means translation and global scaling; 6 means rotation and translation; 7 means
     * rotation, translation, and global scaling; 9 means rotation, translation, and scaling; and 12 means rotation,
     * translation, scaling, and skewing.
     * 
     * @param vector Vector that represented a "point" in the algorithm which needs to be converted to a matrix.
     * 
     * @return The transformation matrix created from the vector.
     */
    public TransMatrixd convertToMatrix(double[] vector) {

        // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
        // = 6 + 1 scale = 7 + 3 scales = 9 + 3 skews = 12
        double rotX = start[0];
        double rotY = start[1];
        double rotZ = start[2];
        double transX = start[3];
        double transY = start[4];
        double transZ = start[5];
        double scaleX = start[6];
        double scaleY = start[7];
        double scaleZ = start[8];
        double skewX = start[9];
        double skewY = start[10];
        double skewZ = start[11];

        // set up parts of transform properly
        if (vector.length == 3) {
            transX = vector[0];
            transY = vector[1];
            transZ = vector[2];
        } else if (vector.length == 4) {
            transX = vector[1];
            transY = vector[2];
            transZ = vector[3];
            scaleX = scaleY = scaleZ = vector[0];
        } else if ( (vector.length == 6) || (vector.length == 7) || (vector.length == 9) || (vector.length == 12)) {
            rotX = vector[0];
            rotY = vector[1];
            rotZ = vector[2];
            transX = vector[3];
            transY = vector[4];
            transZ = vector[5];

            if (vector.length == 7) {
                scaleX = scaleY = scaleZ = vector[6];
            } else if (vector.length > 7) {
                scaleX = vector[6];
                scaleY = vector[7];
                scaleZ = vector[8];

                if (vector.length > 9) {
                    skewX = vector[9];
                    skewY = vector[10];
                    skewZ = vector[11];
                }
            }
        }

        TransMatrixd matrix = new TransMatrixd(4);

        matrix.setTransform(transX, transY, transZ, rotX, rotY, rotZ, scaleX, scaleY, scaleZ, skewX, skewY, skewZ);

        matrix.MultLeft(toOrigin);
        matrix.Mult(fromOrigin);
        // Matrix mtx = (toOrigin.times(matrix)).times(fromOrigin);

        return matrix;
    }

    /**
     * Helper method to take the "point" we've been working with as a vector, and convert it into a transformation
     * matrix. The length of the vector should be equal to the global variable nDims, which in turn was initialized by
     * the degrees of freedom originally sent to this algorithm. Therefore, if there are 3 degrees of freedom, we set
     * only the translations; 4 means translation and global scaling; 6 means rotation and translation; 7 means
     * rotation, translation, and global scaling; 9 means rotation, translation, and scaling; and 12 means rotation,
     * translation, scaling, and skewing.
     * 
     * @param vector Vector that represented a "point" in the algorithm which needs to be converted to a matrix.
     * 
     * @return The transformation matrix created from the vector.
     */
    public TransMatrixd convertToMatrixHalf(double[] vector) {

        // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
        // = 6 + 1 scale = 7 + 3 scales = 9 + 3 skews = 12
        double rotX = start[0];
        double rotY = start[1];
        double rotZ = start[2];
        double transX = start[3];
        double transY = start[4];
        double transZ = start[5];
        double scaleX = start[6];
        double scaleY = start[7];
        double scaleZ = start[8];
        double skewX = start[9];
        double skewY = start[10];
        double skewZ = start[11];

        // set up parts of transform properly
        if (vector.length == 3) {
            transX = vector[0];
            transY = vector[1];
            transZ = vector[2];
        } else if (vector.length == 4) {
            transX = vector[1];
            transY = vector[2];
            transZ = vector[3];
            scaleX = scaleY = scaleZ = vector[0];
        } else if ( (vector.length == 6) || (vector.length == 7) || (vector.length == 9) || (vector.length == 12)) {
            rotX = vector[0];
            rotY = vector[1];
            rotZ = vector[2];
            transX = vector[3];
            transY = vector[4];
            transZ = vector[5];

            if (vector.length == 7) {
                scaleX = scaleY = scaleZ = vector[6];
            } else if (vector.length > 7) {
                scaleX = vector[6];
                scaleY = vector[7];
                scaleZ = vector[8];

                if (vector.length > 9) {
                    skewX = vector[9];
                    skewY = vector[10];
                    skewZ = vector[11];
                }
            }
        }

        rotX /= 2;
        rotY /= 2;
        rotZ /= 2;
        transX /= 2;
        transY /= 2;
        transZ /= 2;
        scaleX /= 2;
        scaleY /= 2;
        scaleZ /= 2;
        skewX /= 2;
        skewY /= 2;
        skewZ /= 2;

        TransMatrixd matrix = new TransMatrixd(4);

        matrix.setTransform(transX, transY, transZ, rotX, rotY, rotZ, scaleX, scaleY, scaleZ, skewX, skewY, skewZ);

        matrix.MultLeft(toOrigin);
        matrix.Mult(fromOrigin);
        // Matrix mtx = (toOrigin.times(matrix)).times(fromOrigin);

        // matrix.convertFromMatrix(mtx);

        return matrix;
    }

    /**
     * Helper method to take the "point" we've been working with as a vector, and convert it into a transformation
     * matrix. The length of the vector should be equal to the global variable nDims, which in turn was initialized by
     * the degrees of freedom originally sent to this algorithm. Therefore, if there are 3 degrees of freedom, we set
     * only the translations; 4 means translation and global scaling; 6 means rotation and translation; 7 means
     * rotation, translation, and global scaling; 9 means rotation, translation, and scaling; and 12 means rotation,
     * translation, scaling, and skewing.
     * 
     * @param vector Vector that represented a "point" in the algorithm which needs to be converted to a matrix.
     * 
     * @return The transformation matrix created from the vector.
     */
    public TransMatrixd convertToMatrixMidsagittal(double[] vector) {

        // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
        // = 6 + 1 scale = 7 + 3 scales = 9 + 3 skews = 12
        double rotX = start[0];
        double rotY = start[1];
        double rotZ = start[2];
        double transX = start[3];
        double transY = start[4];
        double transZ = start[5];
        double scaleX = start[6];
        double scaleY = start[7];
        double scaleZ = start[8];
        double skewX = start[9];
        double skewY = start[10];
        double skewZ = start[11];

        // set up parts of transform properly
        if (vector.length == 3) {
            transX = vector[0];
            transY = vector[1];
            transZ = vector[2];
        } else if (vector.length == 4) {
            transX = vector[1];
            transY = vector[2];
            transZ = vector[3];
            scaleX = scaleY = scaleZ = vector[0];
        } else if ( (vector.length == 6) || (vector.length == 7) || (vector.length == 9) || (vector.length == 12)) {
            rotX = vector[0];
            rotY = vector[1];
            rotZ = vector[2];
            transX = vector[3];
            transY = vector[4];
            transZ = vector[5];

            if (vector.length == 7) {
                scaleX = scaleY = scaleZ = vector[6];
            } else if (vector.length > 7) {
                scaleX = vector[6];
                scaleY = vector[7];
                scaleZ = vector[8];

                if (vector.length > 9) {
                    skewX = vector[9];
                    skewY = vector[10];
                    skewZ = vector[11];
                }
            }
        }

        // only want z rotation and translations in x and y dims
        rotX /= 2;
        rotY /= 2;
        rotZ /= 2;
        transX /= 2;
        transY /= 2;
        transZ = 0;
        scaleX = 1;
        scaleY = 1;
        scaleZ = 1;
        skewX = 0;
        skewY = 0;
        skewZ = 0;

        TransMatrixd matrix = new TransMatrixd(4);

        matrix.setTransform(transX, transY, transZ, rotX, rotY, rotZ, scaleX, scaleY, scaleZ, skewX, skewY, skewZ);

        matrix.MultLeft(toOrigin);
        matrix.Mult(fromOrigin);
        // Matrix mtx = (toOrigin.times(matrix)).times(fromOrigin);

        // matrix.convertFromMatrix(mtx);

        return matrix;
    }

    /**
     * Accessor that returns the final point with translations, rotations, scales, and skews representing the best
     * tranformation.
     * 
     * @return vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public double[] getFinal() {

        for (int i = 0; i < start.length; i++) {
            finalPoint[i] = start[i];
        }

        if (nDims == 3) {
            finalPoint[3] = point[0]; // translation x
            finalPoint[4] = point[1]; // translation y
            finalPoint[5] = point[2]; // translation z
        } else if (nDims == 4) {
            finalPoint[7] = finalPoint[8] = finalPoint[6] = point[0]; // global scaling factor
            finalPoint[3] = point[1]; // translation x
            finalPoint[4] = point[2]; // translation y
            finalPoint[5] = point[3]; // translation z
        } else if (nDims >= 6) {

            for (int i = 0; i < nDims; i++) {
                finalPoint[i] = point[i]; // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
            }

            if (nDims == 7) {
                finalPoint[7] = finalPoint[8] = finalPoint[6];
            }
        }

        return finalPoint;
    }

    /**
     * Accessor that returns the final point with translations, rotations, scales, and skews representing the best
     * tranformation.
     * 
     * @return vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public double[] getFinal(double[] point) {

        for (int i = 0; i < start.length; i++) {
            finalPoint[i] = start[i];
        }

        if (nDims == 3) {
            finalPoint[3] = point[0]; // translation x
            finalPoint[4] = point[1]; // translation y
            finalPoint[5] = point[2]; // translation z
        } else if (nDims == 4) {
            finalPoint[7] = finalPoint[8] = finalPoint[6] = point[0]; // global scaling factor
            finalPoint[3] = point[1]; // translation x
            finalPoint[4] = point[2]; // translation y
            finalPoint[5] = point[3]; // translation z
        } else if (nDims >= 6) {

            for (int i = 0; i < nDims; i++) {
                finalPoint[i] = point[i]; // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
            }

            if (nDims == 7) {
                finalPoint[7] = finalPoint[8] = finalPoint[6];
            }
        }

        return finalPoint;
    }

    /**
     * Accessor that returns the final point with translations, rotations, scales, and skews representing the best
     * tranformation.
     * 
     * @param sample the voxel resolution
     * 
     * @return vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public double[] getFinal(float sample) {
        double transX = 0.0;
        double transY = 0.0;
        double transZ = 0.0;

        for (int i = 0; i < start.length; i++) {
            finalPoint[i] = start[i];
        }

        if (nDims <= 12) {
            TransMatrixd mat = convertToMatrix(point);

            transX = mat.Get(0, 3) * sample;
            transY = mat.Get(1, 3) * sample;
            transZ = mat.Get(2, 3) * sample;
        }

        if (nDims == 3) {
            finalPoint[3] = transX; // translation x
            finalPoint[4] = transY; // translation y
            finalPoint[5] = transZ; // translation z
        } else if (nDims == 4) {
            finalPoint[7] = finalPoint[8] = finalPoint[6] = point[0]; // global scaling factor
            finalPoint[3] = transX; // translation x
            finalPoint[4] = transY; // translation y
            finalPoint[5] = transZ; // translation z
        } else if (nDims >= 6) {

            for (int i = 0; i < nDims; i++) {

                if (i == 3) {
                    finalPoint[3] = transX;
                } else if (i == 4) {
                    finalPoint[4] = transY;
                } else if (i == 5) {
                    finalPoint[5] = transZ;
                } else {
                    finalPoint[i] = point[i]; // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
                }
            }

            if (nDims == 7) {
                finalPoint[7] = finalPoint[8] = finalPoint[6];
            }
        }

        return finalPoint;
    }

    /**
     * Accessor that returns the matrix representing the best transformation.
     * 
     * @return matrix representing the best transformation.
     */
    public TransMatrixd getMatrix() {
        return convertToMatrix(point);
    }

    /**
     * Accessor that returns the matrix representing the best transformation. The passed in parameter represents the
     * resolution (same in all directions and for both input and reference images, since resampled isotropically). Since
     * the optimization was done in pixel space, not millimeter space, the translation parameters need to be scaled by
     * the sample value.
     * 
     * @param sample the voxel resolution
     * 
     * @return matrix representing the best transformation.
     */
    public TransMatrixd getMatrix(float sample) {

        // will resolution affect scale??
        TransMatrixd mat = convertToMatrix(point);
        double transX = mat.get(0, 3) * sample;
        double transY = mat.get(1, 3) * sample;
        double transZ = mat.get(2, 3) * sample;

        mat.set(0, 3, transX);
        mat.set(1, 3, transY);
        mat.set(2, 3, transZ);

        return mat;
    }

    /**
     * Accessor that returns the matrix representing the best tranformation. All of the components of the transformation
     * are halved from the 'best transformation' matrix.
     * 
     * @return matrix representing the best transformation with its components halved.
     */
    public TransMatrixd getMatrixHalf() {
        return convertToMatrixHalf(point);
    }

    /**
     * Accessor that returns the matrix representing the best tranformation. The passed in parameter represents the
     * resolution (same in all directions and for both input and reference images, since resampled isotropically). Since
     * the optimization was done in pixel space, not millimeter space, the translation parameters need to be scaled by
     * the sample value. All of the components of the transformation are halved from the 'best transformation' matrix.
     * 
     * @param sample the voxel resolution
     * 
     * @return matrix representing the best transformation with its components halved.
     */
    public TransMatrixd getMatrixHalf(float sample) {

        // will resolution affect scale??
        TransMatrixd mat = convertToMatrixHalf(point);
        double transX = mat.get(0, 3) * sample;
        double transY = mat.get(1, 3) * sample;
        double transZ = mat.get(2, 3) * sample;

        mat.set(0, 3, transX);
        mat.set(1, 3, transY);
        mat.set(2, 3, transZ);

        return mat;
    }

    /**
     * Accessor that returns the matrix representing the best tranformation. This transformation contains only the z
     * rotation and the x and y translation, to be used in the midsagittal alignment algorithm.
     * 
     * @return matrix representing the best transformation's z rot and x and y trans.
     */
    public TransMatrixd getMatrixMidsagittal() {
        return convertToMatrixMidsagittal(point);
    }

    /**
     * Accessor that returns the matrix representing the best tranformation. The passed in parameter represents the
     * resolution (same in all directions and for both input and reference images, since resampled isotropically). Since
     * the optimization was done in pixel space, not millimeter space, the translation parameters need to be scaled by
     * the sample value. This transformation contains only the z rotation and the x and y translation, to be used in the
     * midsagittal alignment algorithm.
     * 
     * @param sample the voxel resolution
     * 
     * @return matrix representing the best transformation's z rot and x and y trans.
     */
    public TransMatrixd getMatrixMidsagittal(float sample) {

        // will resolution affect scale??
        TransMatrixd mat = convertToMatrixMidsagittal(point);
        double transX = mat.get(0, 3) * sample;
        double transY = mat.get(1, 3) * sample;
        double transZ = mat.get(2, 3) * sample;

        mat.set(0, 3, transX);
        mat.set(1, 3, transY);
        mat.set(2, 3, transZ);

        return mat;
    }

    /**
     * Calls cost function with point and saves result in functionAtBest.
     */
    public void measureCost() {
        functionAtBest = costFunction.cost(convertToMatrix(point));
    }

    /**
     * Runs ELSUNC along one dimension at a time as long as the costFunction improves during one cycle of runs along
     * every dimension.
     */
    public void runAlgorithm() {
        int i;
        boolean anotherCycle = true;
        double[] lastPoint = new double[nDims];
        // Initialize data.
        functionAtBest = Double.MAX_VALUE;
        minFunctionAtBest = Double.MAX_VALUE;
        int cycles = 0;
        int maxCycles = 10;

        while (anotherCycle) {
            cycles++;
            if (cycles > maxCycles) {
                break;
            }
            anotherCycle = false;
            for (i = 0; i < nDims; i++) {
                lastPoint[i] = point[i];
                dModel = new FitOAR3DConstrainedModel(i);
                dModel.driver();
                status = dModel.getExitStatus();
                // dModel.statusMessage(status);
                // status == -2 if maxIterations reached
                if ( (status > 0) || (status == -2)) {
                    double params[] = dModel.getParameters();
                    point[i] = params[0];
                    double[] fullPoint = getFinal(point);
                    functionAtBest = costFunction.cost(convertToMatrix(fullPoint));
                    if (functionAtBest < minFunctionAtBest) {
                        minFunctionAtBest = functionAtBest;
                        if (Math.abs(point[i] - lastPoint[i]) > OARTolerance[i]) {
                            anotherCycle = true;
                        }
                    } else {
                        point[i] = lastPoint[i];
                    }
                } // if (status > 0)
                else {
                    point[i] = lastPoint[i];
                }
            } // for (i = 0; i < nDims; i++)
        } // while (anotherCycle)
    }

    /**
     * Sets the initial point to the value passed in.
     * 
     * @param initial Initial point.
     */
    public void setInitialPoint(double[] initial) {

        for (int i = 0; i < initial.length; i++) {
            start[i] = initial[i];
        }

        // set up initial point properly
        if (nDims == 3) {
            point[0] = initial[3]; // translation x
            point[1] = initial[4]; // translation y
            point[2] = initial[5]; // translation z
        } else if (nDims == 4) {
            point[0] = initial[6]; // global scaling factor
            point[1] = initial[3]; // translation x
            point[2] = initial[4]; // translation y
            point[3] = initial[5]; // translation z
        } else if (nDims >= 6) {

            for (int i = 0; i < nDims; i++) {
                point[i] = initial[i]; // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
            }
        }
    }

    /**
     * Sets the limits on rotation and translation.
     * 
     * @param limits limits
     */
    public void setLimits(float[][] limits) {
        /*
         * double[][] xi = new double[nDims][nDims]; double[] unit_tolerance = new double[nDims]; for (int i = 0; i <
         * nDims; i++) { xi[i][i] = 1.0; double tol = 0, sum = 0;
         * 
         * for (int j = 0; j < nDims; j++) { sum += xi[j][i] * xi[j][i]; } sum = Math.sqrt(sum);
         * 
         * for (int j = 0; j < nDims; j++) { if (tolerance[j] > 1.0E-20) { tol += Math.abs(xi[j][i] / (sum *
         * tolerance[j])); } } unit_tolerance[i] = Math.abs(1.0 / tol); }
         */

        if (nDims == 3) {
            trLimits = new float[2][3];

            for (int i = 0; i < 3; i++) { // translation x, y, z; i=0, 1, 2

                for (int j = 0; j < 2; j++) { // min and max = j=0,1
                    trLimits[j][i] = limits[j][i + 3];
                }

                Preferences.debug("3D optimization.  For direction " + i + ", the minimum is: " + trLimits[0][i]
                        + " and the maximum is " + trLimits[1][i] + " pixels.\n", Preferences.DEBUG_ALGORITHM);
            }
        } else if (nDims == 4) {
            trLimits = new float[2][4];
            trLimits[0][0] = -(float) Math.pow(10, 10); // global scale is unbound
            trLimits[1][0] = (float) Math.pow(10, 10); // global scale is unbound
            // trLimits[0][0] = (float)(point[0] - bracketBound * unit_tolerance[0]);
            // trLimits[1][0] = (float)(point[0] + bracketBound * unit_tolerance[0]);

            for (int i = 0; i < 3; i++) { // translation x, y, z; i=0, 1, 2

                for (int j = 0; j < 2; j++) { // min and max = j=0,1
                    trLimits[j][i + 1] = limits[j][i + 3];
                }
            }
        } else if (nDims >= 6) {
            trLimits = new float[2][nDims];

            for (int i = 0; i < limits[0].length; i++) { // translation x, y, z and rotation 1, 2, 3

                for (int j = 0; j < 2; j++) { // min and max = j=0,1
                    trLimits[j][i] = limits[j][i];
                }

                Preferences.debug("6D optimization.  For direction " + i + ", minimum is: " + trLimits[0][i]
                        + ", maximum is " + trLimits[1][i] + ".\n", Preferences.DEBUG_ALGORITHM);
            }

            for (int i = limits[0].length; i < nDims; i++) {
                trLimits[0][i] = -Float.MAX_VALUE;
                trLimits[1][i] = Float.MAX_VALUE;
                // trLimits[0][i] = (float)(point[i] - bracketBound * unit_tolerance[i]);
                // trLimits[1][i] = (float)(point[i] + bracketBound * unit_tolerance[i]);
            }
        }
    }

    /**
     * Returns the cost of the best transformation.
     * 
     * @return The cost of the best transformation.
     */
    public double getCost() {
        return functionAtBest;
    }

    /**
     * Returns the optimized point, with length == degrees of freedom.
     * 
     * @return The optimized point.
     */
    public double[] getPoint() {
        double[] pt = new double[point.length];

        for (int i = 0; i < pt.length; i++) {
            pt[i] = point[i];
        }

        return pt;
    }

    /**
     * Sets the maximum number of iterations.
     * 
     * @param max The max number of iterations.
     */
    public void setMaxIterations(int max) {
        maxIterations = max;
    }

    /**
     * Returns whether or not a minimum was found.
     * 
     * @return whether or not a minimum was found.
     */
    public boolean didSucceed() {
        // status == -2 for maxIterations reached
        if ( (status > 0) || (status == -2)) {
            success = true;
        } else {
            success = false;
        }
        return success;
    }

    /**
     * Accessor that sets the progress bar so it can be updated from here.
     * 
     * @param progress DOCUMENT ME!
     * @param begin Value of progress bar when sent here.
     * @param max Maximum value allowed.
     */
    public void setProgressBar(ViewJProgressBar progress, int begin, int max) {
        myProgressBar = progress;
        progressBegin = begin;
        progressMax = max;
    }

    class FitOAR3DConstrainedModel extends NLConstrainedEngine {
        private int currentDim;

        /**
         * Creates a new FitOAR3DConstrainedModel object.
         * 
         * @param currentDim Only optimize along 1 dimension at a time
         */
        public FitOAR3DConstrainedModel(int currentDim) {

            super(1, 1);
            this.currentDim = currentDim;

            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            bl[0] = trLimits[0][currentDim];
            bu[0] = trLimits[1][currentDim];
            gues[0] = point[currentDim];

            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;
            parameterConvergence = OARTolerance[currentDim];
            maxIterations = 20;
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying OAR3DConstrained fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitOAR3DConstrainedModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * 
         * 
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            try {
                ctrl = ctrlMat[0];
                if ( (ctrl == -1) || (ctrl == 1)) {
                    point[currentDim] = a[0];
                    double[] fullPoint = getFinal(point);
                    residuals[0] = costFunction.cost(convertToMatrix(fullPoint));
                } // if ((ctrl == -1) || (ctrl == 1))

                // Calculate the Jacobian numerically
                else if (ctrl == 2) {
                    ctrlMat[0] = 0;
                }
            } catch (final Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }
}
