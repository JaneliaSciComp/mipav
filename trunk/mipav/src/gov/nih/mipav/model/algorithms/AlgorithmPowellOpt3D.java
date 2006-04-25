package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;


/**
 * Runs Powell's method for a 3D image. Check the parent class comment for more detailed information.
 *
 * @version  0.1 Oct 1, 2001
 * @author   Neva Cherniavsky
 */
public class AlgorithmPowellOpt3D extends AlgorithmPowellOptBase {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new algorithm with the given centers of mass (needed for setting the transformations), the given
     * cost function (which was constructed with the proper images), the initial point we're looking at, some tolerance
     * within that point to look for the minimum, and the maximum number of iterations.
     *
     * @param  parent           Algorithm that called this optimization.
     * @param  com              Center of Mass of the input image.
     * @param  degreeOfFreedom  Degree of freedom for transformation (must be 3, 4, 6, 7, 9, or 12).
     * @param  costFunc         Cost function to use.
     * @param  initial          Initial point to start from, length of 12.
     * @param  tols             Tolerance for each dimension (tols.length == degreeOfFreedom).
     * @param  maxIter          Maximum number of iterations.
     * @param  bracketBound     DOCUMENT ME!
     */
    public AlgorithmPowellOpt3D(AlgorithmBase parent, Point3Dd com, int degreeOfFreedom,
                                AlgorithmOptimizeFunctionBase costFunc, double[] initial, double[] tols, int maxIter,
                                int bracketBound) {
        super(parent, degreeOfFreedom, costFunc, initial, tols, maxIter, 3, bracketBound);

        if (degreeOfFreedom <= 12) {
            toOrigin = new TransMatrix(4);
            toOrigin.setTranslate(com.x, com.y, com.z);

            fromOrigin = new TransMatrix(4);
            fromOrigin.setTranslate(-com.x, -com.y, -com.z);
        }

        finalPoint = new double[start.length];

        // set up initial point properly
        if (degreeOfFreedom == 3) {
            point[0] = initial[3]; // translation x
            point[1] = initial[4]; // translation y
            point[2] = initial[5]; // translation z
        } else if (degreeOfFreedom == 4) {
            point[0] = initial[6]; // global scaling factor
            point[1] = initial[3]; // translation x
            point[2] = initial[4]; // translation y
            point[3] = initial[5]; // translation z
        } else if (degreeOfFreedom >= 6) {

            for (int i = 0; i < degreeOfFreedom; i++) {
                point[i] = initial[i]; // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
            } // = 6       + 1 scale = 7   + 3 scales = 9   + 3 skews = 12
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Helper method to take the "point" we've been working with as a vector, and convert it into a transformation
     * matrix. The length of the vector should be equal to the global variable nDims, which in turn was initialized by
     * the degrees of freedom originally sent to this algorithm. Therefore, if there are 3 degrees of freedom, we set
     * only the translations; 4 means translation and global scaling; 6 means rotation and translation; 7 means
     * rotation, translation, and global scaling; 9 means rotation, translation, and scaling; and 12 means rotation,
     * translation, scaling, and skewing.
     *
     * @param   vector  Vector that represented a "point" in the algorithm which needs to be converted to a matrix.
     *
     * @return  The transformation matrix created from the vector.
     */
    public TransMatrix convertToMatrix(double[] vector) {

        // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
        // = 6       + 1 scale = 7   + 3 scales = 9   + 3 skews = 12
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
        } else if ((vector.length == 6) || (vector.length == 7) || (vector.length == 9) || (vector.length == 12)) {
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

        TransMatrix matrix = new TransMatrix(4);

        matrix.setTransform(transX, transY, transZ, rotX, rotY, rotZ, scaleX, scaleY, scaleZ, skewX, skewY, skewZ);

        Matrix mtx = (toOrigin.times(matrix)).times(fromOrigin);

        matrix.convertFromMatrix(mtx);

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
     * @param   vector  Vector that represented a "point" in the algorithm which needs to be converted to a matrix.
     *
     * @return  The transformation matrix created from the vector.
     */
    public TransMatrix convertToMatrixHalf(double[] vector) {

        // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
        // = 6       + 1 scale = 7   + 3 scales = 9   + 3 skews = 12
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
        } else if ((vector.length == 6) || (vector.length == 7) || (vector.length == 9) || (vector.length == 12)) {
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

        TransMatrix matrix = new TransMatrix(4);

        matrix.setTransform(transX, transY, transZ, rotX, rotY, rotZ, scaleX, scaleY, scaleZ, skewX, skewY, skewZ);

        Matrix mtx = (toOrigin.times(matrix)).times(fromOrigin);

        matrix.convertFromMatrix(mtx);

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
     * @param   vector  Vector that represented a "point" in the algorithm which needs to be converted to a matrix.
     *
     * @return  The transformation matrix created from the vector.
     */
    public TransMatrix convertToMatrixMidsagittal(double[] vector) {

        // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
        // = 6       + 1 scale = 7   + 3 scales = 9   + 3 skews = 12
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
        } else if ((vector.length == 6) || (vector.length == 7) || (vector.length == 9) || (vector.length == 12)) {
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

        TransMatrix matrix = new TransMatrix(4);

        matrix.setTransform(transX, transY, transZ, rotX, rotY, rotZ, scaleX, scaleY, scaleZ, skewX, skewY, skewZ);

        Matrix mtx = (toOrigin.times(matrix)).times(fromOrigin);

        matrix.convertFromMatrix(mtx);

        return matrix;
    }

    /**
     * Accessor that returns the final point with translations, rotations, scales, and skews representing the best
     * tranformation.
     *
     * @return  vector representing the best transformation in terms of translations, rotations, scales, and skews.
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
     * @param   sample  the voxel resolution
     *
     * @return  vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public double[] getFinal(float sample) {
        double transX = 0.0;
        double transY = 0.0;
        double transZ = 0.0;

        for (int i = 0; i < start.length; i++) {
            finalPoint[i] = start[i];
        }

        if (nDims <= 12) {
            TransMatrix mat = convertToMatrix(point);

            transX = mat.get(0, 3) * sample;
            transY = mat.get(1, 3) * sample;
            transZ = mat.get(2, 3) * sample;
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
     * Accessor that returns the matrix representing the best tranformation.
     *
     * @return  matrix representing the best transformation.
     */
    public TransMatrix getMatrix() {
        return convertToMatrix(point);
    }

    /**
     * Accessor that returns the matrix representing the best tranformation. The passed in parameter represents the
     * resolution (same in all directions and for both input and reference images, since resampled isotropically). Since
     * the optimization was done in pixel space, not millimeter space, the translation parameters need to be scaled by
     * the sample value.
     *
     * @param   sample  the voxel resolution
     *
     * @return  matrix representing the best transformation.
     */
    public TransMatrix getMatrix(float sample) {

        // will resolution affect scale??
        TransMatrix mat = convertToMatrix(point);
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
     * @return  matrix representing the best transformation with its components halved.
     */
    public TransMatrix getMatrixHalf() {
        return convertToMatrixHalf(point);
    }

    /**
     * Accessor that returns the matrix representing the best tranformation. The passed in parameter represents the
     * resolution (same in all directions and for both input and reference images, since resampled isotropically). Since
     * the optimization was done in pixel space, not millimeter space, the translation parameters need to be scaled by
     * the sample value. All of the components of the transformation are halved from the 'best transformation' matrix.
     *
     * @param   sample  the voxel resolution
     *
     * @return  matrix representing the best transformation with its components halved.
     */
    public TransMatrix getMatrixHalf(float sample) {

        // will resolution affect scale??
        TransMatrix mat = convertToMatrixHalf(point);
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
     * @return  matrix representing the best transformation's z rot and x and y trans.
     */
    public TransMatrix getMatrixMidsagittal() {
        return convertToMatrixMidsagittal(point);
    }

    /**
     * Accessor that returns the matrix representing the best tranformation. The passed in parameter represents the
     * resolution (same in all directions and for both input and reference images, since resampled isotropically). Since
     * the optimization was done in pixel space, not millimeter space, the translation parameters need to be scaled by
     * the sample value. This transformation contains only the z rotation and the x and y translation, to be used in the
     * midsagittal alignment algorithm.
     *
     * @param   sample  the voxel resolution
     *
     * @return  matrix representing the best transformation's z rot and x and y trans.
     */
    public TransMatrix getMatrixMidsagittal(float sample) {

        // will resolution affect scale??
        TransMatrix mat = convertToMatrixMidsagittal(point);
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
     * Runs Powell's method. Powell's method is a way to find minimums without finding derivatives. Basically, it starts
     * at some point P in N-dimensional space, proceeds in a direction, and minimizes along that line using golden
     * search. It then resets the point and minimizes again, until the point moves by less than the tolerance. This
     * method starts with the initial point defined in the constructor and initial directions of (1 0 ... 0) (0 1 0 ...
     * ) ..., the basis vectors. At the end, "point" is the best point found and functionAtBest is the value at "point".
     */
    public void runAlgorithm() {
        int count = 0;
        int boundGuess;
        double initialGuess;
        double[] pt = new double[nDims];
        double[] directions = new double[nDims];
        boolean keepGoing = true;

        functionAtBest = Double.MAX_VALUE;

        while ((count < maxIterations) && keepGoing) {

            // Initialize data for testing tolerance.
            for (int i = 0; i < nDims; i++) {
                pt[i] = point[i];
            }

            keepGoing = false; // Only terminate loop when point changes for ALL directions are less

            // than their respective tolerances.  Will only stay false if ALL are false.

            for (int i = 0; (i < nDims) && !parent.isThreadStopped(); i++) {

                if (myProgressBar != null) {
                    myProgressBar.updateValue(progressBegin +
                                              ((((nDims * count) + (i + 1)) * progressMax) / (nDims * maxIterations)),
                                              activeImage);
                }

                // Change by Z Cohen on 6/14/04
                // Old code handled the first run through this loop differently than
                // subsequent runs.
                // The first time, initialGuess was set to zero and the boundGuess was
                // set to the bracketBound.  On subsequent runs, initialGuess was set to
                // functionAtBest and boundGuess to 1.
                // Each parsing of this loop, though, is for a different degree of freedom and
                // so the Powell parameters should be the same each the loop starts.
                // Here's the old code:
                //
                // Initialize values for call to lineMinimization.
                //
                // if (count == 0) {
                // // first time through use large bounds (obtained from JDialogRegistrationOAR3D)
                // // lineMinimization will use boundGuess*tolerance to get bracket
                // boundGuess = bracketBound;
                // initialGuess = 0;
                // } else {
                // boundGuess = 1;
                // initialGuess = functionAtBest;
                // }
                //
                // End of old code.
                //
                // Here's the new code:
                boundGuess = bracketBound;
                initialGuess = 0;

                // directions should hold "1" for i and "0" for all other dimensions.
                for (int j = 0; j < nDims; j++) {
                    directions[j] = 0;
                }

                directions[i] = 1;

                // Preferences.debug("Calling lineMinimization for dimension "+i +".\n");
                functionAtBest = lineMinimization(initialGuess, boundGuess, directions);

                if (Math.abs(point[i] - pt[i]) > tolerance[i]) {
                    keepGoing = true;
                }
            } // end of nDims loop

            if (parent.isThreadStopped()) {
                return;
            }

            count++;
        }

        // Preferences.debug("Exited loop with count= " +count +", maxIters " +maxIterations +
        // ", and keepGoing= " +keepGoing +".\n");
        return;
    }

    /**
     * Sets the initial point to the value passed in.
     *
     * @param  initial  Initial point.
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
     * Accessor that sets the progress bar so it can be updated from here.
     *
     * @param  progress  DOCUMENT ME!
     * @param  begin     Value of progress bar when sent here.
     * @param  max       Maximum value allowed.
     */
    public void setProgressBar(ViewJProgressBar progress, int begin, int max) {
        myProgressBar = progress;
        progressBegin = begin;
        progressMax = max;
    }
}
