package gov.nih.mipav.model.algorithms;


import java.util.concurrent.CountDownLatch;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.util.MipavUtil;
import gov.nih.mipav.view.*;


/**
 * Runs Powell's method for a 2D image. Check the parent class comment for more detailed information.
 *
 * @version  0.1 Oct 1, 2001
 * @author   Neva Cherniavsky
 * @author   Matthew McAuliffe
 */
public class AlgorithmPowellOpt2D extends AlgorithmPowellOptBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag indicating this is a rigid transformation. */
    private boolean rigid = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new algorithm with the given centers of mass (needed for setting the transformations), the given
     * cost function (which was constructed with the proper images), the initial point we're looking at, some tolerance
     * within that point to look for the minimum, and the maximum number of iterations.
     *
     * @param  parent           Algorithm that called this optimization.
     * @param  com              Center of Mass of the input image.
     * @param  degreeOfFreedom  Degree of freedom for transformation (must be 2, 3, 4, 5, 7).
     * @param  costFunc         Cost function to use.
     * @param  initial          Initial point to start from, length of 7 for linear or more for nonlinear.
     * @param  tols             Tolerance for each dimension (tols.length == degreeOfFreedom).
     * @param  maxIter          Maximum number of iterations.
     * @param  _rigid           <code>true</code> means this was a rigid transformation
     * @param  bracketBound     DOCUMENT ME!
     */
    public AlgorithmPowellOpt2D(AlgorithmBase parent, Point2Dd com, int degreeOfFreedom,
                                AlgorithmOptimizeFunctionBase costFunc, double[] initial, double[] tols, int maxIter,
                                boolean _rigid, int bracketBound) {
        super(parent, degreeOfFreedom, costFunc, initial, tols, maxIter, 2, bracketBound);

        this.rigid = _rigid;
        toOrigin = new TransMatrix(3);
        toOrigin.setTranslate(com.x, com.y);

        fromOrigin = new TransMatrix(3);
        fromOrigin.setTranslate(-com.x, -com.y);

        finalPoint = new double[start.length];

        // set up initial point properly
        if (degreeOfFreedom == 2) {
            point[0] = initial[1];
            point[1] = initial[2];
        }

        if ((degreeOfFreedom == 3) && (rigid == true)) {
            point[0] = initial[0]; // rotation
            point[1] = initial[1]; // translation x
            point[2] = initial[2]; // translation y
        } else if (degreeOfFreedom == 3) {
            point[0] = initial[3]; // global scaling factor
            point[1] = initial[1]; // translation x
            point[2] = initial[2]; // translation y
        } else if (degreeOfFreedom >= 4) {

            for (int i = 0; i < degreeOfFreedom; i++) {
                point[i] = initial[i]; // 1 rotation, then 2 translations, then 2 scalings, then 2 skews
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Helper method to take the "point" we've been working with as a vector, and convert it into a transformation
     * matrix. The length of the vector should be equal to the global variable nDims, which in turn was initialized by
     * the degrees of freedom originally sent to this algorithm. Therefore, if there are 2 degrees of freedom, we set
     * only the translations; 3 and no rigid means translation and global scaling; 3 and rigid means rotation and
     * translation; 4 means rotation, translation, and global scaling; 5 means rotation, translation, and scaling; and 6
     * means translation, scaling, and skewing.
     *
     * @param   vector  Vector that represented a "point" in the algorithm which needs to be converted to a matrix.
     *
     * @return  The transformation matrix created from the vector.
     */
    public TransMatrix convertToMatrix(double[] vector) {

        double rot = start[0];
        double transX = start[1];
        double transY = start[2];
        double scaleX = start[3];
        double scaleY = start[4];
        double skewX = start[5];
        double skewY = start[6];

        // set up parts of transform properly
        if (vector.length == 2) {
            transX = vector[0];
            transY = vector[1];
        } else if ((vector.length == 3) && (rigid == true)) {
            rot = vector[0];
            transX = vector[1];
            transY = vector[2];
        } else if ((vector.length == 3) && (rigid == false)) {
            scaleX = scaleY = vector[0];
            transX = vector[1];
            transY = vector[2];
        } else if (vector.length == 4) {
            rot = vector[0];
            transX = vector[1];
            transY = vector[2];
            scaleX = scaleY = vector[3];
        } else if (vector.length == 5) {
            rot = vector[0];
            transX = vector[1];
            transY = vector[2];
            scaleX = vector[3];
            scaleY = vector[4];
        } else if (vector.length == 7) {
            rot = vector[0];
            transX = vector[1];
            transY = vector[2];
            scaleX = vector[3];
            scaleY = vector[4];
            skewX = vector[5];
            skewY = vector[6];
        }

        TransMatrix matrix = new TransMatrix(3);

        matrix.setTransform(transX, transY, rot);
        matrix.setSkew(skewX, skewY);
        matrix.setZoom(scaleX, scaleY);

        Matrix mtx = (toOrigin.times(matrix)).times(fromOrigin);

        matrix.convertFromMatrix(mtx);

        return matrix;
    }

    /**
     * Accessor that returns the final point with translations, rotations, scales, and skews representing the best
     * tranformation.
     *
     * @return  A vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public double[] getFinal() {

        for (int i = 0; i < start.length; i++) {
            finalPoint[i] = start[i];
        }

        // nDims = degrees of freedom
        if (nDims == 2) {
            finalPoint[1] = point[0];
            finalPoint[2] = point[1];
        } else if ((nDims == 3) && (rigid == true)) {
            finalPoint[0] = point[0]; // rotation
            finalPoint[1] = point[1]; // translation x
            finalPoint[2] = point[2]; // translation y
        } else if (nDims == 3) {
            finalPoint[3] = finalPoint[4] = point[0]; // global scaling factor
            finalPoint[1] = point[1]; // translation x
            finalPoint[2] = point[2]; // translation y
        } else if (nDims >= 4) {

            for (int i = 0; i < nDims; i++) {
                finalPoint[i] = point[i]; // 1 rotation, then 2 translations, then 2 scalings, then 2 skews

            }

            if (nDims == 4) {
                finalPoint[4] = finalPoint[3];
            }
        }

        return finalPoint;
    }

    /**
     * Accessor that returns the matrix representing the best tranformation.
     *
     * @return  A matrix representing the best transformation.
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
     * @param   sample  DOCUMENT ME!
     *
     * @return  A matrix representing the best transformation.
     */
    public TransMatrix getMatrix(float sample) {

        // will resolution affect scale??
        TransMatrix mat = convertToMatrix(point);
        double transX = mat.get(0, 2) * sample;
        double transY = mat.get(1, 2) * sample;

        mat.set(0, 2, transX);
        mat.set(1, 2, transY);

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
        boolean keepGoing = true;

        functionAtBest = 0;
        double[] pt = new double[nDims];
        while ((count < maxIterations) && keepGoing && (nDims > 0)) {
            keepGoing = false; // Only terminate loop when point changes for ALL directions are less

            // than their respective tolerances.  Will only stay false if ALL are false.

        	fireProgressStateChanged((int)(minProgressValue + count*progressStep));
            System.arraycopy(point, 0, pt, 0, nDims);
            
            final CountDownLatch doneSignal = new CountDownLatch(nDims);

            for (int i = 0; (i < nDims) && !parent.isThreadStopped(); i++) {

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
                int boundGuess = bracketBound;
                double initialGuess = 0;
                final double[] directions = new double[nDims];

                // directions should hold "1" for i and "0" for all other dimensions.
                for (int j = 0; j < nDims; j++) {
                    directions[j] = 0;
                }
                if (i < directions.length) {
                    directions[i] = 1;
                }
                if (count == 0) {
                    boundGuess = bracketBound; // first time through use large bounds

                    // lineMinimization will use boundGuess*tolerance to get bracket
                    initialGuess = 0;
                } else {
                    boundGuess = 1;
                    initialGuess = functionAtBest;
                    // old version of algorithm had boundGuess = 10 or =1
                }

                final int fboundGuess = boundGuess;
                final double finitialGuess = initialGuess;
                // Preferences.debug("Calling lineMinimization for dimension "+i +".\n");
                Runnable task = new Runnable(){
                	public void run(){
                        lineMinimization(point, finitialGuess, fboundGuess, directions);
                        doneSignal.countDown();
                	}
                };
                MipavUtil.mipavThreadPool.execute(task);
            } // end of nDims loop
            try {
                doneSignal.await();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            for(int i = 0; i < nDims; i++){
                if (Math.abs(point[i] - pt[i]) > tolerance[i]){
                	keepGoing = true;
                	break;
                }
            }

            if (parent.isThreadStopped()) {
                return;
            }

            count++;
        }

        // Preferences.debug("Exited loop with count= " +count +" > maxIters" +maxIterations +" and keepGoing= "
        // +keepGoing +".\n");
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
        // nDims = degrees of freedom
        if (nDims == 2) {
            point[0] = initial[1]; // translation x
            point[1] = initial[2]; // translation y
        } else if ((nDims == 3) && (rigid == true)) {
            point[0] = initial[0]; // rotation
            point[1] = initial[1]; // translation x
            point[2] = initial[2]; // translation y
        } else if (nDims == 3) {
            point[0] = initial[3]; // global scaling factor
            point[1] = initial[1]; // translation x
            point[2] = initial[2]; // translation y
        } else if (nDims >= 4) {

            for (int i = 0; i < nDims; i++) {
                point[i] = initial[i]; // 1 rotation, then 2 translations, then 2 scalings, then 2 skews

                //
            }
        }
    }

   

}
