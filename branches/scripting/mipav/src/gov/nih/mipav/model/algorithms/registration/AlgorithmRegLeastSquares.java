package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;


/**
 * "Least-Squares Fitting of 2 3-D Point Sets", K. S. Arun PAMI-9(5), September, 1987 (698-700)
 *
 * <p>Rotation and translation but not scale. That is, this is a rigid transformation based on two homologous datasets.
 * </p>
 *
 * @version  1.0 October 1998
 * @author   Delia McGarry idea to use point matching for surgical cases is from the Virginia Neurological Institute,
 *           Charlottesville, VA Registers 1. 2 sets of 2D points (number of points in sets should be equal and greater
 *           or equal to 3 and non-colinear) 2. 2 sets of 3D points (number of points in sets should be equal and
 *           greater or equal to 4 and non-colinear) Software can handle cases which are are coplanar but are not
 *           colinear.
 *
 *           <p>USAGE: AlgorithmleastSquares LSMatch = new AlgorithmLeastSquares(coordsA, coordsB); LSMatch.run(); where
 *           coordsA = double[3][n] or Point3Dd[n]</p>
 *
 *           <p>USAGE FOR SUBSEQUENT EXECUTIONS: LSMatch.setup(coordsA, coordsB); LSMatch.run();</p>
 */
public class AlgorithmRegLeastSquares extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int numCoords;
    
    /** The number of dimensions (2D or 3D). */
    private int dim;


    /** Input point set A (2D or 3D) */
    private double[][] pointSetA;
    
    /** Input point set B (2D or 3D) */
    private double[][] pointSetB;


    /** The transformation matrix that describes the transform from point set B to point set A. */
    private TransMatrix xfrmBA;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmRegLeastSquares - Constructor.
     *
     * @param  coordsA  - double[dim][n] 3D or 2D point set
     * @param  coordsB  - double[dim][n] 3D or 2D point set
     * @param  dim2or3  - indicates whether data set is 2D or 3D
     */
    public AlgorithmRegLeastSquares(double[][] coordsA, double[][] coordsB, int dim2or3) {
        dim = dim2or3;
        numCoords = -1;
        pointSetA = null;
        pointSetB = null;
        xfrmBA = null;
        setup(coordsA, coordsB);
    }

    /**
     * AlgorithmRegLeastSquares - Constructor.
     *
     * @param  coordsA3D  Point3Dd[n] 3D point set
     * @param  coordsB3D  Point3Dd[n] 3D point set
     * @param  dim2or3    indicates whether data set is 2D or 3D
     */
    public AlgorithmRegLeastSquares(Point3Dd[] coordsA3D, Point3Dd[] coordsB3D, int dim2or3) {
        dim = dim2or3;
        numCoords = -1;
        pointSetA = null;
        pointSetB = null;
        xfrmBA = null;
        Preferences.debug("coordsA3D.length = " + coordsA3D.length + "\n");

        try {

            if (dim == 2) {
                Point2Dd[] coordsA2D = new Point2Dd[coordsA3D.length];
                Point2Dd[] coordsB2D = new Point2Dd[coordsB3D.length];
                Preferences.debug("coordsA3D[0].x=" + coordsA3D[0].x + "\n");

                for (int i = 0; i < coordsA3D.length; i++) {
                    coordsA2D[i] = new Point2Dd(coordsA3D[i].x, coordsA3D[i].y);
                }

                Preferences.debug("assigned A" + "\n");

                for (int i = 0; i < coordsB3D.length; i++) {
                    coordsB2D[i] = new Point2Dd(coordsB3D[i].x, coordsB3D[i].y);
                }

                setup(coordsA2D, coordsB2D);
            } else if (dim == 3) {
                setup(coordsA3D, coordsB3D);
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Algo Register Least Squares: unable to allocate enough memory");

            return;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * SQR = x^2.
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static double SQR(double x) {
        x *= x;

        return x;
    }

    /**
     * Builds 4x4 transformation matrix from R and T where T=p2-R*p1.
     *
     * @param   p1  from Match
     * @param   p2  from Match
     * @param   R   =rotation matrix from Match
     *
     * @return  DOCUMENT ME!
     */
    public TransMatrix buildXfrm(double[] p1, double[] p2, Matrix R) {
        int i, j;

        try {
            double[] T = new double[dim]; // translation parameters
            TransMatrix xfrm = new TransMatrix(dim + 1);
            Matrix P1 = new Matrix(dim, 1);

            for (i = 0; i < dim; i++) {
                P1.set(i, 0, p1[i]);
            }

            P1 = R.times(P1);

            for (i = 0; i < dim; i++) {
                T[i] = p2[i] - P1.get(i, 0); // T=p2-R*p1
                xfrm.set(i, dim, T[i]); // set last col of xfrm to T
            }

            xfrm.setMatrix(0, dim - 1, 0, dim - 1, R); // copy R into dimxdim elements of xfrm

            for (j = 0; j < dim; j++) {
                xfrm.set(dim, j, 0); // set last row of xfrm to 0,0,0,1
            }

            xfrm.set(dim, dim, 1.0);
            Preferences.debug(xfrm.toString());

            return xfrm;
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Algo Register Least Squares: unable to allocate enough memory");

            return null;
        }
    }

    /**
     * CalculateResiduals.
     */
    public void calculateResiduals() {
        int i, j;
        double[] ptB = new double[dim];
        double[] ptA = new double[dim];
        double[] ptBT = new double[dim];
        double[] residual = new double[numCoords];

        for (j = 0; j < numCoords; j++) {

            for (i = 0; i < dim; i++) {
                ptB[i] = pointSetB[i][j];
                ptA[i] = pointSetA[i][j];
            }

            xfrmBA.transform(ptB, ptBT);
            residual[j] = EuclideanDistance(ptA, ptBT);
            ViewUserInterface.getReference().setDataText("Residual[" + j + "] = " + residual[j] + "\n");
        }
    }

    /**
     * EuclideanDistance.
     *
     * @param   ptA  DOCUMENT ME!
     * @param   ptB  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double EuclideanDistance(double[] ptA, double[] ptB) {
        double dist = 0;
        int i;
        double sum = 0;

        if (ptA.length != ptB.length) {
            MipavUtil.displayError("Residual error");
        } else {

            for (i = 0; i < ptA.length; i++) {
                sum += SQR(ptA[i] - ptB[i]);
            }

            dist = Math.sqrt(sum);
        }

        return dist;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Accesses xfrm from point set B to point set A.
     *
     * @return  xfrmBA
     */
    public TransMatrix getTransformBtoA() {
        return xfrmBA;
    }

    /**
     * matchBtoA pA and pB are 2 3D point sets (pointSetA and B).
     *
     * <p>p1[i]=(1/numCoords)*(sum from j = 0 to j = numCoords - 1 of pB[i][j]) p2[i]=(1/numCoords)*(sum from j = 0 to j
     * = numCoords - 1 of pA[i][j])</p>
     *
     * <p>q1[i][j] =pB[i][j]-p1[i] q2[i][j]=pA[i][j]-p2[i]</p>
     */
    public void matchBtoA() {

        try {
            int i, j;

            // buildProgressBar(srcImage.getImageName(), "Registering image ...", 0, 100);
            // initProgressBar();
            // set p1 and p2 to zeros
            double[] p1 = new double[dim];
            double[] p2 = new double[dim];
            double[][] q1 = new double[dim][numCoords];
            double[][] q2 = new double[dim][numCoords];
            Matrix Q1, Q2;
            Matrix H;
            Matrix X;
            Matrix rotateBA;
            SingularValueDecomposition SVD;
            double det;
            double[] singularValues;
            double ratio;
            int numberZeroes;
            boolean equalValues = false;
            Matrix V;
            double[][] vArray;

            for (i = 0; i < dim; i++) {
                p1[i] = 0;
                p2[i] = 0;
            }

            for (i = 0; i < dim; i++) {

                for (j = 0; j < numCoords; j++) {
                    p1[i] += pointSetB[i][j];
                    p2[i] += pointSetA[i][j];
                }

                p1[i] *= 1 / ((double) numCoords);
                p2[i] *= 1 / ((double) numCoords);
            }

            for (i = 0; i < dim; i++) {

                for (j = 0; j < numCoords; j++) {
                    q1[i][j] = pointSetB[i][j] - p1[i];
                    q2[i][j] = pointSetA[i][j] - p2[i];
                }
            }

            Q1 = new Matrix(q1, dim, numCoords);
            Q2 = new Matrix(q2, dim, numCoords);
            H = Q1.times(Q2.transpose());
            SVD = H.svd();

            // X=V*U'
            X = SVD.getV().times(SVD.getU().transpose());
            det = X.det();
            Preferences.debug("det=" + det + "\n");

            if ((det >= 0.99) && (det <= 1.01)) {
                rotateBA = X.copy();
                xfrmBA = buildXfrm(p1, p2, rotateBA);
                Preferences.debug("Least Squares Succeeded\n");
                setCompleted(true);
                // return 0;
            } else if ((det <= -0.99) && (det >= -1.01) && (dim == 3)) {
                singularValues = SVD.getSingularValues();

                // The singular values are ordered so that sigma[0] >=
                // sigma[1] >= ... >= sigma[n-1] >= 0
                // If one and only one of the singular values is zero,
                // then the case is coplanar but not colinear and the
                // rotation can be performed
                numberZeroes = 0;

                for (i = 0; i < 3; i++) {

                    if ((singularValues[i] >= (-1.0E3 * Float.MIN_VALUE)) &&
                            (singularValues[i] <= (1.0E3 * Float.MIN_VALUE))) {
                        numberZeroes++;
                    }
                }

                if (numberZeroes == 0) {
                    MipavUtil.displayError("Least Squares Failed due to excessive noise \n");
                    Preferences.debug("Least Squares Failed. Determinate = -1\n");
                    Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n");
                    Preferences.debug("The points are too noisy.\n");

                    // The points are colinear.
                    setCompleted(false);
                    disposeProgressBar();
                } else if (numberZeroes == 1) {
                    ratio = singularValues[2] / singularValues[1];

                    if ((ratio > 0.9999) && (ratio < 1.0001)) {
                        MipavUtil.displayError("Least Squares Failed in 3D colinear case \n");
                        Preferences.debug("Least Squares Failed. Determinate = -1\n");
                        Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n");
                        Preferences.debug("The points are colinear.\n");

                        // The points are colinear.
                        setCompleted(false);
                        disposeProgressBar();
                    }

                    V = SVD.getV();
                    vArray = V.getArray();
                    vArray[0][2] = -vArray[0][2];
                    vArray[1][2] = -vArray[1][2];
                    vArray[2][2] = -vArray[2][2];
                    V.setArray(vArray);
                    X = V.times(SVD.getU().transpose());
                    rotateBA = X.copy();
                    xfrmBA = buildXfrm(p1, p2, rotateBA);
                    Preferences.debug("Least Squares Succeeded in coplanar case\n");
                    setCompleted(true);
                } else if (numberZeroes == 2) {
                    MipavUtil.displayError("Least Squares Failed in 3D colinear case \n");
                    Preferences.debug("Least Squares Failed. Determinate = -1\n");
                    Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n");
                    Preferences.debug("The points are colinear.\n");

                    // The points are colinear.
                    setCompleted(false);
                    disposeProgressBar();
                }
            } // if det(X) = -1, this is a degenerate case.  X is a reflection instead of a rotation.

            // The points are colinear.
            else if ((det <= -0.99) && (det >= -1.01)) {
                singularValues = SVD.getSingularValues();

                // The singular values are ordered so that sigma[0] >=
                // sigma[1] >= ... >= sigma[n-1] >= 0
                // If one and only one of the singular values is zero,
                // then the case is coplanar but not colinear and the
                // rotation can be performed
                numberZeroes = 0;

                for (i = 0; i < 3; i++) {

                    if ((singularValues[i] >= (-1.0E3 * Float.MIN_VALUE)) &&
                            (singularValues[i] <= (1.0E3 * Float.MIN_VALUE))) {
                        numberZeroes++;
                    }
                }

                if (numberZeroes == 0) {
                    MipavUtil.displayError("Least Squares Failed due to noisy data \n");
                    Preferences.debug("Least Squares Failed. Determinate = -1\n");
                    Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n");
                    Preferences.debug("The points are too noisy.\n");
                    setCompleted(false);
                    disposeProgressBar();
                } else if (numberZeroes == 1) {
                    MipavUtil.displayError("Least Squares Failed due to colinear points \n");
                    Preferences.debug("Least Squares Failed. Determinate = -1\n");
                    Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n");
                    Preferences.debug("The points are colinear.\n");

                    // The points are colinear.
                    setCompleted(false);
                    disposeProgressBar();
                }
                // return -1;
            } else {
                MipavUtil.displayError("Least Squares Rounding Problem");
                Preferences.debug("Least Squares Rounding Problem: determinate did not = 1 or -1\n");
                setCompleted(false);
                // disposeProgressBar();
                // return -2;
            }
            // disposeProgressBar();

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Algo Register Least Squares: unable to allocate enough memory");
            // disposeProgressBar();
            // return -3;
        }
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        matchBtoA();
    }

    /**
     * Accesses xfrm from point set A to point set B.
     *
     * @param  xfrm  DOCUMENT ME!
     */
    public void setTransformBtoA(TransMatrix xfrm) {
        xfrmBA = (TransMatrix) xfrm.copy();
    }

    /**
     * Checks if pointSetA is same dimension as pointSetB sets pointSetA and pointSetB.
     *
     * @param   coordsA  first 3D point set
     * @param   coordsB  second 3D point set
     *
     * @return  int distinguishing reason for error
     */
    public int setup(double[][] coordsA, double[][] coordsB) {
        int i, j;
        int n = coordsA[0].length;

        if (coordsA[0].length != coordsB[0].length) {
            Preferences.debug("Least Squares ERROR: nA != nB\n");

            return -1;
        }

        // if new algoLS or if point sets of last run are not same
        // dim as new point sets then clear arrays and reallocate
        try {

            if ((numCoords == -1) || (numCoords != n)) {
                pointSetA = null;
                pointSetB = null;
                numCoords = n;
                pointSetA = new double[dim][numCoords];
                pointSetB = new double[dim][numCoords];
            }

            pointSetA = coordsA;
            pointSetB = coordsB;

            for (j = 0; j < numCoords; j++) {

                for (i = 0; i < dim; i++) {
                    Preferences.debug("pointSetA[" + i + "][" + j + "]=" + pointSetA[i][j] + "\n");
                    Preferences.debug("pointSetB[" + i + "][" + j + "]=" + pointSetB[i][j] + "\n");
                }
            }

            return 0;
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Algo Register Least Squares: unable to allocate enough memory");

            return -1;
        }
    }

    /**
     * Checks if pointSetA is same dimension as pointSetB sets pointSetA and pointSetB.
     *
     * @param   coordsA  first 3D point set
     * @param   coordsB  second 3D point Set
     *
     * @return  int distinguishing reason for error
     */
    public int setup(Point3Dd[] coordsA, Point3Dd[] coordsB) {
        int i, j;
        int n = coordsA.length;

        if (coordsA.length != coordsB.length) {
            Preferences.debug("Least Squares ERROR: nA != nB\n");

            return -1;
        }

        // if new algoLS or if point sets of last run are not same
        // dim as new point sets then clear arrays and reallocate
        try {

            if ((numCoords == -1) || (numCoords != n)) {
                pointSetA = null;
                pointSetB = null;
                numCoords = n;
                pointSetA = new double[3][numCoords];
                pointSetB = new double[3][numCoords];
            }

            for (j = 0; j < numCoords; j++) {
                pointSetA[0][j] = coordsA[j].x;
                pointSetA[1][j] = coordsA[j].y;
                pointSetA[2][j] = coordsA[j].z;
                pointSetB[0][j] = coordsB[j].x;
                pointSetB[1][j] = coordsB[j].y;
                pointSetB[2][j] = coordsB[j].z;
            }

            for (i = 0; i < 3; i++) {

                for (j = 0; j < numCoords; j++) {
                    Preferences.debug("pointSetA[" + i + "][" + j + "]=" + pointSetA[i][j] + "\n");
                    Preferences.debug("pointSetB[" + i + "][" + j + "]=" + pointSetB[i][j] + "\n");
                }
            }

            return 0;
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Algo Register Least Squares: unable to allocate enough memory");

            return -1;
        }
    }

    /**
     * Checks if pointSetA is same dimension as pointSetB sets pointSetA and pointSetB.
     *
     * @param   coordsA  first 2D point set
     * @param   coordsB  second 2D point Set
     *
     * @return  int distinguishing reason for error
     */
    public int setup(Point2Dd[] coordsA, Point2Dd[] coordsB) {
        int i, j;
        int n = coordsA.length;

        if (coordsA.length != coordsB.length) {
            Preferences.debug("Least Squares ERROR: nA != nB\n");

            return -1;
        }

        // if new algoLS or if point sets of last run are not same
        // dim as new point sets then clear arrays and reallocate
        try {

            if ((numCoords == -1) || (numCoords != n)) {
                pointSetA = null;
                pointSetB = null;
                numCoords = n;
                pointSetA = new double[2][numCoords];
                pointSetB = new double[2][numCoords];
            }

            for (j = 0; j < numCoords; j++) {
                pointSetA[0][j] = coordsA[j].x;
                pointSetA[1][j] = coordsA[j].y;
                pointSetB[0][j] = coordsB[j].x;
                pointSetB[1][j] = coordsB[j].y;
            }

            for (i = 0; i < 2; i++) {

                for (j = 0; j < numCoords; j++) {
                    Preferences.debug("pointSetA[" + i + "][" + j + "]=" + pointSetA[i][j] + "\n");
                    Preferences.debug("pointSetB[" + i + "][" + j + "]=" + pointSetB[i][j] + "\n");
                }
            }

            return 0;
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Algo Register Least Squares: unable to allocate enough memory");

            return -1;
        }
    }

}
