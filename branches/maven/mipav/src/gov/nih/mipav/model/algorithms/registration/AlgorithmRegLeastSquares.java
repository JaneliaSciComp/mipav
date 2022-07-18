package gov.nih.mipav.model.algorithms.registration;


import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import Jama.*;
import de.jtem.numericalMethods.algebra.linear.decompose.Singularvalue;

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
 *           coordsA = double[3][n] or Vector3f[n]</p>
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
     * @param  coordsA3D  Vector3f[n] 3D point set
     * @param  coordsB3D  Vector3f[n] 3D point set
     * @param  dim2or3    indicates whether data set is 2D or 3D
     */
    public AlgorithmRegLeastSquares(Vector3f[] coordsA3D, Vector3f[] coordsB3D, int dim2or3) {
        dim = dim2or3;
        numCoords = -1;
        pointSetA = null;
        pointSetB = null;
        xfrmBA = null;
        Preferences.debug("coordsA3D.length = " + coordsA3D.length + "\n",Preferences.DEBUG_ALGORITHM);

        try {

            if (dim == 2) {
                Vector2f[] coordsA2D = new Vector2f[coordsA3D.length];
                Vector2f[] coordsB2D = new Vector2f[coordsB3D.length];
                Preferences.debug("coordsA3D[0].x=" + coordsA3D[0].X + "\n",Preferences.DEBUG_ALGORITHM);

                for (int i = 0; i < coordsA3D.length; i++) {
                    coordsA2D[i] = new Vector2f((float)coordsA3D[i].X, (float)coordsA3D[i].Y);
                }

                Preferences.debug("assigned A" + "\n",Preferences.DEBUG_ALGORITHM);

                for (int i = 0; i < coordsB3D.length; i++) {
                    coordsB2D[i] = new Vector2f((float)coordsB3D[i].X, (float)coordsB3D[i].Y);
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
    private TransMatrix buildXfrm(double[] p1, double[] p2, Matrix R) {
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
                xfrm.set(i, dim, (float)T[i]); // set last col of xfrm to T
            }

            xfrm.setMatrix(0, dim - 1, 0, dim - 1, R.getArray()); // copy R into dimxdim elements of xfrm

            for (j = 0; j < dim; j++) {
                xfrm.set(dim, j, 0); // set last row of xfrm to 0,0,0,1
            }

            xfrm.set(dim, dim, 1.0);
            Preferences.debug(xfrm.toString(),Preferences.DEBUG_ALGORITHM);

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

            // fireProgressStateChanged(srcImage.getImageName(), "Registering image ...");
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
            double det;
            
            double ratio;
            int numberZeroes;

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

            int m = H.getRowDimension();
            int n = H.getColumnDimension();
            double[][] U = new double[m][n];
            double[][] V = new double[n][n];
            double[] singularValues = new double[Math.min(m+1,n)];

            Singularvalue.decompose( H.getArray(), U, V, singularValues );
            Matrix Vmat = new Matrix(V);
            Matrix Umat = new Matrix(U);
            // X=V*U'
            X = Vmat.times(Umat.transpose());
            
            det = X.det();
            Preferences.debug("det=" + det + "\n",Preferences.DEBUG_ALGORITHM);

            if ((det >= 0.99) && (det <= 1.01)) {
                rotateBA = X.copy();
                xfrmBA = buildXfrm(p1, p2, rotateBA);
                Preferences.debug("Least Squares Succeeded\n",Preferences.DEBUG_ALGORITHM);
                setCompleted(true);
                // return 0;
            } else if ((det <= -0.99) && (det >= -1.01) && (dim == 3)) {
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
                    Preferences.debug("Least Squares Failed. Determinate = -1\n",Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n",
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("The points are too noisy.\n",Preferences.DEBUG_ALGORITHM);

                    // The points are colinear.
                    setCompleted(false);
                    
                } else if (numberZeroes == 1) {
                    ratio = singularValues[2] / singularValues[1];

                    if ((ratio > 0.9999) && (ratio < 1.0001)) {
                        MipavUtil.displayError("Least Squares Failed in 3D colinear case \n");
                        Preferences.debug("Least Squares Failed. Determinate = -1\n",Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n",
                        		Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("The points are colinear.\n",Preferences.DEBUG_ALGORITHM);

                        // The points are colinear.
                        setCompleted(false);
                        
                    }

                    V[0][2] = -V[0][2];
                    V[1][2] = -V[1][2];
                    V[2][2] = -V[2][2];
                    Vmat = new Matrix(V);
                    X = Vmat.times(Umat.transpose());
                    rotateBA = X.copy();
                    xfrmBA = buildXfrm(p1, p2, rotateBA);
                    Preferences.debug("Least Squares Succeeded in coplanar case\n",Preferences.DEBUG_ALGORITHM);
                    setCompleted(true);
                } else if (numberZeroes == 2) {
                    MipavUtil.displayError("Least Squares Failed in 3D colinear case \n");
                    Preferences.debug("Least Squares Failed. Determinate = -1\n",Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n",
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("The points are colinear.\n",Preferences.DEBUG_ALGORITHM);

                    // The points are colinear.
                    setCompleted(false);
                    
                }
            } // if det(X) = -1, this is a degenerate case.  X is a reflection instead of a rotation.

            // The points are colinear.
            else if ((det <= -0.99) && (det >= -1.01)) {

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
                    Preferences.debug("Least Squares Failed. Determinate = -1\n",Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n",
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("The points are too noisy.\n",Preferences.DEBUG_ALGORITHM);
                    setCompleted(false);
                    
                } else if (numberZeroes == 1) {
                    MipavUtil.displayError("Least Squares Failed due to colinear points \n");
                    Preferences.debug("Least Squares Failed. Determinate = -1\n",Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("This is a degenerate case.\n X is a reflection instead of a rotation.\n",
                    		Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("The points are colinear.\n",Preferences.DEBUG_ALGORITHM);

                    // The points are colinear.
                    setCompleted(false);
                    
                }
                // return -1;
            } else {
                MipavUtil.displayError("Least Squares Rounding Problem");
                Preferences.debug("Least Squares Rounding Problem: determinate did not = 1 or -1\n",
                		Preferences.DEBUG_ALGORITHM);
                setCompleted(false);
                // 
                // return -2;
            }
            // 

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Algo Register Least Squares: unable to allocate enough memory");
            // 
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
     * Set transform from point set A to point set B.
     *
     * @param  xfrm  transform to copy
     */
    public void setTransformBtoA(TransMatrix xfrm) {
        xfrmBA = new TransMatrix( xfrm );
    }

    /**
     * Checks if pointSetA is same dimension as pointSetB sets pointSetA and pointSetB.
     *
     * @param   coordsA  first 3D point set
     * @param   coordsB  second 3D point set
     *
     * @return  distinguishing reason for error
     */
    public int setup(double[][] coordsA, double[][] coordsB) {
        int i, j;
        int n = coordsA[0].length;

        if (coordsA[0].length != coordsB[0].length) {
            Preferences.debug("Least Squares ERROR: nA != nB\n",Preferences.DEBUG_ALGORITHM);

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
                    Preferences.debug("pointSetA[" + i + "][" + j + "]=" + pointSetA[i][j] + "\n",Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("pointSetB[" + i + "][" + j + "]=" + pointSetB[i][j] + "\n",Preferences.DEBUG_ALGORITHM);
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
    public int setup(Vector3f[] coordsA, Vector3f[] coordsB) {
        int i, j;
        int n = coordsA.length;

        if (coordsA.length != coordsB.length) {
            Preferences.debug("Least Squares ERROR: nA != nB\n",Preferences.DEBUG_ALGORITHM);

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
                pointSetA[0][j] = coordsA[j].X;
                pointSetA[1][j] = coordsA[j].Y;
                pointSetA[2][j] = coordsA[j].Z;
                pointSetB[0][j] = coordsB[j].X;
                pointSetB[1][j] = coordsB[j].Y;
                pointSetB[2][j] = coordsB[j].Z;
            }

            for (i = 0; i < 3; i++) {

                for (j = 0; j < numCoords; j++) {
                    Preferences.debug("pointSetA[" + i + "][" + j + "]=" + pointSetA[i][j] + "\n",Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("pointSetB[" + i + "][" + j + "]=" + pointSetB[i][j] + "\n",Preferences.DEBUG_ALGORITHM);
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
    public int setup(Vector2f[] coordsA, Vector2f[] coordsB) {
        int i, j;
        int n = coordsA.length;

        if (coordsA.length != coordsB.length) {
            Preferences.debug("Least Squares ERROR: nA != nB\n",Preferences.DEBUG_ALGORITHM);

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
                pointSetA[0][j] = coordsA[j].X;
                pointSetA[1][j] = coordsA[j].Y;
                pointSetB[0][j] = coordsB[j].X;
                pointSetB[1][j] = coordsB[j].Y;
            }

            for (i = 0; i < 2; i++) {

                for (j = 0; j < numCoords; j++) {
                    Preferences.debug("pointSetA[" + i + "][" + j + "]=" + pointSetA[i][j] + "\n",Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("pointSetB[" + i + "][" + j + "]=" + pointSetB[i][j] + "\n",Preferences.DEBUG_ALGORITHM);
                }
            }

            return 0;
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Algo Register Least Squares: unable to allocate enough memory");

            return -1;
        }
    }

}
