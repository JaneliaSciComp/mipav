package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.SelectedEigenvalue;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

import Jama.*;




public class AlgorithmLLE extends AlgorithmBase {
    
    /** This is a port of the file lle.m created by Professor Lawrence K. Saul
     * 
     *  References:
     *  1.) Nonlinear Dimensionality Reduction by Locally Linear Embedding by Sam T. Roweis and Lawrence K Saul,
     *  Science, Vol. 290, December 22, 2000, pp. 2323 - 2326.
     *  
     *  2.) Think Globally, Fit Locally: Unsupervised Learning of Low Dimensional Manifolds by Lawrence K. Saul
     *  and Sam T. Roweis, Journal of Machine Learning Research, Vol. 4, 2003, pp. 119 - 155.
     *  
     *  3.) Unsupervised Learning of Image Manifolds by Semidefinite Programming by Kilian Q. Weinberger and
     *  Lawrence K. Saul. 
     */

    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    private int numberOfNeighbors;  // Reference 2 uses examples with values of 4, 8, 12, 18, and 24.
                                    // Reference 3 uses examples with values of 4 an 6.
    private double tol = 1.0E-4; // regularizer
    private int d = 2; // embedding dimensionality

    /**
     * Creates a new AlgorithmLLE object.
     *
     * @param  destImg           list of image models where result image is to stored
     * @param  srcImg            source image model
       @param d                  Number of embedded dimensions
       @param numberOfNeighbors  Number of neighbors
       @param tol                regularizer
     */
    public AlgorithmLLE(ModelImage destImg, ModelImage srcImg, int d, int numberOfNeighbors, double tol) {
        super(destImg, srcImg);
        this.d = d;
        this.numberOfNeighbors = numberOfNeighbors;
        this.tol = tol;
        
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        int xDim;
        int yDim;
        int zDim;
        int sliceSize;
        int newXDim;
        int newYDim;
        double X[][];
        int z;
        double sliceBuffer[];
        int i, j, k;
        double X2[];
        double repmat1[][];
        double repmat2[][];
        Matrix matX;
        double XpX[][];
        double two[][];
        Matrix twoMat;
        double distance[][];
        ArrayList<distanceIndexItem> colList[];
        int index[][];
        int neighbors[][];
        double W[][];
        double zd[][];
        double C[][];
        Matrix matZ;
        double traceC;
        double prod;
        Matrix matC;
        double invC[][];
        double sumInvC[];
        double sumSumInvC;
        double M[][];
        double w[][];
        int jV[];
        Matrix matw;
        double wpw[][];
        char jobz;
        char range;
        char uplo;
        int il;
        int iu;
        double abstol;
        GeneralizedEigenvalue ge;
        int numEigenvaluesFound[] = new int[1];
        double eigenvalues[];
        double eigenvectors[][];
        double work[];
        int lwork;
        int iwork[];
        int ifail[];
        int info[] = new int[1];
        SelectedEigenvalue se;

        if (srcImage == null) {
            displayError("LLE: Source Image is null");

            return;
        }
        
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        sliceBuffer = new double[sliceSize];
        
        newXDim = zDim;
        newYDim = xDim * yDim;
        X = new double[newYDim][zDim];
        
        for (z = 0; z < zDim; z++) {
            try {
                srcImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException on srcImage.exportData" + e);
                setCompleted(false);
                return;
            }
            
            for (i = 0; i < sliceSize; i++) {
                X[i][z] = sliceBuffer[i];
            }
        } // for (z = 0; z < zDim; z++)
        sliceBuffer = null;
        
        // PAIRWISE DISTANCES
        X2 = new double[zDim];
        for (i = 0; i < zDim; i++) {
            for (j = 0; j < newYDim; j++) {
                X2[i] += X[j][i]*X[j][i];    
            }
        }
        
        repmat1 = new double[newXDim][newXDim];
        for (i = 0; i < zDim; i++) {
            for (j = 0; j < zDim; j++) {
                // Each value is placed into zDim rows
                repmat1[i][j] = X2[j];
            }
        }
        
        repmat2 = new double[newXDim][newXDim];
        for (i = 0; i < zDim; i++) {
            for (j = 0; j < zDim; j++) {
                // Each value is placed into zDim columns
                repmat2[j][i] = X2[j];
            }
        }
        
        two = new double[zDim][zDim];
        for (i = 0; i < zDim; i++) {
            two[i][i] = 2.0;
        }
        twoMat = new Matrix(two);
        
        matX = new Matrix(X);
        XpX = (twoMat.times((matX.transpose()).times(matX))).getArray();
        
        distance = new double[zDim][zDim];
        for (i = 0; i < zDim; i++) {
            for (j = 0; j < zDim; j++) {
                distance[i][j] = repmat1[i][j] + repmat2[i][j] - XpX[i][j];
            }
        }
        
        // NEIGHBORS
        colList = new ArrayList[zDim];
        
        // Process each column
        for (j = 0; j < zDim; j++) {
            colList[j] = new ArrayList<distanceIndexItem>();
            for (i = 0; i < zDim; i++) {
                colList[j].add(new distanceIndexItem(distance[i][j],i));
            }
        }
        
        for (j = 0; j <zDim; j++) {
            Collections.sort(colList[j], new distanceIndexComparator());
        }
        
        index = new int[zDim][zDim];
        for (j = 0; j < zDim; j++) {
            for (i = 0; i < zDim; i++) {
                index[i][j] = colList[j].get(i).getIndex();
            }
        }
        
        neighbors = new int[numberOfNeighbors][zDim];
        for (i = 1; i <= numberOfNeighbors; i++) {
            for (j = 0; j < zDim; j++) {
                neighbors[i-1][j] = index[i][j];
            }
        }
        
        // RECONSTRUCTION WEIGHTS
        W = new double[numberOfNeighbors][zDim];
        zd = new double[newYDim][numberOfNeighbors];
        sumInvC = new double[numberOfNeighbors];
        for (i = 0; i < zDim; i++) {
            for (j = 0; j < newYDim; j++) {
                for (k = 0; k < numberOfNeighbors; k++) {
                  zd[j][k] =  X[j][neighbors[k][i]] - X[j][i];     
                }
            }
            matZ = new Matrix(zd);
            C = ((matZ.transpose()).times(matZ)).getArray();
            traceC = 0.0;
            for (j = 0; j < numberOfNeighbors; j++) {
                traceC += C[j][j];
            }
            prod = tol * traceC/numberOfNeighbors;
            // Regularization
            for (j = 0; j < numberOfNeighbors; j++) {
                C[j][j] += prod;
            }
            matC = new Matrix(C);
            invC = (matC.inverse()).getArray();
            for (k = 0; k < numberOfNeighbors; k++) {
                sumInvC[k] = 0.0;
                for (j = 0; j < numberOfNeighbors; j++) {
                    sumInvC[k] += invC[j][k];
                }
            }
            sumSumInvC = 0.0;
            for (j = 0; j < numberOfNeighbors; j++) {
                sumSumInvC += sumInvC[j];
            }
            for (j = 0; j < numberOfNeighbors; j++) {
                W[j][i] = sumInvC[j]/sumSumInvC;
            }
        } // for (i = 0; i < zDim; i++)
        
        // COST MATRIX
        M = new double[zDim][zDim];
        for (i = 0; i < zDim; i++) {
            M[i][i] = 1.0;
        }
        w = new double[numberOfNeighbors][1];
        jV = new int[numberOfNeighbors];
        for (i = 0; i < zDim; i++) {
            for (j = 0; j < numberOfNeighbors; j++) {
                w[j][0] = W[j][i];
            }
            for (j = 0; j < numberOfNeighbors; j++) {
                jV[j] = neighbors[j][i];
            }
            for (j = 0; j < numberOfNeighbors; j++) {
                M[i][jV[j]] = M[i][jV[j]] - w[j][0];
            }
            for (j = 0; j < numberOfNeighbors; j++) {
                M[jV[j]][i] = M[jV[j]][i] - w[j][0];
            }
            matw = new Matrix(w);
            wpw = (matw.times(matw.transpose())).getArray();
            for (j = 0; j < numberOfNeighbors; j++) {
                for (k = 0; k < numberOfNeighbors; k++) {
                    M[jV[j]][jV[k]] = M[jV[j]][jV[k]] + wpw[j][k];
                }
            }
        } // for (i = 0; i < zDim; i++)
        
        // CALCULATION OF EMBEDDING
        jobz = 'V'; // Compute eigenvalues and eigenvectors
        range = 'I'; // The il-th through iu-th eigenvectors will be found
        uplo = 'U'; // Upper triangle of A is stored.  'L' for lower triangle would be equally acceptable.
        // n, the order of array A = zDim;
        // Use M for array A.
        // Use zDim for lda, the leading dimension of array A.
        // vl and vu are not used for range = 'I'.
        il = 2; // smallest eigenvalue returned
        iu = d+1; // largest eigenvalue returned
        ge = new GeneralizedEigenvalue();
        abstol = 2.0 * ge.dlamch('S'); // The absolute error tolerance for eigenvalues
                                       // Eigenvalues will be computed most accurately when abstol is set to twice the underflow
                                       // threshold 2*dlamch('S'), not zero.  
        ge = null;
        // numEigenvaluesFound[0] should return d;
        eigenvalues = new double[zDim];// will return the found eigenvalues in the first d elements in ascending order
        eigenvectors = new double[zDim][d];
        // Use zDim for ldz
        work = new double[8 * zDim]; //  double workspace
        lwork = 8 * zDim; // length of work 
        iwork = new int[5 * zDim]; // int workspace
        ifail = new int[zDim]; // If info[0] > 0, contains the indices of the eigenvectors that failed to converge
        // info output int[] of dimension 1.  
        //        = 0:  successful exit
        //        < 0:  if info[0] = -i, the i-th argument had an illegal value
        //        > 0:  if info[0] = i, then i eigenvectors failed to converge.
        //        Their indices are stored in array ifail.
        se = new SelectedEigenvalue();
        se.dsyevx(jobz, range, uplo, zDim, M, zDim, 0.0, 0.0, il, iu, abstol, numEigenvaluesFound,
                  eigenvalues, eigenvectors, zDim, work, lwork, iwork, ifail, info);
        if (info[0] < 0) {
            // illegal argument
            setCompleted(false);
            return;
        }
        else if (info[0] > 0) {
            System.out.println(info[0] + " eigenvectors failed to converge");
            Preferences.debug(info[0] + " eigenvectors failed to converge\n", Preferences.DEBUG_ALGORITHM);
            System.out.println("The zero based slices of the eigenvectors that failed to converge are:\n");
            Preferences.debug("The zero based slices of the eigenvectors that failed to converge are:\n", Preferences.DEBUG_ALGORITHM);
            for (i = 0; i < info[0]; i++) {
                System.out.println("Slice = " + (ifail[i]-1));
                Preferences.debug("Slice = " + (ifail[i]-1) + "\n", Preferences.DEBUG_ALGORITHM);
            }
        }
        else {
            System.out.println("info[0] = 0 indicating a successful exit");
            Preferences.debug("info[0] = 0 indicating a successful exit\n", Preferences.DEBUG_ALGORITHM);
        }
        
        if (numEigenvaluesFound[0] == d) {
            System.out.println("The number of eigenvalues found equals the number of embedded dimensions = " + d + " as expected");
            Preferences.debug("The number of eigenvalues found equals the number of embedded dimensions = " + d + " as expected\n",
                             Preferences.DEBUG_ALGORITHM);
        }
        else {
            System.out.println("The number of eigenvalues found = " + numEigenvaluesFound[0]);
            System.out.println("This is less than the number of embedded dimensions = " + d);
            Preferences.debug("The number of eigenvalues found = " + numEigenvaluesFound[0] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("This is less than the number of embedded dimensions = " + d + "\n", Preferences.DEBUG_ALGORITHM);
        }
    } // runAlgorithm
    
    private class distanceIndexComparator implements Comparator<distanceIndexItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final distanceIndexItem o1, final distanceIndexItem o2) {
            final double a = o1.getDistance();
            final double b = o2.getDistance();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }
    
    private class distanceIndexItem {

        /** DOCUMENT ME! */
        private final double distance;

        /** DOCUMENT ME! */
        private final int index;

        /**
         * Creates a new positionWieghtItem object.
         * 
         * @param position
         * @param weight
         */
        public distanceIndexItem(final double distance, final int index) {
            this.distance = distance;
            this.index = index;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getDistance() {
            return distance;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getIndex() {
            return index;
        }

    }


    
}
