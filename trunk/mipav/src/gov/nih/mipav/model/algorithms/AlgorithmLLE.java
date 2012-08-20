package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;
import Jama.*;




public class AlgorithmLLE extends AlgorithmBase {

    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    private int numberOfNeighbors;
    private double tol = 1.0E-4; // regularizer

    /**
     * Creates a new AlgorithmLLE object.
     *
     * @param  destImg           list of image models where result image is to stored
     * @param  srcImg            source image model
     
     */
    public AlgorithmLLE(ModelImage destImg, ModelImage srcImg, int numberOfNeighbors, double tol) {
        super(destImg, srcImg);
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
