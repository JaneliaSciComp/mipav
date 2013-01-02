package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/**
 * 
 * @author William Gandler
 * This program is an implementation of the algorithm described in "On Spectral Clustering: Analysis and an algorithm"
 * by Andrew Y. Ng, Michael I. Jordan, and Yair Weiss.
 *
 */
public class AlgorithmSpectralClustering extends AlgorithmBase {
    
//~ Instance fields ------------------------------------------------------------------------------------------------
    
    private ModelImage image;
    
 // K-means, global k-means, or fast global k-means
    private int algoSelection;
    
    // Take resolutions from any black and white image.
    // Use 1.0 in every dimension if not scaled or if colored image.
    // Subscript goes from 0 to nDims - 1 for black and white
    // and for 0 to 1 for color.
    private double scale[];
    
    // First subscript x = 0, y = 1, z = 2, t = 3
    // Second subscript 0 to nPoints-1 for each point
    // Value is the point position
    private double[][] pos;
    
    // subscript goes from 0 to nPoints-1 for each point
    // Value is the cluster number from 0 to numberClusters-1.
    private int[] groupNum;
    
    // subscript goes form 0 to nPoints-1 for each point
    // Value is the weight or number of occurrences of each point
    private double[] weight;
    
    // First subscript is number of dimensions used in spectral clustering = numberClusters
    // Second subscript 0 to numberClusters-1 for each cluster
    // Value is the cluster position
    private double[][] centroidPos;
    
    private String resultsFileName;
    
    private int initSelection;
    
    private float redBuffer[] = null;
    
    private float greenBuffer[] = null;
    
    private float blueBuffer[] = null;
    
    // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
    private double scaleMax = 255.0;
    
    // If true, each color is weighed proportional to its frequency
    private boolean useColorHistogram = false;
    
    // Euclidean squared using means or city-block using median
    private int distanceMeasure;
    
    private boolean scaleVariablesToUnitVariance;
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmSpectralClustering(ModelImage image, double[][] pos, double[] scale, int groupNum[],
            double[][] centroidPos, String resultsFileName, float[] redBuffer,
            float[] greenBuffer, float[] blueBuffer, double scaleMax) {
        this.image = image;
        this.pos = pos;
        this.scale = scale;
        this.groupNum = groupNum;
        this.centroidPos = centroidPos;
        this.resultsFileName = resultsFileName;
        this.redBuffer = redBuffer;
        this.greenBuffer = greenBuffer;
        this.blueBuffer = blueBuffer;
        this.scaleMax = scaleMax;
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
        super.finalize();
    }

    

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        int nDims;
        int nPoints;
        int numberClusters;
        // Initialize sigma with 1.0
        double sigma = 1.0;
        double twoSigmaSquared;
        int i;
        int j;
        int k;
        int m;
        double diff;
        double distSquared;
        
        nDims = pos.length;
        nPoints = pos[0].length;
        // centroidPos is [numberClusters][numberClusters]
        numberClusters = centroidPos.length;
        double A[][] = new double[nPoints][nPoints];
        double scale2[] = new double[scale.length];
        for (i = 0; i < scale.length; i++) {
            scale2[i] = scale[i]*scale[i];
        }
        double D[][] = new double[nPoints][nPoints];
        double L[][] = new double[nPoints][nPoints];
        double eigenvalue[] = new double[nPoints];
        double V[][] = new double[nPoints][nPoints];
        double X[][] = new double[nPoints][numberClusters]; 
        boolean found[] = new boolean[nPoints];
        double largestEigenvalue;
        int largestIndex = 0;
        double Y[][] = new double[nPoints][numberClusters];
        double rowSize;
        double innerProduct;
        double absInnerProduct;
        double smallestAbsInnerProduct;
        int smallestIndex = 0;
        
        twoSigmaSquared = 2.0 * sigma * sigma;
        // Form the affinity matrix A defined by Aij = exp(-||si - sj||**2/(2*sigma**2)) if i != j, and Aii = 0
        for (i = 0; i < nPoints; i++) {
            for (j = i+1; j < nPoints; j++) {
                distSquared = 0.0;
                for (k = 0; k < nDims; k++) {
                    diff = pos[k][i] - pos[k][j];
                    distSquared += scale2[k]*diff*diff;    
                }
                A[i][j] = Math.exp(-distSquared/twoSigmaSquared);
                A[j][i] = A[i][j];
            } // for (j = i+1; j < nPoints; j++)
        } // for (i = 0; i < nPoints; i++)
        
        // Define D to be the diagonal matrix whose (i,i)-element is the sum of A's i-th row,
        // and construct the matrix L = D**(-1/2) A D**(-1/2)
        for (i = 0; i < nPoints; i++) {
            D[i][i] = 0.0;
            for (j = 0; j < nPoints; j++) {
                D[i][i] += A[i][j];
            }
            D[i][i] = 1.0/Math.sqrt(D[i][i]);
        } // for (i = 0; i < nPoints; i++)
        
        for (i = 0; i < nPoints; i++) {
            for (j = i+1; j < nPoints; j++) {
                L[i][j] = D[i][i] * A[i][j] * D[j][j];
                L[j][i] = L[i][j];
            } // for (j = i+1; j < nPoints; j++)
            L[i][i] = D[i][i] * D[i][i] * A[i][i];
        } // for (i = 0; i < nPoints; i++)
        
        // Find the x1, x2,...,xk, the numberClusters largest eigenvectors of L (chosen to be orthogonal to each other in the case
        // of repeated eigenvalues), and form the matrix X = [x1,x2...xk] a n by numberClusters matrix by stacking the eigenvectors 
        // in columns
        // In EigenvalueDecomposition the columns of V represent the eigenvectors,
        // Only real eigenvalue components are needed for symmetric matrices
        Eigenvalue.decompose(L, V, eigenvalue);
        // Arrange the eigenvalues and corresponding eigenvectors in descending order
        // so that ej >= ej+1 for the numberClusters largest eigenvalues
        for (i = 0; i < nPoints; i++) {
            found[i] = false;
        }
       
        for (i = 0; i < numberClusters; i++) {
            largestEigenvalue = -Double.MAX_VALUE;
            for (j = 0; j < nPoints; j++) {
                if ((!found[j]) && (eigenvalue[j] > largestEigenvalue)) {
                    largestEigenvalue = eigenvalue[j];
                    largestIndex = j;
                }
            } // for (j = 0; j < nPoints; j++)
            found[largestIndex] = true;
            for (j = 0; j < nPoints; j++) {
                X[j][i] = V[j][largestIndex];    
            }
        } // for (i = 0; i < numberClusters; i++)
        
        // Form the matrix Y from X by renormalizing each of X's rows to habve unit length
        // (i.e. Yij = Xij/((sum over j of Xij**2)**(1/2)))
        for (i = 0; i < nPoints; i++) {
            rowSize = 0.0;
            for (j = 0; j < numberClusters; j++) {
                rowSize += X[i][j]*X[i][j];
            }
            rowSize = Math.sqrt(rowSize);
            for (j = 0; j < numberClusters; j++) {
                Y[i][j] = X[i][j]/rowSize;
            }
        } // for (i = 0; i < nPoints; i++)
        
        // Treating each row of Y as a point in numberClusters dimensional space, cluster them into numberClusters clusters
        // via k-means.  Let the first cluster centroid be a randomly chosen row of Y, and then repeatedly choose as the
        // next centroid the row of Y that is closest to being 90 degrees from all the centroids already picked. 
        for (i = 0; i < numberClusters; i++) {
            centroidPos[i][0] = Y[0][i];
        }
        
        found[0] = true;
        for (i = 1; i < nPoints; i++) {
            found[i] = false;
        }
        
        for (i = 1; i < numberClusters; i++) {
            smallestAbsInnerProduct = Double.MAX_VALUE;
            for (j = 1; (!found[j]) && (j < nPoints); j++) {
                absInnerProduct = 0.0;
                for (k = 0; k < i; k++) {
                    innerProduct = 0.0;
                    for (m = 0; m < numberClusters; m++) {
                        innerProduct += Y[j][m] * centroidPos[m][k];    
                    } // for (m = 0; m < numberClusters; m++)
                    if (Math.abs(innerProduct) > absInnerProduct) {
                        absInnerProduct = Math.abs(innerProduct);
                    }
                } // for (k = 0; k < i; k++)
                if (absInnerProduct < smallestAbsInnerProduct) {
                    smallestAbsInnerProduct = absInnerProduct;
                    smallestIndex = j;
                }
            } // for (j = 1; (!found[j]) && (j < nPoints); j++)
            found[smallestIndex] = true;
            for (j = 0; j < numberClusters; j++) {
                centroidPos[j][i] = Y[smallestIndex][j];
            }
        } // for (i = 1; i < numberClusters; i++)
        
        // Finally, assign the original point si to cluster j if and only if row i of the matrix Y was assigned to cluster j.
    }
}