package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.SelectedEigenvalue;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Color;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

import de.jtem.numericalMethods.calculus.function.RealFunctionOfOneVariable;
import de.jtem.numericalMethods.calculus.minimizing.Brent;

/**
 * 
 * @author William Gandler
 * This program is an implementation of the algorithm described in "On Spectral Clustering: Analysis and an algorithm"
 * by Andrew Y. Ng, Michael I. Jordan, and Yair Weiss.
 *
 */
public class AlgorithmSpectralClustering extends AlgorithmBase implements RealFunctionOfOneVariable {
    
//~ Instance fields ------------------------------------------------------------------------------------------------
    
    private ModelImage image;
    
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
    
    // First subscript is number of dimensions used in spectral clustering = numberClusters
    // Second subscript 0 to numberClusters-1 for each cluster
    // Value is the cluster position
    private double[][] centroidPos;
    
    private String resultsFileName;
    
    private float redBuffer[] = null;
    
    private float greenBuffer[] = null;
    
    private float blueBuffer[] = null;
    
    // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
    private double scaleMax = 255.0;
    
    // If true, each color is weighed proportional to its frequency
    private boolean useColorHistogram = false;
    
    private boolean scaleVariablesToUnitVariance;
    
    private int nDims;
    private int nPoints;
    private int numberClusters;
    private double A[][];
    private double scale2[];
    private double D[][];
    private double L[][];
    private char jobz;
    private char range;
    private char uplo;
    private int il;
    private int iu;
    private double abstol;
    private int numEigenvaluesFound[];
    private double eigenvalues[];
    private double X[][];
    private double work[];
    private int lwork;
    private int iwork[];
    private int ifail[];
    private int info[];
    private boolean found[];
    private double Y[][];
    private int totalWeight[];
    private double centroidDistances[][];
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmSpectralClustering(ModelImage image, double[][] pos, double[] scale, int groupNum[],
            double[][] centroidPos, String resultsFileName, float[] redBuffer,
            float[] greenBuffer, float[] blueBuffer, double scaleMax, boolean scaleVariablesToUnitVariance) {
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
        this.scaleVariablesToUnitVariance = scaleVariablesToUnitVariance;
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
        
        // Initialize sigma with 1.0
        double sigma = 1.0;
        int i;
        int j;
        int k;
        int m;
        Color color[];
        VOI newPtVOI;
        float xArr[] = new float[1];
        float yArr[] = new float[1];
        float zArr[] = new float[1];
        File file;
        RandomAccessFile raFile;
        String dataString = "";
        
        nDims = pos.length;
        nPoints = pos[0].length;
        // centroidPos is [numberClusters][numberClusters]
        numberClusters = centroidPos.length;
        A = new double[nPoints][nPoints];
        scale2 = new double[scale.length];
        for (i = 0; i < scale.length; i++) {
            scale2[i] = scale[i]*scale[i];
        }
        D = new double[nPoints][nPoints];
        L = new double[nPoints][nPoints];
        //double eigenvalue[] = new double[nPoints];
        //double V[][] = new double[nPoints][nPoints];
        X = new double[nPoints][numberClusters]; 
        found = new boolean[nPoints];
        //double largestEigenvalue;
        //int largestIndex = 0;
        Y = new double[nPoints][numberClusters];
        jobz = 'V'; // // Compute eigenvalues and eigenvectors
        range = 'I'; // The il-th through iu-th eigenvectors will be found
        uplo = 'U'; // Upper triangle of A is stored.  'L' for lower triangle would be equally acceptable.
        il = nPoints - numberClusters + 1; // smallest eigenvalue returned
        iu = nPoints; // largest eigenvalue returned
        GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
        abstol = 2.0 * ge.dlamch('S'); // The absolute error tolerance for eigenvalues
        // Eigenvalues will be computed most accurately when abstol is set to twice the underflow
        // threshold 2*dlamch('S'), not zero.
        ge = null;
        numEigenvaluesFound = new int[1];
        // Will return the found eigenvalues in the first numberClusters elements in ascending order
        eigenvalues = new double[numberClusters]; 
        work = new double[8 * nPoints]; //  double workspace
        lwork = 8 * nPoints; // length of work 
        iwork = new int[5 * nPoints]; // int workspace
        ifail = new int[nPoints]; // If info[0] > 0, contains the 1 based indices of the eigenvectors that failed to converge
        //      = 0:  successful exit
        //        < 0:  if info[0] = -i, the i-th argument had an illegal value
        //        > 0:  if info[0] = i, then i eigenvectors failed to converge.
        //        Their indices are stored in array ifail.
        info = new int[1];
        totalWeight = new int[numberClusters];
        centroidDistances = new double[numberClusters][numberClusters];
        double squaresArray[] = new double[13];
        double smallestSquares = Double.MAX_VALUE;
        int squaresIndex = -1;
        double tol;
        double maxLikelihoodSigma[] = new double[1];
        
        for (i = -6; i <= 6; i++) {
            sigma = Math.pow(10.0, i);
            squaresArray[6 + i] = eval(sigma);
            if (squaresArray[6 + i] < smallestSquares) {
                smallestSquares = squaresArray[6 + i];
                squaresIndex = 6 + i;
            }
        } // for (i = -6; i <= 6; i++)
        
        if ((i >= 1) && (i <= 11)) {
            tol = Math.pow(10.0, squaresIndex - 4);
            Brent.search(Math.pow(10.0, squaresIndex-1), Math.pow(10.0, squaresIndex), Math.pow(10.0, squaresIndex+1), 
                    maxLikelihoodSigma, this, tol );    
        } // if ((i >= 1) && (i <= 11))
        
        
        // Finally, assign the original point si to cluster j if and only if row i of the matrix Y was assigned to cluster j.
        eval(maxLikelihoodSigma[0]);
        
        if ((image != null) && (nDims >= 2) && (nDims <= 3) && (numberClusters <= 9)) {
            color = new Color[numberClusters];
            color[0] = Color.red;
            color[1] = Color.green;
            if (numberClusters >= 3) {
                color[2] = Color.blue;
                if (numberClusters >= 4) {
                    color[3] = Color.cyan;
                    if (numberClusters >= 5) {
                        color[4] = Color.magenta;
                        if (numberClusters >= 6) {
                            color[5] = Color.orange;
                            if (numberClusters >= 7) {
                                color[6] = Color.pink;
                                if (numberClusters >= 8) {
                                    color[7] = Color.yellow;
                                    if (numberClusters >= 9) {
                                        color[8] = Color.gray;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            for (i = 0; i < nPoints; i++) {
                newPtVOI = new VOI((short) (i), "", VOI.POINT, -1.0f);
                newPtVOI.setColor(color[groupNum[i]]);
                xArr[0] = (float)pos[0][i];
                yArr[0] = (float)pos[1][i];
                if (nDims == 2) {
                    zArr[0] = 0.0f;
                }
                else {
                    zArr[0] = (float)pos[2][i];
                }
                newPtVOI.importCurve(xArr, yArr, zArr);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                image.registerVOI(newPtVOI);
            }
            
            new ViewJFrameImage(image);
        } // if ((image != null) && (nDims >= 2) && (nDims <= 3) && (numberClusters <= 9)
        
        Preferences.debug("Putting results in " + resultsFileName + "\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("Putting results in " + resultsFileName);
        file = new File(resultsFileName);
        try {
        raFile = new RandomAccessFile( file, "rw" );
        }
        catch(FileNotFoundException e) {
            MipavUtil.displayError("new RandomAccessFile gave FileNotFoundException " + e);
            setCompleted(false);
            return;
        }
        // Necessary so that if this is an overwritten file there isn't any junk at the end
        try {
            raFile.setLength( 0 );
        }
        catch (IOException e) {
            MipavUtil.displayError("raFile.setLength(0) gave IOException " + e);
            setCompleted(false);
            return; 
        }
        
        if (scaleVariablesToUnitVariance) {
            dataString += "Variables scaled to unit variance\n";
        }
        else {
            dataString += "Variables not scaled to unit variance\n";
        }
        dataString += "Number of clusters = " + String.valueOf(numberClusters) + "\n\n";
        
        for (i = 0; i < nPoints; i++) {
            dataString += "Point number " + String.valueOf(i+1) + "  Location  ";
            for (j = 0; j < nDims; j++) {
                dataString += String.valueOf(pos[j][i]) + "  "; 
            }
            dataString += "Cluster  " + String.valueOf(groupNum[i]+1) + "\n";
        }
        try {
            raFile.write(dataString.getBytes());
        }
        catch (IOException e) {
            MipavUtil.displayError("raFile.write gave IOException " + e);
            setCompleted(false);
            return;     
        }
        try {
            raFile.close();
        }
        catch (IOException e) {
            MipavUtil.displayError("raFile.close gave IOException " + e);
            setCompleted(false);
            return;     
        }
        
        setCompleted(true);
        return;
    }
    
    public double eval(double sigma) {
        int i;
        int j;
        int k;
        int m;
        double distSquared;
        double diff;
        SelectedEigenvalue se;
        double rowSize;
        double smallestAbsInnerProduct;
        double absInnerProduct;
        double innerProduct;
        int smallestIndex = 0;
        boolean changeOccurred;
        int iteration;
        double minDistSquared;
        int originalGroupNum;
        int clustersWithoutPoints = 0;
        double withinClustersSumsOfSquares = 0.0;
        
        double twoSigmaSquared = 2.0 * sigma * sigma;
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
        // n, the order of array A = zDim;
        // Use L for array A.
        // Use nPointds for lda, the leading dimension of array A.
        // vl and vu are not used for range = 'I'.
        // numEigenvaluesFound[0] should return numberClusters;
        // Use nPoints for ldz
        
        se = new SelectedEigenvalue();
        se.dsyevx(jobz, range, uplo, nPoints, L, nPoints, 0.0, 0.0, il, iu, abstol, numEigenvaluesFound,
                  eigenvalues, X, nPoints, work, lwork, iwork, ifail, info);
        se = null;
        if (info[0] < 0) {
            // illegal argument
            System.out.println("se.dsyevx had info[0] = " + info[0] + " indicating an illegal argument");
            Preferences.debug("se.dsyevs had info[0] = " + info[0] + " indicating an illegal argument\n",
                              Preferences.DEBUG_ALGORITHM);
            setCompleted(false);
            return Double.NaN;
        }
        else if (info[0] > 0) {
            System.out.println(info[0] + " eigenvectors failed to converge");
            Preferences.debug(info[0] + " eigenvectors failed to converge\n", Preferences.DEBUG_ALGORITHM);
            System.out.println("The eigenvectors that failed to converge are:\n");
            Preferences.debug("The eigenvectors that failed to converge are:\n", Preferences.DEBUG_ALGORITHM);
            for (i = 0; i < info[0]; i++) {
                System.out.println("Eigenvector = " + ifail[i]);
                Preferences.debug("Eigenvector = " + ifail[i] + "\n", Preferences.DEBUG_ALGORITHM);
            }
            setCompleted(false);
            return Double.MAX_VALUE;
        }
        else {
            Preferences.debug("se.dsyevx had info[0] = 0 indicating a successful exit\n", Preferences.DEBUG_ALGORITHM);
        }
        if (numEigenvaluesFound[0] < numberClusters) {
            System.out.println("numEigenvaluesFound[0] = " + numEigenvaluesFound[0] + " less than the needed " + numberClusters);
            Preferences.debug("numEigenvaluesFound[0] = " + numEigenvaluesFound[0] + " less than the needed " + numberClusters + "\n",
                              Preferences.DEBUG_ALGORITHM);
            setCompleted(false);
            return Double.MAX_VALUE;
        }
        // In EigenvalueDecomposition the columns of V represent the eigenvectors,
        // Only real eigenvalue components are needed for symmetric matrices
        //Eigenvalue.decompose(L, V, eigenvalue);
        // Arrange the eigenvalues and corresponding eigenvectors in descending order
        // so that ej >= ej+1 for the numberClusters largest eigenvalues
        /*for (i = 0; i < nPoints; i++) {
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
        } // for (i = 0; i < numberClusters; i++)*/
        
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
        groupNum[0] = 0;
        
        found[0] = true;
        for (i = 1; i < nPoints; i++) {
            found[i] = false;
            groupNum[i] = -1;
        }
        
        for (i = 1; i < numberClusters; i++) {
            smallestAbsInnerProduct = Double.MAX_VALUE;
            for (j = 1; j < nPoints; j++) {
                if (!found[j]) {
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
                } // if (!found[j])
            } // for (j = 1; j < nPoints; j++)
            found[smallestIndex] = true;
            for (j = 0; j < numberClusters; j++) {
                centroidPos[j][i] = Y[smallestIndex][j];
            }
            groupNum[i] = smallestIndex;
        } // for (i = 1; i < numberClusters; i++)
        
        changeOccurred = true;
        iteration = 1;
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        while (changeOccurred) {
            fireProgressStateChanged("K-means iteration = " + iteration);
            Preferences.debug("K-means iteration = " + iteration + "\n", Preferences.DEBUG_ALGORITHM);
            iteration++;
            changeOccurred = false;
            for (i = 0; i < numberClusters; i++) {
                totalWeight[i] = 0;
                for (j = 0; j < numberClusters; j++) {
                    centroidDistances[i][j] = 0.0;
                }
            }
            for (i = 0; i < numberClusters; i++) {
                for (j = i+1; j < numberClusters; j++) {
                    for (k = 0; k < numberClusters; k++) {
                        diff = centroidPos[k][i] - centroidPos[k][j];
                        centroidDistances[i][j] += diff*diff;
                    } // for (k = 0; k < numberClusters; k++)
                    centroidDistances[i][j] /= 4.0;
                } // for (j = i+1; j < numberClusters; j++)
            } // for (i = 0; i < numberClusters; i++)
            for (i = 0; i < nPoints; i++) {
                minDistSquared = 0.0;
                originalGroupNum = groupNum[i];
                for (k = 0; k < numberClusters; k++) {
                    diff = Y[i][k] - centroidPos[k][0];
                    minDistSquared = minDistSquared + diff*diff;
                }
                groupNum[i] = 0;
                for (j = 1; j < numberClusters; j++) {
                    // Use triangle inequality to avoid unnecessary calculations
                    if (minDistSquared > centroidDistances[groupNum[i]][j]) {
                        distSquared = 0.0;
                        for (k = 0; k < numberClusters; k++) {
                            diff = Y[i][k] - centroidPos[k][j];
                            distSquared = distSquared + diff*diff;
                        }
                        if (distSquared < minDistSquared) {
                            minDistSquared = distSquared;
                            groupNum[i] = j;
                        }
                    } // if (minDistSquared > centroidDistances[groupNum[i]][j]))
                } // for (j = 1; j < numberClusters; j++)
                totalWeight[groupNum[i]]++;
                if (originalGroupNum != groupNum[i]) {
                    changeOccurred = true;
                }
            } // for (i = 0; i < nPoints; i++)  
            
            for (i = 0; i < numberClusters; i++) {
                for (j = 0; j < numberClusters; j++) {
                    centroidPos[j][i] = 0.0;
                }
            }
            for (i = 0; i < numberClusters; i++) {
                totalWeight[i] = 0;
            }
            for (i = 0; i < nPoints; i++) {
                totalWeight[groupNum[i]]++;
                for (j = 0; j < numberClusters; j++) {
                    centroidPos[j][groupNum[i]] += Y[i][j];
                }
            }
            clustersWithoutPoints = 0;
            for (i = 0; i < numberClusters; i++) {
                if (totalWeight[i] <= 0) {
                    Preferences.debug("Cluster centroid " + (i+1) + " has no points\n", Preferences.DEBUG_ALGORITHM);
                    clustersWithoutPoints++;
                }
                else {
                    Preferences.debug("Cluster centroid " + (i+1) + ":\n", Preferences.DEBUG_ALGORITHM);
                    for (j = 0; j < numberClusters; j++) {
                        centroidPos[j][i] = centroidPos[j][i]/totalWeight[i];
                        Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n", 
                                Preferences.DEBUG_ALGORITHM);
                    }
                } // else
            }   
        } // while (changeOccurred)
        Preferences.debug("There are " + clustersWithoutPoints + " clusters without points\n", Preferences.DEBUG_ALGORITHM);
        withinClustersSumsOfSquares = 0.0;
        for (i = 0; i < nPoints; i++) {
            for (j = 0; j < numberClusters; j++) {
                diff = Y[i][j] - centroidPos[j][groupNum[i]];
                withinClustersSumsOfSquares += diff * diff;
            } // for (j = 0; j < numberClusters; j++)
        } // for (i = 0; i < nPoints; i++) 
        
        return withinClustersSumsOfSquares;
    } // public double eval(double sigma)
}