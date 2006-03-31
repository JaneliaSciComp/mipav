package gov.nih.mipav.model.structures.jama;
import  gov.nih.mipav.model.structures.*;
import  gov.nih.mipav.model.structures.jama.util.*;
/** QR Decomposition.
<P>
   For an m-by-n matrix A with m >= n, the QR decomposition is an m-by-n
   orthogonal matrix Q and an n-by-n upper triangular matrix R so that
   A = Q*R.
<P>
   The QR decompostion always exists, even if the matrix does not have
   full rank, so the constructor will never fail.  The primary use of the
   QR decomposition is in the least squares solution of nonsquare systems
   of simultaneous linear equations.  This will fail if isFullRank()
   returns false.
*/

public class QRDecomposition implements java.io.Serializable {

/* ------------------------
   Class variables
 * ------------------------ */

   /** Array for internal storage of decomposition.
   @serial internal array storage.
   */
   private double[][] QR;

   /** Row and column dimensions.
   @serial row dimension.
   @serial column dimension.
   */
   private int mRow, nCol;

   /** Array for internal storage of diagonal of R.
   @serial diagonal of R.
   */
   private double[] Rdiag;

/* ------------------------
   Constructor
 * ------------------------ */

   /** QR Decomposition, computed by Householder reflections.
   @param A    Rectangular matrix
   @return     Structure to access R and the Householder vectors and compute Q.
   */

   public QRDecomposition (Matrix A) {
      // Initialize.
      QR = A.getArrayCopy();
      mRow = A.getRowDimension();
      nCol = A.getColumnDimension();
      Rdiag = new double[nCol];

      // Main loop.
      for (int k = 0; k < nCol; k++) {
         // Compute 2-norm of k-th column without under/overflow.
         double nrm = 0;
         for (int i = k; i < mRow; i++) {
            nrm = Maths.hypot(nrm,QR[i][k]);
         }

         if (nrm != 0.0) {
            // Form k-th Householder vector.
            if (QR[k][k] < 0) {
               nrm = -nrm;
            }
            for (int i = k; i < mRow; i++) {
               QR[i][k] /= nrm;
            }
            QR[k][k] += 1.0;

            // Apply transformation to remaining columns.
            for (int j = k+1; j < nCol; j++) {
               double s = 0.0; 
               for (int i = k; i < mRow; i++) {
                  s += QR[i][k]*QR[i][j];
               }
               s = -s/QR[k][k];
               for (int i = k; i < mRow; i++) {
                  QR[i][j] += s*QR[i][k];
               }
            }
         }
         Rdiag[k] = -nrm;
      }
   }

/* ------------------------
   Public Methods
 * ------------------------ */

   /** Is the matrix full rank?
   @return     true if R, and hence A, has full rank.
   */

   public boolean isFullRank () {
      for (int j = 0; j < nCol; j++) {
         if (Rdiag[j] == 0)
            return false;
      }
      return true;
   }

   /** Return the Householder vectors
   @return     Lower trapezoidal matrix whose columns define the reflections
   */

   public Matrix getH () {
      Matrix X = new Matrix(mRow,nCol);
      double[][] H = X.getArray();
      for (int i = 0; i < mRow; i++) {
         for (int j = 0; j < nCol; j++) {
            if (i >= j) {
               H[i][j] = QR[i][j];
            } else {
               H[i][j] = 0.0;
            }
         }
      }
      return X;
   }

   /** Return the upper triangular factor
   @return     R
   */

   public Matrix getR () {
      Matrix X = new Matrix(nCol,nCol);
      double[][] R = X.getArray();
      for (int i = 0; i < nCol; i++) {
         for (int j = 0; j < nCol; j++) {
            if (i < j) {
               R[i][j] = QR[i][j];
            } else if (i == j) {
               R[i][j] = Rdiag[i];
            } else {
               R[i][j] = 0.0;
            }
         }
      }
      return X;
   }

   /** Generate and return the (economy-sized) orthogonal factor
   @return     Q
   */

   public Matrix getQ () {
      Matrix X = new Matrix(mRow,nCol);
      double[][] Q = X.getArray();
      for (int k = nCol-1; k >= 0; k--) {
         for (int i = 0; i < mRow; i++) {
            Q[i][k] = 0.0;
         }
         Q[k][k] = 1.0;
         for (int j = k; j < nCol; j++) {
            if (QR[k][k] != 0) {
               double s = 0.0;
               for (int i = k; i < mRow; i++) {
                  s += QR[i][k]*Q[i][j];
               }
               s = -s/QR[k][k];
               for (int i = k; i < mRow; i++) {
                  Q[i][j] += s*QR[i][k];
               }
            }
         }
      }
      return X;
   }

   /** Least squares solution of A*X = B
   @param B    A Matrix with as many rows as A and any number of columns.
   @return     X that minimizes the two norm of Q*R*X-B.
   @exception  IllegalArgumentException  Matrix row dimensions must agree.
   @exception  RuntimeException  Matrix is rank deficient.
   */

   public Matrix solve (Matrix B) {
      if (B.getRowDimension() != mRow) {
         throw new IllegalArgumentException("Matrix row dimensions must agree.");
      }
      if (!this.isFullRank()) {
         throw new RuntimeException("Matrix is rank deficient.");
      }
      
      // Copy right hand side
      int nx = B.getColumnDimension();
      double[][] X = B.getArrayCopy();

      // Compute Y = transpose(Q)*B
      for (int k = 0; k < nCol; k++) {
         for (int j = 0; j < nx; j++) {
            double s = 0.0; 
            for (int i = k; i < mRow; i++) {
               s += QR[i][k]*X[i][j];
            }
            s = -s/QR[k][k];
            for (int i = k; i < mRow; i++) {
               X[i][j] += s*QR[i][k];
            }
         }
      }
      // Solve R*X = Y;
      for (int k = nCol-1; k >= 0; k--) {
         for (int j = 0; j < nx; j++) {
            X[k][j] /= Rdiag[k];
         }
         for (int i = 0; i < k; i++) {
            for (int j = 0; j < nx; j++) {
               X[i][j] -= X[k][j]*QR[i][k];
            }
         }
      }
      return (new Matrix(X,nCol,nx).getMatrix(0,nCol-1,0,nx-1));
   }
}
