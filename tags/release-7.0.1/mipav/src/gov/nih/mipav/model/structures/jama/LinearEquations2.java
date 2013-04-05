package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class LinearEquations2 implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    GeneralizedInverse2 gi = new GeneralizedInverse2();
    private ViewUserInterface UI = ViewUserInterface.getReference();
    
    private int iparms[];

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    /**
     * Creates a new LinearEquations2 object.
     */
    public LinearEquations2() {}
    
    /*
     * This is a port of a portion of LAPACK routine DGETRF.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgetrf computes an LU factorization of a general m-by-n matrix A
       using partial pivoting with row interchanges.

       The factorization has the form
          A = P * L * U
       where P is a permutation matrix, L is lower triangular with unit
       diagonal elements (lower trapezoidal if m > n), and U is upper
       triangular (upper trapezoidal if m < n).
  
       This is the right-looking Level 3 BLAS version of the algorithm.

       @param input int m
           The number of rows of the matrix A.  m >= 0.
       @param input int n
           The number of columns of the matrix A.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the m by n matrix to be factored.
           On exit, the factors L and U from the factorization
           A = P*L*U; the unit diagonal elements of L are not stored.
       @param input int lda
           The leading dimension of the array A.  LDA >= max(1,m).
       @param output int[] ipiv of dimension (min(m,n))
           The pivot indices; for 1 <= i <= min(m,n), row i of the
           matrix was interchanged with row ipiv[i].
       @param output int[] info of dimension (1)
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
           > 0: if info[0] = i, U[i-1][i-1] is exactly zero. The factorization
                has been completed, but the factor U is exactly
                singular, and division by zero will occur if it is used
                to solve a system of equations.
     */
    private void dgetrf(int m, int n, double[][] A, int lda, int[] ipiv, int[] info) {
        int i;
        int iinfo[] = new int[1];
        int j;
        int jb;
        int nb;
        String name;
        String opts;
        
        // Test the input parameters.
        
        info[0] = 0;
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -4;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgetrf had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (m == 0 || n == 0) {
            return;
        }
    
        // Determine the block size for this environment.
    
        name = new String("DGETRF");
        opts = new String(" ");
        nb = ge.ilaenv( 1, name, opts, m, n, -1, -1);
        if (nb <= 1 || nb >= Math.min(m, n)) {
    
            // Use unblocked code.
    
            dgetf2(m, n, A, lda, ipiv, info);
        } // if (nb <= 1 || nb >= Math.min(m, n))
        /*else {
    
    *        Use blocked code.
    *
             DO 20 J = 1, MIN( M, N ), NB
                JB = MIN( MIN( M, N )-J+1, NB )
    *
    *           Factor diagonal and subdiagonal blocks and test for exact
    *           singularity.
    *
                CALL DGETF2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
    *
    *           Adjust INFO and the pivot indices.
    *
                IF( INFO.EQ.0 .AND. IINFO.GT.0 )
         $         INFO = IINFO + J - 1
                DO 10 I = J, MIN( M, J+JB-1 )
                   IPIV( I ) = J - 1 + IPIV( I )
       10       CONTINUE
    *
    *           Apply interchanges to columns 1:J-1.
    *
                CALL DLASWP( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
    *
                IF( J+JB.LE.N ) THEN
    *
    *              Apply interchanges to columns J+JB:N.
    *
                   CALL DLASWP( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1,
         $                      IPIV, 1 )
    *
    *              Compute block row of U.
    *
                   CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', JB,
         $                     N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ),
         $                     LDA )
                   IF( J+JB.LE.M ) THEN
    *
    *                 Update trailing submatrix.
    *
                      CALL DGEMM( 'No transpose', 'No transpose', M-J-JB+1,
         $                        N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,
         $                        A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ),
         $                        LDA )
                   END IF
                END IF
       20    CONTINUE
        } // else use blocked code*/
        return;

    } // dgetrf
    
    /*
     * This is a port of a portion of LAPACK routine DGETF2.f version 3.4.2
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., September, 2012
     * 
     * dget2 computes the LU factorization of a general m-by-n matrix using partial pivoting with row
     * interchanges (unblocked algorithm).
     * 
     * dgetf2 computes an LU factorization of a general m-by-n matrix A
       using partial pivoting with row interchanges.

       The factorization has the form
          A = P * L * U
       where P is a permutation matrix, L is lower triangular with unit
       diagonal elements (lower trapezoidal if m > n), and U is upper
       triangular (upper trapezoidal if m < n).

       This is the right-looking Level 2 BLAS version of the algorithm.

       @param input int m
           The number of rows of the matrix A.  m >= 0.
       @param input int n
           The number of columns of the matrix A.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the m by n matrix to be factored.
           On exit, the factors L and U from the factorization
           A = P*L*U; the unit diagonal elements of L are not stored.
       @param input int lda
           The leading dimension of the array A.  LDA >= max(1,m).
       @param output int[] ipiv of dimension (min(m,n))
           The pivot indices; for 1 <= i <= min(m,n), row i of the
           matrix was interchanged with row ipiv[i].
       @param output int[] info of dimension (1)
           = 0: successful exit
           < 0: if info[0] = -k, the k-th argument had an illegal value
           > 0: if info[0] = k, U[k-1][k-1] is exactly zero. The factorization
                has been completed, but the factor U is exactly
                singular, and division by zero will occur if it is used
                to solve a system of equations.
     */
    private void dgetf2(int m, int n, double[][] A, int lda, int[] ipiv, int[] info) {
        double sfmin;
        int i;
        int j;
        int jp;
        int index;
        double maxVal;
        double temp;
        double vec[];
        double vec2[];
        double arr[][];
        int k;
        
        // Test the input parameters.
        
        info[0] = 0;
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (lda < Math.max(1,m)) {
            info[0] = -4;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgetf2 had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (m == 0 || n == 0) {
            return;
        }
    
        // Compute machine safe minimum 
    
        sfmin = ge.dlamch('S');  
    
        for (j= 1; j <= Math.min(m, n); j++) {
    
            // Find pivot and test for singularity.
             index = j;
             maxVal = Math.abs(A[j-1][j-1]);
             for (index = j; index <= m; index++) {
                 if (Math.abs(A[index-1][j-1]) > maxVal) {
                     maxVal = Math.abs(A[index-1][j-1]);
                     index = j;
                 }
             }
             jp = j - 1 + index;
             ipiv[j-1] = jp;
             if (A[jp-1][j-1] != 0.0) {
    
                 // Apply the interchange to columns 1:n.
    
                 if (jp != j) {
                   for (i = 0; i < n; i++) {
                       temp = A[j-1][i];
                       A[j-1][i] = A[jp-1][i];
                       A[jp-1][i] = temp;
                   }
                 }
    
                 // Compute elements j+1:m of j-th column.
    
                if (j < m) { 
                    if (Math.abs(A[j-1][j-1]) >= sfmin) {
                        for (i = 0; i < m-j; i++) {
                            A[j+i][j-1] = (1.0/A[j-1][j-1]) * A[j+i][j-1];
                        }
                    }
                    else { 
                     for (i = 1; i <= m-j; i++) {
                        A[j+i-1][j-1] = A[j+i-1][j-1] / A[j-1][j-1]; 
                     } // for (i = 1; i <= m-j; i++)
                    } // else
                } // if (j < m)
             } // if (A[jp-1][j-1] != 0.0)
             else if (info[0] == 0) {
    
                 info[0] = j;
             } // else if (info[0] == 0)
    
             if (j < Math.min(m, n)) {
    
                 // Update trailing submatrix.
                 vec = new double[m-j];
                 for (i = 0; i < m-j; i++) {
                     vec[i] = A[j+i][j-1];
                 }
                 vec2 = new double[n-j];
                 for (i = 0; i < n-j; i++) {
                     vec2[i] = A[j-1][j+i];
                 }
                 arr = new double[m-j][n-j];
                 for (i = 0; i < m-j; i++) {
                     for (k = 0; k < n-j; k++) {
                         arr[i][k] = A[j+i][j+k];
                     }
                 }
                 ge.dger(m-j, n-j, -1.0, vec, 1, vec2, 1, arr, lda);
                 for (i = 0; i < m-j; i++) {
                     for (k = 0; k < n-j; k++) {
                         A[j+i][j+k] = arr[i][k];
                     }
                 }
             } // if (j < Math.min(m, n))
        } // for (j= 1; j <= Math.min(m, n); j++)
        return;

    } // dgetf2
    
    /*
     * This is a port of a portion of LAPACK auxiliary routine DLASWP.f version 3.4.2
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., September, 2012
     * Modified by
       R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA

     * 
     * dlaswp performs a series of row interchanges on a general rectangular matrix.
     * 
     * dlaswp performs a series of row interchanges on the matrix A.
       One row interchange is initiated for each of rows K1 through K2 of A.

       @param input int n
           The number of columns of the matrix A.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the matrix of column dimension N to which the row
           interchanges will be applied.
           On exit, the permuted matrix.
       @param input int lda
           The leading dimension of the array A.
       @param input int k1
           The first element of ipiv for which a row interchange will
           be done.
       @param input int k2
           The last element of IPIV for which a row interchange will
           be done.
       @param input int[] ipiv of dimension (k2 * abs(incx))
           The vector of pivot indices.  Only the elements in positions
           k1 through k2 of ipiv are accessed.
           ipiv[k] = L implies rows k and L are to be interchanged
       @param input int incx
           The increment between successive values of IPIV.  If IPIV
           is negative, the pivots are applied in reverse order.
     */
    private void dlaswp(int n, double[][] A, int lda, int k1, int k2, int[] ipiv, int incx) {
        int i;
        int i1;
        int i2;
        int inc;
        int ip;
        int ix;
        int ix0;
        int j;
        int k;
        int n32;
        double temp;
        
        // Interchange row i with row ipiv[i] for each of rows k1 through k2.
        
        if (incx > 0) {
           ix0 = k1;
           i1 = k1;
           i2 = k2;
           inc = 1;
        }
        else if (incx < 0) {
           ix0 = 1 + (1-k2)*incx;
           i1 = k2;
           i2 = k1;
           inc = -1;
        }
        else {
           return;
        }
    
        n32 = (n / 32)*32;
        if (n32 != 0) {
            for (j = 1; j <= n32; j +=32) {
                ix = ix0;
                if (inc == 1) {
                    for (i = i1; i <= i2; i++) {
                        ip = ipiv[ix-1];
                        if (ip != i) {
                            for (k = j; k <= j + 31; k++) {
                                temp = A[i-1][k-1];
                                A[i-1][k-1] = A[ip-1][k-1];
                                A[ip-1][k-1] = temp;
                            } // for (k = j; k <= j + 31; k++)
                        } // if (ip != i)
                        ix = ix + incx;
                    } /// for (i = i1; i <= i2; i++)
                } // if (inc == 1)
                else { // inc == -1
                    for (i = i1; i >= i2; i--) {
                        ip = ipiv[ix-1];
                        if (ip != i) {
                            for (k = j; k <= j + 31; k++) {
                                temp = A[i-1][k-1];
                                A[i-1][k-1] = A[ip-1][k-1];
                                A[ip-1][k-1] = temp;
                            } // for (k = j; k <= j + 31; k++)
                        } // if (ip != i)
                        ix = ix + incx;    
                    } // for (i = i1; i >= i2; i--)
                } // else inc == -1
            } // for (j = 1; j <= n32; j +=32)
        } // if (n32 != 0)
        if (n32 != n) {
            n32 = n32 + 1;
            ix = ix0;
            if (inc == 1) {
                for (i = i1; i <= i2; i++) {
                    ip = ipiv[ix-1];
                    if (ip != i) {
                        for (k = n32; k <= n; k++) {
                            temp = A[i-1][k-1];
                            A[i-1][k-1] = A[ip-1][k-1];
                            A[ip-1][k-1] = temp;
                        } // for (k = n32; k <= n; k++)
                    } // if (ip != i)
                    ix = ix + incx;
                } // for (i = i1; i <= i2; i++)
            } // if (inc == 1)
            else { // inc == -1
                for (i = i1; i >= i2; i--) {
                    ip = ipiv[ix-1];
                    if (ip != i) {
                        for (k = n32; k <= n; k++) {
                            temp = A[i-1][k-1];
                            A[i-1][k-1] = A[ip-1][k-1];
                            A[ip-1][k-1] = temp;
                        } // for (k = n32; k <= n; k++)
                    } // if (ip != i)
                    ix = ix + incx;    
                } // for (i = i1; i >= i2; i--)
            } // else inc == -1
        } // if (n32 != n)
     
        return;

    } // dlaswp
}