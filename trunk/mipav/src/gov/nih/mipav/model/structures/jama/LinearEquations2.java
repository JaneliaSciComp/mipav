package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class LinearEquations2 implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    GeneralizedInverse2 gi = new GeneralizedInverse2();
    LinearEquations le = new LinearEquations();
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
     * This is a port of a portion of LAPACK driver routine DGESV.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgesv computes the solution to system of linear equations A * X = B for GE matrices
     * 
     * dgesv computes the solution to a real system of linear equations
          A * X = B,
       where A is an n-by-n matrix and X and B are n-by-nrhs matrices.

       The LU decomposition with partial pivoting and row interchanges is
       used to factor A as
          A = P * L * U,
       where P is a permutation matrix, L is unit lower triangular, and U is
       upper triangular.  The factored form of A is then used to solve the
       system of equations A * X = B.

       @param input int n
           The number of linear equations, i.e., the order of the
           matrix A.  n >= 0.
       @param input int nrhs
           The number of right hand sides, i.e., the number of columns
           of the matrix B.  nrhs >= 0.
       @param (input/output) double[][] A of dimension (lda,n)
           On entry, the n-by-n coefficient matrix A.
           On exit, the factors L and U from the factorization
           A = P*L*U; the unit diagonal elements of L are not stored.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param output int[] ipiv of dimension (n)
           The pivot indices that define the permutation matrix P;
           row i of the matrix was interchanged with row ipiv[i].
       @param (input/output) double[][] B of dimension (ldb, nrhs)
           On entry, the n-by-nrhs matrix of right hand side matrix B.
           On exit, if info[0] = 0, the n-by-nrhs solution matrix X.
       @param input int ldb
           The leading dimension of the array B.  ldb >= max(1,n).
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
           > 0:  if info[0] = i, U[i-1][i-1] is exactly zero.  The factorization
                 has been completed, but the factor U is exactly
                 singular, so the solution could not be computed.
     */
    public void dgesv(int n, int nrhs, double[][] A, int lda, int ipiv[], double[][] B,
                      int ldb, int info[]) {
        // Test the input parameters.
        
        info[0] = 0;
        if (n < 0) {
            info[0] = -1;
        }
        else if (nrhs < 0) {
            info[0] = -2;
        }
        else if (lda < Math.max(1, n)) {
            info[0] = -4;
        }
        else if (ldb < Math.max(1,n)) {
            info[0] = -7;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgesv had info[0] = " + info[0]);
            return;
        }
    
        // Compute the LU factorization of A.
    
        dgetrf(n, n, A, lda, ipiv, info);
        if (info[0] == 0) {
    
            // Solve the system A*X = B, overwriting B with X.
    
            dgetrs('N', n, nrhs, A, lda, ipiv, B, ldb, info);
        } // if (info[0] == 0)
        return;
    
    } // dgesv
    
    /*
     * This is a port of a portion of LAPACK routine DGERFS.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgerfs improves the computed solution to a system of linear
       equations and provides error bounds and backward error estimates for
       the solution.

       @param input char trans
           Specifies the form of the system of equations:
           = 'N':  A * X = B     (No transpose)
           = 'T':  A**T * X = B  (Transpose)
           = 'C':  A**H * X = B  (Conjugate transpose = Transpose)
       @param input int n
           The order of the matrix A.  n >= 0.
       @param input int nrhs
           The number of right hand sides, i.e., the number of columns
           of the matrices B and X.  nrhs >= 0.
       @param input double[][] A of dimension (lda, n)
           The original N-by-N matrix A.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param input double[][] AF of dimension (ldaf, n)
           The factors L and U from the factorization A = P*L*U
           as computed by dgetrf.
       @param input int ldaf
           The leading dimension of the array AF.  ldaf >= max(1,n).
       @param input int[] ipiv of dimension (n)
           The pivot indices from dgetrf; for 1<=i<=n, row i of the
           matrix was interchanged with row ipiv[i].
       @param input double[][] B of dimension (ldb, nrhs)
           The right hand side matrix B.
       @param input int ldb
           The leading dimension of the array B.  ldb >= max(1,n).
       @param (input/output) double[][] X of dimension (ldx, nrhs)
           On entry, the solution matrix X, as computed by dgetrs.
           On exit, the improved solution matrix X.
       @param input int ldx
           The leading dimension of the array X.  ldx >= max(1,n).
       @param output double[] ferr of dimension (nrhs)
           The estimated forward error bound for each solution vector
           X(j) (the j-th column of the solution matrix X).
           If XTRUE is the true solution corresponding to X(j), ferr[j]
           is an estimated upper bound for the magnitude of the largest
           element in (X(j) - XTRUE) divided by the magnitude of the
           largest element in X(j).  The estimate is as reliable as
           the estimate for rcond, and is almost always a slight
           overestimate of the true error.
       @param output double[] berr of dimension (nrhs)
           The componentwise relative backward error of each solution
           vector X(j) (i.e., the smallest relative change in
           any element of A or B that makes X(j) an exact solution).
       @param output double[] work of dimension (3*n)
       @param output int[] iwork of dimension (n)
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
     */
    private void dgerfs(char trans, int n, int nrhs, double[][] A, int lda, double[][] AF,
                        int ldaf, int[] ipiv, double[][] B, int ldb, double[][] X, int ldx,
                        double[] ferr, double[] berr, double[] work, int[] iwork, int[] info) {
        // itmax is the maximum number of steps of iterative refinement.
        final int itmax = 5;
        boolean notran;
        char transt;
        int count;
        int i;
        int j;
        int k;
        int kase;
        int nz;
        int isave[] = new int[3];
        double eps;
        double lstres;
        double s;
        double safe1;
        double safe2;
        double safmin;
        double xk;
        
        // Test the input parameters.
        
        info[0] = 0;
        notran = ((trans == 'N') || (trans == 'n'));
        if (!notran && !((trans == 'T') || (trans == 't')) && !((trans == 'C') || (trans == 'c'))) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (nrhs < 0) {
            info[0] = -3;
        }
        else if (lda < Math.max(1, n)) {
            info[0] = -5;
        }
        else if (ldaf < Math.max(1, n)) {
            info[0] = -7;
        }
        else if (ldb < Math.max(1, n)) {
            info[0] = -10;
        }
        else if (ldx < Math.max(1, n)) {
            info[0] = -12;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgerfs had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (n == 0 || nrhs == 0) {
            for (j = 0; j < nrhs; j++) {
                ferr[j] = 0.0;
                berr[j] = 0.0;
            }
            return;
        }
    
        if (notran) {
            transt = 'T';
        }
        else {
            trans = 'N';
        }
    
        // nz = maximum number of nonzero elements in each row of A, plus 1
    
        nz = n + 1;
        eps = ge.dlamch('E'); // Epsilon
        safmin = ge.dlamch('S'); // Safe minimum
        safe1 = nz*safmin;
        safe2 = safe1 / eps;
    
        // Do for each right hand side
    
        /*for (j = 0; j < nrhs; j++) {
    
            count = 1;
            lstres = 3.0;
            while (true) {
    
                // Loop until stopping criterion is satisfied.
    
                // Compute residual R = B - op(A) * X,
                // where op(A) = A, A**T, or A**H, depending on trans.
             for (i = 0; i < n; i++) {
                 work[n+i] = B[i][j-1];
             }
             CALL DGEMV( TRANS, N, N, -ONE, A, LDA, X( 1, J ), 1, ONE,
         $               WORK( N+1 ), 1 )
    *
    *        Compute componentwise relative backward error from formula
    *
    *        max(i) ( abs(R(i)) / ( abs(op(A))*abs(X) + abs(B) )(i) )
    *
    *        where abs(Z) is the componentwise absolute value of the matrix
    *        or vector Z.  If the i-th component of the denominator is less
    *        than SAFE2, then SAFE1 is added to the i-th components of the
    *        numerator and denominator before dividing.
    *
             DO 30 I = 1, N
                WORK( I ) = ABS( B( I, J ) )
       30    CONTINUE
    *
    *        Compute abs(op(A))*abs(X) + abs(B).
    *
             IF( NOTRAN ) THEN
                DO 50 K = 1, N
                   XK = ABS( X( K, J ) )
                   DO 40 I = 1, N
                      WORK( I ) = WORK( I ) + ABS( A( I, K ) )*XK
       40          CONTINUE
       50       CONTINUE
             ELSE
                DO 70 K = 1, N
                   S = ZERO
                   DO 60 I = 1, N
                      S = S + ABS( A( I, K ) )*ABS( X( I, J ) )
       60          CONTINUE
                   WORK( K ) = WORK( K ) + S
       70       CONTINUE
             END IF
             S = ZERO
             DO 80 I = 1, N
                IF( WORK( I ).GT.SAFE2 ) THEN
                   S = MAX( S, ABS( WORK( N+I ) ) / WORK( I ) )
                ELSE
                   S = MAX( S, ( ABS( WORK( N+I ) )+SAFE1 ) /
         $             ( WORK( I )+SAFE1 ) )
                END IF
       80    CONTINUE
             BERR( J ) = S
    *
    *        Test stopping criterion. Continue iterating if
    *           1) The residual BERR(J) is larger than machine epsilon, and
    *           2) BERR(J) decreased by at least a factor of 2 during the
    *              last iteration, and
    *           3) At most ITMAX iterations tried.
    *
             IF( BERR( J ).GT.EPS .AND. TWO*BERR( J ).LE.LSTRES .AND.
         $       COUNT.LE.ITMAX ) THEN
    *
    *           Update solution and try again.
    *
                CALL DGETRS( TRANS, N, 1, AF, LDAF, IPIV, WORK( N+1 ), N,
         $                   INFO )
                CALL DAXPY( N, ONE, WORK( N+1 ), 1, X( 1, J ), 1 )
                LSTRES = BERR( J )
                COUNT = COUNT + 1
                continue;
             END IF
                break;
            } // while (true)
    *
    *        Bound error from formula
    *
    *        norm(X - XTRUE) / norm(X) .le. FERR =
    *        norm( abs(inv(op(A)))*
    *           ( abs(R) + NZ*EPS*( abs(op(A))*abs(X)+abs(B) ))) / norm(X)
    *
    *        where
    *          norm(Z) is the magnitude of the largest component of Z
    *          inv(op(A)) is the inverse of op(A)
    *          abs(Z) is the componentwise absolute value of the matrix or
    *             vector Z
    *          NZ is the maximum number of nonzeros in any row of A, plus 1
    *          EPS is machine epsilon
    *
    *        The i-th component of abs(R)+NZ*EPS*(abs(op(A))*abs(X)+abs(B))
    *        is incremented by SAFE1 if the i-th component of
    *        abs(op(A))*abs(X) + abs(B) is less than SAFE2.
    *
    *        Use DLACN2 to estimate the infinity-norm of the matrix
    *           inv(op(A)) * diag(W),
    *        where W = abs(R) + NZ*EPS*( abs(op(A))*abs(X)+abs(B) )))
    *
             DO 90 I = 1, N
                IF( WORK( I ).GT.SAFE2 ) THEN
                   WORK( I ) = ABS( WORK( N+I ) ) + NZ*EPS*WORK( I )
                ELSE
                   WORK( I ) = ABS( WORK( N+I ) ) + NZ*EPS*WORK( I ) + SAFE1
                END IF
       90    CONTINUE
    *
             KASE = 0
      100    CONTINUE
             CALL DLACN2( N, WORK( 2*N+1 ), WORK( N+1 ), IWORK, FERR( J ),
         $                KASE, ISAVE )
             IF( KASE.NE.0 ) THEN
                IF( KASE.EQ.1 ) THEN
    *
    *              Multiply by diag(W)*inv(op(A)**T).
    *
                   CALL DGETRS( TRANST, N, 1, AF, LDAF, IPIV, WORK( N+1 ),
         $                      N, INFO )
                   DO 110 I = 1, N
                      WORK( N+I ) = WORK( I )*WORK( N+I )
      110          CONTINUE
                ELSE
    *
    *              Multiply by inv(op(A))*diag(W).
    *
                   DO 120 I = 1, N
                      WORK( N+I ) = WORK( I )*WORK( N+I )
      120          CONTINUE
                   CALL DGETRS( TRANS, N, 1, AF, LDAF, IPIV, WORK( N+1 ), N,
         $                      INFO )
                END IF
                GO TO 100
             END IF
    *
    *        Normalize error.
    *
             LSTRES = ZERO
             DO 130 I = 1, N
                LSTRES = MAX( LSTRES, ABS( X( I, J ) ) )
      130    CONTINUE
             IF( LSTRES.NE.ZERO )
         $      FERR( J ) = FERR( J ) / LSTRES
    *
        } // for (j = 0; j < nrhs; j++)*/
    
        return;

    } // dgerfs
    
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
    public void dgetrf(int m, int n, double[][] A, int lda, int[] ipiv, int[] info) {
        int i;
        int iinfo[] = new int[1];
        int j;
        int jb;
        int nb;
        String name;
        String opts;
        double arr[][];
        int ivec[];
        int k;
        double arr2[][];
        double arr3[][];
        
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
        else {
    
            // Use blocked code.
    
            for (j = 1; j <= Math.min(m, n); j += nb) {
                jb = Math.min(Math.min(m, n)-j+1, nb);
    
                // Factor diagonal and subdiagonal blocks and test for exact
                // singularity.
    
                arr = new double[m-j+1][jb];
                for (i = 0; i < m-j+1; i++) {
                    for (k = 0; k < jb; k++) {
                        arr[i][k] = A[j-1+i][j-1+k];
                    }
                }
                ivec = new int[Math.min(m-j+1, jb)];
                dgetf2(m-j+1, jb, arr, lda, ivec, iinfo);
                for (i = 0; i < m-j+1; i++) {
                    for (k = 0; k < jb; k++) {
                        A[j-1+i][j-1+k] = arr[i][k];
                    }
                }
                for (i = 0; i < Math.min(m-j+1, jb); i++) {
                    ipiv[j-1+i] = ivec[i];
                }
    
                // Adjust info[0] and the pivot indices.
    
                if (info[0] == 0 && iinfo[0] > 0) {
                    info[0] = iinfo[0] + j - 1;
                }
                for (i = j; i <= Math.min(m, j+jb-1); i++) {
                    ipiv[i-1] = j - 1 + ipiv[i-1];
                } // for (i = j; i <= Math.min(m, j+jb-1); i++)
    
                // Apply interchanges to columns 1:j-1.
    
                dlaswp(j-1, A, lda, j, j+jb-1, ipiv, 1);
    
                if (j+jb <= n) {
    
                    // Apply interchanges to columns j+jb:n.
    
                    arr = new double[A.length][n-j-jb+1];
                    for (i = 0; i < A.length; i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                            arr[i][k] = A[i][j+jb-1+k];
                        }
                    }
                    dlaswp(n-j-jb+1, arr, lda, j, j+jb-1, ipiv, 1);
                    for (i = 0; i < A.length; i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                            A[i][j+jb-1+k] = arr[i][k];
                        }
                    }
    
                    // Compute block row of U.
                    arr = new double[jb][jb];
                    for (i = 0; i < jb; i++) {
                        for (k = 0; k < jb; k++) {
                            arr[i][k] = A[j-1+i][j-1+k];
                        }
                    }
                    arr2 = new double[Math.max(1,jb)][n-j-jb+1];
                    for (i = 0; i < Math.max(1, jb); i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                            arr2[i][k] = A[j-1+i][j+jb-1+k];
                        }
                    }
                    ge.dtrsm('L', 'L', 'N', 'U', jb,
                             n-j-jb+1, 1.0, arr, lda, arr2,
                             lda);
                   for (i = 0; i < Math.max(1, jb); i++) {
                       for (k = 0; k < n-j-jb+1; k++) {
                           A[j-1+i][j+jb-1+k] = arr2[i][k];
                       }
                   }
                   if (j+jb <= m) {
    
                       // Update trailing submatrix.
                       arr = new double[Math.max(1, m-j-jb+1)][jb];
                       for (i = 0; i < Math.max(1,m-j-jb+1); i++) {
                           for (k = 0; k < jb; k++) {
                               arr[i][k] = A[j+jb-1+i][j-1+k];    
                           }
                       }
                       arr3 = new double[Math.max(1,m-j-jb+1)][n-j-jb+1];
                       for (i = 0; i < Math.max(1, m-j-jb+1); i++) {
                           for (k = 0; k < n-j-jb+1; k++) {
                               arr3[i][k] = A[j+jb-1+i][j+jb-1+k];
                           }
                       }
                       ge.dgemm('N', 'N', m-j-jb+1, n-j-jb+1, jb, -1.0, arr, lda,
                                arr2, lda, 1.0, arr3, lda);
                       for (i = 0; i < Math.max(1, m-j-jb+1); i++) {
                          for (k = 0; k < n-j-jb+1; k++) {
                              A[j+jb-1+i][j+jb-1+k] = arr3[i][k];
                          }
                      }
                   } // if (j+jb <= m)
                } // if (j+jb <= n)
            } // for (j = 1; j <= Math.min(m, n); j += nb)
        } // else use blocked code
        return;

    } // dgetrf
    
    /*
     * This is a port of a portion of LAPACK routine DGETRI.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgetri computes the inverse of a matrix using the LU factorization
       computed by dgetrf.

       This method inverts U and then computes inv(A) by solving the system
       inv(A)*L = inv(U) for inv(A).
       
       @param input int n
           The order of the matrix A.  n >= 0.
       @param (input/output) double[][] A of (lda, n)
           On entry, the factors L and U from the factorization
           A = P*L*U as computed by dgetrf.
           On exit, if info[0] = 0, the inverse of the original matrix A.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param input int[] ipiv of dimension (n)
           The pivot indices from dgetrf; for 1<=i<=n, row i of the
           matrix was interchanged with row ipiv[i].
       @param output double[] work of dimension (max(1, lwork))
           On exit, if info[0] =0, then work[0] returns the optimal lwork.
       @param input int lwork
           The dimension of the array work.  lwork >= max(1,n).
           For optimal performance lwork >= n*nb, where nb is
           the optimal blocksize returned by ilaenv.

           If lwork = -1, then a workspace query is assumed; the routine
           only calculates the optimal size of the work array, returns
           this value as the first entry of the work array, and no error
           message related to lwork is issued.
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
           > 0:  if info[0] = i, U[i-1][i-1] is exactly zero; the matrix is
                 singular and its inverse could not be computed.
     */
    private void dgetri(int n, double[][] A, int lda, int[] ipiv, double[] work,
                        int lwork, int[] info) {
        boolean lquery;
        int i;
        int iws;
        int j;
        int jb;
        int jj;
        int jp;
        int ldwork;
        int lwkopt;
        int nb;
        int nbmin;
        int nn;
        String name;
        String opts;
        double arr[][];
        double vec[];
        double vec2[];
        int k;
        double arr2[][];
        int i1;
        int i2;
        double arr3[][];
        double temp;
        
        // Test the input parameters.
        
        info[0] = 0;
        name = new String("DGETRI");
        opts = new String(" ");
        nb = ge.ilaenv(1, name, opts, n, -1, -1, -1);
        lwkopt = n*nb;
        work[0] = lwkopt;
        lquery = (lwork == -1);
        if (n < 0) {
            info[0] = -1;
        }
        else if (lda < Math.max(1, n)) {
            info[0] = -3;
        }
        else if (lwork < Math.max(1, n) && !lquery) {
            info[0] = -6;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgetri had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
    
        // Quick return if possible
    
        if (n == 0) {
            return;
        }
    
        // Form inv(U).  If INFO[0] > 0 from dtrtri, then U is singular,
        // and the inverse is not computed.
    
        le.dtrtri('U', 'N', n, A, lda, info);
        if (info[0] > 0) {
            return;
        }
    
        nbmin = 2;
        ldwork = n;
        if (nb > 1 && nb < n) {
            iws = Math.max(ldwork*nb, 1);
            if (lwork < iws) {
                nb = lwork / ldwork;
                nbmin = Math.max(2, ge.ilaenv(2, name, opts, n, -1, -1, -1));
            } // if (lwork < iws)
        } // if (nb > 1 && nb < n)
        else {
            iws = n;
        }
    
        // Solve the equation inv(A)*L = inv(U) for inv(A).
    
        if (nb < nbmin || nb >= n) {
    
            // Use unblocked code.
    
            for (j = n; j >= 1; j--) {
    
                // Copy current column of L to WORK and replace with zeros.
    
                for(i = j + 1; i <= n; i++) {
                    work[i-1] = A[i-1][j-1];
                    A[i-1][j-1] = 0.0;
                } // for (i = j + 1; i <= n; i++)
    
                // Compute current column of inv(A).
    
                if (j < n) {
                    arr = new double[n][n-j];
                    for (i = 0; i < n; i++) {
                        for (k = 0; k < n-j; k++) {
                            arr[i][k] = A[i][j+k];
                        }
                    }
                    vec = new double[n-j];
                    for (i = 0; i < n-j; i++) {
                        vec[i] = work[j+i];
                    }
                    vec2 = new double[n];
                    for (i = 0; i < n; i++) {
                        vec2[i] = A[i][j-1];
                    }
                    ge.dgemv('N', n, n-j, -1.0, arr,
                             lda, vec, 1, 1.0, vec2, 1);
                    for (i = 0; i < n; i++) {
                        A[i][j-1] = vec2[i];
                    }
                } // if (j < n)
            } // for (j = n; j >= 1; j--)
        } // if (nb < nbmin || nb >= n)
        else {
    
            // Use blocked code.
    
            nn = ((n-1) / nb )*nb + 1;
            for (j = nn; j >= 1; j -= nb) {
                jb = Math.min(nb, n-j+1);
    
                // Copy current block column of L to WORK and replace with
                // zeros.
    
                for (jj = j; jj <= j + jb - 1; jj++) {
                    for (i = jj + 1; i <= n; i++) {
                        work[i+(jj-j)*ldwork-1] = A[i-1][jj-1];
                        A[i-1][jj-1] = 0.0;
                    } // for (i = jj + 1; i <= n; i++)
                } //  for (jj = j; jj <= j + jb - 1; jj++)
    
                // Compute current block column of inv(A).
    
                if (j+jb <= n) {
                    arr = new double[n][n-j-jb+1];
                    for (i = 0; i < n; i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                            arr[i][k] = A[i][j+jb-1+k];
                        }
                    }
                    arr2 = new double[n-j-jb+1][jb];
                    i = 0;
                    for (i2 = 0; i2 < jb; i2++) {
                        for (i1 = 0; i1 < n-j-jb+1; i1++) {
                            arr2[i1][i2] = work[j+jb-1+i];
                            i++;
                        }
                    }
                    arr3 = new double[n][jb];
                    for (i = 0; i < n; i++) {
                        for (k = 0; k < jb; k++) {
                            arr3[i][k] = A[i][j-1+k];
                        }
                    }
                    ge.dgemm('N', 'N', n, jb,
                             n-j-jb+1, -1.0, arr, lda,
                             arr2, ldwork, 1.0, arr3, lda);
                    for (i = 0; i < n; i++) {
                        for (k = 0; k < jb; k++) {
                            A[i][j-1+k] = arr3[i][k];
                        }
                    }
                } // if (j+jb <= n)
                arr = new double[jb][jb];
                i = 0;
                for (i2 = 0; i2 < jb; i2++) {
                    for (i1 = 0; i1 < jb; i1++) {
                        arr[i1][i2] = work[j-1+i];
                        i++;
                    }
                }
                arr2 = new double[n][jb];
                for (i = 0; i < n; i++) {
                    for (k = 0; k < jb; k++) {
                        arr2[i][k] = A[i][j-1+k];
                    }
                }
                ge.dtrsm('R', 'L', 'N', 'U', n, jb,
                         1.0, arr, ldwork, arr2, lda);
                for (i = 0; i < n; i++) {
                    for (k = 0; k < jb; k++) {
                        A[i][j-1+k] = arr2[i][k];
                    }
                }
            } // for (j = nn; j >= 1; j -= nb)
        } // else
    
        // Apply column interchanges.
    
        for (j = n - 1; j >= 1; j--) {
            jp = ipiv[j-1];
            if (jp != j) {
                for (i = 0; i < n; i++) {
                    temp = A[i][j-1];
                    A[i][j-1] = A[i][jp-1];
                    A[i][jp-1] = temp;
                }
            } // if (jp != j)
        } // for (j = n - 1; j >= 1; j--)
    
        work[0] = iws;
        return;

    } // dgetri
    
    /*
     * This is a port of a portion of LAPACK routine DGETRS.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgetrs solves a system of linear equations
          A * X = B  or  A**T * X = B
       with a general N-by-N matrix A using the LU factorization computed
       by dgetrf.
       
       @param input char trans
           Specifies the form of the system of equations:
           = 'N':  A * X = B  (No transpose)
           = 'T':  A**T* X = B  (Transpose)
           = 'C':  A**T* X = B  (Conjugate transpose = Transpose)
       @param input int n
           The order of the matrix A.  n >= 0.
       @param input int nrhs
           The number of right hand sides, i.e., the number of columns
           of the matrix B.  nrhs >= 0.
       @param input double[][] A of dimension (lda, n)
           The factors L and U from the factorization A = P*L*U
           as computed by dgetrf.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param input int[] ipiv of dimension (n)
           The pivot indices from dgetrf; for 1<=i<=n, row i of the
           matrix was interchanged with row ipiv[i].
       @param (input/output) double[][] B of dimension (ldb,nrhs)
           On entry, the right hand side matrix B.
           On exit, the solution matrix X.
       @param input int ldb
           The leading dimension of the array B.  ldb >= max(1,n).
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
     */
    private void dgetrs(char trans, int n, int nrhs, double[][] A, int lda, int[] ipiv,
                        double[][] B, int ldb, int[] info) {
        boolean notran;
        
        // Test the input parameters.
    
        info[0] = 0;
        notran = ((trans == 'N') || (trans == 'n'));
        if (!notran && !((trans == 'T') || (trans == 't')) && !((trans == 'C') || (trans == 'c'))) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (nrhs < 0) {
            info[0] = -3;
        }
        else if (lda < Math.max(1, n)) {
            info[0] = -5;
        }
        else if (ldb < Math.max(1, n)) {
            info[0] = -8;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgetrs had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (n == 0 || nrhs == 0) {
            return;
        }
    
        if (notran) {
    
            // Solve A * X = B.
    
            // Apply row interchanges to the right hand sides.
    
            dlaswp(nrhs, B, ldb, 1, n, ipiv, 1);
    
            // Solve L*X = B, overwriting B with X.
    
            ge.dtrsm('L', 'L', 'N', 'U', n, nrhs,
                     1.0, A, lda, B, ldb);
    
            // Solve U*X = B, overwriting B with X.
    
            ge.dtrsm('L', 'U', 'N', 'N', n,
                     nrhs, 1.0, A, lda, B, ldb);
        } // if (notran)
        else {
    
            // Solve A**T * X = B.
    
            // Solve U**T *X = B, overwriting B with X.
    
            ge.dtrsm('L', 'U', 'T', 'N', n, nrhs,
                     1.0, A, lda, B, ldb);
    
            // Solve L**T *X = B, overwriting B with X.
    
            ge.dtrsm('L', 'L', 'T', 'U', n, nrhs, 1.0,
                     A, lda, B, ldb);
    
            // Apply row interchanges to the solution vectors.
    
            dlaswp(nrhs, B, ldb, 1, n, ipiv, -1);
        } // else
    
        return;

    } // dgetrs
    
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