package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class LinearEquations implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    /**
     * Creates a new LinearEquations object.
     */
    public LinearEquations() {}
    
    /**
     * This is a port of LAPACK version test routine 3.4.0 DPOT01.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dpot01 reconstructs a symmetric positive definite matrix  A  from
       its L*L' or U'*U factorization and computes the residual
       norm( L*L' - A ) / ( N * norm(A) * eps) or
       norm( U'*U - A ) / ( N * norm(A) * eps),
       where eps is the machine epsilon.
       @param input char uplo
           Specifies whether the upper or lower triangular part of the
           symmetric matrix A is stored:
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input int n  The number of rows and columns of the matrix A.  n >= 0.
       @param input double[][] A of dimension (lda, n).  The original symmetric matrix A.
       @param input int lda  The leading dimension of the array A.  lda >= max(1, n).
       @param (input/output) double[][] afac of dimension (ldafac, n).
           On entry, the factor L or U from the L*L' or U'*U
           factorization of A.
           Overwritten with the reconstructed matrix, and then with the
           difference L*L' - A (or U'*U - A).
       @param input int ldafac  The leading dimension of the array afac.  ldafac >= max(1, n).
       @param output double[] rwork of dimension n
       @param output double[] resid of dimension 1
           If uplo = 'L', norm(L*L' - A) / (n * norm(A) * eps)
           If uplo = 'U', norm(U'*U - A) / (n * norm(A) * eps)
     */
     private void dpot01(char uplo, int n, double[][] A, int lda, double[][] afac, int ldafac, double rwork[], double resid[]) {
         int i;
         int j;
         int k;
         double anorm;
         double eps;
         double t;
         double vec[];
         double arr[][];
         
         // Quick exit if n = 0.
     
         if (n <= 0) {
             resid[0] = 0.0;
             return;
         }
     
         // Exit with resid[0] = 1/eps if anorm = 0.
     
         eps = ge.dlamch('E'); // Epsilon
         anorm = ge.dlansy('1', uplo, n, A, lda, rwork);
         if (anorm <= 0.0) {
             resid[0] = 1.0 / eps;
              return;
         }
     
         if ((uplo == 'U') || (uplo == 'u')) {
          // Compute the product U'*U, overwriting U.
             for (k = n; k >= 1; k--) {
     
                 // Compute the [k-1][k-1] element of the result.
                 vec = new double[k];
                 for (i = 0; i < k; i++) {
                     vec[i] = afac[i][k-1];
                 }
                 t = ge.ddot(k, vec, 1, vec, 1);
                 afac[k-1][k-1] = t;
      
                 // Compute the rest of column k-1.
                 for (i = 0; i < k-1; i++) {
                     vec[i] = afac[i][k-1];
                 }
                 ge.dtrmv('U', 'T', 'N', k-1, afac,
                          ldafac, vec, 1 );
                 for (i = 0; i < k-1; i++) {
                     afac[i][k-1] = vec[i];
                 }
             } // for (k = n; k >= 1; k--)
         } // if ((uplo == 'U') || (uplo == 'u'))
         else { 
             //  Compute the product L*L', overwriting L.
             for (k = n; k >= 1; k--) {
     
                 // Add a multiple of column k-1 of the factor L to each of
                 // columns k through n-1.
      
                 if (k+1 <= n) {
                     vec = new double[n-k];
                     for (i = 0; i < n-k; i++) {
                         vec[i] = afac[k+i][k-1];
                     }
                     arr = new double[n-k][n-k];
                     for (i = 0; i < n-k; i++) {
                         for (j = 0; j < n-k; j++) {
                             arr[i][j] = afac[k+i][k+j];
                         }
                     }
                     ge.dsyr('L', n-k, 1.0, vec, 1,
                             arr, ldafac);
                     for (i = 0; i < n-k; i++) {
                         for (j = 0; j < n-k; j++) {
                             afac[k+i][k+j] = arr[i][j];
                         }
                     }
                 } // if (k+1 <= n)
      
                 // Scale column k-1 by the diagonal element.
      
                 t = afac[k-1][k-1];
                 vec = new double[n-k+1];
                 for (i = 0; i < n-k+1; i++) {
                     vec[i] = afac[k-1+i][k-1];
                 }
                 ge.dscal(n-k+1, t, vec, 1);
                 for (i = 0; i < n-k+1; i++) {
                     afac[k-1+i][k-1] = vec[i];
                 }
             } // for (k = n; k >= 1; k--)
         } // else 
         
         // Compute the difference  L*L' - A (or U'*U - A).
      
         if ((uplo == 'U') || (uplo == 'u')) {
             for (j = 1; j <= n; j++) {
                 for (i = 1; i <= j; i++) {
                     afac[i-1][j-1] = afac[i-1][j-1] - A[i-1][j-1];
                 } 
             } // for (j = 1; j <= n; j++);
         } // if ((uplo == 'U') || (uplo == 'u'))
         else {
             for (j = 1; j <= n; j++) {
                 for (i = j; i <= n; i++) {
                     afac[i-1][j-1] = afac[i-1][j-1] - A[i-1][j-1];   
                 } // for (i = j; i <= n; i++)
             } // for (j = 1; j <= n; j++)
         } // else
     
         // Compute norm( L*U - A ) / (n * norm(A) * eps)
     
         resid[0] = ge.dlansy('1', uplo, n, afac, ldafac, rwork);
     
         resid[0] = ((resid[0] / (double)( n ) ) / anorm ) / eps;
     
         return;

     } // dpot01
     
     /**
      * This is a port of LAPACK version test routine 3.4.0 DPOT02.F created by the University of Tennessee, University
      * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
      *
      * dpot02 computes the residual for the solution of a symmetric system
        of linear equations  A*x = b:

        resid[0] = norm(B - A*X) / ( norm(A) * norm(X) * eps),
        where eps is the machine epsilon.

        @param input char uplo
            Specifies whether the upper or lower triangular part of the
            symmetric matrix A is stored:
            = 'U':  Upper triangular
            = 'L':  Lower triangular
        @param input int n  The number of rows and columns of the matrix A.  n >= 0.
        @param input int nrhs  The number of columns of B, the matrix of right hand sides.  nrhs >= 0.
        @param input double[][] A of dimension (lda, n).  The original symmetric matrix A.
        @param input int lda  The leading dimension of the array A.  lda >= max(1, n).
        @param input double[][] X of dimension (ldx, nrhs).
            The computed solution vectors for the system of linear equations.
        @param (input/output) double[][] B of dimension (ldb, nrhs)
            On entry, the right hand side vectors for the system of
            linear equations.
            On exit, B is overwritten with the difference B - A*X.
        @param input int ldb  The leading dimension of the array B.  ldb >= max(1, n).
        @param output double[] rwork of dimension n
        @param output double[] resid of dimension 1
            The maximum over the number of right hand sides of
            norm(B - A*X) / ( norm(A) * norm(X) * eps).
      */
      private void dpot02(char uplo, int n, int nrhs, double[][] A, int lda, double[][] X, int ldx,
                          double[][] B, int ldb, double[] rwork, double[] resid) {
          int j;
          double eps;
          double anorm;
          double bnorm;
          double xnorm;
          int i;
          
          // Quick exit if n = 0 or nrhs = 0.
       
          if (n <= 0 || nrhs <= 0) {
              resid[0] = 0.0;
              return;
          }
      
          // Exit with resid[0] = 1/eps if anorm = 0.
      
          eps = ge.dlamch('E'); // Epsilon
          anorm = ge.dlansy('1', uplo, n, A, lda, rwork);
          if (anorm <= 0.0) {
              resid[0] = 1.0 / eps;
              return;
          }
      
          // Compute  B - A*X
      
          ge.dsymm('L', uplo, n, nrhs, -1.0, A, lda, X, ldx, 1.0, B, ldb);
      
          // Compute the maximum over the number of right hand sides of
          // norm( B - A*X ) / ( norm(A) * norm(X) * eps) .
      
          resid[0] = 0.0;
          for (j = 0; j < nrhs; j++) {
               bnorm = 0.0;
               for (i = 0; i < n; i++) {
                   bnorm += Math.abs(B[i][j]);
               }
               xnorm = 0.0;
               for (i = 0; i < n; i++) {
                   xnorm += Math.abs(X[i][j]);
               }
               if (xnorm <= 0.0) {
                   resid[0] = 1.0 / eps;
               }
               else {
                   resid[0] = Math.max(resid[0], ( ( bnorm / anorm ) / xnorm ) / eps );
               }
          } // for (j = 0; j < nrhs; j++)
          
          return;

      } // dpot02
      
    /**
     * This is a port of LAPACK version test routine 3.4.0 DPOT03.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dpot03 computes the residual for a symmetric matrix times its inverse:
       norm( I - A*AINV ) / (n * norm(A) * norm(AINV) * eps),
       where eps is the machine epsilon.
       @param input char uplo
           Specifies whether the upper or lower triangular part of the
           symmetric matrix A is stored:
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input int n  The number of rows and columns of the matrix A.  n >= 0.
       @param input double[][] A of dimension (lda, n).  The original symmetric matrix A.
       @param input int lda  The leading dimension of the array A.  lda >= max(1, n).
       @param (input/output) double[][] AINV of dimension (ldainv, n)
           On entry, the inverse of the matrix A, stored as a symmetric
           matrix in the same format as A.
           In this version, AINV is expanded into a full matrix and
           multiplied by A, so the opposing triangle of AINV will be
           changed; i.e., if the upper triangular part of AINV is
           stored, the lower triangular part will be used as work space.
       @param input int ldainv  The leading dimension of the array AINV.  ldainv >= max(1, n).
       @param output double[][] work of dimension (ldwork, n).
       @param input int ldwork  The leading dimension of the array work.  ldwork >= max(1, n).
       @param output double[] rwork of dimension n
       @param output double[] rcond of dimension 1
           The reciprocal of the condition number of A, computed as
           ( 1/norm(A) ) / norm(AINV).
       @param output double[] resid of dimension 1
           norm(I - A*AINV) / (n * norm(A) * norm(AINV) * eps)
     */
     private void dpot03(char uplo, int n, double[][] A, int lda, double[][] AINV, int ldainv,
                         double[][] work, int ldwork, double rwork[], double[] rcond, double[] resid) {
         int i;
         int j;
         double ainvnm;
         double anorm;
         double eps;
         
         // Quick exit if n = 0.
     
         if (n <= 0) {
             rcond[0] = 1.0;
             resid[0] = 0.0;
             return;
         }
     
         // Exit with resid[0] = 1/eps if anorm = 0 or ainvnm = 0.
     
         eps = ge.dlamch('E'); // Epsilon
         anorm = ge.dlansy('1', uplo, n, A, lda, rwork);
         ainvnm = ge.dlansy('1', uplo, n, AINV, ldainv, rwork);
         if (anorm <= 0.0 || ainvnm <= 0.0) {
             rcond[0] = 0.0;
             resid[0] = 1.0 / eps;
             return;
         }
         rcond[0] = ( 1.0 / anorm ) / ainvnm;
     
         // Expand AINV into a full matrix and call dsymm to multiply
         // AINV on the left by A.
      
         if ((uplo == 'U') || (uplo == 'u')) {
             for (j = 1; j <= n; j++) {
                 for (i = 1; i <= j-1; i++) {
                     AINV[j-1][i-1] = AINV[i-1][j-1];
                 }
             } // for (j = 1; j <= n; j++)
         }
         else {
             for (j = 1; j <= n; j++) {
                 for (i = j+1; i <= n; i++) {
                     AINV[j-1][i-1] = AINV[i-1][j-1];
                 }
             } // for (j = 1; j <= n; j++)
         }
         ge.dsymm('L', uplo, n, n, -1.0, A, lda, AINV, ldainv, 0.0,
                  work, ldwork);
     
         // Add the identity matrix to WORK .
     
         for (i = 0; i < n; i++) {
             work[i][i] = work[i][i] + 1.0;
         }
     
         // Compute norm(I - A*AINV) / (n * norm(A) * norm(AINV) * eps)
     
         resid[0] = ge.dlange('1', n, n, work, ldwork, rwork);
     
         resid[0] = ( (resid[0]*rcond[0] ) / eps ) / (double)( n );
     
         return;

     } // dpot03
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DPOSV.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November, 2011.
     * 
     * dposv computes the solution to a system of linear equations A*X = B for PO matrices
     * 
     * dposv computes the solution to a real system of linear equations
     *     A * X = B
     * where A is an n by n symmetric positive definite matrix and X and B are n by nrhs matrices.
     * 
     * The Cholesky decomposition is used to factor A as 
     *     A = U**T * U, if uplo = 'U', or
     *     A = L * L**T, if uplo = 'L',
     * where U is an upper triangular matrix and L is a lower triangular matrix.  The factored form
     * of A is then used to solve the system of equations A * X = B.
     * @param input char uplo
     *     = 'U': Upper triangle of A is stored 
     *     = 'L': Lower triangle of A is stored
     * @param input int n  The number of linear equations, i.e., the order of the matrix A.  n >= 0.
     * @param input int nrhs  The number of right hand sides, i.e., the number of columns of the matrix B.
     *     nrhs >= 0.
     * @param (input/output) double[][] A of dimension (lda, n)
           On entry, the symmetric matrix A.  If uplo = 'U', the leading
           n-by-n upper triangular part of A contains the upper
           triangular part of the matrix A, and the strictly lower
           triangular part of A is not referenced.  If uplo = 'L', the
           leading n-by-n lower triangular part of A contains the lower
           triangular part of the matrix A, and the strictly upper
           triangular part of A is not referenced.

           On exit, if info[0] = 0, the factor U or L from the Cholesky
           factorization A = U**T*U or A = L*L**T.
       @param input int lda.  The leading dimension of the array A.  lda >= max(1, n).
       @param (input/output) double[][] B of dimension (ldb,nrhs)
           On entry, the n by nrhs right hand side of matrix B.
           On exit, if info[0] = 0, the n by nrhs solution matrix X.
       @param input int ldb  The leading dimension of the array B.  ldb >= max(1,n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
           > 0: if info[0] = i, the leading minor of order i of A is not positive definite, so the
                factorization could not be completed, and the solution has not been computed.
     */
    public void dposv(char uplo, int n, int nrhs, double[][] A, int lda, double[][] B, int ldb, int info[]) {
        
        // Test the input parameters.
    
        info[0] = 0;
        if (!((uplo == 'U') || (uplo == 'u')) && !((uplo == 'L') || (uplo == 'l'))) {
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
            info[0] = -7;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dposv had info[0] = " + info[0]);
            return;
        }
    
        // Compute the Cholesky factorization A = U**T*U or A = L*L**T.
   
        dpotrf(uplo, n, A, lda, info);
        if (info[0] == 0) {
                
            // Solve the system A*X = B, overwriting B with X.
                
            dpotrs(uplo, n, nrhs, A, lda, B, ldb, info);
        } // if (info[0] == 0)

        return;

    } // dposv
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DPOTRF.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November, 2011.
     * 
     * dpotrf computes the Cholesky factorization of a real symmetric positive definite matrix A.
     * 
     * The factorization has the form
     *     A = U**T * U, if uplo = 'U', or
     *     A = L * L**T, if uplo = 'L',
     * where U is an upper triangular matrix and L is lower triangular.
     * 
     * This is the block version of the algorithm
     * 
     * @param input char uplo
     *     = 'U': Upper triangle of A is stored 
     *     = 'L': Lower triangle of A is stored
     * @param input int n  The order of the matrix A.  n >= 0.
     * @param (input/output) double[][] A of dimension (lda, n)
            On entry, the symmetric matrix A.  If uplo = 'U', the leading
            n by n upper triangular part of A contains the upper
            triangular part of the matrix A, and the strictly lower
            triangular part of A is not referenced.  If uplo = 'L', the
            leading n by n lower triangular part of A contains the lower
            triangular part of the matrix A, and the strictly upper
            triangular part of A is not referenced.

            On exit, if info[0] = 0, the factor U or L from the Cholesky
            factorization A = U**T *U  or A = L*L**T.
       @param input int lda.  The leading dimension of the array A.  lda >= max(1, n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
           > 0: if info[0] = i, the leading minor of order i is not
                positive definite, and the factorization could not be
                completed.
     */
    private void dpotrf(char uplo, int n, double[][] A, int lda, int info[]) {
        boolean upper;
        int j;
        int jb;
        int nb;
        int i;
        double arr[][];
        int k;
        String name;
        char optsChar[] = new char[1];
        String opts;
        double arr2[][];
        double arr3[][];
        
        // Test the input parameters.
        
          info[0] = 0;
          upper = ((uplo == 'U') || (uplo == 'u'));
          if (!upper && !((uplo == 'L') || (uplo == 'l')) ) {
             info[0] = -1;
          }
          else if (n < 0) {
             info[0] = -2;
          }
          else if (lda < Math.max(1, n)) {
             info[0] = -4;
          }
          if (info[0] != 0) {
             MipavUtil.displayError("dptorf had info[0] = " + info[0]);
             return;
          }
    
          // Quick return if possible
    
          if (n == 0) {
             return;
          }
    
          // Determine the block size for this environment.
          name = new String("DPOTRF");
          optsChar[0] = uplo;
          opts = new String(optsChar);
          nb = ge.ilaenv(1, name, opts, n, -1, -1, -1);
          if (nb <= 1 || nb >= n) {
       
              // Use unblocked code.
      
              dpotf2(uplo, n, A, lda, info);
          }
          else { // blocked code
      
              // Use blocked code.
      
              if (upper) {
      
                  // Compute the Cholesky factorization A = U**T*U.
       
                  for (j = 1; j <= n; j += nb) {
      
                     // Update and factorize the current diagonal block and test
                     // for non-positive-definiteness.
       
                     jb = Math.min(nb, n-j+1);
                     arr = new double[j-1][jb];
                     for (i = 0; i < j-1; i++) {
                         for (k = 0; k < jb; k++) {
                             arr[i][k] = A[i][j-1+k];
                         }
                     }
                     arr2 = new double[jb][jb];
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             arr2[i][k] = A[j-1+i][j-1+k];
                         }
                     }
                     ge.dsyrk('U', 'T', jb, j-1, -1.0,
                              arr, lda, 1.0, arr2, lda);
                     dpotf2('U', jb, arr2, lda, info);
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             A[j-1+i][j-1+k] = arr2[i][k];
                         }
                     }
                     if (info[0] != 0) {
                         info[0] = info[0] + j - 1;
                         return;
                     }
                     if (j+jb <= n) {
       
                        // Compute the current block row.
                        arr = new double[j-1][jb];
                        for (i = 0; i < j-1; i++) {
                            for (k = 0; k < jb; k++) {
                                arr[i][k] = A[i][j-1+k];
                            }
                        }
                        arr2 = new double[j-1][n-j-jb+1];
                        for (i = 0; i < j-1; i++) {
                            for (k = 0; k < n-j-jb+1; k++) {
                                arr2[i][k] = A[i][j+jb-1+k];
                            }
                        }
                        arr3 = new double[jb][n-j-jb+1];
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < n-j-jb+1; k++) {
                                arr3[i][k] = A[j-1+i][j+jb-1+k];
                            }
                        }
                        ge.dgemm('T', 'N', jb, n-j-jb+1,
                                 j-1, -1.0, arr, lda, arr2,
                                 lda, 1.0, arr3, lda);
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < n-j-jb+1; k++) {
                                A[j-1+i][j+jb-1+k] = arr3[i][k];
                            }
                        }
                        arr = new double[jb][jb];
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < jb; k++) {
                                arr[i][k] = A[j-1+i][j-1+k];
                            }
                        }
                        ge.dtrsm('L', 'U', 'T', 'N',
                                 jb, n-j-jb+1, 1.0, arr, lda,
                                 arr3, lda);
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < n-j-jb+1; k++) {
                                A[j-1+i][j+jb-1+k] = arr3[i][k];
                            }
                        }
                     } // if (j+jb <= n)
                  } // for (j = 1; j <= n; j += nb)
       
              } // if (upper)
              else { // lower
       
                  // Compute the Cholesky factorization A = L*L**T.
       
                  for (j = 1 ;j <= n; j += nb) {
       
                     // Update and factorize the current diagonal block and test
                     // for non-positive-definiteness.
       
                     jb = Math.min(nb, n-j+1);
                     arr = new double[jb][j-1];
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < j-1; k++) {
                             arr[i][k] = A[j-1+i][k];
                         }
                     }
                     arr2 = new double[jb][jb];
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             arr2[i][k] = A[j-1+i][j-1+k];
                         }
                     }
                     ge.dsyrk('L', 'N', jb, j-1, -1.0,
                              arr, lda, 1.0, arr2, lda);
                     dpotf2('L', jb, arr2, lda, info);
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             A[j-1+i][j-1+k] = arr2[i][k];
                         }
                     }
                     if (info[0] != 0) {
                         info[0] = info[0] + j - 1;
                         return;
                     }
                     if (j+jb <= n) {
       
                        // Compute the current block column.
                        arr = new double[n-j-jb+1][j-1];
                        for (i = 0; i < n-j-jb+1; i++) {
                            for (k = 0; k < j-1; k++) {
                                arr[i][k] = A[j+jb-1+i][k];
                            }
                        }
                        arr2 = new double[jb][j-1];
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < j-1; k++) {
                                arr2[i][k] = A[j-1+i][k];
                            }
                        }
                        arr3 = new double[n-j-jb+1][jb];
                        for (i = 0; i < n-j-jb+1; i++) {
                            for (k = 0; k < jb; k++) {
                                arr3[i][k] = A[j+jb-1+i][j-1+k];
                            }
                        }
                        ge.dgemm('N', 'T', n-j-jb+1, jb,
                                 j-1, -1.0, arr, lda, arr2,
                                 lda, 1.0, arr3, lda);
                        for (i = 0; i < n-j-jb+1; i++) {
                            for (k = 0; k < jb; k++) {
                                A[j+jb-1+i][j-1+k] = arr3[i][k];
                            }
                        }
                        arr = new double[jb][jb];
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < jb; k++) {
                                arr[i][k] = A[j-1+i][j-1+k];
                            }
                        }
                        ge.dtrsm('R', 'L', 'T', 'N',
                                 n-j-jb+1, jb, 1.0, arr, lda,
                                 arr3, lda);
                        for (i = 0; i < n-j-jb+1; i++) {
                            for (k = 0; k < jb; k++) {
                                A[j+jb-1+i][j-1+k] = arr3[i][k];
                            }
                        }
                     } // if (j+jb <= n)
                  } // for (j = 1; j <= n; j += nb)
              } // else lower
          } // else blocked code
           
          return;
    } // dpotrf
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DPOTRS.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November, 2011.
     * 
     * dpotrs solves a system of linear equations A*X = B with a symmetric positive definite matrix A
     * using the Cholesky factorization A = U**T*U or A = L*L**T computed by dpotrf.
     * 
     * @param input char uplo
     *     = 'U': Upper triangle of A is stored 
     *     = 'L': Lower triangle of A is stored
     * @param input int n  The order of the matrix A.  n >= 0.
     * @param input int nrhs  The number of right hand sides, i.e., the number of columns of the matrix B.
     *     nrhs >= 0.
     * @param input double[][] A of dimension (lda, n)
           The triangular factor U or L from the Cholesky factorization 
           A = U**T*U or A = L*L**T, as computed by dpotrf.
       @param input int lda.  The leading dimension of the array A.  lda >= max(1, n).
       @param (input/output) double[][] B of dimension (ldb,nrhs)
           On entry, the right hand side of the array B.
           On exit, the solution matrix X.
       @param input int ldb  The leading dimension of the array B.  ldb >= max(1,n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
     */
    private void dpotrs(char uplo, int n, int nrhs, double[][] A, int lda, double[][] B, int ldb, int info[]) {
        boolean upper;
        
        // Test the input parameters.
    
        info[0] = 0;
        upper = ((uplo == 'U') || (uplo == 'u'));
        if (!upper && !((uplo == 'L') || (uplo == 'l'))) {
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
            info[0] = -7;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dpotrs had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (n == 0 || nrhs == 0) {
            return;
        }
    
        if (upper) {
    
            // Solve A*X = B where A = U**T *U.
    
            // Solve U**T *X = B, overwriting B with X.
    
            ge.dtrsm('L', 'U', 'T', 'N', n, nrhs,
                      1.0, A, lda, B, ldb);
    
            // Solve U*X = B, overwriting B with X.
     
            ge.dtrsm('L', 'U', 'N', 'N', n,
                      nrhs, 1.0, A, lda, B, ldb);
        } // if (upper)
        else { // lower
    
            // Solve A*X = B where A = L*L**T.
    
            // Solve L*X = B, overwriting B with X.
    
            ge.dtrsm('L', 'L', 'N', 'N', n,
                     nrhs, 1.0, A, lda, B, ldb);
    
            // Solve L**T *X = B, overwriting B with X.
     
            ge.dtrsm('L', 'L', 'T', 'N', n, nrhs,
                     1.0, A, lda, B, ldb);
        } // else lower 
    
        return;

    } // dpotrs
    
    /**
     * This is a port of LAPACK version routine 3.4.2 DPOTF2.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., September 2012.
     * 
     * dpotf2 computes the Cholesky factorization of a symmetric Hermetian positive definite matrix (unblocked
     * algorithm).
     * 
     * dpotf2 computes the Cholesky factorization of a real symmetric positive definite matrix A.
     * 
     * The factorization has the form
     *     A = U**T * U, if uplo = 'U', or
     *     A = L * L**T, if uplo = 'L',
     * where U is an upper triangular matrix and L is lower triangular.
     * 
     * This is the unblocked version of the algorithm
     * 
     * @param input char uplo Specifies whether the upper or lower triangular part of the symmetric matrix A is stored.
     *     = 'U': Upper triangular 
     *     = 'L': Lower triangular
     * @param input int n  The order of the matrix A.  n >= 0.
     * @param (input/output) double[][] A of dimension (lda, n)
            On entry, the symmetric matrix A.  If uplo = 'U', the leading
            n by n upper triangular part of A contains the upper
            triangular part of the matrix A, and the strictly lower
            triangular part of A is not referenced.  If uplo = 'L', the
            leading n by n lower triangular part of A contains the lower
            triangular part of the matrix A, and the strictly upper
            triangular part of A is not referenced.

            On exit, if info[0] = 0, the factor U or L from the Cholesky
            factorization A = U**T *U  or A = L*L**T.
       @param input int lda.  The leading dimension of the array A.  lda >= max(1, n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -k, the k-th argument had an illegal value
           > 0: if info[0] = k, the leading minor of order k is not
                positive definite, and the factorization could not be
                completed.
     */
    private void dpotf2(char uplo, int n, double[][] A, int lda, int info[]) {
        boolean upper;
        int j;
        double ajj;
        double vec[];
        int i;
        double arr[][];
        int k;
        double vec2[];
        
        // Test the input parameters.
        
          info[0] = 0;
          upper = ((uplo == 'U') || (uplo == 'u'));
          if (!upper && !((uplo == 'L') || (uplo == 'l')) ) {
             info[0] = -1;
          }
          else if (n < 0) {
             info[0] = -2;
          }
          else if (lda < Math.max(1, n)) {
             info[0] = -4;
          }
          if (info[0] != 0) {
             MipavUtil.displayError("dptof2 had info[0] = " + info[0]);
             return;
          }
    
          // Quick return if possible
    
          if (n == 0) {
             return;
          }
    
          if (upper) {
     
             // Compute the Cholesky factorization A = U**T *U.
     
             for (j = 1; j <= n; j++) {
     
                // Compute U[j-1][j-1] and test for non-positive-definiteness.
                vec = new double[j-1];
                for (i = 0; i < j-1; i++) {
                    vec[i] = A[i][j-1];
                }
                ajj = A[j-1][j-1] - ge.ddot(j-1, vec, 1, vec, 1);
                if (ajj <= 0.0 || Double.isNaN(ajj)) {
                   A[j-1][j-1] = ajj;
                   info[0] = j;
                   return;
                }
                ajj = Math.sqrt(ajj);
                A[j-1][j-1] = ajj;
    
                // Compute elements j:n-1 of row j-1.
    
                if (j < n) {
                   arr = new double[j-1][n-j];
                   for (i = 0; i < j-1; i++) {
                       for (k = 0; k < n-j; k++) {
                           arr[i][k] = A[i][j+k];
                       }
                   }
                   vec = new double[j-1];
                   for (i = 0; i < j-1; i++) {
                       vec[i] =  A[i][j-1];
                   }
                   vec2 = new double[n-j];
                   for (i = 0; i < n-j; i++) {
                       vec2[i] = A[j-1][j+i];
                   }
                   ge.dgemv('T', j-1, n-j, -1.0, arr,
                               lda, vec, 1, 1.0, vec2, 1);
                   ge.dscal(n-j, 1.0/ajj, vec2, 1);
                   for (i = 0; i < n-j; i++) {
                       A[j-1][j+i] = vec2[i];
                   }
                } // if (j < n)
             } // for (j = 1; j <= n; j++)
          } // if (upper)
          else { // lower
     
             // Compute the Cholesky factorization A = L*L**T.
     
             for (j = 1; j <= n; j++) {
     
                // Compute L[j-1][j-1] and test for non-positive-definiteness.
                vec = new double[j-1];
                for (i = 0; i < j-1; i++) {
                    vec[i] = A[j-1][i];
                }
                ajj = A[j-1][j-1] - ge.ddot(j-1, vec, 1, vec, 1);
                if (ajj <= 0.0 || Double.isNaN(ajj)) {
                   A[j-1][j-1] = ajj;
                   info[0] = j;
                   return;
                }
                ajj = Math.sqrt(ajj);
                A[j-1][j-1] = ajj;
    
                // Compute elements j:n-1 of column j-1.
    
                if (j < n) {
                   arr = new double[n-j][j-1];
                   for (i = 0; i < n-j; i++) {
                       for (k = 0; k < j-1; k++) {
                           arr[i][k] = A[j+i][k];
                       }
                   }
                   vec = new double[j-1];
                   for (i = 0; i < j-1; i++) {
                       vec[i] = A[j-1][i];
                   }
                   vec2 = new double[n-j];
                   for (i = 0; i < n-j; i++) {
                       vec2[i] = A[j+i][j-1];
                   }
                   ge.dgemv('N', n-j, j-1, -1.0, arr,
                            lda, vec, 1, 1.0, vec2, 1);
                   ge.dscal(n-j, 1.0/ajj, vec2, 1);
                   for (i = 0; i < n-j; i++) {
                       A[j+i][j-1] = vec2[i];
                   }
                } // if (j < n)
             } // for (j = 1; j <= n; j++)
          } // else lower
          
          return;
    } // dpotf2
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DPOTRI.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dpotri computes the inverse of a real symmetric positive definite
       matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
       computed by dpotrf.
       
       @param input char uplo
           = 'U': Upper triangle of A is stored;
           = 'L': Lower triangle of A is stored.
       @param input int n  The order of the matrix A.  n >= 0.
       @param (input/output) double[][] of dimension (lda, n)
           On entry, the triangular factor U or L from the Cholesky
           factorization A = U**T*U or A = L*L**T, as computed by dpotrf.
           On exit, the upper or lower triangle of the (symmetric)
           inverse of A, overwriting the input factor U or L.
       @param input int lda  The leading dimension of the array A.   lda >= max(1,n).
       @param output int[] of dimension 1.
           = 0: successful exit
           < 0: if info = -i, the i-th argument had an illegal value
           > 0: if info = i, the [i-1][i-1] element of the factor U or L is zero,
                and the inverse could not be computed.
     */
    private void dpotri(char uplo, int n, double[][] A, int lda, int info[]) {
     // Test the input parameters.
        
        info[0] = 0;
        if (!((uplo == 'U') || (uplo == 'u')) && !((uplo == 'L') || (uplo == 'l')) ) {
           info[0] = -1;
        }
        else if (n < 0) {
           info[0] = -2;
        }
        else if (lda < Math.max(1, n)) {
           info[0] = -4;
        }
        if (info[0] != 0) {
           MipavUtil.displayError("dpotri had info[0] = " + info[0]);
           return;
        }
  
        // Quick return if possible
  
        if (n == 0) {
           return;
        }   
        
        // Invert the triangular Cholesky factor U or L.
        
        dtrtri(uplo, 'N', n, A, lda, info);
        if (info[0] > 0) {
            return;
        }
        
        // Form inv(U) * inv(U)**T or inv(L)**T * inv(L).
        
        dlauum(uplo, n, A, lda, info);
        
        return;

    } // dpotri
    
    /**
     * This is a port of LAPACK version auxiliary routine 3.4.2 DLAUUM.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., September 2012.
     *
     * dlauum computes the product UUH or LHL, where U and L are upper or lower triangular matrices (blocked algorithm).
     * 
     * dlauum computes the product U * U**T or L**T * L, where the triangular
       factor U or L is stored in the upper or lower triangular part of the array A.

       If uplo = 'U' or 'u' then the upper triangle of the result is stored,
       overwriting the factor U in A.
       If uplo = 'L' or 'l' then the lower triangle of the result is stored,
       overwriting the factor L in A.

       This is the blocked form of the algorithm
       @param input char uplo  Specifies whether the triangular factor stored in the array 
           is upper or lower triangular:
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input int n  The order of the triangular factor U or L.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the triangular factor U or L.
           On exit, if uplo = 'U', the upper triangle of A is
           overwritten with the upper triangle of the product U * U**T;
           if uplo = 'L', the lower triangle of A is overwritten with
           the lower triangle of the product L**T * L.
       @param input int lda  The leading dimension of array A.  lda >= max(1, n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: If info[0] = -k, the k-th argument had an illegal value
     */
     private void dlauum(char uplo, int n, double[][] A, int lda, int info[]) {
         boolean upper;
         int i;
         int ib;
         int nb;
         int j;
         double arr[][];
         int k;
         String name;
         char charOpts[] = new char[1];
         String opts;
         double arr2[][];
         double arr3[][];
         
         // Test the input parameters.
         
         info[0] = 0;
         upper = ((uplo == 'U') || (uplo == 'u'));
         if (!upper && !((uplo == 'L') || (uplo == 'l')) ) {
            info[0] = -1;
         }
         else if (n < 0) {
            info[0] = -2;
         }
         else if (lda < Math.max(1, n)) {
            info[0] = -4;
         }
         if (info[0] != 0) {
            MipavUtil.displayError("dlauum had info[0] = " + info[0]);
            return;
         }
   
         // Quick return if possible
   
         if (n == 0) {
            return;
         }
         
         // Determine the block size for this environment.
     
         name = new String("DLAUUM");
         charOpts[0] = uplo;
         opts = new String(charOpts);
         nb = ge.ilaenv(1, name, opts, n, -1, -1, -1);
     
         if (nb <= 1 || nb >= n) {
     
             // Use unblocked code
     
             dlauu2(uplo, n, A, lda, info);
         }
         else { 
     
             // Use blocked code
   
             if (upper) {
     
                 // Compute the product U * U**T.
     
                 for (i = 1; i <= n; i += nb) {
                     ib = Math.min(nb, n-i+1);
                     arr = new double[ib][ib];
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             arr[j][k] = A[i-1+j][i-1+k];
                         }
                     }
                     arr2 = new double[i-1][ib];
                     for (j = 0; j < i-1; j++) {
                         for (k = 0; k < ib; k++) {
                             arr2[j][k] = A[j][i-1+k];
                         }
                     }
                     ge.dtrmm('R', 'U', 'T', 'N',
                              i-1, ib, 1.0, arr, lda, arr2, lda);
                     for (j = 0; j < i-1; j++) {
                         for (k = 0; k < ib; k++) {
                             A[j][i-1+k] = arr2[j][k];
                         }
                     }
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             arr[j][k] = A[i-1+j][i-1+k];
                         }
                     }
                     dlauu2('U', ib, arr, lda, info);
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                              A[i-1+j][i-1+k] = arr[j][k];
                         }
                     }
                     if (i+ib <= n) {
                         arr = new double[i-1][n-i-ib+1];
                         for (j = 0; j < i-1; j++) {
                             for (k = 0; k < n-i-ib+1; k++) {
                                 arr[j][k] = A[j][i+ib-1+k];
                             }
                         }
                         arr2 = new double[ib][n-i-ib+1];
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < n-i-ib+1; k++) {
                                 arr2[j][k] = A[i-1+j][i+ib-1+k];
                             }
                         }
                         arr3 = new double[i-1][ib];
                         for (j = 0; j < i-1; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr3[j][k] = A[j][i-1+k];
                             }
                         }
                         ge.dgemm('N', 'T', i-1, ib,
                                   n-i-ib+1, 1.0, arr, lda,
                                   arr2, lda, 1.0, arr3, lda);
                         for (j = 0; j < i-1; j++) {
                             for (k = 0; k < ib; k++) {
                                 A[j][i-1+k] = arr3[j][k];
                             }
                         }
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < n-i-ib+1; k++) {
                                 arr2[j][k] = A[i-1+j][i+ib-1+k];
                             }
                         }
                         arr = new double[ib][ib];
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr[j][k] = A[i-1+j][i-1+k];
                             }
                         }
                         ge.dsyrk('U', 'N', ib, n-i-ib+1,
                                  1.0, arr2, lda, 1.0, arr, lda);
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < ib; k++) {
                                 A[i-1+j][i-1+k] = arr[j][k];
                             }
                         }
                     } // if (i+ib <= n)
                 } // for (i = 1; i <= n; i += nb)
             } // if (upper)
             else { // lower
     
                 // Compute the product L**T * L.
     
                 for (i = 1; i <= n; i += nb) {
                     ib = Math.min(nb, n-i+1);
                     arr = new double[ib][ib];
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             arr[j][k] = A[i-1+j][i-1+k];
                         }
                     }
                     arr2 = new double[ib][i-1];
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < i-1; k++) {
                             arr2[j][k] = A[i-1+j][k];
                         }
                     }
                     ge.dtrmm('L', 'L', 'T', 'N', ib,
                              i-1, 1.0, arr, lda, arr2, lda);
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < i-1; k++) {
                             A[i-1+j][k] = arr2[j][k];
                         }
                     }
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             arr[j][k] = A[i-1+j][i-1+k];
                         }
                     }
                     dlauu2('L', ib, arr, lda, info);
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             A[i-1+j][i-1+k] = arr[j][k];
                         }
                     }
                     if (i+ib <= n) {
                         arr = new double[n-i-ib+1][ib];
                         for (j = 0; j < n-i-ib+1; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr[j][k] = A[i+ib-1+j][i-1+k];
                             }
                         }
                         arr2 = new double[n-i-ib+1][i-1];
                         for (j = 0; j < n-i-ib+1; j++) {
                             for (k = 0; k < i-1; k++) {
                                 arr2[j][k] = A[i+ib-1+j][k];
                             }
                         }
                         arr3 = new double[ib][i-1];
                         for (j = 0; j < ib; j ++) {
                             for (k = 0; k < i-1; k++) {
                                 arr3[j][k] = A[i-1+j][k];
                             }
                         }
                         ge.dgemm('T', 'N', ib, i-1,
                                  n-i-ib+1, 1.0, arr, lda,
                                  arr2, lda, 1.0, arr3, lda);
                         for (j = 0; j < ib; j ++) {
                             for (k = 0; k < i-1; k++) {
                                 A[i-1+j][k] = arr3[j][k];
                             }
                         }
                         for (j = 0; j < n-i-ib+1; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr[j][k] = A[i+ib-1+j][i-1+k];
                             }
                         }
                         arr2 = new double[ib][ib];
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr2[j][k] = A[i-1+j][i-1+k];
                             }
                         }
                         ge.dsyrk('L', 'T', ib, n-i-ib+1, 1.0,
                                  arr, lda, 1.0, arr2, lda);
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < ib; k++) {
                                 A[i-1+j][i-1+k] = arr2[j][k];
                             }
                         }
                     } // if (i+ib <= n)
                 } // for (i = 1; i <= n; i += nb)
             } // else lower
         } // else use blocked code
     
         return;

     } // dlauum
    
    /**
     * This is a port of LAPACK version auxiliary routine 3.4.2 DLAUU2.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., September 2012.
     *
     * dlauu2 computes the product UUH or LHL, where U and L are upper or lower triangular matrices (unblocked algorithm).
     * 
     * dlauu2 computes the product U * U**T or L**T * L, where the triangular
       factor U or L is stored in the upper or lower triangular part of the array A.

       If uplo = 'U' or 'u' then the upper triangle of the result is stored,
       overwriting the factor U in A.
       If uplo = 'L' or 'l' then the lower triangle of the result is stored,
       overwriting the factor L in A.

       This is the unblocked form of the algorithm
       @param input char uplo  Specifies whether the triangular factor stored in the array 
           is upper or lower triangular:
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input int n  The order of the triangular factor U or L.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the triangular factor U or L.
           On exit, if uplo = 'U', the upper triangle of A is
           overwritten with the upper triangle of the product U * U**T;
           if uplo = 'L', the lower triangle of A is overwritten with
           the lower triangle of the product L**T * L.
       @param input int lda  The leading dimension of array A.  lda >= max(1, n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: If info[0] = -k, the k-th argument had an illegal value
     */
     private void dlauu2(char uplo, int n, double[][] A, int lda, int info[]) {
         boolean upper;
         int i;
         double aii;
         double vec[];
         int j;
         double arr[][];
         double vec2[];
         int k;
         
         // Test the input parameters.
         
         info[0] = 0;
         upper = ((uplo == 'U') || (uplo == 'u'));
         if (!upper && !((uplo == 'L') || (uplo == 'l')) ) {
            info[0] = -1;
         }
         else if (n < 0) {
            info[0] = -2;
         }
         else if (lda < Math.max(1, n)) {
            info[0] = -4;
         }
         if (info[0] != 0) {
            MipavUtil.displayError("dlauu2 had info[0] = " + info[0]);
            return;
         }
   
         // Quick return if possible
   
         if (n == 0) {
            return;
         }
         
         if (upper) {
         
             // Compute the product U * U**T.
         
             for (i = 1; i <= n; i++) {
                 aii = A[i-1][i-1];
                 if (i < n) {
                     vec = new double[n-i+1];
                     for (j = 0; j < n-i+1; j++) {
                         vec[j] = A[i-1][i-1+j];
                     }
                     A[i-1][i-1] = ge.ddot(n-i+1, vec, 1, vec, 1);
                     arr = new double[i-1][n-i];
                     for (j = 0; j < i-1; j++) {
                         for (k = 0; k < n-i; k++) {
                             arr[j][k] = A[j][i+k];
                         }
                     }
                     vec = new double[n-i];
                     for (j = 0; j < n-i; j++) {
                         vec[j] = A[i-1][i+j];
                     }
                     vec2 = new double[i-1];
                     for (j = 0; j < i-1; j++) {
                         vec2[j] = A[j][i-1];
                     }
                     ge.dgemv('N', i-1, n-i, 1.0, arr,
                              lda, vec, 1, aii, vec2, 1);
                     for (j = 0; j < i-1; j++) {
                         A[j][i-1] = vec2[j];
                     }
                 }
                 else {
                     vec = new double[i];
                     for (j = 0; j < i; j++) {
                         vec[j] = A[j][i-1];
                     }
                     ge.dscal(i, aii, vec, 1 );
                     for (j = 0; j < i; j++) {
                         A[j][i-1] = vec[j];
                     }
                 }
             } // for (i = 1; i <= n; i++)
         
         } // if (upper)
         else { // lower
        
             // Compute the product L**T * L.
         
             for (i = 1; i <= n; i++) {
                 aii = A[i-1][i-1];
                 if (i < n) {
                     vec = new double[n-i+1];
                     for (j = 0; j < n-i+1; j++) {
                         vec[j] = A[i-1+j][i-1];
                     }
                     A[i-1][i-1] = ge.ddot(n-i+1, vec, 1, vec, 1 );
                     arr = new double[n-i][i-1];
                     for (j = 0; j < n-i; j++) {
                         for (k = 0; k < i-1; k++) {
                             arr[j][k] = A[i+j][k];
                         }
                     }
                     vec = new double[n-i];
                     for (j = 0; j < n-i; j++) {
                         vec[j] = A[i+j][i-1];
                     }
                     vec2 = new double[i-1];
                     for (j = 0; j < i-1; j++) {
                         vec2[j] = A[i-1][j];
                     }
                     ge.dgemv('T', n-i, i-1, 1.0, arr, lda,
                              vec, 1, aii, vec2, 1);
                     for (j = 0; j < i-1; j++) {
                         A[i-1][j] = vec2[j];
                     }
                 }
                 else {
                     vec = new double[i];
                     for (j = 0; j < i; j++) {
                         vec[j] = A[i-1][j];
                     }
                     ge.dscal(i, aii, vec, 1);
                     for (j = 0; j < i; j++) {
                         A[i-1][j] = vec[j];
                     }
                 }
             } // for (i = 1; i <= n; i++)
         } // else lower
         
         return;

     } // dlauu2
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DTRTRI.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dtrtri computes the inverse of a real upper or lower triangular matrix A.
     * 
       @param input char uplo
           = 'U':  A is upper triangular
           = 'L':  A is lower triangular
       @param input char diag 
           = 'N':  A is non-unit triangular
           = 'U':  A is unit triangular
       @param input int n  The order of the matrix A.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the triangular matrix A.  If uplo = 'U', the
           leading n by n upper triangular part of the array A contains
           the upper triangular matrix, and the strictly lower
           triangular part of A is not referenced.  If uplo = 'L', the
           leading n by n lower triangular part of the array A contains
           the lower triangular matrix, and the strictly upper
           triangular part of A is not referenced.  If diag = 'U', the
           diagonal elements of A are also not referenced and are
           assumed to be 1.

           On exit, the (triangular) inverse of the original matrix, in
           the same storage format.
       @param inpuut int lda  The leading dimension of the array A.  lda >= max(1,n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
           > 0: if info[0] = i, A[i-1][i-1] is exactly zero.  The triangular matrix is 
                singular and its inverse can not be computed.  
     */
      private void dtrtri(char uplo, char diag, int n, double[][] A, int lda, int info[]) {
          boolean nounit;
          boolean upper;
          int j;
          int jb;
          int nb;
          int nn;
          int i;
          double arr[][];
          int k;
          String name;
          char charOpts[] = new char[2];
          String opts;
          double arr2[][];
          
          //Test the input parameters.
      
          info[0] = 0;
          upper = ((uplo == 'U') || (uplo == 'u'));
          nounit = ((diag == 'N') || (diag == 'n'));
          if(!upper && !(uplo == 'L') || (uplo == 'l')) {
              info[0] = -1;
          }
          else if (!nounit && !((diag == 'U') || (diag == 'u'))) {
              info[0] = -2;
          }
          else if (n < 0) {
              info[0] = -3;
          }
          else if (lda < Math.max(1, n)) {
              info[0] = -5;
          }
          if (info[0] != 0) {
              MipavUtil.displayError("dtrtri had info[0] = " + info[0]);
              return;
          }
          
      
          // Quick return if possible
      
          if (n == 0) {
              return;
          }
      
          // Check for singularity if non-unit.
      
          if (nounit) {
               for (info[0] = 1; info[0] <= n; info[0]++) {
                   if (A[info[0]-1][info[0]-1] == 0.0) {
                       return;
                   }
               } // for (info[0] = 1; info[0] <= n; info[0]++)
               info[0] = 0;
          } // if (nounit)
      
          // Determine the block size for this environment.
          name = new String("DTRTRI");
          charOpts[0] = uplo;
          charOpts[1] = diag;
          opts = new String(charOpts);
          nb = ge.ilaenv( 1, name, opts, n, -1, -1, -1);
          if (nb <= 1 || nb >= n) {
      
              // Use unblocked code
       
              dtrti2(uplo, diag, n, A, lda, info);
          }
          else {
      
              // Use blocked code
      
              if (upper) {
      
                  // Compute inverse of upper triangular matrix
      
                  for (j = 1; j <= n; j += nb) {
                      jb = Math.min(nb, n-j+1);
      
                      // Compute rows 0:j-2 of current block column
                      arr = new double[j-1][jb];
                      for (i = 0; i < j-1; i++) {
                          for (k = 0; k < jb; k++) {
                              arr[i][k] = A[i][j-1+k];
                          }
                      }
                      ge.dtrmm('L', 'U', 'N', diag, j-1,
                               jb, 1.0, A, lda, arr, lda);
                      for (i = 0; i < j-1; i++) {
                          for (k = 0; k < jb; k++) {
                              A[i][j-1+k] = arr[i][k];
                          }
                      }
                      arr2 = new double[jb][jb];
                      for (i = 0; i < jb; i++) {
                          for (k = 0; k < jb; k++) {
                              arr2[i][k] = A[j-1+i][j-1+k];
                          }
                      }
                      ge.dtrsm('R', 'U', 'N', diag, j-1,
                               jb, -1.0, arr2, lda, arr, lda);
                      for (i = 0; i < j-1; i++) {
                          for (k = 0; k < jb; k++) {
                              A[i][j-1+k] = arr[i][k];
                          }
                      }
                      for (i = 0; i < jb; i++) {
                          for (k = 0; k < jb; k++) {
                              arr2[i][k] = A[j-1+i][j-1+k];
                          }
                      }
      
                      //  Compute inverse of current diagonal block
      
                      dtrti2('U', diag, jb, arr2, lda, info);
                      for (i = 0; i < jb; i++) {
                          for (k = 0; k < jb; k++) {
                              A[j-1+i][j-1+k] = arr2[i][k];
                          }
                      }
                  } // for (j = 1; j <= n; j += nb)
              } // if (upper)
              else { // lower
      
                  // Compute inverse of lower triangular matrix
      
                  nn = ((n-1) / nb )*nb + 1;
                  for (j = nn; j >= 1; j -= nb) {
                      jb = Math.min(nb, n-j+1);
                      if (j+jb <= n) {
      
                          // Compute rows j+jb-1:n-1 of current block column
                          arr = new double[n-j-jb+1][n-j-jb+1];
                          for (i = 0; i < n-j-jb+1; i++) {
                              for (k = 0; k < n-j-jb+1; k++) {
                                  arr[i][k] = A[j+jb-1+i][j+jb-1+k];
                              }
                          }
                          arr2 = new double[n-j-jb+1][jb];
                          for (i = 0; i < n-j-jb+1; i++) {
                              for (k = 0; k < jb; k++) {
                                  arr2[i][k] = A[j+jb-1+i][j-1+k];
                              }
                          }
                          ge.dtrmm('L', 'L', 'N', diag,
                                    n-j-jb+1, jb, 1.0, arr, lda,
                                    arr2, lda);
                          for (i = 0; i < n-j-jb+1; i++) {
                              for (k = 0; k < jb; k++) {
                                  A[j+jb-1+i][j-1+k] = arr2[i][k];
                              }
                          }
                          arr = new double[jb][jb];
                          for (i = 0; i < jb; i++) {
                              for (k = 0; k < jb; k++) {
                                  arr[i][k] = A[j-1+i][j-1+k];
                              }
                          }
                          ge.dtrsm('R', 'L', 'N', diag,
                                   n-j-jb+1, jb, -1.0, arr, lda,
                                   arr2, lda);
                          for (i = 0; i < n-j-jb+1; i++) {
                              for (k = 0; k < jb; k++) {
                                  A[j+jb-1+i][j-1+k] = arr2[i][k];
                              }
                          }
                      } // if (j+jb <= n)
      
                     // Compute inverse of current diagonal block
                     arr = new double[jb][jb];
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             arr[i][k] = A[j-1+i][j-1+k];
                         }
                     }
                     dtrti2('L', diag, jb, arr, lda, info);
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             A[j-1+i][j-1+k] = arr[i][k];
                         }
                     }
                  } // for (j = nn; j >= 1; j -= nb)
              } // else lower
          } // else Use blocked code

          return;

      } // dtrtri
    
    /**
     * This is a port of LAPACK version routine 3.4.2 DTRTI2.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., September 2012.
     *
     * dtrti2 computes the inverse of a triangular matrix (unblocked algorithm).
     * 
     * dtrti2 computes the inverse of a real upper or lower triangular matrix.
       @param input char uplo  Specifies whether the matrix A is upper or lower triangular.
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input char diag  Specifies whether or not the matrix A is unit triangular.
           = 'N':  Non-unit triangular
           = 'U':  Unit triangular
       @param input int n  The order of the matrix A.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the triangular matrix A.  If uplo = 'U', the
           leading n by n upper triangular part of the array A contains
           the upper triangular matrix, and the strictly lower
           triangular part of A is not referenced.  If uplo = 'L', the
           leading n by n lower triangular part of the array A contains
           the lower triangular matrix, and the strictly upper
           triangular part of A is not referenced.  If diag = 'U', the
           diagonal elements of A are also not referenced and are
           assumed to be 1.

           On exit, the (triangular) inverse of the original matrix, in
           the same storage format.
       @param inpuut int lda  The leading dimension of the array A.  lda >= max(1,n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -k, the k-th argument had an illegal value  
     */
      private void dtrti2(char uplo, char diag, int n, double[][] A, int lda, int info[]) {
          boolean nounit;
          boolean upper;
          int j;
          double ajj;
          double vec[];
          int i;
          double arr[][];
          int k;
          
          //Test the input parameters.
      
          info[0] = 0;
          upper = ((uplo == 'U') || (uplo == 'u'));
          nounit = ((diag == 'N') || (diag == 'n'));
          if(!upper && !(uplo == 'L') || (uplo == 'l')) {
              info[0] = -1;
          }
          else if (!nounit && !((diag == 'U') || (diag == 'u'))) {
              info[0] = -2;
          }
          else if (n < 0) {
              info[0] = -3;
          }
          else if (lda < Math.max(1, n)) {
              info[0] = -5;
          }
          if (info[0] != 0) {
              MipavUtil.displayError("dtrti2 had info[0] = " + info[0]);
              return;
          }
      
          if (upper) {
      
              // Compute inverse of upper triangular matrix.
      
              for (j = 1; j <= n; j++) {
                  if (nounit) {
                     A[j-1][j-1] = 1.0/ A[j-1][j-1];
                     ajj = -A[j-1][j-1];
                  }
                  else {
                     ajj = -1.0;
                  }
      
                  // Compute elements 0:j-2 of (j-1)-th column.
                  vec = new double[j-1];
                  for (i = 0; i < j-1; i++) {
                      vec[i] = A[i][j-1];
                  }
                  ge.dtrmv('U', 'N', diag, j-1, A, lda, vec, 1);
                  ge.dscal(j-1, ajj, vec, 1 );
                  for (i = 0; i < j-1; i++) {
                      A[i][j-1] = vec[i];
                  }
              } // for (j = 1; j <= n; j++)
          } // if (upper)
          else { // lower
      
              // Compute inverse of lower triangular matrix.
 
              for (j = n; j >= 1; j--) {
                  if (nounit) {
                      A[j-1][j-1] = 1.0/ A[j-1][j-1];
                      ajj = -A[j-1][j-1];
                   }
                   else {
                      ajj = -1.0;
                   }
                   if (j < n) {
      
                       // Compute elements j+1:n of j-th column.
                       arr = new double[n-j][n-j];
                       for (i = 0; i < n-j; i++) {
                           for (k = 0; k < n-j; k++) {
                               arr[i][k] = A[j+i][j+k];
                           }
                       }
                       vec = new double[n-j];
                       for (i = 0; i < n-j; i++) {
                           vec[i] = A[j+i][j-1];
                       }
                       ge.dtrmv('L', 'N', diag, n-j, arr, lda, vec, 1);
                       ge.dscal(n-j, ajj, vec, 1);
                       for (i = 0; i < n-j; i++) {
                           A[j+i][j-1] = vec[i];
                       }
                   } // if (j < n)
              } // for (j = n; j >= 1; j--)
          } // else lower
      
          return;

      } // dtrti2
    
}