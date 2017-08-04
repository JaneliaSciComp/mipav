package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class ComplexLinearEquations implements java.io.Serializable {
	GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    private ViewUserInterface UI = ViewUserInterface.getReference();
    
    private int iparms[];

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    /**
     * Creates a new ComplexLinearEquations object.
     */
    public ComplexLinearEquations() {}
    
    /*
     * This is a port of a portion of LAPACK routine ZGETRS.f version 3.7.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., December, 2016
     * 
     * zgetrs solves a system of linear equations
          A * X = B, A**T * X = B, or A**H * X = B
       with a general N-by-N matrix A using the LU factorization computed
       by zgetrf.
       
       @param input char trans
           Specifies the form of the system of equations:
           = 'N':  A * X = B  (No transpose)
           = 'T':  A**T* X = B  (Transpose)
           = 'C':  A**H* X = B  (Conjugate transpose)
       @param input int n
           The order of the matrix A.  n >= 0.
       @param input int nrhs
           The number of right hand sides, i.e., the number of columns
           of the matrix B.  nrhs >= 0.
       @param input double[][][2] A complex of dimension (lda, n)
           The factors L and U from the factorization A = P*L*U
           as computed by zgetrf.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param input int[] ipiv of dimension (n)
           The pivot indices from zgetrf; for 1<=i<=n, row i of the
           matrix was interchanged with row ipiv[i].
       @param (input/output) double[][][2] complex B of dimension (ldb,nrhs)
           On entry, the right hand side matrix B.
           On exit, the solution matrix X.
       @param input int ldb
           The leading dimension of the array B.  ldb >= max(1,n).
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
     */
    public void zgetrs(char trans, int n, int nrhs, double[][][] A, int lda, int[] ipiv,
                        double[][][] B, int ldb, int[] info) {
        boolean notran;
        double alpha[] = new double[2];
        alpha[0] = 1.0;
        alpha[1] = 0.0;
        
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
            MipavUtil.displayError("zgetrs had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (n == 0 || nrhs == 0) {
            return;
        }
    
        if (notran) {
    
            // Solve A * X = B.
    
            // Apply row interchanges to the right hand sides.
    
            zlaswp(nrhs, B, ldb, 1, n, ipiv, 1);
    
            // Solve L*X = B, overwriting B with X.
    
            ztrsm('L', 'L', 'N', 'U', n, nrhs,
                     alpha, A, lda, B, ldb);
    
            // Solve U*X = B, overwriting B with X.
    
            ztrsm('L', 'U', 'N', 'N', n,
                     nrhs, alpha, A, lda, B, ldb);
        } // if (notran)
        else {
    
            // Solve A**T * X = B oe A**H * X = B.
    
            // Solve U**T *X = B or U**H *X = B, overwriting B with X.
    
            ztrsm('L', 'U', 'T', 'N', n, nrhs,
                     alpha, A, lda, B, ldb);
    
            // Solve L**T *X = B or L**H *X = B, overwriting B with X.
    
            ztrsm('L', 'L', 'T', 'U', n, nrhs, alpha,
                     A, lda, B, ldb);
    
            // Apply row interchanges to the solution vectors.
    
            zlaswp(nrhs, B, ldb, 1, n, ipiv, -1);
        } // else
    
        return;

    } // zgetrs
    
    /*
     * This is a port of a portion of LAPACK routine ZGETRF.f version 3.7.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., December, 2016
     * 
     * zgetrf computes an LU factorization of a general m-by-n matrix A
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
       @param (input/output) complex double[][][] A of dimension (lda, n)
           On entry, the m by n matrix to be factored.
           On exit, the factors L and U form the factorization
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
    public void zgetrf(int m, int n, double[][][] A, int lda, int[] ipiv, int[] info) {
        int i;
        int iinfo[] = new int[1];
        int j;
        int p;
        int jb;
        int nb;
        String name;
        String opts;
        double arr[][][];
        int ivec[];
        int k;
        double arr2[][][];
        double arr3[][][];
        double alpha[] = new double[2];
        double beta[] = new double[2];
        
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
            MipavUtil.displayError("zgetrf had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (m == 0 || n == 0) {
            return;
        }
    
        // Determine the block size for this environment.
    
        name = new String("ZGETRF");
        opts = new String(" ");
        nb = ge.ilaenv( 1, name, opts, m, n, -1, -1);
        if (nb <= 1 || nb >= Math.min(m, n)) {
    
            // Use unblocked code.
    
            zgetrf2(m, n, A, lda, ipiv, info);
        } // if (nb <= 1 || nb >= Math.min(m, n))
        else {
    
            // Use blocked code.
    
            for (j = 1; j <= Math.min(m, n); j += nb) {
                jb = Math.min(Math.min(m, n)-j+1, nb);
    
                // Factor diagonal and subdiagonal blocks and test for exact
                // singularity.
    
                arr = new double[m-j+1][jb][2];
                for (i = 0; i < m-j+1; i++) {
                    for (k = 0; k < jb; k++) {
                    	for (p = 0; p < 2; p++) {
                            arr[i][k][p] = A[j-1+i][j-1+k][p];
                    	}
                    }
                }
                ivec = new int[Math.min(m-j+1, jb)];
                zgetrf2(m-j+1, jb, arr, lda, ivec, iinfo);
                for (i = 0; i < m-j+1; i++) {
                    for (k = 0; k < jb; k++) {
                    	for (p = 0; p < 2; p++) {
                            A[j-1+i][j-1+k][p] = arr[i][k][p];
                    	}
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
    
                zlaswp(j-1, A, lda, j, j+jb-1, ipiv, 1);
    
                if (j+jb <= n) {
    
                    // Apply interchanges to columns j+jb:n.
    
                    arr = new double[A.length][n-j-jb+1][2];
                    for (i = 0; i < A.length; i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                        	for (p = 0; p < 2; p++) {
                                arr[i][k][p] = A[i][j+jb-1+k][p];
                        	}
                        }
                    }
                    zlaswp(n-j-jb+1, arr, lda, j, j+jb-1, ipiv, 1);
                    for (i = 0; i < A.length; i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                        	for (p = 0; p < 2; p++) {
                                A[i][j+jb-1+k][p] = arr[i][k][p];
                        	}
                        }
                    }
    
                    // Compute block row of U.
                    arr = new double[jb][jb][2];
                    for (i = 0; i < jb; i++) {
                        for (k = 0; k < jb; k++) {
                        	for (p = 0; p < 2; p++) {
                                arr[i][k][p] = A[j-1+i][j-1+k][p];
                        	}
                        }
                    }
                    arr2 = new double[Math.max(1,jb)][n-j-jb+1][2];
                    for (i = 0; i < Math.max(1, jb); i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                        	for (p = 0; p < 2; p++) {
                                arr2[i][k][p] = A[j-1+i][j+jb-1+k][p];
                        	}
                        }
                    }
                    alpha[0] = 1.0;
                    alpha[1] = 0.0;
                    ztrsm('L', 'L', 'N', 'U', jb,
                             n-j-jb+1, alpha, arr, lda, arr2,
                             lda);
                   for (i = 0; i < Math.max(1, jb); i++) {
                       for (k = 0; k < n-j-jb+1; k++) {
                    	   for (p = 0; p < 2; p++) {
                               A[j-1+i][j+jb-1+k][p] = arr2[i][k][p];
                    	   }
                       }
                   }
                   if (j+jb <= m) {
    
                       // Update trailing submatrix.
                       arr = new double[Math.max(1, m-j-jb+1)][jb][2];
                       for (i = 0; i < Math.max(1,m-j-jb+1); i++) {
                           for (k = 0; k < jb; k++) {
                        	   for (p = 0; p < 2; p++) {
                                   arr[i][k][p] = A[j+jb-1+i][j-1+k][p];
                        	   }
                           }
                       }
                       arr3 = new double[Math.max(1,m-j-jb+1)][n-j-jb+1][2];
                       for (i = 0; i < Math.max(1, m-j-jb+1); i++) {
                           for (k = 0; k < n-j-jb+1; k++) {
                        	   for (p = 0; p < 2; p++) {
                                   arr3[i][k][p] = A[j+jb-1+i][j+jb-1+k][p];
                        	   }
                           }
                       }
                       alpha[0] = -1.0;
                       alpha[1] = 0.0;
                       beta[0] = 1.0;
                       beta[1] = 0.0;
                       zgemm('N', 'N', m-j-jb+1, n-j-jb+1, jb, alpha, arr, lda,
                                arr2, lda, beta, arr3, lda);
                       for (i = 0; i < Math.max(1, m-j-jb+1); i++) {
                          for (k = 0; k < n-j-jb+1; k++) {
                        	  for (p = 0; p < 2; p++) {
                                  A[j+jb-1+i][j+jb-1+k][p] = arr3[i][k][p];
                        	  }
                          }
                      }
                   } // if (j+jb <= m)
                } // if (j+jb <= n)
            } // for (j = 1; j <= Math.min(m, n); j += nb)
        } // else use blocked code
        return;

    } // zgetrf
    
    /*
     * This is a port of a portion of LAPACK routine ZGETRF2.f version 3.7.0.
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., June, 2016.
     * 
     * zgetrf2 computes the LU factorization of a general m-by-n matrix using partial pivoting with row
     * interchanges.

       The factorization has the form
          A = P * L * U
       where P is a permutation matrix, L is lower triangular with unit
       diagonal elements (lower trapezoidal if m > n), and U is upper
       triangular (upper trapezoidal if m < n).

     * This is the recursive version of the algorithm. It divides
     * the matrix into four submatrices:
     *
     *        [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
     *    A = [ -----|----- ]  with n1 = min(m,n)/2
     *        [  A21 | A22  ]       n2 = n-n1
     *
     *                                       [ A11 ]
     * The subroutine calls itself to factor [ --- ],
     *                                       [ A12 ]
     *                 [ A12 ]
     * do the swaps on [ --- ], solve A12, update A22,
     *                 [ A22 ]
     *
     * then calls itself to factor A22 and do the swaps on A21.


       @param input int m
           The number of rows of the matrix A.  m >= 0.
       @param input int n
           The number of columns of the matrix A.  n >= 0.
       @param (input/output) double[][][2] complex A of dimension (lda, n)
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
    private void zgetrf2(int m, int n, double[][][] A, int lda, int[] ipiv, int[] info) {
        double sfmin;
        int i;
        int j;
        int jp;
        int k;
        int p;
        double cr[] = new double[1];
        double ci[] = new double[1];
        double scale[] = new double[2];
        int n1;
        int n2;
        int iinfo[] = new int[1];
        double A2[][][];
        double alpha[] = new double[2];
        double beta[] = new double[2];
        double A3[][][];
        double A4[][][];
        int ipiv2[];
        double temp;
        
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
            MipavUtil.displayError("zgetrf2 had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (m == 0 || n == 0) {
            return;
        }
        
        if (m == 1) {
        	// Use unblocked code for one row case
        	// Just need to handel ipiv and info
        	ipiv[0] = 0;
        	if ((A[0][0][0] == 0.0) && (A[0][0][1] == 0.0)) {
        		info[0] = 1;
        	}
        } // if (m = 1)
        else if (n == 1) {
        	// Use unblocked code for one column case
        	
        	// Compute machine safe minimum
        	sfmin = ge.dlamch('S');
        	
        	// Find pivot and test for singularity
        	i = -1;
        	double maxval = -Double.MAX_VALUE;
        	for (j = 0; j < m; j++) {
                double value = Math.abs(A[j][0][0]) + Math.abs(A[j][0][1]);
                if (value > maxval) {
                	maxval = value;
                	i = j;
                }
        	} // for (j = 0; j < m; j++)
        	ipiv[0] = i;
        	if ((A[i][0][0] != 0.0) || (A[i][0][1] != 0.0)) {
        	    // Apply the interchange
        		if (i != 0) {
        		    for (p = 0; p < 2; p++)	{
        		    	temp = A[0][0][p];
        		    	A[0][0][p] = A[i][0][p];
        		    	A[i][0][p] = temp;
        		    }
        		} // if (i != 0)
        		
        		// Compute elements 1:M-1 of the column
        		if (zabs(A[0][0][0], A[0][0][1]) >= sfmin) {
        			zdiv(1.0, 0.0, A[0][0][0], A[0][0][1], cr, ci);
        			scale[0] = cr[0];
        			scale[1] = ci[0];
        			for (i = 0; i < m-1; i++) {
        				zmlt(scale[0], scale[1], A[1+i][0][0], A[1+i][0][1], cr, ci);
        				A[1+i][0][0] = cr[0];
        				A[1+i][0][1] = ci[0];
        			}
        		} // if (zabs(A[0][0][0], A[0][0][1]) >= sfmin)
        		else {
        			for (i = 0; i < m-1; i++) {
        				zdiv(A[1+i][0][0], A[1+i][0][1], A[0][0][0], A[0][0][1], cr, ci);
        				A[1+i][0][0] = cr[0];
        				A[1+i][0][1] = ci[0];
        			}
        		}
        	} // if ((A[i][0][0] != 0.0) || (A[i][0][1] != 0.0))
        	else {
        		info[0] = 1;
        	}
        } // else if (n == 1)
        else {
        	// Use recursive code
        	n1 = Math.min(m, n)/2;
        	n2 = n - n1;
        	
        	
        	//               [ A11 ]
        	//        Factor [ --- ]
        	//               [ A21 ]
        	
        	zgetrf2(m, n1, A, lda, ipiv, iinfo);
        	
        	if ((info[0] == 0) && (iinfo[0] > 0)) {
        		info[0] = iinfo[0];
        	}
        	
        	
        	//                              [ A12 ]
        	//        Apply interchanges to [ --- ]
        	//                              [ A22 ]
        	A2 = new double[lda][n2][2];
        	for (i = 0; i < lda; i++) {
        		for (j = 0; j < n2; j++) {
        			for (p = 0; p < 2; p++) {
        				A2[i][j][p] = A[i][n1+j][p];
        			}
        		}
        	} // for (i = 0; i < lda; i++)
        	zlaswp(n2, A2, lda, 1, n1, ipiv, 1);
        	for (i = 0; i < lda; i++) {
        		for (j = 0; j < n2; j++) {
        			for (p = 0; p < 2; p++) {
        				A[i][n1+j][p] = A2[i][j][p];
        			}
        		}
        	} // for (i = 0; i < lda; i++)
        	
        	// Solve A12
        	alpha[0] = 1.0;
        	alpha[1] = 0.0;
        	ztrsm('L', 'L', 'N', 'U', n1, n2, alpha, A, lda, A2, lda);
        	for (i = 0; i < lda; i++) {
        		for (j = 0; j < n2; j++) {
        			for (p = 0; p < 2; p++) {
        				A[i][n1+j][p] = A2[i][j][p];
        			}
        		}
        	} // for (i = 0; i < lda; i++)
        	A3 = new double[m-n1][n1][2];
        	for (i = 0; i < m-n1; i++) {
        	    for (j = 0; j < n1; j++) {
        	    	for (p = 0; p < 2; p++) {
        	    	    A3[i][j][p] = A[i+n1][j][p];	
        	    	}
        	    }
        	}
        	A4 = new double[m-n1][n2][2];
        	for (i = 0; i < m-n1; i++) {
        		for (j = 0; j < n2; j++) {
        			for (p = 0; p < 2; p++) {
        				A4[i][j][p] = A[i+n1][j+n1][p];
        			}
        		}
        	}
        	alpha[0] = -1.0;
        	alpha[1] = 0.0;
        	beta[0] = 1.0;
        	beta[1] = 0.0;
        	// Update A22
        	zgemm('N', 'N', m-n1, n2, n1, alpha, A3, m-n1, A2, lda, beta, A4, m-n1);
        	
        	// Factor A22
        	ipiv2 = new int[Math.min(m-n1, n2)];
        	zgetrf2(m-n1, n2, A4, m-n1, ipiv2, iinfo);
        	for (i = 0; i < m-n1; i++) {
        		for (j = 0; j < n2; j++) {
        			for (p = 0; p < 2; p++) {
        				A[i+n1][j+n1][p] = A4[i][j][p];
        			}
        		}
        	}
        	for (i = 0; i < Math.min(m-n1, n2); i++) {
        		ipiv[i+n1] = ipiv2[i];
        	}
        	
        	// Adjust info and the pivot indices
        	if ((info[0] == 0) && (iinfo[0] > 0)) {
        		info[0] = iinfo[0] + n1;
        	}
        	for ( i = n1; i < Math.min(m,n); i++) {
        		ipiv[i] = ipiv[i] + n1;
        	}
        	
        	// Apply interchanges to A21
        	zlaswp(n1, A, lda, n1+1, Math.min(m, n), ipiv, 1);
        } // else
    
        
        return;

    } // zgetrf2
    
    /*
     * This is a port of a portion of LAPACK auxiliary routine zLASWP.f version 3.7.1
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., June 2017
     * Modified by
       R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA

     * 
     * zlaswp performs a series of row interchanges on a general rectangular matrix.
     * 
     * dlaswp performs a series of row interchanges on the matrix A.
       One row interchange is initiated for each of rows K1 through K2 of A.

       @param input int n
           The number of columns of the matrix A.
       @param (input/output) double[][][2] complex A of dimension (lda, n)
           On entry, the matrix of column dimension N to which the row
           interchanges will be applied.
           On exit, the permuted matrix.
       @param input int lda
           The leading dimension of the array A.
       @param input int k1
           The first element of ipiv for which a row interchange will
           be done.
       @param input int k2
           (k2-k1+1) is the number of elements of ipiv for 
           which row interchange will be done.
       @param input int[] ipiv of dimension (k1 + (k2-k1) * abs(incx))
           The vector of pivot indices.  Only the elements in positions
           k1 through k1 +(k2-k1)*abs(incx) of ipiv are accessed.
           ipiv[k1 + (k-k1)*abs(incx)] = L implies rows k and L are to be interchanged
       @param input int incx
           The increment between successive values of IPIV.  If IPIV
           is negative, the pivots are applied in reverse order.
     */
    private void zlaswp(int n, double[][][] A, int lda, int k1, int k2, int[] ipiv, int incx) {
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
        double temp[] = new double[2];
        int p = 2;
        
        // Interchange row i with row ipiv[k1 + (i-k1)*abs(incx)] for each of rows k1 through k2.
        
        if (incx > 0) {
           ix0 = k1;
           i1 = k1;
           i2 = k2;
           inc = 1;
        }
        else if (incx < 0) {
           ix0 = k1 + (k1-k2)*incx;
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
                            	for (p = 0; p < 2; p++) {
	                                temp[p] = A[i-1][k-1][p];
	                                A[i-1][k-1][p] = A[ip-1][k-1][p];
	                                A[ip-1][k-1][p] = temp[p];
                            	}
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
                            	for (p = 0; p < 2; p++) {
	                                temp[p] = A[i-1][k-1][p];
	                                A[i-1][k-1][p] = A[ip-1][k-1][p];
	                                A[ip-1][k-1][p] = temp[p];
                            	}
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
                        	for (p = 0; p < 2; p++) {
	                            temp[p] = A[i-1][k-1][p];
	                            A[i-1][k-1][p] = A[ip-1][k-1][p];
	                            A[ip-1][k-1][p] = temp[p];
                        	}
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
                        	for (p = 0; p < 2; p++) {
	                            temp[p] = A[i-1][k-1][p];
	                            A[i-1][k-1][p] = A[ip-1][k-1][p];
	                            A[ip-1][k-1][p] = temp[p];
                        	}
                        } // for (k = n32; k <= n; k++)
                    } // if (ip != i)
                    ix = ix + incx;    
                } // for (i = i1; i >= i2; i--)
            } // else inc == -1
        } // if (n32 != n)
     
        return;

    } // zlaswp
    
    /**
     * This is a port of the 2/8/89 Blas routine ZTRSM Original code written by: Jack Dongarra, Argonne National
     * Laboratory Iain Duff, AERE Harwell. Jeremy Du Croz, Numerical Algorithms Group Ltd. Sven Hammarling, Numerical
     * Algorithms Group Ltd. ztrsm solves one of the matrix equations op(A)*X = alpha*B or X*op(A) = alpha*B, where
     * alpha is a scalar, X and B are m by n matrices, A is a unit, or non-unit, upper or lower triangular matrix and
     * op(A) is one of op(A) = A or op(A) = A**T or op(A) = A**H. The matrix X is overwritten on B.
     * 
     * @param side input char On entry, side specifies whether op(A) appears on the left or right or X as follows: = 'L'
     *            or 'l' op(A)*X = alpha*B = 'R' or 'r' X*op(A) = alpha*B
     * @param uplo input char On entry, uplo specifies whether the matrix A is an upper or lower triangular matrix as
     *            follows: = 'U' or 'u' A is an upper triangular matrix = 'L' or 'l' A is a lower triangular matrix
     * @param transa input char On entry, transa specifies the form of op(A) to be used in the matrix multiplication as
     *            follows: = 'N' or 'n' op(A) = A, 'T' or 't' op(A) = A**T, 'C' or 'c' op(A) = A**H
     * @param diag input char On entry, diag specifies whether or not A is unit triangular as follows: = 'U' or 'u' A is
     *            assumed to be unit triangular. = 'N' or 'n' A is not assumed to be unit triangular.
     * @param m input int On entry, m specifies the number of rows of B. m must be at least zero.
     * @param n input int On entry, n specifies the number of columns of B. n must be at least zero.
     * @param alpha input double[2] Specified complex scalar. When alpha is zero then A is not referenced and B need not be set
     *            before entry.
     * @param A input double[][][2] complex of dimension lda by k, where k is m when side = 'L' or 'l' and is n when side = 'R' or
     *            'r'. Before entry with uplo = 'U' or 'u', the leading k by k upper triangular part of the array A must
     *            contain the upper triangular matrix and the strictly lower part of A is not referenced. Before entry
     *            with uplo = 'L' or 'l', the leading k by k lower triangular part of the array A must contain the lower
     *            triangular matrix and the strictly upper triangular part of A is not referenced. Note that when diag =
     *            'U' or 'u', the diagonal elements of A are not referenced either, but are assumed to be unity.
     * @param lda input int On entry, lda specifies the first dimension of A as declared in the calling (sub) program.
     *            When side = 'L' or 'l', then lda must be at least max(1,m). When side = 'R' or 'r', then lda must be
     *            at least max(1,n).
     * @param B input/output double[][][2] complex of dimension ldb by n. Before entry, the leading m by n part of the array B must
     *            contain the right-hand side matrix B, and on exit is overwritten by the solution matrix X.
     * @param ldb input int On entry, ldb specifies the first dimension of B as declared in the calling (sub) program.
     *            ldb must be at least max(1,m).
     */
    public void ztrsm(final char side, final char uplo, final char transa, final char diag, final int m, final int n,
            final double alpha[], final double[][][] A, final int lda, final double[][][] B, final int ldb) {
        boolean lside;
        boolean noconj;
        boolean nounit;
        boolean upper;
        int i;
        int info;
        int j;
        int k;
        int nrowa;
        double temp[] = new double[2];
        int p;
        double cr[] = new double[1];
        double ci[] = new double[1];

        // Test the input parameters
        if ( (side == 'L') || (side == 'l')) {
            lside = true;
        } else {
            lside = false;
        }

        if (lside) {
            nrowa = m;
        } else {
            nrowa = n;
        }
        
        if ((transa == 'T') ||(transa == 't')) {
        	noconj = true;
        }
        else {
        	noconj = false;
        }

        if ( (diag == 'N') || (diag == 'n')) {
            nounit = true;
        } else {
            nounit = false;
        }

        if ( (uplo == 'U') || (uplo == 'u')) {
            upper = true;
        } else {
            upper = false;
        }

        info = 0;

        if ( ( !lside) && (side != 'R') && (side != 'r')) {
            info = 1;
        } else if ( ( !upper) && (uplo != 'L') && (uplo != 'l')) {
            info = 2;
        } else if ( (transa != 'N') && (transa != 'n') && (transa != 'T') && (transa != 't') && (transa != 'C')
                && (transa != 'c')) {
            info = 3;
        } else if ( (diag != 'U') && (diag != 'u') && (diag != 'N') && (diag != 'n')) {
            info = 4;
        } else if (m < 0) {
            info = 5;
        } else if (n < 0) {
            info = 6;
        } else if (lda < Math.max(1, nrowa)) {
            info = 9;
        } else if (ldb < Math.max(1, m)) {
            info = 11;
        }

        if (info != 0) {
            MipavUtil.displayError("Error ztrsm had info = " + info);

            return;
        }

        // Quick return if possible
        if ((m == 0) || (n == 0)) {
            return;
        }
        
        // And when alpha equals zero

        if ((alpha[0] == 0.0) && (alpha[1] == 0.0)) {

            for (j = 0; j < n; j++) {

                for (i = 0; i < m; i++) {
                	for (p = 0; p < 2; p++) {
                        B[i][j][p] = 0.0;
                	}
                }
            }

            return;
        } // if ((alpha[0] == 0.0) && (alpha[1] == 0.0))

        if (lside) {

            if ( (transa == 'N') || (transa == 'n')) {

                // Form B = alpha*inv(A)*B
                if (upper) {

                    for (j = 0; j < n; j++) {

                        if ((alpha[0] != 1.0) || (alpha[1] != 0.0)) {

                            for (i = 0; i < m; i++) {
                            	zmlt(alpha[0], alpha[1], B[i][j][0], B[i][j][1], cr, ci);
                            	B[i][j][0] = cr[0];
                            	B[i][j][1] = ci[0];
                            }
                        } // if ((alpha[0] != 1.0) || (alpha[1] != 0.0))

                        for (k = m - 1; k >= 0; k--) {

                            if ((B[k][j][0] != 0.0)|| (B[k][j][1] != 0.0)) {

                                if (nounit) {
                                	zdiv(B[k][j][0], B[k][j][1], A[k][k][0], A[k][k][1], cr, ci);
                                	B[k][j][0] = cr[0];
                                	B[k][j][1] = ci[0];
                                }

                                for (i = 0; i <= (k - 1); i++) {
                                	zmlt(B[k][j][0], B[k][j][1], A[i][k][0], A[i][k][1], cr, ci);
                                	B[i][j][0] = B[i][j][0] - cr[0];
                                	B[i][j][1] = B[i][j][1] - ci[0];
                                }
                            } //  if ((B[k][j][0] != 0.0)|| (B[k][j][1] != 0.0))
                        } // for (k = m-1; k >= 0; k--)
                    } // for (j = 0; j < n; j++)
                } // if (upper)
                else { // lower

                    for (j = 0; j < n; j++) {

                    	if ((alpha[0] != 1.0) || (alpha[1] != 0.0)) {

                            for (i = 0; i < m; i++) {
                            	zmlt(alpha[0], alpha[1], B[i][j][0], B[i][j][1], cr, ci);
                            	B[i][j][0] = cr[0];
                            	B[i][j][1] = ci[0];
                            }
                        } // if ((alpha[0] != 1.0) || (alpha[1] != 0.0))

                        for (k = 0; k < m; k++) {

                        	 if ((B[k][j][0] != 0.0)|| (B[k][j][1] != 0.0)) {

                                if (nounit) {
                                	zdiv(B[k][j][0], B[k][j][1], A[k][k][0], A[k][k][1], cr, ci);
                                	B[k][j][0] = cr[0];
                                	B[k][j][1] = ci[0];
                                }

                                for (i = k + 1; i < m; i++) {
                                	zmlt(B[k][j][0], B[k][j][1], A[i][k][0], A[i][k][1], cr, ci);
                                	B[i][j][0] = B[i][j][0] - cr[0];
                                	B[i][j][1] = B[i][j][1] - ci[0];
                                }
                            } //  if ((B[k][j][0] != 0.0)|| (B[k][j][1] != 0.0))
                        } // for (k = 0; k < m; k++)
                    } // for (j = 0; j < n; j++)
                } // else lower
            } // if ((transa == 'N') || (transa == 'n')
            else { // ((transa != 'N') && (transa != 'n'))

                // Form B = alpha*inv(A**T)*B
            	// or   B = alpha*inv(A**H)*B
                if (upper) {

                    for (j = 0; j < n; j++) {

                        for (i = 0; i < m; i++) {
                        	zmlt(alpha[0], alpha[1], B[i][j][0], B[i][j][1], cr, ci);
                        	temp[0] = cr[0];
                        	temp[1] = ci[0];
                            if (noconj) {
	                            for (k = 0; k <= (i - 1); k++) {
	                            	zmlt(A[k][i][0], A[k][i][1], B[k][j][0], B[k][j][1], cr, ci);
	                            	temp[0] = temp[0] - cr[0];
	                            	temp[1] = temp[1] - ci[0];
	                            }
	
	                            if (nounit) {
	                            	zdiv(temp[0], temp[1], A[i][i][0], A[i][i][1], cr, ci);
	                            	temp[0] = cr[0];
	                            	temp[1] = ci[0];
	                            }
                            } // if (noconj)
                            else {
                            	for (k = 0; k <= (i - 1); k++) {
	                            	zmlt(A[k][i][0], -A[k][i][1], B[k][j][0], B[k][j][1], cr, ci);
	                            	temp[0] = temp[0] - cr[0];
	                            	temp[1] = temp[1] - ci[0];
	                            }
	
	                            if (nounit) {
	                            	zdiv(temp[0], temp[1], A[i][i][0], -A[i][i][1], cr, ci);
	                            	temp[0] = cr[0];
	                            	temp[1] = ci[0];
	                            }	
                            } // else

                            B[i][j][0] = temp[0];
                            B[i][j][1] = temp[1];
                        } // for (i = 0; i < m; i++)
                    } // for (j = 0; j < n; j++)
                } // if (upper)
                else { // lower

                    for (j = 0; j < n; j++) {

                        for (i = m - 1; i >= 0; i--) {
                        	zmlt(alpha[0], alpha[1], B[i][j][0], B[i][j][1], cr, ci);
                        	temp[0] = cr[0];
                        	temp[1] = ci[0];
                            if (noconj) {
	                            for (k = i + 1; k < m; k++) {
	                            	zmlt(A[k][i][0], A[k][i][1], B[k][j][0], B[k][j][1], cr, ci);
	                            	temp[0] = temp[0] - cr[0];
	                            	temp[1] = temp[1] - ci[0];
	                            }
	
	                            if (nounit) {
	                            	zdiv(temp[0], temp[1], A[i][i][0], A[i][i][1], cr, ci);
	                            	temp[0] = cr[0];
	                            	temp[1] = ci[0];
	                            }
                            } // if (noconj)
                            else {
                            	for (k = i + 1; k < m; k++) {
	                            	zmlt(A[k][i][0], -A[k][i][1], B[k][j][0], B[k][j][1], cr, ci);
	                            	temp[0] = temp[0] - cr[0];
	                            	temp[1] = temp[1] - ci[0];
	                            }
	
	                            if (nounit) {
	                            	zdiv(temp[0], temp[1], A[i][i][0], -A[i][i][1], cr, ci);
	                            	temp[0] = cr[0];
	                            	temp[1] = ci[0];
	                            }	
                            }

                            B[i][j][0] = temp[0];
                            B[i][j][1] = temp[1];
                        } // for (i = m-1; i >= 0; i--)
                    } // for (j = 0; j < n; j++)
                } // else lower
            } // else ((transa != 'N') && (transa != 'n'))
        } // if (lside)
        else { // !lside

            if ( (transa == 'N') || (transa == 'n')) {

                // Form B = alpha*B*inv(A)
                if (upper) {

                    for (j = 0; j < n; j++) {

                    	if ((alpha[0] != 1.0) || (alpha[1] != 0.0)) {

                            for (i = 0; i < m; i++) {
                            	zmlt(alpha[0], alpha[1], B[i][j][0], B[i][j][1], cr, ci);
                            	B[i][j][0] = cr[0];
                            	B[i][j][1] = ci[0];
                            }
                        } // if ((alpha[0] != 1.0) || (alpha[1] != 0.0))

                        for (k = 0; k <= (j - 1); k++) {

                            if ((A[k][j][0] != 0.0) || (A[k][j][1] != 0.0)) {

                                for (i = 0; i < m; i++) {
                                	zmlt(A[k][j][0], A[k][j][1], B[i][k][0], B[i][k][1], cr, ci);
                                	B[i][j][0] = B[i][j][0] - cr[0];
                                	B[i][j][1] = B[i][j][1] - ci[0];
                                }
                            } // if ((A[k][j][0] != 0.0) || (A[k][j][1] != 0.0))
                        } // for (k = 0; k <= j-1; k++)

                        if (nounit) {
                        	zdiv(1.0, 0.0, A[j][j][0], A[j][j][1], cr, ci);
                        	temp[0] = cr[0];
                        	temp[1] = ci[0];

                            for (i = 0; i < m; i++) {
                            	zmlt(temp[0], temp[1], B[i][j][0], B[i][j][1], cr, ci);
                            	B[i][j][0] = cr[0];
                            	B[i][j][1] = ci[0];
                            }
                        } // if (nounit)
                    } // for (j = 0; j < n; j++)
                } // if (upper)
                else { // lower

                    for (j = n - 1; j >= 0; j--) {

                    	if ((alpha[0] != 1.0) || (alpha[1] != 0.0)) {

                            for (i = 0; i < m; i++) {
                            	zmlt(alpha[0], alpha[1], B[i][j][0], B[i][j][1], cr, ci);
                            	B[i][j][0] = cr[0];
                            	B[i][j][1] = ci[0];
                            }
                        } // if ((alpha[0] != 1.0) || (alpha[1] != 0.0))

                        for (k = j + 1; k < n; k++) {

                        	if ((A[k][j][0] != 0.0) || (A[k][j][1] != 0.0)) {

                                for (i = 0; i < m; i++) {
                                	zmlt(A[k][j][0], A[k][j][1], B[i][k][0], B[i][k][1], cr, ci);
                                	B[i][j][0] = B[i][j][0] - cr[0];
                                	B[i][j][1] = B[i][j][1] - ci[0];
                                }
                            } // if ((A[k][j][0] != 0.0) || (A[k][j][1] != 0.0))
                        } // for (k = j+1; k < n; k++)

                        if (nounit) {
                        	zdiv(1.0, 0.0, A[j][j][0], A[j][j][1], cr, ci);
                        	temp[0] = cr[0];
                        	temp[1] = ci[0];

                            for (i = 0; i < m; i++) {
                            	zmlt(temp[0], temp[1], B[i][j][0], B[i][j][1], cr, ci);
                            	B[i][j][0] = cr[0];
                            	B[i][j][1] = ci[0];
                            }
                        } // if (nounit)
                    } // for (j = n-1; j >= 0; j--)
                } // else lower
            } // if ((transa == 'N') || (transa == 'n')
            else { // ((transa != 'N') && (transa != 'n'))

                // Form B = alpha*B*inv(A**T)
            	// or   B = alpha*B*inv(A**H).
                if (upper) {

                    for (k = n - 1; k >= 0; k--) {

                        if (nounit) {
                        	if (noconj) {
                        		zdiv(1.0, 0.0, A[k][k][0], A[k][k][1], cr, ci);
                            	temp[0] = cr[0];
                            	temp[1] = ci[0];
                        	}
                        	else {
                        		zdiv(1.0, 0.0, A[k][k][0], -A[k][k][1], cr, ci);
                            	temp[0] = cr[0];
                            	temp[1] = ci[0];	
                        	}

                            for (i = 0; i < m; i++) {
                            	zmlt(temp[0], temp[1], B[i][k][0], B[i][k][1], cr, ci);
                            	B[i][k][0] = cr[0];
                            	B[i][k][1] = ci[0];
                            }
                        } // if (nounit)

                        for (j = 0; j <= (k - 1); j++) {

                            if ((A[j][k][0] != 0.0) || (A[j][k][1] != 0.0)) {
                            	if (noconj) {
                                    temp[0] = A[j][k][0];
                                    temp[1] = A[j][k][1];
                            	}
                            	else {
                            		temp[0] = A[j][k][0];
                                    temp[1] = -A[j][k][1];	
                            	}

                                for (i = 0; i < m; i++) {
                                	zmlt(temp[0], temp[1], B[i][k][0], B[i][k][1], cr, ci);
                                	B[i][j][0] = B[i][j][0] - cr[0];
                                	B[i][j][1] = B[i][j][1] - ci[0];
                                }
                            } // if ((A[j][k][0] != 0.0) || (A[j][k][1] != 0.0))
                        } // for (j = 0; j <= k-1; j++)

                        if ((alpha[0] != 1.0) || (alpha[1] != 0.0)) {

                            for (i = 0; i < m; i++) {
                            	zmlt(alpha[0], alpha[1], B[i][k][0], B[i][k][1], cr, ci);
                            	B[i][k][0] = cr[0];
                            	B[i][k][1] = ci[0];
                            }
                        } // if ((alpha[0] != 1.0) || (alpha[1] != 0.0))
                    } // for (k = n-1; k >= 0; k--)
                } // if (upper)
                else { // lower

                    for (k = 0; k < n; k++) {

                        if (nounit) {
                        	if (noconj) {
                        		zdiv(1.0, 0.0, A[k][k][0], A[k][k][1], cr, ci);
                            	temp[0] = cr[0];
                            	temp[1] = ci[0];
                        	}
                        	else {
                        		zdiv(1.0, 0.0, A[k][k][0], -A[k][k][1], cr, ci);
                            	temp[0] = cr[0];
                            	temp[1] = ci[0];	
                        	}

                            for (i = 0; i < m; i++) {
                            	zmlt(temp[0], temp[1], B[i][k][0], B[i][k][1], cr, ci);
                            	B[i][k][0] = cr[0];
                            	B[i][k][1] = ci[0];
                            }
                        } // if (nounit)

                        for (j = k + 1; j < n; j++) {

                        	if ((A[j][k][0] != 0.0) || (A[j][k][1] != 0.0)) {
                        		if (noconj) {
                                    temp[0] = A[j][k][0];
                                    temp[1] = A[j][k][1];
                        		}
                        		else {
                        			temp[0] = A[j][k][0];
                                    temp[1] = -A[j][k][1];	
                        		}

                                for (i = 0; i < m; i++) {
                                	zmlt(temp[0], temp[1], B[i][k][0], B[i][k][1], cr, ci);
                                	B[i][j][0] = B[i][j][0] - cr[0];
                                	B[i][j][1] = B[i][j][1] - ci[0];
                                }
                            } // if ((A[j][k][0] != 0.0) || (A[j][k][1] != 0.0))
                        } // for (j = k+1; j < n; j++)

                        if ((alpha[0] != 1.0) || (alpha[1] != 0.0)) {

                            for (i = 0; i < m; i++) {
                            	zmlt(alpha[0], alpha[1], B[i][k][0], B[i][k][1], cr, ci);
                            	B[i][k][0] = cr[0];
                            	B[i][k][1] = ci[0];
                            }
                        } // if ((alpha[0] != 1.0) || (alpha[1] != 0.0))
                    } // for (k = 0; k < n; k++)
                } // else lower
            } // else ((transa != 'N') && (transa != 'n'))
        } // else !lside

        return;
    } // ztrsm
    
    /**
     * This is a port of the 2/8/89 Blas routine Original version written by: Jack Dongarra, Argonne National Laboratory
     * Iain Duff, AERE Harwell. Jeremy Du Croz, Numerical Algorithms Group Ltd. Sven Hammarling, Numerical Algorithms
     * Group Ltd. zgemm performs one of the matrix-matrix operations C = alpha*op(A)*op(B) + beta*C, where op(X) is one
     * of op(X) = X or op(X) = X**T or op(X) = x**H, alpha and beta are scalars, and A, B, and C are matrices, with op(A) an m by k
     * matrix, op(B) a k by n matrix, and C an m by n matrix.
     * 
     * @param transa input char On entry, transa specifies the form of op(A) to be used in the matrix multiplication as
     *            follows:' = 'N' or 'n', op(A) = A. = 'T' or 't', op(A) = A**T. = 'C' or 'c', op(A) = A**H.
     * @param transb input char On entry, transb specifies the form of op(B) to be used in the matrix multiplication as
     *            follows: = 'N' or 'n', op(B) = B. = 'T' or 't', op(B) = B**T. = 'C' or 'c', op(B) = B**H.
     * @param m input int On entry, m specifies the number of rows of the matrix op(A) and of the matrix C. m must be at
     *            least zero.
     * @param n input int On entry, n specifies the number of columns of the matrix op(B) and the number of columns of
     *            the matrix C. n must be at least zero.
     * @param k input int On entry, k specifies the number of columns of the matrix op(A) and the number of rows of the
     *            matrix op(B). k must be at least zero.
     * @param alpha input double[2] specified complex scalar
     * @param A input double[][][2] complex dimension lda by ka, where ka is k when transa = 'N' or 'n', and is m otherwise. Before
     *            entry with transa = 'N' or 'n', the leading m by k part of the array A must contain the matrix A,
     *            otherwise the leading k by m part of the array A must contain the matrix A
     * @param lda input int On entry, lda specifies the first dimension of A as declared in the calling (sub) program.
     *            When transa = 'N' or 'n' then lda must be at least max(1,m), otherwise lda must be at least max(1,k)
     * @param B input double[][][2] complex dimension ldb by kb, where kb is n when transb = 'N' or 'n', and is k otherwise. Before
     *            entry with transb = 'N' or 'n', the leading k by n part of the array B must contain the matrix B,
     *            otherwise the leading n by k part of the array B must contain the matrix B
     * @param ldb input int On entry, ldb specifies the first dimension of B as declared in the calling (sub) program.
     *            When transb = 'N' or 'n' then ldb must be at least max(1,k), otherwise ldb must be at least max(1,n).
     * @param beta input double[2] specified complex scalar When beta is supplied as zero, then C need not be set on input.
     * @param C input/output double[][][2] complex dimension ldc by n. Before entry, the leading m by n part of the array C must
     *            contain the matrix C, except when beta is zero, in which case C need not be set on entry. On exit, the
     *            array C is overwritten by the m by n matrix (alpha*op(A)*op(B) + beta*C).
     * @param ldc input int On entry, ldc specifies the first dimension of C as declared in the calling (sub) program.
     *            ldc must be at least max(1,m).
     */
    public void zgemm(final char transa, final char transb, final int m, final int n, final int k, final double alpha[],
            final double[][][] A, final int lda, final double[][][] B, final int ldb, final double beta[],
            final double[][][] C, final int ldc) {
    	boolean conja;
    	boolean conjb;
        boolean nota;
        boolean notb;
        int i;
        int info;
        int j;
        int L;
        int ncola;
        int nrowa;
        int nrowb;
        double temp[] = new double[2];
        int p;
        double cr[] = new double[1];
        double ci[] = new double[1];
        double cr2[] = new double[1];
        double ci2[] = new double[1];

        // Set nota and notb as true if A and B respectively are not comjugated or transposed,
        // set trnasa and tranb as true if A and B are to be transposed but not conjugated.
        // set conja and conjb as true if A and B are to be conjugated
        // Set ncola as the number of columns of A.
        // and set nrowa and nrowb as the number of rows of A
        // and the number of rows of B respectively.

        if ( (transa == 'N') || (transa == 'n')) {
            nota = true;
        } else {
            nota = false;
        }

        if ( (transb == 'N') || (transb == 'n')) {
            notb = true;
        } else {
            notb = false;
        }
        
        if ( (transa == 'C') || (transa == 'c')) {
            conja = true;
        } else {
            conja = false;
        }

        if ( (transb == 'C') || (transb == 'c')) {
            conjb = true;
        } else {
            conjb = false;
        }

        if (nota) {
            nrowa = m;
            ncola = k;
        } else {
            nrowa = k;
            ncola = m;
        }

        if (notb) {
            nrowb = k;
        } else {
            nrowb = n;
        }

        // Test the input parameters
        info = 0;

        if ( (!nota) && (!conja) && (transa != 'T') && (transa != 't')) {
            info = 1;
        } else if ( (!notb) && (!conjb) && (transb != 'T') && (transb != 't')) {
            info = 2;
        } else if (m < 0) {
            info = 3;
        } else if (n < 0) {
            info = 4;
        } else if (k < 0) {
            info = 5;
        } else if (lda < Math.max(1, nrowa)) {
            info = 8;
        } else if (ldb < Math.max(1, nrowb)) {
            info = 10;
        } else if (ldc < Math.max(1, m)) {
            info = 13;
        }

        if (info != 0) {
            MipavUtil.displayError("Error zgemm has info = " + info);

            return;
        } // if (info != 0)

        // Quick return if possible
        if ( (m == 0) || (n == 0) || ( ( ((alpha[0] == 0.0) && (alpha[1] == 0.0)) || (k == 0)) && ((beta[0] == 1.0) && (beta[1] == 0.0)))) {
            return;
        }
        
        // And when alpha equals zero

        if ((alpha[0] == 0.0) && (alpha[1] == 0.0)) {

            if ((beta[0] == 0.0) && (beta[1] == 0.0)) {

                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                    	for (p = 0; p < 2; p++) {
                            C[i][j][p] = 0.0;
                    	}
                    }
                }
            } // if ((beta[0] == 0.0) && (beta[1] == 0.0))
            else { // beta != 0.0

                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                    	zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr, ci);
                    	C[i][j][0] = cr[0];
                    	C[i][j][1] = ci[0];
                    }
                }
            } // else beta != 0.0

            return;
        } // if (alpha == 0.0)
        
        // Start the operations

        if (notb) {

            if (nota) {

                // Form C = alpha*A*B + beta*C.
                for (j = 0; j < n; j++) {

                    if ((beta[0] == 0.0) && (beta[1] == 0.0)) {

                        for (i = 0; i < m; i++) {
                        	for (p = 0; p < 2; p++) {
                                C[i][j][p] = 0.0;
                        	}
                        }
                    } // if ((beta[0] == 0.0) && (beta[1] == 0.0))
                    else if ((beta[0] != 1.0) || (beta[1] != 0.0)) {

                        for (i = 0; i < m; i++) {
                        	zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr, ci);
                        	C[i][j][0] = cr[0];
                        	C[i][j][1] = ci[0];
                        }
                    } // else if ((beta[0] != 1.0) || (beta[1] != 0.0))

                    for (L = 0; L < k; L++) {

                        zmlt(alpha[0], alpha[1], B[L][j][0], B[L][j][1], cr, ci);
                        temp[0] = cr[0];
                        temp[1] = ci[0];

                        for (i = 0; i < m; i++) {
                        	zmlt(temp[0], temp[1], A[i][L][0], A[i][L][1], cr, ci);
                        	C[i][j][0] = C[i][j][0] + cr[0];
                        	C[i][j][1] = C[i][j][1] + ci[0];
                        }
                    } // for (L = 0; L < k; L++)
                } // for (j = 0; j < n; j++)
            } // if (nota)
            else if (conja){ 

                // Form C = alpha*A**T*B + beta*C
                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        temp[0] = 0.0;
                        temp[1] = 0.0;

                        for (L = 0; L < k; L++) {
                        	zmlt(A[L][i][0], -A[L][i][1], B[L][j][0], B[L][j][1], cr, ci);
                        	temp[0] = temp[0] + cr[0];
                        	temp[1] = temp[1] + ci[0];
                        }

                        if ((beta[0] == 0.0) && (beta[1] == 0.0)) {
                        	zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                        	C[i][j][0] = cr[0];
                        	C[i][j][1] = ci[0];
                        } else {
                        	zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                        	zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr2, ci2);
                        	C[i][j][0] = cr[0] + cr2[0];
                        	C[i][j][1] = ci[0] + ci2[0];
                        }
                    } // for (i = 0; i < m; i++)
                } // for (j = 0; j < n; j++)
            } // else if (conja)
            else { 

                // Form C = alpha*A**T*B + beta*C
                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        temp[0] = 0.0;
                        temp[1] = 0.0;

                        for (L = 0; L < k; L++) {
                        	zmlt(A[L][i][0], A[L][i][1], B[L][j][0], B[L][j][1], cr, ci);
                        	temp[0] = temp[0] + cr[0];
                        	temp[1] = temp[1] + ci[0];
                        }

                        if ((beta[0] == 0.0) && (beta[1] == 0.0)) {
                        	zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                        	C[i][j][0] = cr[0];
                        	C[i][j][1] = ci[0];
                        } else {
                        	zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                        	zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr2, ci2);
                        	C[i][j][0] = cr[0] + cr2[0];
                        	C[i][j][1] = ci[0] + ci2[0];
                        }
                    } // for (i = 0; i < m; i++)
                } // for (j = 0; j < n; j++)
            } // else
        } // if (notb)
        else if (nota) {
        	if (conjb) {
        		// Form C = alpha*A*B**H + beta*C
                for (j = 0; j < n; j++) {

                    if ((beta[0] == 0.0) && (beta[1] == 0.0)) {

                        for (i = 0; i < m; i++) {
                        	for (p = 0; p < 2; p++) {
                                C[i][j][p] = 0.0;
                        	}
                        }
                    } // if ((beta[0] == 0.0) && (beta[1] == 0.0))
                    else if ((beta[0] != 1.0) || (beta[1] != 0.0)) {

                        for (i = 0; i < m; i++) {
                        	zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr, ci);
                        	C[i][j][0] = cr[0];
                        	C[i][j][1] = ci[0];
                        }
                    } // else if ((beta[0] != 1.0) || (beta[1] != 0.0))

                    for (L = 0; L < k; L++) {
                        zmlt(alpha[0], alpha[1], B[j][L][0], -B[j][L][1], cr, ci);
                        temp[0] = cr[0];
                        temp[1] = ci[0];

                        for (i = 0; i < m; i++) {
                        	zmlt(temp[0], temp[1], A[i][L][0], A[i][L][1], cr, ci);
                        	C[i][j][0] = C[i][j][0] + cr[0];
                        	C[i][j][1] = C[i][j][1] + ci[0];
                        }
                    } // for (L = 0; L < k; L++)
                } // for (j = 0; j < n; j++)	
        	} // if (conjb)
        	else {
        		// Form C = alpha*A*B**T + beta*C
                for (j = 0; j < n; j++) {

                    if ((beta[0] == 0.0) && (beta[1] == 0.0)) {

                        for (i = 0; i < m; i++) {
                        	for (p = 0; p < 2; p++) {
                                C[i][j][p] = 0.0;
                        	}
                        }
                    } // if ((beta[0] == 0.0) && (beta[1] == 0.0))
                    else if ((beta[0] != 1.0) || (beta[1] != 0.0)) {

                        for (i = 0; i < m; i++) {
                        	zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr, ci);
                        	C[i][j][0] = cr[0];
                        	C[i][j][1] = ci[0];
                        }
                    } // else if ((beta[0] != 1.0) || (beta[1] != 0.0))

                    for (L = 0; L < k; L++) {
                        zmlt(alpha[0], alpha[1], B[j][L][0], B[j][L][1], cr, ci);
                        temp[0] = cr[0];
                        temp[1] = ci[0];

                        for (i = 0; i < m; i++) {
                        	zmlt(temp[0], temp[1], A[i][L][0], A[i][L][1], cr, ci);
                        	C[i][j][0] = C[i][j][0] + cr[0];
                        	C[i][j][1] = C[i][j][1] + ci[0];
                        }
                    } // for (L = 0; L < k; L++)
                } // for (j = 0; j < n; j++)	
        	} // else
        } // else if (nota)
        else if (conja) {
        	if (conjb) {
        		 // Form C = alpha*A**H*B**H + beta*C
                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        temp[0] = 0.0;
                        temp[1] = 0.0;

                        for (L = 0; L < k; L++) {
                        	zmlt(A[L][i][0], -A[L][i][1], B[j][L][0], -B[j][L][1], cr, ci);
                        	temp[0] = temp[0] + cr[0];
                        	temp[1] = temp[1] + ci[0];
                        }

                        if ((beta[0] == 0.0) && (beta[1] == 0.0)) {
                        	zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                        	C[i][j][0] = cr[0];
                        	C[i][j][1] = ci[0];
                        } else {
                        	zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                        	zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr2, ci2);
                        	C[i][j][0] = cr[0] + cr2[0];
                        	C[i][j][1] = ci[0] + ci2[0];
                        }
                    } // for (i = 0; i < m; i++)
                } // for (j = 0; j < n; j++)	
        	} // if (conjb)
        	else {
       		 // Form C = alpha*A**H*B**T + beta*C
               for (j = 0; j < n; j++) {

                   for (i = 0; i < m; i++) {
                       temp[0] = 0.0;
                       temp[1] = 0.0;

                       for (L = 0; L < k; L++) {
                       	zmlt(A[L][i][0], -A[L][i][1], B[j][L][0], B[j][L][1], cr, ci);
                       	temp[0] = temp[0] + cr[0];
                       	temp[1] = temp[1] + ci[0];
                       }

                       if ((beta[0] == 0.0) && (beta[1] == 0.0)) {
                       	    zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                       	    C[i][j][0] = cr[0];
                       	    C[i][j][1] = ci[0];
                       } else {
                       	    zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                       	    zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr2, ci2);
                       	    C[i][j][0] = cr[0] + cr2[0];
                       	    C[i][j][1] = ci[0] + ci2[0];
                       }
                   } // for (i = 0; i < m; i++)
               } // for (j = 0; j < n; j++)	
       	    } // else
        } // else if (conja)
        else { 

            if (conjb) {
            	 // Form C = alpha*A**T*B**H + beta*C
                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        temp[0] = 0.0;
                        temp[1] = 0.0;

                        for (L = 0; L < k; L++) {
                        	zmlt(A[L][i][0], A[L][i][1], B[j][L][0], -B[j][L][1], cr, ci);
                           	temp[0] = temp[0] + cr[0];
                           	temp[1] = temp[1] + ci[0];
                        }

                        if ((beta[0] == 0.0) && (beta[1] == 0.0)) {
                       	    zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                       	    C[i][j][0] = cr[0];
                       	    C[i][j][1] = ci[0];
                       } else {
                       	    zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                       	    zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr2, ci2);
                       	    C[i][j][0] = cr[0] + cr2[0];
                       	    C[i][j][1] = ci[0] + ci2[0];
                       }
                    } // for (i = 0; i < m; i++)
                } // for (j = 0; j < n; j++)
                
            } // if (conjb)
            else { 

                // Form C = alpha*A**t*B**T + beta*C
                for (j = 0; j < n; j++) {

                    for (i = 0; i < m; i++) {
                        temp[0] = 0.0;
                        temp[1] = 0.0;

                        for (L = 0; L < k; L++) {
                        	zmlt(A[L][i][0], A[L][i][1], B[j][L][0], B[j][L][1], cr, ci);
                           	temp[0] = temp[0] + cr[0];
                           	temp[1] = temp[1] + ci[0];
                        }

                        if ((beta[0] == 0.0) && (beta[1] == 0.0)) {
                       	    zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                       	    C[i][j][0] = cr[0];
                       	    C[i][j][1] = ci[0];
                       } else {
                       	    zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                       	    zmlt(beta[0], beta[1], C[i][j][0], C[i][j][1], cr2, ci2);
                       	    C[i][j][0] = cr[0] + cr2[0];
                       	    C[i][j][1] = ci[0] + ci2[0];
                       }
                    } // for (i = 0; i < m; i++)
                } // for (j = 0; j < n; j++)
            } // else
        } // else

        return;
    } // zgemm
    
    /**
     * complex divide c = a/b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    private void zdiv(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double bm, cc, cd, ca, cb;

        bm = 1.0 / zabs(br, bi);
        cc = br * bm;
        cd = bi * bm;
        ca = ( (ar * cc) + (ai * cd)) * bm;
        cb = ( (ai * cc) - (ar * cd)) * bm;
        cr[0] = ca;
        ci[0] = cb;

        return;
    }
    
    /**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     * 
     * @param zr double
     * @param zi double
     * 
     * @return double
     */
    private double zabs(final double zr, final double zi) {
        double u, v, q, s;
        u = Math.abs(zr);
        v = Math.abs(zi);
        s = u + v;

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        s = s * 1.0;

        if (s == 0.0) {
            return 0.0;
        } else if (u > v) {
            q = v / u;

            return (u * Math.sqrt(1.0 + (q * q)));
        } else {
            q = u / v;

            return (v * Math.sqrt(1.0 + (q * q)));
        }
    }
    
    /**
     * complex multiply c = a * b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    private void zmlt(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double ca, cb;

        ca = (ar * br) - (ai * bi);
        cb = (ar * bi) + (ai * br);
        cr[0] = ca;
        ci[0] = cb;

        return;
    }
}