package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.*;

public class GeneralizedInverse2 implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    private double A[][];
    
    // Number of rows in A
    private int NR;
    // Number of columns in A
    private int NC;
    
    // Double precision routine variables found in routine ge.dlamch
    private double base;
    private double emax;
    private double emin;
    private double eps;
    private boolean first = true;
    private double prec;
    private double rmax;
    private double rmin;
    private double rnd;
    private double sfmin;
    private double t;
    
    /** Double precison machine variables found in routine ge.dlartg. */
    private boolean first_ge.dlartg = true;
    private double safmin;
    private double safmn2;
    private double safmx2
    
    /** Found in routine xlaenv */
    private int iparms[];
    
    /** Found in routine dlatb4 */
    private boolean first_dlatb4 = true;
    private double eps_dlatb4;
    private double small_dlatb4[] = new double[1];
    private double large_dlatb4[] = new double[1];
    private double badc1_dlatb4;
    private double badc2_dlatb4;
    
    public GeneralizedInverse2() {
            
    }
    
    public GeneralizedInverse2(double A1[][], int NR, int NC) {
        int i;
        int j;
        this.NR = NR;
        this.NC = NC;
        A = new double[NR][NC];
        for (i = 0; i < NR; i++) {
            for (j = 0; j < NC; j++) {
                A[i][j] = A1[i][j];
            }
        }
    }
    
    
    
    // Computes the inner product of columns JC and KC
    private double dot(double A[][], int NR, int JC, int KC) {
        double dot = 0.0;
        int i;
        for (i = 0; i < NR; i++) {
            dot += A[i][JC-1]*A[i][KC-1];
        }
        return dot;
    }
    
   
    
    /**
     * Slight modification of pinv in dtiFitTensor.c.
     * This is an interface to the Java port of dgelss.
     * @return n by m pseudoinverse of m by n matrix A
     */
    public double[][] pinv() {
        int m = A.length;
        int n = A[0].length;
        int i;
        int j;
        
        // Output matrix that contains the pseudoinverse or generalized inverse of A.
        double AIN[][];
        // leading dimension of B
        int ldb = Math.max(m, n);
        // Number of singular values
        int singularValues = Math.min(m, n);
        // rcond is used to determine the effective rank of A.
        // Singular values s[i] <= rcond*s[0] are treated as zero.
        // If rcond < 0, machine precision is used instead.
        double rcond = 1.0E-8;
        // Size of workspace
        int lwork = 30 * m * n;
        // The number of right hand sides, i.e., the number of columns of the
        // matrices B and X.
        int nrhs = m;
        // The leading dimension of array A
        int lda = m;
        double work[] = new double[lwork];
        // The singular values of A in decreasing order.
        // The condition number of A in the 2-norm = s[0]/(s[min(m-1,n-1)]).
        double s[] = new double[singularValues];
        // The effective rank of A, i.e., the number of singular values
        // which are greater than rcond*s[0].
        int rank[] = new int[1];
        // info[0] = 0: successful exit
        // info[0] < 0: If info = -i, the i-th argument had an illegal value
        // info[0] > 0: The algorithm for computing the SVD failed to converge.
        //              if info[0] = i, i off-diagonal elements of an intermediate
        //              bidiagonal form did not converge to zero.
        int info[] = new int[1];
        
        double B[][] = new double[ldb][m];
        for (i = 0; i < m; i++) {
            B[i][i] = 1.0;
        }
        
        dgelss(m, n, nrhs, A, lda, B, ldb, s, rcond, rank, work, lwork, info);
        
        AIN = new double[n][m];
        for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                AIN[i][j] = B[i][j];
            }
        }
        return AIN;
    } // pinv
    
    // There are 4 self tests used in testing the port of dgelss.
    // 1.) dchkqr_test() tests ge.dgeqrf, ge.dorgqr, and ge.dormqr.  All 30744 tests run passed the threshold.
    // 2.) dchklq_test() tests dgelqf, dorglq, and dormlq.  All 30744 tests run passed the threshold.
    // 3.) dchkbd_test() tests dgebrd, dorgbr, and dbdsqr.  All 4123 tests pass the threshold.
    // 4.) ddrvls_test() tests dgelss. All 17640 tests passed the threshold.
    
    /* This is a port of dgelss, LAPACK driver routine from version 3.2
    *
    *  -- LAPACK driver routine (version 3.2) --
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *     November 2006
    *
    *     .. Scalar Arguments ..
          INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
          DOUBLE PRECISION   RCOND
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DGELSS computes the minimum norm solution to a real linear least
    *  squares problem:
    *
    *  Minimize 2-norm(| b - A*x |).
    *
    *  using the singular value decomposition (SVD) of A. A is an M-by-N
    *  matrix which may be rank-deficient.
    *
    *  Several right hand side vectors b and solution vectors x can be
    *  handled in a single call; they are stored as the columns of the
    *  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
    *  X.
    *
    *  The effective rank of A is determined by treating as zero those
    *  singular values which are less than RCOND times the largest singular
    *  value.
    *
    
    */
    private void dgelss(int m, int n, int nrhs, double A[][], int lda, double B[][], int ldb, double s[],
                        double rcond, int rank[], double work[], int lwork, int info[]) {
        /*  Arguments
        *  =========
        *
        *  m       (input) INTEGER
        *          The number of rows of the matrix A. m >= 0.
        *
        *  n       (input) INTEGER
        *          The number of columns of the matrix A. n >= 0.
        *
        *  nrhs    (input) INTEGER
        *          The number of right hand sides, i.e., the number of columns
        *          of the matrices B and X. nrhs >= 0.
        *
        *  A       (input/output) DOUBLE PRECISION array, dimension (lda,n)
        *          On entry, the m-by-n matrix A.
        *          On exit, the first min(m,n) rows of A are overwritten with
        *          its right singular vectors, stored rowwise.
        *
        *  lda     (input) INTEGER
        *          The leading dimension of the array A.  lda >= max(1,m).
        *
        *  B       (input/output) DOUBLE PRECISION array, dimension (ldb,nrhs)
        *          On entry, the m-by-nrhs right hand side matrix B.
        *          On exit, B is overwritten by the n-by-nrhs solution
        *          matrix X.  If m >= n and RANK = n, the residual
        *          sum-of-squares for the solution in the i-th column is given
        *          by the sum of squares of elements n+1:m in that column.
        *
        *  ldb     (input) INTEGER
        *          The leading dimension of the array B. ldb >= max(1,max(m,n)).
        *
        *  s       (output) DOUBLE PRECISION array, dimension (min(m,n))
        *          The singular values of A in decreasing order.
        *          The condition number of A in the 2-norm = S(1)/S(min(m,n)).
        *
        *  rcond   (input) DOUBLE PRECISION
        *          rcond is used to determine the effective rank of A.
        *          Singular values s(i) <= RCOND*s[0] are treated as zero.
        *          If  rcond < 0, machine precision is used instead.
        *
        *  rank    (output) INTEGER
        *          The effective rank of A, i.e., the number of singular values
        *          which are greater than rcond*s[0].
        *
        *  work    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,lwork))
        *          On exit, if info[0] = 0, work[0] returns the optimal lwork.
        *
        *  lwork   (input) INTEGER
        *          The dimension of the array work. lwork >= 1, and also:
        *          lwork >= 3*min(m,n) + max( 2*min(m,n), max(m,n), nrhs )
        *          For good performance, LWORK should generally be larger.
        *
        *          If lwork = -1, then a workspace query is assumed; the routine
        *          only calculates the optimal size of the work array, returns
        *          this value as the first entry of the work array, and no error
        *          message related to lwork is issued by XERBLA.
        *
        *  info    (output) INTEGER
        *          = 0:  successful exit
        *          < 0:  if info[0] = -i, the i-th argument had an illegal value.
        *          > 0:  the algorithm for computing the SVD failed to converge;
        *                if info[0] = i, i off-diagonal elements of an intermediate
        *                bidiagonal form did not converge to zero.
        */
        int minmn;
        int maxmn;
        boolean lquery;
        int minwrk;
        int maxwrk = 0;
        int mm;
        int mnthr = 0;
        String name;
        String opts;
        int bdspac;
        double[] smlnum = new double[1];
        double[] bignum = new double[1];
        double anrm;
        int iascl;
        int i;
        double bnrm;
        int ibscl;
        int itau;
        int iwork;
        double vector1[];
        double vector2[];
        int row1;
        double array1[][];
        double array2[][];
        double array3[][];
        int j;
        int k;
        int ie;
        int itauq;
        int itaup;
        double work2[];
        double work3[];
        double work4[];
        double work5[];
        int dimw;
        double vdum2[][] = new double[1][1];
        double thr;
        int chunk;
        int bl;
        int len;
        int ldwork;
        int il;
        int p;
        
        // Test the input arguments
        info[0] = 0;
        minmn = Math.min(m, n);
        maxmn = Math.max(m, n);
        lquery = (lwork == -1);
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (nrhs < 0) {
            info[0] = -3;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -5;
        }
        else if (ldb < Math.max(1, maxmn)) {
            info[0] = -7;
        }
        
        // Compute workspace
        // (Note: Comments in the code beginning "Workspace:" describe the minimal amount of workspace needed at
        // that point in the code, as well as the preferred amount for good performance.  nb refers to the
        // optimal block size for the immediately following subroutine, as returned by ge.ilaenv.)
        
        if (info[0] == 0) {
            minwrk = 1;
            maxwrk = 1;
            if (minmn > 0) {
                mm = m;
                name = new String("DGELSS");
                opts = new String(" ");
                mnthr = ge.ilaenv(6, name, opts, m, n, nrhs, -1);
                if ((m >= n) && (m >= mnthr)) {
                    // Path 1a - overdetermined, with many more rows than columns
                    mm = n;
                    name = new String("DGEQRF");
                    opts = new String(" ");
                    maxwrk = Math.max(maxwrk, n + n*ge.ilaenv(1, name, opts, m, n, -1, -1));
                    name = new String("DORMQR");
                    opts = new String("LT");
                    maxwrk = Math.max(maxwrk, n + nrhs*ge.ilaenv(1, name, opts, m, nrhs, n, -1));
                } // if ((m >= n) && (m >= mnthr))
                if (m >= n) {
                    // Path 1 - overdetermined or exactly determined
                    
                    // Compute workspace needed for dbdsqr
                    bdspac = Math.max(1, 5*n);
                    name = new String("DGEBRD");
                    opts = new String(" ");
                    maxwrk = Math.max(maxwrk, 3*n + (mm + n)*ge.ilaenv(1, name, opts, mm, n, -1, -1));
                    name = new String("DORMBR");
                    opts = new String("QLT");
                    maxwrk = Math.max(maxwrk, 3*n + nrhs*ge.ilaenv(1, name, opts, mm, nrhs, n, -1));
                    name = new String("DORGBR");
                    opts = new String("P");
                    maxwrk = Math.max(maxwrk, 3*n + (n - 1)*ge.ilaenv(1, name, opts, n, n, n, -1));
                    maxwrk = Math.max(maxwrk, bdspac);
                    maxwrk = Math.max(maxwrk, n*nrhs);
                    minwrk = Math.max(3*n + mm, Math.max(3*n + nrhs, bdspac));
                    maxwrk = Math.max(minwrk, maxwrk);
                } // if (m >= n)
                if (n > m) {
                    // Compute workspace needed for dbdsqr
                    
                    bdspac = Math.max(1, 5*m);
                    minwrk = Math.max(3*m + nrhs, Math.max(3*m + n, bdspac));
                    if (n >= mnthr) {
                        // Path 2a - undetermined, with many more columns than rows
                        name = new String("DGELQF");
                        opts = new String(" ");
                        maxwrk = m + m*ge.ilaenv(1, name, opts, m, n, -1, -1);
                        name = new String("DGEBRD");
                        opts = new String(" ");
                        maxwrk = Math.max(maxwrk, m*m + 4*m + 2*m*ge.ilaenv(1, name, opts, m, m, -1, -1));
                        name = new String("DORMBR");
                        opts = new String("QLT");
                        maxwrk = Math.max(maxwrk, m*m + 4*m + nrhs*ge.ilaenv(1, name, opts, m, nrhs, m, -1));
                        name = new String("DORGBR");
                        opts = new String("P");
                        maxwrk = Math.max(maxwrk, m*m + 4*m + (m - 1)*ge.ilaenv(1, name, opts, m, m, m, -1));
                        maxwrk = Math.max(maxwrk, m*m + m + bdspac);
                        if (nrhs > 1) {
                            maxwrk = Math.max(maxwrk, m*m + m + m*nrhs);    
                        } // if (nrhs > 1)
                        else {
                            maxwrk = Math.max(maxwrk, m*m + 2*m);
                        }
                        name = new String("DORMLQ");
                        opts = new String("LT");
                        maxwrk = Math.max(maxwrk, m + nrhs*ge.ilaenv(1, name, opts, n, nrhs, m, -1));
                    } // if (n >= mnthr)
                    else { // n < mnthr
                        // Path 2 - underdetermined
                        name = new String("DGEBRD");
                        opts = new String(" ");
                        maxwrk = 3*m + (n + m)*ge.ilaenv(1, name, opts, m, n, -1, -1);
                        name = new String("DORMBR");
                        opts = new String("QLT");
                        maxwrk = Math.max(maxwrk, 3*m + nrhs*ge.ilaenv(1, name, opts, m, nrhs, m, -1));
                        name = new String("DORGBR");
                        opts = new String("P");
                        maxwrk = Math.max(maxwrk, 3*m + m*ge.ilaenv(1, name, opts, m, n, m, -1));
                        maxwrk = Math.max(maxwrk, bdspac);
                        maxwrk = Math.max(maxwrk, n*nrhs);
                    } // else n < mnthr
                } // if (n > m)
                maxwrk = Math.max(minwrk, maxwrk);
            } // if (minmn > 0)
            work[0] = maxwrk;
            
            if ((lwork < minwrk) && (!lquery)) {
                info[0] = -12;
            }
        } // if (info[0] == 0)
        
        if (info[0] != 0) {
            MipavUtil.displayError("DGELSS exits with error info[0] = " + info[0]);
            Preferences.debug("DGELSS exits with error info[0] = " + info[0] + "\n");
            return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        if ((m == 0) || (n == 0)) {
            rank[0] = 0;
            return;
        }
        
        // Get machine parameters
        eps = ge.dlamch('P');
        sfmin = ge.dlamch('S');
        smlnum[0] = sfmin/eps;
        bignum[0] = 1.0 / smlnum[0];
        ge.dlabad(smlnum, bignum);
        
        // Scale A if max element outside range [smlnum[0], bignum[0]]
        anrm = ge.dlange('M', m, n, A, lda, work);
        iascl = 0;
        if ((anrm > 0.0) && (anrm < smlnum[0])) {
            // Scale matrix norm up to smlnum[0]
            ge.dlascl('G', 0, 0, anrm, smlnum[0], m, n, A, lda, info);
            iascl = 1;
        } // if ((anrm > 0.0) && (anrm < smlnum[0]))
        else if (anrm > bignum[0]) {
            // Scale matrix norm down to bignum[0]
            ge.dlascl('G', 0, 0, anrm, bignum[0], m, n, A, lda, info);
            iascl = 2;
        } // else if (anrm > bignum[0])
        else if (anrm == 0.0) {
            // Matrix all zero.  Return zero solution.
            ge.dlaset('F', Math.max(m,n), nrhs, 0.0, 0.0, B, ldb);
            for (i = 0; i < minmn; i++) {
                s[i] = 0.0;
            }
            rank[0] = 0;
            work[0] = maxwrk;
            return;
        } // else if (anrm == 0.0)
        
        // Scale B if max element outside range [smlnum[0], bignum[0]]
        bnrm = ge.dlange('M', m, nrhs, B, ldb, work);
        ibscl = 0;
        if ((bnrm > 0.0) && (bnrm < smlnum[0])) {
            // Scale matrix norm up to smlnum[0]
            ge.dlascl('G', 0, 0, bnrm, smlnum[0], m, nrhs, B, ldb, info);
            ibscl = 1;
        }
        else if (bnrm > bignum[0]) {
            // Scale matrix down to bignum[0]
            ge.dlascl('G', 0, 0, bnrm, bignum[0], m, nrhs, B, ldb, info);
            ibscl = 2;
        }
        
        // Overdetermined case
        if (m >= n) {
            // Path 1 - overdetermined or exactly determined
            mm = m;
            if (m >= mnthr) {
                // Path 1a - overdetermined, with many more rows than columns
                mm = n;
                itau = 1;
                iwork = itau + n;
                
                // Compute A = Q*R
                // (Workspace: need 2*n, prefer n + n*nb)
                vector1 = new double[Math.min(m, n)];
                vector2 = new double[Math.max(1, lwork - iwork + 1)];
                ge.dgeqrf(m, n, A, lda, vector1, vector2, lwork - iwork + 1, info);
                for (i = 0; i < vector1.length; i++) {
                    work[itau - 1 + i] = vector1[i];
                }
                
                for (i = 0; i < vector2.length; i++) {
                    work[iwork - 1 + i] = vector2[i];
                }
                
                // Multiply B by transpose(Q)
                // (Workspace: need n + nrhs, prefer n + nrhs*nb)
                vector1 = new double[n];
                for (i = 0; i < n; i++) {
                    vector1[i] = work[itau - 1 + i];
                }
                ge.dormqr('L', 'T', m, nrhs, n, A, lda, vector1, B, ldb, vector2, lwork - iwork + 1, info);
                for (i = 0; i < vector2.length; i++) {
                    work[iwork - 1 + i] = vector2[i];
                }
                
                // Zero out below R
                if (n > 1) {
                    row1 = Math.max(1, n - 1); 
                    array1 = new double[row1][n-1];
                    for (i = 0; i < row1; i++) {
                        for (j = 0; j < n - 1; j++) {
                            array1[i][j] = A[1 + i][j];    
                        }
                    }
                    ge.dlaset('L', n - 1, n - 1, 0.0, 0.0, array1, row1);
                    for (i = 0; i < row1; i++) {
                        for (j = 0; j < n - 1; j++) {
                            A[1 + i][j] = array1[i][j];    
                        }
                    }
                } // if (n > 1)
            }  // if (m >= mnthr)
            
            ie = 1;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            
            // Bidiagonalize R in A
            // (Workspace: need 3*n + mm, prefer 3*n + (mm + n)*nb)
            work2 = new double[n];
            work3 = new double[n];
            dimw = Math.max(1, lwork-iwork+1);
            work4 = new double[dimw];
            dgebrd(mm, n, A, lda, s, work, work2, work3, work4, lwork-iwork+1, info);
            for (j = 0; j < Math.min(mm,n); j++) {
                work[itauq - 1 + j] = work2[j];
                work[itaup - 1 + j] = work3[j];
            }
            for (j = 0; j < dimw; j++) {
                work[iwork - 1 + j] = work4[j];
            }
            
            // Multiply B by transpose of left bidiagonalizing vectors of R
            // (Workspace: need 3*n + nrhs, prefer 3*n + nrhs*nb)
            dormbr('Q', 'L', 'T', mm, nrhs, n, A, lda, work2, B, ldb, work4, lwork-iwork+1, info);
            for (j = 0; j < dimw; j++) {
                work[iwork - 1 + j] = work4[j];
            }
            
            // Generate right bidiagonalizing vectors of R in A
            // (Workspace: need 4*n - 1, prefer 3*n + (n-1)*nb
            dorgbr('P', n, n, n, A, lda, work3, work4, lwork-iwork+1, info);
            for (j = 0; j < dimw; j++) {
                work[iwork - 1 + j] = work4[j];
            }
            
            iwork = ie + n;
            // Perform bidiagonal QR iteration
            // Multiply B by transpose of left singular vectors
            // Compute right singular vectors in A
            // (Workspace: need bdspac)
            work4 = new double[4*n];
            dbdsqr('U', n, n, 0, nrhs, s, work, A, lda, vdum2, 1, B, ldb, work4, info);
            if (info[0] != 0) {
                Preferences.debug("For m >= n dbdsqr had info[0] = " + info[0] + "\n");
                work[0] = maxwrk;
                return;
            }
            
            // Multiply B by reciprocals of singular values
            
            thr = Math.max(rcond*s[0], sfmin);
            if (rcond < 0.0) {
                thr = Math.max(eps*s[0], sfmin);
            }
            rank[0] = 0;
            for (i = 1; i <= n; i++) {
                if (s[i-1] > thr) {
                    vector1 = new double[nrhs];
                    for (j = 0; j < nrhs; j++) {
                        vector1[j] = B[i-1][j];
                    }
                    drscl(nrhs, s[i-1], vector1, 1);
                    for (j = 0; j < nrhs; j++) {
                        B[i-1][j] = vector1[j];
                    }
                    rank[0] = rank[0] + 1;
                } // if (s[i-1] > thr)
                else {
                    array1 = new double[1][nrhs];
                    for (j = 0; j < nrhs; j++) {
                        array1[0][j] = B[i-1][j];
                    }
                    ge.dlaset('F', 1, nrhs, 0.0, 0.0, array1, 1);
                    for (j = 0; j < nrhs; j++) {
                        B[i-1][j] = array1[0][j];
                    }
                } // else
            } // for (i = 1; i <= n; i++)
            
            // Multiply B by right singular vectors
            // (Workspace: need n, prefer n*nrhs)
            
            if ((lwork >= ldb*nrhs) && (nrhs > 1)) {
                array1 = new double[ldb][nrhs];
                // For beta = 0.0 array1 need not be set on input.
                ge.dgemm('T', 'N', n, nrhs, n, 1.0, A, lda, B, ldb, 0.0, array1, ldb);
                ge.dlacpy('G', n, nrhs, array1, ldb, B, ldb);
                for (j = 0; j < ldb; j++) {
                    for (k = 0; k < nrhs; k++) {
                        work[j + k*ldb] = array1[j][k];    
                    }
                }
            } // if ((lwork >= ldb*nrhs) && (nrhs > 1))
            else if (nrhs > 1) {
                chunk = lwork/n;
                for (i = 1; i <= nrhs; i += chunk) {
                    bl = Math.min(nrhs-i+1, chunk);
                    array1 = new double[n][bl];
                    array2 = new double[n][bl];
                    for (j = 0; j < n; j++) {
                        for (k = 0; k < bl; k++) {
                            array1[j][k] = B[j][i-1+k];
                        }
                    }
                    // For beta = 0.0 array2 need not be set on input
                    ge.dgemm('T', 'N', n, bl, n, 1.0, A, lda, array1, n, 0.0, array2, n);
                    ge.dlacpy('G', n, bl, array2, n, array1, n);
                    for (j = 0; j < n; j++) {
                        for (k = 0; k < bl; k++) {
                            B[j][i-1+k] = array1[j][k];
                        }
                    }
                } // for (i = 1; i <= nrhs; i += chunk)
            } // else if (nrhs > 1)
            else {
                vector1 = new double[n];
                len = 0;
                for (k = 0; k < B[0].length && len < n; k++) {
                    for (j = 0; j < B.length && len < n; j++) {
                        vector1[len++] = B[j][k];
                    }
                }
                ge.dgemv('T', n, n, 1.0, A, lda, vector1, 1, 0.0, work, 1); 
                len = 0;
                for (k = 0; k < B[0].length && len < n; k++) {
                    for (j = 0; j < B.length && len < n; j++) {
                        B[j][k] = work[len++];
                    }
                }
            } // else
        } // if (m >= n)
        else if ((n >= mnthr) && (lwork >= 4*m + m*m + Math.max(m, Math.max(2*m-4, 
                  Math.max(nrhs, n - 3*m))))) {
            // Path2a - underdetermined, with many more columns than rows
            // and sufficient workspace for an efficient algorithm
            ldwork = m;
            if (lwork >= Math.max(4*m + m*lda + Math.max(m, Math.max(2*m-4, 
                    Math.max(nrhs, n-3*m))), m*lda + m + m*nrhs)) {
                ldwork = lda;
            }
            itau = 1;
            iwork = m + 1;
            
            // Compute A = L*Q
            // (Workspace: need 2*m, prefer m + m*nb)
            work2 = new double[Math.max(1, lwork-iwork+1)];
            dgelqf(m, n, A, lda, work, work2, lwork-iwork+1, info);
            for (j = 0; j < Math.max(1, lwork-iwork+1); j++) {
                work[j+iwork-1] = work2[j];
            }
            il = iwork;
            
            // Copy L to work(il), zeroing out above it.
            array1 = new double[ldwork][m];
            ge.dlacpy('L', m, m, A, lda, array1, ldwork);
            for (k = 0; k < m; k++) {
                for (j = 0; j < ldwork; j++) {
                    work[il - 1 + j + k*ldwork] = array1[j][k]; 
                }
            }
            array1 = new double[ldwork][m-1];
            p = 0;
            for (k = 0; k < m-1; k++) {
                for (j = 0; j < ldwork; j++, p++) {
                    array1[j][k] = work[il+ldwork-1+p];
                }
            }
            ge.dlaset('U', m-1, m-1, 0.0, 0.0, array1, ldwork);
            p = 0;
            for (k = 0; k < m-1; k++) {
                for (j = 0; j < ldwork; j++, p++) {
                    work[il+ldwork-1+p] = array1[j][k];
                }
            }
            ie = il + ldwork*m;
            itauq = ie + m;
            itaup = itauq + m;
            iwork = itaup + m;
            
            // Bidiagonalize L in work(il)
            // (Workspace: need m*m+5*m prefer m*m+4*m+2*m*nb)
            array1 = new double[ldwork][m];
            p = 0;
            for (k = 0; k < m; k++) {
                for (j = 0; j < ldwork; j++, p++) {
                    array1[j][k] = work[il-1+p];
                }
            }
            work2 = new double[m-1];
            work3 = new double[m];
            work4 = new double[m];
            work5 = new double[Math.max(1, lwork-iwork+1)];
            dgebrd(m, m, array1, ldwork, s, work2, work3, work4, work5, lwork-iwork+1, info);
            p = 0;
            for (k = 0; k < m; k++) {
                for (j = 0; j < ldwork; j++, p++) {
                    work[il-1+p] = array1[j][k];
                }
            }
            for (j = 0; j < m-1; j++) {
                work[ie-1+j] = work2[j];
            }
            for (j = 0; j < m; j++) {
                work[itauq-1+j] = work3[j];
                work[itaup-1+j] = work4[j];
            }
            for (j = 0; j < Math.max(1, lwork-iwork+1); j++) {
                work[iwork-1+j] = work5[j];
            }
            
            // Multiply B by transpose of left bidiagonalizing vectors of L
            // (Workspace: need m*m+4*m+nrhs, prefer m*m + 4*m + nrhs*nb)
            dormbr('Q', 'L', 'T', m, nrhs, m, array1, ldwork, work3, B, ldb, work5, lwork-iwork+1, info);
            for (j = 0; j < Math.max(1, lwork-iwork+1); j++) {
                work[iwork-1+j] = work5[j];
            }
            
            // Generate right bidiagonalizing vectors of R in work[il]
            // (Workspace:  need m*m +5*m - 1, prefer m*m + 4*m + (m-1)*nb)
            p = 0;
            for (k = 0; k < m; k++) {
                for (j = 0; j < ldwork; j++, p++) {
                    array1[j][k] = work[il-1+p];
                }
            }
            work2 = new double[m];
            for (p = 0; p < m; p++) {
                work2[p] = work[itaup - 1 + p];
            }
            work3 = new double[Math.max(1, lwork-iwork+1)];
            dorgbr('P', m, m, m, array1, ldwork, work2, work3, lwork-iwork+1, info);
            p = 0;
            for (k = 0; k < m; k++) {
                for (j = 0; j < ldwork; j++, p++) {
                    work[il-1+p] = array1[j][k];
                }
            }
            for (p = 0; p < Math.max(1, lwork-iwork+1); p++) {
                work[iwork-1+p] = work3[p];
            }
            iwork = ie + m;
            
            // Perform bidiagonal QR iteration,
            // computing right singular vectors of L in work[il-1] and
            // multiplying B by transpose of left singular vectors
            // (Workspace: need m*m + m + bdspac)
            work2 = new double[m-1];
            for (p = 0; p < m-1; p++) {
                work2[p] = work[ie-1+p];
            }
            p = 0;
            for (k = 0; k < m; k++) {
                for (j = 0; j < ldwork; j++, p++) {
                    array1[j][k] = work[il-1+p];
                }
            }
            work3 = new double[4*m];
            dbdsqr('U', m, m, 0, nrhs, s, work2, array1, ldwork, A, lda, B, ldb, work3, info);
            for (p = 0; p < m-1; p++) {
                work[ie-1+p] = work2[p];
            }
            p = 0;
            for (k = 0; k < m; k++) {
                for (j = 0; j < ldwork; j++, p++) {
                    work[il-1+p] = array1[j][k];
                }
            }
            if (info[0] != 0) {
                Preferences.debug("dbdsqr had info[0] = " + info[0] + "\n");
                work[0] = maxwrk;
                return;
            }
            
            // Multiply B by reciprocals of singular values
            thr = Math.max(rcond*s[0], sfmin);
            if (rcond < 0.0) {
                thr = Math.max(eps*s[0], sfmin);
            }
            rank[0] = 0;
            for (i = 1; i <= m; i++) {
                if (s[i-1] > thr) {
                    work2 = new double[nrhs];
                    for (p = 0; p < nrhs; p++) {
                        work2[p] = B[i-1][p];
                    }
                    drscl(nrhs, s[i-1], work2, 1);
                    for (p = 0; p < nrhs; p++) {
                        B[i-1][p] = work2[p];
                    }
                    rank[0] = rank[0] + 1;
                } // if (s[i-1] > thr)
                else {
                    array1 = new double[1][nrhs];
                    for (p = 0; p < nrhs; p++) {
                        array1[0][p] = B[i-1][p];
                    }
                    ge.dlaset('F', 1, nrhs, 0.0, 0.0, array1, 1);
                    for (p = 0; p < nrhs; p++) {
                        B[i-1][p] = array1[0][p];
                    }
                } // else
            } // for (i = 1; i <= m; i++)
            iwork = ie;
            
            // Multiply B by right singular vectors of L in work[il-1]
            // (Workspace: need m*m + 2*m, prefer m*m + m + m*nrhs)
            
            if ((lwork >= ldb*nrhs+iwork-1) && (nrhs > 1)) {
                array1 = new double[ldwork][m]; 
                p = 0;
                for (k = 0; k < m; k++) {
                    for (j = 0; j < ldwork; j++, p++) {
                        array1[j][k] = work[il-1+p];
                    }
                }
                array2 = new double[ldb][nrhs];
                p = 0;
                for (k = 0; k < nrhs; k++) {
                    for (j = 0; j < ldb; j++, p++) {
                        array2[j][k] = work[iwork-1+p];
                    }
                }
                ge.dgemm('T', 'N', m, nrhs, m, 1.0, array1, ldwork, B, ldb, 0.0, array2, ldb);
                p = 0;
                for (k = 0; k < nrhs; k++) {
                    for (j = 0; j < ldb; j++, p++) {
                        work[iwork-1+p] = array2[j][k];
                    }
                }
                ge.dlacpy('G', m, nrhs, array2, ldb, B, ldb);
            } // if ((lwork >= ldb*nrhs+iwork-1) && (nrhs > 1))
            else if (nrhs > 1) {
                chunk = (lwork-iwork+1)/m;
                for (i = 1; i <= nrhs; i += chunk) {
                    bl = Math.min(nrhs-i+1, chunk);
                    array1 = new double[ldwork][m];
                    p = 0;
                    for (k = 0; k < m; k++) {
                        for (j = 0; j < ldwork; j++, p++) {
                            array1[j][k] = work[il-1+p];
                        }
                    }
                    array2 = new double[m][bl];
                    for (j = 0; j < m; j++) {
                        for (k = 0; k < bl; k++) {
                            array2[j][k] = B[j][i-1+k];
                        }
                    }
                    array3 = new double[m][bl];
                    ge.dgemm('T', 'N', m, bl, m, 1.0, array1, ldwork, array2, m, 0.0, array3, m);
                    p = 0;
                    for (k = 0; k < bl; k++) {
                        for (j = 0; j < m; j++, p++) {
                            work[iwork-1+p] = array3[j][k];
                        }
                    }
                    ge.dlacpy('G', m, bl, array3, m, array2, m);
                    for (j = 0; j < m; j++) {
                        for (k = 0; k < bl; k++) {
                            B[j][i-1+k] = array2[j][k];
                        }
                    }
                } // for (i = 1; i <= nrhs; i += chunk)
            } // else if (nrhs > 1)
            else {
                array1 = new double[ldwork][m];
                p = 0;
                for (k = 0; k < m; k++) {
                    for (j = 0; j < ldwork; j++, p++) {
                        array1[j][k] = work[il-1+p];
                    }
                }
                work2 = new double[m];
                for (p = 0; p < m; p++) {
                    work2[p] = B[p][0];
                }
                work3 = new double[m];
                ge.dgemv('T', m, m, 1.0, array1, ldwork, work2, 1, 0.0, work3, 1);
                for (p = 0; p < m; p++) {
                    work[iwork-1+p] = work3[p];
                }
                for (p = 0; p < m; p++) {
                    B[p][0] = work[iwork-1+p];
                }
            } // else
            
            // Zero out below first m rows of B
            row1 = Math.max(1, n-m);
            array1 = new double[row1][nrhs];
            for (j = 0; j < row1; j++) {
                for (k = 0; k < nrhs; k++) {
                    array1[j][k] = B[m+j][k];
                }
            }
            ge.dlaset('F', n-m, nrhs, 0.0, 0.0, array1, row1);
            for (j = 0; j < row1; j++) {
                for (k = 0; k < nrhs; k++) {
                    B[m+j][k] = array1[j][k];
                }
            }
            iwork = itau + m;
            
            // Multiply transpose(Q) by B
            // (Workspace: need m + nrhs, prefer m + nrhs*nb)
            work2 = new double[m];
            for (p = 0; p < m; p++) {
                work2[p] = work[itau-1+p];
            }
            work3 = new double[Math.max(1, lwork - iwork + 1)];
            dormlq('L', 'T', n, nrhs, m, A, lda, work2, B, ldb, work3, lwork-iwork+1, info);
            for (p = 0; p < Math.max(1, lwork-iwork+1); p++) {
                work[iwork-1+p] = work3[p];
            }
        } // else if ((n >= mnthr) && (lwork >= 4*m + m*m + Math.max(m, Math.max(2*m-4,
        else {
            // Path 2 - remaining underdetermined cases
            
            ie = 1;
            itauq = ie + m;
            itaup = itauq + m;
            iwork = itaup + m;
            
            // Bidiagonalize A
            // (Workspace: need 3*m + m, prefer 3*m+(m+n)*nb)
            
            work2 = new double[Math.min(m, n)];
            work3 = new double[Math.min(m, n)];
            work4 = new double[Math.max(1, lwork-iwork+1)];
            dgebrd(m, n, A, lda, s, work, work2, work3, work4, lwork-iwork+1, info);
            for (p = 0; p < Math.min(m, n); p++) {
                work[itauq-1+p] = work2[p];
                work[itaup-1+p] = work3[p];
            }
            for (p = 0; p < Math.max(1, lwork-iwork+1); p++) {
                work[iwork-1+p] = work4[p];    
            }
            
            // Multiply B by transpose of left bidiagonalizing vectors
            // (Workspace: need 3*m+nrhs, prefer 3*m+nrhs*nb)
            dormbr('Q', 'L', 'T', m, nrhs, n, A, lda, work2, B, ldb, work4, lwork-iwork+1, info);
            for (p = 0; p < Math.max(1, lwork-iwork+1); p++) {
                work[iwork-1+p] = work4[p];    
            }
            
            // Generate right bidiagonalizing vectors in A
            // (Workspace: need 4*m, prefer 3*m + m*nb)
            dorgbr('P', m, n, m, A, lda, work3, work4, lwork-iwork+1, info);
            for (p = 0; p < Math.max(1, lwork-iwork+1); p++) {
                work[iwork-1+p] = work4[p];    
            }
            iwork = ie + m;
            
            // Perform bidiagonal QR iteration.
            // computing right singular vectors of A in A and
            // multiplying B by transpose of left singular vectors
            // (Workspace: need bdspac)
            work2 = new double[4*m];
            dbdsqr('L', m, n, 0, nrhs, s, work, A, lda, vdum2, 1, B, ldb, work2, info);
            if (info[0] != 0) {
                Preferences.debug("dbdsqr had info[0] = " + info[0] + "\n");
                work[0] = maxwrk;
                return;
            }
            
            // Multiply B by reciprocals of singular values
            thr = Math.max(rcond*s[0], sfmin);
            if (rcond < 0.0) {
                thr = Math.max(eps*s[0], sfmin);
            }
            rank[0] = 0;
            for (i = 1; i <= m; i++) {
                if (s[i-1] > thr) {
                    work2 = new double[nrhs];
                    for (p = 0; p < nrhs; p++) {
                        work2[p] = B[i-1][p];
                    }
                    drscl(nrhs, s[i-1], work2, 1);
                    for (p = 0; p < nrhs; p++) {
                        B[i-1][p] = work2[p];
                    }
                    rank[0] = rank[0] + 1;
                }// if (s[i-1] > thr)
                else {
                    array1 = new double[1][nrhs];
                    for (p = 0; p < nrhs; p++) {
                        array1[0][p] = B[i-1][p];
                    }
                    ge.dlaset('F', 1, nrhs, 0.0, 0.0, array1, 1);
                    for (p = 0; p < nrhs; p++) {
                        B[i-1][p] = array1[0][p];
                    }
                } // else
            } // for (i = 1; i <= m; i++)
            
            // Multiply B by right singular vectors of A
            // (Workspace: need n, prefer n*nrhs)
            
            if ((lwork >= ldb*nrhs) && (nrhs > 1)) {
                array1 = new double[ldb][nrhs];
                ge.dgemm('T', 'N', n, nrhs, m, 1.0, A, lda, B, ldb, 0.0, array1, ldb); 
                p = 0;
                for (k = 0; k < nrhs; k++) {
                    for (j = 0; j < ldb; j++,p++) {
                        work[p] = array1[j][k];
                    }
                }
                ge.dlacpy('F', n, nrhs, array1, ldb, B, ldb);
            } // if ((lwork >= ldb*nrhs) && (nrhs > 1))
            else if (nrhs > 1) {
                chunk = lwork/n;
                for (i = 1; i <= nrhs; i += chunk) {
                    bl = Math.min(nrhs-i+1, chunk);
                    array1 = new double[m][bl];
                    for (j = 0; j < m; j++) {
                        for (k = 0; k < bl; k++) {
                            array1[j][k] = B[j][i-1+k];
                        }
                    }
                    array2 = new double[n][bl];
                    ge.dgemm('T', 'N', n, bl, m, 1.0, A, lda, array1, m, 0.0, array2, n);
                    p = 0;
                    for (k = 0; k < bl; k++) {
                        for (j = 0; j < n; j++,p++) {
                            work[p] = array2[j][k];
                        }
                    }
                    array1 = new double[n][bl];
                    ge.dlacpy('F', n, bl, array2, n, array1, n);
                    for (j = 0; j < n; j++) {
                        for (k = 0; k < bl; k++) {
                            B[j][i-1+k] = array1[j][k];
                        }
                    }
                } // for (i = 1; i <= nrhs; i += chunk)
            } // else if (nrhs > 1)
            else {
                work2 = new double[m];
                for (p = 0; p < m; p++) {
                    work2[p] = B[p][0];
                }
                ge.dgemv('T', m, n, 1.0, A, lda, work2, 1, 0.0, work, 1); 
                for (p = 0; p < n; p++) {
                    B[p][0] = work[p];
                }
            } // else
        } // else
        
        // Undo scaling
        if (iascl == 1) {
            ge.dlascl('G', 0, 0, anrm, smlnum[0], n, nrhs, B, ldb, info);
            array1 = new double[minmn][1];
            for (p = 0; p < minmn; p++) {
                array1[p][0] = s[p];
            }
            ge.dlascl('G', 0, 0, smlnum[0], anrm, minmn, 1, array1, minmn, info);
            for (p = 0; p < minmn; p++) {
                s[p] = array1[p][0];
            }
        } // if (iascl == 1)
        else if (iascl == 2) {
            ge.dlascl('G', 0, 0, anrm, bignum[0], n, nrhs, B, ldb, info);
            array1 = new double[minmn][1];
            for (p = 0; p < minmn; p++) {
                array1[p][0] = s[p];
            }
            ge.dlascl('G', 0, 0, bignum[0], anrm, minmn, 1, array1, minmn, info);
            for (p = 0; p < minmn; p++) {
                s[p] = array1[p][0];
            }
        } // else if (iascl == 2)
        if (ibscl == 1) {
            ge.dlascl('G', 0, 0, smlnum[0], bnrm, n, nrhs, B, ldb, info);
        }
        else if (ibscl == 2) {
            ge.dlascl('G', 0, 0, bignum[0], bnrm, n, nrhs, B, ldb, info);
        }
        
        work[0] = maxwrk;
        return;
    } // dgelss
    
    /** This is a section of BLAS routine DSWAP created by Jack Dongarra, 3/11/78.  Modified 12/3/93
     *  Interchanges tow vectors
     */
    private void dswap(int n, double dx[], int incx, double dy[], int incy) {
        int ix;
        int iy;
        double dtemp;
        int i;
        
        if (n <= 0) {
            return;
        }
        
        ix = 0;
        iy = 0;
        if (incx < 0) {
            ix = (-n+1)*incx;
        }
        if (incy < 0) {
            iy = (-n+1)*incy;
        }
        for (i = 1; i <= n; i++) {
            dtemp = dx[ix];
            dx[ix] = dy[iy];
            dy[iy] = dtemp;
            ix = ix + incx;
            iy = iy + incy;
        }
        return;
        
    } // dswap
    
     // ge.dlange
    
    /** This is a port of version 3.2 LAPACK routine DGELQF
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *     November 2006
    *
    *     .. Scalar Arguments ..
          INTEGER            INFO, LDA, LWORK, M, N
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DGELQF computes an LQ factorization of a real M-by-N matrix A:
    *  A = L * Q.
    *
    *  Arguments
    *  =========
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix A.  M >= 0.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix A.  N >= 0.
    *
    *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
    *          On entry, the M-by-N matrix A.
    *          On exit, the elements on and below the diagonal of the array
    *          contain the m-by-min(m,n) lower trapezoidal matrix L (L is
    *          lower triangular if m <= n); the elements above the diagonal,
    *          with the array TAU, represent the orthogonal matrix Q as a
    *          product of elementary reflectors (see Further Details).
    *
    *  LDA     (input) INTEGER
    *          The leading dimension of the array A.  LDA >= max(1,M).
    *
    *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
    *          The scalar factors of the elementary reflectors (see Further
    *          Details).
    *
    *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
    *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
    *
    *  LWORK   (input) INTEGER
    *          The dimension of the array WORK.  LWORK >= max(1,M).
    *          For optimum performance LWORK >= M*NB, where NB is the
    *          optimal blocksize.
    *
    *          If LWORK = -1, then a workspace query is assumed; the routine
    *          only calculates the optimal size of the WORK array, returns
    *          this value as the first entry of the WORK array, and no error
    *          message related to LWORK is issued by XERBLA.
    *
    *  INFO    (output) INTEGER
    *          = 0:  successful exit
    *          < 0:  if INFO = -i, the i-th argument had an illegal value
    *
    *  Further Details
    *  ===============
    *
    *  The matrix Q is represented as a product of elementary reflectors
    *
    *     Q = H(k) . . . H(2) H(1), where k = min(m,n).
    *
    *  Each H(i) has the form
    *
    *     H(i) = I - tau * v * v'
    *
    *  where tau is a real scalar, and v is a real vector with
    *  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
    *  and tau in TAU(i).
    */
    private void dgelqf(int m, int n, double A[][], int lda, double tau[], double work[],
                        int lwork, int info[]) {
        boolean lquery;
        int i;
        int ib;
        int iinfo[] = new int[1];
        int iws;
        int k;
        int ldwork;
        int lwkopt;
        int nb;
        int nbmin;
        int nx;
        String name;
        String opts;
        int row1;
        double array1[][];
        int p;
        int q;
        double v1[];
        double work2[][];
        double array2[][];
        int row2;
        double work3[][];
        
        // Test the input arguments
        
        info[0] = 0;
        name = new String("DGELQF");
        opts = new String(" ");
        nb = ge.ilaenv(1, name, opts, m, n, -1, -1);
        lwkopt = m*nb;
        work[0] = lwkopt;
        lquery = (lwork == -1);
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -4;
        }
        else if (lwork < Math.max(1, m) && (!lquery)) {
            info[0] = -7;
        }
        if (info[0] != 0) {
             MipavUtil.displayError("Error dgelqf had info[0] = " + info[0]);
             return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        k = Math.min(m, n);
        if (k == 0) {
            work[0] = 1;
            return;
        }
        
        nbmin = 2;
        nx = 0;
        iws = m;
        if ((nb > 1) && (nb < k)) {
            // Determine when to cross over from blocked to unblocked code.
            nx = Math.max(0, ge.ilaenv(3, name, opts, m, n, -1, -1));
            if (nx < k) {
                // Determine if workspace is large enough for blocked code.
                ldwork = m;
                iws = ldwork * nb;
                if (lwork < iws) {
                    //  Not enough workspace to used optimal nb:  reduce nb and
                    // determine the minimum value of nb;
                    nb = lwork/ldwork;
                    nbmin = Math.max(2, ge.ilaenv(2, name, opts, m, n, -1, -1));
                } // if (lwork < iws)
            } // if (nx < k)
        } // if ((nb > 1) && (nb < k))
        
        if ((nb >= nbmin) && (nb < k) && (nx < k)) {
            // Use blocked code initially
            for (i = 1; i <= k - nx; i += nb) {
                ib = Math.min(k-i+1, nb);
                // Compute the LQ factorization of the current block A(i:i+ib-1,i:n)
                row1 = Math.max(1, ib);
                array1 = new double[row1][n-i+1];
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n-i+1; q++) {
                        array1[p][q] = A[i-1+p][i-1+q];    
                    }
                }
                v1 = new double[Math.min(ib, n-i+1)];
                dgelq2(ib, n-i+1, array1, row1, v1, work, iinfo);
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n-i+1; q++) {
                        A[i-1+p][i-1+q] = array1[p][q];    
                    }
                }
                for (p = 0; p < Math.min(ib, n-i+1); p++) {
                    tau[i-1+p] = v1[p];
                }
                if (i+ib <= m) {
                    // Form the triangular factor of the block reflector
                    // H = H(i) H(i+1) ... H(i+ib-1)
                    v1 = new double[ib];
                    for (p = 0; p < ib; p++) {
                        v1[p] = tau[i-1+p];
                    }
                    work2 = new double[ib][ib];
                    ge.dlarft('F', 'R', n-i+1, ib, array1, ib, v1, work2, ib);
                    for (p = 0; p < ib; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            A[i-1+p][i-1+q] = array1[p][q];    
                        }
                    }
                    for (q = 0; q < ib; q++) {
                        for (p = 0; p < ib; p++) {
                            work[p + ib*q] = work2[p][q];
                        }
                    }
                    
                    // Apply H to A(i+ib:m, i:n) from the right
                    row2 = Math.max(1,m-i-ib+1);
                    array2 = new double[row2][n-i+1];
                    for (p = 0; p < row2; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            array2[p][q] = A[i+ib-1+p][i-1+q];
                        }
                    }
                    work3 = new double[row2][ib];
                    ge.dlarfb('R', 'N', 'F', 'R', m-i-ib+1, n-i+1, ib, array1, ib, work2, ib,
                            array2, row2, work3, row2);
                    for (p = 0; p < row2; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            A[i+ib-1+p][i-1+q] = array2[p][q];
                        }
                    }
                } // if (i+ib <= m)
            } // for (i = 1; i <= k - nx; i+= nb)
        } // if ((nb >= nbmin) && (nb < k) && (nx < k))
        else {
            i = 1;
        }
        
        // Use unblocked code to factor the last or only block
        if (i <= k) {
            row1 = Math.max(1, m-i+1);
            array1 = new double[row1][n-i+1];
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-i+1; q++) {
                    array1[p][q] = A[i-1+p][i-1+q];    
                }
            }
            v1 = new double[Math.min(m-i+1, n-i+1)];
            dgelq2(m-i+1, n-i+1, array1, row1, v1, work, iinfo);
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-i+1; q++) {
                    A[i-1+p][i-1+q] = array1[p][q];    
                }
            }
            for (p = 0; p < Math.min(m-i+1, n-i+1); p++) {
                tau[i-1+p] = v1[p];
            }
        } // if (i <= k)
        
        work[0] = iws;
        return;
    } // dgelqf

    
    /** This is a port of version 3.2 LAPACK routine DGELQ2
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *     November 2006
    *
    *     .. Scalar Arguments ..
          INTEGER            INFO, LDA, M, N
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DGELQ2 computes an LQ factorization of a real m by n matrix A:
    *  A = L * Q.
    *
    *  Arguments
    *  =========
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix A.  M >= 0.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix A.  N >= 0.
    *
    *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
    *          On entry, the m by n matrix A.
    *          On exit, the elements on and below the diagonal of the array
    *          contain the m by min(m,n) lower trapezoidal matrix L (L is
    *          lower triangular if m <= n); the elements above the diagonal,
    *          with the array TAU, represent the orthogonal matrix Q as a
    *          product of elementary reflectors (see Further Details).
    *
    *  LDA     (input) INTEGER
    *          The leading dimension of the array A.  LDA >= max(1,M).
    *
    *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
    *          The scalar factors of the elementary reflectors (see Further
    *          Details).
    *
    *  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
    *
    *  INFO    (output) INTEGER
    *          = 0: successful exit
    *          < 0: if INFO = -i, the i-th argument had an illegal value
    *
    *  Further Details
    *  ===============
    *
    *  The matrix Q is represented as a product of elementary reflectors
    *
    *     Q = H(k) . . . H(2) H(1), where k = min(m,n).
    *
    *  Each H(i) has the form
    *
    *     H(i) = I - tau * v * v'
    *
    *  where tau is a real scalar, and v is a real vector with
    *  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
    *  and tau in TAU(i).
    */
    private void dgelq2(int m, int n, double A[][], int lda, double tau[], double work[], int info[]) {
        int i;
        int k;
        double aii;
        double v1[] = new double[1];
        double v2[];
        double v3[] = new double[1];
        double array1[][];
        int j;
        int p;
        
        // Test the input arguments
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
            MipavUtil.displayError("Error dgelq2 had info[0] = " + info[0]);
            return;
        }
        
        k = Math.min(m, n);
        for (i = 1; i <= k; i++) {
            // Generate elementary reflector H(i) to annihilate A(i,i+1:n)
            v1[0] = A[i-1][i-1];
            v2 = new double[n-i];
            for (j = 0; j < n-i; j++) {
                v2[j] = A[i-1][Math.min(i,n-1)+j];
            }
            ge.dlarfp(n-i+1, v1, v2, 1, v3);
            A[i-1][i-1] = v1[0];
            for (j = 0; j < n-i; j++) {
                A[i-1][Math.min(i,n-1)+j] = v2[j];
            }
            tau[i-1] = v3[0];
            if (i < m) {
                // Apply H(i) to A(i+1:m,i:n) from the right
                aii = A[i-1][i-1];
                A[i-1][i-1] = 1.0;
                v2 = new double[n-i+1];
                for (j = 0; j < n-i+1; j++) {
                    v2[j] = A[i-1][i-1+j];
                }
                array1 = new double[m-i][n-i+1];
                for (j = 0; j < m-i; j++) {
                    for (p = 0; p < n-i+1; p++) {
                        array1[j][p] = A[i+j][i-1+p];
                    }
                }
                ge.dlarf('R', m-i, n-i+1, v2, 1, v3[0], array1, m-i, work);
                for (j = 0; j < m-i; j++) {
                    for (p = 0; p < n-i+1; p++) {
                        A[i+j][i-1+p] = array1[j][p];
                    }
                }
                A[i-1][i-1] = aii;
            } // if (i < m)
        } // for (i = 1; i <= k; i++)
        return;
    } // dgelq2

    
    /** This is a port of version 3.2 LAPACK routine DBDSQR.
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *     January 2007
    *
    *     .. Scalar Arguments ..
          CHARACTER          UPLO
          INTEGER            INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   C( LDC, * ), D( * ), E( * ), U( LDU, * ),
         $                   VT( LDVT, * ), WORK( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DBDSQR computes the singular values and, optionally, the right and/or
    *  left singular vectors from the singular value decomposition (SVD) of
    *  a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
    *  zero-shift QR algorithm.  The SVD of B has the form
    * 
    *     B = Q * S * P**T
    * 
    *  where S is the diagonal matrix of singular values, Q is an orthogonal
    *  matrix of left singular vectors, and P is an orthogonal matrix of
    *  right singular vectors.  If left singular vectors are requested, this
    *  subroutine actually returns U*Q instead of Q, and, if right singular
    *  vectors are requested, this subroutine returns P**T*VT instead of
    *  P**T, for given real input matrices U and VT.  When U and VT are the
    *  orthogonal matrices that reduce a general matrix A to bidiagonal
    *  form:  A = U*B*VT, as computed by DGEBRD, then
    *
    *     A = (U*Q) * S * (P**T*VT)
    *
    *  is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
    *  for a given real input matrix C.
    *
    *  See "Computing  Small Singular Values of Bidiagonal Matrices With
    *  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
    *  LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
    *  no. 5, pp. 873-912, Sept 1990) and
    *  "Accurate singular values and differential qd algorithms," by
    *  B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
    *  Department, University of California at Berkeley, July 1992
    *  for a detailed description of the algorithm.
    *
    *  Arguments
    *  =========
    *
    *  UPLO    (input) CHARACTER*1
    *          = 'U':  B is upper bidiagonal;
    *          = 'L':  B is lower bidiagonal.
    *
    *  N       (input) INTEGER
    *          The order of the matrix B.  N >= 0.
    *
    *  NCVT    (input) INTEGER
    *          The number of columns of the matrix VT. NCVT >= 0.
    *
    *  NRU     (input) INTEGER
    *          The number of rows of the matrix U. NRU >= 0.
    *
    *  NCC     (input) INTEGER
    *          The number of columns of the matrix C. NCC >= 0.
    *
    *  D       (input/output) DOUBLE PRECISION array, dimension (N)
    *          On entry, the n diagonal elements of the bidiagonal matrix B.
    *          On exit, if INFO=0, the singular values of B in decreasing
    *          order.
    *
    *  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
    *          On entry, the N-1 offdiagonal elements of the bidiagonal
    *          matrix B. 
    *          On exit, if INFO = 0, E is destroyed; if INFO > 0, D and E
    *          will contain the diagonal and superdiagonal elements of a
    *          bidiagonal matrix orthogonally equivalent to the one given
    *          as input.
    *
    *  VT      (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
    *          On entry, an N-by-NCVT matrix VT.
    *          On exit, VT is overwritten by P**T * VT.
    *          Not referenced if NCVT = 0.
    *
    *  LDVT    (input) INTEGER
    *          The leading dimension of the array VT.
    *          LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
    *
    *  U       (input/output) DOUBLE PRECISION array, dimension (LDU, N)
    *          On entry, an NRU-by-N matrix U.
    *          On exit, U is overwritten by U * Q.
    *          Not referenced if NRU = 0.
    *
    *  LDU     (input) INTEGER
    *          The leading dimension of the array U.  LDU >= max(1,NRU).
    *
    *  C       (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
    *          On entry, an N-by-NCC matrix C.
    *          On exit, C is overwritten by Q**T * C.
    *          Not referenced if NCC = 0.
    *
    *  LDC     (input) INTEGER
    *          The leading dimension of the array C.
    *          LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
    *
    *  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
    *
    *  INFO    (output) INTEGER
    *          = 0:  successful exit
    *          < 0:  If INFO = -i, the i-th argument had an illegal value
    *          > 0:
    *             if NCVT = NRU = NCC = 0,
    *                = 1, a split was marked by a positive value in E
    *                = 2, current block of Z not diagonalized after 30*N
    *                     iterations (in inner while loop)
    *                = 3, termination criterion of outer while loop not met 
    *                     (program created more than N unreduced blocks)
    *             else NCVT = NRU = NCC = 0,
    *                   the algorithm did not converge; D and E contain the
    *                   elements of a bidiagonal matrix which is orthogonally
    *                   similar to the input matrix B;  if INFO = i, i
    *                   elements of E have not converged to zero.
    *
    *  Internal Parameters
    *  ===================
    *
    *  TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))
    *          TOLMUL controls the convergence criterion of the QR loop.
    *          If it is positive, TOLMUL*EPS is the desired relative
    *             precision in the computed singular values.
    *          If it is negative, abs(TOLMUL*EPS*sigma_max) is the
    *             desired absolute accuracy in the computed singular
    *             values (corresponds to relative accuracy
    *             abs(TOLMUL*EPS) in the largest singular value.
    *          abs(TOLMUL) should be between 1 and 1/EPS, and preferably
    *             between 10 (for fast convergence) and .1/EPS
    *             (for there to be some accuracy in the results).
    *          Default is to lose at either one eighth or 2 of the
    *             available decimal digits in each computed singular value
    *             (whichever is smaller).
    *
    *  MAXITR  INTEGER, default = 6
    *          MAXITR controls the maximum number of passes of the
    *          algorithm through its inner loop. The algorithms stops
    *          (and so fails to converge) if the number of passes
    *          through the inner loop exceeds MAXITR*N**2.
    */
    private void dbdsqr(char uplo, int n, int ncvt, int nru, int ncc, double d[], double e[],
                        double VT[][], int ldvt, double U[][], int ldu, double C[][], int ldc,
                        double work[], int info[]) {
        int maxitr = 6;
        boolean lower;
        boolean rotate;
        int i;
        int idir;
        int isub;
        int iter;
        int j;
        int k;
        int p;
        int ll;
        int lll;
        int m;
        int maxit;
        int nm1;
        int nm12;
        int nm13;
        int oldll;
        int oldm;
        double abse;
        double abss;
        double cosl[] = new double[1];
        double cosr[] = new double[1];
        double cs[] = new double[1];
        double eps;
        double f;
        double g;
        double h;
        double mu;
        double oldcs[] = new double[1];
        double oldsn[] = new double[1];
        double r[] = new double[1];
        double shift[] = new double[1];
        double sigmn[] = new double[1];
        double sigmx[] = new double[1];
        double sinl[] = new double[1];
        double sinr[] = new double[1];
        double sll;
        double smax;
        double smin;
        double sminl;
        double sminoa;
        double sn[] = new double[1];
        double temp;
        double thresh;
        double tol;
        double tolmul;
        double unfl;
        double w1[];
        double w2[];
        double w3[];
        double array1[][];
        int row1;
        
        // Test the input parameters.
        info[0] = 0;
        lower = ((uplo == 'L') || (uplo == 'l'));
        if ((uplo != 'U') && (uplo != 'u') && (!lower)) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (ncvt < 0) {
            info[0] = -3;
        }
        else if (nru < 0) {
            info[0] = -4;
        }
        else if (ncc < 0) {
            info[0] = -5;
        }
        else if (((ncvt == 0) && (ldvt < 1)) || ((ncvt > 0) && (ldvt < Math.max(1,n)))) {
            info[0] = -9;
        }
        else if (ldu < Math.max(1, nru)) {
            info[0] = -11;
        }
        else if (((ncc == 0) && (ldc < 1)) || ((ncc > 0) && (ldc < Math.max(1,n)))) {
            info[0] = -13;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("Error dbdsqr had info[0] = " + info[0]);
            return;
        }
        if (n == 0) {
            return;
        }
        if (n != 1) {
            // Rotate is true if any singular vectors desired, false otherwise
            rotate = (ncvt > 0) || (nru > 0) || (ncc > 0);
            
            // If no singular vectors desired, use qd algorithm
            if (!rotate) {
                dlasq1(n, d, e, work, info);
                return;
            }
            
            nm1 = n - 1;
            nm12 = nm1 + nm1;
            nm13 = nm12 + nm1;
            idir = 0;
            
            // Get machine constants
            
            eps = ge.dlamch('E'); // Epsilon
            unfl = ge.dlamch('S'); // Safe minimum
            
            // If matrix lower bidiagonal, rotate to be upper bidiagonal
            // by applying Givens rotations on the left
            
            if (lower) {
                for (i = 1; i <= n-1; i++) {
                    ge.dlartg(d[i-1], e[i-1], cs, sn, r);  
                    d[i-1] = r[0];
                    e[i-1] = sn[0]*d[i];
                    d[i] = cs[0]*d[i];
                    work[i-1] = cs[0];
                    work[nm1+i-1] = sn[0];
                } // for (i = 1; i <= n-1; i++)
                
                // Update singular vectors if desired
                if (nru > 0) {
                    w1 = new double[n-1];
                    w2 = new double[n-1];
                    for (k = 0; k < n-1; k++) {
                        w1[k] = work[k];
                        w2[k] = work[n-1+k];
                    }
                    ge.dlasr('R', 'V', 'F', nru, n, w1, w2, U, ldu);
                } // if (nru > 0)
                
                if (ncc > 0) {
                    w1 = new double[n-1];
                    w2 = new double[n-1];
                    for (k = 0; k < n-1; k++) {
                        w1[k] = work[k];
                        w2[k] = work[n-1+k];
                    } 
                    ge.dlasr('L', 'V', 'F', n, ncc, w1, w2, C, ldc);
                } // if (ncc > 0)
            } // if (lower)
            
            // Compute singular values to relative accuracy tol
            // (By setting tol to be negative, algorithm will compute
            // singular values to absolute accuracy abs(tol)*norm(input matrix))
            
            tolmul = Math.max(10.0, Math.min(100.0, Math.pow(eps, -0.125)));
            tol = tolmul * eps;
            
            // Compute approximate maximum, minimum singular values
            
            smax = 0.0;
            for (i = 1; i <= n; i++) {
                smax = Math.max(smax, Math.abs(d[i-1]));
            }
            for (i = 1; i <= n-1; i++) {
                smax = Math.max(smax, Math.abs(e[i-1]));
            }
            sminl = 0.0;
            if (tol >= 0.0) {
                // Relative accuracy desired
                sminoa = Math.abs(d[0]);
                if (sminoa != 0.0) {
                    mu = sminoa;
                    for (i = 2; i <= n; i++) {
                        mu = Math.abs(d[i-1]) * (mu/(mu + Math.abs(e[i-2])));
                        sminoa = Math.min(sminoa, mu);
                        if (sminoa == 0.0) {
                            break;
                        }
                    } // for (i = 2; i <= n; i++)
                } // if (sminoa != 0.0)
                sminoa = sminoa/Math.sqrt((double)n);
                thresh = Math.max(tol * sminoa, maxitr * n * n * unfl);
            } // if (tol >= 0.0)
            else {
                // Absolute accuracy required
                thresh = Math.max(Math.abs(tol)*smax, maxitr * n * n * unfl);
            }
            
            // Prepare for main iteration loop for the singular values
            // (maxit is the maximum number of passes through the inner
            // loop permitted before nonconvergence signalled.)
            
            maxit = maxitr * n * n;
            iter = 0;
            oldll = -1;
            oldm = -1;
            
            // m points to the last element of unconverged part of matrix
            
            m = n;
            
            // Begin main iteration loop
            loop1: while (true) {
                // Check for convergence or exceeding iteration count
                if (m <= 1) {
                    break;
                }
                if (iter > maxit) {
                    info[0] = 0;
                    for (i = 1; i <= n-1; i++) {
                        if (e[i-1] != 0.0) {
                            info[0] = info[0] + 1;
                        }
                    }
                    return;
                } // if (iter > maxit)
                
                // Find diagonal block of matrix to work on
                
                if ((tol < 0.0) && (Math.abs(d[m-1]) <= thresh)) {
                    d[m-1] = 0.0;
                }
                smax = Math.abs(d[m-1]);
                smin = smax;
                loop2: {
                    for(lll = 1; lll <= m-1; lll++) {
                        ll = m - lll;
                        abss = Math.abs(d[ll-1]);
                        abse = Math.abs(e[ll-1]);
                        if ((tol < 0.0) && (abss <= thresh)) {
                            d[ll-1] = 0.0;
                        }
                        if (abse <= thresh) {
                            e[ll-1] = 0.0;
                            
                            // Matrix splits since e[ll-1] = 0.0
                            
                            if (ll == m-1) {
                                // Convergence of bottom singular value, return to top of loop1
                                m = m - 1;
                                continue loop1;
                            } // if (ll == m-1)
                            break loop2;
                        } // if (abse <= thresh)
                        smin = Math.min(smin, abss);
                        smax = Math.max(smax, Math.max(abss, abse));
                    } // for(lll = 1; lll <= m-1; lll++)
                    ll = 0;
                } // loop2:
                
                ll = ll + 1;
                
                // e[ll-1] through e[m-2] are nonzero, e[ll-2] is zero.
                
                if (ll == m-1) {
                    // 2 by 2 block, handle separately
                    ge.dlasv2(d[m-2], e[m-2], d[m-1], sigmn, sigmx, sinr, cosr, sinl, cosl);
                    d[m-2] = sigmx[0];
                    e[m-2] = 0.0;
                    d[m-1] = sigmn[0];
                    
                    // Compute singular vectors if desired
                    if (ncvt > 0) {
                        w1 = new double[ncvt];
                        w2 = new double[ncvt];
                        for (k = 0; k < ncvt; k++) {
                            w1[k] = VT[m-2][k];
                            w2[k] = VT[m-1][k];
                        }
                        ge.drot(ncvt, w1, 1, w2, 1, cosr[0], sinr[0]);
                        for (k = 0; k < ncvt; k++) {
                            VT[m-2][k] = w1[k];
                            VT[m-1][k] = w2[k];
                        }
                    } // if (ncvt > 0)
                    
                    if (nru > 0) {
                        w1 = new double[nru];
                        w2 = new double[nru];
                        for (k = 0; k < nru; k++) {
                            w1[k] = U[k][m-2];
                            w2[k] = U[k][m-1];
                        }
                        ge.drot(nru, w1, 1, w2, 1, cosl[0], sinl[0]);
                        for (k = 0; k < nru; k++) {
                            U[k][m-2] = w1[k];
                            U[k][m-1] = w2[k];
                        }    
                    } // if (nru > 0)
                    
                    if (ncc > 0) {
                        w1 = new double[ncc];
                        w2 = new double[ncc];
                        for (k = 0; k < ncc; k++) {
                            w1[k] = C[m-2][k];
                            w2[k] = C[m-1][k];
                        }
                        ge.drot(ncc, w1, 1, w2, 1, cosl[0], sinl[0]);
                        for (k = 0; k < ncc; k++) {
                            C[m-2][k] = w1[k];
                            C[m-1][k] = w2[k];
                        }    
                    } // if (ncc > 0)
                    
                    m = m-2;
                    continue loop1;
                } // if (ll == m-1)
                
                // If working on new submatrix, choose shift direction
                // (from larger end diagonal element towards smaller)
                
                if ((ll > oldm) || (m < oldll)) {
                    if (Math.abs(d[ll-1]) >= Math.abs(d[m-1])) {
                        // Chase bulge from top (big end) to bottom (small end)
                        idir = 1;
                    }
                    else {
                        // Chase bulge from bottom (big end) to top (small end)
                        idir = 2;
                    }
                } // if ((ll > oldm) || (m < oldll))
                
                // Apply convergence tests
                
                if (idir == 1) {
                    // Run convergence test in forward direction
                    // First apply standard test to bottom of matrix
                    
                    if ((Math.abs(e[m-2]) <= Math.abs(tol)*Math.abs(d[m-1])) ||
                        ((tol < 0.0) && (Math.abs(e[m-2]) <= thresh))) {
                        e[m-2] = 0.0;
                        continue loop1;
                    }
                    
                    if (tol >= 0.0) {
                        // If relative accuracy desired,
                        // apply convergence criterion forward
                        mu = Math.abs(d[ll-1]);
                        sminl = mu;
                        for (lll = ll; lll <= m-1; lll++) {
                            if (Math.abs(e[lll-1]) <= tol*mu) {
                                e[lll-1] = 0.0;
                                continue loop1;
                            }
                            mu = Math.abs(d[lll]) * (mu/(mu + Math.abs(e[lll-1])));
                            sminl = Math.min(sminl, mu);
                        } // for (lll = ll; lll <= m-1; lll++)
                    } // if (tol >= 0.0)
                } // if (idir == 1)
                else { // idir == 2
                    // Run convergence test in backward direction
                    // First apply standard test to top of matrix
                    if ((Math.abs(e[ll-1]) <= Math.abs(tol) * Math.abs(d[ll-1])) || 
                        ((tol < 0.0) && (Math.abs(e[ll-1]) <= thresh))) {
                        e[ll-1] = 0.0;
                        continue loop1;
                    }
                    
                    if (tol >= 0.0) {
                        // If relative accuracy desired,
                        // apply convergence criterion backward
                        mu = Math.abs(d[m-1]);
                        sminl = mu;
                        for (lll = m - 1; lll >= ll; lll--) {
                            if (Math.abs(e[lll-1]) <= tol*mu) {
                                e[lll-1] = 0.0;
                                continue loop1;
                            }
                            mu = Math.abs(d[lll-1]) * (mu/ (mu + Math.abs(e[lll-1])));
                            sminl = Math.min(sminl, mu);
                        } // for (lll = m - 1; lll >= ll; lll--)
                    } // if (tol >= 0.0)
                } // else idir == 2
                oldll = ll;
                oldm = m;
                
                // Compute shift.  First, test if shifting would ruin relative
                // accuracy, and if so set the shift to zero.
                
                if ((tol >= 0.0) && (n*tol*(sminl/smax) <= Math.max(eps, 0.01*tol))) {
                    // Use a zero shift to avoid loss of relative accuracy
                    shift[0] = 0;
                }
                else {
                    // Compute the shift from 2-by-2 block at end of matrix
                    w1 = new double[1];
                    w2 = new double[1];
                    w3 = new double[1];
                    if (idir == 1) {
                        sll = Math.abs(d[ll-1]);
                        w1[0] = d[m-2];
                        w2[0] = e[m-2];
                        w3[0] = d[m-1];
                        dlas2(w1, w2, w3, shift, r);
                    }
                    else {
                        sll = Math.abs(d[m-1]);
                        w1[0] = d[ll-1];
                        w2[0] = e[ll-1];
                        w3[0] = d[ll];
                        dlas2(w1, w2, w3, shift, r);
                    }
                    
                    // Test if shift negligible, and if so set to zero
                    if (sll > 0.0) {
                        temp = shift[0]/sll;
                        if (temp*temp < eps) {
                            shift[0] = 0.0;
                        }
                    } // if (sll > 0.0)
                } // else
                
                // Increment iteration count
                iter = iter + m - ll;
                
                // If shift[0] = 0, do simplified QR iteration
                if (shift[0] == 0) {
                    if (idir == 1) {
                        // Chase bulge from top to bottom
                        // Save cosines and sines for later singular vector updates
                        
                        cs[0] = 1.0;
                        oldcs[0] = 1.0;
                        w1 = new double[1];
                        for (i = ll; i <= m-1; i++) {
                            ge.dlartg(d[i-1]*cs[0], e[i-1], cs, sn, r);
                            if (i > ll) {
                                e[i-2] = oldsn[0] * r[0];
                            }
                            ge.dlartg(oldcs[0]*r[0], d[i]*sn[0], oldcs, oldsn, w1);
                            d[i-1] = w1[0];
                            work[i-ll] = cs[0];
                            work[i-ll+nm1] = sn[0];
                            work[i-ll+nm12] = oldcs[0];
                            work[i-ll+nm13] = oldsn[0];
                        } // for (i = ll; i <= m-1; i++)
                        h = d[m-1] * cs[0];
                        d[m-1] = h * oldcs[0];
                        e[m-2] = h * oldsn[0];
                        
                        // Update singular vectors
                        if (ncvt > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[k];
                                w2[k] = work[n-1+k];
                            }
                            row1 = Math.max(1, m-ll+1);
                            array1 = new double[row1][ncvt];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncvt; p++) {
                                    array1[k][p] = VT[ll-1+k][p];
                                }
                            }
                            ge.dlasr('L', 'V', 'F', m-ll+1, ncvt, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncvt; p++) {
                                    VT[ll-1+k][p] = array1[k][p];
                                }
                            }
                        } // if (ncvt > 0)
                        
                        if (nru > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[k+nm12];
                                w2[k] = work[k+nm13];
                            }
                            row1 = Math.max(1, nru);
                            array1 = new double[row1][m-ll+1];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < m-ll+1; p++) {
                                    array1[k][p] = U[k][ll-1+p];
                                }
                            }
                            ge.dlasr('R', 'V', 'F', nru, m-ll+1, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < m-ll+1; p++) {
                                    U[k][ll-1+p] = array1[k][p];
                                }
                            }    
                        } // if (nru > 0)
                        
                        if (ncc > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[k+nm12];
                                w2[k] = work[k+nm13];
                            }
                            row1 = Math.max(1, m-ll+1);
                            array1 = new double[row1][ncc];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncc; p++) {
                                    array1[k][p] = C[ll-1+k][p];
                                }
                            }
                            ge.dlasr('L', 'V', 'F', m-ll+1, ncc, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncc; p++) {
                                    C[ll-1+k][p] = array1[k][p];
                                }
                            }    
                        } // if (ncc > 0)
                        
                        // Test convergence
                        if (Math.abs(e[m-2]) <= thresh) {
                            e[m-2] = 0.0;
                        }
                    } // if (idir == 1)
                    else { // idir == 2
                        // Chase bulge from bottom to top
                        // Save cosines and sines for later singular vector updates
                        cs[0] = 1.0;
                        oldcs[0] = 1.0;
                        w1 = new double[1];
                        for (i = m; i >= ll + 1; i--) {
                            ge.dlartg(d[i-1]*cs[0], e[i-2], cs, sn, r);
                            if (i < m) {
                                e[i-1] = oldsn[0]*r[0];
                            }
                            ge.dlartg(oldcs[0]*r[0], d[i-2]*sn[0], oldcs, oldsn, w1);
                            d[i-1] = w1[0];
                            work[i-ll-1] = cs[0];
                            work[i-ll+nm1-1] = -sn[0];
                            work[i-ll+nm12-1] = oldcs[0];
                            work[i-ll+nm13-1] = -oldsn[0];
                        } // for (i = m; i >= ll + 1; i--)
                        h = d[ll-1]*cs[0];
                        d[ll-1] = h * oldcs[0];
                        e[ll-1] = h * oldsn[0];
                        
                        // Update singular vectors
                        if (ncvt > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[nm12 + k];
                                w2[k] = work[nm13+k];
                            }
                            row1 = Math.max(1, m-ll+1);
                            array1 = new double[row1][ncvt];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncvt; p++) {
                                    array1[k][p] = VT[ll-1+k][p];
                                }
                            }
                            ge.dlasr('L', 'V', 'B', m-ll+1, ncvt, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncvt; p++) {
                                    VT[ll-1+k][p] = array1[k][p];
                                }
                            }
                        } // if (ncvt > 0)
                        
                        if (nru > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[k];
                                w2[k] = work[n-1+k];
                            }
                            row1 = Math.max(1, nru);
                            array1 = new double[row1][m-ll+1];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < m-ll+1; p++) {
                                    array1[k][p] = U[k][ll-1+p];
                                }
                            }
                            ge.dlasr('R', 'V', 'B', nru, m-ll+1, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < m-ll+1; p++) {
                                    U[k][ll-1+p] = array1[k][p];
                                }
                            }    
                        } // if (nru > 0)
                        
                        if (ncc > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[k];
                                w2[k] = work[n-1+k];
                            }
                            row1 = Math.max(1, m-ll+1);
                            array1 = new double[row1][ncc];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncc; p++) {
                                    array1[k][p] = C[ll-1+k][p];
                                }
                            }
                            ge.dlasr('L', 'V', 'B', m-ll+1, ncc, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncc; p++) {
                                    C[ll-1+k][p] = array1[k][p];
                                }
                            }    
                        } // if (ncc > 0)
                        
                        // Test convergence
                        if (Math.abs(e[ll-1]) <= thresh) {
                            e[ll-1] = 0.0;
                        }
                    } // else idir == 2
                } // if (shift[0] == 0)
                else { // shift[0] != 0
                    // Use nonzero shift
                    
                    if (idir == 1) {
                        // Chase bulge from top to bottom
                        // Save cosines and sines for later singular vector updates
                        if (d[ll-1] >= 0) {
                            f = (Math.abs(d[ll-1]) - shift[0]) * (1.0 + shift[0]/d[ll-1]);
                        }
                        else {
                            f = (Math.abs(d[ll-1]) - shift[0]) * (-1.0 + shift[0]/d[ll-1]);
                        }
                        g = e[ll-1];
                        for (i = ll; i <= m-1; i++) {
                            ge.dlartg(f, g, cosr, sinr, r);
                            if (i > ll) {
                                e[i-2] = r[0];
                            }
                            f = cosr[0]*d[i-1] + sinr[0]*e[i-1];
                            e[i-1] = cosr[0]*e[i-1] - sinr[0]*d[i-1];
                            g = sinr[0]*d[i];
                            d[i] = cosr[0]*d[i];
                            ge.dlartg(f, g, cosl, sinl, r);
                            d[i-1] = r[0];
                            f = cosl[0]*e[i-1] + sinl[0]*d[i];
                            d[i] = cosl[0]*d[i] - sinl[0]*e[i-1];
                            if (i < m-1) {
                                g = sinl[0]*e[i];
                                e[i] = cosl[0]*e[i];
                            }
                            work[i-ll] = cosr[0];
                            work[i-ll+nm1] = sinr[0];
                            work[i-ll+nm12] = cosl[0];
                            work[i-ll+nm13] = sinl[0];
                        } // for (i = ll; i <= m-1; i++)
                        e[m-2] = f;
                        
                        // Update singular vectors
                        if (ncvt > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[k];
                                w2[k] = work[n-1+k];
                            }
                            row1 = Math.max(1, m-ll+1);
                            array1 = new double[row1][ncvt];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncvt; p++) {
                                    array1[k][p] = VT[ll-1+k][p];
                                }
                            }
                            ge.dlasr('L', 'V', 'F', m-ll+1, ncvt, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncvt; p++) {
                                    VT[ll-1+k][p] = array1[k][p];
                                }
                            }
                        } // if (ncvt > 0)
                        
                        if (nru > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[nm12+k];
                                w2[k] = work[nm13+k];
                            }
                            row1 = Math.max(1, nru);
                            array1 = new double[row1][m-ll+1];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < m-ll+1; p++) {
                                    array1[k][p] = U[k][ll-1+p];
                                }
                            }
                            ge.dlasr('R', 'V', 'F', nru, m-ll+1, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < m-ll+1; p++) {
                                    U[k][ll-1+p] = array1[k][p];
                                }
                            }    
                        } // if (nru > 0)
                        
                        if (ncc > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[nm12+k];
                                w2[k] = work[nm13+k];
                            }
                            row1 = Math.max(1, m-ll+1);
                            array1 = new double[row1][ncc];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncc; p++) {
                                    array1[k][p] = C[ll-1+k][p];
                                }
                            }
                            ge.dlasr('L', 'V', 'F', m-ll+1, ncc, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncc; p++) {
                                    C[ll-1+k][p] = array1[k][p];
                                }
                            }    
                        } // if (ncc > 0)
                        
                        // Test convergence
                        if (Math.abs(e[m-2]) <= thresh) {
                            e[m-2] = 0.0;
                        }
                    } // if (idir == 1)
                    else { // idir == 2
                        // Chase bulge from bottom to top
                        // Save cosines and sines for later singular vector updates
                        if (d[m-1] >= 0.0) {
                            f = (Math.abs(d[m-1]) - shift[0]) * (1.0 + shift[0]/d[m-1]);
                        }
                        else {
                            f = (Math.abs(d[m-1]) - shift[0]) * (-1.0 + shift[0]/d[m-1]);
                        }
                        g = e[m-2];
                        for (i = m; i >= ll + 1; i--) {
                            ge.dlartg(f, g, cosr, sinr, r);
                            if (i < m) {
                                e[i-1] = r[0];
                            }
                            f = cosr[0]*d[i-1] + sinr[0]*e[i-2];
                            e[i-2] = cosr[0]*e[i-2] - sinr[0]*d[i-1];
                            g = sinr[0]*d[i-2];
                            d[i-2] = cosr[0] * d[i-2];
                            ge.dlartg(f, g, cosl, sinl, r);
                            d[i-1] = r[0];
                            f = cosl[0]*e[i-2] + sinl[0]*d[i-2];
                            d[i-2] = cosl[0]*d[i-2] - sinl[0]*e[i-2];
                            if (i > ll+1) {
                                g = sinl[0]*e[i-3];
                                e[i-3] = cosl[0]*e[i-3];
                            }
                            work[i-ll-1] = cosr[0];
                            work[i-ll+nm1-1] = -sinr[0];
                            work[i-ll+nm12-1] = cosl[0];
                            work[i-ll+nm13-1] = -sinl[0];
                        } // for (i = m;i >= ll + 1; i--)
                        e[ll-1] = f;
                        
                        // Test convergence
                        if (Math.abs(e[ll-1]) <= thresh) {
                            e[ll-1] = 0.0;
                        }
                        
                        // Update singular vectors if desired
                        
                        if (ncvt > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[k+nm12];
                                w2[k] = work[k+nm13];
                            }
                            row1 = Math.max(1, m-ll+1);
                            array1 = new double[row1][ncvt];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncvt; p++) {
                                    array1[k][p] = VT[ll-1+k][p];
                                }
                            }
                            ge.dlasr('L', 'V', 'B', m-ll+1, ncvt, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncvt; p++) {
                                    VT[ll-1+k][p] = array1[k][p];
                                }
                            }
                        } // if (ncvt > 0)
                        
                        if (nru > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[k];
                                w2[k] = work[k+n-1];
                            }
                            row1 = Math.max(1, nru);
                            array1 = new double[row1][m-ll+1];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < m-ll+1; p++) {
                                    array1[k][p] = U[k][ll-1+p];
                                }
                            }
                            ge.dlasr('R', 'V', 'B', nru, m-ll+1, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < m-ll+1; p++) {
                                    U[k][ll-1+p] = array1[k][p];
                                }
                            }    
                        } // if (nru > 0)
                        
                        if (ncc > 0) {
                            w1 = new double[m-ll];
                            w2 = new double[m-ll];
                            for (k = 0; k < m-ll; k++) {
                                w1[k] = work[k];
                                w2[k] = work[k+n-1];
                            }
                            row1 = Math.max(1, m-ll+1);
                            array1 = new double[row1][ncc];
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncc; p++) {
                                    array1[k][p] = C[ll-1+k][p];
                                }
                            }
                            ge.dlasr('L', 'V', 'B', m-ll+1, ncc, w1, w2, array1, row1);
                            for (k = 0; k < row1; k++) {
                                for (p = 0; p < ncc; p++) {
                                    C[ll-1+k][p] = array1[k][p];
                                }
                            }    
                        } // if (ncc > 0)
                    } // else idir == 2
                } // else shift[0] != 0
                
                // QR iteration finished, go back and check convergence
            } // loop1: while(true)
        } // if (n != 1)
        
        // All singular values converged, so make them positive
        for (i = 1; i <= n; i++) {
            if (d[i-1] < 0.0) {
                d[i-1] = -d[i-1];
                
                // Change sign of singular vectors, if desired
                if (ncvt > 0) {
                    for (k = 0; k < ncvt; k++) {
                        VT[i-1][k] = -1.0 * VT[i-1][k];
                    }
                } // if (ncvt > 0)
            } // if (d[i-1] < 0.0)
        } // for (i = 1; i <= n; i++)
        
        // Sort the singular values into decreasing order (insertion sort on
        // singular values, but only one transposition per singular vector)
        for (i = 1; i <= n-1; i++) {
            // Scan for the smallest d[i-1]
            
            isub = 1;
            smin = d[0];
            for (j = 2; j <= n+1-i; j++) {
                if (d[j-1] <= smin) {
                    isub = j;
                    smin = d[j-1];
                }
            } // for (j = 2; j <= n+1-i; j++)
            if (isub != n+i-1) {
                // Swap singular values and vectors
                d[isub-1] = d[n-i];
                d[n-i] = smin;
                if (ncvt > 0) {
                    for (k = 0; k < ncvt; k++) {
                        temp = VT[isub-1][k];
                        VT[isub-1][k] = VT[n-i][k];
                        VT[n-i][k] = temp;
                    }
                } // if (ncvt > 0)
                
                if (nru > 0) {
                    for (k = 0; k < nru; k++) {
                        temp = U[k][isub-1];
                        U[k][isub-1] = U[k][n-i];
                        U[k][n-i] = temp;
                    }
                } // if (nru > 0)
                
                if (ncc > 0) {
                    for (k = 0; k < ncc; k++) {
                        temp = C[isub-1][k];
                        C[isub-1][k] = C[n-i][k];
                        C[n-i][k] = temp;
                    }
                } // if (ncc > 0)
            } // if (isub != n+i-1)
        } // for (i = 1; i <= n-1; i++)
        return;
    } // dbdsqr

    
   // ge.ge.dlasrt
    
    /** This is a port of version 3.2 LAPACK routine DLASQ1.                                 --
    *
    *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
    *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
    *  -- Berkeley                                                        --
    *  -- November 2008                                                   --
    *
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *
    *     .. Scalar Arguments ..
          INTEGER            INFO, N
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   D( * ), E( * ), WORK( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DLASQ1 computes the singular values of a real N-by-N bidiagonal
    *  matrix with diagonal D and off-diagonal E. The singular values
    *  are computed to high relative accuracy, in the absence of
    *  denormalization, underflow and overflow. The algorithm was first
    *  presented in
    *
    *  "Accurate singular values and differential qd algorithms" by K. V.
    *  Fernando and B. N. Parlett, Numer. Math., Vol-67, No. 2, pp. 191-230,
    *  1994,
    *
    *  and the present implementation is described in "An implementation of
    *  the dqds Algorithm (Positive Case)", LAPACK Working Note.
    *
    *  Arguments
    *  =========
    *
    *  N     (input) INTEGER
    *        The number of rows and columns in the matrix. N >= 0.
    *
    *  D     (input/output) DOUBLE PRECISION array, dimension (N)
    *        On entry, D contains the diagonal elements of the
    *        bidiagonal matrix whose SVD is desired. On normal exit,
    *        D contains the singular values in decreasing order.
    *
    *  E     (input/output) DOUBLE PRECISION array, dimension (N)
    *        On entry, elements E(1:N-1) contain the off-diagonal elements
    *        of the bidiagonal matrix whose SVD is desired.
    *        On exit, E is overwritten.
    *
    *  WORK  (workspace) DOUBLE PRECISION array, dimension (4*N)
    *
    *  INFO  (output) INTEGER
    *        = 0: successful exit
    *        < 0: if INFO = -i, the i-th argument had an illegal value
    *        > 0: the algorithm failed
    *             = 1, a split was marked by a positive value in E
    *             = 2, current block of Z not diagonalized after 30*N
    *                  iterations (in inner while loop)
    *             = 3, termination criterion of outer while loop not met 
    *                  (program created more than N unreduced blocks)
    */
    private void dlasq1(int n, double d[], double e[], double work[], int info[]) {
        int i;
        int iinfo[] = new int[1];
        double eps;
        double scale;
        double safmin;
        double sigmn[] = new double[1];
        double sigmx[] = new double[1];
        double d0[] = new double[1];
        double e0[] = new double[1];
        double d1[] = new double[1];
        double work1[][] = new double[2*n-1][1];
        double dmat[][] = new double[d.length][1];
        
        info[0] = 0;
        if (n < 0) {
            info[0] = -2;
            MipavUtil.displayError("Error dlasq1 had n < 0");
            return;
        }
        else if (n == 0) {
            return;
        }
        else if (n == 1) {
            d[0] = Math.abs(d[0]);
            return;
        }
        else if (n == 2) {
            d0[0] = d[0];
            e0[0] = e[0];
            d1[0] = d[1];
            dlas2(d0, e0, d1, sigmn, sigmx);
            d[0] = sigmx[0];
            d[1] = sigmn[0];
            return;
        }
        
        // Estimate the largest singular value.
        sigmx[0] = 0.0;
        for (i = 1; i <= n-1; i++) {
            d[i-1] = Math.abs(d[i-1]);
            sigmx[0] = Math.max(sigmx[0], Math.abs(e[i-1]));
        }
        d[n-1] = Math.abs(d[n-1]);
        
        // Early return if simgx[0] is zero (matrix is already diagonal).
        if (sigmx[0] == 0) {
            ge.ge.dlasrt('D', n, d, iinfo);
            return;
        }
        
        for (i = 1; i <= n; i++) {
            sigmx[0] = Math.max(sigmx[0], d[i-1]);
        }
        
        // Copy d and e int work (in the Z format) and scale (squaring the
        // input data makes scaling by a power of the radix pointless).
        eps = ge.dlamch('P'); // Precision
        safmin = ge.dlamch('S'); // Safe minimum
        scale = Math.sqrt(eps/safmin);
        for (i = 0; i < n; i++) {
            work1[2*i][0] = d[i];
        }
        for (i = 0; i < n-1; i++) {
            work1[2*i + 1][0] = e[i];
        }
        ge.dlascl('G', 0, 0, sigmx[0], scale, 2*n-1, 1, work1, 2*n-1, iinfo);
        for (i = 0; i < 2*n-1; i++) {
            work[i] = work1[i][0];
        }
        
        // Compute the q's and e's
        
        for (i = 1; i <= 2*n - 1; i++) {
            work[i-1] = work[i-1] * work[i-1];
        }
        work[2*n-1] = 0.0;
        
        
        dlasq2(n, work, info);
        if (info[0] != 0) {
       	 Preferences.debug("dlasq2 returned info[0] = " + info[0] + "\n");
        }
        
        if (info[0] == 0) {
            for (i = 1; i <= n; i++) {
                dmat[i-1][0] = Math.sqrt(work[i-1]);
            }
            ge.dlascl('G', 0, 0, scale, sigmx[0], n, 1, dmat, n, iinfo);
            for (i = 0; i < n; i++) {
                d[i] = dmat[i][0];
            }
        } // if (info[0] == 0)
        
        return;
    } // dlasq1

    
    /** This is a port of version 3.2 LAPACK routine DLASQ2.
    *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
    *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
    *  -- Berkeley                                                        --
    *  -- November 2008                                                   --
    *
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *
    *     .. Scalar Arguments ..
          INTEGER            INFO, N
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   Z( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DLASQ2 computes all the eigenvalues of the symmetric positive 
    *  definite tridiagonal matrix associated with the qd array Z to high
    *  relative accuracy are computed to high relative accuracy, in the
    *  absence of denormalization, underflow and overflow.
    *
    *  To see the relation of Z to the tridiagonal matrix, let L be a
    *  unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
    *  let U be an upper bidiagonal matrix with 1's above and diagonal
    *  Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
    *  symmetric tridiagonal to which it is similar.
    *
    *  Note : DLASQ2 defines a logical variable, IEEE, which is true
    *  on machines which follow ieee-754 floating-point standard in their
    *  handling of infinities and NaNs, and false otherwise. This variable
    *  is passed to DLASQ3.
    *
    *  Arguments
    *  =========
    *
    *  N     (input) INTEGER
    *        The number of rows and columns in the matrix. N >= 0.
    *
    *  Z     (input/output) DOUBLE PRECISION array, dimension ( 4*N )
    *        On entry Z holds the qd array. On exit, entries 1 to N hold
    *        the eigenvalues in decreasing order, Z( 2*N+1 ) holds the
    *        trace, and Z( 2*N+2 ) holds the sum of the eigenvalues. If
    *        N > 2, then Z( 2*N+3 ) holds the iteration count, Z( 2*N+4 )
    *        holds NDIVS/NIN^2, and Z( 2*N+5 ) holds the percentage of
    *        shifts that failed.
    *
    *  INFO  (output) INTEGER
    *        = 0: successful exit
    *        < 0: if the i-th argument is a scalar and had an illegal
    *             value, then INFO = -i, if the i-th argument is an
    *             array and the j-entry had an illegal value, then
    *             INFO = -(i*100+j)
    *        > 0: the algorithm failed
    *              = 1, a split was marked by a positive value in E
    *              = 2, current block of Z not diagonalized after 30*N
    *                   iterations (in inner while loop)
    *              = 3, termination criterion of outer while loop not met 
    *                   (program created more than N unreduced blocks)
    *                   

    *
    *  Further Details
    *  ===============
    *  Local Variables: I0:N0 defines a current unreduced segment of Z.
    *  The shifts are accumulated in SIGMA. Iteration count is in ITER.
    *  Ping-pong is controlled by PP (alternates between 0 and 1).
    */
    private void dlasq2(int n, double z[], int info[]) {
        double cbias = 1.50;
        boolean ieee;
        int i0[] = new int[1];
        int i4;
        int iinfo[] = new int[1];
        int ipn4;
        int iter[] = new int[1];
        int iwhila;
        int iwhilb;
        int k;
        int kmin;
        int n0[] = new int[1];
        int nbig;
        int ndiv[] = new int[1];
        int nfail[] = new int[1];
        int pp[] = new int[1];
        int splt;
        int ttype[] = new int[1];
        double d;
        double dee;
        double deemin;
        double desig[] = new double[1];
        double dmin[] = new double[1];
        double dmin1[] = new double[1];
        double dmin2[] = new double[1];
        double dn[] = new double[1];
        double dn1[] = new double[1];
        double dn2[] = new double[1];
        double e;
        double emax;
        double emin;
        double eps;
        double g[] = new double[1];
        double oldemn;
        double qmax;
        double qmin;
        double s;
        double safmin;
        double sigma[] = new double[1];
        double t;
        double tau[] = new double[1];
        double temp;
        double tol;
        double tol2;
        double trace;
        double zmax;
        String name;
        String opts;
        
        // Test the input arguments
        // (in case dlasq2 is not called by dlasq1)
        info[0] = 0;
        eps = ge.dlamch('P'); // precision
        safmin = ge.dlamch('S'); // Safe minimum
        tol = 100 * eps;
        tol2 = tol * tol;
        
        if (n < 0) {
            info[0] = -1;
            MipavUtil.displayError("Error dlasq2 had n < 0");
            return;
        }
        else if (n == 0) {
            return;
        }
        else if (n == 1) {
            // 1-by-1 case
            if (z[0] < 0.0) {
                info[0] = -201;
                MipavUtil.displayError("Error dlsaq2 had z[0] < 0.0");
            }
            return;
        } // else if (n == 1)
        else if (n == 2) {
            // 2-by-2 case
            if ((z[1] < 0.0) || (z[2] < 0.0)) {
                info[0] = -2;
                MipavUtil.displayError("Error dlasq2 had z[1] < 0.0 or z[2] < 0.0");
                return;
            }
            else if (z[2] > z[0]) {
                d = z[2];
                z[2] = z[0];
                z[0] = d;
            }
            z[4] = z[0] + z[1] + z[2];
            if (z[1] > z[2]*tol2) {
                t = 0.5 *((z[0] - z[2]) + z[1]);
                s = z[2] * (z[1]/t);
                if (s <= t) {
                    s = z[2] * (z[1]/(t*(1.0 + Math.sqrt(1.0 + s/t))));
                }
                else {
                    s = z[2] * (z[1]/(t + Math.sqrt(t)*Math.sqrt(t+s)));
                }
                t = z[0] + (s + z[1]);
                z[2] = z[2] * (z[0]/t);
                z[0] = t;
            } // if (z[1] > z[2]*tol2)
            z[1] = z[2];
            z[5] = z[1] + z[0];
            return;
        } // else if (n == 2)
        
        // Check for negative data and compute sums of q's and e's.
        
        z[2*n-1] = 0.0;
        emin = z[1];
        qmax = 0.0;
        zmax = 0.0;
        d = 0.0;
        e = 0.0;
        
        for (k = 1; k <= 2*(n-1); k += 2) {
            if (z[k-1] < 0.0) {
                info[0] = -(200+k);
                MipavUtil.displayError("Error dlasq2 had info[0] = " + info[0]);
                return;
            }
            else if (z[k] < 0.0) {
                info[0] = -(200+k+1);
                MipavUtil.displayError("Error dlasq2 had info[0] = " + info[0]);
                return;
            }
            d = d + z[k-1];
            e = e + z[k];
            qmax = Math.max(qmax, z[k-1]);
            emin = Math.min(emin, z[k]);
            zmax = Math.max(qmax, Math.max(zmax, z[k]));
        } // (k = 1; k <= 2*(n-1); k += 2)
        if (z[2*n-2] < 0.0) {
            info[0] = -(200+2*n-1);
            MipavUtil.displayError("Error dlasq2 had info[0] = " + info[0]);
            return;
        }
        d = d + z[2*n-2];
        qmax = Math.max(qmax, z[2*n-2]);
        zmax = Math.max(qmax, zmax);
        
        // Check for diagonality
        
        if (e == 0.0) {
            for (k = 2; k <= n; k++) {
                z[k-1] = z[2*k-2];
            }
            ge.ge.dlasrt('D', n, z, iinfo);
            z[2*n-2] = d;
            return;
        } // if (e == 0.0)
        
        trace = d + e;
        
        // Check for zero data
        
        if (trace == 0.0) {
            z[2*n-2] = 0.0;
            return;
        }
        
        // Check whether the machine is IEEE conformable.
        name = new String("DLASQ2");
        opts = new String("N");
        ieee = ((ge.ilaenv(10, name, opts, 1, 2, 3, 4) == 1) && (ge.ilaenv(11, name, opts, 1, 2, 3, 4) == 1));
        
        // Rearrange data for locality: z = (q1,qq1,e1,ee1,q2,qq2,e2,ee2,...).
        
        for (k = 2*n; k >= 2; k -= 2) {
            z[2*k-1] = 0.0;
            z[2*k-2] = z[k-1];
            z[2*k-3] = 0.0;
            z[2*k-4] = z[k-2];
        } // for (k = 2*n; k >= 2; k -= 2)
        
        i0[0] = 1;
        n0[0] = n;
        
        // Reverse the qd-array, if warranted
        
        if (cbias * z[4*i0[0]-4] < z[4*n0[0]-4]) {
            ipn4 = 4 * (i0[0] + n0[0]);
            for (i4 = 4*i0[0]; i4 <= 2*(i0[0]+n0[0]-1); i4 += 4) {
                temp = z[i4-4];
                z[i4-4] = z[ipn4 -i4 - 4];
                z[ipn4 - i4 - 4] = temp;
                temp = z[i4-2];
                z[i4-2] = z[ipn4 - i4 - 6];
                z[ipn4 - i4 - 6] = temp;
            } // for (i4 = 4*i0[0]; i4 <= 2*(i0[0]+n0[0]-1); i4 += 4)
        } // if (cbias * z[4*i0[0]-4] < z[4*n0[0]-4])
        
        // Initial split checking via dqd and Li's test.
        
        pp[0] = 0;
        
        for (k = 1; k <= 2; k++) {
            d = z[4*n0[0]+pp[0]-4];
            for (i4 = 4*(n0[0]-1) + pp[0]; i4 >= 4*i0[0] + pp[0]; i4 -= 4) {
                if (z[i4-2] <= tol2*d) {
                    z[i4-2] = -0.0;
                    d = z[i4-4];
                }
                else {
                    d = z[i4-4]*(d/(d + z[i4-2]));
                }
            } // for (i4 = 4*(n0[0]-1) + pp[0]; i4 >= 4*i0[0] + pp[0]; i4 -= 4)
            
            // dqd maps z to zz plus Li's test
            
            emin = z[4*i0[0]+pp[0]];
            d = z[4*i0[0]+pp[0]-4];
            for (i4 = 4*i0[0] + pp[0]; i4 <= 4*(n0[0]-1) + pp[0]; i4 += 4) {
                z[i4-2*pp[0]-3] = d + z[i4-2];
                if (z[i4-2] <= tol2*d) {
                    z[i4-2] = -0.0;
                    z[i4-2*pp[0]-3] = d;
                    z[i4-2*pp[0]-1] = 0.0;
                    d = z[i4];
                } // if (z[i4-2] <= tol2*d)
                else if ((safmin * z[i4] < z[i4-2*pp[0]-3]) && (safmin*z[i4-2*pp[0]-3] < z[i4])) {
                    temp = z[i4]/z[i4-2*pp[0]-3];
                    z[i4-2*pp[0]-1] = z[i4-2]*temp;
                    d = d * temp;
                }
                else {
                    z[i4-2*pp[0]-1] = z[i4] *(z[i4-2]/z[i4-2*pp[0]-3]);
                    d = z[i4] * (d/z[i4-2*pp[0]-3]);
                }
                emin = Math.min(emin, z[i4-2*pp[0]-1]);
            } // for (i4 = 4*i0[0] + pp[0]; i4 <= 4*(n0[0]-1) + pp[0]; i4 += 4)
            z[4*n0[0] - pp[0] - 3] = d;
            
            // Now find qmax.
            
            qmax = z[4*i0[0]-pp[0]-3];
            for (i4 = 4*i0[0] - pp[0] + 2; i4 <= 4*n0[0] - pp[0] - 2; i4 += 4) {
                qmax = Math.max(qmax, z[i4-1]);
            }
            
            // Prepare for the next iteration on k.
            pp[0] = 1 - pp[0];
        } // for (k = 1; k <= 2; k++)
        
        // Initialize variables to pass to dlasq3
        
        ttype[0] = 0;
        dmin1[0] = 0.0;
        dmin2[0] = 0.0;
        dn[0] = 0.0;
        dn1[0] = 0.0;
        dn2[0] = 0.0;
        g[0] = 0.0;
        tau[0] = 0.0;
        
        iter[0] = 2;
        nfail[0] = 0;
        ndiv[0] = 2*(n0[0] - i0[0]);
        
        loop1: {
            loop2: for (iwhila = 1; iwhila <= n + 1; iwhila++) {
                if (n0[0] < 1) {
                    break loop1;
                }
                
                // While array unfinished do
                
                // e[n0-1] holds the value of sigma when submatrix in i0-1:n0-1
                // splits from the rest of the array, but is negated.
                
                desig[0] = 0.0;
                if (n0[0] == n) {
                    sigma[0] = 0.0;
                }
                else {
                    sigma[0] = -z[4*n0[0]-2];
                }
                if (sigma[0] < 0.0) {
                    info[0] = 1;
                    return;
                }
                
                // Find the last unreduced submatrix's top index i0, find qmax and
                // emin.  Find Gershgorin-type bound if Q's much greater than E's.
                
                emax = 0.0;
                if (n0[0] > i0[0]) {
                    emin = Math.abs(z[4*n0[0]-6]);
                }
                else {
                    emin = 0.0;
                }
                qmin = z[4*n0[0]-4];
                qmax = qmin;
                loop3: {
                    for (i4 = 4*n0[0]; i4 >= 8; i4 -= 4) {
                        if (z[i4-6] <= 0.0) {
                            break loop3;
                        }
                        if (qmin >= 4.0*emax) {
                            qmin = Math.min(qmin, z[i4-4]);
                            emax = Math.max(emax, z[i4-6]);
                        }
                        qmax = Math.max(qmax, z[i4-8] + z[i4-6]);
                        emin = Math.min(emin, z[i4-6]);
                    } // for (i4 = 4*n0[0]; i4 >= 8; i4 -= 4)
                    i4 = 4;
                } // loop3
                
                i0[0] = i4/4;
                pp[0] = 0;
                
                if (n0[0] - i0[0] > 1) {
                    dee = z[4*i0[0] - 4];
                    deemin = dee;
                    kmin = i0[0];
                    for (i4 = 4*i0[0]+1; i4 <= 4*n0[0]-3; i4 += 4) {
                        dee = z[i4-1] * (dee/(dee + z[i4-3]));
                        if (dee <= deemin) {
                            deemin = dee;
                            kmin = (i4+3)/4;
                        }
                    } // for (i4 = 4*i0[0]+1; i4 <= 4*n0[0]-3; i4 += 4)
                    if ((2*(kmin - i0[0]) < n0[0] - kmin) && (deemin <= 0.5 * z[4*n0[0]-4])) {
                        ipn4 = 4*(i0[0]+n0[0]);
                        pp[0] = 2;
                        for (i4 = 4*i0[0]; i4 <= 2*(i0[0] + n0[0] - 1); i4 += 4) {
                            temp = z[i4-4];
                            z[i4-4] = z[ipn4-i4-4];
                            z[ipn4-i4-4] = temp;
                            temp = z[i4-3];
                            z[i4-3] = z[ipn4-i4-3];
                            z[ipn4-i4-3] = temp;
                            temp = z[i4-2];
                            z[i4-2] = z[ipn4-i4-6];
                            z[ipn4-i4-6] = temp;
                            temp = z[i4-1];
                            z[i4-1] = z[ipn4-i4-5];
                            z[ipn4-i4-5] = temp;
                        } // for (i4 = 4*i0[0]; i4 <= 2*(i0[0] + n0[0] - 1); i4 += 4)
                    } // if ((2*(kmin - i0[0]) < n0[0] - kmin) && (deemin <= 0.5 * z[4*n0[0]-4]))
                } // if (n0[0] - i0[0] > 1)
                
                // Put -(initial shift) into dmin.
                
                dmin[0] = -Math.max(0.0, qmin - 2.0*Math.sqrt(qmin)*Math.sqrt(emax));
                
                // Now i0:n0 is unreduced.
                // pp = 0 for ping, pp = 1 for pong.
                // pp = 2 indicates tht flipping was applied to the z array and
                // that the tests for deflation upon entry in dlasq3 should not
                // be performed.
                
                nbig = 30*(n0[0] - i0[0] + 1);
                for (iwhilb = 1; iwhilb <= nbig; iwhilb++) {
                    if (i0[0] > n0[0]) {
                        continue loop2;
                    }
                    
                    // While submatrix unfinished take a good dqds step.
                    dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, iter, ndiv, ieee, ttype,
                           dmin1, dmin2, dn, dn1, dn2, g, tau);
                    
                    pp[0] = 1 - pp[0];
                    
                    // When emin is very small check for splits
                    if ((pp[0] == 0) && (n0[0] - i0[0] >= 3)) {
                        if ((z[4*n0[0]-1] <= tol2*qmax) || (z[4*n0[0]-2] <= tol2*sigma[0])) {
                            splt = i0[0] - 1;
                            qmax = z[4*i0[0]-4];
                            emin = z[4*i0[0]-2];
                            oldemn = z[4*i0[0]-1];
                            for (i4 = 4*i0[0]; i4 <= 4*(n0[0]-3); i4 += 4) {
                                if ((z[i4-1] <= tol2*z[i4-4]) || (z[i4-2] <= tol2*sigma[0])) {
                                    z[i4-2] = -sigma[0];
                                    splt = i4/4;
                                    qmax = 0.0;
                                    emin = z[i4+2];
                                    oldemn = z[i4+3];
                                } // if ((z[i4-1] <= tol2*z[i4-4]) || (z[i4-2] <= tol2*sigma[0]))
                                else {
                                    qmax = Math.max(qmax, z[i4]);
                                    emin = Math.min(emin, z[i4-2]);
                                    oldemn = Math.min(oldemn, z[i4-1]);
                                }
                            } // for (i4 = 4*i0; i4 <= 4*(n0-3); i4 += 4)
                            z[4*n0[0]-2] = emin;
                            z[4*n0[0]-1] = oldemn;
                            i0[0] = splt + 1;
                        } // if ((z[4*n0[0]-1] <= tol2*qmax) || (z[4*n0[0]-2] <= tol2*sigma[0]))
                    } // if ((pp[0] == 0) && (n0[0] - i0[0] >= 3))
                } // for (iwhilb = 1; iwhilb <= nbig; iwhilb++)
                info[0] = 2;
                return;
            } // loop2: for (iwhila = 1; iwhila <= n + 1; iwhila++)
            info[0] = 3;
            return;
        } // loop1
        
        // Move q's to the front.
        
        for (k = 2; k <= n; k++) {
            z[k-1] = z[4*k-4];
        }
        
        // Sort and compute sum of eigenvalues.
        
        ge.ge.dlasrt('D', n, z, iinfo);
        
        e = 0.0;
        for (k = n; k >= 1; k--) {
            e = e + z[k-1];
        }
        
        // Store trace, sum(eigenvalues), and information on performance.
        
        z[2*n] = trace;
        z[2*n+1] = e;
        z[2*n+2] = (double)iter[0];
        z[2*n+3] = (double)ndiv[0]/(double)(n*n);
        z[2*n+4] = 100.0 * nfail[0]/(double)iter[0];
        return;
    } // dlasq2

    /** This is a port of version 3.2 LAPACK routine DLASQ3
     *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
     *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
     *  -- Berkeley                                                        --
     *  -- November 2008                                                   --
     *
     *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
     *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
     *
     *     .. Scalar Arguments ..
           LOGICAL            IEEE
           INTEGER            I0, ITER, N0, NDIV, NFAIL, PP
           DOUBLE PRECISION   DESIG, DMIN, DMIN1, DMIN2, DN, DN1, DN2, G,
          $                   QMAX, SIGMA, TAU
     *     ..
     *     .. Array Arguments ..
           DOUBLE PRECISION   Z( * )
     *     ..
     *
     *  Purpose
     *  =======
     *
     *  DLASQ3 checks for deflation, computes a shift (TAU) and calls dqds.
     *  In case of failure it changes shifts, and tries again until output
     *  is positive.
     *
     *  Arguments
     *  =========
     *
     *  I0     (input/output) INTEGER
     *         First index.
     *
     *  N0     (inputoutput) INTEGER
     *         Last index.
     *
     *  Z      (input) DOUBLE PRECISION array, dimension ( 4*N )
     *         Z holds the qd array.
     *
     *  PP     (input/output) INTEGER
     *         PP=0 for ping, PP=1 for pong.
     *         PP=2 indicates that flipping was applied to the Z array   
     *         and that the initial tests for deflation should not be 
     *         performed.
     *
     *  DMIN   (output) DOUBLE PRECISION
     *         Minimum value of d.
     *
     *  SIGMA  (output) DOUBLE PRECISION
     *         Sum of shifts used in current segment.
     *
     *  DESIG  (input/output) DOUBLE PRECISION
     *         Lower order part of SIGMA
     *
     *  QMAX   (input) DOUBLE PRECISION
     *         Maximum value of q.
     *
     *  NFAIL  (output) INTEGER
     *         Number of times shift was too big.
     *
     *  ITER   (output) INTEGER
     *         Number of iterations.
     *
     *  NDIV   (output) INTEGER
     *         Number of divisions.
     *
     *  IEEE   (input) LOGICAL
     *         Flag for IEEE or non IEEE arithmetic (passed to DLASQ5).
     *
     *  TTYPE  (input/output) INTEGER
     *         Shift type.
     *
     *  DMIN1, DMIN2, DN, DN1, DN2, G, TAU (input/output) DOUBLE PRECISION
     *         These are passed as arguments in order to save their values
     *         between calls to DLASQ3.
     */
  private void dlasq3(int i0[], int n0[], double z[], int pp[], double dmin[], double sigma[],
                 double desig[], double qmax, int nfail[], int iter[], int ndiv[],
                 boolean ieee, int ttype[], double dmin1[], double dmin2[], double dn[],
                 double dn1[], double dn2[], double g[], double tau[]) {
      double cbias = 1.50;
      int ipn4;
      int j4;
      int n0in;
      int nn;
      double eps;
      double s;
      double t;
      double temp;
      double tol;
      double tol2;
      boolean calldlasq6;
      
      n0in = n0[0];
      eps = ge.dlamch('P'); // Precision
      tol = 100.0 * eps;
      tol2 = tol * tol;
      
      // Check for deflation.
      while (true) {
          if (n0[0] < i0[0]) {
              return;
          }
          if (n0[0] == i0[0]) {
              z[4*n0[0]-4] = z[4*n0[0]+pp[0]-4] + sigma[0];
              n0[0] = n0[0] - 1;
              continue;
          }
          nn = 4*n0[0] + pp[0];
          if (n0[0] != (i0[0]+1)) {
              // Check whether e[n0[0]-2] is negligible, 1 eigenvalue.
              if ((z[nn-6] <= tol2*(sigma[0]+z[nn-4])) ||
                  (z[nn-2*pp[0]-5] <= tol2*z[nn-8])) {
                  z[4*n0[0]-4] = z[4*n0[0]+pp[0]-4] + sigma[0];
                  n0[0] = n0[0] - 1;
                  continue;    
              }
              // Check whether e[n0[0]-3] is negligible, 2 eigenvalues.
              if ((z[nn-10] > tol2*sigma[0]) && (z[nn-2*pp[0]-9] > tol2*z[nn-12])) {
                  break;
              }
          } // if (n0[0] != (i0[0] + 1))
          
          if (z[nn - 4] > z[nn-8]) {
              s = z[nn-4];
              z[nn-4] = z[nn-8];
              z[nn-8] = s;
          } // if (z[nn - 4] > z[nn-8])
          if (z[nn-6] > z[nn-4]*tol2) {
              t = 0.5 * ((z[nn-8] - z[nn-4]) + z[nn-6]);
              s = z[nn-4] * (z[nn-6]/t);
              if (s <= t) {
                  s = z[nn-4] * (z[nn-6]/(t*(1.0 + Math.sqrt(1.0 + s/t))));
              }
              else {
                  s = z[nn-4] * (z[nn-6]/(t + Math.sqrt(t)*Math.sqrt(t+s)));
              }
              t = z[nn-8] + (s + z[nn-6]);
              z[nn-4] = z[nn-4] * (z[nn-8]/t);
              z[nn-8] = t;
          } // if (z[nn-6] > z[nn-4]*tol2)
          z[4*n0[0]-8] = z[nn-8] + sigma[0];
          z[4*n0[0]-4] = z[nn-4] + sigma[0];
          n0[0] = n0[0] - 2;
      } // while (true)
      
      if (pp[0] == 2) {
          pp[0] = 0;
      }
      
      // Reverse the qd-array, if warranted.
      
      if ((dmin[0] <= 0.0) || (n0[0] < n0in)) {
          if (cbias*z[4*i0[0]+pp[0]-4] < z[4*n0[0]+pp[0]-4]) {
              ipn4 = 4 * (i0[0] + n0[0]);
              for (j4 = 4*i0[0]; j4 <= 2*(i0[0] + n0[0] - 1); j4 += 4) {
                  temp = z[j4-4];
                  z[j4-4] = z[ipn4-j4-4];
                  z[ipn4-j4-4] = temp;
                  temp = z[j4-3];
                  z[j4-3] = z[ipn4-j4-3];
                  z[ipn4-j4-3] = temp;
                  temp = z[j4-2];
                  z[j4-2] = z[ipn4-j4-6];
                  z[ipn4-j4-6] = temp;
                  temp = z[j4-1];
                  z[j4-1] = z[ipn4-j4-5];
                  z[ipn4-j4-5] = temp;
              } // for (j4 = 4*i0[0]; j4 <= 2*(i0[0] + n0[0] - 1); j4 += 4)
              if (n0[0] - i0[0] <= 4) {
                  z[4*n0[0]+pp[0]-2] = z[4*i0[0]+pp[0]-2];
                  z[4*n0[0]-pp[0]-1] = z[4*i0[0]-pp[0]-1];
              }
              dmin2[0] = Math.min(dmin2[0], z[4*n0[0]+pp[0]-2]);
              z[4*n0[0]+pp[0]-2] = Math.min(z[4*n0[0]+pp[0]-2], Math.min(z[4*i0[0]+pp[0]-2],
                                         z[4*i0[0]+pp[0]+2]));
              z[4*n0[0]-pp[0]-1] = Math.min(z[4*n0[0]-pp[0]-1], Math.min(z[4*i0[0]-pp[0]-1], 
                                         z[4*i0[0]-pp[0]+3]));
              qmax = Math.max(qmax, Math.max(z[4*i0[0]+pp[0]-4], z[4*i0[0]+pp[0]]));
              dmin[0] = -0.0;
          } // if (cbias*z[4*i0[0]+pp[0]-4] < z[4*n0[0]+pp[0]-4])
      } // if ((dmin[0] <= 0.0) || (n0[0] < n0in))
      
      // Choose a shift
      dlasq4(i0[0], n0[0], z, pp[0], n0in, dmin[0], dmin1[0], dmin2[0], dn[0], dn1[0], dn2[0], tau, ttype, g);
      
      // Call dqds until dmin > 0
      
      while (true) {
          dlasq5(i0[0], n0[0], z, pp[0], tau[0], dmin, dmin1, dmin2, dn, dn1, dn2, ieee);
          
          ndiv[0] = ndiv[0] + (n0[0] - i0[0] + 2);
          iter[0] = iter[0] + 1;
          
          // Check status
          if (Double.isNaN(dmin[0])) {
              // NaN
              
              if (tau[0] == 0.0) {
                  calldlasq6 = true;
                  break;
              }
              else {
                  tau[0] = 0.0;
                  continue;
              }
          } // else if (Double.isNaN(dmin[0]))
          else if ((dmin[0] >= 0.0) && (dmin1[0] > 0.0)) {
              // Success
              calldlasq6 = false;
              break;
          }
          else if ((dmin[0] < 0.0) && (dmin1[0] > 0.0) && 
                  (z[4*(n0[0]-1)-pp[0]-1] < tol*(sigma[0]+dn1[0])) &&
                  (Math.abs(dn[0]) < tol*sigma[0])) {
              // Convergence hidden by negative dn[0]
              z[4*(n0[0]-1)-pp[0]+1] = 0.0;
              dmin[0] = 0.0;
              calldlasq6 = false;
              break;
          }
          else if (dmin[0] < 0.0) {
              //tau[0] too big.  Select new tau[0] and try again.
              nfail[0] = nfail[0] + 1;
              if (ttype[0] < -22) {
                  // Failed twice.  Play it safe.
                  tau[0] = 0.0;
              }
              else if (dmin1[0] > 0.0) {
                  // Late failure.  Gives excellent shift.
                  tau[0] = (tau[0] + dmin[0]) * (1.0 - 2.0 * eps);
                  ttype[0] = ttype[0] - 11;
              }
              else {
                  // Early failure.  Divide by 4.
                  tau[0] = 0.25 * tau[0];
                  ttype[0] = ttype[0] - 12;
              }
              continue;
          } // else if (dmin[0] < 0.0)
          // Possible underflow.  Play it safe.
          calldlasq6 = true;
          break;
      } // while (true)
      
      if (calldlasq6) {
          // Risk of underflow
          dlasq6(i0[0], n0[0], z, pp[0], dmin, dmin1, dmin2, dn, dn1, dn2);
          ndiv[0] = ndiv[0] + (n0[0] - i0[0] + 2);
          iter[0] = iter[0] + 1;
          tau[0] = 0.0;
      } // if (calldlasq6)
      
      if (tau[0] < sigma[0]) {
          desig[0] = desig[0] + tau[0];
          t = sigma[0] + desig[0];
          desig[0] = desig[0] - (t - sigma[0]);
      }
      else {
          t = sigma[0] + tau[0];
          desig[0] = sigma[0] - (t - tau[0]) + desig[0];
      }
      sigma[0] = t;
      return;
  } // dlasq3
    
    /** This is a port of version 3.2 LAPACK routine DLASQ4.
       *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
       *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
       *  -- Berkeley                                                        --
       *  -- November 2008                                                   --
       *
       *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
       *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
       *
       *     .. Scalar Arguments ..
             INTEGER            I0, N0, N0IN, PP, TTYPE
             DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DN1, DN2, G, TAU
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   Z( * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DLASQ4 computes an approximation TAU to the smallest eigenvalue
       *  using values of d from the previous transform.
       *
       *  I0    (input) INTEGER
       *        First index.
       *
       *  N0    (input) INTEGER
       *        Last index.
       *
       *  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
       *        Z holds the qd array.
       *
       *  PP    (input) INTEGER
       *        PP=0 for ping, PP=1 for pong.
       *
       *  NOIN  (input) INTEGER
       *        The value of N0 at start of EIGTEST.
       *
       *  DMIN  (input) DOUBLE PRECISION
       *        Minimum value of d.
       *
       *  DMIN1 (input) DOUBLE PRECISION
       *        Minimum value of d, excluding D( N0 ).
       *
       *  DMIN2 (input) DOUBLE PRECISION
       *        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
       *
       *  DN    (input) DOUBLE PRECISION
       *        d(N)
       *
       *  DN1   (input) DOUBLE PRECISION
       *        d(N-1)
       *
       *  DN2   (input) DOUBLE PRECISION
       *        d(N-2)
       *
       *  TAU   (output) DOUBLE PRECISION
       *        This is the shift.
       *
       *  TTYPE (output) INTEGER
       *        Shift type.
       *
       *  G     (input/output) REAL
       *        G is passed as an argument in order to save its value between
       *        calls to DLASQ4.
       *
       *  Further Details
       *  ===============
       *  CNST1 = 9/16
       */
    private void dlasq4(int i0, int n0, double z[], int pp, int n0in, double dmin, double dmin1,
                        double dmin2, double dn, double dn1, double dn2, double tau[], int ttype[],
                        double g[]) {
        double cnst1 = 0.5630;
        double cnst2 = 1.010;
        double cnst3 = 1.050;
        double third = 0.3330;
        int i4;
        int nn;
        int np;
        double a2;
        double b1;
        double b2;
        double gam;
        double gap1;
        double gap2;
        double s = 0.0;
        
        // A negative dmin forces the shift to take that absolute value.
        // ttype records the type of shift.
        if (dmin <= 0.0) {
            tau[0] = -dmin;
            ttype[0] = -1;
            return;
        }  // if (dmin <= 0.0)
        
        nn = 4*n0 + pp;
        if (n0in == n0) {
            // No eigenvalues deflated.
            if ((dmin == dn) || (dmin == dn1)) {
                b1 = Math.sqrt(z[nn-4]) * Math.sqrt(z[nn-6]);
                b2 = Math.sqrt(z[nn-8]) * Math.sqrt(z[nn-10]);
                a2 = z[nn-8] + z[nn-6];
                
                // Cases 2 and 3
                
                if ((dmin == dn) && (dmin1 == dn1)) {
                    gap2 = 0.75*dmin2 - a2;
                    if ((gap2 > 0.0) && (gap2 > b2)) {
                        gap1 = a2 - dn - (b2/gap2)*b2;
                    }
                    else {
                        gap1 = a2 - dn - (b1 + b2);
                    }
                    if ((gap1 > 0.0) && (gap1 > b1)) {
                        s = Math.max(dn-(b1/gap1)*b1, 0.5*dmin);
                        ttype[0] = -2;
                    }
                    else {
                        s = 0.0;
                        if (dn > b1) {
                            s = dn - b1;
                        }
                        if (a2 > (b1 + b2)) {
                            s = Math.min(s, a2 - (b1 + b2));
                        }
                        s = Math.max(s, third * dmin);
                        ttype[0] = -3;
                    }
                } // if ((dmin == dn) && (dmin1 == dn1))
                else {
                    // Case 4.
                    
                    ttype[0] = -4;
                    s = 0.25 * dmin;
                    if (dmin == dn) {
                        gam = dn;
                        a2 = 0.0;
                        if (z[nn-6] > z[nn-8]) {
                            return;
                        }
                        b2 = z[nn-6]/z[nn-8];
                        np = nn - 9;
                    } // if (dmin == dn)
                    else { // dmin != dn
                        np = nn - 2*pp;
                        b2 = z[np-3];
                        gam = dn1;
                        if (z[np-5] > z[np-3]) {
                            return;
                        }
                        a2 = z[np-5]/z[np-3];
                        if (z[nn-10] > z[nn-12]) {
                            return;
                        }
                        b2 = z[nn-10]/z[nn-12];
                        np = nn - 13;
                    } // else dmin != dn
                    // Approximate contribution to norm squared from i < nn - 1.
                    a2 = a2 + b2;
                    for (i4 = np; i4 >= 4*i0 - 1 + pp; i4 -= 4) {
                        if (b2 == 0.0) {
                            break;
                        }
                        b1 = b2;
                        if (z[i4-1] > z[i4-3]) {
                            return;
                        }
                        b2 = b2 *(z[i4-1]/z[i4-3]);
                        a2 = a2 + b2;
                        if ((100.0 * Math.max(b2, b1) < a2) || (cnst1 < a2)) {
                            break;
                        }
                    } // for (i4 = np; i4 >= 4*i0 - 1 + pp; i4 -= 4)
                    a2 = cnst3 * a2;
                    
                    // Rayleigh quotient residual bond.
                    if (a2 < cnst1) {
                        s = gam * (1.0 - Math.sqrt(a2)) / (1.0 + a2); 
                    }
                } // else
            } // if ((dmin == dn) || (dmin == dn1))
            else if (dmin == dn2) {
                // Case 5.
                
                ttype[0] = -5;
                s = 0.25*dmin;
                
                // Compute contribution to norm squared from i > nn - 2.
                np = nn - 2*pp;
                b1 = z[np-3];
                b2 = z[np-7];
                gam = dn2;
                if ((z[np-9] > b2) || (z[np-5] > b1)) {
                    return;
                }
                a2 = (z[np-9]/b2)* (1.0 + z[np-5]/b1);
                
                // Approixmate contribution to norm squared from i < nn - 2.
                
                if (n0 - i0 > 2) {
                    b2 = z[nn-14]/z[nn-16];
                    a2 = a2 + b2;
                    for (i4 = nn - 17; i4 >= 4*i0 - 1 + pp; i4 -= 4) {
                        if (b2 == 0.0) {
                            break;
                        }
                        b1 = b2; 
                        if (z[i4-1] > z[i4-3]) {
                            return;
                        }
                        b2 = b2 * (z[i4-1]/z[i4-3]);
                        a2 = a2 + b2;
                        if ((100.0 * Math.max(b2,b1) < a2) || (cnst1 < a2)) {
                            break;
                        }
                    } // for (i4 = nn - 17; i4 >= 4*i0 - 1 + pp[0]; i4 -= 4)
                    a2 = cnst3 * a2;
                } // if (n0 - i0 > 2)
                if (a2 < cnst1) {
                    s = gam * (1.0 - Math.sqrt(a2))/(1.0 + a2);
                }
            } // else if (dmin == dn2)
            else {
                // Case 6, no information to guide us.
                if (ttype[0] == -6) {
                    g[0] = g[0] + third * (1.0 - g[0]);
                }
                else if (ttype[0] == -18) {
                    g[0] = 0.25*third;
                }
                else {
                    g[0] = 0.25;
                }
                s = g[0] * dmin;
                ttype[0] = -6;
            } // else
        } // if (n0in == n0)
        else if (n0in == (n0 + 1)) {
            // One eigenvalue just deflated.  Use dmin, dn1 for dmin and dn.
            if ((dmin1 == dn1) && (dmin2 == dn2)) {
                // Cases 7 and 8.
                
                ttype[0] = -7;
                s = third * dmin1;
                if (z[nn-6] > z[nn-8]) {
                    return;
                }
                b1 = z[nn-6]/z[nn-8];
                b2 = b1;
                if (b2 != 0.0) {
                    for (i4 = 4*n0 - 9 + pp; i4 >= 4*i0 - 1 + pp; i4 -= 4) {
                        a2 = b1;
                        if (z[i4-1] > z[i4-3]) {
                            return;
                        }
                        b1 = b1 *(z[i4-1]/z[i4-3]);
                        b2 = b2 + b1;
                        if (100.0 * Math.max(b1, a2) < b2) {
                            break;
                        }
                    } // for (i4 = 4*n0 - 9 + pp; i4 >= 4*i0 - 1 + pp; i4 -= 4)
                } // if (b2 != 0.0)
                b2 = Math.sqrt(cnst3*b2);
                a2 = dmin1 / (1.0 + b2*b2);
                gap2 = 0.5*dmin2 - a2;
                if ((gap2 > 0.0) && (gap2 > b2*a2)) {
                    s = Math.max(s, a2*(1.0 - cnst2*a2*(b2/gap2)*b2));
                }
                else {
                    s = Math.max(s, a2 * (1.0 - cnst2 * b2));
                    ttype[0] = -8;
                }
            } // if ((dmin1 == dn1) && (dmin2 == dn2))
            else {
                // Case 9.
                
                s = 0.25 * dmin1;
                if (dmin1 == dn1) {
                    s = 0.5 * dmin1;
                }
                ttype[0] = -9;
            }
        } // else if (n0in == (n0 + 1))
        else if (n0in == (n0 + 2)) {
            // Two eigenvalues deflated.  Use dmin2, dn2 for dmin and dn.
            // Cases 10 and 11
            
            if ((dmin2 == dn2) && (2.0*z[nn-6] < z[nn-8])) {
                ttype[0] = -10;
                s = third * dmin2;
                if (z[nn-6] > z[nn-8]) {
                    return;
                }
                b1 = z[nn-6]/z[nn-8];
                b2 = b1;
                if (b2 != 0.0) {
                    for (i4 = 4*n0 - 9 + pp; i4 >= 4*i0 - 1 + pp; i4 -= 4) {
                        if (z[i4 - 1] > z[i4-3]) {
                            return;
                        }
                        b1 = b1 * (z[i4-1]/z[i4-3]);
                        b2 = b2 + b1;
                        if (100.0 * b1 < b2) {
                            break;
                        }
                    } // for (i4 = 4*n0 - 9 + pp; i4 >= 4*i0 - 1 + pp; i4 -= 4)
                } // if (b2 != 0.0)
                b2 = Math.sqrt(cnst3 * b2);
                a2 = dmin2 / (1.0 + b2*b2);
                gap2 = z[nn-8] + z[nn-10] - Math.sqrt(z[nn-12]) * Math.sqrt(z[nn-10]) - a2;
                if ((gap2 > 0.0) && (gap2 > b2*a2)) {
                    s = Math.max(s, a2*(1.0 - cnst2*a2*(b2/gap2)*b2));
                }
                else {
                    s = Math.max(s, a2*(1.0 - cnst2 * b2));
                }
            } // if ((dmin2 == dn2) && (2.0*z[nn-6] < z[nn-8]))
            else {
                s = 0.25 * dmin2;
                ttype[0] = -11;
            }
        } // else if (n0in == (n0 + 2))
        else if (n0in > (n0 + 2)) {
            // Case 12, more than two eigenvalues deflated.  No information.
            s = 0.0;
            ttype[0] = -12;
        } // else if (n0in > (n0 + 2))
        
        tau[0] = s;
        return;
    } // dlasq4

    
    /** This is a port of version 3.2 LAPACK routine DLASQ5
    *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
    *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
    *  -- Berkeley                                                        --
    *  -- November 2008                                                   --
    *
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *
    *     .. Scalar Arguments ..
          LOGICAL            IEEE
          INTEGER            I0, N0, PP
          DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2, TAU
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   Z( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DLASQ5 computes one dqds transform in ping-pong form, one
    *  version for IEEE machines another for non IEEE machines.
    *
    *  Arguments
    *  =========
    *
    *  I0    (input) INTEGER
    *        First index.
    *
    *  N0    (input) INTEGER
    *        Last index.
    *
    *  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
    *        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
    *        an extra argument.
    *
    *  PP    (input) INTEGER
    *        PP=0 for ping, PP=1 for pong.
    *
    *  TAU   (input) DOUBLE PRECISION
    *        This is the shift.
    *
    *  DMIN  (output) DOUBLE PRECISION
    *        Minimum value of d.
    *
    *  DMIN1 (output) DOUBLE PRECISION
    *        Minimum value of d, excluding D( N0 ).
    *
    *  DMIN2 (output) DOUBLE PRECISION
    *        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
    *
    *  DN    (output) DOUBLE PRECISION
    *        d(N0), the last value of d.
    *
    *  DNM1  (output) DOUBLE PRECISION
    *        d(N0-1).
    *
    *  DNM2  (output) DOUBLE PRECISION
    *        d(N0-2).
    *
    *  IEEE  (input) LOGICAL
    *        Flag for IEEE or non IEEE arithmetic.
    */
    private void dlasq5(int i0, int n0, double z[], int pp, double tau, double dmin[], double dmin1[],
                        double dmin2[], double dn[], double dnm1[], double dnm2[], boolean ieee)  {
        int j4;
        int j4p2;
        double d;
        double emin;
        double temp;
        
        if ((n0 - i0 - 1) <= 0) {
            return;
        }
        
        j4 = 4*i0 + pp - 3;
        emin = z[j4+3];
        d = z[j4-1] - tau;
        dmin[0] = d;
        dmin1[0] = -z[j4-1];
        
        if (ieee) {
            // Code for IEEE arithmetic.
            if (pp == 0) {
                for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                    z[j4-3] = d + z[j4-2];
                    temp = z[j4]/z[j4-3];
                    d = d * temp - tau;
                    dmin[0] = Math.min(dmin[0], d);
                    z[j4-1] = z[j4-2] * temp;
                    emin = Math.min(z[j4-1], emin);
                } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
            } // if (pp == 0)
            else { // pp != 0
                for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                    z[j4-4] = d + z[j4-1];
                    temp = z[j4+1]/z[j4-4];
                    d = d * temp - tau;
                    dmin[0] = Math.min(dmin[0], d);
                    z[j4-2] = z[j4-1] * temp;
                    emin = Math.min(z[j4-2], emin);
                } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
            } // else pp != 0
            
            // Unroll last 2 steps.
            
            dnm2[0] = d;
            dmin2[0] = dmin[0];
            j4 = 4*(n0-2) - pp;
            j4p2 = j4 + 2*pp - 1;
            z[j4-3] = dnm2[0] + z[j4p2-1];
            z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
            dnm1[0] = z[j4p2+1] * (dnm2[0]/z[j4-3]) - tau;
            dmin[0] = Math.min(dmin[0], dnm1[0]);
            
            dmin1[0] = dmin[0];
            j4 = j4 + 4;
            j4p2 = j4 + 2*pp - 1;
            z[j4-3] = dnm1[0] + z[j4p2-1];
            z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
            dn[0] = z[j4p2+1] * (dnm1[0]/z[j4-3]) - tau;
            dmin[0] = Math.min(dmin[0], dn[0]);
        } // if (ieee)
        else { // !ieee
            // Code for non IEEE arithmetic.
            
            if (pp == 0) {
                for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                    z[j4-3] = d + z[j4-2];
                    if (d < 0.0) {
                        return;
                    }
                    else {
                        z[j4-1] = z[j4] * (z[j4-2]/z[j4-3]);
                        d = z[j4] * (d/z[j4-3]) - tau;
                    }
                    dmin[0] = Math.min(dmin[0], d);
                    emin = Math.min(emin, z[j4-1]);
                } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
            } // if (pp == 0)
            else { // pp != 0
                for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                    z[j4-4] = d + z[j4-1];
                    if (d < 0.0) {
                        return;
                    }
                    else {
                        z[j4-2] = z[j4+1] * (z[j4-1]/z[j4-4]);
                        d = z[j4+1] * (d/z[j4-4]) - tau;
                    }
                    dmin[0] = Math.min(dmin[0], d);
                    emin = Math.min(emin, z[j4-2]);
                } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
            } // else pp != 0
            
            // Unroll last 2 steps.
            dnm2[0] = d;
            dmin2[0] = dmin[0];
            j4 = 4*(n0-2) - pp;
            j4p2 = j4 + 2*pp - 1;
            z[j4-3] = dnm2[0] + z[j4p2-1];
            if (dnm2[0] < 0.0) {
                return;
            }
            else {
                z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
                dnm1[0] = z[j4p2+1] * (dnm2[0]/z[j4-3]) - tau;
            }
            dmin[0] = Math.min(dmin[0], dnm1[0]);
            
            dmin1[0] = dmin[0];
            j4 = j4 + 4;
            j4p2 = j4 + 2*pp - 1;
            z[j4-3] = dnm1[0] + z[j4p2-1];
            if (dnm1[0] < 0.0) {
                return;
            }
            else {
                z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
                dn[0] = z[j4p2 + 1] * (dnm1[0]/z[j4-3]) - tau;
            }
            dmin[0] = Math.min(dmin[0], dn[0]);
        } // else !ieee
        
        z[j4+1] = dn[0];
        z[4*n0 - pp - 1] = emin;
        return;
    } // dlasq5
    
    /** This is a port of version 3.2 LAPACK routine DLASQ6.
     * 
    *  -- Contributed by Osni Marques of the Lawrence Berkeley National   --
    *  -- Laboratory and Beresford Parlett of the Univ. of California at  --
    *  -- Berkeley                                                        --
    *  -- November 2008                                                   --
    *
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *
    *     .. Scalar Arguments ..
          INTEGER            I0, N0, PP
          DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   Z( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DLASQ6 computes one dqd (shift equal to zero) transform in
    *  ping-pong form, with protection against underflow and overflow.
    *
    *  Arguments
    *  =========
    *
    *  I0    (input) INTEGER
    *        First index.
    *
    *  N0    (input) INTEGER
    *        Last index.
    *
    *  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
    *        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
    *        an extra argument.
    *
    *  PP    (input) INTEGER
    *        PP=0 for ping, PP=1 for pong.
    *
    *  DMIN  (output) DOUBLE PRECISION
    *        Minimum value of d.
    *
    *  DMIN1 (output) DOUBLE PRECISION
    *        Minimum value of d, excluding D( N0 ).
    *
    *  DMIN2 (output) DOUBLE PRECISION
    *        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
    *
    *  DN    (output) DOUBLE PRECISION
    *        d(N0), the last value of d.
    *
    *  DNM1  (output) DOUBLE PRECISION
    *        d(N0-1).
    *
    *  DNM2  (output) DOUBLE PRECISION
    *        d(N0-2).
    */
    private void dlasq6(int i0, int n0, double z[], int pp, double dmin[], double dmin1[],
                        double dmin2[], double dn[], double dnm1[], double dnm2[]) {
        int j4;
        int j4p2;
        double d;
        double emin;
        double safmin;
        double temp;
        
        if ((n0 - i0 - 1) <= 0) {
            return;
        }
        
        safmin = ge.dlamch('S'); // safe minimum
        j4 = 4*i0 + pp - 3;
        emin = z[j4+3];
        d = z[j4-1];
        dmin[0] = d;
        
        if (pp == 0) {
            for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4) {
                z[j4-3] = d + z[j4-2];
                if (z[j4-3] == 0.0) {
                    z[j4-1] = 0.0;
                    d = z[j4];
                    dmin[0] = d;
                    emin = 0.0;
                } // if (z[j4-3] == 0.0)
                else if ((safmin*z[j4] < z[j4-3]) && (safmin*z[j4-3] < z[j4])) {
                    temp = z[j4]/z[j4-3];
                    z[j4-1] = z[j4-2] * temp;
                    d = d * temp;
                } // else if ((safmin*z[j4] < z[j4-3]) && (safmin*z[j4-3] < z[j4]))
                else {
                    z[j4-1] = z[j4] * (z[j4-2]/z[j4-3]);
                    d = z[j4] * (d/z[j4-3]);
                }
                dmin[0] = Math.min(dmin[0], d);
                emin = Math.min(emin, z[j4-1]);
            } // for (j4 = 4*i0; j4 <= 4*(n0-3); j4 += 4)
        } // if (pp == 0)
        else { // pp != 0
            for (j4 = 4*i0; j4 <= 4*(n0 - 3); j4 += 4) {
                z[j4-4] = d + z[j4-1];
                if (z[j4-4] == 0.0) {
                    z[j4-2] = 0.0;
                    d = z[j4+1];
                    dmin[0] = d;
                    emin = 0.0;
                } // if (z[j4-4] == 0.0)
                else if ((safmin*z[j4+1] < z[j4-4]) && (safmin*z[j4-4] < z[j4+1])) {
                    temp = z[j4+1]/z[j4-4];
                    z[j4-2] = z[j4-1] * temp;
                    d = d * temp;
                } // else if ((safmin*z[j4+1] < z[j4-4]) && (safmin*z[j4-4] < z[j4+1]))
                else {
                    z[j4-2] = z[j4+1] * (z[j4-1]/z[j4-4]);
                    d = z[j4+1] * (d/z[j4-4]);
                }
                dmin[0] = Math.min(dmin[0], d);
                emin = Math.min(emin, z[j4-2]);
            } // for (j4 = 4*i0; j4 <= 4*(n0 - 3); j4 += 4)
        } // else pp != 0
        
        // Unroll last 2 steps.
        
        dnm2[0] = d;
        dmin2[0] = dmin[0];
        j4 = 4*(n0-2) - pp;
        j4p2 = j4 + 2*pp - 1;
        z[j4-3] = dnm2[0] + z[j4p2-1];
        if (z[j4-3] == 0.0) {
            z[j4-1] = 0.0;
            dnm1[0] = z[j4p2+1];
            dmin[0] = dnm1[0];
            emin = 0.0;
        } // if (z[j4-3] == 0.0)
        else if ((safmin*z[j4p2+1] < z[j4-3]) && (safmin*z[j4-3] < z[j4p2+1])) {
            temp = z[j4p2+1]/z[j4-3];
            z[j4-1] = z[j4p2-1] * temp;
            dnm1[0] = dnm2[0] * temp;
        } // else if ((safmin*z[j4p2+1] < z[j4-3]) && (safmin*z[j4-3] < z[j4p2+1]))
        else {
            z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
            dnm1[0] = z[j4p2+1] * (dnm2[0]/z[j4-3]);
        }
        dmin[0] = Math.min(dmin[0], dnm1[0]);
        
        dmin1[0] = dmin[0];
        j4 = j4 + 4;
        j4p2 = j4 + 2*pp - 1;
        z[j4-3] = dnm1[0] + z[j4p2-1];
        if (z[j4-3] == 0.0) {
            z[j4-1] = 0.0;
            dn[0] = z[j4p2+1];
            dmin[0] = dn[0];
            emin = 0.0;
        } // if (z[j4-3] == 0.0)
        else if ((safmin*z[j4p2+1] < z[j4-3]) && (safmin*z[j4-3] < z[j4p2+1])) {
            temp = z[j4p2+1]/z[j4-3];
            z[j4-1] = z[j4p2-1]*temp;
            dn[0] = dnm1[0] * temp;
        } // else if ((safmin*z[j4p2+1] < z[j4-3]) && (safmin*z[j4-3] < z[j4p2+1]))
        else {
            z[j4-1] = z[j4p2+1] * (z[j4p2-1]/z[j4-3]);
            dn[0] = z[j4p2+1] * (dnm1[0]/z[j4-3]);
        }
        dmin[0] = Math.min(dmin[0], dn[0]);
        
        z[j4+1] = dn[0];
        z[4*n0 - pp - 1] = emin;
        return;
    } // dlasq6

    
    /**  This is a port of version 3.2 LAPACK auxiliary routine DLAS2.  Original DLAS2 created by 
     *   Univ. of Tennessee, Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., 
     *   November, 2006
    *  Purpose
    *  =======
    *
    *  DLAS2  computes the singular values of the 2-by-2 matrix
    *     [  F   G  ]
    *     [  0   H  ].
    *  On return, SSMIN is the smaller singular value and SSMAX is the
    *  larger singular value.
    *
    *  Arguments
    *  =========
    *
    *  F       (input) DOUBLE PRECISION
    *          The (1,1) element of the 2-by-2 matrix.
    *
    *  G       (input) DOUBLE PRECISION
    *          The (1,2) element of the 2-by-2 matrix.
    *
    *  H       (input) DOUBLE PRECISION
    *          The (2,2) element of the 2-by-2 matrix.
    *
    *  SSMIN   (output) DOUBLE PRECISION
    *          The smaller singular value.
    *
    *  SSMAX   (output) DOUBLE PRECISION
    *          The larger singular value.
    *
    *  Further Details
    *  ===============
    *
    *  Barring over/underflow, all output quantities are correct to within
    *  a few units in the last place (ulps), even in the absence of a guard
    *  digit in addition/subtraction.
    *
    *  In IEEE arithmetic, the code works correctly if one matrix element is
    *  infinite.
    *
    *  Overflow will not occur unless the largest singular value itself
    *  overflows, or is within a few ulps of overflow. (On machines with
    *  partial overflow, like the Cray, overflow may occur if the largest
    *  singular value is within a factor of 2 of overflow.)
    *
    *  Underflow is harmless if underflow is gradual. Otherwise, results
    *  may correspond to a matrix modified by perturbations of size near
    *  the underflow threshold.
    */
    private void dlas2(double f[], double g[], double h[], double ssmin[], double ssmax[]) {
        double as;
        double at;
        double au;
        double c;
        double fa;
        double fhmn;
        double fhmx;
        double ga;
        double ha;
        double temp;
        
        fa = Math.abs(f[0]);
        ga = Math.abs(g[0]);
        ha = Math.abs(h[0]);
        fhmn = Math.min(fa, ha);
        fhmx = Math.max(fa, ha);
        if (fhmn == 0.0) {
            ssmin[0] = 0.0;
            if (fhmx == 0.0) {
                ssmax[0] = ga;
            }
            else {
                temp = Math.min(fhmx, ga)/Math.max(fhmx, ga);
                ssmax[0] = Math.max(fhmx, ga) * Math.sqrt(1.0 + temp*temp);
             }
        } // if (fhmn == 0.0)
        else { // fhmn != 0.0
            if (ga < fhmx) {
                as = 1.0 + fhmn/fhmx;
                at = (fhmx - fhmn)/fhmx;
                au = ga/fhmx;
                au = au * au;
                c = 2.0/(Math.sqrt(as*as+au) + Math.sqrt(at*at+au));
                ssmin[0] = fhmn * c;
                ssmax[0] = fhmx/c;
            } // if (ga < fhmx)
            else { // ga >= fhmx
                au = fhmx/ga;
                if (au == 0.0) {
                    // Avoid possible harmful underflow if exponent range
                    // asymmetric (true ssmin[0] may not underflow even if 
                    // au underflows.)
                    ssmin[0] = (fhmn * fhmx) / ga;
                    ssmax[0] = ga;
                } // if (au == 0.0)
                else { // au != 0.0
                    as = 1.0 + fhmn/fhmx;
                    at = (fhmx - fhmn)/fhmx;
                    c = 1.0/(Math.sqrt(1.0 + (as*au)*(as*au)) + Math.sqrt(1.0 + (at*au)*(at*au)));
                    ssmin[0] = (fhmn*c)*au;
                    ssmin[0] = ssmin[0] + ssmin[0];
                    ssmax[0] = ga / (c+c);
                } // else au != 0.0
            } // else ga >= fhmx
        } // else fhmn != 0.0
        return;
    } // dlas2
    
    

    
    /** This is a port of version 3.2 LAPACK routine DORGBR.  Original DORGBR created by Univ. of Tennessee,
     *  Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
    *  Purpose
    *  =======
    *
    *  DORGBR generates one of the real orthogonal matrices Q or P**T
    *  determined by DGEBRD when reducing a real matrix A to bidiagonal
    *  form: A = Q * B * P**T.  Q and P**T are defined as products of
    *  elementary reflectors H(i) or G(i) respectively.
    *
    *  If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
    *  is of order M:
    *  if m >= k, Q = H(1) H(2) . . . H(k) and DORGBR returns the first n
    *  columns of Q, where m >= n >= k;
    *  if m < k, Q = H(1) H(2) . . . H(m-1) and DORGBR returns Q as an
    *  M-by-M matrix.
    *
    *  If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
    *  is of order N:
    *  if k < n, P**T = G(k) . . . G(2) G(1) and DORGBR returns the first m
    *  rows of P**T, where n >= m >= k;
    *  if k >= n, P**T = G(n-1) . . . G(2) G(1) and DORGBR returns P**T as
    *  an N-by-N matrix.
    *
    *  Arguments
    *  =========
    *
    *  VECT    (input) CHARACTER*1
    *          Specifies whether the matrix Q or the matrix P**T is
    *          required, as defined in the transformation applied by DGEBRD:
    *          = 'Q':  generate Q;
    *          = 'P':  generate P**T.
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix Q or P**T to be returned.
    *          M >= 0.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix Q or P**T to be returned.
    *          N >= 0.
    *          If VECT = 'Q', M >= N >= min(M,K);
    *          if VECT = 'P', N >= M >= min(N,K).
    *
    *  K       (input) INTEGER
    *          If VECT = 'Q', the number of columns in the original M-by-K
    *          matrix reduced by DGEBRD.
    *          If VECT = 'P', the number of rows in the original K-by-N
    *          matrix reduced by DGEBRD.
    *          K >= 0.
    *
    *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
    *          On entry, the vectors which define the elementary reflectors,
    *          as returned by DGEBRD.
    *          On exit, the M-by-N matrix Q or P**T.
    *
    *  LDA     (input) INTEGER
    *          The leading dimension of the array A. LDA >= max(1,M).
    *
    *  TAU     (input) DOUBLE PRECISION array, dimension
    *                                (min(M,K)) if VECT = 'Q'
    *                                (min(N,K)) if VECT = 'P'
    *          TAU(i) must contain the scalar factor of the elementary
    *          reflector H(i) or G(i), which determines Q or P**T, as
    *          returned by DGEBRD in its array argument TAUQ or TAUP.
    *
    *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
    *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
    *
    *  LWORK   (input) INTEGER
    *          The dimension of the array WORK. LWORK >= max(1,min(M,N)).
    *          For optimum performance LWORK >= min(M,N)*NB, where NB
    *          is the optimal blocksize.
    *
    *          If LWORK = -1, then a workspace query is assumed; the routine
    *          only calculates the optimal size of the WORK array, returns
    *          this value as the first entry of the WORK array, and no error
    *          message related to LWORK is issued by XERBLA.
    *
    *  INFO    (output) INTEGER
    *          = 0:  successful exit
    *          < 0:  if INFO = -i, the i-th argument had an illegal value
    */
    private void dorgbr(char vect, int m, int n, int k, double A[][], int lda, double tau[],
                        double work[], int lwork, int info[]) {
        boolean lquery;
        boolean wantq;
        int i;
        int iinfo[] = new int[1];
        int j;
        int lwkopt = 0;
        int mn;
        int nb;
        double array1[][];
        int p;
        int q;
        
        // Test the input arguments
        info[0] = 0;
        wantq = ((vect == 'Q' ) || (vect == 'q'));
        mn = Math.min(m, n);
        lquery = (lwork == -1);
        if ((!wantq) && (vect != 'P') && (vect != 'p')) {
            info[0] = -1;
        }
        else if (m < 0) {
            info[0] = -2;
        }
        else if ((n < 0) || (wantq && ((n > m) || (n < Math.min(m, k)))) ||
                ((!wantq) && ((m > n) || (m < Math.min(n, k))))) {
            info[0] = -3;
        }
        else if (k < 0) {
            info[0] = -4;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -6;
        }
        else if ((lwork < Math.max(1, mn)) && (!lquery)) {
            info[0] = -9;
        }
        
        if (info[0] == 0) {
            if (wantq) {
                nb = ge.ilaenv(1, "ge.dorgqr", " ", m, n, k, -1);
            }
            else {
                nb = ge.ilaenv(1, "DORGLQ", " ", m, n, k, -1);
            }
            lwkopt = Math.max(1, mn) * nb;
            work[0] = lwkopt;
        } // if (info[0] == 0)
        
        if (info[0] != 0) {
            MipavUtil.displayError("Error dorgbr had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        if ((m == 0) || (n == 0)) {
            work[0] = 1;
            return;
        }
        
        if (wantq) {
            // Form Q, determined by a call to dgebrd to reduce an m-by-k matrix
            if (m >= k) {
                // If m >= k, assume m >= n >= k
                ge.dorgqr(m, n, k, A, lda, tau, work, lwork, iinfo);
            } // if (m >= k)
            else { // m < k
                // If m < k, assume m = n
                
                // Shift the vectors which define the elementary reflectors one 
                // column to the right, and set the first row and column of Q
                // to those of the unit matrix
                
                for (j = m; j >= 2; j--) {
                    A[0][j-1] = 0.0;
                    for (i = j+1; i <= m; i++) {
                        A[i-1][j-1] = A[i-1][j-2];
                    } // for (i = j+1; i <= m; i++)
                } // for (j = m; j >= 2; j--)
                A[0][0] = 1.0;
                for (i = 2; i <= m; i++) {
                    A[i-1][0] = 0.0;
                }
                if (m > 1) {
                    // Form Q(2:m,2:m)
                    array1 = new double[m-1][m-1];
                    for (p = 0; p < m-1; p++) {
                        for (q = 0; q < m-1; q++) {
                            array1[p][q] = A[1 + p][1+q];
                        }
                    }
                    ge.dorgqr(m-1, m-1, m-1, array1, m-1, tau, work, lwork, iinfo);
                    for (p = 0; p < m-1; p++) {
                        for (q = 0; q < m-1; q++) {
                            A[1 + p][1+q] = array1[p][q];
                        }
                    }
                } // if (m > 1)
            } //  else m < k
        } // if (wantq)
        else { // (!wantq)
            // Form P', determined by a call to dgebrd to reduce a k-by-n matrix
            if (k < n) {
                // If k < n, assume k <= m <= n
                dorglq(m, n, k, A, lda, tau, work, lwork, iinfo);
            }
            else { // k >= n
                // If k >= n, assume m = n
                
                // Shift the vectors which define the elementary reflectors one
                // row downward, and set the first row and column of P' to
                // those of the unit matrix
                A[0][0] = 1.0;
                for (i = 1; i < n; i++) {
                    A[i][0] = 0.0;
                }
                for (j = 2; j <= n; j++) {
                    for (i = j - 1; i >= 2; i--) {
                        A[i-1][j-1] = A[i-2][j-1];
                    } // for (i = j - 1; i >= 2; i--)
                    A[0][j-1] = 0.0;
                } // for (j = 2; j <= n; j++)
                if (n > 1) {
                    // Form P'(2:n,2:n)
                    array1 = new double[n-1][n-1];
                    for (p = 0; p < n-1; p++) {
                        for (q = 0; q < n-1; q++) {
                            array1[p][q] = A[1+p][1+q];
                        }
                    }
                    dorglq(n-1, n-1, n-1, array1, n-1, tau, work, lwork, iinfo);
                    for (p = 0; p < n-1; p++) {
                        for (q = 0; q < n-1; q++) {
                            A[1+p][1+q] = array1[p][q];
                        }
                    }
                } // if (n > 1)
            } // else k >= n
        } // else (!wantq)
        work[0] = lwkopt;
        return;
    } // dorgbr 
    
     // ge.dorg2r

    
    /** This is a port of version 3.2 LAPACK routine DORGLQ.  Original DORGLQ created by Univ. of Tennessee,
     *  Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
    *  Purpose
    *  =======
    *
    *  DORGLQ generates an M-by-N real matrix Q with orthonormal rows,
    *  which is defined as the first M rows of a product of K elementary
    *  reflectors of order N
    *
    *        Q  =  H(k) . . . H(2) H(1)
    *
    *  as returned by DGELQF.
    *
    *  Arguments
    *  =========
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix Q. M >= 0.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix Q. N >= M.
    *
    *  K       (input) INTEGER
    *          The number of elementary reflectors whose product defines the
    *          matrix Q. M >= K >= 0.
    *
    *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
    *          On entry, the i-th row must contain the vector which defines
    *          the elementary reflector H(i), for i = 1,2,...,k, as returned
    *          by DGELQF in the first k rows of its array argument A.
    *          On exit, the M-by-N matrix Q.
    *
    *  LDA     (input) INTEGER
    *          The first dimension of the array A. LDA >= max(1,M).
    *
    *  TAU     (input) DOUBLE PRECISION array, dimension (K)
    *          TAU(i) must contain the scalar factor of the elementary
    *          reflector H(i), as returned by DGELQF.
    *
    *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
    *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
    *
    *  LWORK   (input) INTEGER
    *          The dimension of the array WORK. LWORK >= max(1,M).
    *          For optimum performance LWORK >= M*NB, where NB is
    *          the optimal blocksize.
    *
    *          If LWORK = -1, then a workspace query is assumed; the routine
    *          only calculates the optimal size of the WORK array, returns
    *          this value as the first entry of the WORK array, and no error
    *          message related to LWORK is issued by XERBLA.
    *
    *  INFO    (output) INTEGER
    *          = 0:  successful exit
    *          < 0:  if INFO = -i, the i-th argument has an illegal value
    */
    private void dorglq(int m, int n, int k, double A[][], int lda, double tau[], double work[],
                        int lwork, int info[]) {
        boolean lquery;
        int i;
        int ib;
        int iinfo[] = new int[1];
        int iws;
        int j;
        int ki = 0;
        int kk;
        int L;
        int ldwork = 0;
        int lwkopt;
        int nb; 
        int nbmin;
        int nx;
        String name;
        String opts;
        double v1[];
        int row1;
        double array1[][];
        int p;
        int q;
        double work2[][];
        double work3[][];
        int row2;
        double array2[][];
        
        // Test the input arguments
        info[0] = 0;
        name = new String("DORGLQ");
        opts = new String(" ");
        nb = ge.ilaenv(1, name, opts, m, n, k, -1);
        lwkopt = Math.max(1, m) * nb;
        work[0] = lwkopt;
        lquery = (lwork == -1);
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < m) {
            info[0] = -2;
        }
        else if ((k < 0) || (k > m)) {
            info[0] = -3;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -5;
        }
        else if ((lwork < Math.max(1, m)) && (!lquery)) {
            info[0] = -8;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("Error dorglq had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        if (m <= 0) {
            work[0] = 1;
            return;
        }
        
        nbmin = 2;
        nx = 0;
        iws = m;
        if ((nb > 1) && (nb < k)) {
            // Determine when to corss over from blocked to unblocked code
            nx = Math.max(0, ge.ilaenv(3, name, opts, m, n, k, -1));
            if (nx < k) {
                // Determine if workspace is large enough for blocked code.
                ldwork = m;
                iws = ldwork * nb;
                if (lwork < iws) {
                    // Not enough workspace to use optimal nb: reduce nb and 
                    // determine the minimum value of nb
                    nb = lwork/ldwork;
                    nbmin = Math.max(2, ge.ilaenv(2, name, opts, m, n, k, -1));
                } // if (lwork < iws)
            } // if (nx < k)
        } // if ((nb > 1) && (nb < k))
        
        if ((nb >= nbmin) && (nb < k) && (nx < k)) {
            // Use blocked code after the last block.
            // The first kk rows are handled by the block method.
            ki = ((k-nx-1)/nb)*nb;
            kk = Math.min(k, ki+nb);
            
            // Set A(kk+1:m,1:kk) to zero.
            
            for (j = 1; j <= kk; j++) {
                for (i = kk+1; i <= m; i++) {
                    A[i-1][j-1] = 0.0;
                }
            }
        } // if ((nb >= nbmin) && (nb < k) && (nx < k))
        else {
            kk = 0;
        }
        
        // Use unblocked code for the last or only block
        if (kk < m) {
            row1 = Math.max(1, m - kk);
            array1 = new double[row1][n-kk];
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-kk; q++) {
                    array1[p][q] = A[kk+p][kk+q];
                }
            }
            v1 = new double[k-kk];
            for (p = 0; p < k-kk; p++) {
                v1[p] = tau[kk+p];
            }
            dorgl2(m-kk, n-kk, k-kk, array1, row1, v1, work, iinfo);
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-kk; q++) {
                    A[kk+p][kk+q] = array1[p][q];
                }
            }
        } // if (kk < m)
        
        if (kk > 0) {
            // Use blocked code
            for (i = ki+1; i >= 1; i -= nb) {
                ib = Math.min(nb, k-i+1);
                if ((i+ib) <= m) {
                    // Form the triangular factor of the block reflector
                    // H = H(i) H(i+1) ... H(i+ib-1)
                    array1 = new double[ib][n-i+1];
                    for (p = 0; p < ib; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            array1[p][q] = A[i-1+p][i-1+q];
                        }
                    }
                    v1 = new double[ib];
                    for (p = 0; p < ib; p++) {
                        v1[p] = tau[i-1+p];
                    }
                    work2 = new double[ldwork][ib];
                    ge.dlarft('F', 'R', n-i+1, ib, array1, ib, v1, work2, ldwork);
                    for (p = 0; p < ib; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            A[i-1+p][i-1+q] = array1[p][q];
                        }
                    }
                    for (q = 0; q < ib; q++) {
                        for (p = 0; p < ldwork; p++) {
                            work[p + q * ldwork] = work2[p][q];
                        }
                    }
                    
                    array1 = new double[ib][n-i+1];
                    for (p = 0; p < ib; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            array1[p][q] = A[i-1+p][i-1+q];
                        }
                    }
                    row2 = Math.max(1, m-i-ib+1);
                    array2 = new double[row2][n-i+1];
                    for (p = 0; p < row2; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            array2[p][q] = A[i+ib-1+p][i-1+q];
                        }
                    }
                    work3 = new double[row2][ib];
                    ge.dlarfb('R', 'T', 'F', 'R', m-i-ib+1, n-i+1, ib, array1, ib, work2, ldwork,
                            array2, row2, work2, row2);
                    for (p = 0; p < row2; p++) {
                        for (q = 0; q < n-i+1; q++) {
                            A[i+ib-1+p][i-1+q] = array2[p][q];
                        }
                    }
                } // if ((i+ib) <= m)
                
                // Apply H' to columns i:n of current block
                row1 = Math.max(1, ib);
                array1 = new double[row1][n-i+1];
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n-i+1; q++) {
                        array1[p][q] = A[i-1+p][i-1+q];
                    }
                }
                v1 = new double[ib];
                for (p = 0; p < ib; p++) {
                    v1[p] = tau[i-1+p];
                }
                dorgl2(ib, n-i+1, ib, array1, row1, v1, work, iinfo);
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n-i+1; q++) {
                        A[i-1+p][i-1+q] = array1[p][q];
                    }
                }
                
                // Set columns 1:i-1 of current block to zero.
                
                for (j = 1; j <= i-1; j++) {
                    for (L = i; L <= i+ib-1; L++) {
                        A[L-1][j-1] = 0.0;
                    }
                }
            } // for (i = ki+1; i >= 1; i -= nb)
        } // if (kk > 0)
        
        work[0] = iws;
        return;
    } // dorglq

    
    /*  This is a port of version 3.2 LAPACK routine DORGL2.  Original DORGL2 created by Univ. of Tennessee,
     *  Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
    *  Purpose
    *  =======
    *
    *  DORGL2 generates an m by n real matrix Q with orthonormal rows,
    *  which is defined as the first m rows of a product of k elementary
    *  reflectors of order n
    *
    *        Q  =  H(k) . . . H(2) H(1)
    *
    *  as returned by DGELQF.
    *
    *  Arguments
    *  =========
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix Q. M >= 0.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix Q. N >= M.
    *
    *  K       (input) INTEGER
    *          The number of elementary reflectors whose product defines the
    *          matrix Q. M >= K >= 0.
    *
    *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
    *          On entry, the i-th row must contain the vector which defines
    *          the elementary reflector H(i), for i = 1,2,...,k, as returned
    *          by DGELQF in the first k rows of its array argument A.
    *          On exit, the m-by-n matrix Q.
    *
    *  LDA     (input) INTEGER
    *          The first dimension of the array A. LDA >= max(1,M).
    *
    *  TAU     (input) DOUBLE PRECISION array, dimension (K)
    *          TAU(i) must contain the scalar factor of the elementary
    *          reflector H(i), as returned by DGELQF.
    *
    *  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
    *
    *  INFO    (output) INTEGER
    *          = 0: successful exit
    *          < 0: if INFO = -i, the i-th argument has an illegal value
    */
    private void dorgl2(int m, int n, int k, double A[][], int lda, double tau[], double work[], int info[]) {
        int i;
        int j;
        int L;
        double v1[];
        int row1;
        double array1[][];
        int p;
        int q;
        
        // Test the input arguments
        info[0] = 0;
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < m) {
            info[0] = -2;
        }
        else if ((k < 0) || (k > m)) {
            info[0] = -3;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -5;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("Error dorgl2 had info[0] = " + info[0]);
            return;
        }
        
        // Quick return if possible
        if (m <= 0) {
            return;
        }
        
        if (k < m) {
            // Initialize rows k+1:m to rows of the unit matrix
            for (j = 1; j <= n; j++) {
                for (L = k+1; L <= m; L++) {
                    A[L-1][j-1] = 0;
                }
                if ((j > k) && (j <= m)) {
                    A[j-1][j-1] = 1.0;
                }
            } // for (j = 1; j <= n; j++) 
        } // if (k < m)
        
        for (i = k; i >= 1; i--) {
            // Apply H(i) to A(i:m,i;n) from the right
            
            if (i < n) {
                if (i < m) {
                    A[i-1][i-1] = 1.0;
                    v1 = new double[n-i+1];
                    for (p = 0; p < n - i + 1; p++) {
                        v1[p] = A[i-1][i-1+p];
                    }
                    row1 = Math.max(1, m - i);
                    array1 = new double[row1][n-i+1];
                    for (p = 0; p < row1; p++) {
                        for (q = 0; q < n - i + 1; q++) {
                            array1[p][q] = A[i + p][i-1+q];
                        }
                    }
                    ge.dlarf('R', m - i, n - i + 1, v1, 1, tau[i-1], array1, row1, work);
                    for (p = 0; p < row1; p++) {
                        for (q = 0; q < n - i + 1; q++) {
                            A[i + p][i-1+q] = array1[p][q];
                        }
                    }
                } // if (i < m)
                for (p = 0; p < n-i; p++) {
                    A[i-1][i+p] = -tau[i-1] * A[i-1][i+p];
                }
            } // if (i < n)
            A[i-1][i-1] = 1.0 - tau[i-1];
            
            // Set A(i,1:i-1) to zero
            for (L = 1; L <= i-1; L++) {
                A[i-1][L-1] = 0.0;
            }
        } // for (i = k; i >= 1; i--)
        return;
    } // dorgl2
    
    /*
     *  This is a port of version 3.2 LAPACK routine DORMLQ.  Original DORMLQ created by Univ. of Tennessee,
     *  Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
       *  Purpose
       *  =======
       *
       *  DORMLQ overwrites the general real M-by-N matrix C with
       *
       *                  SIDE = 'L'     SIDE = 'R'
       *  TRANS = 'N':      Q * C          C * Q
       *  TRANS = 'T':      Q**T * C       C * Q**T
       *
       *  where Q is a real orthogonal matrix defined as the product of k
       *  elementary reflectors
       *
       *        Q = H(k) . . . H(2) H(1)
       *
       *  as returned by DGELQF. Q is of order M if SIDE = 'L' and of order N
       *  if SIDE = 'R'.
       *
       *  Arguments
       *  =========
       *
       *  SIDE    (input) CHARACTER*1
       *          = 'L': apply Q or Q**T from the Left;
       *          = 'R': apply Q or Q**T from the Right.
       *
       *  TRANS   (input) CHARACTER*1
       *          = 'N':  No transpose, apply Q;
       *          = 'T':  Transpose, apply Q**T.
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix C. M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix C. N >= 0.
       *
       *  K       (input) INTEGER
       *          The number of elementary reflectors whose product defines
       *          the matrix Q.
       *          If SIDE = 'L', M >= K >= 0;
       *          if SIDE = 'R', N >= K >= 0.
       *
       *  A       (input) DOUBLE PRECISION array, dimension
       *                               (LDA,M) if SIDE = 'L',
       *                               (LDA,N) if SIDE = 'R'
       *          The i-th row must contain the vector which defines the
       *          elementary reflector H(i), for i = 1,2,...,k, as returned by
       *          DGELQF in the first k rows of its array argument A.
       *          A is modified by the routine but restored on exit.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A. LDA >= max(1,K).
       *
       *  TAU     (input) DOUBLE PRECISION array, dimension (K)
       *          TAU(i) must contain the scalar factor of the elementary
       *          reflector H(i), as returned by DGELQF.
       *
       *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
       *          On entry, the M-by-N matrix C.
       *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
       *
       *  LDC     (input) INTEGER
       *          The leading dimension of the array C. LDC >= max(1,M).
       *
       *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
       *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
       *
       *  LWORK   (input) INTEGER
       *          The dimension of the array WORK.
       *          If SIDE = 'L', LWORK >= max(1,N);
       *          if SIDE = 'R', LWORK >= max(1,M).
       *          For optimum performance LWORK >= N*NB if SIDE = 'L', and
       *          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
       *          blocksize.
       *
       *          If LWORK = -1, then a workspace query is assumed; the routine
       *          only calculates the optimal size of the WORK array, returns
       *          this value as the first entry of the WORK array, and no error
       *          message related to LWORK is issued by XERBLA.
       *
       *  INFO    (output) INTEGER
       *          = 0:  successful exit
       *          < 0:  if INFO = -i, the i-th argument had an illegal value
       */
    private void dormlq(char side, char trans, int m, int n, int k, double A[][], int lda,
                        double tau[], double C[][], int ldc, double work[], int lwork, int info[]) {
        int nbmax = 64;
        int ldt = nbmax + 1;
        boolean left;
        boolean lquery;
        boolean notran;
        char transt;
        int i;
        int i1;
        int i2;
        int i3;
        int ib;
        int ic = 0;
        int iinfo[] = new int[1];
        int iws;
        int jc = 0;
        int ldwork;
        int lwkopt = 0;
        int mi = 0;
        int nb = 0;
        int nbmin;
        int ni = 0;
        int nq;
        int nw;
        double T[][] = new double[ldt][nbmax];
        String name = null;
        String opts = null;
        char[] optsC = new char[2];
        double array1[][];
        int j;
        int p;
        double v[];
        int cdim;
        double array2[][];
        int row1;
        double work2[][];
        
        // Test the input arguments
        info[0] = 0;
        left = ((side == 'L') || (side == 'l'));
        notran = ((trans == 'N') || (trans == 'n'));
        lquery = (lwork == -1);
        
        // nq is the order of Q and nw is the minimum dimension of work
        
        if (left) {
            nq = m;
            nw = n;
        }
        else {
            nq = n;
            nw = m;
        }
        if ((!left) && (side != 'R') && (side != 'r')) {
            info[0] = -1;
        }
        else if ((!notran) && (trans != 'T') && (trans != 't')) {
            info[0] = -2;
        }
        else if (m < 0) {
            info[0] = -3;
        }
        else if (n < 0) {
            info[0] = -4;
        }
        else if ((k < 0) || (k > nq)) {
            info[0] = -5;
        }
        else if (lda < Math.max(1,k)) {
            info[0] = -7;
        }
        else if (ldc < Math.max(1, m)) {
            info[0] = -10;
        }
        else if ((lwork < Math.max(1, nw)) && (!lquery)) {
            info[0] = -12;
        }
        
        if (info[0] == 0) {
            // Determine the block size.  nb may be at most nbmax, where nbmax
            // is used to define the local array T.
            name = new String("DORMLQ");
            optsC[0] = side;
            optsC[1] = trans;
            opts = new String(optsC);
            nb = Math.min(nbmax, ge.ilaenv(1, name, opts, m, n, k, -1));
            lwkopt = Math.max(1, nw) * nb;
            work[0] = lwkopt;
        } // if (info[0] == 0)
        
        if (info[0] != 0) {
            MipavUtil.displayError("Error dormlq had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        if ((m == 0) || (n == 0) || (k == 0)) {
            work[0] = 1;
            return;
        }
        
        nbmin = 2;
        ldwork = nw;
        if ((nb > 1) && (nb < k)) {
            iws = nw*nb;
            if (lwork < iws) {
                nb = lwork/ldwork;
                nbmin = Math.max(2, ge.ilaenv(2, name, opts, m, n, k, -1));
            }
        } // if ((nb > 1) && (nb < k)) 
        else {
            iws = nw;
        }
        
        if ((nb < nbmin) || (nb >= k)) {
            // Use unblocked code
            dorml2(side, trans, m, n, k, A, lda, tau, C, ldc, work, iinfo);
        } // if (nb < nbmin) || (nb >= k))
        else {
            // Use blocked code
            if ((left && notran) || ((!left) && (!notran))) {
                i1 = 1;
                i2 = k;
                i3 = nb;
            }
            else {
                i1 = ((k-1)/nb)*nb + 1;
                i2 = 1;
                i3 = -nb;
            }
            
            if (left) {
                ni = n;
                jc = 1;
            }
            else {
                mi = m;
                ic = 1;
            }
            
            if (!notran) {
                transt = 'T';
            }
            else {
                transt = 'N';
            }
            
            if (i3 == nb) {
                for (i = i1; i <= i2; i += nb) {
                    ib = Math.min(nb, k-i+1);
                    
                    // Form the triangular factor of the block reflector
                    // H = H(i) H(i+1) ... H(i+ib-1)
                    array1 = new double[ib][nq-i+1];
                    for (j = 0; j < ib; j++) {
                        for (p = 0; p < nq-i+1; p++) {
                            array1[j][p] = A[i-1+j][i-1+p];
                        }
                    }
                    v = new double[ib];
                    for (j = 0; j < ib; j++) {
                        v[j] = tau[i-1+j];
                    }
                    ge.dlarft('F', 'R', nq-i+1, ib, array1, ib, v, T, ldt);
                    for (j = 0; j < ib; j++) {
                        for (p = 0; p < nq-i+1; p++) {
                            A[i-1+j][i-1+p] = array1[j][p];
                        }
                    }
                    
                    if (left) {
                        // H or H' is applied to C(i:m,1:n)
                        mi = m - i + 1;
                        ic = i;
                    }
                    else {
                        // H or H' is applied to C(1:m,i:n)
                        ni = n - i + 1;
                        jc = i;
                    }
                    
                    // Apply H or H'
                    if (left) {
                        cdim = mi;
                    }
                    else {
                        cdim = ni;
                    }
                    array1 = new double[ib][cdim];
                    for (j = 0; j < ib; j++) {
                        for (p = 0; p < cdim; p++) {
                            array1[j][p] = A[i-1+j][i-1+p];
                        }
                    }
                    row1 = Math.max(1, mi);
                    array2 = new double[row1][ni];
                    for (j = 0; j < row1; j++) {
                        for (p = 0; p < ni; p++) {
                            array2[j][p] = C[ic-1+j][jc-1+p];
                        }
                    }
                    work2 = new double[ldwork][ib];
                    ge.dlarfb(side, transt, 'F', 'R', mi, ni, ib, array1, ib, T, ldt, array2, row1,
                           work2, ldwork);
                    for (j = 0; j < row1; j++) {
                        for (p = 0; p < ni; p++) {
                            C[ic-1+j][jc-1+p] = array2[j][p];
                        }
                    }
                } // for (i = i1; i <= i2; i+= nb)
            } // if (i3 == nb)
            else { // i3 == -nb
                for (i = i1; i >= i2; i -= nb) {
ib = Math.min(nb, k-i+1);
                    
                    // Form the triangular factor of the block reflector
                    // H = H(i) H(i+1) ... H(i+ib-1)
                    array1 = new double[ib][nq-i+1];
                    for (j = 0; j < ib; j++) {
                        for (p = 0; p < nq-i+1; p++) {
                            array1[j][p] = A[i-1+j][i-1+p];
                        }
                    }
                    v = new double[ib];
                    for (j = 0; j < ib; j++) {
                        v[j] = tau[i-1+j];
                    }
                    ge.dlarft('F', 'R', nq-i+1, ib, array1, ib, v, T, ldt);
                    for (j = 0; j < ib; j++) {
                        for (p = 0; p < nq-i+1; p++) {
                            A[i-1+j][i-1+p] = array1[j][p];
                        }
                    }
                    
                    if (left) {
                        // H or H' is applied to C(i:m,1:n)
                        mi = m - i + 1;
                        ic = i;
                    }
                    else {
                        // H or H' is applied to C(1:m,i:n)
                        ni = n - i + 1;
                        jc = i;
                    }
                    
                    // Apply H or H'
                    if (left) {
                        cdim = mi;
                    }
                    else {
                        cdim = ni;
                    }
                    array1 = new double[ib][cdim];
                    for (j = 0; j < ib; j++) {
                        for (p = 0; p < cdim; p++) {
                            array1[j][p] = A[i-1+j][i-1+p];
                        }
                    }
                    row1 = Math.max(1, mi);
                    array2 = new double[row1][ni];
                    for (j = 0; j < row1; j++) {
                        for (p = 0; p < ni; p++) {
                            array2[j][p] = C[ic-1+j][jc-1+p];
                        }
                    }
                    work2 = new double[ldwork][ib];
                    ge.dlarfb(side, transt, 'F', 'R', mi, ni, ib, array1, ib, T, ldt, array2, row1,
                           work2, ldwork);
                    for (j = 0; j < row1; j++) {
                        for (p = 0; p < ni; p++) {
                            C[ic-1+j][jc-1+p] = array2[j][p];
                        }
                    }    
                } // for (i = i1; i >= i2; i-= nb)
            } // else i3 == -nb
        } // else
        work[0] = lwkopt;
        return;
    } // dormlq
    
    
    
    /*
     * This is a port of  version 3.2 LAPACK routine DORML2.  Original DORML2 created by Univ. of Tennessee,
     * Univ. ov California Berkeley, Univ. Of Colorado Denver, and NAG Ltd., November, 2006
       *  Purpose
       *  =======
       *
       *  DORML2 overwrites the general real m by n matrix C with
       *
       *        Q * C  if SIDE = 'L' and TRANS = 'N', or
       *
       *        Q'* C  if SIDE = 'L' and TRANS = 'T', or
       *
       *        C * Q  if SIDE = 'R' and TRANS = 'N', or
       *
       *        C * Q' if SIDE = 'R' and TRANS = 'T',
       *
       *  where Q is a real orthogonal matrix defined as the product of k
       *  elementary reflectors
       *
       *        Q = H(k) . . . H(2) H(1)
       *
       *  as returned by DGELQF. Q is of order m if SIDE = 'L' and of order n
       *  if SIDE = 'R'.
       *
       *  Arguments
       *  =========
       *
       *  SIDE    (input) CHARACTER*1
       *          = 'L': apply Q or Q' from the Left
       *          = 'R': apply Q or Q' from the Right
       *
       *  TRANS   (input) CHARACTER*1
       *          = 'N': apply Q  (No transpose)
       *          = 'T': apply Q' (Transpose)
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix C. M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix C. N >= 0.
       *
       *  K       (input) INTEGER
       *          The number of elementary reflectors whose product defines
       *          the matrix Q.
       *          If SIDE = 'L', M >= K >= 0;
       *          if SIDE = 'R', N >= K >= 0.
       *
       *  A       (input) DOUBLE PRECISION array, dimension
       *                               (LDA,M) if SIDE = 'L',
       *                               (LDA,N) if SIDE = 'R'
       *          The i-th row must contain the vector which defines the
       *          elementary reflector H(i), for i = 1,2,...,k, as returned by
       *          DGELQF in the first k rows of its array argument A.
       *          A is modified by the routine but restored on exit.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A. LDA >= max(1,K).
       *
       *  TAU     (input) DOUBLE PRECISION array, dimension (K)
       *          TAU(i) must contain the scalar factor of the elementary
       *          reflector H(i), as returned by DGELQF.
       *
       *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
       *          On entry, the m by n matrix C.
       *          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
       *
       *  LDC     (input) INTEGER
       *          The leading dimension of the array C. LDC >= max(1,M).
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension
       *                                   (N) if SIDE = 'L',
       *                                   (M) if SIDE = 'R'
       *
       *  INFO    (output) INTEGER
       *          = 0: successful exit
       *          < 0: if INFO = -i, the i-th argument had an illegal value
       */
    private void dorml2(char side, char trans, int m, int n, int k, double A[][], int lda, 
                        double tau[], double C[][], int ldc, double work[], int info[]) {
        boolean left;
        boolean notran;
        int i;
        int j;
        int p;
        int i1;
        int i2;
        int i3;
        int ic = 0;
        int jc = 0;
        int mi = 0;
        int ni = 0;
        int nq;
        double aii;
        int dimv;
        double v[];
        int row1;
        double array1[][];
        
        // Test the input arguments
        info[0] = 0;
        left = ((side == 'L') || (side == 'l'));
        notran = ((trans == 'N') || (trans == 'n'));
        
        // nq is of order q
        
        if (left) {
            nq = m;
        }
        else {
            nq = n;
        }
        if ((!left ) && (side != 'R') && (side != 'r')) {
            info[0] = -1;
        }
        else if ((!notran) && (trans != 'T') && (trans != 't')) {
            info[0] = -2;
        }
        else if (m < 0) {
            info[0] = -3;
        }
        else if (n < 0) {
            info[0] = -4;
        }
        else if ((k < 0) || (k > nq)) {
            info[0] = -5;
        }
        else if (lda < Math.max(1,k)) {
            info[0] = -7;
        }
        else if (ldc < Math.max(1, m)) {
            info[0] = -10;
        }
        
        if (info[0] != 0) {
            MipavUtil.displayError("Error dorml2 had info[0] = " + info[0]);
            return;
        }
        
        // Quick return if possible
        if ((m == 0) || (n == 0) || (k == 0)) {
            return;
        }
        
        if ((left && notran) || ((!left) && (!notran))) {
            i1 = 1;
            i2 = k;
            i3 = 1;
        }
        else {
            i1 = k;
            i2 = 1;
            i3 = -1;
        }
        
        if (left) {
            ni = n;
            jc = 1;
        }
        else {
            mi = m;
            ic = 1;
        }
        
        if (i3 == 1) {
            for (i = i1; i <= i2; i++) {
                if (left) {
                    // H(i) is applied to C(i:m,1:n)
                    mi = m - i + 1;
                    ic = i;
                }
                else {
                    // H(i) is applied to C(1:m,i:n)
                    ni = n - i + 1;
                    jc = i;
                }
                
                // Apply H(i)
                
                aii = A[i-1][i-1];
                A[i-1][i-1] = 1.0;
                if (left) {
                    dimv = mi;
                }
                else {
                    dimv = ni;
                }
                v = new double[dimv];
                for (j = 0; j < dimv; j++) {
                    v[j] = A[i-1][i-1+j];
                }
                row1 = Math.max(1, mi);
                array1 = new double[row1][ni];
                for (j = 0; j < row1; j++) {
                    for (p = 0; p < ni; p++) {
                        array1[j][p] = C[ic-1+j][jc-1+p];
                    }
                }
                ge.dlarf(side, mi, ni, v, 1, tau[i-1], array1, row1, work);
                for (j = 0; j < row1; j++) {
                    for (p = 0; p < ni; p++) {
                        C[ic-1+j][jc-1+p] = array1[j][p];
                    }
                }
                A[i-1][i-1] = aii;
            } // for (i = i1; i <= i2; i++)
        } // if (i3 == 1)
        else { // i3 == -1
            for (i = i1; i >= i2; i--) {
                if (left) {
                    // H(i) is applied to C(i:m,1:n)
                    mi = m - i + 1;
                    ic = i;
                }
                else {
                    // H(i) is applied to C(1:m,i:n)
                    ni = n - i + 1;
                    jc = i;
                }
                
                // Apply H(i)
                
                aii = A[i-1][i-1];
                A[i-1][i-1] = 1.0;
                if (left) {
                    dimv = mi;
                }
                else {
                    dimv = ni;
                }
                v = new double[dimv];
                for (j = 0; j < dimv; j++) {
                    v[j] = A[i-1][i-1+j];
                }
                row1 = Math.max(1, mi);
                array1 = new double[row1][ni];
                for (j = 0; j < row1; j++) {
                    for (p = 0; p < ni; p++) {
                        array1[j][p] = C[ic-1+j][jc-1+p];
                    }
                }
                ge.dlarf(side, mi, ni, v, 1, tau[i-1], array1, row1, work);
                for (j = 0; j < row1; j++) {
                    for (p = 0; p < ni; p++) {
                        C[ic-1+j][jc-1+p] = array1[j][p];
                    }
                }
                A[i-1][i-1] = aii;    
            } // for (i = i1; i >= i2; i--)
        } // else i3 == -1
        return;
     } // dorml2
        
    /* This is a port of version 3.2 LAPACK routine DGEBRD.  Original DGEBRD created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
       *  Purpose
       *  =======
       *
       *  DGEBRD reduces a general real M-by-N matrix A to upper or lower
       *  bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
       *
       *  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows in the matrix A.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns in the matrix A.  N >= 0.
       *
       *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
       *          On entry, the M-by-N general matrix to be reduced.
       *          On exit,
       *          if m >= n, the diagonal and the first superdiagonal are
       *            overwritten with the upper bidiagonal matrix B; the
       *            elements below the diagonal, with the array TAUQ, represent
       *            the orthogonal matrix Q as a product of elementary
       *            reflectors, and the elements above the first superdiagonal,
       *            with the array TAUP, represent the orthogonal matrix P as
       *            a product of elementary reflectors;
       *          if m < n, the diagonal and the first subdiagonal are
       *            overwritten with the lower bidiagonal matrix B; the
       *            elements below the first subdiagonal, with the array TAUQ,
       *            represent the orthogonal matrix Q as a product of
       *            elementary reflectors, and the elements above the diagonal,
       *            with the array TAUP, represent the orthogonal matrix P as
       *            a product of elementary reflectors.
       *          See Further Details.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.  LDA >= max(1,M).
       *
       *  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
       *          The diagonal elements of the bidiagonal matrix B:
       *          D(i) = A(i,i).
       *
       *  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
       *          The off-diagonal elements of the bidiagonal matrix B:
       *          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
       *          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
       *
       *  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
       *          The scalar factors of the elementary reflectors which
       *          represent the orthogonal matrix Q. See Further Details.
       *
       *  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
       *          The scalar factors of the elementary reflectors which
       *          represent the orthogonal matrix P. See Further Details.
       *
       *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
       *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
       *
       *  LWORK   (input) INTEGER
       *          The length of the array WORK.  LWORK >= max(1,M,N).
       *          For optimum performance LWORK >= (M+N)*NB, where NB
       *          is the optimal blocksize.
       *
       *          If LWORK = -1, then a workspace query is assumed; the routine
       *          only calculates the optimal size of the WORK array, returns
       *          this value as the first entry of the WORK array, and no error
       *          message related to LWORK is issued by XERBLA.
       *
       *  INFO    (output) INTEGER
       *          = 0:  successful exit
       *          < 0:  if INFO = -i, the i-th argument had an illegal value.
       *
       *  Further Details
       *  ===============
       *
       *  The matrices Q and P are represented as products of elementary
       *  reflectors:
       *
       *  If m >= n,
       *
       *     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
       *
       *  Each H(i) and G(i) has the form:
       *
       *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
       *
       *  where tauq and taup are real scalars, and v and u are real vectors;
       *  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
       *  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
       *  tauq is stored in TAUQ(i) and taup in TAUP(i).
       *
       *  If m < n,
       *
       *     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
       *
       *  Each H(i) and G(i) has the form:
       *
       *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
       *
       *  where tauq and taup are real scalars, and v and u are real vectors;
       *  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
       *  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
       *  tauq is stored in TAUQ(i) and taup in TAUP(i).
       *
       *  The contents of A on exit are illustrated by the following examples:
       *
       *  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
       *
       *    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
       *    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
       *    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
       *    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
       *    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
       *    (  v1  v2  v3  v4  v5 )
       *
       *  where d and e denote diagonal and off-diagonal elements of B, vi
       *  denotes an element of the vector defining H(i), and ui an element of
       *  the vector defining G(i).
       */
    private void dgebrd(int m, int n, double A[][], int lda, double d[], double e[], double tauq[],
                        double taup[], double work[], int lwork, int info[]) {
        int i;
        int iinfo[] = new int[1];
        int j;
        int ldwrkx;
        int ldwrky;
        int lwkopt;
        int minmn;
        int nb;
        int nbmin;
        int nx;
        double ws;
        String name;
        String opts;
        boolean lquery;
        int row1;
        int row3;
        double array1[][];
        double array2[][];
        double array3[][];
        int k;
        double v1[];
        double v2[];
        double v3[];
        double v4[];
        double work1[][];
        double work2[][];
        int dimv;
        
        // Test the input parameters
        info[0] = 0;
        name = new String("DGEBRD");
        opts = new String(" ");
        nb = Math.max(1, ge.ilaenv(1, name, opts, m, n, -1, -1));
        lwkopt = (m + n) * nb;
        work[0] = lwkopt;
        lquery = (lwork == -1);
        if (m < 0) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -4;
        }
        else if ((lwork < Math.max(1, Math.max(m, n))) && (!lquery)) {
            info[0] = -10;
        }
        
        if (info[0] < 0) {
            MipavUtil.displayError("dgebrd had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        minmn = Math.min(m, n);
        if (minmn == 0) {
            work[0] = 1;
            return;
        }
        
        ws = Math.max(m, n);
        ldwrkx = m;
        ldwrky = n;
        
        if ((nb > 1) && (nb < minmn)) {
            // Set the crossover point nx.
            
            nx = Math.max(nb, ge.ilaenv(3, name, opts, m, n, -1, -1));
            
            // Determine when to switch from blocked to unblocked code.
            if (nx < minmn) {
                ws = (m + n) * nb;
                if (lwork < ws) {
                    // Not enough space for the optimal nb, consider using a smaller block size.
                    
                    nbmin = ge.ilaenv(2, name, opts, m, n, -1, -1);
                    if (lwork >= (m+n)*nbmin) {
                        nb = lwork/(m+n);
                    }
                    else {
                        nb = 1;
                        nx = minmn;
                    }
                } // if (lwork < ws)
            } // if (nx < minmn)
        } // if ((nb > 1) && (nb < minmn))
        else {
            nx = minmn;
        }
     
        for (i = 1; i <= minmn - nx; i += nb) {
            // Reduce rows and column i:i+nb-1 to bidiagonal form and return the matrices X and Y 
            // which are needed to update the unreduced part of the matrix
            row1 = Math.max(1,m-i+1);
            array1 = new double[row1][n-i+1];
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i+1; k++) {
                    array1[j][k] = A[i-1+j][i-1+k];
                }
            }
            v1 = new double[nb];
            v2 = new double[nb];
            v3 = new double[nb];
            v4 = new double[nb];
            work1 = new double[ldwrkx][nb];
            work2 = new double[ldwrky][nb];
            dlabrd(m-i+1, n-i+1, nb, array1, row1, v1, v2, v3, v4, work1, ldwrkx, work2, ldwrky);
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i+1; k++) {
                    A[i-1+j][i-1+k] = array1[j][k];
                }
            }
            for (j = 0; j < nb; j++) {
                d[i-1+j] = v1[j];
                e[i-1+j] = v2[j];
                tauq[i-1+j] = v3[j];
                taup[i-1+j] = v4[j];
            }
            for (j = 0; j < ldwrkx; j++) {
                for (k = 0; k < nb; k++) {
                    work[j + k*ldwrkx] = work1[j][k];
                }
            }
            for (j = 0; j < ldwrky; j++) {
                for (k = 0; k < nb; k++) {
                    work[j + k*ldwrky + ldwrkx*nb] = work2[j][k];
                }
            }
            
            // Update the trailing submatrix A(i+nb:m,i+nb:n) using an update of the form
            // A = A - V*Y' - X*U'
            
            row1 = Math.max(1, m-i-nb+1);
            array1 = new double[row1][nb];
            for (j = 0; j < row1; j++) {
                for (k = 0; k < nb; k++) {
                    array1[j][k] = A[i+nb-1+j][i-1+k];
                }
            }
            for (j = 0; j < ldwrky; j++) {
                for (k = 0; k < nb; k++) {
                    work2[j][k] = work[j + k*ldwrky + ldwrkx*nb+nb];
                }
            }
            array2 = new double[row1][n-i-nb+1];
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i-nb+1; k++) {
                    array2[j][k] = A[i+nb-1+j][i+nb-1+k];
                }
            }
            ge.dgemm('N', 'T', m-i-nb+1, n-i-nb+1, nb, -1.0, array1, row1, work2, ldwrky, 1.0,
                  array2, row1);
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i-nb+1; k++) {
                    A[i+nb-1+j][i+nb-1+k] = array2[j][k];
                }
            }
            
            for (j = 0; j < ldwrkx; j++) {
                for (k = 0; k < nb; k++) {
                    work1[j][k] = work[j + k*ldwrkx + nb];
                }
            }
            row3 = Math.max(1, nb);
            array3 = new double[row3][n-i-nb+1];
            for (j = 0; j < row3; j++) {
                for (k = 0; k < n-i-nb+1; k++) {
                    array3[j][k] = A[i-1+j][i+nb-1+k];
                }
            }
            ge.dgemm('N', 'N', m-i-nb+1, n-i-nb+1, nb, -1.0, work1, ldwrkx, array3, row3, 1.0,
                  array2, row1);
            for (j = 0; j < row1; j++) {
                for (k = 0; k < n-i-nb+1; k++) {
                    A[i+nb-1+j][i+nb-1+k] = array2[j][k];
                }
            }
            
            // Copy diagonal and off-diagonal elemnts of B back into A
            if (m >= n) {
                for (j = i; j <= i+nb-1; j++) {
                    A[j-1][j-1] = d[j-1];
                    A[j-1][j] = e[j-1];
                }
            } // if (m >= n)
            else {
                for (j = i; j <= i + nb - 1; j++) {
                    A[j-1][j-1] = d[j-1];
                    A[j][j-1] = e[j-1];
                }
            }
            
        } // for (i = 1; i <= minmn - nx; i += nb)
        // Use unblocked code to reduce the remainder of the matrix
        row1 = Math.max(1, m-i+1);
        array1 = new double[row1][n-i+1];
        for (j = 0; j < row1; j++) {
            for (k = 0; k < n-i+1; k++) {
                array1[j][k] = A[i-1+j][i-1+k];
            }
        }
        dimv = Math.min(m-i+1,n-i+1);
        v1 = new double[dimv];
        v2 = new double[dimv-1];
        v3 = new double[dimv];
        v4 = new double[dimv];
        dgebd2(m-i+1, n-i+1, array1, row1, v1, v2, v3, v4, work, iinfo);
        for (j = 0; j < row1; j++) {
            for (k = 0; k < n-i+1; k++) {
                A[i-1+j][i-1+k] = array1[j][k];
            }
        }
        for (j = 0; j < dimv; j++) {
            d[i-1+j] = v1[j];
            tauq[i-1+j] = v3[j];
            taup[i-1+j] = v4[j];
        }
        for (j = 0; j < dimv - 1; j++) {
            e[i-1+j] = v2[j];
        }
        work[0] = ws;
        return;
    } // dgebrd

    
    /* This is a port of the version 3.2 LAPACK auxiliary routine DLABRD.  Original DLABRD created by 
     * Univ. of Tennessee, Univ. Of California Berkeley, Univ. Of Colorado Denver, and NAG Ltd.,
     * November, 2006
     
       *  Purpose
       *  =======
       *
       *  DLABRD reduces the first NB rows and columns of a real general
       *  m by n matrix A to upper or lower bidiagonal form by an orthogonal
       *  transformation Q' * A * P, and returns the matrices X and Y which
       *  are needed to apply the transformation to the unreduced part of A.
       *
       *  If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
       *  bidiagonal form.
       *
       *  This is an auxiliary routine called by DGEBRD
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows in the matrix A.
       *
       *  N       (input) INTEGER
       *          The number of columns in the matrix A.
       *
       *  NB      (input) INTEGER
       *          The number of leading rows and columns of A to be reduced.
       *
       *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
       *          On entry, the m by n general matrix to be reduced.
       *          On exit, the first NB rows and columns of the matrix are
       *          overwritten; the rest of the array is unchanged.
       *          If m >= n, elements on and below the diagonal in the first NB
       *            columns, with the array TAUQ, represent the orthogonal
       *            matrix Q as a product of elementary reflectors; and
       *            elements above the diagonal in the first NB rows, with the
       *            array TAUP, represent the orthogonal matrix P as a product
       *            of elementary reflectors.
       *          If m < n, elements below the diagonal in the first NB
       *            columns, with the array TAUQ, represent the orthogonal
       *            matrix Q as a product of elementary reflectors, and
       *            elements on and above the diagonal in the first NB rows,
       *            with the array TAUP, represent the orthogonal matrix P as
       *            a product of elementary reflectors.
       *          See Further Details.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.  LDA >= max(1,M).
       *
       *  D       (output) DOUBLE PRECISION array, dimension (NB)
       *          The diagonal elements of the first NB rows and columns of
       *          the reduced matrix.  D(i) = A(i,i).
       *
       *  E       (output) DOUBLE PRECISION array, dimension (NB)
       *          The off-diagonal elements of the first NB rows and columns of
       *          the reduced matrix.
       *
       *  TAUQ    (output) DOUBLE PRECISION array dimension (NB)
       *          The scalar factors of the elementary reflectors which
       *          represent the orthogonal matrix Q. See Further Details.
       *
       *  TAUP    (output) DOUBLE PRECISION array, dimension (NB)
       *          The scalar factors of the elementary reflectors which
       *          represent the orthogonal matrix P. See Further Details.
       *
       *  X       (output) DOUBLE PRECISION array, dimension (LDX,NB)
       *          The m-by-nb matrix X required to update the unreduced part
       *          of A.
       *
       *  LDX     (input) INTEGER
       *          The leading dimension of the array X. LDX >= M.
       *
       *  Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)
       *          The n-by-nb matrix Y required to update the unreduced part
       *          of A.
       *
       *  LDY     (input) INTEGER
       *          The leading dimension of the array Y. LDY >= N.
       *
       *  Further Details
       *  ===============
       *
       *  The matrices Q and P are represented as products of elementary
       *  reflectors:
       *
       *     Q = H(1) H(2) . . . H(nb)  and  P = G(1) G(2) . . . G(nb)
       *
       *  Each H(i) and G(i) has the form:
       *
       *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
       *
       *  where tauq and taup are real scalars, and v and u are real vectors.
       *
       *  If m >= n, v(1:i-1) = 0, v(i) = 1, and v(i:m) is stored on exit in
       *  A(i:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+1:n) is stored on exit in
       *  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
       *
       *  If m < n, v(1:i) = 0, v(i+1) = 1, and v(i+1:m) is stored on exit in
       *  A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i:n) is stored on exit in
       *  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
       *
       *  The elements of the vectors v and u together form the m-by-nb matrix
       *  V and the nb-by-n matrix U' which are needed, with X and Y, to apply
       *  the transformation to the unreduced part of the matrix, using a block
       *  update of the form:  A := A - V*Y' - X*U'.
       *
       *  The contents of A on exit are illustrated by the following examples
       *  with nb = 2:
       *
       *  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
       *
       *    (  1   1   u1  u1  u1 )           (  1   u1  u1  u1  u1  u1 )
       *    (  v1  1   1   u2  u2 )           (  1   1   u2  u2  u2  u2 )
       *    (  v1  v2  a   a   a  )           (  v1  1   a   a   a   a  )
       *    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
       *    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
       *    (  v1  v2  a   a   a  )
       *
       *  where a denotes an element of the original matrix which is unchanged,
       *  vi denotes an element of the vector defining H(i), and ui an element
       *  of the vector defining G(i).
       */
    private void dlabrd(int m, int n, int nb, double A[][], int lda, double d[], double e[],
                        double tauq[], double taup[], double X[][], int ldx, double Y[][], int ldy) {
        int i;
        int row1;
        double array1[][];
        double vector1[];
        double vector2[];
        int j;
        int k;
        double alpha[] = new double[1];
        double tau[] = new double[1];
        
        // Quick return if possible
        if ((m <= 0) || (n <= 0)) {
            return;
        }
        
        if (m >= n) {
            // Reduce to upper bidiagonal form
            for (i = 1; i <= nb; i++) {
                // Update A(i:m,i)
                row1 = Math.max(1,m-i+1);
                array1 = new double[row1][i-1];
                for (j = 0; j < row1; j++) {
                    for (k = 0; k < i-1; k++) {
                        array1[j][k] = A[i-1 + j][k];
                    }
                }
                vector1 = new double[i-1];
                for (j = 0; j < i-1; j++) {
                    vector1[j] = Y[i-1][j];
                }
                vector2 = new double[m-i+1];
                for (j = 0; j < m-i+1; j++) {
                    vector2[j] = A[i-1+j][i-1];
                }
                ge.dgemv('N', m-i+1, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                for (j = 0; j < m-i+1; j++) {
                    A[i-1+j][i-1] = vector2[j];
                }
                
                for (j = 0; j < row1; j++) {
                    for (k = 0; k < i-1; k++) {
                        array1[j][k] = X[i-1 + j][k];
                    }
                }
                for (j = 0; j < i-1; j++) {
                    vector1[j] = A[j][i-1];
                }
                ge.dgemv('N', m-i+1, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                for (j = 0; j < m-i+1; j++) {
                    A[i-1+j][i-1] = vector2[j];
                }
                
                // Generate reflection Q(i) to annihilate A(i+1:m,i)
                alpha[0] = A[i-1][i-1];
                vector1 = new double[m-i];
                for (j = 0; j < m-i; j++) {
                    vector1[j] = A[Math.min(i,m-1) + j][i-1];
                }
                ge.dlarfg(m-i+1, alpha, vector1, 1, tau);
                A[i-1][i-1] = alpha[0];
                for (j = 0; j < m-i; j++) {
                    A[Math.min(i,m-1) + j][i-1] = vector1[j];
                }
                tauq[i-1] = tau[0];
                
                d[i-1] = A[i-1][i-1];
                if (i < n) {
                    A[i-1][i-1] = 1.0;
                    
                    // Compute Y(i+1:n,i)
                    row1 = Math.max(1,m-i+1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i-1+j][i+k];
                        }
                    }
                    vector1 = new double[m-i+1];
                    for (j = 0; j < m-i+1; j++) {
                        vector1[j] = A[i-1+j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    ge.dgemv('T', m-i+1, n-i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = A[i-1+j][k];
                        }
                    }
                    for (j = 0; j < m-i+1; j++) {
                        vector1[j] = A[i-1+j][i-1];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = Y[j][i-1];
                    }
                    ge.dgemv('T', m-i+1, i-1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        Y[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1,n-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = Y[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = Y[j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    ge.dgemv('N', n-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i+1);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = X[i-1+j][k];
                        }
                    }
                    vector1 = new double[m-i+1];
                    for (j = 0; j < m-i+1; j++) {
                        vector1[j] = A[i-1+j][i-1];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = Y[j][i-1];
                    }
                    ge.dgemv('T', m-i+1, i-1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        Y[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, i-1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[j][i+k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = Y[j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    ge.dgemv('T', i-1, n-i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = Y[i+j][i-1];
                    }
                    ge.dscal(n-i, tauq[i-1], vector1, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector1[j];
                    }
                    
                    // update A(i,i+1:n)
                    row1 = Math.max(1,n-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = Y[i+j][k];
                        }
                    }
                    vector1 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector1[j] = A[i-1][j];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = A[i-1][i+j];
                    }
                    ge.dgemv('N', n-i, i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        A[i-1][i+j] = vector2[j];
                    }
                    
                    row1 = Math.max(1, i-1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[j][i+k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = X[i-1][j];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = A[i-1][i+j];
                    }
                    ge.dgemv('T', i-1, n-i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        A[i-1][i+j] = vector2[j];
                    }
                    
                    // Generate reflection P(I) to annihilate A(i,i+2:n)
                    alpha[0] = A[i-1][i];
                    vector1 = new double[n-i-1];
                    for (j = 0; j < n-i-1; j++) {
                        vector1[j] = A[i-1][Math.min(i+1, n-1) + j];
                    }
                    ge.dlarfg(n-i, alpha, vector1, 1, tau);
                    A[i-1][i] = alpha[0];
                    for (j = 0; j < n-i-1; j++) {
                        A[i-1][Math.min(i+1, n-1) + j] = vector1[j];
                    }
                    taup[i-1] = tau[0];
                    
                    e[i-1] = A[i-1][i];
                    A[i-1][i] = 1.0;
                    
                    // Compute X(i+1:m,i)
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i+j][i+k];
                        }
                    }
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = A[i-1][i+j];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    ge.dgemv('N', m-i, n-i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    row1 = Math.max(1, n-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = Y[i+j][k];
                        }
                    }
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = A[i-1][i+j];
                    }
                    vector2 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector2[j] = X[j][i-1];
                    }
                    ge.dgemv('T', n-i, i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i; j++) {
                        X[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = A[i+j][k];
                        }
                    }
                    vector1 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector1[j] = X[j][i-1];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    ge.dgemv('N', m-i, i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    row1 = Math.max(1, i-1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[j][i+k];
                        }
                    }
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = A[i-1][i+j];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = X[j][i-1];
                    }
                    ge.dgemv('N', i-1, n-i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        X[j][i-1] = vector2[j];
                    }
                    
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = X[i+j][i-1];
                    }
                    ge.dscal(m-i, taup[i-1], vector1, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector1[j];
                    }
                } // if (i < n)
            } // for (i = 1; i <= nb; i++)
        } // if (m >= n)
        else { // m < n
            // Reduce to lower bidiagonal form
            for (i = 1; i <= nb; i++) {
                // Update A(i,i:n)
                
                row1 = Math.max(1, n-i+1);
                array1 = new double[row1][i-1];
                for (j = 0; j < row1; j++) {
                    for (k = 0; k < i-1; k++) {
                        array1[j][k] = Y[i-1+j][k];
                    }
                }
                vector1 = new double[i-1];
                for (j = 0; j < i-1; j++) {
                    vector1[j] = A[i-1][j];
                }
                vector2 = new double[n-i+1];
                for (j = 0; j < n-i+1; j++) {
                    vector2[j] = A[i-1][i-1+j];
                }
                ge.dgemv('N', n-i+1, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                for (j = 0; j < n-i+1; j++) {
                    A[i-1][i-1+j] = vector2[j];
                }
                
                row1 = Math.max(1, i-1);
                array1 = new double[row1][n-i+1];
                for (j = 0; j < row1; j++) {
                    for (k = 0; k < n-i+1; k++) {
                        array1[j][k] = A[j][i-1+k];
                    }
                }
                vector1 = new double[i-1];
                for (j = 0; j < i-1; j++) {
                    vector1[j] = X[i-1][j];
                }
                vector2 = new double[n-i+1];
                for (j = 0; j < n-i+1; j++) {
                    vector2[j] = A[i-1][i-1+j];
                }
                ge.dgemv('T', i-1, n-i+1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                for (j = 0; j < n-i+1; j++) {
                    A[i-1][i-1+j] = vector2[j];
                }
                
                // Generate reflection P(i) to annihilate A(i,i+1:n)
                alpha[0] = A[i-1][i-1];
                vector1 = new double[n-i];
                for (j = 0; j < n-i; j++) {
                    vector1[j] = A[i-1][Math.min(i, n-1) + j];
                }
                ge.dlarfg(n-i+1, alpha, vector1, 1, tau);
                A[i-1][i-1] = alpha[0];
                for (j = 0; j < n-i; j++) {
                    A[i-1][Math.min(i, n-1) + j] = vector1[j];
                }
                taup[i-1] = tau[0];
                
                d[i-1] = A[i-1][i-1];
                if (i < m) {
                    A[i-1][i-1] = 1.0;
                    
                    // Compute X(i+1:m,i)
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][n-i+1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i+1; k++) {
                            array1[j][k] = A[i+j][i-1+k];
                        }
                    }
                    vector1 = new double[n-i+1];
                    for (j = 0; j < n-i+1; j++) {
                        vector1[j] = A[i-1][i-1+j];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    ge.dgemv('N', m-i, n-i+1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1,n-i+1);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = Y[i-1+j][k];
                        }
                    }
                    vector1 = new double[n-i+1];
                    for (j = 0; j < n-i+1; j++) {
                        vector1[j] = A[i-1][i-1+j];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = X[j][i-1];
                    }
                    ge.dgemv('T', n-i+1, i-1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        X[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1,m-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = A[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = X[j][i-1];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    ge.dgemv('N', m-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, i-i);
                    array1 = new double[row1][n-i+1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i+1; k++) {
                            array1[j][k] = A[j][i-1+k];
                        }
                    }
                    vector1 = new double[n-i+1];
                    for (j = 0; j < n-i+1; j++) {
                        vector1[j] = A[i-1][i-1+j];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = X[j][i-1];
                    }
                    ge.dgemv('N', i-1, n-i+1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        X[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = X[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = X[j][i-1];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = X[i+j][i-1];
                    }
                    ge.dgemv('N', m-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector2[j];
                    }
                    
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = X[i+j][i-1];
                    }
                    ge.dscal(m-i, taup[i-1], vector1, 1);
                    for (j = 0; j < m-i; j++) {
                        X[i+j][i-1] = vector1[j];
                    } 
                    
                    // Update A(i+1:m,i)
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = A[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = Y[i-1][j];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = A[i+j][i-1];
                    }
                    ge.dgemv('N', m-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        A[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = X[i+j][k];
                        }
                    }
                    vector1 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector1[j] = A[j][i-1];
                    }
                    vector2 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector2[j] = A[i+j][i-1];
                    }
                    ge.dgemv('N', m-i, i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < m-i; j++) {
                        A[i+j][i-1] = vector2[j];
                    }
                    
                    // Generate refletion Q(i) to annihilate A(i+2:m,i)
                    alpha[0] = A[i][i-1];
                    vector1 = new double[m-i-1];
                    for (j = 0; j < m-i-1; j++) {
                        vector1[j] = A[Math.min(i+1, m-1) + j][i-1];
                    }
                    ge.dlarfg(m-i, alpha, vector1, 1, tau);
                    A[i][i-1] = alpha[0];
                    for (j = 0; j < m-i-1; j++) {
                        A[Math.min(i+1, m-1) + j][i-1] = vector1[j];
                    }
                    tauq[i-1] = tau[0];
                    
                    e[i-1] = A[i][i-1];
                    A[i][i-1] = 1.0;
                    
                    // Compute Y(i+1:n,i)
                    
                    row1  = Math.max(1,m-i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i+j][i+k];
                        }
                    }
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = A[i+j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    ge.dgemv('T', m-i, n-i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1,m-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = A[i+j][k];
                        }
                    }
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = A[i+j][i-1];
                    }
                    vector2 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector2[j] = Y[j][i-1];
                    }
                    ge.dgemv('T', m-i, i-1, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i-1; j++) {
                        Y[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, n-i);
                    array1 = new double[row1][i-1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i-1; k++) {
                            array1[j][k] = Y[i+j][k];
                        }
                    }
                    vector1 = new double[i-1];
                    for (j = 0; j < i-1; j++) {
                        vector1[j] = Y[j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    ge.dgemv('N', n-i, i-1, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < i; k++) {
                            array1[j][k] = X[i+j][k];
                        }
                    }
                    vector1 = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        vector1[j] = A[i+j][i-1];
                    }
                    vector2 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector2[j] = Y[j][i-1];
                    }
                    ge.dgemv('T', m-i, i, 1.0, array1, row1, vector1, 1, 0.0, vector2, 1);
                    for (j = 0; j < i; j++) {
                        Y[j][i-1] = vector2[j];
                    }
                    
                    row1 = Math.max(1, i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[j][i+k];
                        }
                    }
                    vector1 = new double[i];
                    for (j = 0; j < i; j++) {
                        vector1[j] = Y[j][i-1];
                    }
                    vector2 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector2[j] = Y[i+j][i-1];
                    }
                    ge.dgemv('T', i, n-i, -1.0, array1, row1, vector1, 1, 1.0, vector2, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector2[j];
                    }
                    
                    vector1 = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        vector1[j] = Y[i+j][i-1];
                    }
                    ge.dscal(n-i, tauq[i-1], vector1, 1);
                    for (j = 0; j < n-i; j++) {
                        Y[i+j][i-1] = vector1[j];
                    }
                } // if (i < m)
            } // for (i = 1; i <= nb; i++)
        } // else m < n
        return;
    } // dlabrd

    
    /**
     * This is a port of version 3.2 LAPACK routine DGEBD2.  Original DGEBD2 created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * *
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEBD2 reduces a real general m by n matrix A to upper or lower
*  bidiagonal form B by an orthogonal transformation: Q' * A * P = B.
*
*  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n general matrix to be reduced.
*          On exit,
*          if m >= n, the diagonal and the first superdiagonal are
*            overwritten with the upper bidiagonal matrix B; the
*            elements below the diagonal, with the array TAUQ, represent
*            the orthogonal matrix Q as a product of elementary
*            reflectors, and the elements above the first superdiagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors;
*          if m < n, the diagonal and the first subdiagonal are
*            overwritten with the lower bidiagonal matrix B; the
*            elements below the first subdiagonal, with the array TAUQ,
*            represent the orthogonal matrix Q as a product of
*            elementary reflectors, and the elements above the diagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The diagonal elements of the bidiagonal matrix B:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
*          The off-diagonal elements of the bidiagonal matrix B:
*          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
*          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
*
*  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix Q. See Further Details.
*
*  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix P. See Further Details.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(M,N))
*
*  INFO    (output) INTEGER
*          = 0: successful exit.
*          < 0: if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*  If m >= n,
*
*     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
*  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n,
*
*     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
*  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The contents of A on exit are illustrated by the following examples:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
*    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
*    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
*    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
*    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
*    (  v1  v2  v3  v4  v5 )
*
*  where d and e denote diagonal and off-diagonal elements of B, vi
*  denotes an element of the vector defining H(i), and ui an element of
*  the vector defining G(i).
*
*/
    private void dgebd2(int m, int n, double A[][], int lda, double d[], double e[], double tauq[],
                        double taup[], double work[], int info[]) {
        int i;
        double alpha[] = new double[1];
        double x[];
        int j;
        double tau[] = new double[1];
        int row1;
        double array1[][];
        int k;
        
        // Test the input parameters
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
        
        if (info[0] < 0) {
            MipavUtil.displayError("dgebd2 had info[0] = " + info[0]);
            return;
        }
        
        if (m >= n) {
            // Reduce to upper bidiagonal form
            for (i = 1; i <= n; i++) {
                // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
                alpha[0] = A[i-1][i-1];
                x = new double[m-i];
                for (j = 0; j < m-i; j++) {
                    x[j] = A[Math.min(i,m-1) + j][i-1];
                }
                ge.dlarfg(m-i+1, alpha, x, 1, tau);
                A[i-1][i-1] = alpha[0];
                for (j = 0; j < m-i; j++) {
                    A[Math.min(i, m-1) + j][i-1] = x[j];
                }
                tauq[i-1] = tau[0];
                
                d[i-1] = A[i-1][i-1];
                A[i-1][i-1] = 1.0;
                
                // Apply H(i) to A(i:m,i+1:n) from the left
                if (i < n) {
                    x = new double[m-i+1];
                    for (j = 0; j < m-i+1; j++) {
                        x[j] = A[i-1+j][i-1];
                    }
                    row1 = Math.max(1, m-i+1);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i-1+j][i+k];
                        }
                    }
                    ge.dlarf('L', m-i+1, n-i, x, 1, tauq[i-1], array1, row1, work);
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            A[i-1+j][i+k] = array1[j][k];
                        }
                    }
                } // if (i < n)
                A[i-1][i-1] = d[i-1];
                
                if (i < n) {
                    // Generate elementary reflector G(i) to annihilate A(i,i+2:n)
                    alpha[0] = A[i-1][i];
                    x = new double[n-i-1];
                    for (j = 0; j < n-i-1; j++) {
                        x[j] = A[i-1][Math.min(i+1,n-1) + j];
                    }
                    ge.dlarfg(n-i, alpha, x, 1, tau);
                    A[i-1][i] = alpha[0];
                    for (j = 0; j < n-i-1; j++) {
                        A[i-1][Math.min(i+1, n-1) + j] = x[j];
                    }
                    taup[i-1] = tau[0];
                    
                    e[i-1] = A[i-1][i];
                    A[i-1][i] = 1.0;
                    
                    // Apply G(i) to A(i+1:m,i+1:n) from the right
                    x = new double[n-i];
                    for (j = 0; j < n-i; j++) {
                        x[j] = A[i-1][i+j];
                    }
                    row1 = Math.max(1,m-i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i+j][i+k];
                        }
                    }
                    ge.dlarf('R', m-i, n-i, x, 1, taup[i-1], array1, row1, work);
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            A[i+j][i+k] = array1[j][k];
                        }
                    }
                    A[i-1][i] = e[i-1];
                } // if (i < n)
                else {
                    taup[i-1] = 0.0;
                }
            } // for (i = 1; i <= n; i++)
        } // if (m >= n)
        else { // m < n
            // Reduce to lower bidiagonal form
            for (i = 1; i <= m; i++) {
                // Generate elementary reflector G(i) to annihilate A(i,i+1:n)
                alpha[0] = A[i-1][i-1];
                x = new double[n-i];
                for (j = 0; j < n-i; j++) {
                    x[j] = A[i-1][Math.min(i, n-1) + j];
                }
                ge.dlarfg(n-i+1, alpha, x, 1, tau);
                A[i-1][i-1] = alpha[0];
                for (j = 0; j < n-i; j++) {
                    A[i-1][Math.min(i, n-1) + j] = x[j];
                }
                taup[i-1] = tau[0];
                d[i-1] = A[i-1][i-1];
                A[i-1][i-1] = 1.0;
                
                // Apply G(i) to A(i+1:m,i:n) from the right
                if (i < m) {
                    x = new double[n-i+1];
                    for (j = 0; j < n-i+1; j++) {
                        x[j] = A[i-1][i-1+j];
                    }
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][n-i+1];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i+1; k++) {
                            array1[j][k] = A[i+j][i-1+k];
                        }
                    }
                    ge.dlarf('R', m-i, n-i+1, x, 1, taup[i-1], array1, row1, work);
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i+1; k++) {
                            A[i+j][i-1+k] = array1[j][k];
                        }
                    }
                } // if (i < m)
                A[i-1][i-1] = d[i-1];
                if (i < m) {
                    // Generate elementary reflector H(i) to annihilate A(i+2:m,i)
                    alpha[0] = A[i][i-1];
                    x = new double[m-i-1];
                    for (j = 0; j < m-i-1; j++) {
                        x[j] = A[Math.min(i+1,m-1) + j][i-1];
                    }
                    ge.dlarfg(m-i, alpha, x, 1, tau);
                    A[i][i-1] = alpha[0];
                    for (j = 0; j < m-i-1; j++) {
                        A[Math.min(i+1,m-1) + j][i-1] = x[j];
                    }
                    tauq[i-1] = tau[0];
                    
                    e[i-1] = A[i][i-1];
                    A[i][i-1] = 1.0;
                    
                    // Apply H(i) to A(i+1:m,i+1:n) from the left
                    x = new double[m-i];
                    for (j = 0; j < m-i; j++) {
                        x[j] = A[i+j][i-1];
                    }
                    row1 = Math.max(1, m-i);
                    array1 = new double[row1][n-i];
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            array1[j][k] = A[i+j][i+k];
                        }
                    }
                    ge.dlarf('L', m-i, n-i, x, 1, tauq[i-1], array1, row1, work);
                    for (j = 0; j < row1; j++) {
                        for (k = 0; k < n-i; k++) {
                            A[i+j][i+k] = array1[j][k];
                        }
                    }
                    A[i][i-1] = e[i-1];
                } // if (i < m)
                else {
                    tauq[i-1] = 0.0;
                }
            } // for (i = 1; i <= m; i++)
        } // else m < n
    } // dgebd2 
    
    
   // ge.dlarft
    
    /* This is a port of version 3.2 LAPACK routine dormbr.  Original DORMBR created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006.
       *  Purpose
       *  =======
       *
       *  If VECT = 'Q', DORMBR overwrites the general real M-by-N matrix C
       *  with
       *                  SIDE = 'L'     SIDE = 'R'
       *  TRANS = 'N':      Q * C          C * Q
       *  TRANS = 'T':      Q**T * C       C * Q**T
       *
       *  If VECT = 'P', DORMBR overwrites the general real M-by-N matrix C
       *  with
       *                  SIDE = 'L'     SIDE = 'R'
       *  TRANS = 'N':      P * C          C * P
       *  TRANS = 'T':      P**T * C       C * P**T
       *
       *  Here Q and P**T are the orthogonal matrices determined by DGEBRD when
       *  reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
       *  P**T are defined as products of elementary reflectors H(i) and G(i)
       *  respectively.
       *
       *  Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
       *  order of the orthogonal matrix Q or P**T that is applied.
       *
       *  If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
       *  if nq >= k, Q = H(1) H(2) . . . H(k);
       *  if nq < k, Q = H(1) H(2) . . . H(nq-1).
       *
       *  If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
       *  if k < nq, P = G(1) G(2) . . . G(k);
       *  if k >= nq, P = G(1) G(2) . . . G(nq-1).
       *
       *  Arguments
       *  =========
       *
       *  VECT    (input) CHARACTER*1
       *          = 'Q': apply Q or Q**T;
       *          = 'P': apply P or P**T.
       *
       *  SIDE    (input) CHARACTER*1
       *          = 'L': apply Q, Q**T, P or P**T from the Left;
       *          = 'R': apply Q, Q**T, P or P**T from the Right.
       *
       *  TRANS   (input) CHARACTER*1
       *          = 'N':  No transpose, apply Q  or P;
       *          = 'T':  Transpose, apply Q**T or P**T.
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix C. M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix C. N >= 0.
       *
       *  K       (input) INTEGER
       *          If VECT = 'Q', the number of columns in the original
       *          matrix reduced by DGEBRD.
       *          If VECT = 'P', the number of rows in the original
       *          matrix reduced by DGEBRD.
       *          K >= 0.
       *
       *  A       (input) DOUBLE PRECISION array, dimension
       *                                (LDA,min(nq,K)) if VECT = 'Q'
       *                                (LDA,nq)        if VECT = 'P'
       *          The vectors which define the elementary reflectors H(i) and
       *          G(i), whose products determine the matrices Q and P, as
       *          returned by DGEBRD.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.
       *          If VECT = 'Q', LDA >= max(1,nq);
       *          if VECT = 'P', LDA >= max(1,min(nq,K)).
       *
       *  TAU     (input) DOUBLE PRECISION array, dimension (min(nq,K))
       *          TAU(i) must contain the scalar factor of the elementary
       *          reflector H(i) or G(i) which determines Q or P, as returned
       *          by DGEBRD in the array argument TAUQ or TAUP.
       *
       *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
       *          On entry, the M-by-N matrix C.
       *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q
       *          or P*C or P**T*C or C*P or C*P**T.
       *
       *  LDC     (input) INTEGER
       *          The leading dimension of the array C. LDC >= max(1,M).
       *
       *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
       *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
       *
       *  LWORK   (input) INTEGER
       *          The dimension of the array WORK.
       *          If SIDE = 'L', LWORK >= max(1,N);
       *          if SIDE = 'R', LWORK >= max(1,M).
       *          For optimum performance LWORK >= N*NB if SIDE = 'L', and
       *          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
       *          blocksize.
       *
       *          If LWORK = -1, then a workspace query is assumed; the routine
       *          only calculates the optimal size of the WORK array, returns
       *          this value as the first entry of the WORK array, and no error
       *          message related to LWORK is issued by XERBLA.
       *
       *  INFO    (output) INTEGER
       *          = 0:  successful exit
       *          < 0:  if INFO = -i, the i-th argument had an illegal value
       */
    private void dormbr(char vect, char side, char trans, int m, int n, int k, double A[][],
                        int lda, double tau[], double C[][], int ldc, double work[], int lwork,
                        int info[]) {
        boolean applyq;
        boolean left;
        boolean lquery;
        boolean notran;
        char transt;
        int i1;
        int i2;
        int iinfo[] = new int[1];
        int lwkopt = 0;
        int mi;
        int nb;
        int ni;
        int nq;
        int nw;
        String name = null;
        String opts = null;
        char[] optsC = new char[2];
        int row1;
        int col1;
        double array1[][];
        int row2;
        double array2[][];
        int i;
        int j;
        
        // Test the arguments
        info[0] = 0;
        applyq = ((vect == 'Q') || (vect == 'q'));
        left = ((side == 'L') || (side == 'l'));
        notran = ((trans == 'N') || (trans == 'n'));
        lquery = (lwork == -1);
        
        // nq is the order of Q or P and nw is the minimum dimension of work
        
        if (left) {
            nq = m;
            nw = n;
        }
        else {
            nq = n;
            nw = m;
        }
        if ((!applyq) && (vect != 'P') && (vect != 'p')) {
            info[0] = -1;
        }
        else if ((!left) && (side !='R') && (side != 'r')) {
            info[0] = -2;
        }
        else if ((!notran) && (trans != 'T') && (trans != 't')) {
            info[0] = -3;
        }
        else if (m < 0) {
            info[0] = -4;
        }
        else if (n < 0) {
            info[0] = -5;
        }
        else if (k < 0) {
            info[0] = -6;
        }
        else if ((applyq && (lda < Math.max(1, nq))) || ((!applyq) && (lda < Math.max(1, Math.min(nq, k))))) {
            info[0] = -8;
        }
        else if (ldc < Math.max(1, m)) {
            info[0] = -11;
        }
        else if ((lwork < Math.max(1, nw)) && (!lquery)) {
            info[0] = -13;
        }
        
        optsC[0] = side;
        optsC[1] = trans;
        opts = new String(optsC);
        if (info[0] == 0) {
            if (applyq) {
                name = new String("DORMQR");
                if (left) {
                    nb = ge.ilaenv(1, name, opts, m - 1, n, m - 1, -1);    
                } // if (left)
                else {
                    nb = ge.ilaenv(1, name, opts, m, n - 1, n - 1, -1);
                }
            } // if (applyq)
            else { // !applyq
                name = new String("DORMLQ");
                if (left) {
                    nb = ge.ilaenv(1, name, opts, m - 1, n, m - 1, -1);
                } // if (left)
                else {
                    nb = ge.ilaenv(1, name, opts, m, n - 1, n - 1, -1);
                }
            } // else !applyq
            lwkopt = Math.max(1, nw) * nb;
            work[0] = lwkopt;
        } // if (info[0] == 0)
        
        if (info[0] != 0) {
            MipavUtil.displayError("Error dormbr had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
        
        // Quick return if possible
        work[0] = 1;
        if ((m == 0) || (n == 0)) {
            return;
        }
        
        if (applyq) {
            // Apply Q
            if (nq >= k) {
                // Q was determined by a call to dgebrd with nq >= k
                ge.dormqr(side, trans, m, n, k, A, lda, tau, C, ldc, work, lwork, iinfo);
            } // if (nq >= k)
            else if (nq > 1) {
                // Q was determined by a call to dgebrd with nq < k
                if (left) {
                    mi = m - 1;
                    ni = n;
                    i1 = 2;
                    i2 = 1;
                } // if (left)
                else {
                    mi = m;
                    ni = n - 1;
                    i1 = 1;
                    i2 = 2;
                }
                if (left) {
                    row1 = Math.max(1, mi);
                }
                else {
                    row1 = Math.max(1, ni);
                }
                array1 = new double[row1][nq-1];
                for (i = 0; i < row1; i++) {
                    for (j = 0; j < nq - 1; j++) {
                        array1[i][j] = A[1 + i][j];
                    }
                }
                row2 = Math.max(1, mi);
                array2 = new double[row2][ni];
                for (i = 0; i < row2; i++) {
                    for (j = 0; j < ni; j++) {
                        array2[i][j] = C[i1-1+i][i2-1+j];
                    }
                }
                ge.dormqr(side, trans, mi, ni, nq - 1, array1, row1, tau, array2, row2, work, lwork, iinfo);
                for (i = 0; i < row2; i++) {
                    for (j = 0; j < ni; j++) {
                        C[i1-1+i][i2-1+j] = array2[i][j];
                    }
                }
            } // else if (nq > 1)
        } // if (applyq)
        else { // !applyq
            // Apply P
            
            if (notran) {
                transt = 'T';
            }
            else {
                transt = 'N';
            }
            if (nq > k) {
                // P was determined by a call to dgebrd with nq > k
                dormlq(side, transt, m, n, k, A, lda, tau, C, ldc, work, lwork, iinfo);
            } // if (nq > k)
            else if (nq > 1) {
                // P was determined by a call to dgebrd with nq <= k
                
                if (left) {
                    mi = m - 1;
                    ni = n;
                    i1 = 2;
                    i2 = 1;
                } // if (left)
                else {
                    mi = m;
                    ni = n - 1;
                    i1 = 1;
                    i2 = 2;
                }
                row1 = Math.max(1, nq - 1);
                if (left) {
                    col1 = mi;
                }
                else {
                    col1 = ni;
                }
                array1 = new double[row1][col1];
                for (i = 0; i < row1; i++) {
                    for (j = 0; j < col1; j++) {
                        array1[i][j] = A[i][1 + j];
                    }
                }
                row2 = Math.max(1, mi);
                array2 = new double[row2][ni];
                for (i = 0; i < row2; i++) {
                    for (j = 0; j < ni; j++) {
                        array2[i][j] = C[i1-1+i][i2-1+j];
                    }
                }
                dormlq(side, transt, mi, ni, nq - 1, array1, row1, tau, array2, row2, work, lwork, iinfo);
                for (i = 0; i < row2; i++) {
                    for (j = 0; j < ni; j++) {
                        C[i1-1+i][i2-1+j] = array2[i][j];
                    }
                }
            } // else if (nq > 1)
        } // !applyq
        work[0] = lwkopt;
        return;
    } // dormbr
    
    /** This is a port of version 3.2 LAPACK auxiliary routine DRSCL.
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *     November 2006
    *
    *     .. Scalar Arguments ..
          INTEGER            INCX, N
          DOUBLE PRECISION   SA
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   SX( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DRSCL multiplies an n-element real vector x by the real scalar 1/a.
    *  This is done without overflow or underflow as long as
    *  the final result x/a does not overflow or underflow.
    *
    *  Arguments
    *  =========
    *
    *  N       (input) INTEGER
    *          The number of components of the vector x.
    *
    *  SA      (input) DOUBLE PRECISION
    *          The scalar a which is used to divide each component of x.
    *          SA must be >= 0, or the subroutine will divide by zero.
    *
    *  SX      (input/output) DOUBLE PRECISION array, dimension
    *                         (1+(N-1)*abs(INCX))
    *          The n-element vector x.
    *
    *  INCX    (input) INTEGER
    *          The increment between successive values of the vector SX.
    *          > 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n
    */
    private void drscl(int n, double sa, double sx[], int incx) {
        boolean done = false;
        double bignum[] = new double[1];
        double cden;
        double cden1;
        double cnum;
        double cnum1;
        double mul;
        double smlnum[] = new double[1];
        
        // Quick return if possible
        if (n <= 0) {
            return;
        }
        
        // Get machine parameters
        smlnum[0] = ge.dlamch('S');
        bignum[0] = 1.0/smlnum[0];
        ge.dlabad(smlnum, bignum);
        
        // Initialize the denominator to sa and the numerator to 1.
        cden = sa;
        cnum = 1.0;
        
        do {
            cden1 = cden * smlnum[0];
            cnum1 = cnum/bignum[0];
            if ((Math.abs(cden1) > Math.abs(cnum)) && (cnum != 0.0)) {
                // Pre-multiply X by smlnum[0] if cden is large compared to cnum.
                mul = smlnum[0];
                done = false;
                cden = cden1;
            } // if ((Math.abs(cden1) > Math.abs(cnum)) && (cnum != 0.0))
            else if (Math.abs(cnum1) > Math.abs(cden)) {
                // Pre-multiply X by bignum[0] if cden is small compared to cnum.
                mul = bignum[0];
                done = false;
                cnum = cnum1;
            } // else if (Math.abs(cnum1) > Math.abs(cden))
            else {
                // Multiply X by cnum/cden and return.
                mul = cnum/cden;
                done = true;
            } // else
            
            // Scale the vector X by mul.
            ge.dscal(n, mul, sx, incx);
        } while(!done);
        return;
    } // drscl
    
    /** This dchkqr_test routine is a port of a portion of the version 3.1.1 LAPACK test routine DCHKAA by
     * Univ. of Tennessee, Univ. Of California Berkeley and NAG Ltd., January, 2007. and some values from 
     * the test data file dtest.in.
     */
    public void dchkqr_test() {
        
        // Number of values of m
        int nm = 7;
        
        // Values of m (row dimension)
        // dtest.in uses 50 rather than 16
        int[] mval = new int[] { 0, 1, 2, 3, 5, 10, 16 };
        
        // Number of values of n
        int nn = 7;
        
        // Values of n (column dimension)
        // dtest.in uses 50 rather than 16
        int[] nval = new int[] { 0, 1, 2, 3, 5, 10, 16 };
        
        // Number of values of nrhs
        // dchkaa has nns = 1.  dtest.in uses nns = 3.
        int nns = 1;
        
        // Values of nrhs (number of right hand sides)
        // dchkaa uses only 2.  dtest.in uses 1, 2, 15.
        // Since dchkqr only accepts nrhs = nsval[0] use only 2.
        int[] nsval = new int[]{2};
        
        // Number of values of nb
        int nnb = 5;
        
        // Values of nb (the blocksize)
        int nbval[] = new int[] {1, 3, 3, 3, 20};
        
        // Values of nx (crossover point)
        // There are nnb values of nx.
        int nxval[] = new int[] {1, 0, 5, 9, 1};
        
        // Number of values of rank
        int nrank = 3;
        
        // Values of rank (as a % of n)
        int rankval[] = new int[] {30, 50, 90};
        
        // Threshold value of test ratio
        // dchkaa has 20.0, dtest.in has 30.0
        double thresh = 20.0;
        
        // Test the LAPACK routines
        boolean tstchk = true;
        
        // Test the driver routines
        boolean tstdrv = true;
        
        // Test the error exits
        // Passed all 49 exits on test.
        // Put at false so as not to have to hit okay to 49 displayError messages.
        boolean tsterr = false;
        
        // The maximum allowable value for n
        int nmax = 132;
        
        // The number of different values that can be used for each of m, n, nrhs, nb, and nx
        int maxin = 12;
        
        // The maximum number of right hand sides
        int maxrhs = 16;
        int nmats = 8;
        int ntypes = 8;
        int nrhs = nsval[0];
        boolean dotype[] = new boolean[ntypes];
        double A[][] = new double[nmax][nmax];
        double AF[][] = new double[nmax][nmax];
        double AQ[][] = new double[nmax][nmax];
        double AR[][] = new double[nmax][nmax];
        double AC[][] = new double[nmax][nmax];
        double B[][] = new double[nmax][nrhs];
        double X[][] = new double[nmax][nrhs];
        double XACT[][] = new double[nmax][nrhs];
        double tau[] = new double[nmax];
        double work[] = new double[nmax*nmax];
        double rwork[] = new double[nmax];
        int iwork[] = new int[nmax];
        iparms = new int[11];
        int i;
        double eps;
       
        for (i = 0; i < ntypes; i++) {
            dotype[i] = true;
        }
        
        // Output the machine dependent constants
        eps = ge.dlamch('U');
        Preferences.debug("Underflow threshold = " + eps + "\n");
        eps = ge.dlamch('O');
        Preferences.debug("Overflow threshold = " + eps + "\n");
        eps = ge.dlamch('E');
        Preferences.debug("Precision = " + eps + "\n");
        
        dchkqr(dotype, nm, mval, nn, nval, nnb, nbval, nxval, nrhs, thresh, tsterr, nmax, 
               A, AF, AQ, AR, AC, B, X, XACT, tau, work, rwork, iwork);
    } // dchkqr_test
    
    /** This is a port of version 3.1 LAPACK test routine DCHKQR
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     November 2006
    *
    *     .. Scalar Arguments ..
          LOGICAL            TSTERR
          INTEGER            NM, NMAX, NN, NNB, NOUT, NRHS
          DOUBLE PRECISION   THRESH
    *     ..
    *     .. Array Arguments ..
          LOGICAL            DOTYPE( * )
          INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NVAL( * ),
         $                   NXVAL( * )
          DOUBLE PRECISION   A( * ), AC( * ), AF( * ), AQ( * ), AR( * ),
         $                   B( * ), RWORK( * ), TAU( * ), WORK( * ),
         $                   X( * ), XACT( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DCHKQR tests ge.dgeqrf, ge.dorgqr and ge.dormqr.
    *
    *  Arguments
    *  =========
    *
    *  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
    *          The matrix types to be used for testing.  Matrices of type j
    *          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
    *          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
    *
    *  NM      (input) INTEGER
    *          The number of values of M contained in the vector MVAL.
    *
    *  MVAL    (input) INTEGER array, dimension (NM)
    *          The values of the matrix row dimension M.
    *
    *  NN      (input) INTEGER
    *          The number of values of N contained in the vector NVAL.
    *
    *  NVAL    (input) INTEGER array, dimension (NN)
    *          The values of the matrix column dimension N.
    *
    *  NNB     (input) INTEGER
    *          The number of values of NB and NX contained in the
    *          vectors NBVAL and NXVAL.  The blocking parameters are used
    *          in pairs (NB,NX).
    *
    *  NBVAL   (input) INTEGER array, dimension (NNB)
    *          The values of the blocksize NB.
    *
    *  NXVAL   (input) INTEGER array, dimension (NNB)
    *          The values of the crossover point NX.
    *
    *  NRHS    (input) INTEGER
    *          The number of right hand side vectors to be generated for
    *          each linear system.
    *
    *  THRESH  (input) DOUBLE PRECISION
    *          The threshold value for the test ratios.  A result is
    *          included in the output file if RESULT >= THRESH.  To have
    *          every test ratio printed, use THRESH = 0.
    *
    *  TSTERR  (input) LOGICAL
    *          Flag that indicates whether error exits are to be tested.
    *
    *  NMAX    (input) INTEGER
    *          The maximum value permitted for M or N, used in dimensioning
    *          the work arrays.
    *
    *  A       (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX]
    *
    *  AF      (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX]
    *
    *  AQ      (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX]
    *
    *  AR      (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX]
    *
    *  AC      (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX}
    *
    *  B       (workspace) DOUBLE PRECISION array, dimension [NMAX*][NRHS]
    *
    *  X       (workspace) DOUBLE PRECISION array, dimension [NMAX][NRHS]
    *
    *  XACT    (workspace) DOUBLE PRECISION array, dimension [NMAX][NRHS]
    *
    *  TAU     (workspace) DOUBLE PRECISION array, dimension (NMAX)
    *
    *  WORK    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
    *
    *  RWORK   (workspace) DOUBLE PRECISION array, dimension (NMAX)
    *
    *  IWORK   (workspace) INTEGER array, dimension (NMAX)
    */
    private void dchkqr(boolean[] dotype, int nm, int[] mval, int nn, int[] nval, int nnb, int[] nbval,
                        int[] nxval, int nrhs, double thresh, boolean tsterr, int nmax, double[][] A,
                        double[][] AF, double[][] AQ, double[][] AR, double[][] AC, double[][] B, double[][] X,
                        double[][] XACT, double[] tau, double[] work, double[] rwork, int[] iwork) {
        int ntests = 8;
        int ntypes = 8;
        int iseedy[] = new int[] {1988, 1989, 1990, 1991};
        int i;
        int ik;
        int im;
        int imat;
        int in;
        int inb;
        int info[] = new int[1];
        int k;
        int kl[] = new int[1];
        int ku[] = new int[1];
        int lda;
        int lwork;
        int m;
        int minmn;
        int mode[] = new int[1];
        int n;
        int nb;
        int nerrs;
        int nfail;
        int nk;
        int nrun;
        int nt;
        int nx;
        double anorm[] = new double[1];
        double cndnum[] = new double[1];
        int iseed[] = new int[4];
        int kval[] = new int[4];
        double result[] = new double[ntests];
        String path;
        char type[] = new char[1];
        char dist[] = new char[1];
        double res[] = new double[4];
        int p;
        int q;
        double vec1[];
        double resid[] = new double[1];
        
        // Initialize constants and the random number seed
        path = new String("DQR"); // D for double precision
        nrun  = 0;
        nfail = 0;
        nerrs = 0;
        for (i = 0; i < 4; i++) {
            iseed[i] = iseedy[i];
        }
        
        // Test the error exits
        if (tsterr) {
            derrqr();
        }
        infot = 0;
        xlaenv(2, 2);
        
        lda = nmax;
        lwork = nmax * Math.max(nmax, nrhs);
        
        // Do for each value of m in mval.
        
        for (im = 1; im <= nm; im++) {
            m = mval[im-1];
            
            // Do for each value of n in nval.
            for (in = 1; in <= nn; in++) {
                n = nval[in-1];
                minmn = Math.min(m, n);
                for (imat = 1; imat <= ntypes; imat++) {
                    // Do the tests only if dotype[imat-1] is true
                    if (!dotype[imat-1]) {
                        continue;
                    }
                    
                    // Set up the parameters with dlatb4 and generate a test matrix
                    // with dlatms.
                    dlatb4(path, imat, m, n, type, kl, ku, anorm, mode, cndnum, dist);
                    
                    srnamt = new String("DLATMS");
                    dlatms(m, n, dist[0], iseed, type[0], rwork, mode[0], cndnum[0], anorm[0],
                           kl[0], ku[0], 'N', A, lda, work, info);
                    
                    // Check error code from dlamts.
                    if (info[0] != 0) {
                        if ((nfail == 0) && (nerrs == 0)) {
                            Preferences.debug("Path = DQR\n");
                            Preferences.debug("QR decomposition of rectangular matrices\n");
                            Preferences.debug("QR matrix types:\n");
                            Preferences.debug("1. Diagonal\n");
                            Preferences.debug("2. Upper triangular\n");
                            Preferences.debug("3. Lower triangular\n");
                            Preferences.debug("4. Random, cndnum[0] = 2\n");
                            Preferences.debug("5. Random, cndnum[0] = sqrt(1.0/eps)\n");
                            Preferences.debug("6. Random, cndnum[0] = 0.1/eps\n");
                            Preferences.debug("7. Scaled near underflow\n");
                            Preferences.debug("8. Scaled near overflow\n");
                            Preferences.debug("Test ratios:\n");
                            Preferences.debug("1: norm(R - Q' * A)/(M * norm(A) * eps)\n");
                            Preferences.debug("2: norm(I - Q'*Q)/(M * eps)\n");
                            Preferences.debug("3: norm(Q*C - Q*C)/(M * norm(C) * eps)\n");
                            Preferences.debug("4: norm(C*Q - C*Q)/(M * norm(C) * eps)\n");
                            Preferences.debug("5: norm(Q' * C - Q' * C)/(M * norm(C) * eps)\n");
                            Preferences.debug("6: norm(C*Q' - C*Q')/(M * norm(C) * eps)\n");
                            Preferences.debug("7: norm(B - A * X)/(norm(A) * norm(X) * eps)\n");
                            Preferences.debug("8: Diagonal is not non-negative\n");
                        } // if ((nfail == 0) && (nerrs == 0))
                        nerrs++;
                        Preferences.debug("Error code from dlatms is info[0] = " + info +
                                          " for m = " + m + " n = " + n + " type = " + imat + "\n");
                        continue;
                    } // if (info[0] != 0)
                    
                    // Get some values for k: the first value must be minmn,
                    // corresponding to the call of dqrt01; other values are
                    // used in the calls of dqrt02, and must not exceed minmn.
                    kval[0] = minmn;
                    kval[1] = 0;
                    kval[2] = 1;
                    kval[3] = minmn/2;
                    if (minmn == 0) {
                        nk = 1;
                    }
                    else if (minmn == 1) {
                        nk = 2;
                    }
                    else if (minmn <= 3) {
                        nk = 3;
                    }
                    else {
                        nk = 4;
                    }
                    
                    for (ik = 1; ik <= nk; ik++) {
                        k = kval[ik-1];
                        
                        // Do for each pair of values (nb, nx) in nbval and nxval.
                        for (inb = 1; inb <= nnb; inb++) {
                            nb = nbval[inb-1];
                            xlaenv(1, nb);
                            nx = nxval[inb-1];
                            xlaenv(3, nx);
                            for (i = 0; i < ntests; i++) {
                                result[i] = 0.0;
                            }
                            nt = 2;
                            if (ik == 1) {
                                // Test ge.dgeqrf
                                dqrt01(m, n, A, AF, AQ, AR, lda, tau, work, lwork, rwork, result);
                                if (!dgennd(m, n, AF, lda)) {
                                    result[7] = 2*thresh;
                                }
                                nt = nt + 1;
                            } // if (ik == 1)
                            else if (m >= n) {
                                // Test ge.dorgqr, using factorization returned by dqrt01
                                dqrt02(m, n, k, A, AF, AQ, AR, lda, tau, work, lwork, rwork,
                                       result);
                            } // else if (m >= n)
                            if (m >= k) {
                                // Test ge.dormqr, using factorization returned by DQRT01 
                                dqrt03(m, n, k, AF, AC, AR, AQ, lda, tau, work, lwork, rwork, res);
                                for (p = 0; p < 4; p++) {
                                    result[2+p] = res[p];
                                }
                                nt = nt + 4;
                                
                                // If m >= n and k == n, call dgers to solve a system
                                // with nrhs right hand sides and compute the residual.
                                if ((k == n) && (inb == 1)) {
                                    // Generate a solution and set the right hand side.
                                    // Here to dgemm extracted from dlarhs
                                    vec1 = new double[n];
                                    for (p = 1; p <= nrhs; p++) {
                                        dlarnv(2, iseed, n, vec1);
                                        for (q = 0; q < n; q++) {
                                            XACT[q][p-1] = vec1[q];    
                                        }
                                    } // for (p = 1; p <= nrhs; p++)
                                    ge.dgemm('N', 'N', m, nrhs, n, 1.0, A, lda, XACT, lda, 0.0,
                                            B, lda);
                                    ge.dlacpy('F', m, nrhs, B, lda, X, lda);
                                    srnamt = new String("DGEQRS");
                                    ge.dgeqrs(m, n, nrhs, AF, lda, tau, X,
                                           lda, work, lwork, info);
                                    
                                    // Check error code from dgeqrs.
                                    if (info[0] != 0) {
                                        if ((nfail == 0) && (nerrs == 0)) {
                                            Preferences.debug("Path = DQR\n");
                                            Preferences.debug("QR decomposition of rectangular matrices\n");
                                            Preferences.debug("QR matrix types:\n");
                                            Preferences.debug("1. Diagonal\n");
                                            Preferences.debug("2. Upper triangular\n");
                                            Preferences.debug("3. Lower triangular\n");
                                            Preferences.debug("4. Random, cndnum[0] = 2\n");
                                            Preferences.debug("5. Random, cndnum[0] = sqrt(1.0/eps)\n");
                                            Preferences.debug("6. Random, cndnum[0] = 0.1/eps\n");
                                            Preferences.debug("7. Scaled near underflow\n");
                                            Preferences.debug("8. Scaled near overflow\n");
                                            Preferences.debug("Test ratios:\n");
                                            Preferences.debug("1: norm(R - Q' * A)/(M * norm(A) * eps)\n");
                                            Preferences.debug("2: norm(I - Q'*Q)/(M * eps)\n");
                                            Preferences.debug("3: norm(Q*C - Q*C)/(M * norm(C) * eps)\n");
                                            Preferences.debug("4: norm(C*Q - C*Q)/(M * norm(C) * eps)\n");
                                            Preferences.debug("5: norm(Q' * C - Q' * C)/(M * norm(C) * eps)\n");
                                            Preferences.debug("6: norm(C*Q' - C*Q')/(M * norm(C) * eps)\n");
                                            Preferences.debug("7: norm(B - A * X)/(norm(A) * norm(X) * eps)\n");
                                            Preferences.debug("8: Diagonal is not non-negative\n");
                                        } // if ((nfail == 0) && (nerrs == 0))
                                        nerrs++;
                                        Preferences.debug("Error code from dgeqrs info[0] = " + info[0] + "\n");
                                        Preferences.debug("m = " + m + " n = " + n + " nrhs = " + nrhs + "\n");
                                        Preferences.debug("nb = " + nb + " type = " + imat + "\n");
                                    } // if (info[0] != 0)
                                    
                                    dget02('N', m, n, nrhs, A, lda, X, lda, B, lda, rwork, resid);
                                    result[6] = resid[0];
                                    nt++;
                                } // if ((k == n) && (inb == 1))
                            } // if (m >= k)
                            
                            // Output information about the tests that did not pass the threshold.
                            for (i = 1; i <= nt; i++) {
                                if (result[i-1] >= thresh) {
                                    if ((nfail == 0) && (nerrs == 0)) {
                                        Preferences.debug("Path = DQR\n");
                                        Preferences.debug("QR decomposition of rectangular matrices\n");
                                        Preferences.debug("QR matrix types:\n");
                                        Preferences.debug("1. Diagonal\n");
                                        Preferences.debug("2. Upper triangular\n");
                                        Preferences.debug("3. Lower triangular\n");
                                        Preferences.debug("4. Random, cndnum[0] = 2\n");
                                        Preferences.debug("5. Random, cndnum[0] = sqrt(1.0/eps)\n");
                                        Preferences.debug("6. Random, cndnum[0] = 0.1/eps\n");
                                        Preferences.debug("7. Scaled near underflow\n");
                                        Preferences.debug("8. Scaled near overflow\n");
                                        Preferences.debug("Test ratios:\n");
                                        Preferences.debug("1: norm(R - Q' * A)/(M * norm(A) * eps)\n");
                                        Preferences.debug("2: norm(I - Q'*Q)/(M * eps)\n");
                                        Preferences.debug("3: norm(Q*C - Q*C)/(M * norm(C) * eps)\n");
                                        Preferences.debug("4: norm(C*Q - C*Q)/(M * norm(C) * eps)\n");
                                        Preferences.debug("5: norm(Q' * C - Q' * C)/(M * norm(C) * eps)\n");
                                        Preferences.debug("6: norm(C*Q' - C*Q')/(M * norm(C) * eps)\n");
                                        Preferences.debug("7: norm(B - A * X)/(norm(A) * norm(X) * eps)\n");
                                        Preferences.debug("8: Diagonal is not non-negative\n");
                                    } // if ((nfail == 0) && (nerrs == 0))
                                    Preferences.debug("m = " + m + " n = " + n + " k = " + k + "\n");
                                    Preferences.debug("nb = " + nb + " nx = " + nx + " type = " + imat + "\n");
                                    Preferences.debug("Test = " + i + " with result = " + result[i-1] + "\n");
                                    nfail++;
                                } // if (result[i-1] >= thresh)
                            } // for (i = 1; i <= nt; i++)
                            nrun = nrun + nt;
                        } // for (inb = 1; inb <= nnb; inb++)
                    } // for (ik = 1; ik <= nk; ik++)
                } // for (imat = 1; imat <= ntypes; imat++)
            } // for (in = 1; in <= nn; in++)
        } // for (im = 1; im <= nm; im++)
        
        // Output a summary of the results.
        if (nfail > 0) {
            Preferences.debug("In dchkqr " + nfail + " out of " + nrun + " tests failed to pass the threshold\n");
        }
        else {
            Preferences.debug("In dchkqr all " + nrun + " tests run passed the threshold\n");
        }
        if (nerrs > 0) {
            Preferences.debug("In dchkqr " + nerrs + " errors occurred\n");
        }
        return;
    } // dchkqr // dchkqr
    
    /** This is a port of a portion of version 3.1 LAPACK routine DERRQR.
     * *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
     *     November 2006
     */
    private void derrqr() {
        int nmax = 2;
        int i;
        int info[] = new int[1];
        int j;
        double A[][] = new double[nmax][nmax];
        double AF[][] = new double[nmax][nmax];
        double B[][] = new double[nmax][nmax];
        double b[] = new double[nmax];
        double w[] = new double[nmax];
        double x[] = new double[nmax];
        int npass = 49;
        int ntotal = 49;
        
        // Set the variables to innocuous values
        for (j = 1; j <= nmax; j++) {
            for (i = 1; i <= nmax; i++) {
                A[i-1][j-1] = 1.0/(double)(i+j);
                AF[i-1][j-1] = 1.0/(double)(i+j);
                B[i-1][j-1] = 1.0/(double)(i+j);
            }
            b[j-1] = 0.0;
            w[j-1] = 0.0;
            x[j-1] = 0.0;
        }
        
        // Error exits for QR factorization
        
        // ge.dgeqrf
        ge.dgeqrf(-1, 0, A, 1, b, w, 1, info);
        if (info[0] != -1) {
            Preferences.debug("ge.dgeqrf(-1, 0, A, 1, b, w, 1, info) produced info[0] = " + info[0] +
                              " instead of -1\n");
            npass--;
        }
        
        ge.dgeqrf(0, -1, A, 1, b, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dgeqrf(-0, -1, A, 1, b, w, 1, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;    
        }
        
        ge.dgeqrf(2, 1, A, 1, b, w, 1, info);
        if (info[0] != -4) {
            Preferences.debug("ge.dgeqrf(2, 1, A, 1, b, w, 1, info) produced info[0] = " + info[0] +
            " instead of -4\n");
            npass--;    
        }
        
        ge.dgeqrf(1, 2, A, 1, b, w, 1, info);
        if (info[0] != -7) {
            Preferences.debug("ge.dgeqrf(1, 2, A, 1, b, w, 1, info) produced info[0] = " + info[0] +
            " instead of -7\n");
            npass--;    
        }
        
        // ge.dgeqr2
        ge.dgeqr2(-1, 0, A, 1, b, w, info);
        if (info[0] != -1) {
            Preferences.debug("ge.dgeqr2(-1, 0, A, 1, b, w, info) produced info[0] = " + info[0] +
            " instead of -1\n");
            npass--;     
        }
        
        ge.dgeqr2(0, -1, A, 1, b, w, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dgeqr2(0, -1, A, 1, b, w, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;     
        }
        
        ge.dgeqr2(2, 1, A, 1, b, w, info);
        if (info[0] != -4) {
            Preferences.debug("ge.dgeqr2(2, 1, A, 1, b, w, info) produced info[0] = " + info[0] +
            " instead of -4\n");
            npass--;     
        }
        
        // DGEQRS
        ge.dgeqrs(-1, 0, 0, A, 1, x, B, 1, w, 1, info);
        if (info[0] != -1) {
            Preferences.debug("ge.dgeqrs(-1, 0, 0, A, 1, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -1\n");
            npass--;
        }
        
        ge.dgeqrs(0, -1, 0, A, 1, x, B, 1, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dgeqrs(0, -1, 0, A, 1, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -2\n");
            npass--;
        }
        
        ge.dgeqrs(1, 2, 0, A, 2, x, B, 2, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dgeqrs(1, 2, 0, A, 2, x, B, 2, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -2\n");
            npass--;
        }
        
        ge.dgeqrs(0, 0, -1, A, 1, x, B, 1, w, 1, info);
        if (info[0] != -3) {
            Preferences.debug("ge.dgeqrs(0, 0, -1, A, 1, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -3\n");
            npass--;
        }
        
        ge.dgeqrs(2, 1, 0, A, 1, x, B, 2, w, 1, info);
        if (info[0] != -5) {
            Preferences.debug("ge.dgeqrs(2, 1, 0, A, 1, x, B, 2, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -5\n");
            npass--;
        }
        
        ge.dgeqrs(2, 1, 0, A, 2, x, B, 1, w, 1, info);
        if (info[0] != -8) {
            Preferences.debug("ge.dgeqrs(2, 1, 0, A, 2, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -8\n");
            npass--;
        }
        
        ge.dgeqrs(1, 1, 2, A, 1, x, B, 1, w, 1, info);
        if (info[0] != -10) {
            Preferences.debug("ge.dgeqrs(1, 1, 2, A, 1, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -10\n");
            npass--;
        }
        
        // ge.dorgqr
        ge.dorgqr(-1, 0, 0, A, 1, x, w, 1, info);
        if (info[0] != -1) {
            Preferences.debug("ge.dorgqr(-1, 0, 0, A, 1, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -1\n");
            npass--;      
        }
        
        ge.dorgqr(0, -1, 0, A, 1, x, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dorgqr(0, -1, 0, A, 1, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;      
        }
        
        ge.dorgqr(1, 2, 0, A, 1, x, w, 2, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dorgqr(1, 2, 0, A, 1, x, w, 2, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;      
        }
        
        ge.dorgqr(0, 0, -1, A, 1, x, w, 1, info);
        if (info[0] != -3) {
            Preferences.debug("ge.dorgqr(0, 0, -1, A, 1, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -3\n");
            npass--;      
        }
        
        ge.dorgqr(1, 1, 2, A, 1, x, w, 1, info);
        if (info[0] != -3) {
            Preferences.debug("ge.dorgqr(1, 1, 2, A, 1, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -3\n");
            npass--;      
        }
        
        ge.dorgqr(2, 2, 0, A, 1, x, w, 2, info);
        if (info[0] != -5) {
            Preferences.debug("ge.dorgqr(2, 2, 0, A, 1, x, w, 2, info) produced info[0] = " + info[0] +
            " instead of -5\n");
            npass--;      
        }
        
        ge.dorgqr(2, 2, 0, A, 2, x, w, 1, info);
        if (info[0] != -8) {
            Preferences.debug("ge.dorgqr(2, 2, 0, A, 2, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -8\n");
            npass--;      
        }
        
        // ge.dorg2r
        ge.dorg2r(-1, 0, 0, A, 1, x, w, info);
        if (info[0] != -1) {
            Preferences.debug("ge.dorg2r(-1, 0, 0, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -1\n");
            npass--;
        }
        
        ge.dorg2r(0, -1, 0, A, 1, x, w, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dorg2r(0, -1, 0, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;
        }
        
        ge.dorg2r(1, 2, 0, A, 1, x, w, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dorg2r(1, 2, 0, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;
        }
        
        ge.dorg2r(0, 0, -1, A, 1, x, w, info);
        if (info[0] != -3) {
            Preferences.debug("ge.dorg2r(0, 0, -1, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -3\n");
            npass--;
        }
        
        ge.dorg2r(2, 1, 2, A, 2, x, w, info);
        if (info[0] != -3) {
            Preferences.debug("ge.dorg2r(2, 1, 2, A, 2, x, w, info) produced info[0] = " + info[0] +
            " instead of -3\n");
            npass--;
        }
        
        ge.dorg2r(2, 1, 0, A, 1, x, w, info);
        if (info[0] != -5) {
            Preferences.debug("ge.dorg2r(2, 1, 0, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -5\n");
            npass--;
        }
        
        // ge.dormqr
        ge.dormqr('/', 'N', 0, 0, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -1) {
            Preferences.debug("ge.dormqr('/', 'N', 0, 0, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -1\n");
            npass--;
        }
        
        ge.dormqr('L', '/', 0, 0, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dormqr('L', '/', 0, 0, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -2\n");
            npass--;
        }
        
        ge.dormqr('L', 'N', -1, 0, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -3) {
            Preferences.debug("ge.dormqr('L', 'N', -1, 0, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -3\n");
            npass--;
        }
        
        ge.dormqr('L', 'N', 0, -1, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -4) {
            Preferences.debug("ge.dormqr('L', 'N', 0, -1, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -4\n");
            npass--;
        }
        
        ge.dormqr('L', 'N', 0, 0, -1, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -5) {
            Preferences.debug("ge.dormqr('L', 'N', 0, 0, -1, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        ge.dormqr('L', 'N', 0, 1, 1, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -5) {
            Preferences.debug("ge.dormqr('L', 'N', 0, 1, 1, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        ge.dormqr('R', 'N', 1, 0, 1, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -5) {
            Preferences.debug("ge.dormqr('R', 'N', 1, 0, 1, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        ge.dormqr('L', 'N', 2, 1, 0, A, 1, x, AF, 2, w, 1, info);
        if (info[0] != -7) {
            Preferences.debug("ge.dormqr('L', 'N', 2, 1, 0, A, 1, x, AF, 2, w, 1, info) produced info[0] = " +
            info[0] + " instead of -7\n");
            npass--;
        }
        
        ge.dormqr('R', 'N', 1, 2, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -7) {
            Preferences.debug("ge.dormqr('R', 'N', 1, 2, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -7\n");
            npass--;
        }
        
        ge.dormqr('L', 'N', 2, 1, 0, A, 2, x, AF, 1, w, 1, info);
        if (info[0] != -10) {
            Preferences.debug("ge.dormqr('L', 'N', 2, 1, 0, A, 2, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -10\n");
            npass--;
        }
        
        ge.dormqr('L', 'N', 1, 2, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -12) {
            Preferences.debug("ge.dormqr('L', 'N', 1, 2, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -12\n");
            npass--;
        }
        
        ge.dormqr('R', 'N', 2, 1, 0, A, 1, x, AF, 2, w, 1, info);
        if (info[0] != -12) {
            Preferences.debug("ge.dormqr('R', 'N', 2, 1, 0, A, 1, x, AF, 2, w, 1, info) produced info[0] = " +
            info[0] + " instead of -12\n");
            npass--;
        }
        
        // DORM2R
        ge.dorm2r('/', 'N', 0, 0, 0, A, 1, x, AF, 1, w, info);
        if (info[0] != -1) {
            Preferences.debug("ge.dorm2r('/', 'N', 0, 0, 0, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -1\n");
            npass--;
        }
        
        ge.dorm2r('L', '/', 0, 0, 0, A, 1, x, AF, 1, w, info);
        if (info[0] != -2) {
            Preferences.debug("ge.dorm2r('L', '/', 0, 0, 0, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -2\n");
            npass--;
        }
        
        ge.dorm2r('L', 'N', -1, 0, 0, A, 1, x, AF, 1, w, info);
        if (info[0] != -3) {
            Preferences.debug("ge.dorm2r('L', 'N', -1, 0, 0, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -3\n");
            npass--;
        }
        
        ge.dorm2r('L', 'N', 0, -1, 0, A, 1, x, AF, 1, w, info);
        if (info[0] != -4) {
            Preferences.debug("ge.dorm2r('L', 'N', 0, -1, 0, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -4\n");
            npass--;
        }
        
        ge.dorm2r('L', 'N', 0, 0, -1, A, 1, x, AF, 1, w, info);
        if (info[0] != -5) {
            Preferences.debug("ge.dorm2r('L', 'N', 0, 0, -1, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        ge.dorm2r('L', 'N', 0, 1, 1, A, 1, x, AF, 1, w, info);
        if (info[0] != -5) {
            Preferences.debug("ge.dorm2r('L', 'N', 0, 1, 1, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        ge.dorm2r('R', 'N', 1, 0, 1, A, 1, x, AF, 1, w, info);
        if (info[0] != -5) {
            Preferences.debug("ge.dorm2r('R', 'N', 1, 0, 1, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        ge.dorm2r('L', 'N', 2, 1, 0, A, 1, x, AF, 2, w, info);
        if (info[0] != -7) {
            Preferences.debug("ge.dorm2r('L', 'N', 2, 1, 0, A, 1, x, AF, 2, w, info) produced info[0] = " +
            info[0] + " instead of -7\n");
            npass--;
        }
        
        ge.dorm2r('R', 'N', 1, 2, 0, A, 1, x, AF, 1, w, info);
        if (info[0] != -7) {
            Preferences.debug("ge.dorm2r('R', 'N', 1, 2, 0, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -7\n");
            npass--;
        }
        
        ge.dorm2r('L', 'N', 2, 1, 0, A, 2, x, AF, 1, w, info);
        if (info[0] != -10) {
            Preferences.debug("ge.dorm2r('L', 'N', 2, 1, 0, A, 2, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -10\n");
            npass--;
        }
        
        Preferences.debug("derrqr correctly found " + npass + " of " + ntotal + " error exits\n");
        return;
    } // derrqr
    

    /** This is a port of version 3.1 LAPACK auxiliary routine XLAENV.
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     November 2006
    *
    *     .. Scalar Arguments ..
          INTEGER            ISPEC, NVALUE
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  XLAENV sets certain machine- and problem-dependent quantities
    *  which will later be retrieved by ge.ilaenv.
    *
    *  Arguments
    *  =========
    *
    *  ISPEC   (input) INTEGER
    *          Specifies the parameter to be set in the COMMON array IPARMS.
    *          = 1: the optimal blocksize; if this value is 1, an unblocked
    *               algorithm will give the best performance.
    *          = 2: the minimum block size for which the block routine
    *               should be used; if the usable block size is less than
    *               this value, an unblocked routine should be used.
    *          = 3: the crossover point (in a block routine, for N less
    *               than this value, an unblocked routine should be used)
    *          = 4: the number of shifts, used in the nonsymmetric
    *               eigenvalue routines
    *          = 5: the minimum column dimension for blocking to be used;
    *               rectangular blocks must have dimension at least k by m,
    *               where k is given by ge.ilaenv(2,...) and m by ge.ilaenv(5,...)
    *          = 6: the crossover point for the SVD (when reducing an m by n
    *               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
    *               this value, a QR factorization is used first to reduce
    *               the matrix to a triangular form)
    *          = 7: the number of processors
    *          = 8: another crossover point, for the multishift QR and QZ
    *               methods for nonsymmetric eigenvalue problems.
    *          = 9: maximum size of the subproblems at the bottom of the
    *               computation tree in the divide-and-conquer algorithm
    *               (used by xGELSD and xGESDD)
    *          =10: ieee NaN arithmetic can be trusted not to trap
    *          =11: infinity arithmetic can be trusted not to trap
    *
    *  NVALUE  (input) INTEGER
    *          The value of the parameter specified by ISPEC.
    */
    private void xlaenv(int ispec, int nvalue) {
        if ((ispec >= 1) && (ispec <= 9)) {
            iparms[ispec-1] = nvalue;
        }
        return;
    } // xlaenv
    
    /** This is a port of version 3.1 LAPACK test routine DLATB4.
     *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
     *     November 2006
     *
     *     .. Scalar Arguments ..
           CHARACTER          DIST, TYPE
           CHARACTER*3        PATH
           INTEGER            IMAT, KL, KU, M, MODE, N
           DOUBLE PRECISION   ANORM, CNDNUM
     *     ..
     *
     *  Purpose
     *  =======
     *
     *  DLATB4 sets parameters for the matrix generator based on the type of
     *  matrix to be generated.
     *
     *  Arguments
     *  =========
     *
     *  PATH    (input) String
     *          The LAPACK path name.
     *
     *  IMAT    (input) INTEGER
     *          An integer key describing which matrix to generate for this
     *          path.
     *
     *  M       (input) INTEGER
     *          The number of rows in the matrix to be generated.
     *
     *  N       (input) INTEGER
     *          The number of columns in the matrix to be generated.
     *
     *  TYPE    (output) CHARACTER*1
     *          The type of the matrix to be generated:
     *          = 'S':  symmetric matrix
     *          = 'P':  symmetric positive (semi)definite matrix
     *          = 'N':  nonsymmetric matrix
     *
     *  KL      (output) INTEGER
     *          The lower band width of the matrix to be generated.
     *
     *  KU      (output) INTEGER
     *          The upper band width of the matrix to be generated.
     *
     *  ANORM   (output) DOUBLE PRECISION
     *          The desired norm of the matrix to be generated.  The diagonal
     *          matrix of singular values or eigenvalues is scaled by this
     *          value.
     *
     *  MODE    (output) INTEGER
     *          A key indicating how to choose the vector of eigenvalues.
     *
     *  CNDNUM  (output) DOUBLE PRECISION
     *          The desired condition number.
     *
     *  DIST    (output) CHARACTER*1
     *          The type of distribution to be used by the random number
     *          generator.
     */
    private void dlatb4(String path, int imat, int m, int n, char[] type, int[] kl, int[] ku,
                        double anorm[], int[] mode, double[] cndnum, char[] dist) {
        String c2;
        int mat;
        // Set some constants for use in the subroutine.
        if (first_dlatb4) {
            first_dlatb4 = false;
            eps_dlatb4 = ge.dlamch('P'); // Precision
            badc2_dlatb4 = 0.1/eps;
            badc1_dlatb4 = Math.sqrt(badc2_dlatb4);
            small_dlatb4[0] = ge.dlamch('S'); // Safe minimum
            large_dlatb4[0] = 1.0/small_dlatb4[0];
            
            // If it looks like we're on a Cray, take the square root of
            // small_dlatb4 and large_dlatb4 to avoid overflow and underflow problems.
            ge.dlabad(small_dlatb4, large_dlatb4);
            small_dlatb4[0] = 0.25 * (small_dlatb4[0]/eps_dlatb4);
            large_dlatb4[0] = 1.0/small_dlatb4[0];
        } // if (first_dlatb4)
        
        c2 = path.substring(1, 3);
        
        // Set some parameters we don't plan to change.
        
        dist[0] = 'S';
        mode[0] = 3;
        if ((c2.equalsIgnoreCase("QR")) || (c2.equalsIgnoreCase("LQ")) ||
            (c2.equalsIgnoreCase("QL")) || (c2.equalsIgnoreCase("RQ"))) {
            // xQR, xLQ, xQL, xRQ: Set parameters to generate a general m x n matrix.
            // Set type[0], the type of matrix to be generated.
            type[0] = 'N';
            
            // Set the lower and upper bandwidths.
            if (imat == 1) {
                kl[0] = 0;
                ku[0] = 0;
            }
            else if (imat == 2) {
                kl[0] = 0;
                ku[0] = Math.max(n-1, 0);
            }
            else if (imat == 3) {
                kl[0] = Math.max(m-1, 0);
                ku[0] = 0;
            }
            else {
                kl[0] = Math.max(m-1, 0);
                ku[0] = Math.max(n-1, 0);
            }
            
            // Set the condition number and norm.
            if (imat == 5) {
                cndnum[0] = badc1_dlatb4;
            }
            else if (imat == 6) {
                cndnum[0] = badc2_dlatb4;
            }
            else {
                cndnum[0] = 2.0;
            }
            
            if (imat == 7) {
                anorm[0] = small_dlatb4[0];
            }
            else if (imat == 8) {
                anorm[0] = large_dlatb4[0];
            }
            else {
                anorm[0] = 1.0;
            }
        } // if ((c2.equalsIgnoreCase("QR")) || (c2.equalsIgnoreCase("LQ")) ||
        else if (c2.equalsIgnoreCase("GE")) {
            // xGE: Set parameters to generate a general m x n matrix.
            
            // Set type[0], the type of matrix to be generated.
            type[0] = 'N';
            
            // Set the lower and upper bandwidths.
            if (imat == 1) {
                kl[0] = 0;
                ku[0] = 0;
            }
            else if (imat == 2) {
                kl[0] = 0;
                ku[0] = Math.max(n-1, 0);
            }
            else if (imat == 3) {
                kl[0] = Math.max(m-1, 0);
                ku[0] = 0;
            }
            else {
                kl[0] = Math.max(m-1, 0);
                ku[0] = Math.max(n-1, 0);
            }
            
            // Set the condition number and norm.
            if (imat == 8) {
                cndnum[0] = badc1_dlatb4;
            }
            else if (imat == 9) {
                cndnum[0] = badc2_dlatb4;
            }
            else {
                cndnum[0] = 2.0;
            }
            
            if (imat == 10) {
                anorm[0] = small_dlatb4[0];
            }
            else if (imat == 11) {
                anorm[0] = large_dlatb4[0];
            }
            else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("GE"))
        else if (c2.equalsIgnoreCase("GB")) {
            // xGB: Set parameters to generate a general banded matrix.
            
            // Set type[0], the type of matrix to be generated.
            type[0] = 'N';
            
            // Set the condition number and norm
            if (imat == 5) {
                cndnum[0] = badc1_dlatb4;
            }
            else if (imat == 6) {
                cndnum[0] = 0.1 * badc2_dlatb4;
            }
            else {
                cndnum[0] = 2.0;
            }
            
            if (imat == 7) {
                anorm[0] = small_dlatb4[0];
            }
            else if (imat == 8) {
                anorm[0] = large_dlatb4[0];
            }
            else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("GB"))
        else if (c2.equalsIgnoreCase("GT")) {
            // xGT; Set parameters to generate a generate tridiagonal matrix.
            
            // Set type[0], the type of matrix to be generated
            type[0] = 'N';
            
            // Set the lower and upper bandwidths.
            if (imat == 1) {
                kl[0] = 0;
            }
            else {
                kl[0] = 1;
            }
            ku[0] = kl[0];
            
            // Set the condition number and norm.
            if (imat == 3) {
                cndnum[0] = badc1_dlatb4;
            }
            else if (imat == 4) {
                cndnum[0] = badc2_dlatb4;
            }
            else {
                cndnum[0] = 2.0;
            }
            
            if ((imat == 5) || (imat == 11)) {
                anorm[0] = small_dlatb4[0];
            }
            else if ((imat == 6) || (imat == 12)) {
                anorm[0] = large_dlatb4[0];
            }
            else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("GT))
        else if ((c2.equalsIgnoreCase("PO")) || (c2.equalsIgnoreCase("PP")) ||
                 (c2.equalsIgnoreCase("SY")) || (c2.equalsIgnoreCase("SP"))) {
            // xPO, xPP, XSY, xSP: Set parameters to generate a symmetric matrx.
            
            // Set type[0], the type of matrix to be generated
            type[0] = c2.charAt(0);
            
            // Set the lower and upper bandwidths.
            if (imat == 1) {
                kl[0] = 0;
            }
            else {
                kl[0] = Math.max(0, n-1);
            }
            ku[0] = kl[0];
            
            // Set the condition number and norm.
            if (imat == 6) {
                cndnum[0] = badc1_dlatb4;
            }
            else if (imat == 7) {
                cndnum[0] = badc2_dlatb4;
            }
            else {
                cndnum[0] = 2.0;
            }
            
            if (imat == 8) {
                anorm[0] = small_dlatb4[0];
            }
            else if (imat == 9) {
                anorm[0] = large_dlatb4[0];
            }
            else {
                anorm[0] = 1.0;
            }
        } // else if ((c2.equalsIgnoreCase("PO")) || (c2.equalsIgnoreCase("PP")) ||
        else if (c2.equalsIgnoreCase("PB")) {
            // xPB: Set the parameters to generate a symmetric band matrix.
            
            // Set type[0], the type of matrix to be generated.
            type[0] = 'P';
            
            // Set the norm and condition number.
            if (imat == 5) {
                cndnum[0] = badc1_dlatb4;
            }
            else if (imat == 6) {
                cndnum[0] = badc2_dlatb4;
            }
            else {
                cndnum[0] = 2.0;
            }
            
            if (imat == 7) {
                anorm[0] = small_dlatb4[0];
            }
            else if (imat == 8) {
                anorm[0] = large_dlatb4[0];
            }
            else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("PB))
        else if (c2.equalsIgnoreCase("PT")) {
            // xPT: Set parameters to generate a symmetric positive definite tridiagonal matrix.
            
            type[0] = 'P';
            if (imat == 1) {
                kl[0] = 0;
            }
            else {
                kl[0] = 1;
            }
            ku[0] = kl[0];
            
            // Set the condition number and norm.
            if (imat == 3) {
                cndnum[0] = badc1_dlatb4;
            }
            else if (imat == 4) {
                cndnum[0] = badc2_dlatb4;
            }
            else {
                cndnum[0] = 2.0;
            }
            
            if ((imat == 5) || (imat == 11)) {
                anorm[0] = small_dlatb4[0];
            }
            else if ((imat == 6) || (imat == 12)) {
                anorm[0] = large_dlatb4[0];
            }
            else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("PT"))
        else if ((c2.equalsIgnoreCase("TR")) || (c2.equalsIgnoreCase("TP"))) {
            // xTR, xTP: Set parameters to generate a triangular matrix
            
            // Set type[0], the type of matrix to be generated.
            type[0] = 'N';
            
            // Set the lower and upper bandwidths.
            mat = Math.abs(imat);
            if ((mat == 1) || (mat == 7)) {
                kl[0] = 0;
                ku[0] = 0;
            }
            else if (imat < 0) {
                kl[0] = Math.max(0, n-1);
                ku[0] = 0;
            }
            else {
                kl[0] = 0;
                ku[0] = Math.max(0, n-1);
            }
            
            // Set the condition number and norm.
            if ((mat == 3) || (mat == 9)) {
                cndnum[0] = badc1_dlatb4;
            }
            else if ((mat == 4) || (mat == 10)) {
                cndnum[0] = badc2_dlatb4;
            }
            else {
                cndnum[0] = 2.0;
            }
            
            if (mat == 5) {
                anorm[0] = small_dlatb4[0];
            }
            else if (mat == 6) {
                anorm[0] = large_dlatb4[0];
            }
            else {
                anorm[0] = 1.0;
            }
        } // else if ((c2.equalsIgnoreCase("TR")) || (c2.equalsIgnoreCase("TP")))
        else if (c2.equalsIgnoreCase("TB")) {
            // xTB: Set parameters to generate a triangular band matrix.
            
            // Set type[0], the type of matrix to be generated.
            type[0] = 'N';
            
            // Set the norm and condition number.
            if ((imat == 2) || (imat == 8)) {
                cndnum[0] = badc1_dlatb4;
            }
            else if ((imat == 3) || (imat == 9)) {
                cndnum[0] = badc2_dlatb4;
            }
            else {
                cndnum[0] = 2.0;
            }
            
            if (imat == 4) {
                anorm[0] = small_dlatb4[0];
            }
            else if (imat == 5) {
                anorm[0] = large_dlatb4[0];
            }
            else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("TB"))
        
        if (n <= 1) {
            cndnum[0] = 1.0;
        }
        
        return;
    } // dlatb4
    
    /**
     * This is a port of version 3.1 LAPACK test routine DLATMS Original DLATMS created by Univ. of Tennessee, Univ. of
     * California Berkeley, and NAG Ltd., November, 2006
     * dlatms generates random matrices with specified singular values (or symmetric/hermitian with specified
     * eigenvalues) for testing LAPACK programs.
     *
     * <p>dlatms operates by applying the following sequence of operations:</p>
     *
     * <p>Set the diagonal to D, where D may be input or computed according to mode, cond, dmax, and sym as described
     * below.</p>
     *
     * <p>Generate a matrix with appropriate band structure, by one of two methods:</p>
     *
     * <p>Method A: Generate a dense m by n matrix by multiplying D on the left and the right by random unitary
     * matrices, then:</p>
     *
     * <p>Reduce the bandwidth according to kl and ku, using Householder transformations.</p>
     *
     * <p>Method B: Convert the bandwidth-0 (i.e., diagonal) matrix to a bandwidth-1 matrix using Givens rotations,
     * "chasing" out-of-band elements back, much as in QR; then convert the bandwidth-1 to a bandwidth-2 matrix, etc.
     * Note that for reasonably samll bandwidths (relative to m and n) this requires less storage, as a dense matrix is
     * not generated. Also, for symmetric matrices, only, one triangle is generated.</p>
     *
     * <p>Method A is chosen if the bandwidth is a large fraction of the order of the matrix, and lda is at least m (so
     * a dense matrix can be stored.) Method B is chosen if the bandwidth is small ( < 1/2 n for symmetric, < .3 n+m for
     * non-symmetric), or lda is less than m and not less than the bandwidth.</p>
     *
     * <p>Pack the matrix if desired. Options specified by pack are: no packing, zero out upper half (if symmetric), zero
     * out lower half (if symmetric), store the upper half columnwise (if symmetric or upper triangular), store the lower
     * half columnwise (if symmetric or lower triangular), store the lower triangle in banded format (if symmetric or
     * lower triangular), store the upper triangle in banded format (if symmetric or upper triangular), and store the entire
     * matrix in banded format If method B is chosen, and band format is specified, then the matrix will be generated in
     * the band format, so no repacking will be necessary.</p>
     *
     * @param  m      input int The number of rows of A.
     * @param  n      input int The number of columns of A.
     * @param  dist   input char On entry, dist specifies the type of distribution to be used to generate the random
     *                eigen-/singular values. 
     *                'U' => uniform(0,1) ('U' for uniform) 
     *                'S' => uniform(-1,1) ('S' for symmetric) 
     *                'N' => normal(0,1) ('N' for normal)
     * @param  iseed  input/output int[] of dimension 4 On entry iseed specifies the seed of the random number
     *                generator. They should lie between 0 and 4095 inclusive, and iseed[3] should be odd. The random
     *                number generator uses a linear congruential sequence limited to small integers, and so should
     *                produce machine independent random numbers. The values of iseed are changed on exit, and can be
     *                used in the next call to dlatms to continue the same random number sequence. Changed on exit.
     * @param  sym    input char 
     *                If sym = 'S' or 'H', the generated matrix is symmetric, with eigenvalues specified by
     *                D, cond, mode, and dmax; they may be positive, negative, or zero. 
     *                If sym = 'P', the generated matrix is symmetric, with eigenvalues (= singular values) specified
     *                by D, cond, mode, and dmax; they will not be negative. 
     *                If sym = 'N', the generated matrix is nonsymmetric, with singular values specified by D, cond,
     *                mode, and dmax; they will not be negative.
     * @param  D      input/output double[] of dimension (min(m,n)) This array is used to specify the singular values or
     *                eigenvalues of A (see sym, above.) If mode = 0, then D is assumed to contain the
     *                singular/eigenvalues, otherwise they will be computed according to mode, cond, and dmax, and
     *                placed in D. Modified if mode is nonzero.
     * @param  mode   input int On entry this describes how the singular/ eigenvalues are to be specified: 
     *                = 0 means use D as input 
     *                = 1 sets D[0] = 1 and D[1:n-1] = 1.0/cond 
     *                = 2 sets D[0:n-2] = 1 and D[n-1] = 1.0/cond 
     *                = 3 sets D[i] = cond**(-(i)/(n-1)) 
     *                = 4 sets D[i] = 1 - (i)/(n-1)*(1 - 1/cond) 
     *                = 5 sets D to random numbers in the range (1/cond, 1) such that their logarithms are
     *                    uniformly distributed 
     *                = 6 sets D to random numbers from same distribution as the rest of the matrix mode 
     *                < 0 has the same meaning as abs(mode), except that the order of the elements of D is reversed.
     *                    Thus, if mode is positive, D has entries ranging from 1 to 1/cond, if negative,
     *                    from 1/cond to 1.
     *                If sym = 'S' or 'H', and mode is neither 0, 6, nor -6, then the elements of D will also be
     *                multiplied by a random sign (1.e., +1 or -1).
     * @param  cond   input double On entry, this is used as described under mode above. If used, it must be >= 1.
     * @param  dmax   input double If mode is neither -6, 0, nor 6, the contents of D, as computed according to mode and
     *                cond, will be scaled by dmax/max(abs(D[i])); thus the maximum absolute eigen- or singular value
     *                (which is to say the norm) will be abs(dmax). Note that dmax need not be positive: if dmax is
     *                negative (or zero), D will be scaled by a negative number (or zero).
     * @param  kl     input int This specifies the lower bandwidth of the matrix. For example, kl = 0 implies upper
     *                triangular, kl = 1 implies upper Hessenberg, and kl being at least m-1 means that the matrix has
     *                full lower bandwidth. kl must equal ku if the matrix is symmetric.
     * @param  ku     input int This specifies the upper bandwidth of the matrix. For example, ku = 0 implies lower
     *                triangular, ku = 1 implies lower Hessenberg, and ku being at least n-1 means that the matrix has
     *                full upper bandwidth. kl must equal ku if the matrix is symmetric.
     * @param  pack   input char This specifies packing of the matrix as follows: 
     *                'N' => no packing 
     *                'U' => zero out all subdiagonal entries (if symmetric) 
     *                'L' => zero out all superdiagonal entries (if symmetric) 
     *                'C' => store the upper triangle columnwise (only if the matrix is symmetric or upper triangular) 
     *                'R' => store the lower triangle columnwise (only if the matrix is symmetric or lower triangular)
     *                'B' => store the lower triangle in band storage scheme (only if matrix symmetric or lower triangular) 
     *                'Q' => store the upper triangle in band storage scheme (only if matrix symmetric or upper triangular)
     *                'Z' => store the entire matrix in band storage scheme (pivoting can be provided for by using this
     *                option to store A in the trailing rows of the allocated storage) 
     *                
     *                Using these options, the various LAPACK packed and banded storage schemes can be obtained: 
     *                GB - use 'Z' 
     *                PB, SB, or TB - use 'B' or 'Q' 
     *                PP, SP, or TP - use 'C' or 'R' 
     *                If two calls to dlatms differ only in the pack parameter, they will generate mathematically
     *                equivalent matrices
     * @param  A      input/output double[][] of dimension (lda,n) On exit A is the desired test matrix. A is first
     *                generated in full (unpacked) form, and then packed, if so specified by pack. Thus, the first m
     *                elements of the first n columns will always be modified. If pack specifies a packed or banded
     *                storage scheme, all lda elements of the first n columns will be modified; the elements of the
     *                array which do not correspond to elements of the generated matrix are set to zero.
     * @param  lda    input int lda specifies the first dimension of A as declared in the calling program. If pack =
     *                'N', 'U', 'L', 'C', or 'R', then lda must be at least m. If pack = 'B' or 'Q', then lda must be at
     *                least min(kl,m-1) (which is equal to min(ku,n-1)). If pack = 'Z', lda must be large enough to hold
     *                the packed array: min(ku,n-1) + min(kl,m-1) + 1.
     * @param  work   workspace double[] of dimension (3*max(n,m))
     * @param  info   output int[] Error code.  On exit, info[0] will be set to one of the following values:
     *                0 => normal return 
     *                -1 => m negative or unequal to n and sym = 'S', 'H', or 'P' 
     *                -2 => n negative 
     *                -3 => dist illegal string
     *                -5 => sym illegal string 
     *                -7 => mode not in range -6 to 6 
     *                -8 => cond less than 1.0, and mode neither -6, 0, nor 6 
     *                -10 => kl negative 
     *                -11 => ku negative, or sym = 'S' or 'H' and ku not equal to kl 
     *                -12 => pack illegal string, or pack = 'U' or 'L', and sym = 'N'; or pack = 'C' or 'Q' and
     *                sym = 'N' and kl is not zero; or pack = 'R' or 'B' and sym = 'N' and ku is not zero; or pack =
     *                'U', 'L', 'C', 'R', 'B', or 'Q' and m != n. 
     *                -14 => lda is less than m, or pack = 'Z' and lda is less than min(ku,n-1) + min(kl,m-1) + 1. 
     *                1 => Error return from dlatm1 
     *                2 => Cannot scale to dmax (maximum singular value is 0) 
     *                3 => Error return from dlagge or dlagsy
     */
    private void dlatms(int m, int n, char dist, int[] iseed, char sym, double[] D, int mode, double cond, double dmax,
                        int kl, int ku, char pack, double[][] A, int lda, double[] work, int[] info) {
        boolean givens;
        boolean ilextr;
        boolean iltemp;
        boolean topdwn;
        int i;
        int ic;
        int icol = 0;
        int idist;
        int iendch;
        int[] iinfo = new int[1];
        int il;
        int ilda;
        int ioffg;
        int ioffst;
        int ipack;
        int ipackg;
        int ir;
        int ir1;
        int ir2;
        int irow = 0;
        int irsign = 0;
        int iskew;
        int isym;
        int isympk = 0;
        int j;
        int jc;
        int jch;
        int jkl;
        int jku;
        int jr;
        int k;
        int llb;
        int minlda;
        int mnmin;
        int mr;
        int nc;
        int uub;
        double alpha;
        double angle;
        double[] c = new double[1];
        double[] dummy = new double[1];
        double[] extra = new double[1];
        double[] s = new double[1];
        double[] temp = new double[1];
        int length;
        double[] ap;
        int index;

        // Decode and test the input parameters.  Initialize flags & seed.
        info[0] = 0;

        // Quick return if possible
        if ((m == 0) || (n == 0)) {
            return;
        }

        // Decode dist
        if ((dist == 'U') || (dist == 'u')) {
            idist = 1;
        } else if ((dist == 'S') || (dist == 's')) {
            idist = 2;
        } else if ((dist == 'N') || (dist == 'n')) {
            idist = 3;
        } else {
            idist = -1;
        }

        // Decode sym
        if ((sym == 'N') || (sym == 'n')) {
            isym = 1;
            irsign = 0;
        } else if ((sym == 'P') || (sym == 'p')) {
            isym = 2;
            irsign = 0;
        } else if ((sym == 'S') || (sym == 's')) {
            isym = 2;
            irsign = 1;
        } else if ((sym == 'H') || (sym == 'h')) {
            isym = 2;
            irsign = 1;
        } else {
            isym = -1;
        }

        // Decode pack
        if ((pack == 'N') || (pack == 'n')) {
            ipack = 0;
        } else if ((pack == 'U') || (pack == 'u')) {
            ipack = 1;
            isympk = 1;
        } else if ((pack == 'L') || (pack == 'l')) {
            ipack = 2;
            isympk = 1;
        } else if ((pack == 'C') || (pack == 'c')) {
            ipack = 3;
            isympk = 2;
        } else if ((pack == 'R') || (pack == 'r')) {
            ipack = 4;
            isympk = 3;
        } else if ((pack == 'B') || (pack == 'b')) {
            ipack = 5;
            isympk = 3;
        } else if ((pack == 'Q') || (pack == 'q')) {
            ipack = 6;
            isympk = 2;
        } else if ((pack == 'Z') || (pack == 'z')) {
            ipack = 7;
        } else {
            ipack = -1;
        }

        // Set certain internal parameters
        mnmin = Math.min(m, n);
        llb = Math.min(kl, m - 1);
        uub = Math.min(ku, n - 1);
        mr = Math.min(m, n + llb);
        nc = Math.min(n, m + uub);

        if ((ipack == 5) || (ipack == 6)) {
            minlda = uub + 1;
        } else if (ipack == 7) {
            minlda = llb + uub + 1;
        } else {
            minlda = m;
        }

        // Use Givens rotation method if bandwidth small enough, or if lda is
        // too small to store the matrix unpacked.

        givens = false;

        if (isym == 1) {

            if ((llb + uub) < (0.3 * Math.max(1, mr + nc))) {
                givens = true;
            }
        } // if (isym == 1)
        else if ((2 * llb) < m) {
            givens = true;
        }

        if ((lda < m) && (lda >= minlda)) {
            givens = true;
        }

        // Set info if an error
        if (m < 0) {
            info[0] = -1;
        } else if ((m != n) && (isym != 1)) {
            info[0] = -1;
        } else if (n < 0) {
            info[0] = -2;
        } else if (idist == -1) {
            info[0] = -3;
        } else if (isym == -1) {
            info[0] = -5;
        } else if (Math.abs(mode) > 6) {
            info[0] = -7;
        } else if ((mode != 0) && (Math.abs(mode) != 6) && (cond < 1.0)) {
            info[0] = -8;
        } else if (kl < 0) {
            info[0] = -10;
        } else if ((ku < 0) || ((isym != 1) && (kl != ku))) {
            info[0] = -11;
        } else if ((ipack == -1) || ((isympk == 1) && (isym == 1)) || ((isympk == 2) && (isym == 1) && (kl > 0)) ||
                       ((isympk == 3) && (isym == 1) && (ku > 0)) || ((isympk != 0) && (m != n))) {
            info[0] = -12;
        } else if (lda < Math.max(1, minlda)) {
            info[0] = -14;
        }

        if (info[0] != 0) {
            MipavUtil.displayError("Error dlatms had info[0] = " + info[0]);

            return;
        }

        // Initialize random number generator
        for (i = 0; i < 4; i++) {
            iseed[i] = Math.abs(iseed[i]) % 4096;
        }

        if ((iseed[3] % 2) != 1) {
            iseed[3] = iseed[3] + 1;
        }

        // Setup D if indicated
        // Compute D according to cond and mode
        dlatm1(mode, cond, irsign, idist, iseed, D, mnmin, iinfo);

        if (iinfo[0] != 0) {
            info[0] = 1;

            return;
        }

        // Choose Top-Down if D is (apparently) increasing,
        // Bottom-Up if D is (apparently) decreasing
        if (Math.abs(D[0]) <= Math.abs(D[mnmin - 1])) {
            topdwn = true;
        } else {
            topdwn = false;
        }

        if ((mode != 0) && (Math.abs(mode) != 6)) {

            // Scale by dmax
            temp[0] = Math.abs(D[0]);

            for (i = 1; i < mnmin; i++) {
                temp[0] = Math.max(temp[0], Math.abs(D[i]));
            }

            if (temp[0] > 0.0) {
                alpha = dmax / temp[0];
            } else {
                info[0] = 2;

                return;
            }

            for (i = 0; i < mnmin; i++) {
                D[i] = alpha * D[i];
            }
        } // if ((mode != 0) && (Math.abs(mode) != 6))

        // Generate Banded Matrix using Givens rotations.
        // Also the special case of uub = llb = 0

        // Compute Addressing constants to cover all storage formats.  Whether
        // GE, SY, GB, or SB, upper or lower triangle or both, the (i,j)-th
        // element is in A[i - iskew*j + ioffst - 1][j - 1];

        if (ipack > 4) {
            ilda = lda - 1;
            iskew = 1;

            if (ipack > 5) {
                ioffst = uub + 1;
            } else {
                ioffst = 1;
            }
        } // if (ipack > 4)
        else { // ipack <= 4
            ilda = lda;
            iskew = 0;
            ioffst = 0;
        } // else ipack <= 4

        // ipackg is the format that the matrix is generated in.  If this is
        // different from ipack, then the matrix must be repacked at the end.
        // It also signals how to compute the norm, for scaling.
        ipackg = 0;
        ge.dlaset('F', lda, n, 0.0, 0.0, A, lda);

        // Diagonal Matrix -- We are done, unless it is to be stored SP/PP/TP
        // (pack = 'R' or 'C')
        if ((llb == 0) && (uub == 0)) {

            if (ipack > 4) {

                for (i = 0; i < mnmin; i++) {
                    A[-iskew + ioffst][i] = D[i];
                }
            } // if (ipack > 4)
            else { // ipack <= 4

                for (i = 0; i < mnmin; i++) {
                    A[i - iskew + ioffst][i] = D[i];
                }
            } // else ipack <= 4

            if ((ipack <= 2) || (ipack >= 5)) {
                ipackg = ipack;
            }
        } // if ((llb == 0) && (uub == 0))
        else if (givens) {

            // Check whether to use Givens rotations, Householder
            // transformations, or nothing
            if (isym == 1) {

                // Non-symmetric -- A = U D V
                if (ipack > 4) {
                    ipackg = ipack;
                } else {
                    ipackg = 0;
                }

                if (ipack > 4) {

                    for (i = 0; i < mnmin; i++) {
                        A[-iskew + ioffst][i] = D[i];
                    }
                } // if (ipack > 4)
                else { // ipack <= 4

                    for (i = 0; i < mnmin; i++) {
                        A[i - iskew + ioffst][i] = D[i];
                    }
                } // else ipack <= 4

                if (topdwn) {
                    jkl = 0;

                    for (jku = 1; jku <= uub; jku++) {
                        // Transform from bandwidth jkl, jku-1 to jkl, jku
                        // Last row actually rotated is m-1
                        // Last column actually rotated is min(m+jku,n)-1

                        for (jr = 1; jr <= (Math.min(m + jku, n) + jkl - 1); jr++) {
                            extra[0] = 0.0;
                            angle = 2.0 * Math.PI * dlarnd(1, iseed);
                            c[0] = Math.cos(angle);
                            s[0] = Math.sin(angle);
                            icol = Math.max(1, jr - jkl);

                            if (jr < m) {
                                il = Math.min(n, jr + jku) + 1 - icol;
                                length = lda - (jr - (iskew * icol) + ioffst) + 1 + (lda * (n - icol));
                                ap = new double[length];
                                index = 0;

                                for (i = jr - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                    ap[index++] = A[i][icol - 1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(true, jr > jkl, false, il, c[0], s[0], ap, ilda, extra, dummy);
                                index = 0;

                                for (i = jr - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                    A[i][icol - 1] = ap[index++];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }
                            } // if (jr < m)

                            // Chase "extra" back up
                            ir = jr;
                            ic = icol;

                            for (jch = jr - jkl; jch >= 1; jch -= (jkl + jku)) {

                                if (ir < m) {
                                    ge.dlartg(A[ir - (iskew * (ic + 1)) + ioffst][ic], extra[0], c, s, dummy);
                                } // if (ir < m)

                                irow = Math.max(1, jch - jku);
                                il = ir + 2 - irow;
                                temp[0] = 0.0;
                                iltemp = jch > jku;
                                length = lda - (irow - (iskew * ic) + ioffst) + 1 + (lda * (n - ic));
                                ap = new double[length];
                                index = 0;

                                for (i = irow - (iskew * ic) + ioffst - 1; i < lda; i++) {
                                    ap[index++] = A[i][ic - 1];
                                }

                                for (j = ic; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(false, iltemp, true, il, c[0], -s[0], ap, ilda, temp, extra);
                                index = 0;

                                for (i = irow - (iskew * ic) + ioffst - 1; i < lda; i++) {
                                    A[i][ic - 1] = ap[index++];
                                }

                                for (j = ic; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }

                                if (iltemp) {
                                    ge.dlartg(A[irow - (iskew * (ic + 1)) + ioffst][ic], temp[0], c, s, dummy);
                                    icol = Math.max(1, jch - jku - jkl);
                                    il = ic + 2 - icol;
                                    extra[0] = 0.0;
                                    length = lda - (irow - (iskew * icol) + ioffst) + 1 + (lda * (n - icol));
                                    ap = new double[length];
                                    index = 0;

                                    for (i = irow - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                        ap[index++] = A[i][icol - 1];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < lda; i++) {
                                            ap[index++] = A[i][j];
                                        }
                                    }

                                    dlarot(true, jch > (jku + jkl), true, il, c[0], -s[0], ap, ilda, extra, temp);
                                    index = 0;

                                    for (i = irow - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                        A[i][icol - 1] = ap[index++];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < lda; i++) {
                                            A[i][j] = ap[index++];
                                        }
                                    }

                                    ic = icol;
                                    ir = irow;
                                } // if (iltemp)
                            } // for (jch = jr - jkl; jch >= 1; jch -= (jkl+jku))
                        } // for (jr = 1; jr <= Math.min(m+jku,n) + jkl - 1; jr++)
                    } // for (jku = 1; jku <= uub; jku++)

                    jku = uub;

                    for (jkl = 1; jkl <= llb; jkl++) {

                        // Transform from bandwidth jkl-1, jku to jkl, jku
                        for (jc = 1; jc <= (Math.min(n + jkl, m) + jku - 1); jc++) {
                            extra[0] = 0.0;
                            angle = 2.0 * Math.PI * dlarnd(1, iseed);
                            c[0] = Math.cos(angle);
                            s[0] = Math.sin(angle);
                            irow = Math.max(1, jc - jku);

                            if (jc < n) {
                                il = Math.min(m, jc + jkl) + 1 - irow;
                                length = lda - (irow - (iskew * jc) + ioffst) + 1 + (lda * (n - jc));
                                ap = new double[length];
                                index = 0;

                                for (i = irow - (iskew * jc) + ioffst - 1; i < lda; i++) {
                                    ap[index++] = A[i][jc - 1];
                                }

                                for (j = jc; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(false, jc > jku, false, il, c[0], s[0], ap, ilda, extra, dummy);
                                index = 0;

                                for (i = irow - (iskew * jc) + ioffst - 1; i < lda; i++) {
                                    A[i][jc - 1] = ap[index++];
                                }

                                for (j = jc; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }
                            } // if (jc < n)

                            // Chase "extra" back up
                            ic = jc;
                            ir = irow;

                            for (jch = jc - jku; jch >= 1; jch -= (jkl + jku)) {

                                if (ic < n) {
                                    ge.dlartg(A[ir - (iskew * (ic + 1)) + ioffst][ic], extra[0], c, s, dummy);
                                } // if (ic < n)

                                icol = Math.max(1, jch - jkl);
                                il = ic + 2 - icol;
                                temp[0] = 0.0;
                                iltemp = jch > jkl;
                                length = lda - (ir - (iskew * icol) + ioffst) + 1 + (lda * (n - icol));
                                ap = new double[length];
                                index = 0;

                                for (i = ir - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                    ap[index++] = A[i][icol - 1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(true, iltemp, true, il, c[0], -s[0], ap, ilda, temp, extra);
                                index = 0;

                                for (i = ir - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                    A[i][icol - 1] = ap[index++];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }

                                if (iltemp) {
                                    ge.dlartg(A[ir - (iskew * (icol + 1)) + ioffst][icol], temp[0], c, s, dummy);
                                    irow = Math.max(1, jch - jkl - jku);
                                    il = ir + 2 - irow;
                                    extra[0] = 0.0;
                                    length = lda - (irow - (iskew * icol) + ioffst) + 1 + (lda * (n - icol));
                                    ap = new double[length];
                                    index = 0;

                                    for (i = irow - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                        ap[index++] = A[i][icol - 1];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < lda; i++) {
                                            ap[index++] = A[i][j];
                                        }
                                    }

                                    dlarot(false, jch > (jkl + jku), true, il, c[0], -s[0], ap, ilda, extra, temp);
                                    index = 0;

                                    for (i = irow - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                        A[i][icol - 1] = ap[index++];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < lda; i++) {
                                            A[i][j] = ap[index++];
                                        }
                                    }

                                    ic = icol;
                                    ir = irow;
                                } // if (iltemp)
                            } // for (jch = jc - jku; jch >= 1; jch -= (jkl+jku))
                        } // for (jc = 1; jc <= Math.min(n+jkl,m)+jku-1; jc++)
                    } // for (jkl = 1; jkl <= llb; jkl++)
                } // if (topdwn)
                else { // !topdwn

                    // Bottom-Up -- Start at the bottom right.
                    jkl = 0;

                    for (jku = 1; jku <= uub; jku++) {

                        // Transform from bandwidth jkl, jku-1 to jkl, jku
                        // First row actually rotated is m-1
                        // First column actually rotated is min(m+jku,n)-1
                        iendch = Math.min(m, n + jkl) - 1;

                        for (jc = Math.min(m + jku, n) - 1; jc >= (1 - jkl); jc--) {
                            extra[0] = 0.0;
                            angle = 2.0 * Math.PI * dlarnd(1, iseed);
                            c[0] = Math.cos(angle);
                            s[0] = Math.sin(angle);
                            irow = Math.max(1, jc - jku + 1);

                            if (jc > 0) {
                                il = Math.min(m, jc + jkl + 1) + 1 - irow;
                                length = lda - (irow - (iskew * jc) + ioffst) + 1 + (lda * (n - jc));
                                ap = new double[length];
                                index = 0;

                                for (i = irow - (iskew * jc) + ioffst - 1; i < lda; i++) {
                                    ap[index++] = A[i][jc - 1];
                                }

                                for (j = jc; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(false, false, (jc + jkl) < m, il, c[0], s[0], ap, ilda, dummy, extra);
                                index = 0;

                                for (i = irow - (iskew * jc) + ioffst - 1; i < lda; i++) {
                                    A[i][jc - 1] = ap[index++];
                                }

                                for (j = jc; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }
                            } // if (jc > 0)

                            // Chase "extra" back down
                            ic = jc;

                            for (jch = jc + jkl; jch <= iendch; jch += (jkl + jku)) {
                                ilextr = ic > 0;

                                if (ilextr) {
                                    ge.dlartg(A[jch - (iskew * ic) + ioffst - 1][ic - 1], extra[0], c, s, dummy);
                                } // if (ilextr)

                                ic = Math.max(1, ic);
                                icol = Math.min(n - 1, jch + jku);
                                iltemp = (jch + jku) < n;
                                temp[0] = 0.0;
                                length = lda - (jch - (iskew * ic) + ioffst) + 1 + (lda * (n - ic));
                                ap = new double[length];
                                index = 0;

                                for (i = jch - (iskew * ic) + ioffst - 1; i < lda; i++) {
                                    ap[index++] = A[i][ic - 1];
                                }

                                for (j = ic; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(true, ilextr, iltemp, icol + 2 - ic, c[0], s[0], ap, ilda, extra, temp);
                                index = 0;

                                for (i = jch - (iskew * ic) + ioffst - 1; i < lda; i++) {
                                    A[i][ic - 1] = ap[index++];
                                }

                                for (j = ic; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }

                                if (iltemp) {
                                    ge.dlartg(A[jch - (iskew * icol) + ioffst - 1][icol - 1], temp[0], c, s, dummy);
                                    il = Math.min(iendch, jch + jkl + jku) + 2 - jch;
                                    extra[0] = 0.0;
                                    length = lda - (jch - (iskew * icol) + ioffst) + 1 + (lda * (n - icol));
                                    ap = new double[length];
                                    index = 0;

                                    for (i = jch - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                        ap[index++] = A[i][icol - 1];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < lda; i++) {
                                            ap[index++] = A[i][j];
                                        }
                                    }

                                    dlarot(false, true, (jch + jkl + jku) <= iendch, il, c[0], s[0], ap, ilda, temp,
                                           extra);
                                    index = 0;

                                    for (i = jch - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                        A[i][icol - 1] = ap[index++];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < lda; i++) {
                                            A[i][j] = ap[index++];
                                        }
                                    }

                                    ic = icol;
                                } // if (iltemp)
                            } // for (jch = jc+jkl; jch <= iendch; jch += (jkl+jku))
                        } // for (jc = Math.min(m+jku,n)-1; jc >= 1 - jkl; jc--)
                    } // for (jku = 1; jku <= uub; jku++)

                    jku = uub;

                    for (jkl = 1; jkl <= llb; jkl++) {

                        // Transform from bandwidth jkl-1, jku to jkl, jku
                        // First row actually rotated is min(n+jkl,m)-1
                        // First column actually rotated is n-1
                        iendch = Math.min(n, m + jku) - 1;

                        for (jr = Math.min(n + jkl, m) - 1; jr >= (1 - jku); jr--) {
                            extra[0] = 0.0;
                            angle = 2.0 * Math.PI * dlarnd(1, iseed);
                            c[0] = Math.cos(angle);
                            s[0] = Math.sin(angle);
                            icol = Math.max(1, jr - jkl + 1);

                            if (jr > 0) {
                                il = Math.min(n, jr + jku + 1) + 1 - icol;
                                length = lda - (jr - (iskew * icol) + ioffst) + 1 + (lda * (n - icol));
                                ap = new double[length];
                                index = 0;

                                for (i = jr - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                    ap[index++] = A[i][icol - 1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(true, false, (jr + jku) < n, il, c[0], s[0], ap, ilda, dummy, extra);
                                index = 0;

                                for (i = jr - (iskew * icol) + ioffst - 1; i < lda; i++) {
                                    A[i][icol - 1] = ap[index++];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }
                            } // if (jr > 0)

                            // Chase "extra" back down
                            ir = jr;

                            for (jch = jr + jku; jch <= iendch; jch += (jkl + jku)) {
                                ilextr = ir > 0;

                                if (ilextr) {
                                    ge.dlartg(A[ir - (iskew * jch) + ioffst - 1][jch - 1], extra[0], c, s, dummy);
                                } // if (ilextr)

                                ir = Math.max(1, ir);
                                irow = Math.min(m - 1, jch + jkl);
                                iltemp = (jch + jkl) < m;
                                temp[0] = 0.0;
                                length = lda - (ir - (iskew * jch) + ioffst) + 1 + (lda * (n - jch));
                                ap = new double[length];
                                index = 0;

                                for (i = ir - (iskew * jch) + ioffst - 1; i < lda; i++) {
                                    ap[index++] = A[i][jch - 1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(false, ilextr, iltemp, irow + 2 - ir, c[0], s[0], ap, ilda, extra, temp);
                                index = 0;

                                for (i = ir - (iskew * jch) + ioffst - 1; i < lda; i++) {
                                    A[i][jch - 1] = ap[index++];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }

                                if (iltemp) {
                                    ge.dlartg(A[irow - (iskew * jch) + ioffst - 1][jch - 1], temp[0], c, s, dummy);
                                    il = Math.min(iendch, jch + jkl + jku) + 2 - jch;
                                    extra[0] = 0.0;
                                    length = lda - (irow - (iskew * jch) + ioffst) + 1 + (lda * (n - jch));
                                    ap = new double[length];
                                    index = 0;

                                    for (i = irow - (iskew * jch) + ioffst - 1; i < lda; i++) {
                                        ap[index++] = A[i][jch - 1];
                                    }

                                    for (j = jch; j < n; j++) {

                                        for (i = 0; i < lda; i++) {
                                            ap[index++] = A[i][j];
                                        }
                                    }

                                    dlarot(true, true, (jch + jkl + jku) <= iendch, il, c[0], s[0], ap, ilda, temp,
                                           extra);
                                    index = 0;

                                    for (i = irow - (iskew * jch) + ioffst - 1; i < lda; i++) {
                                        A[i][jch - 1] = ap[index++];
                                    }

                                    for (j = jch; j < n; j++) {

                                        for (i = 0; i < lda; i++) {
                                            A[i][j] = ap[index++];
                                        }
                                    }

                                    ir = irow;
                                } // if (iltemp)
                            } // for (jch = jr + jku; jch <= iendch; jch += (jkl+jku))
                        } // for (jr = Math.min(n+jkl,m)-1; jr >= 1 - jku; jr--)
                    } // for (jkl = 1; jkl <= llb; jkl++)
                } // else !topdwn
            } // if (isym == 1)
            else { // isym != 1

                // Symmetric -- A = U D U'
                ipackg = ipack;
                ioffg = ioffst;

                if (topdwn) {

                    // Top-Down -- Generate Upper triangle only
                    if (ipack >= 5) {
                        ipackg = 6;
                        ioffg = uub + 1;
                    } else {
                        ipackg = 1;
                    }

                    if (ipack > 4) {

                        for (i = 0; i < mnmin; i++) {
                            A[-iskew + ioffg][i] = D[i];
                        }
                    } // if (ipack > 4)
                    else { // ipack <= 4

                        for (i = 0; i < mnmin; i++) {
                            A[i - iskew + ioffg][i] = D[i];
                        }
                    } // else ipack <= 4

                    for (k = 1; k <= uub; k++) {

                        for (jc = 1; jc <= (n - 1); jc++) {
                            irow = Math.max(1, jc - k);
                            il = Math.min(jc + 1, k + 2);
                            extra[0] = 0.0;
                            temp[0] = A[jc - (iskew * (jc + 1)) + ioffg - 1][jc];
                            angle = 2.0 * Math.PI * dlarnd(1, iseed);
                            c[0] = Math.cos(angle);
                            s[0] = Math.sin(angle);
                            length = lda - (irow - (iskew * jc) + ioffg) + 1 + (lda * (n - jc));
                            ap = new double[length];
                            index = 0;

                            for (i = irow - (iskew * jc) + ioffg - 1; i < lda; i++) {
                                ap[index++] = A[i][jc - 1];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < lda; i++) {
                                    ap[index++] = A[i][j];
                                }
                            }

                            dlarot(false, jc > k, true, il, c[0], s[0], ap, ilda, extra, temp);
                            index = 0;

                            for (i = irow - (iskew * jc) + ioffg - 1; i < lda; i++) {
                                A[i][jc - 1] = ap[index++];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < lda; i++) {
                                    A[i][j] = ap[index++];
                                }
                            }

                            length = lda - (((1 - iskew) * jc) + ioffg) + 1 + (lda * (n - jc));
                            ap = new double[length];
                            index = 0;

                            for (i = ((1 - iskew) * jc) + ioffg - 1; i < lda; i++) {
                                ap[index++] = A[i][jc - 1];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < lda; i++) {
                                    ap[index++] = A[i][j];
                                }
                            }

                            dlarot(true, true, false, Math.min(k, n - jc) + 1, c[0], s[0], ap, ilda, temp, dummy);
                            index = 0;

                            for (i = ((1 - iskew) * jc) + ioffg - 1; i < lda; i++) {
                                A[i][jc - 1] = ap[index++];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < lda; i++) {
                                    A[i][j] = ap[index++];
                                }
                            }

                            // Chase extra back up the matrix
                            icol = jc;

                            for (jch = jc - k; jch >= 1; jch -= k) {
                                ge.dlartg(A[jch - (iskew * (icol + 1)) + ioffg][icol], extra[0], c, s, dummy);
                                temp[0] = A[jch - (iskew * (jch + 1)) + ioffg - 1][jch];
                                length = lda - (((1 - iskew) * jch) + ioffg) + 1 + (lda * (n - jch));
                                ap = new double[length];
                                index = 0;

                                for (i = ((1 - iskew) * jch) + ioffg - 1; i < lda; i++) {
                                    ap[index++] = A[i][jch - 1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(true, true, true, k + 2, c[0], -s[0], ap, ilda, temp, extra);
                                index = 0;

                                for (i = ((1 - iskew) * jch) + ioffg - 1; i < lda; i++) {
                                    A[i][jch - 1] = ap[index++];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }

                                irow = Math.max(1, jch - k);
                                il = Math.min(jch + 1, k + 2);
                                extra[0] = 0.0;
                                length = lda - (irow - (iskew * jch) + ioffg) + 1 + (lda * (n - jch));
                                ap = new double[length];
                                index = 0;

                                for (i = irow - (iskew * jch) + ioffg - 1; i < lda; i++) {
                                    ap[index++] = A[i][jch - 1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(false, jch > k, true, il, c[0], -s[0], ap, ilda, extra, temp);
                                index = 0;

                                for (i = irow - (iskew * jch) + ioffg - 1; i < lda; i++) {
                                    A[i][jch - 1] = ap[index++];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }

                                icol = jch;
                            } // for (jch = jc-k; jch >= 1; jch -= k)
                        } // for (jc = 1; jc <= n-1; jc++)
                    } // for (k = 1; k <= uub; k++)

                    // If we need lower triangle, copy from upper.  Note that
                    // the order of copying is chosen to work for 'q' -> 'b'
                    if ((ipack != ipackg) && (ipack != 3)) {

                        for (jc = 1; jc <= n; jc++) {
                            irow = ioffst - (iskew * jc);

                            for (jr = jc; jr <= Math.min(n, jc + uub); jr++) {
                                A[jr + irow - 1][jc - 1] = A[jc - (iskew * jr) + ioffg - 1][jr - 1];
                            }
                        } // for (jc = 1; jc <= n; jc++)

                        if (ipack == 5) {

                            for (jc = n - uub + 1; jc <= n; jc++) {

                                for (jr = n + 2 - jc; jr <= (uub + 1); jr++) {
                                    A[jr - 1][jc - 1] = 0.0;
                                }
                            }
                        } // if (ipack == 5)

                        if (ipackg == 6) {
                            ipackg = ipack;
                        } else {
                            ipackg = 0;
                        }
                    } // if ((ipack != ipackg) && (ipack != 3))
                } // if (topdwn)
                else { // !topdwn

                    // Bottom-Up -- Generate Lower triangle only
                    if (ipack >= 5) {
                        ipackg = 5;

                        if (ipack == 6) {
                            ioffg = 1;
                        }
                    } // if (ipack >= 5)
                    else { // ipack < 5
                        ipackg = 2;
                    } // else ipack < 5

                    if (ipack > 4) {

                        for (i = 0; i < mnmin; i++) {
                            A[-iskew + ioffg][i] = D[i];
                        }
                    } // if (ipack > 4)
                    else { // ipack <= 4

                        for (i = 0; i < mnmin; i++) {
                            A[i - iskew + ioffg][i] = D[i];
                        }
                    } // else ipack <= 4

                    for (k = 1; k <= uub; k++) {

                        for (jc = n - 1; jc >= 1; jc--) {
                            il = Math.min(n + 1 - jc, k + 2);
                            extra[0] = 0.0;
                            temp[0] = A[((1 - iskew) * jc) + ioffg][jc - 1];
                            angle = 2.0 * Math.PI * dlarnd(1, iseed);
                            c[0] = Math.cos(angle);
                            s[0] = -Math.sin(angle);
                            length = lda - (((1 - iskew) * jc) + ioffg) + 1 + (lda * (n - jc));
                            ap = new double[length];
                            index = 0;

                            for (i = ((1 - iskew) * jc) + ioffg - 1; i < lda; i++) {
                                ap[index++] = A[i][jc - 1];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < lda; i++) {
                                    ap[index++] = A[i][j];
                                }
                            }

                            dlarot(false, true, (n - jc) > k, il, c[0], s[0], ap, ilda, temp, extra);
                            index = 0;

                            for (i = ((1 - iskew) * jc) + ioffg - 1; i < lda; i++) {
                                A[i][jc - 1] = ap[index++];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < lda; i++) {
                                    A[i][j] = ap[index++];
                                }
                            }

                            icol = Math.max(1, jc - k + 1);
                            length = lda - (jc - (iskew * icol) + ioffg) + 1 + (lda * (n - icol));
                            ap = new double[length];
                            index = 0;

                            for (i = jc - (iskew * icol) + ioffg - 1; i < lda; i++) {
                                ap[index++] = A[i][icol - 1];
                            }

                            for (j = icol; j < n; j++) {

                                for (i = 0; i < lda; i++) {
                                    ap[index++] = A[i][j];
                                }
                            }

                            dlarot(true, false, true, jc + 2 - icol, c[0], s[0], ap, ilda, dummy, temp);
                            index = 0;

                            for (i = jc - (iskew * icol) + ioffg - 1; i < lda; i++) {
                                A[i][icol - 1] = ap[index++];
                            }

                            for (j = icol; j < n; j++) {

                                for (i = 0; i < lda; i++) {
                                    A[i][j] = ap[index++];
                                }
                            }

                            // Chase extra back down the matrix
                            icol = jc;

                            for (jch = jc + k; jch <= (n - 1); jch += k) {
                                ge.dlartg(A[jch - (iskew * icol) + ioffg - 1][icol - 1], extra[0], c, s, dummy);
                                temp[0] = A[((1 - iskew) * jch) + ioffg][jch - 1];
                                length = lda - (jch - (iskew * icol) + ioffg) + 1 + (lda * (n - icol));
                                ap = new double[length];
                                index = 0;

                                for (i = jch - (iskew * icol) + ioffg - 1; i < lda; i++) {
                                    ap[index++] = A[i][icol - 1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(true, true, true, k + 2, c[0], s[0], ap, ilda, extra, temp);
                                index = 0;

                                for (i = jch - (iskew * icol) + ioffg - 1; i < lda; i++) {
                                    A[i][icol - 1] = ap[index++];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }

                                il = Math.min(n + 1 - jch, k + 2);
                                extra[0] = 0.0;
                                length = lda - (((1 - iskew) * jch) + ioffg) + 1 + (lda * (n - jch));
                                ap = new double[length];
                                index = 0;

                                for (i = ((1 - iskew) * jch) + ioffg - 1; i < lda; i++) {
                                    ap[index++] = A[i][jch - 1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index++] = A[i][j];
                                    }
                                }

                                dlarot(false, true, (n - jch) > k, il, c[0], s[0], ap, ilda, temp, extra);
                                index = 0;

                                for (i = ((1 - iskew) * jch) + ioffg - 1; i < lda; i++) {
                                    A[i][jch - 1] = ap[index++];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        A[i][j] = ap[index++];
                                    }
                                }

                                icol = jch;
                            } // for (jch = jc+k; jch <= n-1; jch += k)
                        } // for (jc = n-1; jc >= 1; jc--)
                    } // for (k = 1; k <= uub; k++)

                    // If we need upper triangle, copy from lower.  Note that the
                    // order of copying is chosen to work for 'b' -> 'q'
                    if ((ipack != ipackg) && (ipack != 4)) {

                        for (jc = n; jc >= 1; jc--) {
                            irow = ioffst - (iskew * jc);

                            for (jr = jc; jr >= Math.max(1, jc - uub); jr--) {
                                A[jr + irow - 1][jc - 1] = A[jc - (iskew * jr) + ioffg - 1][jr - 1];
                            }
                        } // for (jc = n; jc >= 1; jc--)

                        if (ipack == 6) {

                            for (jc = 1; jc <= uub; jc++) {

                                for (jr = 1; jr <= (uub + 1 - jc); jr++) {
                                    A[jr - 1][jc - 1] = 0.0;
                                }
                            }
                        } // if (ipack == 6)

                        if (ipackg == 5) {
                            ipackg = ipack;
                        } // if (ipackg == 5)
                        else { // ipackg != 5
                            ipackg = 0;
                        } // else ipackg != 5
                    } // if ((ipack != ipackg) && (ipack != 4))
                } // else !topdwn
            } // else isym != 1
        } // else if (givens)
        else {

            // Generate Banded Matrix by first rotating by random Unitary
            // matrices, then reducing the bandwidth using Householder
            // transformations.
            // Note: We should only get here if lda >= n.
            if (isym == 1) {

                // Non-symmetric -- A = U D V
                dlagge(mr, nc, llb, uub, D, A, lda, iseed, work, iinfo);
            } // if (isym == 1)
            else { // isym != 1

                // Symmetric -- A = U D U'
                dlagsy(m, llb, D, A, lda, iseed, work, iinfo);
            } // else isym != 1

            if (iinfo[0] != 0) {
                info[0] = 3;

                return;
            } // if (iinfo[0] != 0)
        } // else

        // Pack the matrix
        if (ipack != ipackg) {

            if (ipack == 1) {

                // 'U' -- Upper triangular, not packed
                for (j = 0; j < m; j++) {

                    for (i = j + 1; i < m; i++) {
                        A[i][j] = 0.0;
                    }
                }
            } // if (ipack == 1)
            else if (ipack == 2) {

                // 'L' -- Lower triangular, not packed
                for (j = 1; j < m; j++) {

                    for (i = 0; i <= (j - 1); i++) {
                        A[i][j] = 0.0;
                    }
                }
            } // else if (ipack == 2)
            else if (ipack == 3) {

                // 'C' -- Upper triangle packed Columnwise.
                icol = 1;
                irow = 0;

                for (j = 0; j < m; j++) {

                    for (i = 0; i <= j; i++) {
                        irow = irow + 1;

                        if (irow > lda) {
                            irow = 1;
                            icol = icol + 1;
                        }

                        A[irow - 1][icol - 1] = A[i][j];
                    }
                }
            } // else if (ipack == 3)
            else if (ipack == 4) {

                // 'R' -- Lower triangle packed columnwise.
                icol = 1;
                irow = 0;

                for (j = 0; j < m; j++) {

                    for (i = j; i < m; i++) {
                        irow = irow + 1;

                        if (irow > lda) {
                            irow = 1;
                            icol = icol + 1;
                        }

                        A[irow - 1][icol - 1] = A[i][j];
                    }
                }
            } // else if (ipack == 4)
            else if (ipack >= 5) {
                // 'B' -- The lower triangle is packed as a band matrix.
                // 'Q' -- The upper triangle is packed as a band matrix.
                // 'Z' -- The whole matrix is packed as a band matrix.

                if (ipack == 5) {
                    uub = 0;
                } // if (ipack == 5)

                if (ipack == 6) {
                    llb = 0;
                } // if (ipack == 6)

                for (j = 1; j <= uub; j++) {

                    for (i = Math.min(j + llb, m); i >= 1; i--) {
                        A[i - j + uub][j - 1] = A[i - 1][j - 1];
                    }
                }

                for (j = uub + 2; j <= n; j++) {

                    for (i = j - uub; i <= Math.min(j + llb, m); i++) {
                        A[i - j + uub][j - 1] = A[i - 1][j - 1];
                    }
                }
            } // else if (ipack >= 5)

            // If packed, zero out extraneous elements

            // Symmetric/Triangular Packed
            // zero out everything after A[irow-1][icol-1]

            if ((ipack == 3) || (ipack == 4)) {

                for (jc = icol - 1; jc < m; jc++) {

                    for (jr = irow; jr < lda; jr++) {
                        A[jr][jc] = 0.0;
                    }

                    irow = 0;
                }
            } // if ((ipack == 3) || (ipack == 4))
            else if (ipack >= 5) {

                // Packed Band
                // 1st row is now in A[uub+1-j][j-1], zero above it
                // m-th row is now in A[m-1+uub-j][j-1], zero below it
                // last non-zero diagonal is now in A[uub+llb][j-1],
                // zero below it, too.
                ir1 = uub + llb + 2;
                ir2 = uub + m + 2;

                for (jc = 1; jc <= n; jc++) {

                    for (jr = 1; jr <= (uub + 1 - jc); jr++) {
                        A[jr - 1][jc - 1] = 0.0;
                    }

                    for (jr = Math.max(1, Math.min(ir1, ir2 - jc)); jr <= lda; jr++) {
                        A[jr - 1][jc - 1] = 0.0;
                    }
                } // for (jc = 1; jc <= n; jc++)
            } // else if (ipack >= 5)
        } // if (ipack != ipackg)

        return;
    } // dlatms
    
    /**
     * This is a port of version 3.1 LAPACK auxiliary test routine DLATM1 Original DLATM1 created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., November, 2006
     * dlatm1 computes the entries of D[0...n-1] as specified by mode, cond, and irsign. idist and iseed
     * determine the generation of random numbers. dlatm1 is called by dlatmr to generate random test matrices for
     * LAPACK programs.
     *
     * @param  mode    input int On entry describes how D is to be computed: 
     *                 = 0 means do not change D. 
     *                 = 1 sets D[0] = 1 and D[1:n-1] = 1.0/cond. 
     *                 = 2 sets D[0:n-2] = 1 and D[n-1] = 1.0/cond. 
     *                 = 3 sets D[i] = cond**(-(i)/(n-1)) 
     *                 = 4 sets D[i] = 1 - (i)/(n-1)*(1 - 1/cond) 
     *                 = 5 sets D to random numbers in the range (1/cond, 1) such that their logarithms are uniformly distributed.
     *                 = 6 sets D to random numbers from the same distribution as the rest of the matrix. 
     *                 < 0 has the same meaning as abs(mode), except that the order of the elements of D is reversed.
     *                    Thus if mode is positive, D has entries ranging from 1 to 1/cond. If negative, from 1/cond to 1.
     * @param  cond    input double On entry, used as described under mode above. If used, it must be >= 1.
     * @param  irsign  input int On entry, if mode neither -6, 0, or 6, determines sign of entries of D. 
     *                 0 => leave entries of D unchanged 
     *                 1 => multiply each entry of D by 1 or -1 with probability 0.5
     * @param  idist   input int On entry, idist specifies the type of distribution to be used to generate a random
     *                 matrix. 
     *                 1 => uniform(0,1) 
     *                 2 => uniform(-1,1) 
     *                 3 => normal(0,1)
     * @param  iseed   input/output int[] On entry iseed specifies the seed of the random number generator. The random
     *                 number generator uses a linear congruential sequence limited to small integers, and so should
     *                 produce machine independent random numbers. The values of iseed are changed on exit, and can be
     *                 used in the next call to dlatm1 to continue the same random number sequence.
     * @param  D       input/output double[] of dimension min(m,n). Array to be computed according to mode, cond, and
     *                 irsign. May be changed on exit if mode is nonzero.
     * @param  n       input int The number of entries of D.
     * @param  info    output int[] 
     *                 0 => normal termination 
     *                 -1 => if mode not in range -6 to 6. 
     *                 -2 => if mode neither -6, 0, or 6, and irsign neither 0 nor 1 
     *                 -3 => if mode neither -6, 0, or 6 and cond less than 1 
     *                 -4 => if mode equals 6 or -6 and idist not in range 1 to 3
     *                 -7 => if n negative
     */
    private void dlatm1(int mode, double cond, int irsign, int idist, int[] iseed, double[] D, int n, int[] info) {
        int i;
        double alpha;
        double temp;

        // Decode and test the input parameters.  Initialiize flags & seed.
        info[0] = 0;

        // Quick return if possible
        if (n == 0) {
            return;
        }

        // Set info if an error
        if ((mode < -6) || (mode > 6)) {
            info[0] = -1;
        } else if ((mode != -6) && (mode != 0) && (mode != 6) && (irsign != 0) && (irsign != 1)) {
            info[0] = -2;
        } else if ((mode != -6) && (mode != 0) && (mode != 6) && (cond < 1.0)) {
            info[0] = -3;
        } else if (((mode == 6) || (mode == -6)) && ((idist < 1) || (idist > 3))) {
            info[0] = -4;
        } else if (n < 0) {
            info[0] = -7;
        }

        if (info[0] != 0) {
            MipavUtil.displayError("Error dlatm1 had info[0] = " + info[0]);

            return;
        }

        // Compute D according to cond and mode
        if (mode != 0) {

            switch (Math.abs(mode)) {

                case 1:

                    // One large D value
                    for (i = 1; i < n; i++) {
                        D[i] = 1.0 / cond;
                    }

                    D[0] = 1.0;
                    break;

                case 2:

                    // One small D value
                    for (i = 0; i < (n - 1); i++) {
                        D[i] = 1.0;
                    }

                    D[n - 1] = 1.0 / cond;
                    break;

                case 3:

                    // Exponentially distributed D values
                    D[0] = 1.0;
                    if (n > 1) {
                        alpha = Math.pow(cond, -1.0 / (n - 1));

                        for (i = 1; i < n; i++) {
                            D[i] = Math.pow(alpha, i);
                        }
                    } // if (n > 1)

                    break;

                case 4:

                    // Arithmetically distributed D values
                    D[0] = 1.0;
                    if (n > 1) {
                        temp = 1.0 / cond;
                        alpha = (1.0 - temp) / (n - 1.0);

                        for (i = 2; i <= n; i++) {
                            D[i - 1] = ((n - i) * alpha) + temp;
                        }
                    } // if (n > 1)

                    break;

                case 5:

                    // Randomly distributed D values on (1/cond, 1)
                    alpha = Math.log(1.0 / cond);
                    for (i = 0; i < n; i++) {
                        D[i] = Math.exp(alpha * dlaran(iseed));
                    }

                    break;

                case 6:

                    // Randomly distributed values from idist
                    dlarnv(idist, iseed, n, D);
                    break;
            } // switch(Math.abs(mode))

            // If mode neither -6, nor 0, or nor 6, and irsign = 1, assign random
            // signs to D
            if ((mode != -6) && (mode != 0) && (mode != 6) && (irsign == 1)) {

                for (i = 0; i < n; i++) {
                    temp = dlaran(iseed);

                    if (temp > 0.5) {
                        D[i] = -D[i];
                    }
                }
            } // if ((mode != -6) && (mode != 0) && (mode != 6) && (irsign == 1))

            // Reverse if mode < 0
            if (mode < 0) {

                for (i = 1; i <= (n / 2); i++) {
                    temp = D[i - 1];
                    D[i - 1] = D[n - i];
                    D[n - i] = temp;
                }
            } // if (mode < 0)
        } // if (mode != 0)

        return;
    } // dlatm1
    
    /**
     * This is a port of version 3.1 LAPACK auxiliary routine DLARND Original DLARND created by Univ. of Tennessee, Univ.
     * of California Berkeley, and NAG Ltd., November, 2006
     * dlarnd returns a random real number from a uniform or normal distribution. This routine calls the auxiliary
     * routine dlaran to generate a random real number from a uniform (0,1) distribution. The Box-Muller method is used
     * to transform numbers from a uniform to a normal distribution.
     *
     * @param   idist  input int Specifies the distribution of the random numbers: 
     *                 = 1: uniform (0,1) 
     *                 = 2: uniform (-1,1) 
     *                 = 3: normal (0,1)
     * @param   iseed  (input/output) int[] of dimension 4 On entry, the seed of the random number generator; the array
     *                 elements must be between 0 and 4095, and iseed[3] must be odd. On exit, the seed is updated.
     *
     * @return  DOCUMENT ME!
     */
    private double dlarnd(int idist, int[] iseed) {
        double t1;
        double t2;
        double result = 0.0;

        // Generate a real random number from a uniform (0,1) distribution
        t1 = dlaran(iseed);

        if (idist == 1) {

            // uniform (0,1)
            result = t1;
        } else if (idist == 2) {

            // Uniform (-1,1)
            result = (2.0 * t1) - 1.0;
        } else if (idist == 3) {

            // Normal (0,1)
            t2 = dlaran(iseed);
            result = Math.sqrt(-2.0 * Math.log(t1)) * Math.cos(2.0 * Math.PI * t2);
        }

        return result;
    } // dlarnd
    
    /**
     * This is a port of version 3.1 LAPACK auxiliary test routine DLAROT Original DLAROT created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., November, 2006
     * dlarot applies a (Givens) rotation to two adjacent rows or columns, where one element of the first
     * and/or last column/row may be a separate variable. This is specifically intended for use on matrices stored in
     * some format other than GE, so that elements of the matrix may be used or modified for which no array element is
     * provided.
     *
     * <p>One example is a symmetric matrix in SB format (bandwidth=4), for which uplo = 'L': Two adjacent rows will
     * have the format: 
     * row j:      * * * * * . . . . 
     * row j+1:      * * * * * . . . .
     * '*' indicates elements for which storage is provided, 
     * '.' indicates elements for which no storage is provided, but are not necessrily zero; their values are
     *     determined by symmetry. 
     * ' ' indicatres elements which are mecessarily zero, and have no storage provided.</p>
     *
     * <p>Those columns which have two '*'s can be handled by ge.drot. Those columns which have no '*'s can be ignored,
     * since as long as the Givens rotations are carefully applied to preserve symmetry, their values are determined.
     * Those columns which have one '*' have to be handled separately, by using separate variables "p" and "q": 
     * row j:          * * * * * p . . . 
     * row j+1:        q * * * * * . . . .</p>
     *
     * <p>The element p would have to be set correctly, then that column is rotated, setting p to its new value. The
     * next call to dlarot would rotate columns j and j+1, using p, and restore symmetry. The element q would start out
     * being zero, and be made non-zero by the rotation. Later, rotations would presumably be chosen to zero q out.</p>
     *
     * <p>Typical Calling Sequences: rotating the i-th and (i+1)-st rows.</p>
     *
     * <p>General dense matrix: dlarot(true, false, false, n, c, s, A[i-1][0], lda, dummy, dummy);</p>
     *
     * <p>General banded matrix in GB format: j = Math.max(1, i-kl); nl = Math.min(n, i+ku+1) + 1 - j; dlarot(true,
     * (i-kl) >= 1, (i+ku) < n, nl, c,s,A[ku+i-j][j-1], lda - 1, xleft, xright); Note that i+1-j is just min(i,kl+1)</p>
     *
     * <p>Symmetric banded matrix in SY format, bandwidth k, lower triangle only: j = Math.max(1,i-k); nl =
     * Math.min(k+1,i) + 1; dlarot(true, (i-k) >= 1, true, nl, c, s, A[i-1][j-1], lda, xleft, xright);</p>
     *
     * <p>Same, but upper triangle only: nl = Math.min(k+1,n-i) + 1; dlarot(true, true, (i+k) < n, nl, c, s,
     * A[i-1][i-1], lda, xleft, xright);</p>
     *
     * <p>Symmetric banded matrix in SB format, bandwidth k, lower triangle only: same as for sy, except: A[i-j][j-1],
     * lda - 1, xleft, xright); Note that i+1-j is just min(i,k+1)</p>
     *
     * <p>Same, but upper triangle only: A[k][i-1], lda-1, xleft, xright);</p>
     *
     * <p>Rotating columns is just the transpose of rotating rows, except for GB and SB: (rotating columns i and i+1)
     * </p>
     *
     * <p>GB: j = Math.max(1,i-ku); nl = Math.min(n, i+kl+1) + 1 - j; dlarot(true, i-ku >= 1, i+kl < n, nl, c, s,
     * A[ku+j-i][i-1], lda - 1, xtop, xbottm); Note that ku+j+1-i is just max(1,ku+2-i)</p>
     *
     * <p>SB: (upper triangle) ............. A[k+j-i][i-1], lda-1, xtop, xbottm);</p>
     *
     * <p>SB: (lower triangle) ................... A[0][i-1], lda-1, xtop, xbottm);</p>
     *
     * @param  lrows   input boolean If true, then dlarot will rotate two rows. If false, then it will rotate two
     *                 columns.
     * @param  lleft   input boolean If true, then xleft will be used instead of the corresponding element of A for the
     *                 first element in the second row (if lrows = false) or column (if lrows = true) If false, then the
     *                 corresponding element of A will be used.
     * @param  lright  input boolean If true, then xright will be used instead of the corresponding element of A for the
     *                 last element in the first row (if lrows = false) or column (if lrows = true). If false, then the
     *                 corresponding element of A will be used.
     * @param  nl      input int The length of the rows (if lrows = true) or columns (if lrows = false) to be rotated.
     *                 If xleft and/or xright are used, the columns/rows they are in should be included in nl, e.g., if
     *                 lleft = lright = true, then nl must be at least 2. The number of rows/columns to be rotated
     *                 exclusive of those involving xleft and/or xright may not be negative, i.e., nl minus how many of
     *                 lleft and lright are true must be at least zero; if not, an error message will be output.
     * @param  c       input double
     * @param  s       input double c and s specify the Givens rotation to be applied. If lrows is true, then the matrix
     *                 ( c s )
     *                 (-s c ) is applied from the left; if false, then the transpose thereof is applied from
     *                 the right. For a Givens rotation, c**2 + s**2 should be 1, but this is not checked.
     * @param  A       input/output double[] The array containing the rows/columns to be rotated. The first element of A
     *                 should be the upper left element to be rotated.
     * @param  lda     input int The "effective" leading dimension of A. If A contains a matrix stored in GE or SY
     *                 format, then this is just the leading dimension of A as dimensioned in the calling routine. If A
     *                 contains a matrix stored in band (GB or SB) format, then this should be *one less* than the
     *                 leading dimension used in the calling routine. Thus, if A were dimensioned A(lda,*) in dlarot,
     *                 then A[0][j-1] would be the j-th element in the first of the two rows to be rotated, and
     *                 A[1][j-1] would be the j-th in the second, regardless of how the array may be stored in the
     *                 calling routine. [A cannot, however, actually be dimensioned thus, since for band format, the row
     *                 number may exceed lda, which is not legal code.] If lrows = true, then lda must be at least 1,
     *                 otherwise it must be at least nl minus the number of true values in xleft and xright.
     * @param  xleft   input/output double[] If lleft is true, then xleft will be used and modified instead of A[1][0]
     *                 (if lrows = true) or A[0][1] (if lrows = false).
     * @param  xright  input/output double[] If lright is true, then xright will be used and modified instead of
     *                 A[0][nl-1] (if lrows = true) or A[nl-1][0] (if lrows = false).
     */
    private void dlarot(boolean lrows, boolean lleft, boolean lright, int nl, double c, double s, double[] A, int lda,
                        double[] xleft, double[] xright) {
        int iinc;
        int inext;
        int ix;
        int iy;
        int iyt = 0;
        int nt;
        double[] xt = new double[2];
        double[] yt = new double[2];
        double[] dx;
        double[] dy;
        int i;
        int index;

        // Set up indices, arrays for ends
        if (lrows) {
            iinc = lda;
            inext = 1;
        } else {
            iinc = 1;
            inext = lda;
        }

        if (lleft) {
            nt = 1;
            ix = 1 + iinc;
            iy = 2 + lda;
            xt[0] = A[0];
            yt[0] = xleft[0];
        } else {
            nt = 0;
            ix = 1;
            iy = 1 + inext;
        }

        if (lright) {
            iyt = 1 + inext + ((nl - 1) * iinc);
            nt = nt + 1;
            xt[nt - 1] = xright[0];
            yt[nt - 1] = A[iyt - 1];
        }

        // Check for errors
        if (nl < nt) {
            MipavUtil.displayError("dlarot has error 4");

            return;
        }

        if ((lda <= 0) || ((!lrows) && (lda < (nl - nt)))) {
            MipavUtil.displayError("dlarot had error 8");

            return;
        }

        // Rotate
        dx = new double[nl - nt];
        dy = new double[nl - nt];
        index = 0;

        for (i = 0; i < (nl - nt); i++) {
            dx[i] = A[ix + index - 1];
            dy[i] = A[iy + index - 1];
            index += iinc;
        }

        ge.drot(nl - nt, dx, 1, dy, 1, c, s);
        index = 0;

        for (i = 0; i < (nl - nt); i++) {
            A[ix + index - 1] = dx[i];
            A[iy + index - 1] = dy[i];
            index += iinc;
        }

        ge.drot(nt, xt, 1, yt, 1, c, s);

        // Stuff values back into xleft, xright, etc.
        if (lleft) {
            A[0] = xt[0];
            xleft[0] = yt[0];
        }

        if (lright) {
            xright[0] = xt[nt - 1];
            A[iyt - 1] = yt[nt - 1];
        }

        return;
    } // dlarot
    
    /**
     * This is a port of version 3.1 LAPACK auxiliary test routine DLAGGE Original DLAGGE created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., November, 2006
     * dlagge generates a real general m by n matrix A, by pre- and post- multiplying a real diagonal matrix D
     * with random orthogonal matrices: A = U*D*V. The lower and upper bandwidths may then be reduced to kl and ku by
     * additional orthogonal transformations.
     *
     * @param  m      input int The number of rows of the matrix A. m >= 0.
     * @param  n      input int The number of columns of the matrix A. n >= 0.
     * @param  kl     input int The number of nonzero subdiagonals within the band of A. 0 <= kl <= m-1.
     * @param  ku     input int The number of nonzero superdiagonals within the band of A. 0 <= ku <= n-1.
     * @param  D      input double[] of dimension (min(m,n)) The diagonal elements of the diagonal matrix D
     * @param  A      output double[][] of dimension (lda,n) The generated m by n matrix A.
     * @param  lda    input int The leading dimension of the array A. lda >= m.
     * @param  iseed  input/output int[] of dimension 4 On entry, the seed of the random number generator; the array
     *                elements must be between 0 and 4095, and iseed[3] must be odd. On exit, the seed is updated.
     * @param  work   workspace double[] of dimension (m+n)
     * @param  info   output int[] 
     *         = 0: successful exit 
     *         < 0: If info = -i, the i-th argument had an illegal value
     */
    private void dlagge(int m, int n, int kl, int ku, double[] D, double[][] A, int lda, int[] iseed, double[] work,
                        int[] info) {
        int i;
        int j;
        int k;
        double tau;
        double wa;
        double wb;
        double wn;
        double[][] B;
        double[] work2;
        double[] x;

        // Test the input arguments
        info[0] = 0;

        if (m < 0) {
            info[0] = -1;
        } else if (n < 0) {
            info[0] = -2;
        } else if ((kl < 0) || (kl > (m - 1))) {
            info[0] = -3;
        } else if ((ku < 0) || (ku > (n - 1))) {
            info[0] = -4;
        } else if (lda < Math.max(1, m)) {
            info[0] = -7;
        }

        if (info[0] < 0) {
            MipavUtil.displayError("Error dlagge had info[0] = " + info[0]);

            return;
        }

        // Initialize A to diagonal matrix
        for (j = 0; j < n; j++) {

            for (i = 0; i < m; i++) {
                A[i][j] = 0.0;
            }
        }

        for (i = 0; i < Math.min(m, n); i++) {
            A[i][i] = D[i];
        }

        // pre- and post- multiply by random orthogonal matrices
        for (i = Math.min(m, n); i >= 1; i--) {

            if (i < m) {

                // generate random reflection
                dlarnv(3, iseed, m - i + 1, work);
                wn = ge.dnrm2(m - i + 1, work, 1);

                if (work[0] >= 0) {
                    wa = Math.abs(wn);
                } else {
                    wa = -Math.abs(wn);
                }

                if (wn == 0.0) {
                    tau = 0.0;
                } else {
                    wb = work[0] + wa;

                    for (j = 0; j < (m - i); j++) {
                        work[j + 1] = (1.0 / wb) * work[j + 1];
                    }

                    work[0] = 1.0;
                    tau = wb / wa;
                }

                // multiply A(i-1:m-1,i-1:n-1) by random reflection from the left
                B = new double[m - i + 1][n - i + 1];

                for (j = 0; j < (m - i + 1); j++) {

                    for (k = 0; k < (n - i + 1); k++) {
                        B[j][k] = A[j + i - 1][k + i - 1];
                    }
                }

                work2 = new double[work.length - m];

                for (j = 0; j < (work.length - m); j++) {
                    work2[j] = work[m + j];
                }

                ge.dgemv('T', m - i + 1, n - i + 1, 1.0, B, m - i + 1, work, 1, 0.0, work2, 1);
                ge.dger(m - i + 1, n - i + 1, -tau, work, 1, work2, 1, B, m - i + 1);

                for (j = 0; j < (m - i + 1); j++) {

                    for (k = 0; k < (n - i + 1); k++) {
                        A[j + i - 1][k + i - 1] = B[j][k];
                    }
                }

                for (j = 0; j < (work.length - m); j++) {
                    work[m + j] = work2[j];
                }
            } // if (i < m)

            if (i < n) {

                // generate random reflection
                dlarnv(3, iseed, n - i + 1, work);
                wn = ge.dnrm2(n - i + 1, work, 1);

                if (work[0] >= 0) {
                    wa = Math.abs(wn);
                } else {
                    wa = -Math.abs(wn);
                }

                if (wn == 0.0) {
                    tau = 0.0;
                } else {
                    wb = work[0] + wa;

                    for (j = 0; j < (n - i); j++) {
                        work[j + 1] = (1.0 / wb) * work[j + 1];
                    }

                    work[0] = 1.0;
                    tau = wb / wa;
                }

                // multiply A(i-1:m-1,i-1:n-1) by random reflection from right
                B = new double[m - i + 1][n - i + 1];

                for (j = 0; j < (m - i + 1); j++) {

                    for (k = 0; k < (n - i + 1); k++) {
                        B[j][k] = A[j + i - 1][k + i - 1];
                    }
                }

                work2 = new double[work.length - n];

                for (j = 0; j < (work.length - n); j++) {
                    work2[j] = work[n + j];
                }

                ge.dgemv('N', m - i + 1, n - i + 1, 1.0, B, m - i + 1, work, 1, 0.0, work2, 1);
                ge.dger(m - i + 1, n - i + 1, -tau, work2, 1, work, 1, B, m - i + 1);

                for (j = 0; j < (m - i + 1); j++) {

                    for (k = 0; k < (n - i + 1); k++) {
                        A[j + i - 1][k + i - 1] = B[j][k];
                    }
                }

                for (j = 0; j < (work.length - n); j++) {
                    work[n + j] = work2[j];
                }
            } // if (i < n)
        } // for (i = Math.min(m,n); i >= 1; i--)

        // Reduce number of subdiagonals to kl and number of superdiagonals to
        // ku
        for (i = 1; i <= Math.max(m - 1 - kl, n - 1 - ku); i++) {

            if (kl <= ku) {

                // annihilate subdiagonal elements first (necessary if kl = 0)
                if (i <= Math.min(m - 1 - kl, n)) {

                    // generate reflection to annihilate A(kl+i:m-1,i-1)
                    x = new double[m - kl - i + 1];

                    for (j = 0; j < (m - kl - i + 1); j++) {
                        x[j] = A[kl + i - 1 + j][i - 1];
                    }

                    wn = ge.dnrm2(m - kl - i + 1, x, 1);

                    if (A[kl + i - 1][i - 1] >= 0) {
                        wa = Math.abs(wn);
                    } else {
                        wa = -Math.abs(wn);
                    }

                    if (wn == 0.0) {
                        tau = 0.0;
                    } else {
                        wb = A[kl + i - 1][i - 1] + wa;

                        for (j = 0; j < (m - kl - i); j++) {
                            A[kl + i + j][i - 1] = (1.0 / wb) * A[kl + i + j][i - 1];
                        }

                        A[kl + i - 1][i - 1] = 1.0;
                        tau = wb / wa;
                    }

                    // apply reflection to A(kl+i-1:m-1,i:n-1) from the left
                    B = new double[m - kl - i + 1][n - i];

                    for (j = 0; j < (m - kl - i + 1); j++) {

                        for (k = 0; k < (n - i); k++) {
                            B[j][k] = A[kl + i - 1 + j][i + k];
                        }
                    }

                    x = new double[m - kl - i + 1];

                    for (j = 0; j < (m - kl - i + 1); j++) {
                        x[j] = A[kl + i - 1 + j][i - 1];
                    }

                    ge.dgemv('T', m - kl - i + 1, n - i, 1.0, B, m - kl - i + 1, x, 1, 0.0, work, 1);
                    ge.dger(m - kl - i + 1, n - i, -tau, x, 1, work, 1, B, m - kl - i + 1);

                    for (j = 0; j < (m - kl - i + 1); j++) {

                        for (k = 0; k < (n - i); k++) {
                            A[kl + i - 1 + j][i + k] = B[j][k];
                        }
                    }

                    A[kl + i - 1][i - 1] = -wa;
                } // if (i <= Math.min(m-1-kl, n))

                if (i <= Math.min(n - 1 - ku, m)) {

                    // generate reflection to annihilate A(i-1,ku+i:n-1)
                    x = new double[n - ku - i + 1];

                    for (j = 0; j < (n - ku - i + 1); j++) {
                        x[j] = A[i - 1][ku + i - 1 + j];
                    }

                    wn = ge.dnrm2(n - ku - i + 1, x, 1);

                    if (A[i - 1][ku + i - 1] >= 0) {
                        wa = Math.abs(wn);
                    } else {
                        wa = -Math.abs(wn);
                    }

                    if (wn == 0.0) {
                        tau = 0.0;
                    } else {
                        wb = A[i - 1][ku + i - 1] + wa;

                        for (j = 0; j < (n - ku - i); j++) {
                            A[i - 1][ku + i + j] = (1.0 / wb) * A[i - 1][ku + i + j];
                        }

                        A[i - 1][ku + i - 1] = 1.0;
                        tau = wb / wa;
                    }

                    // Apply reflection to A(i:m-1,ku+i-1:n-1) from the right
                    B = new double[m - i][n - ku - i + 1];

                    for (j = 0; j < (m - i); j++) {

                        for (k = 0; k < (n - ku - i + 1); k++) {
                            B[j][k] = A[i + j][ku + i - 1 + k];
                        }
                    }

                    x = new double[n - ku - i + 1];

                    for (j = 0; j < (n - ku - i + 1); j++) {
                        x[j] = A[i - 1][ku + i - 1 + j];
                    }

                    ge.dgemv('N', m - i, n - ku - i + 1, 1.0, B, m - i, x, 1, 0.0, work, 1);
                    ge.dger(m - i, n - ku - i + 1, -tau, work, 1, x, 1, B, m - i);

                    for (j = 0; j < (m - i); j++) {

                        for (k = 0; k < (n - ku - i + 1); k++) {
                            A[i + j][ku + i - 1 + k] = B[j][k];
                        }
                    }

                    A[i - 1][ku + i - 1] = -wa;
                } // if (i <= Math.min(n-1-ku, m))
            } // if (kl <= ku)
            else { // kl > ku

                // annihilate superdiagonal elements first (necessary if ku = 0)
                if (i <= Math.min(n - 1 - ku, m)) {

                    // generate reflection to annihilate A(i-1,ku+i:n-1)
                    x = new double[n - ku - i + 1];

                    for (j = 0; j < (n - ku - i + 1); j++) {
                        x[j] = A[i - 1][ku + i - 1 + j];
                    }

                    wn = ge.dnrm2(n - ku - i + 1, x, 1);

                    if (A[i - 1][ku + i - 1] >= 0) {
                        wa = Math.abs(wn);
                    } else {
                        wa = -Math.abs(wn);
                    }

                    if (wn == 0.0) {
                        tau = 0.0;
                    } else {
                        wb = A[i - 1][ku + i - 1] + wa;

                        for (j = 0; j < (n - ku - i); j++) {
                            A[i - 1][ku + i + j] = (1.0 / wb) * A[i - 1][ku + i + j];
                        }

                        A[i - 1][ku + i - 1] = 1.0;
                        tau = wb / wa;
                    }

                    // apply reflection to A(i:m-1,ku+i-1:n-1) from the right
                    B = new double[m - i][n - ku - i + 1];

                    for (j = 0; j < (m - i); j++) {

                        for (k = 0; k < (n - ku - i + 1); k++) {
                            B[j][k] = A[i + j][ku + i - 1 + k];
                        }
                    }

                    x = new double[n - ku - i + 1];

                    for (j = 0; j < (n - ku - i + 1); j++) {
                        x[j] = A[i - 1][ku + i - 1 + j];
                    }

                    ge.dgemv('N', m - i, n - ku - i + 1, 1.0, B, m - i, x, 1, 0.0, work, 1);
                    ge.dger(m - i, n - ku - i + 1, -tau, work, 1, x, 1, B, m - i);

                    for (j = 0; j < (m - i); j++) {

                        for (k = 0; k < (n - ku - i + 1); k++) {
                            A[i + j][ku + i - 1 + k] = B[j][k];
                        }
                    }

                    A[i - 1][ku + i - 1] = -wa;
                } // if (i <= Math.min(n-1-ku,m))

                if (i <= Math.min(m - 1 - kl, n)) {

                    // generate reflection to annihilate A(kl+i:m-1,i-1)
                    x = new double[m - kl - i + 1];

                    for (j = 0; j < (m - kl - i + 1); j++) {
                        x[j] = A[kl + i - 1 + j][i - 1];
                    }

                    wn = ge.dnrm2(m - kl - i + 1, x, 1);

                    if (A[kl + i - 1][i - 1] >= 0.0) {
                        wa = Math.abs(wn);
                    } else {
                        wa = -Math.abs(wn);
                    }

                    if (wn == 0.0) {
                        tau = 0.0;
                    } else {
                        wb = A[kl + i - 1][i - 1] + wa;

                        for (j = 0; j < (m - kl - i); j++) {
                            A[kl + i + j][i - 1] = (1.0 / wb) * A[kl + i + j][i - 1];
                        }

                        A[kl + i - 1][i - 1] = 1.0;
                        tau = wb / wa;
                    }

                    // apply reflection to A(kl+i-1:m-1,i:n-1) from the left
                    B = new double[m - kl - i + 1][n - i];

                    for (j = 0; j < (m - kl - i + 1); j++) {

                        for (k = 0; k < (n - i); k++) {
                            B[j][k] = A[kl + i - 1 + j][i + k];
                        }
                    }

                    x = new double[m - kl - i + 1];

                    for (j = 0; j < (m - kl - i + 1); j++) {
                        x[j] = A[kl + i - 1 + j][i - 1];
                    }

                    ge.dgemv('T', m - kl - i + 1, n - i, 1.0, B, m - kl - i + 1, x, 1, 0.0, work, 1);
                    ge.dger(m - kl - i + 1, n - i, -tau, x, 1, work, 1, B, m - kl - i + 1);

                    for (j = 0; j < (m - kl - i + 1); j++) {

                        for (k = 0; k < (n - i); k++) {
                            A[kl + i - 1 + j][i + k] = B[j][k];
                        }
                    }

                    A[kl + i - 1][i - 1] = -wa;
                } // if (i <= Math.min(m-1-kl, n))
            } // else kl > ku

            for (j = kl + i + 1; j <= m; j++) {
                A[j - 1][i - 1] = 0.0;
            }

            for (j = ku + i + 1; j <= n; j++) {
                A[i - 1][j - 1] = 0.0;
            }
        } // for (i = 1; i <= Math.max(m-1-kl, n-1-ku); i++)

        return;
    } // dlagge
    
    /**
     * This is a port of version 3.1 LAPACK auxiliary test routine dlagsy Original DLAGSY created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., November, 2006
     * dlagsy generates a real symmetric matrix A, by pre- and post- multiplying a real diagonal matrix D with
     * a random orthogonal matrix: A = U*D*U'. The semi-bandwidth may then be reduced to k by additional orthogonal
     * transformations.
     *
     * @param  n      input int The order of the matrix A. n >= 0.
     * @param  k      input int The number of nonzero subdiagonals within the band of A. 0 <= k <= n-1.
     * @param  D      input double[] of dimension n. The diagonal elements of the diagonal matrix D.
     * @param  A      output double[][] of dimension (lda,n) The generated n by n symmetric matrix A (the full matrix is
     *                stored).
     * @param  lda    input int The leading dimension of the array A. lda >= n.
     * @param  iseed  input/output int[] of dimension 4 On entry, the seed of the random number generator; the array
     *                elements must be between 0 and 4095, and iseed[3] must be odd. On exit, the seed is updated.
     * @param  work   workspace double[] of dimension (2*n)
     * @param  info   output int[] 
     *         = 0: successful exit 
     *         < 0: If info[0] = -i, the i-th argument had an illegal value
     */
    private void dlagsy(int n, int k, double[] D, double[][] A, int lda, int[] iseed, double[] work, int[] info) {
        int i;
        int j;
        int m;
        double alpha;
        double tau;
        double wa;
        double wb = 0.0;
        double wn;
        double[][] B;
        double[] work2 = new double[n];
        double[] x;

        // Test the input arguments
        info[0] = 0;

        if (n < 0) {
            info[0] = -1;
        } else if ((k < 0) || (k > (n - 1))) {
            info[0] = -2;
        } else if (lda < Math.max(1, n)) {
            info[0] = -5;
        }

        if (info[0] < 0) {
            MipavUtil.displayError("Error dlagsy had info[0] = " + info[0]);

            return;
        }

        // initialize lower triangle of A to diagonal matrix
        for (j = 0; j < n; j++) {

            for (i = j + 1; i < n; i++) {
                A[i][j] = 0.0;
            }
        }

        for (i = 0; i < n; i++) {
            A[i][i] = D[i];
        }

        // Generate lower triangle of symmetric matrix
        for (i = n - 1; i >= 1; i--) {

            // generate random reflection
            dlarnv(3, iseed, n - i + 1, work);
            wn = ge.dnrm2(n - i + 1, work, 1);

            if (work[0] >= 0.0) {
                wa = Math.abs(wn);
            } else {
                wa = -Math.abs(wn);
            }

            if (wn == 0.0) {
                tau = 0.0;
            } else {
                wb = work[0] + wa;

                for (j = 0; j < (n - i); j++) {
                    work[j + 1] = (1.0 / wb) * work[j + 1];
                }

                work[0] = 1.0;
                tau = wb / wa;
            }

            // apply random reflection to A(i-1:n-1,i-1:n-1) from the left and
            // the right
            // compute y = tau * A * u
            B = new double[n - i + 1][n - i + 1];

            for (j = 0; j < (n - i + 1); j++) {

                for (m = 0; m < (n - i + 1); m++) {
                    B[j][m] = A[i - 1 + j][i - 1 + m];
                }
            }

            dsymv('L', n - i + 1, tau, B, n - i + 1, work, 1, 0.0, work2, 1);

            // compute v = y - 1/2 * tau * (y, u) * u
            alpha = -0.5 * tau * ddot(n - i + 1, work2, 1, work, 1);
            daxpy(n - i + 1, alpha, work, 1, work2, 1);

            // apply the transformation as a rank-2 update to A(i-1:n-1,i-1:n-1)
            dsyr2('L', n - i + 1, -1.0, work, 1, work2, 1, B, n - i + 1);

            for (j = 0; j < (n - i + 1); j++) {

                for (m = 0; m < (n - i + 1); m++) {
                    A[i - 1 + j][i - 1 + m] = B[j][m];
                }
            }
        } // for (i = n-1; i >= 1; i--)

        // Reduce number of subdiagonals to k
        for (i = 1; i <= (n - 1 - k); i++) {

            // generate reflection to annihilate A(k+i:n-1,i-1)
            x = new double[n - k - i + 1];

            for (j = 0; j < (n - k - i + 1); j++) {
                x[j] = A[k + i - 1 + j][i - 1];
            }

            wn = ge.dnrm2(n - k - i + 1, x, 1);

            if (A[k + i - 1][i - 1] >= 0.0) {
                wa = Math.abs(wn);
            } else {
                wa = -Math.abs(wn);
            }

            if (wn == 0.0) {
                tau = 0.0;
            } else {
                wb = A[k + i - 1][i - 1] + wa;

                for (j = 0; j < (n - k - i); j++) {
                    A[k + i + j][i - 1] = (1.0 / wb) * A[k + i + j][i - 1];
                }

                A[k + i - 1][i - 1] = 1.0;
                tau = wb / wa;
            }

            // apply reflection to A(k+i-1:n-1,i:k+i-2) from the left
            B = new double[n - k - i + 1][k - 1];

            for (j = 0; j < (n - k - i + 1); j++) {

                for (m = 0; m < (k - 1); m++) {
                    B[j][m] = A[k + i - 1 + j][i + m];
                }
            }

            x = new double[n - k - i + 1];

            for (j = 0; j < (n - k - i + 1); j++) {
                x[j] = A[k + i - 1 + j][i - 1];
            }

            ge.dgemv('T', n - k - i + 1, k - 1, 1.0, B, n - k - i + 1, x, 1, 0.0, work, 1);
            ge.dger(n - k - i + 1, k - 1, -tau, x, 1, work, 1, B, n - k - i + 1);

            for (j = 0; j < (n - k - i + 1); j++) {

                for (m = 0; m < (k - 1); m++) {
                    A[k + i - 1 + j][i + m] = B[j][m];
                }
            }

            // apply reflection to A(k+i-1:n-1,k+i-1:n-1) from the left and
            // the right
            // compute y = tau * A * u
            B = new double[n - k - i + 1][n - k - i + 1];

            for (j = 0; j < (n - k - i + 1); j++) {

                for (m = 0; m < (n - k - i + 1); m++) {
                    B[j][m] = A[k + i - 1 + j][k + i - 1 + m];
                }
            }

            x = new double[n - k - i + 1];

            for (j = 0; j < (n - k - i + 1); j++) {
                x[j] = A[k + i - 1 + j][i - 1];
            }

            dsymv('L', n - k - i + 1, tau, B, n - k - i + 1, x, 1, 0.0, work, 1);

            // compute v = y - 1/2 * tau * (y, u) * u
            alpha = -0.5 * tau * ddot(n - k - i + 1, work, 1, x, 1);
            daxpy(n - k - i + 1, alpha, x, 1, work, 1);

            // apply symmetric rank-2 update to A(k+i-1:n-1,k+i-1:n-1)
            dsyr2('L', n - k - i + 1, -1.0, x, 1, work, 1, B, n - k - i + 1);

            for (j = 0; j < (n - k - i + 1); j++) {

                for (m = 0; m < (n - k - i + 1); m++) {
                    A[k + i - 1 + j][k + i - 1 + m] = B[j][m];
                }
            }

            A[k + i - 1][i - 1] = -wa;

            for (j = k + i + 1; j <= n; j++) {
                A[j - 1][i - 1] = 0.0;
            }
        } // for (i = 1; i <= n - 1 - k; i++)

        // Store full symmetric matrix
        for (j = 0; j < n; j++) {

            for (i = j + 1; i < n; i++) {
                A[j][i] = A[i][j];
            }
        }

        return;
    } // dlagsy
    
    /**
     * This is a port of version 3.2 LAPACK auxiliary routine DLARNV Original DLARNV created by Univ. of Tennessee, Univ.
     * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlarnv returns a vector of n random real numbers from a uniform or normal distribution
     *
     * @param  idist  input int Specifies the distribution of the random numbers: 
     *                = 1: uniform (0,1) 
     *                = 2: uniform (-1,1)
     *                = 3: normal (0,1)
     * @param  iseed  input/output int[] of dimension 4. On entry, the seed of the random number generator; the array
     *                elements must be between 0 and 4095, and iseed[3] must be odd. On exit, the seed is updated
     * @param  n      input int The number of random numbers to be generated.
     * @param  x      output double[] of dimension n. The random generated numbers.
     * 
     * The routine calls the auxiliary routine dlaruv to generate random real numbers from a uniform (0,1) distribution,
     * in batches of up to 128 using vectorizable code. The Box-Muller method is used to transform numbers from a uniform
     * to a normal distribution.
     */
    private void dlarnv(int idist, int[] iseed, int n, double[] x) {
        int lv = 128;
        int i;
        int il;
        int il2;
        int iv;
        double[] u = new double[lv];

        for (iv = 1; iv <= n; iv += lv / 2) {
            il = Math.min(lv / 2, n - iv + 1);

            if (idist == 3) {
                il2 = 2 * il;
            } else {
                il2 = il;
            }

            // Call dlaruv to generate il2 numbers from a uniform (0,1)
            // distribution (il2 <= lv)
            dlaruv(iseed, il2, u);

            if (idist == 1) {

                // Copy generated numbers
                for (i = 0; i < il; i++) {
                    x[iv + i - 1] = u[i];
                }
            } // if (idist == 1)
            else if (idist == 2) {

                // Convert generated numbers to uniform (-1,1) distribution
                for (i = 0; i < il; i++) {
                    x[iv + i - 1] = (2.0 * u[i]) - 1.0;
                }
            } // else if (idist == 2)
            else if (idist == 3) {

                // Convert generated numbers to normal (0,1) distribution
                for (i = 1; i <= il; i++) {
                    x[iv + i - 2] = Math.sqrt(-2.0 * Math.log(u[(2 * i) - 2])) *
                                        Math.cos(2.0 * Math.PI * u[(2 * i) - 1]);
                }
            } // else if (idist == 3)

        } // for (iv = 1; iv <= n; iv += lv/2)
        return;
    } // dlarnv
    
    /**
     * This is a port of version 3.1 LAPACK auxiliary routine DLARAN Original DLAAN created by Univ. of Tennessee, Univ.
     * of California Berkeley, and NAG Ltd., November, 2006
     * dlaran returns a random real number from a uniform (0,1) distribution This routine uses a multiplicative
     * congruential method with modulus 2**48 and multiplier 33952834046453 (see G. S. Fishman, "Multiplicative
     * congruential random number generators with modulus 2**b: an exhaustive analysis for b = 32 and a partial analysis
     * for b = 48", Math. Comp. 189, pp. 331-344, 1990).
     *
     * <p>48-bit integers are stored in 4 integer array elements with 12 bits per element. Hence the routine is portable
     * across machines with integers of 32 bits or more.</p>
     *
     * @param   iseed  (input/output) int[] of dimension 4. On entry, the seed of the random number generator; the array
     *                 elements must be between 0 and 4095, and iseed[3] must be odd. On exit, the seed is updated.
     *
     * @return  double
     */
    private double dlaran(int[] iseed) {
        int m1 = 494;
        int m2 = 322;
        int m3 = 2508;
        int m4 = 2549;
        int ipw2 = 4096;
        double r = 1.0 / ipw2;
        int it1;
        int it2;
        int it3;
        int it4;
        double rndout = 1.0;

        while (rndout == 1.0) {
        // Multiply the seed by the multiplier module 2**48
        it4 = iseed[3] * m4;
        it3 = it4 / ipw2;
        it4 = it4 - (ipw2 * it3);
        it3 = it3 + (iseed[2] * m4) + (iseed[3] * m3);
        it2 = it3 / ipw2;
        it3 = it3 - (ipw2 * it2);
        it2 = it2 + (iseed[1] * m4) + (iseed[2] * m3) + (iseed[3] * m2);
        it1 = it2 / ipw2;
        it2 = it2 - (ipw2 * it1);
        it1 = it1 + (iseed[0] * m4) + (iseed[1] * m3) + (iseed[2] * m2) + (iseed[3] * m1);
        it1 = it1 % ipw2;

        // Return updated seed
        iseed[0] = it1;
        iseed[1] = it2;
        iseed[2] = it3;
        iseed[3] = it4;

        // Convert 48-bit integer to a real number in the interval (0,1)
        rndout =  r * (it1 + (r * (it2 + (r * (it3 + (r * it4))))));
        // If a real number has n bits of precision, and the first n bits of the 48-bit integer above happen
        // to be all 1 (which will occur about once every 2**n calls), then rndout will be rounded to exactly 1.0
        // Since dlaran is not supposed to return exactly 0.0 or 1.0, the statistically correct thing to do in
        // this situation is simply to iterate again.  
        // N.B. The case rndout = 0.0 should not be possible.
        } // while (rndout == 1.0)
        return rndout;
    } // dlaran
    
    /**
     * This is a port of the 10/22/86 Blas routine DSYMV Original code written by: Jack Dongarra, Argonne Nationa Lab.
     * Jeremy Du Croz, Nag Central Office. Sven Hammarling, Nag Central Office. Richard Hanson, Sandia National Labs.
     * dsymv performs the matrix-vector operation y = alpha*A*x + beta*y where alpha and beta are scalars, x and y are n
     * element vectors and A is an n by n symmetric matrix.
     *
     * @param  uplo   input char On entry, uplo specifies whether the upper or lower triangular part of the array A is
     *                to be referenced as follows: = 'U' or 'u' Only the upper triangular part of A is to be referenced.
     *                = 'L' or 'l' Only the lower triangular part of A is to be referenced.
     * @param  n      input int On entry, n specifies the order of the matrix A. n must be at least zero.
     * @param  alpha  input double Specified scalar
     * @param  A      input double[][] of dimension lda by n Before entry with uplo = 'U' or 'u', the leading n by n
     *                upper triangular part of the array A must contain the upper triagular part of the symmetric matrix
     *                and the strictly lower triangular part of A is not referenced. Before entry with uplo = 'L' or
     *                'l', the leading n by n lower triangular part of the array A must contain the lower triangular
     *                part of the symmetric matrix and the strictly upper triangular part of A is not referenced.
     * @param  lda    input int On entry, lda specifies the first dimension of A as declared in the calling (sub)
     *                program. lda must be at least max(1,n).
     * @param  x      input double[] of dimension at least (1 + (n-1)*abs(incx)). Before entry, the incremented array x
     *                must contain the n element vector x.
     * @param  incx   input int On entry, incx specifies the increment for the elements of x. incx must not be zero.
     * @param  beta   input double On entry, beta specifies the scalar beta. When beta is supplied as zero, then y need
     *                not be set on input.
     * @param  y      input/output double[] of dimension at least (1 + (n-1)*abs(incy)). Before entry, the incremented
     *                array y must contain the n element vector y. On exit, y is overwritten by the updated vector y.
     * @param  incy   input int On entry, incy specifies the increment for the elements of y. incy must not be zero.
     */
    private void dsymv(char uplo, int n, double alpha, double[][] A, int lda, double[] x, int incx, double beta,
                       double[] y, int incy) {
        double temp1;
        double temp2;
        int i;
        int info;
        int ix;
        int iy;
        int j;
        int jx;
        int jy;
        int kx;
        int ky;

        // Test the input parameters
        info = 0;

        if ((uplo != 'U') && (uplo != 'u') && (uplo != 'L') && (uplo != 'l')) {
            info = 1;
        } else if (n < 0) {
            info = 2;
        } else if (lda < Math.max(1, n)) {
            info = 5;
        } else if (incx == 0) {
            info = 7;
        } else if (incy == 0) {
            info = 10;
        }

        if (info != 0) {
            MipavUtil.displayError("Error dsymv had error = " + info);

            return;
        }

        // Quick return if possible
        if ((n == 0) || ((alpha == 0.0) && (beta == 1.0))) {
            return;
        }

        // Set up the start points in x and y
        if (incx > 0) {
            kx = 1;
        } else {
            kx = 1 - ((n - 1) * incx);
        }

        if (incy > 0) {
            ky = 1;
        } else {
            ky = 1 - ((n - 1) * incy);
        }

        // Start the operations.  In this version the elements of A are accessed
        // sequentially with one pass through the triangular part of A.

        // First form y = beta*y.

        if (beta != 1.0) {

            if (incy == 1) {

                if (beta == 0.0) {

                    for (i = 0; i < n; i++) {
                        y[i] = 0.0;
                    }
                } // if (beta == 0.0)
                else { // beta != 0.0

                    for (i = 0; i < n; i++) {
                        y[i] = beta * y[i];
                    }
                } // else beta != 0.0
            } // if (incy == 1)
            else { // incy != 1
                iy = ky - 1;

                if (beta == 0.0) {

                    for (i = 0; i < n; i++) {
                        y[iy] = 0.0;
                        iy = iy + incy;
                    }
                } // if (beta == 0.0)
                else { // beta != 0.0

                    for (i = 0; i < n; i++) {
                        y[iy] = beta * y[iy];
                        iy = iy + incy;
                    }
                } // else beta != 0.0
            } // else incy != 1
        } // if (beta != 1.0)

        if (alpha == 0.0) {
            return;
        }

        if ((uplo == 'U') || (uplo == 'u')) {

            // Form y when A is stored in upper triangle
            if ((incx == 1) && (incy == 1)) {

                for (j = 0; j < n; j++) {
                    temp1 = alpha * x[j];
                    temp2 = 0.0;

                    for (i = 0; i <= (j - 1); i++) {
                        y[i] = y[i] + (temp1 * A[i][j]);
                        temp2 = temp2 + (A[i][j] * x[i]);
                    } // for (i = 0; i <= j-1; i++)

                    y[j] = y[j] + (temp1 * A[j][j]) + (alpha * temp2);
                } // for (j = 0; j < n; j++)
            } // if ((incx == 1) && (incy == 1))
            else { // ((incx != 1) || (incy != 1))
                jx = kx - 1;
                jy = ky - 1;

                for (j = 0; j < n; j++) {
                    temp1 = alpha * x[jx];
                    temp2 = 0.0;
                    ix = kx - 1;
                    iy = ky - 1;

                    for (i = 0; i <= (j - 1); i++) {
                        y[iy] = y[iy] + (temp1 * A[i][j]);
                        temp2 = temp2 + (A[i][j] * x[ix]);
                        ix = ix + incx;
                        iy = iy + incy;
                    } // for (i = 0; i <= j-1; i++)

                    y[jy] = y[jy] + (temp1 * A[j][j]) + (alpha * temp2);
                    jx = jx + incx;
                    jy = jy + incy;
                } // for (j = 0; j < n; j++)
            } // else ((incx != 1) || (incy != 1))
        } // if ((uplo == 'U') || (uplo == 'u'))
        else { // ((uplo == 'L') || (uplo == 'l'))

            // Form y when A is stored in lower triangle
            if ((incx == 1) && (incy == 1)) {

                for (j = 0; j < n; j++) {
                    temp1 = alpha * x[j];
                    temp2 = 0.0;
                    y[j] = y[j] + (temp1 * A[j][j]);

                    for (i = j + 1; i < n; i++) {
                        y[i] = y[i] + (temp1 * A[i][j]);
                        temp2 = temp2 + (A[i][j] * x[i]);
                    } // for (i = j+1; i < n; i++)

                    y[j] = y[j] + (alpha * temp2);
                } // for (j = 0; j < n; j++)
            } // if ((incx == 1) && (incy == 1))
            else { // ((incx != 1) || (incy != 1))
                jx = kx - 1;
                jy = ky - 1;

                for (j = 0; j < n; j++) {
                    temp1 = alpha * x[jx];
                    temp2 = 0.0;
                    y[jy] = y[jy] + (temp1 * A[j][j]);
                    ix = jx;
                    iy = jy;

                    for (i = j + 1; i < n; i++) {
                        ix = ix + incx;
                        iy = iy + incy;
                        y[iy] = y[iy] + (temp1 * A[i][j]);
                        temp2 = temp2 + (A[i][j] * x[ix]);
                    } // for (i = j+1; i < n; i++)

                    y[jy] = y[jy] + (alpha * temp2);
                    jx = jx + incx;
                    jy = jy + incy;
                } // for (j = 0; j < n; j++)
            } // else ((incx != 1) || (incy != 1))
        } // else ((uplo == 'L') || (uplo == 'l'))

        return;
    } // dsymv
    
    /**
     * Port of 12/3/93 linpack ddot routine Original version created by Jack Dongarra Forms the dot product of two
     * vectors.
     *
     * @param   n     int
     * @param   dx    double[]
     * @param   incx  int
     * @param   dy    double[]
     * @param   incy  int
     *
     * @return  double answer
     */
    private double ddot(int n, double[] dx, int incx, double[] dy, int incy) {
        double answer;
        int ix;
        int iy;
        int i;
        int m;
        int mp1;

        answer = 0.0;

        if (n <= 0) {
            return 0.0;
        }

        if ((incx != 1) || (incy != 1)) {

            // Code for unequal increments or equal increments not equal to 1
            ix = 0;
            iy = 0;

            if (incx < 0) {
                ix = (-n + 1) * incx;
            }

            if (incy < 0) {
                iy = (-n + 1) * incy;
            }

            for (i = 0; i < n; i++) {
                answer = answer + (dx[ix] * dy[iy]);
                ix = ix + incx;
                iy = iy + incy;
            } // for (i = 0; i < n; i++)

            return answer;
        } // if ((incx != 1) || (incy != 1))

        // Code for both increments equal to 1
        m = n % 5;

        if (m != 0) {

            for (i = 0; i < m; i++) {
                answer = answer + (dx[i] * dy[i]);
            }

            if (n < 5) {
                return answer;
            }
        } // if (m != 0)

        mp1 = m + 1;

        for (i = mp1; i <= n; i += 5) {
            answer = answer + (dx[i - 1] * dy[i - 1]) + (dx[i] * dy[i]) + (dx[i + 1] * dy[i + 1]) +
                     (dx[i + 2] * dy[i + 2]) + (dx[i + 3] * dy[i + 3]);
        } // for (i = mp1; i <= n; i += 5)

        return answer;
    } // ddot
    
    /**
     * Port of 12/3/93 linpack routine daxpy Original version written by Jack Dongarra vector dy = vector dy + da *
     * vector dx.
     *
     * @param  n     input int
     * @param  da    inut double
     * @param  dx    input double[]
     * @param  incx  input int
     * @param  dy    input/output double[]
     * @param  incy  input int
     */
    private void daxpy(int n, double da, double[] dx, int incx, double[] dy, int incy) {
        int i;
        int ix;
        int iy;
        int m;
        int mp1;

        if (n <= 0) {
            return;
        }

        if (da == 0.0) {
            return;
        }

        if ((incx != 1) || (incy != 1)) {

            // Code for unequal increments or equal increments not equal to 1
            ix = 1;
            iy = 1;

            if (incx < 0) {
                ix = ((-n + 1) * incx) + 1;
            }

            if (incy < 0) {
                iy = ((-n + 1) * incy) + 1;
            }

            for (i = 1; i <= n; i++) {
                dy[iy - 1] = dy[iy - 1] + (da * dx[ix - 1]);
                ix = ix + incx;
                iy = iy + incy;
            }

            return;
        } // if ((incx != 1) || (incy != 1))

        // Code for both increments equal to 1
        m = n % 4;

        if (m != 0) {

            for (i = 0; i < m; i++) {
                dy[i] = dy[i] + (da * dx[i]);
            }

            if (n < 4) {
                return;
            }
        } // if (m != 0)

        mp1 = m + 1;

        for (i = mp1; i <= n; i += 4) {
            dy[i - 1] = dy[i - 1] + (da * dx[i - 1]);
            dy[i] = dy[i] + (da * dx[i]);
            dy[i + 1] = dy[i + 1] + (da * dx[i + 1]);
            dy[i + 2] = dy[i + 2] + (da * dx[i + 2]);
        }

        return;
    } // daxpy
    
    /**
     * Port of 10/22/86 Blas DSYR2 routine Original version written by: Jack Dongarra, Argonne National Lab. Jeremy Du
     * Croz, Nag Central Office Sven Hammarling, Nag Central Office. Richard Hanson, Sandia National Labs. dsyr2
     * performs the symmetric rank 2 operation A = alpha*x*y' + alpha*y*x' + A, where alpha is a scalar, x and y are n
     * element vectors, and A is an n by n symmetric matrix.
     *
     * @param  uplo   input char On entry, uplo specifies whether the upper or lower triangular part of the array A is
     *                to be referenced as follows: = 'U' or 'u' Only the upper triangular part of A is to be referenced.
     *                = 'L' or 'l' Only the lower triangular part of A is to be referenced.
     * @param  n      input int On entry, n specifies the order of the matrix A. n must be at least zero.
     * @param  alpha  input double specified scalar
     * @param  x      input double[] of dimension at least (1 + (n-1)*abs(incx)). Before entry, the incremented array x
     *                must contain the n element vector x.
     * @param  incx   input int On entry, incx specifies the increment for the elements of x. incx must not be zero.
     * @param  y      input double[] of dimension at least (1 + (n-1)*abs(incy)). Before entry, the incremented array y
     *                must contain the n element vector y.
     * @param  incy   input int On entry, incy specifies the increment for the elements of y. incy must not be zero.
     * @param  A      input/output double[][] of dimension lda by n. Before entry with uplo = 'U' or 'u', the leading n
     *                by n upper triangular part of the array A must contain the upper triangular part of the symmetric
     *                matrix and the strictly lower triangular part of A is not referenced. On exit, the upper
     *                triangular part of the array A is overwritten by the upper triangular part of the updated matrix.
     *                Before entry with uplo = 'L' or 'l', the leading n by n lower triangular part of the array A must
     *                contain the lower triangular part of the symmetric matrix and the strictly upper triangular part
     *                of A is not referenced. On exit, the lower triangular part of the array A is overwritten by the
     *                lower triangular part of the updated matrix.
     * @param  lda    input int On entry, lda specifies the first dimension of A as declared in the calling (sub)
     *                program. lda must be at least max(1,n).
     */
    private void dsyr2(char uplo, int n, double alpha, double[] x, int incx, double[] y, int incy, double[][] A,
                       int lda) {
        double temp1;
        double temp2;
        int i;
        int info;
        int ix;
        int iy;
        int j;
        int jx = 0;
        int jy = 0;
        int kx = 1;
        int ky = 1;

        // Test the input parameters
        info = 0;

        if ((uplo != 'U') && (uplo != 'u') && (uplo != 'L') && (uplo != 'l')) {
            info = 1;
        } else if (n < 0) {
            info = 2;
        } else if (incx == 0) {
            info = 5;
        } else if (incy == 0) {
            info = 7;
        } else if (lda < Math.max(1, n)) {
            info = 9;
        }

        if (info != 0) {
            MipavUtil.displayError("Error dsyr2 had info = " + info);

            return;
        }

        // Quick return if possible
        if ((n == 0) || (alpha == 0.0)) {
            return;
        }

        // Set up the start points in x and y if the increments are not both unity.

        if ((incx != 1) || (incy != 1)) {

            if (incx > 0) {
                kx = 1;
            } else {
                kx = 1 - ((n - 1) * incx);
            }

            if (incy > 0) {
                ky = 1;
            } else {
                ky = 1 - ((n - 1) * incy);
            }

            jx = kx - 1;
            jy = ky - 1;
        } // if ((incx != 1) || (incy != 1))

        // Start the operations.  In this version the elements of A are accessed
        // sequentially with one pass through the triangular part of A.
        if ((uplo == 'U') || (uplo == 'u')) {

            // Form A when A is stored in the upper triangle
            if ((incx == 1) && (incy == 1)) {

                for (j = 0; j < n; j++) {

                    if ((x[j] != 0.0) || (y[j] != 0.0)) {
                        temp1 = alpha * y[j];
                        temp2 = alpha * x[j];

                        for (i = 0; i <= j; i++) {
                            A[i][j] = A[i][j] + (x[i] * temp1) + (y[i] * temp2);
                        }
                    } // if (x[j] != 0.0) || (y[j] != 0.0))
                } // for (j = 0; j < n; j++)
            } // if ((incx == 1) && (incy == 1))
            else { // ((incx != 1) || (incy != 1))

                for (j = 0; j < n; j++) {

                    if ((x[jx] != 0.0) || (y[jy] != 0.0)) {
                        temp1 = alpha * y[jy];
                        temp2 = alpha * x[jx];
                        ix = kx - 1;
                        iy = ky - 1;

                        for (i = 0; i <= j; i++) {
                            A[i][j] = A[i][j] + (x[ix] * temp1) + (y[iy] * temp2);
                            ix = ix + incx;
                            iy = iy + incy;
                        } // for (i = 0; i <= j; i++)
                    } // if ((x[jx] != 0.0) || (y[jy] != 0.0))

                    jx = jx + incx;
                    jy = jy + incy;
                } // for (j = 0; j < n; j++)
            } // else ((incx != 1) || (incy != 1))
        } // if ((uplo == 'U') || (uplo == 'u'))
        else { // ((uplo == 'L') || (uplo == 'l'))

            // Form A when A is stored in the lower triangle
            if ((incx == 1) && (incy == 1)) {

                for (j = 0; j < n; j++) {

                    if ((x[j] != 0.0) || (y[j] != 0.0)) {
                        temp1 = alpha * y[j];
                        temp2 = alpha * x[j];

                        for (i = j; i < n; i++) {
                            A[i][j] = A[i][j] + (x[i] * temp1) + (y[i] * temp2);
                        }
                    } // if ((x[j] != 0.0) || (y[j] != 0.0))
                } // for (j = 0; j < n; j++)
            } // if ((incx == 1) && (incy == 1))
            else { // ((incx != 1) || (incy != 1))

                for (j = 0; j < n; j++) {

                    if ((x[jx] != 0.0) || (y[jy] != 0.0)) {
                        temp1 = alpha * y[jy];
                        temp2 = alpha * x[jx];
                        ix = jx;
                        iy = jy;

                        for (i = j; i < n; i++) {
                            A[i][j] = A[i][j] + (x[ix] * temp1) + (y[iy] * temp2);
                            ix = ix + incx;
                            iy = iy + incy;
                        } // for (i = j; i < n; i++)
                    } // if ((x[jx] != 0.0) || (y[jy] != 0.0))

                    jx = jx + incx;
                    jy = jy + incy;
                } // for (j = 0; j < n; j++)
            } // else ((incx != 1) || (incy != 1))
        } // else ((uplo == 'L') || (uplo == 'l'))

        return;
    } // dsyr2
    
    /**
     * This is a port of version 3.2 LAPACK auxiliary routine DLARUV Original DLARUV created by Univ. of Tennessee, Univ.
     * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlaruv returns a vector of n random real numbers from a uniform (0,1) distribution (n <= 128). This is an
     * auxiliary routine called by dlarnv.
     *
     * @param  iseed  input/output int[] of dimension 4 On entry, the seed of the random number generator; the array
     *                elements must be between 0 and 4095, and iseed[3] must be odd. On exit, the seed is updated.
     * @param  n      input int The number of random numbers to be generated. n <= 128.
     * @param  x      output double[] of dimension n. The generated random numbers.
     * 
     * This routine uses a multiplicative congruential method with modulus 2**48 and multiplier 33952834046453
     * (see G.S. Fishman, "Multiplicative congruential random number generators with modulus 2**b: an exhaustive analysis
     * for b = 32 and a partial analysis for b = 48", Math. Comp. 189, pp. 331-344, 1990).
     * 
     * 48-bit integers are stored in 4 integer array elements with 12 bits per element. Hence the routine is
     * portable across machines with integers of 32 bits or more.
     */
    private void dlaruv(int[] iseed, int n, double[] x) {
        int lv = 128;
        int ipw2 = 4096;
        double r = 1.0 / ipw2;
        int i;
        int i1;
        int i2;
        int i3;
        int i4;
        int it1 = 0;
        int it2 = 0;
        int it3 = 0;
        int it4 = 0;
        int[][] mm = new int[lv][4];

        mm[0][0] = 494;
        mm[0][1] = 322;
        mm[0][2] = 2508;
        mm[0][3] = 2549;
        mm[1][0] = 2637;
        mm[1][1] = 789;
        mm[1][2] = 3754;
        mm[1][3] = 1145;
        mm[2][0] = 255;
        mm[2][1] = 1440;
        mm[2][2] = 1766;
        mm[2][3] = 2253;
        mm[3][0] = 2008;
        mm[3][1] = 752;
        mm[3][2] = 3572;
        mm[3][3] = 305;
        mm[4][0] = 1253;
        mm[4][1] = 2859;
        mm[4][2] = 2893;
        mm[4][3] = 3301;
        mm[5][0] = 3344;
        mm[5][1] = 123;
        mm[5][2] = 307;
        mm[5][3] = 1065;
        mm[6][0] = 4084;
        mm[6][1] = 1848;
        mm[6][2] = 1297;
        mm[6][3] = 3133;
        mm[7][0] = 1739;
        mm[7][1] = 643;
        mm[7][2] = 3966;
        mm[7][3] = 2913;
        mm[8][0] = 3143;
        mm[8][1] = 2405;
        mm[8][2] = 758;
        mm[8][3] = 3285;
        mm[9][0] = 3468;
        mm[9][1] = 2638;
        mm[9][2] = 2598;
        mm[9][3] = 1241;
        mm[10][0] = 688;
        mm[10][1] = 2344;
        mm[10][2] = 3406;
        mm[10][3] = 1197;
        mm[11][0] = 1657;
        mm[11][1] = 46;
        mm[11][2] = 2922;
        mm[11][3] = 3729;
        mm[12][0] = 1238;
        mm[12][1] = 3814;
        mm[12][2] = 1038;
        mm[12][3] = 2501;
        mm[13][0] = 3166;
        mm[13][1] = 913;
        mm[13][2] = 2934;
        mm[13][3] = 1673;
        mm[14][0] = 1292;
        mm[14][1] = 3649;
        mm[14][2] = 2091;
        mm[14][3] = 541;
        mm[15][0] = 3422;
        mm[15][1] = 339;
        mm[15][2] = 2451;
        mm[15][3] = 2753;
        mm[16][0] = 1270;
        mm[16][1] = 3808;
        mm[16][2] = 1580;
        mm[16][3] = 949;
        mm[17][0] = 2016;
        mm[17][1] = 822;
        mm[17][2] = 1958;
        mm[17][3] = 2361;
        mm[18][0] = 154;
        mm[18][1] = 2832;
        mm[18][2] = 2055;
        mm[18][3] = 1165;
        mm[19][0] = 2862;
        mm[19][1] = 3078;
        mm[19][2] = 1507;
        mm[19][3] = 4081;
        mm[20][0] = 697;
        mm[20][1] = 3633;
        mm[20][2] = 1078;
        mm[20][3] = 2725;
        mm[21][0] = 1706;
        mm[21][1] = 2970;
        mm[21][2] = 3273;
        mm[21][3] = 3305;
        mm[22][0] = 491;
        mm[22][1] = 637;
        mm[22][2] = 17;
        mm[22][3] = 3069;
        mm[23][0] = 931;
        mm[23][1] = 2249;
        mm[23][2] = 854;
        mm[23][3] = 3617;
        mm[24][0] = 1444;
        mm[24][1] = 2081;
        mm[24][2] = 2916;
        mm[24][3] = 3733;
        mm[25][0] = 444;
        mm[25][1] = 4019;
        mm[25][2] = 3971;
        mm[25][3] = 409;
        mm[26][0] = 3577;
        mm[26][1] = 1478;
        mm[26][2] = 2889;
        mm[26][3] = 2157;
        mm[27][0] = 3944;
        mm[27][1] = 242;
        mm[27][2] = 3831;
        mm[27][3] = 1361;
        mm[28][0] = 2184;
        mm[28][1] = 481;
        mm[28][2] = 2621;
        mm[28][3] = 3973;
        mm[29][0] = 1661;
        mm[29][1] = 2075;
        mm[29][2] = 1541;
        mm[29][3] = 1865;
        mm[30][0] = 3482;
        mm[30][1] = 4058;
        mm[30][2] = 893;
        mm[30][3] = 2525;
        mm[31][0] = 657;
        mm[31][1] = 622;
        mm[31][2] = 736;
        mm[31][3] = 1409;
        mm[32][0] = 3023;
        mm[32][1] = 3376;
        mm[32][2] = 3992;
        mm[32][3] = 3445;
        mm[33][0] = 3618;
        mm[33][1] = 812;
        mm[33][2] = 787;
        mm[33][3] = 3577;
        mm[34][0] = 1267;
        mm[34][1] = 234;
        mm[34][2] = 2125;
        mm[34][3] = 77;
        mm[35][0] = 1828;
        mm[35][1] = 641;
        mm[35][2] = 2364;
        mm[35][3] = 3761;
        mm[36][0] = 164;
        mm[36][1] = 4005;
        mm[36][2] = 2460;
        mm[36][3] = 2149;
        mm[37][0] = 3798;
        mm[37][1] = 1122;
        mm[37][2] = 257;
        mm[37][3] = 1449;
        mm[38][0] = 3087;
        mm[38][1] = 3135;
        mm[38][2] = 1574;
        mm[38][3] = 3005;
        mm[39][0] = 2400;
        mm[39][1] = 2640;
        mm[39][2] = 3912;
        mm[39][3] = 225;
        mm[40][0] = 2870;
        mm[40][1] = 2302;
        mm[40][2] = 1216;
        mm[40][3] = 85;
        mm[41][0] = 3876;
        mm[41][1] = 40;
        mm[41][2] = 3248;
        mm[41][3] = 3673;
        mm[42][0] = 1905;
        mm[42][1] = 1832;
        mm[42][2] = 3401;
        mm[42][3] = 3117;
        mm[43][0] = 1593;
        mm[43][1] = 2247;
        mm[43][2] = 2124;
        mm[43][3] = 3089;
        mm[44][0] = 1797;
        mm[44][1] = 2034;
        mm[44][2] = 2762;
        mm[44][3] = 1349;
        mm[45][0] = 1234;
        mm[45][1] = 2637;
        mm[45][2] = 149;
        mm[45][3] = 2057;
        mm[46][0] = 3460;
        mm[46][1] = 1287;
        mm[46][2] = 2245;
        mm[46][3] = 413;
        mm[47][0] = 328;
        mm[47][1] = 1691;
        mm[47][2] = 166;
        mm[47][3] = 65;
        mm[48][0] = 2861;
        mm[48][1] = 496;
        mm[48][2] = 466;
        mm[48][3] = 1845;
        mm[49][0] = 1950;
        mm[49][1] = 1597;
        mm[49][2] = 4018;
        mm[49][3] = 697;
        mm[50][0] = 617;
        mm[50][1] = 2394;
        mm[50][2] = 1399;
        mm[50][3] = 3085;
        mm[51][0] = 2070;
        mm[51][1] = 2584;
        mm[51][2] = 190;
        mm[51][3] = 3441;
        mm[52][0] = 3331;
        mm[52][1] = 1843;
        mm[52][2] = 2879;
        mm[52][3] = 1573;
        mm[53][0] = 769;
        mm[53][1] = 336;
        mm[53][2] = 153;
        mm[53][3] = 3689;
        mm[54][0] = 1558;
        mm[54][1] = 1472;
        mm[54][2] = 2320;
        mm[54][3] = 2941;
        mm[55][0] = 2412;
        mm[55][1] = 2407;
        mm[55][2] = 18;
        mm[55][3] = 929;
        mm[56][0] = 2800;
        mm[56][1] = 433;
        mm[56][2] = 712;
        mm[56][3] = 533;
        mm[57][0] = 189;
        mm[57][1] = 2096;
        mm[57][2] = 2159;
        mm[57][3] = 2841;
        mm[58][0] = 287;
        mm[58][1] = 1761;
        mm[58][2] = 2318;
        mm[58][3] = 4077;
        mm[59][0] = 2045;
        mm[59][1] = 2810;
        mm[59][2] = 2091;
        mm[59][3] = 721;
        mm[60][0] = 1227;
        mm[60][1] = 566;
        mm[60][2] = 3443;
        mm[60][3] = 2821;
        mm[61][0] = 2838;
        mm[61][1] = 442;
        mm[61][2] = 1510;
        mm[61][3] = 2249;
        mm[62][0] = 209;
        mm[62][1] = 41;
        mm[62][2] = 449;
        mm[62][3] = 2397;
        mm[63][0] = 2770;
        mm[63][1] = 1238;
        mm[63][2] = 1956;
        mm[63][3] = 2817;
        mm[64][0] = 3654;
        mm[64][1] = 1086;
        mm[64][2] = 2201;
        mm[64][3] = 245;
        mm[65][0] = 3993;
        mm[65][1] = 603;
        mm[65][2] = 3137;
        mm[65][3] = 1913;
        mm[66][0] = 192;
        mm[66][1] = 840;
        mm[66][2] = 3399;
        mm[66][3] = 1997;
        mm[67][0] = 2253;
        mm[67][1] = 3168;
        mm[67][2] = 1321;
        mm[67][3] = 3121;
        mm[68][0] = 3491;
        mm[68][1] = 1499;
        mm[68][2] = 2271;
        mm[68][3] = 997;
        mm[69][0] = 2889;
        mm[69][1] = 1084;
        mm[69][2] = 3667;
        mm[69][3] = 1833;
        mm[70][0] = 2857;
        mm[70][1] = 3438;
        mm[70][2] = 2703;
        mm[70][3] = 2877;
        mm[71][0] = 2094;
        mm[71][1] = 2408;
        mm[71][2] = 629;
        mm[71][3] = 1633;
        mm[72][0] = 1818;
        mm[72][1] = 1589;
        mm[72][2] = 2365;
        mm[72][3] = 981;
        mm[73][0] = 688;
        mm[73][1] = 2391;
        mm[73][2] = 2431;
        mm[73][3] = 2009;
        mm[74][0] = 1407;
        mm[74][1] = 288;
        mm[74][2] = 1113;
        mm[74][3] = 941;
        mm[75][0] = 634;
        mm[75][1] = 26;
        mm[75][2] = 3922;
        mm[75][3] = 2449;
        mm[76][0] = 3231;
        mm[76][1] = 512;
        mm[76][2] = 2554;
        mm[76][3] = 197;
        mm[77][0] = 815;
        mm[77][1] = 1456;
        mm[77][2] = 184;
        mm[77][3] = 2441;
        mm[78][0] = 3524;
        mm[78][1] = 171;
        mm[78][2] = 2099;
        mm[78][3] = 285;
        mm[79][0] = 1914;
        mm[79][1] = 1677;
        mm[79][2] = 3228;
        mm[79][3] = 1473;
        mm[80][0] = 516;
        mm[80][1] = 2657;
        mm[80][2] = 4012;
        mm[80][3] = 2741;
        mm[81][0] = 164;
        mm[81][1] = 2270;
        mm[81][2] = 1921;
        mm[81][3] = 3129;
        mm[82][0] = 303;
        mm[82][1] = 2587;
        mm[82][2] = 3452;
        mm[82][3] = 909;
        mm[83][0] = 2144;
        mm[83][1] = 2961;
        mm[83][2] = 3901;
        mm[83][3] = 2801;
        mm[84][0] = 3480;
        mm[84][1] = 1970;
        mm[84][2] = 572;
        mm[84][3] = 421;
        mm[85][0] = 119;
        mm[85][1] = 1817;
        mm[85][2] = 3309;
        mm[85][3] = 4073;
        mm[86][0] = 3357;
        mm[86][1] = 676;
        mm[86][2] = 3171;
        mm[86][3] = 2813;
        mm[87][0] = 837;
        mm[87][1] = 1410;
        mm[87][2] = 817;
        mm[87][3] = 2337;
        mm[88][0] = 2826;
        mm[88][1] = 3723;
        mm[88][2] = 3039;
        mm[88][3] = 1429;
        mm[89][0] = 2332;
        mm[89][1] = 2803;
        mm[89][2] = 1696;
        mm[89][3] = 1177;
        mm[90][0] = 2089;
        mm[90][1] = 3185;
        mm[90][2] = 1256;
        mm[90][3] = 1901;
        mm[91][0] = 3780;
        mm[91][1] = 184;
        mm[91][2] = 3715;
        mm[91][3] = 81;
        mm[92][0] = 1700;
        mm[92][1] = 663;
        mm[92][2] = 2077;
        mm[92][3] = 1669;
        mm[93][0] = 3712;
        mm[93][1] = 499;
        mm[93][2] = 3019;
        mm[93][3] = 2633;
        mm[94][0] = 150;
        mm[94][1] = 3784;
        mm[94][2] = 1497;
        mm[94][3] = 2269;
        mm[95][0] = 2000;
        mm[95][1] = 1631;
        mm[95][2] = 1101;
        mm[95][3] = 129;
        mm[96][0] = 3375;
        mm[96][1] = 1925;
        mm[96][2] = 717;
        mm[96][3] = 1141;
        mm[97][0] = 1621;
        mm[97][1] = 3912;
        mm[97][2] = 51;
        mm[97][3] = 249;
        mm[98][0] = 3090;
        mm[98][1] = 1398;
        mm[98][2] = 981;
        mm[98][3] = 3917;
        mm[99][0] = 3765;
        mm[99][1] = 1349;
        mm[99][2] = 1978;
        mm[99][3] = 2481;
        mm[100][0] = 1149;
        mm[100][1] = 1441;
        mm[100][2] = 1813;
        mm[100][3] = 3941;
        mm[101][0] = 3146;
        mm[101][1] = 2224;
        mm[101][2] = 3881;
        mm[101][3] = 2217;
        mm[102][0] = 33;
        mm[102][1] = 2411;
        mm[102][2] = 76;
        mm[102][3] = 2749;
        mm[103][0] = 3082;
        mm[103][1] = 1907;
        mm[103][2] = 3846;
        mm[103][3] = 3041;
        mm[104][0] = 2741;
        mm[104][1] = 3192;
        mm[104][2] = 3694;
        mm[104][3] = 1877;
        mm[105][0] = 359;
        mm[105][1] = 2786;
        mm[105][2] = 1682;
        mm[105][3] = 345;
        mm[106][0] = 3316;
        mm[106][1] = 382;
        mm[106][2] = 124;
        mm[106][3] = 2861;
        mm[107][0] = 1749;
        mm[107][1] = 37;
        mm[107][2] = 1660;
        mm[107][3] = 1809;
        mm[108][0] = 185;
        mm[108][1] = 759;
        mm[108][2] = 3997;
        mm[108][3] = 3141;
        mm[109][0] = 2784;
        mm[109][1] = 2948;
        mm[109][2] = 479;
        mm[109][3] = 2825;
        mm[110][0] = 2202;
        mm[110][1] = 1862;
        mm[110][2] = 1141;
        mm[110][3] = 157;
        mm[111][0] = 2199;
        mm[111][1] = 3802;
        mm[111][2] = 886;
        mm[111][3] = 2881;
        mm[112][0] = 1364;
        mm[112][1] = 2423;
        mm[112][2] = 3514;
        mm[112][3] = 3637;
        mm[113][0] = 1244;
        mm[113][1] = 2051;
        mm[113][2] = 1301;
        mm[113][3] = 1465;
        mm[114][0] = 2020;
        mm[114][1] = 2295;
        mm[114][2] = 3604;
        mm[114][3] = 2829;
        mm[115][0] = 3160;
        mm[115][1] = 1332;
        mm[115][2] = 1888;
        mm[115][3] = 2161;
        mm[116][0] = 2785;
        mm[116][1] = 1832;
        mm[116][2] = 1836;
        mm[116][3] = 3365;
        mm[117][0] = 2772;
        mm[117][1] = 2405;
        mm[117][2] = 1990;
        mm[117][3] = 361;
        mm[118][0] = 1217;
        mm[118][1] = 3638;
        mm[118][2] = 2058;
        mm[118][3] = 2685;
        mm[119][0] = 1822;
        mm[119][1] = 3661;
        mm[119][2] = 692;
        mm[119][3] = 3745;
        mm[120][0] = 1245;
        mm[120][1] = 327;
        mm[120][2] = 1194;
        mm[120][3] = 2325;
        mm[121][0] = 2252;
        mm[121][1] = 3660;
        mm[121][2] = 20;
        mm[121][3] = 3609;
        mm[122][0] = 3904;
        mm[122][1] = 716;
        mm[122][2] = 3285;
        mm[122][3] = 3821;
        mm[123][0] = 2774;
        mm[123][1] = 1842;
        mm[123][2] = 2046;
        mm[123][3] = 3537;
        mm[124][0] = 997;
        mm[124][1] = 3987;
        mm[124][2] = 2107;
        mm[124][3] = 517;
        mm[125][0] = 2573;
        mm[125][1] = 1368;
        mm[125][2] = 3508;
        mm[125][3] = 3017;
        mm[126][0] = 1148;
        mm[126][1] = 1848;
        mm[126][2] = 3525;
        mm[126][3] = 2141;
        mm[127][0] = 545;
        mm[127][1] = 2366;
        mm[127][2] = 3801;
        mm[127][3] = 1537;

        i1 = iseed[0];
        i2 = iseed[1];
        i3 = iseed[2];
        i4 = iseed[3];

        for (i = 0; i < Math.min(n, lv); i++) {
            // Multiply the seed by the (i+1)-th power of the multiplier modulo
            // 2**48
            while (true) {
                it4 = i4 * mm[i][3];
                it3 = it4 / ipw2;
                it4 = it4 - (ipw2 * it3);
                it3 = it3 + (i3 * mm[i][3]) + (i4 * mm[i][2]);
                it2 = it3 / ipw2;
                it3 = it3 - (ipw2 * it2);
                it2 = it2 + (i2 * mm[i][3]) + (i3 * mm[i][2]) + (i4 * mm[i][1]);
                it1 = it2 / ipw2;
                it2 = it2 - (ipw2 * it1);
                it1 = it1 + (i1 * mm[i][3]) + (i2 * mm[i][2]) + (i3 * mm[i][1]) + (i4 * mm[i][0]);
                it1 = it1 % ipw2;
    
                // Convert 48-bit integer to a real number in the interval (0,1)
    
                x[i] = r * (it1 + (r * (it2 + (r * (it3 + (r * it4))))));
                if (x[i] != 1.0) {
                    break;    
                } // if (x[i] != 1.0)
                // x[i] == 1.0
                // If a real number has n bits of precision, and the first n bits of the 48-bit integer
                // happen all to be 1 (which will occur about once every 2**n calls), then x[i] will be
                // rounded to exactly 1.0
                // Since x[i] is not supposed to return exactly 0.0 or 1.0, the statistically correct 
                // thing to do in this situation is simply to iterate again.
                // N.B. The case x[i] = 0.0 should not be possible.
                i1 = i1 + 2;
                i2 = i2 + 2;
                i3 = i3 + 2;
                i4 = i4 + 2;
            } // while(true)
        } // for (i = 0; i < Math.min(n,lv); i++)

        // Return final value of seed
        iseed[0] = it1;
        iseed[1] = it2;
        iseed[2] = it3;
        iseed[3] = it4;

        return;
    } // dlaruv
    
    /** This is a port of version 3.1 LAPACK test routine DQRT01.
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     November 2006
    *
    *     .. Scalar Arguments ..
          INTEGER            LDA, LWORK, M, N
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), Q( LDA, * ),
         $                   R( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
         $                   WORK( LWORK )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DQRT01 tests ge.dgeqrf, which computes the QR factorization of an m-by-n
    *  matrix A, and partially tests ge.dorgqr which forms the m-by-m
    *  orthogonal matrix Q.
    *
    *  DQRT01 compares R with Q'*A, and checks that Q is orthogonal.
    *
    *  Arguments
    *  =========
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix A.  M >= 0.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix A.  N >= 0.
    *
    *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
    *          The m-by-n matrix A.
    *
    *  AF      (output) DOUBLE PRECISION array, dimension (LDA,N)
    *          Details of the QR factorization of A, as returned by ge.dgeqrf.
    *          See ge.dgeqrf for further details.
    *
    *  Q       (output) DOUBLE PRECISION array, dimension (LDA,M)
    *          The m-by-m orthogonal matrix Q.
    *
    *  R       (workspace) DOUBLE PRECISION array, dimension (LDA,max(M,N))
    *
    *  LDA     (input) INTEGER
    *          The leading dimension of the arrays A, AF, Q and R.
    *          LDA >= max(M,N).
    *
    *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
    *          The scalar factors of the elementary reflectors, as returned
    *          by ge.dgeqrf.
    *
    *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
    *
    *  LWORK   (input) INTEGER
    *          The dimension of the array WORK.
    *
    *  RWORK   (workspace) DOUBLE PRECISION array, dimension (M)
    *
    *  RESULT  (output) DOUBLE PRECISION array, dimension (2)
    *          The test ratios:
    *          RESULT(1) = norm( R - Q'*A ) / ( M * norm(A) * EPS )
    *          RESULT(2) = norm( I - Q'*Q ) / ( M * EPS )
    */
    private void dqrt01(int m, int n, double[][] A, double[][] AF, double[][] Q, 
                        double[][] R, int lda, double[] tau, double[] work, int lwork,
                        double[] rwork, double[] result) {
        double rogue = -1.0E10;
        int info[] = new int[1];
        int minmn;
        double anorm;
        double eps;
        double resid;
        double array1[][];
        double array2[][];
        int row1;
        int p;
        int q;
        
        minmn = Math.min(m, n);
        eps = ge.dlamch('E'); // Epsilon
        
        // Copy the matrix A to the array AF.
        ge.dlacpy('F', m, n, A, lda, AF, lda);
        
        // Factorize the matrix A in the array AF.
        srnamt = new String("DGEQRF");
        ge.dgeqrf(m, n, AF, lda, tau, work, lwork, info);
        
        // Copy details of Q
        ge.dlaset('F', m, m, rogue, rogue, Q, lda);
        row1 = Math.max(1, m-1);
        array1 = new double[row1][n];
        array2 = new double[row1][n];
        for (p = 0; p < row1; p++) {
            for (q = 0; q < n; q++) {
                array1[p][q] = AF[1+p][q];
            }
        }
        ge.dlacpy('L', m-1, n, array1, row1, array2, row1);
        for (p = 0; p < row1; p++) {
            for (q = 0; q < n; q++) {
                Q[1+p][q] = array2[p][q];
            }
        }
        
        // Generate the m-by-m matrix Q
        srnamt = new String("DORGQR");
        ge.dorgqr(m, m, minmn, Q, lda, tau, work, lwork, info);
        
        // Copy R
        ge.dlaset('F', m, n, 0.0, 0.0, R, lda);
        ge.dlacpy('U', m, n, AF, lda, R, lda);
        
        // Compute R - Q'*A
        ge.dgemm('T', 'N', m, n, m, -1.0, Q, lda, A, lda, 1.0, R, lda);
        
        // Compute norm(R - Q'*A)/(M * norm(A) * eps).
        anorm = ge.dlange('1', m, n, A, lda, rwork);
        resid = ge.dlange('1', m, n, R, lda, rwork);
        if (anorm > 0.0) {
            result[0] = ((resid /(double)Math.max(1,m))/anorm)/eps;
        }
        else {
            result[0] = 0;
        }
        
        // Compute I - Q'*Q
        ge.dlaset('F', m, m, 0.0, 1.0, R, lda);
        dsyrk('U', 'T', m, m, -1.0, Q, lda, 1.0, R, lda);
        
        // Compute norm(I - Q'*Q)/(M * eps)
        resid = dlansy('1', 'U', m, R, lda, rwork);
        result[1] = (resid/(double)Math.max(1, m))/eps;
        return;
    } // dqrt01
    
    /**
     * dsyrk is the port of the 2/8/89 blas DSYRK routine. Original version written by: Jack Dongarra, Argonne National
     * Laboratory. Iain Duff, AERE Harwell. Jeremy Du Croz, Numerical Algorithms Group Ltd. Sven Hammarling, Numerical
     * Algorithms Group Ltd. dsyrk performs one of the symmetric rank k operations C = alpha*A*A' + beta*C, or C =
     * alpha*A'*A + beta*c where alpha and beta are scalars, C is an n by n symmetric matrix and A is an n by k matrix
     * in the frist case and a k by n matrix in the second case.
     *
     * @param  uplo   input char On entry, uplo specifies whether the upper or lower triangular part of the array C is
     *                to be referenced as follows: = 'U' or 'u' Only the upper triangular part of C is to be referenced.
     *                = 'L' or 'l' Only the lower triangular part of C is to be referenced.
     * @param  trans  input char On entry, trans specifies the operation to be performed as follows: = 'N' or 'n' C =
     *                alpha*A*A' + beta*C. = 'T' or 't' C = alpha*A'*A + beta*C. = 'C' or 'c' C = alpha*A'*A + beta*C.
     * @param  n      input int On entry, n specifies the order of the matrix C. n must be at least zero.
     * @param  k      input int On entry with trans = 'N' or 'n', k specifies the number of columns of the matrix A, and
     *                on entry with trans = 'T' or 't' or 'C' or 'c', k specifies the number of rows of the matrix A. k
     *                must be at least zero.
     * @param  alpha  input double specified scalar
     * @param  A      input double[][] array of dimension lda by ka, where ka is k when trans = 'N' or 'n', and is n
     *                otherwise. Before entry with trans = 'N' or 'n', the leading n by k part of the array A must
     *                contain the matrix A, otherwise the leading k by n part of the array A must contain the matrix A.
     * @param  lda    input int On entry, lda specifies the first dimension of A as declared in the calling (sub)
     *                program. When trans = 'N' or 'n' then lda must be at least max(1,n), otherwise lda must be at
     *                least max(1,k).
     * @param  beta   input double specified scalar
     * @param  C      input/output double[][] array of dimension ldc by n Before entry with uplo = 'U' or 'u', the
     *                leading n by n upper triangular part of the array C must contain the upper triangular part of the
     *                symmetric matrix and the strictly lower triangular part of C is not referenced. On exit, the upper
     *                triangular part of the array C, is overwritten by the upper triangular part of the updated matrix.
     *                Before entry with uplo = 'L' or 'l', the leading n by n lower triangular part of the array C must
     *                contain the lower triangular part of the symmetric matrix and the strictly upper triangular part
     *                of C is not referenced. On exit, the lower triangular part of the array C is overwritten by the
     *                lower triangular part of the updated matrix.
     * @param  ldc    input int On entry, ldc specifies the first dimension of C as declared in the calling (sub)
     *                program. ldc must be at least max(1,n).
     */
    private void dsyrk(char uplo, char trans, int n, int k, double alpha, double[][] A, int lda, double beta,
                       double[][] C, int ldc) {
        int nrowa;
        boolean upper;
        int info;
        int i;
        int j;
        int L;
        double temp;

        // Test the input parameters
        if ((trans == 'N') || (trans == 'n')) {
            nrowa = n;
        } else {
            nrowa = k;
        }

        if ((uplo == 'U') || (uplo == 'u')) {
            upper = true;
        } else {
            upper = false;
        }

        info = 0;

        if ((!upper) && (uplo != 'L') && (uplo != 'l')) {
            info = 1;
        } else if ((trans != 'N') && (trans != 'n') && (trans != 'T') && (trans != 't') && (trans != 'C') &&
                       (trans != 'c')) {
            info = 2;
        } else if (n < 0) {
            info = 3;
        } else if (k < 0) {
            info = 4;
        } else if (lda < Math.max(1, nrowa)) {
            info = 7;
        } else if (ldc < Math.max(1, n)) {
            info = 10;
        }

        if (info != 0) {
            MipavUtil.displayError("Error in dsyrk with info = " + info);

            return;
        } // if (info != 0)

        // Quick return if possible
        if ((n == 0) || (((alpha == 0.0) || (k == 0)) && (beta == 1.0))) {
            return;
        }

        if (alpha == 0.0) {

            if (upper) {

                if (beta == 0.0) {

                    for (j = 0; j < n; j++) {

                        for (i = 0; i <= j; i++) {
                            C[i][j] = 0.0;
                        }
                    }
                } // if (beta == 0.0)
                else { // beta != 0.0

                    for (j = 0; j < n; j++) {

                        for (i = 0; i <= j; i++) {
                            C[i][j] = beta * C[i][j];
                        }
                    }
                } // else beta != 0.0
            } // if (upper)
            else { // lower

                if (beta == 0.0) {

                    for (j = 0; j < n; j++) {

                        for (i = j; i < n; i++) {
                            C[i][j] = 0.0;
                        }
                    }
                } // if (beta == 0.0)
                else { // beta != 0.0

                    for (j = 0; j < n; j++) {

                        for (i = j; i < n; i++) {
                            C[i][j] = beta * C[i][j];
                        }
                    }
                } // else beta != 0.0
            } // else lower
        } // if (alpha == 0.0)

        if ((trans == 'N') || (trans == 'n')) {

            // Form C = alpha*A*A' + beta*C
            if (upper) {

                for (j = 0; j < n; j++) {

                    if (beta == 0.0) {

                        for (i = 0; i <= j; i++) {
                            C[i][j] = 0.0;
                        }
                    } // if (beta == 0.0)
                    else if (beta != 1.0) {

                        for (i = 0; i <= j; i++) {
                            C[i][j] = beta * C[i][j];
                        }
                    } // else if (beta != 1.0)

                    for (L = 0; L < k; L++) {

                        if (A[j][L] != 0.0) {
                            temp = alpha * A[j][L];

                            for (i = 0; i <= j; i++) {
                                C[i][j] = C[i][j] + (temp * A[i][L]);
                            }
                        } // if (A[j][L] != 0.0)
                    } // for (L = 0; L < k; L++)
                } // for (j = 0; j < n; j++)
            } // if (upper)
            else { // lower

                for (j = 0; j < n; j++) {

                    if (beta == 0.0) {

                        for (i = j; i < n; i++) {
                            C[i][j] = 0.0;
                        }
                    } // if (beta == 0.0)
                    else if (beta != 1.0) {

                        for (i = j; i < n; i++) {
                            C[i][j] = beta * C[i][j];
                        }
                    } // else if (beta != 1.0)

                    for (L = 0; L < k; L++) {

                        if (A[j][L] != 0.0) {
                            temp = alpha * A[j][L];

                            for (i = j; i < n; i++) {
                                C[i][j] = C[i][j] + (temp * A[i][L]);
                            }
                        } // if (A[j][L] != 0.0)
                    } // for (L = 0; L < k; L++)
                } // for (j = 0; j < n; j++)
            } // else lower
        } // if ((trans == 'N') || (trans == 'n'))
        else { // trans != 'N' && trans != 'n'

            // Form C = alpha*A'*A + beta*C.
            if (upper) {

                for (j = 0; j < n; j++) {

                    for (i = 0; i <= j; i++) {
                        temp = 0.0;

                        for (L = 0; L < k; L++) {
                            temp = temp + (A[L][i] * A[L][j]);
                        }

                        if (beta == 0.0) {
                            C[i][j] = alpha * temp;
                        } else {
                            C[i][j] = (alpha * temp) + (beta * C[i][j]);
                        }
                    } // for (i = 0; i <= j; i++)
                } // for (j = 0; j < n; j++)
            } // if (upper)
            else { // lower

                for (j = 0; j < n; j++) {

                    for (i = j; i < n; i++) {
                        temp = 0.0;

                        for (L = 0; L < k; L++) {
                            temp = temp + (A[L][i] * A[L][j]);
                        }

                        if (beta == 0.0) {
                            C[i][j] = alpha * temp;
                        } else {
                            C[i][j] = (alpha * temp) + (beta * C[i][j]);
                        }
                    } // for (i = j; i < n; i++)
                } // for (j = 0; j < n; j++)
            } // else lower
        } // else trans != 'N' && trans != 'n'

        return;
    } // dsyrk
    
    /**
     * This is a port of the version 3.2 LAPACK auxiliary routine DLANSY. Original DLANSY created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlansy returns the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of
     * largest absolute value of a real symmetric matrix A.
     *
     * @param   norm  input char Specifies the value to be returned by dlansy as follows: 
     *                = 'M' or 'm', max(abs(A[i][j])); this is not a matrix norm. 
     *                = '1', 'O', or 'o', norm1(A), where norm1 denotes the one norm of a matrix (maximum column sum) 
     *                = 'I' or 'i', normI(A), where normI denotes the infinity norm of a matrix (maximum row sum) 
     *                = 'F', 'f', 'E', or 'e', normF(A) , where normF denotes the Frobenius norm of a matrix
     *                  (square root of sum of squares)
     * @param   uplo  input char Specifies whether the upper or lower triangular part of the symmetric matrix A is to be
     *                referenced.
     *                = 'U': Upper triangular part of A is referenced 
     *                = 'L': Lower triangular part of A is referenced
     * @param   n     input int The order of the matrix A. n >= 0. When n = 0, the answer returned by dlansy is zero.
     * @param   A     input double[][] of dimension lda by n. The symmetric matrix A. If uplo = 'U', the leading n by n
     *                upper triangular part of A contains the upper triangular part of the matrix A, and the strictly
     *                lower triangular part of A is not referenced. If uplo = 'L', the leading n by n lower triangular
     *                part of A contains the lower triangular part of the matrix A, and the strictly upper triangular
     *                part of A is not referenced.
     * @param   lda   input int The leading dimension of the array A. lda >= max(n,1).
     * @param   work  workspace double[] of dimension max(1, lwork), where lwork >= n when used with one norm or infinity norm;
     *                otherwise work is not referenced.
     *
     * @return  double
     */
    private double dlansy(char norm, char uplo, int n, double[][] A, int lda, double[] work) {
        int i;
        int j;
        double absa;
        double[] scale = new double[1];
        double[] sum = new double[1];
        double value = 0.0;
        double[] vector1;

        if (n == 0) {
            value = 0.0;
        } else if ((norm == 'M') || (norm == 'm')) {

            // Find max(abs(A[i][j]))
            value = 0.0;

            if ((uplo == 'U') || (uplo == 'u')) {

                for (j = 0; j < n; j++) {

                    for (i = 0; i <= j; i++) {
                        value = Math.max(value, Math.abs(A[i][j]));
                    }
                }
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { // ((uplo == 'L') || (uplo == 'l'))

                for (j = 0; j < n; j++) {

                    for (i = j; i < n; i++) {
                        value = Math.max(value, Math.abs(A[i][j]));
                    }
                }
            } // else ((uplo == 'L') || (uplo == 'l'))
        } // else if ((norm == 'M') || (norm == 'm'))
        else if ((norm == 'I') || (norm == 'i') || (norm == 'O') || (norm == 'o') || (norm == '1')) {

            // Find normI(A) (== norm1(A), since A is symmetric).
            value = 0.0;

            if ((uplo == 'U') || (uplo == 'u')) {

                for (j = 0; j < n; j++) {
                    sum[0] = 0.0;

                    for (i = 0; i <= (j - 1); i++) {
                        absa = Math.abs(A[i][j]);
                        sum[0] = sum[0] + absa;
                        work[i] = work[i] + absa;
                    } // for (i = 0; i <= j-1; i++)

                    work[j] = sum[0] + Math.abs(A[j][j]);
                } // for (j = 0; j < n; j++)

                for (i = 0; i < n; i++) {
                    value = Math.max(value, work[i]);
                } // for (i = 0; i < n; i++)
            } // if (uplo == 'U') || (uplo == 'u'))
            else { // ((uplo == 'L') || (uplo == 'l'))

                for (i = 0; i < n; i++) {
                    work[i] = 0.0;
                } // for (i = 0; i < n; i++)

                for (j = 0; j < n; j++) {
                    sum[0] = work[j] + Math.abs(A[j][j]);

                    for (i = j + 1; i < n; i++) {
                        absa = Math.abs(A[i][j]);
                        sum[0] = sum[0] + absa;
                        work[i] = work[i] + absa;
                    } // for (i = j+1; i < n; i++)

                    value = Math.max(value, sum[0]);
                } // for (j = 0; j < n; j++)
            } // else ((uplo == 'L') || (uplo == 'l'))
        } // else if ((norm == 'I') || (norm == 'i') || (norm == 'O') || (norm == 'o')
        else if ((norm == 'F') || (norm == 'f') || (norm == 'E') || (norm == 'e')) {
            // Find normF(A)

            scale[0] = 0.0;
            sum[0] = 1.0;

            if ((uplo == 'U') || (uplo == 'u')) {

                for (j = 2; j <= n; j++) {
                    vector1 = new double[j - 1];

                    for (i = 0; i < (j - 1); i++) {
                        vector1[i] = A[i][j - 1];
                    }

                    ge.dlassq(j - 1, vector1, 1, scale, sum);
                } // for (j = 2; j <= n; j++)
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { // ((uplo == 'L') || (uplo == 'l'))

                for (j = 1; j <= (n - 1); j++) {
                    vector1 = new double[n - j];

                    for (i = 0; i < (n - j); i++) {
                        vector1[i] = A[j + i][j - 1];
                    }

                    ge.dlassq(n - j, vector1, 1, scale, sum);
                } // for (j = 1; j <= n-1; j++)
            } // else ((uplo == 'L') || (uplo == 'l'))

            sum[0] = 2 * sum[0];
            vector1 = new double[n];

            for (i = 0; i < n; i++) {
                vector1[i] = A[i][i];
            }

            ge.dlassq(n, vector1, 1, scale, sum);
            value = scale[0] * Math.sqrt(sum[0]);
        } // else if ((norm == 'F') || (norm == 'f') || (norm == 'E') || (norm == 'e'))

        return value;
    } // dlansy
    
    /** This is a port of version 3.1 LAPACK test routine DGENND.
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     February 2008
    *
    *     .. Scalar Arguments ..
          INTEGER M, N, LDA
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION A( LDA, * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *     DGENND tests that its argument has a non-negative diagonal.
    *
    *  Arguments
    *  =========
    *
    *  M       (input) INTEGER
    *          The number of rows in A.
    *
    *  N       (input) INTEGER
    *          The number of columns in A.
    *
    *  A       (input) DOUBLE PRECISION array, dimension (LDA, N)
    *          The matrix.
    *
    *  LDA     (input) INTEGER
    *          Leading dimension of A.
    */
    private boolean dgennd(int m, int n, double A[][], int lda) {
        int i;
        int k;
        
        k = Math.min(m, n);
        for (i = 0; i < k; i++) {
            if (A[i][i] < 0.0) {
                return false;
            }
        }
        return true;
    } // dgennd;
    
   
    
    
    
    
    
    
    
    
    
    /** This is a port of version 3.1 LAPACK test routine DQRT02.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             INTEGER            K, LDA, LWORK, M, N
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), Q( LDA, * ),
            $                   R( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
            $                   WORK( LWORK )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DQRT02 tests ge.dorgqr, which generates an m-by-n matrix Q with
       *  orthonornmal columns that is defined as the product of k elementary
       *  reflectors.
       *
       *  Given the QR factorization of an m-by-n matrix A, DQRT02 generates
       *  the orthogonal matrix Q defined by the factorization of the first k
       *  columns of A; it compares R(1:n,1:k) with Q(1:m,1:n)'*A(1:m,1:k),
       *  and checks that the columns of Q are orthonormal.
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix Q to be generated.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix Q to be generated.
       *          M >= N >= 0.
       *
       *  K       (input) INTEGER
       *          The number of elementary reflectors whose product defines the
       *          matrix Q. N >= K >= 0.
       *
       *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          The m-by-n matrix A which was factorized by DQRT01.
       *
       *  AF      (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          Details of the QR factorization of A, as returned by ge.dgeqrf.
       *          See ge.dgeqrf for further details.
       *
       *  Q       (workspace) DOUBLE PRECISION array, dimension (LDA,N)
       *
       *  R       (workspace) DOUBLE PRECISION array, dimension (LDA,N)
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the arrays A, AF, Q and R. LDA >= M.
       *
       *  TAU     (input) DOUBLE PRECISION array, dimension (N)
       *          The scalar factors of the elementary reflectors corresponding
       *          to the QR factorization in AF.
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
       *
       *  LWORK   (input) INTEGER
       *          The dimension of the array WORK.
       *
       *  RWORK   (workspace) DOUBLE PRECISION array, dimension (M)
       *
       *  RESULT  (output) DOUBLE PRECISION array, dimension (2)
       *          The test ratios:
       *          RESULT(1) = norm( R - Q'*A ) / ( M * norm(A) * EPS )
       *          RESULT(2) = norm( I - Q'*Q ) / ( M * EPS )
       */
    private void dqrt02(int m, int n, int k, double[][] A, double[][] AF, double [][] Q,
                        double[][] R, int lda, double[] tau, double[] work, int lwork,
                        double[] rwork, double[] result) {
        double rogue = -1.0E10;
        int info[] = new int[1];
        double anorm;
        double eps;
        double resid;
        int row1;
        int p;
        int q;
        double array1[][];
        double array2[][];
        
        eps = ge.dlamch('E'); // Epsilon
        
        // Copy the first k columns of the factorization to the array Q
        ge.dlaset('F', m, n, rogue, rogue, Q, lda);
        row1 = Math.max(1, m-1);
        array1 = new double[row1][k];
        for (p = 0; p < row1; p++) {
            for (q = 0; q < k; q++) {
                array1[p][q] = AF[p+1][q];
            }
        }
        array2 = new double[row1][k];
        ge.dlacpy('L', m-1, k, array1, row1, array2, row1);
        for (p = 0; p < row1; p++) {
            for (q = 0; q < k; q++) {
                Q[p+1][q] = array2[p][q];
            }
        }
        
        // Generate the first n columns of the matrix Q
        srnamt = new String("DORGQR");
        ge.dorgqr(m, n, k, Q, lda, tau, work, lwork, info);
        
        // Copy R(1:n,1:k)
        ge.dlaset('F', n, k, 0.0, 0.0, R, lda);
        ge.dlacpy('U', n, k, AF, lda, R, lda);
        
        // Compute R(1:n,1:k) - Q(1:m,1:n)' * A(1:m,1:k)
        ge.dgemm('T', 'N', n, k, m, -1.0, Q, lda, A, lda, 1.0, R, lda);
        
        // Compute norm(R - Q'*A)/(m * norm(A) * eps).
        anorm = ge.dlange('1', m, k, A, lda, rwork);
        resid = ge.dlange('1', n, k, R, lda, rwork);
        if (anorm > 0) {
            result[0] = ((resid/(double)Math.max(1, m))/anorm)/eps;
        }
        else {
            result[0] = 0;
        }
        
        // Compute I - Q'*Q
        ge.dlaset('F', n, n, 0.0, 1.0, R, lda);
        dsyrk('U', 'T', n, m, -1.0, Q, lda, 1.0, R, lda);
        
        // Compute norm(I - Q'*Q)/(m * eps).
        resid = dlansy('1', 'U', n, R, lda, rwork);
        result[1] = (resid/(double)Math.max(1,m))/eps;
        return;
    } // dqrt02
    
    /** This is a port of version 3.1 LAPACK test routine DQRT03.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             INTEGER            K, LDA, LWORK, M, N
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   AF( LDA, * ), C( LDA, * ), CC( LDA, * ),
            $                   Q( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
            $                   WORK( LWORK )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DQRT03 tests ge.dormqr, which computes Q*C, Q'*C, C*Q or C*Q'.
       *
       *  DQRT03 compares the results of a call to ge.dormqr with the results of
       *  forming Q explicitly by a call to ge.dorgqr and then performing matrix
       *  multiplication by a call to DGEMM.
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The order of the orthogonal matrix Q.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of rows or columns of the matrix C; C is m-by-n if
       *          Q is applied from the left, or n-by-m if Q is applied from
       *          the right.  N >= 0.
       *
       *  K       (input) INTEGER
       *          The number of elementary reflectors whose product defines the
       *          orthogonal matrix Q.  M >= K >= 0.
       *
       *  AF      (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          Details of the QR factorization of an m-by-n matrix, as
       *          returnedby ge.dgeqrf. See SGEQRF for further details.
       *
       *  C       (workspace) DOUBLE PRECISION array, dimension (LDA,N)
       *
       *  CC      (workspace) DOUBLE PRECISION array, dimension (LDA,N)
       *
       *  Q       (workspace) DOUBLE PRECISION array, dimension (LDA,M)
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the arrays AF, C, CC, and Q.
       *
       *  TAU     (input) DOUBLE PRECISION array, dimension (min(M,N))
       *          The scalar factors of the elementary reflectors corresponding
       *          to the QR factorization in AF.
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
       *
       *  LWORK   (input) INTEGER
       *          The length of WORK.  LWORK must be at least M, and should be
       *          M*NB, where NB is the blocksize for this environment.
       *
       *  RWORK   (workspace) DOUBLE PRECISION array, dimension (M)
       *
       *  RESULT  (output) DOUBLE PRECISION array, dimension (4)
       *          The test ratios compare two techniques for multiplying a
       *          random matrix C by an m-by-m orthogonal matrix Q.
       *          RESULT(1) = norm( Q*C - Q*C )  / ( M * norm(C) * EPS )
       *          RESULT(2) = norm( C*Q - C*Q )  / ( M * norm(C) * EPS )
       *          RESULT(3) = norm( Q'*C - Q'*C )/ ( M * norm(C) * EPS )
       *          RESULT(4) = norm( C*Q' - C*Q' )/ ( M * norm(C) * EPS )
       */
    private void dqrt03(int m, int n, int k, double[][] AF, double[][] C, double[][] CC,
                        double[][] Q, int lda, double[] tau, double[] work, int lwork,
                        double[] rwork, double[] result) {
        double rogue = -1.0E10;
        char side;
        char trans;
        int info[] = new int[1];
        int iside;
        int itrans;
        int j;
        int mc;
        int nc;
        double cnorm;
        double eps;
        double resid;
        int iseed[] = new int[]{1988, 1989, 1990, 1991};
        int row1;
        int p;
        int q;
        double array1[][];
        double array2[][];
        double vec1[];
        
        eps = ge.dlamch('E'); // epsilon
        
        // Copy the first k columns of the factorization to the array Q
        ge.dlaset('F', m, m, rogue, rogue, Q, lda);
        row1 = Math.max(1, m-1);
        array1 = new double[row1][k];
        for (p = 0; p < row1; p++) {
            for (q = 0; q < k; q++) {
                array1[p][q] = AF[p+1][q];
            }
        }
        array2 = new double[row1][k];
        ge.dlacpy('L', m-1, k, array1, row1, array2, row1);
        for (p = 0; p < row1; p++) {
            for (q = 0; q < k; q++) {
                Q[p+1][q] = array2[p][q];
            }
        }
        
        // Generate the m by m matrix Q
        srnamt = new String("DORGQR");
        ge.dorgqr(m, m, k, Q, lda, tau, work, lwork, info);
        
        for (iside = 1; iside <= 2; iside++) {
            if (iside == 1) {
                side = 'L';
                mc = m;
                nc = n;
            }
            else {
                side = 'R';
                mc = n;
                nc = m;
            }
            
            // Generate mc by nc matrix C
            vec1 = new double[mc];
            for (j = 1; j <= nc; j++) {
                dlarnv(2, iseed, mc, vec1);
                for (p = 0; p < mc; p++) {
                    C[p][j-1] = vec1[p];
                }
            } // for (j = 1; j <= nc; j++)
            cnorm = ge.dlange('1', mc, nc, C, lda, rwork);
            if (cnorm == 0.0) {
                cnorm = 1.0;
            }
            
            for (itrans = 1; itrans <= 2; itrans++) {
                if (itrans == 1) {
                    trans = 'N';
                }
                else {
                    trans = 'T';
                }
                
                // Copy C
                ge.dlacpy('F', mc, nc, C, lda, CC, lda);
                
                // Apply Q or Q' to C
                srnamt = new String("dormqr");
                ge.dormqr(side, trans, mc, nc, k, AF, lda, tau, CC, lda, work, lwork,
                       info);
                
                // Form explicit product and subtract
                if ((side == 'L') || (side == 'l')) {
                    ge.dgemm(trans, 'N', mc, nc, mc, -1.0, Q, lda, C, lda, 1.0, CC, lda);
                }
                else {
                    ge.dgemm('N', trans, mc, nc, nc, -1.0, C, lda, Q, lda, 1.0, CC, lda);
                }
                
                // Compute error in the difference
                resid = ge.dlange('1', mc, nc, CC, lda, rwork);
                result[(iside-1)*2+itrans-1] = resid/(Math.max(1,m)*cnorm*eps);
            } // for (itrans = 1; itrans <= 2; itrans++)
        } // for (iside = 1; iside <= 2; iside++)
        return;
    } // dqrt03

    /** This is a port of version 3.1 LAPACK test routine DGET02.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             CHARACTER          TRANS
             INTEGER            LDA, LDB, LDX, M, N, NRHS
             DOUBLE PRECISION   RESID
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), RWORK( * ),
            $                   X( LDX, * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DGET02 computes the residual for a solution of a system of linear
       *  equations  A*x = b  or  A'*x = b:
       *     RESID = norm(B - A*X) / ( norm(A) * norm(X) * EPS ),
       *  where EPS is the machine epsilon.
       *
       *  Arguments
       *  =========
       *
       *  TRANS   (input) CHARACTER*1
       *          Specifies the form of the system of equations:
       *          = 'N':  A *x = b
       *          = 'T':  A'*x = b, where A' is the transpose of A
       *          = 'C':  A'*x = b, where A' is the transpose of A
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix A.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix A.  N >= 0.
       *
       *  NRHS    (input) INTEGER
       *          The number of columns of B, the matrix of right hand sides.
       *          NRHS >= 0.
       *
       *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          The original M x N matrix A.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.  LDA >= max(1,M).
       *
       *  X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)
       *          The computed solution vectors for the system of linear
       *          equations.
       *
       *  LDX     (input) INTEGER
       *          The leading dimension of the array X.  If TRANS = 'N',
       *          LDX >= max(1,N); if TRANS = 'T' or 'C', LDX >= max(1,M).
       *
       *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
       *          On entry, the right hand side vectors for the system of
       *          linear equations.
       *          On exit, B is overwritten with the difference B - A*X.
       *
       *  LDB     (input) INTEGER
       *          The leading dimension of the array B.  IF TRANS = 'N',
       *          LDB >= max(1,M); if TRANS = 'T' or 'C', LDB >= max(1,N).
       *
       *  RWORK   (workspace) DOUBLE PRECISION array, dimension (M)
       *
       *  RESID   (output) DOUBLE PRECISION
       *          The maximum over the number of right hand sides of
       *          norm(B - A*X) / ( norm(A) * norm(X) * EPS ).
       */
    private void dget02(char trans, int m, int n, int nrhs, double[][] A, int lda,
                        double[][] X, int ldx, double[][] B, int ldb, double[] rwork,
                        double[] resid) {
        int j;
        int n1;
        int n2;
        double anorm;
        double bnorm;
        double eps;
        double xnorm;
        int p;
        
        // Quick exit if m == 0 or n == 0 or nrhs == 0
        if ((m <= 0) || (n <= 0) || (nrhs <= 0)) {
            resid[0] = 0.0;
            return;
        }
        
        if ((trans == 'T') || (trans == 't') || (trans == 'C') || (trans == 'c')) {
            n1 = n;
            n2 = m;
        }
        else {
            n1 = m;
            n2 = n;
        }
        
        // Exit with resid[0] = 1/eps if anorm = 0.
        eps = ge.dlamch('E'); // Epsilon
        anorm = ge.dlange('1', n1, n2, A, lda, rwork);
        if (anorm <= 0.0) {
            resid[0] = 1.0/eps;
            return;
        }
        
        // Compute B - A*X  (or B - A'*X) and store in B.
        ge.dgemm(trans, 'N', n1, nrhs, n2, -1.0, A, lda, X, ldx, 1.0, B, ldb);
        
        // Compute the maximum overr the number of right hand sides of
        // norm(B - A*X)/(norm(A) * norm(X) * eps).
        resid[0] = 0.0;
        for (j = 1; j <= nrhs; j++) {
            bnorm = 0.0;
            for (p = 0; p < n1; p++) {
                bnorm += Math.abs(B[p][j-1]);
            }
            xnorm = 0.0;
            for (p = 0; p < n2; p++) {
                xnorm += Math.abs(X[p][j-1]);
            }
            if (xnorm <= 0.0) {
                resid[0] = 1.0/eps;
            }
            else {
                resid[0] = Math.max(resid[0], ((bnorm/anorm)/xnorm)/eps);
            }
        } // for (j = 1; j <= nrhs; j++)
        return;
    } // dget02
    
    /** This dchklq_test routine is a port of a portion of the version 3.1.1 LAPACK test routine DCHKAA by
     * Univ. of Tennessee, Univ. Of California Berkeley and NAG Ltd., January, 2007. and some values from 
     * the test data file dtest.in.
     */
    public void dchklq_test() {
        
        // Number of values of m
        int nm = 7;
        
        // Values of m (row dimension)
        // dtest.in uses 50 rather than 16
        int[] mval = new int[] { 0, 1, 2, 3, 5, 10, 16 };
        
        // Number of values of n
        int nn = 7;
        
        // Values of n (column dimension)
        // dtest.in uses 50 rather than 16
        int[] nval = new int[] { 0, 1, 2, 3, 5, 10, 16 };
        
        // Number of values of nrhs
        // dchkaa has nns = 1.  dtest.in uses nns = 3.
        int nns = 1;
        
        // Values of nrhs (number of right hand sides)
        // dchkaa uses only 2.  dtest.in uses 1, 2, 15.
        // Since dchklq only accepts nrhs = nsval[0] use only 2.
        int[] nsval = new int[]{2};
        
        // Number of values of nb
        int nnb = 5;
        
        // Values of nb (the blocksize)
        int nbval[] = new int[] {1, 3, 3, 3, 20};
        
        // Values of nx (crossover point)
        // There are nnb values of nx.
        int nxval[] = new int[] {1, 0, 5, 9, 1};
        
        // Number of values of rank
        int nrank = 3;
        
        // Values of rank (as a % of n)
        int rankval[] = new int[] {30, 50, 90};
        
        // Threshold value of test ratio
        // dchkaa has 20.0, dtest.in has 30.0
        double thresh = 20.0;
        
        // Test the LAPACK routines
        boolean tstchk = true;
        
        // Test the driver routines
        boolean tstdrv = true;
        
        // Test the error exits
        // Passed all 49 exits on test.
        // Put at false so as not to have to hit okay to 49 displayError messages.
        boolean tsterr = false;
        
        // The maximum allowable value for n
        int nmax = 132;
        
        // The number of different values that can be used for each of m, n, nrhs, nb, and nx
        int maxin = 12;
        
        // The maximum number of right hand sides
        int maxrhs = 16;
        int nmats = 8;
        int ntypes = 8;
        int nrhs = nsval[0];
        boolean dotype[] = new boolean[ntypes];
        double A[][] = new double[nmax][nmax];
        double AF[][] = new double[nmax][nmax];
        double AQ[][] = new double[nmax][nmax];
        double AL[][] = new double[nmax][nmax];
        double AC[][] = new double[nmax][nmax];
        double B[][] = new double[nmax][nrhs];
        double X[][] = new double[nmax][nrhs];
        double XACT[][] = new double[nmax][nrhs];
        double tau[] = new double[nmax];
        double work[] = new double[nmax*nmax];
        double rwork[] = new double[nmax];
        int iwork[] = new int[nmax];
        iparms = new int[11];
        int i;
        double eps;
       
        for (i = 0; i < ntypes; i++) {
            dotype[i] = true;
        }
        
        // Output the machine dependent constants
        eps = ge.dlamch('U');
        Preferences.debug("Underflow threshold = " + eps + "\n");
        eps = ge.dlamch('O');
        Preferences.debug("Overflow threshold = " + eps + "\n");
        eps = ge.dlamch('E');
        Preferences.debug("Precision = " + eps + "\n");
        
        dchklq(dotype, nm, mval, nn, nval, nnb, nbval, nxval, nrhs, thresh, tsterr, nmax, 
               A, AF, AQ, AL, AC, B, X, XACT, tau, work, rwork, iwork);
    } // dchklq_test
    
    /** This is a port of version 3.1 LAPACK test routine DCHKLQ.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             LOGICAL            TSTERR
             INTEGER            NM, NMAX, NN, NNB, NOUT, NRHS
             DOUBLE PRECISION   THRESH
       *     ..
       *     .. Array Arguments ..
             LOGICAL            DOTYPE( * )
             INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NVAL( * ),
            $                   NXVAL( * )
             DOUBLE PRECISION   A( * ), AC( * ), AF( * ), AL( * ), AQ( * ),
            $                   B( * ), RWORK( * ), TAU( * ), WORK( * ),
            $                   X( * ), XACT( * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DCHKLQ tests DGELQF, DORGLQ and DORMLQ.
       *
       *  Arguments
       *  =========
       *
       *  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
       *          The matrix types to be used for testing.  Matrices of type j
       *          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
       *          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
       *
       *  NM      (input) INTEGER
       *          The number of values of M contained in the vector MVAL.
       *
       *  MVAL    (input) INTEGER array, dimension (NM)
       *          The values of the matrix row dimension M.
       *
       *  NN      (input) INTEGER
       *          The number of values of N contained in the vector NVAL.
       *
       *  NVAL    (input) INTEGER array, dimension (NN)
       *          The values of the matrix column dimension N.
       *
       *  NNB     (input) INTEGER
       *          The number of values of NB and NX contained in the
       *          vectors NBVAL and NXVAL.  The blocking parameters are used
       *          in pairs (NB,NX).
       *
       *  NBVAL   (input) INTEGER array, dimension (NNB)
       *          The values of the blocksize NB.
       *
       *  NXVAL   (input) INTEGER array, dimension (NNB)
       *          The values of the crossover point NX.
       *
       *  NRHS    (input) INTEGER
       *          The number of right hand side vectors to be generated for
       *          each linear system.
       *
       *  THRESH  (input) DOUBLE PRECISION
       *          The threshold value for the test ratios.  A result is
       *          included in the output file if RESULT >= THRESH.  To have
       *          every test ratio printed, use THRESH = 0.
       *
       *  TSTERR  (input) LOGICAL
       *          Flag that indicates whether error exits are to be tested.
       *
       *  NMAX    (input) INTEGER
       *          The maximum value permitted for M or N, used in dimensioning
       *          the work arrays.
       *
       *  A       (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX]
       *
       *  AF      (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX]
       *
       *  AQ      (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX]
       *
       *  AL      (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX]
       *
       *  AC      (workspace) DOUBLE PRECISION array, dimension [NMAX][NMAX]
       *
       *  B       (workspace) DOUBLE PRECISION array, dimension [NMAX][NRHS]
       *
       *  X       (workspace) DOUBLE PRECISION array, dimension [NMAX[NRHS]
       *
       *  XACT    (workspace) DOUBLE PRECISION array, dimension [NMAX][NRHS]
       *
       *  TAU     (workspace) DOUBLE PRECISION array, dimension (NMAX)
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
       *
       *  RWORK   (workspace) DOUBLE PRECISION array, dimension (NMAX)
       *
       *  IWORK   (workspace) INTEGER array, dimension (NMAX)
       *
       *  NOUT    (input) INTEGER
       *          The unit number for output.
       */
    private void dchklq(boolean[] dotype, int nm, int[] mval, int nn, int[] nval, int nnb, int[] nbval,
            int[] nxval, int nrhs, double thresh, boolean tsterr, int nmax, double[][] A,
            double[][] AF, double[][] AQ, double[][] AL, double[][] AC, double[][] B, double[][] X,
            double[][] XACT, double[] tau, double[] work, double[] rwork, int[] iwork) {
        int ntests = 8;
        int ntypes = 8;
        int iseedy[] = new int[] {1988, 1989, 1990, 1991};
        int i;
        int ik;
        int im;
        int imat;
        int in;
        int inb;
        int info[] = new int[1];
        int k;
        int kl[] = new int[1];
        int ku[] = new int[1];
        int lda;
        int lwork;
        int m;
        int minmn;
        int mode[] = new int[1];
        int n;
        int nb;
        int nerrs;
        int nfail;
        int nk;
        int nrun;
        int nt;
        int nx;
        double anorm[] = new double[1];
        double cndnum[] = new double[1];
        int iseed[] = new int[4];
        int kval[] = new int[4];
        double result[] = new double[ntests];
        String path;
        char type[] = new char[1];
        char dist[] = new char[1];
        double res[] = new double[4];
        int p;
        int q;
        double vec1[];
        double resid[] = new double[1];
        
        // Initialize constants and the random number seed
        path = new String("DLQ"); // D for double precision
        nrun  = 0;
        nfail = 0;
        nerrs = 0;
        for (i = 0; i < 4; i++) {
            iseed[i] = iseedy[i];
        }
        
        // Test the error exits
        if (tsterr) {
            derrlq();
        }
        infot = 0;
        xlaenv(2, 2);
        
        lda = nmax;
        lwork = nmax * Math.max(nmax, nrhs);
        
        // Do for each value of m in mval.
        
        for (im = 1; im <= nm; im++) {
            m = mval[im-1];
            
            // Do for each value of n in nval.
            for (in = 1; in <= nn; in++) {
                n = nval[in-1];
                minmn = Math.min(m, n);
                for (imat = 1; imat <= ntypes; imat++) {
                    // Do the tests only if dotype[imat-1] is true
                    if (!dotype[imat-1]) {
                        continue;
                    }
                    
                    // Set up the parameters with dlatb4 and generate a test matrix
                    // with dlatms.
                    dlatb4(path, imat, m, n, type, kl, ku, anorm, mode, cndnum, dist);
                    
                    srnamt = new String("DLATMS");
                    dlatms(m, n, dist[0], iseed, type[0], rwork, mode[0], cndnum[0], anorm[0],
                           kl[0], ku[0], 'N', A, lda, work, info);
                    
                    // Check error code from dlamts.
                    if (info[0] != 0) {
                        if ((nfail == 0) && (nerrs == 0)) {
                            Preferences.debug("Path = DLQ\n");
                            Preferences.debug("LQ decomposition of rectangular matrices\n");
                            Preferences.debug("LQ matrix types:\n");
                            Preferences.debug("1. Diagonal\n");
                            Preferences.debug("2. Upper triangular\n");
                            Preferences.debug("3. Lower triangular\n");
                            Preferences.debug("4. Random, cndnum[0] = 2\n");
                            Preferences.debug("5. Random, cndnum[0] = sqrt(1.0/eps)\n");
                            Preferences.debug("6. Random, cndnum[0] = 0.1/eps\n");
                            Preferences.debug("7. Scaled near underflow\n");
                            Preferences.debug("8. Scaled near overflow\n");
                            Preferences.debug("Test ratios:\n");
                            Preferences.debug("1: norm(L - A*Q')/(N * norm(A) * eps)\n");
                            Preferences.debug("2: norm(I - Q*Q')/(N * eps)\n");
                            Preferences.debug("3: norm(Q*C - Q*C)/(N * norm(C) * eps)\n");
                            Preferences.debug("4: norm(C*Q - C*Q)/(N * norm(C) * eps)\n");
                            Preferences.debug("5: norm(Q' * C - Q' * C)/(N * norm(C) * eps)\n");
                            Preferences.debug("6: norm(C*Q' - C*Q')/(N * norm(C) * eps)\n");
                            Preferences.debug("7: norm(B - A * X)/(norm(A) * norm(X) * eps)\n");
                        } // if ((nfail == 0) && (nerrs == 0))
                        nerrs++;
                        Preferences.debug("Error code from dlatms is info[0] = " + info +
                                          " for m = " + m + " n = " + n + " type = " + imat + "\n");
                        continue;
                    } // if (info[0] != 0)
                    
                    // Get some values for k: the first value must be minmn,
                    // corresponding to the call of dlqt01; other values are
                    // used in the calls of dlqt02, and must not exceed minmn.
                    kval[0] = minmn;
                    kval[1] = 0;
                    kval[2] = 1;
                    kval[3] = minmn/2;
                    if (minmn == 0) {
                        nk = 1;
                    }
                    else if (minmn == 1) {
                        nk = 2;
                    }
                    else if (minmn <= 3) {
                        nk = 3;
                    }
                    else {
                        nk = 4;
                    }
                    
                    for (ik = 1; ik <= nk; ik++) {
                        k = kval[ik-1];
                        
                        // Do for each pair of values (nb, nx) in nbval and nxval.
                        for (inb = 1; inb <= nnb; inb++) {
                            nb = nbval[inb-1];
                            xlaenv(1, nb);
                            nx = nxval[inb-1];
                            xlaenv(3, nx);
                            for (i = 0; i < ntests; i++) {
                                result[i] = 0.0;
                            }
                            nt = 2;
                            if (ik == 1) {
                                // Test dgelqf
                                dlqt01(m, n, A, AF, AQ, AL, lda, tau, work, lwork, rwork, result);
                                if (!dgennd(m, n, AF, lda)) {
                                    result[7] = 2*thresh;
                                }
                                nt = nt + 1;
                            } // if (ik == 1)
                            else if (m <= n) {
                                // Test dorglq, using factorization returned by dlqt01
                                dlqt02(m, n, k, A, AF, AQ, AL, lda, tau, work, lwork, rwork,
                                       result);
                            } // else if (m <= n)
                            else {
                                result[0] = 0.0;
                                result[1] = 0.0;
                            } // else
                            if (m >= k) {
                                // Test DORMLQ, using factorization returned by DLQT01 
                                dlqt03(m, n, k, AF, AC, AL, AQ, lda, tau, work, lwork, rwork, res);
                                for (p = 0; p < 4; p++) {
                                    result[2+p] = res[p];
                                }
                                nt = nt + 4;
                                
                                // If m >= n and k == n, call dgelqs to solve a system
                                // with nrhs right hand sides and compute the residual.
                                if ((k == m) && (inb == 1)) {
                                    // Generate a solution and set the right hand side.
                                    // Here to dgemm extracted from dlarhs
                                    vec1 = new double[n];
                                    for (p = 1; p <= nrhs; p++) {
                                        dlarnv(2, iseed, n, vec1);
                                        for (q = 0; q < n; q++) {
                                            XACT[q][p-1] = vec1[q];    
                                        }
                                    } // for (p = 1; p <= nrhs; p++)
                                    ge.dgemm('N', 'N', m, nrhs, n, 1.0, A, lda, XACT, lda, 0.0,
                                            B, lda);
                                    ge.dlacpy('F', m, nrhs, B, lda, X, lda);
                                    srnamt = new String("DGELQS");
                                    dgelqs(m, n, nrhs, AF, lda, tau, X,
                                           lda, work, lwork, info);
                                    
                                    // Check error code from dgelqs.
                                    if (info[0] != 0) {
                                        if ((nfail == 0) && (nerrs == 0)) {
                                            Preferences.debug("Path = DLQ\n");
                                            Preferences.debug("LQ decomposition of rectangular matrices\n");
                                            Preferences.debug("LQ matrix types:\n");
                                            Preferences.debug("1. Diagonal\n");
                                            Preferences.debug("2. Upper triangular\n");
                                            Preferences.debug("3. Lower triangular\n");
                                            Preferences.debug("4. Random, cndnum[0] = 2\n");
                                            Preferences.debug("5. Random, cndnum[0] = sqrt(1.0/eps)\n");
                                            Preferences.debug("6. Random, cndnum[0] = 0.1/eps\n");
                                            Preferences.debug("7. Scaled near underflow\n");
                                            Preferences.debug("8. Scaled near overflow\n");
                                            Preferences.debug("Test ratios:\n");
                                            Preferences.debug("1: norm(L - A*Q')/(N * norm(A) * eps)\n");
                                            Preferences.debug("2: norm(I - Q*Q')/(N * eps)\n");
                                            Preferences.debug("3: norm(Q*C - Q*C)/(N * norm(C) * eps)\n");
                                            Preferences.debug("4: norm(C*Q - C*Q)/(N * norm(C) * eps)\n");
                                            Preferences.debug("5: norm(Q' * C - Q' * C)/(N * norm(C) * eps)\n");
                                            Preferences.debug("6: norm(C*Q' - C*Q')/(N * norm(C) * eps)\n");
                                            Preferences.debug("7: norm(B - A * X)/(norm(A) * norm(X) * eps)\n");
                                        } // if ((nfail == 0) && (nerrs == 0))
                                        nerrs++;
                                        Preferences.debug("Error code from dgelqs info[0] = " + info[0] + "\n");
                                        Preferences.debug("m = " + m + " n = " + n + " nrhs = " + nrhs + "\n");
                                        Preferences.debug("nb = " + nb + " type = " + imat + "\n");
                                    } // if (info[0] != 0)
                                    
                                    dget02('N', m, n, nrhs, A, lda, X, lda, B, lda, rwork, resid);
                                    result[6] = resid[0];
                                    nt++;
                                } // if ((k == m) && (inb == 1))
                                else {
                                    result[6] = 0.0;
                                }
                            } // if (m >= k)
                            else {
                                result[2] = 0.0;
                                result[3] = 0.0;
                                result[4] = 0.0;
                                result[5] = 0.0;
                            }
                            
                            // Output information about the tests that did not pass the threshold.
                            for (i = 1; i <= nt; i++) {
                                if (result[i-1] >= thresh) {
                                    if ((nfail == 0) && (nerrs == 0)) {
                                        Preferences.debug("Path = DLQ\n");
                                        Preferences.debug("LQ decomposition of rectangular matrices\n");
                                        Preferences.debug("LQ matrix types:\n");
                                        Preferences.debug("1. Diagonal\n");
                                        Preferences.debug("2. Upper triangular\n");
                                        Preferences.debug("3. Lower triangular\n");
                                        Preferences.debug("4. Random, cndnum[0] = 2\n");
                                        Preferences.debug("5. Random, cndnum[0] = sqrt(1.0/eps)\n");
                                        Preferences.debug("6. Random, cndnum[0] = 0.1/eps\n");
                                        Preferences.debug("7. Scaled near underflow\n");
                                        Preferences.debug("8. Scaled near overflow\n");
                                        Preferences.debug("Test ratios:\n");
                                        Preferences.debug("1: norm(L - A*Q')/(N * norm(A) * eps)\n");
                                        Preferences.debug("2: norm(I - Q*Q')/(N * eps)\n");
                                        Preferences.debug("3: norm(Q*C - Q*C)/(N * norm(C) * eps)\n");
                                        Preferences.debug("4: norm(C*Q - C*Q)/(N * norm(C) * eps)\n");
                                        Preferences.debug("5: norm(Q' * C - Q' * C)/(N * norm(C) * eps)\n");
                                        Preferences.debug("6: norm(C*Q' - C*Q')/(N * norm(C) * eps)\n");
                                        Preferences.debug("7: norm(B - A * X)/(norm(A) * norm(X) * eps)\n");
                                    } // if ((nfail == 0) && (nerrs == 0))
                                    Preferences.debug("m = " + m + " n = " + n + " k = " + k + "\n");
                                    Preferences.debug("nb = " + nb + " nx = " + nx + " type = " + imat + "\n");
                                    Preferences.debug("Test = " + i + " with result = " + result[i-1] + "\n");
                                    nfail++;
                                } // if (result[i-1] >= thresh)
                            } // for (i = 1; i <= nt; i++)
                            nrun = nrun + nt;
                        } // for (inb = 1; inb <= nnb; inb++)
                    } // for (ik = 1; ik <= nk; ik++)
                } // for (imat = 1; imat <= ntypes; imat++)
            } // for (in = 1; in <= nn; in++)
        } // for (im = 1; im <= nm; im++)
        
        // Output a summary of the results.
        if (nfail > 0) {
            Preferences.debug("In dchklq " + nfail + " out of " + nrun + " tests failed to pass the threshold\n");
        }
        else {
            Preferences.debug("In dchklq all " + nrun + " tests run passed the threshold\n");
        }
        if (nerrs > 0) {
            Preferences.debug("In dchklq " + nerrs + " errors occurred\n");
        }
        return;
    } // dchklq
    
    /** This is a port of version 3.1 LAPACK routine DGELQS.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), TAU( * ),
            $                   WORK( LWORK )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  Compute a minimum-norm solution
       *      min || A*X - B ||
       *  using the LQ factorization
       *      A = L*Q
       *  computed by DGELQF.
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix A.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix A.  N >= M >= 0.
       *
       *  NRHS    (input) INTEGER
       *          The number of columns of B.  NRHS >= 0.
       *
       *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          Details of the LQ factorization of the original matrix A as
       *          returned by DGELQF.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.  LDA >= M.
       *
       *  TAU     (input) DOUBLE PRECISION array, dimension (M)
       *          Details of the orthogonal matrix Q.
       *
       *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
       *          On entry, the m-by-nrhs right hand side matrix B.
       *          On exit, the n-by-nrhs solution matrix X.
       *
       *  LDB     (input) INTEGER
       *          The leading dimension of the array B. LDB >= N.
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
       *
       *  LWORK   (input) INTEGER
       *          The length of the array WORK.  LWORK must be at least NRHS,
       *          and should be at least NRHS*NB, where NB is the block size
       *          for this environment.
       *
       *  INFO    (output) INTEGER
       *          = 0: successful exit
       *          < 0: if INFO = -i, the i-th argument had an illegal value
       */
    private void dgelqs(int m, int n, int nrhs, double[][] A, int lda, double[] tau,
                        double[][] B, int ldb, double[] work, int lwork,
                        int[] info) {
        int p;
        int q;
        int row1;
        double array1[][];
        
        // Test the input parameters.
        info[0] = 0;
        if (m < 0) {
            info[0] = -1;
        }
        else if ((n < 0) || (m > n)) {
            info[0] = -2;
        }
        else if (nrhs < 0) {
            info[0] = -3;
        }
        else if (lda < Math.max(1, m)) {
            info[0] = -5;
        }
        else if (ldb < Math.max(1, n)) {
            info[0] = -8;
        }
        else if ((lwork < 1) || (lwork < nrhs) && (m > 0) && (n > 0)) {
            info[0] = -10;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("Error dgeqls had info[0] = " + info[0]);
            return;
        }
        
        // Quick return if possible
        if ((n == 0) || (nrhs == 0) || (m == 0)) {
            return;
        }
        
        // Solve L*X = B(1:m,:)
        
        ge.dtrsm('L', 'L', 'N', 'N', m, nrhs, 1.0, A, lda, B, ldb);
        
        // Set B(m+1:n,:) to zero
        
        if (m < n) {
            row1 = Math.max(1, n-m);
            array1 = new double[row1][nrhs];
            for (p = 0; p < row1; p++) {
                for (q = 0; q < nrhs; q++) {
                    array1[p][q] = B[m+p][q];
                }
            }
            ge.dlaset('F', n-m, nrhs, 0.0, 0.0, array1, row1);
            for (p = 0; p < row1; p++) {
                for (q = 0; q < nrhs; q++) {
                    B[m+p][q] = array1[p][q];
                }
            }
        } // if (m < n)
        
        // B = Q' * B
        dormlq('L', 'T', n, nrhs, m, A, lda, tau, B, ldb, work, lwork, info);
        return;
    } // dgelqs
    
    /** This is a port of a portion of version 3.1 LAPACK routine DERRLQ.
     * *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
     *     November 2006
     */
    private void derrlq() {
        int nmax = 2;
        int i;
        int info[] = new int[1];
        int j;
        double A[][] = new double[nmax][nmax];
        double AF[][] = new double[nmax][nmax];
        double B[][] = new double[nmax][nmax];
        double b[] = new double[nmax];
        double w[] = new double[nmax];
        double x[] = new double[nmax];
        int npass = 49;
        int ntotal = 49;
        
        // Set the variables to innocuous values
        for (j = 1; j <= nmax; j++) {
            for (i = 1; i <= nmax; i++) {
                A[i-1][j-1] = 1.0/(double)(i+j);
                AF[i-1][j-1] = 1.0/(double)(i+j);
                B[i-1][j-1] = 1.0/(double)(i+j);
            }
            b[j-1] = 0.0;
            w[j-1] = 0.0;
            x[j-1] = 0.0;
        }
        
        // Error exits for LQ factorization
        
        // DGELQF
        dgelqf(-1, 0, A, 1, b, w, 1, info);
        if (info[0] != -1) {
            Preferences.debug("dgelqf(-1, 0, A, 1, b, w, 1, info) produced info[0] = " + info[0] +
                              " instead of -1\n");
            npass--;
        }
        
        dgelqf(0, -1, A, 1, b, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dgelqf(-0, -1, A, 1, b, w, 1, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;    
        }
        
        dgelqf(2, 1, A, 1, b, w, 2, info);
        if (info[0] != -4) {
            Preferences.debug("dgelqf(2, 1, A, 1, b, w, 2, info) produced info[0] = " + info[0] +
            " instead of -4\n");
            npass--;    
        }
        
        dgelqf(2, 1, A, 2, b, w, 1, info);
        if (info[0] != -7) {
            Preferences.debug("dgelqf(2, 1, A, 2, b, w, 1, info) produced info[0] = " + info[0] +
            " instead of -7\n");
            npass--;    
        }
        
        // DGELQ2
        dgelq2(-1, 0, A, 1, b, w, info);
        if (info[0] != -1) {
            Preferences.debug("dgelq2(-1, 0, A, 1, b, w, info) produced info[0] = " + info[0] +
            " instead of -1\n");
            npass--;     
        }
        
        dgelq2(0, -1, A, 1, b, w, info);
        if (info[0] != -2) {
            Preferences.debug("dgelq2(0, -1, A, 1, b, w, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;     
        }
        
        dgelq2(2, 1, A, 1, b, w, info);
        if (info[0] != -4) {
            Preferences.debug("dgelq2(2, 1, A, 1, b, w, info) produced info[0] = " + info[0] +
            " instead of -4\n");
            npass--;     
        }
        
        // DGELQS
        dgelqs(-1, 0, 0, A, 1, x, B, 1, w, 1, info);
        if (info[0] != -1) {
            Preferences.debug("dgelqs(-1, 0, 0, A, 1, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -1\n");
            npass--;
        }
        
        dgelqs(0, -1, 0, A, 1, x, B, 1, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dgelqs(0, -1, 0, A, 1, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -2\n");
            npass--;
        }
        
        dgelqs(2, 1, 0, A, 2, x, B, 1, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dgelqs(2, 1, 0, A, 2, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -2\n");
            npass--;
        }
        
        dgelqs(0, 0, -1, A, 1, x, B, 1, w, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dgelqs(0, 0, -1, A, 1, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -3\n");
            npass--;
        }
        
        dgelqs(2, 2, 0, A, 1, x, B, 2, w, 1, info);
        if (info[0] != -5) {
            Preferences.debug("dgelqs(2, 2, 0, A, 1, x, B, 2, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -5\n");
            npass--;
        }
        
        dgelqs(1, 2, 0, A, 1, x, B, 1, w, 1, info);
        if (info[0] != -8) {
            Preferences.debug("dgelqs(1, 2, 0, A, 1, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -8\n");
            npass--;
        }
        
        dgelqs(1, 1, 2, A, 1, x, B, 1, w, 1, info);
        if (info[0] != -10) {
            Preferences.debug("dgelqs(1, 1, 2, A, 1, x, B, 1, w, 1, info) produced info[0] = " + 
            info[0] + " instead of -10\n");
            npass--;
        }
        
        // DORGLQ
        dorglq(-1, 0, 0, A, 1, x, w, 1, info);
        if (info[0] != -1) {
            Preferences.debug("dorglq(-1, 0, 0, A, 1, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -1\n");
            npass--;      
        }
        
        dorglq(0, -1, 0, A, 1, x, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dorglq(0, -1, 0, A, 1, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;      
        }
        
        dorglq(2, 1, 0, A, 2, x, w, 2, info);
        if (info[0] != -2) {
            Preferences.debug("dorglq(2, 1, 0, A, 2, x, w, 2, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;      
        }
        
        dorglq(0, 0, -1, A, 1, x, w, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dorglq(0, 0, -1, A, 1, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -3\n");
            npass--;      
        }
        
        dorglq(1, 1, 2, A, 1, x, w, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dorglq(1, 1, 2, A, 1, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -3\n");
            npass--;      
        }
        
        dorglq(2, 2, 0, A, 1, x, w, 2, info);
        if (info[0] != -5) {
            Preferences.debug("dorglq(2, 2, 0, A, 1, x, w, 2, info) produced info[0] = " + info[0] +
            " instead of -5\n");
            npass--;      
        }
        
        dorglq(2, 2, 0, A, 2, x, w, 1, info);
        if (info[0] != -8) {
            Preferences.debug("dorglq(2, 2, 0, A, 2, x, w, 1, info) produced info[0] = " + info[0] +
            " instead of -8\n");
            npass--;      
        }
        
        // DORGL2
        dorgl2(-1, 0, 0, A, 1, x, w, info);
        if (info[0] != -1) {
            Preferences.debug("dorgl2(-1, 0, 0, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -1\n");
            npass--;
        }
        
        dorgl2(0, -1, 0, A, 1, x, w, info);
        if (info[0] != -2) {
            Preferences.debug("dorgl2(0, -1, 0, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;
        }
        
        dorgl2(2, 1, 0, A, 2, x, w, info);
        if (info[0] != -2) {
            Preferences.debug("dorgl2(2, 1, 0, A, 2, x, w, info) produced info[0] = " + info[0] +
            " instead of -2\n");
            npass--;
        }
        
        dorgl2(0, 0, -1, A, 1, x, w, info);
        if (info[0] != -3) {
            Preferences.debug("dorgl2(0, 0, -1, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -3\n");
            npass--;
        }
        
        dorgl2(1, 1, 2, A, 1, x, w, info);
        if (info[0] != -3) {
            Preferences.debug("dorgl2(1, 1, 2, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -3\n");
            npass--;
        }
        
        dorgl2(2, 2, 0, A, 1, x, w, info);
        if (info[0] != -5) {
            Preferences.debug("dorgl2(2, 2, 0, A, 1, x, w, info) produced info[0] = " + info[0] +
            " instead of -5\n");
            npass--;
        }
        
        // DORMLQ
        dormlq('/', 'N', 0, 0, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -1) {
            Preferences.debug("dormlq('/', 'N', 0, 0, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -1\n");
            npass--;
        }
        
        dormlq('L', '/', 0, 0, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dormlq('L', '/', 0, 0, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -2\n");
            npass--;
        }
        
        dormlq('L', 'N', -1, 0, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dormlq('L', 'N', -1, 0, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -3\n");
            npass--;
        }
        
        dormlq('L', 'N', 0, -1, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -4) {
            Preferences.debug("dormlq('L', 'N', 0, -1, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -4\n");
            npass--;
        }
        
        dormlq('L', 'N', 0, 0, -1, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -5) {
            Preferences.debug("dormlq('L', 'N', 0, 0, -1, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        dormlq('L', 'N', 0, 1, 1, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -5) {
            Preferences.debug("dormlq('L', 'N', 0, 1, 1, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        dormlq('R', 'N', 1, 0, 1, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -5) {
            Preferences.debug("dormlq('R', 'N', 1, 0, 1, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        dormlq('L', 'N', 2, 0, 2, A, 1, x, AF, 2, w, 1, info);
        if (info[0] != -7) {
            Preferences.debug("dormlq('L', 'N', 2, 0, 2, A, 1, x, AF, 2, w, 1, info) produced info[0] = " +
            info[0] + " instead of -7\n");
            npass--;
        }
        
        dormlq('R', 'N', 0, 2, 2, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -7) {
            Preferences.debug("dormlq('R', 'N', 0, 2, 2, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -7\n");
            npass--;
        }
        
        dormlq('L', 'N', 2, 1, 0, A, 2, x, AF, 1, w, 1, info);
        if (info[0] != -10) {
            Preferences.debug("dormlq('L', 'N', 2, 1, 0, A, 2, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -10\n");
            npass--;
        }
        
        dormlq('L', 'N', 1, 2, 0, A, 1, x, AF, 1, w, 1, info);
        if (info[0] != -12) {
            Preferences.debug("dormlq('L', 'N', 1, 2, 0, A, 1, x, AF, 1, w, 1, info) produced info[0] = " +
            info[0] + " instead of -12\n");
            npass--;
        }
        
        dormlq('R', 'N', 2, 1, 0, A, 1, x, AF, 2, w, 1, info);
        if (info[0] != -12) {
            Preferences.debug("dormlq('R', 'N', 2, 1, 0, A, 1, x, AF, 2, w, 1, info) produced info[0] = " +
            info[0] + " instead of -12\n");
            npass--;
        }
        
        // DORML2
        dorml2('/', 'N', 0, 0, 0, A, 1, x, AF, 1, w, info);
        if (info[0] != -1) {
            Preferences.debug("dorml2('/', 'N', 0, 0, 0, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -1\n");
            npass--;
        }
        
        dorml2('L', '/', 0, 0, 0, A, 1, x, AF, 1, w, info);
        if (info[0] != -2) {
            Preferences.debug("dorml2('L', '/', 0, 0, 0, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -2\n");
            npass--;
        }
        
        dorml2('L', 'N', -1, 0, 0, A, 1, x, AF, 1, w, info);
        if (info[0] != -3) {
            Preferences.debug("dorml2('L', 'N', -1, 0, 0, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -3\n");
            npass--;
        }
        
        dorml2('L', 'N', 0, -1, 0, A, 1, x, AF, 1, w, info);
        if (info[0] != -4) {
            Preferences.debug("dorml2('L', 'N', 0, -1, 0, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -4\n");
            npass--;
        }
        
        dorml2('L', 'N', 0, 0, -1, A, 1, x, AF, 1, w, info);
        if (info[0] != -5) {
            Preferences.debug("dorml2('L', 'N', 0, 0, -1, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        dorml2('L', 'N', 0, 1, 1, A, 1, x, AF, 1, w, info);
        if (info[0] != -5) {
            Preferences.debug("dorml2('L', 'N', 0, 1, 1, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        dorml2('R', 'N', 1, 0, 1, A, 1, x, AF, 1, w, info);
        if (info[0] != -5) {
            Preferences.debug("dorml2('R', 'N', 1, 0, 1, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -5\n");
            npass--;
        }
        
        dorml2('L', 'N', 2, 1, 2, A, 1, x, AF, 2, w, info);
        if (info[0] != -7) {
            Preferences.debug("dorml2('L', 'N', 2, 1, 2, A, 1, x, AF, 2, w, info) produced info[0] = " +
            info[0] + " instead of -7\n");
            npass--;
        }
        
        dorml2('R', 'N', 1, 2, 2, A, 1, x, AF, 1, w, info);
        if (info[0] != -7) {
            Preferences.debug("dorml2('R', 'N', 1, 2, 2, A, 1, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -7\n");
            npass--;
        }
        
        dorml2('L', 'N', 2, 1, 0, A, 2, x, AF, 1, w, info);
        if (info[0] != -10) {
            Preferences.debug("dorml2('L', 'N', 2, 1, 0, A, 2, x, AF, 1, w, info) produced info[0] = " +
            info[0] + " instead of -10\n");
            npass--;
        }
        
        Preferences.debug("derrqr correctly found " + npass + " of " + ntotal + " error exits\n");
        return;
    } // derrlq
    
    /** This is a port of the version 3.1 LAPACK test routine DLQT01.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             INTEGER            LDA, LWORK, M, N
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), L( LDA, * ),
            $                   Q( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
            $                   WORK( LWORK )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DLQT01 tests DGELQF, which computes the LQ factorization of an m-by-n
       *  matrix A, and partially tests DORGLQ which forms the n-by-n
       *  orthogonal matrix Q.
       *
       *  DLQT01 compares L with A*Q', and checks that Q is orthogonal.
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix A.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix A.  N >= 0.
       *
       *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          The m-by-n matrix A.
       *
       *  AF      (output) DOUBLE PRECISION array, dimension (LDA,N)
       *          Details of the LQ factorization of A, as returned by DGELQF.
       *          See DGELQF for further details.
       *
       *  Q       (output) DOUBLE PRECISION array, dimension (LDA,N)
       *          The n-by-n orthogonal matrix Q.
       *
       *  L       (workspace) DOUBLE PRECISION array, dimension (LDA,max(M,N))
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the arrays A, AF, Q and L.
       *          LDA >= max(M,N).
       *
       *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
       *          The scalar factors of the elementary reflectors, as returned
       *          by DGELQF.
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
       *
       *  LWORK   (input) INTEGER
       *          The dimension of the array WORK.
       *
       *  RWORK   (workspace) DOUBLE PRECISION array, dimension (max(M,N))
       *
       *  RESULT  (output) DOUBLE PRECISION array, dimension (2)
       *          The test ratios:
       *          RESULT(1) = norm( L - A*Q' ) / ( N * norm(A) * EPS )
       *          RESULT(2) = norm( I - Q*Q' ) / ( N * EPS )
       */
    private void dlqt01(int m, int n, double[][] A, double[][] AF, double[][] Q,
                        double[][] L, int lda, double[] tau, double[] work,
                        int lwork, double[] rwork, double[] result) {
        double rogue = -1.0E10;
        int info[] = new int[1];
        int minmn;
        double anorm;
        double eps;
        double resid;
        double array1[][];
        double array2[][];
        int row1;
        int p;
        int q;
        
        minmn = Math.min(m, n);
        eps = ge.dlamch('E'); // Epsilon
        
        // Copy the matrix A to the array AF.
        ge.dlacpy('F', m, n, A, lda, AF, lda);
        
        // Factorize the matrix A in the array AF.
        srnamt = new String("DGELQF");
        dgelqf(m, n, AF, lda, tau, work, lwork, info);
        
        // Copy details of Q
        ge.dlaset('F', n, n, rogue, rogue, Q, lda);
        if (n > 1) {
            row1 = Math.max(1, m);
            array1 = new double[row1][n-1];
            array2 = new double[row1][n-1];
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-1; q++) {
                    array1[p][q] = AF[p][q+1];
                }
            }
            ge.dlacpy('U', m, n-1, array1, row1, array2, row1);
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-1; q++) {
                    Q[p][q+1] = array2[p][q];
                }
            }
        } // if (n > 1)
        
        // Generate the n-by-n matrix Q
        srnamt = new String("DORGLQ");
        dorglq(n, n, minmn, Q, lda, tau, work, lwork, info);
        
        // Copy L
        ge.dlaset('F', m, n, 0.0, 0.0, L, lda);
        ge.dlacpy('L', m, n, AF, lda, L, lda);
        
        // Compute L - A*Q'
        ge.dgemm('N', 'T', m, n, n, -1.0, A, lda, Q, lda, 1.0, L, lda);
        
        // Compute norm(L - Q'*A)/(N * norm(A) * eps).
        anorm = ge.dlange('1', m, n, A, lda, rwork);
        resid = ge.dlange('1', m, n, L, lda, rwork);
        if (anorm > 0.0) {
            result[0] = ((resid /(double)Math.max(1,n))/anorm)/eps;
        }
        else {
            result[0] = 0;
        }
        
        // Compute I - Q*Q'
        ge.dlaset('F', n, n, 0.0, 1.0, L, lda);
        dsyrk('U', 'N', n, n, -1.0, Q, lda, 1.0, L, lda);
        
        // Compute norm(I - Q*Q')/(N * eps)
        resid = dlansy('1', 'U', n, L, lda, rwork);
        result[1] = (resid/(double)Math.max(1, n))/eps;
        return;
    } // dlqt01
    
    /** This is a port of version 3.1 LAPACK routine DLQT02.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             INTEGER            K, LDA, LWORK, M, N
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), L( LDA, * ),
            $                   Q( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
            $                   WORK( LWORK )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DLQT02 tests DORGLQ, which generates an m-by-n matrix Q with
       *  orthonornmal rows that is defined as the product of k elementary
       *  reflectors.
       *
       *  Given the LQ factorization of an m-by-n matrix A, DLQT02 generates
       *  the orthogonal matrix Q defined by the factorization of the first k
       *  rows of A; it compares L(1:k,1:m) with A(1:k,1:n)*Q(1:m,1:n)', and
       *  checks that the rows of Q are orthonormal.
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix Q to be generated.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix Q to be generated.
       *          N >= M >= 0.
       *
       *  K       (input) INTEGER
       *          The number of elementary reflectors whose product defines the
       *          matrix Q. M >= K >= 0.
       *
       *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          The m-by-n matrix A which was factorized by DLQT01.
       *
       *  AF      (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          Details of the LQ factorization of A, as returned by DGELQF.
       *          See DGELQF for further details.
       *
       *  Q       (workspace) DOUBLE PRECISION array, dimension (LDA,N)
       *
       *  L       (workspace) DOUBLE PRECISION array, dimension (LDA,M)
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the arrays A, AF, Q and L. LDA >= N.
       *
       *  TAU     (input) DOUBLE PRECISION array, dimension (M)
       *          The scalar factors of the elementary reflectors corresponding
       *          to the LQ factorization in AF.
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
       *
       *  LWORK   (input) INTEGER
       *          The dimension of the array WORK.
       *
       *  RWORK   (workspace) DOUBLE PRECISION array, dimension (M)
       *
       *  RESULT  (output) DOUBLE PRECISION array, dimension (2)
       *          The test ratios:
       *          RESULT(1) = norm( L - A*Q' ) / ( N * norm(A) * EPS )
       *          RESULT(2) = norm( I - Q*Q' ) / ( N * EPS )
       */
    private void dlqt02(int m, int n, int k, double[][] A, double[][] AF, double[][] Q,
                        double[][] L, int lda, double[] tau, double[] work, int lwork,
                        double[] rwork, double[] result) {
        double rogue = -1.0E10;
        int info[] = new int[1];
        double anorm;
        double eps;
        double resid;
        int row1;
        int p;
        int q;
        double array1[][];
        double array2[][];
        
        eps = ge.dlamch('E'); // Epsilon
        
        // Copy the first k columns of the factorization to the array Q
        ge.dlaset('F', m, n, rogue, rogue, Q, lda);
        row1 = Math.max(1, k);
        array1 = new double[row1][n-1];
        for (p = 0; p < row1; p++) {
            for (q = 0; q < n-1; q++) {
                array1[p][q] = AF[p][q+1];
            }
        }
        array2 = new double[row1][n-1];
        ge.dlacpy('U', k, n-1, array1, row1, array2, row1);
        for (p = 0; p < row1; p++) {
            for (q = 0; q < n-1; q++) {
                Q[p][q+1] = array2[p][q];
            }
        }
        
        // Generate the first n columns of the matrix Q
        srnamt = new String("DORGLQ");
        dorglq(m, n, k, Q, lda, tau, work, lwork, info);
        
        // Copy L(1:k,1:m)
        ge.dlaset('F', k, m, 0.0, 0.0, L, lda);
        ge.dlacpy('L', k, m, AF, lda, L, lda);
        
        // Compute L(1:k,1:m) - A(1:k,1:n) * Q(1:m,1:n)'
        ge.dgemm('N', 'T', k, m, n, -1.0, A, lda, Q, lda, 1.0, L, lda);
        
        // Compute norm(L - A*Q')/(n * norm(A) * eps).
        anorm = ge.dlange('1', k, n, A, lda, rwork);
        resid = ge.dlange('1', k, m, L, lda, rwork);
        if (anorm > 0) {
            result[0] = ((resid/(double)Math.max(1, n))/anorm)/eps;
        }
        else {
            result[0] = 0;
        }
        
        // Compute I - Q*Q'
        ge.dlaset('F', m, m, 0.0, 1.0, L, lda);
        dsyrk('U', 'N', m, n, -1.0, Q, lda, 1.0, L, lda);
        
        // Compute norm(I - Q*Q')/(n * eps).
        resid = dlansy('1', 'U', m, L, lda, rwork);
        result[1] = (resid/(double)Math.max(1,n))/eps;
        return;   
    } // dlqt02

    /** This is a port of version 3.1 LAPACK test routine DLQT03.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             INTEGER            K, LDA, LWORK, M, N
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   AF( LDA, * ), C( LDA, * ), CC( LDA, * ),
            $                   Q( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
            $                   WORK( LWORK )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DLQT03 tests DORMLQ, which computes Q*C, Q'*C, C*Q or C*Q'.
       *
       *  DLQT03 compares the results of a call to DORMLQ with the results of
       *  forming Q explicitly by a call to DORGLQ and then performing matrix
       *  multiplication by a call to DGEMM.
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows or columns of the matrix C; C is n-by-m if
       *          Q is applied from the left, or m-by-n if Q is applied from
       *          the right.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The order of the orthogonal matrix Q.  N >= 0.
       *
       *  K       (input) INTEGER
       *          The number of elementary reflectors whose product defines the
       *          orthogonal matrix Q.  N >= K >= 0.
       *
       *  AF      (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          Details of the LQ factorization of an m-by-n matrix, as
       *          returned by DGELQF. See SGELQF for further details.
       *
       *  C       (workspace) DOUBLE PRECISION array, dimension (LDA,N)
       *
       *  CC      (workspace) DOUBLE PRECISION array, dimension (LDA,N)
       *
       *  Q       (workspace) DOUBLE PRECISION array, dimension (LDA,N)
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the arrays AF, C, CC, and Q.
       *
       *  TAU     (input) DOUBLE PRECISION array, dimension (min(M,N))
       *          The scalar factors of the elementary reflectors corresponding
       *          to the LQ factorization in AF.
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
       *
       *  LWORK   (input) INTEGER
       *          The length of WORK.  LWORK must be at least M, and should be
       *          M*NB, where NB is the blocksize for this environment.
       *
       *  RWORK   (workspace) DOUBLE PRECISION array, dimension (M)
       *
       *  RESULT  (output) DOUBLE PRECISION array, dimension (4)
       *          The test ratios compare two techniques for multiplying a
       *          random matrix C by an n-by-n orthogonal matrix Q.
       *          RESULT(1) = norm( Q*C - Q*C )  / ( N * norm(C) * EPS )
       *          RESULT(2) = norm( C*Q - C*Q )  / ( N * norm(C) * EPS )
       *          RESULT(3) = norm( Q'*C - Q'*C )/ ( N * norm(C) * EPS )
       *          RESULT(4) = norm( C*Q' - C*Q' )/ ( N * norm(C) * EPS )
       */
    private void dlqt03(int m, int n, int k, double[][] AF, double[][] C,
                        double[][] CC, double[][] Q, int lda, double[] tau,
                        double[] work, int lwork, double[] rwork, double[] result) {
        double rogue = -1.0E10;
        char side;
        char trans;
        int info[] = new int[1];
        int iside;
        int itrans;
        int j;
        int mc;
        int nc;
        double cnorm;
        double eps;
        double resid;
        int iseed[] = new int[]{1988, 1989, 1990, 1991};
        int row1;
        int p;
        int q;
        double array1[][];
        double array2[][];
        double vec1[];
        
        eps = ge.dlamch('E'); // epsilon
        
        // Copy the first k columns of the factorization to the array Q
        ge.dlaset('F', n, n, rogue, rogue, Q, lda);
        if (n >= 1) {
            row1 = Math.max(1, k);
            array1 = new double[row1][n-1];
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-1; q++) {
                    array1[p][q] = AF[p][q+1];
                }
            }
            array2 = new double[row1][n-1];
            ge.dlacpy('U', k, n-1, array1, row1, array2, row1);
            for (p = 0; p < row1; p++) {
                for (q = 0; q < n-1; q++) {
                    Q[p][q+1] = array2[p][q];
                }
            }
        } // if (n >= 1)
        
        // Generate the n by n matrix Q
        srnamt = new String("DORGLQ");
        dorglq(n, n, k, Q, lda, tau, work, lwork, info);
        
        for (iside = 1; iside <= 2; iside++) {
            if (iside == 1) {
                side = 'L';
                mc = n;
                nc = m;
            }
            else {
                side = 'R';
                mc = m;
                nc = n;
            }
            
            // Generate mc by nc matrix C
            vec1 = new double[mc];
            for (j = 1; j <= nc; j++) {
                dlarnv(2, iseed, mc, vec1);
                for (p = 0; p < mc; p++) {
                    C[p][j-1] = vec1[p];
                }
            } // for (j = 1; j <= nc; j++)
            cnorm = ge.dlange('1', mc, nc, C, lda, rwork);
            if (cnorm == 0.0) {
                cnorm = 1.0;
            }
            
            for (itrans = 1; itrans <= 2; itrans++) {
                if (itrans == 1) {
                    trans = 'N';
                }
                else {
                    trans = 'T';
                }
                
                // Copy C
                ge.dlacpy('F', mc, nc, C, lda, CC, lda);
                
                // Apply Q or Q' to C
                srnamt = new String("DORMLQ");
                dormlq(side, trans, mc, nc, k, AF, lda, tau, CC, lda, work, lwork,
                       info);
                
                // Form explicit product and subtract
                if ((side == 'L') || (side == 'l')) {
                    ge.dgemm(trans, 'N', mc, nc, mc, -1.0, Q, lda, C, lda, 1.0, CC, lda);
                }
                else {
                    ge.dgemm('N', trans, mc, nc, nc, -1.0, C, lda, Q, lda, 1.0, CC, lda);
                }
                
                // Compute error in the difference
                resid = ge.dlange('1', mc, nc, CC, lda, rwork);
                result[(iside-1)*2+itrans-1] = resid/(Math.max(1,n)*cnorm*eps);
            } // for (itrans = 1; itrans <= 2; itrans++)
        } // for (iside = 1; iside <= 2; iside++)
        return;   
    } // dlqt03
    
    /** This ddrvls_test routine is a port of a portion of the version 3.1.1 LAPACK test routine DCHKAA by
     * Univ. of Tennessee, Univ. Of California Berkeley and NAG Ltd., January, 2007. and some values from 
     * the test data file dtest.in.
     */
    public void ddrvls_test() {
        int i;
        
        // Number of values of m
        int nm = 7;
        
        // Values of m (row dimension)
        // dtest.in uses 50 rather than 16
        int[] mval = new int[] { 0, 1, 2, 3, 5, 10, 16 };
        
        // mmax is the maximum value of m in mval.
        int mmax = 0;
        for (i = 0; i < mval.length; i++) {
            if (mval[i] > mmax) {
                mmax = mval[i];
            }
        }
        
        // Number of values of n
        int nn = 7;
        
        // Values of n (column dimension)
        // dtest.in uses 50 rather than 16
        int[] nval = new int[] { 0, 1, 2, 3, 5, 10, 16 };
        
        // nmax is the maximum value of n in nval.
        int nmax = 0;
        for (i = 0; i < nval.length; i++) {
            if (nval[i] > nmax) {
                nmax = nval[i];
            }
        }
        
        // Number of values of nrhs
        // dchkaa has nns = 1.  dtest.in uses nns = 3.
        int nns = 3;
        
        // Values of nrhs (number of right hand sides)
        // dchkaa uses only 2.  dtest.in uses 1, 2, 15.
        int[] nsval = new int[]{1,2,15};
        
        // nsmax is the maximum value of nrhs in nsval.
        int nsmax = 0;
        for (i = 0; i < nsval.length; i++) {
            if (nsval[i] > nsmax) {
                nsmax = nsval[i];
            }
        }
        
        // Number of values of nb
        int nnb = 5;
        
        // Values of nb (the blocksize)
        int nbval[] = new int[] {1, 3, 3, 3, 20};
        
        // Values of nx (crossover point)
        // There are nnb values of nx.
        int nxval[] = new int[] {1, 0, 5, 9, 1};
        
        // Number of values of rank
        int nrank = 3;
        
        // Values of rank (as a % of n)
        int rankval[] = new int[] {30, 50, 90};
        
        // Threshold value of test ratio
        // dchkaa has 20.0, dtest.in has 30.0
        double thresh = 20.0;
        
        // Test the LAPACK routines
        boolean tstchk = true;
        
        // Test the driver routines
        boolean tstdrv = true;
        
        // Test the error exits
        // Passed all 5 exits on test.
        // Put at false so as not to have to hit okay to 5 displayError messages.
        boolean tsterr = false;
        
        // The number of different values that can be used for each of m, n, nrhs, nb, and nx
        int maxin = 12;
        
        // The maximum number of right hand sides
        int maxrhs = 16;
        int nmats = 6;
        int ntypes = 6;
        int nrhs = nsval[0];
        boolean dotype[] = new boolean[ntypes];
        double A[][] = new double[mmax][nmax];
        double COPYA[][] = new double[mmax][nmax];
        double B[][] = new double[mmax][nsmax];
        double COPYB[][] = new double[mmax][nsmax];
        double C[][] = new double[mmax][nsmax];
        double s[] = new double[Math.min(mmax, nmax)];
        double copys[] = new double[Math.min(mmax, nmax)];
        double work[] = new double[132*(132+16+30)];
        int iwork[] = new int[15*nmax];
        iparms = new int[11];
        double eps;
       
        for (i = 0; i < ntypes; i++) {
            dotype[i] = true;
        }
        
        // Output the machine dependent constants
        eps = ge.dlamch('U');
        Preferences.debug("Underflow threshold = " + eps + "\n");
        eps = ge.dlamch('O');
        Preferences.debug("Overflow threshold = " + eps + "\n");
        eps = ge.dlamch('E');
        Preferences.debug("Precision = " + eps + "\n");
        
        ddrvls(dotype, nm, mval, nn, nval, nns, nsval, nnb, nbval, nxval, thresh, tsterr, 
               A, COPYA, B, COPYB, C, s, copys, work, iwork);
    } // ddrvls_test
    
    /** This is a port of that part of version 3.1.1 LAPACK test routine DDRVLS used for
     *  testing the least squares driver routine DGELSS.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     January 2007
       *
       *     .. Scalar Arguments ..
             LOGICAL            TSTERR
             INTEGER            NM, NN, NNB, NNS, NOUT
             DOUBLE PRECISION   THRESH
       *     ..
       *     .. Array Arguments ..
             LOGICAL            DOTYPE( * )
             INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NSVAL( * ),
            $                   NVAL( * ), NXVAL( * )
             DOUBLE PRECISION   A( * ), B( * ), C( * ), COPYA( * ), COPYB( * ),
            $                   COPYS( * ), S( * ), WORK( * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DDRVLS tests the least squares driver routines DGELS, DGELSS, DGELSX,
       *  DGELSY and DGELSD.
       *
       *  Arguments
       *  =========
       *
       *  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
       *          The matrix types to be used for testing.  Matrices of type j
       *          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
       *          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
       *          The matrix of type j is generated as follows:
       *          j=1: A = U*D*V where U and V are random orthogonal matrices
       *               and D has random entries (> 0.1) taken from a uniform 
       *               distribution (0,1). A is full rank.
       *          j=2: The same of 1, but A is scaled up.
       *          j=3: The same of 1, but A is scaled down.
       *          j=4: A = U*D*V where U and V are random orthogonal matrices
       *               and D has 3*min(M,N)/4 random entries (> 0.1) taken
       *               from a uniform distribution (0,1) and the remaining
       *               entries set to 0. A is rank-deficient. 
       *          j=5: The same of 4, but A is scaled up.
       *          j=6: The same of 5, but A is scaled down.
       *
       *  NM      (input) INTEGER
       *          The number of values of M contained in the vector MVAL.
       *
       *  MVAL    (input) INTEGER array, dimension (NM)
       *          The values of the matrix row dimension M.
       *
       *  NN      (input) INTEGER
       *          The number of values of N contained in the vector NVAL.
       *
       *  NVAL    (input) INTEGER array, dimension (NN)
       *          The values of the matrix column dimension N.
       *
       *  NNS     (input) INTEGER
       *          The number of values of NRHS contained in the vector NSVAL.
       *
       *  NSVAL   (input) INTEGER array, dimension (NNS)
       *          The values of the number of right hand sides NRHS.
       *
       *  NNB     (input) INTEGER
       *          The number of values of NB and NX contained in the
       *          vectors NBVAL and NXVAL.  The blocking parameters are used
       *          in pairs (NB,NX).
       *
       *  NBVAL   (input) INTEGER array, dimension (NNB)
       *          The values of the blocksize NB.
       *
       *  NXVAL   (input) INTEGER array, dimension (NNB)
       *          The values of the crossover point NX.
       *
       *  THRESH  (input) DOUBLE PRECISION
       *          The threshold value for the test ratios.  A result is
       *          included in the output file if RESULT >= THRESH.  To have
       *          every test ratio printed, use THRESH = 0.
       *
       *  TSTERR  (input) LOGICAL
       *          Flag that indicates whether error exits are to be tested.
       *
       *  A       (workspace) DOUBLE PRECISION array, dimension (MMAX*NMAX)
       *          where MMAX is the maximum value of M in MVAL and NMAX is the
       *          maximum value of N in NVAL.
       *
       *  COPYA   (workspace) DOUBLE PRECISION array, dimension (MMAX*NMAX)
       *
       *  B       (workspace) DOUBLE PRECISION array, dimension (MMAX*NSMAX)
       *          where MMAX is the maximum value of M in MVAL and NSMAX is the
       *          maximum value of NRHS in NSVAL.
       *
       *  COPYB   (workspace) DOUBLE PRECISION array, dimension (MMAX*NSMAX)
       *
       *  C       (workspace) DOUBLE PRECISION array, dimension (MMAX*NSMAX)
       *
       *  S       (workspace) DOUBLE PRECISION array, dimension
       *                      (min(MMAX,NMAX))
       *
       *  COPYS   (workspace) DOUBLE PRECISION array, dimension
       *                      (min(MMAX,NMAX))
       *
       *  WORK    (workspace) DOUBLE PRECISION array,
       *                      dimension (MMAX*NMAX + 4*NMAX + MMAX).
       *
       *  IWORK   (workspace) INTEGER array, dimension (15*NMAX)
       */
    private void ddrvls(boolean[] dotype, int nm, int[] mval, int nn, int[]nval,
                        int nns, int[] nsval, int nnb, int[] nbval, int[] nxval,
                        double thresh, boolean tsterr, double[][] A, double[][] COPYA,
                        double[][] B, double[][] COPYB, double[][] C, double[] s,
                        double[] copys, double[] work, int[] iwork) {
        // Only use 4 of the 18 tests, since only 4 apply to dgelss
        int ntests = 4;
        int smlsiz = 25;
        int iseedy[] = new int[]{1988, 1989, 1990, 1991};
        String path;
        int crank[] = new int[1];
        int i;
        int im;
        int in;
        int inb;
        int info[] = new int[1];
        int ins;
        int irank;
        int iscale;
        int itran;
        int itype;
        int j;
        int k;
        int lda;
        int ldb;
        int ldwork;
        int lwork;
        int m;
        int mnmin;
        int n;
        int nb;
        int nerrs;
        int nfail;
        int nlvl;
        int nrhs;
        int nrun;
        int rank[] = new int[1];
        double eps;
        double norma[] = new double[1];
        double normb[] = new double[1];
        double rcond;
        int iseed[] = new int[4];
        double result[] = new double[ntests];
        double das;
        double dacopys;
        int p;
        double work2[][];
        int q;
        double vec1[];
        double resid[] = new double[1];
        
        // Initialize constants and the random number seed.
        path = new String("DLS"); // Double precision
        nrun = 0;
        nfail = 0;
        nerrs = 0;
        for (i = 0; i < 4; i++) {
            iseed[i] = iseedy[i];
        }
        eps = ge.dlamch('E'); // Epsilon
        
        // Threshold for rank estimation
        rcond = Math.sqrt(eps) - (Math.sqrt(eps) - eps)/2;
        
        // Test the error exits
        xlaenv(2, 2);
        xlaenv(9, smlsiz);
        if (tsterr) {
            derrls();
        }
        
        // Output the header if (NM == 0 || NN == 0) && THRESH === 0.
        if (((nm == 0) || (nn == 0)) && (thresh == 0.0)) {
            Preferences.debug("Least squares driver routine dgelss\n");
            Preferences.debug("Matrix types:\n");
            Preferences.debug("1: Full rank normal scaling\n");
            Preferences.debug("2: Full rank scaled near overflow\n");
            Preferences.debug("3: Full rank scaled near underflow\n");
            Preferences.debug("4: Rank deficient normal scaling\n");
            Preferences.debug("5: Rank deficient scaled near overflow\n");
            Preferences.debug("6: Rank deficient scaled near underflow\n");
            // Do 11-14 of 18 test ratios
            Preferences.debug("Test ratios:\n");
            Preferences.debug("11-14: DGELSS\n");
            Preferences.debug("11-14 same as 3-6\n");
            Preferences.debug("3: norm(svd(A) - svd(R))/(min(m,n) * norm(svd(R)) * eps)\n");
            Preferences.debug("4: norm(B - A * X)/(max(m,n) * norm(A) * norm(X) * eps)\n");
            Preferences.debug("5: norm((A*X-B)' * A)/(max(m,n,nrhs) * norm(A) * norm(B) * eps)\n");
            Preferences.debug("6: Check if X is in the row space of A or A'\n");
        } // if (((nm == 0) || (nn == 0)) && (thresh == 0.0))
        infot = 0;
        xlaenv(2, 2);
        xlaenv(9, smlsiz);
        
        for (im = 1; im <= nm; im++) {
            m = mval[im-1];
            lda = Math.max(1, m);
            
            for (in = 1; in <= nn; in++) {
                n = nval[in-1];
                mnmin = Math.min(m, n);
                ldb = Math.max(1, Math.max(m, n));
                
                for (ins = 1; ins <= nns; ins++) {
                    nrhs = nsval[ins-1];
                    nlvl = Math.max((int)(Math.log(Math.max(1.0, (double)(mnmin))/
                           (double)(smlsiz+1))/Math.log(2.0)) + 1, 0);
                    lwork = Math.max(1, (m+nrhs)*(n+2));
                    lwork = Math.max(lwork, (n+nrhs)*(m+2));
                    lwork = Math.max(lwork, m*n+4*mnmin + Math.max(m, n));
                    lwork = Math.max(lwork, 12*mnmin+2*mnmin*smlsiz+
                            8*mnmin*nlvl+mnmin*nrhs+(smlsiz+1)*(smlsiz+1));
                    
                    for (irank = 1; irank <= 2; irank++) {
                        for (iscale = 1; iscale <= 3; iscale++) {
                            itype = (irank-1)*3 + iscale;
                            if (!dotype[itype-1]) {
                                continue;
                            }
                            
                            // Generate a matrix of scaling type iscale and rank type irank.
                            dqrt15(iscale, irank, m, n, nrhs, COPYA, lda, COPYB, ldb, copys,
                                   rank, norma, normb, iseed, work, lwork);
                            
                            // workspace used: max(m+min(m,n),nrhs*min(m,n),2*n+m)
                            // Initialize vector iwork
                            for (j = 0; j < n; j++) {
                                iwork[j] = 0;
                            }
                            ldwork = Math.max(1, m);
                            
                            // Loop for testing different block sizes.
                            for (inb = 1; inb <= nnb; inb++) {
                                nb = nbval[inb-1];
                                xlaenv(1, nb);
                                xlaenv(3, nxval[inb-1]);
                                
                                // Test dgelss
                                // dgelss: Compute the minimum-norm solution X
                                // to min(norm(A * X - B)) using the SVD.
                                ge.dlacpy('F', m, n, COPYA, lda, A, lda);
                                ge.dlacpy('F', m, nrhs, COPYB, ldb, B, ldb);
                                srnamt = new String("DGELSS");
                                
                                dgelss(m, n, nrhs, A, lda, B, ldb, s, rcond, crank,
                                       work, lwork, info);
                                
                                if (info[0] != 0) {
                                    if ((nfail == 0) && (nerrs == 0)) {
                                        Preferences.debug("Least squares driver routine dgelss\n");
                                        Preferences.debug("Matrix types:\n");
                                        Preferences.debug("1: Full rank normal scaling\n");
                                        Preferences.debug("2: Full rank scaled near overflow\n");
                                        Preferences.debug("3: Full rank scaled near underflow\n");
                                        Preferences.debug("4: Rank deficient normal scaling\n");
                                        Preferences.debug("5: Rank deficient scaled near overflow\n");
                                        Preferences.debug("6: Rank deficient scaled near underflow\n");
                                        // Do 11-14 of 18 test ratios
                                        Preferences.debug("Test ratios:\n");
                                        Preferences.debug("11-14: DGELSS\n");
                                        Preferences.debug("11-14 same as 3-6\n");
                                        Preferences.debug("3: norm(svd(A) - svd(R))/(min(m,n) * norm(svd(R)) * eps)\n");
                                        Preferences.debug("4: norm(B - A * X)/(max(m,n) * norm(A) * norm(X) * eps)\n");
                                        Preferences.debug("5: norm((A*X-B)' * A)/(max(m,n,nrhs) * norm(A) * norm(B) * eps)\n");
                                        Preferences.debug("6: Check if X is in the row space of A or A'\n");    
                                    } // if ((nfail == 0) && (nerrs == 0))
                                    nerrs++;
                                    Preferences.debug("dgelss returned with info[0] = " + info[0] + "\n");
                                    Preferences.debug("m = " + m + " n = " + n + " nrhs = " + nrhs + "\n");
                                    Preferences.debug("nb = " + nb + " itype = " + itype + "\n");
                                } // if (info[0] != 0)
                                
                                // workspace used: 3*min(m,n) + max(2*min(mn), nrhs, max(m,n))
                                
                                // Test 11: Compute relative error in svd
                                if (rank[0] > 0) {
                                    daxpy(mnmin, -1.0, copys, 1, s, 1);
                                    das = 0.0;
                                    dacopys = 0.0;
                                    for (p = 0; p < mnmin; p++) {
                                        das += Math.abs(s[p]);
                                        dacopys += Math.abs(copys[p]);
                                    }
                                    result[0] = das/dacopys/(eps * mnmin);
                                } // if (rank[0] > 0)
                                else {
                                    result[0] = 0.0;
                                }
                                
                                // Test 12; Compute error in solution
                                work2 = new double[ldwork][nrhs];
                                ge.dlacpy('F', m, nrhs, COPYB, ldb, work2, ldwork);
                                vec1 = new double[m];
                                dqrt16('N', m, n, nrhs, COPYA, lda, B, ldb, work2, ldwork,
                                        vec1, resid);
                                for (q = 0; q < nrhs; q++) {
                                    for (p = 0; p < ldwork; p++) {
                                        work[p + q * ldwork] = work2[p][q];
                                    }
                                }
                                result[1] = resid[0];
                                
                                // Test 13: Check norm of r'*A
                                result[2] = 0.0;
                                if (m > crank[0]) {
                                    result[2] = dqrt17('N', 1, m, n, nrhs, COPYA, lda, B, ldb,
                                                       COPYB, ldb, C); 
                                } // if (m > crank[0])
                                
                                // Test 14: Check if x is in the rowspace of A
                                result[3] = 0.0;
                                if (n > crank[0]) {
                                    result[3] = dqrt14('N', m, n, nrhs, COPYA, lda, B, ldb, 
                                                       work, lwork);
                                } // if (n > crank[0])
                                
                                for (k = 0; k < ntests; k++) {
                                    if (result[k] >= thresh) {
                                        if ((nfail == 0) && (nerrs == 0)) {
                                            Preferences.debug("Least squares driver routine dgelss\n");
                                            Preferences.debug("Matrix types:\n");
                                            Preferences.debug("1: Full rank normal scaling\n");
                                            Preferences.debug("2: Full rank scaled near overflow\n");
                                            Preferences.debug("3: Full rank scaled near underflow\n");
                                            Preferences.debug("4: Rank deficient normal scaling\n");
                                            Preferences.debug("5: Rank deficient scaled near overflow\n");
                                            Preferences.debug("6: Rank deficient scaled near underflow\n");
                                            // Do 11-14 of 18 test ratios
                                            Preferences.debug("Test ratios:\n");
                                            Preferences.debug("11-14: DGELSS\n");
                                            Preferences.debug("11-14 same as 3-6\n");
                                            Preferences.debug("3: norm(svd(A) - svd(R))/(min(m,n) * norm(svd(R)) * eps)\n");
                                            Preferences.debug("4: norm(B - A * X)/(max(m,n) * norm(A) * norm(X) * eps)\n");
                                            Preferences.debug("5: norm((A*X-B)' * A)/(max(m,n,nrhs) * norm(A) * norm(B) * eps)\n");
                                            Preferences.debug("6: Check if X is in the row space of A or A'\n");    
                                        } // if ((nfail == 0) && (nerrs == 0))
                                        Preferences.debug("m = " + m + " n = " + n + " nrhs = " + nrhs + "\n");
                                        Preferences.debug("nb = " + nb + " itype = " + itype + "\n");
                                        Preferences.debug("k = " + k + " result[" + k + "] = " + result[k] + "\n");
                                        nfail++;
                                    } // if (result[k] >= thresh)
                                } // for (k = 0; k < ntests; k++)
                                nrun = nrun + 4;
                            } // for (inb = 1; inb <= nnb; inb++)
                        } // for (iscale = 1; iscale <= 3; iscale++)
                    } // for (irank = 1; irank <= 2; irank++)
                } // for (ins = 1; ins <= nns; ins++)
            } // for (in = 1; in <= nn; in++)
        } // for (im = 1; im <= nm; im++)
        
        if (nfail > 0) {
            Preferences.debug("In ddrvls " + nfail + " out of " + nrun + " tests failed to pass the threshold\n");
        }
        else {
            Preferences.debug("In ddrvls all " + nrun + " tests passed the threshold\n");
        }
    } // ddrvls
    
    /** This is a port of that portion of version 3.1 LAPACK test routine DERRLS used to test
     * the error exits of the least squares driver routine DGELSS.
     */
    private void derrls() {
        int nmax = 2;
        int i;
        int info[] = new int[1];
        int j;
        double A[][] = new double[nmax][nmax];
        double B[][] = new double[nmax][nmax];
        double s[] = new double[nmax];
        double w[] = new double[nmax];
        int irnk[] = new int[1];
        double rcond = 0.0;
        int npass = 5;
        int ntotal = 5;
        
        A[0][0] = 1.0;
        A[0][1] = 2.0;
        A[1][1] = 3.0;
        A[1][0] = 4.0;
        
        // DGELSS
        dgelss(-1, 0, 0, A, 1, B, 1, s, rcond, irnk, w, 1, info);
        if (info[0] != -1) {
            Preferences.debug("dgelss(-1, 0, 0, A, 1, B, 1, s, rcond, irnk, w, 1, info) produced info[0] = " + info[0] +
                              " instead of -1\n");
            npass--;
        }
        
        dgelss(0, -1, 0, A, 1, B, 1, s, rcond, irnk, w, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dgelss(0, -1, 0, A, 1, B, 1, s, rcond, irnk, w, 1, info) produced info[0] = " + info[0] +
                              " instead of -2\n");
            npass--;
        }
        
        dgelss(0, 0, -1, A, 1, B, 1, s, rcond, irnk, w, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dgelss(0, 0, -1, A, 1, B, 1, s, rcond, irnk, w, 1, info) produced info[0] = " + info[0] +
                              " instead of -3\n");
            npass--;
        }
        
        dgelss(2, 0, 0, A, 1, B, 2, s, rcond, irnk, w, 2, info);
        if (info[0] != -5) {
            Preferences.debug("dgelss(2, 0, 0, A, 1, B, 2, s, rcond, irnk, w, 2, info) produced info[0] = " + info[0] +
                              " instead of -5\n");
            npass--;
        }
        
        dgelss(2, 0, 0, A, 2, B, 1, s, rcond, irnk, w, 2, info);
        if (info[0] != -7) {
            Preferences.debug("dgelss(2, 0, 0, A, 2, B, 1, s, rcond, irnk, w, 2, info) produced info[0] = " + info[0] +
                              " instead of -7\n");
            npass--;
        }
        
        Preferences.debug("derrls correctly found " + npass + " of " + ntotal + " error exits\n");
        return;
    } // derrls
    
    /** This is a port of version 3.1 LAPACK test routine DQRt15.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             INTEGER            LDA, LDB, LWORK, M, N, NRHS, RANK, RKSEL, SCALE
             DOUBLE PRECISION   NORMA, NORMB
       *     ..
       *     .. Array Arguments ..
             INTEGER            ISEED( 4 )
             DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( LWORK )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DQRT15 generates a matrix with full or deficient rank and of various
       *  norms.
       *
       *  Arguments
       *  =========
       *
       *  SCALE   (input) INTEGER
       *          SCALE = 1: normally scaled matrix
       *          SCALE = 2: matrix scaled up
       *          SCALE = 3: matrix scaled down
       *
       *  RKSEL   (input) INTEGER
       *          RKSEL = 1: full rank matrix
       *          RKSEL = 2: rank-deficient matrix
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix A.
       *
       *  N       (input) INTEGER
       *          The number of columns of A.
       *
       *  NRHS    (input) INTEGER
       *          The number of columns of B.
       *
       *  A       (output) DOUBLE PRECISION array, dimension (LDA,N)
       *          The M-by-N matrix A.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.
       *
       *  B       (output) DOUBLE PRECISION array, dimension (LDB, NRHS)
       *          A matrix that is in the range space of matrix A.
       *
       *  LDB     (input) INTEGER
       *          The leading dimension of the array B.
       *
       *  S       (output) DOUBLE PRECISION array, dimension MIN(M,N)
       *          Singular values of A.
       *
       *  RANK    (output) INTEGER
       *          number of nonzero singular values of A.
       *
       *  NORMA   (output) DOUBLE PRECISION
       *          one-norm of A.
       *
       *  NORMB   (output) DOUBLE PRECISION
       *          one-norm of B.
       *
       *  ISEED   (input/output) integer array, dimension (4)
       *          seed for random number generator.
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
       *
       *  LWORK   (input) INTEGER
       *          length of work space required.
       *          LWORK >= MAX(M+MIN(M,N),NRHS*MIN(M,N),2*N+M)
       */
    private void dqrt15(int scale, int rksel, int m, int n, int nrhs, double[][] A,
                        int lda, double[][] B, int ldb, double[] s, int[]rank,
                        double[] norma, double[] normb, int[] iseed, double[] work,
                        int lwork) {
        double svmin = 0.1;
        int info[] = new int[1];
        int j;
        int mn;
        double bignum;
        double eps;
        double smlnum;
        double temp;
        double dummy[] = new double[1];
        double work2[];
        double array1[][];
        int row1;
        int p;
        int q;
        
        mn = Math.min(m, n);
        if (lwork < Math.max(m+mn, Math.max(mn*nrhs, 2*n+m))) {
            MipavUtil.displayError("dqrt15 had lwork too small");
            return;
        }
        
        smlnum = ge.dlamch('S'); // Safe minimum
        bignum = 1.0/smlnum;
        eps = ge.dlamch('E'); // Epsilon
        smlnum = (smlnum/eps)/eps;
        bignum = 1.0/smlnum;
        
        // Determine rank and (unscaled) singular values
        if (rksel == 1) {
            rank[0] = mn;
        }
        else if (rksel == 2) {
            rank[0] = (3*mn)/4;
            for (j = rank[0]+1; j <= mn; j++) {
                s[j-1] = 0.0;
            }
        } // else if (rksel == 2)
        else {
            MipavUtil.displayError("dqrt15 had rksel = " + rksel);
            return;
        }
        
        if (rank[0] > 0) {
            // Nontrivial case
            s[0] = 1.0;
            for (j = 2; j <= rank[0];) {
                temp = dlarnd(1, iseed);
                if (temp > svmin) {
                    s[j-1] = Math.abs(temp);
                    j++;
                }
            } // for (j = 2; j <= rank[0];)
            dlaord('D', rank[0], s, 1);
            
            // Generate 'rank' columns of a random orthogonal matrix in A
            dlarnv(2, iseed, m, work);
            ge.dscal(m, 1.0/ge.dnrm2(m, work, 1), work, 1);
            ge.dlaset('F', m, rank[0], 0.0, 1.0, A, lda);
            work2 = new double[rank[0]];
            ge.dlarf('L', m, rank[0], work, 1, 2.0, A, lda, work2);
            
            // workspace used: m+mn
            // Generate consistent rhs in the range space of A
            dlarnv(2, iseed, rank[0]*nrhs, work);
            array1 = new double[rank[0]][nrhs];
            for (q = 0; q < nrhs; q++) {
                for (p = 0; p < rank[0]; p++) {
                    array1[p][q] = work[p + q*rank[0]];
                }
            }
            ge.dgemm('N', 'N', m, nrhs, rank[0], 1.0, A, lda, array1, rank[0], 0.0, B, ldb);
            
            // work space used <= mn * nrhs
            // generate (unscaled) matrix A
            for (j = 1; j <= rank[0]; j++) {
                for (p = 0; p < m; p++) {
                    A[p][j-1] = s[j-1] * A[p][j-1];
                }
            } // for (j = 1; j <= rank[0]; j++)
            if (rank[0] < n) {
                row1 = Math.max(1, m);
                array1 = new double[row1][n-rank[0]];
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n-rank[0]; q++) {
                        array1[p][q] = A[p][rank[0]+q];
                    }
                }
                ge.dlaset('F', m, n-rank[0], 0.0, 0.0, array1, row1);
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n-rank[0]; q++) {
                        A[p][rank[0]+q] = array1[p][q];
                    }
                }
            } // if (rank[0] < n)
            dlaror('R', 'N', m, n, A, lda, iseed, work, info);
        } // if (rank[0] > 0)
        else { // rank[0] == 0
            // work space used 2*n+m
            // Generate null matrix and rhs
            for (j = 0; j < mn; j++) {
                s[j] = 0.0;
            }
            ge.dlaset('F', m, n, 0.0, 0.0, A, lda);
            ge.dlaset('F', m, nrhs, 0.0, 0.0, B, ldb);
        } // else rank[0] == 0
        
        // Scale the matrix
        if (scale != 1) {
            norma[0] = ge.dlange('M', m, n, A, lda, dummy);
            if (norma[0] != 0.0) {
                if (scale == 2) {
                    // matrix scaled up
                    ge.dlascl('G', 0, 0, norma[0], bignum, m, n, A, lda, info);
                    array1 = new double[mn][1];
                    for (p = 0; p < mn; p++) {
                        array1[p][0] = s[p];
                    }
                    ge.dlascl('G', 0, 0, norma[0], bignum, mn, 1, array1, mn, info);
                    for (p = 0; p < mn; p++) {
                        s[p] = array1[p][0];
                    }
                    ge.dlascl('G', 0, 0, norma[0], bignum, m, nrhs, B, ldb, info);
                } // if (scale == 2)
                else if (scale == 3) {
                    // matrix scaled down
                    ge.dlascl('G', 0, 0, norma[0], smlnum, m, n, A, lda, info);
                    array1 = new double[mn][1];
                    for (p = 0; p < mn; p++) {
                        array1[p][0] = s[p];
                    }
                    ge.dlascl('G', 0, 0, norma[0], smlnum, mn, 1, array1, mn, info);
                    for (p = 0; p < mn; p++) {
                        s[p] = array1[p][0];
                    }
                    ge.dlascl('G', 0, 0, norma[0], smlnum, m, nrhs, B, ldb, info);
                } // else if (scale == 3)
                else {
                    MipavUtil.displayError("dqrt15 had info[0] = 1");
                    return;
                }
            } // if (norma[0] != 0.0)
        } // if (scale != 1)
        
        norma[0] = 0;
        for (p = 0; p < mn; p++) {
            norma[0] += Math.abs(s[p]);
        }
        normb[0] = ge.dlange('1', m, nrhs, B, ldb, dummy);
        
        return;
    } // dqrt15
    
    /** This is a port of version 3.1 LAPACK auxiliary routine DLAORD.
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     November 2006
    *
    *     .. Scalar Arguments ..
          CHARACTER          JOB
          INTEGER            INCX, N
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   X( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DLAORD sorts the elements of a vector x in increasing or decreasing
    *  order.
    *
    *  Arguments
    *  =========
    *
    *  JOB     (input) CHARACTER
    *          = 'I':  Sort in increasing order
    *          = 'D':  Sort in decreasing order
    *
    *  N       (input) INTEGER
    *          The length of the vector X.
    *
    *  X       (input/output) DOUBLE PRECISION array, dimension
    *                         (1+(N-1)*INCX)
    *          On entry, the vector of length n to be sorted.
    *          On exit, the vector x is sorted in the prescribed order.
    *
    *  INCX    (input) INTEGER
    *          The spacing between successive elements of X.  INCX >= 0.
    */
    private void dlaord(char job, int n, double[] x, int incx) {
        int i;
        int inc;
        int ix;
        int ixnext;
        double temp;
        
        inc = Math.abs(incx);
        if ((job == 'I') || (job == 'i')) {
            // Sort in increasing order
            loop1:
            for (i = 2; i <= n; i++) {
                ix = 1 + (i-1)*inc;
                do {
                    if (ix == 1) {
                        continue loop1;
                    }
                    ixnext = ix - inc;
                    if (x[ix-1] > x[ixnext-1]) {
                        continue loop1;
                    }
                    else {
                        temp = x[ix-1];
                        x[ix-1] = x[ixnext-1];
                        x[ixnext-1] = temp;
                    }
                    ix = ixnext;
                } while (true);
            } // for (i = 2; i <= n; i++)
        } // if ((job == 'I) || (job == 'i))
        else if ((job == 'D') || (job == 'd')) {
            // Sort inn decreasing order
            loop2:
            for (i = 2; i <= n; i++) {
                ix = 1 + (i-1)*inc;
                do {
                    if (ix == 1) {
                        continue loop2;
                    }
                    ixnext = ix - inc;
                    if (x[ix-1] < x[ixnext-1]) {
                        continue loop2;
                    }
                    else {
                        temp = x[ix-1];
                        x[ix-1] = x[ixnext-1];
                        x[ixnext-1] = temp;
                    }
                    ix = ixnext;
                } while (true);    
            } // for (i = 2; i <= n; i++)
        } // else if ((job == 'D') || (job == 'd'))
        return;
    } // dlaord

    /** This is a port of version 3.1 LAPACK auxiliary test routine DLAROR.
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     November 2006
    *
    *     .. Scalar Arguments ..
          CHARACTER          INIT, SIDE
          INTEGER            INFO, LDA, M, N
    *     ..
    *     .. Array Arguments ..
          INTEGER            ISEED( 4 )
          DOUBLE PRECISION   A( LDA, * ), X( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DLAROR pre- or post-multiplies an M by N matrix A by a random
    *  orthogonal matrix U, overwriting A.  A may optionally be initialized
    *  to the identity matrix before multiplying by U.  U is generated using
    *  the method of G.W. Stewart (SIAM J. Numer. Anal. 17, 1980, 403-409).
    *
    *  Arguments
    *  =========
    *
    *  SIDE    (input) CHARACTER*1
    *          Specifies whether A is multiplied on the left or right by U.
    *          = 'L':         Multiply A on the left (premultiply) by U
    *          = 'R':         Multiply A on the right (postmultiply) by U'
    *          = 'C' or 'T':  Multiply A on the left by U and the right
    *                          by U' (Here, U' means U-transpose.)
    *
    *  INIT    (input) CHARACTER*1
    *          Specifies whether or not A should be initialized to the
    *          identity matrix.
    *          = 'I':  Initialize A to (a section of) the identity matrix
    *                   before applying U.
    *          = 'N':  No initialization.  Apply U to the input matrix A.
    *
    *          INIT = 'I' may be used to generate square or rectangular
    *          orthogonal matrices:
    *
    *          For M = N and SIDE = 'L' or 'R', the rows will be orthogonal
    *          to each other, as will the columns.
    *
    *          If M < N, SIDE = 'R' produces a dense matrix whose rows are
    *          orthogonal and whose columns are not, while SIDE = 'L'
    *          produces a matrix whose rows are orthogonal, and whose first
    *          M columns are orthogonal, and whose remaining columns are
    *          zero.
    *
    *          If M > N, SIDE = 'L' produces a dense matrix whose columns
    *          are orthogonal and whose rows are not, while SIDE = 'R'
    *          produces a matrix whose columns are orthogonal, and whose
    *          first M rows are orthogonal, and whose remaining rows are
    *          zero.
    *
    *  M       (input) INTEGER
    *          The number of rows of A.
    *
    *  N       (input) INTEGER
    *          The number of columns of A.
    *
    *  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
    *          On entry, the array A.
    *          On exit, overwritten by U A ( if SIDE = 'L' ),
    *           or by A U ( if SIDE = 'R' ),
    *           or by U A U' ( if SIDE = 'C' or 'T').
    *
    *  LDA     (input) INTEGER
    *          The leading dimension of the array A.  LDA >= max(1,M).
    *
    *  ISEED   (input/output) INTEGER array, dimension (4)
    *          On entry ISEED specifies the seed of the random number
    *          generator. The array elements should be between 0 and 4095;
    *          if not they will be reduced mod 4096.  Also, ISEED(4) must
    *          be odd.  The random number generator uses a linear
    *          congruential sequence limited to small integers, and so
    *          should produce machine independent random numbers. The
    *          values of ISEED are changed on exit, and can be used in the
    *          next call to DLAROR to continue the same random number
    *          sequence.
    *
    *  X       (workspace) DOUBLE PRECISION array, dimension (3*MAX( M, N ))
    *          Workspace of length
    *              2*M + N if SIDE = 'L',
    *              2*N + M if SIDE = 'R',
    *              3*N     if SIDE = 'C' or 'T'.
    *
    *  INFO    (output) INTEGER
    *          An error flag.  It is set to:
    *          = 0:  normal return
    *          < 0:  if INFO = -k, the k-th argument had an illegal value
    *          = 1:  if the random numbers generated by DLARND are bad.
    */
    private void dlaror(char side, char init, int m, int n, double[][] A, int lda,
                        int[] iseed, double[] x, int[] info) {
        double toosml = 1.0E-20;
        int irow;
        int itype;
        int ixfrm;
        int j;
        int jcol;
        int kbeg;
        int nxfrm;
        double factor;
        double xnorm;
        double xnorms;
        double vec1[];
        int p;
        int q;
        double array1[][];
        int row1;
        double vec2[];
        
        if ((m == 0) || (n == 0)) {
            return;
        }
        
        itype = 0;
        if ((side == 'L') || (side == 'l')) {
            itype = 1;
        }
        else if ((side == 'R') || (side == 'r')) {
            itype = 2;
        }
        else if ((side == 'C') || (side == 'c') || (side == 'T') || (side == 't')) {
            itype = 3;
        }
        
        // Check for argument errors
        info[0] = 0;
        if (itype == 0) {
            info[0] = -1;
        }
        else if (m < 0) {
            info[0] = -3;
        }
        else if ((n < 0) || ((itype == 3) && (m != n))) {
            info[0] = -4;
        }
        else if (lda < m) {
            info[0] = -6;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("Error dlaror had info[0] = " + info[0]);
            return;
        }
        
        if (itype == 1) {
            nxfrm = m;
        }
        else {
            nxfrm = n;
        }
        
        // Initialize A to the identity matrix if desired
        if ((init == 'I') || (init == 'i')) {
            ge.dlaset('F', m, n, 0.0, 1.0, A, lda);
        }
        
        // If no rotation possible, multiply by random +/-1
        // Compute rotation by computing Householder transformations
        // H(2), H(3), ..., H(nhouse)
        
        for (j = 0; j < nxfrm; j++) {
            x[j] = 0.0;
        }
        
        for (ixfrm = 2; ixfrm <= nxfrm; ixfrm++) {
            kbeg = nxfrm - ixfrm + 1;
            
            // Generate independent normal(0, 1) random numbers
            for (j = kbeg; j <= nxfrm; j++) {
                x[j-1] = dlarnd(3, iseed);
            }
            
            // Generate a Householder transformation from the random vector x
            vec1 = new double[ixfrm];
            for (p = 0; p < ixfrm; p++) {
                vec1[p] = x[kbeg-1+p];
            }
            xnorm = ge.dnrm2(ixfrm, vec1, 1);
            if (x[kbeg-1] >= 0.0) {
                xnorms = Math.abs(xnorm);
            }
            else {
                xnorms = -Math.abs(xnorm);
            }
            if (-x[kbeg-1] >= 0.0) {
                x[kbeg+nxfrm-1] = 1.0;
            }
            else {
                x[kbeg+nxfrm-1] = -1.0;
            }
            factor = xnorms * (xnorms + x[kbeg-1]);
            if (Math.abs(factor) < toosml) {
                info[0] = 1;
                MipavUtil.displayError("Error dlaror had info[0] = " + info[0]);
                return;
            }
            else {
                factor = 1.0/factor;
            }
            x[kbeg-1] = x[kbeg-1] + xnorms;
            
            //Apply Householder transformation to A
            if ((itype == 1) || (itype == 3)) {
                // Apply H(k) from the left.
                row1 = Math.max(1, ixfrm);
                array1 = new double[row1][n];
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n; q++) {
                        array1[p][q] = A[kbeg-1+p][q];
                    }
                }
                vec1 = new double[ixfrm];
                for (p = 0; p < ixfrm; p++) {
                    vec1[p] = x[kbeg-1+p];
                }
                vec2 = new double[n];
                for (p = 0; p < n; p++) {
                    vec2[p] = x[2*nxfrm+p];
                }
                ge.dgemv('T', ixfrm, n, 1.0, array1, row1, vec1, 1, 0.0, vec2, 1);
                for (p = 0; p < n; p++) {
                    x[2*nxfrm+p] = vec2[p];
                }
                for (p = 0; p < ixfrm; p++) {
                    vec1[p] = x[kbeg-1+p];
                }
                ge.dger(ixfrm, n, -factor, vec1, 1, vec2, 1, array1, row1);
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < n; q++) {
                        A[kbeg-1+p][q] = array1[p][q];
                    }
                }
            } // if ((itype == 1) || (itype == 3))
            
            if ((itype == 2) || (itype == 3)) {
                // Apply H(k) from the right
                row1 = Math.max(1, m);
                array1 = new double[row1][ixfrm];
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < ixfrm; q++) {
                        array1[p][q] = A[p][kbeg-1+q];
                    }
                }
                vec1 = new double[ixfrm];
                for (p = 0; p < ixfrm; p++) {
                    vec1[p] = x[kbeg-1+p];
                }
                vec2 = new double[m];
                for (p = 0; p < m; p++) {
                    vec2[p] = x[2*nxfrm+p];
                }
                ge.dgemv('N', m, ixfrm, 1.0, array1, row1, vec1, 1, 0.0, vec2, 1);
                for (p = 0; p < m; p++) {
                    x[2*nxfrm+p] = vec2[p];
                }
                for (p = 0; p < ixfrm; p++) {
                    vec1[p] = x[kbeg-1+p];
                }
                ge.dger(m, ixfrm, -factor, vec2, 1, vec1, 1, array1, row1);
                for (p = 0; p < row1; p++) {
                    for (q = 0; q < ixfrm; q++) {
                        A[p][kbeg-1+q] = array1[p][q];
                    }
                }
            } // if ((itype == 2) || (itype == 3))
        } // for (ixfrm = 2; ixfrm <= nxfrm; ixfrm++)
        
        if (dlarnd(3, iseed) >= 0.0) {
            x[2*nxfrm-1] = 1.0;
        }
        else {
            x[2*nxfrm-1] = -1.0;
        }
        
        // Scale the matrix A by D.
        if ((itype == 1) || (itype == 3)) {
            for (irow = 1; irow <= m; irow++) {
                for (p = 0; p < n; p++) {
                    A[irow-1][p] = x[nxfrm+irow-1] * A[irow-1][p];
                }
            } // for (irow = 1; irow <= m; irow++)
        } // if ((itype == 1) || (itype == 3))
        
        if ((itype == 2) || (itype == 3)) {
            for (jcol = 1; jcol <= n; jcol++) {
                for (p = 0; p < m; p++) {
                    A[p][jcol-1] = x[nxfrm+jcol-1] * A[p][jcol-1];
                }
            } // for (jcol = 1; jcol <= n; jcol++)
        } // if ((itype == 2) || (itype == 3))
        return;
    } // dlaror
    
    /** This is a port of version 3.1 LAPACK test routine DQRT16.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             CHARACTER          TRANS
             INTEGER            LDA, LDB, LDX, M, N, NRHS
             DOUBLE PRECISION   RESID
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), RWORK( * ),
            $                   X( LDX, * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DQRT16 computes the residual for a solution of a system of linear
       *  equations  A*x = b  or  A'*x = b:
       *     RESID = norm(B - A*X) / ( max(m,n) * norm(A) * norm(X) * EPS ),
       *  where EPS is the machine epsilon.
       *
       *  Arguments
       *  =========
       *
       *  TRANS   (input) CHARACTER*1
       *          Specifies the form of the system of equations:
       *          = 'N':  A *x = b
       *          = 'T':  A'*x = b, where A' is the transpose of A
       *          = 'C':  A'*x = b, where A' is the transpose of A
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix A.  M >= 0.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix A.  N >= 0.
       *
       *  NRHS    (input) INTEGER
       *          The number of columns of B, the matrix of right hand sides.
       *          NRHS >= 0.
       *
       *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          The original M x N matrix A.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.  LDA >= max(1,M).
       *
       *  X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)
       *          The computed solution vectors for the system of linear
       *          equations.
       *
       *  LDX     (input) INTEGER
       *          The leading dimension of the array X.  If TRANS = 'N',
       *          LDX >= max(1,N); if TRANS = 'T' or 'C', LDX >= max(1,M).
       *
       *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
       *          On entry, the right hand side vectors for the system of
       *          linear equations.
       *          On exit, B is overwritten with the difference B - A*X.
       *
       *  LDB     (input) INTEGER
       *          The leading dimension of the array B.  IF TRANS = 'N',
       *          LDB >= max(1,M); if TRANS = 'T' or 'C', LDB >= max(1,N).
       *
       *  RWORK   (workspace) DOUBLE PRECISION array, dimension (M)
       *
       *  RESID   (output) DOUBLE PRECISION
       *          The maximum over the number of right hand sides of
       *          norm(B - A*X) / ( max(m,n) * norm(A) * norm(X) * EPS ).
       */
    private void dqrt16(char trans, int m, int n, int nrhs, double[][] A, int lda,
                        double[][] X, int ldx, double[][] B, int ldb, double[] rwork,
                        double[] resid) {
        int j;
        int n1;
        int n2;
        double anorm;
        double bnorm;
        double eps;
        double xnorm;
        int p;
        
        // Quick exit if m == 0 or n == 0 or nrhs == 0
        if ((m <= 0) || (n <= 0) || (nrhs == 0)) {
            resid[0] = 0;
            return;
        }
        
        if ((trans == 'T') || (trans == 't') || (trans == 'C') || (trans == 'C')) {
            anorm = ge.dlange('I', m, n, A, lda, rwork);
            n1 = n;
            n2 = m;
        }
        else {
            anorm = ge.dlange('1', m, n, A, lda, rwork);
            n1 = m;
            n2 = n;
        }
        
        eps = ge.dlamch('E'); // epsilon
        
        // Compute B -A*X (or B - A'*X) and store in B.
        ge.dgemm(trans, 'N', n1, nrhs, n2, -1.0, A, lda, X, ldx, 1.0, B, ldb);
        
        // Compute the maximum over the number of right hand sides of
        // norm(B - A*X)/(max(m,n) * norm(A) * norm(X) * eps).
        
        resid[0] = 0.0;
        for (j = 1; j <= nrhs; j++) {
            bnorm = 0.0;
            for (p = 0; p < n1; p++) {
                bnorm += Math.abs(B[p][j-1]);
            }
            xnorm = 0.0;
            for (p = 0; p < n2; p++) {
                xnorm += Math.abs(X[p][j-1]);
            }
            if ((anorm == 0.0) && (bnorm == 0.0)) {
                resid[0] = 0.0;
            }
            else if ((anorm <= 0.0) || (xnorm <= 0.0)) {
                resid[0] = 1.0/eps;
            }
            else {
                resid[0] = Math.max(resid[0], ((bnorm/anorm)/xnorm)/
                             (Math.max(m, n) * eps));
            }
        } // for (j = 1; j <= nrhs; j++)
        return;
    } // dqrt16
    
    /** This is a port of version 3.1 LAPACK test routine DQRT17.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             CHARACTER          TRANS
             INTEGER            IRESID, LDA, LDB, LDX, LWORK, M, N, NRHS
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDB, * ),
            $                   WORK( LWORK ), X( LDX, * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DQRT17 computes the ratio
       *
       *     || R'*op(A) ||/(||A||*alpha*max(M,N,NRHS)*eps)
       *
       *  where R = op(A)*X - B, op(A) is A or A', and
       *
       *     alpha = ||B|| if IRESID = 1 (zero-residual problem)
       *     alpha = ||R|| if IRESID = 2 (otherwise).
       *
       *  Arguments
       *  =========
       *
       *  TRANS   (input) CHARACTER*1
       *          Specifies whether or not the transpose of A is used.
       *          = 'N':  No transpose, op(A) = A.
       *          = 'T':  Transpose, op(A) = A'.
       *
       *  IRESID  (input) INTEGER
       *          IRESID = 1 indicates zero-residual problem.
       *          IRESID = 2 indicates non-zero residual.
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix A.
       *          If TRANS = 'N', the number of rows of the matrix B.
       *          If TRANS = 'T', the number of rows of the matrix X.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix  A.
       *          If TRANS = 'N', the number of rows of the matrix X.
       *          If TRANS = 'T', the number of rows of the matrix B.
       *
       *  NRHS    (input) INTEGER
       *          The number of columns of the matrices X and B.
       *
       *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          The m-by-n matrix A.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A. LDA >= M.
       *
       *  X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)
       *          If TRANS = 'N', the n-by-nrhs matrix X.
       *          If TRANS = 'T', the m-by-nrhs matrix X.
       *
       *  LDX     (input) INTEGER
       *          The leading dimension of the array X.
       *          If TRANS = 'N', LDX >= N.
       *          If TRANS = 'T', LDX >= M.
       *
       *  B       (input) DOUBLE PRECISION array, dimension (LDB,NRHS)
       *          If TRANS = 'N', the m-by-nrhs matrix B.
       *          If TRANS = 'T', the n-by-nrhs matrix B.
       *
       *  LDB     (input) INTEGER
       *          The leading dimension of the array B.
       *          If TRANS = 'N', LDB >= M.
       *          If TRANS = 'T', LDB >= N.
       *
       *  C       (workspace) DOUBLE PRECISION array, dimension (LDB,NRHS)
       *
       */
    private double dqrt17(char trans, int iresid, int m, int n, int nrhs, double[][] A,
                          int lda, double[][] X, int ldx, double[][] B, int ldb,
                          double[][] C) {
        double val;;
        int info[] = new int[1];
        int iscl;
        int ncols;
        int nrows;
        double bignum;
        double err;
        double norma;
        double normb;
        double normrs;
        double normx;
        double smlnum;
        double rwork[] = new double[1];
        double work2[][];
        int p;
        int q;
        
        val = 0.0;
        
        if ((trans == 'N') || (trans == 'n')) {
            nrows = m;
            ncols = n;
        }
        else if ((trans == 'T') || (trans == 't')) {
            nrows = n;
            ncols = m;
        }
        else {
            MipavUtil.displayError("dqrt17 had an illegal trans value");
            return val; 
        }
        
        if ((m <= 0) || (n <= 0) || (nrhs <= 0)) {
            return val;
        }
        
        norma = ge.dlange('1', m, n, A, lda, rwork);
        smlnum = ge.dlamch('S')/ge.dlamch('P'); // Safe minimum/precision
        bignum = 1.0/smlnum;
        iscl = 0;
        
        // Compute residual and scale it
        ge.dlacpy('A', nrows, nrhs, B, ldb, C, ldb);
        ge.dgemm(trans, 'N', nrows, nrhs, ncols, -1.0, A, lda, X, ldx, 1.0, C, ldb);
        normrs = ge.dlange('M', nrows, nrhs, C, ldb, rwork);
        if (normrs > smlnum) {
            iscl = 1;
            ge.dlascl('G', 0, 0, normrs, 1.0, nrows, nrhs, C, ldb, info);
        }
        
        // Compute R'*A
        work2 = new double[nrhs][ncols];
        ge.dgemm('T', trans, nrhs, ncols, nrows, 1.0, C, ldb, A, lda, 0.0, work2, nrhs);
        
        // Compute and properly scale error
        err = ge.dlange('O', nrhs, ncols, work2, nrhs, rwork);
        if (norma != 0.0) {
            err = err/norma;
        }
        
        if (iscl == 1) {
            err = err * normrs;
        }
        
        if (iresid == 1) {
            normb = ge.dlange('O', nrows, nrhs, B, ldb, rwork);
            if (normb != 0.0) {
                err = err/normb;
            }
        } // if (iresid == 1)
        else {
            normx = ge.dlange('O', ncols, nrhs, X, ldx, rwork);
            if (normx != 0.0) {
                err = err/normx;
            }
        } // else
        
        val = err / (ge.dlamch('E') * Math.max(m, Math.max(n, nrhs)));
        return val;
    } // dqrt17
    
    /** This is a port of version 3.1 LAPACK test routine DQRT14. 
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             CHARACTER          TRANS
             INTEGER            LDA, LDX, LWORK, M, N, NRHS
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   A( LDA, * ), WORK( LWORK ), X( LDX, * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DQRT14 checks whether X is in the row space of A or A'.  It does so
       *  by scaling both X and A such that their norms are in the range
       *  [sqrt(eps), 1/sqrt(eps)], then computing a QR factorization of [A,X]
       *  (if TRANS = 'T') or an LQ factorization of [A',X]' (if TRANS = 'N'),
       *  and returning the norm of the trailing triangle, scaled by
       *  MAX(M,N,NRHS)*eps.
       *
       *  Arguments
       *  =========
       *
       *  TRANS   (input) CHARACTER*1
       *          = 'N':  No transpose, check for X in the row space of A
       *          = 'T':  Transpose, check for X in the row space of A'.
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrix A.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrix A.
       *
       *  NRHS    (input) INTEGER
       *          The number of right hand sides, i.e., the number of columns
       *          of X.
       *
       *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          The M-by-N matrix A.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.
       *
       *  X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)
       *          If TRANS = 'N', the N-by-NRHS matrix X.
       *          IF TRANS = 'T', the M-by-NRHS matrix X.
       *
       *  LDX     (input) INTEGER
       *          The leading dimension of the array X.
       *
       *  WORK    (workspace) DOUBLE PRECISION array dimension (LWORK)
       *
       *  LWORK   (input) INTEGER
       *          length of workspace array required
       *          If TRANS = 'N', LWORK >= (M+NRHS)*(N+2);
       *          if TRANS = 'T', LWORK >= (N+NRHS)*(M+2).
       */
    private double dqrt14(char trans, int m, int n, int nrhs, double[][] A, int lda,
                          double[][] X, int ldx, double[] work, int lwork) {
        double val;
        boolean tpsd;
        int i;
        int info[] = new int[1];
        int j;
        int ldwork;
        double anrm;
        double err;
        double xnrm;
        double rwork[] = new double[1];
        double work2[][];
        double work3[][];
        double work23[][];
        int p;
        int q;
        double work4[];
        double work5[];
        
        val = 0.0;
        if ((trans == 'N') || (trans == 'n')) {
            ldwork = m + nrhs;
            tpsd = false;
            if (lwork < (m+nrhs)*(n+2)) {
                MipavUtil.displayError("dqrt14 had lwork < (m+nrhs)*(n+2)");
                return val;
            }
            else if ((n <= 0) || (nrhs <= 0)) {
                return val;
            }
        } // if ((trans == 'N) || (trans == 'n))
        else if ((trans == 'T') || (trans == 't')) {
            ldwork = m;
            tpsd = true;
            if (lwork < (n+nrhs)*(m+2)) {
                MipavUtil.displayError("dqrt14 had lwork < (n+nrhs)*(m+2)");
                return val;
            }
            else if ((m <= 0) || (nrhs <= 0)) {
                return val;
            }
        } // else if ((trans == 'T) || (trans == 't))
        else {
            MipavUtil.displayError("dqrt14 had an illegal trans value");
            return val;
        }
        
        // Copy and scale A
        work2 = new double[ldwork][n];
        ge.dlacpy('A', m, n, A, lda, work2, ldwork);
        anrm = ge.dlange('M', m, n, work2, ldwork, rwork);
        if (anrm != 0.0) {
            ge.dlascl('G', 0, 0, anrm, 1.0, m, n, work2, ldwork, info);
            if (info[0] != 0) {
                MipavUtil.displayError("In dqrt14 ge.dlascl call #1 gave info[0] = " + info[0]);
            }
        }
        
        for (q = 0; q < n; q++) {
            for (p = 0; p < ldwork; p++) {
                work[p + q*ldwork] = work2[p][q];
            }
        }
        
        // Copy X or X' into the right place and scale it
        if (tpsd) {
            // Copy X into columns n+1:n+nrhs of work.
            work3 = new double[ldwork][nrhs];
            ge.dlacpy('A', m, nrhs, X, ldx, work3, ldwork);
            xnrm = ge.dlange('M', m, nrhs, work3, ldwork, rwork);
            if (xnrm != 0.0) {
                ge.dlascl('G', 0, 0, xnrm, 1.0, m, nrhs, work3, ldwork, info);
                if (info[0] != 0) {
                    MipavUtil.displayError("In dqrt14 ge.dlascl call #2 gave info[0] = " + info[0]);
                }
            } // if (xnrm != 0.0)
            work23 = new double[ldwork][n+nrhs];
            for (q = 0; q < n; q++) {
                for (p = 0; p < ldwork; p++) {
                    work23[p][q] = work2[p][q];
                }
            }
            for (q = 0; q < nrhs; q++) {
                for (p = 0; p < ldwork; p++) {
                    work23[p][q+n] = work3[p][q];
                }
            }
            anrm = ge.dlange('O', m, n+nrhs, work23, ldwork, rwork);
            // Compute QR factorization of X
            work4 = new double[Math.min(m, n+nrhs)];
            work5 = new double[n+nrhs];
            ge.dgeqr2(m, n+nrhs, work23, ldwork, work4, work5, info);
            if (info[0] != 0) {
                MipavUtil.displayError("In dqrt14 ge.dgeqr2 gave info[0] = " + info[0]);
            }
            
            // Compute the largest entry in the upper triangle of
            // work(n+1:m,n+1:n+nrhs)
            err = 0.0;
            for (q = 0; q < n+nrhs; q++) {
                for (p = 0; p < ldwork; p++) {
                    work[p + q * ldwork] = work23[p][q];
                }
            }
            for (p = 0; p < Math.min(m, n+nrhs); p++) {
                work[ldwork*(n+nrhs) + p] = work4[p];
            }
            for (j = n+1; j <= n+nrhs; j++) {
                for (i = n + 1; i <= Math.min(m, j); i++) {
                    err = Math.max(err, Math.abs(work[i-1+(j-1)*m]));
                }
            }
        } // if (tpsd)
        else { // !tpsd
            // Copy X' into rows m+1:m+nrhs of work
            for (i = 1; i <= n; i++) {
                for (j = 1; j <= nrhs; j++) {
                    work[m-1+j+(i-1)*ldwork] = X[i-1][j-1];
                    //if (Double.isNaN(X[i-1][j-1])) {
                        //MipavUtil.displayError("X[" + (i-1) + "][" + (j-1) + "] = is NaN");
                    //}
                }
            }
            work2 = new double[ldwork][n];
            for (q = 0; q < n; q++) {
                for (p = 0; p < ldwork; p++) {
                    work2[p][q] = work[m + p + q*ldwork];
                }
            }
            xnrm = ge.dlange('M', nrhs, n, work2, ldwork, rwork);
            if (xnrm != 0.0) {
                ge.dlascl('G', 0, 0, xnrm, 1.0, nrhs, n, work2, ldwork, info);
                //if (info[0] != 0) {
                    //MipavUtil.displayError("In dqrt14 ge.dlascl call #3 gave info[0] = " + info[0]);
                //}
            }
            
            // Compute LQ factorization of work
            for (q = 0; q < n; q++) {
                for (p = 0; p < ldwork; p++) {
                    work[m + p + q*ldwork] = work2[p][q];
                }
            }
            for (q = 0; q < n; q++) {
                for (p = 0; p < ldwork; p++) {
                    work2[p][q] = work[p + q*ldwork];
                }
            }
            work4 = new double[ldwork];
            work5 = new double[ldwork];
            dgelq2(ldwork, n, work2, ldwork, work4, work5, info);
            if (info[0] != 0) {
                MipavUtil.displayError("In dqrt14 dgeql2 had info[0] = " + info[0]);
            }
            for (q = 0; q < n; q++) {
                for (p = 0; p < ldwork; p++) {
                    work[p + q*ldwork] = work2[p][q];
                }
            }
            for (p = 0; p < ldwork; p++) {
                work[ldwork*n + p] = work4[p];
            }
            
            // Compute largest entry in lower triangle in
            // work(m+1:m+nrhs,m+1:n)
            err = 0.0;
            for (j = m+1; j <= n; j++) {
                for (i = j; i <= ldwork; i++) {
                    err = Math.max(err, Math.abs(work[i-1+(j-1)*ldwork]));
                }
            }
        } // else !tpsd
        
        val = err/(Math.max(m, Math.max(n, nrhs)) * ge.dlamch('E'));
        return val;
    } // dqrt14
    
    /**
     * This routine is an extraction from the FORTRAN program version 3.1.1 DCHKEE of the code needed to drive dchkbd in
     * order to run dchkbd to test the singular value decomposition routines, dgebrd, dorgbr, and dbdsqr.
     * Numerical values were obtained from the svd.in datafile. Original DCHKEE created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., January, 2007
     */
    public void dchkbd_test() {

        // The number of values of m and n contained in the vectors mval and nval.
        // The matrix sizes are used in pairs (m, n).
        int nsizes = 19;

        // The values of the matrix row dimension m
        int[] mval = new int[] {0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 3, 10, 10, 16, 16, 30, 30, 40, 40};
        
        // The values of the matrix column dimension n
        int[] nval = new int[] {0, 1, 3, 0, 1, 2, 0, 1, 0, 1, 3, 10, 16, 10, 16, 30, 40, 30, 40};

        // Number of values of NB, NBMIN, NX, and NRHS.
        int nparms = 5;

        // Values of blocksize NB
        int[] nbval = new int[] { 1, 3, 3, 3, 20 };

        // Values for the minimum blocksize NBMIN
        int[] nbmin = new int[] { 2, 2, 2, 2, 2 };
        
        // Values for the nx crossover point, NXVAL
        int[] nxval = new int[] { 1, 0, 5, 9, 1};

        // The values for the number of right hand sides nrhs.
        int[] nsval = new int[] { 2, 0, 2, 2, 2 };

        // Threshold value for the test ratios.  Information will be printed
        // about each test for which the test ratio is greater than or equal
        // to threshold.
        double thresh = 35.0;

        // Test the LAPACK routines
        boolean tstchk = true;

        // Test the driver routines
        boolean tstdrv = false;

        // Test the error exits for the LAPACK routines and driver routines.
        // Passed all 38 exits on test.
        // Put at false so as not to have to hit okay to 38 displayError messages.
        boolean tsterr = false;

        // Code describing how to set the random number seed.
        // = 0: Set the seed to a default number before each run.
        // = 1: Initialize the seed to a default value only before the first
        // run.
        // = 2: Like 1, but use the seed values in the 4 integer array
        // ioldsd
        int newsd = 1;
        // Number of matrix test types
        int maxtyp = 16;
        int ntypes = 16;
        boolean[] dotype = new boolean[maxtyp];
        int[] ioldsd = new int[] { 0, 0, 0, 1 };
        int[] iseed = new int[] { 0, 0, 0, 1 };
        int nmax = 132;
        int lwork = (nmax * ((5 * nmax) + 5)) + 1;
        double[] work = new double[lwork];
        double[] result = new double[500];
        int[] info = new int[1];
        int liwork = nmax * (5 * nmax + 20);
        int[] iwork = new int[liwork];
        double[][] A;
        double[] bd;
        double[] be;
        double[] s1;
        double[] s2;
        double[][] X;
        double[][] Y;
        double[][] Z;
        double[][] Q;
        double[][] PT;
        double[][] U;
        double[][] V;

        int i;
        int k;
        int nrhs;

        for (i = 0; i < maxtyp; i++) {
            dotype[i] = true;
        }

        iparms = new int[9];
        A = new double[nmax][nmax];
        bd = new double[nmax];
        be = new double[nmax];
        s1 = new double[nmax];
        s2 = new double[nmax];
        X = new double[nmax][nmax];
        Y = new double[nmax][nmax];
        Z = new double[nmax][nmax];
        Q = new double[nmax][nmax];
        PT = new double[nmax][nmax];
        U = new double[nmax][nmax];
        V = new double[nmax][nmax];
        
        xlaenv(1, 1);
        xlaenv(9, 25);
        
        if (tsterr && tstchk) {
            derrbd();
        }

        for (i = 1; i <= nparms; i++) {

            nrhs = nsval[i-1];
            xlaenv(1, nbval[i-1]);
            xlaenv(2, nbmin[i-1]);
            xlaenv(3, nxval[i-1]);
            if (newsd == 0) {

                for (k = 0; k < 4; k++) {
                    iseed[k] = ioldsd[k];
                }
            } // if (newsd == 0)

            Preferences.debug("Paramter " + i + " for dchkbd\n");
            Preferences.debug("Blocksize nb = " + nbval[i-1] + "\n");
            Preferences.debug("Minimum blocksize nbmin = " + nbmin[i - 1] + "\n");
            Preferences.debug("Crossover point nx = " + nxval[i-1] + "\n");
            Preferences.debug("Number of right hand sides nrhs = " + nrhs + "\n");

            if (tstchk) {
                dchkbd(nsizes, mval, nval, maxtyp, dotype, nrhs, iseed, thresh, A,
                       nmax, bd, be, s1, s2, X, nmax, Y, Z, Q, nmax, PT, nmax, U, V,
                       work, lwork, iwork, info);

                if (info[0] != 0) {
                    MipavUtil.displayError("dchkbd had info = " + info[0]);
                }
            } // if (tstchk)
        } // for (i = 1; i <= nparms; i++)
    } // dchkbd_test
    
    /** This is a port of most of version 3.1 LAPACK test routine DCHKBD.
    
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             INTEGER            INFO, LDA, LDPT, LDQ, LDX, LWORK, NOUT, NRHS,
            $                   NSIZES, NTYPES
             DOUBLE PRECISION   THRESH
       *     ..
       *     .. Array Arguments ..
             LOGICAL            DOTYPE( * )
             INTEGER            ISEED( 4 ), IWORK( * ), MVAL( * ), NVAL( * )
             DOUBLE PRECISION   A( LDA, * ), BD( * ), BE( * ), PT( LDPT, * ),
            $                   Q( LDQ, * ), S1( * ), S2( * ), U( LDPT, * ),
            $                   VT( LDPT, * ), WORK( * ), X( LDX, * ),
            $                   Y( LDX, * ), Z( LDX, * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DCHKBD checks the singular value decomposition (SVD) routines.
       *
       *  DGEBRD reduces a real general m by n matrix A to upper or lower
       *  bidiagonal form B by an orthogonal transformation:  Q' * A * P = B
       *  (or A = Q * B * P').  The matrix B is upper bidiagonal if m >= n
       *  and lower bidiagonal if m < n.
       *
       *  DORGBR generates the orthogonal matrices Q and P' from DGEBRD.
       *  Note that Q and P are not necessarily square.
       *
       *  DBDSQR computes the singular value decomposition of the bidiagonal
       *  matrix B as B = U S V'.  It is called three times to compute
       *     1)  B = U S1 V', where S1 is the diagonal matrix of singular
       *         values and the columns of the matrices U and V are the left
       *         and right singular vectors, respectively, of B.
       *     2)  Same as 1), but the singular values are stored in S2 and the
       *         singular vectors are not computed.
       *     3)  A = (UQ) S (P'V'), the SVD of the original matrix A.
       *  In addition, DBDSQR has an option to apply the left orthogonal matrix
       *  U to a matrix X, useful in least squares applications.
       *
       *  DBDSDC computes the singular value decomposition of the bidiagonal
       *  matrix B as B = U S V' using divide-and-conquer. It is called twice
       *  to compute
       *     1) B = U S1 V', where S1 is the diagonal matrix of singular
       *         values and the columns of the matrices U and V are the left
       *         and right singular vectors, respectively, of B.
       *     2) Same as 1), but the singular values are stored in S2 and the
       *         singular vectors are not computed.
       *
       *  For each pair of matrix dimensions (M,N) and each selected matrix
       *  type, an M by N matrix A and an M by NRHS matrix X are generated.
       *  The problem dimensions are as follows
       *     A:          M x N
       *     Q:          M x min(M,N) (but M x M if NRHS > 0)
       *     P:          min(M,N) x N
       *     B:          min(M,N) x min(M,N)
       *     U, V:       min(M,N) x min(M,N)
       *     S1, S2      diagonal, order min(M,N)
       *     X:          M x NRHS
       *
       *  For each generated matrix, 14 tests are performed:
       *
       *  Test DGEBRD and DORGBR
       *
       *  (1)   | A - Q B PT | / ( |A| max(M,N) ulp ), PT = P'
       *
       *  (2)   | I - Q' Q | / ( M ulp )
       *
       *  (3)   | I - PT PT' | / ( N ulp )
       *
       *  Test DBDSQR on bidiagonal matrix B
       *
       *  (4)   | B - U S1 VT | / ( |B| min(M,N) ulp ), VT = V'
       *
       *  (5)   | Y - U Z | / ( |Y| max(min(M,N),k) ulp ), where Y = Q' X
       *                                                   and   Z = U' Y.
       *  (6)   | I - U' U | / ( min(M,N) ulp )
       *
       *  (7)   | I - VT VT' | / ( min(M,N) ulp )
       *
       *  (8)   S1 contains min(M,N) nonnegative values in decreasing order.
       *        (Return 0 if true, 1/ULP if false.)
       *
       *  (9)   | S1 - S2 | / ( |S1| ulp ), where S2 is computed without
       *                                    computing U and V.
       *
       *  (10)  0 if the true singular values of B are within THRESH of
       *        those in S1.  2*THRESH if they are not.  (Tested using
       *        DSVDCH)
       *
       *  Test DBDSQR on matrix A
       *
       *  (11)  | A - (QU) S (VT PT) | / ( |A| max(M,N) ulp )
       *
       *  (12)  | X - (QU) Z | / ( |X| max(M,k) ulp )
       *
       *  (13)  | I - (QU)'(QU) | / ( M ulp )
       *
       *  (14)  | I - (VT PT) (PT'VT') | / ( N ulp )
       *
       *  Test DBDSDC on bidiagonal matrix B
       *
       *  (15)  | B - U S1 VT | / ( |B| min(M,N) ulp ), VT = V'
       *
       *  (16)  | I - U' U | / ( min(M,N) ulp )
       *
       *  (17)  | I - VT VT' | / ( min(M,N) ulp )
       *
       *  (18)  S1 contains min(M,N) nonnegative values in decreasing order.
       *        (Return 0 if true, 1/ULP if false.)
       *
       *  (19)  | S1 - S2 | / ( |S1| ulp ), where S2 is computed without
       *                                    computing U and V.
       *  The possible matrix types are
       *
       *  (1)  The zero matrix.
       *  (2)  The identity matrix.
       *
       *  (3)  A diagonal matrix with evenly spaced entries
       *       1, ..., ULP  and random signs.
       *       (ULP = (first number larger than 1) - 1 )
       *  (4)  A diagonal matrix with geometrically spaced entries
       *       1, ..., ULP  and random signs.
       *  (5)  A diagonal matrix with "clustered" entries 1, ULP, ..., ULP
       *       and random signs.
       *
       *  (6)  Same as (3), but multiplied by SQRT( overflow threshold )
       *  (7)  Same as (3), but multiplied by SQRT( underflow threshold )
       *
       *  (8)  A matrix of the form  U D V, where U and V are orthogonal and
       *       D has evenly spaced entries 1, ..., ULP with random signs
       *       on the diagonal.
       *
       *  (9)  A matrix of the form  U D V, where U and V are orthogonal and
       *       D has geometrically spaced entries 1, ..., ULP with random
       *       signs on the diagonal.
       *
       *  (10) A matrix of the form  U D V, where U and V are orthogonal and
       *       D has "clustered" entries 1, ULP,..., ULP with random
       *       signs on the diagonal.
       *
       *  (11) Same as (8), but multiplied by SQRT( overflow threshold )
       *  (12) Same as (8), but multiplied by SQRT( underflow threshold )
       *
       *  (13) Rectangular matrix with random entries chosen from (-1,1).
       *  (14) Same as (13), but multiplied by SQRT( overflow threshold )
       *  (15) Same as (13), but multiplied by SQRT( underflow threshold )
       *
       *  Special case:
       *  (16) A bidiagonal matrix with random entries chosen from a
       *       logarithmic distribution on [ulp^2,ulp^(-2)]  (I.e., each
       *       entry is  e^x, where x is chosen uniformly on
       *       [ 2 log(ulp), -2 log(ulp) ] .)  For *this* type:
       *       (a) DGEBRD is not called to reduce it to bidiagonal form.
       *       (b) the bidiagonal is  min(M,N) x min(M,N); if M<N, the
       *           matrix will be lower bidiagonal, otherwise upper.
       *       (c) only tests 5--8 and 14 are performed.
       *
       *  A subset of the full set of matrix types may be selected through
       *  the logical array DOTYPE.
       *
       *  Arguments
       *  ==========
       *
       *  NSIZES  (input) INTEGER
       *          The number of values of M and N contained in the vectors
       *          MVAL and NVAL.  The matrix sizes are used in pairs (M,N).
       *
       *  MVAL    (input) INTEGER array, dimension (NM)
       *          The values of the matrix row dimension M.
       *
       *  NVAL    (input) INTEGER array, dimension (NM)
       *          The values of the matrix column dimension N.
       *
       *  NTYPES  (input) INTEGER
       *          The number of elements in DOTYPE.   If it is zero, DCHKBD
       *          does nothing.  It must be at least zero.  If it is MAXTYP+1
       *          and NSIZES is 1, then an additional type, MAXTYP+1 is
       *          defined, which is to use whatever matrices are in A and B.
       *          This is only useful if DOTYPE(1:MAXTYP) is .FALSE. and
       *          DOTYPE(MAXTYP+1) is .TRUE. .
       *
       *  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
       *          If DOTYPE(j) is .TRUE., then for each size (m,n), a matrix
       *          of type j will be generated.  If NTYPES is smaller than the
       *          maximum number of types defined (PARAMETER MAXTYP), then
       *          types NTYPES+1 through MAXTYP will not be generated.  If
       *          NTYPES is larger than MAXTYP, DOTYPE(MAXTYP+1) through
       *          DOTYPE(NTYPES) will be ignored.
       *
       *  NRHS    (input) INTEGER
       *          The number of columns in the "right-hand side" matrices X, Y,
       *          and Z, used in testing DBDSQR.  If NRHS = 0, then the
       *          operations on the right-hand side will not be tested.
       *          NRHS must be at least 0.
       *
       *  ISEED   (input/output) INTEGER array, dimension (4)
       *          On entry ISEED specifies the seed of the random number
       *          generator. The array elements should be between 0 and 4095;
       *          if not they will be reduced mod 4096.  Also, ISEED(4) must
       *          be odd.  The values of ISEED are changed on exit, and can be
       *          used in the next call to DCHKBD to continue the same random
       *          number sequence.
       *
       *  THRESH  (input) DOUBLE PRECISION
       *          The threshold value for the test ratios.  A result is
       *          included in the output file if RESULT >= THRESH.  To have
       *          every test ratio printed, use THRESH = 0.  Note that the
       *          expected value of the test ratios is O(1), so THRESH should
       *          be a reasonably small multiple of 1, e.g., 10 or 100.
       *
       *  A       (workspace) DOUBLE PRECISION array, dimension (LDA,NMAX)
       *          where NMAX is the maximum value of N in NVAL.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.  LDA >= max(1,MMAX),
       *          where MMAX is the maximum value of M in MVAL.
       *
       *  BD      (workspace) DOUBLE PRECISION array, dimension
       *                      (max(min(MVAL(j),NVAL(j))))
       *
       *  BE      (workspace) DOUBLE PRECISION array, dimension
       *                      (max(min(MVAL(j),NVAL(j))))
       *
       *  S1      (workspace) DOUBLE PRECISION array, dimension
       *                      (max(min(MVAL(j),NVAL(j))))
       *
       *  S2      (workspace) DOUBLE PRECISION array, dimension
       *                      (max(min(MVAL(j),NVAL(j))))
       *
       *  X       (workspace) DOUBLE PRECISION array, dimension (LDX,NRHS)
       *
       *  LDX     (input) INTEGER
       *          The leading dimension of the arrays X, Y, and Z.
       *          LDX >= max(1,MMAX)
       *
       *  Y       (workspace) DOUBLE PRECISION array, dimension (LDX,NRHS)
       *
       *  Z       (workspace) DOUBLE PRECISION array, dimension (LDX,NRHS)
       *
       *  Q       (workspace) DOUBLE PRECISION array, dimension (LDQ,MMAX)
       *
       *  LDQ     (input) INTEGER
       *          The leading dimension of the array Q.  LDQ >= max(1,MMAX).
       *
       *  PT      (workspace) DOUBLE PRECISION array, dimension (LDPT,NMAX)
       *
       *  LDPT    (input) INTEGER
       *          The leading dimension of the arrays PT, U, and V.
       *          LDPT >= max(1, max(min(MVAL(j),NVAL(j)))).
       *
       *  U       (workspace) DOUBLE PRECISION array, dimension
       *                      (LDPT,max(min(MVAL(j),NVAL(j))))
       *
       *  V       (workspace) DOUBLE PRECISION array, dimension
       *                      (LDPT,max(min(MVAL(j),NVAL(j))))
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
       *
       *  LWORK   (input) INTEGER
       *          The number of entries in WORK.  This must be at least
       *          3(M+N) and  M(M + max(M,N,k) + 1) + N*min(M,N)  for all
       *          pairs  (M,N)=(MM(j),NN(j))
       *
       *  IWORK   (workspace) INTEGER array, dimension at least 8*min(M,N)
       *
       *  NOUT    (input) INTEGER
       *          The FORTRAN unit number for printing out error messages
       *          (e.g., if a routine returns IINFO not equal to 0.)
       *
       *  INFO    (output) INTEGER
       *          If 0, then everything ran OK.
       *           -1: NSIZES < 0
       *           -2: Some MM(j) < 0
       *           -3: Some NN(j) < 0
       *           -4: NTYPES < 0
       *           -6: NRHS  < 0
       *           -8: THRESH < 0
       *          -11: LDA < 1 or LDA < MMAX, where MMAX is max( MM(j) ).
       *          -17: LDB < 1 or LDB < MMAX.
       *          -21: LDQ < 1 or LDQ < MMAX.
       *          -23: LDPT< 1 or LDPT< MNMAX.
       *          -27: LWORK too small.
       *          If  DLATMR, SLATMS, DGEBRD, DORGBR, or DBDSQR,
       *              returns an error code, the
       *              absolute value of it is returned.
       */
    private void dchkbd(int nsizes, int[] mval, int[] nval, int ntypes, boolean[] dotype, int nrhs,
                        int[] iseed, double thresh, double[][] A, int lda, double[] bd, double[] be,
                        double[] s1, double[] s2, double[][] X, int ldx, double[][] Y, double[][] Z,
                        double[][] Q, int ldq, double[][] PT, int ldpt, double[][] U, double[][] VT,
                        double[] work, int lwork, int[] iwork, int[] info) {
        int maxtyp = 16;
        boolean badmm;
        boolean badnn;
        boolean bidiag = false;
        char uplo;
        String path;
        int i;
        int iinfo[] = new int[1];
        int imode;
        int itype;
        int j;
        int jcol;
        int jsize;
        int jtype;
        int log2ui;
        int m;
        int minwrk;
        int mmax;
        int mnmax;
        int mnmin;
        int mq = 0;
        int mtypes;
        int n;
        int nfail;
        int nmax;
        int ntest;
        double amninv;
        double anorm;
        double cond;
        double ovfl[] = new double[1];
        double rtovfl;
        double rtunfl;
        double temp1;
        double temp2;
        double ulp;
        double ulpinv;
        double unfl[] = new double[1];
        int idum[] = new int[1];
        int ioldsd[] = new int[4];
        int kmagn[] = new int[] {1, 1, 1, 1, 1, 2, 3, 1, 1, 1, 2, 3, 1, 2, 3, 0};
        int kmode[] = new int[] {0, 0, 4, 3, 1, 4, 4, 4, 3, 1, 4, 4, 0, 0, 0, 0};
        int ktype[] = new int[] {1, 2, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 9, 9, 9, 10};
        double dum[] = new double[1];
        double dumma[] = new double[1];
        // Use only first 14 of 19 tests.  Last 5 are for dbdsc.
        double result[] = new double[14];
        double work2[];
        double work3[];
        double array1[][];
        int p;
        int q;
        int row1;
        int index;
        double resid[] = new double[1];
        
        info[0] = 0;
        
        badmm = false;
        badnn = false;
        mmax = 1;
        nmax = 1;
        mnmax = 1;
        minwrk = 1;
        for (j = 0; j < nsizes; j++) {
            mmax = Math.max(mmax, mval[j]);
            if (mval[j] < 0) {
                badmm = true;
            }
            nmax = Math.max(nmax, nval[j]);
            if (nval[j] < 0) {
                badnn = true;
            }
            mnmax = Math.max(mnmax, Math.min(mval[j], nval[j]));
            minwrk = Math.max(minwrk, 3*(mval[j] + nval[j]));
            minwrk = Math.max(minwrk, mval[j]*(mval[j] + Math.max(mval[j],
                     Math.max(nval[j], nrhs))+1) + nval[j]*Math.min(nval[j], mval[j]));
        } // for (j = 0; j < nsizes; j++)
        
        // Check for errors
        if (nsizes < 0) {
            info[0] = -1;
        }
        else if (badmm) {
            info[0] = -2;
        }
        else if (badnn) {
            info[0] = -3;
        }
        else if (ntypes < 0) {
            info[0] = -4;
        }
        else if (nrhs < 0) {
            info[0] = -6;
        }
        else if (lda < mmax) {
            info[0] = -11;
        }
        else if (ldx < mmax) {
            info[0] = -17;
        }
        else if (ldq < mmax) {
            info[0] = -21;
        }
        else if (ldpt < mnmax) {
            info[0] = -23;
        }
        else if (minwrk > lwork) {
            info[0] = -27;
        }
        
        if (info[0] != 0) {
            MipavUtil.displayError("dchkbd had info[0] = " + info[0]);
            return;
        }
        
        // Initialize constants
        
        path = new String("DBD"); // D for Double precision
        nfail = 0;
        ntest = 0;
        unfl[0] = ge.dlamch('S'); // Safe minimum
        ovfl[0] = ge.dlamch('O'); // Overflow
        ge.dlabad(unfl, ovfl);
        ulp = ge.dlamch('P'); // Precision
        ulpinv = 1.0 / ulp;
        log2ui = (int)(Math.log(ulpinv)/Math.log(2.0));
        rtunfl = Math.sqrt(unfl[0]);
        rtovfl = Math.sqrt(ovfl[0]);
        infot = 0;
        
        // Loop over sizes, types
        
        for (jsize = 1; jsize <= nsizes; jsize++) {
            m = mval[jsize-1];
            n = nval[jsize-1];
            mnmin = Math.min(m, n);
            amninv = 1.0/Math.max(m, Math.max(n, 1));
            
            if (nsizes != 1) {
                mtypes = Math.min(maxtyp, ntypes);
            }
            else {
                mtypes = Math.min(maxtyp+1, ntypes);
            }
            
            for (jtype = 1; jtype <= mtypes; jtype++) {
               
                if (!dotype[jtype-1]) {
                    continue;
                }
                
                for (j = 0; j < 4; j++) {
                    ioldsd[j] = iseed[j];
                }
                
                for (j = 0; j < 14; j++) {
                    result[j] = -1.0;
                }
                
                uplo = ' ';
                
                //          Compute "A"
                //
                //          Control parameters:
                //
                //          KMAGN  KMODE        KTYPE
                //      =1  O(1)   clustered 1  zero
                //      =2  large  clustered 2  identity
                //      =3  small  exponential  (none)
                //      =4         arithmetic   diagonal, (w/ eigenvalues)
                //      =5         random       symmetric, w/ eigenvalues
                //      =6                      nonsymmetric, w/ singular values
                //      =7                      random diagonal
                //      =8                      random symmetric
                //     =9                      random nonsymmetric
                //      =10                     random bidiagonal (log. distrib.)
                
                if (mtypes <= maxtyp) {
                    itype = ktype[jtype-1];
                    imode = kmode[jtype-1];
                    
                    // Compute norm
                    if (kmagn[jtype-1] == 1) {
                        anorm = 1.0;
                    }
                    else if (kmagn[jtype-1] == 2) {
                        anorm = (rtovfl * ulp) * amninv;
                    }
                    else if (kmagn[jtype-1] == 3) {
                        anorm = rtunfl * Math.max(m, n) * ulpinv;
                    }
                    else {
                        anorm = 1.0;
                    }
                    
                    ge.dlaset('F', lda, n, 0.0, 0.0, A, lda);
                    iinfo[0] = 0;
                    cond = ulpinv;
                    
                    bidiag = false;
                    if (itype == 1) {
                        // Zero matrix
                        iinfo[0] = 0;
                    } // if (itype == 1)
                    else if (itype == 2) {
                        // Identity
                        for (jcol = 0; jcol < mnmin; jcol++) {
                            A[jcol][jcol] = anorm;
                        }
                    } // else if (itype == 2)
                    else if (itype == 4) {
                        // Diagonal matrix, [Eigen]values specified
                        work2 = new double[3*mnmin];
                        dlatms(mnmin, mnmin, 'S', iseed, 'N', work, imode, cond, anorm,
                               0, 0, 'N', A, lda, work2, iinfo);
                    } // else if (itype == 4)
                    else if (itype == 5) {
                        // Symmetric, eigenvalues specified
                        work2 = new double[3*mnmin];
                        dlatms(mnmin, mnmin, 'S', iseed, 'S', work, imode, cond, anorm,
                               m, n, 'N', A, lda, work2, iinfo);
                    } // else if (itype == 5)
                    else if (itype == 6) {
                        // Nonsymmetric, singular values specified
                        work2 = new double[3*Math.max(m,n)];
                        dlatms(m, n, 'S', iseed, 'N', work, imode, cond, anorm,
                               m, n, 'N', A, lda, work2, iinfo);
                    } // else if (itype == 6)
                    else if (itype == 7) {
                        // Diagonal, random entries
                        work2 = new double[mnmin];
                        work3 = new double[mnmin];
                        for (p = 0; p < mnmin; p++) {
                            work2[p] = work[mnmin + p];
                            work3[p] = work[2*mnmin+p];
                        }
                        dlatmr(mnmin, mnmin, 'S', iseed, 'N', work, 6, 1.0,
                               1.0, 'T', 'N', work2, 1, 1.0, work3, 1, 1.0,
                               'N', iwork, 0, 0, 0.0, anorm, 'N', A, lda, 
                               iwork, iinfo);
                        for (p = 0; p < mnmin; p++) {
                            work[mnmin + p] = work2[p];
                            work[2*mnmin+p] = work3[p];
                        }
                    } // else if (itype == 7)
                    else if (itype == 8) {
                        // Symmetric, random entries
                        work2 = new double[mnmin];
                        work3 = new double[mnmin];
                        for (p = 0; p < mnmin; p++) {
                            work2[p] = work[mnmin + p];
                            work3[p] = work[m+mnmin+p];
                        }
                        dlatmr(mnmin, mnmin, 'S', iseed, 'S', work, 6, 1.0,
                               1.0, 'T', 'N', work2, 1, 1.0, work3, 1, 1.0,
                               'N', iwork, m, n, 0.0, anorm, 'N', A, lda, 
                               iwork, iinfo);
                        for (p = 0; p < mnmin; p++) {
                            work[mnmin + p] = work2[p];
                            work[m+mnmin+p] = work3[p];
                        }
                    } // else if (itype == 8)
                    else if (itype == 9) {
                        // Nonsymmetric, random entries
                        work2 = new double[m];
                        work3 = new double[n];
                        for (p = 0; p < m; p++) {
                            work2[p] = work[mnmin + p];
                        }
                        for (p = 0; p < n; p++) {
                            work3[p] = work[m+mnmin+p];
                        }
                        dlatmr(m, n, 'S', iseed, 'N', work, 6, 1.0,
                               1.0, 'T', 'N', work2, 1, 1.0, work3, 1, 1.0,
                               'N', iwork, m, n, 0.0, anorm, 'N', A, lda, 
                               iwork, iinfo);
                        for (p = 0; p < m; p++) {
                            work[mnmin + p] = work2[p];
                        } 
                        for (p = 0; p < n; p++) {
                            work[m+mnmin+p] = work3[p];
                        }
                    } // else if (itype == 9)
                    else if (itype == 10) {
                        // Bidiagonal, random entries
                        temp1 = -2.0*Math.log(ulp);
                        for (j = 1; j <= mnmin; j++) {
                            bd[j-1] = Math.exp(temp1*dlarnd(2, iseed));
                            if (j < mnmin) {
                                be[j-1] = Math.exp(temp1*dlarnd(2, iseed));
                            }
                        } // for (j = 1; j <= mnmin; j++)
                        
                        iinfo[0] = 0;
                        bidiag = true;
                        if (m >= n) {
                            uplo = 'U';
                        }
                        else {
                            uplo = 'L';
                        }
                    } // else if (itype == 10)
                    else {
                        iinfo[0] = 1;
                    }
                    
                    if (iinfo[0] == 0) {
                        // Generate right-hand side
                        if (bidiag) {
                            work2 = new double[mnmin];
                            work3 = new double[nrhs];
                            for (p = 0; p < mnmin; p++) {
                                work2[p] = work[mnmin+p];
                            }
                            for (p = 0; p < nrhs; p++) {
                                work3[p] = work[2*mnmin+p];
                            }
                            dlatmr(mnmin, nrhs, 'S', iseed, 'N', work, 6, 1.0, 1.0, 
                                   'T', 'N', work2, 1, 1.0, work3, 1, 1.0, 'N', iwork,
                                   mnmin, nrhs, 0.0, 1.0, 'N', Y, ldx, iwork, iinfo);
                            for (p = 0; p < mnmin; p++) {
                                work[mnmin+p] = work2[p];
                            }
                            for (p = 0; p < nrhs; p++) {
                                work[2*mnmin+p] = work3[p];
                            }
                        } // if (bidiag)
                        else { // !bidiag
                            work2 = new double[m];
                            work3 = new double[nrhs];
                            for (p = 0; p < m; p++) {
                                work2[p] = work[m+p];
                            }
                            for (p = 0; p < nrhs; p++) {
                                work3[p] = work[2*m+p];
                            }
                            dlatmr(m, nrhs, 'S', iseed, 'N', work, 6, 1.0, 1.0, 
                                   'T', 'N', work2, 1, 1.0, work3, 1, 1.0, 'N', iwork,
                                   m, nrhs, 0.0, 1.0, 'N', X, ldx, iwork, iinfo);
                            for (p = 0; p < m; p++) {
                                work[m+p] = work2[p];
                            }
                            for (p = 0; p < nrhs; p++) {
                                work[2*m+p] = work3[p];
                            }    
                        } // else !bidiag
                    } // if (iinfo[0] == 0)
                    
                    // Error exit
                    if (iinfo[0] != 0) {
                        Preferences.debug("dchkbd generator returned iinfo[0] = " + iinfo[0] + "\n");
                        Preferences.debug("m = " + m + " n = " + n + " jtype = " + jtype + "\n");
                        Preferences.debug("ioldsd[0] = " + ioldsd[0] + " ioldsd[1] = " + ioldsd[1] + "\n");
                        Preferences.debug("ioldsd[2] = " + ioldsd[2] + " ioldsd[3] = " + ioldsd[3] + "\n");
                        MipavUtil.displayError("Error exit from dchkbd");
                        return;
                    } // if (iinfo[0] != 0)
                } // if (mtypes <= maxtype)

                // Call dgebrd and dorgbr to compute B, Q, and P, do tests.
                if (!bidiag) {
                    // Compute transformations to reduce A to bidiagonal form:
                    // B := Q' * A * P.
                    ge.dlacpy(' ', m, n, A, lda, Q, ldq);
                    work2 = new double[Math.min(m,n)];
                    work3 = new double[Math.max(1, lwork-2*mnmin)];
                    dgebrd(m, n, Q, ldq, bd, be, work, work2, work3, lwork-2*mnmin, iinfo);
                    for (p = 0; p < Math.min(m,n); p++) {
                        work[mnmin+p] = work2[p];
                    }
                    for (p = 0; p < Math.max(1, lwork-2*mnmin); p++) {
                        work[2*mnmin+p] = work3[p];
                    }
                    
                    // Check error code from dgebrd.
                    if (iinfo[0] != 0) {
                        Preferences.debug("dchkbd dgebrd returned iinfo[0] = " + iinfo[0] + "\n");
                        Preferences.debug("m = " + m + " n = " + n + " jtype = " + jtype + "\n");
                        Preferences.debug("ioldsd[0] = " + ioldsd[0] + " ioldsd[1] = " + ioldsd[1] + "\n");
                        Preferences.debug("ioldsd[2] = " + ioldsd[2] + " ioldsd[3] = " + ioldsd[3] + "\n");
                        MipavUtil.displayError("Error exit from dchkbd");
                        return;    
                    } // if (iinfo[0] != 0)
                    
                    ge.dlacpy(' ', m, n, Q, ldq, PT, ldpt);
                    if (m >= n) {
                        uplo = 'U';
                    }
                    else {
                        uplo = 'L';
                    }
                    
                    // Generate Q
                    mq = m;
                    if (nrhs <= 0) {
                        mq = mnmin;
                    }
                    work2 = new double[Math.max(1, lwork-2*mnmin)];
                    dorgbr('Q', m, mq, n, Q, ldq, work, work2, lwork-2*mnmin, iinfo);
                    for (p = 0; p < Math.max(1, lwork-2*mnmin); p++) {
                        work[2*mnmin+p] = work2[p];
                    }
                    
                    // Check error code from dorgbr.
                    if (iinfo[0] != 0) {
                        Preferences.debug("dchkbd dorgbr(Q) returned iinfo[0] = " + iinfo[0] + "\n");
                        Preferences.debug("m = " + m + " n = " + n + " jtype = " + jtype + "\n");
                        Preferences.debug("ioldsd[0] = " + ioldsd[0] + " ioldsd[1] = " + ioldsd[1] + "\n");
                        Preferences.debug("ioldsd[2] = " + ioldsd[2] + " ioldsd[3] = " + ioldsd[3] + "\n");
                        MipavUtil.displayError("Error exit from dchkbd");
                        return;        
                    } // if (iinfo[0] != 0)
                    
                    // Generate P'
                    work2 = new double[Math.min(m,n)];
                    work3 = new double[Math.max(1, lwork-2*mnmin)];
                    for (p = 0; p < Math.min(m, n); p++) {
                        work2[p] = work[mnmin+p];
                    }
                    dorgbr('P', mnmin, n, m, PT, ldpt, work2, work3, lwork-2*mnmin, iinfo);
                    for (p = 0; p < Math.max(1, lwork-2*mnmin); p++) {
                        work[2*mnmin+p] = work3[p];
                    }
                    
                    // Check error code from dorgbr.
                    if (iinfo[0] != 0) {
                        Preferences.debug("dchkbd dorgbr(P) returned iinfo[0] = " + iinfo[0] + "\n");
                        Preferences.debug("m = " + m + " n = " + n + " jtype = " + jtype + "\n");
                        Preferences.debug("ioldsd[0] = " + ioldsd[0] + " ioldsd[1] = " + ioldsd[1] + "\n");
                        Preferences.debug("ioldsd[2] = " + ioldsd[2] + " ioldsd[3] = " + ioldsd[3] + "\n");
                        MipavUtil.displayError("Error exit from dchkbd");
                        return;        
                    } // if (iinfo[0] != 0)
                    
                    // Apply Q' to an M by NRHS matrix X:     Y := Q' * X.
                    ge.dgemm('T', 'N', m, nrhs, m, 1.0, Q, ldq, X, ldx, 0.0, Y, ldx);
                    
                    // Test 1:  Check the decomposition of A := Q * B * PT
                    //      2:  Check the orthogonality of Q
                    //      3:  Check the orthogonality of PT
                    
                    dbdt01(m, n, 1, A, lda, Q, ldq, bd, be, PT, ldpt, work, result);
                    row1 = Math.min(m, mq);
                    array1 = new double[row1][row1];
                    index = 0;
                    for (q = 0; q < row1; q++) {
                        for (p = 0; p < row1; p++) {
                            array1[p][q] = work[index++];
                        }
                    }
                    dort01('C', m, mq, Q, ldq, array1, lwork, resid);
                    index = 0;
                    for (q = 0; q < row1; q++) {
                        for (p = 0; p < row1; p++) {
                            work[index++] = array1[p][q];
                        }
                    }
                    result[1] = resid[0];
                    row1 = Math.min(mnmin, n);
                    array1 = new double[row1][row1];
                    index = 0;
                    for (q = 0; q < row1; q++) {
                        for (p = 0; p < row1; p++) {
                            array1[p][q] = work[index++];
                        }
                    }
                    dort01('R', mnmin, n, PT, ldpt, array1, lwork, resid);
                    index = 0;
                    for (q = 0; q < row1; q++) {
                        for (p = 0; p < row1; p++) {
                            work[index++] = array1[p][q];
                        }
                    }
                    result[2] = resid[0];
                } // if (!bidiag)
                
                // Use dbdsqr to form the SVD of the bidiagonal matrix B:
                // B := U * S1 * VT, and compute Z = U' * Y.
                for (p = 0; p < mnmin; p++) {
                    s1[p] = bd[p];
                }
                if (mnmin > 1) {
                    for (p = 0; p < mnmin - 1; p++) {
                        work[p] = be[p];
                    }
                } // if (mnmin > 1)
                ge.dlacpy(' ', m, nrhs, Y, ldx, Z, ldx);
                ge.dlaset('F', mnmin, mnmin, 0.0, 1.0, U, ldpt);
                ge.dlaset('F', mnmin, mnmin, 0.0, 1.0, VT, ldpt);
                
                work2 = new double[4*mnmin];
                dbdsqr(uplo, mnmin, mnmin, mnmin, nrhs, s1, work, VT,
                       ldpt, U, ldpt, Z, ldx, work2, iinfo);
                
                // check error code from dbdsqr.
                if (iinfo[0] != 0) {
                    Preferences.debug("dchkbd dbdsqr(vects) returned iinfo[0] = " + iinfo[0] + "\n");
                    Preferences.debug("m = " + m + " n = " + n + " jtype = " + jtype + "\n");
                    Preferences.debug("ioldsd[0] = " + ioldsd[0] + " ioldsd[1] = " + ioldsd[1] + "\n");
                    Preferences.debug("ioldsd[2] = " + ioldsd[2] + " ioldsd[3] = " + ioldsd[3] + "\n");
                    if (iinfo[0] < 0) {
                        MipavUtil.displayError("Error exit from dchkbd");
                        return; 
                    }
                    else {
                        result[3] = ulpinv;
                        for (j = 0; j < 4; j++) {
                            if (result[j] >= thresh) {
                                if (nfail == 0) {
                                    Preferences.debug("Real singular value decomposition\n");
                                    Preferences.debug("Matrix types:\n");
                                    Preferences.debug("1: Zero\n");
                                    Preferences.debug("2: Identity\n");
                                    Preferences.debug("3: Evenly splaced entries\n");
                                    Preferences.debug("4: Geometrically spaced entries\n");
                                    Preferences.debug("5: Clustered entries\n");
                                    Preferences.debug("6: Large, evenly spaced entries\n");
                                    Preferences.debug("7: Small, evenly spaced entries\n");
                                    Preferences.debug("8: Evenly spaced singular values\n");
                                    Preferences.debug("9: Geometrically spaced singular values\n");
                                    Preferences.debug("10: Clustered singular values\n");
                                    Preferences.debug("11: Large, evenly spaced singular values\n");
                                    Preferences.debug("12: Small, evenly spaced singular values\n");
                                    Preferences.debug("13: Random, O(1) entries\n");
                                    Preferences.debug("14: Random, scaled near overflow\n");
                                    Preferences.debug("15: Random, scaled near underflow\n");
                                    Preferences.debug("Test ratios:\n");
                                    Preferences.debug("B: bidiagonal, S: diagonal, Q, P, U, and V: orthogonal\n");
                                    Preferences.debug("X: m x nrhs, Y = Q' X, and Z = U' Y\n");
                                    Preferences.debug("1: norm( A - Q B P'' ) / ( norm(A) max(m,n) ulp )\n");
                                    Preferences.debug("2: norm( I - Q'' Q )   / ( m ulp )\n");
                                    Preferences.debug("3: norm( I - P'' P )   / ( n ulp )\n");
                                    Preferences.debug("4: norm( B - U S V'' ) / ( norm(B) min(m,n) ulp )\n");
                                    Preferences.debug("5: norm( Y - U Z )    / ( norm(Z) max(min(m,n),k) ulp )\n");
                                    Preferences.debug("6: norm( I - U'' U )   / ( min(m,n) ulp )\n");
                                    Preferences.debug("7: norm( I - V'' V )   / ( min(m,n) ulp )\n");
                                    Preferences.debug("8: Test ordering of S  (0 if nondecreasing, 1/ulp otherwise)\n");
                                    Preferences.debug("9: norm( S - S2 )     / ( norm(S) ulp )\n");
                                    Preferences.debug("where S2 is computed without computing U and V\n");
                                    Preferences.debug("10: Sturm sequence test\n");
                                    Preferences.debug("0 if sing. vals of B within THRESH of S\n");
                                    Preferences.debug("11: norm( A - (QU) S (V' P') ) / ( norm(A) max(m,n) ulp )\n");
                                    Preferences.debug("12: norm( X - (QU) Z )         / ( |X| max(M,k) ulp )\n");
                                    Preferences.debug("13: norm( I - (QU)''(QU) )      / ( M ulp )\n");
                                    Preferences.debug("14: norm( I - (V'' P'') (P V) )  / ( N ulp )\n"); 
                                } // if (nfail == 0)
                                Preferences.debug("m = " + m + " n = " + n + " jtype = " + jtype + "\n");
                                Preferences.debug("ioldsd[0] = " + ioldsd[0] + " ioldsd[1] = " + ioldsd[1] + "\n");
                                Preferences.debug("ioldsd[2] = " + ioldsd[2] + " ioldsd[3] = " + ioldsd[3] + "\n");
                                Preferences.debug("test(" + (j+1) + ") = " + result[j] + "\n");
                                nfail++;
                            } // if (result[j] >= thresh)
                        } // for (j = 0; j < 4; j++)
                        if (!bidiag) {
                            ntest = ntest + 4;
                        }
                        else {
                            ntest = ntest + 1;
                        }
                        continue;
                    }
                } // if (iinfo[0] != 0)
                
                // Use dbdsqr to compute only the singular values of the 
                // bidiagonal matrix B; U, VT, and Z should not be modified.
                for (p = 0; p < mnmin; p++) {
                    s2[p] = bd[p];
                }
                if (mnmin > 1) {
                    for (p = 0; p < mnmin-1; p++) {
                        work[p] = be[p];
                    }
                } // if (mnmin > 1)
                
                work2 = new double[4*mnmin];
                dbdsqr(uplo, mnmin, 0, 0, 0, s2, work, VT, ldpt, U,
                       ldpt, Z, ldx, work2, iinfo);
                
                // check error code from dbdsqr.
                if (iinfo[0] != 0) {
                    Preferences.debug("dchkbd dbdsqr(values) returned iinfo[0] = " + iinfo[0] + "\n");
                    Preferences.debug("m = " + m + " n = " + n + " jtype = " + jtype + "\n");
                    Preferences.debug("ioldsd[0] = " + ioldsd[0] + " ioldsd[1] = " + ioldsd[1] + "\n");
                    Preferences.debug("ioldsd[2] = " + ioldsd[2] + " ioldsd[3] = " + ioldsd[3] + "\n");
                    if (iinfo[0] < 0) {
                        MipavUtil.displayError("Error exit from dchkbd");
                        return; 
                    }
                    else {
                        result[8] = ulpinv;
                        for (j = 0; j < 9; j++) {
                            if (result[j] >= thresh) {
                                if (nfail == 0) {
                                    Preferences.debug("Real singular value decomposition\n");
                                    Preferences.debug("Matrix types:\n");
                                    Preferences.debug("1: Zero\n");
                                    Preferences.debug("2: Identity\n");
                                    Preferences.debug("3: Evenly splaced entries\n");
                                    Preferences.debug("4: Geometrically spaced entries\n");
                                    Preferences.debug("5: Clustered entries\n");
                                    Preferences.debug("6: Large, evenly spaced entries\n");
                                    Preferences.debug("7: Small, evenly spaced entries\n");
                                    Preferences.debug("8: Evenly spaced singular values\n");
                                    Preferences.debug("9: Geometrically spaced singular values\n");
                                    Preferences.debug("10: Clustered singular values\n");
                                    Preferences.debug("11: Large, evenly spaced singular values\n");
                                    Preferences.debug("12: Small, evenly spaced singular values\n");
                                    Preferences.debug("13: Random, O(1) entries\n");
                                    Preferences.debug("14: Random, scaled near overflow\n");
                                    Preferences.debug("15: Random, scaled near underflow\n");
                                    Preferences.debug("Test ratios:\n");
                                    Preferences.debug("B: bidiagonal, S: diagonal, Q, P, U, and V: orthogonal\n");
                                    Preferences.debug("X: m x nrhs, Y = Q' X, and Z = U' Y\n");
                                    Preferences.debug("1: norm( A - Q B P'' ) / ( norm(A) max(m,n) ulp )\n");
                                    Preferences.debug("2: norm( I - Q'' Q )   / ( m ulp )\n");
                                    Preferences.debug("3: norm( I - P'' P )   / ( n ulp )\n");
                                    Preferences.debug("4: norm( B - U S V'' ) / ( norm(B) min(m,n) ulp )\n");
                                    Preferences.debug("5: norm( Y - U Z )    / ( norm(Z) max(min(m,n),k) ulp )\n");
                                    Preferences.debug("6: norm( I - U'' U )   / ( min(m,n) ulp )\n");
                                    Preferences.debug("7: norm( I - V'' V )   / ( min(m,n) ulp )\n");
                                    Preferences.debug("8: Test ordering of S  (0 if nondecreasing, 1/ulp otherwise)\n");
                                    Preferences.debug("9: norm( S - S2 )     / ( norm(S) ulp )\n");
                                    Preferences.debug("where S2 is computed without computing U and V\n");
                                    Preferences.debug("10: Sturm sequence test\n");
                                    Preferences.debug("0 if sing. vals of B within THRESH of S\n");
                                    Preferences.debug("11: norm( A - (QU) S (V' P') ) / ( norm(A) max(m,n) ulp )\n");
                                    Preferences.debug("12: norm( X - (QU) Z )         / ( |X| max(M,k) ulp )\n");
                                    Preferences.debug("13: norm( I - (QU)''(QU) )      / ( M ulp )\n");
                                    Preferences.debug("14: norm( I - (V'' P'') (P V) )  / ( N ulp )\n"); 
                                } // if (nfail == 0)
                                Preferences.debug("m = " + m + " n = " + n + " jtype = " + jtype + "\n");
                                Preferences.debug("ioldsd[0] = " + ioldsd[0] + " ioldsd[1] = " + ioldsd[1] + "\n");
                                Preferences.debug("ioldsd[2] = " + ioldsd[2] + " ioldsd[3] = " + ioldsd[3] + "\n");
                                Preferences.debug("test(" + (j+1) + ") = " + result[j] + "\n");
                                nfail++;
                            } // if (result[j] >= thresh)
                        } // for (j = 0; j < 9; j++)
                        if (!bidiag) {
                            ntest = ntest + 5;
                        }
                        else {
                            ntest = ntest + 2;
                        }
                        continue;
                    }
                } // if (iinfo[0] != 0)
                
                // Test 4:  Check the decomposition B := U * S1 * VT
                //      5:  Check the computation Z := U' * Y
                //      6:  Check the orthogonality of U
                //      7:  Check the orthogonality of VT
                dbdt03(uplo, mnmin, 1, bd, be, U, ldpt, s1, VT, ldpt, work, resid);
                result[3] = resid[0];
                dbdt02(mnmin, nrhs, Y, ldx, Z, ldx, U, ldpt, work, resid);
                result[4] = resid[0];
                array1 = new double[mnmin][mnmin];
                dort01('C', mnmin, mnmin, U, ldpt, array1, lwork, resid);
                result[5] = resid[0];
                dort01('R', mnmin, mnmin, VT, ldpt, array1, lwork, resid);
                result[6] = resid[0];
                
                // Test 8: Check that the singular values are sorted in
                //         non-increasing order and are non-negative
                result[7] = 0.0;
                for (i = 1; i <= mnmin - 1; i++) {
                    if (s1[i-1] < s1[i]) {
                        result[7] = ulpinv;
                    }
                    if (s1[i-1] < 0.0) {
                        result[7] = ulpinv;
                    }
                } // for (i = 1; i <= mnmin - 1; i++)
                if (mnmin >= 1) {
                    if (s1[mnmin-1] < 0.0) {
                        result[7] = ulpinv;
                    }
                } // if (mnmin >= 1)
                
                // Test 9: Compare dbdsqr with and without singular vectors
                temp2 = 0.0;
                
                for (j = 0; j < mnmin; j++) {
                    temp1 = Math.abs(s1[j]-s2[j])/
                    Math.max(Math.sqrt(unfl[0]) * Math.max(s1[0], 1.0),
                    ulp * Math.max(Math.abs(s1[j]), Math.abs(s2[j])));
                    temp2 = Math.max(temp1, temp2);
                } // for (j = 0; j < mnmin; j++)
                
                result[8] = temp2;
                
                // Test 10: Sturm sequence test of singular values
                //          Go up by factors of two until it succeeds
                temp1 = thresh * (0.5 - ulp);
                
                for (j = 0; j <= log2ui; j++) {
                    dsvdch(mnmin, bd, be, s1, temp1, iinfo);
                    if (iinfo[0] == 0) {
                        break;
                    }
                    temp1 = 2.0 * temp1;
                } // for (j = 0; j <= log2ui; j++)
                result[9] = temp1;
                if (result[9] >= thresh) {
                    Preferences.debug("mnmin = " + mnmin + "iinfo[0] = " + iinfo[0] + "\n");
                    for (j = 0; j < bd.length; j++) {
                        Preferences.debug("bd[" + j + "] = " + bd[j] + "\n");
                    }
                    for (j = 0; j < be.length; j++) {
                        Preferences.debug("be[" + j + "] = " + be[j] + "\n");
                    }
                    for (j = 0; j < s1.length; j++) {
                        Preferences.debug("s1[" + j + "] = " + s1[j] + "\n");
                    }
                }
                
                // Use dbdsqr to form the decomposition A := (QU) S (VT PT)
                // frpom the bidiagonal form A := Q B PT
                if (!bidiag) {
                    for (p = 0; p < mnmin; p++) {
                        s2[p] = bd[p];
                    }
                    if (mnmin > 1) {
                        for (p = 0; p < mnmin - 1; p++) {
                            work[p] = be[p];
                        }
                    } // if (mnmin > 1)
                    
                    work2 = new double[4*mnmin];
                    dbdsqr(uplo, mnmin, n, m, nrhs, s2, work, PT, ldpt,
                           Q, ldq, Y, ldx, work2, iinfo);
                    
                    // Test 11:  Check the decomposition A := Q*U * S2 * VT*PT
                    //      12:  Check the computation Z := U' * Q' * X
                    //      13:  Check the orthogonality of Q*U
                    //      14:  Check the orthogonality of VT*PT
                    dbdt01(m, n, 0, A, lda, Q, ldq, s2, dumma, PT, ldpt, work, resid);
                    result[10] = resid[0];
                    dbdt02(m, nrhs, X, ldx, Y, ldx, Q, ldq, work, resid);
                    result[11] = resid[0];
                    array1 = new double[Math.min(m, mq)][Math.min(m, mq)];
                    dort01('C', m, mq, Q, ldq, array1, lwork, resid);
                    result[12] = resid[0];
                    array1 = new double[Math.min(mnmin, n)][Math.min(mnmin, n)];
                    dort01('R', mnmin, n, PT, ldpt, array1, lwork, resid);
                    result[13] = resid[0];
                } // if (!bidiag)
                
                for (j = 0; j < 14; j++) {
                    if (result[j] >= thresh) {
                        if (nfail == 0) {
                            Preferences.debug("Real singular value decomposition\n");
                            Preferences.debug("Matrix types:\n");
                            Preferences.debug("1: Zero\n");
                            Preferences.debug("2: Identity\n");
                            Preferences.debug("3: Evenly splaced entries\n");
                            Preferences.debug("4: Geometrically spaced entries\n");
                            Preferences.debug("5: Clustered entries\n");
                            Preferences.debug("6: Large, evenly spaced entries\n");
                            Preferences.debug("7: Small, evenly spaced entries\n");
                            Preferences.debug("8: Evenly spaced singular values\n");
                            Preferences.debug("9: Geometrically spaced singular values\n");
                            Preferences.debug("10: Clustered singular values\n");
                            Preferences.debug("11: Large, evenly spaced singular values\n");
                            Preferences.debug("12: Small, evenly spaced singular values\n");
                            Preferences.debug("13: Random, O(1) entries\n");
                            Preferences.debug("14: Random, scaled near overflow\n");
                            Preferences.debug("15: Random, scaled near underflow\n");
                            Preferences.debug("Test ratios:\n");
                            Preferences.debug("B: bidiagonal, S: diagonal, Q, P, U, and V: orthogonal\n");
                            Preferences.debug("X: m x nrhs, Y = Q' X, and Z = U' Y\n");
                            Preferences.debug("1: norm( A - Q B P'' ) / ( norm(A) max(m,n) ulp )\n");
                            Preferences.debug("2: norm( I - Q'' Q )   / ( m ulp )\n");
                            Preferences.debug("3: norm( I - P'' P )   / ( n ulp )\n");
                            Preferences.debug("4: norm( B - U S V'' ) / ( norm(B) min(m,n) ulp )\n");
                            Preferences.debug("5: norm( Y - U Z )    / ( norm(Z) max(min(m,n),k) ulp )\n");
                            Preferences.debug("6: norm( I - U'' U )   / ( min(m,n) ulp )\n");
                            Preferences.debug("7: norm( I - V'' V )   / ( min(m,n) ulp )\n");
                            Preferences.debug("8: Test ordering of S  (0 if nondecreasing, 1/ulp otherwise)\n");
                            Preferences.debug("9: norm( S - S2 )     / ( norm(S) ulp )\n");
                            Preferences.debug("where S2 is computed without computing U and V\n");
                            Preferences.debug("10: Sturm sequence test\n");
                            Preferences.debug("0 if sing. vals of B within THRESH of S\n");
                            Preferences.debug("11: norm( A - (QU) S (V' P') ) / ( norm(A) max(m,n) ulp )\n");
                            Preferences.debug("12: norm( X - (QU) Z )         / ( |X| max(M,k) ulp )\n");
                            Preferences.debug("13: norm( I - (QU)''(QU) )      / ( M ulp )\n");
                            Preferences.debug("14: norm( I - (V'' P'') (P V) )  / ( N ulp )\n"); 
                        } // if (nfail == 0)
                        Preferences.debug("m = " + m + " n = " + n + " jtype = " + jtype + "\n");
                        Preferences.debug("ioldsd[0] = " + ioldsd[0] + " ioldsd[1] = " + ioldsd[1] + "\n");
                        Preferences.debug("ioldsd[2] = " + ioldsd[2] + " ioldsd[3] = " + ioldsd[3] + "\n");
                        Preferences.debug("test(" + (j+1) + ") = " + result[j] + "\n");
                        nfail++;
                    } // if (result[j] >= thresh)
                } // for (j = 0; j < 14; j++)
                if (!bidiag) {
                    ntest = ntest + 14;
                }
                else {
                    ntest = ntest + 7;
                }
            } // for (jtype = 1; jtype <= mtypes; jtype++)
        } // for (jsize = 1; jsize <= nsizes; jsize++)
        
        if (nfail > 0) {
            Preferences.debug("In dchkbd " + nfail + " out of " + ntest + " failed to pass the threshold\n");
            MipavUtil.displayError("In dchkbd " + nfail + " out of " + ntest + " failed to pass the threshold");
        }
        else {
            Preferences.debug("In dchkbd all " + ntest + " tests run passed the threshold\n");
            MipavUtil.displayError("In dchkbd all " + ntest + " tests run passed the threshold");
        }
    } // dchkbd

    /** This is a port of that portion of version 3.1 LAPACK test routine DERRBD used to test the
     * error exits for dgebrd, dorgbr, dormbr, and dbdsqr.
     */
    private void derrbd() {
        int nmax = 4;
        int lw = nmax;
        int i;
        int info[] = new int[1];
        int j;
        int IQ[][] = new int[nmax][nmax];
        int IW[] = new int[nmax];
        double A[][] = new double[nmax][nmax];
        double D[] = new double[nmax];
        double E[] = new double[nmax];
        double Q[][] = new double[nmax][nmax];
        double TP[] = new double[nmax];
        double TQ[] = new double[nmax];
        double U[][] = new double[nmax][nmax];
        double V[][] = new double[nmax][nmax];
        double W[] = new double[lw];
        int npass = 38;
        int ntotal = 38;
        
        for (j = 1; j <= nmax; j++) {
            for (i = 1; i <= nmax; i++) {
                A[i-1][j-1] = 1.0/(double)(i+j);
            }
        }
        
        // Test the error eixts of the SVD routines.
        
        // DGEBRD
        dgebrd(-1, 0, A, 1, D, E, TQ, TP, W, 1, info);
        if (info[0] != -1) {
            Preferences.debug("dgebrd(-1, 0, A, 1, D, E, TQ, TP, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -1\n");
            npass--;
        }
        
        dgebrd(0, -1, A, 1, D, E, TQ, TP, W, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dgebrd(0, -1, A, 1, D, E, TQ, TP, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -2\n");
            npass--;
        }
        
        dgebrd(2, 1, A, 1, D, E, TQ, TP, W, 2, info);
        if (info[0] != -4) {
            Preferences.debug("dgebrd(2, 1, A, 1, D, E, TQ, TP, W, 2, info) produced info[0] = " + info[0] +
                              " instead of -4\n");
            npass--;
        }
        
        dgebrd(2, 1, A, 2, D, E, TQ, TP, W, 1, info);
        if (info[0] != -10) {
            Preferences.debug("dgebrd(2, 1, A, 2, D, E, TQ, TP, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -10\n");
            npass--;
        }
        
        // DGEBD2
        dgebd2(-1, 0, A, 1, D, E, TQ, TP, W, info);
        if (info[0] != -1) {
            Preferences.debug("dgebd2(-1, 0, A, 1, D, E, TQ, TP, W, info) produced info[0] = " + info[0] +
                              " instead of -1\n");
            npass--;
        }
        
        dgebd2(0, -1, A, 1, D, E, TQ, TP, W, info);
        if (info[0] != -2) {
            Preferences.debug("dgebd2(0, -1, A, 1, D, E, TQ, TP, W, info) produced info[0] = " + info[0] +
                              " instead of -2\n");
            npass--;
        }
        
        dgebd2(2, 1, A, 1, D, E, TQ, TP, W, info);
        if (info[0] != -4) {
            Preferences.debug("dgebd2(2, 1, A, 1, D, E, TQ, TP, W, info) produced info[0] = " + info[0] +
                              " instead of -4\n");
            npass--;
        }
        
        // DORGBR
        dorgbr('/', 0, 0, 0, A, 1, TQ, W, 1, info);
        if (info[0] != -1) {
            Preferences.debug("dorgbr('/', 0, 0, 0, A, 1, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -1\n");
            npass--;
        }
        
        dorgbr('Q', -1, 0, 0, A, 1, TQ, W, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dorgbr('Q', -1, 0, 0, A, 1, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -2\n");
            npass--;
        }
        
        dorgbr('Q', 0, -1, 0, A, 1, TQ, W, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dorgbr('Q', 0, -1, 0, A, 1, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -3\n");
            npass--;
        }
        
        dorgbr('Q', 0, 1, 0, A, 1, TQ, W, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dorgbr('Q', 0, 1, 0, A, 1, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -3\n");
            npass--;
        }
        
        dorgbr('Q', 1, 0, 1, A, 1, TQ, W, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dorgbr('Q', 1, 0, 1, A, 1, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -3\n");
            npass--;
        }
        
        dorgbr('P', 1, 0, 0, A, 1, TQ, W, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dorgbr('P', 1, 0, 0, A, 1, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -3\n");
            npass--;
        }
        
        dorgbr('P', 0, 1, 1, A, 1, TQ, W, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dorgbr('P', 0, 1, 1, A, 1, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -3\n");
            npass--;
        }
        
        dorgbr('Q', 0, 0, -1, A, 1, TQ, W, 1, info);
        if (info[0] != -4) {
            Preferences.debug("dorgbr('Q', 0, 0, -1, A, 1, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -4\n");
            npass--;
        }
        
        dorgbr('Q', 2, 1, 1, A, 1, TQ, W, 1, info);
        if (info[0] != -6) {
            Preferences.debug("dorgbr('Q', 2, 1, 1, A, 1, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -6\n");
            npass--;
        }
        
        dorgbr('Q', 2, 2, 1, A, 2, TQ, W, 1, info);
        if (info[0] != -9) {
            Preferences.debug("dorgbr('Q', 2, 2, 1, A, 2, TQ, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -9\n");
            npass--;
        }
        
        // DORMBR
        dormbr('/', 'L', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -1) {
            Preferences.debug("dormbr('/', 'L', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -1\n");
            npass--;
        }
        
        dormbr('Q', '/', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dormbr('Q', '/', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -2\n");
            npass--;
        }
        
        dormbr('Q', 'L', '/', 0, 0, 0, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dormbr('Q', 'L', '/', 0, 0, 0, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -3\n");
            npass--;
        }
        
        dormbr('Q', 'L', 'T', -1, 0, 0, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -4) {
            Preferences.debug("dormbr('Q', 'L', 'T', -1, 0, 0, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -4\n");
            npass--;
        }
        
        dormbr('Q', 'L', 'T', 0, -1, 0, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -5) {
            Preferences.debug("dormbr('Q', 'L', 'T', 0, -1, 0, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -5\n");
            npass--;
        }
        
        dormbr('Q', 'L', 'T', 0, 0, -1, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -6) {
            Preferences.debug("dormbr('Q', 'L', 'T', 0, 0, -1, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -6\n");
            npass--;
        }
        
        dormbr('Q', 'L', 'T', 2, 0, 0, A, 1, TQ, U, 2, W, 1, info);
        if (info[0] != -8) {
            Preferences.debug("dormbr('Q', 'L', 'T', 2, 0, 0, A, 1, TQ, U, 2, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -8\n");
            npass--;
        }
        
        dormbr('Q', 'R', 'T', 0, 2, 0, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -8) {
            Preferences.debug("dormbr('Q', 'R', 'T', 0, 2, 0, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -8\n");
            npass--;
        }
        
        dormbr('P', 'L', 'T', 2, 0, 2, A, 1, TQ, U, 2, W, 1, info);
        if (info[0] != -8) {
            Preferences.debug("dormbr('P', 'L', 'T', 2, 0, 2, A, 1, TQ, U, 2, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -4\n");
            npass--;
        }
        
        dormbr('P', 'R', 'T', 0, 2, 2, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -8) {
            Preferences.debug("dormbr('P', 'R', 'T', 0, 2, 2, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -4\n");
            npass--;
        }
        
        dormbr('Q', 'R', 'T', 2, 0, 0, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -11) {
            Preferences.debug("dormbr('Q', 'R', 'T', 2, 0, 0, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -11\n");
            npass--;
        }
        
        dormbr('Q', 'L', 'T', 0, 2, 0, A, 1, TQ, U, 1, W, 1, info);
        if (info[0] != -13) {
            Preferences.debug("dormbr('Q', 'L', 'T', 0, 2, 0, A, 1, TQ, U, 1, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -13\n");
            npass--;
        }
        
        dormbr('Q', 'R', 'T', 2, 0, 0, A, 1, TQ, U, 2, W, 1, info);
        if (info[0] != -13) {
            Preferences.debug("dormbr('Q', 'R', 'T', 2, 0, 0, A, 1, TQ, U, 2, W, 1, info) produced info[0] = " + info[0] +
                              " instead of -13\n");
            npass--;
        }
        
        // DBDSQR
        dbdsqr('/', 0, 0, 0, 0, D, E, V, 1, U, 1, A, 1, W, info);
        if (info[0] != -1) {
            Preferences.debug("dbdsqr('/', 0, 0, 0, 0, D, E, V, 1, U, 1, A, 1, W, info) produced info[0] = " + info[0] +
                              " instead of -1\n");
            npass--;
        }
        
        dbdsqr('U', -1, 0, 0, 0, D, E, V, 1, U, 1, A, 1, W, info);
        if (info[0] != -2) {
            Preferences.debug("dbdsqr('U', -1, 0, 0, 0, D, E, V, 1, U, 1, A, 1, W, info) produced info[0] = " + info[0] +
                              " instead of -2\n");
            npass--;
        }
        
        dbdsqr('U', 0, -1, 0, 0, D, E, V, 1, U, 1, A, 1, W, info);
        if (info[0] != -3) {
            Preferences.debug("dbdsqr('U', 0, -1, 0, 0, D, E, V, 1, U, 1, A, 1, W, info) produced info[0] = " + info[0] +
                              " instead of -3\n");
            npass--;
        }
        
        dbdsqr('U', 0, 0, -1, 0, D, E, V, 1, U, 1, A, 1, W, info);
        if (info[0] != -4) {
            Preferences.debug("dbdsqr('U', 0, 0, -1, 0, D, E, V, 1, U, 1, A, 1, W, info) produced info[0] = " + info[0] +
                              " instead of -4\n");
            npass--;
        }
        
        dbdsqr('U', 0, 0, 0, -1, D, E, V, 1, U, 1, A, 1, W, info);
        if (info[0] != -5) {
            Preferences.debug("dbdsqr('U', 0, 0, 0, -1, D, E, V, 1, U, 1, A, 1, W, info) produced info[0] = " + info[0] +
                              " instead of -5\n");
            npass--;
        }
        
        dbdsqr('U', 2, 1, 0, 0, D, E, V, 1, U, 1, A, 1, W, info);
        if (info[0] != -9) {
            Preferences.debug("dbdsqr('U', 2, 1, 0, 0, D, E, V, 1, U, 1, A, 1, W, info) produced info[0] = " + info[0] +
                              " instead of -9\n");
            npass--;
        }
        
        dbdsqr('U', 0, 0, 2, 0, D, E, V, 1, U, 1, A, 1, W, info);
        if (info[0] != -11) {
            Preferences.debug("dbdsqr('U', 0, 0, 2, 0, D, E, V, 1, U, 1, A, 1, W, info) produced info[0] = " + info[0] +
                              " instead of -11\n");
            npass--;
        }
        
        dbdsqr('U', 2, 0, 0, 1, D, E, V, 1, U, 1, A, 1, W, info);
        if (info[0] != -13) {
            Preferences.debug("dbdsqr('U', 2, 0, 0, 1, D, E, V, 1, U, 1, A, 1, W, info) produced info[0] = " + info[0] +
                              " instead of -13\n");
            npass--;
        }
        
        Preferences.debug("derrbd correctly found " + npass + " of " + ntotal + " error exits\n");
        return;
    } // derrbd
    
    /**
     * This is a port of version 3.1 LAPACK test routine DLATMR Original DLATMR created by Univ. of Tennessee, Univ. of
     * California Berkeley, and NAG Ltd., November, 2006
     * dlatmr generates random matrices of various types for testing LAPACK programs. dlatmr operates by applying the
     * following sequence of operations: 1.) Generate a matrix A with random entries of distribution dist which is
     * symmetric if sym = 'S' and nonsymmetric if sym = 'N'. 2.) Set the diagonal to D, where D may be input or computed
     * according to mode, cond, dmax, and rsign as described below. 3.) Grade the matrix, if desired, from the left
     * and/or right as specified by grade. The inputs dl, model, condl, dr, moder, and condr also determine the grading
     * as described below. 4.) Permute, if desired, the rows and/or columns as specified by pivtng and ipivot. 5.) Set
     * random entries to zero, if desired, to get a random sparse matrix as specified by sparse. 6.) Make A a band
     * matrix, if desired, by zeroing out the matrix outside a band of lower bandwidth kl and upper bandwidth ku. 7.)
     * Scale A, if desired, to have maximum entry anorm 8.) Pack the matrix if desired. Options specified by pack are:
     * no packing, zero out upper half (if symmetric), zero out lower half (if symmetric), store the upper half columnwise
     * (if symmetric or square upper triangular), store the lower half columnwise (if symmetric or square lower
     * triangular), same as upper half rowwise if symmetric, store the lower triangle in banded format (if symmetric),
     * store the upper triangle if banded format (if symmetric), and store the entire matrix in banded format
     *
     * <p>Note: If two calls to dlatmr differ only in the pack parameter, they will generate mathematically equivalent
     * matrices.</p>
     *
     * <p>If two calls to dlatmr both have full bandwidth (kl = m-1 and ku = n-1), and differ only in the pivtng and
     * pack parameters, then the matrices generated will differ only in the order of the rows and/or columns, and
     * otherwise contain the same data. This consistency cannot be and is not maintained with less than full bandwidth.
     * </p>
     *
     * @param  m       input int Number of rows of A.
     * @param  n       input int Number of columns of A.
     * @param  dist    input char On entry, dist specifies the type of distribution to be used to generate a random
     *                 matrix. 
     *                 'U' => uniform(0,1) ('U' for uniform) 
     *                 'S' => uniform(-1,1) ('S' for symmetric) 
     *                 'N' => normal(0,1) ('N' for normal)
     * @param  iseed   input/output int[] of dimension 4 On entry iseed specifies the seed of the random number
     *                 generator. They should lie between 0 and 4095 inclusive, and iseed[3] should be odd. The random
     *                 number generator uses a linear congruential sequence limited to small integers, and so should
     *                 produce machine independent random numbers. The values of iseed are changed on exit, and can be
     *                 used in the next call to dlatmr to continue the same random number sequence.
     * @param  sym     input char 
     *                 If sym = 'S' or 'H', generated matrix is symmetric. 
     *                 If sym = 'N', generated matrix is nonsymmetric.
     * @param  D       (input/output) double[] of dimension (min(m,n)) On entry this array specifies the diagonal
     *                 entries of the diagonal of A. D may either be specified on entry, or set according to mode and
     *                 cond as described below. May be changed on exit if mode is nonzero.
     * @param  mode    input int On entry describes how D is to be used: 
     *                 = 0 means use D as input 
     *                 = 1 sets D[0] = 1 and D[1:n-1] = 1.0/cond 
     *                 = 2 sets D[0:n-2] = 1 and D[n-1] = 1.0/cond 
     *                 = 3 sets D[i] = cond**(-(i)/(n-1)) 
     *                 = 4 sets D[i] = 1 - (i)/(n-1)*(1 - 1/cond) 
     *                 = 5 sets D to random numbers in the range (1/cond, 1) such that their logarithms are uniformly distributed.
     *                 = 6 sets D to random numbers from the same distribution as the rest of the matrix. 
     *                 < 0 has the same meaning as abs(mode), except that the order of the elements of D is reversed.
     *                    Thus, if mode is positive, D has entries ranging from 1 to 1/cond, and if negative, from 1/cond to 1.
     * @param  cond    input double On entry, used as described under mode above. If used, it must be >= 1.
     * @param  dmax    input double If mode is neither -6, 0, nor 6, the diagonal is scaled by dmax/max(abs(D[i])), so
     *                 that maximum absolute entry of diagonal is abs(dmax). If dmax is negative (or zero), diagonal
     *                 will be scaled by a negative number (or zero).
     * @param  rsign   input char If mode is neither -6, 0, nor 6, specifies sign of diagonal as follows: 
     *                 'T' => diagonal entries are multiplied by 1 or -1 with probability 0.5. 
     *                 'F' => diagonal unchanged
     * @param  grade   input char Specifies grading of matrix as follows: 
     *                 'N' => no grading 
     *                 'L' => matrix premultiplied by diag(dl) (only if matrix nonsymmetric) 
     *                 'R' => matrix postmultiplied by diag(dr) (only if matrix nonsymmetric) 
     *                 'B' => matrix premultiplied by diag(dl) and postmultiplied by diag(dr) (only if matrix nonsymmetric)
     *                 'S' or 'H' => matrix premultiplied by diag(dl) and postmultiplied by diag(dl) 
     *                              ('S' for symmetric, or 'H' for Hermitian)
     *                 'E' => matrix premultiplied for diag(dl) and postmultiplied by inv(diag(dl))
     *                        ( 'E' for eigenvalue invariance) (only if matrix nonsymmetric)
     *                        Note: If grade = 'E', then m must equal n.
     * @param  dl      input/output double[] of dimension m If model = 0, then on entry this array specifies the
     *                 diagonal entries of a diagonal matrix used as described under grade above. If model is not zero,
     *                 then dl will be set according to model and condl, analagous to the way D is set according to mode
     *                 and cond (except that there is no dmax parameter for dl). If grade = 'E', then dl cannot have
     *                 zero entries. Not referenced if grade = 'N' or 'R'.
     * @param  model   input int This specifies how the diagonal array dl is to be computed, just as mode specifies how
     *                 D is to be computed.
     * @param  condl   input double When model is not zero, this specifies the condition number of the computed dl.
     * @param  dr      input/output double[] of dimension n. If moder = 0, then on entry this array specifies the
     *                 diaognal entries of a diagonal matrix used as described under grade above. If moder is not zero,
     *                 then dr will be set according to moder and condr, analagous to the way D is set according to mode
     *                 and cond (except that there is no dmax parameter for dr). Not referenced if grade = 'N', 'L',
     *                 'H', 'S', or 'E'.
     * @param  moder   input int This specifies how the diagonal array dr is to be computed, just as mode specifies how
     *                 D is to be computed.
     * @param  condr   input double While moder is not zero, this specifies the condition number of the computed dr.
     * @param  pivtng  input char On entry specifies the pivoting permutations as follows:
     *                 'N' or ' ' => none 
     *                 'L' => left or row pivoting (matrix must be nonsymmetric). 
     *                 'R' => right or column pivoting (matrix must be nonsymmetric). 
     *                 'B' or 'F' => both or full pivoting, i.e., on both sides. In this case, m must equal n.]
     *                 
     *                 If two calls to dlatmr both have full bandwidth (kl = m-1 and ku = n-1), and differ only
     *                 in the pivtng and pack parameters, then the matrices generated will differ only in the order of
     *                 the rows and/or the columns, and otherwise contain the same data. This consistency cannot be
     *                 maintained with less than full bandwidth.
     * @param  ipivot  input int[] of dimension m or n. This array specifies the permutation used. After the basic
     *                 matrix is generated, the rows, columns, or both are permuted. If, say, row pivoting is selected,
     *                 dlatmr starts with the *last* row and interchanges the m-th and ipivot(m)-th rows, then moves to
     *                 the next-to-last row, interchanging the (m-1)-th and the ipivot(m-1)-th rows, and so on. In terms
     *                 of "2-cycles", the permutation is (1 ipivot[0]) (2 ipivot[1]) ... (m ipivot[m-1]) where the
     *                 rightmost cycle is applied first. This is the *inverse* of the effect of pivoting in LINPACK. The
     *                 idea is that factoring (with pivoting) an identity matrix which has been inverse-pivoted in this
     *                 way should result in a pivot vector identical to ipivot. Not referenced if pivtng = 'N'.
     * @param  kl      input int On entry, specifies the lower bandwidth of the matrix. For example, kl = 0 implies
     *                 upper triangular, kl = 1 implies upper Hessenberg, and kl at least m-1 implies the matrix is
     *                 not banded. Must equal ku if matrix is symmetric.
     * @param  ku      input int On entry specifies the upper bandwidth of the matrix. For example, ku = 0 implies lower
     *                 triangular, ku = 1 implies lower Hessenberg, and ku at least n-1 implies the matrix is not
     *                 banded. Must equal kl if the matrix is symmetric.
     * @param  sparse  input double On entry specifies the sparsity of the matrix if a sparse matrix is to be generated.
     *                 sparse should lie between 0 and 1. To generate a sparse matrix, for each matrix entry a uniform
     *                 (0,1) random number x is generated and compared to sparse; if x is larger the matrix entry is
     *                 unchanged and if x is smaller the entry is set to zero. Thus on average a fraction sparse of the
     *                 entries will be set to zero.
     * @param  anorm   input double On entry specifies the maximum entry of output matrix (output matrix will be
     *                 multiplied by a constant so that its largest absolute entry equals anorm) if anorm is
     *                 nonnegative. If anorm is negative, no scaling is done.
     * @param  pack    input char On entry specifies packing of matrix as follows: 
     *                 'N' => no packing 
     *                 'U' => zero out all subdiagonal entries (if symmetric) 
     *                 'L' => zero out all superdiagonal entries (if symmetric) 
     *                 'C' => store the upper triangle columnwise (only if matrix symmetric or square upper triangular) 
     *                 'R' => store the lower triangle columnwise (only if matrix symmetric or square lower triangular)
     *                       (same as upper half rowwise if symmetric)
     *                 'B' => store the lower triangle in band storage scheme (only if matrix symmetric)
     *                 'Q' => store the upper triangle in band storage scheme (only if matrix symmetric) 
     *                 'Z' => store the entire matrix in band storage scheme (pivoting can be provided for by
     *                        using this option to store A in the trailing rows of the allocated storage)
     *                        
     *                 Using these options, the various LAPACK packed and banded storage schemes can be obtained: 
     *                 GB - use 'Z' 
     *                 PB, SB, or TB - use 'B' or 'Q' 
     *                 PP, SP, or TP - use 'C' or 'R' 
     *                 
     *                 If two calls to dlatmr differ only in the pack parameter, they will generate mathematically
     *                 equivalent matrices.
     * @param  A       output double[][] of dimension (lda, n) On exit A is the desired test matrix. Only those entries
     *                 of A which are significant on output will be referenced (even if A is in packed or band storage
     *                 format). The 'unoccupied corners' of A in band format will be zeroed out.
     * @param  lda     input int On entry lda specifies the first dimension of A as declared in the calling program. 
     *                 If pack = 'N', 'U', or 'L', lda must be at least max(1,m). 
     *                 If pack = 'C' or 'R', lda must be at least 1. 
     *                 If pack = 'B' or 'Q', lda must be at least min (ku+1,n).
     *                 If pack = 'Z', lda must be at least kuu+kll+1, where kuu = min(ku,n-1) and kll = min(kl,n-1).
     * @param  iwork   workspace int[] of dimension m or n Not referenced if pivtng = 'N'.
     * @param  info    output int[] Error parameter on exit: 
     *                 0 => normal return 
     *                 -1 => m negative or unequal to n and sym = 'S' or 'H' 
     *                 -2 => n negative 
     *                 -3 => dist illegal string 
     *                 -5 => sym illegal string 
     *                 -7 => mode not in range -6 to 6 
     *                 -8 => cond less than 1.0, and mode neither -6, 0, nor 6 
     *                 -10 => mode neither -6, 0, nor 6 and rsign illegal string 
     *                 -11 => grade illegal string, or grade = 'E' and m not equal to
     *                        n, or grade = 'L', 'R', 'B', or 'E', and sym = 'S' or 'H'
     *                 -12 => grade = 'E' and dl contains zero
     *                 -13 => model not in range -6 to 6 and grade = 'L', 'B', 'H', 'S', or 'E'
     *                 -14 => condl less than 1.0, grade = 'L', 'B', 'H', 'S', or 'E', and model neither -6, 0, nor 6. 
     *                 -16 => moder not in range -6 to 6 and grade = 'R' or 'B' 
     *                 -17 => condr less than 1.0, grade = 'R' or 'B', and moder neither -6, 0, nor 6. 
     *                 -18 => pivtng illegal string, or pivtng = 'B' or 'F' and m not equal to n, or
     *                        pivtng = 'L' or 'R' and sym = 'S' or 'H'. 
     *                 -19 => ipivot contains out of range number and pivtng not equal to 'N' 
     *                 -20 => kl negative 
     *                 -21 => ku negative, or sym = 'S' or 'H' and ku not equal to kl 
     *                 -22 => sparse not in range 0 to 1. 
     *                 -24 => pack illegal string, or pack = 'U', 'L', 'B', or 'Q' and sym = 'N', 
     *                        or pack = 'C' and sym = 'N' and either kl not equal to 0 or n not equal to m, or
     *                        pack = 'R' and sym = 'N', and either ku not equal to 0 or n not equal to m. 
     *                 -26 => lda too small
     *                 1 => Error return from dlatm1 (computing D) 
     *                 2 => Cannot scale diagonal to dmax (max. entry is 0)
     *                 3 => Error return from dlatm1 (computing dl) 
     *                 4 => Error return form dlatm1 (computing dr) 
     *                 5 => anorm is positive, but matrix, constructed prior to attempting to scale it to have
     *                      norm anorm, is zero.
     */
    private void dlatmr(int m, int n, char dist, int[] iseed, char sym, double[] D, int mode, double cond, double dmax,
                        char rsign, char grade, double[] dl, int model, double condl, double[] dr, int moder,
                        double condr, char pivtng, int[] ipivot, int kl, int ku, double sparse, double anorm, char pack,
                        double[][] A, int lda, int[] iwork, int[] info) {
        boolean badpvt;
        boolean dzero;
        boolean fulbnd;
        int i;
        int idist;
        int igrade;
        int iisub;
        int ipack;
        int ipvtng;
        int irsign;
        int[] isub = new int[1];
        int isym;
        int j;
        int jjsub;
        int[] jsub = new int[1];
        int k;
        int kll;
        int kuu;
        int mnmin;
        int mnsub;
        int mxsub;
        int npvts = 0;
        double alpha;
        double onorm = 0.0;
        double temp;
        double[] tempa = new double[1];
        double[] ap = null;

        // Decode and test the input parameters.  Initialize flags & seed.
        info[0] = 0;

        // Quick return if possible
        if ((m == 0) || (n == 0)) {
            return;
        }

        // Decode dist
        if ((dist == 'U') || (dist == 'u')) {
            idist = 1;
        } else if ((dist == 'S') || (dist == 's')) {
            idist = 2;
        } else if ((dist == 'N') || (dist == 'n')) {
            idist = 3;
        } else {
            idist = -1;
        }

        // Decode sym
        if ((sym == 'S') || (sym == 's')) {
            isym = 0;
        } else if ((sym == 'N') || (sym == 'n')) {
            isym = 1;
        } else if ((sym == 'H') || (sym == 'h')) {
            isym = 0;
        } else {
            isym = -1;
        }

        // Decode rsign
        if ((rsign == 'F') || (rsign == 'f')) {
            irsign = 0;
        } else if ((rsign == 'T') || (rsign == 't')) {
            irsign = 1;
        } else {
            irsign = -1;
        }

        // Decode pivtng
        if ((pivtng == 'N') || (pivtng == 'n')) {
            ipvtng = 0;
        } else if (pivtng == ' ') {
            ipvtng = 0;
        } else if ((pivtng == 'L') || (pivtng == 'l')) {
            ipvtng = 1;
            npvts = m;
        } else if ((pivtng == 'R') || (pivtng == 'r')) {
            ipvtng = 2;
            npvts = n;
        } else if ((pivtng == 'B') || (pivtng == 'b')) {
            ipvtng = 3;
            npvts = Math.min(n, m);
        } else if ((pivtng == 'F') || (pivtng == 'f')) {
            ipvtng = 3;
            npvts = Math.min(n, m);
        } else {
            ipvtng = -1;
        }

        // Decode grade
        if ((grade == 'N') || (grade == 'n')) {
            igrade = 0;
        } else if ((grade == 'L') || (grade == 'l')) {
            igrade = 1;
        } else if ((grade == 'R') || (grade == 'r')) {
            igrade = 2;
        } else if ((grade == 'B') || (grade == 'b')) {
            igrade = 3;
        } else if ((grade == 'E') || (grade == 'e')) {
            igrade = 4;
        } else if ((grade == 'H') || (grade == 'h') || (grade == 'S') || (grade == 's')) {
            igrade = 5;
        } else {
            igrade = -1;
        }

        // Decode pack
        if ((pack == 'N') || (pack == 'n')) {
            ipack = 0;
        } else if ((pack == 'U') || (pack == 'u')) {
            ipack = 1;
        } else if ((pack == 'L') || (pack == 'l')) {
            ipack = 2;
        } else if ((pack == 'C') || (pack == 'c')) {
            ipack = 3;
        } else if ((pack == 'R') || (pack == 'r')) {
            ipack = 4;
        } else if ((pack == 'B') || (pack == 'b')) {
            ipack = 5;
        } else if ((pack == 'Q') || (pack == 'q')) {
            ipack = 6;
        } else if ((pack == 'Z') || (pack == 'z')) {
            ipack = 7;
        } else {
            ipack = -1;
        }

        // Set certain internal parameters
        mnmin = Math.min(m, n);
        kll = Math.min(kl, m - 1);
        kuu = Math.min(ku, n - 1);

        // If inv(dl) is used, check to see if dl has a zero entry.
        dzero = false;

        if ((igrade == 4) && (model == 0)) {

            for (i = 0; i < m; i++) {

                if (dl[i] == 0.0) {
                    dzero = true;
                }
            }
        } // if ((igrade == 4) && (model == 0))

        // Check values in ipivot
        badpvt = false;

        if (ipvtng > 0) {

            for (j = 0; j < npvts; j++) {

                if ((ipivot[j] <= 0) || (ipivot[j] > npvts)) {
                    badpvt = true;
                }
            }
        } // if (ipvtng > 0)

        // Set info if an error
        if (m < 0) {
            info[0] = -1;
        } else if ((m != n) && (isym == 0)) {
            info[0] = -1;
        } else if (n < 0) {
            info[0] = -2;
        } else if (idist == -1) {
            info[0] = -3;
        } else if (isym == -1) {
            info[0] = -5;
        } else if ((mode < -6) || (mode > 6)) {
            info[0] = -7;
        } else if ((mode != -6) && (mode != 0) && (mode != 6) && (cond < 1.0)) {
            info[0] = -8;
        } else if ((mode != -6) && (mode != 0) && (mode != 6) && (irsign == -1)) {
            info[0] = -10;
        } else if ((igrade == -1) || ((igrade == 4) && (m != n)) || (((igrade >= 1) && (igrade <= 4)) && (isym == 0))) {
            info[0] = -11;
        } else if ((igrade == 4) && dzero) {
            info[0] = -12;
        } else if (((igrade == 1) || (igrade == 3) || (igrade == 4) || (igrade == 5)) &&
                       ((model < -6) || (model > 6))) {
            info[0] = -13;
        } else if (((igrade == 1) || (igrade == 3) || (igrade == 4) || (igrade == 5)) && (model != -6) &&
                       (model != 0) && (model != 6) && (condl < 1.0)) {
            info[0] = -14;
        } else if (((igrade == 2) || (igrade == 3)) && ((moder < -6) || (moder > 6))) {
            info[0] = -16;
        } else if (((igrade == 2) || (igrade == 3)) &&
                       ((moder != -6) && (moder != 0) && (moder != 6) && (condr < 1.0))) {
            info[0] = -17;
        } else if ((ipvtng == -1) || ((ipvtng == 3) && (m != n)) || (((ipvtng == 1) || (ipvtng == 2)) && (isym == 0))) {
            info[0] = -18;
        } else if ((ipvtng != 0) && badpvt) {
            info[0] = -19;
        } else if (kl < 0) {
            info[0] = -20;
        } else if ((ku < 0) || ((isym == 0) && (kl != ku))) {
            info[0] = -21;
        } else if ((sparse < 0.0) || (sparse > 1.0)) {
            info[0] = -22;
        } else if ((ipack == -1) || (((ipack == 1) || (ipack == 2) || (ipack == 5) || (ipack == 6)) && (isym == 1)) ||
                       ((ipack == 3) && (isym == 1) && ((kl != 0) || (m != n))) ||
                       ((ipack == 4) && (isym == 1) && ((ku != 0) || (m != n)))) {
            info[0] = -24;
        } else if ((((ipack == 0) || (ipack == 1) || (ipack == 2)) && (lda < Math.max(1, m))) ||
                       (((ipack == 3) || (ipack == 4)) && (lda < 1)) ||
                       (((ipack == 5) || (ipack == 6)) && (lda < (kuu + 1))) ||
                       ((ipack == 7) && (lda < (kll + kuu + 1)))) {
            info[0] = -26;
        }

        if (info[0] != 0) {
            MipavUtil.displayError("Error dlamtr had info[0] = " + info[0]);

            return;
        }

        // Decide if we can pivot consistently
        fulbnd = false;

        if ((kuu == (n - 1)) && (kll == (m - 1))) {
            fulbnd = true;
        }

        // Initialize random number generator
        for (i = 0; i < 4; i++) {
            iseed[i] = Math.abs(iseed[i]) % 4096;
        }

        iseed[3] = (2 * (iseed[3] / 2)) + 1;

        // Set up D, dl, and dr, if indicated
        // Compute D according to cond and mode
        dlatm1(mode, cond, irsign, idist, iseed, D, mnmin, info);

        if (info[0] != 0) {
            info[0] = 1;

            return;
        }

        if ((mode != 0) && (mode != -6) && (mode != 6)) {

            // Scale by dmax
            temp = Math.abs(D[0]);

            for (i = 1; i < mnmin; i++) {
                temp = Math.max(temp, Math.abs(D[i]));
            }

            if ((temp == 0.0) && (dmax != 0.0)) {
                info[0] = 2;

                return;
            }

            if (temp != 0.0) {
                alpha = dmax / temp;
            } else {
                alpha = 1.0;
            }

            for (i = 0; i < mnmin; i++) {
                D[i] = alpha * D[i];
            }
        } // if ((mode != 0) && (mode != -6) && (mode != 6))

        // Compute dl if grading set
        if ((igrade == 1) || (igrade == 3) || (igrade == 4) || (igrade == 5)) {
            dlatm1(model, condl, 0, idist, iseed, dl, m, info);

            if (info[0] != 0) {
                info[0] = 3;

                return;
            }
        } // if ((igrade == 1) || (igrade == 3) || (igrade == 4) || (igrade == 5))

        // Compute dr if grading set
        if ((igrade == 2) || (igrade == 3)) {
            dlatm1(moder, condr, 0, idist, iseed, dr, n, info);

            if (info[0] != 0) {
                info[0] = 4;

                return;
            }
        } // if ((igrade == 2) || (igrade == 3))

        // Generate iwork if pivoting
        if (ipvtng > 0) {

            for (i = 1; i <= npvts; i++) {
                iwork[i - 1] = i;
            }

            if (fulbnd) {

                for (i = 1; i <= npvts; i++) {
                    k = ipivot[i - 1];
                    j = iwork[i - 1];
                    iwork[i - 1] = iwork[k - 1];
                    iwork[k - 1] = j;
                }
            } else {

                for (i = npvts; i >= 1; i--) {
                    k = ipivot[i - 1];
                    j = iwork[i - 1];
                    iwork[i - 1] = iwork[k - 1];
                    iwork[k - 1] = j;
                }
            }
        } // if (ipvtng > 0)

        // Generate matrices for each kind of packing.  Always sweep matrix
        // columnwise (if symmetric, upper half only) so that matrix generated
        // does not depend on pack.

        if (fulbnd) {

            // Use dlatm3 so matrices generated with differing pivoting only
            // differ only in the order of their rows and/or columns
            if (ipack == 0) {

                if (isym == 0) {

                    for (j = 1; j <= n; j++) {

                        for (i = 1; i <= j; i++) {
                            temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng,
                                          iwork, sparse);
                            A[isub[0] - 1][jsub[0] - 1] = temp;
                            A[jsub[0] - 1][isub[0] - 1] = temp;
                        }
                    }
                } // if (isym == 0)
                else if (isym == 1) {

                    for (j = 1; j <= n; j++) {

                        for (i = 1; i <= m; i++) {
                            temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng,
                                          iwork, sparse);
                            A[isub[0] - 1][jsub[0] - 1] = temp;
                        }
                    }
                } // else if (isym == 1)
            } // if (ipack == 0)
            else if (ipack == 1) {

                for (j = 1; j <= n; j++) {

                    for (i = 1; i <= j; i++) {
                        temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                      sparse);
                        mnsub = Math.min(isub[0], jsub[0]);
                        mxsub = Math.max(isub[0], jsub[0]);
                        A[mnsub - 1][mxsub - 1] = temp;

                        if (mnsub != mxsub) {
                            A[mxsub - 1][mnsub - 1] = 0.0;
                        }
                    }
                }
            } // else if (ipack == 1)
            else if (ipack == 2) {

                for (j = 1; j <= n; j++) {

                    for (i = 1; i <= j; i++) {
                        temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                      sparse);
                        mnsub = Math.min(isub[0], jsub[0]);
                        mxsub = Math.max(isub[0], jsub[0]);
                        A[mxsub - 1][mnsub - 1] = temp;

                        if (mnsub != mxsub) {
                            A[mnsub - 1][mxsub - 1] = 0.0;
                        }
                    }
                }
            } // else if (ipack == 2)
            else if (ipack == 3) {
                ap = new double[n * (n + 1) / 2];

                for (j = 1; j <= n; j++) {

                    for (i = 1; i <= j; i++) {
                        temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                      sparse);

                        // Compute k = location of (isub,jsub) entry in
                        // packed array
                        mnsub = Math.min(isub[0], jsub[0]);
                        mxsub = Math.max(isub[0], jsub[0]);
                        k = (mxsub * (mxsub - 1) / 2) + mnsub;

                        // Convert k to (iisub,jjsub) location
                        jjsub = ((k - 1) / lda) + 1;
                        iisub = k - (lda * (jjsub - 1));
                        A[iisub - 1][jjsub - 1] = temp;
                        ap[k - 1] = temp;
                    }
                }
            } // else if (ipack == 3)
            else if (ipack == 4) {
                ap = new double[n * (n + 1) / 2];

                for (j = 1; j <= n; j++) {

                    for (i = 1; i <= j; i++) {
                        temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                      sparse);

                        // Compute k = location of (isub,jsub) entry in
                        // packed array
                        mnsub = Math.min(isub[0], jsub[0]);
                        mxsub = Math.max(isub[0], jsub[0]);

                        if (mnsub == 1) {
                            k = mxsub;
                        } else {
                            k = (n * (n + 1) / 2) - ((n - mnsub + 1) * (n - mnsub + 2) / 2) + mxsub - mnsub + 1;
                        }

                        // Convert k to (iisub,jjsub) location
                        jjsub = ((k - 1) / lda) + 1;
                        iisub = k - (lda * (jjsub - 1));
                        A[iisub - 1][jjsub - 1] = temp;
                        ap[k - 1] = temp;
                    }
                }
            } // else if (ipack == 4)
            else if (ipack == 5) {

                for (j = 1; j <= n; j++) {

                    for (i = j - kuu; i <= j; i++) {

                        if (i < 1) {
                            A[j - i][i + n - 1] = 0.0;
                        } else {
                            temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng,
                                          iwork, sparse);
                            mnsub = Math.min(isub[0], jsub[0]);
                            mxsub = Math.max(isub[0], jsub[0]);
                            A[mxsub - mnsub][mnsub - 1] = temp;
                        }
                    }
                }
            } // else if (ipack == 5)
            else if (ipack == 6) {

                for (j = 1; j <= n; j++) {

                    for (i = j - kuu; i <= j; i++) {
                        temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                      sparse);
                        mnsub = Math.min(isub[0], jsub[0]);
                        mxsub = Math.max(isub[0], jsub[0]);
                        A[mnsub - mxsub + kuu][mxsub - 1] = temp;
                    }
                }
            } // else if (ipack == 6)
            else if (ipack == 7) {

                if (isym == 0) {

                    for (j = 1; j <= n; j++) {

                        for (i = j - kuu; i <= j; i++) {
                            temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng,
                                          iwork, sparse);
                            mnsub = Math.min(isub[0], jsub[0]);
                            mxsub = Math.max(isub[0], jsub[0]);
                            A[mnsub - mxsub + kuu][mxsub - 1] = temp;

                            if (i < 1) {
                                A[j - i + kuu][i + n - 1] = 0.0;
                            }

                            if ((i >= 1) && (mnsub != mxsub)) {
                                A[mxsub - mnsub + kuu][mnsub - 1] = temp;
                            }
                        }
                    }
                } // if (isym == 0)
                else if (isym == 1) {

                    for (j = 1; j <= n; j++) {

                        for (i = j - kuu; i <= (j + kll); i++) {
                            temp = dlatm3(m, n, i, j, isub, jsub, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng,
                                          iwork, sparse);
                            A[isub[0] - jsub[0] + kuu][jsub[0] - 1] = temp;
                        }
                    }
                } // else if (isym == 1)
            } // else if (ipack == 7)
        } // if (fulbnd)
        else { // not fulbnd

            // Use dlatm2
            if (ipack == 0) {

                if (isym == 0) {

                    for (j = 1; j <= n; j++) {

                        for (i = 1; i <= j; i++) {
                            A[i - 1][j - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                                     sparse);
                            A[j - 1][i - 1] = A[i - 1][j - 1];
                        }
                    }
                } // if (isym == 0)
                else if (isym == 1) {

                    for (j = 1; j <= n; j++) {

                        for (i = 1; i <= m; i++) {
                            A[i - 1][j - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                                     sparse);
                        }
                    }
                } // else if (isym == 1)
            } // if (ipack == 0)
            else if (ipack == 1) {

                for (j = 1; j <= n; j++) {

                    for (i = 1; i <= j; i++) {
                        A[i - 1][j - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                                 sparse);

                        if (i != j) {
                            A[j - 1][i - 1] = 0.0;
                        }
                    }
                }
            } // else if (ipack == 1)
            else if (ipack == 2) {

                for (j = 1; j <= n; j++) {

                    for (i = 1; i <= j; i++) {
                        A[j - 1][i - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                                 sparse);

                        if (i != j) {
                            A[i - 1][j - 1] = 0.0;
                        }
                    }
                }
            } // else if (ipack == 2)
            else if (ipack == 3) {
                ap = new double[n * (n + 1) / 2];
                isub[0] = 0;
                jsub[0] = 1;

                for (j = 1; j <= n; j++) {

                    for (i = 1; i <= j; i++) {
                        isub[0] = isub[0] + 1;

                        if (isub[0] > lda) {
                            isub[0] = 1;
                            jsub[0] = jsub[0] + 1;
                        }

                        A[isub[0] - 1][jsub[0] - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr,
                                                             ipvtng, iwork, sparse);
                        k = isub[0] + (lda * (jsub[0] - 1));
                        ap[k - 1] = A[isub[0] - 1][jsub[0] - 1];
                    }
                }
            } // else if (ipack == 3)
            else if (ipack == 4) {
                ap = new double[n * (n + 1) / 2];

                if (isym == 0) {

                    for (j = 1; j <= n; j++) {

                        for (i = 1; i <= j; i++) {

                            // Compute k = location of (i,j) entry in packed
                            // array
                            if (i == 1) {
                                k = j;
                            } else {
                                k = (n * (n + 1) / 2) - ((n - i + 1) * (n - i + 2) / 2) + j - i + 1;
                            }

                            // Convert k to (isub,jsub) location
                            jsub[0] = ((k - 1) / lda) + 1;
                            isub[0] = k - (lda * (jsub[0] - 1));
                            A[isub[0] - 1][jsub[0] - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr,
                                                                 ipvtng, iwork, sparse);
                            ap[k - 1] = A[isub[0] - 1][jsub[0] - 1];
                        }
                    }
                } // if (isym == 0)
                else { // isym != 0
                    isub[0] = 0;
                    jsub[0] = 1;

                    for (j = 1; j <= n; j++) {

                        for (i = j; i <= m; i++) {
                            isub[0] = isub[0] + 1;

                            if (isub[0] > lda) {
                                isub[0] = 1;
                                jsub[0] = jsub[0] + 1;
                            }

                            A[isub[0] - 1][jsub[0] - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr,
                                                                 ipvtng, iwork, sparse);
                            k = isub[0] + (lda * (jsub[0] - 1));
                            ap[k - 1] = A[isub[0] - 1][jsub[0] - 1];
                        }
                    }
                } // else isym != 0
            } // else if (ipack == 4)
            else if (ipack == 5) {

                for (j = 1; j <= n; j++) {

                    for (i = j - kuu; i <= j; i++) {

                        if (i < 1) {
                            A[j - i][i + n - 1] = 0.0;
                        } else {
                            A[j - i][i - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng, iwork,
                                                     sparse);
                        }
                    }
                }
            } // else if (ipack == 5)
            else if (ipack == 6) {

                for (j = 1; j <= n; j++) {

                    for (i = j - kuu; i <= j; i++) {
                        A[i - j + kuu][j - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng,
                                                       iwork, sparse);
                    }
                }
            } // else if (ipack == 6)
            else if (ipack == 7) {

                if (isym == 0) {

                    for (j = 1; j <= n; j++) {

                        for (i = j - kuu; i <= j; i++) {
                            A[i - j + kuu][j - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng,
                                                           iwork, sparse);

                            if (i < 1) {
                                A[j - i + kuu][i + n - 1] = 0.0;
                            }

                            if ((i >= 1) && (i != j)) {
                                A[j - i + kuu][i - 1] = A[i - j + kuu][j - 1];
                            }
                        }
                    }
                } // if (isym == 0)
                else if (isym == 1) {

                    for (j = 1; j <= n; j++) {

                        for (i = j - kuu; i <= (j + kll); i++) {
                            A[i - j + kuu][j - 1] = dlatm2(m, n, i, j, kl, ku, idist, iseed, D, igrade, dl, dr, ipvtng,
                                                           iwork, sparse);
                        }
                    }
                } // else if (isym == 1)
            } // else if (ipack == 7)
        } // else not fulbnd

        // Scaling the norm
        if (ipack == 0) {
            onorm = ge.dlange('M', m, n, A, lda, tempa);
        } else if (ipack == 1) {
            onorm = dlansy('M', 'U', n, A, lda, tempa);
        } else if (ipack == 2) {
            onorm = dlansy('M', 'L', n, A, lda, tempa);
        } else if (ipack == 3) {
            onorm = dlansp('M', 'U', n, ap, tempa);
        } // else if (ipack == 3)
        else if (ipack == 4) {
            onorm = dlansp('M', 'L', n, ap, tempa);
        } // else if (ipack == 4)
        else if (ipack == 5) {
            onorm = dlansb('M', 'L', n, kll, A, lda, tempa);
        } // else if (ipack == 5)
        else if (ipack == 6) {
            onorm = dlansb('M', 'U', n, kuu, A, lda, tempa);
        } // else if (ipack == 6)
        else if (ipack == 7) {
            onorm = dlangb('M', n, kll, kuu, A, lda, tempa);
        } // else if (ipack == 7)

        if (anorm >= 0.0) {

            if ((anorm > 0.0) && (onorm == 0.0)) {

                // Desired scaling impossible
                info[0] = 5;

                return;
            } else if (((anorm > 1.0) && (onorm < 1.0)) || ((anorm < 1.0) && (onorm > 1.0))) {

                // Scale carefully to avoid over / underflow
                if (ipack <= 2) {

                    for (j = 0; j < n; j++) {

                        for (i = 0; i < m; i++) {
                            A[i][j] = (1.0 / onorm) * A[i][j];
                            A[i][j] = anorm * A[i][j];
                        }
                    }
                } // if (ipack <= 2)
                else if ((ipack == 3) || (ipack == 4)) {

                    for (i = 0; i < (n * (n + 1) / 2); i++) {
                        ap[i] = (1.0 / onorm) * ap[i];
                        ap[i] = anorm * ap[i];
                    }
                } // else if ((ipack == 3) || (ipack == 4))
                else if (ipack >= 5) {

                    for (j = 0; j < n; j++) {

                        for (i = 0; i < (kll + kuu + 1); i++) {
                            A[i][j] = (1.0 / onorm) * A[i][j];
                            A[i][j] = anorm * A[i][j];
                        }
                    }
                } // else if (ipack >= 5)
            } // else if (((anorm > 1.0) && (onorm < 1.0)) || ((anorm < 1.0) &&
            else {

                // Scale straightforwardly
                if (ipack <= 2) {

                    for (j = 0; j < n; j++) {

                        for (i = 0; i < m; i++) {
                            A[i][j] = (anorm / onorm) * A[i][j];
                        }
                    }
                } // if (ipack <= 2)
                else if ((ipack == 3) || (ipack == 4)) {

                    for (i = 0; i < (n * (n + 1) / 2); i++) {
                        ap[i] = (anorm / onorm) * ap[i];
                    }
                } // else if ((ipack == 3) || (ipack == 4))
                else if (ipack >= 5) {

                    for (j = 0; j < n; j++) {

                        for (i = 0; i < (kll + kuu + 1); i++) {
                            A[i][j] = (anorm / onorm) * A[i][j];
                        }
                    }
                } // else if (ipack >= 5)
            } // else
        } // if (anorm >= 0.0)

        if (ipack == 3) {
            isub[0] = 0;
            jsub[0] = 1;

            for (j = 1; j <= n; j++) {

                for (i = 1; i <= j; i++) {
                    isub[0] = isub[0] + 1;

                    if (isub[0] > lda) {
                        isub[0] = 1;
                        jsub[0] = jsub[0] + 1;
                    }

                    k = isub[0] + (lda * (jsub[0] - 1));
                    A[isub[0] - 1][jsub[0] - 1] = ap[k - 1];
                }
            }
        } // else if (ipack == 3)
        else if (ipack == 4) {

            if (isym == 0) {

                for (j = 1; j <= n; j++) {

                    for (i = 1; i <= j; i++) {

                        // Compute k = location of (i,j) entry in packed
                        // array
                        if (i == 1) {
                            k = j;
                        } else {
                            k = (n * (n + 1) / 2) - ((n - i + 1) * (n - i + 2) / 2) + j - i + 1;
                        }

                        // Convert k to (isub,jsub) location
                        jsub[0] = ((k - 1) / lda) + 1;
                        isub[0] = k - (lda * (jsub[0] - 1));
                        A[isub[0] - 1][jsub[0] - 1] = ap[k - 1];
                    }
                }
            } // if (isym == 0)
            else { // isym != 0
                isub[0] = 0;
                jsub[0] = 1;

                for (j = 1; j <= n; j++) {

                    for (i = j; i <= m; i++) {
                        isub[0] = isub[0] + 1;

                        if (isub[0] > lda) {
                            isub[0] = 1;
                            jsub[0] = jsub[0] + 1;
                        }

                        k = isub[0] + (lda * (jsub[0] - 1));
                        A[isub[0] - 1][jsub[0] - 1] = ap[k - 1];
                    }
                }
            } // else isym != 0
        } // else if (ipack == 4)
        return;
    } // dlatmr
    
    /**
     * This is a port of version 3.1 LAPACK auxiliary test routine DLATM2 Original DLATM2 created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., November, 2006
     * dlatm2 returns the (i,j) entry of a random matrix of dimension (m,n) described by the other parameters.
     * It is called by the dlatmr routine in order to build random test matrices. No error checking on parameters is
     * done, because this routine is called in a tight loop by dlatmr which has already checked the parameters.
     *
     * <p>Use of dlatm2 differs from dlatm3 in the order in which the random number generator is called to fill in
     * random matrix entries. With dlatm2, the generator is called to fill in the pivoted matrix columnwise. With
     * dlatm3, the generator is called to fill in the matrix columnwise, after which it is pivoted. Thus, dlatm3 can be
     * used to construct random matrices which differ only in their order of rows and/or columns. dlatm2 is used to
     * construct band matrices while avoiding calling the random number generator for entries outisde the band (and
     * therefore generating random numbers).</p>
     *
     * <p>The matrix whose (i,j) entry is returned is constructed as follows (this routine only computes one entry):</p>
     *
     * <p>If i is outside (1..m) or j is outside (1..n), return zero. (This is convenient for generating matrices in
     * band format).</p>
     *
     * <p>Generate a matrix A with random entries of distribution idist.</p>
     *
     * <p>Set the diagonal to D.</p>
     *
     * <p>Grade the matrix, if desired, from the left (by dl) and/or from the right (by dr or dl) as specified by
     * igrade.</p>
     *
     * <p>Permute, if desired, the rows and/or columns as specified by ipvtng and iwork.</p>
     *
     * <p>Band the matrix to have lower bandwidth kl and upper bandwidth ku.</p>
     *
     * <p>Set random entries to zero as specified by sparse.</p>
     *
     * @param   m       input int Number of rows of the matrix.
     * @param   n       input int Number of columns of the matrix.
     * @param   i       input int Row of entry to be returned.
     * @param   j       input int Column of entry to be returned.
     * @param   kl      input int Lower bandwidth.
     * @param   ku      input int Upper bandwidth.
     * @param   idist   input int On entry, idist specifies the type of distribution to be used to generate a random
     *                  matrix. 
     *                  1 => uniform(0,1) 
     *                  2 => uniform(-1,1) 
     *                  3 => normal(0,1)
     * @param   iseed   input/output int[] of dimension 4 Seed for random number generator. Changed on exit.
     * @param   D       input double[] of dimension min(i,j) Diagonal entries of matrix.
     * @param   igrade  input int Specifies grading of matrix as follows: 
     *                  0 => no grading 
     *                  1 => matrix premultiplied by diag(dl) 
     *                  2 => matrix postmultiplied by diag(dr) 
     *                  3 => matrix premultiplied by diag(dl) and postmultiplied by diag(dr) 
     *                  4 => matrix premultiplied by diag(dl) and postmultiplied by inv(diag(dl)) 
     *                  5 => matrix premultiplied by diag(dl) and postmultiplied by diag(dl)
     * @param   dl      input double[] of dimension i or j, as appropriate Left scale factors for grading matrix.
     * @param   dr      input double[] of dimension i or j, as appropriate Right scale factors for grading matrix.
     * @param   ipvtng  input int On entry specifies pivoting permutations as follows: 
     *                  0 => none. 
     *                  1 => row pivoting. 
     *                  2 => column pivoting. 
     *                  3 => full pivoting, i.e., on both sides
     * @param   iwork   input int[] of dimension i or j, as appropriate This array specifies the permutation used. The
     *                  row (or column) in position k was originally in position iwork[k-1]. This differs from iwork for
     *                  dlatm3.
     * @param   sparse  input double between 0.0 and 1.0. On entry specifies the sparsity of the matrix if sparse matrix
     *                  is to be generated. A uniform (0,1) random number x is generated and compared to sparse; if x is
     *                  larger the matrix entry is unchanged and if x is smaller the entry is set to zero. Thus on
     *                  average a fraction sparse of the entries will be set to zero.
     *
     * @return  double
     */
    private double dlatm2(int m, int n, int i, int j, int kl, int ku, int idist, int[] iseed, double[] D, int igrade,
                          double[] dl, double[] dr, int ipvtng, int[] iwork, double sparse) {
        int isub = 0;
        int jsub = 0;
        double temp;

        // Check for i and j in range
        if ((i < 1) || (i > m) || (j < 1) || (j > n)) {
            return 0.0;
        }

        // Check for banding
        if ((j > (i + ku)) || (j < (i - kl))) {
            return 0.0;
        }

        // Check for sparsity
        if (sparse > 0.0) {

            if (dlaran(iseed) < sparse) {
                return 0.0;
            }
        }

        // Compute subscripts depending on ipvtng
        if (ipvtng == 0) {
            isub = i;
            jsub = j;
        } else if (ipvtng == 1) {
            isub = iwork[i - 1];
            jsub = j;
        } else if (ipvtng == 2) {
            isub = i;
            jsub = iwork[j - 1];
        } else if (ipvtng == 3) {
            isub = iwork[i - 1];
            jsub = iwork[j - 1];
        }

        // Compute entry and grade it according to igrade
        if (isub == jsub) {
            temp = D[isub - 1];
        } else {
            temp = dlarnd(idist, iseed);
        }

        if (igrade == 1) {
            temp = temp * dl[isub - 1];
        } else if (igrade == 2) {
            temp = temp * dr[jsub - 1];
        } else if (igrade == 3) {
            temp = temp * dl[isub - 1] * dr[jsub - 1];
        } else if ((igrade == 4) && (isub != jsub)) {
            temp = temp * dl[isub - 1] / dl[jsub - 1];
        } else if (igrade == 5) {
            temp = temp * dl[isub - 1] * dl[jsub - 1];
        }

        return temp;
    } // dlatm2
    
    /**
     * This is the port of the version 3.1 LAPACK auxiliary test routine DLATM3 Original DLATM3 created by Univ. of
     * Tennessee, Univ. of California Berkeley, and NAG Ltd., November, 2006
     * dlatm3 returns the (isub, jsub) entry of a random matrix of dimension (m,n) described by the
     * other parameters. (isub, jsub) is the final position of the (i,j) entry after pivoting according to ipvtng and
     * iwork. dlatm3 is called by the dlatmr routine in order to build random test matrices. No error checking is done,
     * because this routine is called in a tight loop by dlatmr which has already checked the parameters.
     *
     * <p>Use of dlatm3 differs from dlatm2 in the order in which the random number generator is called to fill in
     * random matrix entries. With dlatm2, the generator is called to fill in the pivoted matrix columnwise. With
     * dlatm3, the generator is called to fill in the matrix columnwise, after which it is pivoted. Thus, dlatm3 can be
     * used to construct random matrices which differ only in their order of rows and/or columns. dlatm2 is used to
     * construct band matrices while avoiding calling the random number generator for entries outside the band (and
     * therefore generating random numbers in different orders for different pivot orders).</p>
     *
     * <p>The matrix whose (isub, jsub) entry is returned is constructed as follows (this routine only computes one
     * entry):</p>
     *
     * <p>If isub is outside (1...m) or jsub is outside (1...n), return zero. (this is convenient for generating
     * matrices in band format).</p>
     *
     * <p>Generate a matrix A with random entries of distribution idist.</p>
     *
     * <p>Set the diagonal to D.</p>
     *
     * <p>Grade the matrix, if desired, from the left (by dl) and/or from the right (by dr or dl) as specified by
     * igrade.</p>
     *
     * <p>Permute, if desired, the rows and/or columns as specified by ipvtng and iwork.</p>
     *
     * <p>Band the matrix to have lower bandwidth kl and upper bandwidth ku.</p>
     *
     * <p>Set random entries to zero as specified by sparse.</p>
     *
     * @param   m       input int Number of rows of matrix
     * @param   n       input int Number of columns of matrix.
     * @param   i       input int Row of unpivoted entry to be returned.
     * @param   j       input int Column of unpivoted entry to be returned.
     * @param   isub    output int[] Row of pivoted entry to be returned.
     * @param   jsub    output int[] Column of pivoted entry to be returned.
     * @param   kl      input int Lower bandwidth
     * @param   ku      input int Upper bandwidth
     * @param   idist   input int On entry, idist specifies the type of distribution to be used to generate a random
     *                  matrix. 
     *                  1 => uniform (0,1) 
     *                  2 => uniform (-1,1) 
     *                  3 => normal (0,1)
     * @param   iseed   input/output int[] of dimension 4 Seed for random number generator. Changed on exit.
     * @param   D       input double[] of dimension min(i,j). Diagonal entries of matrix.
     * @param   igrade  input int Specifies the grading of the matrix as follows: 
     *                  0 => no grading 
     *                  1 => matrix premultiplied by diag (dl) 
     *                  2 => matrix postmultiplied by diag (dr) 
     *                  3 => matrix premultiplied by diag (dl) and postmultiplied by diag (dr) 
     *                  4 => matrix premultiplied by diag (dl) and postmultiplied by inv(diag(dl)) 
     *                  5 => matrix premultiplied by diag(dl) and postmultiplied by diag(dl)
     * @param   dl      input double[] of dimension i or j, as appropriate Left scale factors for grading matrix.
     * @param   dr      input double[] of dimension i or j, as appropriate Right scale factors for grading matrix.
     * @param   ipvtng  input int On entry specifies the pivoting permutations as follows: 
     *                  0 => none 
     *                  1 => row pivoting 
     *                  2 => column pivoting 
     *                  3 => full pivoting, i.e., on both sides
     * @param   iwork   input int[] of dimension i or j, as appropriate This array specifies the permutation used. The
     *                  row (or column) originally in position k is in position iwork[k-1] after pivoting. This differs
     *                  from iwork for dlatm2.
     * @param   sparse  input double between 0.0 and 1.0. On entry specifies the sparsity of the matrix if sparse matrix
     *                  is to be generated. A uniform (0,1) random number x is generated and compared to sparse; if x is
     *                  larger the matrix entry is unchanged and if x is smaller the entry is set to zero. Thus on the
     *                  average a fraction sparse of the entries will be set to zero.
     *
     * @return  double
     */
    private double dlatm3(int m, int n, int i, int j, int[] isub, int[] jsub, int kl, int ku, int idist, int[] iseed,
                          double[] D, int igrade, double[] dl, double[] dr, int ipvtng, int[] iwork, double sparse) {
        double temp;

        // Check for i and j in range
        if ((i < 1) || (i > m) || (j < 1) || (j > n)) {
            isub[0] = i;
            jsub[0] = j;

            return 0.0;
        }

        // Compute subscripts depending on ipvtng
        if (ipvtng == 0) {
            isub[0] = i;
            jsub[0] = j;
        } else if (ipvtng == 1) {
            isub[0] = iwork[i - 1];
            jsub[0] = j;
        } else if (ipvtng == 2) {
            isub[0] = i;
            jsub[0] = iwork[j - 1];
        } else if (ipvtng == 3) {
            isub[0] = iwork[i - 1];
            jsub[0] = iwork[j - 1];
        }

        // Check for banding
        if ((jsub[0] > (isub[0] + ku)) || (jsub[0] < (isub[0] - kl))) {
            return 0.0;
        }

        // Check for sparsity
        if (sparse > 0.0) {

            if (dlaran(iseed) < sparse) {
                return 0.0;
            }
        }

        // Compute entry and grade it according it according to igrade
        if (i == j) {
            temp = D[i - 1];
        } else {
            temp = dlarnd(idist, iseed);
        }

        if (igrade == 1) {
            temp = temp * dl[i - 1];
        } else if (igrade == 2) {
            temp = temp * dr[j - 1];
        } else if (igrade == 3) {
            temp = temp * dl[i - 1] * dr[j - 1];
        } else if ((igrade == 4) && (i != j)) {
            temp = temp * dl[i - 1] / dl[j - 1];
        } else if (igrade == 5) {
            temp = temp * dl[i - 1] * dl[j - 1];
        }

        return temp;
    } // dlatm3
    
    /**
     * This is a port of version 3.2 LAPACK auxiliary routine DLANSB Original DLANSB created by Univ. of Tennessee, Univ.
     * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlansb retruns the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest
     * absolute value of an n by n symmetric band matrix A, with k super-diagonals.
     *
     * @param   norm  input char Specifies the value to be returned as: 
     *                = 'M' or 'm', returns max(abs(A[i][j])), which is not a matrix norm 
     *                = '1', 'O', or 'o', returns norm1(A), the one norm of a matrix (maximum column sum) 
     *                = 'I' or 'i', returns normI(A), the inifinity norm of a matrix (maximum row sum) 
     *                = 'F', 'f', 'E', or 'e', returns normF(A), the Frobenius norm of a matrix
     *                  (square root of sum of squares)
     * @param   uplo  input char Specifies whether the upper or lower triangular part of the band matrix A is supplied 
     *                = 'U': Upper triangular part is supplied. 
     *                = 'L': Lower triangular part is supplied.
     * @param   n     input int The order of the matrix A. n >= 0. When n = 0, dlansb returns zero.
     * @param   k     input int The number of super-diagonals or sub-diagonals of the band matrix A. k >= 0.
     * @param   ab    input double[][] of dimension (ldab,n) The upper or lower triangle of the symmetric band matrix A,
     *                stored in the first k+1 rows of ab. The j-th column of A is stored in the j-th column of the array
     *                ab as follows:
     *                If uplo = 'U', ab[k+1+i-j][j] = A[i][j] for max(0,j-k) <= i <= j
     *                If uplo = 'L', ab[1+i-j][j] = A[i][j] for j <= i <= min(n-1,j+k)
     * @param   ldab  input int The leading dimension of array ab. ldab >= k + 1.
     * @param   work  workspace double[] of dimension max(1, lwork), where lwork >= n when norm = 'I' or '1' or 'O';
     *                otherwise, work is not referenced.
     *
     * @return  DOCUMENT ME!
     */
    private double dlansb(char norm, char uplo, int n, int k, double[][] ab, int ldab, double[] work) {
        int i;
        int j;
        int L;
        double absa;
        double[] scale = new double[1];
        double[] sum = new double[1];
        double value = 0.0;
        double[] x;

        if (n == 0) {
            value = 0.0;
        } else if ((norm == 'M') || (norm == 'm')) {

            // Find max(abs(A[i][j]))
            value = 0.0;

            if ((uplo == 'U') || (uplo == 'u')) {

                for (j = 1; j <= n; j++) {

                    for (i = Math.max(k + 2 - j, 1); i <= (k + 1); i++) {
                        value = Math.max(value, Math.abs(ab[i - 1][j - 1]));
                    }
                }
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { // ((uplo == 'L') || (uplo == 'l'))

                for (j = 1; j <= n; j++) {

                    for (i = 1; i <= Math.min(n + 1 - j, k + 1); i++) {
                        value = Math.max(value, Math.abs(ab[i - 1][j - 1]));
                    }
                }
            } // else ((uplo == 'L') || (uplo == 'l'))
        } // else if ((norm == 'M') || (norm == 'm'))
        else if ((norm == 'I') || (norm == 'i') || (norm == 'O') || (norm == 'o') || (norm == '1')) {

            // Find normI(A) = norm1(A), since A is symmetric
            value = 0.0;

            if ((uplo == 'U') || (uplo == 'u')) {

                for (j = 1; j <= n; j++) {
                    sum[0] = 0.0;
                    L = k + 1 - j;

                    for (i = Math.max(1, j - k); i <= (j - 1); i++) {
                        absa = Math.abs(ab[L + i - 1][j - 1]);
                        sum[0] = sum[0] + absa;
                        work[i - 1] = work[i - 1] + absa;
                    }

                    work[j - 1] = sum[0] + Math.abs(ab[k][j - 1]);
                } // for (j = 1; j <= n; j++)

                for (i = 0; i < n; i++) {
                    value = Math.max(value, work[i]);
                }
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { // ((uplo == 'L') || (uplo == 'l'))

                for (i = 0; i < n; i++) {
                    work[i] = 0.0;
                }

                for (j = 1; j <= n; j++) {
                    sum[0] = work[j - 1] + Math.abs(ab[0][j - 1]);
                    L = 1 - j;

                    for (i = j + 1; i <= Math.min(n, j + k); i++) {
                        absa = Math.abs(ab[L + i - 1][j - 1]);
                        sum[0] = sum[0] + absa;
                        work[i - 1] = work[i - 1] + absa;
                    }

                    value = Math.max(value, sum[0]);
                } // for (j = 1; j <= n; j++)
            } // else ((uplo == 'L') || (uplo == 'l'))
        } // else if ((norm == 'I') || (norm == 'i') || (norm == 'O') ||
        else if ((norm == 'F') || (norm == 'f') || (norm == 'E') || (norm == 'e')) {

            // Find normF(A)
            scale[0] = 0.0;
            sum[0] = 1.0;

            if (k > 0) {

                if ((uplo == 'U') || (uplo == 'u')) {

                    for (j = 2; j <= n; j++) {
                        x = new double[Math.min(j - 1, k)];

                        for (i = 0; i < Math.min(j - 1, k); i++) {
                            x[i] = ab[Math.max(k + 1 - j, 0) + i][j - 1];
                        }

                        ge.dlassq(Math.min(j - 1, k), x, 1, scale, sum);
                    }

                    L = k + 1;
                } // if ((uplo == 'U') || (uplo == 'u'))
                else { // ((uplo == 'L') || (uplo == 'l'))

                    for (j = 1; j <= (n - 1); j++) {
                        x = new double[Math.min(n - j, k)];

                        for (i = 0; i <= Math.min(n - j, k); i++) {
                            x[i] = ab[i + 1][j - 1];
                        }

                        ge.dlassq(Math.min(n - j, k), x, 1, scale, sum);
                    }

                    L = 1;
                } // else ((uplo == 'L') || (uplo == 'l'))

                sum[0] = 2.0 * sum[0];
            } // if (k > 0)
            else { // k == 0
                L = 1;
            } // else k == 0

            x = new double[n];

            for (i = 0; i < n; i++) {
                x[i] = ab[L - 1][i];
            }

            ge.dlassq(n, x, 1, scale, sum);
            value = scale[0] * Math.sqrt(sum[0]);
        } // else if ((norm == 'F') || (norm == 'f') || (norm == 'E') ||

        return value;
    } // dlansb
    
    /**
     * This is a port of the version 3.2 LAPACK auxiliary routine DLANSP Original DLANSP created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlansp returns the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of
     * largest absolute value of a real symmetric matrix A, supplied in packed form.
     *
     * @param   norm  input char Specifies the value to be returned as: 
     *                = 'M' or 'm' , max(abs(A[i,j])) Note that this is not a matrix norm. 
     *                = '1', 'O', or 'o', norm1(A) where norm1 denotes the one norm of a matrix (maximum column sum) 
     *                = 'I' or 'i', normI(A) where normI denotes the infinity norm of a matrix (maximum row sum) 
     *                = 'F', 'f', 'E', or 'e', normF(A) where normF denotes the Frobenius norm of a matrix 
     *                  (square root of sum of squares)
     * @param   uplo  input char Specifies whether the upper or lower triangular part of the symmetric matrix A is
     *                supplied. 
     *                = 'U': Upper triangular part of A is supplied 
     *                = 'L': Lower triangular part of A is supplied
     * @param   n     input int The order of the matrix A. n >= 0. When n = 0, dlansp is set to zero.
     * @param   ap    input double[] of dimension (n*(n+1)/2) The upper or lower triangle of the symmetric matrix A,
     *                packed columnwise in a linear array. The j-th column of A is stored in the array AP as follows:
     *                If uplo = 'U', ap[i + (j-1)*j/2] = A[i][j] for 0 <= i <= j
     *                If uplo = 'L', ap[i + (j-1)*(2n-j)/2] = A[i][j] for j <= i <= n-1.
     * @param   work  workspace double[] of dimension max(1, lwork), where lwork >= n when norm = 'I' or '1' or 'O';
     *                otherwise work is not referenced.
     *
     * @return  double
     */
    private double dlansp(char norm, char uplo, int n, double[] ap, double[] work) {
        int i;
        int j;
        int k;
        double absa;
        double[] scale = new double[1];
        double[] sum = new double[1];
        double[] x;
        double value = 0.0;
        double ratio;

        if (n == 0) {
            value = 0.0;
        } else if ((norm == 'M') || (norm == 'm')) {

            // Find max(abs(A[i][j]))
            value = 0.0;

            if ((uplo == 'U') || (uplo == 'u')) {
                k = 1;

                for (j = 1; j <= n; j++) {

                    for (i = k; i <= (k + j - 1); i++) {
                        value = Math.max(value, Math.abs(ap[i - 1]));
                    }

                    k = k + j;
                }
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { // ((uplo == 'L') || (uplo == 'l'))
                k = 1;

                for (j = 1; j <= n; j++) {

                    for (i = k; i <= (k + n - j); i++) {
                        value = Math.max(value, Math.abs(ap[i - 1]));
                    }

                    k = k + n - j + 1;
                }
            } // else ((uplo == 'L') || (uplo == 'l'))
        } // else if ((norm == 'M') || (norm == 'm'))
        else if ((norm == 'I') || (norm == 'i') || (norm == 'O') || (norm == 'o') || (norm == '1')) {

            // Find normI(A) == norm1(A), since A is symmetric
            value = 0.0;
            k = 1;

            if ((uplo == 'U') || (uplo == 'u')) {

                for (j = 1; j <= n; j++) {
                    sum[0] = 0.0;

                    for (i = 1; i <= (j - 1); i++) {
                        absa = Math.abs(ap[k - 1]);
                        sum[0] = sum[0] + absa;
                        work[i - 1] = work[i - 1] + absa;
                        k = k + 1;
                    }

                    work[j - 1] = sum[0] + Math.abs(ap[k - 1]);
                    k = k + 1;
                } // for (j = 1; j <= n; j++)

                for (i = 0; i < n; i++) {
                    value = Math.max(value, work[i]);
                }
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { // ((uplo == 'L') || (uplo == 'l'))

                for (i = 0; i < n; i++) {
                    work[i] = 0.0;
                }

                for (j = 1; j <= n; j++) {
                    sum[0] = work[j - 1] + Math.abs(ap[k - 1]);
                    k = k + 1;

                    for (i = j + 1; i <= n; i++) {
                        absa = Math.abs(ap[k - 1]);
                        sum[0] = sum[0] + absa;
                        work[i - 1] = work[i - 1] + absa;
                        k = k + 1;
                    } // for (i = j+1; i <= n; i++)

                    value = Math.max(value, sum[0]);
                } // for (j = 1; j <= n; j++)
            } // else ((uplo == 'L') || (uplo == 'l'))
        } // else if ((norm == 'I') || (norm == 'i') || (norm == 'O') ||
        else if ((norm == 'F') || (norm == 'f') || (norm == 'E') || (norm == 'e')) {

            // Find normF(A)
            scale[0] = 0.0;
            sum[0] = 1.0;
            k = 2;

            if ((uplo == 'U') || (uplo == 'u')) {

                for (j = 2; j <= n; j++) {
                    x = new double[j - 1];

                    for (i = 0; i < (j - 1); i++) {
                        x[i] = ap[k - 1 + i];
                    }

                    ge.dlassq(j - 1, x, 1, scale, sum);
                    k = k + j;
                }
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { // ((uplo == 'L') || (uplo == 'l'))

                for (j = 1; j <= (n - 1); j++) {
                    x = new double[n - j];

                    for (i = 0; i < (n - j); i++) {
                        x[i] = ap[k - 1 + i];
                    }

                    ge.dlassq(n - j, x, 1, scale, sum);
                    k = k + n - j + 1;
                }
            } // else ((uplo == 'L') || (uplo == 'l'))

            sum[0] = 2.0 * sum[0];
            k = 1;

            for (i = 1; i <= n; i++) {

                if (ap[k - 1] != 0.0) {
                    absa = Math.abs(ap[k - 1]);

                    if (scale[0] < absa) {
                        ratio = scale[0] / absa;
                        sum[0] = 1.0 + (sum[0] * ratio * ratio);
                        scale[0] = absa;
                    } else {
                        ratio = absa / scale[0];
                        sum[0] = sum[0] + (ratio * ratio);
                    }
                } // if (ap[k-1] != 0.0)

                if ((uplo == 'U') || (uplo == 'u')) {
                    k = k + i + 1;
                } else {
                    k = k + n - i + 1;
                }
            } // for (i = 1; i <= n; i++)

            value = scale[0] * Math.sqrt(sum[0]);
        } // else if ((norm == 'F') || (norm == 'f') || (norm == 'E') ||

        return value;
    } // dlansp
    
    /**
     * This is a port of version 3.2 LAPACK auxiliary routine DLANGB Original DLANGB created by Univ. of Tennessee, Univ.
     * of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., November, 2006
     * dlangb returns the value of the one norm, or the Frobenius norm, or the infinity norm, or the element of largest
     * absolute value of an n by n band matrix A, with kl sub-diagonals and ku super-diagonals.
     *
     * @param   norm  input char Specifies the value to be returned as: 
     *                = 'M' or 'm', returns max(abs(A[i][j])), which is not a matrix norm. 
     *                = '1', 'O', or 'o', returns norm1(A), where norm1 denotes the one norm of a matrix
     *                                   (maximum column sum) 
     *                = 'I' or 'i', returns normI(A), the infinity norm of a matrix (maximum row sum) 
     *                = 'F', 'f', 'E', or 'e', returns normF(A), the Frobenius norm of a matrix (square root of
     *                                         sum of squares)
     * @param   n     input int The order of the matrix A. n >= 0. When n = 0, dlangb returns zero.
     * @param   kl    input int The number of sub-diagonals of the matrix A. kl >= 0.
     * @param   ku    input int The number of super-diagonals of the matrix A. ku >= 0.
     * @param   ab    input double[][] of dimension (ldab,n) The band matrix A, stored in rows 0 to kl + ku. The
     *                j-th column of A is stored in the j-th column of the array ab as follows: ab[ku+1+i-j][j] =
     *                A[i][j] for max(0,j-ku) <= i <= min(n-1,j+kl)
     * @param   ldab  input int The leading dimension of the array ab. ldab >= kl + ku + 1
     * @param   work  workspace double[] of dimension max(1, lwork), where lwork >= n when norm = 'I';
     *                otherwise work is notreferenced.
     *
     * @return  double
     */
    private double dlangb(char norm, int n, int kl, int ku, double[][] ab, int ldab, double[] work) {
        int i;
        int j;
        int k;
        int L;
        double[] scale = new double[1];
        double[] sum = new double[1];
        double value = 0.0;
        double[] x;

        if (n == 0) {
            value = 0.0;
        } else if ((norm == 'M') || (norm == 'm')) {

            // Find max(abs(A[i][j]))
            value = 0.0;

            for (j = 1; j <= n; j++) {

                for (i = Math.max(ku + 2 - j, 1); i <= Math.min(n + ku + 1 - j, kl + ku + 1); i++) {
                    value = Math.max(value, Math.abs(ab[i - 1][j - 1]));
                }
            }
        } // else if ((norm == 'M') || (norm == 'm'))
        else if ((norm == 'O') || (norm == 'o') || (norm == '1')) {

            // Find norm1(A)
            value = 0.0;

            for (j = 1; j <= n; j++) {
                sum[0] = 0.0;

                for (i = Math.max(ku + 2 - j, 1); i <= Math.min(n + ku + 1 - j, kl + ku + 1); i++) {
                    sum[0] = sum[0] + Math.abs(ab[i - 1][j - 1]);
                }

                value = Math.max(value, sum[0]);
            }
        } // else if ((norm == 'O') || (norm == 'o') || (norm == '1'))
        else if ((norm == 'I') || (norm == 'i')) {

            // Find normI(A)
            for (i = 0; i < n; i++) {
                work[i] = 0.0;
            }

            for (j = 1; j <= n; j++) {
                k = ku + 1 - j;

                for (i = Math.max(1, j - ku); i <= Math.min(n, j + kl); i++) {
                    work[i - 1] = work[i - 1] + Math.abs(ab[k + i - 1][j - 1]);
                }
            }

            value = 0.0;

            for (i = 0; i < n; i++) {
                value = Math.max(value, work[i]);
            }
        } // else if ((norm == 'I') || (norm == 'i'))
        else if ((norm == 'F') || (norm == 'f') || (norm == 'E') || (norm == 'e')) {

            // Find normF(A)
            scale[0] = 0.0;
            sum[0] = 1.0;

            for (j = 1; j <= n; j++) {
                L = Math.max(1, j - ku);
                k = ku + 1 - j + L;
                x = new double[Math.min(n, j + kl) - L + 1];

                for (i = 0; i < (Math.min(n, j + kl) - L + 1); i++) {
                    x[i] = ab[k + i - 1][j - 1];
                }

                ge.dlassq(Math.min(n, j + kl) - L + 1, x, 1, scale, sum);
            }

            value = scale[0] * Math.sqrt(sum[0]);
        } // else if ((norm == 'F') || (norm == 'f') || (norm == 'E')

        return value;
    } // dlangb
    
    /** This is a port of version 3.1 LAPACK test routine DBDT01.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             INTEGER            KD, LDA, LDPT, LDQ, M, N
             DOUBLE PRECISION   RESID
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), PT( LDPT, * ),
            $                   Q( LDQ, * ), WORK( * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DBDT01 reconstructs a general matrix A from its bidiagonal form
       *     A = Q * B * P'
       *  where Q (m by min(m,n)) and P' (min(m,n) by n) are orthogonal
       *  matrices and B is bidiagonal.
       *
       *  The test ratio to test the reduction is
       *     RESID = norm( A - Q * B * PT ) / ( n * norm(A) * EPS )
       *  where PT = P' and EPS is the machine precision.
       *
       *  Arguments
       *  =========
       *
       *  M       (input) INTEGER
       *          The number of rows of the matrices A and Q.
       *
       *  N       (input) INTEGER
       *          The number of columns of the matrices A and P'.
       *
       *  KD      (input) INTEGER
       *          If KD = 0, B is diagonal and the array E is not referenced.
       *          If KD = 1, the reduction was performed by xGEBRD; B is upper
       *          bidiagonal if M >= N, and lower bidiagonal if M < N.
       *          If KD = -1, the reduction was performed by xGBBRD; B is
       *          always upper bidiagonal.
       *
       *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
       *          The m by n matrix A.
       *
       *  LDA     (input) INTEGER
       *          The leading dimension of the array A.  LDA >= max(1,M).
       *
       *  Q       (input) DOUBLE PRECISION array, dimension (LDQ,N)
       *          The m by min(m,n) orthogonal matrix Q in the reduction
       *          A = Q * B * P'.
       *
       *  LDQ     (input) INTEGER
       *          The leading dimension of the array Q.  LDQ >= max(1,M).
       *
       *  D       (input) DOUBLE PRECISION array, dimension (min(M,N))
       *          The diagonal elements of the bidiagonal matrix B.
       *
       *  E       (input) DOUBLE PRECISION array, dimension (min(M,N)-1)
       *          The superdiagonal elements of the bidiagonal matrix B if
       *          m >= n, or the subdiagonal elements of B if m < n.
       *
       *  PT      (input) DOUBLE PRECISION array, dimension (LDPT,N)
       *          The min(m,n) by n orthogonal matrix P' in the reduction
       *          A = Q * B * P'.
       *
       *  LDPT    (input) INTEGER
       *          The leading dimension of the array PT.
       *          LDPT >= max(1,min(M,N)).
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (M+N)
       *
       *  RESID   (output) DOUBLE PRECISION
       *          The test ratio:  norm(A - Q * B * P') / ( n * norm(A) * EPS )
       */
    private void dbdt01(int m, int n, int kd, double[][] A, int lda, double[][] Q,
                        int ldq, double[] d, double[] e, double[][] PT, int ldpt,
                        double[] work, double[] resid) {
        int i;
        int j;
        double anorm;
        double eps;
        int p;
        double work2[];
        double absSum;
        
        // Quick return if possible
        if ((m <= 0) || (n <= 0)) {
            resid[0] = 0.0;
            return;
        }
        
        // Compute A - Q * B * P' one column at a time.
        resid[0] = 0.0;
        if (kd != 0) {
            // B is bidiagonal.
            if (m >= n) {
                // B is upper bidiagonal and m >= n.
                for (j = 1; j <= n; j++) {
                    for (p = 0; p < m; p++) {
                        work[p] = A[p][j-1];
                    }
                    for (i = 1; i <= n-1; i++) {
                        work[m+i-1] = d[i-1]*PT[i-1][j-1] + e[i-1]*PT[i][j-1];
                    }
                    work[m+n-1] = d[n-1]*PT[n-1][j-1];
                    work2 = new double[n];
                    for (p = 0; p < n; p++) {
                        work2[p] = work[m+p];
                    }
                    ge.dgemv('N', m, n, -1.0, Q, ldq, work2, 1, 1.0, work, 1);
                    absSum = 0.0;
                    for (p = 0; p < m; p++) {
                        absSum += Math.abs(work[p]);
                    }
                    resid[0] = Math.max(resid[0], absSum);
                } // for (j = 1; j <= n; j++)
            } // if (m >= n)
            else if (kd < 0) {
                // B is upper diagonal and M < N.
                for (j = 1; j <= n; j++) {
                    for (p = 0; p < m; p++) {
                        work[p] = A[p][j-1];
                    }
                    for (i = 1; i <= m-1; i++) {
                        work[m+i-1] = d[i-1]*PT[i-1][j-1] + e[i-1]*PT[i][j-1];
                    }
                    work[2*m-1] = d[m-1]*PT[m-1][j-1];
                    work2 = new double[m];
                    for (p = 0; p < m; p++) {
                        work2[p] = work[m+p];
                    }
                    ge.dgemv('N', m, m, -1.0, Q, ldq, work2, 1, 1.0, work, 1);
                    absSum = 0.0;
                    for (p = 0; p < m; p++) {
                        absSum += Math.abs(work[p]);
                    }
                    resid[0] = Math.max(resid[0], absSum);
                } // for (j = 1; j <= n; j++)
            } // else if (kd < 0)
            else {
                // B is lower bidiagonal.
                for (j = 1; j <= n; j++) {
                    for (p = 0; p < m; p++) {
                        work[p] = A[p][j-1];
                    }
                    work[m] = d[0]*PT[0][j-1];
                    for (i = 2; i <= m; i++) {
                        work[m+i-1] = e[i-2]*PT[i-2][j-1] + d[i-1]*PT[i-1][j-1];
                    } // for (i = 2; i <= m; i++)
                    work2 = new double[m];
                    for (p = 0; p < m; p++) {
                        work2[p] = work[m+p];
                    }
                    ge.dgemv('N', m, m, -1.0, Q, ldq, work2, 1, 1.0, work, 1);
                    absSum = 0.0;
                    for (p = 0; p < m; p++) {
                        absSum += Math.abs(work[p]);
                    }
                    resid[0] = Math.max(resid[0], absSum);
                } // for (j = 1; j <= n; j++)
            } // else
        } // if (kd != 0)
        else { // kd == 0
            // B is dialgonal.
            if (m >= n) {
                for (j = 1; j <= n; j++) {
                    for (p = 0; p < m; p++) {
                        work[p] = A[p][j-1];
                    }
                    for (i = 1; i <= n; i++) {
                        work[m+i-1] = d[i-1]*PT[i-1][j-1];
                    }
                    work2 = new double[n];
                    for (p = 0; p < n; p++) {
                        work2[p] = work[m+p];
                    }
                    ge.dgemv('N', m, n, -1.0, Q, ldq, work2, 1, 1.0, work, 1);
                    absSum = 0.0;
                    for (p = 0; p < m; p++) {
                        absSum += Math.abs(work[p]);
                    }
                    resid[0] = Math.max(resid[0], absSum);
                } // for (j = 1; j <= n; j++)
            } // if (m >= n)
            else { // m < n
                for (j = 1; j <= n; j++) {
                    for (p = 0; p < m; p++) {
                        work[p] = A[p][j-1];
                    }
                    for (i = 1; i <= m; i++) {
                        work[m+i-1] = d[i-1]*PT[i-1][j-1];
                    }
                    work2 = new double[m];
                    for (p = 0; p < m; p++) {
                        work2[p] = work[m+p];
                    }
                    ge.dgemv('N', m, m, -1.0, Q, ldq, work2, 1, 1.0, work, 1);
                    absSum = 0.0;
                    for (p = 0; p < m; p++) {
                        absSum += Math.abs(work[p]);
                    }
                    resid[0] = Math.max(resid[0], absSum);
                } // for (j = 1; j <= n; j++)
            } // else m < n
        } // else kd == 0
        
        // Compute norm(A - Q * B * P') / (n * norm(A) * eps)
        anorm = ge.dlange('1', m, n, A, lda, work);
        eps = ge.dlamch('P'); // Precision
        
        if (anorm <= 0.0) {
            if (resid[0] != 0.0) {
                resid[0] = 1.0/eps;
            }
        }
        else {
            if (anorm >= resid[0]) {
                resid[0] = (resid[0]/anorm)/((double)n * eps);
            }
            else {
                if (anorm < 1.0) {
                    resid[0] = (Math.min(resid[0], (double)n * anorm)/anorm)/((double)n * eps);
                }
                else {
                    resid[0] = Math.min(resid[0]/anorm, (double)n)/((double)n * eps);
                }
            }
        } // else
        return;
    } // dbdt01
    
    /** This is the port of version 3.1 LAPACK test routine DORT01.
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     November 2006
    *
    *     .. Scalar Arguments ..
          CHARACTER          ROWCOL
          INTEGER            LDU, LWORK, M, N
          DOUBLE PRECISION   RESID
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   U( LDU, * ), WORK( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DORT01 checks that the matrix U is orthogonal by computing the ratio
    *
    *     RESID = norm( I - U*U' ) / ( n * EPS ), if ROWCOL = 'R',
    *  or
    *     RESID = norm( I - U'*U ) / ( m * EPS ), if ROWCOL = 'C'.
    *
    *  Alternatively, if there isn't sufficient workspace to form
    *  I - U*U' or I - U'*U, the ratio is computed as
    *
    *     RESID = abs( I - U*U' ) / ( n * EPS ), if ROWCOL = 'R',
    *  or
    *     RESID = abs( I - U'*U ) / ( m * EPS ), if ROWCOL = 'C'.
    *
    *  where EPS is the machine precision.  ROWCOL is used only if m = n;
    *  if m > n, ROWCOL is assumed to be 'C', and if m < n, ROWCOL is
    *  assumed to be 'R'.
    *
    *  Arguments
    *  =========
    *
    *  ROWCOL  (input) CHARACTER
    *          Specifies whether the rows or columns of U should be checked
    *          for orthogonality.  Used only if M = N.
    *          = 'R':  Check for orthogonal rows of U
    *          = 'C':  Check for orthogonal columns of U
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrix U.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrix U.
    *
    *  U       (input) DOUBLE PRECISION array, dimension (LDU,N)
    *          The orthogonal matrix U.  U is checked for orthogonal columns
    *          if m > n or if m = n and ROWCOL = 'C'.  U is checked for
    *          orthogonal rows if m < n or if m = n and ROWCOL = 'R'.
    *
    *  LDU     (input) INTEGER
    *          The leading dimension of the array U.  LDU >= max(1,M).
    *
    *  WORK    (workspace) DOUBLE PRECISION array, dimension (min(m,n),min(m,n))
    *          In ge.dlaset, dsyrk, and dlansy work must be 2D array.
    *
    *  LWORK   (input) INTEGER
    *          The length of the array WORK.  For best performance, LWORK
    *          should be at least N*(N+1) if ROWCOL = 'C' or M*(M+1) if
    *          ROWCOL = 'R', but the test will be done even if LWORK is 0.
    *
    *  RESID   (output) DOUBLE PRECISION
    *          RESID = norm( I - U * U' ) / ( n * EPS ), if ROWCOL = 'R', or
    *          RESID = norm( I - U' * U ) / ( m * EPS ), if ROWCOL = 'C'.
    */
    private void dort01(char rowcol, int m, int n, double[][] U, int ldu,
                        double[][] work, int lwork, double[] resid) {
        char transu;
        int i;
        int j;
        int k;
        int ldwork;
        int mnmin;
        double eps;
        double tmp;
        double work2[];
        double v1[];
        double v2[];
        int p;
        
        resid[0] = 0.0;
        
        // Quick return if possible.
        if ((m <= 0) || (n <= 0)) {
            return;
        }
        
        eps = ge.dlamch('P'); // Precision
        if ((m < n) || ((m == n) && ((rowcol == 'R') || (rowcol == 'r')))) {
            transu = 'N';
            k = n;
        }
        else {
            transu = 'T';
            k = m;
        }
        mnmin = Math.min(m, n);
        
        if ((mnmin + 1)*mnmin <= lwork) {
            ldwork = mnmin;
        }
        else {
            ldwork = 0;
        }
        if (ldwork > 0) {
            // Compute I - U*U' or I - U'*U
            ge.dlaset('U', mnmin, mnmin, 0.0, 1.0, work, ldwork);
            dsyrk('U', transu, mnmin, k, -1.0, U, ldu, 1.0, work, ldwork);
            
            // Compute norm(I - U*U') /(k * eps).
            work2 = new double[Math.max(1, mnmin)];
            resid[0] = dlansy('1', 'U', mnmin, work, ldwork, work2);
            resid[0] = (resid[0]/(double)k)/eps;
        } // if (ldwork > 0)
        else if (transu == 'T') {
            // Find the maximum element in abs(I - U'*U)/(m * eps)
            for (j = 1; j <= n; j++) {
                for (i = 1; i <= j; i++) {
                    if (i != j) {
                        tmp = 0.0;
                    }
                    else {
                        tmp = 1.0;
                    }
                    v1 = new double[m];
                    v2 = new double[m];
                    for (p = 0; p < m; p++) {
                        v1[p] = U[p][i-1];
                        v2[p] = U[p][j-1];
                    }
                    tmp = tmp - ddot(m, v1, 1, v2, 1);
                    resid[0] = Math.max(resid[0], Math.abs(tmp));
                } // for (i = 1; i <= j; i++)
            } // for (j = 1; j <= n; j++)
            resid[0] = (resid[0]/(double)m)/eps;
        } // else if (transu == 'T')
        else {
            // Find the maximum element in abs(I - U*U')/(n * eps)
            for (j = 1; j <= m; j++) {
                for (i = 1; i <= j; i++) {
                    if (i != j) {
                        tmp = 0.0;
                    }
                    else {
                        tmp = 1.0;
                    }
                    v1 = new double[n];
                    v2 = new double[n];
                    for (p = 0; p < n; p++) {
                        v1[p] = U[j-1][p];
                        v2[p] = U[i-1][p];
                    }
                    tmp = tmp - ddot(n, v1, 1, v2, 1);
                    resid[0] = Math.max(resid[0], Math.abs(tmp));
                } // for (i = 1; i <= j; i++)
            } // for (j = 1; j <= m; j++)
            resid[0] = (resid[0]/(double)n)/eps;
        } // else
        return;
    } // dort01

    /** This is a port of version 3.1 LAPACK test routine DBDT02.
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     November 2006
    *
    *     .. Scalar Arguments ..
          INTEGER            LDB, LDC, LDU, M, N
          DOUBLE PRECISION   RESID
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   B( LDB, * ), C( LDC, * ), U( LDU, * ),
         $                   WORK( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DBDT02 tests the change of basis C = U' * B by computing the residual
    *
    *     RESID = norm( B - U * C ) / ( max(m,n) * norm(B) * EPS ),
    *
    *  where B and C are M by N matrices, U is an M by M orthogonal matrix,
    *  and EPS is the machine precision.
    *
    *  Arguments
    *  =========
    *
    *  M       (input) INTEGER
    *          The number of rows of the matrices B and C and the order of
    *          the matrix Q.
    *
    *  N       (input) INTEGER
    *          The number of columns of the matrices B and C.
    *
    *  B       (input) DOUBLE PRECISION array, dimension (LDB,N)
    *          The m by n matrix B.
    *
    *  LDB     (input) INTEGER
    *          The leading dimension of the array B.  LDB >= max(1,M).
    *
    *  C       (input) DOUBLE PRECISION array, dimension (LDC,N)
    *          The m by n matrix C, assumed to contain U' * B.
    *
    *  LDC     (input) INTEGER
    *          The leading dimension of the array C.  LDC >= max(1,M).
    *
    *  U       (input) DOUBLE PRECISION array, dimension (LDU,M)
    *          The m by m orthogonal matrix U.
    *
    *  LDU     (input) INTEGER
    *          The leading dimension of the array U.  LDU >= max(1,M).
    *
    *  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
    *
    *  RESID   (output) DOUBLE PRECISION
    *          RESID = norm( B - U * C ) / ( max(m,n) * norm(B) * EPS ),
    */
    private void dbdt02(int m, int n, double[][] B, int ldb, double[][] C,
                        int ldc, double[][] U, int ldu, double[] work,
                        double[] resid) {
        int j;
        double bnorm;
        double eps;
        double realmn;
        int p;
        double v1[];
        double absSum;
        
        // Quick return if possible
        resid[0] = 0.0;
        if ((m <= 0) || (n <= 0)) {
            return;
        }
        realmn = (double)Math.max(m, n);
        eps = ge.dlamch('P'); // Precision
        
        // Compute norm(B - U * C)
        for (j = 1; j <= n; j++) {
            for (p = 0; p < m; p++) {
                work[p] = B[p][j-1];
            }
            v1 = new double[m];
            for (p = 0; p < m; p++) {
                v1[p] = C[p][j-1];
            }
            ge.dgemv('N', m, m, -1.0, U, ldu, v1, 1, 1.0, work, 1);
            absSum = 0.0;
            for (p = 0; p < m; p++) {
                absSum += Math.abs(work[p]);
            }
            resid[0] = Math.max(resid[0], absSum);
        } // for (j = 1; j <= n; j++)
        
        // Compute norm of B.
        bnorm = ge.dlange('1', m, n, B, ldb, work);
        
        if (bnorm <= 0.0) {
            if (resid[0] != 0.0) {
                resid[0] = 1.0/eps;
            }
        } // if (bnorm <= 0.0)
        else {
            if (bnorm >= resid[0]) {
                resid[0] = (resid[0]/bnorm)/(realmn * eps);
            }
            else {
                if (bnorm < 1.0) {
                    resid[0] = (Math.min(resid[0], realmn * bnorm)/ bnorm)/(realmn * eps);
                }
                else {
                    resid[0] = Math.min(resid[0]/bnorm, realmn)/(realmn * eps);
                }
            }
        }
        return;
    } // dbdt02
    
    /** This is a port of version 3.1 LAPACK test routine DBDT03.
       *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
       *     November 2006
       *
       *     .. Scalar Arguments ..
             CHARACTER          UPLO
             INTEGER            KD, LDU, LDVT, N
             DOUBLE PRECISION   RESID
       *     ..
       *     .. Array Arguments ..
             DOUBLE PRECISION   D( * ), E( * ), S( * ), U( LDU, * ),
            $                   VT( LDVT, * ), WORK( * )
       *     ..
       *
       *  Purpose
       *  =======
       *
       *  DBDT03 reconstructs a bidiagonal matrix B from its SVD:
       *     S = U' * B * V
       *  where U and V are orthogonal matrices and S is diagonal.
       *
       *  The test ratio to test the singular value decomposition is
       *     RESID = norm( B - U * S * VT ) / ( n * norm(B) * EPS )
       *  where VT = V' and EPS is the machine precision.
       *
       *  Arguments
       *  =========
       *
       *  UPLO    (input) CHARACTER*1
       *          Specifies whether the matrix B is upper or lower bidiagonal.
       *          = 'U':  Upper bidiagonal
       *          = 'L':  Lower bidiagonal
       *
       *  N       (input) INTEGER
       *          The order of the matrix B.
       *
       *  KD      (input) INTEGER
       *          The bandwidth of the bidiagonal matrix B.  If KD = 1, the
       *          matrix B is bidiagonal, and if KD = 0, B is diagonal and E is
       *          not referenced.  If KD is greater than 1, it is assumed to be
       *          1, and if KD is less than 0, it is assumed to be 0.
       *
       *  D       (input) DOUBLE PRECISION array, dimension (N)
       *          The n diagonal elements of the bidiagonal matrix B.
       *
       *  E       (input) DOUBLE PRECISION array, dimension (N-1)
       *          The (n-1) superdiagonal elements of the bidiagonal matrix B
       *          if UPLO = 'U', or the (n-1) subdiagonal elements of B if
       *          UPLO = 'L'.
       *
       *  U       (input) DOUBLE PRECISION array, dimension (LDU,N)
       *          The n by n orthogonal matrix U in the reduction B = U'*A*P.
       *
       *  LDU     (input) INTEGER
       *          The leading dimension of the array U.  LDU >= max(1,N)
       *
       *  S       (input) DOUBLE PRECISION array, dimension (N)
       *          The singular values from the SVD of B, sorted in decreasing
       *          order.
       *
       *  VT      (input) DOUBLE PRECISION array, dimension (LDVT,N)
       *          The n by n orthogonal matrix V' in the reduction
       *          B = U * S * V'.
       *
       *  LDVT    (input) INTEGER
       *          The leading dimension of the array VT.
       *
       *  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
       *
       *  RESID   (output) DOUBLE PRECISION
       *          The test ratio:  norm(B - U * S * V') / ( n * norm(A) * EPS )
       */
    private void dbdt03(char uplo, int n, int kd, double[] d, double[] e, double[][] U,
                        int ldu, double[] s, double[][] VT, int ldvt, double[] work,
                        double[] resid) {
        int i;
        int j;
        double bnorm;
        double eps;
        double work2[];
        int p;
        double absSum;
        
        // Quick return if possible
        resid[0] = 0.0;
        if (n <= 0) {
            return;
        }
        
        // Compute B - U * S * V' one column at a time.
        bnorm = 0.0;
        if (kd >= 1) {
            // B is bidiagonal
            if ((uplo == 'U') || (uplo == 'u')) {
                // B is upper bidiagonal.
                for (j = 1; j <= n; j++) {
                    for (i = 1; i <= n; i++) {
                        work[n+i-1] = s[i-1]*VT[i-1][j-1];
                    }
                    work2 = new double[n];
                    for (p = 0; p < n; p++) {
                        work2[p] = work[n+p];
                    }
                    ge.dgemv('N', n, n, -1.0, U, ldu, work2, 1, 0.0, work, 1);
                    work[j-1] = work[j-1] + d[j-1];
                    if (j > 1) {
                        work[j-2] = work[j-2] + e[j-2];
                        bnorm = Math.max(bnorm, Math.abs(d[j-1]) + Math.abs(e[j-2]));
                    }
                    else {
                        bnorm = Math.max(bnorm, Math.abs(d[j-1]));
                    }
                    absSum = 0.0;
                    for (p = 0; p < n; p++) {
                        absSum += Math.abs(work[p]);
                    }
                    resid[0] = Math.max(resid[0], absSum);
                } // for (j = 1; j <= n; j++)
            } // if ((uplo == 'U') || (uplo == 'u'))
            else { 
                // B is lower bidiagonal.
                for (j = 1; j <= n; j++) {
                    for (i = 1; i <= n; i++) {
                        work[n+i-1] = s[i-1] * VT[i-1][j-1];
                    }
                    work2 = new double[n];
                    for (p = 0; p < n; p++) {
                        work2[p] = work[n+p];
                    }
                    ge.dgemv('N', n, n, -1.0, U, ldu, work2, 1, 0.0, work, 1);
                    work[j-1] = work[j-1] + d[j-1];
                    if (j < n) {
                        work[j] = work[j] + e[j-1];
                        bnorm = Math.max(bnorm, Math.abs(d[j-1]) + Math.abs(e[j-1]));
                    }
                    else {
                        bnorm = Math.max(bnorm, Math.abs(d[j-1]));
                    }
                    absSum = 0.0;
                    for (p = 0; p < n; p++) {
                        absSum += Math.abs(work[p]);
                    }
                    resid[0] = Math.max(resid[0], absSum);
                } // for (j = 1; j <= n; j++)
            } // else
        } // if (kd >= 1)
        else {
            // B is diagonal.
            for (j = 1; j <= n; j++) {
                for (i = 1; i <= n; i++) {
                    work[n+i-1] = s[i-1] * VT[i-1][j-1];
                }
                work2 = new double[n];
                for (p = 0; p < n; p++) {
                    work2[p] = work[n+p];
                }
                ge.dgemv('N', n, n, -1.0, U, ldu, work2, 1, 0.0, work, 1);
                work[j-1] = work[j-1] + d[j-1];
                absSum = 0.0;
                for (p = 0; p < n; p++) {
                    absSum += Math.abs(work[p]);
                }
                resid[0] = Math.max(resid[0], absSum);
            } // for (j = 1; j <= n; j++)
            bnorm = 0.0;
            for (j = 0; j < n; j++) {
                if (Math.abs(d[j]) > bnorm) {
                    bnorm = Math.abs(d[j]);
                }
            }
        } // else
        
        // Compute norm(B - U * S * V')/ (n * morm(B) * eps)
        eps = ge.dlamch('P'); // Precision
        
        if (bnorm <= 0.0) {
            if (resid[0] != 0.0) {
                resid[0] = 1.0/eps;
            }
        } // if (bnorm <= 0.0)
        else {
            if (bnorm >= resid[0]) {
                resid[0] = (resid[0]/bnorm)/((double)n * eps);
            }
            else {
                if (bnorm < 1.0) {
                    resid[0] = (Math.min(resid[0], (double)n * bnorm)/bnorm)/((double)n * eps);
                }
                else {
                    resid[0] = Math.min(resid[0]/bnorm, (double)n)/((double)n * eps);
                }
            }
        } // else
        return;
    } // dbdt03
    
    /** This is a port of version 3.1 LAPACK test routine DSVDCH.
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     November 2006
    *
    *     .. Scalar Arguments ..
          INTEGER            INFO, N
          DOUBLE PRECISION   TOL
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   E( * ), S( * ), SVD( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DSVDCH checks to see if SVD(1) ,..., SVD(N) are accurate singular
    *  values of the bidiagonal matrix B with diagonal entries
    *  S(1) ,..., S(N) and superdiagonal entries E(1) ,..., E(N-1)).
    *  It does this by expanding each SVD(I) into an interval
    *  [SVD(I) * (1-EPS) , SVD(I) * (1+EPS)], merging overlapping intervals
    *  if any, and using Sturm sequences to count and verify whether each
    *  resulting interval has the correct number of singular values (using
    *  DSVDCT). Here EPS=TOL*MAX(N/10,1)*MAZHEP, where MACHEP is the
    *  machine precision. The routine assumes the singular values are sorted
    *  with SVD(1) the largest and SVD(N) smallest.  If each interval
    *  contains the correct number of singular values, INFO = 0 is returned,
    *  otherwise INFO is the index of the first singular value in the first
    *  bad interval.
    *
    *  Arguments
    *  ==========
    *
    *  N       (input) INTEGER
    *          The dimension of the bidiagonal matrix B.
    *
    *  S       (input) DOUBLE PRECISION array, dimension (N)
    *          The diagonal entries of the bidiagonal matrix B.
    *
    *  E       (input) DOUBLE PRECISION array, dimension (N-1)
    *          The superdiagonal entries of the bidiagonal matrix B.
    *
    *  SVD     (input) DOUBLE PRECISION array, dimension (N)
    *          The computed singular values to be checked.
    *
    *  TOL     (input) DOUBLE PRECISION
    *          Error tolerance for checking, a multiplier of the
    *          machine precision.
    *
    *  INFO    (output) INTEGER
    *          =0 if the singular values are all correct (to within
    *             1 +- TOL*MAZHEPS)
    *          >0 if the interval containing the INFO-th singular value
    *             contains the incorrect number of singular values.
    */
    private void dsvdch(int n, double[] s, double[] e, double[] svd, double tol, int[] info) {
        int bpnt;
        int count;
        int numl[] = new int[1];
        int numu[] = new int[1];
        int tpnt;
        double eps;
        double lower;
        double ovfl;
        double tuppr;
        double unfl;
        double unflep;
        double upper;
        
        // Get the machine constants
        info[0] = 0;
        if (n <= 0) {
            return;
        }
        unfl = ge.dlamch('S'); // Safe minimum;
        ovfl = ge.dlamch('O'); // Overflow
        eps = ge.dlamch('E') * ge.dlamch('B');
        
        // unflep is chosen so that when an eigenvalue is multiplied by the
        // scale factor sqrt(ovfl)*sqrt(sqrt(unfl))/mx in dsvdct, it exceeds
        // sqrt(unfl), which is the lower limit for dsvdct.
        
        unflep = (Math.sqrt(Math.sqrt(unfl))/Math.sqrt(ovfl))*svd[0] + unfl/eps;
        
        // The value of eps works best when tol >= 10.
        eps = tol * Math.max(n/10, 1)*eps;
        
        // tpnt points to singular value at right endpoint of interval
        // bpnt points to singular value at left endpoint of interval
        tpnt = 1;
        bpnt = 1;
        
        // Begin loop over all intervals
        do {
            upper = (1.0 + eps) * svd[tpnt-1] + unflep;
            lower = (1.0 - eps) * svd[bpnt-1] - unflep;
            if (lower <= unflep) {
                lower = -upper;
            }
            
            // Begin loop merging overlapping intervals
            do {
                if (bpnt == n) {
                    break;
                }
                tuppr = (1.0 + eps) * svd[bpnt] + unflep;
                if (tuppr < lower) {
                    break;
                }
                // Merge
                
                bpnt++;
                lower = (1.0 - eps)*svd[bpnt-1] - unflep;
                if (lower <= unflep) {
                    lower = -upper;
                }
            } while(true);
            
            // Count singular values in interval [lower, upper]
            dsvdct(n, s, e, lower, numl);
            dsvdct(n, s, e, upper, numu);
            count = numu[0] - numl[0];
            if (lower < 0.0) {
                count = count/2;
            }
            if (count != bpnt-tpnt+1) {
                // Wrong number of singular values in interval
                info[0] = tpnt;
                break;
            }
            tpnt = bpnt + 1;
            bpnt = tpnt;
        } while (tpnt <= n);
        return;
    } // dsvdch
    
    /** This is a port of version 3.1 LAPACK test routine DSVDCT.
    *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    *     November 2006
    *
    *     .. Scalar Arguments ..
          INTEGER            N, NUM
          DOUBLE PRECISION   SHIFT
    *     ..
    *     .. Array Arguments ..
          DOUBLE PRECISION   E( * ), S( * )
    *     ..
    *
    *  Purpose
    *  =======
    *
    *  DSVDCT counts the number NUM of eigenvalues of a 2*N by 2*N
    *  tridiagonal matrix T which are less than or equal to SHIFT.  T is
    *  formed by putting zeros on the diagonal and making the off-diagonals
    *  equal to S(1), E(1), S(2), E(2), ... , E(N-1), S(N).  If SHIFT is
    *  positive, NUM is equal to N plus the number of singular values of a
    *  bidiagonal matrix B less than or equal to SHIFT.  Here B has diagonal
    *  entries S(1), ..., S(N) and superdiagonal entries E(1), ... E(N-1).
    *  If SHIFT is negative, NUM is equal to the number of singular values
    *  of B greater than or equal to -SHIFT.
    *
    *  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
    *  Matrix", Report CS41, Computer Science Dept., Stanford University,
    *  July 21, 1966
    *
    *  Arguments
    *  =========
    *
    *  N       (input) INTEGER
    *          The dimension of the bidiagonal matrix B.
    *
    *  S       (input) DOUBLE PRECISION array, dimension (N)
    *          The diagonal entries of the bidiagonal matrix B.
    *
    *  E       (input) DOUBLE PRECISION array of dimension (N-1)
    *          The superdiagonal entries of the bidiagonal matrix B.
    *
    *  SHIFT   (input) DOUBLE PRECISION
    *          The shift, used as described under Purpose.
    *
    *  NUM     (output) INTEGER
    *          The number of eigenvalues of T less than or equal to SHIFT.
    */
    private void dsvdct(int n, double[] s, double[] e, double shift, int[] num) {
        int i;
        double m1;
        double m2;
        double mx;
        double ovfl;
        double sov;
        double sshift;
        double ssun;
        double sun;
        double tmp;
        double tom;
        double u;
        double unfl;
        
        // Get machine constants
        unfl = 2 * ge.dlamch('S'); // Sage minimum
        ovfl = 1.0/unfl;
        
        // Find largest entry
        mx = Math.abs(s[0]);
        for (i = 1; i <= n-1; i++) {
            mx = Math.max(mx, Math.max(Math.abs(s[i]), Math.abs(e[i-1])));
        }
        
        if (mx == 0.0) {
            if (shift < 0.0) {
                num[0] = 0;
            }
            else {
                num[0] = 2*n;
            }
            return;
        } // if (mx == 0.0)
        
        // Compute scale factors as in Kahn's report
        sun = Math.sqrt(unfl);
        ssun = Math.sqrt(sun);
        sov = Math.sqrt(ovfl);
        tom = ssun * sov;
        if (mx <= 1.0) {
            m1 = 1.0/mx;
            m2 = tom;
        }
        else {
            m1 = 1.0;
            m2 = tom/mx;
        }
        
        // Being counting
        u = 1.0;
        num[0] = 0;
        sshift = (shift*m1)*m2;
        u = -sshift;
        if (u <= sun) {
            if (u <= 0.0) {
                num[0]++;
                if (u > -sun) {
                    u = -sun;
                }
            } // if (u <= 0.0)
            else {
                u = sun;
            }
        } // if (u <= sun)
        tmp = (s[0]*m1)*m2;
        u = -tmp*(tmp/u) - sshift;
        if (u <= sun) {
            if (u <= 0.0) {
                num[0]++;
                if (u > -sun) {
                    u = -sun;
                }
            } // if (u <= 0.0)
            else {
                u = sun;
            }
        } // if (u <= sun)
        for (i = 1; i <= n-1; i++) {
            tmp = (e[i-1]*m1)*m2;
            u = -tmp*(tmp/u) - sshift;
            if (u <= sun) {
                if (u <= 0.0) {
                    num[0]++;
                    if (u > -sun) {
                        u = -sun;
                    }
                } // if (u <= 0.0)
                else {
                    u = sun;
                }
            } // if (u <= sun)
            tmp = (s[i]*m1)*m2;
            u = -tmp*(tmp/u) - sshift;
            if (u <= sun) {
                if (u <= 0.0) {
                    num[0]++;
                    if (u > -sun) {
                        u = -sun;
                    }
                } // if (u <= 0.0)
                else {
                    u = sun;
                }
            } // if (u <= sun)
        } // for (i = 1; i <= n-1; i++)
        return;
    } // dsvdct
}