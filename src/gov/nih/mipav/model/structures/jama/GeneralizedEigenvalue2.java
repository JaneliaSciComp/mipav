package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.*;

public class GeneralizedEigenvalue2 implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    GeneralizedInverse2 gi2 = new GeneralizedInverse2();
    int iparms[];
    
    public GeneralizedEigenvalue2() {
    	
    }
    
    // In dchkq3 all 4410 tests run passed the threshold
    /**
     * This dchkqr_test routine is a port of a portion of the version 3.1.1 LAPACK test routine DCHKAA by Univ. of
     * Tennessee, Univ. Of California Berkeley and NAG Ltd., January, 2007. and some values from the test data file
     * dtest.in.
     */
    public void dchkq3_test() {

        // Number of values of m
        final int nm = 7;

        // Values of m (row dimension)
        // dtest.in uses 50 rather than 16
        final int[] mval = new int[] {0, 1, 2, 3, 5, 10, 16};

        // Number of values of n
        final int nn = 7;

        // Values of n (column dimension)
        // dtest.in uses 50 rather than 16
        final int[] nval = new int[] {0, 1, 2, 3, 5, 10, 16};

        // Values of nrhs (number of right hand sides)
        // dchkaa uses only 2. dtest.in uses 1, 2, 15.
        // Since dchkr only accepts nrhs = nsval[0] use only 2.
        final int[] nsval = new int[] {2};

        // Number of values of nb
        final int nnb = 5;

        // Values of nb (the blocksize)
        final int nbval[] = new int[] {1, 3, 3, 3, 20};

        // Values of nx (crossover point)
        // There are nnb values of nx.
        final int nxval[] = new int[] {1, 0, 5, 9, 1};

        // Threshold value of test ratio
        // dchkaa has 20.0, dtest.in has 30.0
        final double thresh = 20.0;

        // The maximum allowable value for n
        final int nmax = 132;

        final int ntypes = 6;
        //final int nrhs = nsval[0];
        final boolean dotype[] = new boolean[ntypes];
        final double A[][] = new double[nmax][nmax];
        final double AF[][] = new double[nmax][nmax];
        double s[] = new double[nmax];
        final double tau[] = new double[nmax];
        final double work[] = new double[nmax * nmax];
        final int iwork[] = new int[nmax];
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

        dchkq3(dotype, nm, mval, nn, nval, nnb, nbval, nxval,
        	     thresh, A, AF, s,
        	     tau, work, iwork);
    } // dchkq3_test

    
    /*> \brief \b DCHKQ3
    *
    *  =========== DOCUMENTATION ===========
    *
    * Online html documentation available at
    *            http://www.netlib.org/lapack/explore-html/
    *
    *  Definition:
    *  ===========
    *
    *       SUBROUTINE DCHKQ3( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVAL,
    *                          THRESH, A, COPYA, S, TAU, WORK, IWORK,
    *                          NOUT )
    *
    *       .. Scalar Arguments ..
    *       INTEGER            NM, NN, NNB, NOUT
    *       DOUBLE PRECISION   THRESH
    *       ..
    *       .. Array Arguments ..
    *       LOGICAL            DOTYPE( * )
    *       INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NVAL( * ),
    *      $                   NXVAL( * )
    *       DOUBLE PRECISION   A( * ), COPYA( * ), S( * ),
    *      $                   TAU( * ), WORK( * )
    *       ..
    *
    *
    *> \par Purpose:
    *  =============
    *>
    *> \verbatim
    *>
    *> DCHKQ3 tests DGEQP3.
    *> \endverbatim
    *
    *  Arguments:
    *  ==========
    *
    *> \param[in] DOTYPE
    *> \verbatim
    *>          DOTYPE is LOGICAL array, dimension (NTYPES)
    *>          The matrix types to be used for testing.  Matrices of type j
    *>          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
    *>          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
    *> \endverbatim
    *>
    *> \param[in] NM
    *> \verbatim
    *>          NM is INTEGER
    *>          The number of values of M contained in the vector MVAL.
    *> \endverbatim
    *>
    *> \param[in] MVAL
    *> \verbatim
    *>          MVAL is INTEGER array, dimension (NM)
    *>          The values of the matrix row dimension M.
    *> \endverbatim
    *>
    *> \param[in] NN
    *> \verbatim
    *>          NN is INTEGER
    *>          The number of values of N contained in the vector NVAL.
    *> \endverbatim
    *>
    *> \param[in] NVAL
    *> \verbatim
    *>          NVAL is INTEGER array, dimension (NN)
    *>          The values of the matrix column dimension N.
    *> \endverbatim
    *>
    *> \param[in] NNB
    *> \verbatim
    *>          NNB is INTEGER
    *>          The number of values of NB and NX contained in the
    *>          vectors NBVAL and NXVAL.  The blocking parameters are used
    *>          in pairs (NB,NX).
    *> \endverbatim
    *>
    *> \param[in] NBVAL
    *> \verbatim
    *>          NBVAL is INTEGER array, dimension (NNB)
    *>          The values of the blocksize NB.
    *> \endverbatim
    *>
    *> \param[in] NXVAL
    *> \verbatim
    *>          NXVAL is INTEGER array, dimension (NNB)
    *>          The values of the crossover point NX.
    *> \endverbatim
    *>
    *> \param[in] THRESH
    *> \verbatim
    *>          THRESH is DOUBLE PRECISION
    *>          The threshold value for the test ratios.  A result is
    *>          included in the output file if RESULT >= THRESH.  To have
    *>          every test ratio printed, use THRESH = 0.
    *> \endverbatim
    *>
    *> \param[out] A
    *> \verbatim
    *>          A is DOUBLE PRECISION array, dimension (MMAX*NMAX)
    *>          where MMAX is the maximum value of M in MVAL and NMAX is the
    *>          maximum value of N in NVAL.
    *> \endverbatim
    *>
    *> \param[out] COPYA
    *> \verbatim
    *>          COPYA is DOUBLE PRECISION array, dimension (MMAX*NMAX)
    *> \endverbatim
    *>
    *> \param[out] S
    *> \verbatim
    *>          S is DOUBLE PRECISION array, dimension
    *>                      (min(MMAX,NMAX))
    *> \endverbatim
    *>
    *> \param[out] TAU
    *> \verbatim
    *>          TAU is DOUBLE PRECISION array, dimension (MMAX)
    *> \endverbatim
    *>
    *> \param[out] WORK
    *> \verbatim
    *>          WORK is DOUBLE PRECISION array, dimension
    *>                      (MMAX*NMAX + 4*NMAX + MMAX)
    *> \endverbatim
    *>
    *> \param[out] IWORK
    *> \verbatim
    *>          IWORK is INTEGER array, dimension (2*NMAX)
    *> \endverbatim
    *>
    *> \param[in] NOUT
    *> \verbatim
    *>          NOUT is INTEGER
    *>          The unit number for output.
    *> \endverbatim
    *
    *  Authors:
    *  ========
    *
    *> \author Univ. of Tennessee
    *> \author Univ. of California Berkeley
    *> \author Univ. of Colorado Denver
    *> \author NAG Ltd.
    *
    *> \date December 2016
    *
    *> \ingroup double_lin
    *
    *  =====================================================================
    */
          private void dchkq3(boolean[] dotype, int nm, int[] mval, int nn, int[] nval,
        		  int nnb, int[] nbval, int[] nxval, double thresh, double[][] A, double[][] COPYA,
        		  double s[], double[] tau, double[] work, int[] iwork) {
    /*
    *  -- LAPACK test routine (version 3.7.0) --
    *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    *     December 2016
    *
    *     .. Scalar Arguments ..
          INTEGER            NM, NN, NNB, NOUT
          DOUBLE PRECISION   THRESH
    *     ..
    *     .. Array Arguments ..
          LOGICAL            DOTYPE( * )
          INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NVAL( * ),
         $                   NXVAL( * )
          DOUBLE PRECISION   A( * ), COPYA( * ), S( * ),
         $                   TAU( * ), WORK( * )
    *     ..
    *
    *  =====================================================================
    *
    *     .. Parameters ..
    */     
          final int ntypes = 6;
          final int ntests = 3;
          final int iseedy[] = new int[] {1988, 1989, 1990, 1991};
          //String path;
          int nrun;
          int nfail;
          int nerrs;
          int i;
          int iseed[] = new int[4];
          double eps;
          double result[] = new double[ntests];
          int im;
          int m;
          int lda;
          int in;
          int n;
          int mnmin;
          int lwork;
          int imode;
          int mode;
          int info[] = new int[1];
          int ilow = 0;
          int ihigh = 0;
          int istep = 0;
          int inb;
          int nb;
          int nx;
          int iwork2[];
          int lw;
          int k;
    /*     ..
    *     .. External Functions ..
          DOUBLE PRECISION   DLAMCH, DQPT01, DQRT11, DQRT12
          EXTERNAL           DLAMCH, DQPT01, DQRT11, DQRT12
    *     ..
    *     .. External Subroutines ..
          EXTERNAL           ALAHD, ALASUM, DGEQP3, DLACPY, DLAORD, DLASET,
         $                   DLATMS, ICOPY, XLAENV
    *     ..
    *     .. Intrinsic Functions ..
          INTRINSIC          MAX, MIN
    *     ..
    *     .. Scalars in Common ..
          LOGICAL            LERR, OK
          CHARACTER*32       SRNAMT
          INTEGER            IOUNIT
    *     ..
    *     //.. Common blocks ..
          //COMMON             / INFOC / INFOT, IOUNIT, OK, LERR
          // COMMON             / SRNAMC / SRNAMT*/
  
          // Initialize constants and the random number seed.
          //path = new String("DQ3"); // D for double precision
          nrun = 0;
          nfail = 0;
          nerrs = 0;
          for (i = 0; i < 4; i++) {
             iseed[i] = iseedy[i];
          }
          eps = ge.dlamch( 'E' );
    
          for (im = 1; im <= nm; im++) {
    
             // Do for each value of M in MVAL.
    
             m = mval[im-1];
             lda = Math.max( 1, m);
    
             for (in = 1; in <= nn; in++) {
    
                // Do for each value of N in NVAL.
    
                n = nval[in-1];
                mnmin = Math.min( m, n );
                lwork = Math.max( 1, Math.max(m*Math.max( m, n )+4*mnmin+Math.max( m, n ),
                            m*n + 2*mnmin + 4*n) );
    
                for (imode = 1; imode <= ntypes; imode++) {
                   if (!dotype[imode-1]) {
                	   continue;
                   }
    
                  // Do for each type of matrix
                      // 1:  zero matrix
                      // 2:  one small singular value
                      // 3:  geometric distribution of singular values
                      // 4:  first n/2 columns fixed
                      // 5:  last n/2 columns fixed
                      // 6:  every second column fixed
    
                   mode = imode;
                   if (imode > 3) {
                     mode = 1;
                   }
    
                   // Generate test matrix of size m by n using
                   // singular value distribution indicated by `mode'.
    
                   for (i = 0; i < n; i++) {
                      //iwork[i] = 0;
                	  // Set JPVT[j] = -1 for no pivoting
                	   iwork[i] = -1;
                   }
                   if (imode == 1) {
                      ge.dlaset( 'F', m, n, 0.0, 0.0, COPYA, lda);
                      for (i = 0; i < mnmin; i++) {
                         s[i] = 0.0;
                      }
                   } // if (imode == 1)
                   else { // imode != 1
                      ge.dlatms( m, n, 'U', iseed, 'N', s,
                                 mode, 1.0 / eps, 1.0, m, n, 'N',
                                 COPYA, lda, work, info);
                      if (imode >= 4) {
                         if (imode == 4) {
                            ilow = 1;
                            istep = 1;
                            ihigh = Math.max( 1, n / 2 );
                         }
                         else if (imode == 5) {
                            ilow = Math.max( 1, n / 2 );
                            istep = 1;
                            ihigh = n;
                         }
                         else if (imode == 6) {
                            ilow = 1;
                            istep = 2;
                            ihigh = n;
                         }
                         for(i = ilow; i <= ihigh; i += istep) {
                            iwork[i-1] = 1;
                         }
                      } // if (imode >= 4)
                      gi2.dlaord( 'D', mnmin, s, 1 );
                   } // else imode != 1
    
                   for (inb = 1; inb <= nnb; inb++) {
    
                      // Do for each pair of values (NB,NX) in NBVAL and NXVAL.
    
                      nb = nbval[inb-1];
                      xlaenv( 1, nb);
                      nx = nxval[inb-1];
                      xlaenv( 3, nx);
    
                      // Get a working copy of COPYA into A and a copy of
                      // vector IWORK.
    
                      ge.dlacpy( 'A', m, n, COPYA, lda, A, lda);
                      iwork2 = new int[n];
                      for (i = 0; i < n; i++) {
                    	  iwork2[i] = iwork[i];
                      }
         
    
                      // Compute the QR factorization with pivoting of A
    
                      lw = Math.max( 1, 2*n+nb*( n+1 ) );
    
                      // Compute the QP3 factorization of A
    
                    
                      dgeqp3( m, n, A, lda, iwork2, tau, work, lw, info);
    
                      // Compute norm(svd(a) - svd(r))
    
                      result[0] = dqrt12( m, n, A, lda, s, work, lwork);
    
                      // Compute norm( A*P - Q*R )
    
                      result[1] = dqpt01( m, n, mnmin, COPYA, A, lda, tau,
                                   iwork2, work, lwork);
    
                      // Compute Q'*Q
    
                      result[2] = dqrt11( m, mnmin, A, lda, tau, work, lwork);
    
                      // Print information about the tests that did not pass
                      // the threshold.
    
                      for (k = 1; k <= ntests; k++) {
                         if (result[k-1] >= thresh) {
                            if((nfail == 0) && (nerrs == 0) ) {
                              //CALL ALAHD( NOUT, PATH )
                              // path DQ3 not found in ALAHD 
                              Preferences.debug("1. Zero matrix\n", Preferences.DEBUG_ALGORITHM);
                              Preferences.debug("2. One small singular value\n", Preferences.DEBUG_ALGORITHM);
                              Preferences.debug("3. Geometric distribution of singular values\n", Preferences.DEBUG_ALGORITHM);
                              Preferences.debug("4. First n/2 columns fixed\n", Preferences.DEBUG_ALGORITHM);
                              Preferences.debug("5. Last n/2 columns fixed\n", Preferences.DEBUG_ALGORITHM);
                              Preferences.debug("6. Every second column fixed\n", Preferences.DEBUG_ALGORITHM);
                            } // if((nfail == 0) && (nerrs == 0) ) 
                            Preferences.debug("dgeqp3 m = " + m + " n =" + n + " nb = " + nb + '\n', Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("type = " + imode + " test = " + k + " ratio = " + result[k-1] + "\n",
                            		Preferences.DEBUG_ALGORITHM);
                            nfail = nfail + 1;
                         } // if (result[k-1] >= thresh)
                      } // for (k = 1; k <= ntests; k++)
                      nrun = nrun + ntests;
    
                   } // for (inb = 1; inb <= nnb; inb++)
                } // for (imode = 1; imode <= ntypes; imode++)
             } // for (in = 1; in <= nn; in++)
          } // for (im = 1; im <= nm; im++)
    
          // Print a summary of the results.
          if (nfail > 0) {
              Preferences.debug("In dchkq3 " + nfail + " out of " + nrun + " tests failed to pass the threshold\n");
          } else {
              Preferences.debug("In dchkq3 all " + nrun + " tests run passed the threshold\n");
          }
 
          } // dchkq3
          
      /*> \brief \b DQRT12
      *
      *  =========== DOCUMENTATION ===========
      *
      * Online html documentation available at
      *            http://www.netlib.org/lapack/explore-html/
      *
      *  Definition:
      *  ===========
      *
      *       DOUBLE PRECISION FUNCTION DQRT12( M, N, A, LDA, S, WORK, LWORK )
      *
      *       .. Scalar Arguments ..
      *       INTEGER            LDA, LWORK, M, N
      *       ..
      *       .. Array Arguments ..
      *       DOUBLE PRECISION   A( LDA, * ), S( * ), WORK( LWORK )
      *       ..
      *
      *
      *> \par Purpose:
      *  =============
      *>
      *> \verbatim
      *>
      *> DQRT12 computes the singular values `svlues' of the upper trapezoid
      *> of A(1:M,1:N) and returns the ratio
      *>
      *>      || s - svlues||/(||svlues||*eps*max(M,N))
      *> \endverbatim
      *
      *  Arguments:
      *  ==========
      *
      *> \param[in] M
      *> \verbatim
      *>          M is INTEGER
      *>          The number of rows of the matrix A.
      *> \endverbatim
      *>
      *> \param[in] N
      *> \verbatim
      *>          N is INTEGER
      *>          The number of columns of the matrix A.
      *> \endverbatim
      *>
      *> \param[in] A
      *> \verbatim
      *>          A is DOUBLE PRECISION array, dimension (LDA,N)
      *>          The M-by-N matrix A. Only the upper trapezoid is referenced.
      *> \endverbatim
      *>
      *> \param[in] LDA
      *> \verbatim
      *>          LDA is INTEGER
      *>          The leading dimension of the array A.
      *> \endverbatim
      *>
      *> \param[in] S
      *> \verbatim
      *>          S is DOUBLE PRECISION array, dimension (min(M,N))
      *>          The singular values of the matrix A.
      *> \endverbatim
      *>
      *> \param[out] WORK
      *> \verbatim
      *>          WORK is DOUBLE PRECISION array, dimension (LWORK)
      *> \endverbatim
      *>
      *> \param[in] LWORK
      *> \verbatim
      *>          LWORK is INTEGER
      *>          The length of the array WORK. LWORK >= max(M*N + 4*min(M,N) +
      *>          max(M,N), M*N+2*MIN( M, N )+4*N).
      *> \endverbatim
      *
      *  Authors:
      *  ========
      *
      *> \author Univ. of Tennessee
      *> \author Univ. of California Berkeley
      *> \author Univ. of Colorado Denver
      *> \author NAG Ltd.
      *
      *> \date December 2016
      *
      *> \ingroup double_lin
      *
      *  =====================================================================
      */  
            private double dqrt12(int m, int n, double A[][], int lda,
            		double s[], double work[], int lwork) {
      /*
      *  -- LAPACK test routine (version 3.7.0) --
      *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
      *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
      *     December 2016
      *
      *     .. Scalar Arguments ..
            INTEGER            LDA, LWORK, M, N
      *     ..
      *     .. Array Arguments ..
            DOUBLE PRECISION   A( LDA, * ), S( * ), WORK( LWORK )
      *     ..
      *
      *  =====================================================================
      *
      *     .. Parameters ..
            DOUBLE PRECISION   ZERO, ONE
            PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
      *     ..
      *     .. Local Scalars ..
      */
            int info[] = new int[1];
            double dummy[] = new double[1];
            double DUM[][] = new double[1][1];
            double smlnum[] = new double[1];
            double bignum[] = new double[1];
            double W[][];
            int            i, iscl, j, mn;
            double   anrm, nrmsvl;
            double answer;
            double d[];
            double D[][];
            double e[];
            double tauq[];
            double taup[];
            double workspace[];
            double workspace2[];
            double abssum;
      /*     ..
      *     .. External Functions ..
            DOUBLE PRECISION   DASUM, DLAMCH, DLANGE, DNRM2
            EXTERNAL           DASUM, DLAMCH, DLANGE, DNRM2
      *     ..
      *     .. External Subroutines ..
            EXTERNAL           DAXPY, DBDSQR, DGEBD2, DLABAD, DLASCL, DLASET,
           $                   XERBLA
      *     ..
      *     .. Intrinsic Functions ..
            INTRINSIC          DBLE, MAX, MIN
      *     ..
      *     .. Executable Statements ..
      */
            answer = 0.0;
      
            // Test that enough workspace is supplied
      
            if (lwork < Math.max( m*n+4*Math.min( m, n )+Math.max( m, n ),
                        m*n+2*Math.min( m, n )+4*n) ) {
               MipavUtil.displayError("workspace is too small in dqrt12");
               return answer;
            }
      
            // Quick return if possible
      
            mn = Math.min( m, n);
            if (mn <= 0) {
                return answer;
            }
      
            nrmsvl = ge.dnrm2( mn, s, 1 );
      
            // Copy upper triangle of A into work
      
            for (i = 0; i < m*n; i++) {
            	work[i] = 0;
            }
            for (j = 1; j <= n; j++) {
               for (i = 1; i <= Math.min( j, m ); i++) {
                  work[( j-1 )*m+i-1 ] = A[i-1][j-1];
               }
            }
      
            // Get machine parameters
      
            smlnum[0] = ge.dlamch( 'S' ) / ge.dlamch( 'P' );
            bignum[0] = 1.0 / smlnum[0];
            ge.dlabad(smlnum, bignum);
      
            // Scale work if max entry outside range [SMLNUM,BIGNUM]
      
            W = new double[m][n];
            for (j = 0; j < n; j++) {
                for (i = 0; i < m; i++) {
                   W[i][j] = work[j*m+i];
                }
             }
            anrm = ge.dlange( 'M', m, n, W, m, dummy);
            iscl = 0;
            if ((anrm > 0.0) && (anrm < smlnum[0])) {
      
               // Scale matrix norm up to SMLNUM
      
               ge.dlascl( 'G', 0, 0, anrm, smlnum[0], m, n, W, m, info);
               iscl = 1;
            }
            else if(anrm > bignum[0]) {
      
               // Scale matrix norm down to BIGNUM
      
               ge.dlascl( 'G', 0, 0, anrm, bignum[0], m, n, W, m, info);
               iscl = 1;
            }
            for (j = 0; j < n; j++) {
                for (i = 0; i < m; i++) {
                   work[j*m+i] = W[i][j];
                }
             }
      
            if (anrm != 0.0) {
      
               // Compute SVD of work
               d = new double[mn];
               e = new double[mn-1];
               tauq = new double[mn];
               taup = new double[mn];
               workspace = new double[Math.max(m,n)];
               workspace2 = new double[4*mn];
               gi2.dgebd2( m, n, W, m, d, e,
                           tauq, taup,
                           workspace, info);
               for (j = 0; j < n; j++) {
                   for (i = 0; i < m; i++) {
                      work[j*m+i] = W[i][j];
                   }
                }
               gi2.dbdsqr( 'U', mn, 0, 0, 0, d,
                            e, DUM, mn, DUM, 1, DUM, mn,
                            workspace2, info);
      
               if (iscl == 1) {
            	  D = new double[mn][1];
            	  for (i = 0; i < mn; i++) {
            		  D[i][0] = d[i];
            	  }
                  if(anrm > bignum[0]) {
                     ge.dlascl( 'G', 0, 0, bignum[0], anrm, mn, 1, D, mn, info);
                  }
                  if( anrm < smlnum[0]) {
                     ge.dlascl( 'G', 0, 0, smlnum[0], anrm, mn, 1, D, mn, info);
                  }
                  for (i = 0; i < mn; i++) {
                	  d[i] = D[i][0];
                  }
               } // if (iscl == 1)
               for (i = 0; i < mn; i++) {
             	  work[m*n + i] = d[i];
               }
               for (i = 0; i < mn-1; i++) {
            	   work[m*n+mn+i] = e[i];
               }
            } // if (anrm != 0.0)
            else {
      
               for (i = 1; i <= mn; i++) {
                  work[ m*n+i-1 ] = 0.0;
               }
            }
      
            // Compare s and singular values of work
      
            d = new double[mn];
            for (i = 0; i < mn; i++) {
            	d[i] = work[m*n+i];
            }
            ge.daxpy( mn, -1.0, s, 1, d, 1 );
            abssum = 0.0;
            for (i = 0; i < mn; i++) {
            	work[m*n+i] = d[i];
            	abssum += Math.abs(d[i]);
            }
            answer = abssum /( ge.dlamch( 'E' )*(double)Math.max( m, n ) ) ;
            if(nrmsvl != 0.0) {
              answer = answer/nrmsvl;
            }
      
            return answer;
            } // dqrt12
                
        /*> \brief \b DQPT01
        *
        *  =========== DOCUMENTATION ===========
        *
        * Online html documentation available at
        *            http://www.netlib.org/lapack/explore-html/
        *
        *  Definition:
        *  ===========
        *
        *       DOUBLE PRECISION FUNCTION DQPT01( M, N, K, A, AF, LDA, TAU, JPVT,
        *                        WORK, LWORK )
        *
        *       .. Scalar Arguments ..
        *       INTEGER            K, LDA, LWORK, M, N
        *       ..
        *       .. Array Arguments ..
        *       INTEGER            JPVT( * )
        *       DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), TAU( * ),
        *      $                   WORK( LWORK )
        *       ..
        *
        *
        *> \par Purpose:
        *  =============
        *>
        *> \verbatim
        *>
        *> DQPT01 tests the QR-factorization with pivoting of a matrix A.  The
        *> array AF contains the (possibly partial) QR-factorization of A, where
        *> the upper triangle of AF(1:k,1:k) is a partial triangular factor,
        *> the entries below the diagonal in the first k columns are the
        *> Householder vectors, and the rest of AF contains a partially updated
        *> matrix.
        *>
        *> This function returns ||A*P - Q*R||/(||norm(A)||*eps*M)
        *> \endverbatim
        *
        *  Arguments:
        *  ==========
        *
        *> \param[in] M
        *> \verbatim
        *>          M is INTEGER
        *>          The number of rows of the matrices A and AF.
        *> \endverbatim
        *>
        *> \param[in] N
        *> \verbatim
        *>          N is INTEGER
        *>          The number of columns of the matrices A and AF.
        *> \endverbatim
        *>
        *> \param[in] K
        *> \verbatim
        *>          K is INTEGER
        *>          The number of columns of AF that have been reduced
        *>          to upper triangular form.
        *> \endverbatim
        *>
        *> \param[in] A
        *> \verbatim
        *>          A is DOUBLE PRECISION array, dimension (LDA, N)
        *>          The original matrix A.
        *> \endverbatim
        *>
        *> \param[in] AF
        *> \verbatim
        *>          AF is DOUBLE PRECISION array, dimension (LDA,N)
        *>          The (possibly partial) output of DGEQPF.  The upper triangle
        *>          of AF(1:k,1:k) is a partial triangular factor, the entries
        *>          below the diagonal in the first k columns are the Householder
        *>          vectors, and the rest of AF contains a partially updated
        *>          matrix.
        *> \endverbatim
        *>
        *> \param[in] LDA
        *> \verbatim
        *>          LDA is INTEGER
        *>          The leading dimension of the arrays A and AF.
        *> \endverbatim
        *>
        *> \param[in] TAU
        *> \verbatim
        *>          TAU is DOUBLE PRECISION array, dimension (K)
        *>          Details of the Householder transformations as returned by
        *>          DGEQPF.
        *> \endverbatim
        *>
        *> \param[in] JPVT
        *> \verbatim
        *>          JPVT is INTEGER array, dimension (N)
        *>          Pivot information as returned by DGEQPF.
        *> \endverbatim
        *>
        *> \param[out] WORK
        *> \verbatim
        *>          WORK is DOUBLE PRECISION array, dimension (LWORK)
        *> \endverbatim
        *>
        *> \param[in] LWORK
        *> \verbatim
        *>          LWORK is INTEGER
        *>          The length of the array WORK.  LWORK >= M*N+N.
        *> \endverbatim
        *
        *  Authors:
        *  ========
        *
        *> \author Univ. of Tennessee
        *> \author Univ. of California Berkeley
        *> \author Univ. of Colorado Denver
        *> \author NAG Ltd.
        *
        *> \date December 2016
        *
        *> \ingroup double_lin
        *
        *  =====================================================================
        */  
              private double dqpt01( int m, int n, int k, double A[][], double AF[][], 
            		  int lda, double tau[], int jpvt[], double work[], int lwork) {
        /*
        *  -- LAPACK test routine (version 3.7.0) --
        *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
        *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
        *     December 2016
        *
        *     .. Scalar Arguments ..
              INTEGER            K, LDA, LWORK, M, N
        *     ..
        *     .. Array Arguments ..
              INTEGER            JPVT( * )
              DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), TAU( * ),
             $                   WORK( LWORK )
        *     ..
        *
        *  =====================================================================
        *
        *     .. Parameters ..
              DOUBLE PRECISION   ZERO, ONE
              PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
        *     ..
        *     .. Local Scalars ..
        */
              double rwork[] = new double[1];
              int info[] = new int[1];
              int i,j;
              double norma;
              double answer;
              double work2[];
              double W[][];
              double x[];
              double y[];
        /*     ..
        *     .. External Functions ..
              DOUBLE PRECISION   DLAMCH, DLANGE
              EXTERNAL           DLAMCH, DLANGE
        *     ..
        *     .. External Subroutines ..
              EXTERNAL           DAXPY, DCOPY, DORMQR, XERBLA
        *     ..
        *     .. Intrinsic Functions ..
              INTRINSIC          DBLE, MAX, MIN
        *     ..
        *     .. Executable Statements ..
        */
              answer = 0.0;
        
              // Test if there is enough workspace
        
              if (lwork < m*n+n) {
                 MipavUtil.displayError("Not enough workspace in dqpt01");
                 return answer;
              }
        
              // Quick return if possible
        
              if ((m <= 0) || (n <= 0) ) {
                  return answer;
              }
        
              norma = ge.dlange( 'O', m, n, A, lda, rwork);
        
              for (j = 1; j <= k; j++) {
                 for (i = 1; i <= Math.min( j, m ); i++) {
                    work[ ( j-1 )*m+i-1 ] = AF[i-1][j-1];
                 }
                 for (i = j + 1; i <= m; i++) {
                    work[( j-1 )*m+i-1 ] = 0.0;
                 }
              } // for (j = 1; j <= k; j++)
              for (j = k + 1; j <= n; j++) {
            	 for (i = 0; i < m; i++) {
            		 work[(j-1)*m + i] = AF[i][j-1];
            	 }
              }
              W = new double[m][n];
              for (j = 0; j < n; j++) {
            	  for (i = 0; i < m; i++) {
            		  W[i][j] = work[j*m+i];
            	  }
              }
        
              work2 = new double[Math.max(1,lwork-m*n)];
              ge.dormqr( 'L', 'N', m, n, k, AF, lda, tau, W,
                          m, work2, lwork-m*n, info);
              for (j = 0; j < n; j++) {
            	  for (i = 0; i < m; i++) {
            		  work[j*m+i] = W[i][j];
            	  }
              }
              for (i = 0; i < work2.length; i++) {
            	  work[m*n + i] = work2[i];
              }
        
              for (j = 0; j < n; j++) {
        
                 // Compare i-th column of QR and jpvt(i)-th column of A
                 x = new double[m];
                 y = new double[m];
                 for (i = 0; i < m; i++) {
                	 x[i] = A[i][jpvt[j]];
                	 y[i] = work[j*m + i];
                 }
                 ge.daxpy( m, -1.0, x, 1, y,1 );
                 for (i = 0; i < m; i++) {
                	 work[j*m + i] = y[i];
                 }
              }
        
              for (j = 0; j < n; j++) {
            	  for (i = 0; i < m; i++) {
            		  W[i][j] = work[j*m+i];
            	  }
              }
              answer = ge.dlange( 'O', m, n, W, m, rwork ) /(((double)( Math.max( m, n ) ))*ge.dlamch( 'E' ) );
              if (norma != 0.0) {
                  answer = answer / norma;
              }
        
              return answer;
              } // dqpt01
        
      /*> \brief \b DQRT11
      *
      *  =========== DOCUMENTATION ===========
      *
      * Online html documentation available at
      *            http://www.netlib.org/lapack/explore-html/
      *
      *  Definition:
      *  ===========
      *
      *       DOUBLE PRECISION FUNCTION DQRT11( M, K, A, LDA, TAU, WORK, LWORK )
      *
      *       .. Scalar Arguments ..
      *       INTEGER            K, LDA, LWORK, M
      *       ..
      *       .. Array Arguments ..
      *       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
      *       ..
      *
      *
      *> \par Purpose:
      *  =============
      *>
      *> \verbatim
      *>
      *> DQRT11 computes the test ratio
      *>
      *>       || Q'*Q - I || / (eps * m)
      *>
      *> where the orthogonal matrix Q is represented as a product of
      *> elementary transformations.  Each transformation has the form
      *>
      *>    H(k) = I - tau(k) v(k) v(k)'
      *>
      *> where tau(k) is stored in TAU(k) and v(k) is an m-vector of the form
      *> [ 0 ... 0 1 x(k) ]', where x(k) is a vector of length m-k stored
      *> in A(k+1:m,k).
      *> \endverbatim
      *
      *  Arguments:
      *  ==========
      *
      *> \param[in] M
      *> \verbatim
      *>          M is INTEGER
      *>          The number of rows of the matrix A.
      *> \endverbatim
      *>
      *> \param[in] K
      *> \verbatim
      *>          K is INTEGER
      *>          The number of columns of A whose subdiagonal entries
      *>          contain information about orthogonal transformations.
      *> \endverbatim
      *>
      *> \param[in] A
      *> \verbatim
      *>          A is DOUBLE PRECISION array, dimension (LDA,K)
      *>          The (possibly partial) output of a QR reduction routine.
      *> \endverbatim
      *>
      *> \param[in] LDA
      *> \verbatim
      *>          LDA is INTEGER
      *>          The leading dimension of the array A.
      *> \endverbatim
      *>
      *> \param[in] TAU
      *> \verbatim
      *>          TAU is DOUBLE PRECISION array, dimension (K)
      *>          The scaling factors tau for the elementary transformations as
      *>          computed by the QR factorization routine.
      *> \endverbatim
      *>
      *> \param[out] WORK
      *> \verbatim
      *>          WORK is DOUBLE PRECISION array, dimension (LWORK)
      *> \endverbatim
      *>
      *> \param[in] LWORK
      *> \verbatim
      *>          LWORK is INTEGER
      *>          The length of the array WORK.  LWORK >= M*M + M.
      *> \endverbatim
      *
      *  Authors:
      *  ========
      *
      *> \author Univ. of Tennessee
      *> \author Univ. of California Berkeley
      *> \author Univ. of Colorado Denver
      *> \author NAG Ltd.
      *
      *> \date December 2016
      *
      *> \ingroup double_lin
      *
      *  =====================================================================
      */  
            private double dqrt11(int m, int k, double A[][], int lda,
            		double tau[], double work[], int lwork) {
      /*
      *  -- LAPACK test routine (version 3.7.0) --
      *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
      *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
      *     December 2016
      *
      *     .. Scalar Arguments ..
            INTEGER            K, LDA, LWORK, M
      *     ..
      *     .. Array Arguments ..
            DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
      *     ..
      *
      *  =====================================================================
      *
      *     .. Parameters ..
            DOUBLE PRECISION   ZERO, ONE
            PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
      *     ..
      *     .. Local Scalars ..
      */
            int info[] = new int[1];
            int i;
            int j;
            double rdummy[] = new double[1];
            double answer;
            double W[][];
            double workspace[];
      /*     ..
      *     .. External Functions ..
            DOUBLE PRECISION   DLAMCH, DLANGE
            EXTERNAL           DLAMCH, DLANGE
      *     ..
      *     .. External Subroutines ..
            EXTERNAL           DLASET, DORM2R, XERBLA
      *     ..
      *     .. Intrinsic Functions ..
            INTRINSIC          DBLE
      *     ..
      *     .. Executable Statements ..
      */
            answer = 0.0;
      
            // Test for sufficient workspace
      
            if(lwork < m*m + m) {
               MipavUtil.displayError("Workspace is too small in dqrt11");
               return answer;
            }
      
            // Quick return if possible
      
            if (m <= 0) {
                return answer;
            }    
      
            W = new double[m][m];
            for (j = 0; j < m; j++) {
          	  for (i = 0; i < m; i++) {
          		  W[i][j] = work[j*m+i];
          	  }
            }
            ge.dlaset( 'F', m, m, 0.0, 1.0, W, m);
      
            // Form Q
            workspace = new double[m];
            ge.dorm2r( 'L', 'N', m, m, k, A, m, tau, W,
                        m, workspace, info);
      
            // Form Q'*Q
      
            ge.dorm2r( 'L', 'T', m, m, k, A, m, tau, W, m,
                       workspace, info);
            for (j = 0; j < m; j++) {
            	  for (i = 0; i < m; i++) {
            		  work[j*m+i] = W[i][j];
            	  }
              }
      
            for (j = 1; j <= m; j++) {
               work[( j-1 )*m+j-1 ] = work[( j-1 )*m+j-1] - 1.0;
            }
      
            for (j = 0; j < m; j++) {
            	  for (i = 0; i < m; i++) {
            		  W[i][j] = work[j*m+i];
            	  }
              }
            answer = ge.dlange( 'O', m, m, W, m, rdummy) /
                     ( ((double) m )*ge.dlamch( 'E' ) );
      
            return answer;
            } // dqrt11


    	
    	/*> \brief \b DGEQP3
        *
        *  =========== DOCUMENTATION ===========
        *
        * Online html documentation available at
        *            http://www.netlib.org/lapack/explore-html/
        *
        *> \htmlonly
        *> Download DGEQP3 + dependencies
        *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dgeqp3.f">
        *> [TGZ]</a>
        *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dgeqp3.f">
        *> [ZIP]</a>
        *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dgeqp3.f">
        *> [TXT]</a>
        *> \endhtmlonly
        *
        *  Definition:
        *  ===========
        *
        *       SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
        *
        *       .. Scalar Arguments ..
        *       INTEGER            INFO, LDA, LWORK, M, N
        *       ..
        *       .. Array Arguments ..
        *       INTEGER            JPVT( * )
        *       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        *       ..
        *
        *
        *> \par Purpose:
        *  =============
        *>
        *> \verbatim
        *>
        *> DGEQP3 computes a QR factorization with column pivoting of a
        *> matrix A:  A*P = Q*R  using Level 3 BLAS.
        *> \endverbatim
        *
        *  Arguments:
        *  ==========
        *
        *> \param[in] M
        *> \verbatim
        *>          M is INTEGER
        *>          The number of rows of the matrix A. M >= 0.
        *> \endverbatim
        *>
        *> \param[in] N
        *> \verbatim
        *>          N is INTEGER
        *>          The number of columns of the matrix A.  N >= 0.
        *> \endverbatim
        *>
        *> \param[in,out] A
        *> \verbatim
        *>          A is DOUBLE PRECISION array, dimension (LDA,N)
        *>          On entry, the M-by-N matrix A.
        *>          On exit, the upper triangle of the array contains the
        *>          min(M,N)-by-N upper trapezoidal matrix R; the elements below
        *>          the diagonal, together with the array TAU, represent the
        *>          orthogonal matrix Q as a product of min(M,N) elementary
        *>          reflectors.
        *> \endverbatim
        *>
        *> \param[in] LDA
        *> \verbatim
        *>          LDA is INTEGER
        *>          The leading dimension of the array A. LDA >= max(1,M).
        *> \endverbatim
        *>
        *> \param[in,out] JPVT
        *> \verbatim
        *>          JPVT is INTEGER array, dimension (N)
        *>          On entry, if JPVT(J).ne.-1, the J-th column of A is permuted
        *>          to the front of A*P (a leading column); if JPVT(J)=-1,
        *>          the J-th column of A is a free column.
        *>          On exit, if JPVT(J)=K, then the J-th column of A*P was the
        *>          the K-th column of A.
        *> \endverbatim
        *>
        *> \param[out] TAU
        *> \verbatim
        *>          TAU is DOUBLE PRECISION array, dimension (min(M,N))
        *>          The scalar factors of the elementary reflectors.
        *> \endverbatim
        *>
        *> \param[out] WORK
        *> \verbatim
        *>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        *>          On exit, if INFO=0, WORK(1) returns the optimal LWORK.
        *> \endverbatim
        *>
        *> \param[in] LWORK
        *> \verbatim
        *>          LWORK is INTEGER
        *>          The dimension of the array WORK. LWORK >= 3*N+1.
        *>          For optimal performance LWORK >= 2*N+( N+1 )*NB, where NB
        *>          is the optimal blocksize.
        *>
        *>          If LWORK = -1, then a workspace query is assumed; the routine
        *>          only calculates the optimal size of the WORK array, returns
        *>          this value as the first entry of the WORK array, and no error
        *>          message related to LWORK is issued by XERBLA.
        *> \endverbatim
        *>
        *> \param[out] INFO
        *> \verbatim
        *>          INFO is INTEGER
        *>          = 0: successful exit.
        *>          < 0: if INFO = -i, the i-th argument had an illegal value.
        *> \endverbatim
        *
        *  Authors:
        *  ========
        *
        *> \author Univ. of Tennessee
        *> \author Univ. of California Berkeley
        *> \author Univ. of Colorado Denver
        *> \author NAG Ltd.
        *
        *> \date December 2016
        *
        *> \ingroup doubleGEcomputational
        *
        *> \par Further Details:
        *  =====================
        *>
        *> \verbatim
        *>
        *>  The matrix Q is represented as a product of elementary reflectors
        *>
        *>     Q = H(1) H(2) . . . H(k), where k = min(m,n).
        *>
        *>  Each H(i) has the form
        *>
        *>     H(i) = I - tau * v * v**T
        *>
        *>  where tau is a real scalar, and v is a real/complex vector
        *>  with v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
        *>  A(i+1:m,i), and tau in TAU(i).
        *> \endverbatim
        *
        *> \par Contributors:
        *  ==================
        *>
        *>    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
        *>    X. Sun, Computer Science Dept., Duke University, USA
        *>
        *  =====================================================================
        *  */
              public void dgeqp3(int m, int n, double A[][], int lda, int jpvt[], double tau[],
            		  double work[], int lwork, int info[]) {
        /*
        *  -- LAPACK computational routine (version 3.7.0) --
        *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
        *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
        *     December 2016
        *
        *     .. Scalar Arguments ..
              INTEGER            INFO, LDA, LWORK, M, N
        *     ..
        *     .. Array Arguments ..
              INTEGER            JPVT( * )
              DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        *     ..
        *
        *  =====================================================================
        *
        *     .. Parameters ..
              INTEGER            INB, INBMIN, IXOVER
              PARAMETER          ( INB = 1, INBMIN = 2, IXOVER = 3 )
              */
              final int inb = 1;
              final int inbmin = 2;
              final int ixover = 3;
        //     ..
        //    .. Local Scalars ..
              boolean            lquery;
              int                iws = 0;
              int                minmn = 0;
              int                fjb[] = new int[1];
              int                j, jb, lwkopt, minws, na, nb,
                                 nbmin, nfxd, nx, sm, sminmn, sn, topbmn,i,k;
              double             temp;
              double             x[];
              double             Ainout[][];
              int                jpvtinout[];
              double             tauout[];
              double             vn1[];
              double             vn2[];
              double             auxv[];
              double             F[][];
              double             work2[];
        /*     ..
        *     .. External Subroutines ..
              EXTERNAL           DGEQRF, DLAQP2, DLAQPS, DORMQR, DSWAP, XERBLA
        *     ..
        *     .. External Functions ..
              INTEGER            ILAENV
              DOUBLE PRECISION   DNRM2
              EXTERNAL           ILAENV, DNRM2
        *     ..
        *     .. Intrinsic Functions ..
              INTRINSIC          INT, MAX, MIN
        *     ..
        *     .. Executable Statements ..
        *
        *     Test input arguments
        *  ====================
        */
              info[0] = 0;
              lquery = (lwork == -1 );
              if (m < 0) {
                 info[0] = -1;
              }
              else if (n < 0) {
                 info[0] = -2;
              }
              else if(lda < Math.max(1,m)) {
                 info[0] = -4;
              }
        
              if (info[0] == 0) {
                 minmn = Math.min( m, n );
                 if(minmn == 0 ) {
                    iws = 1;
                    lwkopt= 1;
                 }
                 else {
                    iws = 3*n + 1;
                    nb = ge.ilaenv( inb, "DGEQRF", " ", m, n, -1, -1 );
                    lwkopt = 2*n + ( n + 1 )*nb;
                 }
                 work[0] = lwkopt;
        
                 if( (lwork < iws) && (!lquery) ) {
                    info[0] = -8;
                 }
              } // if (info[0] == 0)
        
              if (info[0] != 0 ) {
            	 MipavUtil.displayError("dgeqp3 had info[0] = " + info[0]);
                 return;
              }
              else if (lquery) {
                 return;
              }
        
              //     Move initial columns up front.
        
              nfxd = 1;
              for (j = 0; j < n; j++) {
                 if( jpvt[j] != -1) {
                    if(j != (nfxd-1) ) {
                       for (k = 0; k < m; k++) {
                           temp = A[k][j];
                           A[k][j] = A[k][nfxd-1];
                           A[k][nfxd-1] = temp;
                       }
                       jpvt[j] = jpvt[nfxd-1];
                       jpvt[nfxd-1] = j;
                    }
                    else {
                       jpvt[j] = j;
                    }
                    nfxd = nfxd + 1;
                 } // if (jpvt[j] != -1)
                 else {
                    jpvt[j] = j;
                 }
              } // for (j = 0; j < n; j++)
              nfxd = nfxd - 1;
        
        //    Factorize fixed columns
        //  =======================
        
        //     Compute the QR factorization of fixed columns and update
        //     remaining columns.
        
              if (nfxd > 0 ) {
                 na = Math.min( m, nfxd );
        //       CALL DGEQR2( M, NA, A, LDA, TAU, WORK, INFO )
                 ge.dgeqrf( m, na, A, lda, tau, work, lwork, info);
                 iws = Math.max(iws, (int)work[0]);
                 if (na < n) {
        //          CALL DORM2R( 'Left', 'Transpose', M, N-NA, NA, A, LDA,
        //                   TAU, A( 1, NA+1 ), LDA, WORK, INFO )
                	double C[][] = new double[lda][n-na];
                	for (i = 0; i < lda; i++) {
                		for (j = na; j < n; j++) {
                			C[i][j-na] = A[i][j];
                		}
                	}
                    ge.dormqr( 'L', 'T', m, n-na, na, A, lda, tau,
                               C, lda, work, lwork, info);
                    for (i = 0; i < lda; i++) {
                		for (j = na; j < n; j++) {
                			A[i][j] = C[i][j-na];
                		}
                	}
                    iws = Math.max( iws, (int)work[0]);
                 } // if (na < n)
              } // if (nfxd > 0)
        
        //     Factorize free columns
        //  ======================
        
              if (nfxd < minmn) {
        
                 sm = m - nfxd;
                 sn = n - nfxd;
                 sminmn = minmn - nfxd;
        
        //       Determine the block size.
        
                 nb = ge.ilaenv(inb, "DGEQRF", " ", sm, sn, -1, -1 );
                 nbmin = 2;
                 nx = 0;
        
                 if ( (nb > 1 ) && (nb < sminmn) ) {
        
        //          Determine when to cross over from blocked to unblocked code.
        
                    nx = Math.max( 0, ge.ilaenv(ixover, "DGEQRF", " ", sm, sn, -1,-1 ) );
        
        
                    if (nx < sminmn) {
        
        //             Determine if workspace is large enough for blocked code.
        
                       minws = 2*sn + ( sn+1 )*nb;
                       iws = Math.max(iws, minws);
                       if (lwork < minws) {
        
        //                Not enough workspace to use optimal NB: Reduce NB and
        //                determine the minimum value of NB.
        
                          nb = ( lwork-2*sn ) / ( sn+1 );
                          nbmin = Math.max( 2, ge.ilaenv(inbmin, "DGEQRF", " ", sm, sn, -1, -1 ) );
        
        
                       } // if (lwork < minws)
                    } // if (nx < sminmn)
                 } // if ( (nb > 1 ) && (nb < sminmn) )
        
        //       Initialize partial column norms. The first N elements of work
        //       store the exact column norms.
        
                 for (j = nfxd; j < n; j++) {
                	x = new double[sm];
                	for (i = 0; i < sm; i++) {
                		x[i] = A[nfxd+i][j];
                	}
                    work[j] = ge.dnrm2(sm, x, 1 );
                    work[n+j] = work[j];
                 } // for (j = nfxd; j < n; j++)
        
                 if ( (nb >= nbmin) && (nb < sminmn) && (nx < sminmn) ) {
        
        //          Use blocked code initially.
        
                    j = nfxd + 1;
        
        //          Compute factorization: while loop.
        
        
                    topbmn = minmn - nx;
                    while (j <= topbmn) {
                       jb = Math.min( nb, topbmn-j+1 );
        
        //             Factorize JB columns among columns J:N.
        
                       Ainout = new double[m][n-j+1];
                       for (i = 0; i < m; i++) {
                    	   for (k = 0; k < n-j+1; k++) {
                    		   Ainout[i][k] = A[i][j-1+k];
                    	   }
                       }
                       jpvtinout = new int[n-j+1];
                       for (i = 0; i < n-j+1; i++) {
                           jpvtinout[i] = jpvt[j-1+i];
                       }
                       tauout = new double[jb];
                       vn1 = new double[n-j+1];
                       for (i = 0; i < n-j+1; i++) {
                    	   vn1[i] = work[j-1+i];
                       }
                       vn2 = new double[n-j+1];
                       for (i = 0; i < n-j+1; i++) {
                    	   vn2[i] = work[n+j-1+i];
                       }
                       auxv = new double[jb];
                       for (i = 0; i < jb; i++) {
                    	   auxv[i] = work[2*n + i];
                       }
                       F = new double[n-j+1][jb];
                       for (k = 0; k < jb; k++) {
                    	   for (i = 0; i < n-j+1; i++) {
                    		   F[i][k] = work[2*nb+jb+i+k*(n-j+1)];
                    	   }
                       }
                       dlaqps( m, n-j+1, j-1, jb, fjb, Ainout, m,
                               jpvtinout, tauout, vn1, vn2,
                               auxv, F, n-j+1 );
                       for (i = 0; i < m; i++) {
                    	   for (k = 0; k < n-j+1; k++) {
                    		   A[i][j-1+k] = Ainout[i][k];
                    	   }
                       }
                       for (i = 0; i < n-j+1; i++) {
                           jpvt[j-1+i] = jpvtinout[i];
                       }
                       for (i = 0; i < fjb[0]; i++) {
                    	   tau[j-1+i] = tauout[i];
                       }
                       for (i = 0; i < n-j+1; i++) {
                    	   work[j-1+i] = vn1[i];
                       }
                       for (i = 0; i < n-j+1; i++) {
                    	   work[n+j-1+i] = vn2[i];
                       }
                       for (i = 0; i < jb; i++) {
                    	   work[2*n + i] = auxv[i];
                       }
                       for (k = 0; k < jb; k++) {
                    	   for (i = 0; i < n-j+1; i++) {
                    		   work[2*nb+jb+i+k*(n-j+1)] = F[i][k];
                    	   }
                       }
                       j = j + fjb[0];
                     
                    } // while (j <= topbmn)
                 } // if ( (nb >= nbmin) && (nb < sminmn) && (nx < sminmn) )
                 else {
                    j = nfxd + 1;
                 }
        
        //       Use unblocked code to factor the last or only block.
        
        
                 if (j <= minmn ) {
                	 Ainout = new double[m][n-j+1];
                     for (i = 0; i < m; i++) {
                  	   for (k = 0; k < n-j+1; k++) {
                  		   Ainout[i][k] = A[i][j-1+k];
                  	   }
                     }
                     jpvtinout = new int[n-j+1];
                     for (i = 0; i < n-j+1; i++) {
                         jpvtinout[i] = jpvt[j-1+i];
                     }
                     tauout = new double[Math.min(m,n-j+1)];
                     vn1 = new double[n-j+1];
                     for (i = 0; i < n-j+1; i++) {
                  	   vn1[i] = work[j-1+i];
                     }
                     vn2 = new double[n-j+1];
                     for (i = 0; i < n-j+1; i++) {
                  	   vn2[i] = work[n+j-1+i];
                     }
                     work2 = new double[n-j+1];
                    dlaqp2( m, n-j+1, j-1, Ainout, m, jpvtinout,
                               tauout, vn1, vn2, work2);
                    for (i = 0; i < m; i++) {
                 	   for (k = 0; k < n-j+1; k++) {
                 		   A[i][j-1+k] = Ainout[i][k];
                 	   }
                    }
                    for (i = 0; i < n-j+1; i++) {
                        jpvt[j-1+i] = jpvtinout[i];
                    }
                    for (i = 0; i < Math.min(m,n-j+1); i++) {
                    	tau[j-1+i] = tauout[i];
                    }
                    for (i = 0; i < n-j+1; i++) {
                 	   work[j-1+i] = vn1[i];
                    }
                    for (i = 0; i < n-j+1; i++) {
                 	   work[n+j-1+i] = vn2[i];
                    }
                    for (i = 0; i < n-j+1; i++) {
                    	work[2*n + i] = work2[i];
                    }
                 } // if (j <= minmn )
        
              } // if (nfxd < minmn)
        
              work[0] = iws;
              return;
              } // dgeqp3
              
              /*> \brief \b DLAQPS computes a step of QR factorization with column pivoting of a real m-by-n matrix A by using BLAS level 3.
              *
              *  =========== DOCUMENTATION ===========
              *
              * Online html documentation available at
              *            http://www.netlib.org/lapack/explore-html/
              *
              *> \htmlonly
              *> Download DLAQPS + dependencies
              *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlaqps.f">
              *> [TGZ]</a>
              *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlaqps.f">
              *> [ZIP]</a>
              *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlaqps.f">
              *> [TXT]</a>
              *> \endhtmlonly
              *
              *  Definition:
              *  ===========
              *
              *       SUBROUTINE DLAQPS( M, N, OFFSET, NB, KB, A, LDA, JPVT, TAU, VN1,
              *                          VN2, AUXV, F, LDF )
              *
              *       .. Scalar Arguments ..
              *       INTEGER            KB, LDA, LDF, M, N, NB, OFFSET
              *       ..
              *       .. Array Arguments ..
              *       INTEGER            JPVT( * )
              *       DOUBLE PRECISION   A( LDA, * ), AUXV( * ), F( LDF, * ), TAU( * ),
              *      $                   VN1( * ), VN2( * )
              *       ..
              *
              *
              *> \par Purpose:
              *  =============
              *>
              *> \verbatim
              *>
              *> DLAQPS computes a step of QR factorization with column pivoting
              *> of a real M-by-N matrix A by using Blas-3.  It tries to factorize
              *> NB columns from A starting from the row OFFSET+1, and updates all
              *> of the matrix with Blas-3 xGEMM.
              *>
              *> In some cases, due to catastrophic cancellations, it cannot
              *> factorize NB columns.  Hence, the actual number of factorized
              *> columns is returned in KB.
              *>
              *> Block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
              *> \endverbatim
              *
              *  Arguments:
              *  ==========
              *
              *> \param[in] M
              *> \verbatim
              *>          M is INTEGER
              *>          The number of rows of the matrix A. M >= 0.
              *> \endverbatim
              *>
              *> \param[in] N
              *> \verbatim
              *>          N is INTEGER
              *>          The number of columns of the matrix A. N >= 0
              *> \endverbatim
              *>
              *> \param[in] OFFSET
              *> \verbatim
              *>          OFFSET is INTEGER
              *>          The number of rows of A that have been factorized in
              *>          previous steps.
              *> \endverbatim
              *>
              *> \param[in] NB
              *> \verbatim
              *>          NB is INTEGER
              *>          The number of columns to factorize.
              *> \endverbatim
              *>
              *> \param[out] KB
              *> \verbatim
              *>          KB is INTEGER
              *>          The number of columns actually factorized.
              *> \endverbatim
              *>
              *> \param[in,out] A
              *> \verbatim
              *>          A is DOUBLE PRECISION array, dimension (LDA,N)
              *>          On entry, the M-by-N matrix A.
              *>          On exit, block A(OFFSET+1:M,1:KB) is the triangular
              *>          factor obtained and block A(1:OFFSET,1:N) has been
              *>          accordingly pivoted, but no factorized.
              *>          The rest of the matrix, block A(OFFSET+1:M,KB+1:N) has
              *>          been updated.
              *> \endverbatim
              *>
              *> \param[in] LDA
              *> \verbatim
              *>          LDA is INTEGER
              *>          The leading dimension of the array A. LDA >= max(1,M).
              *> \endverbatim
              *>
              *> \param[in,out] JPVT
              *> \verbatim
              *>          JPVT is INTEGER array, dimension (N)
              *>          JPVT(I) = K <==> Column K of the full matrix A has been
              *>          permuted into position I in AP.
              *> \endverbatim
              *>
              *> \param[out] TAU
              *> \verbatim
              *>          TAU is DOUBLE PRECISION array, dimension (KB)
              *>          The scalar factors of the elementary reflectors.
              *> \endverbatim
              *>
              *> \param[in,out] VN1
              *> \verbatim
              *>          VN1 is DOUBLE PRECISION array, dimension (N)
              *>          The vector with the partial column norms.
              *> \endverbatim
              *>
              *> \param[in,out] VN2
              *> \verbatim
              *>          VN2 is DOUBLE PRECISION array, dimension (N)
              *>          The vector with the exact column norms.
              *> \endverbatim
              *>
              *> \param[in,out] AUXV
              *> \verbatim
              *>          AUXV is DOUBLE PRECISION array, dimension (NB)
              *>          Auxiliary vector.
              *> \endverbatim
              *>
              *> \param[in,out] F
              *> \verbatim
              *>          F is DOUBLE PRECISION array, dimension (LDF,NB)
              *>          Matrix F**T = L*Y**T*A.
              *> \endverbatim
              *>
              *> \param[in] LDF
              *> \verbatim
              *>          LDF is INTEGER
              *>          The leading dimension of the array F. LDF >= max(1,N).
              *> \endverbatim
              *
              *  Authors:
              *  ========
              *
              *> \author Univ. of Tennessee
              *> \author Univ. of California Berkeley
              *> \author Univ. of Colorado Denver
              *> \author NAG Ltd.
              *
              *> \date December 2016
              *
              *> \ingroup doubleOTHERauxiliary
              *
              *> \par Contributors:
              *  ==================
              *>
              *>    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
              *>    X. Sun, Computer Science Dept., Duke University, USA
              *> \n
              *>  Partial column norm updating strategy modified on April 2011
              *>    Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
              *>    University of Zagreb, Croatia.
              *
              *> \par References:
              *  ================
              *>
              *> LAPACK Working Note 176
              *
              *> \htmlonly
              *> <a href="http://www.netlib.org/lapack/lawnspdf/lawn176.pdf">[PDF]</a>
              *> \endhtmlonly
              *
              *  =====================================================================
              */
                   private void dlaqps(int m, int n, int offset, int nb, int kb[], double A[][], int lda, int jpvt[],
                		   double tau[], double vn1[], double vn2[], double auxv[], double F[][], int ldf) {
              /*
              *  -- LAPACK auxiliary routine (version 3.7.0) --
              *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
              *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
              *     December 2016
              *
              *     .. Scalar Arguments ..
                    INTEGER            KB, LDA, LDF, M, N, NB, OFFSET
              *     ..
              *     .. Array Arguments ..
                    INTEGER            JPVT( * )
                    DOUBLE PRECISION   A( LDA, * ), AUXV( * ), F( LDF, * ), TAU( * ),
                   $                   VN1( * ), VN2( * )
              *     ..
              *
              *  =====================================================================
              *
              *     .. Parameters ..
                    DOUBLE PRECISION   ZERO, ONE
                    PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
              *     ..
              *     .. Local Scalars ..
              */
                    int            itemp, i,j, k, lastrk, lsticc, pvt, rk;
                    double   akk, temp, temp2, tol3z;
                    double Ain[][];
                    double x[];
                    double y[];
                    double y2[];
                    double alpha[] = new double[1];
                    double tauone[] = new double[1];
                    double ratio;
                    double Bin[][];
                    double C[][];
              /*     ..
              *     .. External Subroutines ..
                    EXTERNAL           DGEMM, DGEMV, DLARFG, DSWAP
              *     ..
              *     .. Intrinsic Functions ..
                    INTRINSIC          ABS, DBLE, MAX, MIN, NINT, SQRT
              *     ..
              *     .. External Functions ..
                    INTEGER            IDAMAX
                    DOUBLE PRECISION   DLAMCH, DNRM2
                    EXTERNAL           IDAMAX, DLAMCH, DNRM2
              *     ..
              *     .. Executable Statements ..
              */
                    lastrk = Math.min( m, n+offset );
                    lsticc = 0;
                    k = 0;
                    tol3z = Math.sqrt(ge.dlamch('E'));
              
              //    Beginning of while loop.
              
                    while ( ( k < nb ) && (lsticc == 0) ) {
                       k = k + 1;
                       rk = offset + k;
              
                       // Determine ith pivot column and swap if necessary
              
                       int maxindex = -1;
                       double maxabsval = -Double.MAX_VALUE;
                       for (j = 0; j < n-k+1; j++) {
                    	   if (Math.abs(vn1[k-1+j]) > maxabsval) {
                    		   maxabsval = Math.abs(vn1[k-1+j]);
                    		   maxindex = j+1;
                    	   }
                       }
                       pvt = (k-1) + maxindex;
                       if (pvt != k) {
                    	  for (j = 0; j < m; j++) {
                    		  temp = A[j][pvt-1];
                    		  A[j][pvt-1] = A[j][k-1];
                    		  A[j][k-1] = temp;
                    	  }
                          for (j = 0; j < k-1; j++) {
                        	  temp = F[pvt-1][j];
                        	  F[pvt-1][j] = F[k-1][j];
                        	  F[k-1][j] = temp;
                          }
                          itemp = jpvt[pvt-1];
                          jpvt[pvt-1] = jpvt[k-1];
                          jpvt[k-1] = itemp;
                          vn1[pvt-1] = vn1[k-1];
                          vn2[pvt-1] = vn2[k-1];
                       } // if (pvt != k)
              
              //        Apply previous Householder reflectors to column K:
              //        A(RK:M,K) := A(RK:M,K) - A(RK:M,1:K-1)*F(K,1:K-1)**T.
              
                       if (k > 1) {
                    	  Ain = new double[m-rk+1][k-1];
                    	  for (i = 0; i < m-rk+1; i++) {
                    		  for (j = 0; j < k-1; j++) {
                    			  Ain[i][j] = A[rk-1+i][j];
                    		  }
                    	  }
                    	  x = new double[k-1];
                    	  for (i = 0; i < k-1; i++) {
                    		  x[i] = F[k-1][i];
                    	  }
                    	  y = new double[m-rk+1];
                    	  for (i = 0; i < m-rk+1; i++) {
                    		  y[i] = A[rk-1+i][k-1];
                    	  }
                          ge.dgemv( 'N', m-rk+1, k-1, -1.0, Ain,
                                m-rk+1, x, 1, 1.0, y, 1 );
                          for (i = 0; i < m-rk+1; i++) {
                    		  A[rk-1+i][k-1] = y[i];
                    	  }
                       } // if (k > 1)
              
                       // Generate elementary reflector H(k).
              
                       if (rk < m) {
                    	  alpha[0] = A[rk-1][k-1];
                    	  x = new double[m-rk];
                    	  for (i = 0; i < m-rk; i++) {
                    		  x[i] = A[rk+i][k-1];                	  
                    	  }
                          ge.dlarfg( m-rk+1, alpha, x, 1, tauone);
                          A[rk-1][k-1] = alpha[0];
                          for (i = 0; i < m-rk; i++) {
                    		  A[rk+i][k-1] = x[i];                	  
                          }
                          tau[k-1] = tauone[0];
                       }
                       else {
                    	  tau[k-1] = 0;
                       }
              
                       akk = A[rk-1][k-1];
                       A[rk-1][k-1] = 1.0;
              
              //       Compute Kth column of F:
              
              //       Compute  F(K+1:N,K) := tau(K)*A(RK:M,K+1:N)**T*A(RK:M,K).
              
                       if (k < n) {
                    	  Ain = new double[m-rk+1][n-k];
                    	  for (i = 0; i < m-rk+1; i++) {
                    		  for (j = 0; j < n-k; j++) {
                    			  Ain[i][j] = A[rk-1+i][k+j];
                    		  }
                    	  }
                    	  x = new double[m-rk+1];
                    	  for (i = 0; i < m-rk+1; i++) {
                    		  x[i] = A[rk-1+i][k-1];
                    	  }
                    	  y = new double[n-k];
                    	  for (i = 0; i < n-k; i++) {
                    		  y[i] = F[k+i][k-1];
                    	  }
                          ge.dgemv( 'T', m-rk+1, n-k, tau[k-1],
                                  Ain, m-rk+1, x, 1, 0.0, y, 1 );
                          for (i = 0; i < n-k; i++) {
                    		  F[k+i][k-1] = y[i];
                    	  }
                       } // if (k < n)
              
              //       Padding F(1:K,K) with zeros.
              
                       for (j = 1; j <= k; j++) {
                          F[j-1][k-1] = 0.0;
                       }
              
              //       Incremental updating of F:
              //       F(1:N,K) := F(1:N,K) - tau(K)*F(1:N,1:K-1)*A(RK:M,1:K-1)**T
              //                   *A(RK:M,K).
              
                       if (k > 1) {
                    	  Ain = new double[m-rk+1][k-1];
                    	  for (i = 0; i < m-rk+1; i++) {
                    		  for (j = 0; j < k-1; j++) {
                    			  Ain[i][j] = A[rk-1+i][j];
                    		  }
                    	  }
                    	  x = new double[m-rk+1];
                    	  for (i = 0; i < (m-rk+1); i++) {
                    		  x[i] = A[rk-1+i][k-1];
                    	  }
                    	  y = new double[k-1];
                          ge.dgemv( 'T', m-rk+1, k-1, -tau[k-1], Ain,
                                 m-rk+1, x, 1, 0.0, y, 1 );
                          for (i = 0; i < k-1; i++) {
                        	  auxv[i] = y[i];
                          }
              
                          y2 = new double[n];
                          for (i = 0; i < n; i++) {
                        	  y2[i] = F[i][k-1];
                          }
                          ge.dgemv( 'N', n, k-1, 1.0, F, n,
                                 y, 1, 1.0, y2, 1 );
                          for (i = 0; i < n; i++) {
                        	  F[i][k-1] = y2[i];
                          }
                       } // if (k > 1)
              
              //       Update the current row of A:
              //       A(RK,K+1:N) := A(RK,K+1:N) - A(RK,1:K)*F(K+1:N,1:K)**T.
              
                       if (k < n) {
                    	  Ain = new double[n-k][k];
                    	  for (i = 0; i < n-k; i++) {
                    		  for (j = 0; j < k; j++) {
                    			  Ain[i][j] = F[k+i][j];
                    		  }
                    	  }
                    	  x = new double[k];
                    	  for (i = 0; i < k; i++) {
                    		  x[i] = A[rk-1][i];
                    	  }
                    	  y  = new double[n-k];
                    	  for (i = 0; i < n-k; i++) {
                    		  y[i] = A[rk-1][k+i];
                    	  }
                          ge.dgemv( 'N', n-k, k, -1.0, Ain, n-k,
                                 x, 1, 1.0, y, 1);
                          for (i = 0; i < n-k; i++) {
                    		  A[rk-1][k+i] = y[i];
                    	  }
                       } // if (k < n)
              
              //       Update partial column norms.
              
                       if (rk < lastrk) {
                          for (j = k + 1; j <= n; j++) {
                             if (vn1[j-1] != 0.0) {
              
              //                NOTE: The following 4 lines follow from the analysis in
              //                Lapack Working Note 176.
              
                                temp = Math.abs( A[rk-1][j-1] ) / vn1[j-1];
                                temp = Math.max(0.0, ( 1.0+temp )*( 1.0-temp ) );
                                ratio = vn1[j-1]/vn2[j-1];
                                temp2 = temp * ratio * ratio;
                                if (temp2 <= tol3z) {
                                   vn2[j-1] = (double)lsticc;
                                   lsticc = j;
                                }
                                else {
                                   vn1[j-1] = vn1[j-1]*Math.sqrt(temp);
                                }
                             } // if (vn1[j-1] != 0.0)
                          } // for (j = k + 1; j <= n; j++)
                       } // if (rk < lastrk)
              
                       A[rk-1][k-1] = akk;
              
                    } // while ( ( k < nb ) && (lsticc == 0) )
                    kb[0] = k;
                    rk = offset + kb[0];
              
              //    Apply the block reflector to the rest of the matrix:
              //    A(OFFSET+KB+1:M,KB+1:N) := A(OFFSET+KB+1:M,KB+1:N) -
              //                        A(OFFSET+KB+1:M,1:KB)*F(KB+1:N,1:KB)**T.
              
                    if (kb[0] < Math.min( n, m-offset ) ) {
                       Ain = new double[m-rk][kb[0]];
                       for (i = 0; i < m-rk; i++) {
                    	   for (j = 0; j < kb[0]; j++) {
                    		   Ain[i][j] = A[rk+i][j];
                    	   }
                       }
                       Bin = new double[n-kb[0]][kb[0]];
                       for (i = 0; i < n-kb[0]; i++) {
                    	   for (j = 0; j < kb[0]; j++) {
                    		   Bin[i][j] = F[kb[0]+i][j];
                    	   }
                       }
                       C = new double[m-rk][n-kb[0]];
                       for (i = 0; i < m-rk; i++) {
                    	   for (j = 0; j < n-kb[0]; j++) {
                    		   C[i][j] = A[rk+i][kb[0]+j];
                    	   }
                       }
                       ge.dgemm( 'N', 'T', m-rk, n-kb[0], kb[0], -1.0,
                              Ain, m-rk, Bin, n-kb[0], 1.0, C, m-rk);
                       for (i = 0; i < m-rk; i++) {
                    	   for (j = 0; j < n-kb[0]; j++) {
                    		   A[rk+i][kb[0]+j] = C[i][j];
                    	   }
                       }
                    } // if (kb < Math.min( n, m-offset ) )
              
              //    Recomputation of difficult columns.
              
                    while (lsticc > 0) {
                       itemp = (int)Math.round(vn2[lsticc-1] );
                       x = new double[m-rk];
                       for (i = 0; i < m-rk; i++) {
                    	   x[i] = A[rk+i][lsticc-1];
                       }
                       vn1[lsticc-1] = ge.dnrm2(m-rk, x, 1 );
              
              //       NOTE: The computation of VN1( LSTICC ) relies on the fact that
              //       SNRM2 does not fail on vectors with norm below the value of
              //       SQRT(DLAMCH('S'))
              
                       vn2[lsticc-1] = vn1[lsticc-1];
                       lsticc = itemp;
                    } // while (lsticc > 0)
              
                    return;
                   } // dlaqps
                   
           /*> \brief \b DLAQP2 computes a QR factorization with column pivoting of the matrix block.
           *
           *  =========== DOCUMENTATION ===========
           *
           * Online html documentation available at
           *            http://www.netlib.org/lapack/explore-html/
           *
           *> \htmlonly
           *> Download DLAQP2 + dependencies
           *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlaqp2.f">
           *> [TGZ]</a>
           *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlaqp2.f">
           *> [ZIP]</a>
           *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlaqp2.f">
           *> [TXT]</a>
           *> \endhtmlonly
           *
           *  Definition:
           *  ===========
           *
           *       SUBROUTINE DLAQP2( M, N, OFFSET, A, LDA, JPVT, TAU, VN1, VN2,
           *                          WORK )
           *
           *       .. Scalar Arguments ..
           *       INTEGER            LDA, M, N, OFFSET
           *       ..
           *       .. Array Arguments ..
           *       INTEGER            JPVT( * )
           *       DOUBLE PRECISION   A( LDA, * ), TAU( * ), VN1( * ), VN2( * ),
           *      $                   WORK( * )
           *       ..
           *
           *
           *> \par Purpose:
           *  =============
           *>
           *> \verbatim
           *>
           *> DLAQP2 computes a QR factorization with column pivoting of
           *> the block A(OFFSET+1:M,1:N).
           *> The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
           *> \endverbatim
           *
           *  Arguments:
           *  ==========
           *
           *> \param[in] M
           *> \verbatim
           *>          M is INTEGER
           *>          The number of rows of the matrix A. M >= 0.
           *> \endverbatim
           *>
           *> \param[in] N
           *> \verbatim
           *>          N is INTEGER
           *>          The number of columns of the matrix A. N >= 0.
           *> \endverbatim
           *>
           *> \param[in] OFFSET
           *> \verbatim
           *>          OFFSET is INTEGER
           *>          The number of rows of the matrix A that must be pivoted
           *>          but no factorized. OFFSET >= 0.
           *> \endverbatim
           *>
           *> \param[in,out] A
           *> \verbatim
           *>          A is DOUBLE PRECISION array, dimension (LDA,N)
           *>          On entry, the M-by-N matrix A.
           *>          On exit, the upper triangle of block A(OFFSET+1:M,1:N) is
           *>          the triangular factor obtained; the elements in block
           *>          A(OFFSET+1:M,1:N) below the diagonal, together with the
           *>          array TAU, represent the orthogonal matrix Q as a product of
           *>          elementary reflectors. Block A(1:OFFSET,1:N) has been
           *>          accordingly pivoted, but no factorized.
           *> \endverbatim
           *>
           *> \param[in] LDA
           *> \verbatim
           *>          LDA is INTEGER
           *>          The leading dimension of the array A. LDA >= max(1,M).
           *> \endverbatim
           *>
           *> \param[in,out] JPVT
           *> \verbatim
           *>          JPVT is INTEGER array, dimension (N)
           *>          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
           *>          to the front of A*P (a leading column); if JPVT(i) = 0,
           *>          the i-th column of A is a free column.
           *>          On exit, if JPVT(i) = k, then the i-th column of A*P
           *>          was the k-th column of A.
           *> \endverbatim
           *>
           *> \param[out] TAU
           *> \verbatim
           *>          TAU is DOUBLE PRECISION array, dimension (min(M,N))
           *>          The scalar factors of the elementary reflectors.
           *> \endverbatim
           *>
           *> \param[in,out] VN1
           *> \verbatim
           *>          VN1 is DOUBLE PRECISION array, dimension (N)
           *>          The vector with the partial column norms.
           *> \endverbatim
           *>
           *> \param[in,out] VN2
           *> \verbatim
           *>          VN2 is DOUBLE PRECISION array, dimension (N)
           *>          The vector with the exact column norms.
           *> \endverbatim
           *>
           *> \param[out] WORK
           *> \verbatim
           *>          WORK is DOUBLE PRECISION array, dimension (N)
           *> \endverbatim
           *
           *  Authors:
           *  ========
           *
           *> \author Univ. of Tennessee
           *> \author Univ. of California Berkeley
           *> \author Univ. of Colorado Denver
           *> \author NAG Ltd.
           *
           *> \date December 2016
           *
           *> \ingroup doubleOTHERauxiliary
           *
           *> \par Contributors:
           *  ==================
           *>
           *>    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
           *>    X. Sun, Computer Science Dept., Duke University, USA
           *> \n
           *>  Partial column norm updating strategy modified on April 2011
           *>    Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
           *>    University of Zagreb, Croatia.
           *
           *> \par References:
           *  ================
           *>
           *> LAPACK Working Note 176
           *
           *> \htmlonly
           *> <a href="http://www.netlib.org/lapack/lawnspdf/lawn176.pdf">[PDF]</a>
           *> \endhtmlonly
           *
           *  =====================================================================
           */
                 private void dlaqp2(int m, int n, int offset, double A[][], int lda, 
                		 int jpvt[], double tau[], double vn1[], double vn2[], double work[]) {
           /*
           *  -- LAPACK auxiliary routine (version 3.7.0) --
           *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
           *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
           *     December 2016
           *
           *     .. Scalar Arguments ..
                 INTEGER            LDA, M, N, OFFSET
           *     ..
           *     .. Array Arguments ..
                 INTEGER            JPVT( * )
                 DOUBLE PRECISION   A( LDA, * ), TAU( * ), VN1( * ), VN2( * ),
                $                   WORK( * )
           *     ..
           *
           *  =====================================================================
           *
           *     .. Parameters ..
                 DOUBLE PRECISION   ZERO, ONE
                 PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
           *     ..
           *     .. Local Scalars ..
           */     
                 int            i, itemp, j, mn, offpi, pvt,k;
                 double   aii, temp, temp2, tol3z;
                 double   alpha[] = new double[1];
                 double   x[];
                 double   tauone[] = new double[1];
                 double   v[];
                 double   C[][];
                 double   ratio;
           /*     ..
           *     .. External Subroutines ..
                 EXTERNAL           DLARF, DLARFG, DSWAP
           *     ..
           *     .. Intrinsic Functions ..
                 INTRINSIC          ABS, MAX, MIN, SQRT
           *     ..
           *     .. External Functions ..
                 INTEGER            IDAMAX
                 DOUBLE PRECISION   DLAMCH, DNRM2
                 EXTERNAL           IDAMAX, DLAMCH, DNRM2
           *     ..
           *     .. Executable Statements ..
           */
                 mn = Math.min( m-offset, n);
                 tol3z = Math.sqrt(ge.dlamch('E'));
           
           //    Compute factorization.
           
                 for (i = 1; i <= mn; i++) {
           
                    offpi = offset + i;
           
           //       Determine ith pivot column and swap if necessary.
                    
                    int maxindex = -1;
                    double maxabsval = -Double.MAX_VALUE;
                    for (j = 0; j < n-i+1; j++) {
                 	   if (Math.abs(vn1[i-1+j]) > maxabsval) {
                 		   maxabsval = Math.abs(vn1[i-1+j]);
                 		   maxindex = j+1;
                 	   }
                    }
                    pvt = (i-1) + maxindex;
                    if (pvt != i) {
                 	  for (j = 0; j < m; j++) {
                 		  temp = A[j][pvt-1];
                 		  A[j][pvt-1] = A[j][i-1];
                 		  A[j][i-1] = temp;
                 	  }
                      itemp = jpvt[pvt-1];
                      jpvt[pvt-1] = jpvt[i-1];
                      jpvt[i-1] = itemp;
                      vn1[pvt-1] = vn1[i-1];
                      vn2[pvt-1] = vn2[i-1];
                    } // if (pvt != i)
           
           
           //       Generate elementary reflector H(i).
           
                    if (offpi < m) {
                       alpha[0] = A[offpi-1][i-1];
                       x = new double[m-offpi];
                       for (j = 0; j < m-offpi; j++) {
                    	   x[j] = A[offpi+j][i-1];
                       }
                       ge.dlarfg(m-offpi+1, alpha, x, 1, tauone);
                       A[offpi-1][i-1] = alpha[0];
                       for (j = 0; j < m-offpi; j++) {
                    	   A[offpi+j][i-1] = x[j];
                       }
                       tau[i-1] = tauone[0];
                    }
                    else {
                      tau[i-1] = 0.0;
                    }
           
                    if (i < n) {
           
           //          Apply H(i)**T to A(offset+i:m,i+1:n) from the left.
           
                       aii = A[offpi-1][i-1];
                       A[offpi-1][i-1] = 1.0;
                       v = new double[m-offpi+1];
                       for (j = 0; j < m-offpi+1; j++) {
                    	   v[j] = A[offpi-1+j][i-1];
                       }
                       C = new double[m-offpi+1][n-i];
                       for (j = 0; j < m-offpi+1; j++) {
                    	   for (k = 0; k < n-i; k++) {
                    		   C[j][k] = A[offpi-1+j][i+k];
                    	   }
                       }
                       ge.dlarf( 'L', m-offpi+1, n-i, v, 1,
                                 tau[i-1], C, m-offpi+1, work);
                       for (j = 0; j < m-offpi+1; j++) {
                    	   for (k = 0; k < n-i; k++) {
                    		   A[offpi-1+j][i+k] = C[j][k];
                    	   }
                       }
                       A[offpi-1][i-1] = aii;
                    } // if (i < n)
           
           //       Update partial column norms.
           
                    for (j = i + 1; j <= n; j++) {
                       if( vn1[j-1] != 0.0) {
           
           //             NOTE: The following 4 lines follow from the analysis in
           //             Lapack Working Note 176.
           
                          ratio = Math.abs(A[offpi-1][j-1])/vn1[j-1];
                          temp = 1.0 - ratio * ratio;
                          temp = Math.max(temp,0.0);
                          ratio = vn1[j-1]/vn2[j-1];
                          temp2 = temp * ratio * ratio;
                          if (temp2 <= tol3z) {
                             if (offpi < m) {
                            	x = new double[m-offpi];
                            	for (k = 0; k < m-offpi; k++) {
                            		x[k] = A[offpi+k][j-1];
                            	}
                                vn1[j-1] = ge.dnrm2(m-offpi, x, 1 );
                                vn2[j-1] = vn1[j-1];
                             } // if (offpi < m)
                             else {
                                vn1[j-1] = 0.0;
                                vn2[j-1] = 0.0;
                             }
                          } // if (temp2 <= tol3z) 
                          else {
                             vn1[j-1] = vn1[j-1]*Math.sqrt(temp);
                          }
                       } // if( vn1[j-1] != 0.0)
                    } // for (j = i + 1; j <= n; j++)
           
                 } // for (i = 1; i <= mn; i++)
           
                 return;
           
           } // dlaqp2
                 
                 /**
                  * This is a port of version 3.1 LAPACK auxiliary routine XLAENV. Univ. of Tennessee, Univ. of California Berkeley
                  * and NAG Ltd.. November 2006
                  * 
                  * .. Scalar Arguments .. INTEGER ISPEC, NVALUE ..
                  * 
                  * Purpose =======
                  * 
                  * XLAENV sets certain machine- and problem-dependent quantities which will later be retrieved by ILAENV.
                  * 
                  * Arguments =========
                  * 
                  * ISPEC (input) INTEGER Specifies the parameter to be set in the COMMON array IPARMS. = 1: the optimal blocksize;
                  * if this value is 1, an unblocked algorithm will give the best performance. = 2: the minimum block size for which
                  * the block routine should be used; if the usable block size is less than this value, an unblocked routine should
                  * be used. = 3: the crossover point (in a block routine, for N less than this value, an unblocked routine should be
                  * used) = 4: the number of shifts, used in the nonsymmetric eigenvalue routines = 5: the minimum column dimension
                  * for blocking to be used; rectangular blocks must have dimension at least k by m, where k is given by
                  * ILAENV(2,...) and m by ILAENV(5,...) = 6: the crossover point for the SVD (when reducing an m by n matrix to
                  * bidiagonal form, if max(m,n)/min(m,n) exceeds this value, a QR factorization is used first to reduce the matrix
                  * to a triangular form) = 7: the number of processors = 8: another crossover point, for the multishift QR and QZ
                  * methods for nonsymmetric eigenvalue problems. = 9: maximum size of the subproblems at the bottom of the
                  * computation tree in the divide-and-conquer algorithm (used by xGELSD and xGESDD) =10: ieee NaN arithmetic can be
                  * trusted not to trap =11: infinity arithmetic can be trusted not to trap
                  * 
                  * NVALUE (input) INTEGER The value of the parameter specified by ISPEC.
                  */
                 public void xlaenv(final int ispec, final int nvalue) {
                     if ( (ispec >= 1) && (ispec <= 9)) {
                         iparms[ispec - 1] = nvalue;
                     }
                     return;
                 } // xlaenv             
          
}