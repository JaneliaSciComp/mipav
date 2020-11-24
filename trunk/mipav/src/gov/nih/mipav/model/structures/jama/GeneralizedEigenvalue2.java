package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.*;

public class GeneralizedEigenvalue2 implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    
    public GeneralizedEigenvalue2() {
    	
    }
    	
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
             //       DLAQP2( M, N-J+1, J-1, A( 1, J ), LDA, JPVT( J ),
             //                  TAU( J ), WORK( J ), WORK( N+J ),
             //                   WORK( 2*N+1 ) )
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
           *
                 SUBROUTINE DLAQP2( M, N, OFFSET, A, LDA, JPVT, TAU, VN1, VN2,
                $                   WORK )
           *
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
                 INTEGER            I, ITEMP, J, MN, OFFPI, PVT
                 DOUBLE PRECISION   AII, TEMP, TEMP2, TOL3Z
           *     ..
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
           *
                 MN = MIN( M-OFFSET, N )
                 TOL3Z = SQRT(DLAMCH('Epsilon'))
           *
           *     Compute factorization.
           *
                 DO 20 I = 1, MN
           *
                    OFFPI = OFFSET + I
           *
           *        Determine ith pivot column and swap if necessary.
           *
                    PVT = ( I-1 ) + IDAMAX( N-I+1, VN1( I ), 1 )
           *
                    IF( PVT.NE.I ) THEN
                       CALL DSWAP( M, A( 1, PVT ), 1, A( 1, I ), 1 )
                       ITEMP = JPVT( PVT )
                       JPVT( PVT ) = JPVT( I )
                       JPVT( I ) = ITEMP
                       VN1( PVT ) = VN1( I )
                       VN2( PVT ) = VN2( I )
                    END IF
           *
           *        Generate elementary reflector H(i).
           *
                    IF( OFFPI.LT.M ) THEN
                       CALL DLARFG( M-OFFPI+1, A( OFFPI, I ), A( OFFPI+1, I ), 1,
                $                   TAU( I ) )
                    ELSE
                       CALL DLARFG( 1, A( M, I ), A( M, I ), 1, TAU( I ) )
                    END IF
           *
                    IF( I.LT.N ) THEN
           *
           *           Apply H(i)**T to A(offset+i:m,i+1:n) from the left.
           *
                       AII = A( OFFPI, I )
                       A( OFFPI, I ) = ONE
                       CALL DLARF( 'Left', M-OFFPI+1, N-I, A( OFFPI, I ), 1,
                $                  TAU( I ), A( OFFPI, I+1 ), LDA, WORK( 1 ) )
                       A( OFFPI, I ) = AII
                    END IF
           *
           *        Update partial column norms.
           *
                    DO 10 J = I + 1, N
                       IF( VN1( J ).NE.ZERO ) THEN
           *
           *              NOTE: The following 4 lines follow from the analysis in
           *              Lapack Working Note 176.
           *
                          TEMP = ONE - ( ABS( A( OFFPI, J ) ) / VN1( J ) )**2
                          TEMP = MAX( TEMP, ZERO )
                          TEMP2 = TEMP*( VN1( J ) / VN2( J ) )**2
                          IF( TEMP2 .LE. TOL3Z ) THEN
                             IF( OFFPI.LT.M ) THEN
                                VN1( J ) = DNRM2( M-OFFPI, A( OFFPI+1, J ), 1 )
                                VN2( J ) = VN1( J )
                             ELSE
                                VN1( J ) = ZERO
                                VN2( J ) = ZERO
                             END IF
                          ELSE
                             VN1( J ) = VN1( J )*SQRT( TEMP )
                          END IF
                       END IF
              10    CONTINUE
           *
              20 CONTINUE
           *
                 RETURN
           *
           *     End of DLAQP2
           *
                 END*/

}