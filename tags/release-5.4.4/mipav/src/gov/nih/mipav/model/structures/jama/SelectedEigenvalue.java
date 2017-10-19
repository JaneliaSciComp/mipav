package gov.nih.mipav.model.structures.jama;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class SelectedEigenvalue implements java.io.Serializable {
 // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new SelectedEigenvalue object.
     */
    public SelectedEigenvalue() {}
    
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    
    /** This is a port of the version 3.2 LAPACK DSYEVX routine.  Original DSYEVX created by created by Univ. of Tennessee, Univ. of
    California Berkeley, and NAG Ltd., November, 2006  *  DSYEVX computes selected eigenvalues and, optionally, eigenvectors
    of a real symmetric matrix A.  Eigenvalues and eigenvectors can be selected by specifying either a range of values
    or a range of indices for the desired eigenvalues.
   
    @param jobz input char = 'N': Compute eigenvalues only
                           = 'V': Compute eigenvalues and eigenvectors.
    @param range input char = 'A': all eigenvalues will be found.
                            = 'V': all eigenvalues in the half-open interval (VL,VU]  will be found.
                            = 'I': the IL-th through IU-th eigenvalues will be found.
    @param uplo input char = 'U': Upper triangle of A is stored 
                           = 'L': Lower triangle of A is stored
    @param n input int The order of the matrix A. n >= 0.
    @param A input/output double[][] of dimension lda by n On entry, the symmetric matrix A. If uplo = 'U', the
             leading n-by-n upper triangular part of A contains the upper triangular part of the matrix A. If uplo
              = 'L', the leading n-by-n lower triangular part of A contains the lower triangular part of matrix A.
             On exit, the lower triangle (if UPLO='L') or the upper triangle (if UPLO='U') of A, including the diagonal, is
             destroyed.
    @param lda input int The leading dimension of array A. lda >= max(1,n).
    @param vl input double 
    @param vu input double 
              If RANGE='V', the lower and upper bounds of the interval to be searched for eigenvalues. vl < vu.
              Not referenced if RANGE = 'A' or 'I'.
    @param il input int
    @param iu input int
              If RANGE='I', the indices (in ascending order) of the smallest and largest eigenvalues to be returned.
              1 <= il <= iu <= n, if n > 0; il = 1 and iu = 0 if n = 0.
              Not referenced if range = 'A' or 'V'.
    @param abstol input double  The absolute error tolerance for the eigenvalues.
                  An approximate eigenvalue is accepted as converged when it is determined to lie in an interval [a,b]
                  of width less than or equal to abstol + eps *   max( |a|,|b| ) ,
                  where eps is the machine precision.  If abstol is less than or equal to zero, 
                  then  eps*|T|  will be used in its place, where |T| is the 1-norm of the tridiagonal matrix obtained
                  by reducing A to tridiagonal form.

                 Eigenvalues will be computed most accurately when abstol is set to twice the underflow
                 threshold 2*dlamch('S'), not zero.  If this routine returns with info>0, indicating that some
                 eigenvectors did not converge, try setting abstol to 2*dlamch('S').

                 See "Computing Small Singular Values of Bidiagonal Matrices with Guaranteed High Relative Accuracy,"
                 by Demmel and Kahan, LAPACK Working Note #3.
    @param m output int[] The total number of eigenvalues found.  0 <= m <= n.
                          If range = 'A', m = n, and if RANGE = 'I', n = iu-il+1.
    @param w output double[] of dimension n.    On normal exit, the first M elements contain the selected
                             eigenvalues in ascending order.
    @param Z output double[][] of dimension ldz, max(1,m).  If jobz = 'V', then if info = 0, the first m columns of Z
                               contain the orthonormal eigenvectors of the matrix A corresponding to the selected eigenvalues,
                               with the i-th column of Z holding the eigenvector associated with W(i). If an eigenvector fails to
                               converge, then that column of Z contains the latest approximation to the eigenvector, and the
                               index of the eigenvector is returned in IFAIL.  If jobz = 'N', then Z is not referenced.
                               Note: the user must ensure that at least max(1,m) columns are supplied in the array Z; 
                               if range = 'V', the exact value of m is not known in advance and an upper bound must be used.
    @param ldz input int The leading dimension of the array Z.  ldz >= 1, and if jobz = 'V', ldz >= max(1,n).
    @param work  (workspace/output) double[] of dimension max(1,lwork). On exit, if info[0] = 0, then work[0] returns the
              optimal lwork.
    @param lwork input int The length of the array work.  lwork >= 1, when n <= 1; otherwise 8*n.
                           For optimal efficiency, lwork >= (nb+3)*n, where nb is the max of the blocksize for dsytrd
                           and dortmr returned by ilaenv.

                           If lwork = -1, then a workspace query is assumed; the routine only calculates the optimal
                           size of the work array, returns this value as the first entry of the work array, and no error
                           message related to lwork is issued by xerbla.
    @param iwork worskpace int[] of dimension (5 * n). 
    @param ifail output int[] of dimension n.   If jobz = 'V', then if info[0] = 0, the first m elements of
                              ifail are zero.  If info[0] > 0, then ifail contains the indices of the eigenvectors
                              that failed to converge.  If jobz = 'N', then ifail is not referenced.
    @param info output int[] of dimension 1.  = 0:  successful exit
                                             < 0:  if info[0] = -i, the i-th argument had an illegal value
                                             > 0:  if info[0] = i, then i eigenvectors failed to converge.
                                             Their indices are stored in array ifail.
   */
    
 private void dsyevx(final char jobz, final char range, final char uplo, final int n, final double[][]A, final int lda, 
                     final double vl, final double vu, final int il, final int iu, final double abstol, final int[] m,
                     final double[] w, final double[][] Z, final int ldz, final double[] work, final int lwork,
                     final int[] iwork, final int[] ifail, final int[] info ) {
 boolean lower;
 boolean wantz;
 boolean alleig;
 boolean valeig;
 boolean indeig;
 boolean lquery;
 int lwkmin;
 int nb;
 final char[] ch = new char[1];
 String opts;
 int lwkopt;
 int iscale;
 double abstll;
 double vll;
 double vuu;
 double smlnum;
 double bignum;
 double eps;
 double rmin;
 double rmax;
 double anrm;
 double sigma = 0.0;
 int i, j;
 double vec1[];
 double vec2[];
 double vec3[];
 double vec4[];
 double vec5[];
 int indtau;
 int inde;
 int indd;
 int llwork;
 int iinfo[] = new int[1];
 boolean test;
 int indwrk;
 int indee;
 boolean doHere = true;
 char order;
 int indibl;
 int indisp;
 int indiwo;

 //     Test the input parameters.
 lower =  ( (uplo == 'L') || (uplo == 'l'));
 wantz = ((jobz == 'V') || (jobz == 'v'));
 alleig = ((range == 'A') || (range == 'a'));
 valeig = ((range == 'V') || (range == 'v'));
 indeig = ((range == 'I') || (range == 'i'));
 lquery = (lwork == -1);

 info[0] = 0;
 if ( !( wantz || ((jobz =='N') || (jobz == 'n')))) {
    info[0] = -1;
 }
 else if ( !( alleig || valeig || indeig) ) {
    info[0] = -2;
 }
 else if ( !( lower || ( (uplo  == 'U') || (uplo == 'u') ) ) ) {
    info[0] = -3;
 }
 else if (n < 0 ) {
    info[0] = -4;
 }
 else if (lda < Math.max( 1, n ) ) {
    info[0] = -6;
 }
 else {
    if (valeig) {
       if (n > 0 && vu <= vl) {
         info[0] = -8;
       }
    }
    else if(indeig) {
       if (il < 1 || il > Math.max( 1, n ) ) {
          info[0] = -9;
       }
       else if (iu < Math.min( n, il ) || iu > n ) {
          info[0] = -10;
       }
    }
 }
 if (info[0] == 0 ) {
    if ( ldz < 1 || ( wantz && ldz < n ) ) {
       info[0] = -15;
    }
 }

 if (info[0] == 0 ) {
    if (n != 1 ) {
       lwkmin = 1;
       work[0] = lwkmin;
    }
    else {
       lwkmin = 8*n;
       ch[0] = uplo;
       opts = new String(ch);
       nb = ge.ilaenv(1, "DSYTRD", opts, n, -1, -1, -1);
       nb = Math.max(nb, ge.ilaenv( 1, "DORMTR", opts, n, -1, -1, -1 ) );
       lwkopt = Math.max(lwkmin, (nb + 3 )*n );
       work[0] = lwkopt;
    }

    if (lwork < lwkmin && !lquery ) {
       info[0] = -17;
    }
 } // if (info[0] == 0)

 if (info[0] != 0 ) {
     MipavUtil.displayError("Error dsyevx had info = " + info[0]);
     return;
 }
 else if(lquery) {
    return;
 }

 //     Quick return if possible

 m[0] = 0;
 if (n == 0 ) {
    return;
 }

 if (n == 1) {
    if (alleig || indeig) {
       m[0] = 1;
       w[0] = A[0][0];
    }
    else {
       if (vl < A[0][0] && vu >= A[0][0]) {
          m[0] = 1;
          w[0] = A[0][0];
       }
    }
    if (wantz) {
        Z[0][0] = 1.0;
    }
    return;
 } // if (n == 1)

 // Get machine constants.

 ge.setSafmin(ge.dlamch('S'));
 eps = ge.dlamch('P');
 smlnum = ge.getSafmin() / eps;
 bignum = 1.0 / smlnum;
 rmin = Math.sqrt(smlnum);
 rmax = Math.min(Math.sqrt(bignum), 1.0 / Math.sqrt(Math.sqrt(ge.getSafmin()) ) );

 // Scale matrix to allowable range, if necessary.

 iscale = 0;
 abstll = abstol;
 if (valeig) {
    vll = vl;
    vuu = vu;
 }
 anrm = ge.dlansy('M', uplo, n, A, lda, work);
 if (anrm > 0.0 &&  anrm < rmin ) {
    iscale = 1;
    sigma = rmin / anrm;
 }
 else if (anrm > rmax) {
    iscale = 1;
    sigma = rmax / anrm;
 }
 if (iscale == 1 ) {
    if (lower) {
       for (j = 1; j <= n; j++) {
          vec1 = new double[n-j+1];
          for (i = 0; i < n-j+1; i++) {
              vec1[i] = A[j-1+i][j-1];
          }
          ge.dscal(n-j+1, sigma, vec1, 1 );
          for (i = 0; i < n-j+1; i++) {
              A[j-1+i][j-1] = vec1[i];
          }
       }
    }
    else {
       for (j = 1; j <= n; j++) {
          vec1 = new double[j];
          for (i = 0; i < j; i++) {
              vec1[i] = A[i][j];
          }
          ge.dscal(j, sigma, vec1, 1 );
          for (i = 0; i < j; i++) {
              A[i][j] = vec1[i];
          }
       }
    }
    if (abstol > 0 ) {
      abstll = abstol*sigma;
    }
    if (valeig) {
       vll = vl*sigma;
       vuu = vu*sigma;
    }
 } // if (iscale == 1)

 // Call dsytrd to reduce symmetric matrix to tridiagonal form.

 indtau = 1;
 inde = indtau + n;
 indd = inde + n;
 indwrk = indd + n;
 llwork = lwork - indwrk + 1;
 vec1 = new double[n];
 vec2 = new double[n-1];
 vec3 = new double[n-1];
 vec4 = new double[Math.max(1, llwork)];
 iinfo[0] = 0;
 ge.dsytrd(uplo, n, A, lda, vec1, vec2, vec3, vec4, llwork, iinfo);

// If all eigenvalues are desired and abstol is less than or equal to
// zero, then call dsterf or dorgtr and dsteqr.  If this fails for
// some eigenvalue, then try dstebz.

 test = false;
 if (indeig) {
    if (il == 1 && iu == n) {
       test = true;
    }
 } // if (indeig)
 if ( (alleig || test) && (abstol <= 0.0) ) {
     for (i = 0; i < n; i++) {
         w[i] = vec1[i];
     }
    indee = indwrk + 2*n;
    if (!wantz) {
       vec5 = new double[n-1];
       for (i = 0; i < n-1; i++) {
           vec5[i] = vec2[i];
       }
       ge.dsterf( n, w, vec5, info);
    }
    else {
       ge.dlacpy( 'A', n, n, A, lda, Z, ldz);
       iinfo[0] = 0;
       ge.dorgtr(uplo, n, Z, ldz, vec3, vec4, llwork, iinfo);
       vec5 = new double[n-1];
       for (i = 0; i < n-1; i++) {
           vec5[i] = vec2[i];
       }
       vec4 = new double[Math.max(1,2*n-2)];
       ge.dsteqr(jobz, n, w, vec5, Z, ldz, vec4, info);
       if (info[0] == 0 ) {
          for (i = 0; i < n; i++) {
             ifail[i] = 0;
          }
       }
    }
    if (info[0] == 0 ) {
       m[0] = n;
       doHere = false;
    }
    if (doHere) {
        info[0] = 0;
    }
 } // if ( (alleig || test) && (abstol <= 0.0) )

 // Otherwise, call dstebz and, if eigenvectors are desired, dstein.

 if (doHere) {
     if (wantz) {
        order = 'B';
     }
     else {
        order = 'E';
     }
     indibl = 1;
     indisp = indibl + n;
     indiwo = indisp + n;
     /*CALL DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTLL,
    $             WORK( INDD ), WORK( INDE ), M, NSPLIT, W,
    $             IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWRK ),
    $             IWORK( INDIWO ), INFO )
 *
     IF( WANTZ ) THEN
        CALL DSTEIN( N, WORK( INDD ), WORK( INDE ), M, W,
    $                IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ,
    $                WORK( INDWRK ), IWORK( INDIWO ), IFAIL, INFO )
 *
 *        Apply orthogonal matrix used in reduction to tridiagonal
 *        form to eigenvectors returned by DSTEIN.
 *
        INDWKN = INDE
        LLWRKN = LWORK - INDWKN + 1
        CALL DORMTR( 'L', UPLO, 'N', N, M, A, LDA, WORK( INDTAU ), Z,
    $                LDZ, WORK( INDWKN ), LLWRKN, IINFO )
     END IF*/
 } // if (doHere)

 // If matrix was scaled, then rescale eigenvalues appropriately.

/*40 CONTINUE
 IF( ISCALE.EQ.1 ) THEN
    IF( INFO.EQ.0 ) THEN
       IMAX = M
    ELSE
       IMAX = INFO - 1
    END IF
    CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
 END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.
*
 IF( WANTZ ) THEN
    DO 60 J = 1, M - 1
       I = 0
       TMP1 = W( J )
       DO 50 JJ = J + 1, M
          IF( W( JJ ).LT.TMP1 ) THEN
             I = JJ
             TMP1 = W( JJ )
          END IF
50       CONTINUE
*
       IF( I.NE.0 ) THEN
          ITMP1 = IWORK( INDIBL+I-1 )
          W( I ) = W( J )
          IWORK( INDIBL+I-1 ) = IWORK( INDIBL+J-1 )
          W( J ) = TMP1
          IWORK( INDIBL+J-1 ) = ITMP1
          CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
          IF( INFO.NE.0 ) THEN
             ITMP1 = IFAIL( I )
             IFAIL( I ) = IFAIL( J )
             IFAIL( J ) = ITMP1
          END IF
       END IF
60    CONTINUE
 END IF
*
*     Set WORK(1) to optimal workspace size.
*
 WORK( 1 ) = LWKOPT
*/
 return;
 } // dsyevx
 
 /**
  * This is a port of version 3.3.1 LAPACK routine DSTEBZ.  The original DSTEBZ is created by by Univ. of Tennessee,
  Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. on April 2011
  
  DSTEBZ computes the eigenvalues of a symmetric tridiagonal
*  matrix T.  The user may ask for all eigenvalues, all eigenvalues
*  in the half-open interval (vl, vu], or the il-th through iu-th
*  eigenvalues.
*
*  To avoid overflow, the matrix must be scaled so that its
*  largest element is no greater than overflow**(1/2) *
*  underflow**(1/4) in absolute value, and for greatest
*  accuracy, it should not be much smaller than that.
*
*  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
*  Matrix", Report CS41, Computer Science Dept., Stanford
*  University, July 21, 1966.
*  
*  @param range input char = 'A': ("All")   all eigenvalues will be found.
*                          = 'V': ("Value") all eigenvalues in the half-open interval (vl, vu] will be found.
*                          = 'I': ("Index") the il-th through iu-th eigenvalues (of the entire matrix) will be found.
*  @param order input char = 'B': ("By Block") the eigenvalues will be grouped by split-off block (see iblock, isplit) and
*                             ordered from smallest to largest within the block.
*                          = 'E': ("Entire matrix") the eigenvalues for the entire matrix will be ordered from smallest to
*                                 largest.
*  @param n input int The order of the tridiagonal matrix T.  N >= 0.
*  @param vl input double
*  @param vu input double  
*             If range = 'V', the lower and upper bounds of the interval to be searched for eigenvalues.  Eigenvalues less
*             than or equal to vl, or greater than vu, will not be returned.  vl < vu.
*             Not referenced if range = 'A' or 'I'.
*  @param il input int
*  @param iu input int
*            If range = 'I', the indices (in ascending order) of the smallest and largest eigenvalues to be returned.
*            1 <= il <= iu <= n, if n > 0; il = 1 and iu = 0 if n = 0.
*            Not referenced if RANGE = 'A' or 'V'.
*  @param abstol input double  The absolute tolerance for the eigenvalues.  An eigenvalue
*          (or cluster) is considered to be located if it has been determined to lie in an interval whose width is abstol or
*          less.  If abstol is less than or equal to zero, then ULP*|T| will be used, where |T| means the 1-norm of T.
*          Eigenvalues will be computed most accurately when abstol is set to twice the underflow threshold 2*dlamch('S'), not zero.
*  @param d input double[]  The n diagonal elements of the tridiagonal matrix T.
*  @param e input double[]  The (n-1) off-diagonal elements of the tridiagonal matrix T.
*  @param m output int[] The actual number of eigenvalues found. 0 <= m[0] <= n.
*          (See also the description of info[0]=2,3.)
*  @param nsplit output int[]  The number of diagonal blocks in the matrix T.   1 <= nsplit[0] <= n.
*  @param w output double[] of dimension n.   On exit, the first m[0] elements of w will contain the
*          eigenvalues.  (dstebz may use the remaining n-m[0] elements as workspace.)
*  @param iblock output int[] of dimension n.   At each row/column j where E(j) is zero or small, the
*          matrix T is considered to split into a block diagonal matrix.  On exit, if info[0] = 0, iblock[i] specifies to which
*          block (from 1 to the number of blocks) the eigenvalue w[i] belongs.  (dstebz may use the remaining n-m[0] elements as
*          workspace.)
*  @param isplit output int[] of dimension n.  The splitting points, at which T breaks up into submatrices.
*          The first submatrix consists of rows/columns 1 to ISPLIT(1), the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
*          etc., and the NSPLIT-th consists of rows/columns ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
*          (Only the first NSPLIT elements will actually be used, but since the user cannot know a priori what value NSPLIT will
*          have, N words must be reserved for ISPLIT.)
*  @param work workspace double[] of dimension 4*n.
*  @param iwork workspace int[] of dimension 3*n.
*  @param info output int[] of dimension 1.  
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  some or all of the eigenvalues failed to converge or
*                were not computed:
*                =1 or 3: Bisection failed to converge for some
*                        eigenvalues; these eigenvalues are flagged by a
*                        negative block number.  The effect is that the
*                        eigenvalues may not be as accurate as the
*                        absolute and relative tolerances.  This is
*                        generally caused by unexpectedly inaccurate
*                        arithmetic.
*                =2 or 3: RANGE='I' only: Not all of the eigenvalues
*                        IL:IU were found.
*                        Effect: M < IU+1-IL
*                        Cause:  non-monotonic arithmetic, causing the
*                                Sturm sequence to be non-monotonic.
*                        Cure:   recalculate, using RANGE='A', and pick
*                                out eigenvalues IL:IU.  In some cases,
*                                increasing the PARAMETER "FUDGE" may
*                                make things work.
*                = 4:    RANGE='I', and the Gershgorin interval
*                        initially used was too small.  No eigenvalues
*                        were computed.
*                        Probable cause: your machine has sloppy
*                                        floating-point arithmetic.
*                        Cure: Increase the PARAMETER "FUDGE",
*                              recompile, and try again.
  */
  private void dstebz(char range, char order, int n, double vl, double vu, int il, int iu, double abstol, double d[], double e[],
                     int m[], int nsplit[], double w[], int iblock[], int isplit[], double work[], int iwork[], int info[] ) {

/*
*  Internal Parameters
*  ===================
*
*  RELFAC  DOUBLE PRECISION, default = 2.0e0
*          The relative tolerance.  An interval (a,b] lies within
*          "relative tolerance" if  b-a < RELFAC*ulp*max(|a|,|b|),
*          where "ulp" is the machine precision (distance from 1 to
*          the next larger floating point number.)
*
*  FUDGE   DOUBLE PRECISION, default = 2
*          A "fudge factor" to widen the Gershgorin intervals.  Ideally,
*          a value of 1 should work, but on machines with sloppy
*          arithmetic, this needs to be larger.  The default for
*          publicly released versions should be large enough to handle
*          the worst machine around.  Note that this has no effect
*          on accuracy of the solution.
*
*  =====================================================================
*
*/
  double fudge = 2.1;
  double relfac = 2.0;
  int irange;
  int iorder;
  boolean ncnvrg;
  boolean toofew;
  double ulp;
  double rtoli;
  int nb;
  double pivmin;
  double tmp1;
  double tmp2;
  int i, j;
  double safemn;
  double gl;
  double gu;
  double tnorm;
  int itmax;
  double atoli;
  int ivec1[];
  double vec1[];
  double vec2[];
  double vec3[];
  double vec4[];
  double vec5[];
  int iarray1[][];
  double array1[][];
  int index;
  int idumma[] = new int[1];
  int iinfo[] = new int[1];
  int iout[] = new int[1];
  int im[] = new int[1];
  double wl;
  double wlu = 0.0;
  int nwl;
  double wu;
  double wul = 0.0;
  int nwu;
  int jb;
  int ibegin;
  int iend;
  int ioff;
  int in;
  double bnorm;
  int iwoff;
  int ib;
  int je;
  int idiscl;
  int idiscu;
  double wkill;
  int jdisc;
  int iw;
  int ie;
  int itmp1;
  
  info[0] = 0;

  // Decode RANGE

  if((range == 'A') || (range == 'a')) {
     irange = 1;
  }
  else if ((range == 'V') || (range == 'v')) {
     irange = 2;
  }
  else if ((range == 'I') || (range == 'i')) {
     irange = 3;
  }
  else {
     irange = 0;
  }

  // Decode order

  if ((order == 'B') || (order == 'b')) {
     iorder = 2;
  }
  else if ((order == 'E') || (order == 'e')) {
     iorder = 1;
  }
  else {
     iorder = 0;
  }

  // Check for Errors

  if (irange <= 0 ) {
     info[0] = -1;
  }
  else if (iorder <= 0 ) {
     info[0] = -2;
  }
  else if (n < 0) {
     info[0] = -3;
  }
  else if (irange == 2 )  {
     if (vl >= vu) {
        info[0] = -5;
     }
  }
  else if (irange == 3 && (il < 1 || il > Math.max( 1, n) ) ) {
     info[0] = -6;
  }
  else if (irange == 3 && (iu < Math.min(n, il) || iu > n) ) {
     info[0] = -7;
  }

  if (info[0] != 0 ) {
     MipavUtil.displayError("dstebz had info[0] = " + info[0]);
     return;
  }

  // Initialize error flags

  info[0] = 0;
  ncnvrg = false;
  toofew = false;

  // Quick return if possible

  m[0] = 0;
  if (n == 0) {
      return;
  }

  // Simplifications:

  if (irange == 3 && il == 1 && iu == n) {
      irange = 1;
  }

  // Get machine constants
  // NB is the minimum vector length for vector bisection, or if only scalar is to be done.

  safemn = ge.dlamch( 'S' );
  ulp = ge.dlamch( 'P' );
  rtoli = ulp*relfac;
  nb = ge.ilaenv( 1, "DSTEBZ", " ", n, -1, -1, -1 );
  if (nb <= 1 ) {
      nb = 0;
  }

  // Special Case when N=1

  if (n == 1 ) {
     nsplit[0] = 1;
     isplit[0] = 1;
     if (irange == 2 && (vl >= d[0] || vu < d[0] ) ) {
       m[0] = 0;
     }
     else {
        w[0] = d[0];
        iblock[0] = 1;
        m[0] = 1;
     }
     return;
  } // if (n == 1)

  // Compute Splitting Points

  nsplit[0] = 1;
  work[n-1] = 0.0;
  pivmin = 1.0;

  for (j = 2; j <= n; j++) {
     tmp1 = e[j-2]*e[j-2];
     if ( Math.abs( d[j-1]*d[j-2] )*ulp*ulp+safemn > tmp1) {
        isplit[nsplit[0]-1] = j - 1;
        nsplit[0] = nsplit[0] + 1;
        work[j-2] = 0.0;
     }
     else {
        work[j-2] = tmp1;
        pivmin = Math.max(pivmin, tmp1);
     }
  } // for (j = 2; j <= n; j++)
  isplit[nsplit[0]] = n;
  pivmin = pivmin*safemn;
  

  // Compute Interval and ATOLI

  if (irange == 3 ) {

        // RANGE='I': Compute the interval containing eigenvalues IL through IU.

       // Compute Gershgorin interval for entire (split) matrix and use it as the initial interval

     gu = d[0];
     gl = d[0];
     tmp1 = 0.0;

     for (j = 1; j <= n - 1; j++) {
        tmp2 = Math.sqrt(work[j-1] );
        gu = Math.max(gu, d[j-1]+tmp1+tmp2 );
        gl = Math.min(gl, d[j-1]-tmp1-tmp2 );
        tmp1 = tmp2;
     } // for (j = 1; j <= n - 1; j++)

     gu = Math.max(gu, d[n-1]+tmp1 );
     gl = Math.min(gl, d[n-1]-tmp1 );
     tnorm = Math.max(Math.abs(gl), Math.abs(gu));
     gl = gl - fudge*tnorm*ulp*n - 2.0*fudge*pivmin;
     gu = gu + fudge*tnorm*ulp*n + fudge*pivmin;

     // Compute Iteration parameters

     itmax = (int)((Math.log(tnorm+pivmin)-Math.log(pivmin))/Math.log(2.0)) + 2;
     if (abstol <= 0.0) {
        atoli = ulp*tnorm;
     }
     else {
        atoli = abstol;
     }

     work[n] = gl;
     work[n+1] = gl;
     work[n+2] = gu;
     work[n+3] = gu;
     work[n+4] = gl;
     work[n+5] = gu;
     iwork[0] = -1;
     iwork[1] = -1;
     iwork[2] = n+1;
     iwork[3] = n + 1;
     iwork[4] = il - 1;
     iwork[5] = iu;
     
     ivec1 = new int[2];
     ivec1[0] = iwork[4];
     ivec1[1] = iwork[5];
     array1 = new double[2][2];
     array1[0][0] = work[n];
     array1[1][0] = work[n+1];
     array1[0][1] = work[n+2];
     array1[1][1] = work[n+3];
     vec2 = new double[2];
     vec2[0] = work[n+4];
     vec2[1] = work[n+5];
     iarray1 = new int[2][2];
     iarray1[0][0] = iwork[0];
     iarray1[1][0] = iwork[1];
     iarray1[0][1] = iwork[2];
     iarray1[1][1] = iwork[3];

     dlaebz( 3, itmax, n, 2, 2, nb, atoli, rtoli, pivmin, d, e,
                  work, ivec1, array1, vec2, iout, iarray1, w, iblock, iinfo);
     iwork[4] = ivec1[0];
     iwork[5] = ivec1[1];
     work[n] = array1[0][0];
     work[n+1] = array1[1][0];
     work[n+2] = array1[0][1];
     work[n+3] = array1[1][1];
     work[n+4] = vec2[0];
     work[n+5] = vec2[1];
     iwork[0] = iarray1[0][0];
     iwork[1] = iarray1[1][0];
     iwork[2] = iarray1[0][1];
     iwork[3] = iarray1[1][1];
     
     if (iwork[5] == iu) {
        wl = work[n];
        wlu = work[n+2];
        nwl = iwork[0];
        wu = work[n+3];
        wul = work[n+1];
        nwu = iwork[3];
     }
     else {
        wl = work[n+1];
        wlu = work[n+3];
        nwl = iwork[1];
        wu = work[n+2];
        wul = work[n];
        nwu = iwork[2];
     }

     if (nwl < 0 || nwl >= n || nwu < 1 || nwu > n) {
        info[0] = 4;
        return;
     }
  } // if (irange == 3)
  else { // irange != 3

     // RANGE='A' or 'V' -- Set ATOLI

     tnorm = Math.max(Math.abs(d[0])+Math.abs(e[0] ),
             Math.abs( d[n-1] )+Math.abs(e[n-2] ) );

     for (j = 2; j <= n-1; j++) {
        tnorm = Math.max(tnorm, Math.abs(d[j-1] )+Math.abs(e[j-2] )+ Math.abs(e[j-1] ) );
     } // for (j = 2; j <= n-1; j++)

     if (abstol <= 0.0) {
        atoli = ulp*tnorm;
     }
     else {
        atoli = abstol;
     }

     if (irange == 2) {
        wl = vl;
        wu = vu;
     }
     else {
        wl = 0.0;
        wu = 0.0;
     }
  } // else irange != 3

  // Find Eigenvalues -- Loop Over Blocks and recompute NWL and NWU.
  // NWL accumulates the number of eigenvalues .le. WL,
  // NWU accumulates the number of eigenvalues .le. WU

  m[0] = 0;
  iend = 0;
  info[0] = 0;
  nwl = 0;
  nwu = 0;

  for (jb = 1; jb <= nsplit[0]; jb++) {
     ioff = iend;
     ibegin = ioff + 1;
     iend = isplit[jb-1];
     in = iend - ioff;

     if (in == 1 ) {

        // Special Case -- IN=1

        if (irange == 1 || wl >= d[ibegin-1]-pivmin) {
           nwl = nwl + 1;
        }
        if (irange == 1 || wu >= d[ibegin-1]-pivmin) {
           nwu = nwu + 1;
        }
        if (irange == 1 || (wl < d[ibegin-1]-pivmin && wu >= d[ibegin-1]-pivmin) ) {
           m[0] = m[0] + 1;
           w[m[0]-1] = d[ibegin-1];
           iblock[m[0]-1] = jb;
        }
     } // if (in == 1)
     else { // in > 1

            // General Case -- IN > 1

            // Compute Gershgorin Interval
            // and use it as the initial interval

        gu = d[ibegin-1];
        gl = d[ibegin-1];
        tmp1 = 0.0;

        for (j = ibegin; j <= iend - 1; j++) {
           tmp2 = Math.abs(e[j-1]);
           gu = Math.max(gu, d[j-1]+tmp1+tmp2);
           gl = Math.min(gl, d[j-1]-tmp1-tmp2 );
           tmp1 = tmp2;
        } // for (j = ibegin; j <= iend - 1; j++)

        gu = Math.max(gu, d[iend-1]+tmp1);
        gl = Math.min(gl, d[iend-1]-tmp1);
        bnorm = Math.max(Math.abs(gl), Math.abs(gu) );
        gl = gl - fudge*bnorm*ulp*in - fudge*pivmin;
        gu = gu + fudge*bnorm*ulp*in + fudge*pivmin;

        // Compute ATOLI for the current submatrix

        if (abstol <= 0.0) {
           atoli = ulp*Math.max(Math.abs(gl), Math.abs(gu) );
        }
        else {
           atoli = abstol;
        }

        if (irange > 1) {
           if (gu < wl) {
              nwl = nwl + in;
              nwu = nwu + in;
              break;
           }
           gl = Math.max(gl, wl);
           gu = Math.min(gu, wu);
           if (gl >= gu) {
              break;
           }
        }

        // Set Up Initial Interval

        work[n] = gl;
        work[n+in] = gu;
        vec1 = new double[in];
        for (j = 0; j < in; j++) {
            vec1[j] = d[ibegin-1+j];
        }
        vec2 = new double[in];
        for (j = 0; j < in; j++) {
            vec2[j] = e[ibegin-1+j];
        }
        vec3 = new double[in];
        for (j = 0; j < in; j++) {
            vec3[j] = work[ibegin-1+j];
        }
        array1 = new double[in][2];
        index = 0;
        for (j = 0; j < 1; j++) {
            for (i = 0; i < in; i++) {
                array1[i][j] = work[n + index];
                index++;
            }
        }
        vec4 = new double[in];
        for (j = 0; j < in; j++) {
            vec4[j] = work[n+2*in+j];
        }
        iarray1 = new int[in][2];
        index = 0;
        for (j = 0; j < 1; j++) {
            for (i = 0; i < in; i++) {
                iarray1[i][j] = iwork[index];
                index++;
            }
        }
        vec5 = new double[in];
        ivec1 = new int[in];
        dlaebz( 1, 0, in, in, 1, nb, atoli, rtoli, pivmin,
                     vec1, vec2, vec3,
                     idumma, array1, vec4, im,
                     iarray1, vec5, ivec1, iinfo);
        index = 0;
        for (j = 0; j < 1; j++) {
            for (i = 0; i < in; i++) {
                work[n + index] = array1[i][j];
                index++;
            }
        }
        for (j = 0; j < in; j++) {
            work[n+2*in+j] = vec4[j];
        }
        index = 0;
        for (j = 0; j < 1; j++) {
            for (i = 0; i < in; i++) {
                iwork[index] = iarray1[i][j];
                index++;
            }
        }

        nwl = nwl + iwork[0];
        nwu = nwu + iwork[in];
        iwoff = m[0] - iwork[0];

        // Compute Eigenvalues

        itmax = (int)( (Math.log(gu-gl+pivmin)-Math.log(pivmin) ) / Math.log(2.0) ) + 2;
        for (j = 0; j < in; j++) {
            vec1[j] = d[ibegin-1+j];
        }
        for (j = 0; j < in; j++) {
            vec2[j] = e[ibegin-1+j];
        }
        for (j = 0; j < in; j++) {
            vec3[j] = work[ibegin-1+j];
        }
        index = 0;
        for (j = 0; j < 1; j++) {
            for (i = 0; i < in; i++) {
                array1[i][j] = work[n + index];
                index++;
            }
        }
        for (j = 0; j < in; j++) {
            vec4[j] = work[n+2*in+j];
        }
        index = 0;
        for (j = 0; j < 1; j++) {
            for (i = 0; i < in; i++) {
                iarray1[i][j] = iwork[index];
                index++;
            }
        }
        dlaebz( 2, itmax, in, in, 1, nb, atoli, rtoli, pivmin,
                     vec1, vec2, vec3,
                     idumma, array1, vec4, iout,
                     iarray1, vec5, ivec1, iinfo);
        index = 0;
        for (j = 0; j < 1; j++) {
            for (i = 0; i < in; i++) {
                work[n + index] = array1[i][j];
                index++;
            }
        }
        for (j = 0; j < in; j++) {
            work[n+2*in+j] = vec4[j];
        }
        index = 0;
        for (j = 0; j < 1; j++) {
            for (i = 0; i < in; i++) {
                iwork[index] = iarray1[i][j];
                index++;
            }
        }

        // Copy Eigenvalues Into W and IBLOCK
        // Use -JB for block number for unconverged eigenvalues.

        for (j = 1; j <= iout[0]; j++) {
           tmp1 = 0.5*(work[j+n-1]+work[j+in+n-1] );

           // Flag non-convergence.

           if (j > iout[0] - iinfo[0]) {
              ncnvrg = true;
              ib = -jb;
           }
           else {
              ib = jb;
           }
           for (je = iwork[j-1] + 1 + iwoff; je <= iwork[j+in-1] + iwoff; je++) {
              w[je-1] = tmp1;
              iblock[je-1] = ib;
           } // for (je = iwork[j-1] + 1 + iwoff; je <= iwork[j+in-1] + iwoff; je++)
        } // for (j = 1; j <= iout[0]; j++)

        m[0] = m[0] + im[0];
     } /// else in > 1
  } // for (jb = 1; jb <= nsplit[0]; jb++)

  // If RANGE='I', then (WL,WU) contains eigenvalues NWL+1,...,NWU
  // If NWL+1 < IL or NWU > IU, discard extra eigenvalues.

  if (irange == 3 ) {
     im[0] = 0;
     idiscl = il - 1 - nwl;
     idiscu = nwu - iu;

     if (idiscl > 0 || idiscu > 0) {
        for (je = 1; je <= m[0]; je++) {
           if (w[je-1] <= wlu && idiscl > 0) {
              idiscl = idiscl - 1;
           }
           else if (w[je-1] >= wul && idiscu > 0) {
              idiscu = idiscu - 1;
           }
           else {
              im[0] = im[0] + 1;
              w[im[0]-1] = w[je-1];
              iblock[im[0]-1] = iblock[je-1];
           }
        } // for (je = 1; je <= m[0]; je++)
        m[0] = im[0];
     } // if (idiscl > 0 || idiscu > 0)
     if (idiscl > 0 || idiscu > 0) {

          // Code to deal with effects of bad arithmetic:
          // Some low eigenvalues to be discarded are not in (WL,WLU],
          // or high eigenvalues to be discarded are not in (WUL,WU]
          // so just kill off the smallest IDISCL/largest IDISCU
          // eigenvalues, by simply finding the smallest/largest
          // eigenvalue(s).

          // (If N(w) is monotone non-decreasing, this should never
          //  happen.)

        if (idiscl > 0 ) {
           wkill = wu;
           for (jdisc = 1; jdisc <= idiscl; jdisc++) {
              iw = 0;
              for (je = 1; je <= m[0]; je++) {
                 if (iblock[je-1] != 0 && (w[je-1] < wkill || iw == 0 ) ) {
                    iw = je;
                    wkill = w[je-1];
                 }
               } // for (je = 1; je <= m[0]; je++)
               iblock[iw-1] = 0;
           } // for (jdisc = 1; jdisc <= idiscl; jdisc++)
        } // if (idiscl > 0 )
        if (idiscu > 0) {

           wkill = wl;
           for (jdisc = 1; jdisc <= idiscu; jdisc++) {
              iw = 0;
              for (je = 1; je <= m[0]; je++) {
                 if (iblock[je-1] != 0 && (w[je-1] > wkill || iw == 0 ) ) {
                    iw = je;
                    wkill = w[je-1];
                 }
                } // for (je = 1; je <= m[0]; je++)
              iblock[iw-1] = 0;
           } // for (jdisc = 1; jdisc <= idiscu; jdisc++)
        } // if (idiscu > 0)
        im[0] = 0;
        for (je = 1; je <= m[0]; je++) {
           if (iblock[je-1] != 0 ) {
              im[0] = im[0] + 1;
              w[im[0]-1] = w[je-1];
              iblock[im[0]-1] = iblock[je-1];
           }
        } // for (je = 1; je <= m[0]; je++)
        m[0] = im[0];
     } // if (idiscl > 0 || idiscu > 0)
     if (idiscl < 0 || idiscu < 0 ) {
        toofew = true;
     }
  } // if (irange == 3)

      // If ORDER='B', do nothing -- the eigenvalues are already sorted by block.
      // If ORDER='E', sort the eigenvalues from smallest to largest

  if (iorder == 1 && nsplit[0] > 1 ) {
     for (je = 1; je <= m[0]-1; je++) {
        ie = 0;
        tmp1 = w[je-1];
        for (j = je + 1; j <= m[0]; j++) {
           if ( w[j-1] < tmp1) {
              ie = j;
              tmp1 = w[j-1];
           }
        } // for (j = je + 1; j <= m[0]; j++)

        if (ie != 0) {
           itmp1 = iblock[ie-1];
           w[ie-1] = w[je-1];
           iblock[ie-1] = iblock[je-1];
           w[je-1] = tmp1;
           iblock[je-1] = itmp1;
        }
     } // for (je = 1; je <= m[0]-1; je++)
  } // if (iorder == 1 && nsplit[0] > 1 )

  info[0] = 0;
  if (ncnvrg) {
    info[0] = info[0] + 1;
  }
  if (toofew) {
    info[0] = info[0] + 2;
  }
  return;

} // dstebz
  
  /** This is a port of version 3.3.1 LAPACK routine DLAEBZ.  The original DSTEBZ is created by by Univ. of Tennessee,
  Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. on April 2011
  
   DLAEBZ contains the iteration loops which compute and use the
*  function N(w), which is the count of eigenvalues of a symmetric
*  tridiagonal matrix T less than or equal to its argument  w.  It
*  performs a choice of two types of loops:
*
*  IJOB=1, followed by
*  IJOB=2: It takes as input a list of intervals and returns a list of
*          sufficiently small intervals whose union contains the same
*          eigenvalues as the union of the original intervals.
*          The input intervals are (AB(j,1),AB(j,2)], j=1,...,MINP.
*          The output interval (AB(j,1),AB(j,2)] will contain
*          eigenvalues NAB(j,1)+1,...,NAB(j,2), where 1 <= j <= MOUT.
*
*  IJOB=3: It performs a binary search in each input interval
*          (AB(j,1),AB(j,2)] for a point  w(j)  such that
*          N(w(j))=NVAL(j), and uses  C(j)  as the starting point of
*          the search.  If such a w(j) is found, then on output
*          AB(j,1)=AB(j,2)=w.  If no such w(j) is found, then on output
*          (AB(j,1),AB(j,2)] will be a small interval containing the
*          point where N(w) jumps through NVAL(j), unless that point
*          lies outside the initial interval.
*
*  Note that the intervals are in all cases half-open intervals,
*  i.e., of the form  (a,b] , which includes  b  but not  a .
*
*  To avoid underflow, the matrix should be scaled so that its largest
*  element is no greater than  overflow**(1/2) * underflow**(1/4)
*  in absolute value.  To assure the most accurate computation
*  of small eigenvalues, the matrix should be scaled to be
*  not much smaller than that, either.
*
*  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
*  Matrix", Report CS41, Computer Science Dept., Stanford
*  University, July 21, 1966
*
*  Note: the arguments are, in general, *not* checked for unreasonable
*  values.
  @param ijob input int  Specifies what is to be done:
*          = 1:  Compute nab for the initial intervals.
*          = 2:  Perform bisection iteration to find eigenvalues of T.
*          = 3:  Perform bisection iteration to invert N(w), i.e.,
*                to find a point which has a specified number of
*                eigenvalues of T to its left.
*          Other values will cause dlaebz to return with info[0]=-1.
  @param nitmax input int The maximum number of "levels" of bisection to be
*          performed, i.e., an interval of width W will not be made
*          smaller than 2^(-nitmax) * W.  If not all intervals
*          have converged after nitmax iterations, then info[0] is set
*          to the number of non-converged intervals.
  @param n input int  The dimension n of the tridiagonal matrix T.  It must be at least 1.
  @param mmax input int The maximum number of intervals.  If more than mmax intervals
*          are generated, then dlaebz will quit with info[0]=mmax+1.
  @param minp input int The initial number of intervals.  It may not be greater than mmax.
  @param nbmin input int The smallest number of intervals that should be processed
*          using a vector loop.  If zero, then only the scalar loop will be used.
  @param abstol input double The minimum (absolute) width of an interval.  When an
*          interval is narrower than abstol, or than reltol times the
*          larger (in magnitude) endpoint, then it is considered to be
*          sufficiently small, i.e., converged.  This must be at least
*          zero.
  @param reltol input double The minimum relative width of an interval.  When an interval
*          is narrower than abstol, or than reltol times the larger (in
*          magnitude) endpoint, then it is considered to be
*          sufficiently small, i.e., converged.  Note: this should
*          always be at least radix*machine epsilon.
  @param pivmin input double The minimum absolute value of a "pivot" in the Sturm
*          sequence loop.  This *must* be at least  max |e(j)**2| *
*          safe_min  and at least safe_min, where safe_min is at least
*          the smallest number that can divide one without overflow.
  @param d input double[] of dimension n.  The diagonal elements of the tridiagonal matrix T.
  @param e input double[] of dimension n.  The offdiagonal elements of the tridiagonal matrix T in
*          positions 1 through N-1.  E(N) is arbitrary.
  @param e2 input double[] of dimension n.  The squares of the offdiagonal elements of the tridiagonal
*          matrix T.  E2(N) is ignored.
  @param nval (input/output) int[] of dimension minp.  If IJOB=1 or 2, not referenced.
*          If IJOB=3, the desired values of N(w).  The elements of NVAL
*          will be reordered to correspond with the intervals in AB.
*          Thus, NVAL(j) on output will not, in general be the same as
*          NVAL(j) on input, but it will correspond with the interval
*          (AB(j,1),AB(j,2)] on output.
  @param AB (input/output) double[][] of dimension (mmax,2) The endpoints of the intervals.  AB(j,1) is  a(j), the left
*          endpoint of the j-th interval, and AB(j,2) is b(j), the
*          right endpoint of the j-th interval.  The input intervals
*          will, in general, be modified, split, and reordered by the
*          calculation.
  @param c (input/output) double[] of dimension mmax.  If IJOB=1, ignored.
*          If IJOB=2, workspace.
*          If IJOB=3, then on input C(j) should be initialized to the
*          first search point in the binary search.
  @param mout output int[] of dimension 1. If IJOB=1, the number of eigenvalues in the intervals.
*          If IJOB=2 or 3, the number of intervals output.
*          If IJOB=3, mout[0] will equal minp.
  @param NAB (input/output) int[][] array of dimension (mmax,2)
           If IJOB=1, then on output NAB(i,j) will be set to N(AB(i,j)).
*          If IJOB=2, then on input, NAB(i,j) should be set.  It must
*             satisfy the condition:
*             N(AB(i,1)) <= NAB(i,1) <= NAB(i,2) <= N(AB(i,2)),
*             which means that in interval i only eigenvalues
*             NAB(i,1)+1,...,NAB(i,2) will be considered.  Usually,
*             NAB(i,j)=N(AB(i,j)), from a previous call to DLAEBZ with
*             IJOB=1.
*             On output, NAB(i,j) will contain
*             max(na(k),min(nb(k),N(AB(i,j)))), where k is the index of
*             the input interval that the output interval
*             (AB(j,1),AB(j,2)] came from, and na(k) and nb(k) are the
*             the input values of NAB(k,1) and NAB(k,2).
*          If IJOB=3, then on output, NAB(i,j) contains N(AB(i,j)),
*             unless N(w) > NVAL(i) for all search points  w , in which
*             case NAB(i,1) will not be modified, i.e., the output
*             value will be the same as the input value (modulo
*             reorderings -- see NVAL and AB), or unless N(w) < NVAL(i)
*             for all search points  w , in which case NAB(i,2) will
*             not be modified.  Normally, NAB should be set to some
*             distinctive value(s) before dlaebz is called.
  @param work workspace double[] of dimension mmax.
  @param iwork workspace int[] of dimension mmax.
  @param info output int[] of dimension 1.
           = 0:       All intervals converged.
*          = 1--MMAX: The last INFO intervals did not converge.
*          = MMAX+1:  More than MMAX intervals were generated.
*          
*  Further Details
*  ===============
*
*      This routine is intended to be called only by other LAPACK
*  routines, thus the interface is less user-friendly.  It is intended
*  for two purposes:
*
*  (a) finding eigenvalues.  In this case, DLAEBZ should have one or
*      more initial intervals set up in AB, and DLAEBZ should be called
*      with IJOB=1.  This sets up NAB, and also counts the eigenvalues.
*      Intervals with no eigenvalues would usually be thrown out at
*      this point.  Also, if not all the eigenvalues in an interval i
*      are desired, NAB(i,1) can be increased or NAB(i,2) decreased.
*      For example, set NAB(i,1)=NAB(i,2)-1 to get the largest
*      eigenvalue.  DLAEBZ is then called with IJOB=2 and MMAX
*      no smaller than the value of MOUT returned by the call with
*      IJOB=1.  After this (IJOB=2) call, eigenvalues NAB(i,1)+1
*      through NAB(i,2) are approximately AB(i,1) (or AB(i,2)) to the
*      tolerance specified by ABSTOL and RELTOL.
*
*  (b) finding an interval (a',b'] containing eigenvalues w(f),...,w(l).
*      In this case, start with a Gershgorin interval  (a,b).  Set up
*      AB to contain 2 search intervals, both initially (a,b).  One
*      NVAL element should contain  f-1  and the other should contain  l
*      , while C should contain a and b, resp.  NAB(i,1) should be -1
*      and NAB(i,2) should be N+1, to flag an error if the desired
*      interval does not lie in (a,b).  DLAEBZ is then called with
*      IJOB=3.  On exit, if w(f-1) < w(f), then one of the intervals --
*      j -- will have AB(j,1)=AB(j,2) and NAB(j,1)=NAB(j,2)=f-1, while
*      if, to the specified tolerance, w(f-k)=...=w(f+r), k > 0 and r
*      >= 0, then the interval will have  N(AB(j,1))=NAB(j,1)=f-k and
*      N(AB(j,2))=NAB(j,2)=f+r.  The cases w(l) < w(l+1) and
*      w(l-r)=...=w(l+k) are handled similarly.
*
*/
  private void dlaebz(int ijob, int nitmax, int n, int mmax, int minp, int nbmin, double abstol,
                     double reltol, double pivmin, double d[], double e[], double e2[], int nval[], double AB[][], double c[], int mout[],
                     int NAB[][], double work[], int iwork[], int info[]) {
   int ji;
   double tmp1;
   int jp;
   int j;
   int kf;
   int kl;
   int jit;
   int klnew;
   double tmp2;
   int itmp1;
   int kfnew;
   int itmp2;

  // Check for Errors

  info[0] = 0;
  if (ijob < 1 || ijob > 3) {
     info[0] = -1;
     return;
  }

  // Initialize NAB

  if (ijob == 1 ) {

     //Compute the number of eigenvalues in the initial intervals.

     mout[0] = 0;
     for (ji = 1; ji <= minp; ji++) {
        for (jp = 1; jp <= 2; jp++) {
           tmp1 = d[0] - AB[ji-1][jp-1];
           if (Math.abs(tmp1) < pivmin) {
              tmp1 = -pivmin;
           }
           NAB[ji-1][jp-1] = 0;
           if (tmp1 <= 0.0) {
              NAB[ji-1][jp-1] = 1;
           }

           for (j = 2; j <= n; j++) {
              tmp1 = d[j-1] - e2[j-2] /tmp1 - AB[ji-1][jp-1];
              if (Math.abs(tmp1) < pivmin) {
                 tmp1 = -pivmin;
              }
              if (tmp1 <= 0.0) {
                 NAB[ji-1][jp-1] = NAB[ji-1][jp-1] + 1;
              }
           } // for (j = 2; j <= n; j++) 
        } // for (jp = 1; jp <= 2; jp++)
        mout[0] = mout[0] + NAB[ji-1][1] - NAB[ji-1][0];
     } // for (ji = 1; ji <= minp; ji++)
     return;
  } // if (ijob == 1)

      // Initialize for loop

      // KF and KL have the following meaning:
       // Intervals 1,...,KF-1 have converged.
       // Intervals KF,...,KL  still need to be refined.

  kf = 1;
  kl = minp;

   // If IJOB=2, initialize C.
   // If IJOB=3, use the user-supplied starting point.

  if (ijob == 2) {
     for (ji =1; ji <= minp; ji++) {
        c[ji-1] = 0.5*( AB[ji-1][0]+AB[ji-1][1]);
     } // for (ji =1; ji <= minp; ji++)
  }

    // Iteration loop

  for (jit = 1; jit <= nitmax; jit++) {

        // Loop over intervals

     if (kl-kf+1 >= nbmin && nbmin > 0 ) {

        // Begin of Parallel Version of the loop

        for (ji = kf; ji <= kl; ji++) {

           // Compute N(c), the number of eigenvalues less than c

           work[ji-1] = d[0] - c[ji-1];
           iwork[ji-1] = 0;
           if (work[ji-1] <= pivmin) {
              iwork[ji-1] = 1;
              work[ji-1] = Math.min(work[ji-1], -pivmin);
           }

           for (j = 2; j <= n; j++) {
              work[ji-1] = d[j-1] - e2[j-2] /work[ji-1] - c[ji-1];
              if (work[ji-1] <= pivmin) {
                 iwork[ji-1] = iwork[ji-1] + 1;
                 work[ji-1] = Math.min(work[ji-1], -pivmin);
              }
           } // for (j = 2; j <= n; j++)
        } // for (ji = kf; ji <= kl; ji++)

        if (ijob <= 2) {
            
           // IJOB=2: Choose all intervals containing eigenvalues.

           klnew = kl;
           for (ji = kf; ji <= kl; ji++) {

              // Insure that N(w) is monotone

              iwork[ji-1] = Math.min(NAB[ji-1][1],  Math.max( NAB[ji-1][0], iwork[ji-1] ) );

              // Update the Queue -- add intervals if both halves contain eigenvalues.

              if (iwork[ji-1] == NAB[ji-1][1]) {

                 // No eigenvalue in the upper interval:
                 // just use the lower interval.

                 AB[ji-1][1] = c[ji-1];
              }
              else if (iwork[ji-1] == NAB[ji-1][0] ) {

                  // No eigenvalue in the lower interval:
                  // just use the upper interval.
 
                  AB[ji-1][0] = c[ji-1];
              }
              else {
                 klnew = klnew + 1;
                 if (klnew <= mmax) {

                    // Eigenvalue in both intervals -- add upper to queue.

                    AB[klnew-1][1] = AB[ji-1][1];
                    NAB[klnew-1][1] = NAB[ji-1][1];
                    AB[klnew-1][0] = c[ji-1];
                    NAB[klnew-1][0] = iwork[ji-1];
                    AB[ji-1][1] = c[ji-1];
                    NAB[ji-1][1] = iwork[ji-1];
                 }
                 else {
                    info[0] = mmax + 1;
                 }
              }
            } // for (ji = kf; ji <= kl; ji++)
           if (info[0] != 0) {
              return;
           }
           kl = klnew;
        } // if (ijob <= 2)
        else { // ijob == 3
            
            // IJOB=3: Binary search.  Keep only the interval containing
            // w   s.t. N(w) = NVAL

           for (ji = kf; ji <= kl; ji++) {
              if (iwork[ji-1] <= nval[ji-1]) {
                 AB[ji-1][0] = c[ji-1];
                 NAB[ji-1][0] = iwork[ji-1];
              }
              if (iwork[ji-1] >= nval[ji-1]) {
                 AB[ji-1][1] = c[ji-1];
                 NAB[ji-1][1] = iwork[ji-1];
              }
           } // for (ji = kf; ji <= kl; ji++)
        } // else ijob == 3

     }  // if (kl-kf+1 >= nbmin && nbmin > 0 ) 
     else { // serial version of loop

            // End of Parallel Version of the loop

            // Begin of Serial Version of the loop

        klnew = kl;
        for (ji = kf; ji <= kl; ji++) {

           // Compute N(w), the number of eigenvalues less than w

           tmp1 = c[ji-1];
           tmp2 = d[0] - tmp1;
           itmp1 = 0;
           if (tmp2 <= pivmin) {
              itmp1 = 1;
              tmp2 = Math.min(tmp2, -pivmin);
           }

           for (j = 2; j <= n; j++) {
              tmp2 = d[j-1] - e2[j-2] / tmp2 - tmp1;
              if (tmp2 <= pivmin) {
                 itmp1 = itmp1 + 1;
                 tmp2 = Math.min(tmp2, -pivmin);
              }
           } // for (j = 2; j <= n; j++)

           if (ijob <= 2) {

                 // IJOB=2: Choose all intervals containing eigenvalues.

                 // Insure that N(w) is monotone

              itmp1 = Math.min( NAB[ji-1][1], Math.max(NAB[ji-1][0], itmp1 ) );
              
              // Update the Queue -- add intervals if both halves contain eigenvalues.

              if (itmp1 == NAB[ji-1][1]) {

                 // No eigenvalue in the upper interval:
                 // just use the lower interval.

                 AB[ji-1][1] = tmp1;
              }
              else if (itmp1 == NAB[ji-1][0]) {

                     // No eigenvalue in the lower interval:
                     //  just use the upper interval.

                 AB[ji-1][0] = tmp1;
              }
              else if (klnew < mmax) {

                 // Eigenvalue in both intervals -- add upper to queue.

                 klnew = klnew + 1;
                 AB[klnew-1][1] = AB[ji-1][1];
                 NAB[klnew-1][1] = NAB[ji-1][1];
                 AB[klnew-1][0] = tmp1;
                 NAB[klnew-1][0] = itmp1;
                 AB[ji-1][1] = tmp1;
                 NAB[ji-1][1] = itmp1;
              }
              else {
                 info[0] = mmax + 1;
                 return;
              }
           } // if ijob <= 2)
           else { // ijob == 3

               // IJOB=3: Binary search.  Keep only the interval containing  w  s.t. N(w) = NVAL

              if (itmp1 <= nval[ji-1]) {
                 AB[ji-1][0] = tmp1;
                 NAB[ji-1][0] = itmp1;
              }
              if (itmp1 >= nval[ji-1]) {
                 AB[ji-1][1] = tmp1;
                 NAB[ji-1][1] = itmp1;
              }
           } // else ijob == 3
        } // for (ji = kf; ji <= kl; ji++)
        kl = klnew;

     } // serial version of loop

     // Check for convergence

     kfnew = kf;
     for (ji = kf; ji <= kl; ji++) {
        tmp1 = Math.abs( AB[ji-1][1]-AB[ji-1][0]);
        tmp2 = Math.max(Math.abs( AB[ji-1][1]), Math.abs( AB[ji-1][0] ) );
        if (tmp1 < Math.max(abstol, Math.max(pivmin, reltol*tmp2)) ||
            NAB[ji-1][0] >= NAB[ji-1][1]) {

           // Converged -- Swap with position KFNEW,
           //              then increment KFNEW

           if (ji > kfnew) {
              tmp1 = AB[ji-1][0];
              tmp2 = AB[ji-1][1];
              itmp1 = NAB[ji-1][0];
              itmp2 = NAB[ji-1][1];
              AB[ji-1][0] = AB[kfnew-1][0];
              AB[ji-1][1] = AB[kfnew-1][1];
              NAB[ji-1][0] = NAB[kfnew-1][0];
              NAB[ji-1][1] = NAB[kfnew-1][1];
              AB[kfnew-1][0] = tmp1;
              AB[kfnew-1][1] = tmp2;
              NAB[kfnew-1][0] = itmp1;
              NAB[kfnew-1][1] = itmp2;
              if (ijob == 3) {
                 itmp1 = nval[ji-1];
                 nval[ji-1] = nval[kfnew-1];
                 nval[kfnew-1] = itmp1;
              } // if (ijob == 3)
           } // if (ji > kfnew)
           kfnew = kfnew + 1;
           } // if (tmp1 < Math.max(abstol, Math.max(pivmin, reltol*tmp2))
       } // for (ji = kf; ji <= kl; ji++)
     kf = kfnew;
     
     // Choose Midpoints

     for (ji = kf; ji <= kl; ji++) {
        c[ji-1] = 0.5*( AB[ji-1][0]+AB[ji-1][1]);
     } // for (ji = kf; ji <= kl; ji++)

     // If no more intervals to refine, quit.

     if (kf > kl) {
       break;
     }
 } // for (jit = 1; jit <= nitmax; jit++)

   // Converged

  info[0] = Math.max(kl+1-kf, 0);
  mout[0] = kl;

  return;
  } // dlaebz


}