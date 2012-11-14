package gov.nih.mipav.model.structures.jama;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class SelectedEigenvalue2 implements java.io.Serializable {
    
    // dchkst_test repeats 5 times: All 630 tests for dchkst passed the threshold.  This indicates the dstebz and dstein are working.
    // ddrvst_test repeats 5 times: All 1944 tests for ddrvst passed the threshold.
 // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * 
     */
    //private static final long serialVersionUID;

    /**
     * Creates a new SelectedEigenvalue object.
     */
    public SelectedEigenvalue2() {}
    
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    SelectedEigenvalue se = new SelectedEigenvalue();
    
    /** Common variables in testing routines. */
    private ViewUserInterface UI = ViewUserInterface.getReference();
    
    
 
 /** This is a port of the version 3.2.2 LAPACK DSYEVR routine.  Original DSYEVR created by created by Univ. of Tennessee, Univ. of
 California Berkeley, University of Colorado Denver, and NAG Ltd., June 2010
      Based on contributions by
*     Inderjit Dhillon, IBM Almaden, USA
*     Osni Marques, LBNL/NERSC, USA
*     Ken Stanley, Computer Science Division, University of
*       California at Berkeley, USA
*     Jason Riedy, Computer Science Division, University of
*       California at Berkeley, USA
 
 * dsyevr computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
*  selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
*
*  dsyevr first reduces the matrix A to tridiagonal form T with a call
*  to dsytrd.  Then, whenever possible, dsyevr calls dstemr to compute
*  the eigenspectrum using Relatively Robust Representations.  dstemr
*  computes eigenvalues by the dqds algorithm, while orthogonal
*  eigenvectors are computed from various "good" L D L^T representations
*  (also known as Relatively Robust Representations). Gram-Schmidt
*  orthogonalization is avoided as far as possible. More specifically,
*  the various steps of the algorithm are as follows.
*
*  For each unreduced block (submatrix) of T,
*     (a) Compute T - sigma I  = L D L^T, so that L and D
*         define all the wanted eigenvalues to high relative accuracy.
*         This means that small relative changes in the entries of D and L
*         cause only small relative changes in the eigenvalues and
*         eigenvectors. The standard (unfactored) representation of the
*         tridiagonal matrix T does not have this property in general.
*     (b) Compute the eigenvalues to suitable accuracy.
*         If the eigenvectors are desired, the algorithm attains full
*         accuracy of the computed eigenvalues only right before
*         the corresponding vectors have to be computed, see steps c) and d).
*     (c) For each cluster of close eigenvalues, select a new
*         shift close to the cluster, find a new factorization, and refine
*         the shifted eigenvalues to suitable accuracy.
*     (d) For each eigenvalue with a large enough relative separation compute
*         the corresponding eigenvector by forming a rank revealing twisted
*         factorization. Go back to (c) for any clusters that remain.
*
*  The desired accuracy of the output can be specified by the input
*  parameter abstol.
*
*  For more details, see dstemr's documentation and:
*  - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
*    to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
*    Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
*  - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
*    Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
*    2004.  Also LAPACK Working Note 154.
*  - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
*    tridiagonal eigenvalue/eigenvector problem",
*    Computer Science Division Technical Report No. UCB/CSD-97-971,
*    UC Berkeley, May 1997.
*
*
*  Note 1 : dsyevr calls dstemr when the full spectrum is requested
*  on machines which conform to the ieee-754 floating point standard.
*  dsyevr calls dstebz and dstein on non-ieee machines and
*  when partial spectrum requests are made.
*
*  Normal execution of dstemr may create NaNs and infinities and
*  hence may abort due to a floating point exception in environments
*  which do not handle NaNs and infinities in the ieee standard default
*  manner.
*  
*  @param jobz input char 
*         = 'N':  Compute eigenvalues only;
*         = 'V':  Compute eigenvalues and eigenvectors.
*  @param range input char
*         = 'A': all eigenvalues will be found.
*         = 'V': all eigenvalues in the half-open interval (VL,VU]
*                will be found.
*         = 'I': the IL-th through IU-th eigenvalues will be found.
********** For range = 'V' or 'I' and iu - il < n - 1, dstebz and
********** dstein are called
*  @param uplo input char
*         = 'U':  Upper triangle of A is stored;
*         = 'L':  Lower triangle of A is stored.
*  @param n input int The order of matrix A. n >= 0.
*  @param A (input/output) double[][] of dimension (lda, n)
*         On entry, the symmetric matrix A.  If uplo = 'U', the
*         leading n-by-n upper triangular part of A contains the
*         upper triangular part of the matrix A.  If uplo = 'L',
*         the leading n-by-n lower triangular part of A contains
*         the lower triangular part of the matrix A.
*         On exit, the lower triangle (if uplo='L') or the upper
*         triangle (if uplo='U') of A, including the diagonal, is
*         destroyed.
*  @param lda input int The leading dimension of array A.  lda >= max(1,n).
*  @param vl input double
*  @param vl input double
*         If range='V', the lower and upper bounds of the interval to be searched for eigenvalues. vl < vu.
*         Not referenced if range = 'A' or 'I'.
*  @param il input int
*  @param iu input int
*         If range ='I', the indices (in ascending order) of the
*          smallest and largest eigenvalues to be returned.
*          1 <= il <= iu <= n, if n > 0; il = 1 and iu = 0 if n = 0.
*          Not referenced if range = 'A' or 'V'.
* @param abstol input double
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  abstol + eps *   max( |a|,|b| ) ,
*
*          where eps is the machine precision.  If abstol is less than
*          or equal to zero, then  eps*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing A to tridiagonal form.
*
*          See "Computing Small Singular Values of Bidiagonal Matrices
*          with Guaranteed High Relative Accuracy," by Demmel and
*          Kahan, LAPACK Working Note #3.
*
*          If high relative accuracy is important, set abstolL to
*          dlamch( 'Safe minimum' ).  Doing so will guarantee that
*          eigenvalues are computed to high relative accuracy when
*          possible in future releases.  The current code does not
*          make any guarantees about high relative accuracy, but
*          future releases will. See J. Barlow and J. Demmel,
*          "Computing Accurate Eigensystems of Scaled Diagonally
*          Dominant Matrices", LAPACK Working Note #7, for a discussion
*          of which matrices define their eigenvalues to high relative
*          accuracy.
*  @param m output int  The total number of eigenvalues found.  0 <= m <= n.
*          If range = 'A', m = n, and if range = 'I', m = iu-il+1.
*  @param w output double[] of dimension n.
*          The first m elements contain the selected eigenvalues in ascending order.
*  @param Z output double[][] of dimension (ldz, max(1, m[0])
*          If jobz = 'V', then if info[0] = 0, the first m[0] columns of Z
*          contain the orthonormal eigenvectors of the matrix A
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with w[i].
*          If jobz = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,m[0]) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of m[0]
*          is not known in advance and an upper bound must be used.
*          Supplying n columns is always safe.
*  @param ldz input int The leading dimension of array Z.  ldz >= 1, and if 
*          jobz = 'V', ldz >= max(1, n)
*  @param isuppz ouput int[] of dimension (2*max(1,m[0])
*          The support of the eigenvectors in Z, i.e., the indices
*          indicating the nonzero elements in Z. The i-th eigenvector
*          is nonzero only in elements isuppz[2*i-2] through
*          isuppz[ 2*i -1].
********** Implemented only for range = 'A' or 'I' and iu - il = n - 1
*  @param work (workspace/output) double[] of dimension max(1, lwork)
*         On exit, if info[0] = 0, work[0] returns the optimal lwork.
*  @param lwork input int  The dimension of the array work.  lwork >= max(1,26*n).
*          For optimal efficiency, lwork >= (nb+6)*n,
*          where nb is the max of the blocksize for dsytrd and dormtr
*          returned by ilaenv.
*
*          If lwork = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the work array, returns
*          this value as the first entry of the work array, and no error
*          message related to lwork is issued.
* @param iwork output int[] of dimension (max(1, liwork)).
*          On exit, if info[0] = 0, iwork[0] returns the optimal liwork.
* @param liwork input int The dimension of the array iwork.  liwork >= max(1,10*n).
* @param info[0] output int[] of dimension 1.
*          = 0:  successful exit
*          < 0:  if info[0] = -i, the i-th argument had an illegal value
*          > 0:  Internal error
 */
 public void dsyevr(char jobz, char range, char uplo, int n, double A[][], int lda, double vl, double vu, int il, int iu,
                    double abstol, int m[], double w[], double Z[][], int ldz, int isuppz[], double work[], int lwork,
                    int iwork[], int liwork, int info[]) {
 int i,j;
 int ieeeok;
 int ifail[];
 int iinfo[] = new int[1];
 int imax;
 int indtau;
 int indd;
 int inde;
 int inddd;
 int indee;
 int indwk;
 int indwkn;
 int indibl;
 int indisp;
 int indifl;
 int indiwo;
 int iscale;
 int ivec2[];
 int ivec3[];
 int jj;
 int k;
 int lwmin;
 int liwmin;
 int llwork;
 int llwrkn;
 int lwkopt = 0;
 int nb;
 int nsplit[] = new int[1];
 double abstll;
 double anrm;
 double eps;
 double smlnum;
 double bignum;
 double rmin;
 double rmax;
 double sigma = 0.0;
 double tmp1;
 double tmp2;
 double vec1[];
 double vec3[];
 double vecindd[];
 double vecinde[];
 double vecinddd[];
 double vecindwk[];
 double vecindee[];
 double vll = 0.0;
 double vuu = 0.0;
 boolean doHere = true;
 boolean lower;
 boolean wantz;
 boolean alleig;
 boolean valeig;
 boolean indeig;
 boolean lquery;
 boolean tryac;
 char order;
 char ch[] = new char[1];
 String opts;
 
 ch[0] = uplo;
 opts = new String(ch);
 // Test the input parameters.

 ieeeok = ge.ilaenv( 10, "DSYEVR", "N", 1, 2, 3, 4 );

 lower = ((uplo == 'L') || (uplo == 'l'));
 wantz = ((jobz == 'V') || (jobz == 'v'));
 alleig = ((range == 'A') || (range == 'a'));
 valeig = ((range == 'V') || (range == 'v'));
 indeig = ((range == 'I') || (range == 'i'));

 lquery = ( (lwork == -1 ) || (liwork == -1) );

 lwmin = Math.max(1, 26*n);
 liwmin = Math.max(1, 10*n);

 info[0] = 0;
 if (!(wantz || ((jobz == 'N') || (jobz == 'n')))) {
    info[0] = -1;
 }
 else if(!(alleig || valeig || indeig)) {
    info[0] = -2;
 }
 else if (!(lower || ((uplo == 'U') || (uplo == 'u')))) {
    info[0] = -3;
 }
 else if(n < 0) {
    info[0] = -4;
 }
 else if (lda < Math.max(1, n)) {
    info[0] = -6;
 }
 else {
    if (valeig) {
       if (n > 0 && vu <= vl) {
          info[0] = -8;
       }
    }
    else if (indeig) {
       if (il < 1 || il > Math.max(1, n)) {
          info[0] = -9;
       }
       else if (iu < Math.min(n, il) || iu > n) {
          info[0] = -10;
       }
    }
 }
 if (info[0] == 0) {
    if (ldz < 1 || (wantz && ldz < n)) {
       info[0] = -15;
    }
    else if (lwork < lwmin && !lquery) {
       info[0] = -18;
    }
    else if (liwork < liwmin && !lquery) {
       info[0] = -20;
    }
 }

 if (info[0] == 0 ) {
    nb = ge.ilaenv( 1, "DSYTRD", opts, n, -1, -1, -1 );
    nb = Math.max(nb, ge.ilaenv( 1, "DORMTR", opts, n, -1, -1, -1 ) );
    lwkopt = Math.max((nb+1 )*n, lwmin);
    work[0] = lwkopt;
    iwork[0] = liwmin;
 }

 if (info[0] != 0) {
    MipavUtil.displayError("Error! dsyevr had info[0] = " + info[0]);
    return;
 }
 else if (lquery) {
    return;
 }

 // Quick return if possible

 m[0] = 0;
 if (n == 0) {
    work[0] = 1;
    return;
 }

 if (n == 1 ) {
    work[0] = 7;
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
       isuppz[0] = 1;
       isuppz[1] = 1;
    }
    return;
 } // if (n == 1)
 
//Get machine constants.

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
            vec1[i] = A[i][j-1];
        }
        ge.dscal(j, sigma, vec1, 1 );
        for (i = 0; i < j; i++) {
            A[i][j-1] = vec1[i];
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

//     Initialize indices into workspaces.  Note: The IWORK indices are
//     used only if DSTERF or DSTEMR fail.

//     WORK(INDTAU:INDTAU+N-1) stores the scalar factors of the
//     elementary reflectors used in DSYTRD.
 indtau = 1;
//     WORK(INDD:INDD+N-1) stores the tridiagonal's diagonal entries.
 indd = indtau + n;
//     WORK(INDE:INDE+N-1) stores the off-diagonal entries of the
//     tridiagonal matrix from DSYTRD.
 inde = indd + n;
//     WORK(INDDD:INDDD+N-1) is a copy of the diagonal entries over
//     -written by DSTEMR (the DSTERF path copies the diagonal to W).
 inddd = inde + n;
//     WORK(INDEE:INDEE+N-1) is a copy of the off-diagonal entries over
//     -written while computing the eigenvalues in DSTERF and DSTEMR.
 indee = inddd + n;
//     INDWK is the starting offset of the left-over workspace, and
//     LLWORK is the remaining workspace size.
 indwk = indee + n;
 llwork = lwork - indwk + 1;

//     IWORK(INDIBL:INDIBL+M-1) corresponds to IBLOCK in DSTEBZ and
//     stores the block indices of each of the M<=N eigenvalues.
 indibl = 1;
//     IWORK(INDISP:INDISP+NSPLIT-1) corresponds to ISPLIT in DSTEBZ and
//     stores the starting and finishing indices of each block.
 indisp = indibl + n;
//     IWORK(INDIFL:INDIFL+N-1) stores the indices of eigenvectors
//     that corresponding to eigenvectors that fail to converge in
//     DSTEIN.  This information is discarded; if any fail, the driver
//     returns INFO > 0.
 indifl = indisp + n;
//     INDIWO is the offset of the remaining integer workspace.
 indiwo = indisp + n;


//     Call DSYTRD to reduce symmetric matrix to tridiagonal form.

 vecindd = new double[n];
 vecinde = new double[n];
 vecinddd = new double[n];
 vecindwk = new double[2*n];
 vecindee = new double[n];
 ge.dsytrd(uplo, n, A, lda, vecindd, vecinde, work, vecindwk, llwork, iinfo);

// If all eigenvalues are desired then call dsterf or dstemr and dormtr.

 if((alleig || (indeig && il == 1 && iu == n)) && ieeeok == 1) {
    if (!wantz) {
       for (i = 0; i < n; i++) {
           w[i] = vecindd[i];
       }
       for (i = 0; i < n-1; i++) {
           vecindee[i] = vecinde[i];
       }
       ge.dsterf( n, w, vecindee, info); 
    }
    else {
       for (i = 0; i < n-1; i++) {
           vecindee[i] = vecinde[i];
       }
       for ( i = 0; i < n; i++) {
           vecinddd[i] = vecindd[i];
       }
       
       if (abstol <= 2.0*n*eps) {
          tryac = true;
       }
       else {
          tryac = false;
       }
       /*CALL DSTEMR( JOBZ, 'A', N, WORK( INDDD ), WORK( INDEE ),
$                   VL, VU, IL, IU, M, W, Z, LDZ, N, ISUPPZ,
$                   TRYRAC, WORK( INDWK ), LWORK, IWORK, LIWORK,
$                   INFO )*/



       // Apply orthogonal matrix used in reduction to tridiagonal form to eigenvectors returned by dstein.

       if (wantz && info[0] == 0) {
          indwkn = inde;
          llwrkn = lwork - indwkn + 1;
          se.dormtr( 'L', uplo, 'N', n, m[0], A, lda, work, Z, ldz, vecinde, llwrkn, iinfo);
       }
    }


    if (info[0] == 0) {
           //Everything worked.  Skip dstebz/dstein.  iwork(:) are undefined.
       m[0] = n;
       doHere = false;
    }
    info[0] = 0;
 } // if((alleig || (indeig && il == 1 && iu == n)) && ieeeok == 1)

 if (doHere) {
          // Otherwise, call dstebz and, if eigenvectors are desired, dstein.
          // Also call dstebz and dstein if dstemr fails.
    
     if (wantz) {
        order = 'B';
     }
     else {
        order = 'E';
     }
     
     ivec2 = new int[n];
     vec3 = new double[5*n];
     ivec3 = new int[3*n];
     se.dstebz(range, order, n, vll, vuu, il, iu, abstll,
            vecindd, vecinde, m, nsplit, w,
            iwork, ivec2, vec3, ivec3, info);
     if (wantz) {
         ifail = new int[m[0]];
         se.dstein(n, vecindd, vecinde, m[0], w, iwork, ivec2, Z, ldz,
               vec3, ivec3, ifail, info);
 
         // Apply orthogonal matrix used in reduction to tridiagonal
         // form to eigenvectors returned by DSTEIN.
 
        indwkn = inde;
        llwrkn = lwork - indwkn + 1;
        se.dormtr( 'L', uplo, 'N', n, m[0], A, lda, work, Z, ldz, vecinde, llwrkn, iinfo);
     } // if (wantz)
 } // if (doHere)

   // If matrix was scaled, then rescale eigenvalues appropriately.

   // Jump here if dstemr/dstein succeeded.
 if (iscale == 1) {
    if (info[0] == 0) {
       imax = m[0];
    }
    else {
       imax = info[0] - 1;
    }
    for (i = 0; i < imax; i++) {
        w[i] *= 1.0/sigma;
    }
 } // if (iscale == 1)

      // If eigenvalues are not in order, then sort them, along with
      // eigenvectors.  Note: We do not sort the ifail portion of iwork.
      // It may not be initialized (if dstemr/dstein succeeded), and we do
      // not return this detailed information to the user.
 
 if (wantz) {
    for (j = 1; j <= m[0] - 1; j++) {
       i = 0;
       tmp1 = w[j-1];
       for (jj = j + 1; jj <= m[0]; jj++) {
          if (w[jj-1] < tmp1) {
             i = jj;
             tmp1 = w[jj-1];
          }
       } // for (jj = j + 1; jj <= m[0]; jj++)

       if (i != 0) {
          w[i-1] = w[j-1];
          w[j-1] = tmp1;
          for (k = 0; k < n; k++) {
              tmp2 = Z[k][i-1];
              Z[k][i-1] = Z[k][j-1];
              Z[k][j-1] = tmp2;
          }
       }
    } // for (j = 1; j <= m[0] - 1; j++)
 } // if (wantz)

     // Set WORK(1) to optimal workspace size.

 work[0] = lwkopt;
 iwork[0] = liwmin;

 return;
} // dsyevr


 
 
  
  
  
 
  
  
  
 
  
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
           ge.dlasrt('D', n, z, iinfo);
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
       
       ge.dlasrt('D', n, z, iinfo);
       
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
  
 
  
  
  
  /** This is a port of version 3.4.0 LAPACK routine dstemr.   LAPACK is a software package provided by Univ. of Tennessee,    --
     -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd.  November 2011
     Contributors:
  
     Beresford Parlett, University of California, Berkeley, USA \n
     Jim Demmel, University of California, Berkeley, USA \n
     Inderjit Dhillon, University of Texas, Austin, USA \n
     Osni Marques, LBNL/NERSC, USA \n
     Christof Voemel, University of California, Berkeley, USA
  
     dstemr computes selected eigenvalues and, optionally, eigenvectors
     of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
     a well defined set of pairwise different real eigenvalues, the corresponding
     real eigenvectors are pairwise orthogonal.
    
     The spectrum may be computed either completely or partially by specifying
     either an interval (vl,vu] or a range of indices il:iu for the desired
     eigenvalues.
    
     Depending on the number of desired eigenvalues, these are computed either
     by bisection or the dqds algorithm. Numerically orthogonal eigenvectors are
     computed by the use of various suitable L D L^T factorizations near clusters
     of close eigenvalues (referred to as RRRs, Relatively Robust
     Representations). An informal sketch of the algorithm follows.
    
     For each unreduced block (submatrix) of T,
        (a) Compute T - sigma I  = L D L^T, so that L and D
            define all the wanted eigenvalues to high relative accuracy.
            This means that small relative changes in the entries of D and L
            cause only small relative changes in the eigenvalues and
            eigenvectors. The standard (unfactored) representation of the
            tridiagonal matrix T does not have this property in general.
        (b) Compute the eigenvalues to suitable accuracy.
            If the eigenvectors are desired, the algorithm attains full
            accuracy of the computed eigenvalues only right before
            the corresponding vectors have to be computed, see steps c) and d).
        (c) For each cluster of close eigenvalues, select a new
            shift close to the cluster, find a new factorization, and refine
            the shifted eigenvalues to suitable accuracy.
        (d) For each eigenvalue with a large enough relative separation compute
            the corresponding eigenvector by forming a rank revealing twisted
            factorization. Go back to (c) for any clusters that remain.
    
     For more details, see:
     - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
       to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
       Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
     - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
       Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
       2004.  Also LAPACK Working Note 154.
     - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
       tridiagonal eigenvalue/eigenvector problem",
       Computer Science Division Technical Report No. UCB/CSD-97-971,
       UC Berkeley, May 1997.
    
     Further Details
     1.dstemr works only on machines which follow IEEE-754
     floating-point standard in their handling of infinities and NaNs.
     This permits the use of efficient inner loops avoiding a check for
     zero divisors.
  @param jobz input char
         = 'N':  Compute eigenvalues only;
         = 'V':  Compute eigenvalues and eigenvectors. 
  @param range input char
         = 'A': all eigenvalues will be found.
         = 'V': all eigenvalues in the half-open interval (vl,vu] will be found.
         = 'I': the il-th through iu-th eigenvalues will be found.
  @param n input int  The order of the matrix.  n >= 0.
  @param d input double[] of dimension n.
         On entry, the n diagonal elements of the tridiagonal matrix T. On exit, d is overwritten.
  @param e input/workspace double[] of dimension n.
          On entry, the (n-1) subdiagonal elements of the tridiagonal
          matrix T in elements 0 to n-2 of e. e[n-1] need not be set on input, but is used internally as workspace.
          On exit, e is overwritten.
  @param vl input double
  @param vu input double
         If range = 'V', the lower and upper bounds of the interval to be searched for eigenvalues. vl < vu.
         Not referenced if RANGE = 'A' or 'I'.
  @param il input int
  @param iu input int
         If range = 'I', the indices (in ascending order) of the smallest and largest eigenvalues to be returned.
         1 <= il <= iu <= n, if n > 0.
         Not referenced if range = 'A' or 'V'.
  @param m output int[] of dimension 1.
         The total number of eigenvalues found.  0 <= m[0] <= n.
         If range = 'A', m[0] = n, and if range = 'I', m[0] = iu-il+1. 
  @param w output double[] of dimension n
         The first m[0] elements contain the selected eigenvalues in ascending order.
  @param Z[][] output double[][] of dimension (ldz, max(1, m[0])
         If jobz = 'V', and if info[0] = 0, then the first m[0] columns of Z
         contain the orthonormal eigenvectors of the matrix T
         corresponding to the selected eigenvalues, with the i-th
         column of Z holding the eigenvector associated with w[i].
         If jobz = 'N', then Z is not referenced.
         Note: the user must ensure that at least max(1,m[0]) columns are
         supplied in the array Z; if range = 'V', the exact value of m[0]
         is not known in advance and can be computed with a workspace
         query by setting nzc = -1, see below.
   @param ldz input int  The leading dimension of the array Z.  ldz >= 1, and if
         jobz = 'V', then ldz >= max(1,n).
   @param nzc input int  The number of eigenvectors to be held in the array Z.
         If range = 'A', then nzc >= max(1,n).
         If range = 'V', then nzc >= the number of eigenvalues in (vl,vu].
         If range = 'I', then nzc >= iu-il+1.
         If nzc = -1, then a workspace query is assumed; the routine calculates the number of columns
         of the array Z that are needed to hold the eigenvectors.
         This value is returned as the first entry of the Z array, and no error message related to nzc is issued.
   @param isuppz output int[] of dimension (2*max(1,m[0]))
         The support of the eigenvectors in Z, i.e., the indices indicating the nonzero elements in Z.
         The i-th computed eigenvector is nonzero only in elements isuppz[2*i-2] through
         isuppz[2*i-1]. This is relevant in the case when the matrix  is split. isuppz is only accessed
         when jobz is 'V' and n > 0.
   @param tryac (input/output) boolean[] of dimension 1.
         If tryac[0] == true, indicates that the code should check whether  the tridiagonal matrix defines
         its eigenvalues to high relative accuracy.  If so, the code uses relative-accuracy preserving
         algorithms that might be (a bit) slower depending on the matrix.  If the matrix does not define its
         eigenvalues to high relative accuracy, the code can uses possibly faster algorithms.
         If tryac[0] == false, the code is not required to guarantee relatively accurate eigenvalues and can
         use the fastest possible techniques.
         On exit, a true tryac[0] will be set to false if the matrix does not define its eigenvalues to
         high relative accuracy.
   @param work (workspace/output) double[] of dimension lwork.
         On exit, if info[0] = 0, work[0] returns the optimal (and minimal) lwork.
   @param lwork input int  The dimension of the array work. 
          lwork >= max(1,18*n) if jobz = 'V', 
          and lwork >= max(1,12*n) if jobz = 'N'.
          If lwork = -1, then a workspace query is assumed; the routine only calculates the optimal size of
          the work array, returns this value as the first entry of the work array, and no error message
          related to lwork is issued.
   @param iwork (workspace/output) int[] of dimension liwork.
          On exit, if info[0] = 0, iwork[0] returns the optimal liwork.
   @param liwork input int  The dimension of the array iwork.  
          liwork >= max(1,10*n) if the eigenvectors are desired, 
          and liwork >= max(1,8*n) if only the eigenvalues are to be computed.
          If liwork = -1, then a workspace query is assumed; the routine only calculates the optimal
          size of the iwork array returns this value as the first entry of the iwork array, and
          no error message related to liwork is issued.
   @param info output int of dimension 1.
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
           > 0:  if info[0] = 1X, internal error in dlarre,
                 if info[0] = 2X, internal error in dlarrv.
                 Here, the digit X = abs(iinfo[0]) < 10, where iinfo[0] is the nonzero error code
                 returned by dlarre or dlarrv, respectively.
   */
  private void dstemr(char jobz, char range, int n, double d[], double e[], double vl, double vu, int il, int iu,
                      int m[], double w[], double Z[][], int ldz, int nzc, int isuppz[], boolean tryac[],
                      double work[], int lwork, int iwork[], int liwork, int info[]) {
  
  double minrgp = 1.0E-3;
  double wl[] = new double[1];
  double wu[] = new double[1];
  double safmin;
  double eps;
  double smlnum;
  double bignum;
  double rmin;
  double rmax;
  double r1[] = new double[1];
  double r2[] = new double[1];
  double cs[] = new double[1];
  double sn[] = new double[1];
  double pivmin[] = new double[1];
  double scale;
  double tnrm;
  double thresh;
  double workindd[] = new double[n];
  double workinde2[] = new double[n];
  double workinderr[] = new double[n];
  double workindgp[] = new double[n];
  double workindwrk[] = new double[6*n];
  double rtol1;
  double rtol2;
  boolean wantz;
  boolean alleig;
  boolean valeig;
  boolean indeig;
  boolean lquery;
  boolean zquery;
  int i;
  int j;
  int iil;
  int iiu;
  int itmp[] = new int[1];
  int itmp2[] = new int[1];
  int lwmin;
  int liwmin;
  int nzcmin[] = new int[1];
  int indgrs;
  int inderr;
  int indgp;
  int indd;
  int inde2;
  int indwrk;
  int iinspl;
  int iindbl;
  int iindw;
  int iindwk;
  int iinfo[] = new int[1];
  int nsplit[] = new int[1];
  int iworkiindbl[] = new int[n];
  int iworkiindw[] = new int[n];
  int iworkiindwk[] = new int[5*n];
       // Test the input parameters.
  
        wantz = ((jobz == 'V') || (jobz == 'v'));
        alleig = ((range == 'A') || (range == 'a'));
        valeig = ((range == 'V') || (range == 'v'));
        indeig = ((range == 'I') || (range == 'i'));
  
        lquery = ((lwork == -1 ) || (liwork == -1 ));
        zquery = (nzc == -1);

        // dstemr needs work of size 6*n, iwork of size 3*n.
        // In addition, dlarre needs work of size 6*n, iwork of size 5*n.
        // Furthermore, dlarrv needs work of size 12*n, iwork of size 7*n.
        if (wantz) {
           lwmin = 18*n;
           liwmin = 10*n;
        }
        else {
           // need less workspace if only the eigenvalues are wanted
           lwmin = 12*n;
           liwmin = 8*n;
        }

        wl[0] = 0.0;
        wu[0] = 0.0;
        iil = 0;
        iiu = 0;

        if (valeig) {
           // We do not reference vl, vu in the cases range = 'I','A'
           // The interval (wl, wu] contains all the wanted eigenvalues.
           // It is either given by the user or computed in dlarre.
           wl[0] = vl;
           wu[0] = vu;
        }
        else if (indeig) {
           // We do not reference il, iu in the cases range = 'V','A'
           iil = il;
           iiu = iu;
        }
  
        info[0] = 0;
        if (!(wantz || ((jobz == 'N') || (jobz == 'n')))) {
           info[0] = -1;
        }
        else if (!(alleig || valeig || indeig)) {
           info[0] = -2;
        }
        else if (n < 0) {
           info[0] = -3;
        }
        else if (valeig && n > 0 && wu[0] <= wl[0]) {
           info[0] = -7;
        }
        else if (indeig && (iil < 1 || iil > n)) {
           info[0] = -8;
        }
        else if (indeig && (iiu < iil || iiu > n)) {
           info[0] = -9;
        }
        else if (ldz < 1 || (wantz && ldz < n)) {
           info[0] = -13;
        }
        else if (lwork < lwmin && !lquery) {
           info[0] = -17;
        }
        else if (liwork < liwmin && !lquery) {
           info[0] = -19;
        }
  
        // Get machine constants.
  
        safmin = ge.dlamch('S');
        eps = ge.dlamch('P');
        smlnum = safmin/eps;
        bignum = 1.0/ smlnum;
        rmin = Math.sqrt(smlnum);
        rmax = Math.min(Math.sqrt(bignum), 1.0/Math.sqrt(Math.sqrt(safmin)));
  
        if (info[0] == 0 ) {
           work[0] = lwmin;
           iwork[0] = liwmin;
  
           if (wantz && alleig) {
              nzcmin[0] = n;
           }
           else if (wantz && valeig) {
              dlarrc( 'T', n, vl, vu, d, e, safmin, nzcmin, itmp, itmp2, info);
           }
           else if (wantz && indeig) {
              nzcmin[0] = iiu-iil+1;
           }
           else {
             // wantz == false;
              nzcmin[0] = 0;
           }
           if (zquery && info[0] == 0) {
              Z[0][0] = nzcmin[0];
           }
           else if (nzc < nzcmin[0] && !zquery) {
              info[0] = -14;
           }
        } // if (info[0] == 0)

        if (info[0] != 0) {
  
           MipavUtil.displayError("dstemr had info[0] = " + info[0]);
  
           return;
        }
        else if (lquery || zquery) {
           return;
        }
  
        // Handle n = 0, 1, and 2 cases immediately
  
        m[0] = 0;
        if (n == 0) {
            return;
        }
        
        if (n == 1) {
           if (alleig || indeig) {
              m[0] = 1;
              w[0] = d[0];
           }
           else {
              if (wl[0] < d[0] && wu[0] >= d[0]) {
                 m[0] = 1;
                 w[0] = d[0];
              }
           }
           if (wantz && (!zquery)) {
              Z[0][0] = 1.0;
              isuppz[0] = 1;
              isuppz[1] = 1;
           }
           return;
        } // if (n == 1)
  
        if (n == 2) {
           if (!wantz) {
              ge.dlae2(d[0], e[0], d[1], r1, r2);
           }
           else if (wantz && (!zquery)) {
              ge.dlaev2(d[0], e[0], d[1], r1, r2, cs, sn);
           }
           if (alleig ||
              (valeig && (r2[0] > wl[0]) &&
                         (r2[0] <= wu[0])) ||
              (indeig && (iil == 1)) ) {
              m[0] = m[0]+1;
              w[m[0]-1] = r2[0];
              if (wantz && (!zquery)) {
                 Z[0][m[0]-1] = -sn[0];
                 Z[1][m[0]-1] = cs[0];
                 // Note: At most one of SN and CS can be zero.
                 if (sn[0] != 0.0) {
                    if (cs[0] != 0.0) {
                       isuppz[2*m[0]-2] = 1;
                       isuppz[2*m[0]-1] = 2;
                    }
                    else {
                       isuppz[2*m[0]-2] = 1;
                       isuppz[2*m[0]-1] = 1;
                    }
                 } // if (sn[0] != 0.0)
                 else {
                    isuppz[2*m[0]-2] = 2;
                    isuppz[2*m[0]-1] = 2;
                 }
              } // if (wantz && (!zquery))
           } // if (alleig ||
           if (alleig ||
              (valeig && (r1[0] > wl[0]) &&
                         (r1[0] <= wu[0])) ||
              (indeig && (iiu == 2))) {
              m[0] = m[0]+1;
              w[m[0]-1] = r1[0];
              if (wantz && (!zquery)) {
                 Z[0][m[0]-1] = cs[0];
                 Z[1][m[0]-1] = sn[0];
                 // Note: At most one of sn[0] and cs[0] can be zero.
                 if (sn[0] != 0.0) {
                    if (cs[0] != 0.0) {
                       isuppz[2*m[0]-2] = 1;
                       isuppz[2*m[0]-1] = 2;
                    }
                    else {
                       isuppz[2*m[0]-2] = 1;
                       isuppz[2*m[0]-1] = 1;
                    }
                 } //  if (sn[0] != 0.0)
                 else {
                    isuppz[2*m[0]-2] = 2;
                    isuppz[2*m[0]-1] = 2;
                 }
              } // if (wantz && (!zquery)) 
           } // if (alleig ||
           return;
        } // if (n == 2)

        // Continue with general n

        indgrs = 1;
        inderr = 2*n + 1;
        indgp = 3*n + 1;
        indd = 4*n + 1;
        inde2 = 5*n + 1;
        indwrk = 6*n + 1;
  
        iinspl = 1;
        iindbl = n + 1;
        iindw = 2*n + 1;
        iindwk = 3*n + 1;
  
        //  Scale matrix to allowable range, if necessary.
        // The allowable range is related to the pivmin parameter; see the
        // comments in dlarrd.  The preference for scaling small values
        // up is heuristic; we expect users' matrices not to be close to the
        // rmax threshold.
   
        scale = 1.0;
        tnrm = ge.dlanst('M', n, d, e);
        if (tnrm > 0.0 && tnrm < rmin) {
           scale = rmin / tnrm;
        }
        else if (tnrm > rmax) {
           scale = rmax / tnrm;
        }
        if (scale != 1.0) {
           for (i = 0; i < n; i++) {
               d[i] *= scale;
           }
           for (i = 0; i < n-1; i++) {
               e[i] *= scale;
           }
           tnrm = tnrm*scale;
           if (valeig) {
              // If eigenvalues in interval have to be found,
              // scale (wl, wu] accordingly
              wl[0] = wl[0]*scale;
              wu[0] = wu[0]*scale;
           } // if (valeig)
        } // if (scale != 1.0)
  
        // Compute the desired eigenvalues of the tridiagonal after splitting
        // into smaller subblocks if the corresponding off-diagonal elements
        // are small
        // thresh is the splitting parameter for dlarre
        // A negative thresh forces the old splitting criterion based on the
        // size of the off-diagonal. A positive thresh switches to splitting
        // which preserves relative accuracy.
   
        if (tryac[0]) {
           // Test whether the matrix warrants the more expensive relative approach.
           dlarrr(n, d, e, iinfo);
        }
        else {
           // The user does not care about relative accurately eigenvalues
           iinfo[0] = -1;
        }
        // Set the splitting criterion
        if (iinfo[0] == 0) {
           thresh = eps;
        }
        else {
           thresh = -eps;
           //  relative accuracy is desired but T does not guarantee it
           tryac[0] = false;
        }
  
        if (tryac[0]) {
           // Copy original diagonal, needed to guarantee relative accuracy
           for (i = 0; i < n; i++) {
               workindd[i] = d[i];
           }
        }
        // Store the squares of the offdiagonal values of T
        for (j = 1; j <= n-1; j++) {
           workinde2[j-1] = e[j-1]*e[j-1];
        }

        // Set the tolerance parameters for bisection
        if (!wantz) {
           // DLARRE computes the eigenvalues to full precision.
           rtol1 = 4.0 * eps;
           rtol2 = 4.0 * eps;
        }
        else {
           // dlarre computes the eigenvalues to less than full precision.
           // dlarrv will refine the eigenvalue approximations, and we can
           // need less accurate initial bisection in dlarre.
           // Note: these settings do only affect the subset case and DLARRE
           rtol1 = Math.sqrt(eps);
           rtol2 = Math.max(Math.sqrt(eps)*5.0E-3, 4.0 * eps);
        }
        dlarre(range, n, wl, wu, iil, iiu, d, e, workinde2, rtol1, rtol2, thresh, nsplit,
               iwork, m, w, workinderr, workindgp, iworkiindbl, iworkiindw, work, pivmin,
               workindwrk, iworkiindwk, iinfo);
        if (iinfo[0] != 0) {
           info[0] = 10 + Math.abs(iinfo[0]);
           return;
        }
        // Note that if range .NE. 'V', dlarre computes bounds on the desired
        // part of the spectrum. All desired eigenvalues are contained in
        // (wl[0], wu[0]]


        /*if (wantz) {
   
           //Compute the desired eigenvectors corresponding to the computed eigenvalues
   
           CALL DLARRV( N, WL, WU, D, E,
       $                PIVMIN, IWORK( IINSPL ), M,
       $                1, M, MINRGP, RTOL1, RTOL2,
       $                W, WORK( INDERR ), WORK( INDGP ), IWORK( IINDBL ),
       $                IWORK( IINDW ), WORK( INDGRS ), Z, LDZ,
       $                ISUPPZ, WORK( INDWRK ), IWORK( IINDWK ), IINFO )
           IF( IINFO.NE.0 ) THEN
              INFO = 20 + ABS( IINFO )
              RETURN
           END IF
        } // if (wantz)
        else {
  *        DLARRE computes eigenvalues of the (shifted) root representation
  *        DLARRV returns the eigenvalues of the unshifted matrix.
  *        However, if the eigenvectors are not desired by the user, we need
  *        to apply the corresponding shifts from DLARRE to obtain the
  *        eigenvalues of the original matrix.
           DO 20 J = 1, M
              ITMP = IWORK( IINDBL+J-1 )
              W( J ) = W( J ) + E( IWORK( IINSPL+ITMP-1 ) )
   20      CONTINUE
        }
  *

        IF ( TRYRAC ) THEN
  *        Refine computed eigenvalues so that they are relatively accurate
  *        with respect to the original matrix T.
           IBEGIN = 1
           WBEGIN = 1
           DO 39  JBLK = 1, IWORK( IINDBL+M-1 )
              IEND = IWORK( IINSPL+JBLK-1 )
              IN = IEND - IBEGIN + 1
              WEND = WBEGIN - 1
  *           check if any eigenvalues have to be refined in this block
   36         CONTINUE
              IF( WEND.LT.M ) THEN
                 IF( IWORK( IINDBL+WEND ).EQ.JBLK ) THEN
                    WEND = WEND + 1
                    GO TO 36
                 END IF
              END IF
              IF( WEND.LT.WBEGIN ) THEN
                 IBEGIN = IEND + 1
                 GO TO 39
              END IF

              OFFSET = IWORK(IINDW+WBEGIN-1)-1
              IFIRST = IWORK(IINDW+WBEGIN-1)
              ILAST = IWORK(IINDW+WEND-1)
              RTOL2 = FOUR * EPS
              CALL DLARRJ( IN,
       $                   WORK(INDD+IBEGIN-1), WORK(INDE2+IBEGIN-1),
       $                   IFIRST, ILAST, RTOL2, OFFSET, W(WBEGIN),
       $                   WORK( INDERR+WBEGIN-1 ),
       $                   WORK( INDWRK ), IWORK( IINDWK ), PIVMIN,
       $                   TNRM, IINFO )
              IBEGIN = IEND + 1
              WBEGIN = WEND + 1
   39      CONTINUE
        ENDIF
  *
  *     If matrix was scaled, then rescale eigenvalues appropriately.
  *
        IF( SCALE.NE.ONE ) THEN
           CALL DSCAL( M, ONE / SCALE, W, 1 )
        END IF
  *
  *     If eigenvalues are not in increasing order, then sort them,
  *     possibly along with eigenvectors.
  *
        IF( NSPLIT.GT.1 ) THEN
           IF( .NOT. WANTZ ) THEN
              CALL DLASRT( 'I', M, W, IINFO )
              IF( IINFO.NE.0 ) THEN
                 INFO = 3
                 RETURN
              END IF
           ELSE
              DO 60 J = 1, M - 1
                 I = 0
                 TMP = W( J )
                 DO 50 JJ = J + 1, M
                    IF( W( JJ ).LT.TMP ) THEN
                       I = JJ
                       TMP = W( JJ )
                    END IF
   50            CONTINUE
                 IF( I.NE.0 ) THEN
                    W( I ) = W( J )
                    W( J ) = TMP
                    IF( WANTZ ) THEN
                       CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
                       ITMP = ISUPPZ( 2*I-1 )
                       ISUPPZ( 2*I-1 ) = ISUPPZ( 2*J-1 )
                       ISUPPZ( 2*J-1 ) = ITMP
                       ITMP = ISUPPZ( 2*I )
                       ISUPPZ( 2*I ) = ISUPPZ( 2*J )
                       ISUPPZ( 2*J ) = ITMP
                    END IF
                 END IF
   60         CONTINUE
           END IF
        ENDIF
  *
  *
        WORK( 1 ) = LWMIN
        IWORK( 1 ) = LIWMIN*/
        return;
  } // dstemr
  
  


  /** This is a port of version 3.4.0 LAPACK auxiliary routine dlarra.  LAPACK is a software package provided by Univ. of Tennessee,    --
  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. November 2011
  Contributors:
 
  Beresford Parlett, University of California, Berkeley, USA
  Jim Demmel, University of California, Berkeley, USA
  Inderjit Dhillon, University of Texas, Austin, USA
  Osni Marques, LBNL/NERSC, USA
  Christof Voemel, University of California, Berkeley, USA
  
  Compute the splitting points with threshold spltol.
  dlarra sets any "small" off-diagonal elements to zero.
  
  @param n input int  The order of the matrix.  n > 0.
  @param d input double[] of dimension n.
         On entry, the n diagonal elements of the tridiagonal matrix T.
  @param e (input/output) double[] of dimension n.
         On entry, the first (n-1) entries contain the subdiagonal
         elements of the tridiagonal matrix T; e[n-1] need not be set.
         On exit, the entries e[isplit[i-1]-1], 1 <= i <= nsplit[0],
         are set to zero, the other entries of e are untouched.
  @param e2 (input/output) double[] of dimension n.
         On entry, the first (n-1) entries contain the SQUARES of the
         subdiagonal elements of the tridiagonal matrix T;
         e2[n-1] need not be set.
         On exit, the entries e2[isplit[i-1]-1],
         1 <= i <= nsplit[0], have been set to zero
  @param spltol input double
         The threshold for splitting. Two criteria can be used:
         spltol < 0 : criterion based on absolute off-diagonal value
         spltol > 0 : criterion that preserves relative accuracy
  @param tnrm input double  The norm of the matrix.
  @param nsplit output int[] of dimension 1.
         The number of blocks T splits into.  1 <= nsplit[0] <= n.
  @param isplit output int[] of dimension n.
         The splitting points, at which T breaks up into blocks.
         The first block consists of rows/columns 1 to isplit[0],
         the second of rows/columns isplit[0]+1 through isplit[1],
         etc., and the NSPLIT-th consists of rows/columns
         isplit[nsplit[0]-2]+1 through isplit[nsplit[0]-1]=n.
  @param info output int[] of dimension 1.
         info[0] = 0: successful exit.
  */
  private void dlarra(int n, double d[], double e[], double e2[], double spltol,
                      double tnrm, int nsplit[], int isplit[], int info[]) {
          int i;
          double eabs;
          double tmp1;
          
          info[0] = 0;
    
          // Compute splitting points
          nsplit[0] = 1;
          if (spltol < 0.0) {
             // Criterion based on absolute off-diagonal value
             tmp1 = Math.abs(spltol)* tnrm;
             for (i = 1; i <= n-1; i++) {
                eabs = Math.abs(e[i-1]);
                if (eabs <= tmp1) {
                   e[i-1] = 0.0;
                   e2[i-1] = 0.0;
                   isplit[nsplit[0]-1] = i;
                   nsplit[0] = nsplit[0] + 1;
                } // if (eabs <= tmp1)
             } // for (i = 1; i <= n-1; i++)
          } // if (spltol < 0.0)
          else { // spltol > 0.0
             // Criterion that guarantees relative accuracy
             for (i = 1; i <= n-1; i++) {
                eabs = Math.abs(e[i-1]);
                if (eabs <= spltol * Math.sqrt(Math.abs(d[i-1]))*Math.sqrt(Math.abs(d[i]))) {
                   e[i-1] = 0.0;
                   e2[i-1] = 0.0;
                   isplit[nsplit[0]-1] = i;
                   nsplit[0] = nsplit[0] + 1;
                }
             } // for (i = 1; i <= n-1; i++)
          } // else spltol > 0.0
          isplit[nsplit[0]-1] = n;
    
          return;
  } // dlarra
  
  
  /** This is a port of version 3.4.0 LAPACK auxiliary routine dlarrb.  LAPACK is a software package provided by Univ. of Tennessee,    --
  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. November 2011
  Contributors:
 
  Beresford Parlett, University of California, Berkeley, USA
  Jim Demmel, University of California, Berkeley, USA
  Inderjit Dhillon, University of Texas, Austin, USA
  Osni Marques, LBNL/NERSC, USA
  Christof Voemel, University of California, Berkeley, USA
  
  Given the relatively robust representation(RRR) L D L^T, dlarrb
  does "limited" bisection to refine the eigenvalues of L D L^T,
  W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
  guesses for these eigenvalues are input in W, the corresponding estimate
  of the error in these guesses and their gaps are input in WERR
  and WGAP, respectively. During bisection, intervals
  [left, right] are maintained by storing their mid-points and
  semi-widths in the arrays W and WERR respectively.
  
  @param n input int The order of the matrix
  @param d input double[] of dimension n.  The n diagonal elements of the diagonal matrix D.
  @param lld input double[] of dimension (n-1).   The (n-1) elements L(i)*L(i)*D(i).
  @param ifirst input int  The index of the first eigenvalue to be computed.
  @param ilast input int The index of the last eigenvalue to be computed.
  @param rtol1 input double
  @param rtol2 input double
         Tolerance for the convergence of the bisection intervals.
         An interval [LEFT,RIGHT] has converged if
         RIGHT-LEFT.LT.MAX(rtol1*GAP, rtol2*MAX(|LEFT|,|RIGHT|) )
         where GAP is the (estimated) distance to the nearest
         eigenvalue.
  @param offset input int   Offset for the arrays w, wgap and werr, i.e., the ifirst-offset-1
         through ilast-offset-1 elements of these arrays are to be used.
  @param w (input/output) double[] of dimension n.
         On input, w[ifirst-offset-1] through w[ilast-offset-1] are
         estimates of the eigenvalues of L D L^T indexed ifirst through ilast.
         On output, these estimates are refined.
  @param wgap (input/output) double[] of dimension (n-1).
         On input, the (estimated) gaps between consecutive
         eigenvalues of L D L^T, i.e., wgap[i-offset-1] is the gap between
         eigenvalues I and I+1. Note that if ifirst == ilast
         then wgap[ifirst-offset-1] must be set to ZERO.
         On output, these gaps are refined.
  @param werr (input/output) double[] of dimension n.
         On input, werr[ifirst-offset-1] through werr[ilast-offset-1] are
         the errors in the estimates of the corresponding elements in w.
         On output, these errors are refined.
  @param work workspace double[] of dimension (2*n)
  @param iwork workspace int[] of dimension (2*n)
  @param pivmin input double  The minimum pivot in the Sturm sequence.
  @param spdiam input double The spectral diameter of the matrix.
  @param twist input int
         The twist index for the twisted factorization that is used for the negcount.
         twist = N: Compute negcount from L D L^T - LAMBDA I = L+ D+ L+^T
         twist = 1: Compute negcount from L D L^T - LAMBDA I = U- D- U-^T
         twist = R: Compute negcount from L D L^T - LAMBDA I = N(r) D(r) N(r)
  @param info output int[] of dimension 1.  Error flag.
  */
  private void dlarrb(int n, double d[], double lld[], int ifirst, int ilast, double rtol1, double rtol2,
                      int offset, double w[], double wgap[], double werr[], double work[], int iwork[],
                      double pivmin, double spdiam, int twist, int info[]) {
      int maxitr;
      int i;
      int i1;
      int ii;
      int ip;
      int iter;
      int k;
      int negcnt;
      int next;
      int nint;
      int olnint;
      int prev;
      int r;
      double back;
      double cvrgd;
      double gap;
      double left;
      double lgap;
      double mid;
      double mnwdth;
      double rgap;
      double right;
      double tmp;
      double width;
  
        info[0] = 0;
   
        maxitr = (int)( (Math.log(spdiam+pivmin)-Math.log(pivmin) ) / Math.log(2.0) ) + 2;
        mnwdth = 2.0 * pivmin;
  
        r = twist;
        if ((r < 1) || (r > n)) {
            r = n;
        }
  
        // Initialize unconverged intervals in [work[2*i-2], work[2*i-1] ].
        // The Sturm Count, Count[work[2*i-2]-1 ] is arranged to be I-1, while
        // Count[work[2*i-1]-1 ] is stored in iwork[2*i-1]. The integer iwork[2*i-2]
        // for an unconverged interval is set to the index of the next unconverged
        // interval, and is -1 or 0 for a converged interval. Thus a linked
        // list of unconverged intervals is set up.
   
        i1 = ifirst;
        // The number of unconverged intervals
        nint = 0;
        // The last unconverged interval found
        prev = 0;

        rgap = wgap[i1-offset-1];
        for (i = i1; i <= ilast; i++) {
           k = 2*i;
           ii = i - offset;
           left = w[ii-1] - werr[ii-1];
           right = w[ii-1] + werr[ii-1];
           lgap = rgap;
           rgap = wgap[ii-1];
           gap = Math.min(lgap, rgap);

           // Make sure that [LEFT,RIGHT] contains the desired eigenvalue
           // Compute negcount from dstqds facto L+D+L+^T = L D L^T - LEFT
   
           // Do while( NEGCNT(LEFT).GT.I-1 )
   
           back = werr[ii-1];
           while (true) {
               negcnt = dlaneg(n, d, lld, left, pivmin, r);
               if (negcnt > i-1) {
                   left = left - back;
                   back = 2.0 * back;
               } // if (negcnt > i-1)
               else {
                   break;
               }
           } // while (true)
   
           //Do while( NEGCNT(RIGHT).LT.I )
           // Compute negcount from dstqds facto L+D+L+^T = L D L^T - RIGHT
   
           back = werr[ii-1];
           while (true) {
               negcnt = dlaneg(n, d, lld, right, pivmin, r);
               if (negcnt < i) {
                   right = right + back;
                   back = 2.0 * back;
               } // if (negcnt < i)
               else {
                   break;
               }
           } // while (true)
           width = 0.5*Math.abs(left - right);
           tmp = Math.max(Math.abs(left), Math.abs(right));
           cvrgd = Math.max(rtol1*gap, rtol2*tmp);
           if (width <= cvrgd || width <= mnwdth) {
             // This interval has already converged and does not need refinement.
              // (Note that the gaps might change through refining the
              // eigenvalues, however, they can only get bigger.)
              // Remove it from the list.
              iwork[k-2] = -2;
              // Make sure that I1 always points to the first unconverged interval
              if ((i == i1) && (i < ilast)) {
                  i1 = i + 1;
              }
              if ((prev >= i1) && (i <= ilast)) {
                  iwork[2*prev-2] = i + 1;
              }
           } // if (width <= cvrgd || width <= mnwdth)
           else {
              // unconverged interval found
              prev = i;
              nint = nint + 1;
              iwork[k-2] = i + 1;
              iwork[k-1] = negcnt;
           }
           work[k-2] = left;
           work[k-1] = right;
        } // for (i = i1; i <= ilast; i++)

   
        // Do while(nint > 0), i.e. there are still unconverged intervals
        // and while (iter < maxitr)
   
        iter = 0;
        while (true) {
            prev = i1 - 1;
            i = i1;
            olnint = nint;
    
            for (ip = 1; ip <= olnint; ip++) {
               k = 2*i;
               ii = i - offset;
               rgap = wgap[ii-1];
               lgap = rgap;
               if (ii > 1) {
                   lgap = wgap[ii-2];
               }
               gap = Math.min(lgap, rgap);
               next = iwork[k-2];
               left = work[k-2];
               right = work[k-1];
               mid = 0.5*(left + right);
    
               // semiwidth of interval
               width = right - mid;
               tmp = Math.max(Math.abs(left), Math.abs(right));
               cvrgd = Math.max(rtol1*gap, rtol2*tmp);
               if ((width <= cvrgd) || (width <= mnwdth) || (iter == maxitr)) {
                  // reduce number of unconverged intervals
                  nint = nint - 1;
                  // Mark interval as converged.
                  iwork[k-2] = 0;
                  if (i1 == i) {
                     i1 = next;
                  }
                  else {
                     // Prev holds the last unconverged interval previously examined
                     if (prev >= i1) {
                         iwork[2*prev-2] = next;
                     }
                  }
                  i = next;
                  continue;
               } // if ((width <= cvrgd) || (width <= mnwdth) || (iter == maxitr))
               prev = i;
      
               // Perform one bisection step
       
               negcnt = dlaneg(n, d, lld, mid, pivmin, r);
               if (negcnt <= i-1) {
                  work[k-2] = mid;
               }
               else {
                  work[k-1] = mid;
               }
               i = next;
            } // for (ip = 1; ip <= olnint; ip++)
            iter = iter + 1;
            // do another loop if there are still unconverged intervals
            // However, in the last iteration, all intervals are accepted
            // since this is the best we can do.
            if ((nint > 0 ) && (iter <= maxitr)) {
                // keep looping    
            }
            else {
                break;    
            }
        } // while (true)
   
   
        // At this point, all the intervals have converged
        for (i = ifirst; i <= ilast; i++) {
           k = 2*i;
           ii = i - offset;
           // All intervals marked by '0' have been refined.
           if (iwork[k-2] == 0) {
              w[ii-1] = 0.5*(work[k-2]+work[k-1]);
              werr[ii-1] = work[k-1] - w[ii-1];
           } // if (iwork[k-2] == 0)
        } // for (i = ifirst; i <= ilast; i++)
   
        for (i = ifirst+1; i <= ilast; i++) {
           k = 2*i;
           ii = i - offset;
           wgap[ii-2] = Math.max(0.0, w[ii-1] - werr[ii-1] - w[ii-2] - werr[ii-2]);
        } // for (i = ifirst+1; i <= ilast; i++)

        return;
  } // dlarrb

  
  /** This is a port of version 3.4.0 LAPACK auxiliary routine dlarrc.  LAPACK is a software package provided by Univ. of Tennessee,    --
   -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. November 2011
   Contributors:
  
   Beresford Parlett, University of California, Berkeley, USA
   Jim Demmel, University of California, Berkeley, USA
   Inderjit Dhillon, University of Texas, Austin, USA
   Osni Marques, LBNL/NERSC, USA
   Christof Voemel, University of California, Berkeley, USA
   
   Find the number of eigenvalues of the symmetric tridiagonal matrix T
   that are in the interval (vl,vu] if jobt = 'T', and of L D L^T if jobt = 'L'.
   @param jobt input char
          = 'T':  Compute Sturm count for matrix T.
          = 'L':  Compute Sturm count for matrix L D L^T.
   @param n input int  The order of the matrix.  n > 0.
   @param vl input double The lower bound for the eigenvalues
   @param vu input double The upper bound for the eigenvalues
   @param d input double[] of dimension n
          jobt = 'T': The n diagonal elements of the tridiagonal matrix T.
          jobt = 'L': The n diagonal elements of the diagonal matrix D.
   @param e input double[] of dimension n
          jobt = 'T': The n-1 offdiagonal elements of the matrix T.
          jobt = 'L': The n-1 offdiagonal elements of the matrix L.
   @param pivmin input double  The minimum pivot in the Sturm sequence for T.
   @param eigcnt output int[] of dimension 1.
          The number of eigenvalues of the symmetric tridiagonal matrix T
          that are in the interval (vl,vu]
   @param lcnt output int[] of dimension  The left negcount of the interval
   @param rcnt output int[] of dimension  The right negcount of the interval
   @param info output int[] of dimension 1.
   */
  private void dlarrc(char jobt, int n, double vl, double vu, double d[], double e[], double pivmin,
                      int eigcnt[], int lcnt[], int rcnt[], int info[]) {
        int i;
        boolean matt;
        double lpivot;
        double rpivot;
        double sl;
        double su;
        double tmp;
        double tmp2;
        
        info[0] = 0;
        lcnt[0] = 0;
        rcnt[0] = 0;
        eigcnt[0] = 0;
        matt = ((jobt == 'T') || (jobt == 't'));


        if (matt) {
           // Sturm sequence count on T
           lpivot = d[0] - vl;
           rpivot = d[0] - vu;
           if (lpivot <= 0.0) {
              lcnt[0] = lcnt[0] + 1;
           }
           if (rpivot <= 0.0) {
              rcnt[0] = rcnt[0] + 1;
           }
           for (i = 0; i < n-1; i++) {
              tmp = e[i] * e[i];
              lpivot =  (d[ i+1 ]-vl ) - tmp/lpivot;
              rpivot = (d[i+1]-vu) - tmp/rpivot;
              if (lpivot <= 0.0) {
                 lcnt[0] = lcnt[0] + 1;
              }
              if (rpivot <= 0.0) {
                 rcnt[0] = rcnt[0] + 1;
              }
           } // for (i = 0; i < n-1; i++)
        } // if (matt)   
        else { // !matt
           // Sturm sequence count on L D L^T
           sl = -vl;
           su = -vu;
           for (i = 0; i < n-1; i++) {
              lpivot = d[i] + sl;
              rpivot = d[i] + su;
              if (lpivot <= 0.0) {
                 lcnt[0] = lcnt[0] + 1;
              }
              if (rpivot <= 0.0) {
                 rcnt[0] = rcnt[0] + 1;
              }
              tmp = e[i] * d[i] * e[i];
  
              tmp2 = tmp / lpivot;
              if (tmp2 == 0.0) {
                 sl =  tmp - vl;
              }
              else {
                 sl = sl * tmp2 - vl;
              }
  
              tmp2 = tmp / rpivot;
              if (tmp2 == 0.0) {
                 su = tmp - vu;
              }
              else {
                 su = su * tmp2 - vu;
              }
           } // for (i = 0; i < n-1; i++)
           lpivot = d[n-1] + sl;
           rpivot = d[n-1] + su;
           if (lpivot <= 0.0) {
              lcnt[0] = lcnt[0] + 1;
           }
           if (rpivot <= 0.0) {
              rcnt[0] = rcnt[0] + 1;
           }
        } // else !matt
        eigcnt[0] = rcnt[0] - lcnt[0];

        return;
  } // dlarrc
  
  /** This is a port of version 3.4.0 LAPACK auxiliary routine dlarrd.  LAPACK is a software package provided by Univ. of Tennessee,    --
  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. November 2011
  Contributors:
 
  W. Kahan, University of California, Berkeley, USA
  Beresford Parlett, University of California, Berkeley, USA
  Jim Demmel, University of California, Berkeley, USA
  Inderjit Dhillon, University of Texas, Austin, USA
  Osni Marques, LBNL/NERSC, USA
  Christof Voemel, University of California, Berkeley, USA
  
  dlarrd computes the eigenvalues of a symmetric tridiagonal
  matrix T to suitable accuracy. This is an auxiliary code to be
  called from dstemr.
  The user may ask for all eigenvalues, all eigenvalues
  in the half-open interval (vl, vu], or the il-th through iu-th
  eigenvalues.
  
  To avoid overflow, the matrix must be scaled so that its
  largest element is no greater than overflow**(1/2) * underflow**(1/4) in absolute value, and for greatest
  accuracy, it should not be much smaller than that.
 
  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
  Matrix", Report CS41, Computer Science Dept., Stanford
  University, July 21, 1966.
  
  @param range input char
        = 'A': ("All")   all eigenvalues will be found.
        = 'V': ("Value") all eigenvalues in the half-open interval
                         (vl, vu] will be found.
        = 'I': ("Index") the il-th through iu-th eigenvalues (of the
                         entire matrix) will be found.
  @param order input char
        = 'B': ("By Block") the eigenvalues will be grouped by
                            split-off block (see IBLOCK, ISPLIT) and
                            ordered from smallest to largest within the block.
        = 'E': ("Entire matrix") the eigenvalues for the entire matrix
                                 will be ordered from smallest to largest. 
  @param n input int  The order of the tridiagonal matrix.   n >= 0. 
  @param vl input double
  @param vu input double
        If range = 'V', the lower and upper bounds of the interval to
        be searched for eigenvalues.  Eigenvalues less than or equal
        to vl, or greater than vu, will not be returned.  vl < vu.
        Not referenced if range = 'A' or 'I'.
  @param il input int
  @param iu input int  
        If range ='I', the indices (in ascending order) of the
        smallest and largest eigenvalues to be returned.
        1 <= il <= iu <= n, if n > 0; il = 1 and iu = 0 if n = 0.
        Not referenced if range = 'A' or 'V'. 
  @param gers input double[] of dimension (2*n)
        The n Gerschgorin intervals (the i-th Gerschgorin interval
        is (gres[2*i-2], gers[2*i-1]). 
  @param reltol input double 
        The minimum relative width of an interval.  When an interval
        is narrower than RELTOL times the larger (in magnitude) endpoint,
        then it is considered to be sufficiently small, i.e., converged.  Note: this should
        always be at least radix*machine epsilon.
  @param d input double[] of dimension n. 
        The n diagonal elements of the tridiagonal matrix T.
  @param e input double[] of dimension (n-1)
        The (n-1) off-diagonal elements of the tridiagonal matrix T.  
  @param e2 input double[] of dimension (n-1)
        The (n-1) squared off-diagonal elements of the tridiagonal matrix T.           
  @param pivmin input double
        The minimum pivot allowed in the Sturm sequence for T.
  @param nsplit input int
        The number of diagonal blocks in the matrix T.
        1 <= nsplit <= n
  @param isplit input int[] of dimension n.
        The splitting points, at which T breaks up into submatrices.
        The first submatrix consists of rows/columns 1 to isplit[0],
        the second of rows/columns isplit[0]+1 through isplit[1],
        etc., and the nsplit-th consists of rows/columns
        isplit[nsplit-2]+1 through isplit[nsplit-1]=n.
        (Only the first nsplit elements will actually be used, but
        since the user cannot know a priori what value nsplit will
        have, n words must be reserved for isplit.) 
  @param m output int[] of dimension 1.
        The actual number of eigenvalues found. 0 <= m[0] <= n.
        (See also the description of info[0]=2,3.)
  @param w output double[] of dimension n.
        On exit, the first m[0] elements of w will contain the
        eigenvalue approximations. dlarrd computes an interval
        I_j = (a_j, b_j] that includes eigenvalue j. The eigenvalue
        approximation is given as the interval midpoint
        w[j] = ( a_j + b_j)/2. The corresponding error is bounded by
        werr[j] = abs( a_j - b_j)/2  
  @param werr output double[[] of dimension n.
         The error bound on the corresponding eigenvalue approximation in w.
  @param wl output double[] of dimension 1.
  @param wu output double[] of dimension 1.
         The interval (wl[0], wu[0]] contains all the wanted eigenvalues.
         If range ='V', then wl[0]=vl and wu[0]=vu.
         If range ='A', then wl[0] and wu[0] are the global Gerschgorin bounds
                        on the spectrum.
         If range ='I', then wl[0] and wu[0] are computed by dlaebz from the
                        index range specified.
  @param iblock output int[] of dimension n.
         At each row/column j where e[j] is zero or small, the
         matrix T is considered to split into a block diagonal
         matrix.  On exit, if info[0] = 0, iblock[i] specifies to which
         block (from 1 to the number of blocks) the eigenvalue w[i]
         belongs.  (dlarrd may use the remaining n-m[0] elements as workspace.)
  @param indexw output int[] of dimension n.
          The indices of the eigenvalues within each block (submatrix);
          for example, indexw[i] = j and iblock[i] = k imply that the
          i-th eigenvalue w[i] is the j-th eigenvalue in block k.
  @param work (workspace/output) double[] of dimension (4*n)
  @param iwork (workspace/output) int[] of dimension (3*n)
  @param info[] output int[] of dimension 1.
         = 0:  successful exit
         < 0:  if info[0] = -i, the i-th argument had an illegal value
         > 0:  some or all of the eigenvalues failed to converge or
               were not computed:
                    =1 or 3: Bisection failed to converge for some
                            eigenvalues; these eigenvalues are flagged by a
                            negative block number.  The effect is that the
                            eigenvalues may not be as accurate as the
                            absolute and relative tolerances.  This is
                            generally caused by unexpectedly inaccurate
                            arithmetic.
                    =2 or 3: range ='I' only: Not all of the eigenvalues
                            il:iu were found.
                            Effect: m[0] < iu+1-il
                            Cause:  non-monotonic arithmetic, causing the
                                    Sturm sequence to be non-monotonic.
                            Cure:   recalculate, using range='A', and pick
                                    out eigenvalues il:iu.  In some cases,
                                    increasing the PARAMETER "fudge" may
                                    make things work.
                    = 4:    range='I', and the Gershgorin interval
                            initially used was too small.  No eigenvalues
                            were computed.
                            Probable cause: your machine has sloppy
                                            floating-point arithmetic.
                            Cure: Increase the PARAMETER "fudge",
                                  recompile, and try again.
  */
  private void dlarrd(char range, char order, int n, double vl, double vu, int il, int iu,
                      double gers[], double reltol, double d[], double e[], double e2[],
                      double pivmin, int nsplit, int isplit[], int m[], double w[], double werr[],
                      double wl[], double wu[], int iblock[], int indexw[], double work[],
                      int iwork[], int info[]) {
      
      // A "fudge factor" to widen the Gershgorin intervals.  Ideally,
      // a value of 1 should work, but on machines with sloppy
      // arithmetic, this needs to be larger.  The default for
      // publicly released versions should be large enough to handle
      // the worst machine around.  Note that this has no effect
      // on accuracy of the solution.
      final double fudge = 2.0;
      final int allrng = 1;
      final int valrng = 2;
      final int indrng = 3;
      
      int idumma[] = new int[1];
      int irange;
      int nb;
      int i;
      int itmax;
      int nwl;
      int nwu;
      int iend;
      int jblk;
      int ioff;
      int ibegin;
      int in;
      int je;
      int ib;
      int idiscl;
      int idiscu;
      int im[] = new int[1];
      int jee;
      int j;
      int ie;
      int itmp1;
      int itmp2;
      int iout[] = new int[1];
      int iinfo[] = new int[1];
      int nval[];
      int NAB[][];
      int index;
      int iworkaux[];
      int iwoff;
      int jdisc;
      int iw;
      double AB[][];
      double c[];
      double daux[];
      double eaux[];
      double e2aux[];
      double workaux[];
      boolean ncnvrg;
      boolean toofew;
      double eps;
      double uflow;
      double gl;
      double gu;
      //double spdiam;
      double rtoli;
      double atoli;
      double wlu = 0.0;
      double wul = 0.0;
      //double disc;
      double tmp1;
      //double L1;
      double tmp2;
      double wkill;
      double tnorm;
  
        info[0] = 0;
  
        // Decode RANGE
  
        if ((range == 'A') || (range == 'a')) {
           irange = allrng;
        }
        else if ((range == 'V') || (range == 'v')) {
           irange = valrng;
        }
        else if ((range == 'I') || (range == 'i')) {
           irange = indrng;
        }
        else {
           irange = 0;
        }
  
        // Check for Errors
  
        if (irange <= 0) {
           info[0] = -1;
        }
        else if (!((order == 'B') || (order == 'b') || (order == 'E') || (order == 'e'))) {
           info[0] = -2;
        }
        else if (n < 0) {
           info[0] = -3;
        }
        else if (irange == valrng) {
           if (vl >= vu) {
              info[0] = -5;
           }
        }
        else if (irange == indrng &&
                (il < 1 || il > Math.max( 1, n))) {
           info[0] = -6;
        }
        else if (irange == indrng &&
                (iu < Math.min(n, il) || iu > n)) {
           info[0] = -7;
        }
  
        if (info[0] != 0) {
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

        //Simplification:
        if (irange == indrng && il == 1 && iu == n) {
            irange = 1;
        }

        // Get machine constants
        eps = ge.dlamch('P');
        uflow = ge.dlamch('U');


        // Special Case when N=1
        // Treat case of 1x1 matrix for quick return
        if (n == 1) {
           if ( (irange == allrng) ||
               ((irange == valrng) && (d[0] > vl) && (d[0] <= vu)) ||
               ((irange == indrng) && (il == 1) && (iu == 1))) {
              m[0] = 1;
              w[0] = d[0];
              // The computation error of the eigenvalue is zero
              werr[0] = 0.0;
              iblock[0] = 1;
              indexw[0] = 1;
           }
           return;
        } // if (n == 1)

        // nb is the minimum vector length for vector bisection, or 0
        // if only scalar is to be done.
        nb = ge.ilaenv( 1, "DSTEBZ", " ", n, -1, -1, -1 );
        if (nb <= 1) {
            nb = 0;
        }

        // Find global spectral radius
        gl = d[0];
        gu = d[0];
        for (i = 1; i <= n; i++) {
           gl =  Math.min(gl, gers[2*i-2]);
           gu = Math.max(gu, gers[2*i-1]);
        }
        // Compute global Gerschgorin bounds and spectral diameter
        tnorm = Math.max(Math.abs(gl), Math.abs(gu));
        gl = gl - fudge*tnorm*eps*n - fudge*2.0*pivmin;
        gu = gu + fudge*tnorm*eps*n + fudge*2.0*pivmin;
        // [JAN/28/2009] remove the line below since SPDIAM variable not use
        // SPDIAM = GU - GL
        // Input arguments for DLAEBZ:
        // The relative tolerance.  An interval (a,b] lies within
        // "relative tolerance" if  b-a < reltol*max(|a|,|b|),
        rtoli = reltol;
        // Set the absolute tolerance for interval convergence to zero to force
        // interval convergence based on relative size of the interval.
        // This is dangerous because intervals might not converge when RELTOL is
        // small. But at least a very small number should be selected so that for
        // strongly graded matrices, the code can get relatively accurate
        // eigenvalues.
        atoli = fudge*2.0*uflow + fudge*2.0*pivmin;

        if (irange == indrng) {

           // range ='I': Compute an interval containing eigenvalues
           //il through iu. The initial interval [gl,gu] from the global
           // Gerschgorin bounds gl and gu is refined by dlaebz.
           itmax = (int)( (Math.log(tnorm+pivmin)-Math.log(pivmin) ) /
                   Math.log(2.0) ) + 2;
           nval = new int[2];
           AB = new double[2][2];
           c = new double[2];
           NAB = new int[2][2];
           work[n] = gl;
           work[n+1] = gl;
           work[n+2] = gu;
           work[n+3] = gu;
           AB[0][0] = gl;
           AB[1][0] = gl;
           AB[0][1] = gu;
           AB[1][1] = gu;
           work[n+4] = gl;
           work[n+5] = gu;
           c[0] = gl;
           c[1] = gu;
           iwork[0] = -1;
           iwork[1] = -1;
           iwork[2] = n + 1;
           iwork[3] = n + 1;
           NAB[0][0] = -1;
           NAB[1][0] = -1;
           NAB[0][1] = n + 1;
           NAB[1][1] = n + 1;
           iwork[4] = il - 1;
           iwork[5] = iu;
           nval[0] = il - 1;
           nval[1] = iu;
  
           se.dlaebz( 3, itmax, n, 2, 2, nb, atoli, rtoli, pivmin,
                   d, e, e2, nval, AB, c, iout, NAB, w, iblock, iinfo);
           iwork[0] = NAB[0][0];
           iwork[1] = NAB[1][0];
           iwork[2] = NAB[0][1];
           iwork[3] = NAB[1][1];
           iwork[4] = nval[0];
           iwork[5] = nval[1];
           work[n] = AB[0][0];
           work[n+1] = AB[1][0];
           work[n+2] = AB[0][1];
           work[n+3] = AB[1][1];
           work[n+4] = c[0];
           work[n+5] = c[1];
           if (iinfo[0] != 0) {
              info[0] = iinfo[0];
              return;
           }
          //  On exit, output intervals may not be ordered by ascending negcount
           if (iwork[5] == iu) {
              wl[0] = work[n];
              wlu = work[n+2];
              nwl = iwork[0];
              wu[0] = work[n+3];
              wul = work[n+1];
              nwu = iwork[3];
           }
           else {
              wl[0] = work[n+1];
              wlu = work[n+3];
              nwl = iwork[1];
              wu[0] = work[n+2];
              wul = work[n];
              nwu = iwork[2];
           }
           // On exit, the interval [wl, wlu] contains a value with negcount nwl,
           // and [wul, wu] contains a value with negcount NWUnwu.
           if (nwl < 0 || nwl >= n || nwu < 1 || nwu > n) {
              info[0] = 4;
              return;
           }
        } // if (irange == indrng)
        else if (irange == valrng) {
           wl[0] = vl;
           wu[0] = vu;
        }
        else if (irange == allrng) {
           wl[0] = gl;
           wu[0] = gu;
        }



        // Find Eigenvalues -- Loop Over blocks and recompute nwl and nwu.
        // nwl accumulates the number of eigenvalues <= wl
        // nwu accumulates the number of eigenvalues <= wu
        m[0] = 0;
        iend = 0;
        info[0] = 0;
        nwl = 0;
        nwu = 0;
  
        for (jblk = 1; jblk <= nsplit; jblk++) {
           ioff = iend;
           ibegin = ioff + 1;
           iend = isplit[jblk-1];
           in = iend - ioff;
 
           if (in == 1) {
              // 1x1 block
              if (wl[0] >= d[ibegin-1]-pivmin) {
                 nwl = nwl + 1;
              }
              if (wu[0] >= d[ibegin-1]-pivmin) {
                 nwu = nwu + 1;
              }
              if (irange == allrng ||
                   (wl[0] < d[ibegin-1]-pivmin
                     && wu[0] >= d[ibegin-1]-pivmin ) ) {
                 m[0] = m[0] + 1;
                 w[m[0]-1] = d[ibegin-1];
                 werr[m[0]-1] = 0.0;
                 // The gap for a single block doesn't matter for the later
                 // algorithm and is assigned an arbitrary large value
                 iblock[m[0]-1] = jblk;
                 indexw[m[0]-1] = 1;
              }

           // Disabled 2x2 case because of a failure on the following matrix
           // range = 'I', il = iu = 4
             // Original Tridiagonal, d = [
             //  -0.150102010615740E+00
             //  -0.849897989384260E+00
             //  -0.128208148052635E-15
             //  0.128257718286320E-15
            // ];
             // e = [
             // -0.357171383266986E+00
             //  -0.180411241501588E-15
             // -0.175152352710251E-15
            // ];
           //  } // if (in == 1)
           // else if (in == 2) {
             // 2x2 block
             //  DISC = SQRT( (HALF*(D(IBEGIN)-D(IEND)))**2 + E(IBEGIN)**2 )
             //  TMP1 = HALF*(D(IBEGIN)+D(IEND))
             //   L1 = TMP1 - DISC
             //  IF( WL.GE. L1-PIVMIN )
                 //  NWL = NWL + 1
              //  IF( WU.GE. L1-PIVMIN )
                 // NWU = NWU + 1
              // IF( IRANGE.EQ.ALLRNG .OR. ( WL.LT.L1-PIVMIN .AND. WU.GE.
                  // L1-PIVMIN ) ) THEN
                 // M = M + 1
                 // W( M ) = L1
                  // The uncertainty of eigenvalues of a 2x2 matrix is very small
                 // WERR( M ) = EPS * ABS( W( M ) ) * TWO
                 // IBLOCK( M ) = JBLK
                 // INDEXW( M ) = 1
              // ENDIF
              // L2 = TMP1 + DISC
              // IF( WL.GE. L2-PIVMIN )
                 // NWL = NWL + 1
              // IF( WU.GE. L2-PIVMIN )
                 // NWU = NWU + 1
              // IF( IRANGE.EQ.ALLRNG .OR. ( WL.LT.L2-PIVMIN .AND. WU.GE.
                  // L2-PIVMIN ) ) THEN
                 // M = M + 1
                //  W( M ) = L2
                 // The uncertainty of eigenvalues of a 2x2 matrix is very small
                 // WERR( M ) = EPS * ABS( W( M ) ) * TWO
                 // IBLOCK( M ) = JBLK
                 // INDEXW( M ) = 2
               // ENDIF
           } // if (in == 1)
           else {
              // General Case - block of size IN >= 2
              // Compute local Gerschgorin interval and use it as the initial
              // interval for dlaebz
              gu = d[ibegin-1];
              gl = d[ibegin-1];
              tmp1 = 0.0;

              for (j = ibegin; j <= iend; j++) {
                 gl =  Math.min(gl, gers[2*j-2]);
                 gu = Math.max(gu, gers[2*j-1]);
              } // for (j = ibegin; j <= iend; j++)
              // [JAN/28/2009]
              // change spdiam by tnorm in lines 2 and 3 thereafter
              // line 1: remove computation of SPDIAM (not useful anymore)
              // SPDIAM = GU - GL
              // GL = GL - FUDGE*SPDIAM*EPS*IN - FUDGE*PIVMIN
              // GU = GU + FUDGE*SPDIAM*EPS*IN + FUDGE*PIVMIN
              gl = gl - fudge*tnorm*eps*in - fudge*pivmin;
              gu = gu + fudge*tnorm*eps*in + fudge*pivmin;
   
              if (irange > 1) {
                 if (gu < wl[0]) {
                    // the local block contains none of the wanted eigenvalues
                    nwl = nwl + in;
                    nwu = nwu + in;
                    continue;
                 } // if (gu < wl[0])
                 // refine search interval if possible, only range (wl[0],wu[0]] matters
                 gl = Math.max(gl, wl[0]);
                 gu = Math.min(gu, wu[0]);
                 if (gl >= gu) {
                    continue;
                 }
              } // if (irange > 1)

              // Find negcount of initial interval boundaries gl and gu
              work[n] = gl;
              work[n+in] = gu;
              daux = new double[in];
              for (i = 0; i < in; i++) {
                  daux[i] = d[ibegin-1+i];
              }
              eaux = new double[in];
              for (i = 0; i < in; i++) {
                  eaux[i] = e[ibegin-1+i];
              }
              e2aux = new double[in];
              for (i = 0; i < in; i++) {
                  e2aux[i] = e2[ibegin-1+i];
              }
              AB = new double[in][2];
              index = 0;
              for (j = 0; j < 2; j++) {
                  for (i = 0; i < in; i++) {
                      AB[i][j] = work[n+index];
                      index++;
                  }
              }
              c = new double[in];
              for (i = 0; i < in; i++) {
                  c[i] = work[n+2*in+i];
              }
              NAB = new int[in][2];
              index = 0;
              for (j = 0; j < 2; j++) {
                  for (i = 0; i < in; i++) {
                      NAB[i][j] = iwork[index++];
                  }
              }
              workaux = new double[in];
              iworkaux = new int[in];
              se.dlaebz( 1, 0, in, in, 1, nb, atoli, rtoli, pivmin,
                           daux, eaux, e2aux, idumma, AB, c, im,
                           NAB, workaux, iworkaux, iinfo);
              index = 0;
              for (j = 0; j < 2; j++) {
                  for (i = 0; i < in; i++) {
                      iwork[index++] = NAB[i][j];
                  }
              }
              if (iinfo[0] != 0) {
                 info[0] = iinfo[0];
                 return;
              }
   
              nwl = nwl + iwork[0];
              nwu = nwu + iwork[in];
              iwoff = m[0] - iwork[0];

              // Compute Eigenvalues
              itmax = (int)( (Math.log(gu-gl+pivmin)-Math.log(pivmin) ) /
                      Math.log(2.0) ) + 2;
              se.dlaebz( 2, itmax, in, in, 1, nb, atoli, rtoli, pivmin,
                      daux, eaux, e2aux, idumma, AB, c, iout,
                      NAB, workaux, iworkaux, iinfo);
              index = 0;
              for (j = 0; j < 2; j++) {
                  for (i = 0; i < in; i++) {
                      work[n+index] = AB[i][j];
                      index++;
                  }
              }
              for (i = 0; i < in; i++) {
                  work[n+2*in+i] = c[i];
              }
              index = 0;
              for (j = 0; j < 2; j++) {
                  for (i = 0; i < in; i++) {
                      iwork[index++] = NAB[i][j];
                  }
              }
              if (iinfo[0] != 0) {
                 info[0] = iinfo[0];
                 return;
              }
   
              // Copy eigenvalues into w and iblock
              // Use -jblk for block number for unconverged eigenvalues.
              // Loop over the number of output intervals from dlaebz
              for (j = 1; j <= iout[0]; j++) {
                 // eigenvalue approximation is middle point of interval
                 tmp1 = 0.5*(work[j+n-1]+work[j+in+n-1]);
                 // semi length of error interval
                 tmp2 = 0.5*Math.abs(work[j+n-1]-work[j+in+n-1]);
                 if (j > iout[0] - iinfo[0]) {
                    // Flag non-convergence.
                    ncnvrg = true;
                    ib = -jblk;
                 }
                 else {
                    ib = jblk;
                 }
                 for (je = iwork[j-1] + 1 + iwoff; je <= iwork[j+in-1] + iwoff; je++) {
                    w[je-1] = tmp1;
                    werr[je-1] = tmp2;
                    indexw[je-1] = je - iwoff;
                    iblock[je-1] = ib;
                 } // for (je = iwork[j-1] + 1 + iwoff; je <= iwork[j+in-1] + iwoff; je++)
              } // for (j = 1; j <= iout[0]; j++)
   
              m[0] = m[0] + im[0];
           } // else in >= 2
        } // for (jblk = 1; jblk <= nsplit; jblk++)

        // If range ='I', then (wl[0],wu[0]) contains eigenvalues nwl+1,...,nwu
        // If nwl+1 < il or nwu > iu, discard extra eigenvalues.
        if (irange == indrng) {
           idiscl = il - 1 - nwl;
           idiscu = nwu - iu;
  
           if (idiscl > 0) {
              im[0] = 0;
              for (je = 1; je <= m[0]; je++) {
                 // Remove some of the smallest eigenvalues from the left so that
                 // at the end idiscl =0. Move all eigenvalues up to the left.
                 if (w[je-1] <= wlu && idiscl > 0) {
                    idiscl = idiscl - 1;
                 }
                 else {
                    im[0] = im[0] + 1;
                    w[im[0]-1] = w[je-1];
                    werr[im[0]-1] = werr[je-1];
                    indexw[im[0]-1] = indexw[je-1];
                    iblock[im[0]-1] = iblock[je-1];
                 }
              } // for (je = 1; je <= m[0]; je++)
              m[0] = im[0];
           } // if (idiscl > 0)
           if (idiscu > 0) {
              // Remove some of the largest eigenvalues from the right so that
              // at the end idiscu =0. Move all eigenvalues up to the left.
              im[0]=m[0]+1;
              for (je = m[0]; je >= 1; je--) {
                 if (w[je-1] >= wul && idiscu > 0) {
                    idiscu = idiscu - 1;
                 }
                 else {
                    im[0] = im[0] - 1;
                    w[im[0]-1] = w[je-1];
                    werr[im[0]-1] = werr[je-1];
                    indexw[im[0]-1] = indexw[je-1];
                    iblock[im[0]-1] = iblock[je-1];
                 }
              } // for (je = m[0]; je >= 1; je--) 
              jee = 0;
              for (je = im[0]; je <= m[0]; je++) {
                 jee = jee + 1;
                 w[jee-1] = w[je-1];
                 werr[jee-1] = werr[je-1];
                 indexw[jee-1] = indexw[je-1];
                 iblock[jee-1] = iblock[je-1];
              } // for (je = im[0]; je <= m[0]; je++)
              m[0] = m[0]-im[0]+1;
           } // if (idiscu > 0)

           if (idiscl > 0 || idiscu > 0) {
              // Code to deal with effects of bad arithmetic. (If N(w) is
              // monotone non-decreasing, this should never happen.)
              // Some low eigenvalues to be discarded are not in (wl[0],wlu],
              // or high eigenvalues to be discarded are not in (wul,wu[0]]
              // so just kill off the smallest idiscl/largest idiscu
              // eigenvalues, by marking the corresponding iblock = 0
              if (idiscl > 0) {
                 wkill = wu[0];
                 for (jdisc = 1; jdisc <= idiscl; jdisc++) {
                    iw = 0;
                    for (je = 1; je <= m[0]; je++) {
                       if (iblock[je-1] != 0 &&
                            (w[je-1] < wkill || iw == 0)) {
                          iw = je;
                          wkill = w[je-1];
                       }
                    } // for (je = 1; je <= m[0]; je++)
                    iblock[iw-1] = 0;
                 } // for (jdisc = 1; jdisc <= idiscl; jdisc++)
              } // if (idiscl > 0)
              if (idiscu > 0) {
                 wkill = wl[0];
                 for (jdisc = 1; jdisc <= idiscu; jdisc++) {
                    iw = 0;
                    for (je = 1; je <= m[0]; je++) {
                       if (iblock[je-1] != 0 &&
                            (w[je-1] >= wkill || iw == 0)) {
                          iw = je;
                          wkill = w[je-1];
                       }
                    } // for (je = 1; je <= m[0]; je++)
                    iblock[iw-1] = 0;
                 } // for (jdisc = 1; jdisc <= idiscu; jdisc++)
              } // if (idiscu > 0)
              // Now erase all eigenvalues with iblock set to zero
              im[0] = 0;
              for (je = 1; je <= m[0]; je++) {
                 if (iblock[je-1] != 0) {
                    im[0] = im[0] + 1;
                    w[im[0]-1] = w[je-1];
                    werr[im[0]-1] = werr[je-1];
                    indexw[im[0]-1] = indexw[je-1];
                    iblock[im[0]-1] = iblock[je-1];
                 }
              } // for (je = 1; je <= m[0]; je++)
              m[0] = im[0];
           } // if (idiscl > 0 || idiscu > 0)
           if (idiscl < 0 || idiscu < 0) {
              toofew = true;
           }
        } // if (irange == indrng) 
   
        if ((irange == allrng && m[0] != n) ||
           (irange == indrng && m[0] != iu-il+1) ) {
           toofew = true;
        }

        // If order = 'B', do nothing the eigenvalues are already sorted by block.
        // If order = 'E', sort the eigenvalues from smallest to largest

        if(((order == 'E') || (order == 'e')) && nsplit > 1) {
           for (je = 1; je <= m[0] - 1; je++) {
              ie = 0;
              tmp1 = w[je-1];
              for (j = je + 1; j <= m[0]; j++) {
                 if (w[j-1] < tmp1) {
                    ie = j;
                    tmp1 = w[j-1];
                 }
              } // for (j = je + 1; j <= m[0]; j++)
              if (ie != 0) {
                 tmp2 = werr[ie-1];
                 itmp1 = iblock[ie-1];
                 itmp2 = indexw[ie-1];
                 w[ie-1] = w[je-1];
                 werr[ie-1] = werr[je-1];
                 iblock[ie-1] = iblock[je-1];
                 indexw[ie-1] = indexw[je-1];
                 w[je-1] = tmp1;
                 werr[je-1] = tmp2;
                 iblock[je-1] = itmp1;
                 indexw[je-1] = itmp2;
              } // if (ie != 0)
           } // for (je = 1; je <= m[0] - 1; je++)
        } // if(((order == 'E') || (order == 'e')) && nsplit > 1)
  
        info[0] = 0;
        if (ncnvrg) {
           info[0] = info[0] + 1;
        }
        if (toofew) {
           info[0] = info[0] + 2;
        }
        return;
  } // dlarrd

  
  
  /** This is a port of version 3.4.0 LAPACK auxiliary routine dlarre.  LAPACK is a software package provided by Univ. of Tennessee,    --
  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. November 2011
  Contributors:
 
  Beresford Parlett, University of California, Berkeley, USA
  Jim Demmel, University of California, Berkeley, USA
  Inderjit Dhillon, University of Texas, Austin, USA
  Osni Marques, LBNL/NERSC, USA
  Christof Voemel, University of California, Berkeley, USA
  
  To find the desired eigenvalues of a given real symmetric
  tridiagonal matrix T, dlarre sets any "small" off-diagonal
  elements to zero, and for each unreduced block T_i, it finds
  (a) a suitable shift at one end of the block's spectrum,
  (b) the base representation, T_i - sigma_i I = L_i D_i L_i^T, and
  (c) eigenvalues of each L_i D_i L_i^T.
  The representations and eigenvalues found are then used by
  dstemr to compute the eigenvectors of T.
  The accuracy varies depending on whether bisection is used to
  find a few eigenvalues or the dqds algorithm (subroutine dlasq2) to
  compute all and then discard any unwanted one.
  As an added benefit, dlarreE also outputs the n
  Gerschgorin intervals for the matrices L_i D_i L_i^T.
  
  The base representations are required to suffer very little
  element growth and consequently define all their eigenvalues to
  high relative accuracy.
  
  @param range input char
         = 'A': ("All")   all eigenvalues will be found.
         = 'V': ("Value") all eigenvalues in the half-open interval
                         (vl, vu] will be found.
         = 'I': ("Index") the il-th through iu-th eigenvalues (of the
                          entire matrix) will be found. 
  @param n input int  The order of the matrix.  n > 0. 
  @param vl (input/output) double[] of dimension 1. 
  @param vu (input/output) double[] of dimension 1.
         If range='V', the lower and upper bounds for the eigenvalues.
         Eigenvalues less than or equal to vl[0], or greater than vu[0],
         will not be returned.  vl[0] < vu[0].
         If range ='I' or ='A', dlarre computes bounds on the desired
         part of the spectrum.  
  @param il input int
  @param iu input int 
         If range='I', the indices (in ascending order) of the
         smallest and largest eigenvalues to be returned.
         1 <= il <= iu <= n.
  @param d (input/output) double[] of dimension n.
         On entry, the n diagonal elements of the tridiagonal matrix T.
         On exit, the n diagonal elements of the diagonal matrices D_i. 
  @param e (input/output) double[] of dimension n.
         On entry, the first (n-1) entries contain the subdiagonal
         elements of the tridiagonal matrix T; e[n-1] need not be set.
         On exit, e contains the subdiagonal elements of the unit
         bidiagonal matrices L_i. The entries e[isplit[i-1]-1],
         1 <= i <= nsplit, contain the base points sigma_i on output. 
  @param e2 (input/output) double[] of dimension n.
         On entry, the first (n-1) entries contain the SQUARES of the
         subdiagonal elements of the tridiagonal matrix T;
         e2[n-1] need not be set.
         On exit, the entries e2[isplit[i-1]-1],
         1 <= i <= nsplit, have been set to zero 
  @param rtol1 input double
  @param rtol2 input double 
         Parameters for bisection.
         An interval [LEFT,RIGHT] has converged if
         RIGHT-LEFT < max(rtol1*GAP, rtol2*max(|LEFT|,|RIGHT|) )
  @param spltol input double The threshold for splitting. 
  @param nsplit output int[] of dimension 1.
         The number of blocks T splits into.  1 <= nsplit[0] <= n.
  @param isplit output int[] of dimension n.
         The splitting points, at which T breaks up into blocks.
         The first block consists of rows/columns 1 to isplit[0],
         the second of rows/columns isplit[0]+1 through isplit[1],
         etc., and the NSPLIT-th consists of rows/columns
         isplit[nsplit[0]-2]+1 through isplit[nsplit[0]-1]=n. 
  @param m output int[] of dimension 1. 
         The total number of eigenvalues (of all L_i D_i L_i^T) found. 
  @param w output double[] of dimension n
         The first m[0] elements contain the eigenvalues. The
         eigenvalues of each of the blocks, L_i D_i L_i^T, are
         sorted in ascending order ( dlarre may use the
         remaining n-m[0] elements as workspace).
  @param werr output double[] of dimension n.  
         The error bound on the corresponding eigenvalue in w[].
  @param wgap output double[] of dimension n.
         The separation from the right neighbor eigenvalue in w[].
         The gap is only with respect to the eigenvalues of the same block
         as each block has its own representation tree.
         Exception: at the right end of a block we store the left gap 
  @param iblock output int[] of dimension n.
         The indices of the blocks (submatrices) associated with the
         corresponding eigenvalues in w; iblock[i]=1 if eigenvalue
         w[i] belongs to the first block from the top, =2 if w[i]
         belongs to the second block, etc.
  @param indexw output int[] of dimension n.
         The indices of the eigenvalues within each block (submatrix);
         for example, indexw[i]= 10 and iblock[i]=2 imply that the
         i-th eigenvalue w[i] is the 10-th eigenvalue in block 2 
  @param gers output double[] of dimension (2*n)
         The n Gerschgorin intervals (the i-th Gerschgorin interval is (gers[2*i-2], gers[2*i-1]).
  @param pivmin output double[] of dimension 1.  The minimum pivot in the Sturm sequence for T.
  @param work (workspace/output) double[] of dimension (6*n) 
  @param iwork (workspace/output) int[] of dimension (5*n)
  @param info[] output int of dimension 1.
         = 0:  successful exit
         > 0:  A problem occured in dlarre.
         < 0:  One of the called subroutines signaled an internal problem.
               Needs inspection of the corresponding parameter iinfo[0]
               for further information.
  
         =-1:  Problem in dlarrd.
         = 2:  No base representation could be found in maxtry iterations.
               Increasing maxtry and recompilation might be a remedy.
         =-3:  Problem in dlarrb when computing the refined root representation for dlasq2.
         =-4:  Problem in dlarrb when preforming bisection on the
               desired part of the spectrum.
         =-5:  Problem in dlasq2.
         =-6:  Problem in dlasq2.
  */ 
  private void dlarre(char range, int n, double vl[], double vu[], int il, int iu,
                      double d[], double e[], double e2[], double rtol1, double rtol2,
                      double spltol, int nsplit[], int isplit[], int m[], double w[],
                      double werr[], double wgap[], int iblock[], int indexw[],
                      double gers[], double pivmin[], double work[], int iwork[],
                      int info[]) { 
        final double pert = 8.0;
        final double fac = 0.5;
        final double maxgrowth = 64.0;
        final double fudge = 2.0;
        final int maxtry = 6;
        final int allrng = 1;
        final int indrng = 2;
        final int valrng = 3;
        int i;
        int iseed[] = new int[4];
        int iinfo[] = new int[1];
        int irange = allrng;
        int ibegin;
        int wbegin;
        int jblk;
        int iend;
        int in;
        int mb = 0;
        int wend = 0;
        int indl = 0;
        int indu = 0;
        int idum;
        int j;
        int cnt[] = new int[1];
        int cnt1[] = new int[1];
        int cnt2[] = new int[1];
        int mm[] = new int[1];
        double safmin;
        double eps;
        double rtl;
        double bsrtol;
        double gl;
        double gu;
        double eold;
        double emax;
        double eabs;
        double rtol;
        double tmp1[] = new double[1];
        double spdiam;
        boolean forceb;
        boolean usedqd;
        boolean norep;
        boolean leave = true;
        double sigma;
        double isleft;
        double isrght;
        double s1;
        double s2;
        double tmp[] = new double[1];
        double sgndef;
        double tau;
        double clwdth;
        double avgap;
        double dpivot;
        double dmax;
        double daux[];
        double eaux[];
        double e2aux[];
        double lld[];
        double waux[];
        double wgapaux[];
        double werraux[];
        double workaux[];

        info[0] = 0;

  
        // Decode range
  
        if ((range == 'A') || (range == 'a')) {
           irange = allrng;
        }
        else if ((range == 'V') || (range == 'v')) {
           irange = valrng;
        }
        else if ((range == 'I') || (range == 'i')) {
           irange = indrng;
        }

        m[0] = 0;

        // Get machine constants
        safmin = ge.dlamch('S');
        eps = ge.dlamch('P');

        // Set parameters
        rtl = Math.sqrt(eps);
        bsrtol = Math.sqrt(eps);

        // Treat case of 1x1 matrix for quick return
        if (n == 1) {
           if ((irange == allrng) ||
               ((irange == valrng) && (d[0] > vl[0]) && (d[0] <= vu[0])) ||
               ((irange == indrng) && (il == 1) && (iu == 1))) {
              m[0] = 1;
              w[0] = d[0];
              // The computation error of the eigenvalue is zero
              werr[0] = 0.0;
              wgap[0] = 0.0;
              iblock[0] = 1;
              indexw[0] = 1;
              gers[0] = d[0];
              gers[1] = d[0];
           } // if ((irange == allrng) ||
           // store the shift for the initial RRR, which is zero in this case
           e[0] = 0.0;
           return;
        } // if (n == 1)

        // General case: tridiagonal matrix of order > 1
  
        // Init werr[], wgap[]. Compute Gerschgorin intervals and spectral diameter.
        // Compute maximum off-diagonal entry and pivmin.
        gl = d[0];
        gu = d[0];
        eold = 0.0;
        emax = 0.0;
        e[n-1] = 0.0;
        for (i = 1; i <= n; i++) {
           werr[i-1] = 0.0;
           wgap[i-1] = 0.0;
           eabs = Math.abs(e[i-1]);
           if (eabs >= emax) {
              emax = eabs;
           }
           tmp1[0] = eabs + eold;
           gers[2*i-2] = d[i-1] - tmp1[0];
           gl =  Math.min(gl, gers[2*i-2]);
           gers[2*i-1] = d[i-1] + tmp1[0];
           gu = Math.max(gu, gers[2*i-1]);
           eold  = eabs;
        } // for (i= 1; i <= n; i++)
        // The minimum pivot allowed in the Sturm sequence for T
        pivmin[0] = safmin * Math.max(1.0, emax*emax);
        // Compute spectral diameter. The Gerschgorin bounds give an
        // estimate that is wrong by at most a factor of SQRT(2)
        spdiam = gu - gl;

        // Compute splitting points
        dlarra(n, d, e, e2, spltol, spdiam, nsplit, isplit, iinfo);

        // Can force use of bisection instead of faster DQDS.
        // Option left in the code for future multisection work.
        forceb = false;

        // Initialize usedqd, DQDS should be used for allrng unless someone
        // explicitly wants bisection.
        usedqd = ((irange == allrng) && (!forceb));

        if (usedqd) {
          //  Set interval [vl[0],vu[0]] that contains all eigenvalues
           vl[0] = gl;
           vu[0] = gu;
        }
        else {
           // We call dlarrd to find crude approximations to the eigenvalues
           // in the desired range. In case irange = indrng, we also obtain the
           // interval (vl[0],vu[0]] that contains all the wanted eigenvalues.
           // An interval [LEFT,RIGHT] has converged if
           // RIGHT-LEFT < rtol*MAX(ABS(LEFT),ABS(RIGHT))
           // dlarrd needs a work of size 4*n, iwork of size 3*n
           dlarrd(range, 'B', n, vl[0], vu[0], il, iu, gers,
                            bsrtol, d, e, e2, pivmin[0], nsplit[0], isplit,
                            mm, w, werr, vl, vu, iblock, indexw,
                            work, iwork, iinfo);
           if(iinfo[0] != 0) {
              info[0] = -1;
              return;
           }
           // Make sure that the entries M+1 to N in W, WERR, IBLOCK, INDEXW are 0
           for (i = mm[0]; i < n; i++) {
              w[i] = 0.0;
              werr[i] = 0.0;
              iblock[i] = 0;
              indexw[i] = 0;
           }
        }


  
        // Loop over unreduced blocks
        ibegin = 1;
        wbegin = 1;
        for (jblk = 1; jblk <= nsplit[0]; jblk++) {
           iend = isplit[jblk-1];
           in = iend - ibegin + 1;

           // 1 X 1 block
           if (in == 1) {
              if ( (irange == allrng) || ( (irange == valrng) &&
                 ( d[ibegin-1] > vl[0]) && (d[ibegin] <= vu[0]) )
                 || ( (irange == indrng) && (iblock[wbegin-1] == jblk))) {
                 m[0] = m[0] + 1;
                 w[m[0]-1] = d[ibegin-1];
                 werr[m[0]-1] = 0.0;
                 // The gap for a single block doesn't matter for the later
                 // algorithm and is assigned an arbitrary large value
                 wgap[m[0]-1] = 0.0;
                 iblock[m[0]-1] = jblk;
                 indexw[m[0]-1] = 1;
                 wbegin = wbegin + 1;
              } // if ( (irange == allrng) || ( (irange == valrng) &&
              // e[iend-1] holds the shift for the initial RRR
              e[iend-1] = 0.0;
              ibegin = iend + 1;
              continue;
           } // if (in == 1)
   
           // Blocks of size larger than 1x1
   
           // e[iend-1] will hold the shift for the initial RRR, for now set it =0
           e[iend-1] = 0.0;
   
           // Find local outer bounds gl,gu for the block
           gl = d[ibegin-1];
           gu = d[ibegin-1];
           for (i = ibegin; i <= iend; i++) {
              gl = Math.min(gers[2*i-2], gl);
              gu = Math.max(gers[2*i-1], gu);
           }
           spdiam = gu - gl;

           if (!((irange == allrng) && (!forceb))) {
              // Count the number of eigenvalues in the current block.
              mb = 0;
              for (i = wbegin; i <= mm[0]; i++) {
                 if (iblock[i-1] == jblk) {
                    mb = mb + 1;
                 }
                 else {
                    break;
                 }
              } // for (i = wbegin; i <= mm[0]; i++)

              if (mb == 0) {
                 // No eigenvalue in the current block lies in the desired range
                 // e[iend-1] holds the shift for the initial RRR
                 e[iend-1] = 0.0;
                 ibegin = iend + 1;
                 continue;
              } // if (mb == 0)
              else {

                 // Decide whether dqds or bisection is more efficient
                 usedqd = ((mb > fac*in) && (!forceb));
                 wend = wbegin + mb - 1;
                 // Calculate gaps for the current block
                 // In later stages, when representations for individual
                 // eigenvalues are different, we use sigma = e[iend-1].
                 sigma = 0.0;
                 for (i = wbegin; i <= wend-1; i++) {
                    wgap[i-1] = Math.max(0.0, w[i]-werr[i] - (w[i-1]+werr[i-1]) );
                 } // for (i = wbegin; i <= wend-1; i++)
                 wgap[wend-1] = Math.max(0.0, vu[0] - sigma - (w[wend-1]+werr[wend-1]));
                 // Find local index of the first and last desired evalue.
                 indl = indexw[wbegin-1];
                 indu = indexw[wend-1];
              } // else
           } // if (!((irange == allrng) && (!forceb)))
           if (( (irange == allrng) && (!forceb)) || usedqd) {
              // Case of DQDS
              // Find approximations to the extremal eigenvalues of the block
              daux = new double[in];
              for (i = 0; i < in; i++) {
                  daux[i] = d[ibegin-1+i];
              }
              e2aux = new double[in-1];
              for (i = 0; i < in-1; i++) {
                  e2aux[i] = e2[ibegin-1+i];
              }
              dlarrk(in, 1, gl, gu, daux, e2aux, pivmin[0], rtl, tmp, tmp1, iinfo);
              if (iinfo[0] != 0) {
                 info[0] = -1;
                 return;
              }
              isleft = Math.max(gl, tmp[0] - tmp1[0] - 100.0 * eps * Math.abs(tmp[0] - tmp1[0]));

              dlarrk(in, in, gl, gu, daux, e2aux, pivmin[0], rtl, tmp, tmp1, iinfo);
              if (iinfo[0] != 0) {
                 info[0] = -1;
                 return;
              }
              isrght = Math.min(gu, tmp[0] + tmp1[0] + 100.0 * eps * Math.abs(tmp[0] + tmp1[0]));
              // Improve the estimate of the spectral diameter
              spdiam = isrght - isleft;
           } // if (( (irange == allrng) && (!forceb)) || usedqd)
           else {
              // Case of bisection
              // Find approximations to the wanted extremal eigenvalues
              isleft = Math.max(gl, w[wbegin-1] - werr[wbegin-1]
                          - 100.0 * eps * Math.abs(w[wbegin-1]- werr[wbegin-1] ));
              isrght = Math.min(gu, w[wend-1] + werr[wend-1]
                          + 100.0 * eps * Math.abs(w[wend-1]+ werr[wend-1]));
           }


           // Decide whether the base representation for the current block
           // L_JBLK D_JBLK L_JBLK^T = T_JBLK - sigma_JBLK I
           // should be on the left or the right end of the current block.
           // The strategy is to shift to the end which is "more populated"
           //  Furthermore, decide whether to use DQDS for the computation of
           // the eigenvalue approximations at the end of dlarre or bisection.
           // dqds is chosen if all eigenvalues are desired or the number of
           // eigenvalues to be computed is large compared to the blocksize.
           if ( (irange == allrng) && (!forceb)) {
              // If all the eigenvalues have to be computed, we use dqd
              usedqd = true;
              // indl is the local index of the first eigenvalue to compute
              indl = 1;
              indu = in;
              // mb =  number of eigenvalues to compute
              mb = in;
              wend = wbegin + mb - 1;
              // Define 1/4 and 3/4 points of the spectrum
              s1 = isleft + 0.25 * spdiam;
              s2 = isrght - 0.25 * spdiam;
           } // if ( (irange == allrng) && (!forceb))
           else {
              // dlarrd has computed iblock and indexw for each eigenvalue approximation.
              // choose sigma
              if (usedqd) {
                 s1 = isleft + 0.25 * spdiam;
                 s2 = isrght - 0.25 * spdiam;
              }
              else {
                 tmp[0] = Math.min(isrght,vu[0]) -  Math.max(isleft,vl[0]);
                 s1 =  Math.max(isleft,vl[0]) + 0.25 * tmp[0];
                 s2 =  Math.min(isrght,vu[0]) - 0.25 * tmp[0];
              }
           }

           // Compute the negcount at the 1/4 and 3/4 points
           if (mb > 1) {
               daux = new double[in];
               eaux = new double[in];
               for (i = 0; i < in; i++) {
                   daux[i] = d[ibegin-1+i];
                   eaux[i] = e[ibegin-1+i];
               }
               dlarrc( 'T', in, s1, s2, daux, eaux, pivmin[0], cnt, cnt1, cnt2, iinfo);
           }

           if (mb == 1) {
              sigma = gl;
              sgndef = 1.0;
           }
           else if (cnt1[0] - indl >= indu - cnt2[0]) {
              if ( (irange == allrng) && (!forceb) ) {
                 sigma = Math.max(isleft,gl);
              }
              else if (usedqd) {
                 // use Gerschgorin bound as shift to get pos def matrix for dqds
                 sigma = isleft;
              }
              else {
                 // use approximation of the first desired eigenvalue of the block as shift
                 sigma = Math.max(isleft, vl[0]);
              }
              sgndef = 1.0;
           } // else if (cnt1[0] - indl >= indu - cnt2[0])
           else {
              if ( (irange == allrng) && (!forceb) ) {
                 sigma = Math.min(isrght, gu);
              }
              else if (usedqd) {
                 // use Gerschgorin bound as shift to get neg def matrix for dqds
                 sigma = isrght;
              }
              else {
                 // use approximation of the first desired eigenvalue of the block as shift
                 sigma = Math.min(isrght, vu[0]);
              }
              sgndef = -1.0;
           }


           // An initial sigma has been chosen that will be used for computing
           // T - sigma I = L D L^T
           // Define the increment tau of the shift in case the initial shift
           // needs to be refined to obtain a factorization with not too much
           // element growth.
           if (usedqd) {
              // The initial sigma was to the outer end of the spectrum
              // the matrix is definite and we need not retreat.
              tau = spdiam*eps*n + 2.0*pivmin[0];
              tau = Math.max(tau, 2.0*eps*Math.abs(sigma));
           }
           else {
              if (mb > 1) {
                 clwdth = w[wend-1] + werr[wend-1] - w[wbegin-1] - werr[wbegin-1];
                 avgap = Math.abs(clwdth /(double)(wend-wbegin));
                 if(sgndef == 1.0) {
                    tau = 0.5*Math.max(wgap[wbegin-1], avgap);
                    tau = Math.max(tau,werr[wbegin-1]);
                 }
                 else {
                    tau = 0.5*Math.max(wgap[wend-2], avgap);
                    tau = Math.max(tau,werr[wend-1]);
                 }
              }
              else {
                 tau = werr[wbegin-1];
              }
           }
   
           for (idum = 1; idum <= maxtry; idum++) {
              // Compute L d L^T factorization of tridiagonal matrix T - sigma I.
              // Store d in work[0:in-1], L in work[in:2*in-1], and reciprocals of
              // pivots in work[2*in:3*in-1]
              dpivot = d[ibegin-1] - sigma;
              work[0] = dpivot;
              dmax = Math.abs(work[0]);
              j = ibegin;
              for (i = 1; i <= in-1; i++) {
                 work[2*in+i-1] = 1.0 / work[i-1];
                 tmp[0] = e[j-1]*work[2*in+i-1];
                 work[in+i-1] = tmp[0];
                 dpivot = ( d[j]-sigma) - tmp[0]*e[j-1];
                 work[i] = dpivot;
                 dmax = Math.max(dmax, Math.abs(dpivot));
                 j = j + 1;
              } // for (i = 1; i <= in-1; i++)
              // check for element growth
              if (dmax > maxgrowth*spdiam) {
                 norep = true;
              }
              else {
                 norep = false;
              }
              if (usedqd && !norep) {
                 // Ensure the definiteness of the representation
                 // All entries of d (of L d L^T) must have the same sign
                 for (i = 0; i < in; i++) {
                    tmp[0] = sgndef * work[i];
                    if (tmp[0] < 0.0) {
                        norep = true;
                    }
                 } // for (i = 0; i < in; i++)
              } // if (usedqd && !norep)
              if (norep) {
                 // Note that in the case of irange = allrng, we use the Gerschgorin
                 // shift which makes the matrix definite. So we should end up
                 // here really only in the case of irange = valrng or indrng.
                 if (idum == maxtry-1) {
                    if (sgndef == 1.0) {
                       // The fudged Gerschgorin shift should succeed
                       sigma = gl - fudge*spdiam*eps*n - fudge*2.0*pivmin[0];
                    } // if (sgndef == 1.0)
                    else {
                       sigma = gu + fudge*spdiam*eps*n + fudge*2.0*pivmin[0];
                    }
                 } // if (idum == maxtry-1)
                 else {
                    sigma = sigma - sgndef * tau;
                    tau = 2.0 * tau;
                 }
              } // if (norep)
              else {
                 // an initial RRR is found
                 leave = false;
                 break;
              }
           } // for (idum = 1; idum <= maxtry; idum++)
           // if the program reaches this point, no base representation could be
           // found in maxtry iterations.
           if (leave) {
               info[0] = 2;
               return;
           }

           // At this point, we have found an initial base representation
           // T - sigma I = L d L^T with not too much element growth.
           // Store the shift.
           e[iend-1] = sigma;
           // Store d and L.
           for (i = 0; i < in; i++) {
               d[ibegin-1+i] = work[i];
           }
           for (i = 0; i < in-1; i++) {
               e[ibegin-1+i] = work[in+i];
           }


           if (mb > 1) {
   
              // Perturb each entry of the base representation by a small
              // (but random) relative amount to overcome difficulties with
              // glued matrices.
   
              for (i = 0; i < 4; i++) {
                 iseed[i] = 1;
              }

              ge.dlarnv(2, iseed, 2*in-1, work);
              for (i = 0; i < in-1; i++) {
                 d[ibegin+i-1] = d[ibegin+i-1]*(1.0+eps*pert*work[i]);
                 e[ibegin+i-1] = e[ibegin+i-1]*(1.0+eps*pert*work[in+i]);
              } // for (i = 0; i < in-1; i++)
              d[iend-1] = d[iend-1]*(1.0+eps*4.0*work[in-1]);
   
           } // if (mb > 1)
   
           // Don't update the Gerschgorin intervals because keeping track
           // of the updates would be too much work in dlarrv.
           // We update w instead and use it to locate the proper Gerschgorin
           // intervals.

           // Compute the required eigenvalues of L D L' by bisection or dqds
           if (!usedqd) {
              // If dlarrd has been used, shift the eigenvalue approximations
              // according to their representation. This is necessary for
              // a uniform DLARRV since dqds computes eigenvalues of the
              // shifted representation. In dlarrv, w will always hold the
              // UNshifted eigenvalue approximation.
              for (j = wbegin; j <= wend; j++) {
                 w[j-1] = w[j-1] - sigma;
                 werr[j-1] = werr[j-1] + Math.abs(w[j-1]) * eps;
              } // for (j = wbegin; j <= wend; j++)
              // call dlarrb to reduce eigenvalue error of the approximations
              // from dlarrd
              for (i = ibegin; i <= iend-1; i++) {
                 work[i-1] = d[i-1] * e[i-1] * e[i-1];
              }
              // use bisection to find EV from indl to indu
              daux = new double[in];
              for (i = 0; i < in; i++) {
                  daux[i] = d[ibegin-1+i];
              }
              lld = new double[in-1];
              for (i = 0; i < in-1; i++) {
                  lld[i] = work[ibegin-1+i];
              }
              waux = new double[in];
              for (i = 0; i < in; i++) {
                  waux[i] = w[wbegin-1+i];
              }
              wgapaux = new double[in-1];
              for (i = 0; i < in-1; i++) {
                  wgapaux[i] = wgap[wbegin-1+i];
              }
              werraux = new double[in];
              for (i = 0; i < in; i++) {
                  werraux[i] = werr[wbegin-1+i];
              }
              workaux = new double[2*in];
              dlarrb(in, daux, lld, indl, indu, rtol1, rtol2, indl-1,
                     waux, wgapaux, werraux, workaux, iwork, pivmin[0], 
                     spdiam, in, iinfo);
              for (i = 0; i < in; i++) {
                  w[wbegin-1+i] = waux[i];
              }
              for (i = 0; i < in-1; i++) {
                  wgap[wbegin-1+i] = wgapaux[i];
              }
              for (i = 0; i < in; i++) {
                  werr[wbegin-1+i] = werraux[i];
              }
              if (iinfo[0] != 0) {
                 info[0] = -4;
                 return;
              }
              // dlarrb computes all gaps correctly except for the last one
              // Record distance to vu[0]/gu
              wgap[wend-1] = Math.max(0.0, (vu[0]-sigma) - (w[wend-1] + werr[wend-1]));
              for (i = indl; i <= indu; i++) {
                 m[0] = m[0] + 1;
                 iblock[m[0]-1] = jblk;
                 indexw[m[0]-1] = i;
              } // for (i = indl; i <= indu; i++)
           }
           else {
              // Call dqds to get all eigs (and then possibly delete unwanted
              // eigenvalues).
              // Note that dqds finds the eigenvalues of the L D L^T representation
              // of T to high relative accuracy. High relative accuracy
              // might be lost when the shift of the RRR is subtracted to obtain
              // the eigenvalues of T. However, T is not guaranteed to define its
              // eigenvalues to high relative accuracy anyway.
              // Set rtol to the order of the tolerance used in dlasq2
              // This is an ESTIMATED error, the worst case bound is 4*n*eps
              // which is usually too large and requires unnecessary work to be
              // done by bisection when computing the eigenvectors
              rtol = Math.log((double)in) * 4.0 * eps;
              j = ibegin;
              for (i = 1; i <= in - 1; i++) {
                 work[2*i-2] = Math.abs(d[j-1]);
                 work[2*i-1] = e[j-1]*e[j-1]*work[2*i-2];
                 j = j + 1;
              } // for (i = 1; i <= in - 1; i++)
              work[2*in-2] = Math.abs(d[iend-1]);
              work[2*in-1] = 0.0;
              dlasq2(in, work, iinfo);
              if (iinfo[0] != 0) {
                 // If iinfo[0] = -5 then an index is part of a tight cluster
                 // and should be changed. The index is in iwork[0] and the
                 // gap is in work[n]
                 info[0] = -5;
                 return;
              }
              else {
                 // Test that all eigenvalues are positive as expected
                 for (i = 0; i < in; i++) {
                    if (work[i] < 0.0) {
                       info[0] = -6;
                       return;
                    }
                 } // for (i = 0; i < in; i++)
              }
              if (sgndef > 0.0) {
                 for (i = indl; i <= indu; i++) {
                    m[0] = m[0] + 1;
                    w[m[0]-1] = work[in-i];
                    iblock[m[0]-1] = jblk;
                    indexw[m[0]-1] = i;
                 } // for (i = indl; i <= indu; i++)
              } // if (sgndef > 0.0)
              else {
                 for (i = indl; i <= indu; i++) {
                    m[0] = m[0] + 1;
                    w[m[0]-1] = -work[i-1];
                    iblock[m[0]-1] = jblk;
                    indexw[m[0]-1] = i;
                 } // for (i = indl; i <= indu; i++)
              }

              for (i = m[0] - mb + 1; i <= m[0]; i++) {
                 // the value of rtol below should be the tolerance in dlasq2
                 werr[i-1] = rtol * Math.abs(w[i-1]);
              } // for (i = m[0] - mb + 1; i <= m[0]; i++)
              for (i = m[0] - mb + 1; i <= m[0] - 1; i++) {
                 // compute the right gap between the intervals
                 wgap[i-1] = Math.max(0.0, w[i]-werr[i] - (w[i-1]+werr[i-1]));
              } // for (i = m[0] - mb + 1; i <= m[0] - 1; i++)
              wgap[m[0]-1] = Math.max(0.0, (vu[0]-sigma ) - (w[m[0]-1] + werr[m[0]-1]));
           }
           // proceed with next block
           ibegin = iend + 1;
           wbegin = wend + 1;
        } // for (jblk = 1; jblk <= nsplit[0]; jblk++)
  

        return;
  } // dlarre
  
  /*> \brief \b DLARRF finds a new relatively robust representation such that at least one of the eigenvalues is relatively isolated.
  *
  * =========== DOCUMENTATION ===========
  *
  * Online html documentation available at
  * http://www.netlib.org/lapack/explore-html/
  *
  *> \htmlonly
  *> Download DLARRF + dependencies
  *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlarrf.f">
  *> [TGZ]</a>
  *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlarrf.f">
  *> [ZIP]</a>
  *> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlarrf.f">
  *> [TXT]</a>
  *> \endhtmlonly
  *
  * Definition:
  * ===========
  *
  * SUBROUTINE DLARRF( N, D, L, LD, CLSTRT, CLEND,
  * W, WGAP, WERR,
  * SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA,
  * DPLUS, LPLUS, WORK, INFO )
  *
  * .. Scalar Arguments ..
  * INTEGER CLSTRT, CLEND, INFO, N
  * DOUBLE PRECISION CLGAPL, CLGAPR, PIVMIN, SIGMA, SPDIAM
  * ..
  * .. Array Arguments ..
  * DOUBLE PRECISION D( * ), DPLUS( * ), L( * ), LD( * ),
  * $ LPLUS( * ), W( * ), WGAP( * ), WERR( * ), WORK( * )
  * ..
  *
  *
  *> \par Purpose:
  * =============
  *>
  *> \verbatim
  *>
  *> Given the initial representation L D L^T and its cluster of close
  *> eigenvalues (in a relative measure), W( CLSTRT ), W( CLSTRT+1 ), ...
  *> W( CLEND ), DLARRF finds a new relatively robust representation
  *> L D L^T - SIGMA I = L(+) D(+) L(+)^T such that at least one of the
  *> eigenvalues of L(+) D(+) L(+)^T is relatively isolated.
  *> \endverbatim
  *
  * Arguments:
  * ==========
  *
  *> \param[in] N
  *> \verbatim
  *> N is INTEGER
  *> The order of the matrix (subblock, if the matrix splitted).
  *> \endverbatim
  *>
  *> \param[in] D
  *> \verbatim
  *> D is DOUBLE PRECISION array, dimension (N)
  *> The N diagonal elements of the diagonal matrix D.
  *> \endverbatim
  *>
  *> \param[in] L
  *> \verbatim
  *> L is DOUBLE PRECISION array, dimension (N-1)
  *> The (N-1) subdiagonal elements of the unit bidiagonal
  *> matrix L.
  *> \endverbatim
  *>
  *> \param[in] LD
  *> \verbatim
  *> LD is DOUBLE PRECISION array, dimension (N-1)
  *> The (N-1) elements L(i)*D(i).
  *> \endverbatim
  *>
  *> \param[in] CLSTRT
  *> \verbatim
  *> CLSTRT is INTEGER
  *> The index of the first eigenvalue in the cluster.
  *> \endverbatim
  *>
  *> \param[in] CLEND
  *> \verbatim
  *> CLEND is INTEGER
  *> The index of the last eigenvalue in the cluster.
  *> \endverbatim
  *>
  *> \param[in] W
  *> \verbatim
  *> W is DOUBLE PRECISION array, dimension
  *> dimension is >= (CLEND-CLSTRT+1)
  *> The eigenvalue APPROXIMATIONS of L D L^T in ascending order.
  *> W( CLSTRT ) through W( CLEND ) form the cluster of relatively
  *> close eigenalues.
  *> \endverbatim
  *>
  *> \param[in,out] WGAP
  *> \verbatim
  *> WGAP is DOUBLE PRECISION array, dimension
  *> dimension is >= (CLEND-CLSTRT+1)
  *> The separation from the right neighbor eigenvalue in W.
  *> \endverbatim
  *>
  *> \param[in] WERR
  *> \verbatim
  *> WERR is DOUBLE PRECISION array, dimension
  *> dimension is >= (CLEND-CLSTRT+1)
  *> WERR contain the semiwidth of the uncertainty
  *> interval of the corresponding eigenvalue APPROXIMATION in W
  *> \endverbatim
  *>
  *> \param[in] SPDIAM
  *> \verbatim
  *> SPDIAM is DOUBLE PRECISION
  *> estimate of the spectral diameter obtained from the
  *> Gerschgorin intervals
  *> \endverbatim
  *>
  *> \param[in] CLGAPL
  *> \verbatim
  *> CLGAPL is DOUBLE PRECISION
  *> \endverbatim
  *>
  *> \param[in] CLGAPR
  *> \verbatim
  *> CLGAPR is DOUBLE PRECISION
  *> absolute gap on each end of the cluster.
  *> Set by the calling routine to protect against shifts too close
  *> to eigenvalues outside the cluster.
  *> \endverbatim
  *>
  *> \param[in] PIVMIN
  *> \verbatim
  *> PIVMIN is DOUBLE PRECISION
  *> The minimum pivot allowed in the Sturm sequence.
  *> \endverbatim
  *>
  *> \param[out] SIGMA
  *> \verbatim
  *> SIGMA is DOUBLE PRECISION
  *> The shift used to form L(+) D(+) L(+)^T.
  *> \endverbatim
  *>
  *> \param[out] DPLUS
  *> \verbatim
  *> DPLUS is DOUBLE PRECISION array, dimension (N)
  *> The N diagonal elements of the diagonal matrix D(+).
  *> \endverbatim
  *>
  *> \param[out] LPLUS
  *> \verbatim
  *> LPLUS is DOUBLE PRECISION array, dimension (N-1)
  *> The first (N-1) elements of LPLUS contain the subdiagonal
  *> elements of the unit bidiagonal matrix L(+).
  *> \endverbatim
  *>
  *> \param[out] WORK
  *> \verbatim
  *> WORK is DOUBLE PRECISION array, dimension (2*N)
  *> Workspace.
  *> \endverbatim
  *>
  *> \param[out] INFO
  *> \verbatim
  *> INFO is INTEGER
  *> Signals processing OK (=0) or failure (=1)
  *> \endverbatim
  *
  * Authors:
  * ========
  *
  *> \author Univ. of Tennessee
  *> \author Univ. of California Berkeley
  *> \author Univ. of Colorado Denver
  *> \author NAG Ltd.
  *
  *> \date September 2012
  *
  *> \ingroup auxOTHERauxiliary
  *
  *> \par Contributors:
  * ==================
  *>
  *> Beresford Parlett, University of California, Berkeley, USA \n
  *> Jim Demmel, University of California, Berkeley, USA \n
  *> Inderjit Dhillon, University of Texas, Austin, USA \n
  *> Osni Marques, LBNL/NERSC, USA \n
  *> Christof Voemel, University of California, Berkeley, USA
  *
  * =====================================================================
  SUBROUTINE DLARRF( N, D, L, LD, CLSTRT, CLEND,
  $ W, WGAP, WERR,
  $ SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA,
  $ DPLUS, LPLUS, WORK, INFO )
  *
  * -- LAPACK auxiliary routine (version 3.4.2) --
  * -- LAPACK is a software package provided by Univ. of Tennessee, --
  * -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
  * September 2012
  *
  * .. Scalar Arguments ..
  INTEGER CLSTRT, CLEND, INFO, N
  DOUBLE PRECISION CLGAPL, CLGAPR, PIVMIN, SIGMA, SPDIAM
  * ..
  * .. Array Arguments ..
  DOUBLE PRECISION D( * ), DPLUS( * ), L( * ), LD( * ),
  $ LPLUS( * ), W( * ), WGAP( * ), WERR( * ), WORK( * )
  * ..
  *
  * =====================================================================
  *
  * .. Parameters ..
  DOUBLE PRECISION FOUR, MAXGROWTH1, MAXGROWTH2, ONE, QUART, TWO
  PARAMETER ( ONE = 1.0D0, TWO = 2.0D0, FOUR = 4.0D0,
  $ QUART = 0.25D0,
  $ MAXGROWTH1 = 8.D0,
  $ MAXGROWTH2 = 8.D0 )
  * ..
  * .. Local Scalars ..
  LOGICAL DORRR1, FORCER, NOFAIL, SAWNAN1, SAWNAN2, TRYRRR1
  INTEGER I, INDX, KTRY, KTRYMAX, SLEFT, SRIGHT, SHIFT
  PARAMETER ( KTRYMAX = 1, SLEFT = 1, SRIGHT = 2 )
  DOUBLE PRECISION AVGAP, BESTSHIFT, CLWDTH, EPS, FACT, FAIL,
  $ FAIL2, GROWTHBOUND, LDELTA, LDMAX, LSIGMA,
  $ MAX1, MAX2, MINGAP, OLDP, PROD, RDELTA, RDMAX,
  $ RRR1, RRR2, RSIGMA, S, SMLGROWTH, TMP, ZNM2
  * ..
  * .. External Functions ..
  LOGICAL DISNAN
  DOUBLE PRECISION DLAMCH
  EXTERNAL DISNAN, DLAMCH
  * ..
  * .. External Subroutines ..
  EXTERNAL DCOPY
  * ..
  * .. Intrinsic Functions ..
  INTRINSIC ABS
  * ..
  * .. Executable Statements ..
  *
  INFO = 0
  FACT = DBLE(2**KTRYMAX)
  EPS = DLAMCH( 'Precision' )
  SHIFT = 0
  FORCER = .FALSE.
  * Note that we cannot guarantee that for any of the shifts tried,
  * the factorization has a small or even moderate element growth.
  * There could be Ritz values at both ends of the cluster and despite
  * backing off, there are examples where all factorizations tried
  * (in IEEE mode, allowing zero pivots & infinities) have INFINITE
  * element growth.
  * For this reason, we should use PIVMIN in this subroutine so that at
  * least the L D L^T factorization exists. It can be checked afterwards
  * whether the element growth caused bad residuals/orthogonality.
  * Decide whether the code should accept the best among all
  * representations despite large element growth or signal INFO=1
  NOFAIL = .TRUE.
  *
  * Compute the average gap length of the cluster
  CLWDTH = ABS(W(CLEND)-W(CLSTRT)) + WERR(CLEND) + WERR(CLSTRT)
  AVGAP = CLWDTH / DBLE(CLEND-CLSTRT)
  MINGAP = MIN(CLGAPL, CLGAPR)
  * Initial values for shifts to both ends of cluster
  LSIGMA = MIN(W( CLSTRT ),W( CLEND )) - WERR( CLSTRT )
  RSIGMA = MAX(W( CLSTRT ),W( CLEND )) + WERR( CLEND )
  * Use a small fudge to make sure that we really shift to the outside
  LSIGMA = LSIGMA - ABS(LSIGMA)* FOUR * EPS
  RSIGMA = RSIGMA + ABS(RSIGMA)* FOUR * EPS
  * Compute upper bounds for how much to back off the initial shifts
  LDMAX = QUART * MINGAP + TWO * PIVMIN
  RDMAX = QUART * MINGAP + TWO * PIVMIN
  LDELTA = MAX(AVGAP,WGAP( CLSTRT ))/FACT
  RDELTA = MAX(AVGAP,WGAP( CLEND-1 ))/FACT
  *
  * Initialize the record of the best representation found
  *
  S = DLAMCH( 'S' )
  SMLGROWTH = ONE / S
  FAIL = DBLE(N-1)*MINGAP/(SPDIAM*EPS)
  FAIL2 = DBLE(N-1)*MINGAP/(SPDIAM*SQRT(EPS))
  BESTSHIFT = LSIGMA
  *
  * while (KTRY <= KTRYMAX)
  KTRY = 0
  GROWTHBOUND = MAXGROWTH1*SPDIAM
  5 CONTINUE
  SAWNAN1 = .FALSE.
  SAWNAN2 = .FALSE.
  * Ensure that we do not back off too much of the initial shifts
  LDELTA = MIN(LDMAX,LDELTA)
  RDELTA = MIN(RDMAX,RDELTA)
  * Compute the element growth when shifting to both ends of the cluster
  * accept the shift if there is no element growth at one of the two ends
  * Left end
  S = -LSIGMA
  DPLUS( 1 ) = D( 1 ) + S
  IF(ABS(DPLUS(1)).LT.PIVMIN) THEN
  DPLUS(1) = -PIVMIN
  * Need to set SAWNAN1 because refined RRR test should not be used
  * in this case
  SAWNAN1 = .TRUE.
  ENDIF
  MAX1 = ABS( DPLUS( 1 ) )
  DO 6 I = 1, N - 1
  LPLUS( I ) = LD( I ) / DPLUS( I )
  S = S*LPLUS( I )*L( I ) - LSIGMA
  DPLUS( I+1 ) = D( I+1 ) + S
  IF(ABS(DPLUS(I+1)).LT.PIVMIN) THEN
  DPLUS(I+1) = -PIVMIN
  * Need to set SAWNAN1 because refined RRR test should not be used
  * in this case
  SAWNAN1 = .TRUE.
  ENDIF
  MAX1 = MAX( MAX1,ABS(DPLUS(I+1)) )
  6 CONTINUE
  SAWNAN1 = SAWNAN1 .OR. DISNAN( MAX1 )
  IF( FORCER .OR.
  $ (MAX1.LE.GROWTHBOUND .AND. .NOT.SAWNAN1 ) ) THEN
  SIGMA = LSIGMA
  SHIFT = SLEFT
  GOTO 100
  ENDIF
  * Right end
  S = -RSIGMA
  WORK( 1 ) = D( 1 ) + S
  IF(ABS(WORK(1)).LT.PIVMIN) THEN
  WORK(1) = -PIVMIN
  * Need to set SAWNAN2 because refined RRR test should not be used
  * in this case
  SAWNAN2 = .TRUE.
  ENDIF
  MAX2 = ABS( WORK( 1 ) )
  DO 7 I = 1, N - 1
  WORK( N+I ) = LD( I ) / WORK( I )
  S = S*WORK( N+I )*L( I ) - RSIGMA
  WORK( I+1 ) = D( I+1 ) + S
  IF(ABS(WORK(I+1)).LT.PIVMIN) THEN
  WORK(I+1) = -PIVMIN
  * Need to set SAWNAN2 because refined RRR test should not be used
  * in this case
  SAWNAN2 = .TRUE.
  ENDIF
  MAX2 = MAX( MAX2,ABS(WORK(I+1)) )
  7 CONTINUE
  SAWNAN2 = SAWNAN2 .OR. DISNAN( MAX2 )
  IF( FORCER .OR.
  $ (MAX2.LE.GROWTHBOUND .AND. .NOT.SAWNAN2 ) ) THEN
  SIGMA = RSIGMA
  SHIFT = SRIGHT
  GOTO 100
  ENDIF
  * If we are at this point, both shifts led to too much element growth
  * Record the better of the two shifts (provided it didn't lead to NaN)
  IF(SAWNAN1.AND.SAWNAN2) THEN
  * both MAX1 and MAX2 are NaN
  GOTO 50
  ELSE
  IF( .NOT.SAWNAN1 ) THEN
  INDX = 1
  IF(MAX1.LE.SMLGROWTH) THEN
  SMLGROWTH = MAX1
  BESTSHIFT = LSIGMA
  ENDIF
  ENDIF
  IF( .NOT.SAWNAN2 ) THEN
  IF(SAWNAN1 .OR. MAX2.LE.MAX1) INDX = 2
  IF(MAX2.LE.SMLGROWTH) THEN
  SMLGROWTH = MAX2
  BESTSHIFT = RSIGMA
  ENDIF
  ENDIF
  ENDIF
  * If we are here, both the left and the right shift led to
  * element growth. If the element growth is moderate, then
  * we may still accept the representation, if it passes a
  * refined test for RRR. This test supposes that no NaN occurred.
  * Moreover, we use the refined RRR test only for isolated clusters.
  IF((CLWDTH.LT.MINGAP/DBLE(128)) .AND.
  $ (MIN(MAX1,MAX2).LT.FAIL2)
  $ .AND.(.NOT.SAWNAN1).AND.(.NOT.SAWNAN2)) THEN
  DORRR1 = .TRUE.
  ELSE
  DORRR1 = .FALSE.
  ENDIF
  TRYRRR1 = .TRUE.
  IF( TRYRRR1 .AND. DORRR1 ) THEN
  IF(INDX.EQ.1) THEN
  TMP = ABS( DPLUS( N ) )
  ZNM2 = ONE
  PROD = ONE
  OLDP = ONE
  DO 15 I = N-1, 1, -1
  IF( PROD .LE. EPS ) THEN
  PROD =
  $ ((DPLUS(I+1)*WORK(N+I+1))/(DPLUS(I)*WORK(N+I)))*OLDP
  ELSE
  PROD = PROD*ABS(WORK(N+I))
  END IF
  OLDP = PROD
  ZNM2 = ZNM2 + PROD**2
  TMP = MAX( TMP, ABS( DPLUS( I ) * PROD ))
  15 CONTINUE
  RRR1 = TMP/( SPDIAM * SQRT( ZNM2 ) )
  IF (RRR1.LE.MAXGROWTH2) THEN
  SIGMA = LSIGMA
  SHIFT = SLEFT
  GOTO 100
  ENDIF
  ELSE IF(INDX.EQ.2) THEN
  TMP = ABS( WORK( N ) )
  ZNM2 = ONE
  PROD = ONE
  OLDP = ONE
  DO 16 I = N-1, 1, -1
  IF( PROD .LE. EPS ) THEN
  PROD = ((WORK(I+1)*LPLUS(I+1))/(WORK(I)*LPLUS(I)))*OLDP
  ELSE
  PROD = PROD*ABS(LPLUS(I))
  END IF
  OLDP = PROD
  ZNM2 = ZNM2 + PROD**2
  TMP = MAX( TMP, ABS( WORK( I ) * PROD ))
  16 CONTINUE
  RRR2 = TMP/( SPDIAM * SQRT( ZNM2 ) )
  IF (RRR2.LE.MAXGROWTH2) THEN
  SIGMA = RSIGMA
  SHIFT = SRIGHT
  GOTO 100
  ENDIF
  END IF
  ENDIF
  50 CONTINUE
  IF (KTRY.LT.KTRYMAX) THEN
  * If we are here, both shifts failed also the RRR test.
  * Back off to the outside
  LSIGMA = MAX( LSIGMA - LDELTA,
  $ LSIGMA - LDMAX)
  RSIGMA = MIN( RSIGMA + RDELTA,
  $ RSIGMA + RDMAX )
  LDELTA = TWO * LDELTA
  RDELTA = TWO * RDELTA
  KTRY = KTRY + 1
  GOTO 5
  ELSE
  * None of the representations investigated satisfied our
  * criteria. Take the best one we found.
  IF((SMLGROWTH.LT.FAIL).OR.NOFAIL) THEN
  LSIGMA = BESTSHIFT
  RSIGMA = BESTSHIFT
  FORCER = .TRUE.
  GOTO 5
  ELSE
  INFO = 1
  RETURN
  ENDIF
  END IF
  100 CONTINUE
  IF (SHIFT.EQ.SLEFT) THEN
  ELSEIF (SHIFT.EQ.SRIGHT) THEN
  * store new L and D back into DPLUS, LPLUS
  CALL DCOPY( N, WORK, 1, DPLUS, 1 )
  CALL DCOPY( N-1, WORK(N+1), 1, LPLUS, 1 )
  ENDIF
  RETURN
  *
  * End of DLARRF
  *
  END*/
  
  /** This is a port of version 3.4.0 LAPACK auxiliary routine dlarrk.  LAPACK is a software package provided by Univ. of Tennessee,    --
  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. November 2011
  dlarrk computes one eigenvalue of a symmetric tridiagonal
  matrix T to suitable accuracy. This is an auxiliary code to be
  called from dstemr.
  
  To avoid overflow, the matrix must be scaled so that its
  largest element is no greater than overflow**(1/2) * underflow**(1/4) in absolute value, and for greatest
  accuracy, it should not be much smaller than that.
  
  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
  Matrix", Report CS41, Computer Science Dept., Stanford
  University, July 21, 1966.
  
  @param n input int  The order of the tridiagonal matrix T.  n >= 0.
  @param iw input int The index of the eigenvalue to be returned.'
  @param gl input double A lower bound on the eigenvalue.
  @param gu input double An upper bound on the eigenvalue.
  @param d input double[] of dimension n. The n diagonal elements of the tridiagonal matrix T.
  @param e2 input double[] of dimension (n-1). The (n-1) squared off-diagonal elements of the tridiagonal matrix T.
  @param pivmin input double The minimum pivot allowed in the Sturm sequence for T.
  @param reltol input double The minimum relative width of an interval.  When an interval
         is narrower than reltol times the larger (in magnitude) endpoint, then it is considered to be
         sufficiently small, i.e., converged.  Note: this should always be at least radix*machine epsilon.
  @param w output double[] of dimension 1.  Output eigevvalue.
  @param werr output double[] of dimension 1.  The error bound on the corrsponding eigenvalue 
         approximation in w[0].
  @param info output int[] of dimension 1.
         = 0:       Eigenvalue converged
         = -1:      Eigenvalue did NOT converge
 */
  
  private void dlarrk(int n, int iw, double gl, double gu, double d[], double e2[], double pivmin,
                      double reltol, double w[], double werr[], int info[]) {
      
      // A "fudge factor" to widen the Gershgorgin intervals.  
      final double fudge = 2.0;
      int i;
      int it;
      int itmax;
      int negcnt;
      double atoli;
      double eps;
      double left;
      double mid;
      double right;
      double rtoli;
      double tmp1;
      double tmp2;
      double tnorm;
      
        // Get machine constants
        eps = ge.dlamch('P');

        tnorm = Math.max(Math.abs(gl), Math.abs(gu));
        rtoli = reltol;
        atoli = fudge*2.0*pivmin;

        itmax = (int)( (Math.log(tnorm+pivmin)-Math.log(pivmin) ) /
                   Math.log(2.0) ) + 2;

        info[0] = -1;

        left = gl - fudge*tnorm*eps*n - fudge*2.0*pivmin;
        right = gu + fudge*tnorm*eps*n + fudge*2.0*pivmin;
        it = 0;

        while (true) {
     
            // Check if interval converged or maximum number of iterations reached
      
            tmp1 = Math.abs(right - left);
            tmp2 = Math.max(Math.abs(right), Math.abs(left));
            if (tmp1 < Math.max(atoli, Math.max(pivmin, rtoli * tmp2) ) ) {
               info[0] = 0;
               break;
            }
            if (it > itmax) {
               break;
            }
    
      
            // Count number of negative pivots for mid-point
      
            it = it + 1;
            mid = 0.5 * (left + right);
            negcnt = 0;
            tmp1 = d[0] - mid;
            if (Math.abs(tmp1) < pivmin) {
               tmp1 = -pivmin;
            }
            if (tmp1 <= 0.0) {
               negcnt = negcnt + 1;
            }
      
            for (i = 2; i <= n; i++) {
               tmp1 = d[i-1] - e2[i-2] / tmp1 -mid;
               if (Math.abs(tmp1) < pivmin) {
                  tmp1 = -pivmin;
               } // if (Math.abs(tmp1) < pivmin)
               if (tmp1 <= 0.0) {
                  negcnt = negcnt + 1;
               }
            } // for (i = 2; i <= n; i++)
    
            if(negcnt >= iw) {
               right = mid;
            }
            else {
               left = mid;
            }
        } // while (true) 
   
        // Converged or maximum number of iterations reached
   
        w[0] = 0.5 * (left + right);
        werr[0] = 0.5 * Math.abs(right - left);

        return;
  } // dlarrk



  /** This is a port of version 3.4.0 LAPACK auxiliary routine dlarrr.  LAPACK is a software package provided by Univ. of Tennessee,    --
  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. November 2011
  Contributors:
 
  Beresford Parlett, University of California, Berkeley, USA
  Jim Demmel, University of California, Berkeley, USA
  Inderjit Dhillon, University of Texas, Austin, USA
  Osni Marques, LBNL/NERSC, USA
  Christof Voemel, University of California, Berkeley, USA
  
  Perform tests to decide whether the symmetric tridiagonal matrix T
  warrants expensive computations which guarantee high relative accuracy
  in the eigenvalues.
  
  @param n input int  The order of the matrix.  n > 0
  @param d input double[] of dimension n.
         The n diagonal elements of the tridiagonal matrix T.
  @param e (input/output) double[] of dimension n.
         On entry, the first (n-1) entries contain the subdiagonal elements of the tridiagonal matrix T;
         e[n-1] is set to zero.
  @param info output int[] of dimension 1.
         info[0] = 0 : The matrix warrants computations preserving relative accuracy.
         info[0] = 1 (default) : The matrix warrants computations guaranteeing only absolute accuracy.
  */
  private void dlarrr(int n, double d[], double e[], int info[]) {
        double relcond = 0.999;
        int i;
        boolean yesrel;
        double eps;
        double safmin;
        double smlnum;
        double rmin;
        double tmp;
        double tmp2;
        double offdig;
        double offdig2;
        
        // As a default, do NOT go for relative-accuracy preserving computations.
        info[0] = 1;

        safmin = ge.dlamch('S');
        eps = ge.dlamch('P');
        smlnum = safmin / eps;
        rmin = Math.sqrt(smlnum);

        // Tests for relative accuracy
  
        // Test for scaled diagonal dominance
        // Scale the diagonal entries to one and check whether the sum of the
        // off-diagonals is less than one
   
        // The sdd relative error bounds have a 1/(1- 2*x) factor in them,
        // x = max(offdig, offdig2), so when x is close to 1/2, no relative
        // accuracy is promised.  In the notation of the code fragment below,
        // 1/(1 - (offdig + offdig2)) is the condition number.
        // We don't think it is worth going into "sdd mode" unless the relative
        // condition number is reasonable, not 1/macheps.
        // The threshold should be compatible with other thresholds used in the
        // code. We set  offdig + offdig2 <= .999 =: relcond, it corresponds
        // to losing at most 3 decimal digits: 1 / (1 - (offdig + offdig2)) <= 1000
        // instead of the current offdig + offdig2 < 1
  
        yesrel = true;
        offdig = 0.0;
        tmp = Math.sqrt(Math.abs(d[0]));
        if (tmp < rmin) {
            yesrel = false;
        }
        if (yesrel) {
            for (i = 1; i < n; i++) {
               tmp2 = Math.sqrt(Math.abs(d[i]));
               if (tmp2 < rmin) {
                   yesrel = false;
               }
               if (!yesrel) {
                   break;
               }
               offdig2 = Math.abs(e[i-1])/(tmp*tmp2);
               if (offdig+offdig2 >= relcond) {
                   yesrel = false;
               }
               if (!yesrel) {
                   break;
               }
               tmp = tmp2;
               offdig = offdig2;
            } // if (i = 1; i < n; i++)
        } // if (yesrel)

        if (yesrel) {
           info[0] = 0;
           return;
        }
  

  
  //      *** MORE TO BE IMPLEMENTED ***
  

  
  //    Test if the lower bidiagonal matrix L from T = L D L^T
  //    (zero shift facto) is well conditioned
  

  
  //    Test if the upper bidiagonal matrix U from T = U D U^T
  //    (zero shift facto) is well conditioned.
  //    In this case, the matrix needs to be flipped and, at the end
  //    of the eigenvector computation, the flip needs to be applied
  //    to the computed eigenvectors (and the support)
  

  
        return;
  } // dlarrr
  
  /** This is a port of version 3.4.0 LAPACK auxiliary routine dlarrv.  LAPACK is a software package provided by Univ. of Tennessee,    --
  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. November 2011
  Contributors:
 
  Beresford Parlett, University of California, Berkeley, USA
  Jim Demmel, University of California, Berkeley, USA
  Inderjit Dhillon, University of Texas, Austin, USA
  Osni Marques, LBNL/NERSC, USA
  Christof Voemel, University of California, Berkeley, USA
  
  dlarrv computes the eigenvectors of the tridiagonal matrix
  T = L D L**T given L, D and APPROXIMATIONS to the eigenvalues of L D L**T.
  The input eigenvalues should have been computed by dlarre.
  
  @param n input int  The order of the matrix.  n >= 0
  @param vl input double  Lower bound of the interval that contains the desired eigenvalues.
  @param vu input double  Upper bound of the interval that contains the desired eigenvalues.  vl < vu.
            Needed to compute gaps on the left or right end of the extremal eigenvalues in the desired range.
  @param d input/output double[] of dimension n.  On entry, the n diagonal elements of the diagonal matrix d.
            On exit, d may be overwritten.
  @param l input/output double[] of dimension n.  On entry, the (n-1) subdiagonal elements of the unit 
           bidiagonal matrix l are in elements 0 to n-2 of l (if the matrix is not splitted.) At the end of
           each block is stored the corresponding shift as given by dlarre.  On exit, l is overwritten.
  @param pivmin input double  The minimum input allowed in the Sturm sequence.
  @param isplit input int[] of dimension n.  The splitting points, at which T breaks into blocks.
           The first block consists of rows/columns 0 to isplit[0]-1, the second of rows/columns isplit[0]
           through isplit[1]-1, etc.
  @param m input int  The total number of input eigenvalues.  0 <= m <= n.
  @param dol input int
  @param dou input int  If the user wants to compute only selected eigenvectors from all the eigenvalues 
           supplied, he can specify an index range dol:dou.  Or else the setting dol = 1, dou = m should
           be applied.  Note that dol and dou refer to the order in which the eigenvalues are stored in w.
           If the user wants to compute only selected eigenvalues, then the columns dol-2 to dou of the
           eigenvector space Z contain the computed eigenvectors.  All other columns of Z are set to 0.
  @param minrgp input double
  @param rtol1 input double
  @param rtol2 input double  Parameters for bisection.  An interval [left, right] has converged if
           right - left < max(rtol1*gap, rtol2 * max(|left|, |right|))
  @param w input/output double[] of dim n  The first m elements of w[] contain the approximate eigenvalues
           for which the eigenvectors are to be computed.  The eigenvalues should be grouped by split-off
           block and ordered from smallest to largest within the block (The output array w from dlarre is
           expected here).  Furthermore, they are with respect to the shift of the corresponding root 
           representation for their block.  On exit, w holds the eigenvalues of the unshifted matrix.
  @param werr input/output double[] of dimension n  The first m elements contain the semiwidth of the 
           uncertainty interval of the corresponding eigenvalue in w.
  @param wgap input/output of dimension n  The separation from the right neighbor eigenvalue in w.
  @param iblock input int[] of dimension n.  The indices of the blocks (submatrices) associated with
           the corresponding eigenvalues in w; iblock[i] = 1 if the eigenvalues w[i] belongs to the
           first block from the top, =2 if w[i] belongs to the second block, etc.
  @param indexw input int[] of dimension n.  The indices of the eigenvalues within each block (submatrix);
           for example, indexw[i] = 10 and iblock[i] = 2 imply that the i-th eigenvalue w[i] is the
           10-th eigenvalue in the second block.
  @param gers input double[] of dimension (2*n).  The n Gerschgorin intervals (the i-th Gerschgorin 
           interval is (gers(2*i-2), gers(2*i-1).  The Gerschgorin intervals should be computed from
           the original unshifted matrix.
  @param Z output double[][] of dimension (ldz, max(1,m))
           If info[0] = 0, the first m columns of Z contain the orthonormal eigenvectors of the matrix T
           corresponding to to the input eigenvalues, with the i-th column of Z holding the eigenvector
           associated with w[i].  Note: the user must ensure that at least max(1,m) columns are supplied
           in the array Z.
  @param ldz input int The leading dimension of the array Z.  ldz >= 1, and if jobz = 'V', ldz >= max(1,n).
  @param isuppz output int[] of dimension (2*max(1,m))  The support of the eigenvectors in Z, i.e., the 
           indices indicating the nonzero elements in Z.  The i-th eigenvector is nonzero only in elements
           isuppz[2*i-2] through isuppz[2*i-1].
  @param work output double[] of dimension (12*n)
  @param iwork output int of dimension (7*n)
  @param info output int[] of dimension 1
         = 0:  successful exit
         > 0:  A problem occured in dlarrv.
         < 0:  One of the called subroutines signaled an internal problem.
               Needs inspection of the corresponding parameter iinfo
               for further information.
  
         =-1:  Problem in dlarrb when refining a child's eigenvalues.
         =-2:  Problem in dlarrf when computing the RRR of a child.
               When a child is inside a tight cluster, it can be difficult
               to find an RRR. A partial remedy from the user's point of
               view is to make the parameter mingrp smaller and recompile.
               However, as the orthogonality of the computed vectors is
               proportional to 1/mingrp, the user should be aware that
               he might be trading in precision when he decreases MINRGP.
         =-3:  Problem in dlarrb when refining a single eigenvalue
               after the Rayleigh correction was rejected.
         = 5:  The Rayleigh Quotient Iteration failed to converge to
               full accuracy in maxitr steps.
  */
  
  private void dlarrv(int n, double vl, double vu, double d[], double l[], double pivmin, int isplit[],
                      int m, int dol, int dou, double minrgp, double rtol1, double rtol2, double w[],
                      double werr[], double wgap[], int iblock[], int indexw[], double gers[], double Z[][],
                      int ldz, int isuppz[], double work[], int iwork[], int info[]) {
  
 
        final int maxitr = 10;
        boolean eskip;
        boolean needbs;
        boolean stp2ii;
        boolean tryrqc;
        boolean usedbs;
        boolean usedrq;
        int done;
        int i;
        int ibegin;
        int idone;
        int iend;
        int ii;
        int iindc1;
        int iindc2;
        int iindr;
        int iindwk;
        int iinfo[] = new int[1];
        int im;
        int in;
        int indeig;
        int index;
        int indld;
        int indlld;
        int indwrk;
        int isupmn;
        int isupmx;
        int iter;
        int itmp1;
        int j;
        int jblk;
        int k;
        int miniwsize;
        int minwsize;
        int nclus;
        int ndepth;
        int negcnt;
        int newcls;
        int newfst;
        int newftt;
        int newlst;
        int newsiz;
        int offset;
        int oldcls;
        int oldfst;
        int oldien;
        int oldlst;
        int oldncl;
        int p;
        int parity;
        int q;
        int wbegin;
        int wend;
        int windex;
        int windmn;
        int windpl;
        int zfrom;
        int zto;
        int zusedl;
        int zusedu;
        int zusedw;
        double bstres;
        double bstw;
        double eps;
        double fudge;
        double gap;
        double gaptol;
        double gl;
        double gu;
        double lambda;
        double left;
        double lgap;
        double mingma;
        double nrminv;
        double resid;
        double rgap;
        double right;
        double rqcorr;
        double rqtol;
        double savgap;
        double sgndef;
        double sigma;
        double spdiam;
        double ssigma;
        double tau;
        double tmp;
        double tol;
        double ztz;
        double array[][];
        double vec[];
        double vec2[];
        double vec3[];
        double vec4[];
        double vec5[];
        double vec6[];
        int ivec[];
       
        
        // Executable Statements ..

        // The first n entries of work are reserved for the eigenvalues
        indld = n+1;
        indlld= 2*n+1;
        indwrk= 3*n+1;
        minwsize = 12 * n;

        for (i = 0; i < minwsize; i++) {
           work[i] = 0.0;
        }

        // iwork[iindr:iindr+n-1] hold the twist indices R for the factorization used to compute the FP vector
        iindr = 0;
        // iwork[iindc1:iindc2+n-1] are used to store the clusters of the current layer and the one above.
        iindc1 = n;
        iindc2 = 2*n;
        iindwk = 3*n + 1;

        miniwsize = 7 * n;
        for (i = 0; i < miniwsize; i++) {
           iwork[i] = 0;
        }

        zusedl = 1;
        if (dol > 1) {
           // Set lower bound for use of Z
           zusedl = dol-1;
        }
        zusedu = m;
        if (dou < m) {
           // Set lower bound for use of Z
           zusedu = dou+1;
        }
        // The width of the part of Z that is used
        zusedw = zusedu - zusedl + 1;

        array = new double[ldz][zusedw];
        for (i = 0; i < ldz; i++) {
            for (j = 0; j < zusedw; j++) {
                array[i][j] = Z[i][zusedl-1+j];
            }
        }
        ge.dlaset( 'F', n, zusedw, 0.0, 0.0, array, ldz);
        for (i = 0; i < ldz; i++) {
            for (j = 0; j < zusedw; j++) {
                Z[i][zusedl-1+j] = array[i][j];
            }
        }

        eps = ge.dlamch( 'P' );
        rqtol = 2.0 * eps;
  
        // Set expert flags for standard code.
        tryrqc = true;

        if (!((dol == 1) && (dou == m))) {
           // Only selected eigenpairs are computed. Since the other evalues
           // are not refined by RQ iteration, bisection has to compute to full accuracy.
           rtol1 = 4.0 * eps;
           rtol2 = 4.0 * eps;
        }

        // The entries wbegin-1:wend-1 in w, werr, wgap correspond to the
        // desired eigenvalues. The support of the nonzero eigenvector
        // entries is contained in the interval ibegin-1:iend-1.
        // Remark that if k eigenpairs are desired, then the eigenvectors
        // are stored in k contiguous columns of Z.

        // done is the number of eigenvectors already computed
        done = 0;
        ibegin = 1;
        wbegin = 1;
        for (jblk = 1; jblk <= iblock[m-1]; jblk++) {
           iend = isplit[jblk-1];
           sigma = l[iend-1];
           // Find the eigenvectors of the submatrix indexed ibegin-1 through iend-1.
           wend = wbegin - 1;
           while((wend < m) && (iblock[wend] == jblk)) {
               wend++;
           }
           if (wend < wbegin) {
              ibegin = iend + 1;
              continue;
           }
           else if ( (wend < dol) || (wbegin > dou) ) {
              ibegin = iend + 1;
              wbegin = wend + 1;
              continue;
           }

           // Find local spectral diameter of the block
           gl = gers[ 2*ibegin-2];
           gu = gers[ 2*ibegin-1];
           for (i = ibegin+1; i <= iend; i++) {
              gl = Math.min(gers[2*i-2], gl);
              gu = Math.max(gers[2*i-1], gu);
           }
           spdiam = gu - gl;

           // oldien is the last index of the previous block
           oldien = ibegin - 1;
           // Calculate the size of the current block
           in = iend - ibegin + 1;
           // The number of eigenvalues in the current block
           im = wend - wbegin + 1;

           // This is for a 1x1 block
           if (ibegin == iend) {
              done++;
              Z[ibegin-1][wbegin-1] = 1.0;
              isuppz[2*wbegin-2] = ibegin;
              isuppz[2*wbegin-1] = ibegin;
              w[wbegin-1] = w[wbegin-1] + sigma;
              work[wbegin-1] = w[wbegin-1];
              ibegin = iend + 1;
              wbegin++;
              continue;
           } // if (ibegin == iend)

           // The desired (shifted) eigenvalues are stored in w[wbegin-1:wend-1]
           // Note that these can be approximations, in this case, the corresp.
           // entries of werr give the size of the uncertainty interval.
           // The eigenvalue approximations will be refined when necessary as
           // high relative accuracy is required for the computation of the
           // corresponding eigenvectors.
           for (i = 0; i < im; i++) {
               work[wbegin-1+i] = w[wbegin-1+i];
           }

           // We store in w the eigenvalue approximations w.r.t. the original matrix T.
           for (i = 0; i < im; i++) {
              w[wbegin+i-1] = w[wbegin+i-1]+sigma;
           }


           // ndepth is the current depth of the representation tree
           ndepth = 0;
           // parity is either 1 or 0
           parity = 1;
           // nclus is the number of clusters for the next level of the
           // representation tree, we start with nclus = 1 for the root
           nclus = 1;
           iwork[iindc1] = 1;
           iwork[iindc1+1] = im;

           // idone is the number of eigenvectors already computed in the current block
           idone = 0;
           // loop while( IDONE.LT.IM )
           // generate the representation tree for the current block and
           // compute the eigenvectors
           while (idone < im) {
              // This is a crude protection against infinitely deep trees
              if (ndepth > m) {
                 info[0] = -2;
                 return;
              }
              //  breadth first processing of the current level of the representation
              // tree: oldncl = number of clusters on current level
              oldncl = nclus;
              // reset nclus to count the number of child clusters
              nclus = 0;
   
              parity = 1 - parity;
              if (parity == 0) {
                 oldcls = iindc1;
                 newcls = iindc2;
              }
              else {
                 oldcls = iindc2;
                 newcls = iindc1;
              }
              // Process the clusters on the current level
              for (i = 1; i <= oldncl; i++) {
                 j = oldcls + 2*i;
                 // oldfst, oldlst = first, last index of current cluster.
                                  // cluster indices start with 1 and are relative
                                  // to wbegin when accessing w, wgap, werr, Z
                 oldfst = iwork[j-2];
                 oldlst = iwork[j-1];
                 if (ndepth > 0) {
                    // Retrieve relatively robust representation (RRR) of cluster
                    // that has been computed at the previous level
                    // The RRR is stored in Z and overwritten once the eigenvectors
                    // have been computed or when the cluster is refined

                    if ((dol == 1) && (dou == m)) {
                       // Get representation from location of the leftmost evalue of the cluster
                       j = wbegin + oldfst - 1;
                    }
                    else {
                       if (wbegin+oldfst-1 < dol) {
                          // Get representation from the left end of Z array
                          j = dol - 1;
                       }
                       else if (wbegin+oldfst-1 > dou) {
                          // Get representation from the right end of Z array
                          j = dou;
                       }
                       else {
                          j = wbegin + oldfst - 1;
                       }
                    }
                    
                    for (index = 0; index < in; index++) {
                        d[ibegin-1+index] = Z[ibegin-1+index][j-1];
                    }
                    for (index = 0; index < in-1; index++) {
                        l[ibegin-1+index] = Z[ibegin-1+index][j];
                    }
                    sigma = Z[iend-1][j];

                    //Set the corresponding entries in Z to zero
                    array = new double[ldz][2];
                    for (index = 0; index < ldz; index++) {
                        array[index][0] = Z[ibegin-1+index][j-1];
                        array[index][1] = Z[ibegin-1+index][j];
                    }
                    ge.dlaset( 'F', in, 2, 0.0, 0.0, array, ldz);
                    for (index = 0; index < ldz; index++) {
                        Z[ibegin-1+index][j-1] = array[index][0];
                        Z[ibegin-1+index][j] = array[index][1];
                    }
                 } // if (ndepth > 0)

                 // Compute DL and DLL of current RRR
                 for (j = ibegin-1; j <= iend-2; j++) {
                    tmp = d[j]*l[j];
                    work[indld-1+j] = tmp;
                    work[indlld-1+j] = tmp*l[j];
                 }

                 if (ndepth > 0) {
                    // p and q are index of the first and last eigenvalue to compute within the current block
                    p = indexw[wbegin-2+oldfst];
                    q = indexw[wbegin-2+oldlst];
                    // Offset for the arrays work, wgap and werr, i.e., the p-OFFSET
                    // through the q-OFFSET elements of these arrays are to be used.
                    //  offset = p-oldfst
                    offset = indexw[wbegin-1] - 1;
                    // perform limited bisection (if necessary) to get approximate
                    // eigenvalues to the precision needed.
                    vec = new double[in];
                    for (index = 0; index < in; index++) {
                        vec[index] = d[ibegin-1+index];
                    }
                    vec2 = new double[in-1];
                    for (index = 0; index < in-1; index++) {
                        vec2[index] = work[indlld+ibegin-2+index];
                    }
                    vec3 =  new double[in];
                    for (index = 0; index < in; index++) {
                        vec3[index] = work[wbegin-1+index];
                    }
                    vec4 = new double[in-1];
                    for (index = 0; index <in-1; index++) {
                        vec4[index] = wgap[wbegin-1+index];
                    }
                    vec5 = new double[in];
                    for (index = 0; index < in; index++) {
                        vec5[index] = werr[wbegin-1+index];
                    }
                    vec6 = new double[2*in];
                    ivec = new int[2*in];
                    dlarrb(in, vec, vec2, p, q, rtol1, rtol2, offset,
                           vec3, vec4, vec5, vec6, ivec, pivmin, spdiam, in, iinfo);
                    for (index = 0; index < in; index++) {
                        work[wbegin-1+index] = vec3[index];
                    }
                    for (index = 0; index <in-1; index++) {
                        wgap[wbegin-1+index] = vec4[index];
                    }
                    for (index = 0; index < in; index++) {
                        werr[wbegin-1+index] = vec5[index];
                    }
                    if (iinfo[0] != 0) {
                       info[0] = -1;
                       return;
                    }
                    // We also recompute the extremal gaps. w holds all eigenvalues
                    // of the unshifted matrix and must be used for computation
                    // of wgap, the entries of work might stem from RRRs with
                    // different shifts. The gaps from wbegin-1+oldfst to
                    // wbegin-1+oldlst are correctly computed in dlarrb.
                    // However, we only allow the gaps to become greater since
                    // this is what should happen when we decrease werr
                    if (oldfst > 1) {
                       wgap[wbegin+oldfst-3] = Math.max(wgap[wbegin+oldfst-3],
                         w[wbegin+oldfst-2]-werr[wbegin+oldfst-2]- w[wbegin+oldfst-3]-werr[wbegin+oldfst-3] );
                    }
                    if (wbegin + oldlst -1 < wend) {
                       wgap[wbegin+oldlst-2] = Math.max(wgap[wbegin+oldlst-2],
                           w[wbegin+oldlst-1]-werr[wbegin+oldlst-1] - w[wbegin+oldlst-2]-werr[wbegin+oldlst-2] );
                    }
                    // Each time the eigenvalues in WORK get refined, we store
                    // the newly found approximation with all shifts applied in w
                    for (j = oldfst; j <= oldlst; j++) {
                       w[wbegin+j-2] = work[wbegin+j-2]+sigma;
                    }
                 } // if (ndepth > 0)

                 // Process the current node.
                 newfst = oldfst;
                 for (j = oldfst; j <= oldlst; j++) {
                    if (j == oldlst) {
                       // we are at the right end of the cluster, this is also the boundary of the child cluster
                       newlst = j;
                    }
                    else if (wgap[wbegin + j -2] >= minrgp * Math.abs(work[wbegin+j-2] ) ) {
                       // the right relative gap is big enough, the child cluster
                       // (newfst,..,newlst) is well separated from the following
                       newlst = j;
                    }
                    else {
                       // inside a child cluster, the relative gap is not big enough.
                       continue;
                    }

                    // Compute size of child cluster found
                    newsiz = newlst - newfst + 1;

                    // newfit is the place in Z where the new RRR or the computed eigenvector is to be stored
                    if ((dol == 1) && (dou == m)) {
                       // Store representation at location of the leftmost evalue of the cluster
                       newftt = wbegin + newfst - 1;
                    }
                    else {
                       if (wbegin+newfst-1 < dol) {
                          // Store representation at the left end of Z array
                          newftt = dol - 1;
                       }
                       else if (wbegin+newfst-1 > dou) {
                          // Store representation at the right end of Z array
                          newftt = dou;
                       }
                       else {
                          newftt = wbegin + newfst - 1;
                       }
                    }

                    if (newsiz > 1) {
  
                       // Current child is not a singleton but a cluster.
                       // Compute and store new representation of child.
   
   
                       // Compute left and right cluster gap.
   
                       // lgap and rgap are not computed from work because
                       // the eigenvalue approximations may stem from RRRs
                       // different shifts. However, w hold all eigenvalues
                       // of the unshifted matrix. Still, the entries in WGAP
                       // have to be computed from work since the entries
                       // in w might be of the same order so that gaps are not
                       // exhibited correctly for very close eigenvalues.
                       if (newfst == 1) {
                          lgap = Math.max(0.0, w[wbegin-1]-werr[wbegin-1] - vl);
                       }
                       else {
                          lgap = wgap[wbegin+newfst-3];
                       }
                       rgap = wgap[wbegin+newlst-2];
   
                       // Compute left- and rightmost eigenvalue of child
                       // to high precision in order to shift as close
                       // as possible and obtain as large relative gaps
                       // as possible
   
                       for (k = 1; k <= 2; k++) {
                          if (k == 1) {
                             p = indexw[wbegin-2+newfst];
                          }
                          else {
                             p = indexw[wbegin-2+newlst];
                          }
                          offset = indexw[wbegin-1] - 1;
                          vec = new double[in];
                          for (index = 0; index < in; index++) {
                              vec[index] = d[ibegin-1+index];
                          }
                          vec2 = new double[in-1];
                          for (index = 0; index < in-1; index++) {
                              vec2[index] = work[indlld+ibegin-2+index];
                          }
                          vec3 =  new double[in];
                          for (index = 0; index < in; index++) {
                              vec3[index] = work[wbegin-1+index];
                          }
                          vec4 = new double[in-1];
                          for (index = 0; index <in-1; index++) {
                              vec4[index] = wgap[wbegin-1+index];
                          }
                          vec5 = new double[in];
                          for (index = 0; index < in; index++) {
                              vec5[index] = werr[wbegin-1+index];
                          }
                          vec6 = new double[2*in];
                          ivec = new int[2*in];
                          dlarrb(in, vec, vec2, p, p, rqtol, rqtol, offset,
                                 vec3, vec4, vec5, vec6, ivec, pivmin, spdiam, in, iinfo);
                          for (index = 0; index < in; index++) {
                              work[wbegin-1+index] = vec3[index];
                          }
                          for (index = 0; index <in-1; index++) {
                              wgap[wbegin-1+index] = vec4[index];
                          }
                          for (index = 0; index < in; index++) {
                              werr[wbegin-1+index] = vec5[index];
                          }
                       } // for (k = 1; k <= 2; k++)
   
                       if ((wbegin+newlst-1 < dol) || (wbegin+newfst-1 > dou)) {
                          // if the cluster contains no desired eigenvalues
                          // skip the computation of that branch of the rep. tree
   
                          // We could skip before the refinement of the extremal
                          // eigenvalues of the child, but then the representation
                          // tree could be different from the one when nothing is
                          // skipped. For this reason we skip at this place.
                          idone = idone + newlst - newfst + 1;
                          // Proceed to any remaining child nodes
                          newfst = j + 1;
                          continue;
                       }
   
                       // Compute RRR of child cluster.
                       // Note that the new RRR is stored in Z
   
                       // dlarrf needs LWORK = 2*n
                       /*CALL DLARRF( IN, D( IBEGIN ), L( IBEGIN ),
       $                         WORK(INDLD+IBEGIN-1),
       $                         NEWFST, NEWLST, WORK(WBEGIN),
       $                         WGAP(WBEGIN), WERR(WBEGIN),
       $                         SPDIAM, LGAP, RGAP, PIVMIN, TAU,
       $                         Z(IBEGIN, NEWFTT),Z(IBEGIN, NEWFTT+1),
       $                         WORK( INDWRK ), IINFO )
                       IF( IINFO.EQ.0 ) THEN
  *                       a new RRR for the cluster was found by DLARRF
  *                       update shift and store it
                          SSIGMA = SIGMA + TAU
                          Z( IEND, NEWFTT+1 ) = SSIGMA
  *                       WORK() are the midpoints and WERR() the semi-width
  *                       Note that the entries in W are unchanged.
                          DO 116 K = NEWFST, NEWLST
                             FUDGE =
       $                          THREE*EPS*ABS(WORK(WBEGIN+K-1))
                             WORK( WBEGIN + K - 1 ) =
       $                          WORK( WBEGIN + K - 1) - TAU
                             FUDGE = FUDGE +
       $                          FOUR*EPS*ABS(WORK(WBEGIN+K-1))
  *                          Fudge errors
                             WERR( WBEGIN + K - 1 ) =
       $                          WERR( WBEGIN + K - 1 ) + FUDGE
  *                          Gaps are not fudged. Provided that WERR is small
  *                          when eigenvalues are close, a zero gap indicates
  *                          that a new representation is needed for resolving
  *                          the cluster. A fudge could lead to a wrong decision
  *                          of judging eigenvalues 'separated' which in
  *                          reality are not. This could have a negative impact
  *                          on the orthogonality of the computed eigenvectors.
   116                    CONTINUE

                          NCLUS = NCLUS + 1
                          K = NEWCLS + 2*NCLUS
                          IWORK( K-1 ) = NEWFST
                          IWORK( K ) = NEWLST
                       ELSE
                          INFO = -2
                          RETURN
                       ENDIF*/
                    } // if (newsiz > 1)
                    /*ELSE
  *
  *                    Compute eigenvector of singleton
  *
                       ITER = 0
  *
                       TOL = FOUR * LOG(DBLE(IN)) * EPS
  *
                       K = NEWFST
                       WINDEX = WBEGIN + K - 1
                       WINDMN = MAX(WINDEX - 1,1)
                       WINDPL = MIN(WINDEX + 1,M)
                       LAMBDA = WORK( WINDEX )
                       DONE = DONE + 1
  *                    Check if eigenvector computation is to be skipped
                       IF((WINDEX.LT.DOL).OR.
       $                  (WINDEX.GT.DOU)) THEN
                          ESKIP = .TRUE.
                          GOTO 125
                       ELSE
                          ESKIP = .FALSE.
                       ENDIF
                       LEFT = WORK( WINDEX ) - WERR( WINDEX )
                       RIGHT = WORK( WINDEX ) + WERR( WINDEX )
                       INDEIG = INDEXW( WINDEX )
  *                    Note that since we compute the eigenpairs for a child,
  *                    all eigenvalue approximations are w.r.t the same shift.
  *                    In this case, the entries in WORK should be used for
  *                    computing the gaps since they exhibit even very small
  *                    differences in the eigenvalues, as opposed to the
  *                    entries in W which might "look" the same.

                       IF( K .EQ. 1) THEN
  *                       In the case RANGE='I' and with not much initial
  *                       accuracy in LAMBDA and VL, the formula
  *                       LGAP = MAX( ZERO, (SIGMA - VL) + LAMBDA )
  *                       can lead to an overestimation of the left gap and
  *                       thus to inadequately early RQI 'convergence'.
  *                       Prevent this by forcing a small left gap.
                          LGAP = EPS*MAX(ABS(LEFT),ABS(RIGHT))
                       ELSE
                          LGAP = WGAP(WINDMN)
                       ENDIF
                       IF( K .EQ. IM) THEN
  *                       In the case RANGE='I' and with not much initial
  *                       accuracy in LAMBDA and VU, the formula
  *                       can lead to an overestimation of the right gap and
  *                       thus to inadequately early RQI 'convergence'.
  *                       Prevent this by forcing a small right gap.
                          RGAP = EPS*MAX(ABS(LEFT),ABS(RIGHT))
                       ELSE
                          RGAP = WGAP(WINDEX)
                       ENDIF
                       GAP = MIN( LGAP, RGAP )
                       IF(( K .EQ. 1).OR.(K .EQ. IM)) THEN
  *                       The eigenvector support can become wrong
  *                       because significant entries could be cut off due to a
  *                       large GAPTOL parameter in LAR1V. Prevent this.
                          GAPTOL = ZERO
                       ELSE
                          GAPTOL = GAP * EPS
                       ENDIF
                       ISUPMN = IN
                       ISUPMX = 1
  *                    Update WGAP so that it holds the minimum gap
  *                    to the left or the right. This is crucial in the
  *                    case where bisection is used to ensure that the
  *                    eigenvalue is refined up to the required precision.
  *                    The correct value is restored afterwards.
                       SAVGAP = WGAP(WINDEX)
                       WGAP(WINDEX) = GAP
  *                    We want to use the Rayleigh Quotient Correction
  *                    as often as possible since it converges quadratically
  *                    when we are close enough to the desired eigenvalue.
  *                    However, the Rayleigh Quotient can have the wrong sign
  *                    and lead us away from the desired eigenvalue. In this
  *                    case, the best we can do is to use bisection.
                       USEDBS = .FALSE.
                       USEDRQ = .FALSE.
  *                    Bisection is initially turned off unless it is forced
                       NEEDBS =  .NOT.TRYRQC
   120                 CONTINUE
  *                    Check if bisection should be used to refine eigenvalue
                       IF(NEEDBS) THEN
  *                       Take the bisection as new iterate
                          USEDBS = .TRUE.
                          ITMP1 = IWORK( IINDR+WINDEX )
                          OFFSET = INDEXW( WBEGIN ) - 1
                          CALL DLARRB( IN, D(IBEGIN),
       $                       WORK(INDLLD+IBEGIN-1),INDEIG,INDEIG,
       $                       ZERO, TWO*EPS, OFFSET,
       $                       WORK(WBEGIN),WGAP(WBEGIN),
       $                       WERR(WBEGIN),WORK( INDWRK ),
       $                       IWORK( IINDWK ), PIVMIN, SPDIAM,
       $                       ITMP1, IINFO )
                          IF( IINFO.NE.0 ) THEN
                             INFO = -3
                             RETURN
                          ENDIF
                          LAMBDA = WORK( WINDEX )
  *                       Reset twist index from inaccurate LAMBDA to
  *                       force computation of true MINGMA
                          IWORK( IINDR+WINDEX ) = 0
                       ENDIF
  *                    Given LAMBDA, compute the eigenvector.
                       CALL DLAR1V( IN, 1, IN, LAMBDA, D( IBEGIN ),
       $                    L( IBEGIN ), WORK(INDLD+IBEGIN-1),
       $                    WORK(INDLLD+IBEGIN-1),
       $                    PIVMIN, GAPTOL, Z( IBEGIN, WINDEX ),
       $                    .NOT.USEDBS, NEGCNT, ZTZ, MINGMA,
       $                    IWORK( IINDR+WINDEX ), ISUPPZ( 2*WINDEX-1 ),
       $                    NRMINV, RESID, RQCORR, WORK( INDWRK ) )
                       IF(ITER .EQ. 0) THEN
                          BSTRES = RESID
                          BSTW = LAMBDA
                       ELSEIF(RESID.LT.BSTRES) THEN
                          BSTRES = RESID
                          BSTW = LAMBDA
                       ENDIF
                       ISUPMN = MIN(ISUPMN,ISUPPZ( 2*WINDEX-1 ))
                       ISUPMX = MAX(ISUPMX,ISUPPZ( 2*WINDEX ))
                       ITER = ITER + 1

  *                    sin alpha <= |resid|/gap
  *                    Note that both the residual and the gap are
  *                    proportional to the matrix, so ||T|| doesn't play
  *                    a role in the quotient

  *
  *                    Convergence test for Rayleigh-Quotient iteration
  *                    (omitted when Bisection has been used)
  *
                       IF( RESID.GT.TOL*GAP .AND. ABS( RQCORR ).GT.
       $                    RQTOL*ABS( LAMBDA ) .AND. .NOT. USEDBS)
       $                    THEN
  *                       We need to check that the RQCORR update doesn't
  *                       move the eigenvalue away from the desired one and
  *                       towards a neighbor. -> protection with bisection
                          IF(INDEIG.LE.NEGCNT) THEN
  *                          The wanted eigenvalue lies to the left
                             SGNDEF = -ONE
                          ELSE
  *                          The wanted eigenvalue lies to the right
                             SGNDEF = ONE
                          ENDIF
  *                       We only use the RQCORR if it improves the
  *                       the iterate reasonably.
                          IF( ( RQCORR*SGNDEF.GE.ZERO )
       $                       .AND.( LAMBDA + RQCORR.LE. RIGHT)
       $                       .AND.( LAMBDA + RQCORR.GE. LEFT)
       $                       ) THEN
                             USEDRQ = .TRUE.
  *                          Store new midpoint of bisection interval in WORK
                             IF(SGNDEF.EQ.ONE) THEN
  *                             The current LAMBDA is on the left of the true
  *                             eigenvalue
                                LEFT = LAMBDA
  *                             We prefer to assume that the error estimate
  *                             is correct. We could make the interval not
  *                             as a bracket but to be modified if the RQCORR
  *                             chooses to. In this case, the RIGHT side should
  *                             be modified as follows:
  *                              RIGHT = MAX(RIGHT, LAMBDA + RQCORR)
                             ELSE
  *                             The current LAMBDA is on the right of the true
  *                             eigenvalue
                                RIGHT = LAMBDA
  *                             See comment about assuming the error estimate is
  *                             correct above.
  *                              LEFT = MIN(LEFT, LAMBDA + RQCORR)
                             ENDIF
                             WORK( WINDEX ) =
       $                       HALF * (RIGHT + LEFT)
  *                          Take RQCORR since it has the correct sign and
  *                          improves the iterate reasonably
                             LAMBDA = LAMBDA + RQCORR
  *                          Update width of error interval
                             WERR( WINDEX ) =
       $                             HALF * (RIGHT-LEFT)
                          ELSE
                             NEEDBS = .TRUE.
                          ENDIF
                          IF(RIGHT-LEFT.LT.RQTOL*ABS(LAMBDA)) THEN
  *                             The eigenvalue is computed to bisection accuracy
  *                             compute eigenvector and stop
                             USEDBS = .TRUE.
                             GOTO 120
                          ELSEIF( ITER.LT.MAXITR ) THEN
                             GOTO 120
                          ELSEIF( ITER.EQ.MAXITR ) THEN
                             NEEDBS = .TRUE.
                             GOTO 120
                          ELSE
                             INFO = 5
                             RETURN
                          END IF
                       ELSE
                          STP2II = .FALSE.
          IF(USEDRQ .AND. USEDBS .AND.
       $                     BSTRES.LE.RESID) THEN
                             LAMBDA = BSTW
                             STP2II = .TRUE.
                          ENDIF
                          IF (STP2II) THEN
  *                          improve error angle by second step
                             CALL DLAR1V( IN, 1, IN, LAMBDA,
       $                          D( IBEGIN ), L( IBEGIN ),
       $                          WORK(INDLD+IBEGIN-1),
       $                          WORK(INDLLD+IBEGIN-1),
       $                          PIVMIN, GAPTOL, Z( IBEGIN, WINDEX ),
       $                          .NOT.USEDBS, NEGCNT, ZTZ, MINGMA,
       $                          IWORK( IINDR+WINDEX ),
       $                          ISUPPZ( 2*WINDEX-1 ),
       $                          NRMINV, RESID, RQCORR, WORK( INDWRK ) )
                          ENDIF
                          WORK( WINDEX ) = LAMBDA
                       END IF
  *
  *                    Compute FP-vector support w.r.t. whole matrix
  *
                       ISUPPZ( 2*WINDEX-1 ) = ISUPPZ( 2*WINDEX-1 )+OLDIEN
                       ISUPPZ( 2*WINDEX ) = ISUPPZ( 2*WINDEX )+OLDIEN
                       ZFROM = ISUPPZ( 2*WINDEX-1 )
                       ZTO = ISUPPZ( 2*WINDEX )
                       ISUPMN = ISUPMN + OLDIEN
                       ISUPMX = ISUPMX + OLDIEN
  *                    Ensure vector is ok if support in the RQI has changed
                       IF(ISUPMN.LT.ZFROM) THEN
                          DO 122 II = ISUPMN,ZFROM-1
                             Z( II, WINDEX ) = ZERO
   122                    CONTINUE
                       ENDIF
                       IF(ISUPMX.GT.ZTO) THEN
                          DO 123 II = ZTO+1,ISUPMX
                             Z( II, WINDEX ) = ZERO
   123                    CONTINUE
                       ENDIF
                       CALL DSCAL( ZTO-ZFROM+1, NRMINV,
       $                       Z( ZFROM, WINDEX ), 1 )
   125                 CONTINUE
  *                    Update W
                       W( WINDEX ) = LAMBDA+SIGMA
  *                    Recompute the gaps on the left and right
  *                    But only allow them to become larger and not
  *                    smaller (which can only happen through "bad"
  *                    cancellation and doesn't reflect the theory
  *                    where the initial gaps are underestimated due
  *                    to WERR being too crude.)
                       IF(.NOT.ESKIP) THEN
                          IF( K.GT.1) THEN
                             WGAP( WINDMN ) = MAX( WGAP(WINDMN),
       $                          W(WINDEX)-WERR(WINDEX)
       $                          - W(WINDMN)-WERR(WINDMN) )
                          ENDIF
                          IF( WINDEX.LT.WEND ) THEN
                             WGAP( WINDEX ) = MAX( SAVGAP,
       $                          W( WINDPL )-WERR( WINDPL )
       $                          - W( WINDEX )-WERR( WINDEX) )
                          ENDIF
                       ENDIF
                       IDONE = IDONE + 1
                    ENDIF*/
                    // here ends the code for the current child
   
                    // Proceed to any remaining child nodes
                    newfst = j + 1;
                 } // for (j = oldfst; j <= oldlst; j++)
              } // for (i = 1; i <= oldncl; i++)
              ndepth++;
           } // while (idone < im)
           ibegin = iend + 1;
           wbegin = wend + 1;
        } // for (jblk = 1; jblk <= iblock[m-1]; jblk++)

        return;

  } // private void dlarrv

  
  /** This is a port of version 3.4.0 LAPACK auxiliary routine dlaneg.  LAPACK is a software package provided by Univ. of Tennessee,    --
  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. November 2011
  
  Contributors:
  Osni Marques, LBNL/NERSC, USA
  Christof Voemel, University of California, Berkeley, USA
  Jason Riedy, University of California, Berkeley, USA
  
  dlaneg computes the Sturm count, the number of negative pivots
  encountered while factoring tridiagonal T - sigma I = L D L^T.
  This implementation works directly on the factors without forming
  the tridiagonal matrix T.  The Sturm count is also the number of
  eigenvalues of T less than sigma.
  
  This routine is called from dlarrb.
  
  The current routine does not use the PIVMIN parameter but rather
  requires IEEE-754 propagation of Infinities and NaNs.  This
  routine also has no input range restrictions but does require
  default exception handling such that x/0 produces Inf when x is
  non-zero, and Inf/Inf produces NaN.  For more information, see:
 
  Marques, Riedy, and Voemel, "Benefits of IEEE-754 Features in
  Modern Symmetric Tridiagonal Eigensolvers," SIAM Journal on
  Scientific Computing, v28, n5, 2006.  DOI 10.1137/050641624
  (Tech report version in LAWN 172 with the same title.)
  
  @param n input int  The order of the matrix
  @param d input double[] of dimension n.   The n diagonal elements of the diagonal matrix D.
  @param lld input double[] of dimension (n-1).  The (n-1) elements L(i)*L(i)*D(i).
  @param sigma input double  Shift amount in T - sigma I = L D L^T.
  @param pivmin input double  The minimum pivot in the Sturm sequence.  May be used
         when zero pivots are encountered on non-IEEE-754 architectures.
  @param r input int  The twist index for the twisted factorization that is used
         for the negcount.
  */
  private int dlaneg(int n, double d[], double lld[], double sigma, double pivmin, int r) {
 
        // Some architectures propagate Infinities and NaNs very slowly, so
        // the code computes counts in blklen chunks.  Then a NaN can
        // propagate at most blklen columns before being detected.  This is
        // not a general tuning parameter; it needs only to be just large
        // enough that the overhead is tiny in common cases.
        final int blklen = 128;
        
        int bj;
        int j;
        int neg1;
        int neg2;
        int negcnt;
        
        double bsav;
        double dminus;
        double dplus;
        double gamma;
        double p;
        double t;
        double tmp;
        
        boolean sawnan;

        negcnt = 0;

        // I) upper part: L D L^T - SIGMA I = L+ D+ L+^T
        t = -sigma;
        for (bj = 1; bj <= r-1; bj += blklen) {
           neg1 = 0;
           bsav = t;
           for (j = bj; j <= Math.min(bj+blklen-1, r-1); j++) {
              dplus = d[j-1] + t;
              if (dplus < 0.0) {
                  neg1 = neg1 + 1;
              }
              tmp = t / dplus;
              t = tmp * lld[j-1] - sigma;
           } // for (j = bj; j <= Math.min(bj+blklen-1, r-1); j++)
           sawnan = Double.isNaN(t);
        // Run a slower version of the above loop if a NaN is detected.
        // A NaN should occur only with a zero pivot after an infinite
        // pivot.  In that case, substituting 1 for t/dplus is the
        // correct limit.
           if (sawnan) {
              neg1 = 0;
              t = bsav;
              for (j = bj; j <= Math.min(bj+blklen-1, r-1); j++) {
                 dplus = d[j-1] + t;
                 if (dplus < 0.0) {
                     neg1 = neg1 + 1;
                 }
                 tmp = t / dplus;
                 if (Double.isNaN(tmp)) {
                     tmp = 1.0;
                 }
                 t = tmp * lld[j-1] - sigma;
              } // for (j = bj; j <= Math.min(bj+blklen-1, r-1); j++)
           } // if (sawnan)
           negcnt = negcnt + neg1;
        
        } // for (bj = 1; bj <= r-1; bj += blklen)
   
        // II) lower part: L D L^T - SIGMA I = U- D- U-^T
        p = d[n-1] - sigma;
        for (bj = n-1; bj >= r; bj -= blklen) {
           neg2 = 0;
           bsav = p;
           for (j = bj; j >=  Math.max(bj-blklen+1, r); j--) {
              dminus = lld[j-1] + p;
              if (dminus < 0.0) {
                  neg2 = neg2 + 1;
              }
              tmp = p / dminus;
              p = tmp * d[j-1] - sigma;
           } // for (j = bj; j >=  Math.max(bj-blklen+1, r); j--)
           sawnan = Double.isNaN(p);
        // As above, run a slower version that substitutes 1 for Inf/Inf.
  
           if (sawnan) {
              neg2 = 0;
              p = bsav;
              for (j = bj; j >= Math.max(bj-blklen+1, r); j--) {
                 dminus = lld[j-1] + p;
                 if (dminus < 0.0) {
                     neg2 = neg2 + 1;
                 }
                 tmp = p / dminus;
                 if (Double.isNaN(tmp)) {
                     tmp = 1.0;
                 }
                 p = tmp * d[j-1] - sigma;
              } // for (j = bj; j >= Math.max(bj-blklen+1, r); j--)
           } // if (sawnan)
           negcnt = negcnt + neg2;
        } // for (bj = n-1; bj >= r; bj -= blklen)
   
        // III) Twist index
          // T was shifted by sigma initially.
        gamma = (t + sigma) + p;
        if (gamma < 0.0) {
            negcnt = negcnt+1;
        }

        return negcnt;
  }

  
}
    