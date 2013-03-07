package gov.nih.mipav.model.structures.jama;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class SVD implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    GeneralizedInverse2 gi = new GeneralizedInverse2();
    
    public SVD () {}
    
    /* dgesvd computes the singular value decomposition (SVD) for GE matrices.
     
       This is a port of LAPACK driver routine (version 3.4.1) DGESVD.f of April, 2012 
       LAPACK is a software package privided by University of Tennessee, University of
       California Berkeley, University of Colorado Denver, and NAG Ltd.
        
       dgesvd computes the singular value decomposition (SVD) of a real
       m by n  matrix A, optionally computing the left and/or right singular
       vectors. The SVD is written
    
          A = U * SIGMA * transpose(V)
  
       where SIGMA is an m-by-n matrix which is zero except for its
       min(m,n) diagonal elements, U is an m-by-m orthogonal matrix, and
       V is an n-by-n orthogonal matrix.  The diagonal elements of SIGMA
       are the singular values of A; they are real and non-negative, and
       are returned in descending order.  The first min(m,n) columns of
       U and V are the left and right singular vectors of A.
    
       Note that the routine returns V**T, not V.
       
       @param input char jobu
           Specifies options for computing all or part of the matrix U:
           = 'A':  all m columns of U are returned in array U:
           = 'S':  the first min(m,n) columns of U (the left singular
                   vectors) are returned in the array U;
           = 'O':  the first min(m,n) columns of U (the left singular
                   vectors) are overwritten on the array A;
           = 'N':  no columns of U (no left singular vectors) are
                   computed.
                   
       @param input char jobvt
           Specifies options for computing all or part of the matrix
           V**T:
           = 'A':  all N rows of V**T are returned in the array VT;
           = 'S':  the first min(m,n) rows of V**T (the right singular
                   vectors) are returned in the array VT;
           = 'O':  the first min(m,n) rows of V**T (the right singular
                   vectors) are overwritten on the array A;
           = 'N':  no rows of V**T (no right singular vectors) are
                   computed.
      
          jobvt and jobu cannot both be 'O'.
          
       @param input int m  The number of rows of the input matrix A.  m >= 0.
       
       @param input int n  The number of columns of the input matrix A.  n >= 0.
       
       @param (input/output) double[][] A of dim lda by n.
           On entry, the m-by-n matrix A.
           On exit,
                if jobu = 'O',  A is overwritten with the first min(m,n)
                                columns of U (the left singular vectors,
                                stored columnwise);
                if jobvt = 'O', A is overwritten with the first min(m,n)
                                rows of V**T (the right singular vectors,
                                stored rowwise);
                if jobu .ne. 'O' and jobvt .ne. 'O', the contents of A
                                are destroyed.
                                
       @param input int lda The leading dimension of the array A.  lda >= max(1,m).
       
       @param output double[] s of dim(min(m,n)) The singular values of A, sorted so
           that s[i] >= s[i+1].
           
       @param output double[][] U of dim ldu by ucol.
           (ldu,m) if jobu = 'A' or (ldu,min(m,n)) if jobu = 'S'.
           If jobu = 'A', U contains the m-by-m orthogonal matrix U;
           if jobu = 'S', U contains the first min(m,n) columns of U
                          (the left singular vectors, stored columnwise);
           if jobu = 'N' or 'O', U is not referenced. 
           
       @param input int ldu  The leading dimension of the array U.  ldu >= 1; if
           lobu = 'S' or 'A', ldu >= m.
           
       @param output double[][] VT of dim ldvt by n.
           If jobvt = 'A', VT contains the n-by-n orthogonal matrix V**T;
           if jobvt = 'S', VT contains the first min(m,n) rows of
                           V**T (the right singular vectors, stored rowwise);
           if jobvt = 'N' or 'O', VT is not referenced.
           
        @param input int ldvt
            The leading dimension of the array VT.  ldvt >= 1; if
            jobvt = 'A', ldvt >= n; if jobvt = 'S', ldvt >= min(m,n).
            
        @param output double[] of dim max(1,lwork)
            On exit, if info[0] = 0, work[0] returns the optimal lwork;
            if info[0] > 0, work[1:min(n,m)-1] contains the unconverged
            superdiagonal elements of an upper bidiagonal matrix B
            whose diagonal is in s (not necessarily sorted). B
            satisfies A = U * B * VT, so it has the same singular values
            as A, and singular vectors related by U and VT.
            
        @param input int lwork.  The dimension of the array work.
            lwork >= max(1,5*MIN(m,n)) for the paths (see comments inside code):
                  - PATH 1  (m much larger than n, jobu = 'N') 
                  - PATH 1t (n much larger than m, jobvt = 'N')
            lwork >= max(1,3*min(m,n)+max(m,n),5*min(m,n)) for the other paths
            For good performance, lwork should generally be larger.
    
            If lwork = -1, then a workspace query is assumed; the routine
            only calculates the optimal size of the work array, returns
            this value as the first entry of the work array, and no error
            message related to lwork is issued.
            
        @param output int[] info of dim 1.
             = 0:  successful exit.
             < 0:  if info[0] = -i, the i-th argument had an illegal value.
             > 0:  if dbdsqr did not converge, info[0] specifies how many
                   superdiagonals of an intermediate bidiagonal form B
                   did not converge to zero. See the description of work
                   above for details.
     */
   
      public void dgesvd(char jobu, char jobvt, int m, int n, double A[][], int lda, double s[], 
                         double U[][], int ldu, double VT[][], int ldvt, double work[], int lwork,
                         int info[]) {
    
          boolean lquery;
          boolean wntua;
          boolean wntuas;
          boolean wntun;
          boolean wntuo;
          boolean wntus;
          boolean wntva;
          boolean wntvas;
          boolean wntvn;
          boolean wntvo;
          boolean wntvs;
          int bdspac = 0;
          int blk;
          int chunk;
          int i;
          int j;
          int k;
          int L;
          int p;
          int ie = 0;
          int ierr[] = new int[1];
          int ir;
          int iscl;
          int itau;
          int itaup;
          int itauq;
          int iu;
          int iwork;
          int ldwrkr;
          int ldwrku;
          int maxwrk;
          int minmn;
          int minwrk;
          int mnthr = 0;
          int ncu = 0;
          int ncvt = 0;
          int nru = 0;
          int nrvt = 0;
          int wrkbl = 0;
          int lwork_dgeqrf;
          int lwork_dorgqr_n;
          int lwork_dorgqr_m;
          int lwork_dgebrd;
          int lwork_dorgbr_p;
          int lwork_dorgbr_q;
          int lwork_dgelqf;
          int lwork_dorglq_n;
          int lwork_dorglq_m;
          double anrm;
          double bignum;
          double eps;
          double smlnum;
          double dum[] = new double[1];
          double dumArr[][] = new double[1][1];
          String name;
          char optsChar[];
          String opts;
          double workiwork[];
          double workitauq[];
          double workitaup[];
          double workie[];
          double workitau[];
          double arr[][];
          double arr2[][];
          double arr3[][];
         
          //Test the input arguments
    
          info[0] = 0;
          minmn = Math.min(m, n);
          wntua = ((jobu == 'A') || (jobu == 'a'));
          wntus = ((jobu =='S') || (jobu == 's'));
          wntuas = wntua || wntus;
          wntuo = ((jobu ==  'O') || (jobu == 'o'));
          wntun = ((jobu == 'N') || (jobu == 'n'));
          wntva = ((jobvt == 'A') || (jobvt == 'a'));
          wntvs = ((jobvt == 'S') || (jobvt == 's'));
          wntvas = wntva || wntvs;
          wntvo = ((jobvt == 'O') || (jobvt == 'o'));
          wntvn = ((jobvt == 'N') || (jobvt == 'n'));
          lquery = (lwork == -1);
    
          if (!(wntua || wntus || wntuo || wntun)) {
             info[0] = -1;
          }
          else if (!(wntva || wntvs || wntvo || wntvn) ||
                  ( wntvo && wntuo)) {
             info[0] = -2;
          }
          else if (m < 0) {
             info[0] = -3;
          }
          else if (n < 0) {
             info[0] = -4;
          }
          else if (lda < Math.max(1, m)) {
             info[0] = -6;
          }
          else if (ldu < 1 || (wntuas && ldu < m)) {
             info[0] = -9;
          }
          else if (ldvt < 1 || (wntva && ldvt < n) ||
                  (wntvs && ldvt < minmn)) {
             info[0] = -11;
          }
    
          // Compute workspace
          // (Note: Comments in the code beginning "Workspace:" describe the
          // minimal amount of workspace needed at that point in the code,
          // as well as the preferred amount for good performance.
          // NB refers to the optimal block size for the immediately
          // following subroutine, as returned by ilaenv.)
    
          if (info[0] == 0) {
             minwrk = 1;
             maxwrk = 1;
             if (m >= n && minmn > 0) {
    
                // Compute space needed for dbdsqr
                name = new String("DGESVD");
                optsChar = new char[2];
                optsChar[0] = jobu;
                optsChar[1] = jobvt;
                opts = new String(optsChar);
                mnthr = ge.ilaenv( 6,  name, opts, m, n, 0, 0 );
                bdspac = 5*n;
                // Compute space needed for dgeqrf
                ge.dgeqrf(m, n, A, lda, dum, dum, -1, ierr);
                lwork_dgeqrf = (int)Math.round(dum[0]);
                // Compute space needed for dorgqr
                ge.dorgqr(m, n, n, A, lda, dum, dum, -1, ierr);
                lwork_dorgqr_n = (int)Math.round(dum[0]);
                ge.dorgqr(m, m, n, A, lda, dum, dum, -1, ierr);
                lwork_dorgqr_m = (int)Math.round(dum[0]);
                // Compute space needed for dgebrd
                gi.dgebrd(n, n, A, lda, s, dum, dum, dum, dum, -1, ierr);
                lwork_dgebrd = (int)Math.round(dum[0]);
                // Compute space needed for DORGBR P
                gi.dorgbr( 'P', n, n, n, A, lda, dum, dum, -1, ierr);
                lwork_dorgbr_p = (int)Math.round(dum[0]);
                // Compute space needed for DORGBR Q
                gi.dorgbr( 'Q', n, n, n, A, lda, dum, dum, -1, ierr);
                lwork_dorgbr_q = (int)Math.round(dum[0]);
     
                if (m >= mnthr) {
                   if (wntun) {
    
                      // Path 1 (m much larger than n, jobu = 'N')
     
                      maxwrk = n + lwork_dgeqrf;
                      maxwrk = Math.max(maxwrk, 3*n+lwork_dgebrd);
                      if (wntvo || wntvas) {
                         maxwrk = Math.max(maxwrk, 3*n+lwork_dorgbr_p);
                      }
                      maxwrk = Math.max(maxwrk, bdspac);
                      minwrk = Math.max(4*n, bdspac);
                   } // if (wntun)
                   else if (wntuo && wntvn) {
     
                      // Path 2 (m much larger than n, jobu = 'O', jobvt = 'N')
     
                      wrkbl = n + lwork_dgeqrf;
                      wrkbl = Math.max(wrkbl, n+lwork_dorgqr_n);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = Math.max(n*n+wrkbl, n*n+m*n+n);
                      minwrk = Math.max(3*n+m, bdspac);
                   } // else if (wntuo && wntvn)
                   else if (wntuo && wntvas) {
     
                      // Path 3 (m much larger than n, jobu = 'O', jobvt = 'S' or 'A')
     
                      wrkbl = n + lwork_dgeqrf;
                      wrkbl = Math.max(wrkbl, n+lwork_dorgqr_n);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = Math.max(n*n+wrkbl, n*n+m*n+n);
                      minwrk = Math.max(3*n+m, bdspac);
                   } // else if (wntuo && wntvas)
                   else if (wntus && wntvn) {
    
                      // Path 4 (m much larger than n, job = 'S', jobvt = 'N')
     
                      wrkbl = n + lwork_dgeqrf;
                      wrkbl = Math.max(wrkbl, n+lwork_dorgqr_n);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = n*n + wrkbl;
                      minwrk = Math.max(3*n+m, bdspac);
                   } // else if (wntus && wntvn)
                   else if (wntus && wntvo) {
     
                      // Path 5 (m much larger than n, jobu = 'S', jobvt = 'O')
     
                      wrkbl = n + lwork_dgeqrf;
                      wrkbl = Math.max(wrkbl, n+lwork_dorgqr_n);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = 2*n*n + wrkbl;
                      minwrk = Math.max(3*n+m, bdspac);
                   } // else if (wntus && wntvo)
                   else if (wntus && wntvas) {
     
                      // Path 6 (m much larger than n, jobu = 'S', jobvt = 'S' or 'A')
     
                      wrkbl = n + lwork_dgeqrf;
                      wrkbl = Math.max(wrkbl, n+lwork_dorgqr_n);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = n*n + wrkbl;
                      minwrk = Math.max( 3*n+m, bdspac);
                   } // else if (wntus && wntvas)
                   else if (wntua && wntvn) {
     
                      // Path 7 (m much larger than n, jobu = 'A', jobvt = 'N')
     
                      wrkbl = n + lwork_dgeqrf;
                      wrkbl = Math.max(wrkbl, n+lwork_dorgqr_m);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = n*n + wrkbl;
                      minwrk = Math.max(3*n+m, bdspac);
                   } // else if (wntua && wntvn)
                   else if (wntua && wntvo) {
     
                      // Path 8 (m much larger than n, jobu = 'A', jobvt = 'O')
      
                      wrkbl = n + lwork_dgeqrf;
                      wrkbl = Math.max(wrkbl, n+lwork_dorgqr_m);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = 2*n*n + wrkbl;
                      minwrk = Math.max(3*n+m, bdspac);
                   } // else if (wntua && wntvo)
                   else if (wntua && wntvas) {
     
                      // Path 9 (m much larger than n, jobu = 'A', jobvt = 'S' or 'A')
     
                      wrkbl = n + lwork_dgeqrf;
                      wrkbl = Math.max(wrkbl, n+lwork_dorgqr_m);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, 3*n+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = n*n + wrkbl;
                      minwrk = Math.max(3*n+m, bdspac);
                   } // else if (wntua && wntvas)
                } // if (m >= mnthr)
                else { // m < mnthr
     
                   // Path 10 (m at least n, but not much larger)
     
                   gi.dgebrd(m, n, A, lda, s, dum, dum, dum, dum, -1, ierr);
                   lwork_dgebrd = (int)Math.round(dum[0]);
                   maxwrk = 3*n + lwork_dgebrd;
                   if (wntus || wntuo) {
                      gi.dorgbr('Q', m, n, n, A, lda, dum, dum, -1, ierr);
                      lwork_dorgbr_q = (int)Math.round(dum[0]);
                      maxwrk = Math.max(maxwrk, 3*n+lwork_dorgbr_q);
                   } // if (wntus || wntuo)
                   if (wntua) {
                      gi.dorgbr('Q', m, m, n, A, lda, dum, dum, -1, ierr);
                      lwork_dorgbr_q = (int)Math.round(dum[0]);
                      maxwrk = Math.max(maxwrk, 3*n+lwork_dorgbr_q);
                   } /// if (wntua)
                   if (!wntvn) {
                     maxwrk = Math.max(maxwrk, 3*n+lwork_dorgbr_p);
                   } // if (!wntvn)
                   maxwrk = Math.max(maxwrk, bdspac);
                   minwrk = Math.max(3*n+m, bdspac);
                } // else m < mnthr
             } // if (m >= n && minmn > 0)
             else if (minmn > 0) {
     
                // Compute space needed for DBDSQR
     
                name = new String("DGESVD");
                optsChar = new char[2];
                optsChar[0] = jobu;
                optsChar[1] = jobvt;
                opts = new String(optsChar);
                mnthr = ge.ilaenv( 6, name, opts, m, n, 0, 0);
                bdspac = 5*m;
                // Compute space needed for dgelqf
                gi.dgelqf(m, n, A, lda, dum, dum, -1, ierr);
                lwork_dgelqf = (int)Math.round(dum[0]);
                // Compute space needed for dorglq
                gi.dorglq(n, n, m, dumArr, n, dum, dum, -1, ierr);
                lwork_dorglq_n = (int)Math.round(dum[0]);
                gi.dorglq(m, n, m, A, lda, dum, dum, -1, ierr);
                lwork_dorglq_m = (int)Math.round(dum[0]);
                // Compute space needed for dgebrd
                gi.dgebrd(m, m, A, lda, s, dum, dum, dum, dum, -1, ierr);
                lwork_dgebrd = (int)Math.round(dum[0]);
                // Compute space needed for dorgbr P
                gi.dorgbr('P', m, m, m, A, n, dum, dum, -1, ierr);
                lwork_dorgbr_p = (int)Math.round(dum[0]);
                // Compute space needed for dorgbr Q
                gi.dorgbr('Q', m, m, m, A, n, dum, dum, -1, ierr);
                lwork_dorgbr_q = (int)Math.round(dum[0]);
                if (n >= mnthr) {
                   if (wntvn) {
     
                      // Path 1t(n much larger than m, jobvt = 'N')
     
                      maxwrk = m + lwork_dgelqf;
                      maxwrk = Math.max(maxwrk, 3*m+lwork_dgebrd);
                      if (wntuo || wntuas) {
                         maxwrk = Math.max(maxwrk, 3*m+lwork_dorgbr_q);
                      }
                      maxwrk = Math.max(maxwrk, bdspac);
                      minwrk = Math.max(4*m, bdspac);
                   } // if (wntvn)
                   else if (wntvo && wntun) {
     
                      // Path 2t(n much larger than m, jobu = 'N', jobvt = 'O')
     
                      wrkbl = m + lwork_dgelqf;
                      wrkbl = Math.max(wrkbl, m+lwork_dorglq_m);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = Math.max(m*m+wrkbl, m*m+m*n+m);
                      minwrk = Math.max(3*m+n, bdspac);
                   } // else if (wntvo && wntun)
                   else if (wntvo && wntuas) {
     
                      // Path 3t(n much larger than m, jobu = 'S' or 'A', jobvt = 'O')
     
                      wrkbl = m + lwork_dgelqf;
                      wrkbl = Math.max(wrkbl, m+lwork_dorglq_m);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = Math.max(m*m+wrkbl, m*m+m*n+m);
                      minwrk = Math.max(3*m+n, bdspac);
                   } // else if (wntvo && wntuas)
                   else if (wntvs && wntun) {
     
                      // Path 4t(n much larger than m, jobu = 'N', jobvt = 'S')
    
                      wrkbl = m + lwork_dgelqf;
                      wrkbl = Math.max(wrkbl, m+lwork_dorglq_m);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = m*m + wrkbl;
                      minwrk = Math.max(3*m+n, bdspac);
                   } // else if (wntvs && wntun)
                   else if (wntvs && wntuo) {
    
                      // Path 5t(n much larger than m, jobu = 'O', jobvt = 'S')
     
                      wrkbl = m + lwork_dgelqf;
                      wrkbl = Math.max(wrkbl, m+lwork_dorglq_m);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = 2*m*m + wrkbl;
                      minwrk = Math.max(3*m+n, bdspac);
                   } // else if (wntvs && wntuo)
                   else if (wntvs && wntuas) {
     
                      // Path 6t(n much larger than m, jobu = 'S' or 'A', jobvt = 'S')
    
                      wrkbl = m + lwork_dgelqf;
                      wrkbl = Math.max(wrkbl, m+lwork_dorglq_m);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = m*m + wrkbl;
                      minwrk = Math.max(3*m+n, bdspac);
                   } // else if (wntvs && wntuas)
                   else if (wntva && wntun) {
     
                      // Path 7t(n much larger than m, jobu = 'N', jobvt = 'A')
    
                      wrkbl = m + lwork_dgelqf;
                      wrkbl = Math.max(wrkbl, m+lwork_dorglq_n);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = m*m + wrkbl;
                      minwrk = Math.max(3*m+n, bdspac);
                   } // else if (wntva && wntun)
                   else if (wntva && wntuo) {
     
                      // Path 8t(n much larger than m, jobu = 'O', jobvt = 'A')
    
                      wrkbl = m + lwork_dgelqf;
                      wrkbl = Math.max(wrkbl, m+lwork_dorglq_n);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = 2*m*m + wrkbl;
                      minwrk = Math.max(3*m+n, bdspac);
                   } // else if (wntva && wntuo)
                   else if (wntva && wntuas) {
    
                      // Path 9t(n much larger than m, jobu = 'S' or 'A', jobvt = 'A')
     
                      wrkbl = m + lwork_dgelqf;
                      wrkbl = Math.max(wrkbl, m+lwork_dorglq_n);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dgebrd);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_p);
                      wrkbl = Math.max(wrkbl, 3*m+lwork_dorgbr_q);
                      wrkbl = Math.max(wrkbl, bdspac);
                      maxwrk = m*m + wrkbl;
                      minwrk = Math.max( 3*m+n, bdspac);
                   } // else if (wntva && wntuas)
                } // if (n >= mnthr)
                else { // n < mnthr
     
                   // Path 10t(n greater than m, but not much larger)
     
                   gi.dgebrd(m, n, A, lda, s, dum, dum, dum, dum, -1, ierr);
                   lwork_dgebrd = (int)Math.round(dum[0]);
                   maxwrk = 3*m + lwork_dgebrd;
                   if (wntvs || wntvo) {
                     // Compute space needed for DORGBR P
                     gi.dorgbr('P', m, n, m, A, n, dum, dum, -1, ierr);
                     lwork_dorgbr_p = (int)Math.round(dum[0]);
                     maxwrk = Math.max(maxwrk, 3*m+lwork_dorgbr_p);
                   } // if (wntvs || wntvo)
                   if (wntva) {
                     gi.dorgbr('P', n, n, m, A, n, dum, dum, -1, ierr);
                     lwork_dorgbr_p = (int)Math.round(dum[0]);
                     maxwrk = Math.max(maxwrk, 3*m+lwork_dorgbr_p);
                   } // if (wntva)
                   if (!wntun ) {
                      maxwrk = Math.max(maxwrk, 3*m+lwork_dorgbr_q);
                   }
                   maxwrk = Math.max(maxwrk, bdspac);
                   minwrk = Math.max(3*m+n, bdspac);
                } // else n < mnthr
             } // else if (minmn > 0)
             maxwrk = Math.max(maxwrk, minwrk);
             work[0] = maxwrk;
    
             if(lwork < minwrk && !lquery) {
                info[0] = -13;
             }
          } // if (info[0] == 0)
    
          if (info[0] != 0) {
             MipavUtil.displayError("dgesvd had info[0] = " + info[0]);
             return;
          }
          else if (lquery) {
             return;
          }
    
          // Quick return if possible
    
          if (m == 0 || n == 0) {
             return;
          }
    
          // Get machine constants
     
          eps = ge.dlamch('P');
          smlnum = Math.sqrt(ge.dlamch('S')) / eps;
          bignum = 1.0 / smlnum;
     
          // Scale A if max element outside range [SMLNUM,BIGNUM]
     
          anrm = ge.dlange( 'M', m, n, A, lda, dum);
          iscl = 0;
          if (anrm > 0.0 && anrm < smlnum) {
             iscl = 1;
             ge.dlascl( 'G', 0, 0, anrm, smlnum, m, n, A, lda, ierr);
          }
          else if (anrm > bignum) {
             iscl = 1;
             ge.dlascl( 'G', 0, 0, anrm, bignum, m, n, A, lda, ierr);
          }
    
          if (m >= n) {
    
             // A has at least as many rows as columns. If A has sufficiently
             // more rows than columns, first reduce using the QR
             // decomposition (if sufficient workspace available)
     
             if (m >= mnthr) {
    
                if (wntun) {
    
                   // Path 1 (m much larger than n, jobu = 'N')
                   // No left singular vectors to be computed
    
                   itau = 1;
                   iwork = itau + n;
    
                   // Compute A=Q*R
                   // (Workspace: need 2*n, prefer n+n*nb)
    
                   workiwork = new double[Math.max(4*n,Math.max(1, lwork - iwork + 1))];
                   ge.dgeqrf(m, n, A, lda, work, workiwork, lwork-iwork+1, ierr);
     
                   // Zero out below R
     
                   arr = new double[n-1][n-1];
                   for (i = 0; i < n-1; i++) {
                       for (j = 0; j < n-1; j++) {
                           arr[i][j] = A[i+1][j];
                       }
                   }
                   // lda is unused in dlaset
                   ge.dlaset( 'L', n-1, n-1, 0.0, 0.0, arr, n-1);
                   for (i = 0; i < n-1; i++) {
                       for (j = 0; j < n-1; j++) {
                           A[i+1][j] = arr[i][j];
                       }
                   }
                   ie = 1;
                   itauq = ie + n;
                   itaup = itauq + n;
                   iwork = itaup + n;
                   workitauq = new double[n];
                   workitaup = new double[n];
     
                   // Bidiagonalize R in A
                   // (Workspace: need 4*n, prefer 3*n+2*n*nb)
     
                   gi.dgebrd(n, n, A, lda, s, work, workitauq, workitaup,
                            workiwork, lwork-iwork+1, ierr);
                   ncvt = 0;
                   if (wntvo || wntvas) {
     
                      // If right singular vectors desired, generate P'.
                      // (Workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
     
                      gi.dorgbr('P', n, n, n, A, lda, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
                      ncvt = n;
                   } // if (wntvo || wntvas)
                   iwork = ie + n;
    
                   // Perform bidiagonal QR iteration, computing right
                   // singular vectors of A in A if desired
                   // (Workspace: need BDSPAC)
     
                   gi.dbdsqr( 'U', n, ncvt, 0, 0, s, work, A, lda,
                              dumArr, 1, dumArr, 1, workiwork, info);
    
                   // If right singular vectors desired in VT, copy them there
     
                   if (wntvas) {
                      ge.dlacpy( 'F', n, n, A, lda, VT, ldvt);
                   }
    
                } // if (wntun)
                else if (wntuo && wntvn) {
     
                   // Path 2 (m much larger than n, jobu = 'O', jobvt = 'N')
                   // n left singular vectors to be overwritten on A and
                   // no right singular vectors to be computed
    
                   if (lwork >= n*n+Math.max(4*n, bdspac)) {
    
                      // Sufficient workspace for a fast algorithm
     
                      ir = 1;
                      if (lwork >= Math.max(wrkbl, lda*n+n)+lda*n) {
     
                         // WORK(IU) is lda by n, WORK(IR) is lda by n
     
                         ldwrku = lda;
                         ldwrkr = lda;
                      }
                      else if (lwork >= Math.max(wrkbl, lda*n+n)+n*n) {
     
                         // WORK(IU) is lda by n, WORK(IR) is n by n
    
                         ldwrku = lda;
                         ldwrkr = n;
                      }
                      else {
    
                         // WORK(IU) is ldwrku by n, WORK(IR) is n by n
    
                         ldwrku = (lwork-n*n-n) / n;
                         ldwrkr = n;
                      }
                      itau = ir + ldwrkr*n;
                      iwork = itau + n;
    
                      // Compute A=Q*R
                      // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
                      workitau = new double[n];
                      workiwork = new double[Math.max(1, lwork-iwork+1)];
                      ge.dgeqrf(m, n, A, lda, workitau,
                                workiwork, lwork-iwork+1, ierr);
     
                      // Copy R to WORK(IR) and zero out below it
     
                      arr = new double[n][n];
                      ge.dlacpy('U', n, n, A, lda, arr, ldwrkr);
                      arr2 = new double[n-1][n-1];
                      for (i = 0; i < n-1; i++) {
                          for (j = 0; j < n-1; j++) {
                              arr2[i][j] = arr[i+1][j];
                          }
                      }
                      ge.dlaset( 'L', n-1, n-1, 0.0, 0.0, arr2, ldwrkr);
                      for (i = 0; i < n-1; i++) {
                          for (j = 0; j < n-1; j++) {
                              arr[i+1][j] = arr2[i][j];
                          }
                      }
    
                      // Generate Q in A
                      // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
    
                      ge.dorgqr(m, n, n, A, lda, workitau,
                                workiwork, lwork-iwork+1, ierr);
                      ie = itau;
                      itauq = ie + n;
                      itaup = itauq + n;
                      iwork = itaup + n;
    
                      // Bidiagonalize R in WORK(IR)
                      // (Workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                      workie = new double[n];
                      workitauq = new double[n];
                      workitaup = new double[n];
                      workiwork = new double[Math.max(1, lwork-iwork+1)];
                      gi.dgebrd(n, n, arr, ldwrkr, s, workie,
                                workitauq, workitaup,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Generate left vectors bidiagonalizing R
                      // (Workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
    
                      gi.dorgbr('Q', n, n, n, arr, ldwrkr,
                                workitauq, workiwork,
                                lwork-iwork+1, ierr);
                      iwork = ie + n;
    
                      // Perform bidiagonal QR iteration, computing left
                      // singular vectors of R in WORK(IR)
                      // (Workspace: need n*n+bdspac)
     
                      gi.dbdsqr('U', n, 0, n, 0, s, workie, dumArr, 1,
                                arr, ldwrkr, dumArr, 1, workiwork, info);
                      iu = ie + n;
    
                      // Multiply Q in A by left singular vectors of R in
                      // WORK(IR), storing result in WORK(IU) and copying to A
                      // (Workspace: need n*n+2*n, prefer n*n+m*n+n)
     
                      for (i = 1; i <= m; i += ldwrku) {
                         chunk = Math.min(m-i+1, ldwrku);
                         arr2 = new double[chunk][n];
                         for (j = 0; j < chunk; j++) {
                             for (k = 0; k < n; k++) {
                                 arr2[j][k] = A[i-1+j][k];
                             }
                         }
                         arr3 = new double[chunk][n];
                         for (j = 0; j < n; j++) {
                             for (L = 0; L < n; L++) {
                                 if (arr[L][j] != 0.0) {
                                     for (p = 0; p < chunk; p++) {
                                         arr3[p][j] = arr3[p][j] + (arr[L][j] * arr2[p][L]);
                                     }
                                 }
                             }
                         }
                         //ge.dgemm('N', 'N', chunk, n, n, 1.0, arr2,
                                     //lda, arr, ldwrkr, 0.0,
                                     //arr3, ldwrku);
                         for (j = 0; j < chunk; j++) {
                             for (k = 0; k < n; k++) {
                                 A[i-1+j][k] = arr3[j][k];
                             }
                         }
                      } // for (i = 1; i <= m; i += ldwrku)
    
                   } // if (lwork >= n*n+Math.max(4*n, bdspac))    
                   else {
    
                      // Insufficient workspace for a fast algorithm
    
                      ie = 1;
                      itauq = ie + n;
                      itaup = itauq + n;
                      iwork = itaup + n;
    
                      // Bidiagonalize A
                      // (Workspace: need 3*n+m, prefer 3*n+(m+n)*nb)
                      workitauq = new double[n];
                      workitaup = new double[n];
                      workiwork = new double[Math.max(1, lwork-iwork+1)];
                      gi.dgebrd(m, n, A, lda, s, work,
                                workitauq, workitaup,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Generate left vectors bidiagonalizing A
                      // (Workspace: need 4*n, prefer 3*n+n*nb)
    
                      gi.dorgbr('Q', m, n, n, A, lda, workitauq,
                                workiwork, lwork-iwork+1, ierr);
                      iwork = ie + n;
    
                      // Perform bidiagonal QR iteration, computing left
                      // singular vectors of A in A
                      // (Workspace: need bdspac)
                      workiwork = new double[4*n];
    
                      gi.dbdsqr('U', n, 0, m, 0, s, work, dumArr, 1,
                                A, lda, dumArr, 1, workiwork, info);
    
                   } // else 
    
                } // else if (wntuo && wntvn)
                else if (wntuo && wntvas) {
     
                   // Path 3 (m much larger than n, jobu = 'O', jobvt = 'S' or 'A')
                   // n left singular vectors to be overwritten on A and
                   // n right singular vectors to be computed in VT
    
                   if (lwork >= n*n+Math.max(4*n, bdspac)) {
    
                      // Sufficient workspace for a fast algorithm
     
                      ir = 1;
                      if (lwork >= Math.max(wrkbl, lda*n+n)+lda*n) {
     
                         // WORK(IU) is lda by n and WORK(IR) is lda by n
     
                         ldwrku = lda;
                         ldwrkr = lda;
                      }
                      else if (lwork >= Math.max(wrkbl, lda*n+n)+n*n) {
    
                         // WORK(IU) is lda by n and WORK(IR) is n by n
    
                         ldwrku = lda;
                         ldwrkr = n;
                      }
                      else {
    
                         // WORK(IU) is ldwrku by n and WORK(IR) is n by n
    
                         ldwrku = (lwork-n*n-n) / n;
                         ldwrkr = n;
                      }
                      itau = ir + ldwrkr*n;
                      iwork = itau + n;
    
                      // Compute A=Q*R
                      // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
                      workitau = new double[n];
                      workiwork = new double[Math.max(1, lwork-iwork+1)];
                      ge.dgeqrf(m, n, A, lda, workitau,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Copy R to VT, zeroing out below it
    
                      ge.dlacpy('U', n, n, A, lda, VT, ldvt);
                      if (n  > 1) {
                         arr = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i][j] = VT[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0, arr, ldvt);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 VT[i+1][j] = arr[i][j];
                             }
                         }
                      }
    
                      // Generate Q in A
                      // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
    
                      ge.dorgqr(m, n, n, A, lda, workitau,
                                workiwork, lwork-iwork+1, ierr);
                      ie = itau;
                      itauq = ie + n;
                      itaup = itauq + n;
                      iwork = itaup + n;
    
                      // Bidiagonalize R in VT, copying result to WORK(IR)
                      // (Workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                      workie = new double[n];
                      workitauq = new double[n];
                      workitaup = new double[n];
                      workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                      gi.dgebrd(n, n, VT, ldvt, s, workie,
                                workitauq, workitaup,
                                workiwork, lwork-iwork+1, ierr);
                      arr = new double[n][n];
                      ge.dlacpy('L', n, n, VT, ldvt, arr, ldwrkr);
    
                      // Generate left vectors bidiagonalizing R in WORK(IR)
                      // (Workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
    
                      gi.dorgbr('Q', n, n, n, arr, ldwrkr,
                                workitauq, workiwork,
                                lwork-iwork+1, ierr);
    
                      // Generate right vectors bidiagonalizing R in VT
                      // (Workspace: need n*n+4*n-1, prefer n*n+3*n+(n-1)*nb)
    
                      gi.dorgbr('P', n, n, n, VT, ldvt, workitaup,
                                workiwork, lwork-iwork+1, ierr);
                      iwork = ie + n;
    
                      // Perform bidiagonal QR iteration, computing left
                      // singular vectors of R in WORK(IR) and computing right
                      // singular vectors of R in VT
                      // (Workspace: need n*n+bdspac)
    
                      gi.dbdsqr('U', n, n, n, 0, s, workie, VT, ldvt,
                                arr, ldwrkr, dumArr, 1,
                                workiwork, info);
                      iu = ie + n;
    
                      //  Multiply Q in A by left singular vectors of R in
                      // WORK(IR), storing result in WORK(IU) and copying to A
                      // (Workspace: need n*n+2*n, prefer n*n+m*n+n)
    
                     for (i = 1; i <= m; i += ldwrku) {
                         chunk = Math.min(m-i+1, ldwrku);
                         arr2 = new double[chunk][n];
                         for (j = 0; j < chunk; j++) {
                             for (k = 0; k < n; k++) {
                                 arr2[j][k] = A[i-1+j][k];
                             }
                         }
                         arr3 = new double[chunk][n];
                         for (j = 0; j < n; j++) {
                             for (L = 0; L < n; L++) {
                                 if (arr[L][j] != 0.0) {
                                     for (p = 0; p < chunk; p++) {
                                         arr3[p][j] = arr3[p][j] + (arr[L][j] * arr2[p][L]);
                                     }
                                 }
                             }
                         }
                         //ge.dgemm('N', 'N', chunk, n, n, 1.0, arr2,
                                     //lda, arr, ldwrkr, 0.0,
                                     //arr3, ldwrku);
                         for (j = 0; j < chunk; j++) {
                             for (k = 0; k < n; k++) {
                                 A[i-1+j][k] = arr3[j][k];
                             }
                         }
                     } // for (i = 1; i <= m; i += ldwrku)
    
                   } // if (lwork >= n*n+Math.max(4*n, bdspac))
                   else {
    
                      // Insufficient workspace for a fast algorithm
    
                      itau = 1;
                      iwork = itau + n;
    
                      // Compute A=Q*R
                      // (Workspace: need 2*n, prefer n+n*nb)
                      workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                      ge.dgeqrf(m, n, A, lda, work,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Copy R to VT, zeroing out below it
     
                      ge.dlacpy('U', n, n, A, lda, VT, ldvt);
                      if (n  > 1) {
                         arr = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i][j] = VT[i+1][j];
                             }
                         }
                         ge.dlaset( 'L', n-1, n-1, 0.0, 0.0, arr, ldvt);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 VT[i+1][j] = arr[i][j];
                             }
                         }
                      }
    
                      // Generate Q in A
                      // (Workspace: need 2*n, prefer n+n*nb)
    
                      ge.dorgqr(m, n, n, A, lda, work,
                                workiwork, lwork-iwork+1, ierr);
                      ie = itau;
                      itauq = ie + n;
                      itaup = itauq + n;
                      iwork = itaup + n;
    
                      // Bidiagonalize R in VT
                      // (Workspace: need 4*N, prefer 3*N+2*N*NB)
                      workitauq = new double[n];
                      workitaup = new double[n];
                      workiwork = new double[Math.max(1, lwork-iwork+1)];
                      gi.dgebrd(n, n, VT, ldvt, s, work,
                                workitauq, workitaup,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Multiply Q in A by left vectors bidiagonalizing R
                      // (Workspace: need 3*n+m, prefer 3*n+m*nb)
    
                      gi.dormbr('Q', 'R', 'N', m, n, n, VT, ldvt,
                                workitauq, A, lda, workiwork,
                                lwork-iwork+1, ierr);
    
                      // Generate right vectors bidiagonalizing R in VT
                      // (Workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
   
                      gi.dorgbr('P', n, n, n, VT, ldvt, workitaup,
                                workiwork, lwork-iwork+1, ierr);
                      iwork = ie + n;
     
                      // Perform bidiagonal QR iteration, computing left
                      // singular vectors of A in A and computing right
                      // singular vectors of A in VT
                      // (Workspace: need bdspac)
    
                      gi.dbdsqr('U', n, n, m, 0, s, work, VT, ldvt,
                                A, lda, dumArr, 1, workiwork, info);
    
                   } // else
    
                } // else if (wntuo && wntvas)
                else if (wntus) {
    
                   if (wntvn) {
    
                      // Path 4 (m much larger than n, jobu = 'S', jobvt = 'N')
                      // n left singular vectors to be computed in U and
                      // no right singular vectors to be computed
    
                      if (lwork >= n*n+Math.max(4*n, bdspac)) {
    
                         // Sufficient workspace for a fast algorithm
     
                         ir = 1;
                         if (lwork >= wrkbl+lda*n) {
     
                            // WORK(IR) is lda by n
     
                            ldwrkr = lda;
                         }
                         else {
     
                            // WORK(IR) is n by n
     
                            ldwrkr = n;
                         }
                         itau = ir + ldwrkr*n;
                         iwork = itau + n;
    
                         // Compute A=Q*R
                         // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
                         workitau = new double[n];
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         ge.dgeqrf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy R to WORK(IR), zeroing out below it
                         arr = new double[n][n];
                         ge.dlacpy( 'U', n, n, A, lda, arr, ldwrkr);
                         arr2 = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0 ; j < n-1; j++) {
                                 arr2[i][j] = arr[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0,
                                   arr2, ldwrkr);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0 ; j < n-1; j++) {
                                 arr[i+1][j] = arr2[i][j];
                             }
                         }
    
                         // Generate Q in A
                         // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
     
                         ge.dorgqr(m, n, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
    
                         // Bidiagonalize R in WORK(IR)
                         // (Workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                         workie = new double[n];
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd(n, n, arr, ldwrkr, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate left vectors bidiagonalizing R in WORK(IR)
                         // (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
     
                         gi.dorgbr('Q', n, n, n, arr, ldwrkr,
                                   workitauq, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of R in WORK(IR)
                         // (Workspace: need n*n+bdspac)
     
                         gi.dbdsqr('U', n, 0, n, 0, s, workie, dumArr,
                                   1, arr, ldwrkr, dumArr, 1,
                                   workiwork, info);
    
                         // Multiply Q in A by left singular vectors of R in
                         // WORK(IR), storing result in U
                         // (Workspace: need n*n)
     
                         ge.dgemm('N', 'N', m, n, n, 1.0, A, lda,
                                  arr, ldwrkr, 0.0, U, ldu);
    
                      } // if (lwork >= n*n+Math.max(4*n, bdspac)) 
                      else {
    
                         // Insufficient workspace for a fast algorithm
    
                         itau = 1;
                         iwork = itau + n;
    
                         // Compute A=Q*R, copying result to U
                         // (Workspace: need 2*n, prefer n+n*nb)
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         ge.dgeqrf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, n, A, lda, U, ldu);
    
                         // Generate Q in U
                         // (Workspace: need 2*N, prefer N+N*NB)
    
                         ge.dorgqr(m, n, n, U, ldu, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
    
                         // Zero out below R in A
                         arr = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i][j] = A[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0, arr, lda);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 A[i+1][j] = arr[i][j];
                             }
                         }
    
                         // Bidiagonalize R in A
                         // (Workspace: need 4*n, prefer 3*n+2*n*nb)
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd(n, n, A, lda, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Multiply Q in U by left vectors bidiagonalizing R
                         // (Workspace: need 3*n+m, prefer 3*n+m*nb)
     
                         gi.dormbr('Q', 'R', 'N', m, n, n, A, lda,
                                   workitauq, U, ldu, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of A in U
                         // (Workspace: need bdspac)
     
                         gi.dbdsqr('U', n, 0, m, 0, s, work, dumArr,
                                   1, U, ldu, dumArr, 1, workiwork, info);
    
                      } // else
    
                   } // if (wntvn)
                   else if (wntvo) {
    
                      // Path 5 (m much larger than n, jobu = 'S', jobvt = 'O')
                      // n left singular vectors to be computed in U and
                      // n right singular vectors to be overwritten on A
    
                      if (lwork >= 2*n*n+Math.max(4*n, bdspac)) {
    
                         // Sufficient workspace for a fast algorithm
     
                         iu = 1;
                         if (lwork >= wrkbl+2*lda*n) {
    
                            // WORK(IU) is lda by n and WORK(IR) is lda by n
    
                            ldwrku = lda;
                            ir = iu + ldwrku*n;
                            ldwrkr = lda;
                         }
                         else if (lwork >= wrkbl+(lda+n)*n) {
    
                            // WORK(IU) is lda by n and WORK(IR) is n by n
    
                            ldwrku = lda;
                            ir = iu + ldwrku*n;
                            ldwrkr = n;
                         }
                         else {
     
                            // WORK(IU) is n by n and WORK(IR) is n by n
    
                            ldwrku = n;
                            ir = iu + ldwrku*n;
                            ldwrkr = n;
                         }
                         itau = ir + ldwrkr*n;
                         iwork = itau + n;
     
                         // Compute A=Q*R
                         // (Workspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                         workitau = new double[n];
                         workiwork = new double[Math.max(1, lwork-iwork+1)];
                         ge.dgeqrf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy R to WORK(IU), zeroing out below it
                         arr = new double[n][n];
                         ge.dlacpy('U', n, n, A, lda, arr, ldwrku);
                         arr2 = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr2[i][j] = arr[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0, arr2, ldwrku);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i+1][j] = arr2[i][j];
                             }
                         }
    
                         // Generate Q in A
                         // (Workspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
    
                         ge.dorgqr(m, n, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
     
                         // Bidiagonalize R in WORK(IU), copying result to
                         // WORK(IR)
                         // (Workspace: need 2*n*n+4*n,
                                     // prefer 2*n*n+3*n+2*n*nb)
                         workie = new double[n];
                         workitauq = new double[n];
                         workitaup = new double[n];
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         gi.dgebrd(n, n, arr, ldwrku, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         arr2 = new double[n][n];
                         ge.dlacpy('U', n, n, arr, ldwrku,
                                   arr2, ldwrkr);
    
                         // Generate left bidiagonalizing vectors in WORK(IU)
                         // (Workspace: need 2*n*n+4*n, prefer 2*n*n+3*n+n*nb)
    
                         gi.dorgbr('Q', n, n, n, arr, ldwrku,
                                   workitauq, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate right bidiagonalizing vectors in WORK(IR)
                         // (Workspace: need 2*n*n+4*n-1,
                                     // prefer 2*n*n+3*n+(n-1)*nb)
    
                         gi.dorgbr('P', n, n, n, arr2, ldwrkr,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of R in WORK(IU) and computing
                         // right singular vectors of R in WORK(IR)
                         // (Workspace: need 2*n*n+bdspac)
     
                         gi.dbdsqr('U', n, n, n, 0, s, workie,
                                   arr2, ldwrkr, arr,
                                   ldwrku, dumArr, 1, workiwork, info);
    
                         // Multiply Q in A by left singular vectors of R in
                         // WORK(IU), storing result in U
                         // (Workspace: need N*N)
     
                         ge.dgemm('N', 'N', m, n, n, 1.0, A, lda,
                                  arr, ldwrku, 0.0, U, ldu);
     
                         // Copy right singular vectors of R to A
                         // (Workspace: need n*n)
     
                         ge.dlacpy('F', n, n, arr2, ldwrkr, A, lda);
     
                      } // if (lwork >= 2*n*n+Math.max(4*n, bdpsac))
                      else {
     
                         // Insufficient workspace for a fast algorithm
     
                         itau = 1;
                         iwork = itau + n;
     
                         // Compute A=Q*R, copying result to U
                         // (Workspace: need 2*n, prefer n+n*nb)
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         ge.dgeqrf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, n, A, lda, U, ldu);
    
                         // Generate Q in U
                         // (Workspace: need 2*n, prefer n+n*nb)
    
                         ge.dorgqr(m, n, n, U, ldu, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
     
                         // Zero out below R in A
                         arr = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i][j] = A[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0, arr, lda);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 A[i+1][j] = arr[i][j];
                             }
                         }
    
                         // Bidiagonalize R in A
                         // (Workspace: need 4*n, prefer 3*n+2*n*nb)
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd( n, n, A, lda, s, work,
                                    workitauq, workitaup,
                                    workiwork, lwork-iwork+1, ierr);
    
                         // Multiply Q in U by left vectors bidiagonalizing R
                         // (Workspace: need 3*n+m, prefer 3*n+m*nb)
     
                         gi.dormbr('Q', 'R', 'N', m, n, n, A, lda,
                                   workitauq, U, ldu, workiwork,
                                   lwork-iwork+1, ierr);
     
                         // Generate right vectors bidiagonalizing R in A
                         // (Workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
    
                         gi.dorgbr('P', n, n, n, A, lda, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + n;
     
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of A in U and computing right
                         // singular vectors of A in A
                         // (Workspace: need bdspac)
    
                         gi.dbdsqr('U', n, n, m, 0, s, work, A,
                                   lda, U, ldu, dumArr, 1, workiwork, info);
    
                      } // else 
    
                   } // else if (wntvo)
                   else if (wntvas) {
    
                      // Path 6 (m much larger than n, jobu = 'S', jobvt = 'S'or 'A')
                      // n left singular vectors to be computed in U and
                      // n right singular vectors to be computed in VT
    
                      if (lwork >= n*n+Math.max(4*n, bdspac)) {
     
                         // Sufficient workspace for a fast algorithm
     
                         iu = 1;
                         if (lwork >= wrkbl+lda*n) {
    
                            // WORK(IU) is lda by n
    
                            ldwrku = lda;
                         }
                         else {
    
                            // WORK(IU) is n by n
    
                            ldwrku = n;
                         }
                         itau = iu + ldwrku*n;
                         iwork = itau + n;
     
                         // Compute A=Q*R
                         // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
                         workitau = new double[n];
                         workiwork = new double[Math.max(1, lwork-iwork+1)];
                         ge.dgeqrf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy R to WORK(IU), zeroing out below it
                         arr = new double[n][n];
                         ge.dlacpy('U', n, n, A, lda, arr, ldwrku);
                         arr2 = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr2[i][j] = arr[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0,
                                   arr2, ldwrku);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i+1][j] = arr2[i][j];
                             }
                         }
    
                         // Generate Q in A
                         // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
     
                         ge.dorgqr(m, n, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
    
                         // Bidiagonalize R in WORK(IU), copying result to VT
                         // (Workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
    
                         workie = new double[n];
                         workitauq = new double[n];
                         workitaup = new double[n];
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         gi.dgebrd(n, n, arr, ldwrku, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         ge.dlacpy('U', n, n, arr, ldwrku, VT, ldvt);
    
                         // Generate left bidiagonalizing vectors in WORK(IU)
                         // (Workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
     
                         gi.dorgbr('Q', n, n, n, arr, ldwrku,
                                      workitauq, workiwork,
                                      lwork-iwork+1, ierr);
    
                         // Generate right bidiagonalizing vectors in VT
                         // (Workspace: need n*n+4*n-1,
                                     // prefer n*n+3*n+(n-1)*nb)
    
                         gi.dorgbr('P', n, n, n, VT, ldvt, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + n;
     
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of R in WORK(IU) and computing
                         // right singular vectors of R in VT
                         // (Workspace: need n*n+bdspac)
    
                         gi.dbdsqr('U', n, n, n, 0, s, workie, VT,
                                   ldvt, arr, ldwrku, dumArr, 1,
                                   workiwork, info);
    
                         // Multiply Q in A by left singular vectors of R in
                         // WORK(IU), storing result in U
                         // (Workspace: need n*n)
    
                         ge.dgemm('N', 'N', m, n, n, 1.0, A, lda,
                                  arr, ldwrku, 0.0, U, ldu);
    
                      } // if (lwork >= n*n+Math.max(4*n, bdspac))
                      else {
    
                         // Insufficient workspace for a fast algorithm
    
                         itau = 1;
                         iwork = itau + n;
    
                         // Compute A=Q*R, copying result to U
                         // (Workspace: need 2*n, prefer n+n*nb)
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         ge.dgeqrf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, n, A, lda, U, ldu);
    
                         // Generate Q in U
                         // (Workspace: need 2*n, prefer n+n*nb)
     
                         ge.dorgqr(m, n, n, U, ldu, work,
                                   workiwork, lwork-iwork+1, ierr);
     
                         // Copy R to VT, zeroing out below it
     
                         ge.dlacpy('U', n, n, A, lda, VT, ldvt);
                         if (n  > 1) {
                            arr = new double[n-1][n-1];
                            for (i = 0; i < n-1; i++) {
                                for (j = 0; j < n-1; j++) {
                                    arr[i][j] = VT[i+1][j];
                                }
                            }
                            ge.dlaset('L', n-1, n-1, 0.0, 0.0,
                                      arr, ldvt);
                            for (i = 0; i < n-1; i++) {
                                for (j = 0; j < n-1; j++) {
                                    VT[i+1][j] = arr[i][j];
                                }
                            }
                         }
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
     
                         // Bidiagonalize R in VT
                         // (Workspace: need 4*n, prefer 3*n+2*n*nb)
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd(n, n, VT, ldvt, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Multiply Q in U by left bidiagonalizing vectors in VT
                         // (Workspace: need 3*n+m, prefer 3*n+m*nb)
    
                         gi.dormbr('Q', 'R', 'N', m, n, n, VT, ldvt,
                                   workitauq, U, ldu, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate right bidiagonalizing vectors in VT
                         // (Workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
    
                         gi.dorgbr('P', n, n, n, VT, ldvt, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of A in U and computing right
                         // singular vectors of A in VT
                         // (Workspace: need bdspac)
     
                         gi.dbdsqr('U', n, n, m, 0, s, work, VT,
                                   ldvt, U, ldu, dumArr, 1, workiwork, info);
    
                      } // else
    
                   } // else if (wntvas)
    
                } // else if (wntus)
                else if (wntua) {
    
                   if (wntvn) {
    
                      // Path 7 (m much larger than n, jobu = 'A', jobvt = 'N')
                      // m left singular vectors to be computed in U and
                      // no right singular vectors to be computed
    
                      if (lwork >= n*n+Math.max(n+m, Math.max(4*n, bdspac))) {
    
                         // Sufficient workspace for a fast algorithm
     
                         ir = 1;
                         if (lwork >= wrkbl + lda*n) {
    
                            // WORK(IR) is lda by n
    
                            ldwrkr = lda;
                         }
                         else {
    
                            // WORK(IR) is n by n
    
                            ldwrkr = n;
                         }
                         itau = ir + ldwrkr*n;
                         iwork = itau + n;
     
                         // Compute A=Q*R, copying result to U
                         // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
                         workitau = new double[n];
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         ge.dgeqrf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, n, A, lda, U, ldu);
    
                         // Copy R to WORK(IR), zeroing out below it
     
                         arr = new double[n][n];
                         ge.dlacpy('U', n, n, A, lda, arr, ldwrkr);
                         arr2 = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr2[i][j] = arr[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0,
                                   arr2, ldwrkr);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i+1][j] = arr2[i][j];
                             }
                         }
    
                         // Generate Q in U
                         // (Workspace: need n*n+n+m, prefer n*n+n+m*nb)
    
                         ge.dorgqr(m, m, n, U, ldu, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
    
                         // Bidiagonalize R in WORK(IR)
                         // (Workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                         workie = new double[n];
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd(n, n, arr, ldwrkr, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate left bidiagonalizing vectors in WORK(IR)
                         // (Workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
     
                         gi.dorgbr('Q', n, n, n, arr, ldwrkr,
                                   workiwork, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of R in WORK(IR)
                         // (Workspace: need n*n+bdspac)
    
                         gi.dbdsqr('U', n, 0, n, 0, s, workie, dumArr,
                                   1, arr, ldwrkr, dumArr, 1,
                                   workiwork, info);
    
                         //  Multiply Q in U by left singular vectors of R in
                         // WORK(IR), storing result in A
                         // (Workspace: need n*n)
     
                         ge.dgemm('N', 'N', m, n, n, 1.0, U, ldu,
                                  arr, ldwrkr, 0.0, A, lda);
    
                         // Copy left singular vectors of A from A to U
    
                         ge.dlacpy('F', m, n, A, lda, U, ldu);
    
                      } // if (lwork >= n*n+Math.max(n+m, Math.max(4*n, bdspac))
                      else {
    
                         // Insufficient workspace for a fast algorithm
     
                         itau = 1;
                         iwork = itau + n;
    
                         // Compute A=Q*R, copying result to U
                         // (Workspace: need 2*n, prefer n+n*nb)
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         ge.dgeqrf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, n, A, lda, U, ldu);
    
                         // Generate Q in U
                         // (Workspace: need n+m, prefer n+m*nb)
    
                         ge.dorgqr(m, m, n, U, ldu, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
    
                         // Zero out below R in A
                         arr = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i][j] = A[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0, arr, lda);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 A[i+1][j] = arr[i][j];
                             }
                         }
    
                         // Bidiagonalize R in A
                         // (Workspace: need 4*n, prefer 3*n+2*n*nb)
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd(n, n, A, lda, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Multiply Q in U by left bidiagonalizing vectors
                         // in A
                         // (Workspace: need 3*n+m, prefer 3*n+m*nb)
    
                         gi.dormbr('Q', 'R', 'N', m, n, n, A, lda,
                                   workitauq, U, ldu, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of A in U
                         // (Workspace: need bdspac)
    
                         gi.dbdsqr('U', n, 0, m, 0, s, work, dumArr,
                                   1, U, ldu, dumArr, 1, workiwork, info);
    
                      } // else
    
                   } // if (wntvn)
                   else if (wntvo) {
    
                      // Path 8 (m much larger than n, jobu = 'A', jobvt = 'O')
                      // m left singular vectors to be computed in U and
                      // n right singular vectors to be overwritten on A
    
                      if (lwork >= 2*n*n+Math.max(n+m, Math.max(4*n, bdspac))) {
    
                         // Sufficient workspace for a fast algorithm
     
                         iu = 1;
                         if (lwork >= wrkbl+2*lda*n) {
     
                            // WORK(IU) is lda by n and WORK(IR) is lda by n
    
                            ldwrku = lda;
                            ir = iu + ldwrku*n;
                            ldwrkr = lda;
                         }
                         else if (lwork >= wrkbl+(lda+n)*n) {
    
                            // WORK(IU) is lda by n and WORK(IR) is n by n
    
                            ldwrku = lda;
                            ir = iu + ldwrku*n;
                            ldwrkr = n;
                         }
                         else {
    
                            // WORK(IU) is n by n and WORK(IR) is n by n
    
                            ldwrku = n;
                            ir = iu + ldwrku*n;
                            ldwrkr = n;
                         }
                         itau = ir + ldwrkr*n;
                         iwork = itau + n;
    
                         // Compute A=Q*R, copying result to U
                         // (Workspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                         workitau = new double[n];
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         ge.dgeqrf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, n, A, lda, U, ldu);
    
                         // Generate Q in U
                         // (Workspace: need 2*n*n+n+m, prefer 2*n*n+n+m*nb)
    
                         ge.dorgqr(m, m, n, U, ldu, workitau,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy R to WORK(IU), zeroing out below it
                         arr = new double[n][n];
                         ge.dlacpy( 'U', n, n, A, lda, arr, ldwrku);
                         arr2 = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr2[i][j] = arr[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0, arr2, ldwrku);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i+1][j] = arr[i][j];
                             }
                         }
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
    
                         // Bidiagonalize R in WORK(IU), copying result to
                         // WORK(IR)
                         // (Workspace: need 2*n*n+4*n,
                                     // prefer 2*n*n+3*n+2*n*nb)
                         workie = new double[n];
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd(n, n, arr, ldwrku, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         arr2 = new double[n][n];
                         ge.dlacpy('U', n, n, arr, ldwrku,
                                   arr2, ldwrkr);
    
                         // Generate left bidiagonalizing vectors in WORK(IU)
                         // (Workspace: need 2*n*n+4*n, prefer 2*n*n+3*n+n*nb)
    
                         gi.dorgbr('Q', n, n, n, arr, ldwrku,
                                   workitauq, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate right bidiagonalizing vectors in WORK(IR)
                         // (Workspace: need 2*n*n+4*n-1,
                                     // prefer 2*n*n+3*n+(n-1)*nb)
    
                         gi.dorgbr('P', n, n, n, arr2, ldwrkr,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of R in WORK(IU) and computing
                         // right singular vectors of R in WORK(IR)
                         // (Workspace: need 2*n*n+bdspac)
     
                         gi.dbdsqr('U', n, n, n, 0, s, workie,
                                   arr2, ldwrkr, arr,
                                   ldwrku, dumArr, 1, workiwork, info);
     
                         // Multiply Q in U by left singular vectors of R in
                         // WORK(IU), storing result in A
                         // (Workspace: need n*n)
    
                         ge.dgemm('N', 'N', m, n, n, 1.0, U, ldu,
                                  arr, ldwrku, 0.0, A, lda);
    
                         // Copy left singular vectors of A from A to U
    
                         ge.dlacpy('F', m, n, A, lda, U, ldu);
    
                         // Copy right singular vectors of R from WORK(IR) to A
     
                         ge.dlacpy('F', n, n, arr2, ldwrkr, A, lda);
    
                      } // if (lwork >= 2*n*n+Math.max(n+m, Math.max(4*n, bdspac)))
                      else {
    
                         // Insufficient workspace for a fast algorithm
    
                         itau = 1;
                         iwork = itau + n;
    
                         // Compute A=Q*R, copying result to U
                         // (Workspace: need 2*n, prefer n+n*nb)
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         ge.dgeqrf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, n, A, lda, U, ldu);
                         
                         // Generate Q in U
                         // (Workspace: need n+m, prefer n+m*nb)
    
                         ge.dorgqr(m, m, n, U, ldu, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
    
                         // Zero out below R in A
                         arr = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i][j] = A[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0, arr, lda);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 A[i+1][j] = arr[i][j];
                             }
                         }
    
                         // Bidiagonalize R in A
                         // (Workspace: need 4*n, prefer 3*n+2*n*nb)
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd(n, n, A, lda, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
     
                         // Multiply Q in U by left bidiagonalizing vectors
                         // in A
                         // (Workspace: need 3*n+m, prefer 3*n+m*nb)
    
                         gi.dormbr('Q', 'R', 'N', m, n, n, A, lda,
                                   workitauq, U, ldu, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate right bidiagonalizing vectors in A
                         // (Workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
    
                         gi.dorgbr('P', n, n, n, A, lda, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of A in U and computing right
                         // singular vectors of A in A
                         // (Workspace: need bdspac)
     
                         gi.dbdsqr('U', n, n, m, 0, s, work, A,
                                   lda, U, ldu, dumArr, 1, workiwork,
                                   info);
    
                      } // else
    
                   } // else if (wntvo)
                   else if (wntvas) {
    
                      // Path 9 (m much larger than n, jobu = 'A', jobvt = 'S' or 'A')
                      // m left singular vectors to be computed in U and
                      // n right singular vectors to be computed in VT
    
                      if (lwork >= n*n+Math.max(n+m, Math.max(4*n, bdspac))) {
    
                         // Sufficient workspace for a fast algorithm
     
                         iu = 1;
                         if (lwork >= wrkbl+lda*n) {
    
                            // WORK(IU) is lda by n
     
                            ldwrku = lda;
                         }
                         else {
    
                            // WORK(IU) is n by n
     
                            ldwrku = n;
                         }
                         itau = iu + ldwrku*n;
                         iwork = itau + n;
    
                         // Compute A=Q*R, copying result to U
                         // (Workspace: need n*n+2*n, prefer n*n+n+n*nb)
                         workitau = new double[n];
                         workiwork = new double[Math.max(1, lwork-iwork+1)];
                         ge.dgeqrf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, n, A, lda, U, ldu);
    
                         // Generate Q in U
                         // (Workspace: need n*n+n+m, prefer n*n+n+m*nb)
    
                         ge.dorgqr(m, m, n, U, ldu, workitau,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy R to WORK(IU), zeroing out below it
                         arr = new double[n][n];
                         ge.dlacpy( 'U', n, n, A, lda, arr, ldwrku);
                         arr2 = new double[n-1][n-1];
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr2[i][j] = arr[i+1][j];
                             }
                         }
                         ge.dlaset('L', n-1, n-1, 0.0, 0.0,
                                   arr2, ldwrku);
                         for (i = 0; i < n-1; i++) {
                             for (j = 0; j < n-1; j++) {
                                 arr[i+1][j] = arr2[i][j];
                             }
                         }
                         
                         ie = itau;
                         itauq = ie+ n;
                         itaup = itauq + n;
                         iwork = itaup + n;
    
                         // Bidiagonalize R in WORK(IU), copying result to VT
                         // (Workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                         workie = new double[n];
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd( n, n, arr, ldwrku, s,
                                    workie, workitauq,
                                    workitaup, workiwork,
                                    lwork-iwork+1, ierr);
                         ge.dlacpy('U', n, n, arr, ldwrku, VT, ldvt);
    
                         // Generate left bidiagonalizing vectors in WORK(IU)
                         // (Workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
    
                         gi.dorgbr('Q', n, n, n, arr, ldwrku,
                                   workitauq, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate right bidiagonalizing vectors in VT
                         // (Workspace: need n*n+4*n-1,
                                     // prefer n*n+3*n+(n-1)*nb)
    
                         gi.dorgbr('P', n, n, n, VT, ldvt, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of R in WORK(IU) and computing
                         // right singular vectors of R in VT
                         // (Workspace: need n*n+bdspac)
    
                         gi.dbdsqr('U', n, n, n, 0, s, workie, VT,
                                   ldvt, arr, ldwrku, dumArr, 1,
                                   workiwork, info);
    
                         // Multiply Q in U by left singular vectors of R in
                         // WORK(IU), storing result in A
                         // (Workspace: need n*n)
     
                         ge.dgemm('N', 'N', m, n, n, 1.0, U, ldu,
                                  arr, ldwrku, 0.0, A, lda);
    
                         // Copy left singular vectors of A from A to U
    
                         ge.dlacpy('F', m, n, A, lda, U, ldu);
    
                      } // if (lwork >= n*n+Math.max(n+m, Math.max(4*n, bdspac)))
                      else {
    
                         // Insufficient workspace for a fast algorithm
    
                         itau = 1;
                         iwork = itau + n;
    
                         // Compute A=Q*R, copying result to U
                         // (Workspace: need 2*n, prefer n+n*nb)
                         workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                         ge.dgeqrf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, n, A, lda, U, ldu);
    
                         // Generate Q in U
                         // (Workspace: need n+m, prefer n+m*nb)
    
                         ge.dorgqr(m, m, n, U, ldu, work,
                                   workiwork, lwork-iwork+1, ierr);
     
                         // Copy R from A to VT, zeroing out below it
     
                         ge.dlacpy('U', n, n, A, lda, VT, ldvt);
                         if (n > 1) {
                            arr = new double[n-1][n-1];
                            for (i = 0; i < n-1; i++) {
                                for (j = 0; j < n-1; j++) {
                                    arr[i][j] = VT[i+1][j];
                                }
                            }
                            ge.dlaset('L', n-1, n-1, 0.0, 0.0,
                                         arr, ldvt);
                            for (i = 0; i < n-1; i++) {
                                for (j = 0; j < n-1; j++) {
                                    VT[i+1][j] = arr[i][j];
                                }
                            }
                         }
                         ie = itau;
                         itauq = ie + n;
                         itaup = itauq + n;
                         iwork = itaup + n;
    
                         // Bidiagonalize R in VT
                         // (Workspace: need 4*n, prefer 3*n+2*n*nb)
                         workitauq = new double[n];
                         workitaup = new double[n];
                         gi.dgebrd(n, n, VT, ldvt, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Multiply Q in U by left bidiagonalizing vectors
                         // in VT
                         // (Workspace: need 3*n+m, prefer 3*n+m*nb)
    
                         gi.dormbr('Q', 'R', 'N', m, n, n, VT, ldvt,
                                   workitauq, U, ldu, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate right bidiagonalizing vectors in VT
                         // (Workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
    
                         gi.dorgbr('P', n, n, n, VT, ldvt, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + n;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of A in U and computing right
                         // singular vectors of A in VT
                         // (Workspace: need bdspac)
     
                         gi.dbdsqr('U', n, n, m, 0, s, work, VT,
                                   ldvt, U, ldu, dumArr, 1, workiwork,
                                   info);
    
                      } // else 
    
                   } // else if (wntvas)
    
                } // else if (wntua)
    
             } // if (m >= mnthr)
             else { // m < mnthr
    
                // m .LT. mnthr
    
                // Path 10 (m at least n, but not much larger)
                // Reduce to bidiagonal form without QR decomposition
     
                ie = 1;
                itauq = ie + n;
                itaup = itauq + n;
                iwork = itaup + n;
    
                // Bidiagonalize A
                // (Workspace: need 3*n+m, prefer 3*n+(m+n)*nb)
                workitauq = new double[n];
                workitaup = new double[n];
                workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
                gi.dgebrd(m, n, A, lda, s, work, workitauq,
                          workitaup, workiwork, lwork-iwork+1,
                          ierr);
                if (wntuas) {
     
                   // If left singular vectors desired in U, copy result to U
                   // and generate left bidiagonalizing vectors in U
                   // (Workspace: need 3*n+ncu, prefer 3*n+ncu*nb)
    
                   ge.dlacpy('L', m, n, A, lda, U, ldu);
                   if (wntus) {
                      ncu = n;
                   }
                   if (wntua) {
                      ncu = m;
                   }
                   gi.dorgbr('Q', m, ncu, n, U, ldu, workitauq,
                             workiwork, lwork-iwork+1, ierr);
                } // if (wntuas)
                if (wntvas) {
    
                   // If right singular vectors desired in VT, copy result to
                   // VT and generate right bidiagonalizing vectors in VT
                   // (Workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
    
                   ge.dlacpy('U', n, n, A, lda, VT, ldvt);
                   gi.dorgbr('P', n, n, n, VT, ldvt, workitaup,
                             workiwork, lwork-iwork+1, ierr);
                } // if (wntvas)
                if (wntuo) {
    
                   // If left singular vectors desired in A, generate left
                   // bidiagonalizing vectors in A
                   // (Workspace: need 4*n, prefer 3*n+n*nb)
    
                   gi.dorgbr('Q', m, n, n, A, lda, workitauq,
                             workiwork, lwork-iwork+1, ierr);
                } // if (wntuo)
                if (wntvo) {
    
                   // If right singular vectors desired in A, generate right
                   // bidiagonalizing vectors in A
                   // (Workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
    
                   gi.dorgbr('P', n, n, n, A, lda, workitaup,
                             workiwork, lwork-iwork+1, ierr);
                } // if (wntvo)
                iwork = ie + n;
                if (wntuas || wntuo) {
                   nru = m;
                }
                if (wntun) {
                   nru = 0;
                }
                if (wntvas || wntvo) {
                   ncvt = n;
                }
                if (wntvn) {
                   ncvt = 0;
                }
                if((!wntuo) && (!wntvo)) {
    
                   // Perform bidiagonal QR iteration, if desired, computing
                   // left singular vectors in U and computing right singular
                   // vectors in VT
                   // (Workspace: need BDSPAC)
    
                   gi.dbdsqr('U', n, ncvt, nru, 0, s, work, VT,
                             ldvt, U, ldu, dumArr, 1, workiwork, info);
                } // if((!wntuo) && (!wntvo))
                else if((!wntuo) && wntvo) {
    
                   // Perform bidiagonal QR iteration, if desired, computing
                   // left singular vectors in U and computing right singular
                   // vectors in A
                   // (Workspace: need bdspac)
    
                   gi.dbdsqr('U', n, ncvt, nru, 0, s, work, A, lda,
                             U, ldu, dumArr, 1, workiwork, info);
                } // else if((!wntuo) && wntvo)
                else {
    
                   // Perform bidiagonal QR iteration, if desired, computing
                   // left singular vectors in A and computing right singular
                   // vectors in VT
                   // (Workspace: need bdspac)
    
                   gi.dbdsqr('U', n, ncvt, nru, 0, s, work, VT,
                             ldvt, A, lda, dumArr, 1, workiwork, info);
                } // else  
    
             } // else m < mnthr
    
          } // if (m >= n)
          else { // m < n
    
             // A has more columns than rows. If A has sufficiently more
             // columns than rows, first reduce using the LQ decomposition (if
             // sufficient workspace available)
    
             if(n >= mnthr) {
    
                if (wntvn) {
    
                   // Path 1t(n much larger than m, jobvt = 'N')
                   // No right singular vectors to be computed
    
                   itau = 1;
                   iwork = itau + m;
    
                   // Compute A=L*Q
                   // (Workspace: need 2*m, prefer m+m*nb)
                   workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                   gi.dgelqf(m, n, A, lda, work, workiwork,
                             lwork-iwork+1, ierr);
    
                   // Zero out above L
                   arr = new double[m-1][m-1];
                   for (i = 0; i < m-1; i++) {
                       for (j = 0; j < m-1; j++) {
                           arr[i][j] = A[i][j+1];
                       }
                   }
                   ge.dlaset('U', m-1, m-1, 0.0, 0.0, arr, lda);
                   for (i = 0; i < m-1; i++) {
                       for (j = 0; j < m-1; j++) {
                           A[i][j+1] = arr[i][j];
                       }
                   }
                   ie = 1;
                   itauq = ie + m;
                   itaup = itauq + m;
                   iwork = itaup + m;
    
                   // Bidiagonalize L in A
                   // (Workspace: need 4*m, prefer 3*m+2*m*nb)
                   workitauq = new double[m];
                   workitaup = new double[m];
                   gi.dgebrd(m, m, A, lda, s, work, workitauq,
                             workitaup, workiwork, lwork-iwork+1,
                             ierr);
                   if (wntuo || wntuas) {
     
                      // If left singular vectors desired, generate Q
                      // (Workspace: need 4*m, prefer 3*m+m*nb)
    
                      gi.dorgbr('Q', m, m, m, A, lda, workitauq,
                                workiwork, lwork-iwork+1, ierr);
                   } // if (wntuo || wntuas)
                   iwork = ie + m;
                   nru = 0;
                   if (wntuo || wntuas) {
                      nru = m;
                   }
    
                   // Perform bidiagonal QR iteration, computing left singular
                   // vectors of A in A if desired
                   // (Workspace: need bdspac)
     
                   gi.dbdsqr('U', m, 0, nru, 0, s, work, dumArr, 1, A,
                             lda, dumArr, 1, workiwork, info);
    
                   // If left singular vectors desired in U, copy them there
     
                   if (wntuas) {
                      ge.dlacpy('F', m, m, A, lda, U, ldu);
                   }
    
                } // if (wntvn)
                else if (wntvo && wntun) {
     
                   // Path 2t(n much larger than m, jobu = 'N', jobvt = 'O')
                   //  right singular vectors to be overwritten on A and
                   // no left singular vectors to be computed
    
                   if (lwork >= m*m+Math.max(4*m, bdspac)) {
     
                      // Sufficient workspace for a fast algorithm
    
                      ir = 1;
                      if (lwork >= Math.max(wrkbl, lda*n+m)+lda*m) {
    
                         // WORK(IU) is lda by n and WORK(IR) is lda bym
    
                         ldwrku = lda;
                         chunk = n;
                         ldwrkr = lda;
                      }
                      else if (lwork >= Math.max(wrkbl, lda*n+m)+m*m) {
    
                         // WORK(IU) is lda by n and WORK(IR) is m by m
    
                         ldwrku = lda;
                         chunk = n;
                         ldwrkr = m;
                      }
                      else {
    
                         // WORK(IU) is m by chunk and WORK(IR) is m by m
    
                         ldwrku = m;
                         chunk = (lwork-m*m-m) / m;
                         ldwrkr = m;
                      }
                      itau = ir +ldwrkr*m;
                      iwork = itau + m;
    
                      // Compute A=L*Q
                      // (Workspace: need m*m+2*m, prefer m*m+m+m*nb)
                      workitau = new double[m];
                      workiwork = new double[Math.max(4*m, Math.max(1,lwork-iwork+1))];
                      gi.dgelqf(m, n, A, lda, workitau,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Copy L to WORK(IR) and zero out above it
                      arr = new double[m][m];
                      ge.dlacpy('L', m, m, A, lda, arr, ldwrkr);
                      arr2 = new double[m-1][m-1];
                      for (i = 0; i < m-1; i++) {
                          for (j = 0; j < m-1; j++) {
                              arr2[i][j] = arr[i][j+1];
                          }
                      }
                      ge.dlaset('U', m-1, m-1, 0.0, 0.0,
                                arr, ldwrkr);
                      for (i = 0; i < m-1; i++) {
                          for (j = 0; j < m-1; j++) {
                              arr[i][j+1] = arr2[i][j];
                          }
                      }
    
                      // Generate Q in A
                      // (Workspace: need m*m+2*m, prefer m*m+m+m*nb)
     
                      gi.dorglq(m, n, m, A, lda, workitau,
                                workiwork, lwork-iwork+1, ierr);
                      ie = itau;
                      itauq = ie + m;
                      itaup = itauq + m;
                      iwork = itaup + m;
    
                      // Bidiagonalize L in WORK(IR)
                      // (Workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                      workie = new double[m];
                      workitauq = new double[m];
                      workitaup = new double[m];
                      gi.dgebrd(m, m, arr, ldwrkr, s, workie,
                                workitauq, workitaup,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Generate right vectors bidiagonalizing L
                      // (Workspace: need m*m+4*m-1, prefer m*m+3*m+(m-1)*nb)
     
                      gi.dorgbr('P', m, m, m, arr, ldwrkr,
                                workitaup, workiwork,
                                lwork-iwork+1, ierr);
                      iwork = ie + m;
     
                      // Perform bidiagonal QR iteration, computing right
                      // singular vectors of L in WORK(IR)
                      // (Workspace: need m*m+bdspac)
    
                      gi.dbdsqr('U', m, m, 0, 0, s, workie,
                                arr, ldwrkr, dumArr, 1, dumArr, 1,
                                workiwork, info);
                      iu = ie + m;
    
                      // Multiply right singular vectors of L in WORK(IR) by Q
                      // in A, storing result in WORK(IU) and copying to A
                      // (Workspace: need m*m+2*m, prefer m*m+m*n+m)
    
                      for (i = 1; i <= n; i+= chunk) {
                         blk = Math.min(n-i+1, chunk);
                         arr2 = new double[m][blk];
                         for (j = 0; j < m; j++) {
                             for (k = 0; k < blk; k++) {
                                 arr2[j][k] = A[j][i-1+k];
                             }
                         }
                         arr3 = new double[m][blk];
                         ge.dgemm('N', 'N', m, blk, m, 1.0, arr,
                                  ldwrkr, arr2, lda, 0.0,
                                  arr3, ldwrku);
                         ge.dlacpy('F', m, blk, arr3, ldwrku,
                                   arr2, lda);
                         for (j = 0; j < m; j++) {
                             for (k = 0; k < blk; k++) {
                                 A[j][i-1+k] = arr2[j][k];
                             }
                         }
                      } // for (i = 1; i <= n; i+= chunk)
    
                   } // if (lwork >= m*m+Math.max(4*m, bdspac))
                   else {
    
                      // Insufficient workspace for a fast algorithm
    
                      ie = 1;
                      itauq = ie + m;
                      itaup = itauq + m;
                      iwork = itaup + m;
    
                      // Bidiagonalize A
                      // (Workspace: need 3*m+n, prefer 3*m+(m+n)*nb)
                      workitauq = new double[m];
                      workitaup = new double[m];
                      workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                      gi.dgebrd(m, n, A, lda, s, work,
                                workitauq, workitaup,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Generate right vectors bidiagonalizing A
                      // (Workspace: need 4*m, prefer 3*m+m*nb)
    
                      gi.dorgbr('P', m, n, m, A, lda, workitaup,
                                workiwork, lwork-iwork+1, ierr);
                      iwork = ie + m;
    
                      // Perform bidiagonal QR iteration, computing right
                      // singular vectors of A in A
                      // (Workspace: need bdspac)
     
                      gi.dbdsqr('L', m, n, 0, 0, s, work, A, lda,
                                dumArr, 1, dumArr, 1, workiwork, info);
    
                   } // else
    
                } // else if (wntvo && wntun)
                else if (wntvo &&  wntuas) {
    
                   // Path 3t(n much larger than m, jobu = 'S' or 'A', jobvt = 'O')
                   // m right singular vectors to be overwritten on A and
                   // m left singular vectors to be computed in U
    
                   if (lwork >= m*m+Math.max(4*m, bdspac)) {
    
                      // Sufficient workspace for a fast algorithm
     
                      ir = 1;
                      if (lwork >= Math.max(wrkbl, lda*n+m)+lda*m) {
    
                         // WORK(IU) is lda by n and WORK(IR) is lda by m
    
                         ldwrku = lda;
                         chunk = n;
                         ldwrkr = lda;
                      }
                      else if (lwork >= Math.max(wrkbl, lda*n+m)+m*m) {
    
                         // WORK(IU) is lda by n and WORK(IR) is m by m
    
                         ldwrku = lda;
                         chunk = n;
                         ldwrkr = m;
                      }
                      else {
    
                         // WORK(IU) is m by chunk and WORK(IR) is m by m
    
                         ldwrku = m;
                         chunk = (lwork-m*m-m) / m;
                         ldwrkr = m;
                      }
                      itau = ir + ldwrkr*m;
                      iwork = itau + m;
    
                      // Compute A=L*Q
                      // (Workspace: need m*m+2*m, prefer m*m+m+m*nb)
                      workitau = new double[m];
                      workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                      gi.dgelqf(m, n, A, lda, workitau,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Copy L to U, zeroing about above it
     
                      ge.dlacpy('L', m, m, A, lda, U, ldu);
                      arr = new double[m-1][m-1];
                      for (i = 0; i < m-1; i++) {
                          for (j = 0; j < m-1; j++) {
                              arr[i][j] = U[i][j+1];
                          }
                      }
                      ge.dlaset('U', m-1, m-1, 0.0, 0.0, arr,
                                ldu);
                      for (i = 0; i < m-1; i++) {
                          for (j = 0; j < m-1; j++) {
                              U[i][j+1] = arr[i][j];
                          }
                      }
    
                      // Generate Q in A
                      // (Workspace: need m*m+2*m, prefer m*m+m+m*nb)
    
                      gi.dorglq(m, n, m, A, lda, workitau,
                                workiwork, lwork-iwork+1, ierr);
                      ie = itau;
                      itauq = ie + m;
                      itaup = itauq + m;
                      iwork = itaup + m;
    
                      // Bidiagonalize L in U, copying result to WORK(IR)
                      // (Workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                      workie = new double[m];
                      workitauq = new double[m];
                      workitaup = new double[m];
                      gi.dgebrd(m, m, U, ldu, s, workie,
                                workitauq, workitaup,
                                workiwork, lwork-iwork+1, ierr);
                      arr = new double[m][m];
                      ge.dlacpy('U', m, m, U, ldu, arr, ldwrkr);
    
                      // Generate right vectors bidiagonalizing L in WORK(IR)
                      // (Workspace: need m*m+4*m-1, prefer m*m+3*m+(m-1)*nb)
    
                      gi.dorgbr('P', m, m, m, arr, ldwrkr,
                                workitaup, workiwork,
                                lwork-iwork+1, ierr);
    
                      // Generate left vectors bidiagonalizing L in U
                      // (Workspace: need m*m+4*m, prefer m*m+3*m+m*nb)
    
                      gi.dorgbr('Q', m, m, m, U, ldu, workitauq,
                                workiwork, lwork-iwork+1, ierr);
                      iwork = ie + m;
    
                      // Perform bidiagonal QR iteration, computing left
                      // singular vectors of L in U, and computing right
                      // singular vectors of L in WORK(IR)
                      // (Workspace: need m*m+bdspac)
    
                      gi.dbdsqr('U', m, m, m, 0, s, workie,
                                arr, ldwrkr, U, ldu, dumArr, 1,
                                workiwork, info);
                      iu = ie + m;
    
                      // Multiply right singular vectors of L in WORK(IR) by Q
                      // in A, storing result in WORK(IU) and copying to A
                      // (Workspace: need m*m+2*m, prefer m*m+m*n+m))
                      for (i = 1; i <= n; i+= chunk) {
                          blk = Math.min(n-i+1, chunk);
                          arr2 = new double[m][blk];
                          for (j = 0; j < m; j++) {
                              for (k = 0; k < blk; k++) {
                                  arr2[j][k] = A[j][i-1+k];
                              }
                          }
                          arr3 = new double[m][blk];
                          ge.dgemm('N', 'N', m, blk, m, 1.0, arr,
                                   ldwrkr, arr2, lda, 0.0,
                                   arr3, ldwrku);
                          ge.dlacpy('F', m, blk, arr3, ldwrku,
                                    arr2, lda);
                          for (j = 0; j < m; j++) {
                              for (k = 0; k < blk; k++) {
                                  A[j][i-1+k] = arr2[j][k];
                              }
                          }
                       } // for (i = 1; i <= n; i+= chunk)
                      
    
                   } // if (lwork >= m*m+Math.max(4*m, bdspac))
                   else {
    
                      // Insufficient workspace for a fast algorithm
    
                      itau = 1;
                      iwork = itau + m;
    
                      // Compute A=L*Q
                      // (Workspace: need 2*m, prefer m+m*nb)
                      workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                      gi.dgelqf(m, n, A, lda, work,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Copy L to U, zeroing out above it
    
                      ge.dlacpy('L', m, m, A, lda, U, ldu);
                      arr = new double[m-1][m-1];
                      for (i = 0; i < m-1; i++) {
                          for (j = 0; j < m-1; j++) {
                              arr[i][j] = U[i][j+1];
                          }
                      }
                      ge.dlaset('U', m-1, m-1, 0.0, 0.0, arr, ldu);
                      for (i = 0; i < m-1; i++) {
                          for (j = 0; j < m-1; j++) {
                              U[i][j+1] = arr[i][j];
                          }
                      }
    
                      // Generate Q in A
                      // (Workspace: need 2*m, prefer m+m*nb)
    
                      gi.dorglq(m, n, m, A, lda, work,
                                workiwork, lwork-iwork+1, ierr);
                      ie = itau;
                      itauq = ie + m;
                      itaup = itauq + m;
                      iwork = itaup + m;
    
                      // Bidiagonalize L in U
                      // (Workspace: need 4*m, prefer 3*m+2*m*nb)
                      workitauq = new double[m];
                      workitaup = new double[m];
                      gi.dgebrd(m, m, U, ldu, s, work,
                                workitauq, workitaup,
                                workiwork, lwork-iwork+1, ierr);
    
                      // Multiply right vectors bidiagonalizing L by Q in A
                      // (Workspace: need 3*m+n, prefer 3*m+n*nb)
    
                      gi.dormbr('P', 'L', 'T', m, n, m, U, ldu,
                                workitaup, A, lda, workiwork,
                                lwork-iwork+1, ierr);
    
                      // Generate left vectors bidiagonalizing L in U
                      // (Workspace: need 4*m, prefer 3*m+m*nb)
    
                      gi.dorgbr('Q', m, m, m, U, ldu, workitauq,
                                workiwork, lwork-iwork+1, ierr);
                      iwork = ie + m;
    
                      // Perform bidiagonal QR iteration, computing left
                      // singular vectors of A in U and computing right
                      // singular vectors of A in A
                      // (Workspace: need bdspac)
     
                      gi.dbdsqr('U', m, n, m, 0, s, work, A, lda,
                                U, ldu, dumArr, 1, workiwork, info);
    
                   } // else
    
                } //  else if (wntvo &&  wntuas)
                else if (wntvs) {
    
                   if (wntun) {
    
                      // Path 4t(n much larger than m, jobu = 'N', jobvt = 'S')
                      // m right singular vectors to be computed in VT and
                      // no left singular vectors to be computed
    
                      if (lwork >= m*m+Math.max(4*m, bdspac)) {
    
                         // Sufficient workspace for a fast algorithm
    
                         ir = 1;
                         if (lwork >= wrkbl+lda*m) {
    
                            // WORK(IR) is lda by m
    
                            ldwrkr = lda;
                         }
                         else {
    
                            // WORK(IR) is m by m
     
                            ldwrkr = m;
                         }
                         itau = ir + ldwrkr*m;
                         iwork = itau + m;
    
                         // Compute A=L*Q
                         // (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                         workitau = new double[m];
                         workiwork = new double[Math.max(1, lwork-iwork+1)];
                         gi.dgelqf(m, n, A, lda, workiwork,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy L to WORK(IR), zeroing out above it
                         arr = new double[m][m];
                         ge.dlacpy('L', m, m, A, lda, arr, ldwrkr);
                         arr2 = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr2[i][j] = arr[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0,
                                   arr2, ldwrkr);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j+1] = arr2[i][j];
                             }
                         }
    
                         // Generate Q in A
                         // (Workspace: need m*m+2*m, prefer m*m+m+m*nb)
    
                         gi.dorglq(m, n, m, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Bidiagonalize L in WORK(IR)
                         // (Workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                         workie = new double[m];
                         workitauq = new double[m];
                         workitaup = new double[m];
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgebrd(m, m, arr, ldwrkr, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate right vectors bidiagonalizing L in
                         // WORK(IR)
                         // (Workspace: need m*m+4*m, prefer m*m+3*m+(m-1)*nb)
    
                         gi.dorgbr('P', m, m, m, arr, ldwrkr,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing right
                         // singular vectors of L in WORK(IR)
                         // (Workspace: need m*m+bdspac)
    
                         gi.dbdsqr('U', m, m, 0, 0, s, workie,
                                   arr, ldwrkr, dumArr, 1, dumArr, 1,
                                   workiwork, info);
    
                         // Multiply right singular vectors of L in WORK(IR) by
                         // Q in A, storing result in VT
                         // (Workspace: need m*m)
    
                         ge.dgemm('N', 'N', m, n, m, 1.0, arr,
                                  ldwrkr, A, lda, 0.0, VT, ldvt);
    
                      } // if (lwork >= m*m+Math.max(4*m, bdspac))
                      else {
    
                         // Insufficient workspace for a fast algorithm
    
                         itau = 1;
                         iwork = itau + m;
    
                         // Compute A=L*Q
                         // (Workspace: need 2*m, prefer m+m*nb)
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy result to VT
    
                         ge.dlacpy('U', m, n, A, lda, VT, ldvt);
    
                         // Generate Q in VT
                         // (Workspace: need 2*m, prefer m+m*nb)
    
                         gi.dorglq(m, n, m, VT, ldvt, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
     
                         // Zero out above L in A
                         arr = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j] = A[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0, arr, lda);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 A[i][j+1] = arr[i][j];
                             }
                         }
    
                         // Bidiagonalize L in A
                         // (Workspace: need 4*m, prefer 3*m+2*m*nb)
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, A, lda, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Multiply right vectors bidiagonalizing L by Q in VT
                         // (Workspace: need 3*m+n, prefer 3*m+n*nb)
    
                         gi.dormbr('P', 'L', 'T', m, n, m, A, lda,
                                   workitaup, VT, ldvt,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing right
                         // singular vectors of A in VT
                         // (Workspace: need bdspac)
     
                         gi.dbdsqr('U', m, n, 0, 0, s, work, VT,
                                   ldvt, dumArr, 1, dumArr, 1, workiwork,
                                   info);
    
                      } // else
    
                   } // if (wntun)
                   else if (wntuo) {
    
                      // Path 5t(n much larger than m, jobu = 'O', jobvt = 'S')
                      // m right singular vectors to be computed in VT and
                      // m left singular vectors to be overwritten on A
    
                      if (lwork >= 2*m*m+Math.max(4*m, bdspac)) {
    
                         // Sufficient workspace for a fast algorithm
    
                         iu = 1;
                         if (lwork >= wrkbl+2*lda*m) {
    
                            // WORK(IU) is lda by m and WORK(IR) is lda by m
    
                            ldwrku = lda;
                            ir = iu + ldwrku*m;
                            ldwrkr = lda;
                         }
                         else if (lwork >= wrkbl+(lda+m)*m) {
    
                            // WORK(IU) is lda by m and WORK(IR) is m by m
    
                            ldwrku = lda;
                            ir = iu + ldwrku*m;
                            ldwrkr = m;
                         }
                         else {
    
                            // WORK(IU) is m by m and WORK(IR) is m by m
    
                            ldwrku = m;
                            ir = iu + ldwrku*m;
                            ldwrkr = m;
                         }
                         itau = ir + ldwrkr*m;
                         iwork = itau + m;
    
                         // Compute A=L*Q
                         // (Workspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                         workitau = new double[m];
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy L to WORK(IU), zeroing out below it
                         arr = new double[m][m];
                         ge.dlacpy('L', m, m, A, lda, arr, ldwrku);
                         arr2 = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr2[i][j] = arr[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0,
                                   arr2, ldwrku);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j+1] = arr2[i][j];
                             }
                         }
    
                         // Generate Q in A
                         // (Workspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
    
                         gi.dorglq(m, n, m, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Bidiagonalize L in WORK(IU), copying result to
                         // WORK(IR)
                         // (Workspace: need 2*m*m+4*m,
                                     // prefer 2*m*m+3*m+2*m*nb)
                         workie = new double[m];
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, arr, ldwrku, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         arr2 = new double[m][m];
                         ge.dlacpy('L', m, m, arr, ldwrku,
                                   arr2, ldwrkr);
    
                         // Generate right bidiagonalizing vectors in WORK(IU)
                         // (Workspace: need 2*m*m+4*m-1,
                                     // prefer 2*m*m+3*m+(m-1)*nb)
     
                         gi.dorgbr('P', m, m, m, arr, ldwrku,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate left bidiagonalizing vectors in WORK(IR)
                         // (Workspace: need 2*m*m+4*m, prefer 2*m*m+3*m+m*nb)
    
                         gi.dorgbr('Q', m, m, m, arr2, ldwrkr,
                                   workitauq, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of L in WORK(IR) and computing
                         // right singular vectors of L in WORK(IU)
                         // (Workspace: need 2*m*m+bdspac)
    
                         gi.dbdsqr('U', m, m, m, 0, s, workie,
                                   arr, ldwrku, arr2,
                                   ldwrkr, dumArr, 1, workiwork, info);
    
                         // Multiply right singular vectors of L in WORK(IU) by
                         // Q in A, storing result in VT
                         // (Workspace: need m*m)
     
                         ge.dgemm('N', 'N', m, n, m, 1.0, arr,
                                  ldwrku, A, lda, 0.0, VT, ldvt);
    
                         // Copy left singular vectors of L to A
                         // (Workspace: need m*m)
    
                         ge.dlacpy('F', m, m, arr2, ldwrkr, A, lda);
    
                      } // if (lwork >= 2*m*m+Math.max(4*m, bdspac))
                      else {
    
                         // Insufficient workspace for a fast algorithm
     
                         itau = 1;
                         iwork = itau + m;
    
                         // Compute A=L*Q, copying result to VT
                         // (Workspace: need 2*m, prefer m+m*nb)
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('U', m, n, A, lda, VT, ldvt);
    
                         // Generate Q in VT
                         // (Workspace: need 2*m, prefer m+m*nb)
    
                         gi.dorglq(m, n, m, VT, ldvt, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Zero out above L in A
                         arr = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j] = A[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0, arr, lda);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 A[i][j+1] = arr[i][j];
                             }
                         }
    
                         // Bidiagonalize L in A
                         // (Workspace: need 4*m, prefer 3*m+2*m*nb)
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, A, lda, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Multiply right vectors bidiagonalizing L by Q in VT
                         // (Workspace: need 3*m+n, prefer 3*m+n*nb)
    
                         gi.dormbr('P', 'L', 'T', m, n, m, A, lda,
                                   workitaup, VT, ldvt,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Generate left bidiagonalizing vectors of L in A
                         // (Workspace: need 4*m, prefer 3*m+m*nb)
    
                         gi.dorgbr('Q', m, m, m, A, lda, workitauq,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, compute left
                         // singular vectors of A in A and compute right
                         // singular vectors of A in VT
                         // (Workspace: need bdspac)
    
                         gi.dbdsqr('U', m, n, m, 0, s, work, VT,
                                   ldvt, A, lda, dumArr, 1, workiwork,
                                   info);
    
                      } // else
    
                   } // else if (wntuo)
                   else if (wntuas) {
    
                      // Path 6t(n much larger than m, jobu = 'S' or 'A',
                              // jobvt = 'S')
                      // m right singular vectors to be computed in VT and
                      // m left singular vectors to be computed in U
    
                      if (lwork >= m*m+Math.max(4*m, bdspac)) {
    
                         // Sufficient workspace for a fast algorithm
     
                         iu = 1;
                         if (lwork >= wrkbl+lda*m) {
    
                            // WORK(IU) is lda by n
    
                            ldwrku = lda;
                         }
                         else {
    
                            // WORK(IU) is lda by m
    
                            ldwrku = m;
                         }
                         itau = iu + ldwrku*m;
                         iwork = itau + m;
    
                         // Compute A=L*Q
                         // (Workspace: need m*m+2*m, prefer m*m+m+m*nb)
                         workitau = new double[m];
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy L to WORK(IU), zeroing out above it
                         arr = new double[m][m];
                         ge.dlacpy('L', m, m, A, lda, arr, ldwrku);
                         arr2 = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr2[i][j] = arr[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0,
                                     arr2, ldwrku);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j+1] = arr2[i][j];
                             }
                         }
    
                         // Generate Q in A
                         // (Workspace: need m*m+2*m, prefer m*m+m+m*nb)
    
                         gi.dorglq(m, n, m, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Bidiagonalize L in WORK(IU), copying result to U
                         // (Workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                         workie = new double[m];
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, arr, ldwrku, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, m, arr, ldwrku, U, ldu);
    
                         // Generate right bidiagonalizing vectors in WORK(IU)
                         // (Workspace: need m*m+4*m-1,
                                     // prefer m*m+3*m+(m-1)*nb)
    
                         gi.dorgbr('P', m, m, m, arr, ldwrku,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate left bidiagonalizing vectors in U
                         // (Workspace: need m*m+4*m, prefer m*m+3*m+m*nb)
    
                         gi.dorgbr('Q', m, m, m, U, ldu, workitauq,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of L in U and computing right
                         // singular vectors of L in WORK(IU)
                         // (Workspace: need m*m+bdspac)
    
                         gi.dbdsqr('U', m, m, m, 0, s, workie,
                                   arr, ldwrku, U, ldu, dumArr, 1,
                                   workiwork, info);
    
                         // Multiply right singular vectors of L in WORK(IU) by
                         // Q in A, storing result in VT
                         // (Workspace: need m*m)
    
                         ge.dgemm('N', 'N', m, n, m, 1.0, arr,
                                  ldwrku, A, lda, 0.0, VT, ldvt);
    
                      } // if (lwork >= m*m+Math.max(4*m, bdspac))
                      else {
    
                         // Insufficient workspace for a fast algorithm
     
                         itau = 1;
                         iwork = itau + m;
    
                         // Compute A=L*Q, copying result to VT
                         // (Workspace: need 2*m, prefer m+m*nb)
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('U', m, n, A, lda, VT, ldvt);
    
                         // Generate Q in VT
                         // (Workspace: need 2*m, prefer m+m*nb)
    
                         gi.dorglq(m, n, m, VT, ldvt, work,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy L to U, zeroing out above it
    
                         ge.dlacpy('L', m, m, A, lda, U, ldu);
                         arr = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j] = U[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0, arr, ldu);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 U[i][j+1] = arr[i][j];
                             }
                         }
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Bidiagonalize L in U
                         // (Workspace: need 4*m, prefer 3*m+2*m*nb)
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, U, ldu, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Multiply right bidiagonalizing vectors in U by Q
                         // in VT
                         // (Workspace: need 3*m+n, prefer 3*m+n*nb)
    
                         gi.dormbr('P', 'L', 'T', m, n, m, U, ldu,
                                   workitaup, VT, ldvt,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Generate left bidiagonalizing vectors in U
                         // (Workspace: need 4*m, prefer 3*m+m*nb)
    
                         gi.dorgbr('Q', m, m, m, U, ldu, workitauq,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of A in U and computing right
                         // singular vectors of A in VT
                         // (Workspace: need bdspac)
    
                         gi.dbdsqr('U', m, n, m, 0, s, work, VT,
                                   ldvt, U, ldu, dumArr, 1, workiwork,
                                   info);
    
                      } // else
    
                   } // else if (wntuas)
    
                } // else if (wntvs)
                else if (wntva) {
    
                   if (wntun) {
    
                      // Path 7t(n much larger than m, jobu = 'N', jobvt = 'A')
                      // n right singular vectors to be computed in VT and 
                      // no left singular vectors to be computed
    
                      if (lwork >= m*m+Math.max(n+m, Math.max(4*m, bdspac))) {
    
                         // Sufficient workspace for a fast algorithm
     
                         ir = 1;
                         if (lwork >= wrkbl+lda*m) {
    
                            // WORK(IR) is lda by ,
    
                            ldwrkr = lda;
                         }
                         else {
    
                            // WORK(IR) is m by m
    
                            ldwrkr = m;
                         }
                         itau = ir + ldwrkr*m;
                         iwork = itau + m;
    
                         // Compute A=L*Q, copying result to VT
                         // (Workspace: need m*m+2*m, prefer m*m+m+m*nb)
                         workitau = new double[m];
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('U', m, n, A, lda, VT, ldvt);
    
                         // Copy L to WORK(IR), zeroing out above it
                         arr = new double[m][m];
                         ge.dlacpy('L', m, m, A, lda, arr, ldwrkr);
                         arr2 = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr2[i][j] = arr[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0,
                                   arr2, ldwrkr);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j+1] = arr2[i][j];
                             }
                         }
    
                         // Generate Q in VT
                         // (Workspace: need m*m+m+n, prefer m*m+m+n*nb)
     
                         gi.dorglq(n, n, m, VT, ldvt, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Bidiagonalize L in WORK(IR)
                         // (Workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                         workie = new double[m];
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, arr, ldwrkr, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
     
                         // Generate right bidiagonalizing vectors in WORK(IR)
                         // (Workspace: need m*m+4*m-1,
                                     // prefer m*m+3*m+(m-1)*nb)
    
                         gi.dorgbr('P', m, m, m, arr, ldwrkr,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing right
                         // singular vectors of L in WORK(IR)
                         // (Workspace: need m*m+bdspac)
    
                         gi.dbdsqr('U', m, m, 0, 0, s, workie,
                                   arr, ldwrkr, dumArr, 1, dumArr, 1,
                                   workiwork, info);
    
                         // Multiply right singular vectors of L in WORK(IR) by
                         // Q in VT, storing result in A
                         // (Workspace: need m*m)
    
                         ge.dgemm('N', 'N', m, n, m, 1.0, arr,
                                  ldwrkr, VT, ldvt, 0.0, A, lda);
    
                         // Copy right singular vectors of A from A to VT
    
                         ge.dlacpy('F', m, n, A, lda, VT, ldvt);
    
                      } // if (lwork >= m*m+Math.max(n+m, Math.max(4*m, bdspac))
                      else {
    
                         // Insufficient workspace for a fast algorithm
    
                         itau = 1;
                         iwork = itau + m;
    
                         // Compute A=L*Q, copying result to VT
                         // (Workspace: need 2*m, prefer m+m*nb)
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('U', m, n, A, lda, VT, ldvt);
    
                         // Generate Q in VT
                         // (Workspace: need m+n, prefer m+n*nb)
    
                         gi.dorglq(n, n, m, VT, ldvt, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Zero out above L in A
                         arr = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j] = A[i][j+1];
                             }
                         }
                         ge.dlaset( 'U', m-1, m-1, 0.0, 0.0, arr, lda);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 A[i][j+1] = arr[i][j];
                             }
                         }
    
                         // Bidiagonalize L in A
                         // (Workspace: need 4*m, prefer 3*m+2*m*nb)
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, A, lda, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         //Multiply right bidiagonalizing vectors in A by Q
                         // in VT
                         // (Workspace: need 3*m+n, prefer 3*m+n*nb)
    
                         gi.dormbr('P', 'L', 'T', m, n, m, A, lda,
                                   workitaup, VT, ldvt,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing right
                         // singular vectors of A in VT
                         // (Workspace: need bdspac)
     
                         gi.dbdsqr('U', m, n, 0, 0, s, work, VT,
                                   ldvt, dumArr, 1, dumArr, 1, workiwork,
                                   info);
    
                      } // else
    
                   } // if (wntun)
                   else if (wntuo) {
    
                      // Path 8t(n much larger than m, JOBU = 'O', JOBVT = 'A')
                      // n right singular vectors to be computed in VT and
                      // m left singular vectors to be overwritten on A
     
                      if (lwork >= 2*m*m+Math.max(n+m, Math.max(4*m, bdspac))) {
    
                         // Sufficient workspace for a fast algorithm
    
                         iu = 1;
                         if (lwork >= wrkbl+2*lda*m) {
    
                            // WORK(IU) is lda by m and WORK(IR) is lda by m
     
                            ldwrku = lda;
                            ir = iu + ldwrku*m;
                            ldwrkr = lda;
                         }
                         else if (lwork >= wrkbl+(lda+m)*m) {
    
                            // WORK(IU) is lda by m and WORK(IR) is m by m
    
                            ldwrku = lda;
                            ir = iu + ldwrku*m;
                            ldwrkr = m;
                         }
                         else {
    
                            // WORK(IU) is m by m and WORK(IR) is m by m
    
                            ldwrku = m;
                            ir = iu + ldwrku*m;
                            ldwrkr = m;
                         }
                         itau = ir + ldwrkr*m;
                         iwork = itau + m;
    
                         // Compute A=L*Q, copying result to VT
                         // (Workspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                         workitau = new double[m];
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('U', m, n, A, lda, VT, ldvt);
    
                         // Generate Q in VT
                         // (Workspace: need 2*m*m+m+n, prefer 2*m*m+m+n*nb)
    
                         gi.dorglq(n, n, m, VT, ldvt, workitau,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy L to WORK(IU), zeroing out above it
                         arr = new double[m][m];
                         ge.dlacpy('L', m, m, A, lda, arr, ldwrku);
                         arr2 = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr2[i][j] = arr[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0,
                                   arr2, ldwrku);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j+1] = arr2[i][j];
                             }
                         }
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Bidiagonalize L in WORK(IU), copying result to
                         // WORK(IR)
                         // (Workspace: need 2*m*m+4*m,
                                     // prefer 2*m*m+3*m+2*m*nb)
    
                         workie = new double[m];
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, arr, ldwrku, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         arr2 = new double[m][m];
                         ge.dlacpy('L', m, m, arr, ldwrku,
                                   arr2, ldwrkr);
    
                         // Generate right bidiagonalizing vectors in WORK(IU)
                         // (Workspace: need 2*m*m+4*m-1,
                                     // prefer 2*m*m+3*m+(m-1)*nb)
    
                         gi.dorgbr('P', m, m, m, arr, ldwrku,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate left bidiagonalizing vectors in WORK(IR)
                         // (Workspace: need 2*m*m+4*m, prefer 2*m*m+3*m+m*nb)
    
                         gi.dorgbr('Q', m, m, m, arr2, ldwrkr,
                                   workitauq, workiwork,
                                   lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of L in WORK(IR) and computing
                         // right singular vectors of L in WORK(IU)
                         // (Workspace: need 2*m*m+bdspac)
    
                         gi.dbdsqr('U', m, m, m, 0, s, workie,
                                   arr, ldwrku, arr2,
                                   ldwrkr, dumArr, 1, workiwork, info);
    
                         // Multiply right singular vectors of L in WORK(IU) by
                         // Q in VT, storing result in A
                         // (Workspace: need m*m)
    
                         ge.dgemm('N', 'N', m, n, m, 1.0, arr,
                                  ldwrku, VT, ldvt, 0.0, A, lda);
    
                         // Copy right singular vectors of A from A to VT
    
                         ge.dlacpy('F', m, n, A, lda, VT, ldvt);
    
                         // Copy left singular vectors of A from WORK(IR) to A
     
                         ge.dlacpy('F', m, m, arr2, ldwrkr, A, lda);
    
                      } // if (lwork >= 2*m*m+Math.max(n+m, Math.max(4*m, bdspac)))
                      else {
    
                         // Insufficient workspace for a fast algorithm
    
                         itau = 1;
                         iwork = itau + m;
    
                         // Compute A=L*Q, copying result to VT
                         // (Workspace: need 2*m, prefer m+m*nb)
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('U', m, n, A, lda, VT, ldvt);
    
                         // Generate Q in VT
                         // (Workspace: need m+n, prefer m+n*nb)
    
                         gi.dorglq(n, n, m, VT, ldvt, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Zero out above L in A
                         arr = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j] = A[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0, arr, lda);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 A[i][j+1] = arr[i][j];
                             }
                         }
    
                         // Bidiagonalize L in A
                         // (Workspace: need 4*m, prefer 3*m+2*m*nb)
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, A, lda, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Multiply right bidiagonalizing vectors in A by Q
                         // in VT
                         // (Workspace: need 3*m+n, prefer 3*m+n*nb)
    
                         gi.dormbr('P', 'L', 'T', m, n, m, A, lda,
                                   workitaup, VT, ldvt,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Generate left bidiagonalizing vectors in A
                         // (Workspace: need 4*m, prefer 3*m+m*nb)
    
                         gi.dorgbr('Q', m, m, m, A, lda, workitauq,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of A in A and computing right
                         // singular vectors of A in VT
                         // (Workspace: need bdspac)
     
                         gi.dbdsqr('U', m, n, m, 0, s, work, VT,
                                   ldvt, A, lda, dumArr, 1, workiwork,
                                   info);
    
                      } // else
    
                   } // else if (wntuo)
                   else if (wntuas) {
     
                      // Path 9t(n much larger than m, jobu = 'S' or 'A',
                              // jobvt = 'A')
                      // n right singular vectors to be computed in VT and
                      // m left singular vectors to be computed in U
     
                      if (lwork >= m*m+Math.max(n+m, Math.max(4*m, bdspac))) {
    
                         // Sufficient workspace for a fast algorithm
    
                         iu = 1;
                         if (lwork >= wrkbl+lda*m) {
    
                            // WORK(IU) is lda by m
    
                            ldwrku = lda;
                         }
                         else {
    
                            // WORK(IU) is m by m
    
                            ldwrku = m;
                         }
                         itau = iu + ldwrku*m;
                         iwork = itau + m;
    
                         // Compute A=L*Q, copying result to VT
                         // (Workspace: need m*m+2*m, prefer m*m+m+m*nb)
                         workitau = new double[m];
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, workitau,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('U', m, n, A, lda, VT, ldvt);
    
                         // Generate Q in VT
                         // (Workspace: need m*m+m+n, prefer m*m+m+n*nb)
    
                         gi.dorglq(n, n, m, VT, ldvt, workitau,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy L to WORK(IU), zeroing out above it
                         arr = new double[m][m];
                         ge.dlacpy('L', m, m, A, lda, arr, ldwrku);
                         arr2 = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr2[i][j] = arr[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0,
                                   arr2, ldwrku);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j+1] = arr2[i][j];
                             }
                         }
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Bidiagonalize L in WORK(IU), copying result to U
                         // (Workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                         workie = new double[m];
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, arr, ldwrku, s,
                                   workie, workitauq,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
                         ge.dlacpy('L', m, m, arr, ldwrku, U, ldu);
    
                         // Generate right bidiagonalizing vectors in WORK(IU)
                         // (Workspace: need m*m+4*m, prefer m*m+3*m+(m-1)*nb)
    
                         gi.dorgbr('P', m, m, m, arr, ldwrku,
                                   workitaup, workiwork,
                                   lwork-iwork+1, ierr);
    
                         // Generate left bidiagonalizing vectors in U
                         // (Workspace: need m*m+4*m, prefer m*m+3*m+m*nb)
    
                         gi.dorgbr('Q', m, m, m, U, ldu, workitauq,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of L in U and computing right
                         // singular vectors of L in WORK(IU)
                         // (Workspace: need m*m+bdspac)
    
                         gi.dbdsqr('U', m, m, m, 0, s, workie,
                                   arr, ldwrku, U, ldu, dumArr, 1,
                                   workiwork, info);
    
                         // Multiply right singular vectors of L in WORK(IU) by
                         // Q in VT, storing result in A
                         // (Workspace: need M*M)
    
                         ge.dgemm('N', 'N', m, n, m, 1.0, arr,
                                  ldwrku, VT, ldvt, 0.0, A, lda);
    
                         // Copy right singular vectors of A from A to VT
    
                         ge.dlacpy('F', m, n, A, lda, VT, ldvt);
    
                      } // if (lwork >= m*m+Math.max(n+m, Math.max(4*m, bdspac)))
                      else {
     
                         // Insufficient workspace for a fast algorithm
    
                         itau = 1;
                         iwork = itau + m;
    
                         // Compute A=L*Q, copying result to VT
                         // (Workspace: need 2*m, prefer m+m*nb)
                         workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                         gi.dgelqf(m, n, A, lda, work,
                                   workiwork, lwork-iwork+1, ierr);
                         ge.dlacpy('U', m, n, A, lda, VT, ldvt);
    
                         // Generate Q in VT
                         // (Workspace: need m+n, prefer m+n*nb)
    
                         gi.dorglq(n, n, m, VT, ldvt, work,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Copy L to U, zeroing out above it
     
                         ge.dlacpy('L', m, m, A, lda, U, ldu);
                         arr = new double[m-1][m-1];
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 arr[i][j] = U[i][j+1];
                             }
                         }
                         ge.dlaset('U', m-1, m-1, 0.0, 0.0, arr, ldu);
                         for (i = 0; i < m-1; i++) {
                             for (j = 0; j < m-1; j++) {
                                 U[i][j+1] = arr[i][j];
                             }
                         }
                         ie = itau;
                         itauq = ie + m;
                         itaup = itauq + m;
                         iwork = itaup + m;
    
                         // Bidiagonalize L in U
                         // (Workspace: need 4*m, prefer 3*m+2*m*nb)
                         workitauq = new double[m];
                         workitaup = new double[m];
                         gi.dgebrd(m, m, U, ldu, s, work,
                                   workitauq, workitaup,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Multiply right bidiagonalizing vectors in U by Q
                         // in VT
                         // (Workspace: need 3*m+n, prefer 3*m+n*nb)
    
                         gi.dormbr('P', 'L', 'T', m, n, m, U, ldu,
                                   workitaup, VT, ldvt,
                                   workiwork, lwork-iwork+1, ierr);
    
                         // Generate left bidiagonalizing vectors in U
                         // (Workspace: need 4*m, prefer 3*m+m*nb)
    
                         gi.dorgbr('Q', m, m, m, U, ldu, workitauq,
                                   workiwork, lwork-iwork+1, ierr);
                         iwork = ie + m;
    
                         // Perform bidiagonal QR iteration, computing left
                         // singular vectors of A in U and computing right
                         // singular vectors of A in VT
                         // (Workspace: need bdspac)
    
                         gi.dbdsqr('U', m, n, m, 0, s, work, VT,
                                   ldvt, U, ldu, dumArr, 1, workiwork,
                                   info);
    
                      } // else
    
                   } // else if (wntuas)
    
                } // else if (wntva)
     
             } // if (n >= mnthr)
             else { // n < mnthr
    
                // N .LT. MNTHR
    
                // Path 10t(n greater than m, but not much larger)
                // Reduce to bidiagonal form without LQ decomposition
    
                ie = 1;
                itauq = ie + m;
                itaup = itauq + m;
                iwork = itaup + m;
    
                // Bidiagonalize A
                // (Workspace: need 3*m+n, prefer 3*m+(m+n)*nb)
                workitauq = new double[m];
                workitaup = new double[m];
                workiwork = new double[Math.max(4*m, Math.max(1, lwork-iwork+1))];
                gi.dgebrd(m, n, A, lda, s, work, workitauq,
                          workitaup, workiwork, lwork-iwork+1,
                          ierr);
                if (wntuas) {
     
                   // If left singular vectors desired in U, copy result to U
                   // and generate left bidiagonalizing vectors in U
                   // (Workspace: need 4*m-1, prefer 3*m+(m-1)*nb)
    
                   ge.dlacpy('L', m, m, A, lda, U, ldu);
                   gi.dorgbr('Q', m, m, n, U, ldu, workitauq,
                             workiwork, lwork-iwork+1, ierr);
                } // if (wntuas)
                if (wntvas) {
    
                   // If right singular vectors desired in VT, copy result to
                   // VT and generate right bidiagonalizing vectors in VT
                   // (Workspace: need 3*m+nrvt, prefer 3*m+nrvt*nb)
    
                   ge.dlacpy('U', m, n, A, lda, VT, ldvt);
                   if (wntva) {
                      nrvt = n;
                   }
                   if (wntvs) {
                      nrvt = m;
                   }
                   gi.dorgbr('P', nrvt, n, m, VT, ldvt, workitaup,
                             workiwork, lwork-iwork+1, ierr);
                } // if (wntvas)
                if (wntuo) {
    
                   // If left singular vectors desired in A, generate left
                   // bidiagonalizing vectors in A
                   // (Workspace: need 4*m-1, prefer 3*m+(m-1)*nb)
    
                   gi.dorgbr('Q', m, m, n, A, lda, workitauq,
                             workiwork, lwork-iwork+1, ierr);
                } // if (wntuo)
                if (wntvo) {
    
                   // If right singular vectors desired in A, generate right
                   // bidiagonalizing vectors in A
                   // (Workspace: need 4*m, prefer 3*m+m*nb)
    
                   gi.dorgbr('P', m, n, m, A, lda, workitaup,
                             workiwork, lwork-iwork+1, ierr);
                } // if (wntvo)
                iwork = ie + m;
                if (wntuas || wntuo) {
                   nru = m;
                }
                if (wntun) {
                   nru = 0;
                }
                if (wntvas || wntvo) {
                   ncvt = n;
                }
                if (wntvn) {
                   ncvt = 0;
                }
                if ((!wntuo) && (!wntvo)) {
     
                   // Perform bidiagonal QR iteration, if desired, computing
                   // left singular vectors in U and computing right singular
                   // vectors in VT
                   // (Workspace: need bdspac)
    
                   gi.dbdsqr('L', m, ncvt, nru, 0, s, work, VT,
                             ldvt, U, ldu, dumArr, 1, workiwork, info);
                }
                else if ((!wntuo) && wntvo) {
     
                   // Perform bidiagonal QR iteration, if desired, computing
                   // left singular vectors in U and computing right singular
                   // vectors in A
                   // (Workspace: need bdspac)
    
                   gi.dbdsqr('L', m, ncvt, nru, 0, s, work, A, lda,
                             U, ldu, dumArr, 1, workiwork, info);
                }
                else {
     
                   // Perform bidiagonal QR iteration, if desired, computing
                   // left singular vectors in A and computing right singular
                   // vectors in VT
                   // (Workspace: need bdspac)
    
                   gi.dbdsqr('L', m, ncvt, nru, 0, s, work, VT,
                             ldvt, A, lda, dumArr, 1, workiwork, info);
                }
    
             } // else n < mnthr
    
          } // else m < n
    
          // If dbdsqr failed to converge, copy unconverged superdiagonals
          // to work[1:minmn-1]
    
          if (info[0] != 0) {
             if (ie > 2) {
                for (i = 0; i < minmn - 1; i++) {
                   work[i+1] = work[i+ie-1];
                }
             } // if (ie > 2)
             if (ie < 2) {
                for (i = minmn - 2; i >=  0; i--) {
                   work[i+1] = work[i+ie-1];
                }
             } // if (ie < 2)
          } // if (info[0] != 0)
    
          // Undo scaling if necessary
    
          /*if (iscl == 1) {
             if (anrm > bignum) {
                ge.dlascl( 'G', 0, 0, bignum, anrm, minmn, 1, s, minmn, ierr);
             }
             ( INFO.NE.0 .AND. ANRM.GT.BIGNUM )
         $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN-1, 1, WORK( 2 ),
         $                   MINMN, IERR )
             IF( ANRM.LT.SMLNUM )
         $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
         $                   IERR )
             IF( INFO.NE.0 .AND. ANRM.LT.SMLNUM )
         $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN-1, 1, WORK( 2 ),
         $                   MINMN, IERR )
          } // if (iscl == 1)
    
         // Return optimal workspace in WORK(1)
   
          work[0] = maxwrk;*/
    
          return;
      } // dgesvd

}