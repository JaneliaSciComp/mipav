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
          int bdspac;
          int blk;
          int chunk;
          int i;
          int j;
          int k;
          int L;
          int p;
          int ie;
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
          int mnthr;
          int ncu;
          int ncvt;
          int nru;
          int nrvt;
          int wrkbl;
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
          double arr[][];
          double arr2[][];
         
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
    
          /*if (m >= n) {
    
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
                              dum, 1, dum, 1, workiwork, info);
    
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
                              arr2[i][j] = arr[i][j+1];
                          }
                      }
                      ge.dlaset( 'L', n-1, n-1, 0.0, 0.0, arr2, ldwrkr);
                      for (i = 0; i < n-1; i++) {
                          for (j = 0; j < n-1; j++) {
                              arr[i][j+1] = arr2[i][j];
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
                      work[iwork] = new double[Math.max(1, lwork-iwork+1)];
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
                      workiwork = new double[Math.max(1, lwork-iwork+1)];
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
    
                      CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
         $                         WORK( ITAUQ ), A, LDA, WORK( IWORK ),
         $                         LWORK-IWORK+1, IERR )
    *
    *                 Generate right vectors bidiagonalizing R in VT
    *                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    *
                      CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                      IWORK = IE + N
    *
    *                 Perform bidiagonal QR iteration, computing left
    *                 singular vectors of A in A and computing right
    *                 singular vectors of A in VT
    *                 (Workspace: need BDSPAC)
    *
                      CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT, LDVT,
         $                         A, LDA, DUM, 1, WORK( IWORK ), INFO )
    *
                   } // else
    *
                } // else if (wntuo && wntvas)
                ELSE IF( WNTUS ) THEN
    *
                   IF( WNTVN ) THEN
    *
    *                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
    *                 N left singular vectors to be computed in U and
    *                 no right singular vectors to be computed
    *
                      IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IR = 1
                         IF( LWORK.GE.WRKBL+LDA*N ) THEN
    *
    *                       WORK(IR) is LDA by N
    *
                            LDWRKR = LDA
                         ELSE
    *
    *                       WORK(IR) is N by N
    *
                            LDWRKR = N
                         END IF
                         ITAU = IR + LDWRKR*N
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R
    *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy R to WORK(IR), zeroing out below it
    *
                         CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ),
         $                            LDWRKR )
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
         $                            WORK( IR+1 ), LDWRKR )
    *
    *                    Generate Q in A
    *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    *
                         CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Bidiagonalize R in WORK(IR)
    *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate left vectors bidiagonalizing R in WORK(IR)
    *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    *
                         CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
         $                            WORK( ITAUQ ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of R in WORK(IR)
    *                    (Workspace: need N*N+BDSPAC)
    *
                         CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
         $                            1, WORK( IR ), LDWRKR, DUM, 1,
         $                            WORK( IWORK ), INFO )
    *
    *                    Multiply Q in A by left singular vectors of R in
    *                    WORK(IR), storing result in U
    *                    (Workspace: need N*N)
    *
                         CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
         $                           WORK( IR ), LDWRKR, ZERO, U, LDU )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R, copying result to U
    *                    (Workspace: need 2*N, prefer N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
    *
    *                    Generate Q in U
    *                    (Workspace: need 2*N, prefer N+N*NB)
    *
                         CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Zero out below R in A
    *
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
         $                            LDA )
    *
    *                    Bidiagonalize R in A
    *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply Q in U by left vectors bidiagonalizing R
    *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    *
                         CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
         $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of A in U
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
         $                            1, U, LDU, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   ELSE IF( WNTVO ) THEN
    *
    *                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
    *                 N left singular vectors to be computed in U and
    *                 N right singular vectors to be overwritten on A
    *
                      IF( LWORK.GE.2*N*N+MAX( 4*N, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IU = 1
                         IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
    *
    *                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
    *
                            LDWRKU = LDA
                            IR = IU + LDWRKU*N
                            LDWRKR = LDA
                         ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
    *
    *                       WORK(IU) is LDA by N and WORK(IR) is N by N
    *
                            LDWRKU = LDA
                            IR = IU + LDWRKU*N
                            LDWRKR = N
                         ELSE
    *
    *                       WORK(IU) is N by N and WORK(IR) is N by N
    *
                            LDWRKU = N
                            IR = IU + LDWRKU*N
                            LDWRKR = N
                         END IF
                         ITAU = IR + LDWRKR*N
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R
    *                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy R to WORK(IU), zeroing out below it
    *
                         CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
         $                            LDWRKU )
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
         $                            WORK( IU+1 ), LDWRKU )
    *
    *                    Generate Q in A
    *                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
    *
                         CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Bidiagonalize R in WORK(IU), copying result to
    *                    WORK(IR)
    *                    (Workspace: need 2*N*N+4*N,
    *                                prefer 2*N*N+3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU,
         $                            WORK( IR ), LDWRKR )
    *
    *                    Generate left bidiagonalizing vectors in WORK(IU)
    *                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
    *
                         CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
         $                            WORK( ITAUQ ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right bidiagonalizing vectors in WORK(IR)
    *                    (Workspace: need 2*N*N+4*N-1,
    *                                prefer 2*N*N+3*N+(N-1)*NB)
    *
                         CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of R in WORK(IU) and computing
    *                    right singular vectors of R in WORK(IR)
    *                    (Workspace: need 2*N*N+BDSPAC)
    *
                         CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
         $                            WORK( IR ), LDWRKR, WORK( IU ),
         $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
    *
    *                    Multiply Q in A by left singular vectors of R in
    *                    WORK(IU), storing result in U
    *                    (Workspace: need N*N)
    *
                         CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
         $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
    *
    *                    Copy right singular vectors of R to A
    *                    (Workspace: need N*N)
    *
                         CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
         $                            LDA )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R, copying result to U
    *                    (Workspace: need 2*N, prefer N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
    *
    *                    Generate Q in U
    *                    (Workspace: need 2*N, prefer N+N*NB)
    *
                         CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Zero out below R in A
    *
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
         $                            LDA )
    *
    *                    Bidiagonalize R in A
    *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply Q in U by left vectors bidiagonalizing R
    *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    *
                         CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
         $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right vectors bidiagonalizing R in A
    *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    *
                         CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of A in U and computing right
    *                    singular vectors of A in A
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
         $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   ELSE IF( WNTVAS ) THEN
    *
    *                 Path 6 (M much larger than N, JOBU='S', JOBVT='S'
    *                         or 'A')
    *                 N left singular vectors to be computed in U and
    *                 N right singular vectors to be computed in VT
    *
                      IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IU = 1
                         IF( LWORK.GE.WRKBL+LDA*N ) THEN
    *
    *                       WORK(IU) is LDA by N
    *
                            LDWRKU = LDA
                         ELSE
    *
    *                       WORK(IU) is N by N
    *
                            LDWRKU = N
                         END IF
                         ITAU = IU + LDWRKU*N
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R
    *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy R to WORK(IU), zeroing out below it
    *
                         CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
         $                            LDWRKU )
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
         $                            WORK( IU+1 ), LDWRKU )
    *
    *                    Generate Q in A
    *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    *
                         CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Bidiagonalize R in WORK(IU), copying result to VT
    *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
         $                            LDVT )
    *
    *                    Generate left bidiagonalizing vectors in WORK(IU)
    *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    *
                         CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
         $                            WORK( ITAUQ ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right bidiagonalizing vectors in VT
    *                    (Workspace: need N*N+4*N-1,
    *                                prefer N*N+3*N+(N-1)*NB)
    *
                         CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of R in WORK(IU) and computing
    *                    right singular vectors of R in VT
    *                    (Workspace: need N*N+BDSPAC)
    *
                         CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
         $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
         $                            WORK( IWORK ), INFO )
    *
    *                    Multiply Q in A by left singular vectors of R in
    *                    WORK(IU), storing result in U
    *                    (Workspace: need N*N)
    *
                         CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
         $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R, copying result to U
    *                    (Workspace: need 2*N, prefer N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
    *
    *                    Generate Q in U
    *                    (Workspace: need 2*N, prefer N+N*NB)
    *
                         CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy R to VT, zeroing out below it
    *
                         CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                         IF( N.GT.1 )
         $                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
         $                               VT( 2, 1 ), LDVT )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Bidiagonalize R in VT
    *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply Q in U by left bidiagonalizing vectors
    *                    in VT
    *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    *
                         CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
         $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right bidiagonalizing vectors in VT
    *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    *
                         CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of A in U and computing right
    *                    singular vectors of A in VT
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
         $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   END IF
    *
                ELSE IF( WNTUA ) THEN
    *
                   IF( WNTVN ) THEN
    *
    *                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
    *                 M left singular vectors to be computed in U and
    *                 no right singular vectors to be computed
    *
                      IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IR = 1
                         IF( LWORK.GE.WRKBL+LDA*N ) THEN
    *
    *                       WORK(IR) is LDA by N
    *
                            LDWRKR = LDA
                         ELSE
    *
    *                       WORK(IR) is N by N
    *
                            LDWRKR = N
                         END IF
                         ITAU = IR + LDWRKR*N
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R, copying result to U
    *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
    *
    *                    Copy R to WORK(IR), zeroing out below it
    *
                         CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ),
         $                            LDWRKR )
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
         $                            WORK( IR+1 ), LDWRKR )
    *
    *                    Generate Q in U
    *                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
    *
                         CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Bidiagonalize R in WORK(IR)
    *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate left bidiagonalizing vectors in WORK(IR)
    *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    *
                         CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
         $                            WORK( ITAUQ ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of R in WORK(IR)
    *                    (Workspace: need N*N+BDSPAC)
    *
                         CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
         $                            1, WORK( IR ), LDWRKR, DUM, 1,
         $                            WORK( IWORK ), INFO )
    *
    *                    Multiply Q in U by left singular vectors of R in
    *                    WORK(IR), storing result in A
    *                    (Workspace: need N*N)
    *
                         CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
         $                           WORK( IR ), LDWRKR, ZERO, A, LDA )
    *
    *                    Copy left singular vectors of A from A to U
    *
                         CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R, copying result to U
    *                    (Workspace: need 2*N, prefer N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
    *
    *                    Generate Q in U
    *                    (Workspace: need N+M, prefer N+M*NB)
    *
                         CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Zero out below R in A
    *
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
         $                            LDA )
    *
    *                    Bidiagonalize R in A
    *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply Q in U by left bidiagonalizing vectors
    *                    in A
    *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    *
                         CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
         $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of A in U
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
         $                            1, U, LDU, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   ELSE IF( WNTVO ) THEN
    *
    *                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
    *                 M left singular vectors to be computed in U and
    *                 N right singular vectors to be overwritten on A
    *
                      IF( LWORK.GE.2*N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IU = 1
                         IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
    *
    *                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
    *
                            LDWRKU = LDA
                            IR = IU + LDWRKU*N
                            LDWRKR = LDA
                         ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
    *
    *                       WORK(IU) is LDA by N and WORK(IR) is N by N
    *
                            LDWRKU = LDA
                            IR = IU + LDWRKU*N
                            LDWRKR = N
                         ELSE
    *
    *                       WORK(IU) is N by N and WORK(IR) is N by N
    *
                            LDWRKU = N
                            IR = IU + LDWRKU*N
                            LDWRKR = N
                         END IF
                         ITAU = IR + LDWRKR*N
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R, copying result to U
    *                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
    *
    *                    Generate Q in U
    *                    (Workspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
    *
                         CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy R to WORK(IU), zeroing out below it
    *
                         CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
         $                            LDWRKU )
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
         $                            WORK( IU+1 ), LDWRKU )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Bidiagonalize R in WORK(IU), copying result to
    *                    WORK(IR)
    *                    (Workspace: need 2*N*N+4*N,
    *                                prefer 2*N*N+3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU,
         $                            WORK( IR ), LDWRKR )
    *
    *                    Generate left bidiagonalizing vectors in WORK(IU)
    *                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
    *
                         CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
         $                            WORK( ITAUQ ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right bidiagonalizing vectors in WORK(IR)
    *                    (Workspace: need 2*N*N+4*N-1,
    *                                prefer 2*N*N+3*N+(N-1)*NB)
    *
                         CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of R in WORK(IU) and computing
    *                    right singular vectors of R in WORK(IR)
    *                    (Workspace: need 2*N*N+BDSPAC)
    *
                         CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
         $                            WORK( IR ), LDWRKR, WORK( IU ),
         $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
    *
    *                    Multiply Q in U by left singular vectors of R in
    *                    WORK(IU), storing result in A
    *                    (Workspace: need N*N)
    *
                         CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
         $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
    *
    *                    Copy left singular vectors of A from A to U
    *
                         CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
    *
    *                    Copy right singular vectors of R from WORK(IR) to A
    *
                         CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
         $                            LDA )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R, copying result to U
    *                    (Workspace: need 2*N, prefer N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
    *
    *                    Generate Q in U
    *                    (Workspace: need N+M, prefer N+M*NB)
    *
                         CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Zero out below R in A
    *
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
         $                            LDA )
    *
    *                    Bidiagonalize R in A
    *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply Q in U by left bidiagonalizing vectors
    *                    in A
    *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    *
                         CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
         $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right bidiagonalizing vectors in A
    *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    *
                         CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of A in U and computing right
    *                    singular vectors of A in A
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
         $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   ELSE IF( WNTVAS ) THEN
    *
    *                 Path 9 (M much larger than N, JOBU='A', JOBVT='S'
    *                         or 'A')
    *                 M left singular vectors to be computed in U and
    *                 N right singular vectors to be computed in VT
    *
                      IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IU = 1
                         IF( LWORK.GE.WRKBL+LDA*N ) THEN
    *
    *                       WORK(IU) is LDA by N
    *
                            LDWRKU = LDA
                         ELSE
    *
    *                       WORK(IU) is N by N
    *
                            LDWRKU = N
                         END IF
                         ITAU = IU + LDWRKU*N
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R, copying result to U
    *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
    *
    *                    Generate Q in U
    *                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
    *
                         CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy R to WORK(IU), zeroing out below it
    *
                         CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
         $                            LDWRKU )
                         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
         $                            WORK( IU+1 ), LDWRKU )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Bidiagonalize R in WORK(IU), copying result to VT
    *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
         $                            LDVT )
    *
    *                    Generate left bidiagonalizing vectors in WORK(IU)
    *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
    *
                         CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
         $                            WORK( ITAUQ ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right bidiagonalizing vectors in VT
    *                    (Workspace: need N*N+4*N-1,
    *                                prefer N*N+3*N+(N-1)*NB)
    *
                         CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of R in WORK(IU) and computing
    *                    right singular vectors of R in VT
    *                    (Workspace: need N*N+BDSPAC)
    *
                         CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
         $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
         $                            WORK( IWORK ), INFO )
    *
    *                    Multiply Q in U by left singular vectors of R in
    *                    WORK(IU), storing result in A
    *                    (Workspace: need N*N)
    *
                         CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
         $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
    *
    *                    Copy left singular vectors of A from A to U
    *
                         CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + N
    *
    *                    Compute A=Q*R, copying result to U
    *                    (Workspace: need 2*N, prefer N+N*NB)
    *
                         CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
    *
    *                    Generate Q in U
    *                    (Workspace: need N+M, prefer N+M*NB)
    *
                         CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy R from A to VT, zeroing out below it
    *
                         CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                         IF( N.GT.1 )
         $                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
         $                               VT( 2, 1 ), LDVT )
                         IE = ITAU
                         ITAUQ = IE + N
                         ITAUP = ITAUQ + N
                         IWORK = ITAUP + N
    *
    *                    Bidiagonalize R in VT
    *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
    *
                         CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply Q in U by left bidiagonalizing vectors
    *                    in VT
    *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
    *
                         CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
         $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right bidiagonalizing vectors in VT
    *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    *
                         CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + N
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of A in U and computing right
    *                    singular vectors of A in VT
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
         $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   END IF
    *
                END IF
    *
             } // if (m >= mnthr)
             else { // m < mnthr
    *
    *           M .LT. MNTHR
    *
    *           Path 10 (M at least N, but not much larger)
    *           Reduce to bidiagonal form without QR decomposition
    *
                IE = 1
                ITAUQ = IE + N
                ITAUP = ITAUQ + N
                IWORK = ITAUP + N
    *
    *           Bidiagonalize A
    *           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
    *
                CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
         $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
         $                   IERR )
                IF( WNTUAS ) THEN
    *
    *              If left singular vectors desired in U, copy result to U
    *              and generate left bidiagonalizing vectors in U
    *              (Workspace: need 3*N+NCU, prefer 3*N+NCU*NB)
    *
                   CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
                   IF( WNTUS )
         $            NCU = N
                   IF( WNTUA )
         $            NCU = M
                   CALL DORGBR( 'Q', M, NCU, N, U, LDU, WORK( ITAUQ ),
         $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
                END IF
                IF( WNTVAS ) THEN
    *
    *              If right singular vectors desired in VT, copy result to
    *              VT and generate right bidiagonalizing vectors in VT
    *              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    *
                   CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                   CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
         $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
                END IF
                IF( WNTUO ) THEN
    *
    *              If left singular vectors desired in A, generate left
    *              bidiagonalizing vectors in A
    *              (Workspace: need 4*N, prefer 3*N+N*NB)
    *
                   CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
         $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
                END IF
                IF( WNTVO ) THEN
    *
    *              If right singular vectors desired in A, generate right
    *              bidiagonalizing vectors in A
    *              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
    *
                   CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
         $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
                END IF
                IWORK = IE + N
                IF( WNTUAS .OR. WNTUO )
         $         NRU = M
                IF( WNTUN )
         $         NRU = 0
                IF( WNTVAS .OR. WNTVO )
         $         NCVT = N
                IF( WNTVN )
         $         NCVT = 0
                IF( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) THEN
    *
    *              Perform bidiagonal QR iteration, if desired, computing
    *              left singular vectors in U and computing right singular
    *              vectors in VT
    *              (Workspace: need BDSPAC)
    *
                   CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
         $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
                ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
    *
    *              Perform bidiagonal QR iteration, if desired, computing
    *              left singular vectors in U and computing right singular
    *              vectors in A
    *              (Workspace: need BDSPAC)
    *
                   CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
         $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
                ELSE
    *
    *              Perform bidiagonal QR iteration, if desired, computing
    *              left singular vectors in A and computing right singular
    *              vectors in VT
    *              (Workspace: need BDSPAC)
    *
                   CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
         $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
                END IF
    *
             } // else m < mnthr
    
          } // if (m >= n)
          else { // m < n
    
             // A has more columns than rows. If A has sufficiently more
             // columns than rows, first reduce using the LQ decomposition (if
             // sufficient workspace available)
    
             if(n >= mnthr) {
    *
                IF( WNTVN ) THEN
    *
    *              Path 1t(N much larger than M, JOBVT='N')
    *              No right singular vectors to be computed
    *
                   ITAU = 1
                   IWORK = ITAU + M
    *
    *              Compute A=L*Q
    *              (Workspace: need 2*M, prefer M+M*NB)
    *
                   CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
         $                      LWORK-IWORK+1, IERR )
    *
    *              Zero out above L
    *
                   CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
                   IE = 1
                   ITAUQ = IE + M
                   ITAUP = ITAUQ + M
                   IWORK = ITAUP + M
    *
    *              Bidiagonalize L in A
    *              (Workspace: need 4*M, prefer 3*M+2*M*NB)
    *
                   CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
         $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
         $                      IERR )
                   IF( WNTUO .OR. WNTUAS ) THEN
    *
    *                 If left singular vectors desired, generate Q
    *                 (Workspace: need 4*M, prefer 3*M+M*NB)
    *
                      CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                   END IF
                   IWORK = IE + M
                   NRU = 0
                   IF( WNTUO .OR. WNTUAS )
         $            NRU = M
    *
    *              Perform bidiagonal QR iteration, computing left singular
    *              vectors of A in A if desired
    *              (Workspace: need BDSPAC)
    *
                   CALL DBDSQR( 'U', M, 0, NRU, 0, S, WORK( IE ), DUM, 1, A,
         $                      LDA, DUM, 1, WORK( IWORK ), INFO )
    *
    *              If left singular vectors desired in U, copy them there
    *
                   IF( WNTUAS )
         $            CALL DLACPY( 'F', M, M, A, LDA, U, LDU )
    *
                ELSE IF( WNTVO .AND. WNTUN ) THEN
    *
    *              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
    *              M right singular vectors to be overwritten on A and
    *              no left singular vectors to be computed
    *
                   IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
    *
    *                 Sufficient workspace for a fast algorithm
    *
                      IR = 1
                      IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
    *
    *                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
    *
                         LDWRKU = LDA
                         CHUNK = N
                         LDWRKR = LDA
                      ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
    *
    *                    WORK(IU) is LDA by N and WORK(IR) is M by M
    *
                         LDWRKU = LDA
                         CHUNK = N
                         LDWRKR = M
                      ELSE
    *
    *                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
    *
                         LDWRKU = M
                         CHUNK = ( LWORK-M*M-M ) / M
                         LDWRKR = M
                      END IF
                      ITAU = IR + LDWRKR*M
                      IWORK = ITAU + M
    *
    *                 Compute A=L*Q
    *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                      CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                 Copy L to WORK(IR) and zero out above it
    *
                      CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ), LDWRKR )
                      CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
         $                         WORK( IR+LDWRKR ), LDWRKR )
    *
    *                 Generate Q in A
    *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                      CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                      IE = ITAU
                      ITAUQ = IE + M
                      ITAUP = ITAUQ + M
                      IWORK = ITAUP + M
    *
    *                 Bidiagonalize L in WORK(IR)
    *                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    *
                      CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S, WORK( IE ),
         $                         WORK( ITAUQ ), WORK( ITAUP ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                 Generate right vectors bidiagonalizing L
    *                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
    *
                      CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
         $                         WORK( ITAUP ), WORK( IWORK ),
         $                         LWORK-IWORK+1, IERR )
                      IWORK = IE + M
    *
    *                 Perform bidiagonal QR iteration, computing right
    *                 singular vectors of L in WORK(IR)
    *                 (Workspace: need M*M+BDSPAC)
    *
                      CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
         $                         WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
         $                         WORK( IWORK ), INFO )
                      IU = IE + M
    *
    *                 Multiply right singular vectors of L in WORK(IR) by Q
    *                 in A, storing result in WORK(IU) and copying to A
    *                 (Workspace: need M*M+2*M, prefer M*M+M*N+M)
    *
                      DO 30 I = 1, N, CHUNK
                         BLK = MIN( N-I+1, CHUNK )
                         CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
         $                           LDWRKR, A( 1, I ), LDA, ZERO,
         $                           WORK( IU ), LDWRKU )
                         CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
         $                            A( 1, I ), LDA )
       30             CONTINUE
    *
                   ELSE
    *
    *                 Insufficient workspace for a fast algorithm
    *
                      IE = 1
                      ITAUQ = IE + M
                      ITAUP = ITAUQ + M
                      IWORK = ITAUP + M
    *
    *                 Bidiagonalize A
    *                 (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
    *
                      CALL DGEBRD( M, N, A, LDA, S, WORK( IE ),
         $                         WORK( ITAUQ ), WORK( ITAUP ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                 Generate right vectors bidiagonalizing A
    *                 (Workspace: need 4*M, prefer 3*M+M*NB)
    *
                      CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                      IWORK = IE + M
    *
    *                 Perform bidiagonal QR iteration, computing right
    *                 singular vectors of A in A
    *                 (Workspace: need BDSPAC)
    *
                      CALL DBDSQR( 'L', M, N, 0, 0, S, WORK( IE ), A, LDA,
         $                         DUM, 1, DUM, 1, WORK( IWORK ), INFO )
    *
                   END IF
    *
                ELSE IF( WNTVO .AND. WNTUAS ) THEN
    *
    *              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
    *              M right singular vectors to be overwritten on A and
    *              M left singular vectors to be computed in U
    *
                   IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
    *
    *                 Sufficient workspace for a fast algorithm
    *
                      IR = 1
                      IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
    *
    *                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
    *
                         LDWRKU = LDA
                         CHUNK = N
                         LDWRKR = LDA
                      ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
    *
    *                    WORK(IU) is LDA by N and WORK(IR) is M by M
    *
                         LDWRKU = LDA
                         CHUNK = N
                         LDWRKR = M
                      ELSE
    *
    *                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
    *
                         LDWRKU = M
                         CHUNK = ( LWORK-M*M-M ) / M
                         LDWRKR = M
                      END IF
                      ITAU = IR + LDWRKR*M
                      IWORK = ITAU + M
    *
    *                 Compute A=L*Q
    *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                      CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                 Copy L to U, zeroing about above it
    *
                      CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                      CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
         $                         LDU )
    *
    *                 Generate Q in A
    *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                      CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                      IE = ITAU
                      ITAUQ = IE + M
                      ITAUP = ITAUQ + M
                      IWORK = ITAUP + M
    *
    *                 Bidiagonalize L in U, copying result to WORK(IR)
    *                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    *
                      CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
         $                         WORK( ITAUQ ), WORK( ITAUP ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                      CALL DLACPY( 'U', M, M, U, LDU, WORK( IR ), LDWRKR )
    *
    *                 Generate right vectors bidiagonalizing L in WORK(IR)
    *                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
    *
                      CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
         $                         WORK( ITAUP ), WORK( IWORK ),
         $                         LWORK-IWORK+1, IERR )
    *
    *                 Generate left vectors bidiagonalizing L in U
    *                 (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
    *
                      CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                      IWORK = IE + M
    *
    *                 Perform bidiagonal QR iteration, computing left
    *                 singular vectors of L in U, and computing right
    *                 singular vectors of L in WORK(IR)
    *                 (Workspace: need M*M+BDSPAC)
    *
                      CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
         $                         WORK( IR ), LDWRKR, U, LDU, DUM, 1,
         $                         WORK( IWORK ), INFO )
                      IU = IE + M
    *
    *                 Multiply right singular vectors of L in WORK(IR) by Q
    *                 in A, storing result in WORK(IU) and copying to A
    *                 (Workspace: need M*M+2*M, prefer M*M+M*N+M))
    *
                      DO 40 I = 1, N, CHUNK
                         BLK = MIN( N-I+1, CHUNK )
                         CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
         $                           LDWRKR, A( 1, I ), LDA, ZERO,
         $                           WORK( IU ), LDWRKU )
                         CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
         $                            A( 1, I ), LDA )
       40             CONTINUE
    *
                   ELSE
    *
    *                 Insufficient workspace for a fast algorithm
    *
                      ITAU = 1
                      IWORK = ITAU + M
    *
    *                 Compute A=L*Q
    *                 (Workspace: need 2*M, prefer M+M*NB)
    *
                      CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                 Copy L to U, zeroing out above it
    *
                      CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                      CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
         $                         LDU )
    *
    *                 Generate Q in A
    *                 (Workspace: need 2*M, prefer M+M*NB)
    *
                      CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                      IE = ITAU
                      ITAUQ = IE + M
                      ITAUP = ITAUQ + M
                      IWORK = ITAUP + M
    *
    *                 Bidiagonalize L in U
    *                 (Workspace: need 4*M, prefer 3*M+2*M*NB)
    *
                      CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
         $                         WORK( ITAUQ ), WORK( ITAUP ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                 Multiply right vectors bidiagonalizing L by Q in A
    *                 (Workspace: need 3*M+N, prefer 3*M+N*NB)
    *
                      CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
         $                         WORK( ITAUP ), A, LDA, WORK( IWORK ),
         $                         LWORK-IWORK+1, IERR )
    *
    *                 Generate left vectors bidiagonalizing L in U
    *                 (Workspace: need 4*M, prefer 3*M+M*NB)
    *
                      CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
         $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                      IWORK = IE + M
    *
    *                 Perform bidiagonal QR iteration, computing left
    *                 singular vectors of A in U and computing right
    *                 singular vectors of A in A
    *                 (Workspace: need BDSPAC)
    *
                      CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), A, LDA,
         $                         U, LDU, DUM, 1, WORK( IWORK ), INFO )
    *
                   END IF
    *
                ELSE IF( WNTVS ) THEN
    *
                   IF( WNTUN ) THEN
    *
    *                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
    *                 M right singular vectors to be computed in VT and
    *                 no left singular vectors to be computed
    *
                      IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IR = 1
                         IF( LWORK.GE.WRKBL+LDA*M ) THEN
    *
    *                       WORK(IR) is LDA by M
    *
                            LDWRKR = LDA
                         ELSE
    *
    *                       WORK(IR) is M by M
    *
                            LDWRKR = M
                         END IF
                         ITAU = IR + LDWRKR*M
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q
    *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy L to WORK(IR), zeroing out above it
    *
                         CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ),
         $                            LDWRKR )
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
         $                            WORK( IR+LDWRKR ), LDWRKR )
    *
    *                    Generate Q in A
    *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                         CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Bidiagonalize L in WORK(IR)
    *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right vectors bidiagonalizing L in
    *                    WORK(IR)
    *                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
    *
                         CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing right
    *                    singular vectors of L in WORK(IR)
    *                    (Workspace: need M*M+BDSPAC)
    *
                         CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
         $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
         $                            WORK( IWORK ), INFO )
    *
    *                    Multiply right singular vectors of L in WORK(IR) by
    *                    Q in A, storing result in VT
    *                    (Workspace: need M*M)
    *
                         CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
         $                           LDWRKR, A, LDA, ZERO, VT, LDVT )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q
    *                    (Workspace: need 2*M, prefer M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy result to VT
    *
                         CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
    *
    *                    Generate Q in VT
    *                    (Workspace: need 2*M, prefer M+M*NB)
    *
                         CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Zero out above L in A
    *
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
         $                            LDA )
    *
    *                    Bidiagonalize L in A
    *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply right vectors bidiagonalizing L by Q in VT
    *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    *
                         CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
         $                            WORK( ITAUP ), VT, LDVT,
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing right
    *                    singular vectors of A in VT
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
         $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   ELSE IF( WNTUO ) THEN
    *
    *                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
    *                 M right singular vectors to be computed in VT and
    *                 M left singular vectors to be overwritten on A
    *
                      IF( LWORK.GE.2*M*M+MAX( 4*M, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IU = 1
                         IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
    *
    *                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
    *
                            LDWRKU = LDA
                            IR = IU + LDWRKU*M
                            LDWRKR = LDA
                         ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
    *
    *                       WORK(IU) is LDA by M and WORK(IR) is M by M
    *
                            LDWRKU = LDA
                            IR = IU + LDWRKU*M
                            LDWRKR = M
                         ELSE
    *
    *                       WORK(IU) is M by M and WORK(IR) is M by M
    *
                            LDWRKU = M
                            IR = IU + LDWRKU*M
                            LDWRKR = M
                         END IF
                         ITAU = IR + LDWRKR*M
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q
    *                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy L to WORK(IU), zeroing out below it
    *
                         CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
         $                            LDWRKU )
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
         $                            WORK( IU+LDWRKU ), LDWRKU )
    *
    *                    Generate Q in A
    *                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
    *
                         CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Bidiagonalize L in WORK(IU), copying result to
    *                    WORK(IR)
    *                    (Workspace: need 2*M*M+4*M,
    *                                prefer 2*M*M+3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU,
         $                            WORK( IR ), LDWRKR )
    *
    *                    Generate right bidiagonalizing vectors in WORK(IU)
    *                    (Workspace: need 2*M*M+4*M-1,
    *                                prefer 2*M*M+3*M+(M-1)*NB)
    *
                         CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate left bidiagonalizing vectors in WORK(IR)
    *                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
    *
                         CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
         $                            WORK( ITAUQ ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of L in WORK(IR) and computing
    *                    right singular vectors of L in WORK(IU)
    *                    (Workspace: need 2*M*M+BDSPAC)
    *
                         CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
         $                            WORK( IU ), LDWRKU, WORK( IR ),
         $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
    *
    *                    Multiply right singular vectors of L in WORK(IU) by
    *                    Q in A, storing result in VT
    *                    (Workspace: need M*M)
    *
                         CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
         $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
    *
    *                    Copy left singular vectors of L to A
    *                    (Workspace: need M*M)
    *
                         CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
         $                            LDA )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q, copying result to VT
    *                    (Workspace: need 2*M, prefer M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
    *
    *                    Generate Q in VT
    *                    (Workspace: need 2*M, prefer M+M*NB)
    *
                         CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Zero out above L in A
    *
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
         $                            LDA )
    *
    *                    Bidiagonalize L in A
    *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply right vectors bidiagonalizing L by Q in VT
    *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    *
                         CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
         $                            WORK( ITAUP ), VT, LDVT,
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Generate left bidiagonalizing vectors of L in A
    *                    (Workspace: need 4*M, prefer 3*M+M*NB)
    *
                         CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, compute left
    *                    singular vectors of A in A and compute right
    *                    singular vectors of A in VT
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
         $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   ELSE IF( WNTUAS ) THEN
    *
    *                 Path 6t(N much larger than M, JOBU='S' or 'A',
    *                         JOBVT='S')
    *                 M right singular vectors to be computed in VT and
    *                 M left singular vectors to be computed in U
    *
                      IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IU = 1
                         IF( LWORK.GE.WRKBL+LDA*M ) THEN
    *
    *                       WORK(IU) is LDA by N
    *
                            LDWRKU = LDA
                         ELSE
    *
    *                       WORK(IU) is LDA by M
    *
                            LDWRKU = M
                         END IF
                         ITAU = IU + LDWRKU*M
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q
    *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy L to WORK(IU), zeroing out above it
    *
                         CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
         $                            LDWRKU )
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
         $                            WORK( IU+LDWRKU ), LDWRKU )
    *
    *                    Generate Q in A
    *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                         CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Bidiagonalize L in WORK(IU), copying result to U
    *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
         $                            LDU )
    *
    *                    Generate right bidiagonalizing vectors in WORK(IU)
    *                    (Workspace: need M*M+4*M-1,
    *                                prefer M*M+3*M+(M-1)*NB)
    *
                         CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate left bidiagonalizing vectors in U
    *                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
    *
                         CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of L in U and computing right
    *                    singular vectors of L in WORK(IU)
    *                    (Workspace: need M*M+BDSPAC)
    *
                         CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
         $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
         $                            WORK( IWORK ), INFO )
    *
    *                    Multiply right singular vectors of L in WORK(IU) by
    *                    Q in A, storing result in VT
    *                    (Workspace: need M*M)
    *
                         CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
         $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q, copying result to VT
    *                    (Workspace: need 2*M, prefer M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
    *
    *                    Generate Q in VT
    *                    (Workspace: need 2*M, prefer M+M*NB)
    *
                         CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy L to U, zeroing out above it
    *
                         CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
         $                            LDU )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Bidiagonalize L in U
    *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply right bidiagonalizing vectors in U by Q
    *                    in VT
    *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    *
                         CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
         $                            WORK( ITAUP ), VT, LDVT,
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Generate left bidiagonalizing vectors in U
    *                    (Workspace: need 4*M, prefer 3*M+M*NB)
    *
                         CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of A in U and computing right
    *                    singular vectors of A in VT
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
         $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   END IF
    *
                ELSE IF( WNTVA ) THEN
    *
                   IF( WNTUN ) THEN
    *
    *                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
    *                 N right singular vectors to be computed in VT and
    *                 no left singular vectors to be computed
    *
                      IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IR = 1
                         IF( LWORK.GE.WRKBL+LDA*M ) THEN
    *
    *                       WORK(IR) is LDA by M
    *
                            LDWRKR = LDA
                         ELSE
    *
    *                       WORK(IR) is M by M
    *
                            LDWRKR = M
                         END IF
                         ITAU = IR + LDWRKR*M
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q, copying result to VT
    *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
    *
    *                    Copy L to WORK(IR), zeroing out above it
    *
                         CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ),
         $                            LDWRKR )
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
         $                            WORK( IR+LDWRKR ), LDWRKR )
    *
    *                    Generate Q in VT
    *                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
    *
                         CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Bidiagonalize L in WORK(IR)
    *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate right bidiagonalizing vectors in WORK(IR)
    *                    (Workspace: need M*M+4*M-1,
    *                                prefer M*M+3*M+(M-1)*NB)
    *
                         CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing right
    *                    singular vectors of L in WORK(IR)
    *                    (Workspace: need M*M+BDSPAC)
    *
                         CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
         $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
         $                            WORK( IWORK ), INFO )
    *
    *                    Multiply right singular vectors of L in WORK(IR) by
    *                    Q in VT, storing result in A
    *                    (Workspace: need M*M)
    *
                         CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
         $                           LDWRKR, VT, LDVT, ZERO, A, LDA )
    *
    *                    Copy right singular vectors of A from A to VT
    *
                         CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q, copying result to VT
    *                    (Workspace: need 2*M, prefer M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
    *
    *                    Generate Q in VT
    *                    (Workspace: need M+N, prefer M+N*NB)
    *
                         CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Zero out above L in A
    *
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
         $                            LDA )
    *
    *                    Bidiagonalize L in A
    *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply right bidiagonalizing vectors in A by Q
    *                    in VT
    *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    *
                         CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
         $                            WORK( ITAUP ), VT, LDVT,
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing right
    *                    singular vectors of A in VT
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
         $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   ELSE IF( WNTUO ) THEN
    *
    *                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
    *                 N right singular vectors to be computed in VT and
    *                 M left singular vectors to be overwritten on A
    *
                      IF( LWORK.GE.2*M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IU = 1
                         IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
    *
    *                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
    *
                            LDWRKU = LDA
                            IR = IU + LDWRKU*M
                            LDWRKR = LDA
                         ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
    *
    *                       WORK(IU) is LDA by M and WORK(IR) is M by M
    *
                            LDWRKU = LDA
                            IR = IU + LDWRKU*M
                            LDWRKR = M
                         ELSE
    *
    *                       WORK(IU) is M by M and WORK(IR) is M by M
    *
                            LDWRKU = M
                            IR = IU + LDWRKU*M
                            LDWRKR = M
                         END IF
                         ITAU = IR + LDWRKR*M
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q, copying result to VT
    *                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
    *
    *                    Generate Q in VT
    *                    (Workspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
    *
                         CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy L to WORK(IU), zeroing out above it
    *
                         CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
         $                            LDWRKU )
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
         $                            WORK( IU+LDWRKU ), LDWRKU )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Bidiagonalize L in WORK(IU), copying result to
    *                    WORK(IR)
    *                    (Workspace: need 2*M*M+4*M,
    *                                prefer 2*M*M+3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU,
         $                            WORK( IR ), LDWRKR )
    *
    *                    Generate right bidiagonalizing vectors in WORK(IU)
    *                    (Workspace: need 2*M*M+4*M-1,
    *                                prefer 2*M*M+3*M+(M-1)*NB)
    *
                         CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate left bidiagonalizing vectors in WORK(IR)
    *                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
    *
                         CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
         $                            WORK( ITAUQ ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of L in WORK(IR) and computing
    *                    right singular vectors of L in WORK(IU)
    *                    (Workspace: need 2*M*M+BDSPAC)
    *
                         CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
         $                            WORK( IU ), LDWRKU, WORK( IR ),
         $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
    *
    *                    Multiply right singular vectors of L in WORK(IU) by
    *                    Q in VT, storing result in A
    *                    (Workspace: need M*M)
    *
                         CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
         $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
    *
    *                    Copy right singular vectors of A from A to VT
    *
                         CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
    *
    *                    Copy left singular vectors of A from WORK(IR) to A
    *
                         CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
         $                            LDA )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q, copying result to VT
    *                    (Workspace: need 2*M, prefer M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
    *
    *                    Generate Q in VT
    *                    (Workspace: need M+N, prefer M+N*NB)
    *
                         CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Zero out above L in A
    *
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
         $                            LDA )
    *
    *                    Bidiagonalize L in A
    *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply right bidiagonalizing vectors in A by Q
    *                    in VT
    *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    *
                         CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
         $                            WORK( ITAUP ), VT, LDVT,
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Generate left bidiagonalizing vectors in A
    *                    (Workspace: need 4*M, prefer 3*M+M*NB)
    *
                         CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of A in A and computing right
    *                    singular vectors of A in VT
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
         $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   ELSE IF( WNTUAS ) THEN
    *
    *                 Path 9t(N much larger than M, JOBU='S' or 'A',
    *                         JOBVT='A')
    *                 N right singular vectors to be computed in VT and
    *                 M left singular vectors to be computed in U
    *
                      IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
    *
    *                    Sufficient workspace for a fast algorithm
    *
                         IU = 1
                         IF( LWORK.GE.WRKBL+LDA*M ) THEN
    *
    *                       WORK(IU) is LDA by M
    *
                            LDWRKU = LDA
                         ELSE
    *
    *                       WORK(IU) is M by M
    *
                            LDWRKU = M
                         END IF
                         ITAU = IU + LDWRKU*M
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q, copying result to VT
    *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
    *
    *                    Generate Q in VT
    *                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
    *
                         CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy L to WORK(IU), zeroing out above it
    *
                         CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
         $                            LDWRKU )
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
         $                            WORK( IU+LDWRKU ), LDWRKU )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Bidiagonalize L in WORK(IU), copying result to U
    *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
         $                            WORK( IE ), WORK( ITAUQ ),
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
         $                            LDU )
    *
    *                    Generate right bidiagonalizing vectors in WORK(IU)
    *                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
    *
                         CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
         $                            WORK( ITAUP ), WORK( IWORK ),
         $                            LWORK-IWORK+1, IERR )
    *
    *                    Generate left bidiagonalizing vectors in U
    *                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
    *
                         CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of L in U and computing right
    *                    singular vectors of L in WORK(IU)
    *                    (Workspace: need M*M+BDSPAC)
    *
                         CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
         $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
         $                            WORK( IWORK ), INFO )
    *
    *                    Multiply right singular vectors of L in WORK(IU) by
    *                    Q in VT, storing result in A
    *                    (Workspace: need M*M)
    *
                         CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
         $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
    *
    *                    Copy right singular vectors of A from A to VT
    *
                         CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
    *
                      ELSE
    *
    *                    Insufficient workspace for a fast algorithm
    *
                         ITAU = 1
                         IWORK = ITAU + M
    *
    *                    Compute A=L*Q, copying result to VT
    *                    (Workspace: need 2*M, prefer M+M*NB)
    *
                         CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
    *
    *                    Generate Q in VT
    *                    (Workspace: need M+N, prefer M+N*NB)
    *
                         CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Copy L to U, zeroing out above it
    *
                         CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
         $                            LDU )
                         IE = ITAU
                         ITAUQ = IE + M
                         ITAUP = ITAUQ + M
                         IWORK = ITAUP + M
    *
    *                    Bidiagonalize L in U
    *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
    *
                         CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
         $                            WORK( ITAUQ ), WORK( ITAUP ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Multiply right bidiagonalizing vectors in U by Q
    *                    in VT
    *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
    *
                         CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
         $                            WORK( ITAUP ), VT, LDVT,
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
    *
    *                    Generate left bidiagonalizing vectors in U
    *                    (Workspace: need 4*M, prefer 3*M+M*NB)
    *
                         CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
         $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                         IWORK = IE + M
    *
    *                    Perform bidiagonal QR iteration, computing left
    *                    singular vectors of A in U and computing right
    *                    singular vectors of A in VT
    *                    (Workspace: need BDSPAC)
    *
                         CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
         $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
         $                            INFO )
    *
                      END IF
    *
                   END IF
    *
                END IF
     
             } // if (n >= mnthr)
             else { // n < mnthr
    *
    *           N .LT. MNTHR
    *
    *           Path 10t(N greater than M, but not much larger)
    *           Reduce to bidiagonal form without LQ decomposition
    *
                IE = 1
                ITAUQ = IE + M
                ITAUP = ITAUQ + M
                IWORK = ITAUP + M
    *
    *           Bidiagonalize A
    *           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
    *
                CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
         $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
         $                   IERR )
                IF( WNTUAS ) THEN
    *
    *              If left singular vectors desired in U, copy result to U
    *              and generate left bidiagonalizing vectors in U
    *              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
    *
                   CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                   CALL DORGBR( 'Q', M, M, N, U, LDU, WORK( ITAUQ ),
         $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
                END IF
                IF( WNTVAS ) THEN
    *
    *              If right singular vectors desired in VT, copy result to
    *              VT and generate right bidiagonalizing vectors in VT
    *              (Workspace: need 3*M+NRVT, prefer 3*M+NRVT*NB)
    *
                   CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
                   IF( WNTVA )
         $            NRVT = N
                   IF( WNTVS )
         $            NRVT = M
                   CALL DORGBR( 'P', NRVT, N, M, VT, LDVT, WORK( ITAUP ),
         $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
                END IF
                IF( WNTUO ) THEN
    *
    *              If left singular vectors desired in A, generate left
    *              bidiagonalizing vectors in A
    *              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
    *
                   CALL DORGBR( 'Q', M, M, N, A, LDA, WORK( ITAUQ ),
         $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
                END IF
                IF( WNTVO ) THEN
    *
    *              If right singular vectors desired in A, generate right
    *              bidiagonalizing vectors in A
    *              (Workspace: need 4*M, prefer 3*M+M*NB)
    *
                   CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
         $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
                END IF
                IWORK = IE + M
                IF( WNTUAS .OR. WNTUO )
         $         NRU = M
                IF( WNTUN )
         $         NRU = 0
                IF( WNTVAS .OR. WNTVO )
         $         NCVT = N
                IF( WNTVN )
         $         NCVT = 0
                IF( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) THEN
    *
    *              Perform bidiagonal QR iteration, if desired, computing
    *              left singular vectors in U and computing right singular
    *              vectors in VT
    *              (Workspace: need BDSPAC)
    *
                   CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
         $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
                ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
    *
    *              Perform bidiagonal QR iteration, if desired, computing
    *              left singular vectors in U and computing right singular
    *              vectors in A
    *              (Workspace: need BDSPAC)
    *
                   CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
         $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
                ELSE
    *
    *              Perform bidiagonal QR iteration, if desired, computing
    *              left singular vectors in A and computing right singular
    *              vectors in VT
    *              (Workspace: need BDSPAC)
    *
                   CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
         $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
                END IF
    
             } // else n < mnthr
    
          } // else m < n
    
          // If dbdsqr failed to converge, copy unconverged superdiagonals
          // to work[1:minmn-1]
    
          IF( INFO.NE.0 ) THEN
             IF( IE.GT.2 ) THEN
                DO 50 I = 1, MINMN - 1
                   WORK( I+1 ) = WORK( I+IE-1 )
       50       CONTINUE
             END IF
             IF( IE.LT.2 ) THEN
                DO 60 I = MINMN - 1, 1, -1
                   WORK( I+1 ) = WORK( I+IE-1 )
       60       CONTINUE
             END IF
          END IF
    *
    *     Undo scaling if necessary
    *
          IF( ISCL.EQ.1 ) THEN
             IF( ANRM.GT.BIGNUM )
         $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
         $                   IERR )
             IF( INFO.NE.0 .AND. ANRM.GT.BIGNUM )
         $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN-1, 1, WORK( 2 ),
         $                   MINMN, IERR )
             IF( ANRM.LT.SMLNUM )
         $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
         $                   IERR )
             IF( INFO.NE.0 .AND. ANRM.LT.SMLNUM )
         $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN-1, 1, WORK( 2 ),
         $                   MINMN, IERR )
          END IF
    
         // Return optimal workspace in WORK(1)
   
          work[0] = maxwrk;*/
    
          return;
      } // dgesvd

}