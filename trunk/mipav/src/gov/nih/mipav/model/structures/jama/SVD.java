package gov.nih.mipav.model.structures.jama;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class SVD implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    GeneralizedInverse2 gi = new GeneralizedInverse2();
    /** Common variables in testing routines. */
    private ViewUserInterface UI = ViewUserInterface.getReference();
    private int iparms[];
    
    public SVD () {}
    
    /**
     * This routine is an extraction from the FORTRAN program version 3.4.1 DCHKEE of the code needed to drive ddrvbd in
     * order to run ddrvbd in order to test the singular value decomposition driver dgesvd.
     * Numerical values were obtained from the svd.in datafile. Original DCHKEE created by Univ. of Tennessee,
     * Univ. of California Berkeley, University of Colorado Denver, and NAG Ltd., April, 2012
     */
    public void ddrvbd_test() {

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
        double thresh = 50.0;

        // Test the LAPACK driver
        boolean tstdrv = true;

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
        boolean[] dotype = new boolean[maxtyp];
        int[] ioldsd = new int[] { 0, 0, 0, 1 };
        int[] iseed = new int[] { 0, 0, 0, 1 };
        int nmax = 132;
        int lwork = (nmax * ((5 * nmax) + 5)) + 1;
        double[] work = new double[lwork];
        int[] info = new int[1];
        int liwork = nmax * (5 * nmax + 20);
        int[] iwork = new int[liwork];
        double[][] A;
        double[][] ASAV;
        double[] e;
        double[] s;
        double[] ssav;
        double[][] U;
        double[][] USAV;
        double[][] VT;
        double[][] VTSAV;

        int i;
        int k;
        int nrhs;

        for (i = 0; i < maxtyp; i++) {
            dotype[i] = true;
        }

        iparms = new int[9];
        A = new double[nmax][nmax];
        ASAV = new double[nmax][nmax];
        e = new double[nmax];
        s = new double[nmax];
        ssav = new double[nmax];
        U = new double[nmax][nmax];
        USAV = new double[nmax][nmax];
        VT = new double[nmax][nmax];
        VTSAV = new double[nmax][nmax];
        
        xlaenv(1, 1);
        xlaenv(9, 25);
        
        if (tsterr && tstdrv) {
            //derred();
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

            Preferences.debug("Paramter " + i + " for ddrvbd\n");
            Preferences.debug("Blocksize nb = " + nbval[i-1] + "\n");
            Preferences.debug("Minimum blocksize nbmin = " + nbmin[i - 1] + "\n");
            Preferences.debug("Crossover point nx = " + nxval[i-1] + "\n");
            Preferences.debug("Number of right hand sides nrhs = " + nrhs + "\n");

            if (tstdrv) {
                ddrvbd(nsizes, mval, nval, maxtyp, dotype, iseed, thresh, A,
                       nmax,  U, nmax, VT, nmax, ASAV, USAV, VTSAV, s, ssav, e,
                       work, lwork, iwork, info);

                if (info[0] != 0) {
                    MipavUtil.displayError("ddrvbd had info = " + info[0]);
                }
            } // if (tstdrv)
        } // for (i = 1; i <= nparms; i++)
    } // ddrvbd_test
    
    /*  ddrvbd checks the singular value dcomposition (SVD) driver dgesvd.
      
       This is a port of a portion of LAPACK test routine DDRVBD.F version 3.4.0 
       provided by University of Tennessee, University of California Berkeley,
       University of Colorado Denver, and NAG Ltd.
       November, 2011
       
       dgesvd factors A = U diag(s) VT, where U and VT are
       orthogonal and diag(s) is diagonal with the entries of the array s
       on its diagonal. The entries of s are the singular values,
       nonnegative and stored in decreasing order.  U and VT can be
       optionally not computed, overwritten on A, or computed partially.
      
       A is m by n. Let mnmin = min(m, n). s has dimension mnmin.
       U can be m by m or m by mnmin. VT can be n by n or mnmin by n.
      
       When ddrvbd is called, a number of matrix "sizes" (m's and n's)
       and a number of matrix "types" are specified.  For each size (m,n)
       and each type of matrix, and for the minimal workspace as well as
       workspace adequate to permit blocking, an  m x n  matrix "A" will be
       generated and used to test the SVD routines.  For each matrix, A will
       be factored as A = U diag(s) VT and the following 7 tests computed:
      
       Test for dgesvd:
    
       (1)    | A - U diag(s) VT | / ( |A| max(m,n) ulp )
    
       (2)    | I - U'U | / ( m ulp )
      
       (3)    | I - VT VT' | / ( n ulp )
    
       (4)    s contains mnmin nonnegative values in decreasing order.
              (Return 0 if true, 1/ulp if false.)
      
       (5)    | U - Upartial | / ( m ulp ) where Upartial is a partially
              computed U.
      
       (6)    | VT - VTpartial | / ( n ulp ) where VTpartial is a partially
              computed VT.
      
       (7)    | s - spartial | / ( mnmin ulp |s| ) where spartial is the
              vector of singular values from the partial SVD
              
       The "sizes" are specified by the arrays mm[0:nsizes-1] and
       nn[0:nsizes-1]; the value of each element pair (mm[j],nn[j])
       specifies one size.  The "types" are specified by a boolean array
       dotype[0:ntypes-1]; if dotype[j] is true, then matrix type "j"
       will be generated.
       Currently, the list of possible types is:
      
       (1)  The zero matrix.
       (2)  The identity matrix.
       (3)  A matrix of the form  U D V, where U and V are orthogonal and
            D has evenly spaced entries 1, ..., ulp with random signs
            on the diagonal.
       (4)  Same as (3), but multiplied by the underflow-threshold / ulp.
       (5)  Same as (3), but multiplied by the overflow-threshold * ulp.
       
       @param input int nsizes  
              The number of matrix sizes (m,n) contained in the vectors mm and nn.
       @param input int[] mm of dimension nsizes
              The values of the matrix row dimension m.
       @param input int[] nn of dimension nsizes
              The values of the matrix column dimension n.
       @param input int ntypes 
              The number of elements in dotype.   If it is zero, ddrvbd
              does nothing.  It must be at least zero.  If it is maxtyp+1
              and nsizes is 1, then an additional type, maxtyp+1 is
              defined, which is to use whatever matrices are in A and B.
              This is only useful if dotype[0:maxtyp-1] is false and
              dotype[maxtyp] is true.
       @param input boolean[] dotype of dimension ntypes.
              If dotype[j] is .true, then for each size (m,n), a matrix
              of type j will be generated.  If ntypes is smaller than the
              maximum number of types defined (PARAMETER maxtyp), then
              types ntypes+1 through maxtyp will not be generated.  If
              ntypes is larger than maxtyp, dotype[maxtyp] through
              dotype[ntypes-1] will be ignored.
       @param input/output int[] iseed of dimension 4.
              On entry, the seed of the random number generator.  The array
              elements should be between 0 and 4095; if not they will be
              reduced mod 4096.  Also, iseed[3] must be odd.
              On exit, iseed is changed and can be used in the next call to
              ddrvbd to continue the same random number sequence.
       @param input double thresh
              The threshold value for the test ratios.  A result is
              included in the output file if result >= thresh.  The test
              ratios are scaled to be O(1), so thresh should be a small
              multiple of 1, e.g., 10 or 100.  To have every test ratio
              printed, use thresh = 0.
       @param output double[][] of dimension (lda, nmax)
              where nmax is the maximum value of n in nn.
       @param input int lda
              The leading dimension of the array A.  lda >= max(1, mmax).
              where mmax is the maximum value of m in mm.
       @param output double[][] U of dimension (ldu, mmax)
       @param input int ldu
              The leading dimension of the array U.  ldu >= max(1, mmax).
       @param out double[][] VT of dimension (ldvt, nmax) 
       @param input int ldvt
              The leading dimension of array VT.  ldvt >= max(1, nmax).
       @param out double[][] ASAV of dimension (lda, nmax)
       @param out double[][] USAV of dimension (ldu, mmax)
       @param out double[][] VTSAV of dimension (ldvt, nmax)
       @param out double[] s of dimension (max(min(mm,nn))
       @param out double[] ssav of dimension (max(min(mm,nn)) 
       @param out double[] e of dimension (max(min(mm,nn))
       @param out double[] work of dimension lwork 
       @param in int lwork 
              The number of entries in work.  This must be at least
              max(3*mn+mx,5*mn-4)+2*mn**2 for all pairs
              pairs  (mn,mx)=( min(mm[j],nn[j], max(mm[j],nn[j]) ) 
       @param out int[] iwork of dimension at least 8 * min(m,n)
       @param out int[] info of dimension 1
              If 0, then everything ran OK.
                -1: nsizes < 0
                -2: Some mm[j] < 0
                -3: Some nn[j] < 0
                -4: ntypes < 0
                -7: thresh < 0
               -10: lda < 1 or lda < mmax, where mmax is max( mm[j] ).
               -12: ldu < 1 or ldu < mmax.
               -14: ldvt < 1 or ldvt < nmax, where nmax is max( nn[j] ).
               -21: lwork too small.
                If  dlatms or dgesvd returns an error code, the
                    absolute value of it is returned.  
     */
    
      private void ddrvbd(int nsizes, int[] mm, int[] nn, int ntypes, boolean[] dotype,
                          int iseed[], double thresh, double[][] A, int lda, double[][]U,
                          int ldu, double[][] VT, int ldvt, double[][] ASAV, 
                          double[][] USAV, double[][] VTSAV, double[] s, double[] ssav,
                          double[] e, double[] work, int lwork, int[] iwork, int[] info) {
    
          final int maxtyp = 5;
          boolean badmm;
          boolean badnn;
          char jobu;
          char jobvt;
          char[] path = new char[3];
          int i;
          int iinfo[] = new int[1];
          int iju;
          int ijvt;
          int iws;
          int iwtmp;
          int j;
          int jsize;
          int jtype;
          int lswork;
          int m;
          int minwrk;
          int mmax;
          int mnmax;
          int mnmin;
          int mtypes;
          int n;
          int nfail;
          int nmax;
          int ntest;
          double anorm = 0.0;
          double dif[] = new double[1];
          double div;
          double ovfl[] = new double[1];
          double ulp;
          double ulpinv;
          double unfl[] = new double[1];
          char cjob[] = new char[]{'N', 'O', 'S', 'A'};
          int ioldsd[] = new int[4];
          double result[] = new double[7];
          int minmmnn;
          int maxmmnn;
          double res[] = new double[1];
          double workArr[][];
          
          // Check for errors
     
          info[0] = 0;
          badmm = false;
          badnn = false;
          mmax = 1;
          nmax = 1;
          mnmax = 1;
          minwrk = 1;
          for (j = 0; j < nsizes; j++) {
             mmax = Math.max(mmax, mm[j]);
             if (mm[j] < 0) {
                badmm = true;
             }
             nmax = Math.max(nmax, nn[j]);
             if (nn[j] < 0) {
                badnn = true;
             }
             mnmax = Math.max(mnmax, Math.min(mm[j], nn[j]));
             minmmnn = Math.min(mm[j], nn[j]);
             maxmmnn = Math.max(mm[j], nn[j]);
             minwrk = Math.max(minwrk, Math.max(3*minmmnn + maxmmnn, 
                      5*Math.min(mm[j], nn[j]-4 ) )+2*minmmnn*minmmnn);
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
          else if(lda < Math.max(1, mmax)) {
             info[0] = -10;
          }
          else if (ldu < Math.max(1, mmax)) {
             info[0] = -12;
          }
          else if (ldvt < Math.max(1, nmax)) {
             info[0] = -14;
          }
          else if (minwrk > lwork) {
             info[0] = -21;
          }
    
          if (info[0] != 0) {
             MipavUtil.displayError("ddrvbd had info[0] = " + info[0]);
             return;
          }
    
          // Initialize constants
    
          path[0] = 'D'; // Double precision
          path[1] = 'B';
          path[2] = 'D';
          nfail = 0;
          ntest = 0;
          unfl[0] = ge.dlamch('S'); // Safe minimum
          ovfl[0] = 1.0 / unfl[0];
          ge.dlabad(unfl, ovfl);
          ulp = ge.dlamch('P'); // Precision
          ulpinv = 1.0 / ulp;
    
          // Loop over sizes, types
    
          for (jsize = 0; jsize < nsizes; jsize++) {
             m = mm[jsize];
             n = nn[jsize];
             mnmin = Math.min(m, n);
    
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
    
                // Compute "A"
     
                if (mtypes <= maxtyp) {
                    if (jtype == 1) {
        
                       // Zero matrix
        
                       ge.dlaset('F', m, n, 0.0, 0.0, A, lda);
        
                    } // if (jtype == 1)
                    else if (jtype == 2) {
        
                       // Identity matrix
         
                       ge.dlaset('F', m, n, 0.0, 1.0, A, lda);
        
                    } // else if (jtype == 2)
                    else {
        
                       // (Scaled) random matrix
        
                       if (jtype == 3) {
                          anorm = 1.0;
                       }
                       if (jtype == 4) {
                          anorm = unfl[0] / ulp;
                       }
                       if (jtype == 5) {
                          anorm = ovfl[0]*ulp;
                       }
                       ge.dlatms(m, n, 'U', iseed, 'N', s, 4, (double)mnmin,
                                 anorm, m-1, n-1, 'N', A, lda, work, iinfo);
                       if (iinfo[0] != 0) {
                          Preferences.debug("Genrator ddrvbd returned info[0] = " + iinfo[0] + "\n", Preferences.DEBUG_ALGORITHM);
                          Preferences.debug("m = " + m + "\n", Preferences.DEBUG_ALGORITHM);
                          Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                          Preferences.debug("jtype = " + jtype + "\n", Preferences.DEBUG_ALGORITHM);
                          for (i = 0; i < 3; i++) {
                              Preferences.debug("ioldsd["+i+"] = " + ioldsd[i] + "\n", Preferences.DEBUG_ALGORITHM);
                          }
                          info[0] = Math.abs(iinfo[0]);
                          return;
                       } // if (iinfo[0] != 0)
                    } // else 
    
                } // if (mtypes <= maxtyp)
                ge.dlacpy('F', m, n, A, lda, ASAV, lda);
    
                // Do for minimal and adequate (for blocking) workspace
    
                for (iws = 1; iws <= 4; iws++) {
    
                   for (j = 0; j < 7; j++) {
                      result[j] = -1.0;
                   } // for (j = 0; j < 7; j++)
     
                   // Test dgesvd: Factorize A
    
                   iwtmp = Math.max(3*Math.min(m, n)+Math.max(m, n), 5*Math.min(m, n));
                   lswork = iwtmp + (iws-1)*(lwork-iwtmp) / 3;
                   lswork = Math.min(lswork, lwork);
                   lswork = Math.max(lswork, 1);
                   if (iws == 4) {
                      lswork = lwork;
                   }
     
                   if (iws > 1) {
                      ge.dlacpy('F', m, n, ASAV, lda, A, lda);
                   }
                   dgesvd('A', 'A', m, n, A, lda, ssav, USAV, ldu,
                          VTSAV, ldvt, work, lswork, iinfo);
                   if (iinfo[0] != 0) {
                      Preferences.debug("GESVD ddrvbd returned info[0] = " + iinfo[0] + "\n", Preferences.DEBUG_ALGORITHM);
                      Preferences.debug("m = " + m + "\n", Preferences.DEBUG_ALGORITHM);
                      Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                      Preferences.debug("jtype = " + jtype + "\n", Preferences.DEBUG_ALGORITHM);
                      Preferences.debug("lswork = " + lswork + "\n", Preferences.DEBUG_ALGORITHM);
                      for (i = 0; i < 3; i++) {
                          Preferences.debug("ioldsd["+i+"] = " + ioldsd[i] + "\n", Preferences.DEBUG_ALGORITHM);
                      }
                      info[0] = Math.abs(iinfo[0]);
                      return;
                   } // if (iinfo[0] != 0)
    
                   // Do tests 1--4
    
                   gi.dbdt01(m, n, 0, ASAV, lda, USAV, ldu, ssav, e,
                                VTSAV, ldvt, work, result);
                   if (m != 0 && n != 0) {
                      workArr = new double[Math.max(m,n)][Math.max(m,n)];
                      gi.dort01('C', m, m, USAV, ldu, workArr, lwork, res);
                      result[1] = res[0];
                      gi.dort01('R', n, n, VTSAV, ldvt, workArr, lwork, res);
                      result[2] = res[0];
                   } // if (m != 0 && n != 0)
                   result[3] = 0.0;
                   for (i = 0; i < mnmin - 1; i++) {
                      if (ssav[i] < ssav[i+1]) {
                         result[3] = ulpinv;
                      }
                      if (ssav[i] < 0.0) {
                         result[3] = ulpinv;
                      }
                   } // for (i = 0; i < mnmin - 1; i++)
                   if (mnmin >= 1) {
                      if (ssav[mnmin-1] < 0.0) {
                         result[3] = ulpinv;
                      }
                   } // if (mnmin >= 1)
    
                   // Do partial SVDs, comparing to SSAV, USAV, and VTSAV
    
                   result[4] = 0.0;
                   result[5] = 0.0;
                   result[6] = 0.0;
                   for (iju = 0; iju <= 3; iju++) {
                      for (ijvt = 0; ijvt <= 3; ijvt++) {
                         if((iju == 3 && ijvt == 3) ||
                            (iju == 1 && ijvt == 1)) {
                             continue;
                         }
                         jobu = cjob[iju];
                         jobvt = cjob[ijvt];
                         ge.dlacpy('F', m, n, ASAV, lda, A, lda);
                         dgesvd(jobu, jobvt, m, n, A, lda, s, U, ldu,
                                VT, ldvt, work, lswork, iinfo);
     
                         //Compare U
    
                         dif[0] = 0.0;
                         if (m > 0 && n > 0) {
                            if (iju == 1) {
                               dort03('C', m, mnmin, m, mnmin, USAV,
                                       ldu, A, lda, work, lwork, dif,
                                       iinfo);
                            }
                            else if (iju == 2) {
                               dort03('C', m, mnmin, m, mnmin, USAV,
                                      ldu, U, ldu, work, lwork, dif,
                                      iinfo);
                            }
                            else if (iju == 3) {
                               dort03('C', m, m, m, mnmin, USAV, ldu,
                                      U, ldu, work, lwork, dif,
                                      iinfo);
                            }
                         } // if (m > 0 && n > 0)
                         result[4] = Math.max(result[4], dif[0]);
    
                         // Compare VT
    
                         dif[0] = 0.0;
                         if (m > 0 && n > 0) {
                            if (ijvt == 1) {
                               dort03('R', n, mnmin, n, mnmin, VTSAV,
                                      ldvt, A, lda, work, lwork, dif,
                                      iinfo);
                            }
                            else if (ijvt == 2) {
                               dort03('R', n, mnmin, n, mnmin, VTSAV,
                                      ldvt, VT, ldvt, work, lwork,
                                      dif, iinfo);
                            }
                            else if (ijvt == 3) {
                               dort03('R', n, n, n, mnmin, VTSAV,
                                      ldvt, VT, ldvt, work, lwork,
                                      dif, iinfo);
                            }
                         } // if (m > 0 && n > 0)
                         result[5] = Math.max(result[5], dif[0]);
    
                         // Compare s
    
                         dif[0] = 0.0;
                         div = Math.max(mnmin*ulp*s[0], unfl[0]);
                         for (i = 0; i < mnmin - 1; i++) {
                            if (ssav[i] < ssav[i+1]) {
                               dif[0] = ulpinv;
                            }
                            if (ssav[i] < 0.0) {
                               dif[0] = ulpinv;
                            }
                            dif[0] = Math.max(dif[0], Math.abs(ssav[i] - s[i]) / div);
                         } // for (i = 0; i < mnmin - 1; i++)
                         result[6] = Math.max(result[6], dif[0]);
                      } // for (ijvt = 0; ijvt <= 3; ijvt++)
                   } // for (iju = 0; iju <= 3; iju++)
    
                   // End of Loop -- Check for result[j] >= thresh
    
                   for (j = 0; j < 7; j++) {
                      if (result[j] >= thresh) {
                         if (nfail == 0) {
                            Preferences.debug("SVD -- Real Singular Value Decompostion Driver\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("Matrix types (see ddrvbd for details) :\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("1 = Zero matrix\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("2 = Identity matrix\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("3 = Evenly spaced singular values near 1\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("4 = Evenly spaced singular values near underflow\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("5 = Evenly spaced singular values near overflow\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("Test performed: (A is dense, U and V are orthogonal\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("s is an array, and Upartial, VTpartial, and\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("spartial are partially computed U, VT, and s\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("1 = | A - U diag(s) VT | / ( |A| max(m,n) ulp)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("2 = | I - U**T U | / ( m ulp)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("3 = | I - VT VT**T | / (n ulp)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("4 = 0 if s contains min(m,n) nonnegative values in decreasing order, else 1/ulp\n",
                                               Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("5 = | U - Upartial | / (m ulp)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("6 = | VT - VTpartial | / (n ulp)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("7 = | s - spartial | / (min(m,n) ulp |s| )\n", Preferences.DEBUG_ALGORITHM);
                         } // if (nfail == 0)
                         Preferences.debug("m = " + m + "\n", Preferences.DEBUG_ALGORITHM);
                         Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                         Preferences.debug("jtype = " + jtype + "\n", Preferences.DEBUG_ALGORITHM);
                         Preferences.debug("iws = " + iws + "\n", Preferences.DEBUG_ALGORITHM);
                         for (i = 0; i < 3; i++) {
                             Preferences.debug("ioldsd["+i+"] = " + ioldsd[i] + "\n", Preferences.DEBUG_ALGORITHM);
                         }
                         Preferences.debug("result["+j+"] = " + result[j] + "\n", Preferences.DEBUG_ALGORITHM);
                         nfail = nfail + 1;
                      } // if (result[j] >= thresh)
                   } // for (j = 0; j < 7; j++)
                   ntest = ntest + 7;
    
                } // for (iws = 1; iws <= 4; iws++)
             } // for (jtype = 1; jtype <= mtypes; jtype++)
          } // for (jsize = 0; jsize < nsizes; jsize++)
     
          // Summary
     
          if (nfail > 0) {
              Preferences.debug(nfail + " out of " + ntest + " dgesvd tests failed by being >= the threshold\n",
                                Preferences.DEBUG_ALGORITHM);
              UI.setDataText(nfail + " out of " + ntest + " dgesvd tests failed by being >= the threshold\n");
          }
          else {
              Preferences.debug("All " + ntest + " dgesvd tests passed by being less than the threshold\n",
                                 Preferences.DEBUG_ALGORITHM);
              UI.setDataText("All " + ntest + " dgesvd tests passed by being less than the threshold\n");
          }
      
          return;
      } // ddrvbd
      
      /* This is a port of version 3.4.0 test routine DORT03.f of LAPACK provided by  Univ. of Tennessee, 
      Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd.
      November 2011
      
      dort03 compares two orthogonal matrices U and V to see if their
      corresponding rows or columns span the same spaces.  The rows are
      checked if rc = 'R', and the columns are checked ifrcC = 'C'.
      
      result is the maximum of
      
      | V*V' - I | / ( mv ulp ), if rc = 'R', or
      
      | V'*V - I | / ( mv ulp ), if RC = 'C',
      
      and the maximum over rows (or columns) 1 to k of
     
      | U[i] - s*V[i] |/ ( n ulp )
      
      where s is +-1 (chosen to minimize the expression), U[i] is the i-th
      row (column) of U, and V(i) is the i-th row (column) of V.\
      
      @param input char rc
             If rc = 'R' the rows of U and V are to be compared.
             If rc = 'C' the columns of U and V are to be compared.
      @param input int mu
             The number of rows of U if rc = 'R', and the number of
             columns if rc = 'C'.  If mu = 0 dort03 does nothing.
             mu must be at least zero.
      @param input int mv
             The number of rows of V if rc = 'R', and the number of
             columns if rc = 'C'.  If mv = 0 dort03 does nothing.
             mv must be at least zero.
      @param input int n
             If rc = 'R', the number of columns in the matrices U and V,
             and if rc = 'C', the number of rows in U and V.  If n = 0
             dort03 does nothing.  n must be at least zero.
      @param input int k
             The number of rows or columns of U and V to compare.
             0 <= k <= max(mu,mv).
      @param input double[][] U of dimension (ldu,n)
             The first matrix to compare.  If rc = 'R', U is mu by n, and
             if rc = 'C', U is n by mu.
      @param input int ldu
             The leading dimension of U.  If rc = 'R', ldu >= max(1,mu),
             and if rc = 'C', ldu >= max(1,n).
      @param input double[][] V of dimension (ldv,n)
             The second matrix to compare.  If rc = 'R', V is mv by n, and
             if rc = 'C', V is n by mv.
      @param input int ldv
             The leading dimension of V.  If rc = 'R', ldv >= max(1,mv),
             and if rc = 'C', ldv >= max(1,n).
      @param output double[] work of dimension (lwork)
      @param input int lwork
             The length of the array work.  For best performance, lwork
             should be at least n*n if rc = 'C' or m*m if rc = 'R', but
             the tests will be done even if lwork is 0.
      @param output double[] result of dimension (1)
             The value computed by the test described above.  result is
             limited to 1/ulp to avoid overflow.
      @param output int[] info of dimension (1)
             0  indicates a successful exit
             -k indicates the k-th parameter had an illegal value
      */
      private void dort03(char rc, int mu, int mv, int n, int k, double[][] U,
                          int ldu, double[][] V, int ldv, double work[], int lwork,
                          double result[], int info[]) {
     
            int i;
            int irc;
            int j;
            int lmx;
            double res1;
            double res2[] = new double[1]; 
            double s;
            double ulp;
            int p;
            double maxVal;
            double s1;
            double s2;
            double workArr[][] = new double[Math.min(mv, n)][Math.min(mv, n)];
          
            // Check inputs
      
            info[0] = 0;
            if ((rc == 'R') || (rc == 'r')) {
               irc = 0;
            }
            else if ((rc == 'C') || (rc == 'c')) {
               irc = 1;
            }
            else {
               irc = -1;
            }
            if (irc == -1) {
               info[0] = -1;
            }
            else if (mu < 0) {
               info[0] = -2;
            }
            else if (mv < 0) {
               info[0] = -3;
            }
            else if (n < 0) {
               info[0] = -4;
            }
            else if (k < 0 || k > Math.max(mu, mv)) {
               info[0] = -5;
            }
            else if ((irc == 0 && ldu < Math.max(1, mu)) ||
                     (irc == 1 && ldu < Math.max(1, n))) {
               info[0] = -7;
            }
            else if ((irc == 0 && ldv < Math.max(1, mv)) ||
                     (irc == 1 && ldv < Math.max(1, n))) {
               info[0] = -9;
            }
            if (info[0] != 0) {
               MipavUtil.displayError("dort03 had info[0] = " + info[0]);
               return;
            }
     
            // Initialize result
      
            result[0] = 0.0;
            if (mu == 0 || mv == 0 || n == 0) {
               return;
            }
       
            // Machine constants
       
            ulp = ge.dlamch('P'); // Precision
      
            if (irc == 0) {
      
               // Compare rows
       
               res1 = 0.0;
               for (i = 0; i < k; i++) {
                  lmx = 0;
                  maxVal = Math.abs(U[i][0]);
                  for (p = 1; p < n; p++) {
                      if (Math.abs(U[i][p]) > maxVal) {
                          maxVal = Math.abs(U[i][p]);
                          lmx = p;
                      }
                  } // for (p = 1; p < n; p++)
                  if (U[i][lmx] >= 0.0) {
                      s1 = 1.0;
                  }
                  else {
                      s1 = -1.0;
                  }
                  if (V[i][lmx] >= 0.0) {
                      s2 = 1.0;
                  }
                  else {
                      s2 = -1.0;
                  }
                  s = s1 * s2;
                  for (j = 0; j < n; j++) {
                     res1 = Math.max(res1, Math.abs(U[i][j]-s*V[i][j]));
                  } // for (j = 0; j < n; j++)
               } // for (i = 0; i < k; i++)
               res1 = res1 / ((double)n * ulp);
       
               // Compute orthogonality of rows of V.
       
               gi.dort01('R', mv, n, V, ldv, workArr, lwork, res2);
      
            } // if (irc == 0)
            else {
      
               // Compare columns
      
               res1 = 0.0;
               for (i = 0; i < k; i++) {
                   lmx = 0;
                   maxVal = Math.abs(U[0][i]);
                   for (p = 1; p < n; p++) {
                       if (Math.abs(U[p][i]) > maxVal) {
                           maxVal = Math.abs(U[p][i]);
                           lmx = p;
                       }
                   } // for (p = 1; p < n; p++)
                   if (U[lmx][i] >= 0.0) {
                       s1 = 1.0;
                   }
                   else {
                       s1 = -1.0;
                   }
                   if (V[lmx][i] >= 0.0) {
                       s2 = 1.0;
                   }
                   else {
                       s2 = -1.0;
                   }
                   s = s1 * s2;
                   for (j = 0; j < n; j++) {
                       res1 = Math.max(res1, Math.abs(U[j][i]-s*V[j][i]));
                    } // for (j = 0; j < n; j++)
               } // for (i = 0; i < k; i++)
               res1 = res1 / ((double)n*ulp);
      
               // Compute orthogonality of columns of V.
       
               gi.dort01('C', n, mv, V, ldv, workArr, lwork, res2);
            } // else
      
            result[0] = Math.min(Math.max(res1, res2[0]), 1.0 / ulp);
            return;
      } // dortt03


    
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
          int maxwrk = 0;
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
                      workiwork = new double[Math.max(4*n, Math.max(1, lwork-iwork+1))];
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
                         ge.dgemm('N', 'N', chunk, n, n, 1.0, arr2,
                                     lda, arr, ldwrkr, 0.0,
                                     arr3, ldwrku);
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
                         ge.dgemm('N', 'N', chunk, n, n, 1.0, arr2,
                                     lda, arr, ldwrkr, 0.0,
                                     arr3, ldwrku);
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
                      workiwork = new double[Math.max(4*n,Math.max(1, lwork-iwork+1))];
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
                                   workitauq, workiwork,
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
                                 arr[i+1][j] = arr2[i][j];
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
    
          if (iscl == 1) {
             if (anrm > bignum) {
                arr = new double[minmn][1];
                for (i = 0; i < minmn; i++) {
                    arr[i][0] = s[i];
                }
                ge.dlascl('G', 0, 0, bignum, anrm, minmn, 1, arr, minmn, ierr);
                for (i = 0; i < minmn; i++) {
                    s[i] = arr[i][0];
                }
             }
             if (info[0] != 0 && anrm >  bignum) {
                arr = new double[minmn-1][1];
                for (i = 0; i < minmn-1; i++) {
                    arr[i][0] = work[i+1];
                }
                ge.dlascl('G', 0, 0, bignum, anrm, minmn-1, 1, arr,
                          minmn, ierr);
                for (i = 0; i < minmn-1; i++) {
                    work[i+1] = arr[i][0];
                }
             }
             if (anrm < smlnum) {
                 arr = new double[minmn][1];
                 for (i = 0; i < minmn; i++) {
                     arr[i][0] = s[i];
                 }
                 ge.dlascl('G', 0, 0, smlnum, anrm, minmn, 1, arr, minmn,
                           ierr);
                 for (i = 0; i < minmn; i++) {
                     s[i] = arr[i][0];
                 }
             }
             if (info[0] != 0 && anrm < smlnum) {
                 arr = new double[minmn-1][1];
                 for (i = 0; i < minmn-1; i++) {
                     arr[i][0] = work[i+1];
                 }
                 ge.dlascl('G', 0, 0, smlnum, anrm, minmn-1, 1, arr,
                             minmn, ierr);
                 for (i = 0; i < minmn-1; i++) {
                    work[i+1] = arr[i][0];
                }            
             }
          } // if (iscl == 1)
    
         // Return optimal workspace in WORK(1)
   
          work[0] = maxwrk;
    
          return;
      } // dgesvd
      
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