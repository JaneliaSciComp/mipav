package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class LinearEquations2 implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    GeneralizedInverse2 gi = new GeneralizedInverse2();
    LinearEquations le = new LinearEquations();
    private ViewUserInterface UI = ViewUserInterface.getReference();
    
    private int iparms[];

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    /**
     * Creates a new LinearEquations2 object.
     */
    public LinearEquations2() {}
    
    /*
     * This is a port of a portion of LAPACK test routine DCHKAA.f version 3.4.1 and data file dtest.in.
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., April, 2012
     * 
     * dchkaa is the main test program for the double precision LAPACK linear equation routines
     */
    public void dchkaa() {
        // nmax is the maximum allowable value for m and n.
        final int nmax = 132;
        final int maxrhs = 16;
        // nm is the number of values of m.
        int nm = 7;
        // mval is the values of m (row dimension)
        int mval[] = new int[]{0, 1, 2, 3, 5, 10, 16};
        // nn is the number of values of n
        int nn = 7;
        // nval is the values of n (column dimension)
        int nval[] = new int[]{0, 1, 2, 3, 5, 10, 16};
        //  nns is the number of values of nrhs
        int nns = 3;
        // nsval is the values of nrhs (number of right hand sides)
        int nsval[] = new int[]{1, 2, 15};
        // nnb is the number of values of nb
        int nnb = 5;
        // nbval is the values of nb (the blocksize)
        int nbval[] = new int[]{1, 3, 3, 3, 20};
        // nxval is the values of nx (crossover points)
        int nxval[] = new int[]{1, 0, 5, 9 , 1};
        // nrank is the number of values of rank
        int nrank = 3;
        // rankval is the values of rank (as a % of n)
        int rankval[] = new int[]{30, 50, 90};
        // thresh if the threshold value of the test ratio
        double thresh = 20.0;
        // tstchk is the flag to test the LAPACK routines
        boolean tstchk = true;
        // tstdrv is the flag to test the driver routines
        boolean tstdrv = false;
        String path = new String("DGE");
        int nmats = 11;
        
        // Number of unique values of nb
        int nnb2;
        // nbval2 is the set of unique values of nb
        int nbval2[] = new int[nbval.length];

        int lda;
        boolean fatal;
        int i;
        int j;
        int nb;
        double eps;
        int ntypes = 11;
        boolean dotype[] = new boolean[ntypes];
        double A[][] = new double[nmax][nmax];
        double AFAC[][] = new double[nmax][nmax];
        double AINV[][] = new double[nmax][nmax];
        double ASAV[][] = new double[nmax][nmax];
        double s[] = new double[nmax];
        // nsmax is the largest entry in nsval
        int nsmax;
        nsmax = nsval[0];
        for (i = 1; i < nsval.length; i++) {
            if (nsval[i] > nsmax) {
                nsmax = nsval[i];  
            }
        }
        double B[][] = new double[nmax][maxrhs];
        double BSAV[][] = new double[nmax][maxrhs];
        double X[][] = new double[nmax][maxrhs];
        double XACT[][] = new double[nmax][maxrhs];
        double WORK[][] = new double[nmax][maxrhs];
        double rwork[] = new double[nmax + 2*maxrhs];
        int iwork[] = new int[nmax];
        
        lda = nmax;
        fatal = false;
        
        // Set nbval2 to be the set of unique values of nb
        nnb2 = 0;
        loop:
        for (i = 0; i < nnb; i++) {
            nb = nbval[i];
            for (j = 0; j < nnb2; j++) {
                if (nb == nbval2[j]) {
                    continue loop;
                }
            } // for (j = 0; j < nnb2; j++)
            nbval2[nnb2++] = nb;
        } // for (i = 0; i < nnb; i++)

        for (i = 0; i < ntypes; i++) {
            dotype[i] = true;
        }
        // Calculate and print the machine dependent constants.
        eps = ge.dlamch('U'); // Underflow threshold
        Preferences.debug("Relative machine underflow is taken to be " + eps + "\n", Preferences.DEBUG_ALGORITHM);
        eps = ge.dlamch('O'); // Overflow threshold
        Preferences.debug("Relative machine overflow is taken to be " + eps + "\n", Preferences.DEBUG_ALGORITHM);
        eps = ge.dlamch('E'); // Epsilon
        Preferences.debug("Relative machine precision is taken to be " + eps + "\n", Preferences.DEBUG_ALGORITHM);
        
        if (tstchk) {
            dchkge(dotype, nm, mval, nn, nval, nnb, nbval, nns,
                   nsval, thresh, nmax, A, AFAC, AINV, B,
                   X, XACT, WORK, rwork, iwork);
        }
        if (tstdrv) {
       
        }
    } // dchkaa
    
    /*
     * This is a port of a portion of LAPACK test routine DCHKGE.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dchkge tests dgetrf, dgetri, dgetrs, dgerfs, and dgecon.
     * 
     * All 4533 tests for dchkge passed.
     * 
     * @param input boolean[] of dimension (ntypes)
     *     The matrix types to be used for testing.  Matrices of type j
           (for 1 <= j <= ntypes) are used for testing if dotype[j] =
           true; if dotype[j] = false, then type j is not used.
       @param input int nm
           The number of values of m contained in the vector mval.
       @param input int[] mval of dimension (nm)
           The values of the matrix row dimension m.
       @param input int nn
           The number of values of n contained in the vector nval.
       @param input int[] nval of dimension (nn)
           The values of the matrix column dimension n.
       @param input int nnb
           The number of values of nb contained in the vector nbval.
       @param input int[] nbval of dimension (nnb)
           The values of the blocksize nb.
       @param input int nns
           The number of values of nrhs contained in the vector nsval.
       @param input double thresh
           The threshold value for the test ratios.  A result is
           included in the output file if result >= thresh.  To have
           every test ratio printed, use thresh = 0.
       @param input int nmax
           The maximum value permitted for m or n, used in dimensioning
           the work arrays.
       @param output double[][] A of dimension (nmax, nmax)
       @param output double[][] AFAC of dimension (nmax, nmax)
       @param output double[][] AINV of dimension (nmax, nmax)
       @param output double[][] B of dimension (nmax, nsmax)
           where nsmax is the largest entry in nsval.
       @param output double[][] X of dimension (nmax, nsmax)
       @param output double[][] XACT of dimension (nmax, nsmax)
       @param output double[][] WORK of dimension (nmax, max(3, nsmax))
       @param output double[] rwork of dimension (max(2*nmax, 2*nsmax+nwork))
       @param output int[] iwork of dimension (2*nmax)
     */
    private void dchkge(boolean[] dotype, int nm, int[] mval, int nn, int[] nval, int nnb,
                        int[] nbval, int nns, int[] nsval, double thresh, int nmax, double[][] A,
                        double[][] AFAC, double[][] AINV, double[][] B, double[][] X,
                        double[][] XACT, double[][] WORK, double[] rwork, int[] iwork) {
        final int ntypes = 11;
        final int ntests = 8;
        final int ntran = 3;
        boolean trfcon;
        boolean zerot;
        char dist[] = new char[1];
        char norm;
        char trans;
        char type[] = new char[1];
        char transs[] = new char[]{'N', 'T', 'C'};
        String path;
        int i;
        int j;
        int im;
        int imat;
        int in;
        int inb;
        int info[] = new int[1];
        int ioff;
        int irhs;
        int itran;
        int izero;
        int k;
        int kL[] = new int[1];
        int ku[] = new int[1];
        int lda;
        int lwork;
        int m;
        int mode[] = new int[1];
        int n;
        int nb;
        int nerrs;
        int nfail;
        int nimat;
        int nrhs;
        int nrun;
        int nt;
        int iseed[] = new int[4];
        int iseedy[] = new int[]{1988, 1989, 1990, 1991};
        int itot;
        int irow;
        int icol;
        int iwork2[];
        double ainvnm;
        double anorm[] = new double[1];
        double anormi;
        double anormo;
        double cndnum[] = new double[1];
        double rcond[] = new double[1];
        double rcondc;
        double rcondi;
        double rcondo[] = new double[1];
        double result[] = new double[ntests];
        double arr[][];
        double workspace[];
        double res[] = new double[2];
        double rwork2[];
        double vec[];
        //String srnamt;
        boolean do60 = true;
        
        // Initialize constants and the random number seed.
        
        path = new String("DGE");
        nrun = 0;
        nfail = 0;
        nerrs = 0;
        for (i = 0; i < 4; i++) {
            iseed[i] = iseedy[i];
        }
        
        iparms = new int[2];
        xlaenv(1, 1);
        xlaenv(2, 2);
    
        // Do for each value of m in mval
    
        for (im = 1; im <= nm; im++) {
            m = mval[im-1];
            lda = Math.max( 1, m);
    
            // Do for each value of n in nval
    
            for (in = 1; in <= nn; in++) {
                n = nval[in-1];
               
                nimat = ntypes;
                if (m <= 0 || n <= 0) {
                    nimat = 1;
                }
    
                for (imat = 1; imat <= nimat; imat++) {
    
                    // Do the tests only if DOTYPE( IMAT ) is true.
    
                    if (!dotype[imat-1]) {
                        continue;
                    }
    
                   // Skip types 5, 6, or 7 if the matrix size is too small.
    
                   zerot = imat >= 5 && imat <= 7;
                   if (zerot && n < imat-4) {
                       continue;
                   }
       
                   // Set up parameters with dlatb4 and generate a test matrix
                   // with dlatms.
    
                   ge.dlatb4(path, imat, m, n, type, kL, ku, anorm, mode,
                             cndnum, dist);
    
                   workspace = new double[3*Math.max(m, n)];
                   ge.dlatms(m, n, dist[0], iseed, type[0], rwork, mode[0],
                             cndnum[0], anorm[0], kL[0], ku[0], 'N', A, lda,
                             workspace, info);
    
                   // Check error code from dlatms.
    
                   if (info[0] != 0) {
                       // Print the header if this is the first error message.
                       if (nfail == 0 && nerrs == 0) {
                           printHeader();
                       } // if (nfail == 0 && nerrs == 0)
                       nerrs++;
                       Preferences.debug("Error from dlatms info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                       Preferences.debug("m = " + m + "\n", Preferences.DEBUG_ALGORITHM);
                       Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                       Preferences.debug("kL[0] = " + kL[0] + "\n", Preferences.DEBUG_ALGORITHM);
                       Preferences.debug("ku[0] = " + ku[0] + "\n", Preferences.DEBUG_ALGORITHM);
                       Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                       continue;
                   } // if (info[0] != 0)
    
                   // For types 5-7, zero one or more columns of the matrix to
                   // test that info[0] is returned correctly.
    
                   if (zerot) {
                       if (imat == 5) {
                           izero = 1;
                       }
                       else if (imat == 6) {
                           izero = Math.min(m, n);
                       }
                       else {
                           izero = Math.min(m, n) / 2 + 1;
                       }
                       ioff = (izero-1)*lda;
                       if (imat < 7) {
                           for (i = 1; i <= m; i++) {
                               itot = ioff + i - 1;
                               irow = itot % lda;
                               icol = itot / lda;
                               A[irow][icol] = 0.0;
                           } // for (i = 1; i <= m; i++0
                       } // if (imat < 7)
                       else {
                           irow = ioff % lda;
                           icol = ioff / lda;
                           arr = new double[m][n-izero+1];
                           for (i = 0; i < m; i++) {
                               for (j = 0; j < n-izero+1; j++) {
                                   arr[i][j] = A[irow+i][icol+j];
                               }
                           }
                           ge.dlaset('F', m, n-izero+1, 0.0, 0.0, arr, lda);
                         for (i = 0; i < m; i++) {
                             for (j = 0; j < n-izero+1; j++) {
                                 A[irow+i][icol+j] = arr[i][j];
                             }
                         }
                       } // else
                   } // if (zerot)
                   else {
                       izero = 0;
                   }
    
                   // These lines, if used in place of the calls in the DO 60
                   // loop, cause the code to bomb on a Sun SPARCstation.
    
                    // ANORMO = DLANGE( 'O', M, N, A, LDA, RWORK )
                    // ANORMI = DLANGE( 'I', M, N, A, LDA, RWORK )
     
                    // Do for each blocksize in nbval
    
                    for (inb = 1; inb <= nnb; inb++) {
                        nb = nbval[inb-1];
                        xlaenv(1, nb);
    
                        // Compute the LU factorization of the matrix.
    
                        ge.dlacpy('F', m, n, A, lda, AFAC, lda);
                        dgetrf(m, n, AFAC, lda, iwork, info);
    
                        // Check error code from dgetrf.
    
                        if (info[0] != izero) {
                         // Print the header if this is the first error message.
                            if (nfail == 0 && nerrs == 0) {
                                printHeader();
                            } // if (nfail == 0 && nerrs == 0)
                            nerrs++;
                            if (izero != 0) {
                                Preferences.debug("dgetrf returned with info[0] = " + info[0] + "instad of " +
                                izero + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("m = " + m + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nb = " + nb + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);    
                            }
                            else {
                                Preferences.debug("dgetrf returned with info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("m = " + m + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nb = " + nb + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);        
                            }
                            if (info[0] != 0) {
                                Preferences.debug("Doing only the condition estimate for this case\n", Preferences.DEBUG_ALGORITHM);
                            }
                        } // if (info[0] != izero)
                        trfcon = false;
    
                        // TEST 1
                        // Reconstruct matrix from factors and compute residual.
    
                        ge.dlacpy('F', m, n, AFAC, lda, AINV, lda);
                        dget01(m, n, A, lda, AINV, lda, iwork, rwork, result);
                        nt = 1;
    
                        // TEST 2
                        // Form the inverse if the factorization was successful
                        // and compute the residual.
    
                        if (m == n && info[0] == 0) {
                            ge.dlacpy('F', n, n, AFAC, lda, AINV, lda);
                            nrhs = nsval[0];
                            lwork = nmax*Math.max(3, nrhs);
                            workspace = new double[lwork];
                            dgetri(n, AINV, lda, iwork, workspace, lwork, info);
    
                            // Check error code from dgetri.
    
                            if (info[0] != 0) {
                                // Print the header if this is the first error message.
                                if (nfail == 0 && nerrs == 0) {
                                    printHeader();
                                } // if (nfail == 0 && nerrs == 0)
                                nerrs++;
                                Preferences.debug("Error from dgetri info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("m = " + m + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nb = " + nb + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                            } // if (info[0] != 0)
    
                            // Compute the residual for the matrix times its
                            // inverse.  Also compute the 1-norm condition number
                            // of A.
    
                            dget03(n, A, lda, AINV, lda, WORK, lda,
                                   rwork, rcondo, res);
                            result[1] = res[0];
                            anormo = ge.dlange('O', m, n, A, lda, rwork);
    
                            // Compute the infinity-norm condition number of A.
    
                           anormi = ge.dlange('I', m, n, A, lda, rwork);
                           ainvnm = ge.dlange('I', n, n, AINV, lda, rwork);
                           if (anormi <= 0.0 || ainvnm <= 0.0) {
                               rcondi = 1.0;
                           }
                           else {
                               rcondi = ( 1.0 / anormi ) / ainvnm;
                           }
                           nt = 2;
                        } // if (m == n && info[0] == 0)
                        else {
    
                            // Do only the condition estimate if info[0] > 0.
    
                            trfcon = true;
                            anormo = ge.dlange('O', m, n, A, lda, rwork);
                            anormi = ge.dlange('I', m, n, A, lda, rwork);
                            rcondo[0] = 0.0;
                            rcondi = 0.0;
                        } // else
    
                        // Print information about the tests so far that did not
                        // pass the threshold.
     
                        for (k = 0; k < nt; k++) {
                            if (result[k] >= thresh) {
                                if (nfail == 0 && nerrs == 0) {
                                    printHeader();
                                } // if (nfail == 0 && nerrs == 0)
                                Preferences.debug("m = " + m + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nb = " + nb + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("test("+(k+1) + ") = " + result[k] + "\n", Preferences.DEBUG_ALGORITHM);
                                nfail++;
                            } // if (result[k] >= thresh)
                        } // for (k = 0; k < nt; k++)
                        nrun = nrun + nt;
    
                        // Skip the remaining tests if this is not the first
                        // block size or if m .ne. n.  Skip the solve tests if
                        // the matrix is singular.
    
                        if (inb > 1 || m != n) {
                            continue;
                        }
                        do60 = true;
                        if (trfcon) {
                            do60 = false;
                        }
    
                        if (do60) {
                            for (irhs = 1; irhs <= nns; irhs++) {
                                nrhs = nsval[irhs-1];
    
                                for (itran = 1; itran <= ntran; itran++) {
                                    trans = transs[itran-1];
                                    if (itran == 1) {
                                        rcondc = rcondo[0];
                                    }
                                    else {
                                        rcondc = rcondi;
                                    }
    
                                    // TEST 3
                                    // Solve and compute residual for A * X = B.
    
                                    // Initialize XACT to nrhs random vectors
                                    vec = new double[n];
                                    for (j = 0; j < nrhs; j++) {
                                        ge.dlarnv(2, iseed, n, vec);
                                        for (i = 0; i < n; i++) {
                                            XACT[i][j] = vec[i];
                                        }
                                    }
                                    // Multiply XACT by op( A ) using an appropriate
                                    // matrix multiply routine.
                                    
                                    ge.dgemm(trans, 'N', n, nrhs, n, 1.0, A, lda, XACT, lda, 0.0, B, lda);
    
                                    ge.dlacpy('F', n, nrhs, B, lda, X, lda);
                                    dgetrs(trans, n, nrhs, AFAC, lda, iwork, X, lda, info);
    
                                    // Check error code from dgetrs.
    
                                    if (info[0] != 0) {
                                        // Print the header if this is the first error message.
                                        if (nfail == 0 && nerrs == 0) {
                                            printHeader();
                                        } // if (nfail == 0 && nerrs == 0)
                                        nerrs++;
                                        Preferences.debug("Error from dgetrs info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("trans = " + trans + "\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("nrhs = " + nrhs + "\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                    } // if (info[0] != 0)
    
                                    ge.dlacpy('F', n, nrhs, B, lda, WORK, lda);
                                    ge.dget02(trans, n, n, nrhs, A, lda, X, lda,
                                              WORK, lda, rwork, res);
                                    result[2] = res[0];
    
                                    // TEST 4
                                    // Check solution from generated exact solution.
    
                                    le.dget04(n, nrhs, X, lda, XACT, lda, rcondc, res);
                                    result[3] = res[0];
    
                                    // TESTS 5, 6, and 7
                                    // Use iterative refinement to improve the solution.
    
                                    rwork2 = new double[nrhs];
                                    workspace = new double[n];
                                    iwork2 = new int[n];
                                    dgerfs(trans, n, nrhs, A, lda, AFAC, lda,
                                           iwork, B, lda, X, lda, rwork,
                                           rwork2, workspace, iwork2, info);
    
                                    // Check error code from dgerfs.
    
                                    if (info[0] != 0) {
                                        // Print the header if this is the first error message.
                                        if (nfail == 0 && nerrs == 0) {
                                            printHeader();
                                        } // if (nfail == 0 && nerrs == 0)
                                        nerrs++;
                                        Preferences.debug("Error from dgerfs info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("trans = " + trans + "\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("nrhs = " + nrhs + "\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                    } // if (info[0] != 0)
    
                                    le.dget04(n, nrhs, X, lda, XACT, lda, rcondc, res);
                                    result[4] = res[0];
                                    dget07(trans, n, nrhs, A, lda, B, lda, X,
                                           lda, XACT, lda, rwork, true,
                                           rwork2, res);
                                    result[5] = res[0];
                                    result[6] = res[1];
    
                                    // Print information about the tests that did not
                                    // pass the threshold.
                                    
                                    for (k = 2; k < 7; k++) {
                                        if (result[k] >= thresh) {
                                            if (nfail == 0 && nerrs == 0) {
                                                printHeader();
                                            } // if (nfail == 0 && nerrs == 0)
                                            Preferences.debug("trans = " + trans + "\n", Preferences.DEBUG_ALGORITHM);
                                            Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                            Preferences.debug("nrhs = " + nrhs + "\n", Preferences.DEBUG_ALGORITHM);
                                            Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                            Preferences.debug("test("+(k+1) + ") = " + result[k] + "\n", Preferences.DEBUG_ALGORITHM);
                                            nfail++;
                                        } // if (result[k] >= thresh)
                                    } // for (k = 2; k < 7; k++)
                                    nrun = nrun + 5;
                                } // for (itran = 1; itran <= ntran; itran++)
                            } // for (irhs = 1; irhs <= nns; irhs++)
                        } // if (do60)
    
                        // TEST 8
                        // Get an estimate of rcond = 1/cndnum[0].
    
    
                        for (itran = 1; itran <= 2; itran++) {
                            if (itran == 1) {
                                anorm[0] = anormo;
                                rcondc = rcondo[0];
                                norm = 'O';
                            }
                            else {
                                anorm[0] = anormi;
                                rcondc = rcondi;
                                norm = 'I';
                            }
                            workspace = new double[n];
                            iwork2 = new int[n];
                            dgecon(norm, n, AFAC, lda, anorm[0], rcond,
                                   workspace, iwork2, info);
    
                            // Check error code from dgecon.
    
                            if (info[0] != 0) {
                                // Print the header if this is the first error message.
                                if (nfail == 0 && nerrs == 0) {
                                    printHeader();
                                } // if (nfail == 0 && nerrs == 0)
                                nerrs++;
                                Preferences.debug("Error from dgecon info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("norm = " + norm + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                            } // if (info[0] != 0)
    
                            result[7] = le.dget06(rcond[0], rcondc);
    
                            // Print information about the tests that did not pass
                            // the threshold.
    
                            if (result[7] >= thresh) {
                                if (nfail == 0 && nerrs == 0) {
                                    printHeader();
                                } // if (nfail == 0 && nerrs == 0)
                                Preferences.debug("norm = " + norm + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("test(8) = " + result[7] + "\n", Preferences.DEBUG_ALGORITHM);
                                nfail++;
                            } // if (result[7] >= thresh)
                            nrun++;
                        } // for (itran = 1; itran <= 2; itran++)
                    } // for (inb = 1; inb <= nnb; inb++)
                } // for (imat = 1; imat <= nimat; imat++)
            } // for (in = 1; in <= nn; in++)
        } // for (im = 1; im <= nm; im++)
        
        // Print a summary of results
        if (nfail > 0) {
            Preferences.debug("dchkge: " + nfail + " out of " + nrun + " tests failed with values >= threshold\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("dchkge: " + nfail + " out of " + nrun + " tests failed with values >= threshold\n");
        }
        else {
            Preferences.debug("All " + nrun + " tests for dchkge passed\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("All " + nrun + " tests for dchkge passed\n");
        }
        if (nerrs > 0) {
            Preferences.debug("dchkge: " + nerrs + " error messages recorded\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("dchkge: " + nerrs + " error messages recorded\n");
        }
        
        return;
    } // dchkge
    
    private void printHeader() {
        Preferences.debug("DGE General dense matrices\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
        // GE matrix types
        Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("2. Upper triangular\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("3. Lower triangular\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("4. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("5. First column zero\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("6. Last column zero\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("7. Last n/2 columns zero\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("8. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("9. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("10. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("11. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("1. norm(L * U - A) / ( n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("2. norm(I - A*AINV) / (n * norm(A) * norm(AINV) * eps)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("3. norm(B - A * X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("4. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("5. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps), refined\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("6. norm(X - XACT) / (norm(XACT) * (error bound))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("7. (backward error) / eps\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("8. rcond * condum[0] - 1.0\n", Preferences.DEBUG_ALGORITHM);
    } // printHeader
    
    /*
     * This is a port of a portion of LAPACK test routine DERRGE.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * This code tests the error exits of dgetrf, dgetf2, dgetri, dgetrs, dgerfs, and dgecon
     * derrge correctly found 23 of 23 error exits.
     */
    public void derrge() {
        int i;
        int j;
        int nmax = 4;
        int lw = 3 * nmax;
        double anrm = 0.0;
        double rcond[] = new double[1];
        double A[][] = new double[nmax][nmax];
        double AF[][] = new double[nmax][nmax];
        double B[][] = new double[nmax][nmax];
        double r1[] = new double[nmax];
        double r2[] = new double[nmax];
        double w[] = new double[lw];
        double X[][] = new double[nmax][nmax];
        int info[] = new int[1];
        int ip[] = new int[nmax];
        int iw[] = new int[nmax];
        int npass = 23;
        final int ntotal = 23; 
        
        for (j = 1; j <= nmax; j++) {
            for (i = 1; i <= nmax; i++) {
                A[i-1][j-1] = 1.0/(double)(i+j);
                AF[i-1][j-1] = 1.0/(double)(i+j);
            }
            ip[j-1] = j;
            iw[j-1] = j;
        }
        
        // dgetrf
        dgetrf(-1, 0, A, 1, ip, info);
        if (info[0] != -1) {
            Preferences.debug("dgetrf(-1, 0, A, 1, ip, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);
             npass--;
        }
        
        dgetrf(0, -1, A, 1, ip, info);
        if (info[0] != -2) {
            Preferences.debug("dgetrf(0, -1, A, 1, ip, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);
             npass--;
        }
        
        dgetrf(2, 1, A, 1, ip, info);
        if (info[0] != -4) {
            Preferences.debug("dgetrf(2, 1, A, 1, ip, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -4\n", Preferences.DEBUG_ALGORITHM);
             npass--;
        }
        
        // dgetf2
        dgetf2(-1, 0, A, 1, ip, info);
        if (info[0] != -1) {
            Preferences.debug("dgetf2(-1, 0, A, 1, ip, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);
             npass--;
        }
        
        dgetf2(0, -1, A, 1, ip, info);
        if (info[0] != -2) {
            Preferences.debug("dgetf2(0, -1, A, 1, ip, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);
             npass--;
        }
        
        dgetf2(2, 1, A, 1, ip, info);
        if (info[0] != -4) {
            Preferences.debug("dgetf2(2, 1, A, 1, ip, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -4\n", Preferences.DEBUG_ALGORITHM);
             npass--;
        }
        
        // dgetri
        dgetri(-1, A, 1, ip, w, lw, info);
        if (info[0] != -1) {
            Preferences.debug("dgetri(-1, A, 1, ip, w, lw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgetri(2, A, 1, ip, w, lw, info);
        if (info[0] != -3) {
            Preferences.debug("dgetri(2, A, 1, ip, w, lw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = 3\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        // dgetrs
        dgetrs('/', 0, 0, A, 1, ip, B, 1, info);
        if (info[0] != -1) {
            Preferences.debug("dgetrs('/', 0, 0, A, 1, ip, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgetrs('N', -1, 0, A, 1, ip, B, 1, info);
        if (info[0] != -2) {
            Preferences.debug("dgetrs('N', -1, 0, A, 1, ip, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgetrs('N', 0, -1, A, 1, ip, B, 1, info);
        if (info[0] != -3) {
            Preferences.debug("dgetrs('N', 0, -1, A, 1, ip, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -3\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgetrs('N', 2, 1, A, 1, ip, B, 2, info);
        if (info[0] != -5) {
            Preferences.debug("dgetrs('N', 2, 1, A, 1, ip, B, 2, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -5\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgetrs('N', 2, 1, A, 2, ip, B, 1, info);
        if (info[0] != -8) {
            Preferences.debug("dgetrs('N', 2, 1, A, 2, ip, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -8\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        // dgerfs
        dgerfs('/', 0, 0, A, 1, AF, 1, ip, B, 1, X, 1, r1, r2, w, iw, info);
        if (info[0] != -1) {
            Preferences.debug("dgerfs('/', 0, 0, A, 1, AF, 1, ip, B, 1, X, 1, r1, r2, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgerfs('N', -1, 0, A, 1, AF, 1, ip, B, 1, X, 1, r1, r2, w, iw, info);
        if (info[0] != -2) {
            Preferences.debug("dgerfs('N', -1, 0, A, 1, AF, 1, ip, B, 1, X, 1, r1, r2, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgerfs('N', 0, -1, A, 1, AF, 1, ip, B, 1, X, 1, r1, r2, w, iw, info);
        if (info[0] != -3) {
            Preferences.debug("dgerfs('N', 0, -1, A, 1, AF, 1, ip, B, 1, X, 1, r1, r2, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -3\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgerfs('N', 2, 1, A, 1, AF, 2, ip, B, 2, X, 2, r1, r2, w, iw, info);
        if (info[0] != -5) {
            Preferences.debug("dgerfs('N', 2, 1, A, 1, AF, 2, ip, B, 2, X, 2, r1, r2, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -5\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgerfs('N', 2, 1, A, 2, AF, 1, ip, B, 2, X, 2, r1, r2, w, iw, info);
        if (info[0] != -7) {
            Preferences.debug("dgerfs('N', 2, 1, A, 2, AF, 1, ip, B, 2, X, 2, r1, r2, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -7\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgerfs('N', 2, 1, A, 2, AF, 2, ip, B, 1, X, 2, r1, r2, w, iw, info);
        if (info[0] != -10) {
            Preferences.debug("dgerfs('N', 2, 1, A, 2, AF, 2, ip, B, 1, X, 2, r1, r2, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -10\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgerfs('N', 2, 1, A, 2, AF, 2, ip, B, 2, X, 1, r1, r2, w, iw, info);
        if (info[0] != -12) {
            Preferences.debug("dgerfs('N', 2, 1, A, 2, AF, 2, ip, B, 2, X, 1, r1, r2, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -12\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        // dgecon
        dgecon('/', 0, A, 1, anrm, rcond, w, iw, info);
        if (info[0] != -1) {
            Preferences.debug("dgecon('/', 0, A, 1, anrm, rcond, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgecon('1', -1, A, 1, anrm, rcond, w, iw, info);
        if (info[0] != -2) {
            Preferences.debug("dgecon('1', -1, A, 1, anrm, rcond, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        dgecon('1', 2, A, 1, anrm, rcond, w, iw, info);
        if (info[0] != -4) {
            Preferences.debug("dgecon('1', 2, A, 1, anrm, rcond, w, iw, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -4\n", Preferences.DEBUG_ALGORITHM);    
        }
        
        Preferences.debug("derrge correctly found " + npass + " of " + ntotal + " error exits\n", Preferences.DEBUG_ALGORITHM);
        UI.setDataText("derrge correctly found " + npass + " of " + ntotal + " error exits\n");
        return;
    } // derrge
    
    /*
     * This is a port of a portion of LAPACK test routine DGET01.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dget01 reconstructs a matrix A from its L*U factorization and
       computes the residual
          norm(L*U - A) / ( N * norm(A) * eps ),
       where eps is the machine epsilon.

       @param input int m
           The number of rows of the matrix A.  m >= 0.
       @param input int n
           The number of columns of the matrix A.  n >= 0.
       @param input double[][] A of dimension (lda, n)
           The original m x n matrix A.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,m).
       @param (input/output) double[][] AFAC of dimension (ldafac, n)
           The factored form of the matrix A.  AFAC contains the factors
           L and U from the L*U factorization as computed by dgetrf.
           Overwritten with the reconstructed matrix, and then with the
           difference L*U - A.
       @param input int ldafac
           The leading dimension of the array AFAC.  ldafac >= max(1,m).
       @param input int[] ipiv of dimension (n)
           The pivot indices from dgetrf.
       @param output double[] rwork of dimension (m)
       @param output double[] resid of dimension (1)
           norm(L*U - A) / ( n * norm(A) * eps )
     */
    private void dget01(int m, int n, double[][] A, int lda, double[][] AFAC,
                        int ldafac, int[] ipiv, double[] rwork, double[] resid) {
        int i;
        int j;
        int k;
        double anorm;
        double eps;
        double t;
        double vec[];
        double arr[][];
        double vec2[];
        
        // Quick exit if M = 0 or N = 0.
                
        if (m <= 0 || n <= 0) {
            resid[0] = 0.0;
            return;
        }
    
        // Determine eps and the norm of A.
    
        eps = ge.dlamch('E'); // Epsilon
        anorm = ge.dlange('1', m, n, A, lda, rwork);
   
        // Compute the product L*U and overwrite AFAC with the result.
        // A column at a time of the product is obtained, starting with
        // column n.
    
        for (k = n; k >= 1; k--) {
            if (k > m) {
                vec = new double[m];
                for (i = 0; i < m; i++) {
                    vec[i] = AFAC[i][k-1];
                }
                ge.dtrmv('L', 'N', 'U', m, AFAC, ldafac, vec, 1);
                for (i = 0; i < m; i++) {
                    AFAC[i][k-1] = vec[i];
                }
            } // if (k > m)
            else {
    
                // Compute elements (K+1:M,K)
    
                t = AFAC[k-1][k-1];
                if (k+1 <= m) {
                    for (i = 0; i < m-k; i++) {
                        AFAC[k+i][k-1] = t * AFAC[k+i][k-1];
                    }
                    arr = new double[m-k][k-1];
                    for (i = 0; i < m-k; i++) {
                        for (j = 0; j < k-1; j++) {
                            arr[i][j] = AFAC[k+i][j];
                        }
                    }
                    vec = new double[k-1];
                    for (i = 0; i < k-1; i++) {
                        vec[i] = AFAC[i][k-1];
                    }
                    vec2 = new double[m-k];
                    for (i = 0; i < m-k; i++) {
                        vec2[i] = AFAC[k+i][k-1];
                    }
                    ge.dgemv('N', m-k, k-1, 1.0, arr, ldafac, vec, 1, 1.0, vec2, 1);
                    for (i = 0; i < m-k; i++) {
                        AFAC[k+i][k-1] = vec2[i];
                    }
                } // if (k+1 <= m)
    
                // Compute the (K,K) element
    
                AFAC[k-1][k-1] = t;
                for (i = 0; i < k-1; i++) {
                    AFAC[k-1][k-1] += AFAC[k-1][i] * AFAC[i][k-1];
                }
                
    
                // Compute elements (1:K-1,K)
                vec = new double[k-1];
                for (i = 0; i < k-1; i++) {
                    vec[i] = AFAC[i][k-1];
                }
                ge.dtrmv('L', 'N', 'U', k-1, AFAC, ldafac, vec, 1);
                for (i = 0; i < k-1; i++) {
                    AFAC[i][k-1] = vec[i];
                }
            } // else 
        } // for (k = n; k >= 1; k--)
        dlaswp(n, AFAC, ldafac, 1, Math.min(m, n), ipiv, -1);
    
        // Compute the difference  L*U - A  and store in AFAC.
    
        for (j = 0; j < n; j++) {
            for (i = 0; i < m; i++) {
                AFAC[i][j] = AFAC[i][j] - A[i][j];
            }
        }
    
        // Compute norm( L*U - A ) / ( n * norm(A) * eps )
    
        resid[0] = ge.dlange('1', m, n, AFAC, ldafac, rwork);
    
        if (anorm <= 0.0) {
            if (resid[0] != 0.0) {
                resid[0] = 1.0 / eps;
            }
        }
        else {
            resid[0] = ( ( resid[0] /(double)( n ) ) / anorm ) / eps;
        }
    
        return;

    } // dget01
    
    /*
     * This is a port of a portion of LAPACK test routine DGET03.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dget03 computes the residual for a general matrix times its inverse:
       norm( I - AINV*A ) / ( n * norm(A) * norm(AINV) * eps ),
       where eps is the machine epsilon.

       @param input int n
           The number of rows and columns of the matrix A.  n >= 0.
       @param input double[][] A of dimension (lda, n)
           The original n x n matrix A.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param input double[][] AINV of dimension (ldainv, n)
           The inverse of the matrix A.
       @param input int ldainv
           The leading dimension of the array AINV.  ldainv >= max(1,n).
       @param output double[][] WORK of dimension (ldwork, n)
       @param input int ldwork
           The leading dimension of the array WORK.  ldwork >= max(1,n).
       @param output double[] rwork of dimension (n)
       @param output double[] rcond of dimension (1)
           The reciprocal of the condition number of A, computed as
           ( 1/norm(A) ) / norm(AINV).
       @param output double[] resid of dimension (1)
           norm(I - AINV*A) / ( n * norm(A) * norm(AINV) * eps )
     */
    private void dget03(int n, double[][] A, int lda, double[][] AINV, int ldainv,
                        double[][] WORK, int ldwork, double[] rwork, double[] rcond,
                        double[] resid) {
        int i;
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
    
        eps = ge.dlamch('E');
        anorm = ge.dlange('1', n, n, A, lda, rwork);
        ainvnm = ge.dlange('1', n, n, AINV, ldainv, rwork);
        if (anorm <= 0.0 || ainvnm <= 0.0) {
            rcond[0] = 0.0;
            resid[0] = 1.0 / eps;
            return;
        }
        rcond[0] = ( 1.0 / anorm ) / ainvnm;
    
        // Compute I - A * AINV
    
        ge.dgemm('N', 'N', n, n, n, -1.0, AINV,
                 ldainv, A, lda, 0.0, WORK, ldwork);
        for (i = 0; i < n; i++) {
            WORK[i][i] = 1.0 + WORK[i][i];
        }
    
        // Compute norm(I - AINV*A) / (n * norm(A) * norm(AINV) * eps)
    
        resid[0] = ge.dlange('1', n, n, WORK, ldwork, rwork);
    
        resid[0] = ((resid[0]*rcond[0]) / eps ) / (double)( n );
    
        return;

    } // dget03
    
    /*
     * This is a port of a portion of LAPACK test routine DGET07.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dget07 tests the error bounds from iterative refinement for the
       computed solution to a system of equations op(A)*X = B, where A is a
       general n by n matrix and op(A) = A or A**T, depending on TRANS.

       reslts[0] = test of the error bound
                 = norm(X - XACT) / ( norm(X) * FERR )

       A large value is returned if this ratio is not less than one.

       reslts[1] = residual from the iterative refinement routine
                 = the maximum of BERR / ( (n+1)*EPS + (*) ), where
                   (*) = (n+1)*UNFL / (min_i (abs(op(A))*abs(X) +abs(b))_i )
       
       @param input char trans
           Specifies the form of the system of equations.
           = 'N':  A * X = B     (No transpose)
           = 'T':  A**T * X = B  (Transpose)
           = 'C':  A**H * X = B  (Conjugate transpose = Transpose)
       @param input int n
           The number of rows of the matrices X and XACT.  n >= 0.
       @param input int nrhs
           The number of columns of the matrices X and XACT.  nrhs >= 0
       @param input double[][] A of dimension (lda, n)
           The leading dimension of the array A.  lda >= max(1,n).
       @param input double[][] B of dimension (ldb, nrhs)
           The right hand side vectors for the system of linear
           equations.
       @param input int ldb
           The leading dimension of the array B.  ldb >= max(1,n).
       @param input double[][] X of dimension (ldx, nrhs)
           The computed solution vectors.  Each vector is stored as a
           column of the matrix X.
       @param input int ldx
           The leading dimension of the array X.  ldx >= max(1,n).
       @param input double[][] XACT of dimension (ldxact, nrhs)
           The exact solution vectors.  Each vector is stored as a
           column of the matrix XACT.
       @param input int ldxact
           The leading dimension of the array XACT.  LDXACT >= max(1,n).
       @param input double[] ferr of dimension (nrhs)
           The estimated forward error bounds for each solution vector
           X.  If XTRUE is the true solution, ferr bounds the magnitude
           of the largest entry in (X - XTRUE) divided by the magnitude
           of the largest entry in X.
       @param input boolean chkferr
           Set to true to check ferr, false not to check ferr.
           When the test system is ill-conditioned, the "true"
           solution in XACT may be incorrect.
       @param input double[] berr of dimension (nrhs)
           The componentwise relative backward error of each solution
           vector (i.e., the smallest relative change in any entry of A
           or B that makes X an exact solution).
       @param output double[] reslts of dimension (2)
           The maximum over the NRHS solution vectors of the ratios:
           reslts[0] = norm(X - XACT) / ( norm(X) * ferr )
           reslts[1] = berr / ( (n+1)*eps + (*) )
     */
    private void dget07(char trans, int n, int nrhs, double[][] A, int lda, double[][] B,
                        int ldb, double[][] X, int ldx, double[][] XACT, int ldxact, 
                        double[] ferr, boolean chkferr, double[] berr, double[] reslts) {
        boolean notran;
        int i;
        int imax;
        int j;
        int k;
        double axbi = 0.0;
        double diff;
        double eps;
        double errbnd;
        double ovfl;
        double tmp;
        double unfl;
        double xnorm;
        double maxVal;
        
        // Quick exit if n = 0 or nrhs = 0.
                
        if (n <= 0 || nrhs <= 0) {
            reslts[0] = 0.0;
            reslts[1] = 0.0;
            return;
        }
    
        eps = ge.dlamch('E'); // Epsilon
        unfl = ge.dlamch('S'); // Safe minimum
        ovfl = 1.0 / unfl;
        notran = ((trans == 'N') || (trans == 'n'));
    
        // Test 1:  Compute the maximum of
        //    norm(X - XACT) / ( norm(X) * ferr )
        // over all the vectors X and XACT using the infinity-norm.
    
        errbnd = 0.0;
        if (chkferr) {
            for (j = 0; j < nrhs; j++) {
                imax = 0;
                maxVal = Math.abs(X[0][j]);
                for (i = 1; i < n; i++) {
                    if (Math.abs(X[i][j]) > maxVal) {
                        maxVal = Math.abs(X[i][j]);
                        imax = i;
                    }
                }
                xnorm = Math.max(Math.abs(X[imax][j]), unfl);
                diff = 0.0;
                for (i = 0; i < n; i++) {
                    diff = Math.max(diff, Math.abs(X[i][j]-XACT[i][j]));
                }
    
                if ((xnorm <= 1.0) && (diff > ovfl*xnorm)) {
                    errbnd = 1.0 / eps;
                    continue;
                }
                   
                if (diff / xnorm <= ferr[j]) {
                    errbnd = Math.max(errbnd, (diff / xnorm ) / ferr[j] );
                }
                else {
                    errbnd = 1.0 / eps;
                }
            } // for (j = 0; j < nrhs; j++)
        } // if (chkferr)
        reslts[0] = errbnd;
    
        // Test 2:  Compute the maximum of BERR / ( (n+1)*EPS + (*) ), where
        // (*) = (n+1)*UNFL / (min_i (abs(op(A))*abs(X) +abs(b))_i )
    
        for (k = 0; k < nrhs; k++) {
            for (i = 0; i < n; i++) {
                tmp = Math.abs(B[i][k]);
                if (notran) {
                    for (j = 0; j < n; j++) {
                        tmp = tmp + Math.abs(A[i][j])*Math.abs(X[j][k]);
                    }
                } // if (notran)
                else {
                    for (j = 0; j < n; j++) {
                        tmp = tmp + Math.abs(A[j][i])*Math.abs(X[j][k]);
                    }
                } // else 
                if (i == 0) {
                    axbi = tmp;
                }
                else {
                    axbi = Math.min(axbi, tmp);
                }
            } // for (i = 0; i < n; i++)
            tmp = berr[k] / ((n+1)*eps+(n+1)*unfl /
                   Math.max(axbi, (n+1)*unfl));
            if (k == 0) {
                reslts[1] = tmp;
            }
            else {
                reslts[1] = Math.max(reslts[1], tmp);
            }
        } // for (k = 0; k < nrhs; k++)
    
        return;

    } // dget07
    
    /*
     * This is a port of a portion of LAPACK driver routine DGESV.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgesv computes the solution to system of linear equations A * X = B for GE matrices
     * 
     * dgesv computes the solution to a real system of linear equations
          A * X = B,
       where A is an n-by-n matrix and X and B are n-by-nrhs matrices.

       The LU decomposition with partial pivoting and row interchanges is
       used to factor A as
          A = P * L * U,
       where P is a permutation matrix, L is unit lower triangular, and U is
       upper triangular.  The factored form of A is then used to solve the
       system of equations A * X = B.

       @param input int n
           The number of linear equations, i.e., the order of the
           matrix A.  n >= 0.
       @param input int nrhs
           The number of right hand sides, i.e., the number of columns
           of the matrix B.  nrhs >= 0.
       @param (input/output) double[][] A of dimension (lda,n)
           On entry, the n-by-n coefficient matrix A.
           On exit, the factors L and U from the factorization
           A = P*L*U; the unit diagonal elements of L are not stored.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param output int[] ipiv of dimension (n)
           The pivot indices that define the permutation matrix P;
           row i of the matrix was interchanged with row ipiv[i].
       @param (input/output) double[][] B of dimension (ldb, nrhs)
           On entry, the n-by-nrhs matrix of right hand side matrix B.
           On exit, if info[0] = 0, the n-by-nrhs solution matrix X.
       @param input int ldb
           The leading dimension of the array B.  ldb >= max(1,n).
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
           > 0:  if info[0] = i, U[i-1][i-1] is exactly zero.  The factorization
                 has been completed, but the factor U is exactly
                 singular, so the solution could not be computed.
     */
    public void dgesv(int n, int nrhs, double[][] A, int lda, int ipiv[], double[][] B,
                      int ldb, int info[]) {
        // Test the input parameters.
        
        info[0] = 0;
        if (n < 0) {
            info[0] = -1;
        }
        else if (nrhs < 0) {
            info[0] = -2;
        }
        else if (lda < Math.max(1, n)) {
            info[0] = -4;
        }
        else if (ldb < Math.max(1,n)) {
            info[0] = -7;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgesv had info[0] = " + info[0]);
            return;
        }
    
        // Compute the LU factorization of A.
    
        dgetrf(n, n, A, lda, ipiv, info);
        if (info[0] == 0) {
    
            // Solve the system A*X = B, overwriting B with X.
    
            dgetrs('N', n, nrhs, A, lda, ipiv, B, ldb, info);
        } // if (info[0] == 0)
        return;
    
    } // dgesv
    
    /*
     * This is a port of a portion of LAPACK routine DGECON.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgecon estimates the reciprocal of the condition number of a general
       real matrix A, in either the 1-norm or the infinity-norm, using
       the LU factorization computed by dgetrf.
*
*      An estimate is obtained for norm(inv(A)), and the reciprocal of the
       condition number is computed as
       rcond[0] = 1 / ( norm(A) * norm(inv(A)) ).

       @param input char norm
           Specifies whether the 1-norm condition number or the
           infinity-norm condition number is required:
           = '1' or 'O':  1-norm;
           = 'I':         Infinity-norm.
       @param input int n
           The order of the matrix A.  n >= 0.
       @param input double[][] A of dimension (lda, n)
           The factors L and U from the factorization A = P*L*U
           as computed by dgetrf.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param input double anorm
           If norm = '1' or 'O', the 1-norm of the original matrix A.
           If norm = 'I', the infinity-norm of the original matrix A.
       @param output double[] rcond of dimension (1)
           The reciprocal of the condition number of the matrix A,
           computed as rcond[0] = 1/(norm(A) * norm(inv(A))).
       @param output double[] work of dimension (n)
       @param output int[] iwork of dimension (n)
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
     */
    private void dgecon(char norm, int n, double[][] A, int lda, double anorm,
                        double[] rcond, double[] work, int[] iwork, int[] info) {
        boolean onenrm;
        char normin;
        int ix;
        int kase[] = new int[1];
        int kase1;
        int isave[] = new int[3];
        double ainvnm[] = new double[1];
        double scale;
        double sl[] = new double[1];
        double smlnum;
        double su[] = new double[1];
        double work2[];
        double work3[];
        double work4[];
        int i;
        double maxVal;
        
        // Test the input parameters.
        
        info[0] = 0;
        onenrm = norm == '1' || ((norm == 'O') || (norm == 'o'));
        if (!onenrm && !((norm == 'I') || (norm == 'i'))) {
            info[0] = -1;
        }
        else if (n < 0) {
            info[0] = -2;
        }
        else if (lda < Math.max(1, n)) {
            info[0] = -4;
        }
        else if (anorm < 0.0) {
            info[0] = -5;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgecon had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        rcond[0] = 0.0;
        if (n == 0) {
            rcond[0] = 1.0;
            return;
        }
        else if (anorm == 0.0) {
            return;
        }
    
        smlnum = ge.dlamch('S'); // Safe minimum
        work2 = new double[n];
        work3 = new double[n];
        work4 = new double[n];
    
        // Estimate the norm of inv(A).
    
        ainvnm[0] = 0.0;
        normin = 'N';
        if (onenrm) {
            kase1 = 1;
        }
        else {
            kase1 = 2;
        }
        kase[0] = 0;
        while (true) {
            le.dlacn2(n, work2, work, iwork, ainvnm, kase, isave);
            if (kase[0] != 0) {
                if (kase[0] == kase1) {
    
                    // Multiply by inv(L).
    
                    le.dlatrs('L', 'N', 'U', normin, n, A,
                              lda, work, sl, work3, info);
    
                    // Multiply by inv(U).
    
                    le.dlatrs('U', 'N', 'N', normin, n,
                              A, lda, work, su, work4, info);
                } // if (kase[0] == kase1)
                else {
    
                    // Multiply by inv(U**T).
    
                    le.dlatrs('U', 'T', 'N', normin, n, A,
                              lda, work, su, work4, info);
    
                    // Multiply by inv(L**T).
    
                    le.dlatrs('L', 'T', 'U', normin, n, A,
                              lda, work, sl, work3, info);
                } // else
    
                // Divide X by 1/(sl[0]*su[0]) if doing so will not cause overflow.
    
                scale = sl[0]*su[0];
                normin = 'Y';
                if (scale != 1.0) {
                    ix = 0;
                    maxVal = Math.abs(work[0]);
                    for (i = 1; i < n; i++) {
                        if (Math.abs(work[i]) > maxVal) {
                            ix = i;
                            maxVal = Math.abs(work[i]);
                        }
                    }
                    if (scale < Math.abs(work[ix])*smlnum || scale == 0.0) {
                        return;
                    }
                    gi.drscl(n, scale, work, 1);
                } // if (scale != 1.0)
                continue;
            } // if (kase[0] != 0)
            break;
        } // while (true)
    
        // Compute the estimate of the reciprocal condition number.
    
        if (ainvnm[0] != 0.0) {
            rcond[0] = (1.0 / ainvnm[0] ) / anorm;
        }
    
        return;

    } // dgecon
    
    /*
     * This is a port of a portion of LAPACK routine DGERFS.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgerfs improves the computed solution to a system of linear
       equations and provides error bounds and backward error estimates for
       the solution.

       @param input char trans
           Specifies the form of the system of equations:
           = 'N':  A * X = B     (No transpose)
           = 'T':  A**T * X = B  (Transpose)
           = 'C':  A**H * X = B  (Conjugate transpose = Transpose)
       @param input int n
           The order of the matrix A.  n >= 0.
       @param input int nrhs
           The number of right hand sides, i.e., the number of columns
           of the matrices B and X.  nrhs >= 0.
       @param input double[][] A of dimension (lda, n)
           The original N-by-N matrix A.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param input double[][] AF of dimension (ldaf, n)
           The factors L and U from the factorization A = P*L*U
           as computed by dgetrf.
       @param input int ldaf
           The leading dimension of the array AF.  ldaf >= max(1,n).
       @param input int[] ipiv of dimension (n)
           The pivot indices from dgetrf; for 1<=i<=n, row i of the
           matrix was interchanged with row ipiv[i].
       @param input double[][] B of dimension (ldb, nrhs)
           The right hand side matrix B.
       @param input int ldb
           The leading dimension of the array B.  ldb >= max(1,n).
       @param (input/output) double[][] X of dimension (ldx, nrhs)
           On entry, the solution matrix X, as computed by dgetrs.
           On exit, the improved solution matrix X.
       @param input int ldx
           The leading dimension of the array X.  ldx >= max(1,n).
       @param output double[] ferr of dimension (nrhs)
           The estimated forward error bound for each solution vector
           X(j) (the j-th column of the solution matrix X).
           If XTRUE is the true solution corresponding to X(j), ferr[j]
           is an estimated upper bound for the magnitude of the largest
           element in (X(j) - XTRUE) divided by the magnitude of the
           largest element in X(j).  The estimate is as reliable as
           the estimate for rcond, and is almost always a slight
           overestimate of the true error.
       @param output double[] berr of dimension (nrhs)
           The componentwise relative backward error of each solution
           vector X(j) (i.e., the smallest relative change in
           any element of A or B that makes X(j) an exact solution).
       @param output double[] work of dimension (n)
       @param output int[] iwork of dimension (n)
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
     */
    private void dgerfs(char trans, int n, int nrhs, double[][] A, int lda, double[][] AF,
                        int ldaf, int[] ipiv, double[][] B, int ldb, double[][] X, int ldx,
                        double[] ferr, double[] berr, double[] work, int[] iwork, int[] info) {
        // itmax is the maximum number of steps of iterative refinement.
        final int itmax = 5;
        boolean notran;
        char transt;
        int count;
        int i;
        int j;
        int k;
        int kase[] = new int[1];
        int nz;
        int isave[] = new int[3];
        double eps;
        double lstres;
        double s;
        double safe1;
        double safe2;
        double safmin;
        double xk;
        double work2[];
        double vec[];
        double work3[];
        double arr[][];
        
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
        else if (ldaf < Math.max(1, n)) {
            info[0] = -7;
        }
        else if (ldb < Math.max(1, n)) {
            info[0] = -10;
        }
        else if (ldx < Math.max(1, n)) {
            info[0] = -12;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgerfs had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (n == 0 || nrhs == 0) {
            for (j = 0; j < nrhs; j++) {
                ferr[j] = 0.0;
                berr[j] = 0.0;
            }
            return;
        }
    
        if (notran) {
            transt = 'T';
        }
        else {
            transt = 'N';
        }
    
        // nz = maximum number of nonzero elements in each row of A, plus 1
    
        nz = n + 1;
        eps = ge.dlamch('E'); // Epsilon
        safmin = ge.dlamch('S'); // Safe minimum
        safe1 = nz*safmin;
        safe2 = safe1 / eps;
        work2 = new double[n];
        work3 = new double[n];
        arr = new double[n][1];
    
        // Do for each right hand side
    
        for (j = 0; j < nrhs; j++) {
    
            count = 1;
            lstres = 3.0;
            while (true) {
    
                // Loop until stopping criterion is satisfied.
    
                // Compute residual R = B - op(A) * X,
                // where op(A) = A, A**T, or A**H, depending on trans.
                for (i = 0; i < n; i++) {
                    work2[i] = B[i][j];
                }
                vec = new double[n];
                for (i = 0; i < n; i++) {
                    vec[i] = X[i][j];
                }
                ge.dgemv(trans, n, n, -1.0, A, lda, vec, 1, 1.0, work2, 1);
    
                // Compute componentwise relative backward error from formula
    
                // max(i) ( abs(R(i)) / ( abs(op(A))*abs(X) + abs(B) )(i) )
    
                // where abs(Z) is the componentwise absolute value of the matrix
                // or vector Z.  If the i-th component of the denominator is less
                // than safe2, then safe1 is added to the i-th components of the
                // numerator and denominator before dividing.
    
                for (i = 0; i < n; i++) {
                    work[i] = Math.abs(B[i][j]);
                } 
    
                // Compute abs(op(A))*abs(X) + abs(B).
    
                if (notran) {
                    for (k = 0; k < n; k++) {
                        xk = Math.abs(X[k][j]);
                        for (i = 0; i < n; i++) {
                            work[i] = work[i] + Math.abs(A[i][k])*xk;
                        } // for (i = 0; i < n; i++)
                    } // for (k = 0; k < n; k++)
                } // if (notran)
                else {
                    for (k = 0; k < n; k++) {
                        s = 0.0;
                        for (i = 0; i < n; i++) {
                            s = s + Math.abs(A[i][k])*Math.abs(X[i][j]);
                        } // for (i = 0; i < n; i++)
                        work[k] = work[k] + s;
                    } // for (k = 0; k < n; k++)
                } // else
                s= 0.0;
                for (i = 0; i < n; i++) {
                    if (work[i] > safe2) {
                        s = Math.max(s, Math.abs(work2[i]) / work[i]);
                    }
                    else {
                        s = Math.max(s, (Math.abs(work2[i])+safe1) /(work[i]+safe1));
                    }
                } // for (i = 0; i < n; i++)
                berr[j] = s;
    
                // Test stopping criterion. Continue iterating if
                    // 1) The residual berr[j] is larger than machine epsilon, and
                    // 2) berr[j] decreased by at least a factor of 2 during the
                    //    last iteration, and
                    // 3) At most itmax iterations tried.
    
                if (berr[j] > eps && 2.0*berr[j] <= lstres && count <= itmax) {
    
                    // Update solution and try again.
     
                    for (i = 0; i < n; i++) {
                        arr[i][0] = work2[i];
                    }
                    dgetrs(trans, n, 1, AF, ldaf, ipiv, arr, n, info);
                    for (i = 0; i < n; i++) {
                        work2[i] = arr[i][0];
                        X[i][j] = X[i][j] + work2[i];
                    }
                    lstres = berr[j];
                    count++;
                    continue;
                } // if (berr[j] > eps && 2.0*berr[j] <= lstres && count <= itmax)
                break;
            } // while (true)
    
            // Bound error from formula
    
            // norm(X - XTRUE) / norm(X) .le. ferr =
            // norm( abs(inv(op(A)))*
            //    ( abs(R) + NZ*EPS*( abs(op(A))*abs(X)+abs(B) ))) / norm(X)
    
            // where
            //   norm(Z) is the magnitude of the largest component of Z
            //   inv(op(A)) is the inverse of op(A)
            //   abs(Z) is the componentwise absolute value of the matrix or vector Z
            //   nz is the maximum number of nonzeros in any row of A, plus 1
            //   eps is machine epsilon
    
            // The i-th component of abs(R)+nz*eps*(abs(op(A))*abs(X)+abs(B))
            // is incremented by safe1 if the i-th component of
            // abs(op(A))*abs(X) + abs(B) is less than safe2.
    
            // Use dlacn2 to estimate the infinity-norm of the matrix
            //    inv(op(A)) * diag(W),
            // where W = abs(R) + nz*eps*( abs(op(A))*abs(X)+abs(B) )))
    
            for (i = 0; i < n; i++) {
                if (work[i] > safe2) {
                    work[i] = Math.abs(work2[i]) + nz*eps*work[i];
                }
                else {
                    work[i] = Math.abs(work2[i]) + nz*eps*work[i] + safe1;
                }
            } // for (i = 0; i < n; i++)
    
            kase[0] = 0;
            while (true) {
                vec = new double[1];
                vec[0] = ferr[j];
                le.dlacn2(n, work3, work2, iwork, vec, kase, isave);
                ferr[j] = vec[0];
                if (kase[0] != 0) {
                    if (kase[0] == 1) {
    
                        // Multiply by diag(W)*inv(op(A)**T).
    
                        for (i = 0; i < n; i++) {
                            arr[i][0] = work2[i];
                        }
                        dgetrs(transt, n, 1, AF, ldaf, ipiv, arr, n, info);
                        for (i = 0; i < n; i++) {
                            work2[i] = arr[i][0];
                            work2[i] = work[i]*work2[i];
                        }
                    } // if (kase[0] == 1)
                    else {
    
                        // Multiply by inv(op(A))*diag(W).
    
                        for (i = 0; i < n; i++) {
                            work2[i] = work[i]*work2[i];
                            arr[i][0] = work2[i];
                        }
                        dgetrs(trans, n, 1, AF, ldaf, ipiv, arr, n, info);
                        for (i = 0; i < n; i++) {
                            work2[i] = arr[i][0];
                        }
                    } // else
                    continue;
                } // if (kase[0] != 0)
                break;
            } // while (true)
    
            // Normalize error.
    
            lstres = 0.0;
            for (i = 0; i < n; i++) {
                lstres = Math.max(lstres, Math.abs(X[i][j]));
            }
            if (lstres != 0.0) {
                ferr[j] = ferr[j] / lstres;
            }
    
        } // for (j = 0; j < nrhs; j++)
    
        return;

    } // dgerfs
    
    /*
     * This is a port of a portion of LAPACK routine DGETRF.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgetrf computes an LU factorization of a general m-by-n matrix A
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
       @param (input/output) double[][] A of dimension (lda, n)
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
           < 0: if info[0] = -i, the i-th argument had an illegal value
           > 0: if info[0] = i, U[i-1][i-1] is exactly zero. The factorization
                has been completed, but the factor U is exactly
                singular, and division by zero will occur if it is used
                to solve a system of equations.
     */
    public void dgetrf(int m, int n, double[][] A, int lda, int[] ipiv, int[] info) {
        int i;
        int iinfo[] = new int[1];
        int j;
        int jb;
        int nb;
        String name;
        String opts;
        double arr[][];
        int ivec[];
        int k;
        double arr2[][];
        double arr3[][];
        
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
            MipavUtil.displayError("dgetrf had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (m == 0 || n == 0) {
            return;
        }
    
        // Determine the block size for this environment.
    
        name = new String("DGETRF");
        opts = new String(" ");
        nb = ge.ilaenv( 1, name, opts, m, n, -1, -1);
        if (nb <= 1 || nb >= Math.min(m, n)) {
    
            // Use unblocked code.
    
            dgetf2(m, n, A, lda, ipiv, info);
        } // if (nb <= 1 || nb >= Math.min(m, n))
        else {
    
            // Use blocked code.
    
            for (j = 1; j <= Math.min(m, n); j += nb) {
                jb = Math.min(Math.min(m, n)-j+1, nb);
    
                // Factor diagonal and subdiagonal blocks and test for exact
                // singularity.
    
                arr = new double[m-j+1][jb];
                for (i = 0; i < m-j+1; i++) {
                    for (k = 0; k < jb; k++) {
                        arr[i][k] = A[j-1+i][j-1+k];
                    }
                }
                ivec = new int[Math.min(m-j+1, jb)];
                dgetf2(m-j+1, jb, arr, lda, ivec, iinfo);
                for (i = 0; i < m-j+1; i++) {
                    for (k = 0; k < jb; k++) {
                        A[j-1+i][j-1+k] = arr[i][k];
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
    
                dlaswp(j-1, A, lda, j, j+jb-1, ipiv, 1);
    
                if (j+jb <= n) {
    
                    // Apply interchanges to columns j+jb:n.
    
                    arr = new double[A.length][n-j-jb+1];
                    for (i = 0; i < A.length; i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                            arr[i][k] = A[i][j+jb-1+k];
                        }
                    }
                    dlaswp(n-j-jb+1, arr, lda, j, j+jb-1, ipiv, 1);
                    for (i = 0; i < A.length; i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                            A[i][j+jb-1+k] = arr[i][k];
                        }
                    }
    
                    // Compute block row of U.
                    arr = new double[jb][jb];
                    for (i = 0; i < jb; i++) {
                        for (k = 0; k < jb; k++) {
                            arr[i][k] = A[j-1+i][j-1+k];
                        }
                    }
                    arr2 = new double[Math.max(1,jb)][n-j-jb+1];
                    for (i = 0; i < Math.max(1, jb); i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                            arr2[i][k] = A[j-1+i][j+jb-1+k];
                        }
                    }
                    ge.dtrsm('L', 'L', 'N', 'U', jb,
                             n-j-jb+1, 1.0, arr, lda, arr2,
                             lda);
                   for (i = 0; i < Math.max(1, jb); i++) {
                       for (k = 0; k < n-j-jb+1; k++) {
                           A[j-1+i][j+jb-1+k] = arr2[i][k];
                       }
                   }
                   if (j+jb <= m) {
    
                       // Update trailing submatrix.
                       arr = new double[Math.max(1, m-j-jb+1)][jb];
                       for (i = 0; i < Math.max(1,m-j-jb+1); i++) {
                           for (k = 0; k < jb; k++) {
                               arr[i][k] = A[j+jb-1+i][j-1+k];    
                           }
                       }
                       arr3 = new double[Math.max(1,m-j-jb+1)][n-j-jb+1];
                       for (i = 0; i < Math.max(1, m-j-jb+1); i++) {
                           for (k = 0; k < n-j-jb+1; k++) {
                               arr3[i][k] = A[j+jb-1+i][j+jb-1+k];
                           }
                       }
                       ge.dgemm('N', 'N', m-j-jb+1, n-j-jb+1, jb, -1.0, arr, lda,
                                arr2, lda, 1.0, arr3, lda);
                       for (i = 0; i < Math.max(1, m-j-jb+1); i++) {
                          for (k = 0; k < n-j-jb+1; k++) {
                              A[j+jb-1+i][j+jb-1+k] = arr3[i][k];
                          }
                      }
                   } // if (j+jb <= m)
                } // if (j+jb <= n)
            } // for (j = 1; j <= Math.min(m, n); j += nb)
        } // else use blocked code
        return;

    } // dgetrf
    
    /*
     * This is a port of a portion of LAPACK routine DGETRI.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgetri computes the inverse of a matrix using the LU factorization
       computed by dgetrf.

       This method inverts U and then computes inv(A) by solving the system
       inv(A)*L = inv(U) for inv(A).
       
       @param input int n
           The order of the matrix A.  n >= 0.
       @param (input/output) double[][] A of (lda, n)
           On entry, the factors L and U from the factorization
           A = P*L*U as computed by dgetrf.
           On exit, if info[0] = 0, the inverse of the original matrix A.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param input int[] ipiv of dimension (n)
           The pivot indices from dgetrf; for 1<=i<=n, row i of the
           matrix was interchanged with row ipiv[i].
       @param output double[] work of dimension (max(1, lwork))
           On exit, if info[0] =0, then work[0] returns the optimal lwork.
       @param input int lwork
           The dimension of the array work.  lwork >= max(1,n).
           For optimal performance lwork >= n*nb, where nb is
           the optimal blocksize returned by ilaenv.

           If lwork = -1, then a workspace query is assumed; the routine
           only calculates the optimal size of the work array, returns
           this value as the first entry of the work array, and no error
           message related to lwork is issued.
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
           > 0:  if info[0] = i, U[i-1][i-1] is exactly zero; the matrix is
                 singular and its inverse could not be computed.
     */
    private void dgetri(int n, double[][] A, int lda, int[] ipiv, double[] work,
                        int lwork, int[] info) {
        boolean lquery;
        int i;
        int iws;
        int j;
        int jb;
        int jj;
        int jp;
        int ldwork;
        int lwkopt;
        int nb;
        int nbmin;
        int nn;
        String name;
        String opts;
        double arr[][];
        double vec[];
        double vec2[];
        int k;
        double arr2[][];
        int i1;
        int i2;
        double arr3[][];
        double temp;
        
        // Test the input parameters.
        
        info[0] = 0;
        name = new String("DGETRI");
        opts = new String(" ");
        nb = ge.ilaenv(1, name, opts, n, -1, -1, -1);
        lwkopt = n*nb;
        work[0] = lwkopt;
        lquery = (lwork == -1);
        if (n < 0) {
            info[0] = -1;
        }
        else if (lda < Math.max(1, n)) {
            info[0] = -3;
        }
        else if (lwork < Math.max(1, n) && !lquery) {
            info[0] = -6;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dgetri had info[0] = " + info[0]);
            return;
        }
        else if (lquery) {
            return;
        }
    
        // Quick return if possible
    
        if (n == 0) {
            return;
        }
    
        // Form inv(U).  If INFO[0] > 0 from dtrtri, then U is singular,
        // and the inverse is not computed.
    
        le.dtrtri('U', 'N', n, A, lda, info);
        if (info[0] > 0) {
            return;
        }
    
        nbmin = 2;
        ldwork = n;
        if (nb > 1 && nb < n) {
            iws = Math.max(ldwork*nb, 1);
            if (lwork < iws) {
                nb = lwork / ldwork;
                nbmin = Math.max(2, ge.ilaenv(2, name, opts, n, -1, -1, -1));
            } // if (lwork < iws)
        } // if (nb > 1 && nb < n)
        else {
            iws = n;
        }
    
        // Solve the equation inv(A)*L = inv(U) for inv(A).
    
        if (nb < nbmin || nb >= n) {
    
            // Use unblocked code.
    
            for (j = n; j >= 1; j--) {
    
                // Copy current column of L to WORK and replace with zeros.
    
                for(i = j + 1; i <= n; i++) {
                    work[i-1] = A[i-1][j-1];
                    A[i-1][j-1] = 0.0;
                } // for (i = j + 1; i <= n; i++)
    
                // Compute current column of inv(A).
    
                if (j < n) {
                    arr = new double[n][n-j];
                    for (i = 0; i < n; i++) {
                        for (k = 0; k < n-j; k++) {
                            arr[i][k] = A[i][j+k];
                        }
                    }
                    vec = new double[n-j];
                    for (i = 0; i < n-j; i++) {
                        vec[i] = work[j+i];
                    }
                    vec2 = new double[n];
                    for (i = 0; i < n; i++) {
                        vec2[i] = A[i][j-1];
                    }
                    ge.dgemv('N', n, n-j, -1.0, arr,
                             lda, vec, 1, 1.0, vec2, 1);
                    for (i = 0; i < n; i++) {
                        A[i][j-1] = vec2[i];
                    }
                } // if (j < n)
            } // for (j = n; j >= 1; j--)
        } // if (nb < nbmin || nb >= n)
        else {
    
            // Use blocked code.
    
            nn = ((n-1) / nb )*nb + 1;
            for (j = nn; j >= 1; j -= nb) {
                jb = Math.min(nb, n-j+1);
    
                // Copy current block column of L to WORK and replace with
                // zeros.
    
                for (jj = j; jj <= j + jb - 1; jj++) {
                    for (i = jj + 1; i <= n; i++) {
                        work[i+(jj-j)*ldwork-1] = A[i-1][jj-1];
                        A[i-1][jj-1] = 0.0;
                    } // for (i = jj + 1; i <= n; i++)
                } //  for (jj = j; jj <= j + jb - 1; jj++)
    
                // Compute current block column of inv(A).
    
                if (j+jb <= n) {
                    arr = new double[n][n-j-jb+1];
                    for (i = 0; i < n; i++) {
                        for (k = 0; k < n-j-jb+1; k++) {
                            arr[i][k] = A[i][j+jb-1+k];
                        }
                    }
                    arr2 = new double[n-j-jb+1][jb];
                    i = 0;
                    for (i2 = 0; i2 < jb; i2++) {
                        for (i1 = 0; i1 < n-j-jb+1; i1++) {
                            arr2[i1][i2] = work[j+jb-1+i];
                            i++;
                        }
                    }
                    arr3 = new double[n][jb];
                    for (i = 0; i < n; i++) {
                        for (k = 0; k < jb; k++) {
                            arr3[i][k] = A[i][j-1+k];
                        }
                    }
                    ge.dgemm('N', 'N', n, jb,
                             n-j-jb+1, -1.0, arr, lda,
                             arr2, ldwork, 1.0, arr3, lda);
                    for (i = 0; i < n; i++) {
                        for (k = 0; k < jb; k++) {
                            A[i][j-1+k] = arr3[i][k];
                        }
                    }
                } // if (j+jb <= n)
                arr = new double[jb][jb];
                i = 0;
                for (i2 = 0; i2 < jb; i2++) {
                    for (i1 = 0; i1 < jb; i1++) {
                        arr[i1][i2] = work[j-1+i];
                        i++;
                    }
                }
                arr2 = new double[n][jb];
                for (i = 0; i < n; i++) {
                    for (k = 0; k < jb; k++) {
                        arr2[i][k] = A[i][j-1+k];
                    }
                }
                ge.dtrsm('R', 'L', 'N', 'U', n, jb,
                         1.0, arr, ldwork, arr2, lda);
                for (i = 0; i < n; i++) {
                    for (k = 0; k < jb; k++) {
                        A[i][j-1+k] = arr2[i][k];
                    }
                }
            } // for (j = nn; j >= 1; j -= nb)
        } // else
    
        // Apply column interchanges.
    
        for (j = n - 1; j >= 1; j--) {
            jp = ipiv[j-1];
            if (jp != j) {
                for (i = 0; i < n; i++) {
                    temp = A[i][j-1];
                    A[i][j-1] = A[i][jp-1];
                    A[i][jp-1] = temp;
                }
            } // if (jp != j)
        } // for (j = n - 1; j >= 1; j--)
    
        work[0] = iws;
        return;

    } // dgetri
    
    /*
     * This is a port of a portion of LAPACK routine DGETRS.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * dgetrs solves a system of linear equations
          A * X = B  or  A**T * X = B
       with a general N-by-N matrix A using the LU factorization computed
       by dgetrf.
       
       @param input char trans
           Specifies the form of the system of equations:
           = 'N':  A * X = B  (No transpose)
           = 'T':  A**T* X = B  (Transpose)
           = 'C':  A**T* X = B  (Conjugate transpose = Transpose)
       @param input int n
           The order of the matrix A.  n >= 0.
       @param input int nrhs
           The number of right hand sides, i.e., the number of columns
           of the matrix B.  nrhs >= 0.
       @param input double[][] A of dimension (lda, n)
           The factors L and U from the factorization A = P*L*U
           as computed by dgetrf.
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param input int[] ipiv of dimension (n)
           The pivot indices from dgetrf; for 1<=i<=n, row i of the
           matrix was interchanged with row ipiv[i].
       @param (input/output) double[][] B of dimension (ldb,nrhs)
           On entry, the right hand side matrix B.
           On exit, the solution matrix X.
       @param input int ldb
           The leading dimension of the array B.  ldb >= max(1,n).
       @param output int[] info of dimension (1)
           = 0:  successful exit
           < 0:  if info[0] = -i, the i-th argument had an illegal value
     */
    private void dgetrs(char trans, int n, int nrhs, double[][] A, int lda, int[] ipiv,
                        double[][] B, int ldb, int[] info) {
        boolean notran;
        
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
            MipavUtil.displayError("dgetrs had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (n == 0 || nrhs == 0) {
            return;
        }
    
        if (notran) {
    
            // Solve A * X = B.
    
            // Apply row interchanges to the right hand sides.
    
            dlaswp(nrhs, B, ldb, 1, n, ipiv, 1);
    
            // Solve L*X = B, overwriting B with X.
    
            ge.dtrsm('L', 'L', 'N', 'U', n, nrhs,
                     1.0, A, lda, B, ldb);
    
            // Solve U*X = B, overwriting B with X.
    
            ge.dtrsm('L', 'U', 'N', 'N', n,
                     nrhs, 1.0, A, lda, B, ldb);
        } // if (notran)
        else {
    
            // Solve A**T * X = B.
    
            // Solve U**T *X = B, overwriting B with X.
    
            ge.dtrsm('L', 'U', 'T', 'N', n, nrhs,
                     1.0, A, lda, B, ldb);
    
            // Solve L**T *X = B, overwriting B with X.
    
            ge.dtrsm('L', 'L', 'T', 'U', n, nrhs, 1.0,
                     A, lda, B, ldb);
    
            // Apply row interchanges to the solution vectors.
    
            dlaswp(nrhs, B, ldb, 1, n, ipiv, -1);
        } // else
    
        return;

    } // dgetrs
    
    /*
     * This is a port of a portion of LAPACK routine DGETF2.f version 3.4.2
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., September, 2012
     * 
     * dget2 computes the LU factorization of a general m-by-n matrix using partial pivoting with row
     * interchanges (unblocked algorithm).
     * 
     * dgetf2 computes an LU factorization of a general m-by-n matrix A
       using partial pivoting with row interchanges.

       The factorization has the form
          A = P * L * U
       where P is a permutation matrix, L is lower triangular with unit
       diagonal elements (lower trapezoidal if m > n), and U is upper
       triangular (upper trapezoidal if m < n).

       This is the right-looking Level 2 BLAS version of the algorithm.

       @param input int m
           The number of rows of the matrix A.  m >= 0.
       @param input int n
           The number of columns of the matrix A.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
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
    private void dgetf2(int m, int n, double[][] A, int lda, int[] ipiv, int[] info) {
        double sfmin;
        int i;
        int j;
        int jp;
        int index;
        double maxVal;
        double temp;
        double vec[];
        double vec2[];
        double arr[][];
        int k;
        
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
            MipavUtil.displayError("dgetf2 had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (m == 0 || n == 0) {
            return;
        }
    
        // Compute machine safe minimum 
    
        sfmin = ge.dlamch('S');  
    
        for (j = 1; j <= Math.min(m, n); j++) {
    
            // Find pivot and test for singularity.
             index = 1;
             maxVal = Math.abs(A[j-1][j-1]);
             for (k = j+1; k <= m; k++) {
                 if (Math.abs(A[k-1][j-1]) > maxVal) {
                     maxVal = Math.abs(A[k-1][j-1]);
                     index = k-(j-1);
                 }
             }
             jp = j - 1 + index;
             ipiv[j-1] = jp;
             if (A[jp-1][j-1] != 0.0) {
    
                 // Apply the interchange to columns 1:n.
    
                 if (jp != j) {
                   for (i = 0; i < n; i++) {
                       temp = A[j-1][i];
                       A[j-1][i] = A[jp-1][i];
                       A[jp-1][i] = temp;
                   }
                 }
    
                 // Compute elements j+1:m of j-th column.
    
                if (j < m) { 
                    if (Math.abs(A[j-1][j-1]) >= sfmin) {
                        for (i = 0; i < m-j; i++) {
                            A[j+i][j-1] = (1.0/A[j-1][j-1]) * A[j+i][j-1];
                        }
                    }
                    else { 
                     for (i = 1; i <= m-j; i++) {
                        A[j+i-1][j-1] = A[j+i-1][j-1] / A[j-1][j-1]; 
                     } // for (i = 1; i <= m-j; i++)
                    } // else
                } // if (j < m)
             } // if (A[jp-1][j-1] != 0.0)
             else if (info[0] == 0) {
    
                 info[0] = j;
             } // else if (info[0] == 0)
    
             if (j < Math.min(m, n)) {
    
                 // Update trailing submatrix.
                 vec = new double[m-j];
                 for (i = 0; i < m-j; i++) {
                     vec[i] = A[j+i][j-1];
                 }
                 vec2 = new double[n-j];
                 for (i = 0; i < n-j; i++) {
                     vec2[i] = A[j-1][j+i];
                 }
                 arr = new double[m-j][n-j];
                 for (i = 0; i < m-j; i++) {
                     for (k = 0; k < n-j; k++) {
                         arr[i][k] = A[j+i][j+k];
                     }
                 }
                 ge.dger(m-j, n-j, -1.0, vec, 1, vec2, 1, arr, lda);
                 for (i = 0; i < m-j; i++) {
                     for (k = 0; k < n-j; k++) {
                         A[j+i][j+k] = arr[i][k];
                     }
                 }
             } // if (j < Math.min(m, n))
        } // for (j = 1; j <= Math.min(m, n); j++)
        return;

    } // dgetf2
    
    /*
     * This is a port of a portion of LAPACK auxiliary routine DLASWP.f version 3.4.2
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., September, 2012
     * Modified by
       R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA

     * 
     * dlaswp performs a series of row interchanges on a general rectangular matrix.
     * 
     * dlaswp performs a series of row interchanges on the matrix A.
       One row interchange is initiated for each of rows K1 through K2 of A.

       @param input int n
           The number of columns of the matrix A.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the matrix of column dimension N to which the row
           interchanges will be applied.
           On exit, the permuted matrix.
       @param input int lda
           The leading dimension of the array A.
       @param input int k1
           The first element of ipiv for which a row interchange will
           be done.
       @param input int k2
           The last element of IPIV for which a row interchange will
           be done.
       @param input int[] ipiv of dimension (k2 * abs(incx))
           The vector of pivot indices.  Only the elements in positions
           k1 through k2 of ipiv are accessed.
           ipiv[k] = L implies rows k and L are to be interchanged
       @param input int incx
           The increment between successive values of IPIV.  If IPIV
           is negative, the pivots are applied in reverse order.
     */
    private void dlaswp(int n, double[][] A, int lda, int k1, int k2, int[] ipiv, int incx) {
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
        double temp;
        
        // Interchange row i with row ipiv[i] for each of rows k1 through k2.
        
        if (incx > 0) {
           ix0 = k1;
           i1 = k1;
           i2 = k2;
           inc = 1;
        }
        else if (incx < 0) {
           ix0 = 1 + (1-k2)*incx;
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
                                temp = A[i-1][k-1];
                                A[i-1][k-1] = A[ip-1][k-1];
                                A[ip-1][k-1] = temp;
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
                                temp = A[i-1][k-1];
                                A[i-1][k-1] = A[ip-1][k-1];
                                A[ip-1][k-1] = temp;
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
                            temp = A[i-1][k-1];
                            A[i-1][k-1] = A[ip-1][k-1];
                            A[ip-1][k-1] = temp;
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
                            temp = A[i-1][k-1];
                            A[i-1][k-1] = A[ip-1][k-1];
                            A[ip-1][k-1] = temp;
                        } // for (k = n32; k <= n; k++)
                    } // if (ip != i)
                    ix = ix + incx;    
                } // for (i = i1; i >= i2; i--)
            } // else inc == -1
        } // if (n32 != n)
     
        return;

    } // dlaswp
    
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