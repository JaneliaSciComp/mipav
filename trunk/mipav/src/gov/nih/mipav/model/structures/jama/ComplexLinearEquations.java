package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class ComplexLinearEquations implements java.io.Serializable {
	GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    private ViewUserInterface UI = ViewUserInterface.getReference();
    
    private int iparms[];
    
    /** Found in routine zlatb4 */
    private boolean first_zlatb4 = true;

    private double eps_zlatb4;

    private final double small_zlatb4[] = new double[1];

    private final double large_zlatb4[] = new double[1];

    private double badc1_zlatb4;

    private double badc2_zlatb4;
    
    /** Double precison machine variables found in routine dlartg. */
    private boolean first_zlartg = true;
    
    /** DOCUMENT ME! */
    private double safmin;

    /** DOCUMENT ME! */
    private double safmn2;

    /** DOCUMENT ME! */
    private double safmx2;

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
     * This is a port of a portion of LAPACK test routine ZCHKGE.f version 3.7.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., December, 2016
     * 
     * zchkge tests zgetrf, zgetri, zgetrs, zgerfs, and zgecon.
     * 
     *? tests for zchkge passed.
     * 
     * @param input boolean[] dotype of dimension (ntypes)
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
       @param output double[][][2] complex A of dimension (nmax, nmax)
       @param output double[][][2] complex AFAC of dimension (nmax, nmax)
       @param output double[][][2] complex AINV of dimension (nmax, nmax)
       @param output double[][][2] complex B of dimension (nmax, nsmax)
           where nsmax is the largest entry in nsval.
       @param output double[][][2] complex X of dimension (nmax, nsmax)
       @param output double[][][2] complex XACT of dimension (nmax, nsmax)
       @param output double[][][2] complex WORK of dimension (nmax, max(3, nsmax))
       @param output double[] rwork of dimension (max(2*nmax, 2*nsmax+nwork))
       @param output int[] iwork of dimension (2*nmax)
     */
    /*private void zchkge(boolean[] dotype, int nm, int[] mval, int nn, int[] nval, int nnb,
                        int[] nbval, int nns, int[] nsval, double thresh, int nmax, double[][][] A,
                        double[][][] AFAC, double[][][] AINV, double[][][] B, double[][][] X,
                        double[][][] XACT, double[][][] WORK, double[] rwork, int[] iwork) {
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
        double arr[][][];
        double workspace[][];
        double res[] = new double[2];
        double rwork2[];
        double vec[];
        //String srnamt;
        boolean do60 = true;
        char xtype;
        double alpha[] = new double[2];
        double beta[] = new double[2];
        
        // Initialize constants and the random number seed.
        
        path = new String("ZGE");
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
                xtype = 'N';
               
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
       
                   // Set up parameters with zlatb4 and generate a test matrix
                   // with dlatms.
    
                   zlatb4(path, imat, m, n, type, kL, ku, anorm, mode,
                             cndnum, dist);
    
                   workspace = new double[3*Math.max(m, n)][2];
                   zlatms(m, n, dist[0], iseed, type[0], rwork, mode[0],
                             cndnum[0], anorm[0], kL[0], ku[0], 'N', A, lda,
                             workspace, info);
    
                   // Check error code from zlatms.
    
                   if (info[0] != 0) {
                       // Print the header if this is the first error message.
                       if (nfail == 0 && nerrs == 0) {
                           printHeader();
                       } // if (nfail == 0 && nerrs == 0)
                       nerrs++;
                       Preferences.debug("Error from zlatms info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
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
                               A[irow][icol][0] = 0.0;
                               A[irow][icol][1 ] = 0.0;
                           } // for (i = 1; i <= m; i++)
                       } // if (imat < 7)
                       else {
                           irow = ioff % lda;
                           icol = ioff / lda;
                           arr = new double[m][n-izero+1][2];
                           for (i = 0; i < m; i++) {
                               for (j = 0; j < n-izero+1; j++) {
                                   arr[i][j][0] = A[irow+i][icol+j][0];
                                   arr[i][j][1] = A[irow+i][icol+j][1];
                               }
                           }
                           alpha[0] = 0.0;
                           alpha[1] = 0.0;
                           beta[0] = 0.0;
                           beta[1] = 0.0;
                           zlaset('F', m, n-izero+1, alpha, beta, arr, lda);
                           for (i = 0; i < m; i++) {
                             for (j = 0; j < n-izero+1; j++) {
                                 A[irow+i][icol+j][0] = arr[i][j][0];
                                 A[irow+i][icol+j][1] = arr[i][j][1];
                             }
                         }
                       } // else
                   } // if (zerot)
                   else {
                       izero = 0;
                   }
    
                   // These lines, if used in place of the calls in the DO 60
                   // loop, cause the code to bomb on a Sun SPARCstation.
    
                    // ANORMO = ZLANGE( 'O', M, N, A, LDA, RWORK )
                    // ANORMI = ZLANGE( 'I', M, N, A, LDA, RWORK )
     
                    // Do for each blocksize in nbval
    
                    for (inb = 1; inb <= nnb; inb++) {
                        nb = nbval[inb-1];
                        xlaenv(1, nb);
    
                        // Compute the LU factorization of the matrix.
    
                        zlacpy('F', m, n, A, lda, AFAC, lda);
                        zgetrf(m, n, AFAC, lda, iwork, info);
    
                        // Check error code from dgetrf.
    
                        if (info[0] != izero) {
                         // Print the header if this is the first error message.
                            if (nfail == 0 && nerrs == 0) {
                                printHeader();
                            } // if (nfail == 0 && nerrs == 0)
                            nerrs++;
                            if (izero != 0) {
                                Preferences.debug("zgetrf returned with info[0] = " + info[0] + "instad of " +
                                izero + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("m = " + m + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nb = " + nb + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);    
                            }
                            else {
                                Preferences.debug("zgetrf returned with info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
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
    
                        zlacpy('F', m, n, AFAC, lda, AINV, lda);
                        zget01(m, n, A, lda, AINV, lda, iwork, rwork, result);
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
            Preferences.debug("zchkge: " + nfail + " out of " + nrun + " tests failed with values >= threshold\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("zchkge: " + nfail + " out of " + nrun + " tests failed with values >= threshold\n");
        }
        else {
            Preferences.debug("All " + nrun + " tests for zchkge passed\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("All " + nrun + " tests for zchkge passed\n");
        }
        if (nerrs > 0) {
            Preferences.debug("zchkge: " + nerrs + " error messages recorded\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("zchkge: " + nerrs + " error messages recorded\n");
        }
        
        return;
    } // zchkge */
    
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
        return;
    } // printHeader
    
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
    
    /**
     * This is a port of version 3.7.0 LAPACK test routine DLATB4. Univ. of Tennessee, Univ. of California Berkeley,
     * University of Colorado and NAG Ltd.. December 2106
     * 
     * .. Scalar Arguments .. CHARACTER DIST, TYPE CHARACTER*3 PATH INTEGER IMAT, KL, KU, M, MODE, N DOUBLE PRECISION
     * ANORM, CNDNUM ..
     * 
     * Purpose =======
     * 
     * ZLATB4 sets parameters for the matrix generator based on the type of matrix to be generated.
     * 
     * Arguments =========
     * 
     * PATH (input) String The LAPACK path name.
     * 
     * IMAT (input) INTEGER An integer key describing which matrix to generate for this path.
     * 
     * M (input) INTEGER The number of rows in the matrix to be generated.
     * 
     * N (input) INTEGER The number of columns in the matrix to be generated.
     * 
     * TYPE (output) CHARACTER*1 The type of the matrix to be generated: = 'S': symmetric matrix,
     *  = 'H': Hermetian
     *  = 'P': Hermitian symmetric positive (semi)definite matrix 
     *  = 'N': nonsymmetric matrix
     * 
     * KL (output) INTEGER The lower band width of the matrix to be generated.
     * 
     * KU (output) INTEGER The upper band width of the matrix to be generated.
     * 
     * ANORM (output) DOUBLE PRECISION The desired norm of the matrix to be generated. The diagonal matrix of singular
     * values or eigenvalues is scaled by this value.
     * 
     * MODE (output) INTEGER A key indicating how to choose the vector of eigenvalues.
     * 
     * CNDNUM (output) DOUBLE PRECISION The desired condition number.
     * 
     * DIST (output) CHARACTER*1 The type of distribution to be used by the random number generator.
     */
    public void zlatb4(final String path, final int imat, final int m, final int n, final char[] type, final int[] kl,
            final int[] ku, final double anorm[], final int[] mode, final double[] cndnum, final char[] dist) {
        String c2;
        int mat;
        // Set some constants for use in the subroutine.
        if (first_zlatb4) {
            first_zlatb4 = false;
            eps_zlatb4 = ge.dlamch('P'); // Precision
            badc2_zlatb4 = 0.1 / eps_zlatb4;
            badc1_zlatb4 = Math.sqrt(badc2_zlatb4);
            small_zlatb4[0] = ge.dlamch('S'); // Safe minimum
            large_zlatb4[0] = 1.0 / small_zlatb4[0];

            // If it looks like we're on a Cray, take the square root of
            // small_dlatb4 and large_dlatb4 to avoid overflow and underflow problems.
            ge.dlabad(small_zlatb4, large_zlatb4);
            small_zlatb4[0] = 0.25 * (small_zlatb4[0] / eps_zlatb4);
            large_zlatb4[0] = 1.0 / small_zlatb4[0];
        } // if (first_zlatb4)

        c2 = path.substring(1, 3);

        // Set some parameters we don't plan to change.

        dist[0] = 'S';
        mode[0] = 3;
        if ( (c2.equalsIgnoreCase("QR")) || (c2.equalsIgnoreCase("LQ")) || (c2.equalsIgnoreCase("QL"))
                || (c2.equalsIgnoreCase("RQ"))) {
            // xQR, xLQ, xQL, xRQ: Set parameters to generate a general m x n matrix.
            // Set type[0], the type of matrix to be generated.
            type[0] = 'N';

            // Set the lower and upper bandwidths.
            if (imat == 1) {
                kl[0] = 0;
                ku[0] = 0;
            } else if (imat == 2) {
                kl[0] = 0;
                ku[0] = Math.max(n - 1, 0);
            } else if (imat == 3) {
                kl[0] = Math.max(m - 1, 0);
                ku[0] = 0;
            } else {
                kl[0] = Math.max(m - 1, 0);
                ku[0] = Math.max(n - 1, 0);
            }

            // Set the condition number and norm.
            if (imat == 5) {
                cndnum[0] = badc1_zlatb4;
            } else if (imat == 6) {
                cndnum[0] = badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if (imat == 7) {
                anorm[0] = small_zlatb4[0];
            } else if (imat == 8) {
                anorm[0] = large_zlatb4[0];
            } else {
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
            } else if (imat == 2) {
                kl[0] = 0;
                ku[0] = Math.max(n - 1, 0);
            } else if (imat == 3) {
                kl[0] = Math.max(m - 1, 0);
                ku[0] = 0;
            } else {
                kl[0] = Math.max(m - 1, 0);
                ku[0] = Math.max(n - 1, 0);
            }

            // Set the condition number and norm.
            if (imat == 8) {
                cndnum[0] = badc1_zlatb4;
            } else if (imat == 9) {
                cndnum[0] = badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if (imat == 10) {
                anorm[0] = small_zlatb4[0];
            } else if (imat == 11) {
                anorm[0] = large_zlatb4[0];
            } else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("GE"))
        else if (c2.equalsIgnoreCase("GB")) {
            // xGB: Set parameters to generate a general banded matrix.

            // Set type[0], the type of matrix to be generated.
            type[0] = 'N';

            // Set the condition number and norm
            if (imat == 5) {
                cndnum[0] = badc1_zlatb4;
            } else if (imat == 6) {
                cndnum[0] = 0.1 * badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if (imat == 7) {
                anorm[0] = small_zlatb4[0];
            } else if (imat == 8) {
                anorm[0] = large_zlatb4[0];
            } else {
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
            } else {
                kl[0] = 1;
            }
            ku[0] = kl[0];

            // Set the condition number and norm.
            if (imat == 3) {
                cndnum[0] = badc1_zlatb4;
            } else if (imat == 4) {
                cndnum[0] = badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if ( (imat == 5) || (imat == 11)) {
                anorm[0] = small_zlatb4[0];
            } else if ( (imat == 6) || (imat == 12)) {
                anorm[0] = large_zlatb4[0];
            } else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("GT))
        else if ( (c2.equalsIgnoreCase("PO")) || (c2.equalsIgnoreCase("PP"))) {
            // xPO, xPP: Set parameters to generate a symmetric or Hermitian positive definite matrix.

            // Set type[0], the type of matrix to be generated
            type[0] = c2.charAt(0);

            // Set the lower and upper bandwidths.
            if (imat == 1) {
                kl[0] = 0;
            } else {
                kl[0] = Math.max(0, n - 1);
            }
            ku[0] = kl[0];

            // Set the condition number and norm.
            if (imat == 6) {
                cndnum[0] = badc1_zlatb4;
            } else if (imat == 7) {
                cndnum[0] = badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if (imat == 8) {
                anorm[0] = small_zlatb4[0];
            } else if (imat == 9) {
                anorm[0] = large_zlatb4[0];
            } else {
                anorm[0] = 1.0;
            }
        } // else if ((c2.equalsIgnoreCase("PO")) || (c2.equalsIgnoreCase("PP")))
        else if ( (c2.equalsIgnoreCase("HE")) || (c2.equalsIgnoreCase("HP")) ||
        		(c2.equalsIgnoreCase("SY")) || (c2.equalsIgnoreCase("SP"))) {
            // xHE, xHP, xSY, xSP: Set parameters to generate a symmetric or Hermitian matrix.

            // Set type[0], the type of matrix to be generated
            type[0] = c2.charAt(0);

            // Set the lower and upper bandwidths.
            if (imat == 1) {
                kl[0] = 0;
            } else {
                kl[0] = Math.max(0, n - 1);
            }
            ku[0] = kl[0];

            // Set the condition number and norm.
            if (imat == 7) {
                cndnum[0] = badc1_zlatb4;
            } else if (imat == 8) {
                cndnum[0] = badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if (imat == 9) {
                anorm[0] = small_zlatb4[0];
            } else if (imat == 10) {
                anorm[0] = large_zlatb4[0];
            } else {
                anorm[0] = 1.0;
            }
        } //  ( (c2.equalsIgnoreCase("HE")) || (c2.equalsIgnoreCase("HP")) ||
        else if (c2.equalsIgnoreCase("PB")) {
            // xPB: Set the parameters to generate a symmetric band matrix.

            // Set type[0], the type of matrix to be generated.
            type[0] = 'P';

            // Set the norm and condition number.
            if (imat == 5) {
                cndnum[0] = badc1_zlatb4;
            } else if (imat == 6) {
                cndnum[0] = badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if (imat == 7) {
                anorm[0] = small_zlatb4[0];
            } else if (imat == 8) {
                anorm[0] = large_zlatb4[0];
            } else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("PB))
        else if (c2.equalsIgnoreCase("PT")) {
            // xPT: Set parameters to generate a symmetric positive definite tridiagonal matrix.

            type[0] = 'P';
            if (imat == 1) {
                kl[0] = 0;
            } else {
                kl[0] = 1;
            }
            ku[0] = kl[0];

            // Set the condition number and norm.
            if (imat == 3) {
                cndnum[0] = badc1_zlatb4;
            } else if (imat == 4) {
                cndnum[0] = badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if ( (imat == 5) || (imat == 11)) {
                anorm[0] = small_zlatb4[0];
            } else if ( (imat == 6) || (imat == 12)) {
                anorm[0] = large_zlatb4[0];
            } else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("PT"))
        else if ( (c2.equalsIgnoreCase("TR")) || (c2.equalsIgnoreCase("TP"))) {
            // xTR, xTP: Set parameters to generate a triangular matrix

            // Set type[0], the type of matrix to be generated.
            type[0] = 'N';

            // Set the lower and upper bandwidths.
            mat = Math.abs(imat);
            if ( (mat == 1) || (mat == 7)) {
                kl[0] = 0;
                ku[0] = 0;
            } else if (imat < 0) {
                kl[0] = Math.max(0, n - 1);
                ku[0] = 0;
            } else {
                kl[0] = 0;
                ku[0] = Math.max(0, n - 1);
            }

            // Set the condition number and norm.
            if ( (mat == 3) || (mat == 9)) {
                cndnum[0] = badc1_zlatb4;
            } else if ( (mat == 4) || (mat == 10)) {
                cndnum[0] = badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if (mat == 5) {
                anorm[0] = small_zlatb4[0];
            } else if (mat == 6) {
                anorm[0] = large_zlatb4[0];
            } else {
                anorm[0] = 1.0;
            }
        } // else if ((c2.equalsIgnoreCase("TR")) || (c2.equalsIgnoreCase("TP")))
        else if (c2.equalsIgnoreCase("TB")) {
            // xTB: Set parameters to generate a triangular band matrix.

            // Set type[0], the type of matrix to be generated.
            type[0] = 'N';

            // Set the norm and condition number.
            if ( (imat == 2) || (imat == 8)) {
                cndnum[0] = badc1_zlatb4;
            } else if ( (imat == 3) || (imat == 9)) {
                cndnum[0] = badc2_zlatb4;
            } else {
                cndnum[0] = 2.0;
            }

            if (imat == 4) {
                anorm[0] = small_zlatb4[0];
            } else if (imat == 5) {
                anorm[0] = large_zlatb4[0];
            } else {
                anorm[0] = 1.0;
            }
        } // else if (c2.equalsIgnoreCase("TB"))

        if (n <= 1) {
            cndnum[0] = 1.0;
        }

        return;
    } // zlatb4
    
    /**
     * This is a port of version 3.7.0 LAPACK test routine ZLATMS Original ZLATMS created by Univ. of Tennessee, Univ. of
     * California Berkeley, and NAG Ltd., December, 2016 zlatms generates random matrices with specified singular values
     * (or hermitian with specified eigenvalues) for testing LAPACK programs.
     * 
     * <p>
     * zlatms operates by applying the following sequence of operations:
     * </p>
     * 
     * <p>
     * Set the diagonal to D, where D may be input or computed according to mode, cond, dmax, and sym as described
     * below.
     * </p>
     * 
     * <p>
     * Generate a matrix with appropriate band structure, by one of two methods:
     * </p>
     * 
     * <p>
     * Method A: Generate a dense m by n matrix by multiplying D on the left and the right by random unitary matrices,
     * then:
     * </p>
     * 
     * <p>
     * Reduce the bandwidth according to kl and ku, using Householder transformations.
     * </p>
     * 
     * <p>
     * Method B: Convert the bandwidth-0 (i.e., diagonal) matrix to a bandwidth-1 matrix using Givens rotations,
     * "chasing" out-of-band elements back, much as in QR; then convert the bandwidth-1 to a bandwidth-2 matrix, etc.
     * Note that for reasonably samll bandwidths (relative to m and n) this requires less storage, as a dense matrix is
     * not generated. Also, for hermitian or symmetric matrices, only, one triangle is generated.
     * </p>
     * 
     * <p>
     * Method A is chosen if the bandwidth is a large fraction of the order of the matrix, and lda is at least m (so a
     * dense matrix can be stored.) Method B is chosen if the bandwidth is small ( < 1/2 n for hermitian or symmetric, < .3 n+m for
     * non-symmetric), or lda is less than m and not less than the bandwidth.
     * </p>
     * 
     * <p>
     * Pack the matrix if desired. Options specified by pack are: no packing, zero out upper half (if hermitian), zero
     * out lower half (if hermitian), store the upper half columnwise (if hermitian or upper triangular), store the
     * lower half columnwise (if hermitian or lower triangular), store the lower triangle in banded format (if hermitian
     * or lower triangular), store the upper triangle in banded format (if hermitian or upper triangular), and store the
     * entire matrix in banded format If method B is chosen, and band format is specified, then the matrix will be
     * generated in the band format, so no repacking will be necessary.
     * </p>
     * 
     * @param m input int The number of rows of A.
     * @param n input int The number of columns of A.  n must equal m if the matrix is symmetric or hermitian
     *                    (i.e., if sym is not 'N').
     * @param dist input char On entry, dist specifies the type of distribution to be used to generate the random
     *            eigen-/singular values. 'U' => uniform(0,1) ('U' for uniform) 'S' => uniform(-1,1) ('S' for symmetric)
     *            'N' => normal(0,1) ('N' for normal)
     * @param iseed input/output int[] of dimension 4 On entry iseed specifies the seed of the random number generator.
     *            They should lie between 0 and 4095 inclusive, and iseed[3] should be odd. The random number generator
     *            uses a linear congruential sequence limited to small integers, and so should produce machine
     *            independent random numbers. The values of iseed are changed on exit, and can be used in the next call
     *            to zlatms to continue the same random number sequence. Changed on exit.
     * @param sym input char
     * SYM is CHARACTER*1
     *            If SYM='H', the generated matrix is hermitian, with
     *            eigenvalues specified by D, COND, MODE, and DMAX; they
     *            may be positive, negative, or zero.
     *            If SYM='P', the generated matrix is hermitian, with
     *            eigenvalues (= singular values) specified by D, COND,
     *            MODE, and DMAX; they will not be negative.
     *            If SYM='N', the generated matrix is nonsymmetric, with
     *            singular values specified by D, COND, MODE, and DMAX;
     *            they will not be negative.
     *            If SYM='S', the generated matrix is (complex) symmetric,
     *            with singular values specified by D, COND, MODE, and
     *            DMAX; they will not be negative.
     *            Not modified.
     * @param D input/output double[] of dimension (min(m,n)) This array is used to specify the singular values or
     *            eigenvalues of A (see sym, above.) If mode = 0, then D is assumed to contain the singular/eigenvalues,
     *            otherwise they will be computed according to mode, cond, and dmax, and placed in D. Modified if mode
     *            is nonzero.
     * @param mode input int On entry this describes how the singular/ eigenvalues are to be specified: = 0 means use D
     *            as input = 1 sets D[0] = 1 and D[1:n-1] = 1.0/cond = 2 sets D[0:n-2] = 1 and D[n-1] = 1.0/cond = 3
     *            sets D[i] = cond**(-(i)/(n-1)) = 4 sets D[i] = 1 - (i)/(n-1)*(1 - 1/cond) = 5 sets D to random numbers
     *            in the range (1/cond, 1) such that their logarithms are uniformly distributed = 6 sets D to random
     *            numbers from same distribution as the rest of the matrix mode < 0 has the same meaning as abs(mode),
     *            except that the order of the elements of D is reversed. Thus, if mode is positive, D has entries
     *            ranging from 1 to 1/cond, if negative, from 1/cond to 1. If sym = 'H', and mode is neither 0,
     *            6, nor -6, then the elements of D will also be multiplied by a random sign (1.e., +1 or -1).
     * @param cond input double On entry, this is used as described under mode above. If used, it must be >= 1.
     * @param dmax input double If mode is neither -6, 0, nor 6, the contents of D, as computed according to mode and
     *            cond, will be scaled by dmax/max(abs(D[i])); thus the maximum absolute eigen- or singular value (which
     *            is to say the norm) will be abs(dmax). Note that dmax need not be positive: if dmax is negative (or
     *            zero), D will be scaled by a negative number (or zero).
     * @param kl input int This specifies the lower bandwidth of the matrix. For example, kl = 0 implies upper
     *            triangular, kl = 1 implies upper Hessenberg, and kl being at least m-1 means that the matrix has full
     *            lower bandwidth. kl must equal ku if the matrix is symmetric or hermitian.
     * @param ku input int This specifies the upper bandwidth of the matrix. For example, ku = 0 implies lower
     *            triangular, ku = 1 implies lower Hessenberg, and ku being at least n-1 means that the matrix has full
     *            upper bandwidth. kl must equal ku if the matrix is symmetric or hermitian.
     * @param pack input char This specifies packing of the matrix as follows: 'N' => no packing 'U' => zero out all
     *            subdiagonal entries (if symmetric) 'L' => zero out all superdiagonal entries (if symmetric) 'C' =>
     *            store the upper triangle columnwise (only if the matrix is symmetric,hermitian, or upper triangular) 'R' => store
     *            the lower triangle columnwise (only if the matrix is symmetric, hermitian, or lower triangular) 'B' => store the
     *            lower triangle in band storage scheme (only if matrix symmetric, hermitian, or lower triangular) 'Q' => store the
     *            upper triangle in band storage scheme (only if matrix symmetric, hermitian, or upper triangular) 'Z' => store the
     *            entire matrix in band storage scheme (pivoting can be provided for by using this option to store A in
     *            the trailing rows of the allocated storage)
     * 
     *            Using these options, the various LAPACK packed and banded storage schemes can be obtained: GB - use
     *            'Z' PB, SB, HB, or TB - use 'B' or 'Q' PP, SP, HB, or TP - use 'C' or 'R' If two calls to zlatms differ only
     *            in the pack parameter, they will generate mathematically equivalent matrices
     * @param A input/output double[][][2] complex of dimension (lda,n) On exit A is the desired test matrix. A is first generated
     *            in full (unpacked) form, and then packed, if so specified by pack. Thus, the first m elements of the
     *            first n columns will always be modified. If pack specifies a packed or banded storage scheme, all lda
     *            elements of the first n columns will be modified; the elements of the array which do not correspond to
     *            elements of the generated matrix are set to zero.
     * @param lda input int lda specifies the first dimension of A as declared in the calling program. If pack = 'N',
     *            'U', 'L', 'C', or 'R', then lda must be at least m. If pack = 'B' or 'Q', then lda must be at least
     *            min(kl,m-1) (which is equal to min(ku,n-1)). If pack = 'Z', lda must be large enough to hold the
     *            packed array: min(ku,n-1) + min(kl,m-1) + 1.
     * @param work workspace double[][2] complex of dimension (3*max(n,m))
     * @param info output int[] Error code. On exit, info[0] will be set to one of the following values: 0 => normal
     *            return -1 => m negative or unequal to n and sym = 'S', 'H', or 'P' -2 => n negative -3 => dist illegal
     *            string -5 => sym illegal string -7 => mode not in range -6 to 6 -8 => cond less than 1.0, and mode
     *            neither -6, 0, nor 6 -10 => kl negative -11 => ku negative, or sym is not 'N' and ku not equal to kl
     *            -12 => pack illegal string, or pack = 'U' or 'L', and sym = 'N'; or pack = 'C' or 'Q' and sym = 'N'
     *            and kl is not zero; or pack = 'R' or 'B' and sym = 'N' and ku is not zero; or pack = 'U', 'L', 'C',
     *            'R', 'B', or 'Q' and m != n. -14 => lda is less than m, or pack = 'Z' and lda is less than min(ku,n-1)
     *            + min(kl,m-1) + 1. 1 => Error return from dlatm1 2 => Cannot scale to dmax (maximum singular value is
     *            0) 3 => Error return from zlagge or dlagsy
     */
     public void zlatms(final int m, final int n, final char dist, final int[] iseed, final char sym, final double[] D,
            final int mode, final double cond, final double dmax, final int kl, final int ku, final char pack,
            final double[][][] A, final int lda, final double[][] work, final int[] info) {
        boolean givens;
        boolean ilextr;
        boolean iltemp;
        boolean topdwn;
        int i;
        int ic;
        int icol = 0;
        int idist;
        int iendch;
        final int[] iinfo = new int[1];
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
        double alpha2[] = new double[2];
        double beta[] = new double[2];
        double angle;
        double[] c = new double[2];
        double[] dummy = new double[2];
        double[] extra = new double[2];
        double[] s = new double[2];
        double[] temp = new double[1];
        int length;
        double[][] ap;
        int index;
        boolean zsym = false;
        double result[] = new double[2];
        double var;
        double realc[] = new double[1];
        double f[] = new double[2];
        double cr[] = new double[1];
        double ci[] = new double[1];
        double ct[] = new double[2];
        double st[] = new double[2];

        // Decode and test the input parameters. Initialize flags & seed.
        info[0] = 0;

        // Quick return if possible
        if ( (m == 0) || (n == 0)) {
            return;
        }

        // Decode dist
        if ( (dist == 'U') || (dist == 'u')) {
            idist = 1;
        } else if ( (dist == 'S') || (dist == 's')) {
            idist = 2;
        } else if ( (dist == 'N') || (dist == 'n')) {
            idist = 3;
        } else {
            idist = -1;
        }

        // Decode sym
        if ( (sym == 'N') || (sym == 'n')) {
            isym = 1;
            irsign = 0;
            zsym = false;
        } else if ( (sym == 'P') || (sym == 'p')) {
            isym = 2;
            irsign = 0;
            zsym = false;
        } else if ( (sym == 'S') || (sym == 's')) {
            isym = 2;
            irsign = 0;
            zsym = true;
        } else if ( (sym == 'H') || (sym == 'h')) {
            isym = 2;
            irsign = 1;
            zsym = false;
        } else {
            isym = -1;
        }

        // Decode pack
        if ( (pack == 'N') || (pack == 'n')) {
            ipack = 0;
        } else if ( (pack == 'U') || (pack == 'u')) {
            ipack = 1;
            isympk = 1;
        } else if ( (pack == 'L') || (pack == 'l')) {
            ipack = 2;
            isympk = 1;
        } else if ( (pack == 'C') || (pack == 'c')) {
            ipack = 3;
            isympk = 2;
        } else if ( (pack == 'R') || (pack == 'r')) {
            ipack = 4;
            isympk = 3;
        } else if ( (pack == 'B') || (pack == 'b')) {
            ipack = 5;
            isympk = 3;
        } else if ( (pack == 'Q') || (pack == 'q')) {
            ipack = 6;
            isympk = 2;
        } else if ( (pack == 'Z') || (pack == 'z')) {
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

        if ( (ipack == 5) || (ipack == 6)) {
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

            if ( (llb + uub) < (0.3 * Math.max(1, mr + nc))) {
                givens = true;
            }
        } // if (isym == 1)
        else if ( (2 * llb) < m) {
            givens = true;
        }

        if ( (lda < m) && (lda >= minlda)) {
            givens = true;
        }

        // Set info if an error
        if (m < 0) {
            info[0] = -1;
        } else if ( (m != n) && (isym != 1)) {
            info[0] = -1;
        } else if (n < 0) {
            info[0] = -2;
        } else if (idist == -1) {
            info[0] = -3;
        } else if (isym == -1) {
            info[0] = -5;
        } else if (Math.abs(mode) > 6) {
            info[0] = -7;
        } else if ( (mode != 0) && (Math.abs(mode) != 6) && (cond < 1.0)) {
            info[0] = -8;
        } else if (kl < 0) {
            info[0] = -10;
        } else if ( (ku < 0) || ( (isym != 1) && (kl != ku))) {
            info[0] = -11;
        } else if ( (ipack == -1) || ( (isympk == 1) && (isym == 1)) || ( (isympk == 2) && (isym == 1) && (kl > 0))
                || ( (isympk == 3) && (isym == 1) && (ku > 0)) || ( (isympk != 0) && (m != n))) {
            info[0] = -12;
        } else if (lda < Math.max(1, minlda)) {
            info[0] = -14;
        }

        if (info[0] != 0) {
            MipavUtil.displayError("Error zlatms had info[0] = " + info[0]);

            return;
        }

        // Initialize random number generator
        for (i = 0; i < 4; i++) {
            iseed[i] = Math.abs(iseed[i]) % 4096;
        }

        if ( (iseed[3] % 2) != 1) {
            iseed[3] = iseed[3] + 1;
        }

        // Setup D if indicated
        // Compute D according to cond and mode
        ge.dlatm1(mode, cond, irsign, idist, iseed, D, mnmin, iinfo);

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

        if ( (mode != 0) && (Math.abs(mode) != 6)) {

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
        
        alpha2[0] = 0.0;
        alpha2[1] = 0.0;
        beta[0] = 0.0;
        beta[1] = 0.0;
        zlaset('F', lda, n, alpha2, beta, A, lda);

        // Generate Banded Matrix using Givens rotations.
        // Also the special case of uub = llb = 0

        // Compute Addressing constants to cover all storage formats. Whether
        // GE, HE, SY, GB, HB, or SB, upper or lower triangle or both, the (i,j)-th
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

        // ipackg is the format that the matrix is generated in. If this is
        // different from ipack, then the matrix must be repacked at the end.
        // It also signals how to compute the norm, for scaling.
        ipackg = 0;

        // Diagonal Matrix -- We are done, unless it is to be stored HP/SP/PP/TP
        // (pack = 'R' or 'C')
        if ( (llb == 0) && (uub == 0)) {

            for (i = 0; i < mnmin; i++) {
                A[ (1-iskew)*i + ioffst][i][0] = D[i];
                A[ (1-iskew)*i + ioffst][i][1] = 0.0;
            }
           
            if ( (ipack <= 2) || (ipack >= 5)) {
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
                
                for (i = 0; i < mnmin; i++) {
                    A[ (1-iskew)*i + ioffst][i][0] = D[i];
                    A[ (1-iskew)*i + ioffst][i][1] = 0.0;
                }

                if (topdwn) {
                    jkl = 0;

                    for (jku = 1; jku <= uub; jku++) {
                        // Transform from bandwidth jkl, jku-1 to jkl, jku
                        // Last row actually rotated is m-1
                        // Last column actually rotated is min(m+jku,n)-1

                        for (jr = 1; jr <= (Math.min(m + jku, n) + jkl - 1); jr++) {
                            extra[0] = 0.0;
                            extra[1] = 0.0;
                            angle = 2.0 * Math.PI * ge.dlarnd(1, iseed);
                            result = zlarnd(5, iseed);
                            var =  Math.cos(angle);
                            c[0] = var * result[0];
                            c[1] = var * result[1];
                            result = zlarnd(5, iseed);
                            var = Math.sin(angle);
                            s[0] = var * result[0];
                            s[1] = var * result[1];
                            icol = Math.max(1, jr - jkl);

                            if (jr < m) {
                                il = Math.min(n, jr + jku) + 1 - icol;
                                length = ilda - (jr - (iskew * icol) + ioffst) + 1 + (ilda * (n - icol));
                                ap = new double[length][2];
                                index = 0;

                                for (i = jr - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][icol - 1][0];
                                    ap[index++][1] = A[i][icol-1][1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(true, jr > jkl, false, il, c, s, ap, ilda, extra, dummy);
                                index = 0;

                                for (i = jr - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                    A[i][icol - 1][0] = ap[index][0];
                                    A[i][icol - 1][1] = ap[index++][1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }
                            } // if (jr < m)

                            // Chase "extra" back up
                            ir = jr;
                            ic = icol;

                            for (jch = jr - jkl; jch >= 1; jch -= (jkl + jku)) {

                                if (ir < m) {
                                	f[0] = A[ir - (iskew * (ic + 1)) + ioffst][ic][0];
                                	f[1] = A[ir - (iskew * (ic + 1)) + ioffst][ic][1];
                                    zlartg(A[ir - (iskew * (ic + 1)) + ioffst][ic], extra, realc, s, dummy);
                                    dummy = zlarnd(5, iseed);
                                    c[0] = realc[0] * dummy[0];
                                    c[1] = -realc[0] * dummy[1];
                                    zmlt(-s[0], -s[1], dummy[0], dummy[1], cr, ci);
                                    s[0] = cr[0];
                                    s[1] = -ci[0];
                                } // if (ir < m)

                                irow = Math.max(1, jch - jku);
                                il = ir + 2 - irow;
                                temp[0] = 0.0;
                                temp[1] = 0.0;
                                iltemp = jch > jku;
                                length = ilda - (irow - (iskew * ic) + ioffst) + 1 + (ilda * (n - ic));
                                ap = new double[length][2];
                                index = 0;

                                for (i = irow - (iskew * ic) + ioffst - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][ic - 1][0];
                                    ap[index++][1] = A[i][ic - 1][1];
                                }

                                for (j = ic; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(false, iltemp, true, il, c, s, ap, ilda, temp, extra);
                                index = 0;

                                for (i = irow - (iskew * ic) + ioffst - 1; i < ilda; i++) {
                                    A[i][ic - 1][0] = ap[index][0];
                                    A[i][ic - 1][1] = ap[index++][1];
                                }

                                for (j = ic; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }

                                if (iltemp) {
                                    zlartg(A[irow - (iskew * (ic + 1)) + ioffst][ic], temp, realc, s, dummy);
                                    dummy = zlarnd(5, iseed);
                                    c[0] = realc[0] * dummy[0];
                                    c[1] = -realc[0] * dummy[1];
                                    zmlt(-s[0], -s[1], dummy[0], dummy[1], cr, ci);
                                    s[0] = cr[0];
                                    s[1] = -ci[0];
                                    icol = Math.max(1, jch - jku - jkl);
                                    il = ic + 2 - icol;
                                    extra[0] = 0.0;
                                    extra[1] = 0.0;
                                    length = ilda - (irow - (iskew * icol) + ioffst) + 1 + (ilda * (n - icol));
                                    ap = new double[length][2];
                                    index = 0;

                                    for (i = irow - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                        ap[index][0] = A[i][icol - 1][0];
                                        ap[index++][1] = A[i][icol - 1][1];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < ilda; i++) {
                                            ap[index][0] = A[i][j][0];
                                            ap[index++][1] = A[i][j][1];
                                        }
                                    }

                                    zlarot(true, jch > (jku + jkl), true, il, c, s, ap, ilda, extra, temp);
                                    index = 0;

                                    for (i = irow - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                        A[i][icol - 1][0] = ap[index][0];
                                        A[i][icol - 1][1] = ap[index++][1];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < ilda; i++) {
                                            A[i][j][0] = ap[index][0];
                                            A[i][j][1] = ap[index++][1];
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
                            extra[1] = 0.0;
                            angle = 2.0 * Math.PI * ge.dlarnd(1, iseed);
                            result = zlarnd(5, iseed);
                            var = Math.cos(angle);
                            c[0] = var*result[0];
                            c[1] = var*result[1];
                            result = zlarnd(5, iseed);
                            var = Math.sin(angle);
                            s[0] = var * result[0];
                            s[1] = var * result[1];
                            irow = Math.max(1, jc - jku);

                            if (jc < n) {
                                il = Math.min(m, jc + jkl) + 1 - irow;
                                length = ilda - (irow - (iskew * jc) + ioffst) + 1 + (ilda * (n - jc));
                                ap = new double[length][2];
                                index = 0;

                                for (i = irow - (iskew * jc) + ioffst - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][jc - 1][0];
                                    ap[index++][1] = A[i][jc - 1][1];
                                }

                                for (j = jc; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(false, jc > jku, false, il, c, s, ap, ilda, extra, dummy);
                                index = 0;

                                for (i = irow - (iskew * jc) + ioffst - 1; i < ilda; i++) {
                                    A[i][jc - 1][0] = ap[index][0];
                                    A[i][jc - 1][1] = ap[index++][1];
                                }

                                for (j = jc; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }
                            } // if (jc < n)

                            // Chase "extra" back up
                            ic = jc;
                            ir = irow;

                            for (jch = jc - jku; jch >= 1; jch -= (jkl + jku)) {

                                if (ic < n) {
                                    zlartg(A[ir - (iskew * (ic + 1)) + ioffst][ic], extra, realc, s, dummy);
                                    dummy = zlarnd(5, iseed);
                                    c[0] = realc[0] * dummy[0];
                                    c[1] = -realc[0] * dummy[1];
                                    zmlt(-s[0], -s[1], dummy[0], dummy[1], cr, ci);
                                    s[0] = cr[0];
                                    s[1] = -ci[0];
                                } // if (ic < n)

                                icol = Math.max(1, jch - jkl);
                                il = ic + 2 - icol;
                                temp[0] = 0.0;
                                temp[1] = 0.0;
                                iltemp = jch > jkl;
                                length = ilda - (ir - (iskew * icol) + ioffst) + 1 + (ilda * (n - icol));
                                ap = new double[length][2];
                                index = 0;

                                for (i = ir - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][icol - 1][0];
                                    ap[index++][1] = A[i][icol - 1][1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(true, iltemp, true, il, c, s, ap, ilda, temp, extra);
                                index = 0;

                                for (i = ir - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                    A[i][icol - 1][0] = ap[index][0];
                                    A[i][icol - 1][1] = ap[index++][1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }

                                if (iltemp) {
                                    zlartg(A[ir - (iskew * (icol + 1)) + ioffst][icol], temp, realc, s, dummy);
                                    dummy = zlarnd(5, iseed);
                                    c[0] = realc[0] * dummy[0];
                                    c[1] = -realc[0] * dummy[1];
                                    zmlt(-s[0], -s[1], dummy[0], dummy[1], cr, ci);
                                    s[0] = cr[0];
                                    s[1] = -ci[0];
                                    irow = Math.max(1, jch - jkl - jku);
                                    il = ir + 2 - irow;
                                    extra[0] = 0.0;
                                    extra[1] = 0.0;
                                    length = ilda - (irow - (iskew * icol) + ioffst) + 1 + (ilda * (n - icol));
                                    ap = new double[length][0];
                                    index = 0;

                                    for (i = irow - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                        ap[index][0] = A[i][icol - 1][0];
                                        ap[index++][1] = A[i][icol - 1][1];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < ilda; i++) {
                                            ap[index][0] = A[i][j][0];
                                            ap[index++][1] = A[i][j][1];
                                        }
                                    }

                                    zlarot(false, jch > (jkl + jku), true, il, c, s, ap, ilda, extra, temp);
                                    index = 0;

                                    for (i = irow - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                        A[i][icol - 1][0] = ap[index][0];
                                        A[i][icol - 1][1] = ap[index++][1];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < ilda; i++) {
                                            A[i][j][0] = ap[index][0];
                                            A[i][j][1] = ap[index++][1];
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
                            extra[1] = 0.0;
                            angle = 2.0 * Math.PI * ge.dlarnd(1, iseed);
                            result = zlarnd(5, iseed);
                            var = Math.cos(angle);
                            c[0] = var*result[0];
                            c[1] = var*result[1];
                            result = zlarnd(5, iseed);
                            var = Math.sin(angle);
                            s[0] = var * result[0];
                            s[1] = var * result[1];
                            irow = Math.max(1, jc - jku + 1);

                            if (jc > 0) {
                                il = Math.min(m, jc + jkl + 1) + 1 - irow;
                                length = ilda - (irow - (iskew * jc) + ioffst) + 1 + (ilda * (n - jc));
                                ap = new double[length][2];
                                index = 0;

                                for (i = irow - (iskew * jc) + ioffst - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][jc - 1][0];
                                    ap[index++][1] = A[i][jc - 1][1];
                                }

                                for (j = jc; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(false, false, (jc + jkl) < m, il, c, s, ap, ilda, dummy, extra);
                                index = 0;

                                for (i = irow - (iskew * jc) + ioffst - 1; i < ilda; i++) {
                                    A[i][jc - 1][0] = ap[index][0];
                                    A[i][jc - 1][1] = ap[index++][1];
                                }

                                for (j = jc; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }
                            } // if (jc > 0)

                            // Chase "extra" back down
                            ic = jc;

                            for (jch = jc + jkl; jch <= iendch; jch += (jkl + jku)) {
                                ilextr = ic > 0;

                                if (ilextr) {
                                    zlartg(A[jch - (iskew * ic) + ioffst - 1][ic - 1], extra, realc, s, dummy);
                                    dummy = zlarnd(5, iseed);
                                    c[0] = realc[0] * dummy[0];
                                    c[1] = realc[0] * dummy[1];
                                    zmlt(s[0], s[1], dummy[0], dummy[1], cr, ci);
                                    s[0] = cr[0];
                                    s[1] = ci[0];
                                } // if (ilextr)

                                ic = Math.max(1, ic);
                                icol = Math.min(n - 1, jch + jku);
                                iltemp = (jch + jku) < n;
                                temp[0] = 0.0;
                                temp[1] = 0.0;
                                length = ilda - (jch - (iskew * ic) + ioffst) + 1 + (ilda * (n - ic));
                                ap = new double[length][2];
                                index = 0;

                                for (i = jch - (iskew * ic) + ioffst - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][ic - 1][0];
                                    ap[index++][1] = A[i][ic - 1][1];
                                }

                                for (j = ic; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(true, ilextr, iltemp, icol + 2 - ic, c, s, ap, ilda, extra, temp);
                                index = 0;

                                for (i = jch - (iskew * ic) + ioffst - 1; i < lda; i++) {
                                    A[i][ic - 1][0] = ap[index][0];
                                    A[i][ic - 1][1] = ap[index++][1];
                                }

                                for (j = ic; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }

                                if (iltemp) {
                                    zlartg(A[jch - (iskew * icol) + ioffst - 1][icol - 1], temp, realc, s, dummy);
                                    dummy = zlarnd(5, iseed);
                                    c[0] = realc[0] * dummy[0];
                                    c[1] = realc[0] * dummy[1];
                                    zmlt(s[0], s[1], dummy[0], dummy[1], cr, ci);
                                    s[0] = cr[0];
                                    s[1] = ci[0];
                                    il = Math.min(iendch, jch + jkl + jku) + 2 - jch;
                                    extra[0] = 0.0;
                                    extra[1] = 0.0;
                                    length = ilda - (jch - (iskew * icol) + ioffst) + 1 + (ilda * (n - icol));
                                    ap = new double[length][2];
                                    index = 0;

                                    for (i = jch - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                        ap[index][0] = A[i][icol - 1][0];
                                        ap[index++][1] = A[i][icol - 1][1];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < ilda; i++) {
                                            ap[index][0] = A[i][j][0];
                                            ap[index++][1] = A[i][j][1];
                                        }
                                    }

                                    zlarot(false, true, (jch + jkl + jku) <= iendch, il, c, s, ap, ilda, temp,
                                            extra);
                                    index = 0;

                                    for (i = jch - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                        A[i][icol - 1][0] = ap[index][0];
                                        A[i][icol - 1][1] = ap[index++][1];
                                    }

                                    for (j = icol; j < n; j++) {

                                        for (i = 0; i < ilda; i++) {
                                            A[i][j][0] = ap[index][0];
                                            A[i][j][1] = ap[index++][1];
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
                            extra[1] = 0.0;
                            angle = 2.0 * Math.PI * ge.dlarnd(1, iseed);
                            result = zlarnd(5, iseed);
                            var = Math.cos(angle);
                            c[0] = var*result[0];
                            c[1] = var*result[1];
                            result = zlarnd(5, iseed);
                            var = Math.sin(angle);
                            s[0] = var * result[0];
                            s[1] = var * result[1];
                            icol = Math.max(1, jr - jkl + 1);

                            if (jr > 0) {
                                il = Math.min(n, jr + jku + 1) + 1 - icol;
                                length = ilda - (jr - (iskew * icol) + ioffst) + 1 + (ilda * (n - icol));
                                ap = new double[length][2];
                                index = 0;

                                for (i = jr - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][icol - 1][0];
                                    ap[index++][1] = A[i][icol-1][1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(true, false, (jr + jku) < n, il, c, s, ap, ilda, dummy, extra);
                                index = 0;

                                for (i = jr - (iskew * icol) + ioffst - 1; i < ilda; i++) {
                                    A[i][icol - 1][0] = ap[index][0];
                                    A[i][icol - 1][1] = ap[index++][1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }
                            } // if (jr > 0)

                            // Chase "extra" back down
                            ir = jr;

                            for (jch = jr + jku; jch <= iendch; jch += (jkl + jku)) {
                                ilextr = ir > 0;

                                if (ilextr) {
                                    zlartg(A[ir - (iskew * jch) + ioffst - 1][jch - 1], extra, realc, s, dummy);
                                    dummy = zlarnd(5, iseed);
                                    c[0] = realc[0] * dummy[0];
                                    c[1] = realc[0] * dummy[1];
                                    zmlt(s[0], s[1], dummy[0], dummy[1], cr, ci);
                                    s[0] = cr[0];
                                    s[1] = ci[0];
                                } // if (ilextr)

                                ir = Math.max(1, ir);
                                irow = Math.min(m - 1, jch + jkl);
                                iltemp = (jch + jkl) < m;
                                temp[0] = 0.0;
                                temp[1] = 0.0;
                                length = ilda - (ir - (iskew * jch) + ioffst) + 1 + (ilda * (n - jch));
                                ap = new double[length][2];
                                index = 0;

                                for (i = ir - (iskew * jch) + ioffst - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][jch - 1][0];
                                    ap[index++][1] = A[i][jch - 1][1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(false, ilextr, iltemp, irow + 2 - ir, c, s, ap, ilda, extra, temp);
                                index = 0;

                                for (i = ir - (iskew * jch) + ioffst - 1; i < ilda; i++) {
                                    A[i][jch - 1][0] = ap[index][0];
                                    A[i][jch - 1][1] = ap[index++][1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }

                                if (iltemp) {
                                    zlartg(A[irow - (iskew * jch) + ioffst - 1][jch - 1], temp, realc, s, dummy);
                                    dummy = zlarnd(5, iseed);
                                    c[0] = realc[0] * dummy[0];
                                    c[1] = realc[0] * dummy[1];
                                    zmlt(s[0], s[1], dummy[0], dummy[1], cr, ci);
                                    s[0] = cr[0];
                                    s[1] = ci[0];
                                    il = Math.min(iendch, jch + jkl + jku) + 2 - jch;
                                    extra[0] = 0.0;
                                    extra[1] = 0.0;
                                    length = ilda - (irow - (iskew * jch) + ioffst) + 1 + (ilda * (n - jch));
                                    ap = new double[length][2];
                                    index = 0;

                                    for (i = irow - (iskew * jch) + ioffst - 1; i < ilda; i++) {
                                        ap[index][0] = A[i][jch - 1][0];
                                        ap[index++][1] = A[i][jch - 1][1];
                                    }

                                    for (j = jch; j < n; j++) {

                                        for (i = 0; i < ilda; i++) {
                                            ap[index][0] = A[i][j][0];
                                            ap[index++][1] = A[i][j][1];
                                        }
                                    }

                                    zlarot(true, true, (jch + jkl + jku) <= iendch, il, c, s, ap, ilda, temp,
                                            extra);
                                    index = 0;

                                    for (i = irow - (iskew * jch) + ioffst - 1; i < ilda; i++) {
                                        A[i][jch - 1][0] = ap[index][0];
                                        A[i][jch - 1][1] = ap[index++][1];
                                    }

                                    for (j = jch; j < n; j++) {

                                        for (i = 0; i < ilda; i++) {
                                            A[i][j][0] = ap[index][0];
                                            A[i][j][1] = ap[index++][1];
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
            	// Hermitian -- A = U D U*
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
                    
                    for (i = 0; i < mnmin; i++) {
                        A[(1-iskew)*i + ioffg][i][0] = D[i];
                        A[(1-iskew)*i + ioffg][i][1] = 0.0;
                    }

                    for (k = 1; k <= uub; k++) {

                        for (jc = 1; jc <= (n - 1); jc++) {
                            irow = Math.max(1, jc - k);
                            il = Math.min(jc + 1, k + 2);
                            extra[0] = 0.0;
                            extra[1] = 0.0;
                            temp[0] = A[jc - (iskew * (jc + 1)) + ioffg - 1][jc][0];
                            temp[1] = A[jc - (iskew * (jc + 1)) + ioffg - 1][jc][1];
                            angle = 2.0 * Math.PI * ge.dlarnd(1, iseed);
                            result = zlarnd(5, iseed);
                            var = Math.cos(angle);
                            c[0] = var*result[0];
                            c[1] = var*result[1];
                            result = zlarnd(5, iseed);
                            var = Math.sin(angle);
                            s[0] = var * result[0];
                            s[1] = var * result[1];
                            if (zsym) {
                            	ct[0] = c[0];
                            	ct[1] = c[1];
                            	st[0] = s[0];
                            	st[1] = s[1];
                            }
                            else {
                            	temp[1] = -temp[1];
                            	ct[0] = c[0];
                            	ct[1] = -c[1];
                            	st[0] = s[0];
                            	st[1] = -s[1];
                            } 
                            length = ilda - (irow - (iskew * jc) + ioffg) + 1 + (ilda * (n - jc));
                            ap = new double[length][2];
                            index = 0;

                            for (i = irow - (iskew * jc) + ioffg - 1; i < ilda; i++) {
                                ap[index][0] = A[i][jc - 1][0];
                                ap[index++][1] = A[i][jc - 1][1];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < ilda; i++) {
                                    ap[index][0] = A[i][j][0];
                                    ap[index++][1] = A[i][j][1];
                                }
                            }

                            zlarot(false, jc > k, true, il, c, s, ap, ilda, extra, temp);
                            index = 0;

                            for (i = irow - (iskew * jc) + ioffg - 1; i < ilda; i++) {
                                A[i][jc - 1][0] = ap[index][0];
                                A[i][jc - 1][1] = ap[index++][1];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < ilda; i++) {
                                    A[i][j][0] = ap[index][0];
                                    A[i][j][1] = ap[index++][1];
                                }
                            }

                            length = ilda - ( ( (1 - iskew) * jc) + ioffg) + 1 + (ilda * (n - jc));
                            ap = new double[length][2];
                            index = 0;

                            for (i = ( (1 - iskew) * jc) + ioffg - 1; i < ilda; i++) {
                                ap[index][0] = A[i][jc - 1][0];
                                ap[index++][1] = A[i][jc - 1][1];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < ilda; i++) {
                                    ap[index][0] = A[i][j][0];
                                    ap[index++][1] = A[i][j][1];
                                }
                            }

                            zlarot(true, true, false, Math.min(k, n - jc) + 1, ct, st, ap, ilda, temp, dummy);
                            index = 0;

                            for (i = ( (1 - iskew) * jc) + ioffg - 1; i < ilda; i++) {
                                A[i][jc - 1][0] = ap[index][0];
                                A[i][jc - 1][1] = ap[index++][1];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < ilda; i++) {
                                    A[i][j][0] = ap[index][0];
                                    A[i][j][1] = ap[index++][1];
                                }
                            }

                            // Chase extra back up the matrix
                            icol = jc;

                            for (jch = jc - k; jch >= 1; jch -= k) {
                                zlartg(A[jch - (iskew * (icol + 1)) + ioffg][icol], extra, realc, s, dummy);
                                dummy = zlarnd(5, iseed);
                                c[0] = realc[0] * dummy[0];
                                c[1] = -realc[0] * dummy[1];
                                zmlt(-s[0], -s[1], dummy[0], dummy[1], cr, ci);
                                s[0] = cr[0];
                                s[1] = -ci[0];
                                temp[0] = A[jch - (iskew * (jch + 1)) + ioffg - 1][jch][0];
                                temp[1] = A[jch - (iskew * (jch + 1)) + ioffg - 1][jch][1];
                                if (zsym) {
                                	ct[0] = c[0];
                                	ct[1] = c[1];
                                	st[0] = s[0];
                                	st[1] = s[1];
                                }
                                else {
                                	temp[1] = -temp[1];
                                	ct[0] = c[0];
                                	ct[1] = -c[1];
                                	st[0] = s[0];
                                	st[1] = -s[1];
                                } 
                                length = ilda - ( ( (1 - iskew) * jch) + ioffg) + 1 + (ilda * (n - jch));
                                ap = new double[length][2];
                                index = 0;

                                for (i = ( (1 - iskew) * jch) + ioffg - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][jch - 1][0];
                                    ap[index++][1] = A[i][jch - 1][1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(true, true, true, k + 2, c, s, ap, ilda, temp, extra);
                                index = 0;

                                for (i = ( (1 - iskew) * jch) + ioffg - 1; i < ilda; i++) {
                                    A[i][jch - 1][0] = ap[index][0];
                                    A[i][jch - 1][1] = ap[index++][1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }

                                irow = Math.max(1, jch - k);
                                il = Math.min(jch + 1, k + 2);
                                extra[0] = 0.0;
                                extra[1] = 0.0;
                                length = ilda - (irow - (iskew * jch) + ioffg) + 1 + (ilda * (n - jch));
                                ap = new double[length][2];
                                index = 0;

                                for (i = irow - (iskew * jch) + ioffg - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][jch - 1][0];
                                    ap[index++][1] = A[i][jch - 1][1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(false, jch > k, true, il, ct, st, ap, ilda, extra, temp);
                                index = 0;

                                for (i = irow - (iskew * jch) + ioffg - 1; i < ilda; i++) {
                                    A[i][jch - 1][0] = ap[index][0];
                                    A[i][jch - 1][1] = ap[index++][1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }

                                icol = jch;
                            } // for (jch = jc-k; jch >= 1; jch -= k)
                        } // for (jc = 1; jc <= n-1; jc++)
                    } // for (k = 1; k <= uub; k++)

                    // If we need lower triangle, copy from upper. Note that
                    // the order of copying is chosen to work for 'q' -> 'b'
                    if ( (ipack != ipackg) && (ipack != 3)) {

                        for (jc = 1; jc <= n; jc++) {
                            irow = ioffst - (iskew * jc);

                            if (zsym) {
	                            for (jr = jc; jr <= Math.min(n, jc + uub); jr++) {
	                                A[jr + irow - 1][jc - 1][0] = A[jc - (iskew * jr) + ioffg - 1][jr - 1][0];
	                                A[jr + irow - 1][jc - 1][1] = A[jc - (iskew * jr) + ioffg - 1][jr - 1][1];
	                            }
                            } // if (zsym)
                            else {
                            	for (jr = jc; jr <= Math.min(n, jc + uub); jr++) {
	                                A[jr + irow - 1][jc - 1][0] = A[jc - (iskew * jr) + ioffg - 1][jr - 1][0];
	                                A[jr + irow - 1][jc - 1][1] = -A[jc - (iskew * jr) + ioffg - 1][jr - 1][1];
	                            }	
                            }
                        } // for (jc = 1; jc <= n; jc++)

                        if (ipack == 5) {

                            for (jc = n - uub + 1; jc <= n; jc++) {

                                for (jr = n + 2 - jc; jr <= (uub + 1); jr++) {
                                    A[jr - 1][jc - 1][0] = 0.0;
                                    A[jr - 1][jc - 1][1] = 0.0;
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
                    
                    for (i = 0; i < mnmin; i++) {
                        A[(1-iskew)*i + ioffg][i][0] = D[i];
                        A[(1-iskew)*i + ioffg][i][1] = 0.0;
                    }

                    for (k = 1; k <= uub; k++) {

                        for (jc = n - 1; jc >= 1; jc--) {
                            il = Math.min(n + 1 - jc, k + 2);
                            extra[0] = 0.0;
                            extra[1] = 0.0;
                            temp[0] = A[ ( (1 - iskew) * jc) + ioffg][jc - 1][0];
                            temp[1] = A[ ( (1 - iskew) * jc) + ioffg][jc - 1][1];
                            angle = 2.0 * Math.PI * ge.dlarnd(1, iseed);
                            result = zlarnd(5, iseed);
                            var = Math.cos(angle);
                            c[0] = var*result[0];
                            c[1] = var*result[1];
                            result = zlarnd(5, iseed);
                            var = Math.sin(angle);
                            s[0] = var * result[0];
                            s[1] = var * result[1];
                            if (zsym) {
                            	ct[0] = c[0];
                            	ct[1] = c[1];
                            	st[0] = s[0];
                            	st[1] = s[1];
                            }
                            else {
                            	temp[1] = -temp[1];
                            	ct[0] = c[0];
                            	ct[1] = -c[1];
                            	st[0] = s[0];
                            	st[1] = -s[1];
                            } 
                            length = ilda - ( ( (1 - iskew) * jc) + ioffg) + 1 + (ilda * (n - jc));
                            ap = new double[length][2];
                            index = 0;

                            for (i = ( (1 - iskew) * jc) + ioffg - 1; i < ilda; i++) {
                                ap[index][0] = A[i][jc - 1][0];
                                ap[index++][1] = A[i][jc - 1][1];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < ilda; i++) {
                                    ap[index][0] = A[i][j][0];
                                    ap[index++][1] = A[i][j][1];
                                }
                            }

                            zlarot(false, true, (n - jc) > k, il, c, s, ap, ilda, temp, extra);
                            index = 0;

                            for (i = ( (1 - iskew) * jc) + ioffg - 1; i < ilda; i++) {
                                A[i][jc - 1][0] = ap[index][0];
                                A[i][jc - 1][1] = ap[index++][1];
                            }

                            for (j = jc; j < n; j++) {

                                for (i = 0; i < ilda; i++) {
                                    A[i][j][0] = ap[index][0];
                                    A[i][j][1] = ap[index++][1];
                                }
                            }

                            icol = Math.max(1, jc - k + 1);
                            length = ilda - (jc - (iskew * icol) + ioffg) + 1 + (ilda * (n - icol));
                            ap = new double[length][2];
                            index = 0;

                            for (i = jc - (iskew * icol) + ioffg - 1; i < ilda; i++) {
                                ap[index][0] = A[i][icol - 1][0];
                                ap[index++][1] = A[i][icol - 1][1];
                            }

                            for (j = icol; j < n; j++) {

                                for (i = 0; i < ilda; i++) {
                                    ap[index][0] = A[i][j][0];
                                    ap[index++][1] = A[i][j][1];
                                }
                            }

                            zlarot(true, false, true, jc + 2 - icol, ct, st, ap, ilda, dummy, temp);
                            index = 0;

                            for (i = jc - (iskew * icol) + ioffg - 1; i < ilda; i++) {
                                A[i][icol - 1][0] = ap[index][0];
                                A[i][icol - 1][1] = ap[index++][1];
                            }

                            for (j = icol; j < n; j++) {

                                for (i = 0; i < ilda; i++) {
                                    A[i][j][0] = ap[index][0];
                                    A[i][j][1] = ap[index++][1];
                                }
                            }

                            // Chase extra back down the matrix
                            icol = jc;

                            for (jch = jc + k; jch <= (n - 1); jch += k) {
                                zlartg(A[jch - (iskew * icol) + ioffg - 1][icol - 1], extra, realc, s, dummy);
                                dummy = zlarnd(5, iseed);
                                c[0] = realc[0] * dummy[0];
                                c[1] = realc[0] * dummy[1];
                                zmlt(s[0], s[1], dummy[0], dummy[1], cr, ci);
                                s[0] = cr[0];
                                s[1] = ci[0];
                                temp[0] = A[ ( (1 - iskew) * jch) + ioffg][jch - 1][0];
                                temp[1] = A[ ( (1 - iskew) * jch) + ioffg][jch - 1][1];
                                if (zsym) {
                                	ct[0] = c[0];
                                	ct[1] = c[1];
                                	st[0] = s[0];
                                	st[1] = s[1];
                                }
                                else {
                                	temp[1] = -temp[1];
                                	ct[0] = c[0];
                                	ct[1] = -c[1];
                                	st[0] = s[0];
                                	st[1] = -s[1];
                                } 
                                length = ilda - (jch - (iskew * icol) + ioffg) + 1 + (ilda * (n - icol));
                                ap = new double[length][2];
                                index = 0;

                                for (i = jch - (iskew * icol) + ioffg - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][icol - 1][0];
                                    ap[index++][1] = A[i][icol - 1][1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < lda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(true, true, true, k + 2, c, s, ap, ilda, extra, temp);
                                index = 0;

                                for (i = jch - (iskew * icol) + ioffg - 1; i < ilda; i++) {
                                    A[i][icol - 1][0] = ap[index][0];
                                    A[i][icol - 1][1] = ap[index++][1];
                                }

                                for (j = icol; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }

                                il = Math.min(n + 1 - jch, k + 2);
                                extra[0] = 0.0;
                                extra[1] = 0.0;
                                length = ilda - ( ( (1 - iskew) * jch) + ioffg) + 1 + (ilda * (n - jch));
                                ap = new double[length][2];
                                index = 0;

                                for (i = ( (1 - iskew) * jch) + ioffg - 1; i < ilda; i++) {
                                    ap[index][0] = A[i][jch - 1][0];
                                    ap[index++][1] = A[i][jch - 1][1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        ap[index][0] = A[i][j][0];
                                        ap[index++][1] = A[i][j][1];
                                    }
                                }

                                zlarot(false, true, (n - jch) > k, il, ct, st, ap, ilda, temp, extra);
                                index = 0;

                                for (i = ( (1 - iskew) * jch) + ioffg - 1; i < ilda; i++) {
                                    A[i][jch - 1][0] = ap[index][0];
                                    A[i][jch - 1][1] = ap[index++][1];
                                }

                                for (j = jch; j < n; j++) {

                                    for (i = 0; i < ilda; i++) {
                                        A[i][j][0] = ap[index][0];
                                        A[i][j][1] = ap[index++][1];
                                    }
                                }

                                icol = jch;
                            } // for (jch = jc+k; jch <= n-1; jch += k)
                        } // for (jc = n-1; jc >= 1; jc--)
                    } // for (k = 1; k <= uub; k++)

                    // If we need upper triangle, copy from lower. Note that the
                    // order of copying is chosen to work for 'b' -> 'q'
                    if ( (ipack != ipackg) && (ipack != 4)) {

                        for (jc = n; jc >= 1; jc--) {
                            irow = ioffst - (iskew * jc);
                            
                            if (zsym) {
	                            for (jr = jc; jr >= Math.max(1, jc - uub); jr--) {
	                                A[jr + irow - 1][jc - 1][0] = A[jc - (iskew * jr) + ioffg - 1][jr - 1][0];
	                                A[jr + irow - 1][jc - 1][1] = A[jc - (iskew * jr) + ioffg - 1][jr - 1][1];
	                            }
                            } // if (zsym)
                            else {
                            	for (jr = jc; jr >= Math.max(1, jc - uub); jr--) {
	                                A[jr + irow - 1][jc - 1][0] = A[jc - (iskew * jr) + ioffg - 1][jr - 1][0];
	                                A[jr + irow - 1][jc - 1][1] = -A[jc - (iskew * jr) + ioffg - 1][jr - 1][1];
	                            }	
                            } // else
                        } // for (jc = n; jc >= 1; jc--)

                        if (ipack == 6) {

                            for (jc = 1; jc <= uub; jc++) {

                                for (jr = 1; jr <= (uub + 1 - jc); jr++) {
                                    A[jr - 1][jc - 1][0] = 0.0;
                                    A[jr - 1][jc - 1][1] = 0.0;
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
                
                // Ensure that the diagonal is real if Hermitian
                if (!zsym) {
                	for (jc = 1; jc <= n; jc++) {
                	    irow = ioffst + (1 - iskew)*jc;
                	    A[irow - 1][jc - 1][1] = 0.0;
                	} // for (jc = 1; jc <= n; jc++)
                } // if (!zsym)
            } // else isym != 1
        } // else if (givens)
        else {

            // Generate Banded Matrix by first rotating by random Unitary
            // matrices, then reducing the bandwidth using Householder
            // transformations.
            // Note: We should only get here if lda >= n.
            if (isym == 1) {

                // Non-symmetric -- A = U D V
                zlagge(mr, nc, llb, uub, D, A, lda, iseed, work, iinfo);
            } // if (isym == 1)
            else { // isym != 1

                // Symmetric -- A = U D U' or
            	// Hermitian -- A = U D U*
            	if (zsym) {
                    zlagsy(m, llb, D, A, lda, iseed, work, iinfo);
            	}
            	else {
            		zlaghe(m, llb, D, A, lda, iseed, work, iinfo);	
            	}
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
                        A[i][j][0] = 0.0;
                        A[i][j][1] = 0.0;
                    }
                }
            } // if (ipack == 1)
            else if (ipack == 2) {

                // 'L' -- Lower triangular, not packed
                for (j = 1; j < m; j++) {

                    for (i = 0; i <= (j - 1); i++) {
                        A[i][j][0] = 0.0;
                        A[i][j][1] = 0.0;
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

                        A[irow - 1][icol - 1][0] = A[i][j][0];
                        A[irow - 1][icol - 1][1] = A[i][j][1];
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

                        A[irow - 1][icol - 1][0] = A[i][j][0];
                        A[irow - 1][icol - 1][1] = A[i][j][1];
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
                        A[i - j + uub][j - 1][0] = A[i - 1][j - 1][0];
                        A[i - j + uub][j - 1][1] = A[i - 1][j - 1][1];
                    }
                }

                for (j = uub + 2; j <= n; j++) {

                    for (i = j - uub; i <= Math.min(j + llb, m); i++) {
                        A[i - j + uub][j - 1][0] = A[i - 1][j - 1][0];
                        A[i - j + uub][j - 1][1] = A[i - 1][j - 1][1];
                    }
                }
            } // else if (ipack >= 5)

            // If packed, zero out extraneous elements

            // Symmetric/Triangular Packed
            // zero out everything after A[irow-1][icol-1]

            if ( (ipack == 3) || (ipack == 4)) {

                for (jc = icol - 1; jc < m; jc++) {

                    for (jr = irow; jr < lda; jr++) {
                        A[jr][jc][0] = 0.0;
                        A[jr][jc][1] = 0.0;
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
                        A[jr - 1][jc - 1][0] = 0.0;
                        A[jr - 1][jc - 1][1] = 0.0;
                    }

                    for (jr = Math.max(1, Math.min(ir1, ir2 - jc)); jr <= lda; jr++) {
                        A[jr - 1][jc - 1][0] = 0.0;
                        A[jr - 1][jc - 1][1] = 0.0;
                    }
                } // for (jc = 1; jc <= n; jc++)
            } // else if (ipack >= 5)
        } // if (ipack != ipackg)

        return;
    } // zlatms
    
    /**
     * This is a port of LAPACK auxiliary routine ZLAGHE version 3.7.0.  Provided by the Univ. of Tennessee, Univ.
     * of California Berkeley, Univ.of Colorado Denver and NAG Ltd. Decmeber 2016.
     * zlaghe generates a complex hermitian matrix A, by pre- and post- multiplying a real diagonal matrix D
     * with a random unitary matrix:
     * A = U*D*U'.  The semi-bandwith may then be reduced to k by additional unitary transformations.
     * @param n input integer The order of the matrix A.  n >= 0.
     * @param k input integer The number of nonzero subdiagonals within the band of A.  0 <= k <= n-1.
     * @param d input double[] array, dimension n.  The diagonal elements of tghe diaognla matrix D.
     * @param A output double [][]][2] complex array dimension (lda,n) The generated n by n hermitian matrix A
     *          (the full matrix is stored).
     * @param lda input integer The leading dimension of the array A.  lda >= n.
     * @param iseed input/output integer[] array, dimension (4).  On entryh, the seed of the random number
     *          generator; the array elements must be betewen 0 and 4095, and iseed(3) must be odd.  On exit,
     *          the seed is updated.
     * @param work output double [][2] complex array, dimension (2*n)
     * @paran info output int[] inf[0] = successful exit
     *                    < 0: if info[0] = -i,the i-th argument had an illegal value.
     */
     private void zlaghe(int n, int k, double d[], double A[][][], int lda, int iseed[], double work[][], int info[]) {
    	 int i, j, m;
    	 double wn;
    	 double alpha[] = new double[2];
    	 double tau[] = new double[2];
    	 double wa[] = new double[2];
    	 double wb[] = new double[2];
    	 int p;
    	 double cr[] = new double[1];
    	 double ci[] = new double[1];
    	 double ratio;
    	 double work2[][] = new double[n][2];
    	 double B[][][];
    	 double beta[] = new double[2];
    	 double result[] = new double[2];
    	 double alp[] = new double[2];
    	 double x[][];
    	 double cr2[]= new double[1];
    	 double ci2[] = new double[1];
    	 
    	 // Test the input arguments
    	 info[0] = 0;
    	 if (n < 0) {
    		 info[0] = -1;
    	 }
    	 else if ((k < 0) || (k > (n-1))) {
    		 info[0] = -2;
    	 }
    	 else if (lda < Math.max(1,n)) {
    		 info[0] = -5;
    	 }
    	 if (info[0] < 0) {
    		 MipavUtil.displayError("In zlaghe info[0] = " + info[0]);
    		 return;
    	 }
    	 
    	 // Initialize lower triangle of A to diagonal matrix
    	 
    	 for (j = 0; j < n; j++) {
    	     for (i = j+1; i < n; i++) {
    	    	 for (p = 0; p < 2; p++) {
    	    		 A[i][j][p] = 0.0;
    	    	 }
    	     }
    	 } // for (j = 0; j < n; j++)
    	 for (i = 0; i < n; i++) {
    		A[i][i][0] = d[i];
    	 }
    	 
    	 // Generate lower triangle of hermitian matrix
    	 for (i = n-i+1; i >= 1; i--) {
    	     // Generate random reflection
    		 zlarnv(3, iseed, n-i+1, work);
    		 wn = dznrm2(n-i+1, work, 1);
    		 ratio = wn/zabs(work[0][0], work[0][1]);
    		 wa[0] = ratio * work[0][0];
    		 wa[1] = ratio * work[0][1];
    		 if (wn == 0.0) {
    			 tau[0] = 0.0;
    			 tau[1] = 0.0;
    		 }
    		 else {
    			 wb[0] = work[0][0] + wa[0];
    			 wb[1] = work[0][1] + wa[1];
    			 zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
    			 for (j = 0; j < n-i; j++) {
    				 zmlt(cr[0], ci[0], work[1+j][0], work[1+j][1], cr2, ci2);
    				 work[1+j][0] = cr2[0];
    				 work[1+j][1] = ci2[0];
    			 }
    			 work[0][0] = 1.0;
    			 work[0][1] = 0.0;
    			 zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
    			 tau[0] = cr[0];
    			 tau[1] = 0.0;
    		 } // else
    		 
    		 // apply random reflection to A[i:n-1][i:n-1] from the left and the right
    		 
    		 // compute y := tau * A * u
    		 B = new double[n-i+1][n-i+1][2];
    		 for (j = 0; j < n-i+1; j++) {
    			 for (m = 0; m < n-i+1; m++) {
    				 for (p = 0; p < 2; p++) {
    					 B[j][m][p] = A[i-1+j][i-1+m][p];
    				 }
    			 }
    		 }
    		 beta[0] = 0.0;
    		 beta[1] = 0.0;
    		 zhemv('L',n-i+1, tau, B, n-i+1, work, 1, beta, work2, 1);
    		 // Compute v := y - 1/2 * tau * (y,u) * u
    		 result = zdotc(n-i+1, work2, 1 , work, 1);
    		 zmlt(-0.5*tau[0], -0.5*tau[1], result[0], result[1], cr, ci);
    		 alpha[0] = cr[0];
    		 alpha[1] = ci[0];
    		 zaxpy(n-i+1, alpha, work, 1, work2, 1);
    		// Apply the transformation as a rank-2 update to A[i:n-1][i:n-1]
    		 alp[0] = -1.0;
    		 alp[1] = 0.0;
    		 zher2('L', n-i+1, alp, work, 1, work2, 1, B, n-i+1);
    		 for (j = 0; j < n-i+1; j++) {
    			 for (m = 0; m < n-i+1; m++) {
    				 for (p = 0; p < 2; p++) {
    					 A[i-1+j][i-1+m][p] = B[j][m][p];
    				 }
    			 }
    		 } // for (j = 0; j < n-i+1; j++)
    	 } // for (i = n-i+1; i >= 1; i--)
    	 
    	 // Reduce number of subdiagonals to k
    	 for (i = 1; i <= n-1-k; i++) {
    	     // Generate reflection to annihilate A[k+i:n-1][i-1]
    		 x = new double[n-k-i+1][2];
    		 for (j = 0; j < n-k-i+1; j++) {
    			 for (p = 0; p < 2; p++) {
    			     x[j][p] = A[k+i-1+j][i-1][p];
    			 }
    		 }
    		 wn = dznrm2(n-k-i+1, x, 1);
    		 ratio = wn/zabs(A[k+i-1][i-1][0], A[k+i-1][i-1][1]);
    		 wa[0] = ratio * A[k+i-1][i-1][0];
    		 wa[1] = ratio * A[k+i-1][i-1][1];
    		 if (wn == 0.0) {
    			 tau[0] = 0.0;
    			 tau[1] = 0.0;
    		 }
    		 else {
    			 wb[0] = A[k+i-1][i-1][0] + wa[0];
    			 wb[1] = A[k+i-1][i-1][1] + wa[1];
    			 zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
    			 for (j = 0; j < n-k-i; j++) {
    				 zmlt(cr[0], ci[0], A[k+i+j][i-1][0], A[k+i+j][i-1][1], cr2, ci2);
    				 A[k+i+j][i-1][0] = cr2[0];
    				 A[k+i+j][i-1][1] = ci2[0];
    			 }
    			 A[k+i-1][i-1][0] = 1.0;
    			 A[k+i-1][i-1][1] = 0.0;
    			 zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
    			 tau[0] = cr[0];
    			 tau[1] = 0.0;
    		 }
    		 
    		 // Apply reflection ot A[k+i-1:n-1][i:k+i-2] from the left
    		 B = new double[n-k-i+1][k-1][2];
    		 for (j = 0; j < n-k-i+1; j++) {
    			 for (m = 0; m < k-1; m++) {
    				 for (p = 0; p < 2; p++) {
    					 B[j][m][p] = A[k+i-1+j][i+m][p];
    				 }
    			 }
    		 }
    		 x = new double[n-k-i+1][2];
    		 for (j = 0; j < n-k-i+1; j++) {
    			 for (p = 0; p < 2; p++) {
    				 x[j][p] = A[k+i-1+j][i-1][p];
    			 }
    		 }
    		 alp[0] = 1.0;
    		 alp[1] = 0.0;
    		 beta[0] = 0.0;
    		 beta[1] = 0.0;
    		 zgemv('C', n-k-i+1, k-1, alp, B, n-k-i+1, x, 1, beta, work, 1);
    		 alp[0] = -tau[0];
    		 alp[1] = -tau[1];
    		 zgerc(n-k-i+1, k-1, alp, x, 1, work, 1, B, n-k-i+1);
    		 for (j = 0; j < n-k-i+1; j++) {
    			 for (m = 0; m < k-1; m++) {
    				 for (p = 0; p < 2; p++) {
    					 A[k+i-1+j][i+m][p] = B[j][m][p];
    				 }
    			 }
    		 }
    		 
    		 // Apply refelction to A[k+i-1:n-1][k+i-1:n-1] from the left and from the right
    		 
    		 // Compute y := tau * A * u
    		 B = new double[n-k-i+1][n-k-i+1][2];
    		 for (j = 0; j < n-k-i+1; j++) {
    			 for (m = 0; m < n-k-i+1; m++) {
    				 for (p = 0; p < 2; p++) {
    					 B[j][m][p] = A[k+i-1+j][k+i-1+m][p];
    				 }
    			 }
    		 }
    		 x = new double[n-k-i+1][2];
    		 for (j = 0; j < n-k-i+1; j++) {
    			 for (p = 0; p < 2; p++) {
    				 x[j][p] =  A[k+i-1+j][i-1][p];
    			 }
    		 }
    		 beta[0] = 0.0;
    		 beta[1] = 0.0;
    		 zhemv('L', n-k-i+1, tau, B, n-k-i+1, x, 1, beta, work, 1);
    		 
    		 // Compute v:= y - 1/2 * tau * (y,u) * u
    		 result = zdotc(n-k-i+1, work, 1, x, 1);
    		 zmlt(-0.5*tau[0], -0.5*tau[1], result[0], result[1], cr, ci);
    		 alpha[0] = cr[0];
    		 alpha[1] = ci[0];
    		 zaxpy(n-k-i+1, alpha, x, 1, work, 1);
    		 // Apply rank-2 update to A[k+i-1:n-1][k+i-1:n-1]
    		 alp[0] = -1.0;
    		 alp[1] = 0.0;
    		 zher2('L', n-k-i+1, alp, x, 1, work, 1, B, n-k-i+1);
    		 for (j = 0; j < n-k-i+1; j++) {
    			 for (m = 0; m < n-k-i+1; m++) {
    				 for (p = 0; p < 2; p++) {
    					 A[k+i-1+j][k+i-1+m][p] = B[j][m][p];
    				 }
    			 }
    		 }
    		 A[k+i-1][i-1][0] = -wa[0];
    		 A[k+i-1][i-1][1] = -wa[1];
    		 for (j = k+i+i; j <= n; j++) {
    			 A[j-1][i-1][0] = 0.0;
    			 A[j-1][i-1][1] = 0.0;
    		 }
   	     } // for (i = 1; i <= n-1-k; i++)
    	 
    	 // Store full hermitian matrix
    	 
    	 for (j = 0; j < n; j++) {
    		 for (i = j+1; j < n; j++) {
    			 A[j][i][0] = A[i][j][0];
    			 A[j][i][1] = -A[i][j][1];
    		 }
    	 }
    	 
    	 return;
     } // zlaghe
    
     /**
      * This is a port of version 3.7.0 BLAS level2 routine ZHEMV proivded by Univ. of Tennessee, Univ. of California
      * Berkeley, University of Colorado Denver and NAG Ltd., December, 2016  Originally written 22-October-1986 by
      * Jack Dongarra, Argonne National Lab, Jeremy Du Croz, Nag Central Office, Sven Hammarling, Nag Central Office,
      * and Richard Hanson, Sandia National Labs.
      * zhemv performs the matrix-vector operation y := alpha*A*x + beta*y
      * where alpha and beta are scalars, x and y are n element vectors and A is an n by n hermitian matrix.
      * @param uplo input char On entry, uplo specifies whther the upper or lower triangular part of the
      *             array A is to be referenced as follows:
      *             uplo = 'U' or 'u' Only the upper triangular part of A is to be referenced.
      *             uplo = 'L' or 'l' Only the lower triangular part of A is to be referenced.
      * @param n input integer On entry, n specifies the order of the matrix A.  n must be at least zero.
      * @param alpha input double[2] complex scalar
      * @param A input double[][][2] complex array, of dimension (lda, n)
      *             Before entry with uplo = 'U' or 'u', the leading n by n upper triangular part of the array A
      *             must contain the upper triangular part of the hermitian matrix and the strictly lower part of 
      *             A is not referenced.  Before entry with uplo = 'L' or 'l', the leading n by n lower triangular
      *             part of the array A must contain the lower triangular part of the hermitian matrix and the
      *             strictly upper triangular part of A is not referenced.  Note that the imaginary parts of the
      *             diagonal elements need not be set and are assumed to be zero.
      * @param lda input integer On entry, lda specifies the first dimension of A in tbe calling (sub) program.
      *             lda must be at least max(1,n).
      * @param x input double[][2] complex array, dimension at least (1 + (n-1)*abs(incx)).
      * @param incx input integer  On entry, incx specifies the increment for the elements of x.  incx
      *             must not be zero.
      * @param beta input double[2] complex scalar.  When beta is supplied as zero, y need not be set on input.
      * @param y input/output double[][2] complex array, dimension at least (1 + (n-1)*abs(incy))
      * @param incy input integer  incy specifies the increment for the elements of y.  incy must not be zero.
      */
      private void zhemv(char uplo, int n, double alpha[], double A[][][], int lda, double x[][], int incx,
    		  double beta[], double y[][], int incy) {
    	  double temp1[] = new double[2];
    	  double temp2[] = new double[2];
    	  double cr[] = new double[1];
    	  double ci[] = new double[1];
    	  int i, info, ix, iy, j, jx, jy, kx, ky;
    	  
    	  // Test the input parameters
    	  info = 0;
    	  if ((uplo != 'U') && (uplo != 'u') && (uplo != 'L') && (uplo != 'l')) {
    		  info = 1;
    	  }
    	  else if (n < 0) {
    		  info = 2;
    	  }
    	  else if (lda < Math.max(1, n)) {
    		  info = 5;
    	  }
    	  else if (incx == 0) {
    		  info = 7;
    	  }
    	  else if (incy == 0) {
    		  info = 10;
    	  }
    	  if (info != 0) {
    		  MipavUtil.displayError("zhemv had info = " + info);
    		  return;
    	  }
    	  
    	  // Quick return if possible
    	  if ((n == 0) || (((alpha[0] == 0) && (alpha[1] == 0)) && ((beta[0] == 1.0) && (beta[1] == 0.0)))) {
    		  return;
    	  }
    	  
    	  // Set up the start points in x and y.
    	  if (incx > 0) {
    		  kx = 1;
    	  }
    	  else {
    		  kx = 1 - (n-1)*incx;
    	  }
    	  if (incy > 0) {
    		  ky = 1;
    	  }
    	  else {
    		  ky = 1 - (n-1)*incy;
    	  }
    	  
    	  // Start the operations.  In this version the elements of A are accessed sequentially with one pass
    	  // through the triangular part of A.
    	  
    	  // First form y := beta*y
    	  if ((beta[0] != 1.0) || (beta[1] != 0.0)) {
    	      if (incy == 1) {
    	    	  if ((beta[0] == 0.0) && (beta[1] == 0.0)) {
    	    		  for (i = 0; i < n; i++) {
    	    			  y[i][0] = 0.0;
    	    			  y[i][1] = 0.0;
    	    		  }
    	    	  } // if ((beta[0] == 0.0) && (beta[1] == 0.0))
    	    	  else {
    	    		  for (i = 0; i < n; i++) {
    	    			  zmlt(beta[0], beta[1], y[i][0], y[i][1], cr ,ci);
    	    			  y[i][0] = cr[0];
    	    			  y[i][1] = ci[0];
    	    		  }
    	    	  } // else beta != 0
    	      } // if (incy == 1)
    	      else { // incy != 1
    	    	  iy = ky-1;
    	    	  if ((beta[0] == 0.0) && (beta[1] == 0.0)) {
    	    	    for (i = 1; i <= n; i++) {
    	    	    	y[iy][0] = 0.0;
    	    	    	y[iy][1] = 0.0;
    	    	    	iy = iy + incy;
    	    	    }
    	    	  } // if ((beta[0] == 0.0) && (beta[1] == 0.0))
    	    	  else {
    	    		   for (i = 1; i <= n; i++) {
    	    			   zmlt(beta[0], beta[1], y[iy][0], y[iy][1], cr, ci);
    	    			   y[iy][0] = cr[0];
    	    			   y[iy][1] = ci[0];
    	    			   iy = iy + incy;
    	    		   }
    	    	  } // else beta != 0
    	      } // else incy != 1
    	  } // if ((beta[0] != 1.0) || (beta[1] != 0.0))
    	  if ((alpha[0] == 0.0) && (alpha[1] == 0.0)) {
    		  return;
    	  }
    	  
    	  if ((uplo == 'U') || (uplo == 'u')) {
    	
              // Form y when A is stored in upper triangle.
    		  
    		  if ((incx == 1) && (incy == 1)) {
    		      for (j = 0; j < n; j++) {
    		          zmlt(alpha[0], alpha[1], x[j][0], x[j][1], cr, ci);
    		          temp1[0] = cr[0];
    		          temp1[1] = ci[0];
    		          temp2[0] = 0.0;
    		          temp2[1] = 0.0;
    		          for (i = 0; i <= j-1; i++) {
    		        	  zmlt(temp1[0], temp1[1], A[i][j][0], A[i][j][1], cr, ci);
    		        	  y[i][0] = y[i][0] + cr[0];
    		        	  y[i][1] = y[i][1] + ci[0];
    		        	  zmlt(A[i][j][0], -A[i][j][1], x[i][0], x[i][1], cr, ci);
    		        	  temp2[0] = temp2[0] + cr[0];
    		        	  temp2[1] = temp2[1] + ci[0];
    		          } // for (i = 0; i <= j-1; i++)
    		          zmlt(alpha[0], alpha[1], temp2[0], temp2[1], cr, ci);
    		          y[j][0] = y[j][0] + temp1[0]*A[j][j][0] + cr[0];
    		          y[j][1] = y[j][1] + temp1[1]*A[j][j][0] + ci[0];
    		      } // for (j = 0; j < n; j++)
    		  } // if ((incx == 1) && (incy == 1))
    		  else {
    			  jx = kx-1;
    			  jy = ky-1;
    			  for (j = 0; j < n; j++) {
    				  zmlt(alpha[0], alpha[1], x[jx][0], x[jx][1], cr, ci);
    				  temp1[0] = cr[0];
    				  temp1[1] = ci[0];
    				  temp2[0] = 0.0;
    				  temp2[1] = 0.0;
    				  ix = kx-1;
    				  iy = ky-1;
    				  for (i = 0; i <= j-1; i++) {
    				      zmlt(temp1[0], temp1[1], A[i][j][0], A[i][j][1], cr, ci);
    				      y[iy][0] = y[iy][0] + cr[0];
    				      y[iy][1] = y[iy][1] + ci[0];
    				      zmlt(A[i][j][0], -A[i][j][1], x[ix][0], x[ix][1], cr, ci);
    				      temp2[0] = temp2[0] + cr[0];
    				      temp2[1] = temp2[1] + ci[0];
    				      ix = ix + incx;
    				      iy = iy + incy;
    				  } // for (i = 0; i <= j-1; i++)
    				  zmlt(alpha[0], alpha[1], temp2[0], temp2[1], cr, ci);
    				  y[jy][0] = y[jy][0] + temp1[0]*A[j][j][0] + cr[0];
    				  y[jy][1] = y[jy][1] + temp1[1]*A[j][j][0] + ci[0];
    				  jx = jx + incx;
    				  jy = jy + incy;
    			  } // for (j = 0; j < n; j++)
    		  } // else
    	  } // if ((uplo == 'U') || (uplo == 'u'))
    	  else {
    		  // Form y when A is stored in the lower triangle
    		  if ((incx == 1) && (incy == 1)) {
    			  for (j = 0; j < n; j++) {
    				  zmlt(alpha[0], alpha[1], x[j][0], x[j][1], cr, ci);
    				  temp1[0] = cr[0];
    				  temp1[1] = ci[0];
    				  temp2[0] = 0.0;
    				  temp2[1] = 0.0;
    				  y[j][0] = y[j][0] + temp1[0]*A[j][j][0];
    				  y[j][1] = y[j][1] = temp1[1]*A[j][j][0];
    				  for (i = j+1; i < n; i++) {
    					  zmlt(temp1[0], temp1[1], A[i][j][0], A[i][j][1], cr, ci);
    					  y[i][0] = y[i][0] + cr[0];
    					  y[i][1] = y[i][1] + ci[0];
    					  zmlt(A[i][j][0], -A[i][j][1], x[i][0], x[i][1], cr, ci);
    					  temp2[0] = temp2[0] + cr[0];
    					  temp2[1] = temp2[1] + ci[0];
    				  }
    				  zmlt(alpha[0], alpha[1], temp2[0], temp2[1], cr, ci);
    				  y[j][0] = y[j][0] + cr[0];
    				  y[j][1] = y[j][1] + ci[0];
    			  } // for (j = 0; j < n; j++)
    		  } // if ((incx == 1) && (incy == 1))
    		  else {
    			  jx = kx-1;
    			  jy = ky-1;
    			  for (j = 0; j < n; j++) {
    				  zmlt(alpha[0], alpha[1], x[jx][0], x[jx][1], cr, ci);
    				  temp1[0] = cr[0];
    				  temp1[1] = ci[0];
    				  temp2[0] = 0.0;
    				  temp2[1] = 0.0;
    				  y[jy][0] = y[jy][0] + temp1[0]*A[j][j][0];
    				  y[jy][1] = y[jy][1] + temp1[1]*A[j][j][0];
    				  ix = jx - 1;
    				  iy = jy - 1;
    				  for (i = j+1; i < n; i++) {
    					  ix = ix + incx;
    					  iy = iy + incy;
    					  zmlt(temp1[0], temp1[1], A[i][j][0], A[i][j][1], cr, ci);
    					  y[iy][0] = y[iy][0] + cr[0];
    					  y[iy][1] = y[iy][1] + ci[0];
    					  zmlt(A[i][j][0], -A[i][j][1], x[ix][0], x[ix][1], cr, ci);
    					  temp2[0] = temp2[0] + cr[0];
    					  temp2[1] = temp2[1] + ci[0];
    				  } // for (i = j+1; i < n; i++)
    				  zmlt(alpha[0], alpha[1], temp2[0], temp2[1], cr, ci);
    				  y[jy][0] = y[jy][0] + cr[0];
    				  y[jy][1] = y[jy][1] + ci[0];
    				  jx = jx + incx;
    				  jy = jy + incy;
    			  } // for (j = 0; j < n; j++)
    		  }
    	  } // else
      } // zhemv
      
      /**
       * This is a port of version 3.7.0 BLAS level2 routine ZHER2 proivded by Univ. of Tennessee, Univ. of California
       * Berkeley, University of Colorado Denver and NAG Ltd., December, 2016  Originally written 22-October-1986 by
       * Jack Dongarra, Argonne National Lab, Jeremy Du Croz, Nag Central Office, Sven Hammarling, Nag Central Office,
       * and Richard Hanson, Sandia National Labs.
       * zher2 performs the hermitian rank 2 operation A := alpha*x*y**H + conj(alpha)*y*x**H + A,
       * where alpha is a scalar, x and y are n element vectors, and A is an n by n hermitian matrix.
       * @param uplo input char On entry, uplo specifies whether the upper or lower triangular part of the array A is
       *             to be referenced as follows:
       *             uplo = 'U' or 'u' Only the upper triangular part of A is to be referenced.
       *             uplo = 'L' or 'l' Only the lower triangular part of A is to be referenced.
       * @param n input integer On entry, n specifies the order of the matrix A.  n must be at least zero.
       * @param alpha input double[2] complex scalar
       * @param x input double[][2] complex array, dimension at least (1 + (n-1)*abs(incx)).
       * @param incx input integer On entry, incx specifies the increment for the elements of x.  incx must not be zero.
       * @param y input double[][2] complex array, dimension at least (1 + (n-1)*abs(incy)).
       * @param A input/output double[][][2] complex array, dimension (lda, n)  Before entry with uplo = 'U' or
       *              'u', the leading n by n upper triangular part of the array A must contain the upper triangular
       *              part of the hermitian matrix and the strictly lower part of A is not referenced.  On exit,
       *              the upper triangular part of A is overwritten by the upper triangular part of the updated matrix.
       *              Before entry iwth uplo = 'L' or 'l', the leading n by n lower triangular part of the array A
       *              must contain the lower triangular part of the hermitian matrix and the strictly upper part of
       *              A is not referenced.  On exit, the lower triangular part of the array A is overwritten by the
       *              lower triangular part of the updated matrix.  Note that the imaginary parts of the diagonal 
       *              elements need not be set, they are assumed to be zero, and on exit they are set to zero.
       * @param lda input integer On entry, lda specifies the first dimension of A as declared in the calling
       *             (sub) program.  lda must be at least max(1,n).
       */
      private void zher2(char uplo, int n, double alpha[], double x[][], int incx, double y[][], int incy,
    		  double A[][][], int lda) {
          double temp1[] = new double[2];
          double temp2[] = new double[2];
          int i, info, ix, iy, j; 
          int kx = 0;
          int ky = 0;
          int jx = 0;
          int jy = 0;
          double cr[] = new double[1];
          double ci[] = new double[1];
          double cr2[] = new double[1];
          double ci2[] = new double[1];
          
          // Test the input parameters
          info = 0;
          if ((uplo != 'U') && (uplo != 'u') && (uplo != 'L') && (uplo != 'l')) {
        	  info = 1;
          }
          else if (n < 0) {
        	   info = 2;
          }
          else if (incx == 0) {
        	  info = 5;
          }
          else if (incy == 0) {
        	  info = 7;
          }
          else if (lda < Math.max(1, n)) {
        	  info = 9;
          }
          if (info != 0) {
        	  MipavUtil.displayError("zher2 has info = " + info);
        	  return;
          }
          
          // Quick return if possible.
          
          if ((n == 0) || ((alpha[0] == 0.0) && (alpha[1] == 0.0))) {
        	  return;
          }
          
          // Set up the start points in x and y if the increments are not both unity.
          if ((incx != 1) || (incy != 1)) {
              if (incx > 0) {
            	  kx = 1;
              }
              else {
            	  kx = 1 - (n-1)*incx;
              }
              if (incy > 0) {
            	  ky = 1;
              }
              else {
            	  ky = 1 - (n-1)*incy;
              }
              jx = kx-1;
              jy = ky-1;
          } // if ((incx != 1) || (incy != 1))
          
          // Start the operations.  In this version the elements of A are accessed sequentially with one pass
          // through the triangular part of A.
          if ((uplo == 'U') || (uplo == 'u')) {
              // Form A when A is stored in the upper triangle.
        	  
        	  if ((incx == 1) && (incy == 1)) {
        		  for (j = 0; j < n; j++) {
        			  if ((x[j][0] != 0.0) || (x[j][1] != 0.0) || (y[j][0] != 0.0) || (y[j][1] != 0.0)) {
        			      zmlt(alpha[0], alpha[1], y[j][0], -y[j][1], cr, ci);
        			      temp1[0] = cr[0];
        			      temp1[1] = ci[0];
        			      zmlt(alpha[0], alpha[1], x[j][0], x[j][1], cr, ci);
        			      temp2[0] = cr[0];
        			      temp2[1] = -ci[0];
        			      for (i = 0; i <= j-1; i++) {
        			          zmlt(x[i][0], x[i][1], temp1[0], temp1[1], cr, ci); 
        			          zmlt(y[i][0], y[i][1], temp2[0], temp2[1], cr2, ci2);
        			          A[i][j][0] = A[i][j][0] + cr[0] + cr2[0];
        			          A[i][j][1] = A[i][j][1] + ci[0] + ci2[0];
        			      } // for (i = 0; i <= j-1; i++)
        			      zmlt(x[j][0], x[j][1], temp1[0], temp1[1], cr, ci);
        			      zmlt(y[j][0], y[j][1], temp2[0], temp2[1], cr2, ci2);
        			      A[j][j][0] = A[j][j][0] + cr[0] + cr2[0];
        			      A[j][j][1] = 0.0;
        			  } // if ((x[j][0] != 0.0) || (x[j][1] != 0.0) || (y[j][0] != 0.0) || (y[j][1] != 0.0))
        			  else {
        				  A[j][j][1] = 0.0;
        			  }
        		  } // for (j = 0; j < n; j++)
        	  } // if ((incx == 1) && (incy == 1))
        	  else {
        		  for (j = 0; j < n; j++) {
        			  if ((x[jx][0] != 0.0) || (x[jx][1] != 0.0) || (y[jy][0] != 0.0) || (y[jy][1] != 0.0)) {
        			      zmlt(alpha[0], alpha[1], y[jy][0], -y[jy][1], cr, ci);
        			      temp1[0] = cr[0];
        			      temp1[1] = ci[0];
        			      zmlt(alpha[0], alpha[1], x[jx][0], x[jx][1], cr, ci);
        			      temp2[0] = cr[0];
        			      temp2[1] = -ci[0];
        			      ix = kx-1;
        			      iy =ky-1;
        			      for (i = 0; i <= j-1; i++) {
        			    	  zmlt(x[ix][0], x[ix][1], temp1[0], temp1[1], cr, ci); 
        			          zmlt(y[iy][0], y[iy][1], temp2[0], temp2[1], cr2, ci2);
        			          A[i][j][0] = A[i][j][0] + cr[0] + cr2[0];
        			          A[i][j][1] = A[i][j][1] + ci[0] + ci2[0]; 
        			          ix = ix + incx;
        			          iy = iy + incy;
        			      } // for (i = 0; i <= j-1; i++)
        			      zmlt(x[jx][0], x[jx][1], temp1[0], temp1[1], cr, ci);
        			      zmlt(y[jy][0], y[jy][1], temp2[0], temp2[1], cr2, ci2);
        			      A[j][j][0] = A[j][j][0] + cr[0] + cr2[0];
        			      A[j][j][1] = 0.0;
        			  } // if ((x[jx][0] != 0.0) || (x[jx][1] != 0.0) || (y[jy][0] != 0.0) || (y[jy][1] != 0.0))
        			  else {
        				  A[j][j][1] = 0.0;
        			  }
        			  jx = jx + incx;
        			  jy = jy + incy;
        		  } // for (j = 0; j < n; j++)
        	  } // else
          } // if ((uplo == 'U') || (uplo == 'u'))
          else {
        	  // Form A when A is stored in the lower triangle
        	  if ((incx == 1) && (incy == 1)) {
        		  for (j = 0; j < n; j++) {
        			  if ((x[j][0] != 0.0) || (x[j][1] != 0.0) || (y[j][0] != 0.0) || (y[j][1] != 0.0)) {
        			      zmlt(alpha[0], alpha[1], y[j][0], -y[j][1], cr, ci);
        			      temp1[0] = cr[0];
        			      temp1[1] = ci[0];
        			      zmlt(alpha[0], alpha[1], x[j][0], x[j][1], cr, ci);
        			      temp2[0] = cr[0];
        			      temp2[1] = -ci[0];
        			      zmlt(x[j][0], x[j][1], temp1[0], temp1[1], cr, ci);
        			      zmlt(y[j][0], y[j][1], temp2[0], temp2[1], cr2, ci2);
        			      A[j][j][0] = A[j][j][0] + cr[0] + cr2[0];
        			      A[j][j][1] = 0.0;
        			      for (i = j+1; i < n; i++) {
        			          zmlt(x[i][0], x[i][1], temp1[0], temp1[1], cr, ci); 
        			          zmlt(y[i][0], y[i][1], temp2[0], temp2[1], cr2, ci2);
        			          A[i][j][0] = A[i][j][0] + cr[0] + cr2[0];
        			          A[i][j][1] = A[i][j][1] + ci[0] + ci2[0];
        			      } // for (i = j+1; i < n; i++)
        			  } // if ((x[j][0] != 0.0) || (x[j][1] != 0.0) || (y[j][0] != 0.0) || (y[j][1] != 0.0))
        			  else {
        				  A[j][j][1] = 0.0;
        			  }
        		  } // for (j = 0; j < n; j++)
        	  } // if ((incx == 1) && (incy == 1))
        	  else {
        		  for (j = 0; j < n; j++) {
        			  if ((x[jx][0] != 0.0) || (x[jx][1] != 0.0) || (y[jy][0] != 0.0) || (y[jy][1] != 0.0)) {
        			      zmlt(alpha[0], alpha[1], y[jy][0], -y[jy][1], cr, ci);
        			      temp1[0] = cr[0];
        			      temp1[1] = ci[0];
        			      zmlt(alpha[0], alpha[1], x[jx][0], x[jx][1], cr, ci);
        			      temp2[0] = cr[0];
        			      temp2[1] = -ci[0];
        			      zmlt(x[jx][0], x[jx][1], temp1[0], temp1[1], cr, ci);
        			      zmlt(y[jy][0], y[jy][1], temp2[0], temp2[1], cr2, ci2);
        			      A[j][j][0] = A[j][j][0] + cr[0] + cr2[0];
        			      A[j][j][1] = 0.0;
        			      ix = jx;
        			      iy = jy;
        			      for (i = j+1; i < n; i++) {
        			    	  ix = ix + incx;
        			          iy = iy + incy;
        			    	  zmlt(x[ix][0], x[ix][1], temp1[0], temp1[1], cr, ci); 
        			          zmlt(y[iy][0], y[iy][1], temp2[0], temp2[1], cr2, ci2);
        			          A[i][j][0] = A[i][j][0] + cr[0] + cr2[0];
        			          A[i][j][1] = A[i][j][1] + ci[0] + ci2[0]; 
        			      } // for (i = j+1; i < n; i++)
        			  } // if ((x[jx][0] != 0.0) || (x[jx][1] != 0.0) || (y[jy][0] != 0.0) || (y[jy][1] != 0.0))
        			  else {
        				  A[j][j][1] = 0.0;
        			  }
        			  jx = jx + incx;
        			  jy = jy + incy;
        		  } // for (j = 0; j < n; j++)
        	  } // else
          } // else 
      } // zher2
     
     /**
      * This is a port of version 3.7.0 LAPACK auxiliary test routine ZLAGGE Original ZLAGGE created by Univ. of Tennessee,
      * Univ. of California Berkeley, and NAG Ltd., December, 2016 zlagge generates a complex general m by n matrix A, by
      * pre- and post- multiplying a real diagonal matrix D with random unitary matrices: A = U*D*V. The lower and
      * upper bandwidths may then be reduced to kl and ku by additional unitary transformations.
      * 
      * @param m input int The number of rows of the matrix A. m >= 0.
      * @param n input int The number of columns of the matrix A. n >= 0.
      * @param kl input int The number of nonzero subdiagonals within the band of A. 0 <= kl <= m-1.
      * @param ku input int The number of nonzero superdiagonals within the band of A. 0 <= ku <= n-1.
      * @param D input double[] of dimension (min(m,n)) The diagonal elements of the diagonal matrix D
      * @param A output double[][][2] complex of dimension (lda,n) The generated m by n matrix A.
      * @param lda input int The leading dimension of the array A. lda >= m.
      * @param iseed input/output int[] of dimension 4 On entry, the seed of the random number generator; the array
      *            elements must be between 0 and 4095, and iseed[3] must be odd. On exit, the seed is updated.
      * @param work workspace double[][2] complex of dimension (m+n)
      * @param info output int[] = 0: successful exit < 0: If info = -i, the i-th argument had an illegal value
      */
     public void zlagge(final int m, final int n, final int kl, final int ku, final double[] D, final double[][][] A,
             final int lda, final int[] iseed, final double[][] work, final int[] info) {
         int i;
         int j;
         int k;
         double tau[] = new double[2];
         double wa[] = new double[2];
         double wb[] = new double[2];
         double wn;
         double[][][] B;
         double[][] work2;
         double[][] x;
         double ratio;
         double cr[] = new double[1];
         double ci[] = new double[1];
         double cr2[] = new double[1];
         double ci2[] = new double[1];
         double alpha[] = new double[2];
         double beta[] = new double[2];
         int p;

         // Test the input arguments
         info[0] = 0;

         if (m < 0) {
             info[0] = -1;
         } else if (n < 0) {
             info[0] = -2;
         } else if ( (kl < 0) || (kl > (m - 1))) {
             info[0] = -3;
         } else if ( (ku < 0) || (ku > (n - 1))) {
             info[0] = -4;
         } else if (lda < Math.max(1, m)) {
             info[0] = -7;
         }

         if (info[0] < 0) {
             MipavUtil.displayError("Error zlagge had info[0] = " + info[0]);

             return;
         }

         // Initialize A to diagonal matrix
         for (j = 0; j < n; j++) {

             for (i = 0; i < m; i++) {
                 A[i][j][0] = 0.0;
                 A[i][j][1] = 0.0;
             }
         }

         for (i = 0; i < Math.min(m, n); i++) {
             A[i][i][0] = D[i];
         }
         
         // Quick exit if the user wants a diagonal matrix
         if ((kl == 0) && (ku == 0)) {
        	 return;
         }

         // pre- and post- multiply A by random unitary matrices
         for (i = Math.min(m, n); i >= 1; i--) {

             if (i < m) {

                 // generate random reflection
                 zlarnv(3, iseed, m - i + 1, work);
                 wn = dznrm2(m - i + 1, work, 1);

                 
                 ratio = wn/zabs(work[0][0], work[0][1]);
                 wa[0] = ratio * work[0][0];
                 wa[1] = ratio * work[0][1];
                 if (wn == 0.0) {
                     tau[0] = 0.0;
                     tau[1] = 0.0;
                 } else {
                     wb[0] = work[0][0] + wa[0];
                     wb[1] = work[0][1] + wa[1];

                     zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
                     for (j = 0; j < (m - i); j++) {
                    	 zmlt(cr[0], ci[0], work[j+1][0], work[j+1][1], cr2, ci2);
                         work[j + 1][0] = cr2[0];
                         work[j + 1][1] = ci2[0];
                     }

                     work[0][0] = 1.0;
                     work[0][1] = 0.0;
                     zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
                     tau[0] = cr[0];
                     tau[1] = 0.0;
                 }

                 // multiply A(i-1:m-1,i-1:n-1) by random reflection from the left
                 B = new double[m - i + 1][n - i + 1][2];

                 for (j = 0; j < (m - i + 1); j++) {

                     for (k = 0; k < (n - i + 1); k++) {
                    	 for (p = 0; p < 2; p++) {
                            B[j][k][p] = A[j + i - 1][k + i - 1][p];
                    	 }
                     }
                 }

                 work2 = new double[work.length - m][2];

                 for (j = 0; j < (work.length - m); j++) {
                	 for (p = 0; p < 2; p++) {
                         work2[j][p] = work[m + j][p];
                	 }
                 }

                 alpha[0] = 1.0;
                 alpha[1] = 0.0;
                 beta[0] = 0.0;
                 beta[1] = 0.0;
                 zgemv('C', m - i + 1, n - i + 1, alpha, B, m - i + 1, work, 1, beta, work2, 1);
                 alpha[0] = -tau[0];
                 alpha[1] = -tau[1];
                 zgerc(m - i + 1, n - i + 1, alpha, work, 1, work2, 1, B, m - i + 1);

                 for (j = 0; j < (m - i + 1); j++) {

                     for (k = 0; k < (n - i + 1); k++) {
                    	 for (p = 0; p < 2; p++) {
                             A[j + i - 1][k + i - 1][p] = B[j][k][p];
                    	 }
                     }
                 }

                 for (j = 0; j < (work.length - m); j++) {
                	 for (p = 0; p < 2; p++) {
                         work[m + j][p] = work2[j][p];
                	 }
                 }
             } // if (i < m)

             if (i < n) {

                 // generate random reflection
                 zlarnv(3, iseed, n - i + 1, work);
                 wn = dznrm2(n - i + 1, work, 1);
                 ratio = wn/zabs(work[0][0], work[0][1]);
                 wa[0] = ratio * work[0][0];
                 wa[1] = ratio * work[0][1];
                 if (wn == 0.0) {
                     tau[0] = 0.0;
                     tau[1] = 0.0;
                 } else {
                     wb[0] = work[0][0] + wa[0];
                     wb[1] = work[0][1] + wa[1];

                     zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
                     for (j = 0; j < (n - i); j++) {
                    	 zmlt(cr[0], ci[0], work[j+1][0], work[j+1][1], cr2, ci2);
                         work[j + 1][0] = cr2[0];
                         work[j + 1][1] = ci2[0];
                     }

                     work[0][0] = 1.0;
                     work[0][1] = 0.0;
                     zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
                     tau[0] = cr[0];
                     tau[1] = 0.0;
                 }

                 // multiply A(i-1:m-1,i-1:n-1) by random reflection from right
                 B = new double[m - i + 1][n - i + 1][2];

                 for (j = 0; j < (m - i + 1); j++) {

                     for (k = 0; k < (n - i + 1); k++) {
                    	 for (p = 0; p < 2; p++) {
                             B[j][k][p] = A[j + i - 1][k + i - 1][p];
                    	 }
                     }
                 }

                 work2 = new double[work.length - n][2];

                 for (j = 0; j < (work.length - n); j++) {
                	 for (p = 0; p < 2; p++) {
                         work2[j][p] = work[n + j][p];
                	 }
                 }

                 alpha[0] = 1.0;
                 alpha[1] = 0.0;
                 beta[0] = 0.0;
                 beta[1] = 0.0;
                 zgemv('N', m - i + 1, n - i + 1, alpha, B, m - i + 1, work, 1, beta, work2, 1);
                 alpha[0] = -tau[0];
                 alpha[1] = -tau[1];
                 zgerc(m - i + 1, n - i + 1, alpha, work2, 1, work, 1, B, m - i + 1);

                 for (j = 0; j < (m - i + 1); j++) {

                     for (k = 0; k < (n - i + 1); k++) {
                    	 for (p = 0; p < 2; p++) {
                             A[j + i - 1][k + i - 1][p] = B[j][k][p];
                    	 }
                     }
                 }

                 for (j = 0; j < (work.length - n); j++) {
                	 for (p = 0; p < 2; p++) {
                         work[n + j][p] = work2[j][p];
                	 }
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
                     x = new double[m - kl - i + 1][2];

                     for (j = 0; j < (m - kl - i + 1); j++) {
                    	 for (p = 0; p < 2; p++) {
                             x[j][p] = A[kl + i - 1 + j][i - 1][p];
                    	 }
                     }

                     wn = dznrm2(m - kl - i + 1, x, 1);
                     ratio = wn/zabs(A[kl + i - 1][i-1][0], A[kl + i - 1][i-1][1]);
                     wa[0] = ratio * A[kl + i - 1][i-1][0];
                     wa[1] = ratio * A[kl + i - 1][i-1][1];
                     if (wn == 0.0) {
                         tau[0] = 0.0;
                         tau[1] = 0.0;
                     } else {
                         wb[0] = A[kl + i - 1][i-1][0] + wa[0];
                         wb[1] = A[kl + i - 1][i-1][1] + wa[1];

                         zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
                         for (j = 0; j < (m-kl-i); j++) {
                        	 zmlt(cr[0], ci[0], A[kl + i - 1][i-1][0],  A[kl + i - 1][i-1][1], cr2, ci2);
                        	 A[kl + i - 1][i-1][0] = cr2[0];
                        	 A[kl + i - 1][i-1][1] = ci2[0];
                         }

                         A[kl + i - 1][i-1][0] = 1.0;
                         A[kl + i - 1][i-1][1] = 0.0;
                         zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
                         tau[0] = cr[0];
                         tau[1] = 0.0;
                     }

                     // apply reflection to A(kl+i-1:m-1,i:n-1) from the left
                     B = new double[m - kl - i + 1][n - i][2];

                     for (j = 0; j < (m - kl - i + 1); j++) {

                         for (k = 0; k < (n - i); k++) {
                        	 for (p = 0; p < 2; p++) {
                                 B[j][k][p] = A[kl + i - 1 + j][i + k][p];
                        	 }
                         }
                     }

                     x = new double[m - kl - i + 1][2];

                     for (j = 0; j < (m - kl - i + 1); j++) {
                    	 for (p = 0; p < 2; p++) {
                             x[j][p] = A[kl + i - 1 + j][i - 1][p];
                    	 }
                     }

                     alpha[0] = 1.0;
                     alpha[1] = 0.0;
                     beta[0] = 0.0;
                     beta[1] = 0.0;
                     zgemv('C', m - kl - i + 1, n - i, alpha, B, m - kl - i + 1, x, 1, beta, work, 1);
                     alpha[0] = -tau[0];
                     alpha[1] = -tau[1];
                     zgerc(m - kl - i + 1, n - i, alpha, x, 1, work, 1, B, m - kl - i + 1);

                     for (j = 0; j < (m - kl - i + 1); j++) {

                         for (k = 0; k < (n - i); k++) {
                        	 for (p = 0; p < 2; p++) {
                                 A[kl + i - 1 + j][i + k][p] = B[j][k][p];
                        	 }
                         }
                     }

                     for (p = 0; p < 2; p++) {
                         A[kl + i - 1][i - 1][p] = -wa[p];
                     }
                 } // if (i <= Math.min(m-1-kl, n))

                 if (i <= Math.min(n - 1 - ku, m)) {

                     // generate reflection to annihilate A(i-1,ku+i:n-1)
                     x = new double[n - ku - i + 1][2];

                     for (j = 0; j < (n - ku - i + 1); j++) {
                    	 for (p = 0; p < 2; p++) {
                             x[j][p] = A[i - 1][ku + i - 1 + j][p];
                    	 }
                     }

                     wn = dznrm2(n - ku - i + 1, x, 1);
                     ratio = wn/zabs(A[i - 1][ku+i-1][0], A[i - 1][ku+i-1][1]);
                     wa[0] = ratio * A[i - 1][ku+i-1][0];
                     wa[1] = ratio * A[i - 1][ku+i-1][1];
                     if (wn == 0.0) {
                         tau[0] = 0.0;
                         tau[1] = 0.0;
                     } else {
                         wb[0] = A[i - 1][ku+i-1][0] + wa[0];
                         wb[1] = A[i - 1][ku+i-1][1] + wa[1];

                         zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
                         for (j = 0; j < (n-ku-i); j++) {
                        	 zmlt(cr[0], ci[0], A[i - 1][ku+i-1][0],  A[i - 1][ku+i-1][1], cr2, ci2);
                        	 A[i - 1][ku+i-1][0] = cr2[0];
                        	 A[i - 1][ku+i-1][1] = ci2[0];
                         }

                         A[i - 1][ku+i-1][0] = 1.0;
                         A[i - 1][ku+i-1][1] = 0.0;
                         zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
                         tau[0] = cr[0];
                         tau[1] = 0.0;
                     }

                     // Apply reflection to A(i:m-1,ku+i-1:n-1) from the right
                     for (j = 0; j < n-ku-i-1; j++) {
                    	 A[i-1][ku+i-1+j][1] = -A[i-1][ku+i-1+j][1];
                     }
                     B = new double[m - i][n - ku - i + 1][2];

                     for (j = 0; j < (m - i); j++) {

                         for (k = 0; k < (n - ku - i + 1); k++) {
                        	 for (p = 0; p < 2; p++) {
                                 B[j][k][p] = A[i + j][ku + i - 1 + k][p];
                        	 }
                         }
                     }

                     x = new double[n - ku - i + 1][2];

                     for (j = 0; j < (n - ku - i + 1); j++) {
                    	 for (p = 0; p < 2; p++) {
                             x[j][p] = A[i - 1][ku + i - 1 + j][p];
                    	 }
                     }

                     alpha[0] = 1.0;
                     alpha[1] = 0.0;
                     beta[0] = 0.0;
                     beta[1] = 0.0;
                     zgemv('N', m - i, n - ku - i + 1, alpha, B, m - i, x, 1, beta, work, 1);
                     alpha[0] = -tau[0];
                     alpha[1] = -tau[1];
                     zgerc(m - i, n - ku - i + 1, alpha, work, 1, x, 1, B, m - i);

                     for (j = 0; j < (m - i); j++) {

                         for (k = 0; k < (n - ku - i + 1); k++) {
                        	 for (p = 0; p < 2; p++) {
                                 A[i + j][ku + i - 1 + k][p] = B[j][k][p];
                        	 }
                         }
                     }

                     for (p = 0; p < 2; p++) {
                         A[i - 1][ku + i - 1][p] = -wa[p];
                     }
                 } // if (i <= Math.min(n-1-ku, m))
             } // if (kl <= ku)
             else { // kl > ku

                 // annihilate superdiagonal elements first (necessary if ku = 0)
                 if (i <= Math.min(n - 1 - ku, m)) {

                     // generate reflection to annihilate A(i-1,ku+i:n-1)
                     x = new double[n - ku - i + 1][2];

                     for (j = 0; j < (n - ku - i + 1); j++) {
                    	 for (p = 0; p < 2; p++) {
                             x[j][p] = A[i - 1][ku + i - 1 + j][p];
                    	 }
                     }

                     wn = dznrm2(n - ku - i + 1, x, 1);
                     ratio = wn/zabs(A[i - 1][ku+i-1][0], A[i - 1][ku+i-1][1]);
                     wa[0] = ratio * A[i - 1][ku+i-1][0];
                     wa[1] = ratio * A[i - 1][ku+i-1][1];
                     if (wn == 0.0) {
                         tau[0] = 0.0;
                         tau[1] = 0.0;
                     } else {
                         wb[0] = A[i - 1][ku+i-1][0] + wa[0];
                         wb[1] = A[i - 1][ku+i-1][1] + wa[1];

                         zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
                         for (j = 0; j < (n-ku-i); j++) {
                        	 zmlt(cr[0], ci[0], A[i - 1][ku+i-1][0],  A[i - 1][ku+i-1][1], cr2, ci2);
                        	 A[i - 1][ku+i-1][0] = cr2[0];
                        	 A[i - 1][ku+i-1][1] = ci2[0];
                         }

                         A[i - 1][ku+i-1][0] = 1.0;
                         A[i - 1][ku+i-1][1] = 0.0;
                         zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
                         tau[0] = cr[0];
                         tau[1] = 0.0;
                     }

                     // apply reflection to A(i:m-1,ku+i-1:n-1) from the right
                     for (j = 0; j < n-ku-i-1; j++) {
                    	 A[i-1][ku+i-1+j][1] = -A[i-1][ku+i-1+j][1];
                     }
                     B = new double[m - i][n - ku - i + 1][2];

                     for (j = 0; j < (m - i); j++) {

                         for (k = 0; k < (n - ku - i + 1); k++) {
                        	 for (p = 0; p < 2; p++) {
                                 B[j][k][p] = A[i + j][ku + i - 1 + k][p];
                        	 }
                         }
                     }

                     x = new double[n - ku - i + 1][2];

                     for (j = 0; j < (n - ku - i + 1); j++) {
                    	 for (p = 0; p < 2; p++) {
                             x[j][p] = A[i - 1][ku + i - 1 + j][p];
                    	 }
                     }

                     alpha[0] = 1.0;
                     alpha[1] = 0.0;
                     beta[0] = 0.0;
                     beta[1] = 0.0;
                     zgemv('N', m - i, n - ku - i + 1, alpha, B, m - i, x, 1, beta, work, 1);
                     alpha[0] = -tau[0];
                     alpha[1] = -tau[1];
                     zgerc(m - i, n - ku - i + 1, alpha, work, 1, x, 1, B, m - i);

                     for (j = 0; j < (m - i); j++) {

                         for (k = 0; k < (n - ku - i + 1); k++) {
                        	 for (p = 0; p < 2; p++) {
                                 A[i + j][ku + i - 1 + k][p] = B[j][k][p];
                        	 }
                         }
                     }

                     for (p = 0; p < 2; p++) {
                         A[i - 1][ku + i - 1][p] = -wa[p];
                     }
                 } // if (i <= Math.min(n-1-ku,m))

                 if (i <= Math.min(m - 1 - kl, n)) {

                     // generate reflection to annihilate A(kl+i:m-1,i-1)
                     x = new double[m - kl - i + 1][2];

                     for (j = 0; j < (m - kl - i + 1); j++) {
                    	 for (p = 0; p < 2; p++) {
                             x[j][p] = A[kl + i - 1 + j][i - 1][p];
                    	 }
                     }

                     wn = dznrm2(m - kl - i + 1, x, 1);
                     ratio = wn/zabs(A[kl + i - 1][i-1][0], A[kl + i - 1][i-1][1]);
                     wa[0] = ratio * A[kl + i - 1][i-1][0];
                     wa[1] = ratio * A[kl + i - 1][i-1][1];
                     if (wn == 0.0) {
                         tau[0] = 0.0;
                         tau[1] = 0.0;
                     } else {
                         wb[0] = A[kl + i - 1][i-1][0] + wa[0];
                         wb[1] = A[kl + i - 1][i-1][1] + wa[1];

                         zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
                         for (j = 0; j < (m-kl-i); j++) {
                        	 zmlt(cr[0], ci[0], A[kl + i - 1][i-1][0],  A[kl + i - 1][i-1][1], cr2, ci2);
                        	 A[kl + i - 1][i-1][0] = cr2[0];
                        	 A[kl + i - 1][i-1][1] = ci2[0];
                         }

                         A[kl + i - 1][i-1][0] = 1.0;
                         A[kl + i - 1][i-1][1] = 0.0;
                         zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
                         tau[0] = cr[0];
                         tau[1] = 0.0;
                     }

                     // apply reflection to A(kl+i-1:m-1,i:n-1) from the left
                     B = new double[m - kl - i + 1][n - i][2];

                     for (j = 0; j < (m - kl - i + 1); j++) {

                         for (k = 0; k < (n - i); k++) {
                        	 for (p = 0; p < 2; p++) {
                                 B[j][k][p] = A[kl + i - 1 + j][i + k][p];
                        	 }
                         }
                     }

                     x = new double[m - kl - i + 1][2];

                     for (j = 0; j < (m - kl - i + 1); j++) {
                    	 for (p = 0; p < 2; p++) {
                             x[j][p] = A[kl + i - 1 + j][i - 1][p];
                    	 }
                     }

                     alpha[0] = 1.0;
                     alpha[1] = 0.0;
                     beta[0] = 0.0;
                     beta[1] = 0.0;
                     zgemv('C', m - kl - i + 1, n - i, alpha, B, m - kl - i + 1, x, 1, beta, work, 1);
                     alpha[0] = -tau[0];
                     alpha[1] = -tau[1];
                     zgerc(m - kl - i + 1, n - i, alpha, x, 1, work, 1, B, m - kl - i + 1);

                     for (j = 0; j < (m - kl - i + 1); j++) {

                         for (k = 0; k < (n - i); k++) {
                        	 for (p = 0; p < 2; p++) {
                                 A[kl + i - 1 + j][i + k][p] = B[j][k][p];
                        	 }
                         }
                     }

                     for (p = 0; p < 2; p++) {
                         A[kl + i - 1][i - 1][p] = -wa[p];
                     }
                 } // if (i <= Math.min(m-1-kl, n))
             } // else kl > ku

             for (j = kl + i + 1; j <= m; j++) {
            	 for (p = 0; p < 2; p++) {
                     A[j - 1][i - 1][p] = 0.0;
            	 }
             }

             for (j = ku + i + 1; j <= n; j++) {
            	 for (p = 0; p < 2; p++) {
                     A[i - 1][j - 1][p] = 0.0;
            	 }
             }
         } // for (i = 1; i <= Math.max(m-1-kl, n-1-ku); i++)

         return;
     } // zlagge*/
     
     /**
      * This is a port of version 3.7.0 LAPACK auxiliary routine ZLARNV Original ZLARNV created by Univ. of Tennessee,
      * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., December, 2016 zlarnv returns a vector of n
      * random complex numbers from a uniform or normal distribution
      * 
      * @param idist input int Specifies the distribution of the random numbers: 
      * = 1: real and imaginary parts each uniform (0,1) 
      * = 2: real and imaginary parts each uniform (-1,1) 
      * = 3: real and imaginary parts each normal (0,1)
      * = 4: uniformly distributed on the disc abs(z) < 1
      * = 5: Uniformly distributed on the circle abs(z) = 1
      * @param iseed input/output int[] of dimension 4. On entry, the seed of the random number generator; the array
      *            elements must be between 0 and 4095, and iseed[3] must be odd. On exit, the seed is updated
      * @param n input int The number of random numbers to be generated.
      * @param x output double[][2] complex of dimension n. The random generated numbers.
      * 
      *            The routine calls the auxiliary routine dlaruv to generate random real numbers from a uniform (0,1)
      *            distribution, in batches of up to 128 using vectorizable code. The Box-Muller method is used to
      *            transform numbers from a uniform to a normal distribution.
      */
     public void zlarnv(final int idist, final int[] iseed, final int n, final double[][] x) {
         final int lv = 128;
         int i;
         int il;
         int iv;
         final double[] u = new double[lv];
         double root;
         double arg;

         for (iv = 1; iv <= n; iv += lv / 2) {
             il = Math.min(lv / 2, n - iv + 1);

             // Call dlaruv to generate 2*il real numbers from a uniform (0,1)
             // distribution (2*il <= lv)
             ge.dlaruv(iseed, 2*il, u);

             if (idist == 1) {

                 // Copy generated numbers
                 for (i = 1; i <= il; i++) {
                     x[iv + i - 2][0] = u[2*i-2];
                     x[iv + i - 2][1] = u[2*i-1];
                 }
             } // if (idist == 1)
             else if (idist == 2) {

                 // Convert generated numbers to uniform (-1,1) distribution
                 for (i = 1; i <= il; i++) {
                     x[iv + i - 2][0] = (2.0 * u[2*i-2]) - 1.0;
                     x[iv + i - 2][1] = (2.0 * u[2*i-1]) - 1.0;
                 }
             } // else if (idist == 2)
             else if (idist == 3) {

                 // Convert generated numbers to normal (0,1) distribution
                 for (i = 1; i <= il; i++) {
                	 root = Math.sqrt(-2.0*Math.log(u[2*i-2]));
                	 arg = 2.0 * Math.PI * u[2*i-1];
                	 x[iv + i - 2][0] = root * Math.cos(arg);
                	 x[iv + i - 2][1] = root * Math.sin(arg);
                 }
             } // else if (idist == 3)
             else if (idist == 4) {
            	 // Convert generated numbers to complex numbers uniformly distributed
            	 // on the unit disc
            	 for (i = 1; i <= il; i++) {
            		 root = Math.sqrt(u[2*i-2]);
            		 arg = 2.0 * Math.PI * u[2*i-1];
            		 x[iv + i - 2][0] = root * Math.cos(arg);
            		 x[iv + i - 2][1] = root * Math.sin(arg);
            	 }
             } // else if (idist == 4)
             else if (idist == 5) {
            	 // Convert generated numbers ot complex numbers uniformly distributed
            	 // on the unit circle
            	 for (i = 1; i <= il; i++) {
            		 arg = 2.0 * Math.PI * u[2*i-1];
            		 x[iv + i - 2][0] = Math.cos(arg);
            		 x[iv + i - 2][1] = Math.sin(arg);	 
            	 }
             } // else if (idist == 5)

         } // for (iv = 1; iv <= n; iv += lv/2)
         return;
     } // zlarnv
     
     /**
      * BLAS level1 routine version 3.7.0 December, 2016
      * This is a port of the 10/14/93 DZNRM2 function Original code written by Sven Hammarling, Nag Ltd. dznrm2 returns
      * the euclidean norm of a vector via the function sqrt(x**H*x)
      * 
      * @param n int
      * @param x double[][2] complex vector
      * @param incx int
      * 
      * @return double
      */
     public double dznrm2(final int n, final double[][] x, final int incx) {
         int ix;
         double norm;
         double scale;
         double ssq;
         double ratio;
         double temp;

         if ( (n < 1) || (incx < 1)) {
             norm = 0.0;
         } else {
             scale = 0.0;
             ssq = 1.0;
             
             for (ix = 0; ix <= ( (n - 1) * incx); ix += incx) {
                 if (x[ix][0] != 0.0) {
                	temp = Math.abs(x[ix][0]);
                    if (scale < temp) {
                    	ratio = scale/temp;
                    	ssq = 1.0 + ssq * ratio * ratio;
                    	scale = temp;
                    }
                    else {
                    	ratio = temp/scale;
                    	ssq = ssq + ratio*ratio;
                    }
                 } // if (x[ix][0] != 0)
                 if (x[ix][1] != 0.0) {
                	 temp = Math.abs(x[ix][1]);
                     if (scale < temp) {
                     	ratio = scale/temp;
                     	ssq = 1.0 + ssq * ratio * ratio;
                     	scale = temp;
                     }
                     else {
                     	ratio = temp/scale;
                     	ssq = ssq + ratio*ratio;
                     }   
                 } // if (x[ix][1] != 0.0)
             } // for (ix = 0; ix <= ( (n - 1) * incx); ix += incx)

             norm = scale * Math.sqrt(ssq);
         } // else

         return norm;
     } // dznrm2
     
     /**
      * From LAPACK 3.7.0 December 2106
      * Routine ported from 10/22/86 blas zgemv subroutine Original version written by: Jack Dongarra, Argonne National
      * Lab. Jeremy Du Croz, Nag Central Office Sven Hammarling, Nag Central Office. Richard Hanson, Sandia National
      * Labs. dgemv performs one of the matrix-vector operations y = alpha*A*x + beta*y, or y = alpha*A**T*x + beta*y,
      * or y = alpha*A**H*x + beta*y
      * where alpha and beta are scalars, x and y are vectors, and A is an m by n matrix
      * 
      * @param trans input char On entry, trans specifies the operation to be performed as follows: 
      * = 'N' or 'n' y = alpha*A*x + beta*y 
      * = 'T' or 't' y = alpha*A**T*x + beta*y 
      * = 'C' or 'c' y = alpha*A**H*x + beta*y
      * @param m input int On entry, m specifies the mumber of rows of matrix A. m must be at least zero.
      * @param n input int On entry, n specifies the number of columns of matrix A. n must be at least zero.
      * @param alpha input double[2] complex specified scalar
      * @param A input double[][][2] complex dimension lda by n Before entry, the leading m by n part of the array A must contain
      *            the matrix of coefficients.
      * @param lda input int On entry, lda specifies the first dimension of A as declared in the calling (sub) program.
      *            lda must be at least max(1, m).
      * @param x input double[][2] complex array of dimension at least (1 + (n-1)*abs(incx)) when trans = 'N' or 'n' and at least (1
      *            + (m-1)*abs(incx)) otherwise. Before entry, the incremented array x must contain the vector x.
      * @param incx input int On entry, incx specifies the increment for the elements of x. incx must not be zero.
      * @param beta input double[2] complex specified scalar When beta is supplied as zero, then y need not be set on input.
      * @param y input/output double[][2] complex array of dimension at least (1 + (m-1)*abs(incy)) when trans = 'N' or 'n' and at
      *            least (1 + (n-1)*abs(incy)) otherwise. Before entry with beta non-zero, the incremented array y must
      *            contain the vector y. On exit, array y is overwritten with the updated vector y.
      * @param incy input int On entry, incy specifies the increment for the elements of y. incy must not be zero.
      */
     public void zgemv(final char trans, final int m, final int n, final double alpha[], final double[][][] A,
             final int lda, final double[][] x, final int incx, final double beta[], final double[][] y, final int incy) {
         int info;
         int lenx;
         int leny;
         int kx;
         int ky;
         int i;
         int iy;
         int jx;
         int j;
         int jy;
         int ix;
         double temp[] = new double[2];
         boolean noconj;
         double cr[] = new double[1];
         double ci[] = new double[1];

         // Test the input parameters
         info = 0;

         if ( (trans != 'N') && (trans != 'n') && (trans != 'T') && (trans != 't') && (trans != 'C') && (trans != 'c')) {
             info = 1;
         } else if (m < 0) {
             info = 2;
         } else if (n < 0) {
             info = 3;
         } else if (lda < Math.max(1, m)) {
             info = 6;
         } else if (incx == 0) {
             info = 8;
         } else if (incy == 0) {
             info = 11;
         }

         if (info != 0) {
             MipavUtil.displayError("Error zgemv has info = " + info);

             return;
         } // if (info != 0)

         // Quick return if possible
         if ( (m == 0) || (n == 0) || ( ((alpha[0] == 0.0) && (alpha[1] == 0.0)) && ((beta[0] == 1.0) && (beta[1] == 0.0)))) {
             return;
         }
         noconj = ((trans == 'T') || (trans == 't'));

         // Set lenx and leny, the lengths of vectors x and y, and set up the
         // start points in arrays x and y.

         if ( (trans == 'N') || (trans == 'n')) {
             lenx = n;
             leny = m;
         } else {
             lenx = m;
             leny = n;
         }

         if (incx > 0) {
             kx = 1;
         } else {
             kx = 1 - ( (lenx - 1) * incx);
         }

         if (incy > 0) {
             ky = 1;
         } else {
             ky = 1 - ( (leny - 1) * incy);
         }

         // Start the operations. In this version the elements of A are accessed
         // sequentially with one pass through A.
         // First form y = beta*y.
         if ((beta[0] != 1.0) || (beta[1] != 0.0)) {

             if (incy == 1) {

                 if ((beta[0] == 0.0) && (beta[1] == 0.0)) {

                     for (i = 0; i < leny; i++) {
                         y[i][0] = 0.0;
                         y[i][1] = 0.0;
                     }
                 } // if ((beta[0] == 0.0) && (beta[1] == 0.0))
                 else { // beta != 0.0

                     for (i = 0; i < leny; i++) {
                    	 zmlt(beta[0], beta[1], y[i][0], y[i][1], cr, ci);
                    	 y[i][0] = cr[0];
                    	 y[i][1] = ci[0];
                     }
                 } // else beta != 0.0
             } // if (incy == 1)
             else { // incy != 1
                 iy = ky - 1;

                 if ((beta[0] == 0.0) && (beta[1] == 0.0)) {

                     for (i = 1; i <= leny; i++) {
                         y[iy][0] = 0.0;
                         y[iy][1] = 0.0;
                         iy = iy + incy;
                     }
                 } // if ((beta[0] == 0.0) && (beta[1] == 0.0))
                 else { // beta != 0.0

                     for (i = 1; i <= leny; i++) {
                    	 zmlt(beta[0], beta[1], y[iy][0], y[iy][1], cr, ci);
                    	 y[iy][0] = cr[0];
                    	 y[iy][1] = ci[0];
                         iy = iy + incy;
                     }
                 } // else beta != 0.0
             } // else incy != 1
         } // if ((beta[0] != 1.0) || (beta[1] != 0.0))

         if ((alpha[0] == 0.0) && (alpha[1] == 0.0)) {
             return;
         }

         if ( (trans == 'N') || (trans == 'n')) {

             // Form y = alpha*A*x + y.
             jx = kx - 1;

             if (incy == 1) {

                 for (j = 0; j < n; j++) {

                     zmlt(alpha[0], alpha[1], x[jx][0], x[jx][1], cr, ci);
                     temp[0] = cr[0];
                     temp[1] = ci[0];

                     for (i = 0; i < m; i++) {
                    	 zmlt(temp[0], temp[1], A[i][j][0], A[i][j][1], cr, ci);
                    	 y[i][0] = y[i][0] + cr[0];
                    	 y[i][1] = y[i][1] + ci[0];
                     } // for (i = 0; i < m; i++)

                     jx = jx + incx;
                 } // for (j = 0; j < n; j++)
             } // if (incy == 1)
             else { // incy != 1

                 for (j = 0; j < n; j++) {

                	 zmlt(alpha[0], alpha[1], x[jx][0], x[jx][1], cr, ci);
                     temp[0] = cr[0];
                     temp[1] = ci[0];
                     iy = ky - 1;

                     for (i = 0; i < m; i++) {
                    	 zmlt(temp[0], temp[1], A[i][j][0], A[i][j][1], cr, ci);
                    	 y[iy][0] = y[iy][0] + cr[0];
                    	 y[iy][1] = y[iy][1] + ci[0];
                         iy = iy + incy;
                     } // for (i = 0; i < m; i++)

                     jx = jx + incx;
                 } // for (j = 0; j < n; j++)
             } // else incy != 1
         } // if (trans == 'N') || (trans == 'n'))
         else { // trans != 'N' && trans != 'n'

             // Form y = alpha*A**T*x + y or y = alpha*A**H*x + y
             jy = ky - 1;

             if (incx == 1) {

                 for (j = 0; j < n; j++) {
                     temp[0] = 0.0;
                     temp[1] = 0.0;
                     if (noconj) {
	                     for (i = 0; i < m; i++) {
	                    	 zmlt(A[i][j][0], A[i][j][1], x[i][0], x[i][1], cr, ci);
	                    	 temp[0] = temp[0] + cr[0];
	                    	 temp[1] = temp[1] + ci[0];
	                     } // for (i = 0; i < m; i++)
                     } // if (noconj)
                     else {
                    	 for (i = 0; i < m; i++) {
	                    	 zmlt(A[i][j][0], -A[i][j][1], x[i][0], x[i][1], cr, ci);
	                    	 temp[0] = temp[0] + cr[0];
	                    	 temp[1] = temp[1] + ci[0];
	                     } // for (i = 0; i < m; i++)	 
                     } // else

                     zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                     y[jy][0] = y[jy][0] + cr[0];
                     y[jy][1] = y[jy][1] + ci[0];
                     jy = jy + incy;
                 } // for (j = 0; j < n; j++)
             } // if (incx == 1)
             else { // incx != 1
            	 for (j = 0; j < n; j++) {
                     temp[0] = 0.0;
                     temp[1] = 0.0;
                     ix = kx-1;
                     if (noconj) {
	                     for (i = 0; i < m; i++) {
	                    	 zmlt(A[i][j][0], A[i][j][1], x[ix][0], x[ix][1], cr, ci);
	                    	 temp[0] = temp[0] + cr[0];
	                    	 temp[1] = temp[1] + ci[0];
	                    	 ix = ix + incx;
	                     } // for (i = 0; i < m; i++)
                     } // if (noconj)
                     else {
                    	 for (i = 0; i < m; i++) {
	                    	 zmlt(A[i][j][0], -A[i][j][1], x[ix][0], x[ix][1], cr, ci);
	                    	 temp[0] = temp[0] + cr[0];
	                    	 temp[1] = temp[1] + ci[0];
	                    	 ix = ix + incx;
	                     } // for (i = 0; i < m; i++)	 
                     } // else

                     zmlt(alpha[0], alpha[1], temp[0], temp[1], cr, ci);
                     y[jy][0] = y[jy][0] + cr[0];
                     y[jy][1] = y[jy][1] + ci[0];
                     jy = jy + incy;
            	 } // for (j = 0; j < n; j++)
             } // else incx != 1
         } // else trans != 'N' && trans != 'n'

         return;
     } // zgemv
     
     /**
      * This is a port of the 10/22/86 Blas routine ZGERC Original version written by: Jack Dongarra, Argonne National
      * Lab. Jeremy Du Croz, Nag Central Office. Sven Hammarling, Nag Central Office. Richard Hanson, Sandia National
      * Labs. zgerc performs the rank 1 operation A = alpha*x*y**H + A, where alpha is a scalar, x is an m element vector, y
      * is an n element vector, and A is an m by n matrix.
      * 
      * @param m input int On entry, m specifies the number of rows of the matrix A. m must be at least zero.
      * @param n input int On entry, n specifies the number of columns of the matrix A. n must be at least zero.
      * @param alpha input double[2] Specified complex scalar
      * @param x input double[][2] complex of dimension at least (1 + (m-1)*abs(incx)). Before entry, the incremented array x must
      *            contain the m element vector x.
      * @param incx input int On entry, incx specifies the increment for the elements of x. incx must not be zero.
      * @param y input double[][2] complex of dimension at least (1 + (n-1)*abs(incy)). Before entry, the incremented array y must
      *            contain the n element vector y.
      * @param incy input int On entry, incy specifies the increment for the elements of y. incy must not be zero.
      * @param A input/output double[][][2] complex of dimension lda by n. Before entry, the leading m by n part of the array A must contain the
      *            matrix of coefficients. On exit, A is overwritten by the updated matrix.
      * @param lda input int On entry, lda specifies the first dimension of A as declared in the calling (sub) program.
      *            lda must be at least max(1,m).
      */
     public void zgerc(final int m, final int n, final double alpha[], final double[][] x, final int incx, final double[][] y,
             final int incy, final double[][][] A, final int lda) {
         double temp[] = new double[2];
         int i;
         int info;
         int ix;
         int j;
         int jy;
         int kx;
         double cr[] = new double[1];
         double ci[] = new double[1];

         // Test the input parameters.
         info = 0;

         if (m < 0) {
             info = 1;
         } else if (n < 0) {
             info = 2;
         } else if (incx == 0) {
             info = 5;
         } else if (incy == 0) {
             info = 7;
         } else if (lda < Math.max(1, m)) {
             info = 9;
         }

         if (info != 0) {
             MipavUtil.displayError("Error zgerc had info = " + info);

             return;
         }

         // Quick return if possible
         if ( (m == 0) || (n == 0) || ((alpha[0] == 0.0) && (alpha[1] == 0.0))) {
             return;
         }

         // Start the operations. In this version the elements of A are accessed
         // sequentially with one pass through A.
         if (incy > 0) {
             jy = 0;
         } else {
             jy = - (n - 1) * incy;
         }

         if (incx == 1) {

             for (j = 0; j < n; j++) {

                 if ((y[jy][0] != 0.0) && (y[jy][1] != 0.0)) {
                	 zmlt(alpha[0], alpha[1], y[jy][0], -y[jy][1], cr, ci);
                	 temp[0] = cr[0];
                	 temp[1] = ci[0];

                     for (i = 0; i < m; i++) {
                    	 zmlt(x[i][0], x[i][1], temp[0], temp[1], cr, ci);
                    	 A[i][j][0] = A[i][j][0] + cr[0];
                    	 A[i][j][1] = A[i][j][1] + ci[0];
                     }
                 } // if ((y[jy][0] != 0.0) && (y[jy][1] != 0.0))

                 jy = jy + incy;
             } // for (j = 0; j < n; j++)
         } // if (incx == 1)
         else { // incx != 1

             if (incx > 0) {
                 kx = 1;
             } else {
                 kx = 1 - ( (m - 1) * incx);
             }

             for (j = 0; j < n; j++) {

            	 if ((y[jy][0] != 0.0) && (y[jy][1] != 0.0)) {
            		 zmlt(alpha[0], alpha[1], y[jy][0], -y[jy][1], cr, ci);
                	 temp[0] = cr[0];
                	 temp[1] = ci[0];
                     ix = kx - 1;

                     for (i = 0; i < m; i++) {
                    	 zmlt(x[ix][0], x[ix][1], temp[0], temp[1], cr, ci);
                    	 A[i][j][0] = A[i][j][0] + cr[0];
                    	 A[i][j][1] = A[i][j][1] + ci[0];
                         ix = ix + incx;
                     } // for (i = 0; i < m; i++)
                 } //  if ((y[jy][0] != 0.0) && (y[jy][1] != 0.0))

                 jy = jy + incy;
             } // for (j = 0; j < n; j++)
         } // else incx != 1

         return;
     } // zgerc
     
     /**
      * This is a port of the version 3.7.0 LAPACK auxiliary routine ZLANGE Original ZLANGE created by Univ. of Tennessee,
      * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., December, 2016 zlange returns the value of
      * the one norm, or the Frobenius norm, or the infinity norm, or the largest absolute value of any element of a
      * complex rectangular matrix.
      * 
      * @param norm input char Specifies the value to be returned from zlange as: = 'M' or 'm' returns max(abs(A[i][j])).
      *            Note that this is not a matrix norm. 
      *            = '1', 'O' or 'o' returns norm1(A), where norm1 denotes the one norm of a matrix (maximum column sum) 
      *            = 'I' or 'i' returns normI(A), where normI denotes the infinity norm of a matrix (maximum row sum)
      *             = 'F', 'f', 'E', or 'e' returns normF(A), where normF denotes the
      *            Frobenius norm of a matrix (square root of sum of squares).
      * @param m input int The number of rows of the matrix A. m >= 0. When m = 0, dlange returns zero.
      * @param n input int The number of columns of the matrix A. n >= 0. When n = 0, dlange returns zero.
      * @param A input double[][][2] complex array of dimension (lda,n). Contains the m by n matrix A.
      * @param lda input int The leading dimension of the array A. lda >= max(1,m).
      * @param work workspace double[] of dimension max(1, lwork), where lwork >= m when norm = 'I'; otherwise, work is
      *            not referenced.
      * 
      * @return double
      */
     public double zlange(final char norm, final int m, final int n, final double[][][] A, final int lda,
             final double[] work) {
         int i;
         int j;
         final double[] scale = new double[1];
         final double[] sum = new double[1];
         double value = 0.0;
         double[][] x;
         double temp;

         if (Math.min(m, n) == 0) {
             value = 0.0;
         } else if ( (norm == 'M') || (norm == 'm')) {
             // Find max(abs(A[i][j]))

             value = 0.0;

             for (j = 0; j < n; j++) {

                 for (i = 0; i < m; i++) {
                	 temp = zabs(A[i][j][0], A[i][j][1]);
                     if ((value < temp) || Double.isNaN(temp)) {
                    	 value = temp;
                     }
                 }
             }
         } // else if ((norm == 'M') || (norm == 'm'))
         else if ( (norm == 'O') || (norm == 'o') || (norm == '1')) {

             // Find norm1(A)
             value = 0.0;

             for (j = 0; j < n; j++) {
                 sum[0] = 0.0;

                 for (i = 0; i < m; i++) {
                     sum[0] = sum[0] + zabs(A[i][j][0], A[i][j][1]);
                 }

                 if ((value < sum[0]) || Double.isNaN(sum[0])) {
                	 value = sum[0];
                 }
             } // for (j = 0; j < n; j++)
         } // else if ((norm == 'O') || (norm == 'o') || (norm == '1'))
         else if ( (norm == 'I') || (norm == 'i')) {

             // Find normI(A)
             for (i = 0; i < m; i++) {
                 work[i] = 0.0;
             }

             for (j = 0; j < n; j++) {

                 for (i = 0; i < m; i++) {
                     work[i] = work[i] + zabs(A[i][j][0], A[i][j][1]);
                 }
             } // for (j = 0; j < n; j++)

             value = 0.0;

             for (i = 0; i < m; i++) {
            	 temp = work[i];
            	 if ((value < temp) || Double.isNaN(temp)) {
                	 value = temp;
                 }
             }
         } // else if ((norm == 'I') || (norm == 'i'))
         else if ( (norm == 'F') || (norm == 'f') || (norm == 'E') || (norm == 'e')) {

             // Find normF(A)
             scale[0] = 0.0;
             sum[0] = 1.0;
             x = new double[m][2];

             for (j = 0; j < n; j++) {

                 for (i = 0; i < m; i++) {
                     x[i][0] = A[i][j][0];
                     x[i][1] = A[i][j][1];
                 }

                 zlassq(m, x, 1, scale, sum);
             } // for (j = 0; j < n; j++)

             value = scale[0] * Math.sqrt(sum[0]);
         } // else if ((norm == 'F') || (norm == 'f') || (norm == 'E') ||

         return value;
     } // zlange
     
     /**
      * This is a port of version 3.7.0 LAPACK auxiliary routine ZLASSQ Original ZLASSQ created by Univ. of Tennessee,
      * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., December, 2016 zlassq returns the values
      * scl and smsq such that (scl**2)*smsq = x[0]**2 + x[incx]**2 + ... + x[(n-1)*incx]**2 + (scale**2)*sumsq The value
      * of sumsq is assumed to be non-negative and scl returns the value 
      * scl = max(scale,abs(real(x[i])),abs(imag(x[i))). scale and sumsq
      * refer to the original supplied values in scale[] and sumsq[]. scl and smsq are the returned values in scale[] and
      * sumsq[] that overwrite the orginal values. This routine makes only one pass through the vector x.
      * 
      * @param n input int The number of elements to be used from the vector x
      * @param x input double[][2] The complex vector for which a scaled sum of squares is computed, using x[0], x[incx], ...,
      *            x[(n-1)*incx]
      * @param incx input int The increment between successive values of the vector x. incx > 0.
      * @param scale input/output double[] On entry, the value scale in the equation above. On exit, scale is overwritten
      *            with scl, the scaling factor for the sum of squares
      * @param sumsq input/output double[] On entry, the value sumsq in the equation above. On exit, sumsq is overwritten
      *            with smsq, the basic sum of squares from which scl has been factored out.
      */
     public void zlassq(final int n, final double[][] x, final int incx, final double[] scale, final double[] sumsq) {
         int ix;
         double ratio;
         double temp1;

         if (n > 0) {

             for (ix = 0; ix <= ( (n - 1) * incx); ix += incx) {


                     temp1 = Math.abs(x[ix][0]);
                     if ((temp1 > 0) || Double.isNaN(temp1)) {
	                     if (scale[0] < temp1) {
	                         ratio = scale[0] / temp1;
	                         sumsq[0] = 1 + (sumsq[0] * ratio * ratio);
	                         scale[0] = temp1;
	                     } // if (scale[0] < absxi)
	                     else { // scale[0] >= absxi
	                         ratio = temp1 / scale[0];
	                         sumsq[0] = sumsq[0] + (ratio * ratio);
	                     } // else scale[0] >= absxi
                     } // if ((temp1 > 0) || Double.isNaN(temp1))
                     temp1 = Math.abs(x[ix][1]);
                     if ((temp1 > 0) || Double.isNaN(temp1)) {
	                     if (scale[0] < temp1) {
	                         ratio = scale[0] / temp1;
	                         sumsq[0] = 1 + (sumsq[0] * ratio * ratio);
	                         scale[0] = temp1;
	                     } // if (scale[0] < absxi)
	                     else { // scale[0] >= absxi
	                         ratio = temp1 / scale[0];
	                         sumsq[0] = sumsq[0] + (ratio * ratio);
	                     } // else scale[0] >= absxi
                     } // if ((temp1 > 0) || Double.isNaN(temp1))
             } // for (ix = 0; ix <= (n-1)*incx; ix += incx)
         } // if (n > 0)

         return;
     } // zlassq
     
     /**
      * This is a port of the 10/22/86 blas routine ZTRMV Original version written by: Jack Dongarra, Argonne National
      * Lab. Jeremy Du Croz, Nag Central Office Sven Hammarling, Nag Central Office Richard Hanson, Sandia National Labs.
      * Version 3.7.0 December, 2016.
      * ztrmv performs one of the matrix-vector operations x = A*x or x = A**T*x  or x = A**H*x where x is an n 
      * element vector and A is an n by n unit, or non-unit, upper or lower triangular matrix
      * 
      * @param uplo input char On entry, uplo specifies whether the matrix is an upper or lower triangular matrix as
      *            follows: = 'U' or 'u' A is an upper triangular matrix = 'L' or 'l' A is a lower triangular matrix
      * @param trans input char On entry, trans specifies the operation to be performed as follows: = 'N' or 'n', x = A*x
      *            = 'T' or 't', x = A**T*x = 'C' or 'c', x = A**H*x
      * @param diag input char On entry, diag specifies whether or not A is unit triangular as follows: = 'U' or 'u' A is
      *            assumed to be unit triangular. = 'N' or 'n' A is not assumed to be unit triangular.
      * @param n input int On entry, n specifies the order of the matrix A. n must be at least zero.
      * @param A input double[][][2] dimension lda by n complex Before entry with uplo = 'U' or 'u', the leading n by n upper
      *            triangular part of the array A must contain the upper triangular matrix and the strictly lower
      *            triangular part of A is not referenced. Before entry with uplo = 'L' or 'l', the leading n by n lower
      *            triangular part of the array A must contain the lower triangular matrix and the strictly upper
      *            triangular part of A is not referenced. Note that when diag = 'U' or 'u', the diagonal elements of A
      *            are not referenced either, but are assumed to be unity.
      * @param lda input int On entry, lda specifies the first dimension of A as declared in the calling (sub) program.
      *            lda must be at least max(1,n).
      * @param x input/output double[][2] complex of dimension at least (1 + (n-1)*abs(incx)) Before entry, the incremented array x
      *            must contain the n element vector x. On exit, array x is is overwritten with the transformed vector x.
      * @param incx input int On entry, incx specifies the increment for the elements of x. incx must not be zero.
      */
     public void ztrmv(final char uplo, final char trans, final char diag, final int n, final double[][][] A,
             final int lda, final double[][] x, final int incx) {
         double temp[] = new double[2];
         int i;
         int info;
         int ix;
         int j;
         int jx;
         int kx = 0;
         boolean noconj;
         boolean nounit;
         double cr[] = new double[1];
         double ci[] = new double[1];

         // Test the input parameters
         info = 0;

         if ( (uplo != 'U') && (uplo != 'u') && (uplo != 'L') && (uplo != 'l')) {
             info = 1;
         } else if ( (trans != 'N') && (trans != 'n') && (trans != 'T') && (trans != 't') && (trans != 'C')
                 && (trans != 'c')) {
             info = 2;
         } else if ( (diag != 'U') && (diag != 'u') && (diag != 'N') && (diag != 'n')) {
             info = 3;
         } else if (n < 0) {
             info = 4;
         } else if (lda < Math.max(1, n)) {
             info = 6;
         } else if (incx == 0) {
             info = 8;
         }

         if (info != 0) {
             MipavUtil.displayError("Error ztrmv had info = " + info);

             return;
         }

         // Quick return if possible
         if (n == 0) {
             return;
         }
         
         noconj = ((trans == 'T') || (trans == 't'));

         if ( (diag == 'N') || (diag == 'n')) {
             nounit = true;
         } else {
             nounit = false;
         }

         // Set up the start point in x if the increment is not unity. This will
         // be (n-1)*incx too small for descending loops.

         if (incx <= 0) {
             kx = 1 - ( (n - 1) * incx);
         } else if (incx != 1) {
             kx = 1;
         }

         // Start the operations. In this version the elements of A are accessed
         // sequentially with one pass through A.
         if ( (trans == 'N') || (trans == 'n')) {

             // Form x = A*x
             if ( (uplo == 'U') || (uplo == 'u')) {

                 if (incx == 1) {

                     for (j = 0; j < n; j++) {

                         if ((x[j][0] != 0.0) || (x[j][1] != 0.0)) {
                             temp[0] = x[j][0];
                             temp[1] = x[j][1];

                             for (i = 0; i <= (j - 1); i++) {
                            	 zmlt(temp[0], temp[1], A[i][j][0], A[i][j][1], cr, ci);
                            	 x[i][0] = x[i][0] + cr[0];
                            	 x[i][1] = x[i][1] + ci[0];
                             }

                             if (nounit) {
                            	 zmlt(x[j][0], x[j][1], A[j][j][0], A[j][j][1], cr, ci);
                            	 x[j][0] = cr[0];
                            	 x[j][1] = ci[0];
                             }
                         } // if ((x[j][0] != 0.0) || (x[j][1] != 0.0))
                     } // for (j = 0; j < n; j++)
                 } // if (incx == 1)
                 else { // incx != 1
                     jx = kx - 1;

                     for (j = 0; j < n; j++) {

                         if ((x[jx][0] != 0.0) || (x[jx][1] != 0.0)) {
                             temp[0] = x[jx][0];
                             temp[1] = x[jx][1];
                             ix = kx - 1;

                             for (i = 0; i <= (j - 1); i++) {
                            	 zmlt(temp[0], temp[1], A[i][j][0], A[i][j][1], cr, ci);
                            	 x[ix][0] = x[ix][0] + cr[0];
                            	 x[ix][1] = x[ix][1] + ci[0];
                                 ix = ix + incx;
                             } // for (i = 0; i <= j-1; i++)

                             if (nounit) {
                            	 zmlt(x[jx][0], x[jx][1], A[j][j][0], A[j][j][1], cr, ci);
                            	 x[jx][0] = cr[0];
                            	 x[jx][1] = ci[0];
                             }
                         } // if ((x[jx][0] != 0.0) || (x[jx][1] != 0.0))

                         jx = jx + incx;
                     } // for (j = 0; j < n; j++)
                 } // else incx != 1
             } // if ((uplo == 'U') || (uplo == 'u'))
             else { // uplo == 'L' || uplo == 'l'

                 if (incx == 1) {

                     for (j = n - 1; j >= 0; j--) {

                    	 if ((x[j][0] != 0.0) || (x[j][1] != 0.0)) {
                             temp = x[j];

                             for (i = n - 1; i >= (j + 1); i--) {
                            	 zmlt(temp[0], temp[1], A[i][j][0], A[i][j][1], cr, ci);
                            	 x[i][0] = x[i][0] + cr[0];
                            	 x[i][1] = x[i][1] + ci[0];
                             } // for (i = n-1; i >= j+1; i--)

                             if (nounit) {
                            	 zmlt(x[j][0], x[j][1], A[j][j][0], A[j][j][1], cr, ci);
                            	 x[j][0] = cr[0];
                            	 x[j][1] = ci[0];
                             }
                         } // if ((x[j][0] != 0.0) || (x[j][1] != 0.0))
                     } // for (j = n-1; j >= 0; j--)
                 } // if (incx == 1)
                 else { // incx != 1
                     kx = kx + ( (n - 1) * incx);
                     jx = kx - 1;

                     for (j = n - 1; j >= 0; j--) {

                    	 if ((x[jx][0] != 0.0) || (x[jx][1] != 0.0)) {
                             temp[0] = x[jx][0];
                             temp[1] = x[jx][1];
                             ix = kx - 1;

                             for (i = n - 1; i >= (j + 1); i--) {
                            	 zmlt(temp[0], temp[1], A[i][j][0], A[i][j][1], cr, ci);
                            	 x[ix][0] = x[ix][0] + cr[0];
                            	 x[ix][1] = x[ix][1] + ci[0];
                                 ix = ix - incx;
                             } // for (i = n-1; i >= j+1; i--)

                             if (nounit) {
                            	 zmlt(x[jx][0], x[jx][1], A[j][j][0], A[j][j][1], cr, ci);
                            	 x[jx][0] = cr[0];
                            	 x[jx][1] = ci[0];
                             }
                         } // if ((x[jx][0] != 0.0) || (x[jx][1] != 0.0))

                         jx = jx - incx;
                     } // for (j = n-1; j >= 0; j--)
                 } // else incx != 1
             } // else uplo == 'L' || uplo == 'l'
         } // if ((trans == 'N') || (trans == 'n'))
         else { // trans != 'N' && trans != 'n'

             // Form x = A**T*x or x = A**H*x
             if ( (uplo == 'U') || (uplo == 'u')) {

                 if (incx == 1) {

                     for (j = n - 1; j >= 0; j--) {
                         temp[0] = x[j][0];
                         temp[1] = x[j][1];

                         if (noconj) {
	                         if (nounit) {
	                        	 zmlt(temp[0], temp[1], A[j][j][0], A[j][j][1], cr, ci);
	                        	 temp[0] = cr[0];
	                        	 temp[1] = ci[0];
	                         }
	
	                         for (i = j - 1; i >= 0; i--) {
	                        	 zmlt(A[i][j][0], A[i][j][1], x[i][0], x[i][1], cr, ci);
	                        	 temp[0] = temp[0] + cr[0];
	                        	 temp[1] = temp[1] + ci[0];
	                         } // for (i = j-1; i >= 0; i--)
                         } // if (noconj)
                         else {
                        	 if (nounit) {
	                        	 zmlt(temp[0], temp[1], A[j][j][0], -A[j][j][1], cr, ci);
	                        	 temp[0] = cr[0];
	                        	 temp[1] = ci[0];
	                         }
	
	                         for (i = j - 1; i >= 0; i--) {
	                        	 zmlt(A[i][j][0], -A[i][j][1], x[i][0], x[i][1], cr, ci);
	                        	 temp[0] = temp[0] + cr[0];
	                        	 temp[1] = temp[1] + ci[0];
	                         } // for (i = j-1; i >= 0; i--)	 
                         } // else

                         x[j][0] = temp[0];
                         x[j][1] = temp[1];
                     } // for (j = n-1; j >= 0; j--)
                 } // if (incx == 1)
                 else { // incx != 1
                     jx = kx + ( (n - 1) * incx) - 1;

                     for (j = n - 1; j >= 0; j--) {
                         temp[0] = x[jx][0];
                         temp[1] = x[jx][1];
                         ix = jx;

                         if (noconj) {
	                         if (nounit) {
	                        	 zmlt(temp[0], temp[1], A[j][j][0], A[j][j][1], cr, ci);
	                        	 temp[0] = cr[0];
	                        	 temp[1] = ci[0];
	                         }
	
	                         for (i = j - 1; i >= 0; i--) {
	                             ix = ix - incx;
	                             zmlt(A[i][j][0], A[i][j][1], x[ix][0], x[ix][1], cr, ci);
	                        	 temp[0] = temp[0] + cr[0];
	                        	 temp[1] = temp[1] + ci[0];
	                         } // for (i = j-1; i >= 0; i--)
                         } // if (noconj)
                         else {
                        	 if (nounit) {
	                        	 zmlt(temp[0], temp[1], A[j][j][0], -A[j][j][1], cr, ci);
	                        	 temp[0] = cr[0];
	                        	 temp[1] = ci[0];
	                         }
	
	                         for (i = j - 1; i >= 0; i--) {
	                             ix = ix - incx;
	                             zmlt(A[i][j][0], -A[i][j][1], x[ix][0], x[ix][1], cr, ci);
	                        	 temp[0] = temp[0] + cr[0];
	                        	 temp[1] = temp[1] + ci[0];
	                         } // for (i = j-1; i >= 0; i--)	 
                         } // else

                         x[jx][0] = temp[0];
                         x[jx][1] = temp[1];
                         jx = jx - incx;
                     } // for (j = n-1; j >= 0; j--)
                 } // else incx != 1
             } // if ((uplo == 'U') || (uplo == 'u'))
             else { // ((uplo == 'L') || (uplo == 'l')) {

                 if (incx == 1) {

                     for (j = 0; j < n; j++) {
                         temp[0] = x[j][0];
                         temp[1] = x[j][1];

                         if (noconj) {
	                         if (nounit) {
	                        	 zmlt(temp[0], temp[1], A[j][j][0], A[j][j][1], cr, ci);
	                        	 temp[0] = cr[0];
	                        	 temp[1] = ci[0];
	                         }
	
	                         for (i = j + 1; i < n; i++) {
	                        	 zmlt(A[i][j][0], A[i][j][1], x[i][0], x[i][1], cr, ci);
	                        	 temp[0] = temp[0] + cr[0];
	                        	 temp[1] = temp[1] + ci[0];
	                         } // for (i = j+1; i < n; i++)
                         } // if (noconj)
                         else {
                        	 if (nounit) {
	                        	 zmlt(temp[0], temp[1], A[j][j][0], -A[j][j][1], cr, ci);
	                        	 temp[0] = cr[0];
	                        	 temp[1] = ci[0];
	                         }
	
	                         for (i = j + 1; i < n; i++) {
	                        	 zmlt(A[i][j][0], -A[i][j][1], x[i][0], x[i][1], cr, ci);
	                        	 temp[0] = temp[0] + cr[0];
	                        	 temp[1] = temp[1] + ci[0];
	                         } // for (i = j+1; i < n; i++)	 
                         }

                         x[j][0] = temp[0];
                         x[j][1] = temp[1];
                     } // for (j = 0; j < n; j++)
                 } // if (incx == 1)
                 else { // incx != 1
                     jx = kx - 1;

                     for (j = 0; j < n; j++) {
                         temp[0] = x[jx][0];
                         temp[1] = x[jx][1];
                         ix = jx;

                         if (noconj) {
	                         if (nounit) {
	                        	 zmlt(temp[0], temp[1], A[j][j][0], A[j][j][1], cr, ci);
	                        	 temp[0] = cr[0];
	                        	 temp[1] = ci[0];
	                         }
	
	                         for (i = j + 1; i < n; i++) {
	                             ix = ix + incx;
	                             zmlt(A[i][j][0], A[i][j][1], x[ix][0], x[ix][1], cr, ci);
	                        	 temp[0] = temp[0] + cr[0];
	                        	 temp[1] = temp[1] + ci[0];
	                         } // for (i = j+1; i < n; i++)
                         } // if (noconj)
                         else {
                        	 if (nounit) {
	                        	 zmlt(temp[0], temp[1], A[j][j][0], -A[j][j][1], cr, ci);
	                        	 temp[0] = cr[0];
	                        	 temp[1] = ci[0];
	                         }
	
	                         for (i = j + 1; i < n; i++) {
	                             ix = ix + incx;
	                             zmlt(A[i][j][0], -A[i][j][1], x[ix][0], x[ix][1], cr, ci);
	                        	 temp[0] = temp[0] + cr[0];
	                        	 temp[1] = temp[1] + ci[0];
	                         } // for (i = j+1; i < n; i++)	 
                         } // else

                         x[jx][0] = temp[0];
                         x[jx][1] = temp[1];
                         jx = jx + incx;
                     } // for (j = 0; j < n; j++)
                 } // else incx != 1
             } // else ((uplo == 'L') || (uplo == 'l'))
         } // else trans != 'N' && trans != 'n'

         return;
     } // ztrmv
     
     /**
      * This is a port of LAPACK version 3.7.0 auxiliary routine DLACPY. Original DLACPY created by Univ. of Tennessee,
      * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., December, 2016 zlacpy copies all or part of
      * a two-dimensional matrix A to another matrix B.
      * 
      * @param uplo input char Specifies the part of the matrix A to be copied to B. = 'U': Upper triangular part = 'L':
      *            Lower triangular part Otherwise: All of the matrix A
      * @param m input int The number of rows of the matrix A. m >= 0.
      * @param n input int The number of columns of the matrix A. n >= 0.
      * @param A input double[][][2] complex of dimension (lda,n). Has m by n matrix A. If uplo = 'U', only the upper triangle or
      *            trapezoid is accessed; if uplo = 'L', only the lower triangle or trapezoid is accessed.
      * @param lda input int The leading dimension of the array A. lda >= max(1,m).
      * @param B output double[][][2] co mplexof dimension (ldb,n). On exit, B = A in the locations specified by uplo.
      * @param ldb input int The leading dimension of the array B. ldb >= max(1,m).
      */
     public void zlacpy(final char uplo, final int m, final int n, final double[][][] A, final int lda,
             final double[][][] B, final int ldb) {
         int i, j;

         if ( (uplo == 'U') || (uplo == 'u')) {

             for (j = 0; j < n; j++) {

                 for (i = 0; i <= Math.min(j, m - 1); i++) {
                     B[i][j][0] = A[i][j][0];
                     B[i][j][1] = A[i][j][1];
                 }
             }
         } // if ((uplo == 'U') || (uplo == 'u'))
         else if ( (uplo == 'L') || (uplo == 'l')) {

             for (j = 0; j < n; j++) {

                 for (i = j; i < m; i++) {
                     B[i][j][0] = A[i][j][0];
                     B[i][j][1] = A[i][j][1];
                 }
             }
         } // else if ((uplo == 'L') || (uplo == 'l'))
         else {

             for (j = 0; j < n; j++) {

                 for (i = 0; i < m; i++) {
                     B[i][j][0] = A[i][j][0];
                     B[i][j][1] = A[i][j][1];
                 }
             }
         } // else
         return;
     } // zlacpy
     
     /**
      * This is a a port of BLAS level1 routine ZDOTU version 3.7.0.  Provided by  Univ. of Tennessee, Univ. of
     * California Berkeley, Univ. of Colorado Denver, and NAG Ltd., December, 2016 Original code Jack Dongarra,
     * 3/11/78 and modifed 12/3/93.  zdotu forms the dot product of two complex vectors.
     *            zdotu = x^T * y
     * @param n input integer number of elements in the input vectors
     * @param zx input double[][2] complex array, dimension (1 + (n-1)*abs(incx))
     * @param incx input integer storage spacing between elements of zx
     * @param zy input double[][2] complex array, dimension (1 + (n-10*abs(incy))
     * @param incy input integer storage spacing between elements of zy
     */
     private double[] zdotu(int n, double zx[][], int incx, double zy[][], int incy) {
         double ztemp[] = new double[2];
         int i, ix, iy;
         double cr[] = new double[1];
         double ci[] = new double[1];
         
         if (n <= 0) {
        	 return null;
         }
         
         if ((incx == 1) && (incy == 1)) {
        	 // Code for both increments equal to 1
        	 for (i = 0; i < n; i++) {
        	     zmlt(zx[i][0], zx[i][1], zy[i][0], zy[i][1], cr, ci);
        	     ztemp[0] = ztemp[0] + cr[0];
        	     ztemp[1] = ztemp[1] + ci[0];
        	 }
         }
         else {
        	 // Code for unequal increments or increments not equal to 1
        	 ix = 1;
        	 iy = 1;
        	 if (incx < 0) {
        		 ix = (-n+1)*incx + 1;
        	 }
        	 if (incy < 0) {
        		 iy = (-n+1)*incy + 1;
        	 }
        	 for (i = 1; i <= n; i++) {
        	     zmlt(zx[ix-1][0], zx[ix-1][1], zy[iy-1][0], zy[iy-1][1], cr, ci);
        	     ztemp[0] = ztemp[0] + cr[0];
        	     ztemp[1] = ztemp[1] + ci[0];
        	     ix = ix + incx;
        	     iy = iy + incy;
        	 }
         } // else
         return ztemp;
     }
    
    /**
     * This is a port of version 3.7.0 auxiliary routine ZLASET. Original ZLASET created by Univ. of Tennessee, Univ. of
     * California Berkeley, Univ. of Colorado Denver, and NAG Ltd., December, 2016 zlaset initializes an m-by-n matrix A
     * to beta on the diagonal and alpha on the offdiagonals.
     * 
     * @param uplo input char Specifies the part of the matrix to be set. = 'U': Upper triangular part is set; the
     *            strictly lower triangular part of A is not changed. = 'L': Lower triangular part is set; the strictly
     *            upper triangular part of A is not changed. Otherwise: All of the matrix A is set.
     * @param m input int The number of rows of the matrix A. m >= 0.
     * @param n input int The number of columns of the matrix A. n >= 0.
     * @param alpha input double[2] The complex constant to which the offdiagonal elements are to be set.
     * @param beta input double[2] The complex constant to which the diagonal elements are to be set.
     * @param A input/output double[][][2] complex of dimension lda by n. On exit, the leading m-by-n submatrix of A is set as
     *            follows: If uplo = 'U', A(i,j) = alpha, 0 <= i <= j-1, 0 <= j <= n-1, If uplo = 'L', A(i,j) = alpha,
     *            j+1 <= i <= m-1, 0 <= j <= n-1, Otherwise, A(i,j) = alpha, 0 <= i <= m-1, 0 <= j <= n-1, i!= j and,
     *            for all uplo, A(i,i) = beta, 0 <= i <= min(m-1,n-1).
     * @param lda input int The leading dimension of the array A. lda >= max(1,m).
     */
    public void zlaset(final char uplo, final int m, final int n, final double alpha[], final double beta[],
            final double[][][] A, final int lda) {
        int i;
        int j;
        int p;

        if ( (uplo == 'U') || (uplo == 'u')) {

            // Set the srictly upper triangular or trapezoidal part of the array to
            // alpha.
            for (j = 1; j < n; j++) {

                for (i = 0; i <= Math.min(j - 1, m - 1); i++) {
                	for (p = 0; p < 2; p++) {
                        A[i][j][p] = alpha[p];
                	}
                }
            }
        } // if ((uplo == 'U') || (uplo == 'u'))
        else if ( (uplo == 'L') || (uplo == 'l')) {

            // Set the strictly lower triangular or trapezoidal part of the array to
            // alpha.
            for (j = 0; j <= Math.min(m - 1, n - 1); j++) {

                for (i = j + 1; i <= (m - 1); i++) {
                	for (p = 0; p < 2; p++) {
                        A[i][j][p] = alpha[p];
                	}
                }
            }
        } // else if ((uplo == 'L') || (uplo == 'l'))
        else {

            // Set the leading m-by-n submatrix to alpha
            for (j = 0; j < n; j++) {

                for (i = 0; i < m; i++) {
                	for (p = 0; p < 2; p++) {
                        A[i][j][p] = alpha[p];
                	}
                }
            }
        } // else

        // Set the first min(m,n) diagonal elements to beta
        for (i = 0; i <= Math.min(m - 1, n - 1); i++) {
        	for (p = 0; p < 2; p++) {
                A[i][i][p] = beta[p];
        	}
        }

        return;
    } // zlaset
    
    /**
     * This is a port of the LAPACK routine ZLARND.f version 3.7.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., December, 2016
     * 
     * zlarnd returns  a random complex number from a uniform or normal distribution.
     * @ param input int idist
     *         Specifies the distribution of the random numbers
     *         = 1: real and imaginary parts each uniform (0,1)
     *         = 2: real and imaginary parts each uniform (-1,1)
     *         = 3: real and imaginary parts each normal (0,1)
     *         = 4: uniformly distributed on the disc abs(z) <= 1
     *         = 5: uniformly distributed on the circle abs(z) = 1
     * @ param input/output int array of dimension 4
     *         On entry, the seed of the random number generator; the array elements
     *         must be between 0 and 4095, and iseed[3] must be odd.
     *         On exit, the seed is updated.
     *         
     * This routine calls the auxiliary routine dlaran to generate a random
     * real number from a uniform (0,1) distribution.  The Box-Muller method
     * is used to transform numbers from a uniform to a normal distribution
     */
    private double[] zlarnd(int idist, int iseed[]) {
    	double t1;
    	double t2;
    	double var;
    	double arg;
    	double result[] = new double[2];
    	
    	// Generate a pair of real random numbers from a uniform (0,1) distribution
    	t1 = ge.dlaran(iseed);
    	t2 = ge.dlaran(iseed);
    	
    	if (idist == 1) {
    		// real and imaginary parts each uniform (0,1)
    	    result[0] = t1;
    	    result[1] = t2;
    	}
    	else if (idist == 2) {
    		// real and imaginary parts each uniform (-1,1)
    		result[0] = 2.0*t1-1.0;
    		result[1] = 2.0*t2-1.0;
    	}
    	else if (idist == 3) {
    		// real and imaginary parts 4each normal (0,1)
    	    var = Math.sqrt(-2.0*Math.log(t1));
    	    arg = 2.0*Math.PI*t2;
    	    result[0] = var * Math.cos(arg);
    	    result[1] = var * Math.sin(arg);
    	}
    	else if (idist == 4) {
    		// uniform distribution of the unit disc abs(z) <= 1
    		var = Math.sqrt(t1);
    		arg = 2.0*Math.PI*t2;
    	    result[0] = var * Math.cos(arg);
    	    result[1] = var * Math.sin(arg);
    	}
    	else if (idist == 5) {
    		// uniform distribution on the unit circle abs(z) = 1
    		arg = 2.0*Math.PI*t2;
    	    result[0] = Math.cos(arg);
    	    result[1] = Math.sin(arg);
    	}
    	return result;
    }
    
    /**
     * This is a port of version 3.7.0 LAPACK auxiliary test routine ZLAROT Original ZLAROT created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., December, 2016 zlarot applies a (Givens) rotation to two adjacent
     * rows or columns, where one element of the first and/or last collumn/row for use on matrices stored in some format
     * other than GE, so that elements of the matrix may be used or modified for which no array element is provided. 
     * <p>
     * One example is a symmetric matrix in SB format (bandwidth=4), for which uplo = 'L': Two adjacent rows will have
     * the format: 
     * row j: * * * * * . . . . 
     * row j+1: * * * * * . . . . '*' indicates elements for which storage is
     * provided, '.' indicates elements for which no storage is provided, but are not necessrily zero; their values are
     * determined by symmetry. ' ' indicates elements which are necessarily zero, and have no storage provided.
     * </p>
     * 
     * <p>
     * Those columns which have two '*'s can be handled by zrot. Those columns which have no '*'s can be ignored, since
     * as long as the Givens rotations are carefully applied to preserve symmetry, their values are determined. Those
     * columns which have one '*' have to be handled separately, by using separate variables "p" and "q": 
     * row j: * * * * * * p . . . 
     * row j+1: q * * * * * . . . .
     * </p>
     * 
     * <p>
     * The element p would have to be set correctly, then that column is rotated, setting p to its new value. The next
     * call to zlarot would rotate columns j and j+1, using p, and restore symmetry. The element q would start out being
     * zero, and be made non-zero by the rotation. Later, rotations would presumably be chosen to zero q out.
     * </p>
     * 
     * <p>
     * Typical Calling Sequences: rotating the i-th and (i+1)-st rows.
     * </p>
     * 
     * <p>
     * General dense matrix: zlarot(true, false, false, n, c, s, A[i-1][0], lda, dummy, dummy);
     * </p>
     * 
     * <p>
     * General banded matrix in GB format: j = Math.max(1, i-kl); nl = Math.min(n, i+ku+1) + 1 - j; dlarot(true, (i-kl)
     * >= 1, (i+ku) < n, nl, c,s,A[ku+i-j][j-1], lda - 1, xleft, xright); Note that i+1-j is just min(i,kl+1)
     * </p>
     * 
     * <p>
     * Symmetric banded matrix in SY format, bandwidth k, lower triangle only: j = Math.max(1,i-k); nl = Math.min(k+1,i)
     * + 1; zlarot(true, (i-k) >= 1, true, nl, c, s, A[i-1][j-1], lda, xleft, xright);
     * </p>
     * 
     * <p>
     * Same, but upper triangle only: nl = Math.min(k+1,n-i) + 1; zlarot(true, true, (i+k) < n, nl, c, s, A[i-1][i-1],
     * lda, xleft, xright);
     * </p>
     * 
     * <p>
     * Symmetric banded matrix in SB format, bandwidth k, lower triangle only: same as for sy, except: A[i-j][j-1], lda
     * - 1, xleft, xright); Note that i+1-j is just min(i,k+1)
     * </p>
     * 
     * <p>
     * Same, but upper triangle only: A[k][i-1], lda-1, xleft, xright);
     * </p>
     * 
     * <p>
     * Rotating columns is just the transpose of rotating rows, except for GB and SB: (rotating columns i and i+1)
     * </p>
     * 
     * <p>
     * GB: j = Math.max(1,i-ku); nl = Math.min(n, i+kl+1) + 1 - j; zlarot(true, i-ku >= 1, i+kl < n, nl, c, s,
     * A[ku+j-i][i-1], lda - 1, xtop, xbottm); Note that ku+j+1-i is just max(1,ku+2-i)
     * </p>
     * 
     * <p>
     * SB: (upper triangle) ............. A[k+j-i][i-1], lda-1, xtop, xbottm);
     * </p>
     * 
     * <p>
     * SB: (lower triangle) ................... A[0][i-1], lda-1, xtop, xbottm);
     * </p>
     * 
     * @param lrows input boolean If true, then zlarot will rotate two rows. If false, then it will rotate two columns.
     * @param lleft input boolean If true, then xleft will be used instead of the corresponding element of A for the
     *            first element in the second row (if lrows = false) or column (if lrows = true) If false, then the
     *            corresponding element of A will be used.
     * @param lright input boolean If true, then xright will be used instead of the corresponding element of A for the
     *            last element in the first row (if lrows = false) or column (if lrows = true). If false, then the
     *            corresponding element of A will be used.
     * @param nl input int The length of the rows (if lrows = true) or columns (if lrows = false) to be rotated. If
     *            xleft and/or xright are used, the columns/rows they are in should be included in nl, e.g., if lleft =
     *            lright = true, then nl must be at least 2. The number of rows/columns to be rotated exclusive of those
     *            involving xleft and/or xright may not be negative, i.e., nl minus how many of lleft and lright are
     *            true must be at least zero; if not, an error message will be output.
     * @param c input double[2] complex
     * @param s input double[2] complex c and s specify the Givens rotation to be applied. If lrows is true, then the matrix
     *                ( c s) 
     *                (-s c ) 
     *            is applied from the left; if false, then the transpose (not conjugated) thereof is applied from the right.
     *            Note that in contrast to the output of zrotg or to most versions of zrot, both c and s are complex.
     *            For a Givens rotation, |c|**2 + |s|**2 should be 1, but this is not checked.
     * @param A input/output double[][2] The complex array containing the rows/columns to be rotated. The first element of A should
     *            be the upper left element to be rotated.
     * @param lda input int The "effective" leading dimension of A. If A contains a matrix stored in GE, HE, or SY format,
     *            then this is just the leading dimension of A as dimensioned in the calling routine. If A contains a
     *            matrix stored in band (GB, HB, or SB) format, then this should be *one less* than the leading dimension
     *            used in the calling routine. Thus, if A were dimensioned A(lda,*) in zlarot, then A[0][j-1] would be
     *            the j-th element in the first of the two rows to be rotated, and A[1][j-1] would be the j-th in the
     *            second, regardless of how the array may be stored in the calling routine. [A cannot, however, actually
     *            be dimensioned thus, since for band format, the row number may exceed lda, which is not legal code.]
     *            If lrows = true, then lda must be at least 1, otherwise it must be at least nl minus the number of
     *            true values in xleft and xright.
     * @param xleft input/output double[][2] complex If lleft is true, then xleft will be used and modified instead of A[1][0] (if
     *            lrows = true) or A[0][1] (if lrows = false).
     * @param xright input/output double[][2] complex If lright is true, then xright will be used and modified instead of
     *            A[0][nl-1] (if lrows = true) or A[nl-1][0] (if lrows = false).
     */
    public void zlarot(final boolean lrows, final boolean lleft, final boolean lright, final int nl, final double c[],
            final double s[], final double[][] A, final int lda, final double[] xleft, final double[] xright) {
        int iinc;
        int inext;
        int ix;
        int iy;
        int iyt = 0;
        int nt;
        final double[][] xt = new double[2][2];
        final double[][] yt = new double[2][2];
        int j;
        double cr[] = new double[1];
        double ci[] = new double[1];
        double cr2[] = new double[1];
        double ci2[] = new double[1];
        double tempx[] = new double[2];

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
            xt[0][0] = A[0][0];
            xt[0][1] = A[0][1];
            yt[0][0] = xleft[0];
            yt[0][1] = xleft[1];
        } else {
            nt = 0;
            ix = 1;
            iy = 1 + inext;
        }

        if (lright) {
            iyt = 1 + inext + ( (nl - 1) * iinc);
            nt = nt + 1;
            xt[nt - 1][0] = xright[0];
            xt[nt - 1][0] = xright[1];
            yt[nt - 1][0] = A[iyt - 1][0];
            yt[nt - 1][1] = A[iyt - 1][1];
        }

        // Check for errors
        if (nl < nt) {
            MipavUtil.displayError("zlarot has error 4");

            return;
        }

        if ( (lda <= 0) || ( ( !lrows) && (lda < (nl - nt)))) {
            MipavUtil.displayError("zlarot had error 8");

            return;
        }

        // Rotate
        for (j = 0; j <= nl-nt-1; j++) {
        	zmlt(c[0], c[1], A[ix+j*iinc-1][0], A[ix+j*iinc-1][1], cr, ci);
        	zmlt(s[0], s[1], A[iy+j*iinc-1][0], A[iy+j*iinc-1][1], cr2, ci2);
        	tempx[0] = cr[0] + cr2[0];
        	tempx[1] = ci[0] + ci2[0];
        	zmlt(-s[0], s[1], A[ix+j*iinc-1][0], A[ix+j*iinc-1][1], cr, ci);
        	zmlt(c[0], -c[1], A[iy+j*iinc-1][0], A[iy+j*iinc-1][1], cr2, ci2);
        	A[iy+j*iinc-1][0] = cr[0] + cr2[0];
        	A[iy+j*iinc-1][1] = ci[0] + ci2[0];
        	A[ix+j*iinc-1][0] = tempx[0];
        	A[ix+j*iinc-1][1] = tempx[1];
        } // for (j = 0; j <= nl-nt-1; j++)
        
        for (j = 1; j <= nt; j++) {
            zmlt(c[0], c[1], xt[j-1][0], xt[j-1][1], cr, ci);
            zmlt(s[0], s[1], yt[j-1][0], yt[j-1][1], cr2, ci2);
            tempx[0] = cr[0] + cr2[0];
            tempx[1] = ci[0] + ci2[0];
            zmlt(-s[0], s[1], xt[j-1][0], xt[j-1][1], cr, ci);
            zmlt(c[0], -c[1], yt[j-1][0], yt[j-1][1], cr2, ci2);
            yt[j-1][0] = cr[0] + cr2[0];
            yt[j-1][1] = ci[0] + ci2[0];
            xt[j-1][0] = tempx[0];
            xt[j-1][1] = tempx[1];
        } // for (j = 1; j <= nt; j++)

        // Stuff values back into xleft, xright, etc.
        if (lleft) {
            A[0][0] = xt[0][0];
            A[0][1] = xt[0][1];
            xleft[0] = yt[0][0];
            xleft[1] = yt[0][1];
        }

        if (lright) {
            xright[0] = xt[nt - 1][0];
            xright[1] = xt[nt - 1][1];
            A[iyt - 1][0] = yt[nt - 1][0];
            A[iyt - 1][1] = yt[nt - 1][1];
        }

        return;
    } // zlarot
    
    /**
     * This is a port of version 3.7.0 LAPACK auxiliary routine ZLARTG Original ZLARTG created by Univ. of Tennessee,
     * Univ. of California Berkeley, Univ. of Colorado Denver, and NAG Ltd., December, 2016
     * zlartg generates a plane rotation with real cosine and complex sine.
     * zlartg generates a plane rotation so that 
     * [  cs  sn ] . [ f ] = [ r ] where cs**2 + |sn|**2 = 1.
     * [ -sn  cs ]   [ g ]   [ 0 ] 
     * If g = 0, then cs = 1 and sn = 0. 
     * If f = 0, then cs = 0 and sn is chosenso that r is real.
     * 
     * @param f input double[2] The first complex component of the vector to be rotated.
     * @param g input double[2] The second complex component of the vector to be rotated.
     * @param cs output double[1] The real cosine of the rotation.
     * @param sn output double[2] The complex sine of the rotation.
     * @param r output double[2] The nonzero complex component of the rotated vector.
     * 
     * 3/5/1996 Modified with a new algorithm by W. Kahan and J. Demmel
     */
    public void zlartg(final double f[], final double g[], final double[] cs, final double[] sn, final double[] r) {
        int count;
        int i;
        double eps;
        double scale;
        double fs[] = new double[2];
        double gs[] = new double[2];
        double f2;
        double g2;
        double d;
        double f2s;
        double g2s;
        double ff[] = new double[2];
        double dr;
        double di;
        double cr[] = new double[1];
        double ci[] = new double[1];

        if (first_zlartg) {
            first_zlartg = false;
            safmin = ge.dlamch('S');
            eps = ge.dlamch('E');
            safmn2 = Math.pow(ge.dlamch('B'), (int) (Math.log(safmin / eps) / Math.log(ge.dlamch('B')) / 2.0));
            safmx2 = 1.0 / safmn2;
        } // if (first_zlartg)
        scale = Math.max(abs1(f), abs1(g));
        fs[0] = f[0];
        fs[1] = f[1];
        gs[0] = g[0];
        gs[1] = g[1];
        count = 0;
        
        if (scale >= safmx2) {
            do {
                count = count + 1;
                fs[0] = fs[0] * safmn2;
                fs[1] = fs[1] * safmn2;
                gs[0] = gs[0] * safmn2;
                gs[1] = gs[1] * safmn2;
                scale = scale * safmn2;
            } while (scale >= safmx2);
        } // if (scale >= safmx2)
        else if (scale <= safmn2) {
            if (((g[0] == 0.0) && (g[1] == 0.0)) || Double.isNaN(zabs(g[0],g[1]))) {
                cs[0] = 1.0;
                sn[0] = 0.0;
                sn[1] = 0.0;
                r[0] = f[0];
                r[1] = f[1];
                return;
            } // if (((g[0] == 0.0) && (g[1] == 0.0)) || Double.isNaN(zabs(g[0],g[1])))
            do {
                count = count + 1;
                fs[0] = fs[0] * safmx2;
                fs[1] = fs[1] * safmx2;
                gs[0] = gs[0] * safmx2;
                gs[1] = gs[1] * safmx2;
                scale = scale * safmx2;
            } while(scale <= safmn2);
        } // else if (scale <= safmn2)
        f2 = abssq(fs);
        g2 = abssq(gs);
        if (f2 <= Math.max(g2,  1.0) * safmin) {
            // This is a rare case: f is very small.
        	if ((f[0] == 0.0) && (f[1] == 0.0)) {
        	    cs[0] = 0.0;
        	    r[0] = ge.dlapy2(g[0],  g[1]);
        	    // Do complex/rea division explicitly with two real divisions
        	    d = ge.dlapy2(gs[0], gs[1]);
        	    sn[0] = gs[0]/d;
        	    sn[1] = -gs[1]/d;
        	    return;
        	} // if ((f[0] == 0.0) && (f[1] == 0.0))
        	f2s = ge.dlapy2(fs[0],  fs[1]);
        	// g2 and g2s are accurate.
        	// g2s is at least safmin, and g2s is at least safmn2
        	g2s = Math.sqrt(g2);
        	// Error in cs form underflow in f2s is at most
        	// unfl / safmn2 .lt. sqrt(unfl*eps) .lt. eps
        	// If max(g2,1.0) = g2, then f2 .lt. g2*safmin
        	// and so cs .lt. sqrt(safmin)
        	// If max(g2,1.0) = 1.0, then f2 .lt. safmin
        	// and so cs .lt. sqrt(safmin)/safmn2 = sqrt(eps)
        	// Therefore, cs = f2s/g2s/sqrt(1 + (f2s/g2s)**2) = f2s/g2s
        	cs[0] = f2s/g2s;
        	// Make sure abs(ff) = 1
        	// Do complex/real division explicitly with 2 real divisions
        	if (abs1(f) > 1.0) {
        	    d = ge.dlapy2(f[0], f[1]);
        	    ff[0] = f[0]/d;
        	    ff[1] = f[1]/d;
        	} // if (abs1(f) > 1.0)
        	else {
        		dr = safmx2 * f[0];
        		di = safmx2 * f[1];
        		d = ge.dlapy2(dr, di);
        		ff[0] = dr/d;
        		ff[1] = di/d;
        	} // else
        	zmlt(ff[0], ff[1], gs[0]/g2s, -gs[1]/g2s, cr, ci);
        	sn[0] = cr[0];
        	sn[1] = ci[0];
        	zmlt(sn[0], sn[1], g[0], g[1], cr, ci);
        	r[0] = cs[0]*f[0] + cr[0];
        	r[1] = cs[0]*f[1] + ci[0];
        } // if (f2 <= Math.max(g2,  1.0) * safmin)
        else {
        	// This is the most common case.
        	// Neither f2 nor f2/g2 are less than safmin
        	// f2s cannot overflow, and it is accurate
        	
        	f2s = Math.sqrt(1.0 + g2/f2);
        	// DO the f2s(ral)*fs(complex) multiply with two real multipliers
        	r[0] = f2s * fs[0];
        	r[1] = f2s * fs[1];
        	cs[0] = 1.0/f2s;
        	d = f2 + g2;
        	// Do complex/real division explicitly with two real divisions
        	sn[0] = r[0]/d;
        	sn[1] = r[1]/d;
        	zmlt(sn[0], sn[1], gs[0], -gs[1], cr, ci);
        	sn[0] = cr[0];
        	sn[1] = ci[0];
        	if (count != 0) {
        		if (count > 0) {
        			for (i = 1; i <= count; i++) {
        				r[0] = r[0] * safmx2;
        				r[1] = r[1] * safmx2;
        			}
        		} // if (count > 0)
        		else { // count < 0
        			for (i = 1; i <= -count; i++) {
        				r[0] = r[0] * safmn2;
        				r[1] = r[1] * safmn2;
        			}
        		} // else count < 0
        	} // if (count != 0)
        } // else

        return;
    } // zlartg
    
    /**
     * This is a port of version 3.7.0 LAPACK auxiliary test routine zlagsy Original ZLAGSY created by Univ. of Tennessee,
     * Univ. of California Berkeley, and NAG Ltd., December, 2016 zlagsy generates a complex symmetric matrix A, by pre-
     * and post- multiplying a real diagonal matrix D with a random unitary matrix: A = U*D*U**T. The semi-bandwidth
     * may then be reduced to k by additional unitary transformations.
     * 
     * @param n input int The order of the matrix A. n >= 0.
     * @param k input int The number of nonzero subdiagonals within the band of A. 0 <= k <= n-1.
     * @param D input double[] of dimension n. The diagonal elements of the diagonal matrix D.
     * @param A output double[][][2] complex of dimension (lda,n) The generated n by n symmetric matrix A (the full matrix is
     *            stored).
     * @param lda input int The leading dimension of the array A. lda >= n.
     * @param iseed input/output int[] of dimension 4 On entry, the seed of the random number generator; the array
     *            elements must be between 0 and 4095, and iseed[3] must be odd. On exit, the seed is updated.
     * @param work workspace double[][2] complex of dimension (2*n)
     * @param info output int[] = 0: successful exit < 0: If info[0] = -i, the i-th argument had an illegal value
     */
    public void zlagsy(final int n, final int k, final double[] D, final double[][][] A, final int lda,
            final int[] iseed, final double[][] work, final int[] info) {
        int i;
        int j;
        int m;
        double alpha[] = new double[2];
        double tau[] = new double[2];
        double wa[] = new double[2];
        double wb[] = new double[2];
        double wn;
        double[][][] B;
        final double[][] work2 = new double[n][2];
        double[][] x;
        double cr[] = new double[1];
        double ci[] = new double[1];
        double cr2[] = new double[1];
        double ci2[] = new double[1];
        int p;
        double ratio;
        double alp[] = new double[2];
        double beta[] = new double[2];
        double ztemp[];
        int jj;
        int ii;

        // Test the input arguments
        info[0] = 0;

        if (n < 0) {
            info[0] = -1;
        } else if ( (k < 0) || (k > (n - 1))) {
            info[0] = -2;
        } else if (lda < Math.max(1, n)) {
            info[0] = -5;
        }

        if (info[0] < 0) {
            MipavUtil.displayError("Error zlagsy had info[0] = " + info[0]);

            return;
        }

        // initialize lower triangle of A to diagonal matrix
        for (j = 0; j < n; j++) {

            for (i = j + 1; i < n; i++) {
            	for (p = 0; p < 2; p++) {
                    A[i][j][p] = 0.0;
            	}
            }
        }

        for (i = 0; i < n; i++) {
            A[i][i][0] = D[i];
            A[i][i][1] = 0.0;
        }

        // Generate lower triangle of symmetric matrix
        for (i = n - 1; i >= 1; i--) {

            // generate random reflection
            zlarnv(3, iseed, n - i + 1, work);
            wn = dznrm2(n - i + 1, work, 1);
            ratio = wn/zabs(work[0][0], work[0][1]);
            wa[0] = ratio * work[0][0];
            wa[1] = ratio * work[0][1];
            if (wn == 0.0) {
            	tau[0] = 0.0;
            	tau[1] = 0.0;
            }
            else {
            	wb[0] = work[0][0] + wa[0];
            	wb[1] = work[0][1] + wa[1];
            	zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
            	for (j = 0; j < (n-i); j++) {
            		zmlt(cr[0], ci[0], work[j+1][0], work[j+1][1], cr2, ci2);
            		work[j+1][0] = cr2[0];
            		work[j+1][1] = ci2[0];
            	}
            	work[0][0] = 1.0;
            	work[0][1] = 0.0;
            	zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
            	tau[0] = cr[0];
            	tau[1] = 0.0;
            }

            // apply random reflection to A(i-1:n-1,i-1:n-1) from the left and
            // the right
            // compute y = tau * A * conjg(u)
            for (j = 0; j < n-i+1; j++) {
            	work[i][1] = -work[i][1];
            }
            B = new double[n - i + 1][n - i + 1][2];

            for (j = 0; j < (n - i + 1); j++) {

                for (m = 0; m < (n - i + 1); m++) {
                	for (p = 0; p < 2; p++) {
                        B[j][m][p] = A[i - 1 + j][i - 1 + m][p];
                	}
                }
            }

            beta[0] = 0.0;
            beta[1] = 0.0;
            zsymv('L', n - i + 1, tau, B, n - i + 1, work, 1, beta, work2, 1);
            
            for (j = 0; j < n-i+1; j++) {
            	work[i][1] = -work[i][1];
            }

            // compute v = y - 1/2 * tau * (u, y) * u
            ztemp = zdotc(n - i + 1, work, 1, work2, 1);
            zmlt(-0.5*tau[0], -0.5*tau[1], ztemp[0], ztemp[1], cr, ci);
            alpha[0] = cr[0];
            alpha[1] = ci[0];
            zaxpy(n - i + 1, alpha, work, 1, work2, 1);

            // apply the transformation as a rank-2 update to A(i-1:n-1,i-1:n-1)
            //zsyr2('L', n - i + 1, -1.0, work, 1, work2, 1, B, n - i + 1);

            //for (j = 0; j < (n - i + 1); j++) {

                //for (m = 0; m < (n - i + 1); m++) {
                	//for (p = 0; p < 2; p++) {
                       // A[i - 1 + j][i - 1 + m][p] = B[j][m][p];
                	//}
                //}
            //}
            for (jj = i; jj <= n; jj++) {
                for (ii = jj; ii <= n; ii++) {
                    zmlt(work[ii-i][0], work[ii-i][1], work2[jj-i][0], work2[jj-i][1], cr, ci);
                    zmlt(work2[ii-i][0], work2[ii-i][1], work[jj-i][0], work[jj-i][1], cr2, ci2);
                    A[ii-1][jj-1][0] = A[ii-1][jj-1][0] - cr[0] - cr2[0];
                    A[ii-1][jj-1][1] = A[ii-1][jj-1][1] - ci[0] - ci2[0];
                } // for (ii = jj; ii <= n; ii++)
            } //  for (jj = i; jj <= n; jj++)
        } // for (i = n-1; i >= 1; i--)

        // Reduce number of subdiagonals to k
        for (i = 1; i <= (n - 1 - k); i++) {

            // generate reflection to annihilate A(k+i:n-1,i-1)
            x = new double[n - k - i + 1][2];

            for (j = 0; j < (n - k - i + 1); j++) {
            	for (p = 0; p < 2; p++) {
                    x[j][p] = A[k + i - 1 + j][i - 1][p];
            	}
            }

            wn = dznrm2(n - k - i + 1, x, 1);
            ratio = wn/zabs(A[k+i-1][i-1][0], A[k+i-1][i-1][1]);
            wa[0] = ratio * A[k+i-1][i-1][0];
            wa[1] = ratio * A[k+i-1][i-1][1];
            if (wn == 0.0) {
            	tau[0] = 0.0;
            	tau[1] = 0.0;
            }
            else {
            	wb[0] =   A[k+i-1][i-1][0] + wa[0];
            	wb[1] =   A[k+i-1][i-1][1] + wa[1];
            	zdiv(1.0, 0.0, wb[0], wb[1], cr, ci);
            	for (j = 0; j < (n-k-i); j++) {
            		zmlt(cr[0], ci[0], A[k+i+j][i-1][0], A[k+i+j][i-1][1], cr2, ci2);
            		A[k+i+j][i-1][0] = cr2[0];
            		A[k+i+j][i-1][1] = ci2[0];
            	}
            	A[k+i-1][i-1][0] = 1.0;
            	A[k+i-1][i-1][1] = 0.0;
            	zdiv(wb[0], wb[1], wa[0], wa[1], cr, ci);
            	tau[0] = cr[0];
            	tau[1] = 0.0;
            }

            // apply reflection to A(k+i-1:n-1,i:k+i-2) from the left
            B = new double[n - k - i + 1][k - 1][2];

            for (j = 0; j < (n - k - i + 1); j++) {

                for (m = 0; m < (k - 1); m++) {
                	for (p = 0; p < 2; p++) {
                        B[j][m][p] = A[k + i - 1 + j][i + m][p];
                	}
                }
            }

            x = new double[n - k - i + 1][2];

            for (j = 0; j < (n - k - i + 1); j++) {
            	for (p = 0; p < 2; p++) {
                    x[j][p] = A[k + i - 1 + j][i - 1][p];
            	}
            }

            alp[0] = 1.0;
            alp[1] = 0.0;
            beta[0] = 0.0;
            beta[1] = 0.0;
            zgemv('C', n - k - i + 1, k - 1, alp, B, n - k - i + 1, x, 1, beta, work, 1);
            alp[0] = -tau[0];
            alp[1] = -tau[1];
            zgerc(n - k - i + 1, k - 1, alp, x, 1, work, 1, B, n - k - i + 1);

            for (j = 0; j < (n - k - i + 1); j++) {

                for (m = 0; m < (k - 1); m++) {
                	for (p = 0; p < 2; p++) {
                        A[k + i - 1 + j][i + m][p] = B[j][m][p];
                	}
                }
            }

            // apply reflection to A(k+i-1:n-1,k+i-1:n-1) from the left and
            // the right
            // compute y = tau * A * conjg(u)
            for (j = 0; j < n-k-i+1; j++) {
            	A[k+i-1+j][i-1][1] = -A[k+i-1+j][i-1][1];
            }
            B = new double[n - k - i + 1][n - k - i + 1][2];

            for (j = 0; j < (n - k - i + 1); j++) {

                for (m = 0; m < (n - k - i + 1); m++) {
                	for (p = 0; p < 2; p++) {
                        B[j][m][p] = A[k + i - 1 + j][k + i - 1 + m][p];
                	}
                }
            }

            x = new double[n - k - i + 1][2];

            for (j = 0; j < (n - k - i + 1); j++) {
            	for (p = 0; p < 2; p++) {
                    x[j][p] = A[k + i - 1 + j][i - 1][p];
            	}
            }

            beta[0] = 0.0;
            beta[1] = 0.0;
            zsymv('L', n - k - i + 1, tau, B, n - k - i + 1, x, 1, beta, work, 1);
            
            for (j = 0; j < n-k-i+1; j++) {
            	A[k+i-1+j][i-1][1] = -A[k+i-1+j][i-1][1];
            }

            // compute v = y - 1/2 * tau * (u, y) * u
            ztemp = zdotc(n - k - i + 1, work, 1, x, 1);
            zmlt(-0.5*tau[0], -0.5*tau[1], ztemp[0], ztemp[1], cr, ci);
            alpha[0] = cr[0];
            alpha[1] = ci[0];
            zaxpy(n - k - i + 1, alpha, x, 1, work, 1);

            // apply symmetric rank-2 update to A(k+i-1:n-1,k+i-1:n-1)
            //zsyr2('L', n - k - i + 1, -1.0, x, 1, work, 1, B, n - k - i + 1);

            //for (j = 0; j < (n - k - i + 1); j++) {

                //for (m = 0; m < (n - k - i + 1); m++) {
                    //A[k + i - 1 + j][k + i - 1 + m] = B[j][m];
                //}
            //}
            
            for (jj = k + i; jj <= n; jj++) {
            	for (ii = jj; ii <= n; ii++) {
            	    zmlt(A[ii-1][i-1][0], A[ii-1][i-1][1],work[jj-k-i][0], work[jj-k-i][1], cr, ci);
            	    zmlt(work[ii-k+i][0], work[ii-k+i][1], A[jj-1][i-1][0], A[jj-1][i-1][1], cr2, ci2);
            	    A[ii-1][jj-1][0] = A[ii-1][jj-1][0] - cr[0] - cr2[0];
            	    A[ii-1][jj-1][1] = A[ii-1][jj-1][1] - ci[0] - ci2[0];
            	} // for (ii = jj; i <= n; ii++)
            } // for (jj = k + i; jj <= n; jj++)

            for (p = 0; p < 2; p++) {
                A[k + i - 1][i - 1][p] = -wa[p];
            }

            for (j = k + i + 1; j <= n; j++) {
            	for (p = 0; p < 2; p++) {
                    A[j - 1][i - 1][p] = 0.0;
            	}
            }
        } // for (i = 1; i <= n - 1 - k; i++)

        // Store full symmetric matrix
        for (j = 0; j < n; j++) {

            for (i = j + 1; i < n; i++) {
            	for (p = 0; p < 2; p++) {
                    A[j][i][p] = A[i][j][p];
            	}
            }
        }

        return;
    } // zlagsy
    
    /**
     * BLAS level1 routine version 3.7.0 December, 2016
     * Port of 12/3/93 linpack zdotc routine Original version created by Jack Dongarra Forms the dot product of two
     * complex vectors x**H * Y.
     * 
     * @param n int number of elements input vector(s)
     * @param zx double[][2] complex array of dimension (1 + (n-1)*abs(incx))
     * @param incx int storage spacing between elements of zx
     * @param zy double[][2] complex array of dimension (1 + (n01)*abs(incy))
     * @param incy int storage spacing between elements of zy
     * 
     * @return double[2] complex answer
     */
    public double[] zdotc(final int n, final double[][] zx, final int incx, final double[][] zy, final int incy) {
        double cr[] = new double[1];
        double ci[] = new double[1];
        double ztemp[] = new double[2];
        int ix;
        int iy;
        int i;
        
        if (n <= 0) {
        	return null;
        }
        
        if ((incx == 1) && (incy == 1)) {
        	// Code for both increments equal to 1
        	for (i = 0; i < n; i++) {
        		zmlt(zx[i][0], -zx[i][1], zy[i][0], zy[i][1], cr, ci);
        		ztemp[0] = ztemp[0] + cr[0];
        		ztemp[1] = ztemp[1] + ci[0];
        	}
        } // if ((incx == 1) && (incy == 1))
        else {
        	// Code for unequal increments or equal increments not equal to 1
        	ix = 1;
        	iy = 1;
        	if (incx < 0) {
        		ix = (-n+1)*incx + 1;
        	}
        	if (incy < 0) {
        		iy = (-n+1)*incy + 1;
        	}
			for (i = 1; i <= n; i++) {
				zmlt(zx[ix-1][0], -zx[ix-1][1], zy[iy-1][0], zy[iy-1][1], cr, ci);
	    		ztemp[0] = ztemp[0] + cr[0];
	    		ztemp[1] = ztemp[1] + ci[0];
	    		ix = ix + incx;
	    		iy = iy + incy;
			} // for (i = 1; i <= n; i++)
        } // else
        
        return ztemp;
        
    } // zdotc
    
    /**
     * BLAS level1 routine version 3.7.0 December, 2016
     * Port of 12/3/93 linpack routine zaxpy Original version written by Jack Dongarra vector dy = vector dy + da *
     * vector dx.
     * 
     * @param n input int
     * @param za inut double[2] complex
     * @param zx input double[][2] complex array, dimension (1 + (n-1)*abs(incx))
     * @param incx input int storage spacing between elements of zx
     * @param zy input/output double[][2] complex, dimension (! = (n-1)*abs(incy))
     * @param incy input int spacing between elements of zy
     */
    public void zaxpy(final int n, final double za[], final double[][] zx, final int incx, final double[][] zy,
            final int incy) {
        int i;
        int ix;
        int iy;
        double cr[] = new double[1];
        double ci[] = new double[1];

        if (n <= 0) {
            return;
        }

        if ((za[0] == 0.0) && (za[1] == 0.0)) {
            return;
        }
        
        if ((incx == 1) && (incy == 1)) {
        	// Code for both increments equal to 1
        	for (i = 0; i < n; i++) {
        	    zmlt(za[0], za[1], zx[i][0], zx[i][1], cr, ci);
        	    zy[i][0] = zy[i][0] + cr[0];
        	    zy[i][1] = zy[i][1] + ci[0];
        	}
        }
        else {
        	// Code for unequal increments or equal increments not equal to 1
        	ix = 1;
        	iy = 1;
        	if (incx < 0) {
        		ix = (-n+1)*incx + 1;
        	}
        	if (incy < 0) {
        		iy = (-n+1)*incy + 1;
        	}
        	for (i = 1; i <= n; i++) {
        		zmlt(za[0], za[1], zx[ix-1][0], zx[ix-1][1], cr, ci);
        		zy[iy-1][0] = zy[iy-1][0] + cr[0];
        		zy[iy-1][1] = zy[iy-1][1] + ci[0];
        		ix = ix + incx;
        		iy = iy + incy;
        	} // for (i = 1; i <= n; i++)
        } // else

        return;
    } // zaxpy
    
    /**
     * LAPACK auxiliary routine version 3.7.0 December, 2016
     * This is a port of the 10/22/86 Blas routine ZSYMV Original code written by: Jack Dongarra, Argonne Nationa Lab.
     * Jeremy Du Croz, Nag Central Office. Sven Hammarling, Nag Central Office. Richard Hanson, Sandia National Labs.
     * zsymv performs the matrix-vector operation y = alpha*A*x + beta*y where alpha and beta are scalars, x and y are n
     * element vectors and A is an n by n symmetric matrix.
     * 
     * @param uplo input char On entry, uplo specifies whether the upper or lower triangular part of the array A is to
     *            be referenced as follows: = 'U' or 'u' Only the upper triangular part of A is to be referenced. = 'L'
     *            or 'l' Only the lower triangular part of A is to be referenced.
     * @param n input int On entry, n specifies the order of the matrix A. n must be at least zero.
     * @param alpha input double[2] Specified complex scalar
     * @param A input double[][][2] complex of dimension lda by n Before entry with uplo = 'U' or 'u', the leading n by n upper
     *            triangular part of the array A must contain the upper triagular part of the symmetric matrix and the
     *            strictly lower triangular part of A is not referenced. Before entry with uplo = 'L' or 'l', the
     *            leading n by n lower triangular part of the array A must contain the lower triangular part of the
     *            symmetric matrix and the strictly upper triangular part of A is not referenced.
     * @param lda input int On entry, lda specifies the first dimension of A as declared in the calling (sub) program.
     *            lda must be at least max(1,n).
     * @param x input double[][2] complex of dimension at least (1 + (n-1)*abs(incx)). Before entry, the incremented array x must
     *            contain the n element vector x.
     * @param incx input int On entry, incx specifies the increment for the elements of x. incx must not be zero.
     * @param beta input double[2] complex On entry, beta specifies the scalar beta. When beta is supplied as zero, then y need not
     *            be set on input.
     * @param y input/output double[][2] complex of dimension at least (1 + (n-1)*abs(incy)). Before entry, the incremented array y
     *            must contain the n element vector y. On exit, y is overwritten by the updated vector y.
     * @param incy input int On entry, incy specifies the increment for the elements of y. incy must not be zero.
     */
    public void zsymv(final char uplo, final int n, final double alpha[], final double[][][] A, final int lda,
            final double[][] x, final int incx, final double beta[], final double[][] y, final int incy) {
        double temp1[] = new double[2];
        double temp2[] = new double[2];
        int i;
        int info;
        int ix;
        int iy;
        int j;
        int jx;
        int jy;
        int kx;
        int ky;
        double cr[] = new double[1];
        double ci[] = new double[1];
        double cr2[] = new double[1];
        double ci2[] = new double[1];

        // Test the input parameters
        info = 0;

        if ( (uplo != 'U') && (uplo != 'u') && (uplo != 'L') && (uplo != 'l')) {
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
            MipavUtil.displayError("Error zsymv had error = " + info);

            return;
        }

        // Quick return if possible
        if ( (n == 0) || ( ((alpha[0] == 0.0) && (alpha[1] == 0.0)) && ((beta[0] == 1.0) && (beta[1] == 0.0)))) {
            return;
        }

        // Set up the start points in x and y
        if (incx > 0) {
            kx = 1;
        } else {
            kx = 1 - ( (n - 1) * incx);
        }

        if (incy > 0) {
            ky = 1;
        } else {
            ky = 1 - ( (n - 1) * incy);
        }

        // Start the operations. In this version the elements of A are accessed
        // sequentially with one pass through the triangular part of A.

        // First form y = beta*y.

        if ((beta[0] != 1.0) || (beta[1] != 0.0)) {

            if (incy == 1) {

                if ((beta[0] == 0.0) && (beta[1] == 0.0)) {

                    for (i = 0; i < n; i++) {
                        y[i][0] = 0.0;
                        y[i][1] = 0.0;
                    }
                } // if (beta == 0.0)
                else { // beta != 0.0

                    for (i = 0; i < n; i++) {
                    	zmlt(beta[0], beta[1], y[i][0], y[i][1], cr, ci);
                    	y[i][0] = cr[0];
                    	y[i][1] = ci[0];
                    }
                } // else beta != 0.0
            } // if (incy == 1)
            else { // incy != 1
                iy = ky - 1;

                if ((beta[0] == 0.0) && (beta[1] == 0.0)) {

                    for (i = 0; i < n; i++) {
                        y[iy][0] = 0.0;
                        y[iy][1] = 0.0;
                        iy = iy + incy;
                    }
                } // if ((beta[0] == 0.0) && (beta[1] == 0.0))
                else { // beta != 0.0

                    for (i = 0; i < n; i++) {
                    	zmlt(beta[0], beta[1], y[iy][0], y[iy][1], cr, ci);
                    	y[iy][0] = cr[0];
                    	y[iy][1] = ci[0];
                        iy = iy + incy;
                    }
                } // else beta != 0.0
            } // else incy != 1
        } // if ((beta[0] != 1.0) || (beta[1] != 0.0))

        if ((alpha[0] == 0.0) && (alpha[1] == 0.0)) {
            return;
        }

        if ( (uplo == 'U') || (uplo == 'u')) {

            // Form y when A is stored in upper triangle
            if ( (incx == 1) && (incy == 1)) {

                for (j = 0; j < n; j++) {
                	zmlt(alpha[0], alpha[1], x[j][0], x[j][1], cr, ci);
                	temp1[0] = cr[0];
                	temp1[1] = ci[0];
                    temp2[0] = 0.0;
                    temp2[1] = 0.0;

                    for (i = 0; i <= (j - 1); i++) {
                    	zmlt(temp1[0], temp1[1], A[i][j][0], A[i][j][1], cr, ci);
                    	y[i][0] = y[i][0] + cr[0];
                    	y[i][1] = y[i][1] + ci[0];
                        zmlt(A[i][j][0], A[i][j][1], x[i][0], x[i][1], cr, ci);
                        temp2[0] = temp2[0] + cr[0];
                        temp2[1] = temp2[1] + ci[0];
                    } // for (i = 0; i <= j-1; i++)

                    zmlt(temp1[0], temp1[1], A[j][j][0], A[j][j][1], cr, ci);
                    zmlt(alpha[0], alpha[1], temp2[0], temp2[1], cr2, ci2);
                    y[j][0] = y[j][0] + cr[0] + cr2[0];
                    y[j][1] = y[j][1] + ci[0] + ci2[0];
                } // for (j = 0; j < n; j++)
            } // if ((incx == 1) && (incy == 1))
            else { // ((incx != 1) || (incy != 1))
                jx = kx - 1;
                jy = ky - 1;

                for (j = 0; j < n; j++) {
                	zmlt(alpha[0], alpha[1], x[jx][0], x[jx][1], cr, ci);
                	temp1[0] = cr[0];
                	temp1[1] = ci[0];
                    temp2[0] = 0.0;
                    temp2[1] = 0.0;
                    ix = kx - 1;
                    iy = ky - 1;

                    for (i = 0; i <= (j - 1); i++) {
                    	zmlt(temp1[0], temp1[1], A[i][j][0], A[i][j][1], cr, ci);
                    	y[iy][0] = y[iy][0] + cr[0];
                    	y[iy][1] = y[iy][1] + ci[0];
                    	zmlt(A[i][j][0], A[i][j][1], x[ix][0], x[ix][1], cr, ci);
                        temp2[0] = temp2[0] + cr[0];
                        temp2[1] = temp2[1] + ci[0];
                        ix = ix + incx;
                        iy = iy + incy;
                    } // for (i = 0; i <= j-1; i++)

                    zmlt(temp1[0], temp1[1], A[j][j][0], A[j][j][1], cr, ci);
                    zmlt(alpha[0], alpha[1], temp2[0], temp2[1], cr2, ci2);
                    y[jy][0] = y[jy][0] + cr[0] + cr2[0];
                    y[jy][1] = y[jy][1] + ci[0] + ci2[0];
                    jx = jx + incx;
                    jy = jy + incy;
                } // for (j = 0; j < n; j++)
            } // else ((incx != 1) || (incy != 1))
        } // if ((uplo == 'U') || (uplo == 'u'))
        else { // ((uplo == 'L') || (uplo == 'l'))

            // Form y when A is stored in lower triangle
            if ( (incx == 1) && (incy == 1)) {

                for (j = 0; j < n; j++) {
                	zmlt(alpha[0], alpha[1], x[j][0], x[j][1], cr, ci);
                	temp1[0] = cr[0];
                	temp1[1] = ci[0];
                    temp2[0] = 0.0;
                    temp2[1] = 0.0;
                    zmlt(temp1[0], temp1[1], A[j][j][0], A[j][j][1], cr, ci);
                    y[j][0] = y[j][0] + cr[0];
                    y[j][1] = y[j][1] + ci[0];

                    for (i = j + 1; i < n; i++) {
                    	zmlt(temp1[0], temp1[1], A[i][j][0], A[i][j][1], cr, ci);
                    	y[i][0] = y[i][0] + cr[0];
                    	y[i][1] = y[i][1] + ci[0];
                        zmlt(A[i][j][0], A[i][j][1], x[i][0], x[i][1], cr, ci);
                        temp2[0] = temp2[0] + cr[0];
                        temp2[1] = temp2[1] + ci[0];
                    } // for (i = j+1; i < n; i++)

                    zmlt(alpha[0], alpha[1], temp2[0], temp2[1], cr, ci);
                    y[j][0] = y[j][0] + cr[0];
                    y[j][1] = y[j][1] + ci[0];
                } // for (j = 0; j < n; j++)
            } // if ((incx == 1) && (incy == 1))
            else { // ((incx != 1) || (incy != 1))
                jx = kx - 1;
                jy = ky - 1;

                for (j = 0; j < n; j++) {
                	zmlt(alpha[0], alpha[1], x[jx][0], x[jx][1], cr, ci);
                	temp1[0] = cr[0];
                	temp1[1] = ci[0];
                    temp2[0] = 0.0;
                    temp2[1] = 0.0;
                    zmlt(temp1[0], temp1[1], A[j][j][0], A[j][j][1], cr, ci);
                    y[jy][0] = y[jy][0] + cr[0];
                    y[jy][1] = y[jy][1] + ci[0];
                    ix = jx;
                    iy = jy;

                    for (i = j + 1; i < n; i++) {
                        ix = ix + incx;
                        iy = iy + incy;
                        zmlt(temp1[0], temp1[1], A[i][j][0], A[i][j][1], cr, ci);
                    	y[iy][0] = y[iy][0] + cr[0];
                    	y[iy][1] = y[iy][1] + ci[0];
                    	zmlt(A[i][j][0], A[i][j][1], x[ix][0], x[ix][1], cr, ci);
                        temp2[0] = temp2[0] + cr[0];
                        temp2[1] = temp2[1] + ci[0];
                    } // for (i = j+1; i < n; i++)

                    zmlt(alpha[0], alpha[1], temp2[0], temp2[1], cr, ci);
                    y[jy][0] = y[jy][0] + cr[0];
                    y[jy][1] = y[jy][1] + ci[0];
                    jx = jx + incx;
                    jy = jy + incy;
                } // for (j = 0; j < n; j++)
            } // else ((incx != 1) || (incy != 1))
        } // else ((uplo == 'L') || (uplo == 'l'))

        return;
    } // zsymv

    private double abs1(double ff[]) {
    	return Math.max(Math.abs(ff[0]), Math.abs(ff[1]));
    }
    
    private double abssq(double ff[]) {
    	return (ff[0]*ff[0] + ff[1]*ff[1]);
    }
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
        //int ncola;
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
            //ncola = k;
        } else {
            nrowa = k;
            //ncola = m;
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