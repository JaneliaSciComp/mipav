package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class LinearEquations implements java.io.Serializable {
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    private ViewUserInterface UI = ViewUserInterface.getReference();
    
    private int iparms[];

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    /**
     * Creates a new LinearEquations object.
     */
    public LinearEquations() {}
    
    /*
     * This is a port of LAPACK test routine DDRVPO.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * 
     * ddrvpo tests the driver routines dposv and dposvx.
     * 
     * @param input boolean[] dotype of dimension (ntypes)
           The matrix types to be used for testing.  Matrices of type j
           (for 0 <= j <= ntypes-1) are used for testing if dotype[j] = true;
           if dotype[j] = false, then type j is not used
       @param input int nn
           The number of values of n contained in the vector nval.
       @param input int[] nval of dimension (nn)
           The values of the matrix dimension n
       @param input int nrhs
           The number of right hand side vectors to be generated for
           each linear system.
       @param input double thresh
           The threshold value for the test ratios.  A result is
           included in the output file if result >= THRESH.  To have
           every test ratio printed, use thresh = 0.
       @param input int nmax
           The maximum value permitted for n, used in dimensioning the
           work arrays.
       @param output double[][] A of dimension (nmax, nmax)
       @param output double[][] AFAC of dimension (nmax, nmax)
       @param output double[][] ASAV of dimension (nmax, nmax)
       @param output double[][] B of dimension (nmax, nrhs)
       @param output double[][] BSAV of dimension (nmax, nrhs)
       @param output double[][] X of dimension (nmax, nrhs)
       @param output double[][] XACT of dimension (nmax, nrhs)
       @param output double[] s of dimension (nmax)
       @param output double[][] WORK of dimension (nmax, max(3,nrhs))
       @param output double[] rwork of dimension (nmax + 2*nrhs)
       @param output int[] iwork of dimension (nmax)
     */
     private void ddrvpo(boolean[] dotype, int nn, int[] nval, int nrhs, double thresh, int nmax,
                         double[][] A, double[][] AFAC, double[][] ASAV, double[][] B, double[][] BSAV,
                         double[][] X, double[][] XACT, double[] s, double[][] WORK, double[] rwork,
                         int[] iwork) {
         final int ntypes = 9;
         final int ntests = 6;
         
         boolean equil;
         boolean nofact;
         boolean prefac;
         boolean zerot;
         char dist[] = new char[1];
         char equed[] = new char[1];
         char fact;
         char type[] = new char[1];
         char uplo;
         char xtype;
         String path;
         int i;
         int iequed;
         int ifact;
         int imat;
         int in;
         int info[] = new int[1];
         int ioff;
         int iuplo;
         int izero;
         int k;
         int k1 = 0;
         int kL[] = new int[1];
         int ku[] = new int[1];
         int lda;
         int mode[] = new int[1];
         int n;
         int nb;
         int nbmin;
         int nerrs;
         int nfact;
         int nfail;
         int nimat;
         int nrun;
         int nt;
         double ainvnm;
         double amax[] = new double[1];
         double anorm[] = new double[1];
         double cndnum[] = new double[1];
         double rcond = 0.0;
         double rcondc = 0.0;
         double roldc = 0.0;
         double scond[] = new double[1];
         char[] equeds = new char[]{'N', 'Y'};
         char[] facts = new char[]{'F', 'N', 'E'};
         char[] uplos = new char[]{'U', 'L'};
         int[] iseed = new int[4];
         int[] iseedy = new int[]{1988, 1989, 1990, 1991};
         double[] result = new double[ntests];
         double[] res = new double[2];
         boolean lerr;
         boolean ok;
         String srnamt;
         int infot;
         double[] workspace;
         int itot;
         int irow;
         int icol;
         double vec[];
         int j;
         boolean dposvTest = true;
         
         // Initialize constants and the random number seed.
         
         path = new String("DPO"); // Double precision
         nrun = 0;
         nfail = 0;
         nerrs = 0;
         for (i = 0; i < 4; i++) {
             iseed[i] = iseedy[i];
         }
         
         // Set the block size and minimum block size for testing
         nb = 1;
         nbmin = 2;
         xlaenv(1, nb);
         xlaenv(2, nbmin);
         
         // Do for each value of n in nval
         for (in = 1; in <= nn; in++) {
             n = nval[in-1];
             lda = Math.max(1, n);
             xtype = 'N';
             nimat = ntypes;
             if (n <= 0) {
                 nimat = 1;
             }
             
             for (imat = 1; imat <= nimat; imat++) {
                 
                 // Do the tests only if dotype[imat] is true;
                 
                 if (!dotype[imat-1]) {
                     continue;
                 }
                 
                 // Skip types 3, 4, or 5 if the matrix size is too small.
                 
                 zerot = imat >= 3 && imat <= 5;
                 if (zerot && n < imat-2) {
                     continue;
                 }
                 
                 // Do first for UPLO = 'U', then for uplo = 'L'
                 
                 for (iuplo = 1; iuplo <= 2; iuplo++) {
                     uplo = uplos[iuplo-1];
                     
                  // Set up parameters with dlatb4 and generate a test matrix
                     // with dlatms.
                     
                     ge.dlatb4(path, imat, n, n, type, kL, ku, anorm, mode, cndnum, dist);
                    
                     srnamt = new String("DLATMS");
                     workspace = new double[3*n];
                     ge.dlatms(n, n, dist[0], iseed, type[0], rwork, mode[0], 
                               cndnum[0], anorm[0], kL[0], ku[0], uplo, A, lda, workspace, info);
                     
                     // Check error code from dlatms
                     if (info[0] != 0) {
                         // Print the header if this is the first error message
                         if (nfail == 0 && nerrs == 0) {
                             Preferences.debug("DPO, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                             // PO matrix types
                             Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("2. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("3. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("4. norm( X - XACT ) / ( norm(XACT) * (error bound) )\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("5. (backward error)  / eps\n", Preferences.DEBUG_ALGORITHM);
                             Preferences.debug("6. rcond * cndnum[0] - 1.0\n", Preferences.DEBUG_ALGORITHM);
                         } // if (nfail == 0 && nerrs == 0)
                         nerrs++;
                         
                         // Print the message detailing the error
                         Preferences.debug("Error code from dlatms info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                         Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                         Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                         Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                         continue;
                     } // if (info[0] != 0)
                     
                  // For types 3-5, zero one row and column of the matrix to
                     // test that info[0] is returned correctly
                     
                     if (zerot) {
                         if (imat == 3) {
                             izero = 1;
                         }
                         else if (imat == 4) {
                             izero = n;
                         }
                         else {
                             izero = n/2 + 1;
                         }
                         ioff = (izero - 1)*lda;
                         
                         // Set row and column izero of A to 0.
                         if (iuplo == 1) {
                             for (i = 1; i <= izero-1; i++) {
                                 itot = ioff + i;
                                 irow = itot % lda;
                                 icol = itot / lda;
                                 A[irow-1][icol-1] = 0.0;
                             }
                             ioff = ioff + izero;
                             for (i = izero; i <= n; i++) {
                                 irow = ioff % lda;
                                 icol = ioff / lda;
                                 A[irow-1][icol-1] = 0.0;
                                 ioff = ioff + lda;
                             }
                         } // if (iuplo == 1)
                         else {
                             ioff = izero;
                             for (i = 1; i <= izero-1; i++) {
                                 irow = ioff % lda;
                                 icol = ioff / lda;
                                 A[irow-1][icol-1] = 0.0;
                                 ioff = ioff + lda;
                             }
                             ioff = ioff - izero;
                             for (i = izero; i <= n; i++) {
                                 itot = ioff + i;
                                 irow = itot % lda;
                                 icol = itot / lda;
                                 A[irow-1][icol-1] = 0.0;
                             }
                         }
                     } // if (zerot)
                     else {
                         izero = 0;
                     }
                     
                     // Save a copy of the matrix A in ASAV.
                     
                     ge.dlacpy(uplo, n, n, A, lda, ASAV, lda);
                     
                     for (iequed = 1; iequed <= 2; iequed++) {
                         equed[0] = equeds[iequed-1];
                         if (iequed == 1) {
                             nfact = 3;
                         }
                         else {
                             nfact = 1;
                         }
                         
                         for (ifact = 1; ifact <= nfact; ifact++) {
                             fact = facts[ifact-1];
                             prefac = (fact == 'F');
                             nofact = (fact == 'N');
                             equil = (fact == 'E');
                             
                             if (zerot) {
                                 if (prefac) {
                                     continue;
                                 }
                                 rcondc = 0.0;
                             } // if (zerot)
                             else if (!(fact == 'N')) {
                                 
                                 // Compute the condition number for comparison with
                                 // the value returned by dposvx (fact == 'N' reuses
                                 // the condition number from the previous iteration 
                                 // with fact == 'F').
                                 
                                 ge.dlacpy(uplo, n, n, ASAV, lda, AFAC, lda);
                                 if (equil || iequed > 1) {
                                     
                                     // Compute row and column scale factors to
                                     // equilibrate the matrix A.
                                     
                                     dpoequ(n, AFAC, lda, s, scond, amax, info);
                                     if (info[0] == 0 && n > 0) {
                                         if (iequed > 1) {
                                             scond[0] = 0.0;
                                         }
                                         
                                         // Equilibrate the matrix
                                         dlaqsy(uplo, n, AFAC, lda, s, scond[0], amax[0], equed);
                                     } // if (info[0] == 0 && n > 0)
                                 } // if (equil || iequed > 1)
                                 
                                 // Save the condition number of the non-equilibrated system
                                 // for use in dget04.
                                 
                                 if (equil) {
                                     roldc = rcondc;
                                 }
                                 
                                 // Compute the 1-norm of A
                                 
                                 anorm[0] = ge.dlansy('1', uplo, n, AFAC, lda, rwork);
                                 
                                 // Factor the matrix A
                                 
                                 dpotrf(uplo, n, AFAC, lda, info);
                                 
                                 // Form the inverse of A.
                                 
                                 ge.dlacpy(uplo, n, n, AFAC, lda, A, lda);
                                 dpotri(uplo, n, A, lda, info);
                                 
                                 // Compute the 1-norm condition number of A.
                                 
                                 ainvnm = ge.dlansy('1', uplo, n, A, lda, rwork);
                                 if (anorm[0] <= 0.0 || ainvnm <= 0.0) {
                                     rcondc = 1.0;
                                 }
                                 else {
                                     rcondc = (1.0/anorm[0]) / ainvnm;
                                 }
                             } // else if (!(fact == 'N'))
                             
                             // Restore the matrix A
                             ge.dlacpy(uplo, n, n, ASAV, lda, A, lda);
                             
                             // Form an exact solution and set the right hand side.
                             
                             srnamt = new String("DLARHS");
                             vec = new double[n];
                             for (j = 0; j < nrhs; j++) {
                                 ge.dlarnv(2, iseed, n, vec);
                                 for (k = 0; k < n; k++) {
                                     XACT[k][j] = vec[k];
                                 }
                             } // for (j = 0; j < nrhs; j++)
                             // Symmetric matrix, 2-D storage
                             ge.dsymm('L', uplo, n, nrhs, 1.0, A, lda, XACT, lda, 0.0, B, lda);
                             xtype = 'C';
                             ge.dlacpy('F', n, nrhs, B, lda, BSAV, lda);
                             
                             if (nofact) {
                                 
                                 // Test dposv
                                 
                                 // Compute the L*L' or U'*U factorization of the matrix and solve
                                 // the system.
                                 
                                 ge.dlacpy(uplo, n, n, A, lda, AFAC, lda);
                                 ge.dlacpy('F', n, nrhs, B, lda, X, lda);
                                 
                                 srnamt = new String("DPOSV");
                                 dposv(uplo, n, nrhs, AFAC, lda, X, lda, info);
                                 
                                 // Check error code from dposv.
                                 
                                 if (info[0] != izero) {
                                  // Print the header if this is the first error message
                                     if (nfail == 0 && nerrs == 0) {
                                         Preferences.debug("DPO drivers, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                                         // PO matrix types
                                         Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("2. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("3. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("4. norm( X - XACT ) / ( norm(XACT) * (error bound) )\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("5. (backward error)  / eps\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("6. rcond * cndnum[0] - 1.0\n", Preferences.DEBUG_ALGORITHM);
                                     } // if (nfail == 0 && nerrs == 0)
                                     nerrs++;
                                     
                                     // Print the message detailing the error 
                                     if (info[0] != izero && izero != 0) {
                                         Preferences.debug("dposv returned with info[0] = " + info[0] + " instead of " + izero + "\n",
                                                           Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("nrhs = " + nrhs + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                     }
                                     else {
                                         Preferences.debug("Error code from dposv info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("nrhs = " + nrhs + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                     }
                                     dposvTest = false;
                                 } // if (info[0] != izero)
                                 else if (info[0] != 0) {
                                     dposvTest = false;
                                 }
                                 if (dposvTest) {
                                     // Reconstruct matrix from factors and compute residual.
                                     
                                     dpot01(uplo, n, A, lda, AFAC, lda, rwork, result);
                                     
                                     // Compute residual of the completed solution
                                     
                                     ge.dlacpy('F', n, nrhs, B, lda, WORK, lda);
                                     dpot02(uplo, n, nrhs, A, lda, X, lda,
                                            WORK, lda, rwork, res);
                                     result[1] = res[0];
                                     
                                     // Check solution from generated exact solution.
                                     
                                     dget04(n, nrhs, X, lda, XACT, lda, rcondc, res);
                                     result[2] = res[0];
                                     nt = 3;
                                     
                                     // Print information about the tests that did not pass the threshold
                                     for (k = 0; k < nt; k++) {
                                         if (result[k] >= thresh) {
                                             if (nfail == 0 && nerrs == 0) {
                                                 Preferences.debug("DPO drivers, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                                                 // PO matrix types
                                                 Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("2. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("3. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("4. norm( X - XACT ) / ( norm(XACT) * (error bound) )\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("5. (backward error)  / eps\n", Preferences.DEBUG_ALGORITHM);
                                                 Preferences.debug("6. rcond * cndnum[0] - 1.0\n", Preferences.DEBUG_ALGORITHM);    
                                             } // if (nfail == 0 && nerrs == 0)
                                             Preferences.debug("dposv uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                             Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                             Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                             Preferences.debug("Test " + (k+1) + " has ratio = " + result[k] + "\n", Preferences.DEBUG_ALGORITHM);
                                             nfail++;
                                         } // if (result[k] >= thresh)
                                     } // for (k = 0; k < nt; k++)
                                     nrun = nrun + nt;
                                 } // if (dposvTest)
                                 dposvTest = true;
                             } // if (nofact)
                             
                             // Test dposvx
                             
                             if (!prefac) {
                                 ge.dlaset(uplo, n, n, 0.0, 0.0, AFAC, lda);
                             }
                             ge.dlaset('F', n, nrhs, 0.0, 0.0, X, lda);
                             if (iequed > 1 && n > 0) {
                                 
                                 // Equilibrate the matrix if fact = 'F' and equed == 'Y'.
                                 
                                 dlaqsy(uplo, n, A, lda, s, scond[0], amax[0], equed);
                             } // if (iequed > 1 && n > 0)
                             
                             // Solve the system and compute the condition number and error bounds
                             // using dposvx.
                             
                             srnamt = new String("DPOSVX");
                             
                             // Check the error code from dposvx.
                             
                             if (info[0] != izero) {
                                 // Print the header if this is the first error message
                                 if (nfail == 0 && nerrs == 0) {
                                     Preferences.debug("DPO drivers, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                                     // PO matrix types
                                     Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("2. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("3. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("4. norm( X - XACT ) / ( norm(XACT) * (error bound) )\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("5. (backward error)  / eps\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("6. rcond * cndnum[0] - 1.0\n", Preferences.DEBUG_ALGORITHM);
                                 } // if (nfail == 0 && nerrs == 0)
                                 nerrs++;
                                 
                                 // Print the message detailing the error
                                 if (info[0] != izero && izero != 0) {
                                     Preferences.debug("dposvx returned with info[0] = " + info[0] + " instead of " + izero + "\n",
                                                       Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("fact = " + fact + "\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("nrhs = " + nrhs + "\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                 }
                                 else {
                                     Preferences.debug("Error code from dposvx info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("fact = " + fact + "\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("nrhs = " + nrhs + "\n", Preferences.DEBUG_ALGORITHM);
                                     Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                 }
                                 continue;
                             } // if (info[0] != izero)
                             
                             if (info[0] == 0) {
                                 if (!prefac) {
                                     
                                     // Reconstruct matrix from factors and compute residual.
                                     vec = new double[n];
                                     dpot01(uplo, n , A, lda, AFAC, lda, vec, result);
                                     for (j = 0; j < n; j++) {
                                         rwork[2*nrhs + j] = vec[j];
                                     }
                                     k1 = 1;
                                 } // if (!prefac)
                                 else {
                                     k1 = 2;
                                 }
                                 
                                 // Compute residual of the computed solution.
                                 
                                 ge.dlacpy('F', n, nrhs, BSAV, lda, WORK, lda);
                                 vec = new double[n];
                                 dpot02(uplo, n, nrhs, ASAV, lda, X, lda, WORK,
                                        lda, vec, res);
                                 for (j = 0; j < n; j++) {
                                     rwork[2*nrhs + j] = vec[j];
                                 }
                                 result[1] = res[0];
                                 
                                 // Check solution from generated exact solution.
                                 
                                 if (nofact || (prefac && (equed[0] == 'N'))) {
                                     dget04(n, nrhs, X, lda, XACT, lda, rcondc, res);
                                     result[2] = res[0];
                                 }
                                 else {
                                     dget04(n, nrhs, X, lda, XACT, lda, roldc, res);
                                     result[2] = res[0];    
                                 }
                                 
                                 // Check the error bounds from iterative refinement.
                                 
                                 vec = new double[nrhs];
                                 for (j = 0; j < nrhs; j++) {
                                     vec[j] = rwork[nrhs + j];
                                 }
                                 dpot05(uplo, n, nrhs, ASAV, lda, B, lda, X, lda, 
                                        XACT, lda, rwork, vec, res);
                                 result[3] = res[0];
                                 result[4] = res[1];
                             } // if (info[0] == 0)
                             else {
                                 k1 = 6;
                             }
                             
                             // Compare rcond from dposvx with the computed value in rcondc.
                             
                             result[5] = dget06(rcond, rcondc);
                             
                             // Print information about the tests that did not pass threshold
                             for (k = k1-1; k < 6; k++) {
                                 if (result[k] >= thresh) {
                                     if (nfail == 0 && nerrs == 0) {
                                         Preferences.debug("DPO drivers, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                                         // PO matrix types
                                         Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("2. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("3. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("4. norm( X - XACT ) / ( norm(XACT) * (error bound) )\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("5. (backward error)  / eps\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("6. rcond * cndnum[0] - 1.0\n", Preferences.DEBUG_ALGORITHM);    
                                     } // if (nfail == 0 && nerrs == 0)
                                     if (prefac) {
                                         Preferences.debug("dposvx fact = " + fact + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("equed[0] = " + equed[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("Test " + (k+1) + " has ratio = " + result[k] + "\n", Preferences.DEBUG_ALGORITHM);
                                     }
                                     else {
                                         Preferences.debug("dposvx fact = " + fact + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                         Preferences.debug("Test " + (k+1) + " has ratio = " + result[k] + "\n", Preferences.DEBUG_ALGORITHM);    
                                     }
                                     nfail++;
                                 } // if (result[k] >= thresh)
                             } // for (k = k1-1; k < 6; k++)
                             nrun = nrun + 7 - k1;
                         } // for (ifact = 1; ifact <= nfact; ifact++)
                     } // for (iequed = 1; iequed <= 2; iequed++)
                 } // for (iuplo = 1; iuplo <= 2; iuplo++)
             } // for (imat = 1; imat <= nimat; imat++)
         } // for (in = 1; in <= nn; in++)
         
         // Print a summary of results
         
      // Print a summary of results
         if (nfail > 0) {
             Preferences.debug("ddrvpo: " + nfail + "out of " + nrun + " driver tests failed with values >= threshold\n", Preferences.DEBUG_ALGORITHM);
             UI.setDataText("ddrvpo: " + nfail + "out of " + nrun + " driver tests failed with values >= threshold\n");
         }
         else {
             Preferences.debug("All " + nrun + " driver tests for ddrvpo passed\n", Preferences.DEBUG_ALGORITHM);
             UI.setDataText("All " + nrun + " driver tests for ddrvpo passed\n");
         }
         if (nerrs > 0) {
             Preferences.debug("ddrvpo: " + nerrs + " error messages recorded\n", Preferences.DEBUG_ALGORITHM);
             UI.setDataText("ddrvpo: " + nerrs + " error messages recorded\n");
         }
     } // ddrvpo
    
    /*
     * This is a port of a portion of LAPACK test routine DERRVX.f version 3.4.1
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., April, 2012
     * This routine checks the error exits of dposv.
     * 
     * derrvx correctly found 5 of 5 dposv error exits.
     */
     public void derrvx() {
         int nmax = 4;
         int info[] = new int[1];
         double A[][] = new double[nmax][nmax];
         double B[][] = new double[nmax][nmax];
         int npass = 5;
         final int ntotal = 5; 
         int i;
         int j;
         
         // Set the variables to innocuous values.
         for (j = 0; j < nmax; j++) {
             for (i = 0; i < nmax; i++) {
                 A[i][j] = 1.0/(double)(i+j);
                 B[i][j] = 1.0/(double)(i+j);
             }
         }
         
         // Tests the error exits of dposv
         dposv('/', 0, 0, A, 1, B, 1, info);
         if (info[0] != -1) {
             Preferences.debug("dposv('/', 0, 0, A, 1, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dposv('U', -1, 0, A, 1, B, 1, info);
         if (info[0] != -2) {
             Preferences.debug("dposv('U', -1, 0, A, 1, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dposv('U', 0, -1, A, 1, B, 1, info);
         if (info[0] != -3) {
             Preferences.debug("dposv('U', 0, -1. A, 1, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -3\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dposv('U', 2, 0, A, 1, B, 2, info);
         if (info[0] != -5) {
             Preferences.debug("dposv('U', 2, 0. A, 1, B, 2, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -5\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dposv('U', 2, 0, A, 2, B, 1, info);
         if (info[0] != -7) {
             Preferences.debug("dposv('U', 2, 0. A, 2, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -7\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         Preferences.debug("derrvx correctly found " + npass + " of " + ntotal + " dposv error exits\n", Preferences.DEBUG_ALGORITHM);
         UI.setDataText("derrvx correctly found " + npass + " of " + ntotal + " dpsov error exits\n");
         return;
     } // derrvx
     
     /*
      * This is a port of a portion of LAPACK auxiliary routine DLAQSY.f version 3.4.2
      * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
      * University of Colorado Denver, and NAG Ltd., September, 2012
      * 
      * dlaqsy scales a symmetric/Hermitian matrix, using scaling factors computed by dpoequ.
      * 
      * dlaqsy equilibrates a symmetric matrix A using the scaling factors in the vector s.
      * 
      * @param input char uplo
      *     Specifies whether the upper or lower triangular part of the
            symmetric matrix A is stored.
            = 'U':  Upper triangular
            = 'L':  Lower triangular
        @param input int n
            The order of the matrix A.  n >= 0.
        @param (input/output) double[][] A of dimension (lda, n).
            On entry, the symmetric matrix A.  If uplo = 'U', the leading
            n by n upper triangular part of A contains the upper
            triangular part of the matrix A, and the strictly lower
            triangular part of A is not referenced.  If uplo = 'L', the
            leading n by n lower triangular part of A contains the lower
            triangular part of the matrix A, and the strictly upper
            triangular part of A is not referenced.

            On exit, if equed[0] = 'Y', the equilibrated matrix:
            diag(s) * A * diag(s).
        @param input int lda
            The leading dimension of the array A.  lda >= max(n,1).
        @param input double[] s of dimension (n)
            The scale factors for A.
        @param input double scond
            Ratio of the smallest s[i] to the largest s[i].
        @param input double amax
            Absolute value of largest matrix entry.
        @param output char[] equed of dimension (1).
            Specifies whether or not equilibration was done.
            = 'N':  No equilibration.
            = 'Y':  Equilibration was done, i.e., A has been replaced by
                    diag(s) * A * diag(s).

      */
     private void dlaqsy(char uplo, int n, double[][] A, int lda, double[] s, double scond, double amax,
                         char[] equed) {
         //  thresh is a threshold value used to decide if scaling should be done
         //  based on the ratio of the scaling factors.  If scond < thresh,
         //  scaling is done.
         final double thresh = 0.1;
         //  large and small are threshold values used to decide if scaling should
         //  be done based on the absolute size of the largest matrix element.
         //  If amax > large or amax < small, scaling is done.
         double large;
         double small;
         int i;
         int j;
         double cj;
         
         // Quick return if possible
     
         if (n <= 0) {
             equed[0] = 'N';
             return;
         }
     
         // Initialize large and small.
     
         small = ge.dlamch('S') / ge.dlamch('P'); // Safe minimum / precision
         large = 1.0/small;
     
         if (scond >= thresh && amax >= small && amax <= large) {
     
             // No equilibration
     
             equed[0] = 'N';
         }
         else {
     
             // Replace A by diag(s) * A * diag(s).
     
             if ((uplo == 'U') || (uplo == 'u')) {
     
                 // Upper triangle of A is stored.
     
                 for (j = 1; j <= n; j++) {
                    cj = s[j-1];
                    for (i = 1; i <= j; i++) {
                       A[i-1][j-1] = cj*s[i-1]*A[i-1][j-1];
                    } // for (i = 1; i <= j; i++)
                 } // for (j = 1; j <= n; j++)
             }
             else {
     
                 // Lower triangle of A is stored.
     
                 for (j = 1; j <= n; j++) {
                    cj = s[j-1];
                    for (i = j; i <= n; i++) {
                       A[i-1][j-1] = cj*s[i-1]*A[i-1][j-1];
                    } // for (i = j; i <= n; i++)
                 } // for (j = 1; j <= n; j++)
             } // else
             equed[0] = 'Y';
         } // else
     
         return;

     } // dlaqsy
     
     /*
      * This is a port of a portion of LAPACK computational routine DPOEQU.f version 3.4.0
      * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
      * University of Colorado Denver, and NAG Ltd., November, 2011
      * 
      * dpoequ computes row and column scalings intended to equilibrate a
        symmetric positive definite matrix A and reduce its condition number
        (with respect to the two-norm).  s contains the scale factors,
        s[i] = 1/sqrt(A[i][i]), chosen so that the scaled matrix B with
        elements B[i][j] = s[i]*A[i][j]*s[j] has ones on the diagonal.  This
        choice of s puts the condition number of B within a factor n of the
        smallest possible condition number over all possible diagonal
        scalings.
        
        @param input int n The order of the matrix A.  n >= 0.
        @param input double[][] A of dimension (lda, n)
            The n-by-n symmetric positive definite matrix whose scaling
            factors are to be computed.  Only the diagonal elements of A
            are referenced.
        @param input int lda  
            The leading dimension of the array A.  lda >= max(1,n).
        @param output double[] s of dimension (n)
            If info[0] = 0, s contains the scale factors for A.
        @param output double[] scond of dimension (1).
            If info[0] = 0, scond[0] contains the ratio of the smallest s[i] to
            the largest s[i].  If scond[0] >= 0.1 and amax[0] is neither too
            large nor too small, it is not worth scaling by scond[0].
        @param output double[] amax of dimension (1).
            Absolute value of largest matrix element.  If amax[0] is very
            close to overflow or very close to underflow, the matrix
            should be scaled.
        @param output int[] of dimension (1).
            = 0:  successful exit
            < 0:  if info[0] = -i, the i-th argument had an illegal value
            > 0:  if info[0] = i, the i-th diagonal element is nonpositive.
      */
     private void dpoequ(int n, double[][] A, int lda, double[] s, double[] scond, double[] amax, int info[]) {
         int i;
         double smin;
         
         // Test the input parameters
         
         info[0] = 0;
         if (n < 0) {
            info[0] = -1;
         }
         else if (lda < Math.max(1, n)) {
            info[0] = -3;
         }
         if(info[0] != 0) {
            MipavUtil.displayError("dpoequ had info[0] = " + info[0]);
            return;
         }
    
         // Quick return if possible
    
         if (n == 0) {
            scond[0] = 1.0;
            amax[0] = 0.0;
            return;
         }
   
         // Find the minimum and maximum diagonal elements.
   
         s[0] = A[0][0];
         smin = s[0];
         amax[0] = s[0];
         for (i = 1; i < n; i++) {
            s[i] = A[i][i];
            smin = Math.min(smin, s[i]);
            amax[0] = Math.max(amax[0], s[i]);
         } // for (i = 1; i < n; i++)
   
         if (smin <= 0.0) {
   
            // Find the first non-positive diagonal element and return.
   
            for (i = 1; i <= n; i++) {
               if (s[i-1] <= 0.0) {
                  info[0] = i;
                  return;
               } // if (s[i] <= 0.0)
            } // for (i = 1; i <= n; i++)
         }
         else {
   
            // Set the scale factors to the reciprocals
            // of the diagonal elements.
    
            for (i = 0; i < n; i++) {
               s[i] = 1.0 / Math.sqrt(s[i]);
            }
   
            // Compute scond[0] = min(s[i]) / max(s[i])
   
            scond[0] = Math.sqrt(smin) / Math.sqrt(amax[0]);
         } // else
         return;

     } // dpoequ
    
    /*
     * This is a port of a portion of LAPACK test routine DCHKPO.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * This routine tests dpotrf, dpotri, and dpotrs.
     * 
     * @param input boolean[] dotype of dimension (ntypes)
           The matrix types to be used for testing.  Matrices of type j
           (for 0 <= j <= ntypes-1) are used for testing if dotype[j] = true;
           if dotype[j] = false, then type j is not used
       @param input int nn
           The number of values of n contained in the vector nval.
       @param input int[] nval of dimension (nn)
           The values of the matrix dimension n.
       @param input int nnb
           The number of values of nb contained in the vector nbval.
       @param input int[] nbval of dimension (nnb)
           The values of the blocksize nb.
       @param input int nns
           The number of values of nrhs contained in the vector nsval.
       @param input int[] nsval of dimension (nns)
           The values of the number of right hand sides nrhs.
       @param input double thresh
           The threshold value for the test ratios.  A result is
           included in the output file if result[0] >= thresh.  To have
           every test ratio printed, use thresh = 0.
       @param input int nmax
           The maximum value permitted for n, used in dimensioning the
           work arrays.
       @param output double[][] A of dimension (nmax, nmax) 
       @param output double[][] AFAC of dimension (nmax, nmax)
       @param output double[][] AINV of dimension (nmax, nmax)
       @param output double[][] B of dimension (nmax, nsmax) where nsmax is the largest entry in nsval.
       @param output double[][] X of dimension (nmax, nsmax)
       @param output double[][] XACT of dimension (nmax, nsmax)
       @param output double[][] WORK of dimension (nmax, max(3, nsmax))
       @param output double[] rwork of dimension (max(nmax, 2*nsmax))
       @param output int[] iwork of dimension (nmax)
     */
    private void dchkpo(boolean[] dotype, int nn, int[] nval, int nnb, int[] nbval, int nns, int[] nsval,
                        double thresh, int nmax, double[][] A, double[][] AFAC, double[][] AINV,
                        double[][] B, double[][] X, double[][] XACT, double[][] WORK, double[] rwork,
                        int[] iwork) {
        final int ntypes = 9;
        final int ntests = 4;
        boolean zerot;
        char dist[] = new char[1];
        char type[] = new char[1];
        char uplo;
        String path;
        int i;
        int imat;
        int in;
        int inb;
        int info[] = new int[1];
        int ioff;
        int irhs;
        int iuplo;
        int izero;
        int k;
        int kl[] = new int[1];
        int ku[] = new int[1];
        int lda;
        int mode[] = new int[1];
        int n;
        int nb;
        int nerrs;
        int nfail;
        int nimat;
        int nrhs;
        int nrun;
        double anorm[] = new double[1];
        double cndnum[] = new double[1];
        double rcondc[] = new double[1];
        char uplos[] = new char[]{'U','L'};
        int iseed[] = new int[4];
        int iseedy[] = new int[]{1988, 1989, 1990, 1991};
        double result[] = new double[ntests];
        double workspace[];
        int itot;
        int irow;
        int icol;
        double res[] = new double[1];
        int j;
        double vec[];
        
        // Initialize constants and the random number seed.
        
        path = new String("DPO"); // Double precision
        nrun = 0;
        nfail = 0;
        nerrs = 0;
        for (i = 0; i < 4; i++) {
            iseed[i] = iseedy[i];
        }
        
        xlaenv(2, 2);
        
        // Do for each value of n in nval
        for (in = 1; in <= nn; in++) {
            n = nval[in-1];
            lda = Math.max(n, 1);
            nimat = ntypes;
            if (n <= 0) {
                nimat = 1;
            }
            
            izero = 0;
            for (imat = 1; imat <= nimat; imat++) {
            
                // Do the tests only if dotype[imat-1] is true.
                
                if (!dotype[imat-1]) {
                    continue;
                }
                
                // Skip types 3, 4, or 5 if the matrix size is too small.
                
                zerot = imat >= 3 && imat <= 5;
                if (zerot && n < imat-2) {
                    continue;
                }
                
                // Do first for UPLO = 'U', then for uplo = 'L'
                
                for (iuplo = 1; iuplo <= 2; iuplo++) {
                    uplo = uplos[iuplo-1];
                    
                    // Set up parameters with dlatb4 and generate a test matrix
                    // with dlatms.
                    
                    ge.dlatb4(path, imat, n, n, type, kl, ku, anorm, mode, cndnum, dist);
                   
                    workspace = new double[3*n];
                    ge.dlatms(n, n, dist[0], iseed, type[0], rwork, mode[0], 
                              cndnum[0], anorm[0], kl[0], ku[0], uplo, A, lda, workspace, info);
                    
                    // Check error code from dlatms
                    if (info[0] != 0) {
                        // Print the header if this is the first error message
                        if (nfail == 0 && nerrs == 0) {
                            Preferences.debug("DPO, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                            // Po matrix types
                            Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("2. norm(I - A*AINV) / (n * norm(A)  * norm(AINV) * eps)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("3. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("4. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
                        } // if (nfail == 0 && nerrs == 0)
                        nerrs++;
                        
                        // Print the message detailing the error
                        Preferences.debug("Error code from dlatms info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                        continue;
                    } // if (info[0] != 0)
                    
                    // For types 3-5, zero one row and column of the matrix to
                    // test that info[0] is returned correctly
                    
                    if (zerot) {
                        if (imat == 3) {
                            izero = 1;
                        }
                        else if (imat == 4) {
                            izero = n;
                        }
                        else {
                            izero = n/2 + 1;
                        }
                        ioff = (izero - 1)*lda;
                        
                        // Set row and column izero of A to 0.
                        if (iuplo == 1) {
                            for (i = 1; i <= izero-1; i++) {
                                itot = ioff + i;
                                irow = itot % lda;
                                icol = itot / lda;
                                A[irow-1][icol-1] = 0.0;
                            }
                            ioff = ioff + izero;
                            for (i = izero; i <= n; i++) {
                                irow = ioff % lda;
                                icol = ioff / lda;
                                A[irow-1][icol-1] = 0.0;
                                ioff = ioff + lda;
                            }
                        } // if (iuplo == 1)
                        else {
                            ioff = izero;
                            for (i = 1; i <= izero-1; i++) {
                                irow = ioff % lda;
                                icol = ioff / lda;
                                A[irow-1][icol-1] = 0.0;
                                ioff = ioff + lda;
                            }
                            ioff = ioff - izero;
                            for (i = izero; i <= n; i++) {
                                itot = ioff + i;
                                irow = itot % lda;
                                icol = itot / lda;
                                A[irow-1][icol-1] = 0.0;
                            }
                        }
                    } // if (zerot)
                    else {
                        izero = 0;
                    }
                    
                    // Do for each value of nb in nbval
                    for (inb = 1; inb <= nnb; inb++) {
                        nb = nbval[inb-1];
                        xlaenv(1, nb);
                        
                        // Compute the L*L' or U'*U factorization fo the matrix
                        ge.dlacpy(uplo, n, n, A, lda, AFAC, lda);
                        dpotrf(uplo, n, AFAC, lda, info);
                        
                        // Check error code from dpotrf.
                        
                        if (info[0] != izero) {
                         // Print the header if this is the first error message
                            if (nfail == 0 && nerrs == 0) {
                                Preferences.debug("DPO, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                                // Po matrix types
                                Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("2. norm(I - A*AINV) / (n * norm(A)  * norm(AINV) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("3. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("4. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
                            } // if (nfail == 0 && nerrs == 0)
                            nerrs++;
                            
                            // Print the message detailing the error
                            if (info[0] != izero && izero != 0) {
                                Preferences.debug("dpotrf returned with info[0] = " + info[0] + " instead of " + izero,
                                                  Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nb = " + nb + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                            } // if (info[0] != izero && izero != 0) 
                            else {
                                Preferences.debug("Error code from dpotrf info[0] = " + info[0], Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nb = " + nb + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                            } // else
                            if (info[0] != 0) {
                                Preferences.debug("Doing only the condition estimate for this case\n", Preferences.DEBUG_ALGORITHM);
                            }
                            continue;
                        } // if (info[0] != izero)
                        
                        // Skip tests if info[0] is not 0.
                        if (info[0] != 0) {
                            continue;
                        }
                        
                        // Test 1
                        // Reconstruct matrix from factors and compute residual.
                        
                        ge.dlacpy(uplo, n, n, AFAC, lda, AINV, lda);
                        dpot01(uplo, n, A, lda, AINV, lda, rwork, result);
                        
                        // Test 2
                        // Form the inverse and compute the residual
                        
                        ge.dlacpy(uplo, n, n, AFAC, lda, AINV, lda);
                        dpotri(uplo, n, AINV, lda, info);
                        
                        // Check error code from dpotri.
                        
                        if (info[0] != 0) {
                            // Print the header if this is the first error message
                            if (nfail == 0 && nerrs == 0) {
                                Preferences.debug("DPO, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                                // Po matrix types
                                Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("2. norm(I - A*AINV) / (n * norm(A)  * norm(AINV) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("3. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("4. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
                            } // if (nfail == 0 && nerrs == 0)
                            nerrs++;
                            
                            // Print the message detailing the error
                            Preferences.debug("Error code from dpotri info[0] = " + info[0], Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("nb = " + nb + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                        } // if (info[0] != 0)
                        
                        dpot03(uplo, n, A, lda, AINV, lda, WORK, lda, rwork, rcondc, res);
                        result[1] = res[0];
                        
                        // Print information about tests that did not pass the threshold.
                        for (k = 0; k <= 1; k++) {
                            if (result[k] >= thresh) {
                                if (nfail == 0 && nerrs == 0) {
                                    Preferences.debug("DPO, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                                    // Po matrix types
                                    Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("2. norm(I - A*AINV) / (n * norm(A)  * norm(AINV) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("3. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("4. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);    
                                } // if (nfail == 0 && nerrs == 0)
                                Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nb = " + nb + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("Test = " + (k+1) + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("Ratio = " + result[k] + "\n", Preferences.DEBUG_ALGORITHM);
                                nfail++;
                            } // if (result[k] >= thresh)
                        } // for (k = 0; k <= 1; k++)
                        nrun = nrun + 2;
                        
                        // Skip the rest of the tests unless this is the first blocksize
                        if (inb != 1) {
                            continue;
                        }
                        
                        for (irhs = 1; irhs <= nns; irhs++) {
                            nrhs = nsval[irhs-1];
                            
                            // Test 3
                            // Solve and compute the residual for A * X = B
                            
                            vec = new double[n];
                            for (j = 0; j < nrhs; j++) {
                                ge.dlarnv(2, iseed, n, vec);
                                for (k = 0; k < n; k++) {
                                    XACT[k][j] = vec[k];
                                }
                            } // for (j = 0; j < nrhs; j++)
                            // Symmetric matrix, 2-D storage
                            ge.dsymm('L', uplo, n, nrhs, 1.0, A, lda, XACT, lda, 0.0, B, lda);
                            ge.dlacpy('F', n, nrhs, B, lda, X, lda);
                            
                            dpotrs(uplo, n, nrhs, AFAC, lda, X, lda, info);
                            
                            // Check error code from dpotrs.
                            
                            if (info[0] != 0) {
                                // Print the header if this is the first error message
                                if (nfail == 0 && nerrs == 0) {
                                    Preferences.debug("DPO, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                                    // Po matrix types
                                    Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("2. norm(I - A*AINV) / (n * norm(A)  * norm(AINV) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("3. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("4. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);
                                } // if (nfail == 0 && nerrs == 0)
                                nerrs++;
                                
                                // Print the message detailing the error  
                                Preferences.debug("Error code from dpotrs info[0] = " + info[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nrhs = " + nrhs + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                            } // if (info[0] != 0)
                            
                            ge.dlacpy('F', n, nrhs, B, lda, WORK, lda);
                            dpot02(uplo, n, nrhs, A, lda, X, lda, WORK, lda, rwork, res);
                            result[2] = res[0];
                            
                            // Test 4
                            // Check solution generated from exact solution
                            dget04(n, nrhs, X, lda, XACT, lda, rcondc[0], res);
                            result[3] = res[0];
                            
                         // Print information about tests that did not pass the threshold.
                            for (k = 2; k <= 3; k++) {
                                if (result[k] >= thresh) {
                                    if (nfail == 0 && nerrs == 0) {
                                        Preferences.debug("DPO, Symmetric positive definite matrices\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("Matrix types:\n", Preferences.DEBUG_ALGORITHM);
                                        // Po matrix types
                                        Preferences.debug("1. Diagonal\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("2. Random, cndnum[0] = 2\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("3. First row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("4. Last row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("5. Middle row and column zero\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("6. Random, cndnum[0] = sqrt(0.1/eps)\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("7. Random, cndnum[0] = 0.1/eps\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("8. Scaled near underflow\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("9. Scaled near overflow\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("Test ratios:\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("1. norm(UT * U - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("or norm(L * LT - A) / (n * norm(A) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("2. norm(I - A*AINV) / (n * norm(A)  * norm(AINV) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("3. norm(B - A *X) / (norm(A) * norm(X) * eps)\n", Preferences.DEBUG_ALGORITHM);
                                        Preferences.debug("4. norm(X - XACT) / (norm(XACT) * cndnum[0] * eps)\n", Preferences.DEBUG_ALGORITHM);    
                                    } // if (nfail == 0 && nerrs == 0)
                                    Preferences.debug("uplo = " + uplo + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("nrhs = " + nrhs + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("imat = " + imat + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("Test = " + (k+1) + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("Ratio = " + result[k] + "\n", Preferences.DEBUG_ALGORITHM);
                                    nfail++;
                                } // if (result[k] >= thresh)
                            } // for (k = 2; k <= 3; k++)
                            nrun = nrun + 2;
                        } // for (irhs = 1; irhs <= nns; irhs++)    
                    } // for (inb = 1; inb <= nnb; inb++)
                } // for (iuplo = 1; iuplo <= 2; iuplo++)
            } // for (imat = 1; imat <= nimat; imat++)
        } // for (in = 1; in <= nn; in++)
        
        // Print a summary of results
        if (nfail > 0) {
            Preferences.debug("dchkpo: " + nfail + "out of " + nrun + " tests failed with values >= threshold\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("dchkpo: " + nfail + "out of " + nrun + " tests failed with values >= threshold\n");
        }
        else {
            Preferences.debug("All " + nrun + " tests for dchkpo passed\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("All " + nrun + " tests for dchkpo passed\n");
        }
        if (nerrs > 0) {
            Preferences.debug("dchkpo: " + nerrs + " error messages recorded\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("dchkpo: " + nerrs + " error messages recorded\n");
        }
        
        return;
    } // dchkpo
    
    /*
     * This is a port of a portion of LAPACK test routine DERRPO.f version 3.4.0
     * LAPACK is a software package provided by University of Tennessee, University of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011
     * This routine checks the error exits of dpotrf, dpotf2, dpotri, and dpotrs.
     * 
     * derrpo correctly found 14 of 14 error exits.
     */
     public void derrpo() {
         int nmax = 4;
         int info[] = new int[1];
         double A[][] = new double[nmax][nmax];
         double B[][] = new double[nmax][nmax];
         int npass = 14;
         final int ntotal = 14; 
         int i;
         int j;
         
         // Set the variables to innocuous values.
         for (j = 0; j < nmax; j++) {
             for (i = 0; i < nmax; i++) {
                 A[i][j] = 1.0/(double)(i+j);
                 B[i][j] = 1.0/(double)(i+j);
             }
         }
         
         // Test error exits of routines that use the Cholesky 
         // decomposition of a symmetric positive definite matrix.
         
         // dpotrf
         dpotrf('/', 0, A, 1, info);
         if (info[0] != -1) {
             Preferences.debug("dpotrf('/', 0, A, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotrf('U', -1, A, 1, info);
         if (info[0] != -2) {
             Preferences.debug("dpotrf('U', -1, A, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotrf('U', 2, A, 1, info);
         if (info[0] != -4) {
             Preferences.debug("dpotrf('U', 2, A, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -4\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         // dpotf2
         dpotf2('/', 0, A, 1, info);
         if (info[0] != -1) {
             Preferences.debug("dpotf2('/', 0, A, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotf2('U', -1, A, 1, info);
         if (info[0] != -2) {
             Preferences.debug("dpotf2('U', -1, A, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotf2('U', 2, A, 1, info);
         if (info[0] != -4) {
             Preferences.debug("dpotf2('U', 2, A, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -4\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         // dpotri
         dpotri('/', 0, A, 1, info);
         if (info[0] != -1) {
             Preferences.debug("dpotri('/', 0, A, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotri('U', -1, A, 1, info);
         if (info[0] != -2) {
             Preferences.debug("dpotri('U', -1, A, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotri('U', 2, A, 1, info);
         if (info[0] != -4) {
             Preferences.debug("dpotri('U', 2, A, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -4\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         // dpotrs
         dpotrs('/', 0, 0, A, 1, B, 1, info);
         if (info[0] != -1) {
             Preferences.debug("dpotrs('/', 0, 0, A, 1, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -1\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotrs('U', -1, 0, A, 1, B, 1, info);
         if (info[0] != -2) {
             Preferences.debug("dpotrs('U', -1, 0, A, 1, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -2\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotrs('U', 0, -1, A, 1, B, 1, info);
         if (info[0] != -3) {
             Preferences.debug("dpotrs('U', 0, -1, A, 1, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -3\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotrs('U', 2, 1, A, 1, B, 2, info);
         if (info[0] != -5) {
             Preferences.debug("dpotrs('U', 2, 1, A, 1, B, 2, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -5\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         dpotrs('U', 2, 1, A, 2, B, 1, info);
         if (info[0] != -7) {
             Preferences.debug("dpotrs('U', 2, 1, A, 2, B, 1, info) produced info[0] = " + info[0] +
                               " instead of info[0] = -7\n", Preferences.DEBUG_ALGORITHM);
             npass--;
         }
         
         Preferences.debug("derrpo correctly found " + npass + " of " + ntotal + " error exits\n", Preferences.DEBUG_ALGORITHM);
         UI.setDataText("derrpo correctly found " + npass + " of " + ntotal + " error exits\n");
         return;
     } // derrpo
    
    /**
     * This is a port of LAPACK version test routine 3.4.0 DGET04.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dget04 computes the difference between a computed solution and the
       true solution to a system of linear equations.

       resid[0] =  ( norm(X-XACT) * rcond) / ( norm(XACT) * eps),
       where rcond is the reciprocal of the condition number and eps is the
       machine epsilon.
       
       @param input int n  The number of rows of the matrices X and XACT.  n >= 0.
       @param input int nrhs  The number of columns of the matrices X and XACT.  nrhs >= 0.
       @param input double[][] X of dimension (ldx, nrhs)
           The computed solution vectors.  Each vector is stored as a column of the matrix X.
       @param input int ldx  The leading dimension of the array X.  ldx >= max(1, n).
       @param input double[][] XACT of dimension (ldx, nrhs)
           The exact solution vectors.  Each vector is stored as a column of the matrix XACT.
       @param input int ldxact  The leading dimension of the array XACT.  ldxact >= max(1, n).
       @param input double rcond  
           The reciprocal of the condition number of the coefficient
           matrix in the system of equations.
       @param output double[] of dimension 1
           The maximum over the NRHS solution vectors of
           ( norm(X-XACT) * rcond) / ( norm(XACT) * eps)
     */
    private void dget04(int n, int nrhs, double[][] X, int ldx, double[][] XACT, int ldxact,
                        double rcond, double[] resid) {
        int i;
        int ix;
        int j;
        double diffnm;
        double eps;
        double xnorm;
        int k;
        double maxVal;
        
        // Quick exit if n = 0 or nrhs = 0.
    
        if (n <= 0 || nrhs <= 0) {
            resid[0] = 0.0;
            return;
        }
    
        // Exit with resid[0] = 1/eps if rcond is invalid.
    
        eps = ge.dlamch('E'); // Epsilon
        if (rcond < 0.0) {
             resid[0] = 1.0 / eps;
             return;
        }
    
        // Compute the maximum of
        // norm(X - XACT) / ( norm(XACT) * eps)
        // over all the vectors X and XACT .
    
          resid[0] = 0.0;
          for (j = 0; j < nrhs; j++) {
             ix = 0;
             maxVal = Math.abs(XACT[0][j]);
             for (k = 1; k < n; k++) {
                 if (Math.abs(XACT[k][j]) > maxVal) {
                     maxVal = Math.abs(XACT[k][j]);
                     ix = k;
                 }
             }
             xnorm = Math.abs(XACT[ix][j]);
             diffnm = 0.0;
             for (i = 0; i < n; i++) {
                 diffnm = Math.max(diffnm, Math.abs( X[i][j]-XACT[i][j] ) );
             }
             if (xnorm <= 0.0) {
                if (diffnm > 0.0) {
                   resid[0] = 1.0 / eps;
                }
             }
             else {
                resid[0] = Math.max(resid[0], ( diffnm / xnorm )*rcond );
             }
          } // for (j = 0; j < nrhs; j++)
          if (resid[0]*eps < 1.0) {
             resid[0] = resid[0] / eps;
          }
    
          return;

    } // dget04
    
    /**
     * This is a port of LAPACK version test routine 3.4.0 DGET06.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dget06 computes a test ratio to compare two values for rcond.
     * 
     * @param input double rcond
     *     The estimate of the reciprocal of the condition number of A,
           as computed by dgecon.
       @param input double rcondc
           The reciprocal of the condition number of A, computed as
           ( 1/norm(A) ) / norm(inv(A)).
     */
    private double dget06(double rcond, double rcondc) {
        double eps;
        double rat;
        
        eps = ge.dlamch('E'); // Epsilon
        if (rcond > 0.0) {
            if (rcondc > 0.0) {
                rat = Math.max(rcond, rcondc) / Math.min(rcond, rcondc) - (1.0-eps);
            }
            else {
                rat = rcond / eps;
            }
        }
        else {
            if (rcondc > 0.0) {
                rat = rcondc / eps;
            }
            else {
                rat = 0.0;
            }
        }
            
        return rat;

    } // dget06
    
    /**
     * This is a port of LAPACK version test routine 3.4.0 DPOT01.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dpot01 reconstructs a symmetric positive definite matrix  A  from
       its L*L' or U'*U factorization and computes the residual
       norm( L*L' - A ) / ( N * norm(A) * eps) or
       norm( U'*U - A ) / ( N * norm(A) * eps),
       where eps is the machine epsilon.
       @param input char uplo
           Specifies whether the upper or lower triangular part of the
           symmetric matrix A is stored:
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input int n  The number of rows and columns of the matrix A.  n >= 0.
       @param input double[][] A of dimension (lda, n).  The original symmetric matrix A.
       @param input int lda  The leading dimension of the array A.  lda >= max(1, n).
       @param (input/output) double[][] afac of dimension (ldafac, n).
           On entry, the factor L or U from the L*L' or U'*U
           factorization of A.
           Overwritten with the reconstructed matrix, and then with the
           difference L*L' - A (or U'*U - A).
       @param input int ldafac  The leading dimension of the array afac.  ldafac >= max(1, n).
       @param output double[] rwork of dimension n
       @param output double[] resid of dimension 1
           If uplo = 'L', norm(L*L' - A) / (n * norm(A) * eps)
           If uplo = 'U', norm(U'*U - A) / (n * norm(A) * eps)
     */
     private void dpot01(char uplo, int n, double[][] A, int lda, double[][] afac, int ldafac, double rwork[], double resid[]) {
         int i;
         int j;
         int k;
         double anorm;
         double eps;
         double t;
         double vec[];
         double arr[][];
         
         // Quick exit if n = 0.
     
         if (n <= 0) {
             resid[0] = 0.0;
             return;
         }
     
         // Exit with resid[0] = 1/eps if anorm = 0.
     
         eps = ge.dlamch('E'); // Epsilon
         anorm = ge.dlansy('1', uplo, n, A, lda, rwork);
         if (anorm <= 0.0) {
             resid[0] = 1.0 / eps;
              return;
         }
     
         if ((uplo == 'U') || (uplo == 'u')) {
          // Compute the product U'*U, overwriting U.
             for (k = n; k >= 1; k--) {
     
                 // Compute the [k-1][k-1] element of the result.
                 vec = new double[k];
                 for (i = 0; i < k; i++) {
                     vec[i] = afac[i][k-1];
                 }
                 t = ge.ddot(k, vec, 1, vec, 1);
                 afac[k-1][k-1] = t;
      
                 // Compute the rest of column k-1.
                 for (i = 0; i < k-1; i++) {
                     vec[i] = afac[i][k-1];
                 }
                 ge.dtrmv('U', 'T', 'N', k-1, afac,
                          ldafac, vec, 1 );
                 for (i = 0; i < k-1; i++) {
                     afac[i][k-1] = vec[i];
                 }
             } // for (k = n; k >= 1; k--)
         } // if ((uplo == 'U') || (uplo == 'u'))
         else { 
             //  Compute the product L*L', overwriting L.
             for (k = n; k >= 1; k--) {
     
                 // Add a multiple of column k-1 of the factor L to each of
                 // columns k through n-1.
      
                 if (k+1 <= n) {
                     vec = new double[n-k];
                     for (i = 0; i < n-k; i++) {
                         vec[i] = afac[k+i][k-1];
                     }
                     arr = new double[n-k][n-k];
                     for (i = 0; i < n-k; i++) {
                         for (j = 0; j < n-k; j++) {
                             arr[i][j] = afac[k+i][k+j];
                         }
                     }
                     ge.dsyr('L', n-k, 1.0, vec, 1,
                             arr, ldafac);
                     for (i = 0; i < n-k; i++) {
                         for (j = 0; j < n-k; j++) {
                             afac[k+i][k+j] = arr[i][j];
                         }
                     }
                 } // if (k+1 <= n)
      
                 // Scale column k-1 by the diagonal element.
      
                 t = afac[k-1][k-1];
                 vec = new double[n-k+1];
                 for (i = 0; i < n-k+1; i++) {
                     vec[i] = afac[k-1+i][k-1];
                 }
                 ge.dscal(n-k+1, t, vec, 1);
                 for (i = 0; i < n-k+1; i++) {
                     afac[k-1+i][k-1] = vec[i];
                 }
             } // for (k = n; k >= 1; k--)
         } // else 
         
         // Compute the difference  L*L' - A (or U'*U - A).
      
         if ((uplo == 'U') || (uplo == 'u')) {
             for (j = 1; j <= n; j++) {
                 for (i = 1; i <= j; i++) {
                     afac[i-1][j-1] = afac[i-1][j-1] - A[i-1][j-1];
                 } 
             } // for (j = 1; j <= n; j++);
         } // if ((uplo == 'U') || (uplo == 'u'))
         else {
             for (j = 1; j <= n; j++) {
                 for (i = j; i <= n; i++) {
                     afac[i-1][j-1] = afac[i-1][j-1] - A[i-1][j-1];   
                 } // for (i = j; i <= n; i++)
             } // for (j = 1; j <= n; j++)
         } // else
     
         // Compute norm( L*U - A ) / (n * norm(A) * eps)
     
         resid[0] = ge.dlansy('1', uplo, n, afac, ldafac, rwork);
     
         resid[0] = ((resid[0] / (double)( n ) ) / anorm ) / eps;
     
         return;

     } // dpot01
     
     /**
      * This is a port of LAPACK version test routine 3.4.0 DPOT02.F created by the University of Tennessee, University
      * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
      *
      * dpot02 computes the residual for the solution of a symmetric system
        of linear equations  A*x = b:

        resid[0] = norm(B - A*X) / ( norm(A) * norm(X) * eps),
        where eps is the machine epsilon.

        @param input char uplo
            Specifies whether the upper or lower triangular part of the
            symmetric matrix A is stored:
            = 'U':  Upper triangular
            = 'L':  Lower triangular
        @param input int n  The number of rows and columns of the matrix A.  n >= 0.
        @param input int nrhs  The number of columns of B, the matrix of right hand sides.  nrhs >= 0.
        @param input double[][] A of dimension (lda, n).  The original symmetric matrix A.
        @param input int lda  The leading dimension of the array A.  lda >= max(1, n).
        @param input double[][] X of dimension (ldx, nrhs).
            The computed solution vectors for the system of linear equations.
        @param (input/output) double[][] B of dimension (ldb, nrhs)
            On entry, the right hand side vectors for the system of
            linear equations.
            On exit, B is overwritten with the difference B - A*X.
        @param input int ldb  The leading dimension of the array B.  ldb >= max(1, n).
        @param output double[] rwork of dimension n
        @param output double[] resid of dimension 1
            The maximum over the number of right hand sides of
            norm(B - A*X) / ( norm(A) * norm(X) * eps).
      */
      private void dpot02(char uplo, int n, int nrhs, double[][] A, int lda, double[][] X, int ldx,
                          double[][] B, int ldb, double[] rwork, double[] resid) {
          int j;
          double eps;
          double anorm;
          double bnorm;
          double xnorm;
          int i;
          
          // Quick exit if n = 0 or nrhs = 0.
       
          if (n <= 0 || nrhs <= 0) {
              resid[0] = 0.0;
              return;
          }
      
          // Exit with resid[0] = 1/eps if anorm = 0.
      
          eps = ge.dlamch('E'); // Epsilon
          anorm = ge.dlansy('1', uplo, n, A, lda, rwork);
          if (anorm <= 0.0) {
              resid[0] = 1.0 / eps;
              return;
          }
      
          // Compute  B - A*X
      
          ge.dsymm('L', uplo, n, nrhs, -1.0, A, lda, X, ldx, 1.0, B, ldb);
      
          // Compute the maximum over the number of right hand sides of
          // norm( B - A*X ) / ( norm(A) * norm(X) * eps) .
      
          resid[0] = 0.0;
          for (j = 0; j < nrhs; j++) {
               bnorm = 0.0;
               for (i = 0; i < n; i++) {
                   bnorm += Math.abs(B[i][j]);
               }
               xnorm = 0.0;
               for (i = 0; i < n; i++) {
                   xnorm += Math.abs(X[i][j]);
               }
               if (xnorm <= 0.0) {
                   resid[0] = 1.0 / eps;
               }
               else {
                   resid[0] = Math.max(resid[0], ( ( bnorm / anorm ) / xnorm ) / eps );
               }
          } // for (j = 0; j < nrhs; j++)
          
          return;

      } // dpot02
      
    /**
     * This is a port of LAPACK version test routine 3.4.0 DPOT03.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dpot03 computes the residual for a symmetric matrix times its inverse:
       norm( I - A*AINV ) / (n * norm(A) * norm(AINV) * eps),
       where eps is the machine epsilon.
       @param input char uplo
           Specifies whether the upper or lower triangular part of the
           symmetric matrix A is stored:
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input int n  The number of rows and columns of the matrix A.  n >= 0.
       @param input double[][] A of dimension (lda, n).  The original symmetric matrix A.
       @param input int lda  The leading dimension of the array A.  lda >= max(1, n).
       @param (input/output) double[][] AINV of dimension (ldainv, n)
           On entry, the inverse of the matrix A, stored as a symmetric
           matrix in the same format as A.
           In this version, AINV is expanded into a full matrix and
           multiplied by A, so the opposing triangle of AINV will be
           changed; i.e., if the upper triangular part of AINV is
           stored, the lower triangular part will be used as work space.
       @param input int ldainv  The leading dimension of the array AINV.  ldainv >= max(1, n).
       @param output double[][] work of dimension (ldwork, n).
       @param input int ldwork  The leading dimension of the array work.  ldwork >= max(1, n).
       @param output double[] rwork of dimension n
       @param output double[] rcond of dimension 1
           The reciprocal of the condition number of A, computed as
           ( 1/norm(A) ) / norm(AINV).
       @param output double[] resid of dimension 1
           norm(I - A*AINV) / (n * norm(A) * norm(AINV) * eps)
     */
     private void dpot03(char uplo, int n, double[][] A, int lda, double[][] AINV, int ldainv,
                         double[][] work, int ldwork, double rwork[], double[] rcond, double[] resid) {
         int i;
         int j;
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
     
         eps = ge.dlamch('E'); // Epsilon
         anorm = ge.dlansy('1', uplo, n, A, lda, rwork);
         ainvnm = ge.dlansy('1', uplo, n, AINV, ldainv, rwork);
         if (anorm <= 0.0 || ainvnm <= 0.0) {
             rcond[0] = 0.0;
             resid[0] = 1.0 / eps;
             return;
         }
         rcond[0] = ( 1.0 / anorm ) / ainvnm;
     
         // Expand AINV into a full matrix and call dsymm to multiply
         // AINV on the left by A.
      
         if ((uplo == 'U') || (uplo == 'u')) {
             for (j = 1; j <= n; j++) {
                 for (i = 1; i <= j-1; i++) {
                     AINV[j-1][i-1] = AINV[i-1][j-1];
                 }
             } // for (j = 1; j <= n; j++)
         }
         else {
             for (j = 1; j <= n; j++) {
                 for (i = j+1; i <= n; i++) {
                     AINV[j-1][i-1] = AINV[i-1][j-1];
                 }
             } // for (j = 1; j <= n; j++)
         }
         ge.dsymm('L', uplo, n, n, -1.0, A, lda, AINV, ldainv, 0.0,
                  work, ldwork);
     
         // Add the identity matrix to WORK .
     
         for (i = 0; i < n; i++) {
             work[i][i] = work[i][i] + 1.0;
         }
     
         // Compute norm(I - A*AINV) / (n * norm(A) * norm(AINV) * eps)
     
         resid[0] = ge.dlange('1', n, n, work, ldwork, rwork);
     
         resid[0] = ( (resid[0]*rcond[0] ) / eps ) / (double)( n );
     
         return;

     } // dpot03
     
     /**
      * This is a port of LAPACK version test routine 3.4.0 DPOT05.F created by the University of Tennessee, University
      * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
      * 
      * DPOT05 tests the error bounds from iterative refinement for the
        computed solution to a system of equations A*X = B, where A is a
        symmetric n by n matrix.

        reslts[0] = test of the error bound
                  = norm(X - XACT) / ( norm(X) * ferr)

        A large value is returned if this ratio is not less than one.

        reslts[1] = residual from the iterative refinement routine
                  = the maximum of berr / ( (n+1)*eps + (*) ), where
                 (*) = (n+1)*UNFL / (min_i (abs(A)*abs(X) +abs(b))_i )

        @param input char uplo
            Specifies whether the upper or lower triangular part of the
            symmetric matrix A is stored.
            = 'U':  Upper triangular
            = 'L':  Lower triangular
        @param input int n
            The number of rows of the matrices X, B, and XACT, and the
            order of the matrix A.  n >= 0.
        @param input int nrhs
            The number of columns of the matrices X, B, and XACT.
            nrhs >= 0.
        @param input double[][] A of dimension (lda, n)
            The symmetric matrix A.  If uplo = 'U', the leading n by n
            upper triangular part of A contains the upper triangular part
            of the matrix A, and the strictly lower triangular part of A
            is not referenced.  If uplo = 'L', the leading n by n lower
            triangular part of A contains the lower triangular part of
            the matrix A, and the strictly upper triangular part of A is
            not referenced.
        @param input int lda
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
        @param input double[][] XACT of dimension (ldx, nrhs)
            The exact solution vectors.  Each vector is stored as a
            column of the matrix XACT.
        @param input int ldxact
            The leading dimension of the array XACT.  ldxact >= max(1,n).
        @param input double[] ferr of dimension (nrhs)
            The estimated forward error bounds for each solution vector
            X.  If XTRUE is the true solution, FERR bounds the magnitude
            of the largest entry in (X - XTRUE) divided by the magnitude
            of the largest entry in X.
        @param input double[] berr of dimension (nrhs)
            The componentwise relative backward error of each solution
            vector (i.e., the smallest relative change in any entry of A
            or B that makes X an exact solution).
        @param output double[] reslts of dimension (2).
            The maximum over the nrhs solution vectors of the ratios:
            reslts[0] = norm(X - XACT) / ( norm(X) * ferr)
            reslts[1] = berr / ( (n+1)*eps + (*) )
            (*) = (n+1)*UNFL / (min_i (abs(A)*abs(X) +abs(b))_i )
      */
     private void dpot05(char uplo, int n, int nrhs, double[][] A, int lda, double[][] B,
                         int ldb, double[][] X, int ldx, double[][] XACT, int ldxact, double[] ferr,
                         double[] berr, double[] reslts) {
         boolean upper;
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
         
         // Quick exit if N = 0 or NRHS = 0.
                 
         if (n <= 0 || nrhs <= 0) {
             reslts[0] = 0.0;
             reslts[1] = 0.0;
             return;
         }
 
         eps = ge.dlamch('E'); // Epsilon
         unfl = ge.dlamch('S'); // Safe minimum
         ovfl = 1.0 / unfl;
         upper = ((uplo == 'U') || (uplo == 'u'));
  
         // Test 1:  Compute the maximum of
         // norm(X - XACT) / ( norm(X) * ferr)
         // over all the vectors X and XACT using the infinity-norm.
 
         errbnd = 0.0;
         for (j = 0; j < nrhs; j++) {
             imax = 0;
             maxVal = Math.abs(X[0][j]);
             for (k = 1; k < n; k++) {
                 if (Math.abs(X[k][j]) > maxVal) {
                     maxVal = Math.abs(X[k][j]);
                     imax = k;
                 }
             }
             xnorm = Math.max(Math.abs(X[imax][j]), unfl);
             diff = 0.0;
             for (i = 0; i < n; i++) {
                 diff = Math.max(diff, Math.abs(X[i][j]-XACT[i][j]));
             }
             
             if ((xnorm <= 1.0) && (diff > ovfl * xnorm)) {
                 errbnd = 1.0/eps;
                 continue;
             }
  
             if (diff / xnorm <= ferr[j]) {
                 errbnd = Math.max(errbnd, (diff/xnorm) / ferr[j]);
             }
             else {
                 errbnd = 1.0 / eps;
             }
         } // for (j = 0; j < nrhs; j++)
         reslts[0] = errbnd;
 
         // Test 2:  Compute the maximum of BERR / ( (n+1)*EPS + (*) ), where
         // (*) = (n+1)*UNFL / (min_i (abs(A)*abs(X) +abs(b))_i )
  
         for (k = 1; k <= nrhs; k++) {
             for (i = 1; i <= n; i++) {
                 tmp = Math.abs(B[i-1][k-1]);
                 if (upper) {
                     for (j = 1; j <= i; j++) {
                         tmp = tmp + Math.abs(A[j-1][i-1])*Math.abs(X[j-1][k-1]);
                     } // for (j = 1; j <= i; j++)
                     for (j = i+1; j <= n; j++) {
                         tmp = tmp + Math.abs(A[i-1][j-1])*Math.abs(X[j-1][k-1]);
                     } // for (j = i+1; j <= n; j++)
                 } // if (upper)
                 else {
                     for (j = 1; j <= i-1; j++) {
                         tmp = tmp + Math.abs(A[i-1][j-1])*Math.abs(X[j-1][k-1]);
                     } // for (j = 1; j <= i-1; j++)
                     for (j = i; j <= n; j++) {
                         tmp = tmp + Math.abs(A[j-1][i-1])*Math.abs(X[j-1][k-1]);
                     } // for (j = i; j <= n; j++)
                 } // else
                 if (i == 1) {
                     axbi = tmp;
                 }
                 else {
                     axbi = Math.min(axbi, tmp);
                 }
             } // for (i = 1; i <= n; i++)
             tmp = berr[k-1] / ( (n+1)*eps+(n+1)*unfl / Math.max(axbi, (n+1)*unfl));
             if (k == 1) {
                 reslts[1] = tmp;
             }
             else {
                 reslts[1] = Math.max(reslts[1], tmp);
             }
         } // for (k = 1; k <= nrhs; k++)
 
         return;

     } // dpot05
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DPOSV.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November, 2011.
     * 
     * dposv computes the solution to a system of linear equations A*X = B for PO matrices
     * 
     * dposv computes the solution to a real system of linear equations
     *     A * X = B
     * where A is an n by n symmetric positive definite matrix and X and B are n by nrhs matrices.
     * 
     * The Cholesky decomposition is used to factor A as 
     *     A = U**T * U, if uplo = 'U', or
     *     A = L * L**T, if uplo = 'L',
     * where U is an upper triangular matrix and L is a lower triangular matrix.  The factored form
     * of A is then used to solve the system of equations A * X = B.
     * @param input char uplo
     *     = 'U': Upper triangle of A is stored 
     *     = 'L': Lower triangle of A is stored
     * @param input int n  The number of linear equations, i.e., the order of the matrix A.  n >= 0.
     * @param input int nrhs  The number of right hand sides, i.e., the number of columns of the matrix B.
     *     nrhs >= 0.
     * @param (input/output) double[][] A of dimension (lda, n)
           On entry, the symmetric matrix A.  If uplo = 'U', the leading
           n-by-n upper triangular part of A contains the upper
           triangular part of the matrix A, and the strictly lower
           triangular part of A is not referenced.  If uplo = 'L', the
           leading n-by-n lower triangular part of A contains the lower
           triangular part of the matrix A, and the strictly upper
           triangular part of A is not referenced.

           On exit, if info[0] = 0, the factor U or L from the Cholesky
           factorization A = U**T*U or A = L*L**T.
       @param input int lda.  The leading dimension of the array A.  lda >= max(1, n).
       @param (input/output) double[][] B of dimension (ldb,nrhs)
           On entry, the n by nrhs right hand side of matrix B.
           On exit, if info[0] = 0, the n by nrhs solution matrix X.
       @param input int ldb  The leading dimension of the array B.  ldb >= max(1,n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
           > 0: if info[0] = i, the leading minor of order i of A is not positive definite, so the
                factorization could not be completed, and the solution has not been computed.
     */
    public void dposv(char uplo, int n, int nrhs, double[][] A, int lda, double[][] B, int ldb, int info[]) {
        
        // Test the input parameters.
    
        info[0] = 0;
        if (!((uplo == 'U') || (uplo == 'u')) && !((uplo == 'L') || (uplo == 'l'))) {
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
            info[0] = -7;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dposv had info[0] = " + info[0]);
            return;
        }
    
        // Compute the Cholesky factorization A = U**T*U or A = L*L**T.
   
        dpotrf(uplo, n, A, lda, info);
        if (info[0] == 0) {
                
            // Solve the system A*X = B, overwriting B with X.
                
            dpotrs(uplo, n, nrhs, A, lda, B, ldb, info);
        } // if (info[0] == 0)

        return;

    } // dposv
    
    /**
     * This is a port of LAPACK version routine 3.4.1 DPOSVX.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., April, 2012.
     * 
     * dposvx computes the solution to a system of linear equations A*X = B for PO matrices
     * 
     * dposvx uses the Cholesky factorization A = U**T*U or A = L*L**T to
       compute the solution to a real system of linear equations
       A * X = B,
       where A is an n-by-n symmetric positive definite matrix and X and B
       are n-by-nrhs matrices.

       Error bounds on the solution and a condition estimate are also
       provided.
       
       The following steps are performed:

       1. If fact = 'E', real scaling factors are computed to equilibrate
          the system:
          diag(s) * A * diag(s) * inv(diag(s)) * X = diag(s) * B
          Whether or not the system will be equilibrated depends on the
          scaling of the matrix A, but if equilibration is used, A is
          overwritten by diag(s)*A*diag(s) and B by diag(s)*B.

       2. If fact = 'N' or 'E', the Cholesky decomposition is used to
          factor the matrix A (after equilibration if fact = 'E') as
          A = U**T* U,  if uplo = 'U', or
          A = L * L**T,  if uplo = 'L',
          where U is an upper triangular matrix and L is a lower triangular
          matrix.

       3. If the leading i-by-i principal minor is not positive definite,
          then the routine returns with info[0] = i. Otherwise, the factored
          form of A is used to estimate the condition number of the matrix
          A.  If the reciprocal of the condition number is less than machine
          precision, info[0] = n+1 is returned as a warning, but the routine
          still goes on to solve for X and compute error bounds as
          described below.

       4. The system of equations is solved for X using the factored form
          of A.

       5. Iterative refinement is applied to improve the computed solution
          matrix and calculate error bounds and backward error estimates
          for it.

       6. If equilibration was used, the matrix X is premultiplied by
          diag(s) so that it solves the original system before
          equilibration.

       @param input char fact
           Specifies whether or not the factored form of the matrix A is
           supplied on entry, and if not, whether the matrix A should be
           equilibrated before it is factored.
           = 'F':  On entry, AF contains the factored form of A.
                   If equed[0] = 'Y', the matrix A has been equilibrated
                   with scaling factors given by s.  A and AF will not
                   be modified.
           = 'N':  The matrix A will be copied to AF and factored.
           = 'E':  The matrix A will be equilibrated if necessary, then
                   copied to AF and factored.
       @param input char uplo
           = 'U':  Upper triangle of A is stored;
           = 'L':  Lower triangle of A is stored.
       @param input int n
           The number of linear equations, i.e., the order of the
           matrix A.  n >= 0.
       @param input int nrhs
           The number of right hand sides, i.e., the number of columns
           of the matrices B and X.  nrhs >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the symmetric matrix A, except if fact = 'F' and
           equed[0] = 'Y', then A must contain the equilibrated matrix
           diag(s)*A*diag(s).  If uplo = 'U', the leading
           n-by-n upper triangular part of A contains the upper
           triangular part of the matrix A, and the strictly lower
           triangular part of A is not referenced.  If uplo = 'L', the
           leading n-by-n lower triangular part of A contains the lower
           triangular part of the matrix A, and the strictly upper
           triangular part of A is not referenced.  A is not modified if
           fact = 'F' or 'N', or if FACT = 'E' and equed[0] = 'N' on exit.

           On exit, if fact = 'E' and equed[0] = 'Y', A is overwritten by
           diag(s)*A*diag(s).
       @param input int lda
           The leading dimension of the array A.  lda >= max(1,n).
       @param (input/output) double[][] AF of dimension (ldaf, n)
           If fact = 'F', then AF is an input argument and on entry
           contains the triangular factor U or L from the Cholesky
           factorization A = U**T*U or A = L*L**T, in the same storage
           format as A.  If equed[0] .ne. 'N', then AF is the factored form
           of the equilibrated matrix diag(s)*A*diag(s).

           If fact = 'N', then AF is an output argument and on exit
           returns the triangular factor U or L from the Cholesky
           factorization A = U**T*U or A = L*L**T of the original
           matrix A.
  
           If fact = 'E', then AF is an output argument and on exit
           returns the triangular factor U or L from the Cholesky
           factorization A = U**T*U or A = L*L**T of the equilibrated
           matrix A (see the description of A for the form of the
           equilibrated matrix).
       @param input int ldaf
           The leading dimension of the array AF.  ldaf >= max(1,n).
       @param (input/output) char[] equed of dimension (1).
           Specifies the form of equilibration that was done.
           = 'N':  No equilibration (always true if fact = 'N').
           = 'Y':  Equilibration was done, i.e., A has been replaced by
                   diag(s) * A * diag(s).
           equed[0] is an input argument if fact = 'F'; otherwise, it is an
           output argument.
       @param (input/output) double[] s of dimension (n)
           The scale factors for A; not accessed if equed[0] = 'N'.  s is
           an input argument if fact = 'F'; otherwise, s is an output
           argument.  If fact = 'F' and equed[0] = 'Y', each element of s
           must be positive.
       @param (input/output) double[][] B of dimension (ldb, nrhs)
           On entry, the n-by-nrhs right hand side matrix B.
           On exit, if equed[0] = 'N', B is not modified; if equed[0] = 'Y',
           B is overwritten by diag(s) * B.
       @param input int ldb
           The leading dimension of the array B.  LDB >= max(1,n).
       @param output double[][] X of dimension (ldx, nrhs)
           If info[0] = 0 or info[0] = n+1, the n-by-nrhs solution matrix X to
           the original system of equations.  Note that if equed[0] = 'Y',
           A and B are modified on exit, and the solution to the
           equilibrated system is inv(diag(s))*X.
       @param input int ldx
           The leading dimension of the array X.  ldx >= max(1,n).
       @param output double[] rcond of dimension (1).
           The estimate of the reciprocal condition number of the matrix
           A after equilibration (if done).  If rcond is less than the
           machine precision (in particular, if rcond = 0), the matrix
           is singular to working precision.  This condition is
           indicated by a return code of info[0] > 0.
       @param output double[] ferr of dimension (nrhs)
           The estimated forward error bound for each solution vector
           X[j] (the j-th column of the solution matrix X).
           If XTRUE is the true solution corresponding to X[j], ferr[j]
           is an estimated upper bound for the magnitude of the largest
           element in (X[j] - XTRUE) divided by the magnitude of the
           largest element in X[j].  The estimate is as reliable as
           the estimate for rcond, and is almost always a slight
           overestimate of the true error.
       @param output double[] berr of dimension (nrhs)
           The componentwise relative backward error of each solution
           vector X[j] (i.e., the smallest relative change in
           any element of A or B that makes X[j] an exact solution).
       @param output double[] work of dimension (3*n)
       @param output int[] iwork of dimension (n)
       @param output int[] info of dimension (1)
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
           > 0: if info[0] = i, and i is
                <= n:  the leading minor of order i of A is
                       not positive definite, so the factorization
                       could not be completed, and the solution has not
                       been computed. rcond = 0 is returned.
                = n+1: U is nonsingular, but rcond is less than machine
                       precision, meaning that the matrix is singular
                       to working precision.  Nevertheless, the
                       solution and error bounds are computed because
                       there are a number of situations where the
                       computed solution can be more accurate than the
                       value of rcond would suggest.
     */
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DPOTRF.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November, 2011.
     * 
     * dpotrf computes the Cholesky factorization of a real symmetric positive definite matrix A.
     * 
     * The factorization has the form
     *     A = U**T * U, if uplo = 'U', or
     *     A = L * L**T, if uplo = 'L',
     * where U is an upper triangular matrix and L is lower triangular.
     * 
     * This is the block version of the algorithm
     * 
     * @param input char uplo
     *     = 'U': Upper triangle of A is stored 
     *     = 'L': Lower triangle of A is stored
     * @param input int n  The order of the matrix A.  n >= 0.
     * @param (input/output) double[][] A of dimension (lda, n)
            On entry, the symmetric matrix A.  If uplo = 'U', the leading
            n by n upper triangular part of A contains the upper
            triangular part of the matrix A, and the strictly lower
            triangular part of A is not referenced.  If uplo = 'L', the
            leading n by n lower triangular part of A contains the lower
            triangular part of the matrix A, and the strictly upper
            triangular part of A is not referenced.

            On exit, if info[0] = 0, the factor U or L from the Cholesky
            factorization A = U**T *U  or A = L*L**T.
       @param input int lda.  The leading dimension of the array A.  lda >= max(1, n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
           > 0: if info[0] = i, the leading minor of order i is not
                positive definite, and the factorization could not be
                completed.
     */
    private void dpotrf(char uplo, int n, double[][] A, int lda, int info[]) {
        boolean upper;
        int j;
        int jb;
        int nb;
        int i;
        double arr[][];
        int k;
        String name;
        char optsChar[] = new char[1];
        String opts;
        double arr2[][];
        double arr3[][];
        
        // Test the input parameters.
        
          info[0] = 0;
          upper = ((uplo == 'U') || (uplo == 'u'));
          if (!upper && !((uplo == 'L') || (uplo == 'l')) ) {
             info[0] = -1;
          }
          else if (n < 0) {
             info[0] = -2;
          }
          else if (lda < Math.max(1, n)) {
             info[0] = -4;
          }
          if (info[0] != 0) {
             MipavUtil.displayError("dptorf had info[0] = " + info[0]);
             return;
          }
    
          // Quick return if possible
    
          if (n == 0) {
             return;
          }
    
          // Determine the block size for this environment.
          name = new String("DPOTRF");
          optsChar[0] = uplo;
          opts = new String(optsChar);
          nb = ge.ilaenv(1, name, opts, n, -1, -1, -1);
          if (nb <= 1 || nb >= n) {
       
              // Use unblocked code.
      
              dpotf2(uplo, n, A, lda, info);
          }
          else { // blocked code
      
              // Use blocked code.
      
              if (upper) {
      
                  // Compute the Cholesky factorization A = U**T*U.
       
                  for (j = 1; j <= n; j += nb) {
      
                     // Update and factorize the current diagonal block and test
                     // for non-positive-definiteness.
       
                     jb = Math.min(nb, n-j+1);
                     arr = new double[j-1][jb];
                     for (i = 0; i < j-1; i++) {
                         for (k = 0; k < jb; k++) {
                             arr[i][k] = A[i][j-1+k];
                         }
                     }
                     arr2 = new double[jb][jb];
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             arr2[i][k] = A[j-1+i][j-1+k];
                         }
                     }
                     ge.dsyrk('U', 'T', jb, j-1, -1.0,
                              arr, lda, 1.0, arr2, lda);
                     dpotf2('U', jb, arr2, lda, info);
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             A[j-1+i][j-1+k] = arr2[i][k];
                         }
                     }
                     if (info[0] != 0) {
                         info[0] = info[0] + j - 1;
                         return;
                     }
                     if (j+jb <= n) {
       
                        // Compute the current block row.
                        arr = new double[j-1][jb];
                        for (i = 0; i < j-1; i++) {
                            for (k = 0; k < jb; k++) {
                                arr[i][k] = A[i][j-1+k];
                            }
                        }
                        arr2 = new double[j-1][n-j-jb+1];
                        for (i = 0; i < j-1; i++) {
                            for (k = 0; k < n-j-jb+1; k++) {
                                arr2[i][k] = A[i][j+jb-1+k];
                            }
                        }
                        arr3 = new double[jb][n-j-jb+1];
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < n-j-jb+1; k++) {
                                arr3[i][k] = A[j-1+i][j+jb-1+k];
                            }
                        }
                        ge.dgemm('T', 'N', jb, n-j-jb+1,
                                 j-1, -1.0, arr, lda, arr2,
                                 lda, 1.0, arr3, lda);
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < n-j-jb+1; k++) {
                                A[j-1+i][j+jb-1+k] = arr3[i][k];
                            }
                        }
                        arr = new double[jb][jb];
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < jb; k++) {
                                arr[i][k] = A[j-1+i][j-1+k];
                            }
                        }
                        ge.dtrsm('L', 'U', 'T', 'N',
                                 jb, n-j-jb+1, 1.0, arr, lda,
                                 arr3, lda);
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < n-j-jb+1; k++) {
                                A[j-1+i][j+jb-1+k] = arr3[i][k];
                            }
                        }
                     } // if (j+jb <= n)
                  } // for (j = 1; j <= n; j += nb)
       
              } // if (upper)
              else { // lower
       
                  // Compute the Cholesky factorization A = L*L**T.
       
                  for (j = 1 ;j <= n; j += nb) {
       
                     // Update and factorize the current diagonal block and test
                     // for non-positive-definiteness.
       
                     jb = Math.min(nb, n-j+1);
                     arr = new double[jb][j-1];
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < j-1; k++) {
                             arr[i][k] = A[j-1+i][k];
                         }
                     }
                     arr2 = new double[jb][jb];
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             arr2[i][k] = A[j-1+i][j-1+k];
                         }
                     }
                     ge.dsyrk('L', 'N', jb, j-1, -1.0,
                              arr, lda, 1.0, arr2, lda);
                     dpotf2('L', jb, arr2, lda, info);
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             A[j-1+i][j-1+k] = arr2[i][k];
                         }
                     }
                     if (info[0] != 0) {
                         info[0] = info[0] + j - 1;
                         return;
                     }
                     if (j+jb <= n) {
       
                        // Compute the current block column.
                        arr = new double[n-j-jb+1][j-1];
                        for (i = 0; i < n-j-jb+1; i++) {
                            for (k = 0; k < j-1; k++) {
                                arr[i][k] = A[j+jb-1+i][k];
                            }
                        }
                        arr2 = new double[jb][j-1];
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < j-1; k++) {
                                arr2[i][k] = A[j-1+i][k];
                            }
                        }
                        arr3 = new double[n-j-jb+1][jb];
                        for (i = 0; i < n-j-jb+1; i++) {
                            for (k = 0; k < jb; k++) {
                                arr3[i][k] = A[j+jb-1+i][j-1+k];
                            }
                        }
                        ge.dgemm('N', 'T', n-j-jb+1, jb,
                                 j-1, -1.0, arr, lda, arr2,
                                 lda, 1.0, arr3, lda);
                        for (i = 0; i < n-j-jb+1; i++) {
                            for (k = 0; k < jb; k++) {
                                A[j+jb-1+i][j-1+k] = arr3[i][k];
                            }
                        }
                        arr = new double[jb][jb];
                        for (i = 0; i < jb; i++) {
                            for (k = 0; k < jb; k++) {
                                arr[i][k] = A[j-1+i][j-1+k];
                            }
                        }
                        ge.dtrsm('R', 'L', 'T', 'N',
                                 n-j-jb+1, jb, 1.0, arr, lda,
                                 arr3, lda);
                        for (i = 0; i < n-j-jb+1; i++) {
                            for (k = 0; k < jb; k++) {
                                A[j+jb-1+i][j-1+k] = arr3[i][k];
                            }
                        }
                     } // if (j+jb <= n)
                  } // for (j = 1; j <= n; j += nb)
              } // else lower
          } // else blocked code
           
          return;
    } // dpotrf
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DPOTRS.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November, 2011.
     * 
     * dpotrs solves a system of linear equations A*X = B with a symmetric positive definite matrix A
     * using the Cholesky factorization A = U**T*U or A = L*L**T computed by dpotrf.
     * 
     * @param input char uplo
     *     = 'U': Upper triangle of A is stored 
     *     = 'L': Lower triangle of A is stored
     * @param input int n  The order of the matrix A.  n >= 0.
     * @param input int nrhs  The number of right hand sides, i.e., the number of columns of the matrix B.
     *     nrhs >= 0.
     * @param input double[][] A of dimension (lda, n)
           The triangular factor U or L from the Cholesky factorization 
           A = U**T*U or A = L*L**T, as computed by dpotrf.
       @param input int lda.  The leading dimension of the array A.  lda >= max(1, n).
       @param (input/output) double[][] B of dimension (ldb,nrhs)
           On entry, the right hand side of the array B.
           On exit, the solution matrix X.
       @param input int ldb  The leading dimension of the array B.  ldb >= max(1,n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
     */
    private void dpotrs(char uplo, int n, int nrhs, double[][] A, int lda, double[][] B, int ldb, int info[]) {
        boolean upper;
        
        // Test the input parameters.
    
        info[0] = 0;
        upper = ((uplo == 'U') || (uplo == 'u'));
        if (!upper && !((uplo == 'L') || (uplo == 'l'))) {
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
            info[0] = -7;
        }
        if (info[0] != 0) {
            MipavUtil.displayError("dpotrs had info[0] = " + info[0]);
            return;
        }
    
        // Quick return if possible
    
        if (n == 0 || nrhs == 0) {
            return;
        }
    
        if (upper) {
    
            // Solve A*X = B where A = U**T *U.
    
            // Solve U**T *X = B, overwriting B with X.
    
            ge.dtrsm('L', 'U', 'T', 'N', n, nrhs,
                      1.0, A, lda, B, ldb);
    
            // Solve U*X = B, overwriting B with X.
     
            ge.dtrsm('L', 'U', 'N', 'N', n,
                      nrhs, 1.0, A, lda, B, ldb);
        } // if (upper)
        else { // lower
    
            // Solve A*X = B where A = L*L**T.
    
            // Solve L*X = B, overwriting B with X.
    
            ge.dtrsm('L', 'L', 'N', 'N', n,
                     nrhs, 1.0, A, lda, B, ldb);
    
            // Solve L**T *X = B, overwriting B with X.
     
            ge.dtrsm('L', 'L', 'T', 'N', n, nrhs,
                     1.0, A, lda, B, ldb);
        } // else lower 
    
        return;

    } // dpotrs
    
    /**
     * This is a port of LAPACK version routine 3.4.2 DPOTF2.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., September 2012.
     * 
     * dpotf2 computes the Cholesky factorization of a symmetric Hermetian positive definite matrix (unblocked
     * algorithm).
     * 
     * dpotf2 computes the Cholesky factorization of a real symmetric positive definite matrix A.
     * 
     * The factorization has the form
     *     A = U**T * U, if uplo = 'U', or
     *     A = L * L**T, if uplo = 'L',
     * where U is an upper triangular matrix and L is lower triangular.
     * 
     * This is the unblocked version of the algorithm
     * 
     * @param input char uplo Specifies whether the upper or lower triangular part of the symmetric matrix A is stored.
     *     = 'U': Upper triangular 
     *     = 'L': Lower triangular
     * @param input int n  The order of the matrix A.  n >= 0.
     * @param (input/output) double[][] A of dimension (lda, n)
            On entry, the symmetric matrix A.  If uplo = 'U', the leading
            n by n upper triangular part of A contains the upper
            triangular part of the matrix A, and the strictly lower
            triangular part of A is not referenced.  If uplo = 'L', the
            leading n by n lower triangular part of A contains the lower
            triangular part of the matrix A, and the strictly upper
            triangular part of A is not referenced.

            On exit, if info[0] = 0, the factor U or L from the Cholesky
            factorization A = U**T *U  or A = L*L**T.
       @param input int lda.  The leading dimension of the array A.  lda >= max(1, n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -k, the k-th argument had an illegal value
           > 0: if info[0] = k, the leading minor of order k is not
                positive definite, and the factorization could not be
                completed.
     */
    private void dpotf2(char uplo, int n, double[][] A, int lda, int info[]) {
        boolean upper;
        int j;
        double ajj;
        double vec[];
        int i;
        double arr[][];
        int k;
        double vec2[];
        
        // Test the input parameters.
        
          info[0] = 0;
          upper = ((uplo == 'U') || (uplo == 'u'));
          if (!upper && !((uplo == 'L') || (uplo == 'l')) ) {
             info[0] = -1;
          }
          else if (n < 0) {
             info[0] = -2;
          }
          else if (lda < Math.max(1, n)) {
             info[0] = -4;
          }
          if (info[0] != 0) {
             MipavUtil.displayError("dptof2 had info[0] = " + info[0]);
             return;
          }
    
          // Quick return if possible
    
          if (n == 0) {
             return;
          }
    
          if (upper) {
     
             // Compute the Cholesky factorization A = U**T *U.
     
             for (j = 1; j <= n; j++) {
     
                // Compute U[j-1][j-1] and test for non-positive-definiteness.
                vec = new double[j-1];
                for (i = 0; i < j-1; i++) {
                    vec[i] = A[i][j-1];
                }
                ajj = A[j-1][j-1] - ge.ddot(j-1, vec, 1, vec, 1);
                if (ajj <= 0.0 || Double.isNaN(ajj)) {
                   A[j-1][j-1] = ajj;
                   info[0] = j;
                   return;
                }
                ajj = Math.sqrt(ajj);
                A[j-1][j-1] = ajj;
    
                // Compute elements j:n-1 of row j-1.
    
                if (j < n) {
                   arr = new double[j-1][n-j];
                   for (i = 0; i < j-1; i++) {
                       for (k = 0; k < n-j; k++) {
                           arr[i][k] = A[i][j+k];
                       }
                   }
                   vec = new double[j-1];
                   for (i = 0; i < j-1; i++) {
                       vec[i] =  A[i][j-1];
                   }
                   vec2 = new double[n-j];
                   for (i = 0; i < n-j; i++) {
                       vec2[i] = A[j-1][j+i];
                   }
                   ge.dgemv('T', j-1, n-j, -1.0, arr,
                               lda, vec, 1, 1.0, vec2, 1);
                   ge.dscal(n-j, 1.0/ajj, vec2, 1);
                   for (i = 0; i < n-j; i++) {
                       A[j-1][j+i] = vec2[i];
                   }
                } // if (j < n)
             } // for (j = 1; j <= n; j++)
          } // if (upper)
          else { // lower
     
             // Compute the Cholesky factorization A = L*L**T.
     
             for (j = 1; j <= n; j++) {
     
                // Compute L[j-1][j-1] and test for non-positive-definiteness.
                vec = new double[j-1];
                for (i = 0; i < j-1; i++) {
                    vec[i] = A[j-1][i];
                }
                ajj = A[j-1][j-1] - ge.ddot(j-1, vec, 1, vec, 1);
                if (ajj <= 0.0 || Double.isNaN(ajj)) {
                   A[j-1][j-1] = ajj;
                   info[0] = j;
                   return;
                }
                ajj = Math.sqrt(ajj);
                A[j-1][j-1] = ajj;
    
                // Compute elements j:n-1 of column j-1.
    
                if (j < n) {
                   arr = new double[n-j][j-1];
                   for (i = 0; i < n-j; i++) {
                       for (k = 0; k < j-1; k++) {
                           arr[i][k] = A[j+i][k];
                       }
                   }
                   vec = new double[j-1];
                   for (i = 0; i < j-1; i++) {
                       vec[i] = A[j-1][i];
                   }
                   vec2 = new double[n-j];
                   for (i = 0; i < n-j; i++) {
                       vec2[i] = A[j+i][j-1];
                   }
                   ge.dgemv('N', n-j, j-1, -1.0, arr,
                            lda, vec, 1, 1.0, vec2, 1);
                   ge.dscal(n-j, 1.0/ajj, vec2, 1);
                   for (i = 0; i < n-j; i++) {
                       A[j+i][j-1] = vec2[i];
                   }
                } // if (j < n)
             } // for (j = 1; j <= n; j++)
          } // else lower
          
          return;
    } // dpotf2
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DPOTRI.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dpotri computes the inverse of a real symmetric positive definite
       matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
       computed by dpotrf.
       
       @param input char uplo
           = 'U': Upper triangle of A is stored;
           = 'L': Lower triangle of A is stored.
       @param input int n  The order of the matrix A.  n >= 0.
       @param (input/output) double[][] of dimension (lda, n)
           On entry, the triangular factor U or L from the Cholesky
           factorization A = U**T*U or A = L*L**T, as computed by dpotrf.
           On exit, the upper or lower triangle of the (symmetric)
           inverse of A, overwriting the input factor U or L.
       @param input int lda  The leading dimension of the array A.   lda >= max(1,n).
       @param output int[] of dimension 1.
           = 0: successful exit
           < 0: if info = -i, the i-th argument had an illegal value
           > 0: if info = i, the [i-1][i-1] element of the factor U or L is zero,
                and the inverse could not be computed.
     */
    private void dpotri(char uplo, int n, double[][] A, int lda, int info[]) {
     // Test the input parameters.
        
        info[0] = 0;
        if (!((uplo == 'U') || (uplo == 'u')) && !((uplo == 'L') || (uplo == 'l')) ) {
           info[0] = -1;
        }
        else if (n < 0) {
           info[0] = -2;
        }
        else if (lda < Math.max(1, n)) {
           info[0] = -4;
        }
        if (info[0] != 0) {
           MipavUtil.displayError("dpotri had info[0] = " + info[0]);
           return;
        }
  
        // Quick return if possible
  
        if (n == 0) {
           return;
        }   
        
        // Invert the triangular Cholesky factor U or L.
        
        dtrtri(uplo, 'N', n, A, lda, info);
        if (info[0] > 0) {
            return;
        }
        
        // Form inv(U) * inv(U)**T or inv(L)**T * inv(L).
        
        dlauum(uplo, n, A, lda, info);
        
        return;

    } // dpotri
    
    /**
     * This is a port of LAPACK version auxiliary routine 3.4.2 DLAUUM.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., September 2012.
     *
     * dlauum computes the product UUH or LHL, where U and L are upper or lower triangular matrices (blocked algorithm).
     * 
     * dlauum computes the product U * U**T or L**T * L, where the triangular
       factor U or L is stored in the upper or lower triangular part of the array A.

       If uplo = 'U' or 'u' then the upper triangle of the result is stored,
       overwriting the factor U in A.
       If uplo = 'L' or 'l' then the lower triangle of the result is stored,
       overwriting the factor L in A.

       This is the blocked form of the algorithm
       @param input char uplo  Specifies whether the triangular factor stored in the array 
           is upper or lower triangular:
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input int n  The order of the triangular factor U or L.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the triangular factor U or L.
           On exit, if uplo = 'U', the upper triangle of A is
           overwritten with the upper triangle of the product U * U**T;
           if uplo = 'L', the lower triangle of A is overwritten with
           the lower triangle of the product L**T * L.
       @param input int lda  The leading dimension of array A.  lda >= max(1, n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: If info[0] = -k, the k-th argument had an illegal value
     */
     private void dlauum(char uplo, int n, double[][] A, int lda, int info[]) {
         boolean upper;
         int i;
         int ib;
         int nb;
         int j;
         double arr[][];
         int k;
         String name;
         char charOpts[] = new char[1];
         String opts;
         double arr2[][];
         double arr3[][];
         
         // Test the input parameters.
         
         info[0] = 0;
         upper = ((uplo == 'U') || (uplo == 'u'));
         if (!upper && !((uplo == 'L') || (uplo == 'l')) ) {
            info[0] = -1;
         }
         else if (n < 0) {
            info[0] = -2;
         }
         else if (lda < Math.max(1, n)) {
            info[0] = -4;
         }
         if (info[0] != 0) {
            MipavUtil.displayError("dlauum had info[0] = " + info[0]);
            return;
         }
   
         // Quick return if possible
   
         if (n == 0) {
            return;
         }
         
         // Determine the block size for this environment.
     
         name = new String("DLAUUM");
         charOpts[0] = uplo;
         opts = new String(charOpts);
         nb = ge.ilaenv(1, name, opts, n, -1, -1, -1);
     
         if (nb <= 1 || nb >= n) {
     
             // Use unblocked code
     
             dlauu2(uplo, n, A, lda, info);
         }
         else { 
     
             // Use blocked code
   
             if (upper) {
     
                 // Compute the product U * U**T.
     
                 for (i = 1; i <= n; i += nb) {
                     ib = Math.min(nb, n-i+1);
                     arr = new double[ib][ib];
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             arr[j][k] = A[i-1+j][i-1+k];
                         }
                     }
                     arr2 = new double[i-1][ib];
                     for (j = 0; j < i-1; j++) {
                         for (k = 0; k < ib; k++) {
                             arr2[j][k] = A[j][i-1+k];
                         }
                     }
                     ge.dtrmm('R', 'U', 'T', 'N',
                              i-1, ib, 1.0, arr, lda, arr2, lda);
                     for (j = 0; j < i-1; j++) {
                         for (k = 0; k < ib; k++) {
                             A[j][i-1+k] = arr2[j][k];
                         }
                     }
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             arr[j][k] = A[i-1+j][i-1+k];
                         }
                     }
                     dlauu2('U', ib, arr, lda, info);
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                              A[i-1+j][i-1+k] = arr[j][k];
                         }
                     }
                     if (i+ib <= n) {
                         arr = new double[i-1][n-i-ib+1];
                         for (j = 0; j < i-1; j++) {
                             for (k = 0; k < n-i-ib+1; k++) {
                                 arr[j][k] = A[j][i+ib-1+k];
                             }
                         }
                         arr2 = new double[ib][n-i-ib+1];
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < n-i-ib+1; k++) {
                                 arr2[j][k] = A[i-1+j][i+ib-1+k];
                             }
                         }
                         arr3 = new double[i-1][ib];
                         for (j = 0; j < i-1; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr3[j][k] = A[j][i-1+k];
                             }
                         }
                         ge.dgemm('N', 'T', i-1, ib,
                                   n-i-ib+1, 1.0, arr, lda,
                                   arr2, lda, 1.0, arr3, lda);
                         for (j = 0; j < i-1; j++) {
                             for (k = 0; k < ib; k++) {
                                 A[j][i-1+k] = arr3[j][k];
                             }
                         }
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < n-i-ib+1; k++) {
                                 arr2[j][k] = A[i-1+j][i+ib-1+k];
                             }
                         }
                         arr = new double[ib][ib];
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr[j][k] = A[i-1+j][i-1+k];
                             }
                         }
                         ge.dsyrk('U', 'N', ib, n-i-ib+1,
                                  1.0, arr2, lda, 1.0, arr, lda);
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < ib; k++) {
                                 A[i-1+j][i-1+k] = arr[j][k];
                             }
                         }
                     } // if (i+ib <= n)
                 } // for (i = 1; i <= n; i += nb)
             } // if (upper)
             else { // lower
     
                 // Compute the product L**T * L.
     
                 for (i = 1; i <= n; i += nb) {
                     ib = Math.min(nb, n-i+1);
                     arr = new double[ib][ib];
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             arr[j][k] = A[i-1+j][i-1+k];
                         }
                     }
                     arr2 = new double[ib][i-1];
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < i-1; k++) {
                             arr2[j][k] = A[i-1+j][k];
                         }
                     }
                     ge.dtrmm('L', 'L', 'T', 'N', ib,
                              i-1, 1.0, arr, lda, arr2, lda);
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < i-1; k++) {
                             A[i-1+j][k] = arr2[j][k];
                         }
                     }
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             arr[j][k] = A[i-1+j][i-1+k];
                         }
                     }
                     dlauu2('L', ib, arr, lda, info);
                     for (j = 0; j < ib; j++) {
                         for (k = 0; k < ib; k++) {
                             A[i-1+j][i-1+k] = arr[j][k];
                         }
                     }
                     if (i+ib <= n) {
                         arr = new double[n-i-ib+1][ib];
                         for (j = 0; j < n-i-ib+1; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr[j][k] = A[i+ib-1+j][i-1+k];
                             }
                         }
                         arr2 = new double[n-i-ib+1][i-1];
                         for (j = 0; j < n-i-ib+1; j++) {
                             for (k = 0; k < i-1; k++) {
                                 arr2[j][k] = A[i+ib-1+j][k];
                             }
                         }
                         arr3 = new double[ib][i-1];
                         for (j = 0; j < ib; j ++) {
                             for (k = 0; k < i-1; k++) {
                                 arr3[j][k] = A[i-1+j][k];
                             }
                         }
                         ge.dgemm('T', 'N', ib, i-1,
                                  n-i-ib+1, 1.0, arr, lda,
                                  arr2, lda, 1.0, arr3, lda);
                         for (j = 0; j < ib; j ++) {
                             for (k = 0; k < i-1; k++) {
                                 A[i-1+j][k] = arr3[j][k];
                             }
                         }
                         for (j = 0; j < n-i-ib+1; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr[j][k] = A[i+ib-1+j][i-1+k];
                             }
                         }
                         arr2 = new double[ib][ib];
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < ib; k++) {
                                 arr2[j][k] = A[i-1+j][i-1+k];
                             }
                         }
                         ge.dsyrk('L', 'T', ib, n-i-ib+1, 1.0,
                                  arr, lda, 1.0, arr2, lda);
                         for (j = 0; j < ib; j++) {
                             for (k = 0; k < ib; k++) {
                                 A[i-1+j][i-1+k] = arr2[j][k];
                             }
                         }
                     } // if (i+ib <= n)
                 } // for (i = 1; i <= n; i += nb)
             } // else lower
         } // else use blocked code
     
         return;

     } // dlauum
    
    /**
     * This is a port of LAPACK version auxiliary routine 3.4.2 DLAUU2.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., September 2012.
     *
     * dlauu2 computes the product UUH or LHL, where U and L are upper or lower triangular matrices (unblocked algorithm).
     * 
     * dlauu2 computes the product U * U**T or L**T * L, where the triangular
       factor U or L is stored in the upper or lower triangular part of the array A.

       If uplo = 'U' or 'u' then the upper triangle of the result is stored,
       overwriting the factor U in A.
       If uplo = 'L' or 'l' then the lower triangle of the result is stored,
       overwriting the factor L in A.

       This is the unblocked form of the algorithm
       @param input char uplo  Specifies whether the triangular factor stored in the array 
           is upper or lower triangular:
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input int n  The order of the triangular factor U or L.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the triangular factor U or L.
           On exit, if uplo = 'U', the upper triangle of A is
           overwritten with the upper triangle of the product U * U**T;
           if uplo = 'L', the lower triangle of A is overwritten with
           the lower triangle of the product L**T * L.
       @param input int lda  The leading dimension of array A.  lda >= max(1, n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: If info[0] = -k, the k-th argument had an illegal value
     */
     private void dlauu2(char uplo, int n, double[][] A, int lda, int info[]) {
         boolean upper;
         int i;
         double aii;
         double vec[];
         int j;
         double arr[][];
         double vec2[];
         int k;
         
         // Test the input parameters.
         
         info[0] = 0;
         upper = ((uplo == 'U') || (uplo == 'u'));
         if (!upper && !((uplo == 'L') || (uplo == 'l')) ) {
            info[0] = -1;
         }
         else if (n < 0) {
            info[0] = -2;
         }
         else if (lda < Math.max(1, n)) {
            info[0] = -4;
         }
         if (info[0] != 0) {
            MipavUtil.displayError("dlauu2 had info[0] = " + info[0]);
            return;
         }
   
         // Quick return if possible
   
         if (n == 0) {
            return;
         }
         
         if (upper) {
         
             // Compute the product U * U**T.
         
             for (i = 1; i <= n; i++) {
                 aii = A[i-1][i-1];
                 if (i < n) {
                     vec = new double[n-i+1];
                     for (j = 0; j < n-i+1; j++) {
                         vec[j] = A[i-1][i-1+j];
                     }
                     A[i-1][i-1] = ge.ddot(n-i+1, vec, 1, vec, 1);
                     arr = new double[i-1][n-i];
                     for (j = 0; j < i-1; j++) {
                         for (k = 0; k < n-i; k++) {
                             arr[j][k] = A[j][i+k];
                         }
                     }
                     vec = new double[n-i];
                     for (j = 0; j < n-i; j++) {
                         vec[j] = A[i-1][i+j];
                     }
                     vec2 = new double[i-1];
                     for (j = 0; j < i-1; j++) {
                         vec2[j] = A[j][i-1];
                     }
                     ge.dgemv('N', i-1, n-i, 1.0, arr,
                              lda, vec, 1, aii, vec2, 1);
                     for (j = 0; j < i-1; j++) {
                         A[j][i-1] = vec2[j];
                     }
                 }
                 else {
                     vec = new double[i];
                     for (j = 0; j < i; j++) {
                         vec[j] = A[j][i-1];
                     }
                     ge.dscal(i, aii, vec, 1 );
                     for (j = 0; j < i; j++) {
                         A[j][i-1] = vec[j];
                     }
                 }
             } // for (i = 1; i <= n; i++)
         
         } // if (upper)
         else { // lower
        
             // Compute the product L**T * L.
         
             for (i = 1; i <= n; i++) {
                 aii = A[i-1][i-1];
                 if (i < n) {
                     vec = new double[n-i+1];
                     for (j = 0; j < n-i+1; j++) {
                         vec[j] = A[i-1+j][i-1];
                     }
                     A[i-1][i-1] = ge.ddot(n-i+1, vec, 1, vec, 1 );
                     arr = new double[n-i][i-1];
                     for (j = 0; j < n-i; j++) {
                         for (k = 0; k < i-1; k++) {
                             arr[j][k] = A[i+j][k];
                         }
                     }
                     vec = new double[n-i];
                     for (j = 0; j < n-i; j++) {
                         vec[j] = A[i+j][i-1];
                     }
                     vec2 = new double[i-1];
                     for (j = 0; j < i-1; j++) {
                         vec2[j] = A[i-1][j];
                     }
                     ge.dgemv('T', n-i, i-1, 1.0, arr, lda,
                              vec, 1, aii, vec2, 1);
                     for (j = 0; j < i-1; j++) {
                         A[i-1][j] = vec2[j];
                     }
                 }
                 else {
                     vec = new double[i];
                     for (j = 0; j < i; j++) {
                         vec[j] = A[i-1][j];
                     }
                     ge.dscal(i, aii, vec, 1);
                     for (j = 0; j < i; j++) {
                         A[i-1][j] = vec[j];
                     }
                 }
             } // for (i = 1; i <= n; i++)
         } // else lower
         
         return;

     } // dlauu2
    
    /**
     * This is a port of LAPACK version routine 3.4.0 DTRTRI.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., November 2011.
     * 
     * dtrtri computes the inverse of a real upper or lower triangular matrix A.
     * 
       @param input char uplo
           = 'U':  A is upper triangular
           = 'L':  A is lower triangular
       @param input char diag 
           = 'N':  A is non-unit triangular
           = 'U':  A is unit triangular
       @param input int n  The order of the matrix A.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the triangular matrix A.  If uplo = 'U', the
           leading n by n upper triangular part of the array A contains
           the upper triangular matrix, and the strictly lower
           triangular part of A is not referenced.  If uplo = 'L', the
           leading n by n lower triangular part of the array A contains
           the lower triangular matrix, and the strictly upper
           triangular part of A is not referenced.  If diag = 'U', the
           diagonal elements of A are also not referenced and are
           assumed to be 1.

           On exit, the (triangular) inverse of the original matrix, in
           the same storage format.
       @param inpuut int lda  The leading dimension of the array A.  lda >= max(1,n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -i, the i-th argument had an illegal value
           > 0: if info[0] = i, A[i-1][i-1] is exactly zero.  The triangular matrix is 
                singular and its inverse can not be computed.  
     */
      private void dtrtri(char uplo, char diag, int n, double[][] A, int lda, int info[]) {
          boolean nounit;
          boolean upper;
          int j;
          int jb;
          int nb;
          int nn;
          int i;
          double arr[][];
          int k;
          String name;
          char charOpts[] = new char[2];
          String opts;
          double arr2[][];
          
          //Test the input parameters.
      
          info[0] = 0;
          upper = ((uplo == 'U') || (uplo == 'u'));
          nounit = ((diag == 'N') || (diag == 'n'));
          if(!upper && !(uplo == 'L') || (uplo == 'l')) {
              info[0] = -1;
          }
          else if (!nounit && !((diag == 'U') || (diag == 'u'))) {
              info[0] = -2;
          }
          else if (n < 0) {
              info[0] = -3;
          }
          else if (lda < Math.max(1, n)) {
              info[0] = -5;
          }
          if (info[0] != 0) {
              MipavUtil.displayError("dtrtri had info[0] = " + info[0]);
              return;
          }
          
      
          // Quick return if possible
      
          if (n == 0) {
              return;
          }
      
          // Check for singularity if non-unit.
      
          if (nounit) {
               for (info[0] = 1; info[0] <= n; info[0]++) {
                   if (A[info[0]-1][info[0]-1] == 0.0) {
                       return;
                   }
               } // for (info[0] = 1; info[0] <= n; info[0]++)
               info[0] = 0;
          } // if (nounit)
      
          // Determine the block size for this environment.
          name = new String("DTRTRI");
          charOpts[0] = uplo;
          charOpts[1] = diag;
          opts = new String(charOpts);
          nb = ge.ilaenv( 1, name, opts, n, -1, -1, -1);
          if (nb <= 1 || nb >= n) {
      
              // Use unblocked code
       
              dtrti2(uplo, diag, n, A, lda, info);
          }
          else {
      
              // Use blocked code
      
              if (upper) {
      
                  // Compute inverse of upper triangular matrix
      
                  for (j = 1; j <= n; j += nb) {
                      jb = Math.min(nb, n-j+1);
      
                      // Compute rows 0:j-2 of current block column
                      arr = new double[j-1][jb];
                      for (i = 0; i < j-1; i++) {
                          for (k = 0; k < jb; k++) {
                              arr[i][k] = A[i][j-1+k];
                          }
                      }
                      ge.dtrmm('L', 'U', 'N', diag, j-1,
                               jb, 1.0, A, lda, arr, lda);
                      for (i = 0; i < j-1; i++) {
                          for (k = 0; k < jb; k++) {
                              A[i][j-1+k] = arr[i][k];
                          }
                      }
                      arr2 = new double[jb][jb];
                      for (i = 0; i < jb; i++) {
                          for (k = 0; k < jb; k++) {
                              arr2[i][k] = A[j-1+i][j-1+k];
                          }
                      }
                      ge.dtrsm('R', 'U', 'N', diag, j-1,
                               jb, -1.0, arr2, lda, arr, lda);
                      for (i = 0; i < j-1; i++) {
                          for (k = 0; k < jb; k++) {
                              A[i][j-1+k] = arr[i][k];
                          }
                      }
                      for (i = 0; i < jb; i++) {
                          for (k = 0; k < jb; k++) {
                              arr2[i][k] = A[j-1+i][j-1+k];
                          }
                      }
      
                      //  Compute inverse of current diagonal block
      
                      dtrti2('U', diag, jb, arr2, lda, info);
                      for (i = 0; i < jb; i++) {
                          for (k = 0; k < jb; k++) {
                              A[j-1+i][j-1+k] = arr2[i][k];
                          }
                      }
                  } // for (j = 1; j <= n; j += nb)
              } // if (upper)
              else { // lower
      
                  // Compute inverse of lower triangular matrix
      
                  nn = ((n-1) / nb )*nb + 1;
                  for (j = nn; j >= 1; j -= nb) {
                      jb = Math.min(nb, n-j+1);
                      if (j+jb <= n) {
      
                          // Compute rows j+jb-1:n-1 of current block column
                          arr = new double[n-j-jb+1][n-j-jb+1];
                          for (i = 0; i < n-j-jb+1; i++) {
                              for (k = 0; k < n-j-jb+1; k++) {
                                  arr[i][k] = A[j+jb-1+i][j+jb-1+k];
                              }
                          }
                          arr2 = new double[n-j-jb+1][jb];
                          for (i = 0; i < n-j-jb+1; i++) {
                              for (k = 0; k < jb; k++) {
                                  arr2[i][k] = A[j+jb-1+i][j-1+k];
                              }
                          }
                          ge.dtrmm('L', 'L', 'N', diag,
                                    n-j-jb+1, jb, 1.0, arr, lda,
                                    arr2, lda);
                          for (i = 0; i < n-j-jb+1; i++) {
                              for (k = 0; k < jb; k++) {
                                  A[j+jb-1+i][j-1+k] = arr2[i][k];
                              }
                          }
                          arr = new double[jb][jb];
                          for (i = 0; i < jb; i++) {
                              for (k = 0; k < jb; k++) {
                                  arr[i][k] = A[j-1+i][j-1+k];
                              }
                          }
                          ge.dtrsm('R', 'L', 'N', diag,
                                   n-j-jb+1, jb, -1.0, arr, lda,
                                   arr2, lda);
                          for (i = 0; i < n-j-jb+1; i++) {
                              for (k = 0; k < jb; k++) {
                                  A[j+jb-1+i][j-1+k] = arr2[i][k];
                              }
                          }
                      } // if (j+jb <= n)
      
                     // Compute inverse of current diagonal block
                     arr = new double[jb][jb];
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             arr[i][k] = A[j-1+i][j-1+k];
                         }
                     }
                     dtrti2('L', diag, jb, arr, lda, info);
                     for (i = 0; i < jb; i++) {
                         for (k = 0; k < jb; k++) {
                             A[j-1+i][j-1+k] = arr[i][k];
                         }
                     }
                  } // for (j = nn; j >= 1; j -= nb)
              } // else lower
          } // else Use blocked code

          return;

      } // dtrtri
    
    /**
     * This is a port of LAPACK version routine 3.4.2 DTRTI2.F created by the University of Tennessee, University
     * of California Berkeley, University of Colorado Denver, and NAG Ltd., September 2012.
     *
     * dtrti2 computes the inverse of a triangular matrix (unblocked algorithm).
     * 
     * dtrti2 computes the inverse of a real upper or lower triangular matrix.
       @param input char uplo  Specifies whether the matrix A is upper or lower triangular.
           = 'U':  Upper triangular
           = 'L':  Lower triangular
       @param input char diag  Specifies whether or not the matrix A is unit triangular.
           = 'N':  Non-unit triangular
           = 'U':  Unit triangular
       @param input int n  The order of the matrix A.  n >= 0.
       @param (input/output) double[][] A of dimension (lda, n)
           On entry, the triangular matrix A.  If uplo = 'U', the
           leading n by n upper triangular part of the array A contains
           the upper triangular matrix, and the strictly lower
           triangular part of A is not referenced.  If uplo = 'L', the
           leading n by n lower triangular part of the array A contains
           the lower triangular matrix, and the strictly upper
           triangular part of A is not referenced.  If diag = 'U', the
           diagonal elements of A are also not referenced and are
           assumed to be 1.

           On exit, the (triangular) inverse of the original matrix, in
           the same storage format.
       @param inpuut int lda  The leading dimension of the array A.  lda >= max(1,n).
       @param output int[] info of dimension 1.
           = 0: successful exit
           < 0: if info[0] = -k, the k-th argument had an illegal value  
     */
      private void dtrti2(char uplo, char diag, int n, double[][] A, int lda, int info[]) {
          boolean nounit;
          boolean upper;
          int j;
          double ajj;
          double vec[];
          int i;
          double arr[][];
          int k;
          
          //Test the input parameters.
      
          info[0] = 0;
          upper = ((uplo == 'U') || (uplo == 'u'));
          nounit = ((diag == 'N') || (diag == 'n'));
          if(!upper && !(uplo == 'L') || (uplo == 'l')) {
              info[0] = -1;
          }
          else if (!nounit && !((diag == 'U') || (diag == 'u'))) {
              info[0] = -2;
          }
          else if (n < 0) {
              info[0] = -3;
          }
          else if (lda < Math.max(1, n)) {
              info[0] = -5;
          }
          if (info[0] != 0) {
              MipavUtil.displayError("dtrti2 had info[0] = " + info[0]);
              return;
          }
      
          if (upper) {
      
              // Compute inverse of upper triangular matrix.
      
              for (j = 1; j <= n; j++) {
                  if (nounit) {
                     A[j-1][j-1] = 1.0/ A[j-1][j-1];
                     ajj = -A[j-1][j-1];
                  }
                  else {
                     ajj = -1.0;
                  }
      
                  // Compute elements 0:j-2 of (j-1)-th column.
                  vec = new double[j-1];
                  for (i = 0; i < j-1; i++) {
                      vec[i] = A[i][j-1];
                  }
                  ge.dtrmv('U', 'N', diag, j-1, A, lda, vec, 1);
                  ge.dscal(j-1, ajj, vec, 1 );
                  for (i = 0; i < j-1; i++) {
                      A[i][j-1] = vec[i];
                  }
              } // for (j = 1; j <= n; j++)
          } // if (upper)
          else { // lower
      
              // Compute inverse of lower triangular matrix.
 
              for (j = n; j >= 1; j--) {
                  if (nounit) {
                      A[j-1][j-1] = 1.0/ A[j-1][j-1];
                      ajj = -A[j-1][j-1];
                   }
                   else {
                      ajj = -1.0;
                   }
                   if (j < n) {
      
                       // Compute elements j+1:n of j-th column.
                       arr = new double[n-j][n-j];
                       for (i = 0; i < n-j; i++) {
                           for (k = 0; k < n-j; k++) {
                               arr[i][k] = A[j+i][j+k];
                           }
                       }
                       vec = new double[n-j];
                       for (i = 0; i < n-j; i++) {
                           vec[i] = A[j+i][j-1];
                       }
                       ge.dtrmv('L', 'N', diag, n-j, arr, lda, vec, 1);
                       ge.dscal(n-j, ajj, vec, 1);
                       for (i = 0; i < n-j; i++) {
                           A[j+i][j-1] = vec[i];
                       }
                   } // if (j < n)
              } // for (j = n; j >= 1; j--)
          } // else lower
      
          return;

      } // dtrti2
      
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