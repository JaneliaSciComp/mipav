package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.*;

public class GeneralizedInverse {
    // This is a port of the FORTRAN suborutine GINV2, A Simple Algorithm for Computing the Generalized Inverse
    // of a Matrix by B. Rust, W. R. Burrus, and C. Schneeberger, CACM 9(5): 381-387 (May, 1966)
    private double A[][];
    
    // Number of rows in A
    private int NR;
    // Number of columns in A
    private int NC;
    
    public GeneralizedInverse() {
        
    }
    
    public GeneralizedInverse(double A1[][], int NR, int NC) {
        int i;
        int j;
        this.NR = NR;
        this.NC = NC;
        A = new double[NR][NC];
        for (i = 0; i < NR; i++) {
            for (j = 0; j < NC; j++) {
                A[i][j] = A1[i][j];
            }
        }
    }
    
    public double[][] ginv() {
        // Note there were 2 errors in file http://ftp.aset.psu.edu/pub/ger/fortran/hdk/ginv.for
        // Line 55   DO 60 I = I, JM1 should be 55    DO 60 I = 1, JM1
        // Line DO 65 K = I, JM1 should be DO 65 K = 1, JM1
        int i;
        double Ainv[][] = new double[NC][NR];
        double U[][] = new double[NC][NC];
        double aflag[] = new double[NC];
        double atemp[] = new double[NC];
        double fac;
        int n;
        double tol;
        int j;
        double dot1;
        int jm1;
        int L;
        int k;
        double dot2;
        
        for (i = 0; i < NC; i++) {
            U[i][i] = 1.0;
        }
        fac = dot(A, NR, 1, 1);
        fac = 1.0/Math.sqrt(fac);
        for (i = 0; i < NR; i++) {
            A[i][0] = A[i][0] * fac;
        }
        for (i = 0; i < NC; i++) {
            U[i][0] = U[i][0] * fac;
        }
        aflag[0] = 1.0;
        
        // Dependent column tolerance for n bit floating point fraction
        n = 27;
        tol = 10.0 * Math.pow(0.5, n);
        tol = tol * tol;
        for (j = 2; j <= NC; j++) {
            dot1 = dot(A, NR, j, j);
            jm1 = j - 1;
            for (L = 1; L <= 2; L++) {
                for (k = 1; k <= jm1; k++) {
                    atemp[k-1] = dot(A, NR, j, k);    
                } // for (k = 1; k <= jm1; k++)
                for (k = 1; k <= jm1; k++) {
                    for (i = 1; i <= NR; i++) {
                        A[i-1][j-1] = A[i-1][j-1] - atemp[k-1]*A[i-1][k-1]*aflag[k-1];    
                    } // for (i = 1; i <= NR; i++)
                    for (i = 1; i <= NC; i++) {
                        U[i-1][j-1] = U[i-1][j-1] - atemp[k-1]*U[i-1][k-1];
                    } // for (i = 1; i <= NC; i++)
                } // for (k = 1; k <= jm1; k++)
            } // for (L = 1; L <= 2; L++)
            dot2 = dot(A, NR, j, j);
            if (((dot2/dot1) - tol) <= 0.0) {
                for (i = 1; i <= jm1; i++) {
                    atemp[i-1] = 0.0;
                    for (k = 1; k <= i; k++) {
                        atemp[i - 1] = atemp[i - 1] + U[k-1][i-1]*U[k-1][j-1];
                    } // for (k = 1; k <= i; k++)
                } // for (i = 1; i <= jm1; i++)
                for (i = 1; i <= NR; i++) {
                    A[i-1][j-1] = 0.0;
                    for (k = 1; k <= jm1; k++) {
                        A[i-1][j-1] = A[i-1][j-1] - A[i-1][k-1]*atemp[k-1]*aflag[k-1];    
                    } // for (k = i; k <= jm1; k++)
                } // for (i = 1; i <= NR; i++)
                aflag[j-1] = 0.0;
                fac = dot(U, NC, j, j);
                fac = 1.0/Math.sqrt(fac);
            } // if (((dot2/dot1) - tol) <= 0.0)
            else { // ((dot2/dot1) - tol) > 0.0
                aflag[j-1] = 1.0;
                fac = 1.0/Math.sqrt(dot2);
            } // else ((dot2/dot1) - tol) > 0.0)
            for (i = 1; i <= NR; i++) {
                A[i-1][j-1] = A[i-1][j-1]*fac;
            } // for (i = 1; i <= NR; i++)
            for (i = 1; i <= NC; i++) {
                U[i-1][j-1] = U[i-1][j-1]*fac;
            } // for (i = 1; i <= NC; i++)
        } // for (j = 2; j <= NC; j++)
        for (j = 1; j <= NC; j++) {
            for (i = 1; i <= NR; i++) {
                fac = 0.0;
                for (k = j; k <= NC; k++) {
                    fac = fac + A[i-1][k-1]*U[j-1][k-1];
                } // for (k = j; k <= NC; k++)
                A[i-1][j-1] = fac;
            } // for (i = 1; i <= NR; i++)
        } // for (j = 1; j <= NC; j++)
        for (i = 1; i <= NC; i++) {
            for (j = 1; j <= NR; j++) {
                Ainv[i-1][j-1] = A[j-1][i-1];
            }
        }
        return Ainv;
      }
    
    // Computes the inner product of columns JC and KC
    private double dot(double A[][], int NR, int JC, int KC) {
        double dot = 0.0;
        int i;
        for (i = 0; i < NR; i++) {
            dot += A[i][JC-1]*A[i][KC-1];
        }
        return dot;
    }
    
    // This is a port of algorithm 645, a program for testing generalized inverse subroutines.
    // ALGORITHM 645 COLLECTED ALGORITHMS FROM ACM.
    // ALGORITHM APPEARED IN ACM-TRANS. MATH. SOFTWARE, VOL. 12, NO. 3, SEPT., 1986, P. 274
    // ORIGINAL CODE BY J. C. NASH 1979, J. C. NASH AND C. E. GRATTON 1982
    // J. C. NASH 1983, 1984
    // J. C. NASH AND R. L. C. WANG 1985, 1986
    public void ginvTest() {
        int m[] = new int[1];
        int n[] = new int[1];
        int ma;
        int na;
        int nb;
        int k[] = new int[1];
        int mopt;
        int iseed[] = new int[1];
        double alpha[] = new double[1];
        double Ag[][] = new double[30][30];
        double X[][] = new double[30][30];
        double C[][] = new double[30][30];
        double B[][] = new double[30][30];
        double G[][] = new double[30][30];
        double ta[] = new double[4];
        double tm[] = new double[4];
        boolean fail[] = new boolean[1];
        int i;
        int j;
        int test;
        int nTests = 22;
        String message[] = new String[]{"Testing invalid option A9+", "Testing iseed[0] < 0 in gmatx",
                "Testing m[0] < 1 in gmatx", "Testing alpha[0] < 0.0 in gmatx",
                "Testing k[0] < 0 in gmatx", "Testing k > min(m[0],n[0]) in gmatx",
                "Testing n[0] > m[0] in gmatx, ginvse not called",
                "Test no error data set which generates identity matrix",
                "Test 1 by 5 row vector", "Test 5 by 1 column vector",
                "Test 1 by 1 trivial case", "Test 5 by 10 matrix with power of 2 singular values",
                "Test 10 by 5 generated matrix with rank 2",
                "Test 5 by 10 generated matrix -- power of 0.1, rank 3",
                "Test Zielke A1", "Test Zielke A2", "Test Zielke A3",
                "Test Zielke / Rutishauser A4", "Test Zielke inverse A1",
                "Test Zielke inverse A2", "Test Zielke inverse A3",
                "Test Zielke / Rutishauser A4 inverse"};
        
        int mVar[] = new int[]{0,3,-1,4,5,5,3,4,1,5,1,10,10,5,5,5,5,5,5,5,5,5};
        int nVar[] = new int[]{0,3,1,4,5,3,4,4,5,1,1,5,5,10,5,5,5,5,5,5,5,5};
        int kVar[] = new int[]{0,2,1,4,-1,8,1,4,1,1,1,5,2,3,5,5,5,5,5,5,5,5};
        int moptVar[] = new int[]{-9,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,-1,-2,-3,-4};
        int iseedVar[] = new int[]{0,-1,34,56,98,1,0,453,12345,12345,12345,12345,54321,
                                   34521,0,0,0,0,0,0,0,0};
        double alphaVar[] = new double[]{2.0,0.2,0.1,-1.8,0.09,1.0,1.0,1.0,0.5,0.5,0.5,
                                         0.5,0.5,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1};
        // From Chapter 8 Gerneralized Inverses of Matrix Algebra Useful for Statistics by
        // Shayle R. Searle:
        //  Given any matrix A, there is a unique matrix M such that (1):
        //  (i) AMA = A
        //  (ii) MAM = M
        //  (iii) AM is symmetric
        //  (iv) MA is symmetric
        // The matrix M defined by the four Penrose conditions in (1) is unique for a given A.
        // But there are many matrices G which satisfy just the first Penrose condition:
        // AGA = A (5)
        // Nevertheless, they are of such importance in solving linear equations that we direct
        // most attention to those matrices G rather than to the Moore-Penorose inverse M.
        
        //   Any matrix G satisfying (5) is called a generalized inverse of A; any, by (5), when A
        // is p by q then G is q by p.  Although the name generalized inverse has not been adopted
        // universally, it is widely used.  Notice that G is "a" generalized inverse of A and not
        // "the" generalized inverse, because for any given A there are generally many matrices G
        // satisfying (5).  The exception is when A is nonsingular, in which case there is only one
        // G satisfying (5) and it is the regular inverse G = (A)-1 = M.  A useful alternative symbol
        // for G satisfying AGA = A is A with a superscript -.
        // Test ginvse routine that comes with the tester package.
        // Note.  ginvse is not designed to produce a 'good' generalized inverse.
        // It is meant only to furnish test data for routines gmatx, zielke, and ptst.
        // All 4 Penrose tests are failed.
        boolean doginvse = false;
        // All 4 Penrose tests passed
        boolean doginv = true;
        double tol = Math.pow(16.0, -5.0);
        
        // The following sequence of tests is used to test error traps in ginvse, ptst, and zielke
        // This section can be omitted withot affecting computations
        Preferences.debug("Test traps for calling sequence errors\n");
        Preferences.debug("Provoke 11 errors reports for dimensions and invalid arguments\n");
        
        // Test trap for matrix row dimension less than 1
        // Testing ma < 1
        ma = 0;
        na = 10;
        m[0] = 10;
        n[0] = 10;
        ptst(m, n, Ag, ma, X, na, C, ma, ta, tm, fail);
        // Ought to fail, stop if it has not failed
        if (!fail[0]) {
            return;
        }
        ginvse(G, ma, na, X, ma, m, n, tol, C, ma, fail);
        if (!fail[0]) {
            return;
        }
        
        // Test trap for matrix column dimension less than 1
        // Testing na < 1
        ma = 11;
        na = 0;
        m[0] = 11;
        n[0] = 11;
        ptst(m, n, Ag, ma, X, na, C, ma, ta, tm, fail);
        // Ought to fail, stop if it has not failed
        if (!fail[0]) {
            return;
        }
        ginvse(G, ma, na, X, ma, m, n, tol, C, ma, fail);
        if (!fail[0]) {
            return;
        }
        
        // Test trap for matrix row order less than 1
        // Testing m[0] < 1
        ma = 12;
        na = 12;
        m[0] = 0;
        n[0] = 12;
        ptst(m, n, Ag, ma, X, na, C, ma, ta, tm, fail);
       // Ought to fail, stop if it has not failed
        if (!fail[0]) {
            return;
        }
        ginvse(G, ma, na, X, ma, m, n, tol, C, ma, fail);
        if (!fail[0]) {
            return;
        }
        
        // Test trap for matrix column order less than 1
        // Testing n[0] < 1
        ma = 13;
        na = 13;
        m[0] = 13;
        n[0] = 0;
        ptst(m, n, Ag, ma, X, na, C, ma, ta, tm, fail);
       // Ought to fail, stop if it has not failed
        if (!fail[0]) {
            return;
        }
        ginvse(G, ma, na, X, ma, m, n, tol, C, ma, fail);
        if (!fail[0]) {
            return;
        }
        
        // Test trap for number of rows greater than dimension
        // Testing m[0] > ma
        ma = 5;
        na = 5;
        m[0] = 6;
        n[0] = 5;
        ginvse(G, ma, na, X, ma, m, n, tol, C, ma, fail);
        // Ought to fail, stop if it has not failed
        if (!fail[0]) {
            return;
        }
        
        // Test trap fornumber of columns greater than dimension
        // Testing n[0] > na
        ma = 5;
        na = 5;
        m[0] = 5;
        n[0] = 7;
        ginvse(G, ma, na, X, ma, m, n, tol, C, ma, fail);
        // Ought to fail, stop if it has not failed
        if (!fail[0]) {
            return;
        }
        
        // Testing ma and na forbidden by zielke
        ma = 3;
        na = 3;
        m[0] = 3;
        n[0] = 3;
        mopt = 2;
        zielke(m, n, Ag, ma, na, mopt, alpha, fail);
        // Ought to fail, stop if it has not failed
        if (!fail[0]) {
            return;
        }
        
        // End of sequence of calls to test error traps
        Preferences.debug("End of calling sequence for error tests\n");
        
        // The following tests are designed to ensure that the routine ptst is performing correctly.
        // First set up the matrix and a supposed inverse.
        m[0] = 3;
        n[0] = 2;
        Ag[0][0] = 1.0;
        Ag[0][1] = 2.0;
        Ag[1][0] = 2.0;
        Ag[1][1] = -3.0;
        Ag[2][0] = 0.5;
        Ag[2][1] = 0.0;
        
        // Supposed inverse
        X[0][0] = 0.1;
        X[0][1] = 0.2;
        X[0][2] = -0.2;
        X[1][0] = 0.3;
        X[1][1] = -0.3;
        X[1][2] = 0.9;
        ma = 30;
        na = 30;
        Preferences.debug("Test ptst with 3 by 2 matrices to verify matrix multiplications\n");
        Preferences.debug("Ag = " + "\n");
        for (i = 0; i < 3; i++) {
            for (j = 0; j < 2; j++) {
                Preferences.debug(Ag[i][j] + " ");    
            }
            Preferences.debug("\n");
        }
        Preferences.debug("X = " + "\n");
        for (i = 0; i < 2; i++) {
            for (j = 0; j < 3; j++) {
                Preferences.debug(X[i][j] + " ");    
            }
            Preferences.debug("\n");
        }
        ptst(m, n, Ag, ma, X, na, C, ma, ta, tm, fail);
        
        // Approximate results
        // Test PTST with 3 by 2 matrices to verify matrix multiplications
        //                 Ag                       X
        //              1.0   2.0           0.1   0.2   -0.2
        //              2.0  -3.0           0.3  -0.3    0.9
        //              0.5   0.0
        //  Test Penrose Conditions
        //  Ag is the input matrix, X is the inverse of Ag
        //  Input matrix norm     = 4.27200100E00
        //  Inverse matrix norm =   1.03923000E00
        //                       Average Deviation       Maximum Deviation
        // AgXAg=TEST=Ag ACTUAL        0.891666                 2.3
        //            NORMALIZED       0.2087232                0.5383886
        // XAgX=TEST=X ACTUAL          0.1875                   0.42
        //            NORMALIZED       0.1804217                0.4041449
        // (AgX)T=TEST=AgX ACTUAL      0.1683332                0.32
        //               NORMALIZED    0.3791636                0.7207864
        // (XAg)T=TEST=XAg ACTUAL      0.55                     0.55
        //               NORMALIZED    0.1238851                0.1238851
        
        // Reverse call to test row size < column size case
        Preferences.debug("Same test on ptst with Ag and X interchanged\n");
        ptst(n, m, X, na, Ag, ma, C, ma, ta, tm, fail);
        
        // Test row and column vector problems
        // Matrix (column vector) Ag
        Ag[0][0] = 3.0;
        Ag[1][0] = 4.0;
        
        // Matrix (row vector) X
        X[0][0] = 3.0/25.0;
        X[0][1] = 4.0/25.0;
        
        m[0] = 2;
        n[0] = 1;
        Preferences.debug("Test ptst with row and column vectors\n");
        Preferences.debug("Ag = " + "\n");
        Preferences.debug(Ag[0][0] + "\n");
        Preferences.debug(Ag[1][0] + "\n");
        Preferences.debug("X = " + "\n");
        Preferences.debug(X[0][0] + " " + X[0][1] + "\n");
        ptst(m, n, Ag, ma, X, na, C, ma, ta, tm, fail);
        
        // Reverse call to test row size < column size case
        Preferences.debug("Same vector test on ptst with Ag and X interchanged\n");
        ptst(n, m, X, na, Ag, ma, C, ma, ta, tm, fail);
        
        // Test trivial case (1 by 1)
        Ag[0][0] = 2.0;
        X[0][0] = 0.5;
        m[0] = 1;
        n[0] = 1;
        Preferences.debug("Test ptst with trivial 1 by 1 matrix Ag = 2.0 and matrix X = 0.5\n");
        ptst(n, m, X, na, Ag, ma, C, ma, ta, tm, fail);
        
        // Test main driver code for generalized inverse testing
        
        // ma is the maximum first dimension of working arrays
        // na is the maximum second dimension of working arrays
        // nb is max(ma, na) and the first and second dimension of the working array B.
        
        // Size of the input matrix should not exceed 30 by 30
        
        for (test = 0; test < nTests; test++) {
            // top of cycle
            ma = 30;
            na = 30;
            nb = 30;
            
            // m[0] and n[0] give the size of the matrix to be generated by gmatx
            // k[0] gives its rank
            // mopt = 0 if gmatx is to be called
            // mopt = 1, 2, 3, 4 for Zielke matrices
            //      = -1, -2, -3, -4 for their Moore-Penrose inverses
            // iseed[0] = an integer seed for use by the random number generator called by gmatx
            // alpha[0] = a parameter used to adjust the singular values of the matrix generated by gmatx
            
            // For details of the controls, see the comments in the routines gmatx and zielke
            Preferences.debug("Test number = " + (test + 1) + "\n");
            Preferences.debug(message[test] + "\n");
            // For each test set m, n, k, mopt, iseed, alpha
            m[0] = mVar[test];
            n[0] = nVar[test];
            k[0] = kVar[test];
            mopt = moptVar[test];
            iseed[0] = iseedVar[test];
            alpha[0] = alphaVar[test];
            
            // initialize error flag to imply correct execution
            fail[0] = false;
            
            Preferences.debug("m[0] = " + m[0] + "\n");
            Preferences.debug("n[0] = " + n[0] + "\n");
            Preferences.debug("k[0] = " + k[0] + "\n");
            Preferences.debug("mopt = " + mopt + "\n");
            Preferences.debug("iseed[0] = " + iseed[0] + "\n");
            Preferences.debug("alpha[0] = " + alpha[0] + "\n");
            
            // Other tests for valid inputs are made in the routines
            
            // Check for dimensions exceeded
            if (m[0] >= ma) {
                Preferences.debug("m[0] >= ma dimension exceeded input ignored\n");
                continue;
            }
            if (n[0] >= na) {
                Preferences.debug("n[0] >= na dimension exceeded input ignored\n");
                continue;    
            }
            
            if (mopt != 0) {
                // zielke routine call
                // Note that m[0] and n[0] are replaced by appropriate values.
                // mopt is the Zielke matrix selected
                // alpha is needed in zielke as a parameter in formulas for the matrix elements generated.
                zielke(m, n, Ag, ma, na, mopt, alpha, fail);
                if (fail[0]) {
                    continue;
                }
            } // if (mopt != 0)
            else { // mopt == 0
                // Using generated matrix
                // We suggest values of alpha to lie within 0.10 and 10.0, but other values of alpha
                // will be tolerated.  Note that alpha cannot be equal to zero, and if so will be set
                // to one in gmatx
                gmatx(m, n, Ag, ma, na, B, nb, alpha, k, iseed, fail);
                if (fail[0]) {
                    continue;
                }
            } // else mopt == 0
            
            // Add a different call for each generalized inverse routine tested
            
            if (doginv) {
                NR = m[0];
                NC = n[0];
                A = new double[NR][NC];
                for (i = 0; i < NR; i++) {
                    for (j = 0; j < NC; j++) {
                        A[i][j] = Ag[i][j];
                    }
                } 
                
                A = ginv();
                for (i = 0; i < NC; i++) {
                    for (j = 0; j < NR; j++) {
                        X[i][j] = A[i][j];
                    }
                } 
            } // if (doginv)
            
            if (doginvse) {
                // ginvse is not designed to handle more columns than rows 
                // that is, n[0] > m[0], so avoid call in such cases
                
                if (n[0] > m[0]) {
                    Preferences.debug("ginvse not called because n[0] = " + n[0] + " > m[0] = " + m[0] + "\n");
                    continue;
                }
                
                // ginvse is also unable to handle column vectors, that is an m[0] by 1 matrix Ag
                if (n[0] < 2) {
                    continue;
                }
                
                // Note additional statements to get conformity with test program
                // Copy matrix to avoid overwriting it
                for (i = 0; i < m[0]; i++) {
                    for (j = 0; j < n[0]; j++) {
                        G[i][j] = Ag[i][j];    
                    }
                } // for (i = 0; i < m[0]; i++)
                
                // IBM single precision
                tol = Math.pow(16.0, -5.0);
                ginvse(G, ma, na, X, ma, m, n, tol, C, ma, fail);
                
                if (fail[0]) {
                    continue;
                }
            } // if (doginvse)
            
            // Test penrose conditions
            ptst(m, n, Ag, ma, X, ma, C, ma, ta, tm, fail);
        } // for (test = 0; test < nTests; test++)
    } // ginvTest
    
    private void ptst(int m[], int n[], double A[][], int ma, double X[][], int nx, double C[][], 
                      int nc, double ta[], double tm[], boolean fail[]) {
        // This subroutine is designed to test a proposed generalized inverse of a matrix labelled A,
        // which is m by n.  X is the n by m matrix containing the supposed inverse.
        // Program originally for square, symmetric matrices written by Richard L. C. Wnag, 1977
        // Modified and adapted by J. C. Nash 1979, 1982
        // Note that A and X may be interchanged -- tester does not object.
        // Average and maximum absolute deviations are caclulated for the matrices
        // AXA - A         ... Penrose condition 1
        // XAX - X         ... Penrose condition 2
        // (AX)T - AX      ... Penrose condition 3
        // (XA)T - XA      ... Penrose condition 4
        // m[0] = number of rows in the 'original' matrix A
        //   = number of columns in purported inverse
        // unchanged by this routine
        // n[0] = number of columns in the 'original' matrix A
        //   = number of rows in purported inverse
        // unchanged by this routine
        // A = 'original' matrix of which a generalized inverse has supposedly been computed
        // unchanged by this program
        // ma = first or row dimension of A
        // unchanged by this program
        // X = purported generalized inverse of matrix A
        // unchanged by this routine
        // nx = first or row dimension of X
        // unchanged by this routine
        // C = double precision working array
        // should be dimensioned at least ma by ma
        // nc = first or row dimension of C
        // unchanged by this routine
        // should be at least as large as ma
        // ta = vector (1 dimensional array) of 4 elements to store average absolute
        // deviations from each of the four penrose conditions
        // tm = vector (1 dimensional array) of 4 elements to store maximum absolute
        // deviations from each of the four penrose conditions
        // fail = failure flag set true for failure
        // false otherwise
        
        // This routine uses double precision accumulation of inner products to limit rounding error.
        // reference Wilkinson, J. H., The Algebraic Eigenvalue Problem, Oxford, 1965
        // ptst calls double anorm(m, n, A, ma)
        // anorm computes the square norm of a matrix A
        // norm = SQRT(SUM(A[i][j]**2), for i = 1, m, j = 1, n)
        // This norm is used for simplicity.  Other norms are acceptable
        double s;
        double v;
        int i;
        int j;
        int j1;
        int L;
        int n1;
        int m1;
        double ama;
        double anx;
        double t1;
        double t2;
        
        // Dimensions must be at least one
        // No tests are made to see if dimensions are at least 1
        
        // Initialize failure flag to indicate a successful operation
        fail[0] = false;
        if (ma <= 0) {
            Preferences.debug("ptst failed because ma = " + ma + " is less than 1\n");
            System.out.println("ptst failed because ma = " + ma + " is less than 1");
            fail[0] = true;
            return;
        }
        if (n[0] <= 0) {
            Preferences.debug("ptst failed because n[0] = " + n[0] + " is less than 1\n");
            System.out.println("ptst failed because n[0] = " + n[0] + " is less than 1");
            fail[0] = true;
            return;    
        }
        if (nx <= 0) {
            Preferences.debug("ptst failed because nx = " + nx + " is less than 1\n");
            System.out.println("ptst failed because nx = " + nx + " is less than 1");
            fail[0] = true;
            return;
        }
        if (m [0]<= 0) {
            Preferences.debug("ptst failed because m[0] = " + m[0] + " is less than 1\n");
            System.out.println("ptst failed because m[0] = " + m[0] + " is less than 1");
            fail[0] = true;
            return;
        }
        if (nc <= 0) {
            Preferences.debug("ptst failed because nc = " + nc + " is less than 1\n");
            System.out.println("ptst failed because nc = " + nc + " is less than 1");
            fail[0] = true;
            return;
        }
        
        // Zero test values
        for (i = 0; i < 4; i++) {
            ta[i] = 0.0;
            tm[i] = 0.0;
        }
        
        // Compute AX
        for (i = 0; i < m[0]; i++) {
            for (j = 0; j < m[0]; j++) {
                s = 0.0;
                for (L = 0; L < n[0]; L++) {
                    s = s + A[i][L] * X[L][j];
                } // for (L = 0; L < n[0]; L++) 
                C[i][j] = s;
            } // for (j = 0; j < m[0]; j++)
        } // for (i = 0; i < m[0]; i++)
        
        // Compute AXA, AXA - A = (m[0] by n[0])
        for (i = 0; i < m[0]; i++) {
            for (j = 0; j < n[0]; j++) {
                s = 0.0;
                for (L = 0; L < m[0]; L++) {
                    s = s + C[i][L] * A[L][j];
                } // for (L = 0; L < m[0]; L++)
                t1 = Math.abs(A[i][j] - s);
                if (t1 > tm[0]) {
                    tm[0] = t1;
                }
                ta[0] = ta[0] + t1;
            } // for (j = 0; j < n[0]; j++)
        } // for (i = 0; i < m[0]; i++)
        ta[0] = ta[0]/(m[0] * n[0]);
        
        // Compute XAX, XAX - X = (n[0] by m[0])
        for (i = 0; i < n[0]; i++) {
            for (j = 0; j < m[0]; j++) {
                s = 0.0;
                for (L = 0; L < m[0]; L++) {
                    s = s + X[i][L]*C[L][j];    
                } // for (L = 0; L < m[0]; L++)
                t1 = Math.abs(X[i][j] - s);
                if (t1 > tm[1]) {
                    tm[1] = t1;
                }
                ta[1] = ta[1] + t1;
            } // for (j = 0; j < m[0]; j++)
        } // for (i = 0; i < n[0]; i++)
        ta[1] = ta[1]/(m[0] * n[0]);
        
        // Asymmetry of AX (m[0] by n[0])
        // Test for trivial case
        // Note that normalization unnecessary when m[0] = 1 since AX will be 1 by 1
        if (m[0] > 1) {
            m1 = m[0] - 1;
            for (i = 1; i <= m1; i++) {
                j1 = i + 1;
                for (j = j1; j <= m[0]; j++) {
                    t1 = Math.abs(C[i-1][j-1] - C[j-1][i-1]);
                    if (t1 > tm[2]) {
                        tm[2] = t1;
                    }
                    ta[2] = ta[2] + t1;
                } // for (j = j1; j <= m[0]; j++)
            } // for (i = 1; i <= m1; i++)
            ta[2] = ta[2]/(m[0]*(m[0]-1)/2);
        } // if (m[0] > 1)
        
        // Asymmetry of XA (n[0] by n[0])
        // Test for trivial case
        // Noate that normalization unnecessary when n[0] = 1 since XA will be 1 by 1
        if (n[0] > 1) {
            n1 = n[0] - 1;
            for (i = 1; i <= n1; i++) {
                j1 = i + 1;
                for (j = j1; j <= n[0]; j++) {
                    s = 0.0;
                    v = 0.0;
                    for (L = 1; L <= m[0]; L++) {
                        s = s + X[i-1][L-1]*A[L-1][j-1];
                        v = v + X[j-1][L-1]*A[L-1][i-1];
                    } // for (L = 1; L <= m[0]; L++)
                    t1 = Math.abs(s-v);
                    if (t1 > tm[3]) {
                        tm[3] = t1;
                    }
                    ta[3] = ta[3] + t1;
                } // for (j = j1; j <= n[0]; j++)
            } // for (i = 1; i <= n1; i++)
            ta[3] = ta[3]/(n[0]*(n[0]-1)/2);
        } // if (n > 1)
        
        Preferences.debug("Test of penrose conditions\n");
        Preferences.debug("A is the input matrix, X is the inverse of A\n");
        
        // Compute norms in order to gauge relative error sizes
        ama = anorm(m, n, A, ma);
        Preferences.debug("Input matrix norm = " + ama + "\n");
        anx = anorm(n, m, X, ma);
        Preferences.debug("Inverse matrix norm = " + anx + "\n");
        
        Preferences.debug("Average Deviation       Maximum Deviation\n");
        t1 = ta[0]/ama;
        Preferences.debug("AXA=TEST=A Average deviation actual = " + ta[0] + "\n");
        Preferences.debug("AXA=TEST=A Average deviation normalized = " + t1 + "\n");
        t2 = tm[0]/ama;
        Preferences.debug("AXA=TEST=A Maximum deviation actual = " + tm[0] + "\n");
        Preferences.debug("AXA=TEST=A Maximum deviation normalized = " + t2 + "\n");
        t1 = ta[1]/anx;
        Preferences.debug("XAX=TEST=X Average deviation actual = " + ta[1] + "\n");
        Preferences.debug("XAX=TEST=X Average deviation normalized = " + t1 + "\n");
        t2 = tm[1]/anx;
        Preferences.debug("XAX=TEST=X Maximum deviation actual = " + tm[1] + "\n");
        Preferences.debug("XAX=TEST=X Maximum deviation normalized = " + t2 + "\n");
        t1 = ta[2]/(ama*anx);
        Preferences.debug("(AX)T=TEST=AX Average deviation actual = " + ta[2] + "\n");
        Preferences.debug("(AX)T=TEST=AX Average deviation normalized = " + t1 + "\n");
        t2 = tm[2]/(ama*anx);
        Preferences.debug("(AX)T=TEST=AX Maximum deviation actual = " + tm[2] + "\n");
        Preferences.debug("(AX)T=TEST=AX Maximum deviation normalized = " + t2 + "\n");
        t1 = ta[3]/(ama*anx);
        Preferences.debug("(XA)T=TEST=XA Average deviation actual = " + ta[3] + "\n");
        Preferences.debug("(XA)T=TEST=XA Average deviation normalized = " + t1 + "\n");
        t2 = tm[3]/(ama*anx);
        Preferences.debug("(XA)T=TEST=XA Maximum deviation actual = " + tm[3] + "\n");
        Preferences.debug("(XA)T=TEST=XA Maximum deviation normalized = " + t2 + "\n");
        return;
    } // ptst
    
    private double anorm(int m[], int n[], double A[][], int ma) {
        // Compute square (euclidean) norm of matrix A
        // norm = SQRT(SUM(A[i][j]**2), for i = 1, m[0], j = 1, n[0])
        // This norm is used for simplicity - others acceptable
        // m[0] = number of rows in matrix A
        // n[0] = number of columns in matrix A
        // A = subject matrix which is m[0] by n[0]
        // ma = first dimension of A
        // None of these arguments are altered by this routine
        int i;
        int j;
        double dnorm;
        
        dnorm = 0.0;
        for (i = 0; i < m[0]; i++) {
            for (j = 0; j < n[0]; j++) {
                dnorm = dnorm + A[i][j]*A[i][j];
            } // for (j = 0; j < n[0]; j++)
        } // for (i = 0; i < m[0]; i++)
        dnorm = Math.sqrt(dnorm);
        return dnorm;
    } // anorm
    
    private void zielke(int m[], int n[], double A[][], int ma, int na, int mopt, double alpha[], boolean fail[]) {
        // Computes generalized inverse test matrices due to Zielke, Signum Newsletter, Vol. 13, #4, Dec. 78,
        // pages 10 - 12 and Zielke, G., Signum Newsletter, Vol. 16, #3, Sept. 81, pages 7-8.
        // Note corrections in Signum Newsletter, Vol. 16, #4, Dec. 81, page 6.
        // These matrices are labelled A1, A2, A3, A4 or their generalized inverses A1+, A2+, A3+, A4+.
        // m[0] = number of rows in matrix A produced
        // This is set (changed) by this subroutine
        // n[0] = number of columns in matrix A produces
        // This is set (changed) by this subroutine
        // A - The matrix which is generated
        // ma - The first or row dimension of A in the calling program
        // Unchanged by this routine
        // na - The second or column dimension of A in the calling program.=
        // Unchanged by this routine
        // mopt - An integer used to select the matrix to be generated
        // Unchanged by this routine
        // The following table determines the possible matrices which may be generated.
        // mopt = - 4 yields Zielke/RutiShauser A4+ matrix (The Moore-Penrose inverse of A4)
        // mopt = -3 yields Zielke A3+ matrix
        // mopt = -2 yields Zielke A2+ matrix
        // mopt = -1 yields Zielke A1+ matrix
        // mopt = 1 yields Zielke A1 matrix
        // mopt = 2 yields Zielke A2 matrix
        // mopt = 3 yields Zielke A3 matrix
        // mopt = 4 yields Zielke/RutiShauser A4 matrix
        // alpha = parameter used in generating the Zielke matrices
        // Unchanged by this routine
        // fail[0] = failure flag set true on failure.
        // false otherwise
        
        // The declared dimensions must be large enough to hold the resuling matrices.
        // The following table is useful:
        //                     mopt                min. ma            min. na
        //                       1                    5                  4
        //                       2                    5                  4
        //                       3                    5                  4
        //                       4                    7                  3
        //                      -1                    4                  5
        //                      -2                    4                  5
        //                      -3                    4                  5
        //                      -4                    3                  7
        int i;
        int j;
        int nopt;
        
        // Initially failure flag indicates successful operation
        fail[0] = false;
        
        // Use the value of mopt to determine which input matrix is desired
        nopt = mopt + 5;
        
        // Safety check
        if ((nopt < 1) || (nopt > 9) || (nopt == 5)) {
            System.out.println("Error mopt = " + mopt + " is not available for zielke");
            Preferences.debug("Error mopt = " + mopt + " is not available for zielke\n");
            fail[0] = true;
            return;
        } // if ((nopt < 1) || (nopt > 9) || (nopt == 5))
        switch (nopt) {
            case 1:
                // Compute Zielke Rutishauser matrix A4+
                if (ma < 3) {
                    System.out.println("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke\n");
                    fail[0] = true;
                    return;    
                }
                if (na < 7) {
                    System.out.println("Error mopt = " + mopt + " forbids na = " + na + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids na = " + na + " in zielke\n");
                    fail[0] = true;
                    return;      
                }
                A[0][0] = -(9.0*alpha[0]+63.0)/168.0;
                A[0][1] = -(6.0*alpha[0]-46.0)/168.0;
                A[0][2] = -(3.0*alpha[0]-29.0)/168.0;
                A[0][3] = -12.0/168.0;
                A[0][4] = (3.0*alpha[0]+5.0)/168.0;
                A[0][5] = (6.0*alpha[0]+22.0)/168.0;
                A[0][6] = (9.0*alpha[0]+39.0)/168.0;
                A[1][0] = -6.0/168.0;
                A[1][1] = -4.0/168.0;
                A[1][2] = -2.0/168.0;
                A[1][3] = 0.0;
                A[1][4] = 2.0/168.0;
                A[1][5] = 4.0/168.0;
                A[1][6] = 6.0/168.0;
                A[2][0] = (9.0*alpha[0]+51.0)/168.0;
                A[2][1] = (6.0*alpha[0]+38.0)/168.0;
                A[2][2] = (3.0*alpha[0]+25.0)/168.0;
                A[2][3] = 12.0/168.0;
                A[2][4] = -(3.0*alpha[0]+1.0)/168.0;
                A[2][5] = -(6.0*alpha[0]+14.0)/168.0;
                A[2][6] = -(9.0*alpha[0]+27.0)/168.0;
                m[0] = 3;
                n[0] = 7;
                Preferences.debug("Zielke/Rutishauser matrix A4+\n");
                break;
            case 2:
                // Compute Zielke matrix A3+
                if (ma < 4) {
                    System.out.println("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke\n");
                    fail[0] = true;
                    return;    
                }
                if (na < 5) {
                    System.out.println("Error mopt = " + mopt + " forbids na = " + na + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids na = " + na + " in zielke\n");
                    fail[0] = true;
                    return;      
                }
                A[0][0] = 0.5;
                A[0][1] = -0.125;
                A[0][2] = -1.0;
                A[0][3] = 0.875;
                A[0][4] = -0.625;
                A[0][5] = 0.375;
                A[1][0] = -1.0;
                A[1][1] = (2.0*alpha[0]+13.0)/8.0;
                A[1][2] = (-8.0*alpha[0]-28.0)/8.0;
                A[1][3] = (6.0*alpha[0]+17.0)/8.0;
                A[1][4] = (-2.0*alpha[0]-3.0)/8.0;
                A[1][5] = (2.0*alpha[0]+1.0)/8.0;
                A[2][0] = 1.25;
                A[2][1] = -A[1][1] + 0.25;
                A[2][2] = -A[1][2] - 1.25;
                A[2][3] = -A[1][3] + 1.0;
                A[2][4] = -A[1][4] - 0.5;
                A[2][5] = -A[1][5] + 0.25;
                A[3][0] = -0.25;
                A[3][1] = 0.375;
                A[3][2] = -0.25;
                A[3][3] = 0.125;
                A[3][4] = 0.125;
                A[3][5] = -0.125;
                A[4][0] = -0.5;
                A[4][1] = -0.25;
                A[4][2] = 1.5;
                A[4][3] = -1.25;
                A[4][4] = 0.75;
                A[4][5] = -0.25;
                m[0] = 5;
                n[0] = 6;
                Preferences.debug("Zielke matrix A3+\n");
                break;
            case 3:
                // Compute Zielke matrix A2+
                if (ma < 4) {
                    System.out.println("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke\n");
                    fail[0] = true;
                    return;    
                }
                if (na < 5) {
                    System.out.println("Error mopt = " + mopt + " forbids na = " + na + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids na = " + na + " in zielke\n");
                    fail[0] = true;
                    return;      
                }
                A[0][0] = (12.0*alpha[0]+44.0)/60.0;
                A[0][1] = 1.0/3.0;
                A[0][2] = (-12.0*alpha[0]-4.0)/60.0;
                A[0][3] = (-6.0*alpha[0]-27.0)/60.0;
                A[0][4] = (6.0*alpha[0]-3.0)/60.0;
                A[1][0] = -A[0][0] - 0.2;
                A[1][1] = -A[0][1];
                A[1][2] = -A[0][2] + 0.2;
                A[1][3] = -A[0][3] + 0.1;
                A[1][4] = -A[0][4] - 0.1;
                A[2][0] = A[1][0] + 11.0/15.0;
                A[2][1] = 0.0;
                A[2][2] = A[1][2] - 1.0/15.0;
                A[2][3] = A[1][3] - 0.7;
                A[2][4] = A[1][4] - 0.3;
                A[3][0] = alpha[0]/5.0;
                A[3][1] = 0.0;
                A[3][2] = -alpha[0]/5.0;
                A[3][3] = -A[2][3] + 0.1;
                A[3][4] = -A[2][4] - 0.1;
                m[0] = 4;
                n[0] = 5;
                Preferences.debug("Zielke matrix A2+\n");
                break;
            case 4:
                // Compute Zielke matrix A1+
                if (ma < 4) {
                    System.out.println("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke\n");
                    fail[0] = true;
                    return;    
                }
                if (na < 5) {
                    System.out.println("Error mopt = " + mopt + " forbids na = " + na + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids na = " + na + " in zielke\n");
                    fail[0] = true;
                    return;      
                }
                A[0][0] = alpha[0]/2.0;
                A[0][1] = 0.5;
                A[0][2] = alpha[0]/2.0;
                A[0][3] = 0.5;
                A[0][4] = -alpha[0];
                A[1][0] = 0.0;
                A[1][1] = -0.25;
                A[1][2] = 0.0;
                A[1][3] = -0.25;
                A[1][4] = 0.5;
                A[2][0] = -2.0*(alpha[0]+1.0)/4.0;
                A[2][1] = 0.0;
                A[2][2] = A[2][0];
                A[2][3] = 0.0;
                A[2][4] = alpha[0];
                A[3][0] = 0.0;
                A[3][1] = -0.25;
                A[3][2] = 0.0;
                A[3][3] = -0.25;
                A[3][4] = 0.5;
                m[0] = 4;
                n[0] = 5;
                Preferences.debug("Zielke matrix A1+\n");
                break;
            case 6:
                // Compute Zielke matrix A1
                if (ma < 5) {
                    System.out.println("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke\n");
                    fail[0] = true;
                    return;    
                }
                if (na < 4) {
                    System.out.println("Error mopt = " + mopt + " forbids na = " + na + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids na = " + na + " in zielke\n");
                    fail[0] = true;
                    return;      
                }
                A[0][0] = alpha[0];
                A[0][1] = alpha[0];
                A[0][2] = alpha[0] - 1.0;
                A[0][3] = alpha[0];
                A[1][0] = alpha[0] + 1.0;
                A[1][1] = alpha[0];
                A[1][2] = alpha[0];
                A[1][3] = alpha[0];
                A[2][0] = alpha[0];
                A[2][1] = alpha[0];
                A[2][2] = alpha[0] - 1.0;
                A[2][3] = alpha[0];
                A[3][0] = alpha[0] + 1.0;
                A[3][1] = alpha[0];
                A[3][2] = alpha[0];
                A[3][3] = alpha[0];
                A[4][0] = alpha[0] + 1.0;
                A[4][1] = alpha[0] + 1.0;
                A[4][2] = alpha[0];
                A[4][3] = alpha[0] + 1.0;
                m[0] = 5;
                n[0] = 4;
                Preferences.debug("Zielke matrix A1\n");
                break;
            case 7:
                // Compute Zielke matrix A2
                if (ma < 5) {
                    System.out.println("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke\n");
                    fail[0] = true;
                    return;    
                }
                if (na < 4) {
                    System.out.println("Error mopt = " + mopt + " forbids na = " + na + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids na = " + na + " in zielke\n");
                    fail[0] = true;
                    return;      
                }
                A[0][0] = alpha[0] + 1.0;
                A[0][1] = alpha[0];
                A[0][2] = alpha[0];
                A[0][3] = alpha[0] + 1.0;
                for (i = 0; i < 4; i++) {
                    A[1][i] = A[0][i] + 1.0;
                }
                for (i = 0; i < 4; i++) {
                    A[2][i] = A[1][i] + 1.0;
                }
                A[3][0] = alpha[0] + 1.0;
                A[3][1] = alpha[0] + 1.0;
                A[3][2] = alpha[0];
                A[3][3] = alpha[0] + 2.0;
                for (i = 0; i < 4; i++) {
                    A[4][i] = A[3][i] - 1.0;
                }
                m[0] = 5;
                n[0] = 4;
                Preferences.debug("Zielke matrix A2\n");
                break;
            case 8:
                // Compute Zielke matrix A3
                if (ma < 6) {
                    System.out.println("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke\n");
                    fail[0] = true;
                    return;    
                }
                if (na < 5) {
                    System.out.println("Error mopt = " + mopt + " forbids na = " + na + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids na = " + na + " in zielke\n");
                    fail[0] = true;
                    return;      
                }
                A[0][0] = alpha[0];
                A[0][1] = alpha[0] + 1.0;
                A[0][2] = alpha[0] + 2.0;
                A[0][3] = alpha[0] + 3.0;
                A[0][4] = alpha[0];
                A[1][0] = alpha[0];
                A[1][1] = alpha[0] + 2.0;
                A[1][2] = alpha[0] + 3.0;
                A[1][3] = alpha[0] + 5.0;
                A[1][4] = alpha[0] + 1.0;
                for (i = 1; i <= 4; i++) {
                    A[2][i-1] = alpha[0] + i;
                }
                A[2][4] = alpha[0] + 2.0;
                for (i = 0; i < 5; i++) {
                    A[3][i] = A[2][i] + 1.0;    
                }
                for (i = 0; i < 4; i++) {
                    A[4][i] = A[3][i] + 1.0;
                }
                A[4][4] = alpha[0] + 5.0;
                A[5][0] = alpha[0] + 5.0;
                A[5][1] = alpha[0] + 5.0;
                A[5][2] = alpha[0] + 6.0;
                A[5][3] = alpha[0] + 6.0;
                A[5][4] = alpha[0] + 7.0;
                m[0] = 6;
                n[0] = 5;
                Preferences.debug("Zielke matrix A3\n");
                break;
            case 9:
                // Compute Zielke/Rutishauser matrix A4
                if (ma < 7) {
                    System.out.println("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids ma = " + ma + " in zielke\n");
                    fail[0] = true;
                    return;    
                }
                if (na < 3) {
                    System.out.println("Error mopt = " + mopt + " forbids na = " + na + " in zielke");
                    Preferences.debug("Error mopt = " + mopt + " forbids na = " + na + " in zielke\n");
                    fail[0] = true;
                    return;      
                }
                for (i = 1; i <= 7; i++) {
                    for (j = 1; j <= 3; j++) {
                        A[i-1][j-1] = alpha[0] + i + j - 1.0;
                    }
                }
                m[0] = 7;
                n[0] = 3;
                Preferences.debug("Zielke/Rutishauser matrix A4\n");
                break;
        } // switch (nopt)
        Preferences.debug("A =\n");
        for (i = 0; i < m[0]; i++) {
            Preferences.debug("Row = " + i + " ");
            for (j = 0; j < n[0]; j ++) {
                Preferences.debug(A[i][j] + " ");
            }
            Preferences.debug("\n");
        }
        return;
    } // zielke
    
    private void gmatx(int m[], int n[], double A[][], int ma, int na, double B[][], int nb,
                       double alpha[], int k[], int iseed[], boolean fail[]) {
        // February 9, 1984, May 27, 1984, July 18, 1985
        // by J. C. Nash and R. L. C. Wang
        // Computes an m[0] by n[0] matrix - A - for use in testing programs purporting to calculate
        // the generalized inverse of a matrix
        
        // The matrix A, dimension ma by na but size m[0] by n[0], is to be calculated by a sequence
        // of pseudo-random Jacobi rotations applied to a 'diagonal' matrix whose elements are
        // determined by the parameters k and alpha as follows.  A[i-1][i-1] is set to 
        // alpha**(1 - i) for for i = 1,2,...,k and to zero for i > k, where k is a positive integer
        // no larger than the minimum value of m[0] and n[0]
        
        // m[0] = number of rows in matrix A
        // Normally unchanged by this routine
        // Altered during the execution of the routine if n[0] > m[0].
        // Then reset before return to the calling program.
        // m[0] is reset to ma if m[0] > ma on entry
        // n[0] = number of columns in matrix A
        // Normally unchanged by this routine
        // Altered during the execution of the routine if n[0] > m[0]
        // Then reset before return to the calling program.
        // No check is made to verfiy n[0] <= na
        // A = The matrix to be created in this routine
        // A must be declared with first dimension = ma and 
        // second dimension at least n[0] in the calling program.
        // ma = first or row dimension of A
        // ma should correspond to the first dimension of matrix A in the calling program.
        // Unchanged by this routine.
        // na = second or column dimension of matrix A
        // Should not be altered
        // B = working matrix in which test matrix is built
        // nb = first dimension of matrix B
        // Should be at least as large as largest dimension of matrix A
        // Not altered by this routine
        // alpha[0] = Factor used to generate singular values of matrix A.
        // These are generated according to the formula alpha**(1-i) for for = 1,2,...,k[0]
        // where k[0] is the rank (see below)
        // alpha[0] should lie in the interval (0.1, 10.0) to generate 'reasonable' singular
        // values, but other positive values are accepted without change.
        // If alpha[0] <= 0.0, it will be set to 1.0.
        // k[0] = the rank of the matrix to be generated
        // Must be positive and no larger than the minimum of m[0] and n[0].
        // If k[0] > min(m[0],n[0]), it will be set to min(m[0],n[0]) and a warning message
        // displayed.
        // Otherwise, k[0] is unaltered by this routine.
        // iseed[0] = seed for the pseudo-random number generator
        // Only positive values of iseed are allowed
        // If iseed[0] <= 1, it will be set to 1
        // iseed is converted to a double precision variable dseed for used in drand, the
        // generator routine.
        // dseed is altered in every call to drand, and dseed must be supplied in every
        // call to drand.
        // Reference: Schrage, L., A more protable fortran random number generator, 
        // ACM Transactions on Mathematical Software, Vol. 5, No. 2, June, 1979, pages 132-138.
        // fail[0] = error flag
        // fail[0] = true if gmatx has failed to create matrix A
        // fail[0] = false if gmatx has created matrix A
        // Most failures are associated with the use of incorrect arguments for this routine.
        
        double dseed;
        int i;
        int im;
        int imax;
        int j;
        int j1;
        int L;
        int mn;
        int m1;
        int ipass;
        int jpass;
        double c;
        double s;
        double t;
        
        // Set flag initially to false (program O.K.)
        fail[0] = false;
        if (iseed[0] <= 0) {
            System.out.println(iseed[0] + " was an improper choice for iseed[0]");
            Preferences.debug(iseed[0] + " was an improper choice for iseed[0]\n");
            iseed[0] = 1;
            System.out.println("iseed[0] was set to 1 in gmatx");
            Preferences.debug("iseed[0] was set to 1 in gmatx\n");
        } // if (iseed[0] <= 0)
        
        // Tests on m[0], n[0], and ma
        
        // Test for first dimension too small
        if (ma < 1) {
            System.out.println("ma = " + ma + " is incorrectly less than 1");
            Preferences.debug("ma = " + ma + " is incorrectly less than 1\n");
            fail[0] = true;
            return;
        }
        // Test for invalid row or column size
        if (m[0] < 1) {
            System.out.println("m[0] = " + m[0] + " is incorrectly less than 1");
            Preferences.debug("m[0] = " + m[0] + " is incorrectly less than 1\n");
            fail[0] = true;
            return;    
        }
        if (n[0] < 1) {
            System.out.println("n[0] = " + n[0] + " is incorrectly less than 1");
            Preferences.debug("n[0] = " + n[0] + " is incorrectly less than 1\n");
            fail[0] = true;
            return;       
        }
        // Test for valid row size
        if (m[0] > ma) {
            System.out.println("m[0] = " + m[0] + " exceeded allowable value of ma = " + ma);
            Preferences.debug("m[0] = " + m[0] + " exceeded allowable value of ma = " + ma + "\n");
            m[0] = ma;
            System.out.println("m[0] was set to ma = " + ma + " in gmatx");
            Preferences.debug("m[0] was set to ma = " + ma + " in gmatx\n");
        }
        
        Preferences.debug("Genrating matrix by pseudo-random Jacobi rotations\n");
        Preferences.debug("Order = " + m[0]+ " by " + n[0] + "\n");
        Preferences.debug("Pseudo-random number seed = " + iseed[0] + "\n");
        
        // Determine if A or A-transpose is to be generated by the Jacobi rotations
        // imax = 1 if A is generated
        // imax = 0 if A-transpose is generated
        imax = 1;
        mn = n[0];
        if (m[0] < n[0]) {
            imax = 0;
            im = m[0];
            m[0] = n[0];
            n[0] = im;
            mn = m[0];
        } // if (m[0] < n[0])
        
        for (i = 0; i < m[0]; i++) {
            for (j = 0; j < n[0]; j++) {
                B[i][j] = 0.0;
            }
        }
        
        // Replace (alpha[0] <= 0.0) by  1.0
        if (alpha[0] <= 0.0) {
            System.out.println("alpha[0] = " + alpha[0] + " is incorrectly <= 0.0");
            Preferences.debug("alpha[0] = " + alpha[0] + " is incorrectly <= 0.0\n");
            alpha[0] = 1.0;
            System.out.println("alpha[0] was set to 1.0 in gmatx");
            Preferences.debug("alpha[0] was set to 1.0 in gmatx\n");
        }
        
        // Test k[0] and loop, k[0] being the rank
        if (k[0] < 1) {
            System.out.println("k[0] = " + k[0] + " is an invalid value.  Terminating gmatx");
            Preferences.debug("k[0] = " + k[0] + " is an invalid value.  Terminating gmatx\n");
            fail[0] = true;
            return;
        }
        if (k[0] > mn) {
            System.out.println("k[0] = " + k[0] + " exceeds the allowable value for k[0]");
            Preferences.debug("k[0] = " + k[0] + " exceeds the allowable value for k[0]\n");
            k[0] = mn;
            System.out.println("k[0] has been set equal to mn = " + mn);
            Preferences.debug("k[0] has been set equal to mn = " + mn + "\n");
        }
        for (i = 1; i <= k[0]; i++) {
            B[i-1][i-1] = Math.pow(alpha[0], 1-i);
        }
        Preferences.debug("The initial diagonal elements for i = 1 to " + k[0] + " are:\n");
        for (i = 0; i < k[0]; i++) {
            Preferences.debug(B[i][i] + "\n");
        }
        m1 = m[0] - 1;
        
        // Set random number generator seed
        dseed = (double)iseed[0];
        
        // Perform Jacobi rotations (one sweep only)
        jpass = 1;
        
        // More sweeps can be performed as follows
        // jpass = 3;
        // For three sweeps
        for (ipass = 1; ipass <= jpass; ipass++) {
            // Avoid loop when trivial 1 by 1 matrix is to be generated
            if (m1 < 1) {
                continue;
            }
            for (i = 1; i <= m1; i++) {
                j1 = i + 1;
                for (j = j1; j <= m[0]; j++) {
                    // Get sine of angle of rotation from pseudo random number generator
                    s = drand(dseed);
                    // Compute cosine
                    c = Math.sqrt(1.0-s*s);
                    // rows
                    for (L = 1; L <= n[0]; L++) {
                        t = B[i-1][L-1];
                        B[i-1][L-1] = t*c + B[j-1][L-1]*s;
                        B[j-1][L-1] = -t*s + B[j-1][L-1]*c;
                    } // for (L = 1; L <= n[0]; L++)
                    
                    // Column transformations - these are omitted if the indices are too large
                    // (that is, if j > n[0])
                    // Since max(i) = j - 1, there is no test on i
                    if (j > n[0]) {
                        continue;
                    }
                    
                    for (L = 1; L <= m[0]; L++) {
                        t = B[L-1][i-1];
                        B[L-1][i-1] = t*c + B[L-1][j-1]*s;
                        B[L-1][j-1] = -t*s + B[L-1][j-1]*c;
                    } // for (L = 1; L <= m[0]; L++)
                } // for (j = j1; j <= m[0]; j++)
            } // for (i = 1; i <= m1; i++)
        } // for (ipass = 1; ipass <= jpass; ipass++)
        
        // Reset row and column sizes
        if (imax != 1) {
            im = m[0];
            m[0] = n[0];
            n[0] = im;
        } // if (imax != 1)
        
        // Move B or B-transpose to A
        for (i = 0; i < m[0]; i++) {
            for (j = 0; j < n[0]; j++) {
                if (imax == 0) {
                    A[i][j] = B[j][i];
                }
                if (imax == 1) {
                    A[i][j] = B[i][j];
                }
            } // for (j = 0; j < n[0]; j++)
        } // for (i = 0; i < m[0]; i++)
        
        // Output the matrix which has been generated
        Preferences.debug("Matrix A generated by gmatx:\n");
        for (i = 0; i < m[0]; i++) {
            Preferences.debug("Row " + i + " ");
            for (j = 0; j < n[0]; j++) {
                Preferences.debug(A[i][j] + " ");
            }
            Preferences.debug("\n");
        } // for (i = 0; i < m[0]; i++)
        return;
    } // gmatx
    
    private double drand(double dx) {
        // Portable random number generator using the recursion:
        // dx = dx * A mod P
        // From Linus Shrage, ACM Transactions on Mathematical Software, Vol. 5, No. 2, p.134 FF,
        // June, 1979
        // dx = seed for next member of pseudo-random sequence
        // dx should not be altered between calls to drand
        // dx is altered on each call to drand
        // ouput dpprn = double precision pseudo random number in the unit interval which is produced
        // by this routine
        
        double a = 16807.0;
        // 2**31 - 1
        double p = 2147483647.0;
        // 2**15
        double b15 = 32768.0;
        // 2**16
        double b16 = 65536.0;
        double xhi;
        double xalo;
        double leftlo;
        double fhi;
        double k;
        double dpprn;
        
        // Get 15 high order bits of dx
        xhi = dx/b16;
        xhi = Math.floor(xhi);
        
        // Get 16 lo bits of dx and form lo product
        xalo = (dx - xhi*b16)*a;
        
        // Get 15 hi order bits of lo product
        leftlo = xalo/b16;
        leftlo = Math.floor(leftlo);
        
        // Form the 31 highest bits of full product
        fhi = xhi*a + leftlo;
        
        // Get overflow past 31st bit of full product
        k = fhi/b15;
        k = Math.floor(k);
        
        // Assemble all the parts and presubtract p
        // The parentheses are essential
        dx = (((xalo-leftlo*b16)-p)+(fhi-k*b15)*b16) + k;
        
        // Add p back in if necessary
        if (dx < 0.0) {
            dx = dx + p;
        }
        
        // Multiply by 1/(2**31-1)
        dpprn = dx * 4.656612875E-10;
        return dpprn;
    } // drand
    
    private void ginvse(double A[][], int ma, int na, double AIN[][], int nain, int m[], int n[],
                        double tol, double V[][], int nv, boolean fail[]) {
        // Inverse of A = V * inverse of S * transpose of U
        // via singular value decomposition using algorithms 1 and 2 of
        // Nash, J. C., Compact Numerical Methods for Computers, 1979,
        // Adam Hilger, Bristol or Halsted Press, N.Y.
        
        // A = Matrix of size m[0] by n[0] to be 'inverted'
        // A is destroyed by this routine and becomes the 
        // U matrix of the singular value decomposition
        // ma = row dimension of array A
        // unchanged by this routine
        // na = column dimension of array A
        // unchanged by this routine
        // AIN = computed 'inverse' (n[0] rows by m[0] cols.)
        // nain = row dimension of array AIN
        //        (Column dimension assumed to be at least m[0])
        // unchanged by this routine
        // m[0] = number of rows in matrix A and the number of columns in matrix AIN
        // n[0] = number of columns in matrix A the number of rows in matrix AIN
        // unchanged by this routine
        // tol = machine precision for computing environment
        // unchanged by this routine
        // V = work matrix which becomes the V matrix (n[0] by n[0])
        // of the singular value decomposition
        // nv = dimensions of V (Both rows and columns)
        // unchanged by this routine
        // fail[0] = error flag - true implies failure of ginvse
        
        // Note.  This routine is not designed to produce a 'good' generalized inverse.
        // It is meant only to furnish test data for routines gmatx, zielke, and ptst.
        
        int icount;
        int nm1;
        int i;
        int j;
        int jp1;
        int k;
        int sweep;
        int limit;
        double p;
        double q;
        double r;
        double vv = 0.0;
        double c;
        double s;
        double temp;
        
        // Initially set fail[0] = false to imply correct execution
        fail[0] = false;
        
        // Tests on validity of dimensions
        if (m[0] < 2) {
            System.out.println("ginvse failed because m[0] = " + m[0] + " is less than 2");
            Preferences.debug("ginvse failed because m[0] = " + m[0] + " is less than 2\n");
            fail[0] = true;
            return;
        }
        if (ma < 2) {
            System.out.println("ginvse failed because ma = " + ma + " is less than 2");
            Preferences.debug("ginvse failed because ma = " + ma + " is less than 2\n");
            fail[0] = true;
            return;        
        }
        if (n[0] < 2) {
            System.out.println("ginvse failed because n[0] = " + n[0] + " is less than 2");
            Preferences.debug("ginvse failed because n[0] = " + n[0] + " is less than 2\n");
            fail[0] = true;
            return;    
        }
        if (na < 2) {
            System.out.println("ginvse failed because na = " + na + " is less than 2");
            Preferences.debug("ginvse failed because na = " + na + " is less than 2\n");
            fail[0] = true;
            return;        
        }
        if (n[0] > na) {
            System.out.println("ginvse failed because n[0] = " + n[0] + " exceeds na = " + na);
            Preferences.debug("ginvse failed because n[0] = " + n[0] + " exceeds na = " + na + "\n");
            fail[0] = true;
            return;        
        }
        if (m[0] > ma) {
            System.out.println("ginvse failed because m[0] = " + m[0] + " exceeds ma = " + ma);
            Preferences.debug("ginvse failed because m[0] = " + m[0] + " exceeds ma = " + ma + "\n");
            fail[0] = true;
            return;        
        }
        
        // n[0] must not exceed m[0], otherwise ginvse will fail
        if (n[0] > m[0]) {
            System.out.println("ginvse failed because n[0] = " + n[0] + " exceeds m[0] = " + m[0]);
            Preferences.debug("ginvse failed because n[0] = " + n[0] + " exceeds m[0] = " + m[0] + "\n");
            fail[0] = true;
            return;            
        }
        
        // sweep counter initialized to zero
        sweep = 0;
        // set sweep limit
        // max(n[0], 6) was chosen from experience
        limit = Math.max(n[0], 6);
        
        // V[i][j] initially n by n identity
        for (i = 0; i < n[0]; i++) {
            for (j = 0; j < n[0]; j++) {
                V[i][j] = 0.0;
            }
            V[i][i] = 1.0;
        }
        
        do {
            // Initialize rotation counter (counts down to 0)
            icount = n[0]*(n[0] - 1)/2;
            
            // count sweep
            sweep = sweep + 1;
            nm1 = n[0] - 1;
            for (j = 1; j <= nm1; j++) {
                jp1 = j + 1;
                for (k = jp1; k <= n[0]; k++) {
                    p = 0.0;
                    q = 0.0;
                    r = 0.0;
                    for (i = 1; i <= m[0]; i++) {
                        // Test for and avoid underflow
                        // Not needed for machines which underflow to zero without error message
                        if (Math.abs(A[i-1][j-1]) >= tol) {
                            q = q + A[i-1][j-1]*A[i-1][j-1];
                        }
                        if (Math.abs(A[i-1][k-1]) >= tol) {
                            r = r + A[i-1][k-1]*A[i-1][k-1];    
                        }
                        if (Math.abs(A[i-1][j-1]/tol)*Math.abs(A[i-1][k-1]/tol) >= 1.0) {
                            p = p + A[i-1][j-1]*A[i-1][k-1];
                        }
                    } // for (i = 1; i <= m[0]; i++)
                    if (q < r) {
                        c = 0.0;
                        s = 1.0;
                    } // if (q < r)
                    else { // q >= r
                        if ((Math.abs(q) < tol) && (Math.abs(r) < tol)) {
                            icount = icount - 1;
                            continue;
                        }
                        if (r == 0.0) {
                            icount = icount - 1;
                            continue;
                        }
                        if ((p/q)*(p/r) < tol) {
                            icount = icount - 1;
                            continue;
                        }
                        
                        // Calculate the sine and cosine of the angle of rotation
                        q = q - r;
                        vv = Math.sqrt(4.0*p*p + q*q);
                        c = Math.sqrt((vv+q)/(2.0*vv));
                        s = p/(vv*c);
                    } // else q >= r
                    
                    // Apply the rotation to A
                    for (i = 1; i <= m[0]; i++) {
                        r = A[i-1][j-1];
                        A[i-1][j-1] = r*c + A[i-1][k-1]*s;
                        A[i-1][k-1] = -r*s + A[i-1][k-1]*c;
                    } // for (i = 1; i <= m[0]; i++)
                    
                    // Apply the rotation to V
                    for (i = 1; i <= n[0]; i++) {
                        r = V[i-1][j-1];
                        V[i-1][j-1] = r*c + V[i-1][k-1]*s;
                        V[i-1][k-1] = -r*s + V[i-1][k-1]*c;
                    } // for (i = 1; i <= n[0]; i++)
                } // for (k = jp1; k <= n[0]; k++)
            } // for (j = 1; j <= nm1; j++)
            
            // Output the number of sweeps and rotations
            Preferences.debug("Sweep = " + sweep + "\n");
            Preferences.debug("Jacobi rotations performed = " + icount + "\n");
            
            // Check number of sweeps and rotations (termination test)
        } while ((icount > 0) && (sweep < limit));
        
        if (sweep >= limit) {
            Preferences.debug("Sweep limit reached\n");
        }
        
        for (j = 1; j <= n[0]; j++) {
            q = 0.0;
            for (i = 1; i <= m[0]; i++) {
                q = q + A[i-1][j-1]*A[i-1][j-1];
            }
            
            // Arbitrary rank decision
            if (j == 1) {
                vv = 1.0E-3*Math.sqrt(q);
            }
            if (Math.sqrt(q) > vv) {
                for (i = 1; i <= m[0]; i++) {
                    A[i-1][j-1] = A[i-1][j-1]/q;
                }
            } // if (Math.sqrt(q) > vv)
            else { // Math.sqrt(q) <= vv
                for (i = 1; i <= m[0]; i++) {
                    A[i-1][j-1] = 0.0;
                }
            } // else Math.sqrt(q) <= vv
        } // for (j = 1; j <= n[0]; j++)
        
        for (i = 0; i < n[0]; i++) {
            for (j = 0; j < m[0]; j++) {
                temp = 0.0;
                for (k = 0; k < n[0]; k++) {
                    temp = temp + V[i][k]*A[j][k];
                } // for (k = 0; k < n[0]; k++)
                AIN[i][j] = temp;
            } // for (j = 0; j < m[0]; j++)
        } // for (i = 0; i < n[0]; i++)
        return;
    } // ginvse
}