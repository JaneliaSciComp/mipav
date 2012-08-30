package gov.nih.mipav.model.structures.jama;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class SelectedEigenvalue implements java.io.Serializable {
    
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
    public SelectedEigenvalue() {}
    
    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    
    /** Common variables in testing routines. */
    private ViewUserInterface UI = ViewUserInterface.getReference();
    
    /**
     * This routine is an extraction from the FORTRAN program version 3.1.1 DCHKEE of the code needed to drive ddrvst,
     * that tests symmetric generalized eigenvalue drivers. The driver tested is dsyevx. Numerical values were obtained
     * from the sep.in datafile. Original DCHKEE created by Univ. of Tennessee, Univ. of California Berkeley, and NAG
     * Ltd., January, 2007
     */
    public void ddrvst_test() {

        // Number of values of n
        final int nn = 6;

        // Values of matrix dimension n
        final int[] nval = new int[] {0, 1, 2, 3, 5, 20};

        // Number of values of NB, NBMIN, and NX
        final int nparms = 5;

        // Values of blocksize NB
        final int[] nbval = new int[] {1, 3, 3, 3, 10};

        // Values of minimum blocksize NBMIN
        final int[] nbmin = new int[] {2, 2, 2, 2, 2};

        // Values of crossover point NX
        final int[] nxval = new int[] {1, 0, 5, 9, 1};

        // Threshold value for the test ratios. Information will be printed
        // about each test for which the test ratio is greater than or equal
        // to threshold.
        final double thresh = 50.0;

        // Test the driver routines
        final boolean tstdrv = true;

        // Code describing how to set the random number seed.
        // = 0: Set the seed to a default number before each run.
        // = 1: Initialize the seed to a default value only before the first
        // run.
        // = 2: Like 1, but use the seed values in the 4 integer array
        // ioldsd
        int newsd = 1;
        final int maxt = 30;
        final boolean[] dotype = new boolean[maxt];
        final int[] ioldsd = new int[] {0, 0, 0, 1};
        final int[] iseed = new int[] {0, 0, 0, 1};
        final int nmax = 132;
        final int lwork = (nmax * ( (5 * nmax) + 5)) + 1;
        final int liwork = nmax * ( (5 * nmax) + 20);
        final int[] iwork = new int[liwork];
        final double[] work = new double[lwork];
        final double[] result = new double[500];
        final int[] info = new int[1];
        double[][] A;
        double[] D1;
        double[] D2;
        double[] D3;
        double[] D4;
        double[] eveigs;
        double[] WA1;
        double[] WA2;
        double[] WA3;
        double[][] U;
        double[][] V;
        double[] tau;
        double[][] Z;

        final int maxtyp = 21;
        int i;
        int k;
        int maxnval;
        int[] iparms;
        UI.setDataText("Tests of the Symmetric Eigenvalue Problem routines\n");

        for (i = 0; i < maxtyp; i++) {
            dotype[i] = true;
        }

        maxnval = 0;

        for (i = 0; i < nn; i++) {

            if (nval[i] > maxnval) {
                maxnval = nval[i];
            }
        }

        iparms = new int[100];

        // 9 = maximum size of the subproblems at the bottom of the computation
        // tree in the divide-and-conquer algorithm (used by xgelsd and xgesdd)
        iparms[9 - 1] = 25;
        A = new double[nmax][maxnval];
        D1 = new double[maxnval];
        D2 = new double[maxnval];
        D3 = new double[maxnval];
        D4 = new double[maxnval];
        eveigs = new double[maxnval];
        WA1 = new double[maxnval];
        WA2 = new double[maxnval];
        WA3 = new double[maxnval];
        U = new double[nmax][maxnval];
        V = new double[nmax][maxnval];
        tau = new double[maxnval];
        Z = new double[nmax][maxnval];

        for (i = 1; i <= nparms; i++) {

            // 1 = The optimal blocksize; if this value is 1, an unblocked
            // algorithm will give the best performance
            iparms[1 - 1] = nbval[i - 1];

            // 2 = The minimum blocksize for which the block routine should be
            // used; if the usable block size is less than this value, an
            // unblocked routine should be used.
            iparms[2 - 1] = nbmin[i - 1];

            // 3 = The crossover point (in a block routine, for n less than this
            // value, an unblocked routine should be used).
            iparms[3 - 1] = nxval[i - 1];

            if (newsd == 0) {

                for (k = 0; k < 4; k++) {
                    iseed[k] = ioldsd[k];
                }
            } // if (newsd == 0)

            UI.setDataText("Optimal blocksize = " + nbval[i - 1] + "\n");
            UI.setDataText("Minimum blocksize = " + nbmin[i - 1] + "\n");
            UI.setDataText("Crossover point = " + nxval[i - 1] + "\n");

            if (tstdrv) {
                ddrvst(nn, nval, 18, dotype, iseed, thresh, A, nmax, D1, D2, D3, D4, eveigs, WA1, WA2, WA3, U, nmax, V,
                        tau, Z, work, lwork, iwork, liwork, result, info);

                if (info[0] != 0) {
                    MipavUtil.displayError("ddrvst had info = " + info[0]);
                }
            } // if (tstchk)
        } // for (i = 1; i <= nparms; i++)
    } // ddrvst_test
    
    /**
     * This is a port of the part of version 3.4.0 LAPACK test routine DDRVST used to test dsyevx. Original DDRVST created
     * by Univ. of Tennessee, Univ. of California Berkeley, University of Colorado Denver, and NAG Ltd., November, 2011.
     * ddrvst checks the symmetric eigenvalue problem driver dsyevx. dsyevx computes selected eigenvalues and, optionally,
     * eigenvectors of a real symmetric matrix.
     * 
     * <p>
     * When ddrvst is called, a number of matrix "sizes" ("n's") and a number of matrix "types" are specified. For each
     * size ("n") and each type of matrix, one matrix will be generated and used to test the dsyevx driver. For each
     * matrix, the following tests will be performed: (1) | A - Z D Z' | / ( |A| n ulp ) (2) | I - Z Z' | / ( n ulp )
     * (3) | D1 - D2 | / ( |D1| ulp ) where Z is the matrix of eigenvectors returned when the eigenvector option is
     * given and D1 and D2 are the eigenvalues returned with and without the eigenvector option.
     * </p>
     * 
     * <p>
     * The "sizes" are specified by an array nn(0:nsizes-1); the value of each element nn[j] specifies one size. The
     * "types" are specified by a boolean array dotype(0:ntypes-1); if dotype[j] is true, then matrix type "j" will be
     * generated. Currently, the list of possible types is: (1) The zero matrix. (2) The identity matrix. (3) A diagonal
     * matrix with evenly spaced eigenvalues 1, ..., ulp and random signs. (ulp = (first number larger than 1) - 1) (4)
     * A diagonal matrix with geometrically spaced eigenvalues 1, ..., ulp and random signs. (5) A diagonal matrix with
     * "clustered" eigenvalues 1, ulp, ..., ulp and random signs. (6) Same as (4), but multiplied by sqrt(overflow
     * threshold) (7) Same as (4), but multiplied by sqrt(underflow threshold) (8) A matrix of the form U' D U, where U
     * is orthogonal and D has evenly spaced entries 1, ..., ulp with random signs on the diagonal. (9) A matrix of the
     * form U' D U, where U is orthogonal and D has geometrically spaced entries 1, ..., ulp with random signs on the
     * diagonal. (10) A matrix of the form U' D U, where U is orthogonal and D has "clustered" entries 1, ulp, ..., ulp
     * with random signs on the diagonal. (11) Same as (8), but multiplied by sqrt( overflow threshold) (12) Same as
     * (8), but multiplied by sqrt( underflow threshold) (13) Symmetric matrix with random entries chosen from (-1,1).
     * (14) Same as (13), but multiplied by sqrt( overflow threshold) (15) Same as (13), but multiplied by
     * sqrt(underflow threshold) (16) A band matrix with half bandwidth randomly chosen between 0 and n-1, with evenly
     * spaced eigenvalues 1, ..., ulp with random signs. (17) Same as (16), but multiplied by sqrt(overflow threshold)
     * (18) Same as (16), but multiplied by sqrt(underflow threshold)
     * </p>
     * 
     * <p>
     * The tests performed are: 
     * (1) | A - U S U' | / ( |A| n ulp ) dsyevx(jobz = 'V', range = 'A' uplo = 'L',... )
     * (2) | I - U U' | / ( n ulp ) dsyevx(jobz = 'V', range = 'A' uplo = 'L',... ) 
     * (3) |D(with Z) - D(w/o Z)| / (|D| ulp) dsyevx(jobz = 'N', range = 'A' uplo = 'L',... )
     * (4) | A - U S U' | / ( |A| n ulp ) dsyevx(jobz = 'V', range = 'I' uplo = 'L',... )
     * (5) | I - U U' | / ( n ulp ) dsyevx(jobz = 'V', range = 'I' uplo = 'L',... ) 
     * (6) |D(with Z) - D(w/o Z)| / (|D| ulp) dsyevx(jobz = 'N', range = 'I' uplo = 'L',... )
     * (7) | A - U S U' | / ( |A| n ulp ) dsyevx(jobz = 'V', range = 'V' uplo = 'L',... )
     * (8) | I - U U' | / ( n ulp ) dsyevx(jobz = 'V', range = 'V' uplo = 'L',... ) 
     * (9) |D(with Z) - D(w/o Z)| / (|D| ulp) dsyevx(jobz = 'N', range = 'V' uplo = 'L',... )
     *  Tests 1 through 9 are repeated with uplo = 'U'
     * </p>
     * 
     * @param nsizes (input) int The number of sizes of matrices to use. If it is zero, ddrvst does nothing. It must be
     *            at least zero.
     * @param nn (input) int[] of dimension (nsizes) An array containing the sizes to be used for the matrices. Zero
     *            values will be skipped. The values must be at least zero.
     * @param ntypes (input) int The number of elements in dotype. If it is zero, ddrvst does nothing. It must be at
     *            least zero. If it is maxtyp+1 and nsizes is 1, then an additional type, maxtyp+1 is defined, which is
     *            to use whatever matrix is in A. This is only useful if dotype(0:maxtyp-1) is false and dotype[maxtyp]
     *            is true.
     * @param dotype (input) boolean[] of dimension (ntypes) If dotype[j] is true, then for each size in nn a matrix of
     *            that size and of type j will be generated. If ntypes is smaller than the maximum number of types
     *            defined (parameter maxtyp), then types ntypes+1 through maxtyp will not be generated. If ntypes is
     *            larger than maxtyp, dotype[maxtyp] through dotype[ntypes-1] will be ignored.
     * @param iseed (input/output) int[] of dimension (4) On entry iseed specifies the seed of the random number
     *            generator. The array elements should be between 0 and 4095; if not they will be reduced mod 4096.
     *            Also, iseed[3] must be odd. The random number generator uses a linear congruential sequence limited
     *            to small integers, and so should produce machine independent random numbers. The values of iseed are
     *            changed on exit, and can be used in the next call to ddrvst to continue the same random number
     *            sequence.
     * @param thresh (input) double A test will count as "failed" if the "error", computed as described above, exceeds
     *            thresh. Note that the error is scaled to be O(1), so thresh should be a reasonably small multiple of
     *            1, e.g., 10 or 100. In particular, it should not depend on the precision (single vs. double) or the
     *            size of the matrix. It must be at least zero.
     * @param A (input/workspace/output) double[][] of dimension (lda, max(nn)) Used to hold the matrix whose
     *            eigenvalues are to be computed. On exit, A contains the last matrix actually used.
     * @param lda (input) int The leading dimension of A. It must be at least 1 and at least max(nn).
     * @param D1 (workspace/output) double[] of dimension (max(nn)) The eigenvalues of A, as computed by dsteqr
     *            simultaneously with Z. On exit, the eigenvalues in D1 correspond with the matrix in A.
     * @param D2 (workspace/output) double[] of dimension (max(nn)) The eigenvalues of A, as computed by dsteqr if Z is
     *            not computed. On exit, the eigenvalues in D2 correspond with the matrix in A.
     * @param D3 (workspace/output) double[] of dimension max(nn)) The eigenvalues of A, as computed by dsterf. On exit,
     *            the eigenvalues in D3 correspond with the matrix in A.
     * @param D4 double[] of dimension (max(nn))
     * @param eveigs double[] of dimension (max(nn)) The eigenvalues as computed by dstev('N', ... )
     * @param WA1 double[]
     * @param WA2 double[]
     * @param WA3 double[]
     * @param U (workspace/output) double[][] of dimension (ldu, max(nn)) The orthogonal matrix computed by dsytrd +
     *            dorgtr.
     * @param ldu (input) int The leading dimension of U, Z and V. It must be at least 1 and at least max(nn).
     * @param V (workspace/output) double[][] of dimension (ldu, max(nn)) The Householder vectors computed by dsytrd in
     *            reducing A to tridiagonal form.
     * @param tau (workspace/output) double[] of dimension max(nn) The Householder factors computed by dsytrd in
     *            reducing A to tridiagonal form.
     * @param Z (workspace/output) double[][] of dimension (ldu, max(nn)) The orthogonal matrix of eigenvectors computed
     *            by dsteqr, dpteqr, and dstein.
     * @param work (workspace/output) double[] of dimension (lwork)
     * @param lwork (input) int The number of entries in work. This must be at least 1 + 4*nmax + 2 * nmax * lg nmax + 4
     *            * nmax**2 where nmax = max(nn[j], 2) and lg = log base 2.
     * @param iwork workspace int[] of dim (6 + 6*nmax + 5* nmax * lg nmax) where nmax = max(nn[j], 2) and lg = log base
     *            2.
     * @param liwork (input) int length of iwork
     * @param result (output) double[] of dimension (105) The values computed by the tests described above. The values
     *            are currently limited to 1/ulp, to avoid overflow.
     * @param info (output) int[] If 0, then everything ran OK. -1: nsizes < 0 -2: Some nn[j] < 0 -3: ntypes < 0 -5:
     *            thresh < 0 -9: lda < 1 or lda < nmax, where nmax is max(nn[j]) -16: ldu < 1 or ldu < nmax -21: lwork
     *            too small. If dlatmr, dlatms, dsytrd, dorgtr, dsteqr, dsterf, or dormtr returns an error code, the
     *            absolute value of it is returned.
     */
    private void ddrvst(final int nsizes, final int[] nn, final int ntypes, final boolean[] dotype, final int[] iseed,
            final double thresh, final double[][] A, final int lda, final double[] D1, final double[] D2,
            final double[] D3, final double[] D4, final double[] eveigs, final double[] WA1, final double[] WA2,
            final double[] WA3, final double[][] U, final int ldu, final double[][] V, final double[] tau,
            final double[][] Z, final double[] work, final int lwork, final int[] iwork, final int liwork,
            final double[] result, final int[] info) {
        final int maxtyp = 18; // The number of types defined
        boolean badnn;
        char uplo;
        int i;
        int idiag;
        int ihbw;
        final int[] iinfo = new int[1];
        int iL;
        int imode; // Value to be passed to the matrix generators
        int irow;
        int itemp;
        int itype;
        int iu;
        int iuplo;
        int j;
        int j1;
        int j2;
        int jcol;
        int jsize;
        int jtype;
        int lgn;
        int mtypes;
        int m[] = new int[1]; // Store number of eigenvalues found in dsyevx
        int m2[] = new int[1]; // Store number of eigenvalues found in dsyevx
        int m3[] = new int[1]; // Store number of eigenvalues found in dsyevx
        int n;
        final int[] nerrs = new int[1]; // The number of tests which have exceeded thresh
        // so far (computed by dlafts).
        int nmats; // The number of matrices generated so far.
        int nmax; // Largest value in nn.
        int ntest; // The number of tests performed, or which can
        // be performed so far, for the current matrix
        int ntestt; // The total number of tests performed so far.
        double abstol;
        double aninv;
        double anorm; // Norm of A; passed to the matrix generators.
        double cond; // Value to be passed to the matrix generators.
        final double[] ovfl = new double[1]; // Overflow threshold
        double rtovfl; // Square root of overflow threshold
        double rtunfl; // Square root of underflow threshold
        double temp1;
        double temp2;
        double temp3;
        double ulp; // Finest relative precision
        double ulpinv; // Inverse of finest relative precision
        double vl;
        double vu;
        final double[] unfl = new double[1]; // Underflow threshold
        final int[] idumma = new int[1];
        final int[] ioldsd = new int[4];
        final int[] iseed2 = new int[4];
        final int[] iseed3 = new int[4];

        // The order of magnitude (O(1), O(overflow^(1/2)), O(underflow^(1/2))
        final int[] kmagn = new int[] {1, 1, 1, 1, 1, 2, 3, 1, 1, 1, 2, 3, 1, 2, 3, 1, 2, 3};

        // The mode value to be passed to the matrix generator for type "j".
        final int[] kmode = new int[] {0, 0, 4, 3, 1, 4, 4, 4, 3, 1, 4, 4, 0, 0, 0, 4, 4, 4};

        // The general type (1-10) for type "j".
        final int[] ktype = new int[] {1, 2, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 8, 8, 8, 9, 9, 9};
        int[] iwork2;
        double[] work2;
        double[] work3;
        final double[] res = new double[2];
        String typeString;
        
        vl = 0.0;
        vu = 0.0;

        // Check for errors
        ntestt = 0;
        info[0] = 0;

        badnn = false;
        nmax = 1;

        for (j = 0; j < nsizes; j++) {
            nmax = Math.max(nmax, nn[j]);

            if (nn[j] < 0) {
                badnn = true;
            }
        } // for (j = 0; j < nsizes; j++)

        work2 = new double[3 * nmax];
        work3 = new double[nmax];

        if (nsizes < 0) {
            info[0] = -1;
        } else if (badnn) {
            info[0] = -2;
        } else if (ntypes < 0) {
            info[0] = -3;
        } else if (lda < nmax) {
            info[0] = -9;
        } else if (ldu < nmax) {
            info[0] = -16;
        } else if ( (2 * Math.max(2, nmax) * Math.max(2, nmax)) > lwork) {
            info[0] = -21;
        }

        if (info[0] != 0) {
            MipavUtil.displayError("Error ddrvst had info[0] = " + info[0]);

            return;
        }

        // Quick return if possible
        if ( (nsizes == 0) || (ntypes == 0)) {
            return;
        }

        unfl[0] = ge.dlamch('S');
        ovfl[0] = ge.dlamch('O');
        ge.dlabad(unfl, ovfl);
        ulp = ge.dlamch('E') * ge.dlamch('B');
        ulpinv = 1.0 / ulp;
        rtunfl = Math.sqrt(unfl[0]);
        rtovfl = Math.sqrt(ovfl[0]);

        // Loop over sizes, types
        for (i = 0; i < 4; i++) {
            iseed2[i] = iseed[i];
            iseed3[i] = iseed[i];
        }

        nerrs[0] = 0;
        nmats = 0;

        for (jsize = 1; j <= nsizes; jsize++) {
            n = nn[jsize - 1];

            if (n > 0) {
                lgn = (int) (Math.log((double) n) / Math.log(2.0));

                if (Math.pow(2.0, lgn) < n) {
                    lgn = lgn + 1;
                }

                if (Math.pow(2.0, lgn) < n) {
                    lgn = lgn + 1;
                }
            } // if (n > 0)

            aninv = 1.0 / (double) Math.max(1, n);

            if (nsizes != 1) {
                mtypes = Math.min(maxtyp, ntypes);
            } else {
                mtypes = Math.min(maxtyp + 1, ntypes);
            }

            for (jtype = 1; jtype <= mtypes; jtype++) {

                if ( !dotype[jtype - 1]) {
                    continue;
                }

                nmats = nmats + 1;
                ntest = 0;

                for (j = 0; j < 4; j++) {
                    ioldsd[j] = iseed[j];
                }

                // Compute "A"
                // Control parameters:
                /*
                 * kmagn kmode       ktype 
           = 1     O(1)  clustered 1 zero 
           = 2     large clustered 2 identity 
           = 3     small exponential (none) 
           = 4           arithmetic  diagonal, (w/ eigenvalues) 
           = 5           random log  symmetric, w/ eigenvalues 
           = 6           random      (none) 
           = 7                       random diagonal 
           = 8                       random symmetric 
           = 9                       band symmetric, w/ eigenvalues
                 */

                if (mtypes <= maxtyp) {
                    itype = ktype[jtype - 1];
                    imode = kmode[jtype - 1];

                    // Compute norm

                    if (kmagn[jtype - 1] == 1) {
                        anorm = 1.0;
                    } else if (kmagn[jtype - 1] == 2) {
                        anorm = (rtovfl * ulp) * aninv;
                    } else {
                        anorm = rtunfl * n * ulpinv;
                    }

                    ge.dlaset('F', lda, n, 0.0, 0.0, A, lda);
                    iinfo[0] = 0;
                    cond = ulpinv;

                    // Special Matrices -- Identity & Jordan block

                    // Zero
                    if (itype == 1) {
                        iinfo[0] = 0;
                    } else if (itype == 2) {

                        // Identity
                        for (jcol = 0; jcol < n; jcol++) {
                            A[jcol][jcol] = anorm;
                        }
                    } else if (itype == 4) {

                        // Diagonal Matrix, [Eigen]values, Specified
                        ge.dlatms(n, n, 'S', iseed, 'S', work, imode, cond, anorm, 0, 0, 'N', A, lda, work2, iinfo);
                    } else if (itype == 5) {

                        // Symmetric, eigenvalues specified
                        ge.dlatms(n, n, 'S', iseed, 'S', work, imode, cond, anorm, n, n, 'N', A, lda, work2, iinfo);
                    } else if (itype == 7) {

                        // Diagonal, random eigenvalues
                        idumma[0] = 1;
                        ge.dlatmr(n, n, 'S', iseed, 'S', work, 6, 1.0, 1.0, 'T', 'N', work2, 1, 1.0, work3, 1, 1.0, 'N',
                                idumma, 0, 0, 0.0, anorm, 'N', A, lda, iwork, iinfo);
                    } // else if (itype == 7)
                    else if (itype == 8) {

                        // Symmetric, random eigenvalues
                        idumma[0] = 1;
                        ge.dlatmr(n, n, 'S', iseed, 'S', work, 6, 1.0, 1.0, 'T', 'N', work2, 1, 1.0, work3, 1, 1.0, 'N',
                                idumma, n, n, 0.0, anorm, 'N', A, lda, iwork, iinfo);
                    } // else if (itype == 8)
                    else if (itype == 9) {

                        // Symmetric banded, eigenvalues specified
                        ihbw = (int) ( (n - 1) * ge.dlarnd(1, iseed3));
                        ge.dlatms(n, n, 'S', iseed, 'S', work, imode, cond, anorm, ihbw, ihbw, 'Z', U, ldu, work2, iinfo);

                        // Store as dense matrix for most routines
                        ge.dlaset('F', lda, n, 0.0, 0.0, A, lda);

                        for (idiag = -ihbw; idiag <= ihbw; idiag++) {
                            irow = ihbw - idiag + 1;
                            j1 = Math.max(1, idiag + 1);
                            j2 = Math.min(n, n + idiag);

                            for (j = j1; j <= j2; j++) {
                                i = j - idiag;
                                A[i - 1][j - 1] = U[irow - 1][j - 1];
                            }
                        } // for (idiag = -ihbw; idiag <= ihbw; idiag++)
                    } // else if (itype == 9)
                    else {
                        iinfo[0] = 1;
                    } // else

                    if (iinfo[0] != 0) {
                        UI.setDataText("Generator iinfo[0] = " + iinfo[0] + "\n");
                        UI.setDataText("n = " + n + "\n");
                        UI.setDataText("jtype = " + jtype + "\n");
                        UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                        UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                        UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                        UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                        info[0] = Math.abs(iinfo[0]);

                        return;
                    } // if (iinfo[0] != 0)

                } // if (mtypes <= maxtyp)

                abstol = unfl[0] + unfl[0];
                if (n <= 1) {
                    iL = 1;
                    iu = n;
                } else { // n > 1
                    iL = 1 + ( (n - 1) * (int) (ge.dlarnd(1, iseed2)));
                    iu = 1 + ( (n - 1) * (int) (ge.dlarnd(1, iseed2)));

                    if (iL > iu) {
                        itemp = iL;
                        iL = iu;
                        iu = itemp;
                    }
                } // else n > 1

                // Test storing upper or lower triangular part of matrix.
                for (iuplo = 0; iuplo <= 1; iuplo++) {

                    if (iuplo == 0) {
                        uplo = 'L';
                        ntest = 1;
                    } else {
                        uplo = 'U';
                        ntest = 10;
                    }

                    loop1: {
                        ge.dlacpy(' ', n, n, V, ldu, A, lda);
                        
                        if (n > 0) {
                            temp3 = Math.max(Math.abs(D1[0]), Math.abs(D1[n-1])); 
                            if (iL != 1) {
                                vl = D1[iL-1] - Math.max(0.5*(D1[iL-1] - D1[iL-2]),
                                     Math.max(10.0*ulp*temp3, 10.0*rtunfl));    
                            }
                            else {
                                vl = D1[0] - Math.max(0.5*(D1[n-1] - D1[0]),
                                     Math.max(10.0*ulp*temp3, 10.0*rtunfl));
                            }
                            if (iu != n) {
                                vu = D1[iu-1] + Math.max(0.5*(D1[iu] - D1[iu-1]),
                                     Math.max(10.0*ulp*temp3, 10.0*rtunfl));
                            }
                            else {
                                vu = D1[n-1] + Math.max(0.5*(D1[n-1] - D1[0]),
                                     Math.max(10.0*ulp*temp3, 10.0*rtunfl));
                            }
                        } // if (n > 0)
                        else { 
                            temp3 = 0.0;
                            vl = 0.0;
                            vu = 0.0;
                        } // else 
                        iwork2 = new int[n];
                        dsyevx('V', 'A', uplo, n, A, ldu, vl, vu, iL, iu,
                               abstol, m, WA1, Z, ldu, work, lwork, iwork, iwork2, iinfo);
                        if (iinfo[0] != 0) {
                            UI.setDataText("dsyevx(V, A, " + uplo + " ) iinfo[0] = " + iinfo[0] + "\n");
                            UI.setDataText("n = " + n + "\n");
                            UI.setDataText("jtype = " + jtype + "\n");
                            UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                            UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                            UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                            UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                            info[0] = Math.abs(iinfo[0]);

                            if (iinfo[0] < 0) {
                                return;
                            } else {
                                result[ntest - 1] = ulpinv;
                                result[ntest] = ulpinv;
                                result[ntest + 1] = ulpinv;

                                break loop1;
                            }
                        } // if (iinfo[0] != 0)

                         // Do tests 1 and 2 or 10 and 11
                        ge.dlacpy(' ', n, n, V, ldu, A, lda);
                        ge.dsyt21(1, uplo, n, 0, A, ldu, D1, D2, Z, ldu, V, ldu, tau, work, res);
                        result[ntest-1] = res[0];
                        result[ntest] = res[1];
                        ntest = ntest + 2;
                        dsyevx('N', 'A', uplo, n, A, ldu, vl, vu, iL, iu,
                                abstol, m2, WA2, Z, ldu, work, lwork, iwork, iwork2, iinfo);
                        if (iinfo[0] != 0) {
                            UI.setDataText("dsyevx(N, A, " + uplo + " ) iinfo[0] = " + iinfo[0] + "\n");
                            UI.setDataText("n = " + n + "\n");
                            UI.setDataText("jtype = " + jtype + "\n");
                            UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                            UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                            UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                            UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                            info[0] = Math.abs(iinfo[0]);

                            if (iinfo[0] < 0) {
                                return;
                            } else {
                                result[ntest - 1] = ulpinv;

                                break loop1;
                            }
                        } // if (iinfo[0] != 0)
                        
                        // Do test 3 or 12
                        temp1 = 0.0;
                        temp2 = 0.0;
                        for (j = 0; j < n; j++) {
                            temp1 = Math.max(temp1, Math.max(Math.abs(WA1[j]), Math.abs(WA2[j])));
                            temp2 = Math.max(temp2, Math.abs(WA1[j] - WA2[j]));
                        }
                        result[ntest-1] = temp2/Math.max(unfl[0], ulp * Math.max(temp1, temp2));
                    } // loop1
                    
                    loop2: {
                        ntest = ntest + 1;
                        ge.dlacpy(' ', n, n, V, ldu, A, lda);
                        dsyevx('V', 'I', uplo, n, A, ldu, vl, vu, iL, iu,
                                abstol, m2, WA2, Z, ldu, work, lwork, iwork, iwork2, iinfo);
                         if (iinfo[0] != 0) {
                             UI.setDataText("dsyevx(V, I, " + uplo + " ) iinfo[0] = " + iinfo[0] + "\n");
                             UI.setDataText("n = " + n + "\n");
                             UI.setDataText("jtype = " + jtype + "\n");
                             UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                             UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                             UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                             UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                             info[0] = Math.abs(iinfo[0]);

                             if (iinfo[0] < 0) {
                                 return;
                             } else {
                                 result[ntest - 1] = ulpinv;
                                 result[ntest] = ulpinv;
                                 result[ntest + 1] = ulpinv;

                                 break loop2;
                             }
                         } // if (iinfo[0] != 0)
                         
                         // Do tests 4 and 5 or 13 and 14
                         ge.dlacpy(' ', n, n, V, ldu, A, lda);
                         dsyt22(1, uplo, n, m2[0], 0, A, ldu, WA2, D2, Z, ldu, V, ldu, tau, work, res);
                         result[ntest-1] = res[0];
                         result[ntest] = res[1];
                         ntest = ntest + 2;
                         ge.dlacpy(' ', n, n, V, ldu, A, lda);
                         dsyevx('N', 'I', uplo, n, A, ldu, vl, vu, iL, iu,
                                 abstol, m3, WA3, Z, ldu, work, lwork, iwork, iwork2, iinfo);
                         if (iinfo[0] != 0) {
                             UI.setDataText("dsyevx(N, I, " + uplo + " ) iinfo[0] = " + iinfo[0] + "\n");
                             UI.setDataText("n = " + n + "\n");
                             UI.setDataText("jtype = " + jtype + "\n");
                             UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                             UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                             UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                             UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                             info[0] = Math.abs(iinfo[0]);

                             if (iinfo[0] < 0) {
                                 return;
                             } else {
                                 result[ntest - 1] = ulpinv;

                                 break loop2;
                             }
                         } // if (iinfo[0] != 0)
                         
                         // Do test 6 or 15

                         temp1 = dsxt1(1, WA2, m2[0], WA3, m3[0], abstol, ulp, unfl[0]);
                         temp2 = dsxt1(1, WA3, m3[0], WA2, m2[0], abstol, ulp, unfl[0]);
                         result[ntest-1] = (temp1 + temp2)/Math.max(unfl[0], ulp * temp3);
                    } // loop2
                    
                    loop3: {
                        ntest = ntest + 1;
                        ge.dlacpy(' ', n, n, V, ldu, A, lda);
                        dsyevx('V', 'V', uplo, n, A, ldu, vl, vu, iL, iu,
                                abstol, m2, WA2, Z, ldu, work, lwork, iwork, iwork2, iinfo);
                         if (iinfo[0] != 0) {
                             UI.setDataText("dsyevx(V, V, " + uplo + " ) iinfo[0] = " + iinfo[0] + "\n");
                             UI.setDataText("n = " + n + "\n");
                             UI.setDataText("jtype = " + jtype + "\n");
                             UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                             UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                             UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                             UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                             info[0] = Math.abs(iinfo[0]);

                             if (iinfo[0] < 0) {
                                 return;
                             } else {
                                 result[ntest - 1] = ulpinv;
                                 result[ntest] = ulpinv;
                                 result[ntest + 1] = ulpinv;

                                 break loop3;
                             }
                         } // if (iinfo[0] != 0)
                         
                         // Do tests 7 and 8 or 16 and 17
                         ge.dlacpy(' ', n, n, V, ldu, A, lda);
                         dsyt22(1, uplo, n, m2[0], 0, A, ldu, WA2, D2, Z, ldu, V, ldu, tau, work, res);
                         result[ntest-1] = res[0];
                         result[ntest] = res[1];
                         ntest = ntest + 2;
                         ge.dlacpy(' ', n, n, V, ldu, A, lda);
                         dsyevx('N', 'V', uplo, n, A, ldu, vl, vu, iL, iu,
                                 abstol, m3, WA3, Z, ldu, work, lwork, iwork, iwork2, iinfo);
                         if (iinfo[0] != 0) {
                             UI.setDataText("dsyevx(N, V, " + uplo + " ) iinfo[0] = " + iinfo[0] + "\n");
                             UI.setDataText("n = " + n + "\n");
                             UI.setDataText("jtype = " + jtype + "\n");
                             UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                             UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                             UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                             UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                             info[0] = Math.abs(iinfo[0]);

                             if (iinfo[0] < 0) {
                                 return;
                             } else {
                                 result[ntest - 1] = ulpinv;

                                 break loop3;
                             }
                         } // if (iinfo[0] != 0)
                         
                         if (m3[0] == 0 && n > 0) {
                             result[ntest-1] = ulpinv;
                             break loop3;
                         }
                         
                         // Do test 9 or 18

                         temp1 = dsxt1(1, WA2, m2[0], WA3, m3[0], abstol, ulp, unfl[0]);
                         temp2 = dsxt1(1, WA3, m3[0], WA2, m2[0], abstol, ulp, unfl[0]);
                         if (n > 0) {
                             temp3 = Math.max(Math.abs(WA1[0]), Math.abs(WA1[n-1]));
                         }
                         else {
                             temp3 = 0.0;
                         }
                         result[ntest-1] = (temp1 + temp2)/Math.max(unfl[0], temp3 * ulp);    
                    } // loop3
                } // for (iuplo = 0; iuplo <= 1; iuplo++)

                // End of Loop -- Check for result[j] > thresh
                ntestt = ntestt + ntest;
                typeString = new String("DST");
                ge.dlafts(typeString, n, n, jtype, ntest, result, ioldsd, thresh, nerrs);
            } // for (jtype = 1; jtype <= mtypes; jtype++)
        } // for (jsize = 1; j <= nsizes; jsize++)

        // Summary
        if (nerrs[0] > 0) {
            UI.setDataText("ddrvst " + nerrs[0] + " out of " + ntestt + " tests failed to pass the threshold\n");
        } else {
            UI.setDataText("All " + ntestt + " tests for ddrvst passed the threshold\n");
        }

        return;

    } // ddrvst
    
    /** This is the port of version 3.1 LAPACK test routine DORT01.
     *  Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
     *  November 2006
     *     
        DSYT22  generally checks a decomposition of the form
   *
   *            A U = U S
   *
   *    where A is symmetric, the columns of U are orthonormal, and S
   *    is diagonal (if KBAND=0) or symmetric tridiagonal (if
   *    KBAND=1).  If ITYPE=1, then U is represented as a dense matrix,
   *    otherwise the U is expressed as a product of Householder
   *    transformations, whose vectors are stored in the array "V" and
   *    whose scaling constants are in "TAU"; we shall use the letter
   *    "V" to refer to the product of Householder transformations
   *    (which should be equal to U).
   *
   *    Specifically, if ITYPE=1, then:
   *
   *            RESULT(1) = | U' A U - S | / ( |A| m ulp ) *and*
   *            RESULT(2) = | I - U'U | / ( m ulp )
   *            
        @param itype input int Specifies the type of tests to be performed.
       *          1: U expressed as a dense orthogonal matrix:
       *             result[0] = | A - U S U' | / ( |A| n ulp )   *and*
       *             result[1] = | I - UU' | / ( n ulp )  
       *@param uplo input char  If UPLO='U', the upper triangle of A will be used and the
       *          (strictly) lower triangle will not be referenced.  If
       *          UPLO='L', the lower triangle of A will be used and the
       *          (strictly) upper triangle will not be referenced.
       *          Not modified.
       *@param n  input int The size of the matrix.  If it is zero, dsyt22 does nothing.
       *          It must be at least zero.
       *          Not modified.
       *@param m  input int The number of columns of U.  If it is zero, dsyt22 does
       *          nothing.  It must be at least zero.
       *          Not modified.
       *@param kband input int The bandwidth of the matrix.  It may only be zero or one.
       *          If zero, then S is diagonal, and E is not referenced.  If
       *          one, then S is symmetric tri-diagonal.
       *          Not modified.
       *@param A input double[][] of dimension (lda, n).
       *         The original (unfactored) matrix.  It is assumed to be
       *          symmetric, and only the upper (uplo='U') or only the lower
       *          (uplo='L') will be referenced.
       *          Not modified.
       *@param lda input int The leading dimension of A.  It must be at least 1
       *          and at least n.
       *          Not modified.
       *@param d input double[] of dimension n.  The diagonal of the (symmetric tri-) diagonal matrix.
       *          Not modified.
       *@param e input double[] of dimension n.  The off-diagonal of the (symmetric tri-) diagonal matrix.
       *          e[0] is ignored, e[1] is the (0,1) and (1,0) element, etc.
       *          Not referenced if kband=0.
       *          Not modified.
       *@param U input double[][] of dimension (ldu, n).
       *          If itype=1 or 3, this contains the orthogonal matrix in
       *          the decomposition, expressed as a dense matrix.  If itype=2,
       *          then it is not referenced.
       *          Not modified.
       *@param ldu input int The leading dimension of U.  ldu must be at least n and
       *          at least 1.
       *          Not modified.
       *@param V input double[][] of dimension (ldv, n).
       *          If itype=2 or 3, the lower triangle of this array contains
       *          the Householder vectors used to describe the orthogonal
       *          matrix in the decomposition.  If itype=1, then it is not
       *          referenced.
       *          Not modified.
       *@param ldv input int The leading dimension of V.  ldv must be at least n and
       *          at least 1.
       *          Not modified.
       *@param tau input double[] of dimension n.
       *          If itype >= 2, then tau(j) is the scalar factor of
       *          v(j) v(j)' in the Householder transformation H(j) of
       *          the product  U = H(1)...H(n-2)
       *          If itype < 2, then tau is not referenced.
       *          Not modified.
       *@param work workspace double[] of dimension (2*n**2) Modified
       *@param result output double[] of dimension 2.
       *          The values computed by the two tests described above.  The
       *          values are currently limited to 1/ulp, to avoid overflow.
       *          result[0] is always modified.  result[1] is modified only
       *          if ldu is at least n.
       *          Modified.
     */     
    private void dsyt22(int itype, char uplo, int n, int m, int kband, double A[][],
                        int lda, double d[], double e[], double U[][], int ldu,
                        double V[][], int ldv, double tau[], double work[], double result[]) {
             double unfl;
             double ulp;
             double anorm;
             double wnorm;
             int i;
             int j;
             int jj;
             int jj1;
             int jj2;
             int nn;
             int nnp1;
             double array1[][];
             double array2[][];
             int index;
             double temp[] = new double[1];
      
             result[0] = 0.0;
             result[1] = 0.0;
             if (n <= 0 || m <= 0) {
                 return;
             }
       
             unfl = ge.dlamch('S');
             ulp = ge.dlamch('P');
       
             // Do Test 1
       
             // Norm of A:
       
             anorm = Math.max(ge.dlansy('1', uplo, n, A, lda, work), unfl);
             
       
             // Compute error matrix:
       
             // ITYPE=1: error = U' A U - S
       
             array1 = new double[n][m];
             index = 0;
             for (j = 0; j < m; j++) {
                 for (i = 0; i < n; i++) {
                     array1[i][j] = work[index++];
                 }
             }
             ge.dsymm('L', uplo, n, m, 1.0, A, lda, U, ldu, 0.0, array1, n);
             index = 0;
             for (j = 0; j < m; j++) {
                 for (i = 0; i < n; i++) {
                     work[index++] = array1[i][j];
                 }
             }
             nn = n*n;
             nnp1 = nn + 1;
             array2 = new double[n][m];
             index = 0;
             for (j = 0; j < m; j++) {
                 for (i = 0; i < n; i++) {
                     array1[i][j] = work[index];
                     array2[i][j] = work[nnp1-1+index];
                     index++;
                 }
             }
             ge.dgemm('T', 'N', m, m, n, 1.0, U, ldu, array1, n, 0.0, array2, n);
             index = 0;
             for (j = 0; j < m; j++) {
                 for (i = 0; i < n; i++) {
                     work[index] = array1[i][j];
                     work[nnp1-1+index] = array2[i][j];
                     index++;
                 }
             }
             for (j = 1; j <= m; j++) {
                jj = nn + (j-1)*n + j;
                work[jj-1] = work[jj-1] - d[j-1];
             } // for (j = 1; j <= m; j++)
             if (kband == 1 && n > 1) {
                for (j = 2; j <= m; j++) {
                   jj1 = nn + (j-1)*n + j - 1;
                   jj2 = nn + (j-2)*n + j;
                   work[jj1-1] = work[jj1-1] - e[j-2];
                   work[jj2-1] = work[jj2-1] - e[j-2];
                } // for (j = 2; j <= m; j++)
             } // if (kband == 1 && n > 1)
             index = 0;
             for (j = 0; j < m; j++) {
                 for (i = 0; i < n; i++) {
                     array1[i][j] = work[nnp1-1+index];
                     index++;
                 }
             }
             wnorm = ge.dlansy( '1', uplo, m, array1, n, work);
       
             if (anorm > wnorm) {
                result[0] = (wnorm / anorm ) / ( m*ulp );
             }
             else {
                if (anorm < 1.0) {
                   result[0] = ( Math.min(wnorm, m*anorm) / anorm) / (m*ulp);
                }
                else {
                   result[0] = Math.min( wnorm / anorm, (double)(m) ) / ( m*ulp );
                }
             }
       
             // Do Test 2
       
             // Compute  U'U - I
       
             if (itype == 1) {
                array1 = new double[Math.min(m,n)][Math.min(m,n)];
                dort01('C', n, m, U, ldu, array1, 2*n*n, temp);
                result[1] = temp[0];
             }
       
             return;
       
    } // dsyt22
             
 /** This is the port of version 3.1 LAPACK test routine DORT01.
  *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
  *     November 2006
  *
  *     .. Scalar Arguments ..
        CHARACTER          ROWCOL
        INTEGER            LDU, LWORK, M, N
        DOUBLE PRECISION   RESID
  *     ..
  *     .. Array Arguments ..
        DOUBLE PRECISION   U( LDU, * ), WORK( * )
  *     ..
  *
  *  Purpose
  *  =======
  *
  *  DORT01 checks that the matrix U is orthogonal by computing the ratio
  *
  *     RESID = norm( I - U*U' ) / ( n * EPS ), if ROWCOL = 'R',
  *  or
  *     RESID = norm( I - U'*U ) / ( m * EPS ), if ROWCOL = 'C'.
  *
  *  Alternatively, if there isn't sufficient workspace to form
  *  I - U*U' or I - U'*U, the ratio is computed as
  *
  *     RESID = abs( I - U*U' ) / ( n * EPS ), if ROWCOL = 'R',
  *  or
  *     RESID = abs( I - U'*U ) / ( m * EPS ), if ROWCOL = 'C'.
  *
  *  where EPS is the machine precision.  ROWCOL is used only if m = n;
  *  if m > n, ROWCOL is assumed to be 'C', and if m < n, ROWCOL is
  *  assumed to be 'R'.
  *
  *  Arguments
  *  =========
  *
  *  ROWCOL  (input) CHARACTER
  *          Specifies whether the rows or columns of U should be checked
  *          for orthogonality.  Used only if M = N.
  *          = 'R':  Check for orthogonal rows of U
  *          = 'C':  Check for orthogonal columns of U
  *
  *  M       (input) INTEGER
  *          The number of rows of the matrix U.
  *
  *  N       (input) INTEGER
  *          The number of columns of the matrix U.
  *
  *  U       (input) DOUBLE PRECISION array, dimension (LDU,N)
  *          The orthogonal matrix U.  U is checked for orthogonal columns
  *          if m > n or if m = n and ROWCOL = 'C'.  U is checked for
  *          orthogonal rows if m < n or if m = n and ROWCOL = 'R'.
  *
  *  LDU     (input) INTEGER
  *          The leading dimension of the array U.  LDU >= max(1,M).
  *
  *  WORK    (workspace) DOUBLE PRECISION array, dimension (min(m,n),min(m,n))
  *          In dlaset, dsyrk, and dlansy work must be 2D array.
  *
  *  LWORK   (input) INTEGER
  *          The length of the array WORK.  For best performance, LWORK
  *          should be at least N*(N+1) if ROWCOL = 'C' or M*(M+1) if
  *          ROWCOL = 'R', but the test will be done even if LWORK is 0.
  *
  *  RESID   (output) DOUBLE PRECISION
  *          RESID = norm( I - U * U' ) / ( n * EPS ), if ROWCOL = 'R', or
  *          RESID = norm( I - U' * U ) / ( m * EPS ), if ROWCOL = 'C'.
  */
  private void dort01(char rowcol, int m, int n, double[][] U, int ldu,
                      double[][] work, int lwork, double[] resid) {
      char transu;
      int i;
      int j;
      int k;
      int ldwork;
      int mnmin;
      double eps;
      double tmp;
      double work2[];
      double v1[];
      double v2[];
      int p;
      
      resid[0] = 0.0;
      
      // Quick return if possible.
      if ((m <= 0) || (n <= 0)) {
          return;
      }
      
      eps = ge.dlamch('P'); // Precision
      if ((m < n) || ((m == n) && ((rowcol == 'R') || (rowcol == 'r')))) {
          transu = 'N';
          k = n;
      }
      else {
          transu = 'T';
          k = m;
      }
      mnmin = Math.min(m, n);
      
      if ((mnmin + 1)*mnmin <= lwork) {
          ldwork = mnmin;
      }
      else {
          ldwork = 0;
      }
      if (ldwork > 0) {
          // Compute I - U*U' or I - U'*U
          ge.dlaset('U', mnmin, mnmin, 0.0, 1.0, work, ldwork);
          ge.dsyrk('U', transu, mnmin, k, -1.0, U, ldu, 1.0, work, ldwork);
          
          // Compute norm(I - U*U') /(k * eps).
          work2 = new double[Math.max(1, mnmin)];
          resid[0] = ge.dlansy('1', 'U', mnmin, work, ldwork, work2);
          resid[0] = (resid[0]/(double)k)/eps;
      } // if (ldwork > 0)
      else if (transu == 'T') {
          // Find the maximum element in abs(I - U'*U)/(m * eps)
          for (j = 1; j <= n; j++) {
              for (i = 1; i <= j; i++) {
                  if (i != j) {
                      tmp = 0.0;
                  }
                  else {
                      tmp = 1.0;
                  }
                  v1 = new double[m];
                  v2 = new double[m];
                  for (p = 0; p < m; p++) {
                      v1[p] = U[p][i-1];
                      v2[p] = U[p][j-1];
                  }
                  tmp = tmp - ge.ddot(m, v1, 1, v2, 1);
                  resid[0] = Math.max(resid[0], Math.abs(tmp));
              } // for (i = 1; i <= j; i++)
          } // for (j = 1; j <= n; j++)
          resid[0] = (resid[0]/(double)m)/eps;
      } // else if (transu == 'T')
      else {
          // Find the maximum element in abs(I - U*U')/(n * eps)
          for (j = 1; j <= m; j++) {
              for (i = 1; i <= j; i++) {
                  if (i != j) {
                      tmp = 0.0;
                  }
                  else {
                      tmp = 1.0;
                  }
                  v1 = new double[n];
                  v2 = new double[n];
                  for (p = 0; p < n; p++) {
                      v1[p] = U[j-1][p];
                      v2[p] = U[i-1][p];
                  }
                  tmp = tmp - ge.ddot(n, v1, 1, v2, 1);
                  resid[0] = Math.max(resid[0], Math.abs(tmp));
              } // for (i = 1; i <= j; i++)
          } // for (j = 1; j <= m; j++)
          resid[0] = (resid[0]/(double)n)/eps;
      } // else
      return;
  } // dort01
  
  /** This is the port of version 3.1 LAPACK test routine DSXT1.
   *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
   *     November, 2006
   *     
   *    DSXT1  computes the difference between a set of eigenvalues.
     *  The result is returned as the function value.
     *
     *  IJOB = 1:   Computes   max { min | D1(i)-D2(j) | }
     *                          i     j
     *
     *  IJOB = 2:   Computes   max { min | D1(i)-D2(j) | /
     *                          i     j
     *                               ( ABSTOL + |D1(i)|*ULP ) }
     *                               
     *  @param ijob input int Specifies the type of tests to be performed.
     *  @param d1 input double[] of dimension n1  The first array.  d1 should be in increasing order, i.e.,
     *          d1[j] <= d1[j+1].
     *  @param n1 input int The length of d1.
     *  @param d2 input double[] of dimension n2.  The second array.  d2 should be in increasing order, i.e.,
     *          d2[j] <= d2[j+1].
     *  @param n2 input int The length of d2.
     *  @param abstol input double  The absolute tolerance, used as a measure of the error.
     *  @param ulp input double Machine precision.
     *  @param unfl input double The smallest positive number whose reciprocal does not overflow.     
   */     
    private double dsxt1(int ijob, double d1[], int n1, double d2[], int n2, double abstol,
                         double ulp, double unfl) {
           double temp1;
           double temp2;
           int i;
           int j;
           
           temp1 = 0.0;
     
           j = 1;
           for (i = 1; i <= n1; i++) {
              while (d2[j-1] < d1[i-1] && j < n2 ) {
                 j = j + 1;
              }
              if (j == 1) {
                 temp2 = Math.abs(d2[j-1]- d1[i-1]);
                 if (ijob == 2) {
                    temp2 = temp2 / Math.max(unfl, abstol +ulp*Math.abs(d1[i-1] ) );
                 }
              }
              else {
                 temp2 = Math.min(Math.abs(d2[j-1]-d1[i-1]), Math.abs(d1[i-1]-d2[j-2] ) );
                 if (ijob == 2) {
                    temp2 = temp2 /Math.max(unfl, abstol+ulp*Math.abs(d1[i-1]) );
                 }
              }
              temp1 = Math.max(temp1, temp2);
           } // for (i = 1; i <= n1; i++)
     
           return temp1;
     
    } // dsxt1
    
    /**
     * This routine is an extraction from the FORTRAN program version 3.1.1 DCHKEE of the code needed to drive dchkst,
     * that tests routines used in symmetric generalized eigenvalue problem. The routines tested are dstebz and
     * dstein.  Numerical values were obtained from the sep.in datafile. Original DCHKEE created by Univ. of
     * Tennessee, Univ. of California Berkeley, and NAG Ltd., January, 2007
     */
    public void dchkst_test() {

        // Number of values of n
        final int nn = 6;

        // Values of matrix dimension n
        final int[] nval = new int[] {0, 1, 2, 3, 5, 20};

        // Number of values of NB, NBMIN, and NX
        final int nparms = 5;

        // Values of blocksize NB
        final int[] nbval = new int[] {1, 3, 3, 3, 10};

        // Values of minimum blocksize NBMIN
        final int[] nbmin = new int[] {2, 2, 2, 2, 2};

        // Values of crossover point NX
        final int[] nxval = new int[] {1, 0, 5, 9, 1};

        // Threshold value for the test ratios. Information will be printed
        // about each test for which the test ratio is greater than or equal
        // to threshold.
        final double thresh = 50.0;

        // Test the LAPACK routines
        final boolean tstchk = true;

        // Code describing how to set the random number seed.
        // = 0: Set the seed to a default number before each run.
        // = 1: Initialize the seed to a default value only before the first
        // run.
        // = 2: Like 1, but use the seed values in the 4 integer array
        // ioldsd
        int newsd = 1;
        final int maxt = 30;
        final boolean[] dotype = new boolean[maxt];
        final int[] ioldsd = new int[] {0, 0, 0, 1};
        final int[] iseed = new int[] {0, 0, 0, 1};
        final int nmax = 132;
        final int lwork = (nmax * ( (5 * nmax) + 5)) + 1;
        final int liwork = nmax * ( (5 * nmax) + 20);
        final int[] iwork = new int[liwork];
        final double[] work = new double[lwork];
        final double[] result = new double[500];
        final int[] info = new int[1];
        int iparms[];
        double[][] A;
        double[] AP;
        double[] SD;
        double[] SE;
        double[] D1;
        double[] D2;
        double[] D3;
        double[] D4;
        double[] D5;
        double[] WA1;
        double[] WA2;
        double[] WA3;
        double[] WR;
        double[][] U;
        double[][] V;
        double[] VP;
        double[] TAU;
        double[][] Z;

        final int maxtyp = 21;
        int i;
        int k;
        int maxnval;
        UI.setDataText("Tests of the Symmetric Eigenvalue Problem routines\n");

        for (i = 0; i < maxtyp; i++) {
            dotype[i] = true;
        }

        maxnval = 0;

        for (i = 0; i < nn; i++) {

            if (nval[i] > maxnval) {
                maxnval = nval[i];
            }
        }

        iparms = new int[100];

        // 9 = maximum size of the subproblems at the bottom of the computation
        // tree in the divide-and-conquer algorithm (used by xgelsd and xgesdd)
        iparms[9 - 1] = 25;
        A = new double[nmax][maxnval];
        AP = new double[maxnval * (maxnval + 1) / 2];
        SD = new double[maxnval];
        SE = new double[maxnval];
        D1 = new double[maxnval];
        D2 = new double[maxnval];
        D3 = new double[maxnval];
        D4 = new double[maxnval];
        D5 = new double[maxnval];
        WA1 = new double[maxnval];
        WA2 = new double[maxnval];
        WA3 = new double[maxnval];
        WR = new double[maxnval];
        U = new double[nmax][maxnval];
        V = new double[nmax][maxnval];
        VP = new double[maxnval * (maxnval + 1) / 2];
        TAU = new double[maxnval];
        Z = new double[nmax][maxnval];

        for (i = 1; i <= nparms; i++) {

            // 1 = The optimal blocksize; if this value is 1, an unblocked
            // algorithm will give the best performance
            iparms[1 - 1] = nbval[i - 1];

            // 2 = The minimum blocksize for which the block routine should be
            // used; if the usable block size is less than this value, an
            // unblocked routine should be used.
            iparms[2 - 1] = nbmin[i - 1];

            // 3 = The crossover point (in a block routine, for n less than this
            // value, an unblocked routine should be used).
            iparms[3 - 1] = nxval[i - 1];

            if (newsd == 0) {

                for (k = 0; k < 4; k++) {
                    iseed[k] = ioldsd[k];
                }
            } // if (newsd == 0)

            UI.setDataText("Optimal blocksize = " + nbval[i - 1] + "\n");
            UI.setDataText("Minimum blocksize = " + nbmin[i - 1] + "\n");
            UI.setDataText("Crossover point = " + nxval[i - 1] + "\n");

            if (tstchk) {
                dchkst(nn, nval, maxtyp, dotype, iseed, thresh, A, nmax, AP, SD, SE, D1, D2, D3, D4, D5, WA1, WA2, WA3,
                        WR, U, nmax, V, VP, TAU, Z, work, lwork, iwork, liwork, result, info);

                if (info[0] != 0) {
                    MipavUtil.displayError("dckkst had info = " + info[0]);
                }
            } // if (tstchk)
        } // for (i = 1; i <= nparms; i++)
    } // dckkst_test
    
    /**
     * This is a port of the porions of LAPACK version 3.4.0 test routine DCHKST used to test the symmetric eigenvalue
     * routines dstebz and dstein. Original DCHKST created by Univ. of Tennessee, Univ. of California Berkeley,
     * University of Colorado Denver, and NAG Ltd., November, 2011.
     * 
     * <p>
     * dstebz computes selected eigenvalues.  WA1, WA2, and WA3 will denote eigenvalues computed to high absolute
     * accuracy, with different range options.  WR will denote eigenvalues computed to high relative accuracy.
     * </p>
     * 
     * <p>
     * dstein computes Y, the eigenvectors of S, given the eigenvalues.
     * </p>
     * 
     * <p>
     * When dchkst is called, a number of matrix "sizes" ("n's") and a number of matrix "types" are specified. For each
     * size ("n") and each type of matrix, one matrix will be generated and used to test the symmetric eigenroutines.
     * For each matrix, a number of tests will be performed:
     * 1.) When S is also diagonally dominant by the factor gamma < 1,
     *   max | D4(i) - WR(i) | / ( |D4(i)| omega ) ,
         i
         omega = 2 (2n-1) ULP (1 + 8 gamma**2) / (1 - gamma)**4
         dstebz( 'A', 'E', ...)

         (2) | WA1 - D3 | / ( |D3| ulp ) dstebz( 'A', 'E', ...)

         (3) ( max { min | WA2(i)-WA3(j) | } +
                 i     j
             max { min | WA3(i)-WA2(j) | } ) / ( |D3| ulp )
                    i     j                                   dstebz( 'I', 'E', ...)

         (4) | S - Y WA1 Y' | / ( |S| n ulp ) dstebz, dstein

         (25) | I - Y Y' | / ( n ulp ) dstebz, dstein
     * The "sizes" are specified by an array nn(0:nsizes-1); the value
     * of each element nn[j] specifies one size. The "types" are specified by a boolean array dotype(0:ntypes-1); if
     * dotype[j] is true, then the matrix type "j" will be generated. Currently, the list of possible types is: (1) The
     * zero matrix. (2) The identity matrix. (3) A diagonal matrix with evenly spaced entries 1, ..., ulp and random
     * signs. (ulp = (first number larger than 1) - 1 ) (4) A diagonal matrix with geomtrically spaced entries 1, ...,
     * ulp and random signs. (5) A diagonal matrix with "clustered" entries 1, ulp, ..., ulp and random signs. (6) Same
     * as (4), but multiplied by sqrt( overflow threshold ) (7) Same as (4), but multiplied by sqrt( underflow threshold
     * ) (8) A matrix of the form U' D U, where U is orthogonal and D has evenly spaced entries 1, ..., ulp with random
     * signs on the diagonal. (9) A matrix of the form U' D U, where U is orthogonal and D has geometrically spaced
     * entries 1, ..., ulp with random signs on the diagonal. (10) A matrix of the form U' D U, where U is orthogonal
     * and D has "clustered" entries 1, ulp, ..., ulp with random signs on the diagonal. (11) Same as (8), but
     * multiplied by sqrt( overflow threshold) (12) Same as (8), but multiplied by sqrt( underflow threshold) (13)
     * Symmetric matrix with random entries chosen from (-1,1). (14) Same as (13), but multiplied by sqrt( overflow
     * threshold) (15) Same as (13), but multiplied by sqrt( underflow threshold) (16) Same as (8), but diagonal
     * elements are all positive. (17) Same as (9), but diagonal elements are all positive. (18) Same as (10), but
     * diagonal elements are all positive. (19) Same as (16), but multiplied by sqrt( overflow threshold) (20) Same as
     * (16), but multiplied by sqrt( underflow threshold) (21) A diagonally dominant tridiagonal matrix with
     * geometrically spaced diagonal entries 1, ..., ulp.
     * </p>
     * 
     * @param nsizes (input) int The number of sizes of matrices to use. If it is zero, dchkst does nothing. It must be
     *            at least zero.
     * @param nn (input) int[] of dimension (nsizes) An array containing the sizes to be used for the matrices. Zero
     *            values will be skipped. The values must be at least zero.
     * @param ntypes (input) int The number of elements in dotype. If it is zero, dchkst does nothing. It must be at
     *            least zero. If it is maxtyp+1 and nsizes is 1, then an additional type, maxtyp+1 is defined, which is
     *            to use whatever matrix is in A. This is only useful if dotype(0:maxtyp-1) is false and dotype[maxtyp]
     *            is true.
     * @param dotype (input) boolean[] of dimension (ntypes) If dotype[j] is true, then for each size in nn a matrix of
     *            that size and of type j will be generated. If ntypes is smaller than the maximum number of types
     *            defined (parameter maxtyp), then types ntypes+1 through maxtyp will not be generated. If ntypes is
     *            larger than maxtyp, dotype[maxtyp] through dotype[ntypes-1] will be ignored.
     * @param iseed (input/output) int[] of dimension (4) On entry iseed specifies the seed of the random number
     *            generator. The array elements should be between 0 and 4095; if not they will be reduced mod 4096.
     *            Also, iseed[3] must be odd. The random number generator uses an linear congruential sequence limited
     *            to small integers, and so should produce machine independent random numbers. The values of iseed are
     *            changed on exit, and can be used in the next call to dchkst to continue the same random number
     *            sequence.
     * @param thresh (input) double A test will count as "failed" if the "error", computed as described above, exceeds
     *            thresh. Note that the error is scaled to be O(1), so thresh should be a reasonably small multiple of
     *            1, e.g., 10 or 100. In particular, it should not depend on the precision (single vs. double) or the
     *            size of the matrix. It must be at least zero.
     * @param A (input/workspace/output) double[][] of dimension (lda, max(nn)) Used to hold the matrix whose
     *            eigenvalues are to be computed. On exit, A contains the last matrix actually used.
     * @param lda (input) int The leading dimension of A. It must be at least 1 and at least max(nn).
     * @param AP (workspace) double[] of dimension (max(nn)*max(nn+1)/2) The matrix A stored in packed format.
     * @param SD (workspace/output) double[] of dimension (max(nn)) The diagonal of the tridiagonal matrix computed by
     *            dsytrd. On exit, SD and SE contain the tridiagonal form of the matrix in A.
     * @param SE (workspace/output) double[] of dimension (max(nn)) The off-diagonal of the tridiagonal matrix computed
     *            by dsytrd. On exit, SD and SE contain the tridiagonal form of the matrix in A.
     * @param D1 (workspace/output) double[] of dimension (max(nn)) The eigenvalues of A, as computed by dsteqr
     *            simultaneously with Z. On exit, the eigenvalues in D1 correspond with the matrix in A.
     * @param D2 (workspace/output) double[] of dimension (max(nn)) The eigenvalues of A, as computed by dsteqr if Z is
     *            not computed. On exit, the eigenvalues in D2 correspond with the matrix in A.
     * @param D3 (workspace/output) double[] of dimension max(nn)) The eigenvalues of A, as computed by dsterf. On exit,
     *            the eigenvalues in D3 correspond with the matrix in A.
     * @param D4 double[] (out) of dimension max(nn).
     * @param D5 double[]
     * @param WA1 double[] (output) of dimension max(nn).  All eigenvalues of A, computed to high absolute accuracy,
     *             with different range options as computed by dstebz.
     * @param WA2 double[] (output) of dimension max(nn).  Selected eigenvalues of A, computed to high absolute
     *             accuracy, with different range options as computed by dstebz.  Choose random values for il and iu,
     *             and ask fo rthe il-th through iu-th eigenvalues.
     * @param WA3 double[] (output) of dimension max(nn).  Selected eigenvalues of A, computed to high absolute 
     *             accuracy, with different range options as computed by dstebz.  Determine the values of vl and vu
     *             of the il-th and iu-th eigenvalues and ask for all eigenvalues in thsi range.
     * @param WR double[] (output) of dimension max(nn).  ALl eigenvalues of A, computed to high absolute accuracy,
     *             with different options, as computed by dstebz.
     * @param U (workspace/output) double[][] of dimension (ldu, max(nn)) The orthogonal matrix computed by dsytrd +
     *            dorgtr.
     * @param ldu (input) int The leading dimension of U, Z and V. It must be at least 1 and at least max(nn).
     * @param V (workspace/output) double[][] of dimension (ldu, max(nn)) The Householder vectors computed by dsytrd in
     *            reducing A to tridiagonal form. The vectors computed with uplo = 'U' are in the upper triangle, and
     *            the vectors computed with uplo = 'L' are in the lower triangle. (As described in dsytrd, the sub- and
     *            superdiagonal are not set to 1, although the true Householder vector has a 1 in that position. The
     *            routines that use V, such as dorgtr, set those entries to 1 before using them, and then restore them
     *            later.)
     * @param VP (workspace) double[] of dimension(max(nn)*max(nn+1)/2) The matrix V stored in packed format.
     * @param tau (workspace/output) double[] of dimension max(nn) The Householder factors computed by dsytrd in
     *            reducing A to tridiagonal form.
     * @param Z (workspace/output) double[][] of dimension (ldu, max(nn)) The orthogonal matrix of eigenvectors computed
     *            by dsteqr and dstein.
     * @param work (workspace/output) double[] of dimension (lwork)
     * @param lwork (input) int The number of entries in work. This must be at least 1 + 4*nmax + 2 * nmax * lg nmax + 3
     *            * nmax**2 where nmax = max(nn[j], 2) and lg = log base 2.
     * @param iwork (workspace/output) int[] dimension liwork
     * @param liwork (input) int length of iwork  This must be at least (6+ 6*nmax + 5 * nmax * lg nmax) 
     *            where nmax = max(nn[j], 2) and lg = log base 2
     * @param result (output) double[] of dimension (26) The values computed by the tests described above. The values
     *            are currently limited to 1/ulp, to avoid overflow.
     * @param info (output) int[] If 0, then everything ran OK. -1: nsizes < 0 -2: Some nn[j] < 0 -3: ntypes < 0 -5:
     *            thresh < 0 -9: lda < 1 or lda < nmax, where nmax is max(nn[j]) -23: ldu < 1 or ldu < nmax -29: lwork
     *            too small. If dlatmr, dlatms, dsytrd, dorgtr, dsteqr, dsterf, or dormc2 returns an error code, the
     *            absolute value of it is returned.
     */
    private void dchkst(final int nsizes, final int[] nn, final int ntypes, final boolean[] dotype, final int[] iseed,
            final double thresh, final double[][] A, final int lda, final double[] AP, final double[] SD,
            final double[] SE, final double[] D1, final double[] D2, final double[] D3, final double[] D4,
            final double[] D5, final double[] WA1, final double[] WA2, final double[] WA3, final double[] WR,
            final double[][] U, final int ldu, final double[][] V, final double[] VP, final double[] tau,
            final double[][] Z, final double[] work, final int lwork, final int[] iwork, final int liwork,
            final double[] result, final int[] info) {
        final int maxtyp = 21; // The number of types defined.
        boolean badnn;
        int i;
        final int[] iinfo = new int[1];
        int il;
        int imode; // Values to be passed to the matrix generators
        int itemp;
        int itype;
        int iu;
        int j;
        int jc;
        int jr;
        int jsize;
        int jtype;
        int lgn;
        int m[] = new int[1]; // The actual number of eigenvalues found
        int m2[] = new int[1]; // The actual number of eigenvalues found
        int m3[] = new int[1]; // The actual number of eigenvalues found
        int mtypes;
        int n;
        int nblock; // Blocksize as returned by envir.
        int nerrs; // The number of tests which have exceeded thresh so far
        int nmats; // The number of matrices generated so far.
        int nmax; // Largest value in nn.
        int nsplit[] = new int[1]; // The number of diagonal blocks in the matrix T
        int ntest; // The number of tests performed, or which can be performed
        // so far, for the current matrix.
        int ntestt; // The total number of tests performed so far.
        double abstol;
        double aninv;
        double anorm = 0.0; // Norm of A; passed to matrix generators.
        double cond; // Values to be passed to the matrix generators.
        final double[] ovfl = new double[1]; // Overflow threshold.
        double rtovfl; // Square root of ovfl;
        double rtunfl; // Square root of unfl
        double temp1;
        double temp2;
        double temp3;
        double ulp; // Finest relative precision
        double ulpinv; // Inverse of finest relative precision
        final double[] unfl = new double[1]; // Underflow threshold
        double vl;
        double vu;
        final int[] idumma = new int[1];
        final int[] ioldsd = new int[4];
        final int[] iseed2 = new int[4];

        // The general type (1-10) for type "j".
        final int[] ktype = new int[] {1, 2, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 8, 8, 8, 9, 9, 9, 9, 9, 10};

        // The order of magnitude ( O(1), O(overflow^(1/2), O(underflow^(1/2) )
        final int[] kmagn = new int[] {1, 1, 1, 1, 1, 2, 3, 1, 1, 1, 2, 3, 1, 2, 3, 1, 1, 1, 2, 3, 1};

        // The mode value to be passed to the matrix generator for type "j".
        final int[] kmode = new int[] {0, 0, 4, 3, 1, 4, 4, 4, 3, 1, 4, 4, 0, 0, 0, 4, 3, 1, 4, 4, 3};
        final double[] dumma = new double[1];
        String name;
        String opts;
        int[] iwork2;
        int[] iwork3;
        int[] iwork4;
        double[] work2;
        double[] work3;
        final double[] res = new double[2];

        idumma[0] = 1;
        ntestt = 0;
        info[0] = 0;

        badnn = false;
        nmax = 1;

        for (j = 0; j < nsizes; j++) {
            nmax = Math.max(nmax, nn[j]);

            if (nn[j] < 0) {
                badnn = true;
            }
        } // for (j = 0; j < nsizes; j++)

        iwork2 = new int[nmax];
        iwork3 = new int[3 * nmax];
        iwork4 = new int[nmax];
        work2 = new double[3 * nmax];
        work3 = new double[nmax];

        name = new String("DSYTRD");
        opts = new String("L");
        nblock =ge.ilaenv(1, name, opts, nmax, -1, -1, -1);
        nblock = Math.min(nmax, Math.max(1, nblock));

        // Check for errors
        if (nsizes < 0) {
            info[0] = -1;
        } else if (badnn) {
            info[0] = -2;
        } else if (ntypes < 0) {
            info[0] = -3;
        } else if (lda < nmax) {
            info[0] = -9;
        } else if (ldu < nmax) {
            info[0] = -23;
        } else if ( (2 * Math.max(2, nmax) * Math.max(2, nmax)) > lwork) {
            info[0] = -29;
        }

        if (info[0] != 0) {
            MipavUtil.displayError("Error dchkst had info[0] = " + info[0]);

            return;
        }

        // Quick return if possible
        if ( (nsizes == 0) || (ntypes == 0)) {
            return;
        }

        unfl[0] = ge.dlamch('S');
        ovfl[0] = 1.0 / unfl[0];
        ge.dlabad(unfl, ovfl);
        ulp = ge.dlamch('E') * ge.dlamch('B');
        ulpinv = 1.0 / ulp;
        rtunfl = Math.sqrt(unfl[0]);
        rtovfl = Math.sqrt(ovfl[0]);

        // Loop over sizes, types

        for (i = 0; i < 4; i++) {
            iseed2[i] = iseed[i];
        }

        nerrs = 0;
        nmats = 0;

        for (jsize = 1; jsize <= nsizes; jsize++) {
            n = nn[jsize - 1];

            if (n > 0) {
                lgn = (int) (Math.log((double) n) / Math.log(2.0));

                if (Math.pow(2.0, lgn) < n) {
                    lgn = lgn + 1;
                }

                if (Math.pow(2.0, lgn) < n) {
                    lgn = lgn + 1;
                }
            } // if (n > 0)

            aninv = 1.0 / (double) (Math.max(1, n));

            if (nsizes != 1) {
                mtypes = Math.min(maxtyp, ntypes);
            } else {
                mtypes = Math.min(maxtyp + 1, ntypes);
            }

            for (jtype = 1; jtype <= mtypes; jtype++) {

                if ( !dotype[jtype - 1]) {
                    continue;
                }

                nmats = nmats + 1;
                ntest = 0;

                for (j = 0; j < 4; j++) {
                    ioldsd[j] = iseed[j];
                }

                // Compute "A"
                // Control parameters:
                /*
                 * kmagn kmode       ktype 
               = 1 O(1)  clustered 1 zero 
               = 2 large clustered 2 identity 
               = 3 small exponential (none) 
               = 4       arithmetic  diagonal, (w/ eigenvalues) 
               = 5       random log  symmetric, w/ eigenvalues 
               = 6       random      (none) 
               = 7                   random diagonal 
               = 8                   random symmetric 
               = 9                   positive definite 
               = 10                  diagonally dominant tridiagonal
                 */

                if (mtypes <= maxtyp) {
                    itype = ktype[jtype - 1];
                    imode = kmode[jtype - 1];

                    // Compute norm

                    if (kmagn[jtype - 1] == 1) {
                        anorm = 1.0;
                    } else if (kmagn[jtype - 1] == 2) {
                        anorm = (rtovfl * ulp) * aninv;
                    } else {
                        anorm = rtunfl * n * ulpinv;
                    }

                    ge.dlaset('F', lda, n, 0.0, 0.0, A, lda);
                    iinfo[0] = 0;

                    if (jtype <= 15) {
                        cond = ulpinv;
                    } else {
                        cond = ulpinv * aninv / 10.0;
                    }

                    // Special Matrices -- Identity & Jordan block

                    // Zero
                    if (itype == 1) {
                        iinfo[0] = 0;
                    } // if (itype == 1)
                    else if (itype == 2) {

                        // Identity
                        for (jc = 0; jc < n; jc++) {
                            A[jc][jc] = anorm;
                        }
                    } // else if (itype == 2)
                    else if (itype == 4) {

                        // Diagonal Matrix, [Eigen]values, Specified
                        ge.dlatms(n, n, 'S', iseed, 'S', work, imode, cond, anorm, 0, 0, 'N', A, lda, work2, iinfo);
                    } // else if (itype == 4)
                    else if (itype == 5) {

                        // Symmetric, eigenvalues specified
                        ge.dlatms(n, n, 'S', iseed, 'S', work, imode, cond, anorm, n, n, 'N', A, lda, work2, iinfo);
                    } // else if (itype == 5)
                    else if (itype == 7) {

                        // Diagonal, random eigenvalues
                        ge.dlatmr(n, n, 'S', iseed, 'S', work, 6, 1.0, 1.0, 'T', 'N', work2, 1, 1.0, work3, 1, 1.0, 'N',
                                idumma, 0, 0, 0.0, anorm, 'N', A, lda, iwork, iinfo);
                    } // else if (itype == 7)
                    else if (itype == 8) {

                        // Symmetric, random eigenvalues
                        ge.dlatmr(n, n, 'S', iseed, 'S', work, 6, 1.0, 1.0, 'T', 'N', work2, 1, 1.0, work3, 1, 1.0, 'N',
                                idumma, n, n, 0.0, anorm, 'N', A, lda, iwork, iinfo);
                    } // else if (itype == 8)
                    else if (itype == 9) {

                        // Positive definite, eigenvalues specified
                        ge.dlatms(n, n, 'S', iseed, 'P', work, imode, cond, anorm, n, n, 'N', A, lda, work2, iinfo);
                    } // else if (itype == 9)
                    else if (itype == 10) {

                        // Positive definite tridiagonal, eigenvalues specified
                        ge.dlatms(n, n, 'S', iseed, 'P', work, imode, cond, anorm, 1, 1, 'N', A, lda, work2, iinfo);

                        for (i = 1; i < n; i++) {
                            temp1 = Math.abs(A[i - 1][i]) / Math.sqrt(Math.abs(A[i - 1][i - 1] * A[i][i]));

                            if (temp1 > 0.5) {
                                A[i - 1][i] = 0.5 * Math.sqrt(Math.abs(A[i - 1][i - 1] * A[i][i]));
                                A[i][i - 1] = A[i - 1][i];
                            } // if (temp1 > 0.5)
                        } // for (i = 1; i < n; i++)
                    } // else if (itype == 10)
                    else {
                        iinfo[0] = 1;
                    } // else

                    if (iinfo[0] != 0) {
                        UI.setDataText("Generator iinfo[0] = " + iinfo[0] + "\n");
                        UI.setDataText("n = " + n + "\n");
                        UI.setDataText("jtype = " + jtype + "\n");
                        UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                        UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                        UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                        UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                        info[0] = Math.abs(iinfo[0]);

                        return;
                    } // if (iinfo[0] != 0)
                } // if (mtypes <= maxtyp)

                loop1: {
                    // Call dstebz with different options and do tests 1-2
                    
                    // If S is positive definite and diagonally dominant,
                    // ask for all eigenvalues with high relative accuracy.
                    vl = 0.0;
                    vu = 0.0;
                    il = 0;
                    iu = 0;
                    if (jtype == 21) {
                        ntest = 1;
                        abstol = unfl[0] + unfl[0];
                        dstebz('A', 'E', n , vl, vu, il, iu, abstol, SD, SE, m, nsplit, WR, iwork, iwork2,
                               work, iwork3, iinfo);
                        if (iinfo[0] != 0) {
                            UI.setDataText("dstebz(A, rel) iinfo[0] = " + iinfo[0] + "\n");
                            UI.setDataText("n = " + n + "\n");
                            UI.setDataText("jtype = " + jtype + "\n");
                            UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                            UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                            UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                            UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                            info[0] = Math.abs(iinfo[0]);

                            if (iinfo[0] < 0) {
                                return;
                            } else {
                                result[0] = ulpinv;

                                break loop1;
                            }
                        } // if (iinfo[0] != 0)
                        
                        // Do test 1
                        
                        temp2 = 96.0 *(2.0*n - 1.0) * ulp;
                        temp1 = 0.0;
                        for (j = 1; j <= n; j++) {
                            temp1 = Math.max(temp1, Math.abs(D4[j-1] -WR[n-j])/ 
                                    (abstol+ Math.abs(D4[j-1])));
                            result[0] = temp1/temp2;
                        } // for (j = 1; j <= n; j++)
                    } // if (jtype == 21)
                    else {
                        result[0] = 0.0;
                    }
                    
                    // Now ask fo rall eigenvalues with high absolute accuracy.
                    ntest = 2;
                    abstol = unfl[0] + unfl[0];
                    dstebz('A', 'E', n , vl, vu, il, iu, abstol, SD, SE, m, nsplit, WA1, iwork, iwork2,
                            work, iwork3, iinfo);
                     if (iinfo[0] != 0) {
                         UI.setDataText("dstebz(A) iinfo[0] = " + iinfo[0] + "\n");
                         UI.setDataText("n = " + n + "\n");
                         UI.setDataText("jtype = " + jtype + "\n");
                         UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                         UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                         UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                         UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                         info[0] = Math.abs(iinfo[0]);

                         if (iinfo[0] < 0) {
                             return;
                         } else {
                             result[1] = ulpinv;

                             break loop1;
                         }
                     } // if (iinfo[0] != 0)
                     
                     // Do test 18
                     
                     temp1 = 0.0;
                     temp2 = 0.0;
                     for (j = 0; j < n; j++) {
                         temp1 = Math.max(temp1,  Math.max(Math.abs(D3[j]), Math.abs(WA1[j])));  
                         temp2 = Math.max(temp2, Math.abs(D3[j] - WA1[j]));
                     } // for (j = 0; j < n; j++)
                     
                     result[1] = temp2/Math.max(unfl[0], ulp*Math.max(temp1, temp2));
                     
                     // Choose random values for il and iu, and ask for the
                     // il-th through iu-th eigenvalues.
                     
                     ntest = 3;
                     if (n <= 1) {
                         il = 1;
                         iu = n;
                     }
                     else {
                         il = 1 + (n-1)*(int)ge.dlarnd(1, iseed2);
                         iu = 1 + (n-1)*(int)ge.dlarnd(1, iseed2);
                         if (iu < il) {
                             itemp = iu;
                             iu = il;
                             il = itemp;
                         }
                     }
                     dstebz('I', 'E', n , vl, vu, il, iu, abstol, SD, SE, m2, nsplit, WA2, iwork, iwork2,
                             work, iwork3, iinfo);
                     if (iinfo[0] != 0) {
                          UI.setDataText("dstebz(I) iinfo[0] = " + iinfo[0] + "\n");
                          UI.setDataText("n = " + n + "\n");
                          UI.setDataText("jtype = " + jtype + "\n");
                          UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                          UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                          UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                          UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                          info[0] = Math.abs(iinfo[0]);

                          if (iinfo[0] < 0) {
                              return;
                          } else {
                              result[2] = ulpinv;

                              break loop1;
                          }
                     } // if (iinfo[0] != 0)
                      
                      // Determine the values vl and vu of the il-th and iu-th
                      // eigenvalues and ask for all eigenvalues in this range.
                      
                      if (n > 0) {
                          if (il != 1) {
                              vl = WA1[il-1] - Math.max(0.5*(WA1[il-1] - WA1[il-2]), 
                                   Math.max(ulp*anorm, 2.0*rtunfl));
                          }
                          else {
                              vl = WA1[0] - Math.max(0.5*(WA1[n-1] - WA1[0]), 
                                      Math.max(ulp*anorm, 2.0*rtunfl));    
                          }
                          if (iu != n) {
                              vu = WA1[iu-1] + Math.max(0.5*(WA1[iu] - WA1[iu-1]), 
                                      Math.max(ulp*anorm, 2.0*rtunfl));    
                          }
                          else {
                              vu = WA1[n-1] + Math.max(0.5*(WA1[n-1] - WA1[0]), 
                                      Math.max(ulp*anorm, 2.0*rtunfl));     
                          }
                      } // if (n > 0)
                      else {
                          vl = 0.0;
                          vu = 1.0;
                      }
                      dstebz('V', 'E', n , vl, vu, il, iu, abstol, SD, SE, m3, nsplit, WA3, iwork, iwork2,
                              work, iwork3, iinfo);
                      if (iinfo[0] != 0) {
                           UI.setDataText("dstebz(V) iinfo[0] = " + iinfo[0] + "\n");
                           UI.setDataText("n = " + n + "\n");
                           UI.setDataText("jtype = " + jtype + "\n");
                           UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                           UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                           UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                           UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                           info[0] = Math.abs(iinfo[0]);

                           if (iinfo[0] < 0) {
                               return;
                           } else {
                               result[2] = ulpinv;

                               break loop1;
                           }
                      } // if (iinfo[0] != 0)
                      
                      if (m3[0] == 0 && n != 0) {
                          result[2] = ulpinv;
                          break loop1;
                      }
                      
                      // Do test 3
                      
                      temp1 = dsxt1(1, WA2, m2[0], WA3, m3[0], abstol, ulp, unfl[0]);
                      temp2 = dsxt1(1, WA3, m3[0], WA2, m2[0], abstol, ulp, unfl[0]);
                      if (n > 0) {
                          temp3 = Math.max(Math.abs(WA1[n-1]), Math.abs(WA1[0]));
                      }
                      else {
                          temp3 = 0.0;
                      }
                      
                      result[2] = (temp1 + temp2)/Math.max(unfl[0], temp3 * ulp);
                      
                      // Call dstein to compute eigenvectors corresponding to
                      // eigenvalues in WA1.  (First call dstebz again, to make sure
                      // it returns these eigenvalues in the correct order.
                      
                      ntest = 5;
                      dstebz('A', 'B', n , vl, vu, il, iu, abstol, SD, SE, m, nsplit, WA1, iwork, iwork2,
                              work, iwork3, iinfo);
                      if (iinfo[0] != 0) {
                           UI.setDataText("dstebz(A,B) iinfo[0] = " + iinfo[0] + "\n");
                           UI.setDataText("n = " + n + "\n");
                           UI.setDataText("jtype = " + jtype + "\n");
                           UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                           UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                           UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                           UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                           info[0] = Math.abs(iinfo[0]);

                           if (iinfo[0] < 0) {
                               return;
                           } else {
                               result[3] = ulpinv;
                               result[4] = ulpinv;

                               break loop1;
                           }
                      } // if (iinfo[0] != 0)
                      
                      dstein(n, SD, SE, m[0], WA1, iwork, iwork2, Z, ldu, work, iwork3, iwork4, iinfo);
                      if (iinfo[0] != 0) {
                          UI.setDataText("dstein iinfo[0] = " + iinfo[0] + "\n");
                          UI.setDataText("n = " + n + "\n");
                          UI.setDataText("jtype = " + jtype + "\n");
                          UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                          UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                          UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                          UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                          info[0] = Math.abs(iinfo[0]);

                          if (iinfo[0] < 0) {
                              return;
                          } else {
                              result[3] = ulpinv;
                              result[4] = ulpinv;

                              break loop1;
                          }
                      } // if (iinfo[0] != 0)
                      
                      // Do tests 4 and 5
                      
                      ge.dstt21(n, 0, SD, SE, WA1, dumma, Z, ldu, work, res);
                      result[4] = res[0];
                      result[5] = res[1];
                } // loop1

                ntestt = ntestt + ntest;

                // End of loop -- Check for result[j] > thresh

                for (jr = 0; jr < ntest; jr++) {

                    if (result[jr] >= thresh) {

                        // If this is the first data to fail,
                        // print a header to the data file.
                        if (nerrs == 0) {
                            UI.setDataText("DST -- Real Symmetric eigenvalue problem\n");
                            UI.setDataText("Matrix types (see dchkst for details):\n");
                            UI.setDataText("Special matrices:\n");
                            UI.setDataText("1 = Zero matrix\n");
                            UI.setDataText("2 = Identity matrix\n");
                            UI.setDataText("3 = Diagonal: evenly spaced entries\n");
                            UI.setDataText("4 = Diagonal: geometrically spaced entries\n");
                            UI.setDataText("5 = Diagonal: clustered entries\n");
                            UI.setDataText("6 = Diagonal: large, evenly spaced\n");
                            UI.setDataText("7 = Diagonal: small, evenly spaced\n");
                            UI.setDataText("Dense Symmetric Matrices\n");
                            UI.setDataText("8 = Evenly spaced eigenvalues\n");
                            UI.setDataText("9 = Geometrically spaced eigenvalues\n");
                            UI.setDataText("10 = Clustered eigenvalues\n");
                            UI.setDataText("11 = Large, evenly spaced eigenvalues\n");
                            UI.setDataText("12 = Small, evenly spaced eigenvalues\n");
                            UI.setDataText("13 = Matrix with random O(1) entries\n");
                            UI.setDataText("14 = Matrix with large random entries\n");
                            UI.setDataText("15 = Matrix with small random entries\n");
                            UI.setDataText("16 = Positive definite, evenly spaced eigenvalues\n");
                            UI.setDataText("17 = Positive definite, geometrically spaced eigenvalues\n");
                            UI.setDataText("18 = Positive definite, clustered eigenvalues\n");
                            UI.setDataText("19 = Positive definite, small evenly spaced eigenvalues\n");
                            UI.setDataText("20 = Positive definite, large evenly spaced eigenvalues\n");
                            UI.setDataText("21 = Diagonally dominant tridiagonal,\n");
                            UI.setDataText("     geometrically, spaced eigenvalues\n");

                            // Tests performed
                            UI.setDataText("Tests performed: see dchkst for details\n");
                        } // if (nerrs == 0)

                        nerrs = nerrs + 1;
                        UI.setDataText("n = " + n + "\n");
                        UI.setDataText("ioldsd[0] = " + ioldsd[0] + "\n");
                        UI.setDataText("ioldsd[1] = " + ioldsd[1] + "\n");
                        UI.setDataText("ioldsd[2] = " + ioldsd[2] + "\n");
                        UI.setDataText("ioldsd[3] = " + ioldsd[3] + "\n");
                        UI.setDataText("jtype = " + jtype + "\n");
                        UI.setDataText("result[" + jr + "] = " + result[jr] + "\n");
                    } // if (result[jr] >= thresh)
                } // for (jr = 0; jr < ntest; jr++)
            } // for (jtype = 1; jtype <= mtypes; jtype++)
        } // for (jsize = 1; jsize <= nsizes; jsize++)

        // Summary
        if (nerrs > 0) {
            UI.setDataText("dchkst " + nerrs + " out of " + ntestt + " tests failed to pass the threshold\n");
        } else {
            UI.setDataText("All " + ntestt + " tests for dchkst passed the threshold\n");
        }

        return;
    } // dchkst
    
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
    
 public void dsyevx(final char jobz, final char range, final char uplo, final int n, final double[][]A, final int lda, 
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
 int lwkopt = 0;
 int iscale;
 double abstll;
 double vll = 0.0;
 double vuu = 0.0;
 double smlnum;
 double bignum;
 double eps;
 double rmin;
 double rmax;
 double anrm;
 double sigma = 0.0;
 int i, j, k;
 double vec1[];
 double vec3[];
 double vecindd[];
 double vecinde[];
 double vecindwrk[];
 double vecindee[];
 int ivec2[];
 int ivec3[];
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
 int indisp;
 int nsplit[] = new int[1];
 int indwkn;
 int llwrkn;
 int imax;
 int jj;
 double tmp1;
 int itmp1;
 double temp;

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
    if (n <= 1 ) {
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

 // Call dsytrd to reduce symmetric matrix to tridiagonal form.

 indtau = 1;
 inde = indtau + n;
 indd = inde + n;
 indwrk = indd + n;
 indee = indwrk + 2*n;
 llwork = lwork - indwrk + 1;
 vecindd = new double[n];
 vecinde = new double[n];
 vecindwrk = new double[2*n];
 vecindee = new double[n-1];
 ge.dsytrd(uplo, n, A, lda, vecindd, vecinde, work, vecindwrk, llwork, iinfo);

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
         w[i] = vecindd[i];
     }
    if (!wantz) {
       for (i = 0; i < n-1; i++) {
           vecindee[i] = vecinde[i];
       }
       ge.dsterf( n, w, vecindee, info);
    }
    else {
       ge.dlacpy( 'A', n, n, A, lda, Z, ldz);
       ge.dorgtr(uplo, n, Z, ldz, work, vecindwrk, llwork, iinfo);
       for (i = 0; i < n-1; i++) {
           vecindee[i] = vecinde[i];
       }
       ge.dsteqr(jobz, n, w, vecindee, Z, ldz, vecindwrk, info);
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
     indisp = 1 + n;
     ivec2 = new int[n];
     vec3 = new double[4*n];
     ivec3 = new int[3*n];
     dstebz(range, order, n, vll, vuu, il, iu, abstll,
            vecindd, vecinde, m, nsplit, w,
            iwork, ivec2, vec3, ivec3, info);
     for (i = 0; i < n; i++) {
         iwork[indisp-1+i] = ivec2[i];
     }
     if (wantz) {
         vec3 = new double[5*n];
         ivec3 = new int[n];
         dstein(n, vecindd, vecinde, m[0], w, iwork, ivec2, Z, ldz,
               vec3, ivec3, ifail, info);
 
         // Apply orthogonal matrix used in reduction to tridiagonal
         // form to eigenvectors returned by DSTEIN.
 
        indwkn = inde;
        llwrkn = lwork - indwkn + 1;
        dormtr( 'L', uplo, 'N', n, m[0], A, lda, work, Z, ldz, vecinde, llwrkn, iinfo);
     } // if (wantz)
 } // if (doHere)
 for (i = 0; i < n; i++) {
     work[inde-1+i] = vecinde[i];
     work[indd-1+i] = vecindd[i];
 }
 for (i = 0; i < 2*n; i++) {
     work[indwrk-1+i] = vecindwrk[i];
 }
 for (i = 0; i < n-1; i++) {
     work[indee-1+i] = vecindee[i];
 }

 // If matrix was scaled, then rescale eigenvalues appropriately.

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

 // If eigenvalues are not in order, then sort them, along with eigenvectors.

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
          itmp1 = iwork[i-1];
          w[i-1] = w[j-1];
          iwork[i-1] = iwork[j-1];
          w[j-1] = tmp1;
          iwork[j-1] = itmp1;
          for (k = 0; k < n; k++) {
              temp = Z[k][i-1];
              Z[k][i-1] = Z[k][j-1];
              Z[k][j-1] = temp;
          }
          if (info[0] != 0) {
             itmp1 = ifail[i-1];
             ifail[i-1] = ifail[j-1];
             ifail[j-1] = itmp1;
          } // if (info[0] != 0)
       } // if (i != 0)
    } // for (j = 1; j <= m[0] - 1; j++)
 } // if (wantz)

 // Set work[0] to optimal workspace size.

 work[0] = lwkopt;
 return;
 } // dsyevx
 
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
 int indtau;
 int indd;
 int inde;
 int inddd;
 int indee;
 int indwk;
 int indibl;
 int indisp;
 int indifl;
 int indiwo;
 int iscale;
 int lwmin;
 int liwmin;
 int llwork;
 int lwkopt;
 int nb;
 double abstll;
 double anrm;
 double eps;
 double smlnum;
 double bignum;
 double rmin;
 double rmax;
 double sigma = 0.0;
 double vec1[];
 double vll;
 double vuu;
 boolean lower;
 boolean wantz;
 boolean alleig;
 boolean valeig;
 boolean indeig;
 boolean lquery;
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
/*
 CALL DSYTRD( UPLO, N, A, LDA, WORK( INDD ), WORK( INDE ),
$             WORK( INDTAU ), WORK( INDWK ), LLWORK, IINFO )
*
*     If all eigenvalues are desired
*     then call DSTERF or DSTEMR and DORMTR.
*
 IF( ( ALLEIG .OR. ( INDEIG .AND. IL.EQ.1 .AND. IU.EQ.N ) ) .AND.
$    IEEEOK.EQ.1 ) THEN
    IF( .NOT.WANTZ ) THEN
       CALL DCOPY( N, WORK( INDD ), 1, W, 1 )
       CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
       CALL DSTERF( N, W, WORK( INDEE ), INFO )
    ELSE
       CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
       CALL DCOPY( N, WORK( INDD ), 1, WORK( INDDD ), 1 )
*
       IF (ABSTOL .LE. TWO*N*EPS) THEN
          TRYRAC = .TRUE.
       ELSE
          TRYRAC = .FALSE.
       END IF
       CALL DSTEMR( JOBZ, 'A', N, WORK( INDDD ), WORK( INDEE ),
$                   VL, VU, IL, IU, M, W, Z, LDZ, N, ISUPPZ,
$                   TRYRAC, WORK( INDWK ), LWORK, IWORK, LIWORK,
$                   INFO )
*
*
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by DSTEIN.
*
       IF( WANTZ .AND. INFO.EQ.0 ) THEN
          INDWKN = INDE
          LLWRKN = LWORK - INDWKN + 1
          CALL DORMTR( 'L', UPLO, 'N', N, M, A, LDA,
$                      WORK( INDTAU ), Z, LDZ, WORK( INDWKN ),
$                      LLWRKN, IINFO )
       END IF
    END IF
*
*
    IF( INFO.EQ.0 ) THEN
*           Everything worked.  Skip DSTEBZ/DSTEIN.  IWORK(:) are
*           undefined.
       M = N
       GO TO 30
    END IF
    INFO = 0
 END IF
*
*     Otherwise, call DSTEBZ and, if eigenvectors are desired, DSTEIN.
*     Also call DSTEBZ and DSTEIN if DSTEMR fails.
*
 IF( WANTZ ) THEN
    ORDER = 'B'
 ELSE
    ORDER = 'E'
 END IF

 CALL DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTLL,
$             WORK( INDD ), WORK( INDE ), M, NSPLIT, W,
$             IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWK ),
$             IWORK( INDIWO ), INFO )
*
 IF( WANTZ ) THEN
    CALL DSTEIN( N, WORK( INDD ), WORK( INDE ), M, W,
$                IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ,
$                WORK( INDWK ), IWORK( INDIWO ), IWORK( INDIFL ),
$                INFO )
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by DSTEIN.
*
    INDWKN = INDE
    LLWRKN = LWORK - INDWKN + 1
    CALL DORMTR( 'L', UPLO, 'N', N, M, A, LDA, WORK( INDTAU ), Z,
$                LDZ, WORK( INDWKN ), LLWRKN, IINFO )
 END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
*  Jump here if DSTEMR/DSTEIN succeeded.
30 CONTINUE
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
*     eigenvectors.  Note: We do not sort the IFAIL portion of IWORK.
*     It may not be initialized (if DSTEMR/DSTEIN succeeded), and we do
*     not return this detailed information to the user.
*
 IF( WANTZ ) THEN
    DO 50 J = 1, M - 1
       I = 0
       TMP1 = W( J )
       DO 40 JJ = J + 1, M
          IF( W( JJ ).LT.TMP1 ) THEN
             I = JJ
             TMP1 = W( JJ )
          END IF
40       CONTINUE
*
       IF( I.NE.0 ) THEN
          W( I ) = W( J )
          W( J ) = TMP1
          CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
       END IF
50    CONTINUE
 END IF
*
*     Set WORK(1) to optimal workspace size.
*
 WORK( 1 ) = LWKOPT
 IWORK( 1 ) = LIWMIN*/

 return;
} // dsyevr


 
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
  isplit[nsplit[0]-1] = n;
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
  
  /** This is a port of version 3.2 LAPACK routine DSTEIN.  The original DSTEIN is created by by Univ. of Tennessee,
  Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. on November, 2006.
  
   dstein computes the eigenvectors of a real symmetric tridiagonal
*  matrix T corresponding to specified eigenvalues, using inverse
*  iteration.
*
*  The maximum number of iterations allowed for each eigenvector is
*  specified by an internal parameter maxits (currently set to 5).
*  @param n input int The order of the matrix.  n >= 0.
*  @param d input double[] of dimension n.   The n diagonal elements of the tridiagonal matrix T.
*  @param e input double[] of dimension n-1. The (n-1) subdiagonal elements of the tridiagonal matrix
*          T, in elements 1 to N-1.
*  @param m input int The number of eigenvectors to be found.  0 <= m <= n.
*  @param w input double[] of dimension n. The first m elements of w contain the eigenvalues for
*          which eigenvectors are to be computed.  The eigenvalues should be grouped by split-off block and ordered from
*          smallest to largest within the block.  ( The output array w from dstebz with order = 'B' is expected here. )
*  @param iblock input int[] of dimension n  The submatrix indices associated with the corresponding
*          eigenvalues in w; iblock(i)=1 if eigenvalue w(i) belongs to
*          the first submatrix from the top, =2 if w(i) belongs to
*          the second submatrix, etc.  ( The output array iblock
*          from dstebz is expected here. 
*  @param isplit input int[] of dimension n The splitting points, at which T breaks up into submatrices.
*          The first submatrix consists of rows/columns 1 to
*          isplit( 1 ), the second of rows/columns isplit( 1 )+1
*          through isplit( 2 ), etc.
*          ( The output array isplit from dstebz is expected here. )
*  @param Z output double[][] of dimension (ldz, m)  The computed eigenvectors.  The eigenvector associated
*          with the eigenvalue w(i) is stored in the i-th column of
*          Z.  Any vector which fails to converge is set to its current
*          iterate after maxits iterations.
*  @param ldz input int The leading dimension of the array Z.  ldz >= max(1,n).
*  @param work workspace double[] of dimension (5*n).
*  @param iwork workspace int[] of dimension n.
*  @param ifail output int[] of dimension m  On normal exit, all elements of ifail are zero.
*          If one or more eigenvectors fail to converge after
*          maxits iterations, then their indices are stored in array ifail.
*  @param info output int[] of dimension 1.
*          = 0: successful exit.
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          > 0: if INFO = i, then i eigenvectors failed to converge
*               in MAXITS iterations.  Their indices are stored in
*               array IFAIL.
  */
  private void dstein(int n, double d[], double e[], int m, double w[], int iblock[], int isplit[], double Z[][], 
                      int ldz, double work[], int iwork[], int ifail[], int info[]) {

/*  Internal Parameters
*  ===================
*
*  MAXITS  INTEGER, default = 5
*          The maximum number of iterations performed.
*
*  EXTRA   INTEGER, default = 2
*          The number of iterations performed after norm growth
*          criterion is satisfied, should be at least 1.
*/
  int maxits = 5;
  int extra = 2;
  int i;
  int j;
  int k;
  double eps;
  int iseed[] = new int[4];
  int indrv2;
  int indrv3;
  int indrv4;
  int indrv5;
  int j1;
  int nblk;
  int b1;
  int bn;
  int gpind = 0;
  int blksiz;
  double onenrm = 0.0;
  double ortol = 0.0;
  double dtpcrt = 0.0;
  int jblk;
  double xj;
  double xjm = 0.0;
  double eps1;
  double pertol;
  double sep;
  int its;
  int nrmchk;
  double tol[] = new double[1];
  double vec1[];
  double vec2[];
  double vec3[];
  double vec4[];
  int iinfo[] = new int[1];
  boolean doSeg = true;
  double absSum;
  double scl;
  double ztr;
  int jmax;
  double nrm;
 
  // Test the input parameters.

  info[0] = 0;
  for (i = 1; i <= m; i++) {
     ifail[i-1] = 0;
  }

  if (n < 0) {
     info[0] = -1;
  }
  else if (m < 0 || m > n) {
     info[0] = -4;
  }
  else if (ldz < Math.max(1, n)) {
     info[0] = -9;
  }
  else {
     for (j = 2; j <= m; j++) {
        if (iblock[j-1] < iblock[j-2]) {
           info[0] = -6;
           break;
        }
        if (iblock[j-1] == iblock[j-2] && w[j-1] < w[j-2]) {
           info[0] = -5;
           break;
        }
     } // for (j = 2; j <= m; j++)
  } // else

  if (info[0] != 0) {
     MipavUtil.displayError("Error dstein had info[0] = " + info[0]);
     return;
  }

  // Quick return if possible

  if (n == 0 || m == 0) {
     return;
  }
  else if (n == 1) {
     Z[0][0] = 1.0;
     return;
  }

  // Get machine constants.

  eps = ge.dlamch('P');

  // Initialize seed for random number generator DLARNV.

  for (i = 1; i <= 4; i++) {
     iseed[i-1] = 1;
  }

  // Initialize pointers.

  indrv2 = n;
  indrv3 = indrv2 + n;
  indrv4 = indrv3 + n;
  indrv5 = indrv4 + n;

  // Compute eigenvectors of matrix blocks.

  j1 = 1;
  for (nblk = 1; nblk <= iblock[m-1]; nblk++) {

     // Find starting and ending indices of block nblk.

     if (nblk == 1) {
        b1 = 1;
     }
     else {
        b1 = isplit[nblk-2] + 1;
     }
     bn = isplit[nblk-1];
     blksiz = bn - b1 + 1;
     if (blksiz != 1) {
         gpind = b1;

         // Compute reorthogonalization criterion and stopping criterion.

         onenrm = Math.abs(d[b1-1]) + Math.abs(e[b1-1]);
         onenrm = Math.max(onenrm, Math.abs(d[bn-1])+Math.abs(e[bn-2]) );
         for (i = b1+1; i <= bn-1; i++) {
             onenrm = Math.max(onenrm, Math.abs(d[i-1])+Math.abs(e[i-2])+Math.abs(e[i-1]) );
         } // for (i = b1+1; i <= bn-1; i++)
         ortol = 1.0E-3 * onenrm;

         dtpcrt = Math.sqrt(1.0E-1/blksiz);
     } // if (blksiz != 1)

     // Loop through eigenvalues of block nblk.

     jblk = 0;
     for (j = j1; j <= m; j++) {
        if (iblock[j-1] != nblk) {
           j1 = j;
           break;
        }
        jblk = jblk + 1;
        xj = w[j-1];

        // Skip all the work if the block size is one.

        if (blksiz == 1) {
           work[0] = 1.0;
           for (i = 1; i <= n; i++) {
               Z[i-1][j-1] = 0.0;
           }
           for (i = 1; i <= blksiz; i++) { 
               Z[b1+i-2][j-1] = work[i-1];
           }
           
           // Save the shift to check eigenvalue spacing at next generation
           
           xjm = xj;
           
           continue;
        }

        // If eigenvalues j and j-1 are too close, add a relatively small perturbation.

        if (jblk > 1) {
           eps1 = Math.abs(eps * xj);
           pertol = 10.0 * eps1;
           sep = xj - xjm;
           if (sep < pertol) {
              xj = xjm + pertol;
           }
        } // if (jblk > 1)

        its = 0;
        nrmchk = 0;

        // Get random starting vector.

        ge.dlarnv( 2, iseed, blksiz, work);

        // Copy the matrix T so it won't be destroyed in factorization.

        for (i = 0; i < blksiz; i++) {
            work[indrv4+i] = d[b1-1+i];
        }
        for (i = 0; i < blksiz-1; i++) {
            work[indrv2+1+i] = e[b1-1+i];
            work[indrv3+i] = e[b1-1+i];
        }
        

        // Compute LU factors with partial pivoting  ( PT = LU )

        tol[0] = 0.0;
        vec1 = new double[blksiz];
        for (i = 0; i < blksiz; i++) {
            vec1[i] = work[indrv4+i];
        }
        vec2 = new double[blksiz-1];
        for (i = 0; i < blksiz-1; i++) {
            vec2[i] = work[indrv2+1+i];
        }
        vec3 = new double[blksiz-1];
        for (i = 0; i < blksiz-1; i++) {
            vec3[i] = work[indrv3+i];
        }
        vec4 = new double[blksiz-2];
        dlagtf(blksiz, vec1, xj, vec2, vec3, tol[0], vec4, iwork, iinfo);
        for (i = 0; i < blksiz; i++) {
            work[indrv4+i] = vec1[i];
        }
        for (i = 0; i < blksiz-1; i++) {
            work[indrv2+1+i] = vec2[i];
        }
        for (i = 0; i < blksiz-1; i++) {
            work[indrv3+i] = vec3[i];
        }
        for (i = 0; i < blksiz-2; i++) {
            work[indrv5+i] = vec4[i];
        }
        
        // Update iteration count.
        
        its++;
        for (; its <= maxits; its++) {
          
            // Normalize and scale the righthand side vector Pb.
    
            absSum = 0.0;
            for (i = 0; i < blksiz; i++) {
                absSum += Math.abs(work[i]);
            }
            scl = blksiz*onenrm*Math.max(eps, Math.abs(work[indrv4+blksiz-1] ) ) / absSum;
            for (i = 0; i < blksiz; i++) {
                work[i] *= scl;
            }
   
            // Solve the system LU = Pb.
    
            vec1 = new double[blksiz];
            for (i = 0; i < blksiz; i++) {
                vec1[i] = work[indrv4 + i];
            }
            vec2 = new double[blksiz-1];
            for (i = 0; i < blksiz-1; i++) {
                vec2[i] = work[indrv2+1+i];
            }
            vec3 = new double[blksiz-1];
            for (i = 0; i < blksiz-1; i++) {
                vec3[i] = work[indrv3 + i];
            }
            vec4 = new double[blksiz-2];
            for (i = 0; i < blksiz-2; i++) {
                vec4[i] = work[indrv5+i];
            }

            dlagts( -1, blksiz, vec1, vec2, vec3, vec4, iwork, work, tol, iinfo);
    
            // Reorthogonalize by modified Gram-Schmidt if eigenvalues are close enough.
    
            if (jblk != 1) {
                if (Math.abs(xj - xjm) > ortol) {
                   gpind = j;
                }
                if (gpind != j) {
                   for (i = gpind; i <= j-1; i++) {
                      for (k = 0; k < blksiz; k++) {
                          vec1[k] = Z[b1-1+k][i-1];
                      }
                      ztr = -ge.ddot(blksiz, work, 1, vec1, 1);
                      ge.daxpy(blksiz, ztr, vec1, 1, work, 1 );
                   } // for (i = gpind; i <= j-1; i++)
                } // if (gpind != j)
            } // if (jblk != 1)
    
            // Check the infinity norm of the iterate.
    
            jmax = 0;
            nrm = Math.abs(work[0]);
            for (i = 1; i < blksiz; i++) {
                if (Math.abs(work[i]) > nrm) {
                    nrm = Math.abs(work[i]);
                    jmax = i;
                }
            }
   
            // Continue for additional iterations after norm reaches stopping criterion.
    
            if (nrm < dtpcrt) {
                continue;
            }
            nrmchk = nrmchk + 1;
            if (nrmchk < extra+1) {
                continue;
            }
    
            doSeg = false;
            break;
        } // for (; its <= maxits; its++)
        
        if (doSeg) {

            // If stopping criterion was not satisfied, update info and
            // store eigenvector number in array ifail.

            info[0] = info[0] + 1;
            ifail[info[0]-1] = j;
        } // if (doSeg)

        // Accept iterate as jth eigenvector.

        scl = 1.0 / ge.dnrm2(blksiz, work, 1);
        jmax = 0;
        nrm = Math.abs(work[0]);
        for (i = 1; i < blksiz; i++) {
            if (Math.abs(work[i]) > nrm) {
                nrm = Math.abs(work[i]);
                jmax = i;
            }
        }
        if (work[jmax] < 0.0) {
           scl = -scl;
        }
        for (i = 0; i < blksiz; i++) {
            work[i] *= scl;
        }
        for (i = 1; i <= n; i++) {
            Z[i-1][j-1] = 0.0;
        }
        for (i = 1; i <= blksiz; i++) {
            Z[b1+i-2][j-1] = work[i-1];
        }

        // Save the shift to check eigenvalue spacing at next iteration.

        xjm = xj;

     } // for (j = j1; j <= m; j++)
  } // for (nblk = 1; nblk <= iblk[m-1]; nblk++)

  return;

  } // dstein
  
  /** This is a port of version 3.2 LAPACK routine DLAGTF.  The original DLAGTF is created by by Univ. of Tennessee,
  Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. on November, 2006.
  
   dlagtf factorizes the matrix (T - lambda*I), where T is an n by n
*  tridiagonal matrix and lambda is a scalar, as
*
*     T - lambda*I = PLU,
*
*  where P is a permutation matrix, L is a unit lower tridiagonal matrix
*  with at most one non-zero sub-diagonal elements per column and U is
*  an upper triangular matrix with at most two non-zero super-diagonal
*  elements per column.
*
*  The factorization is obtained by Gaussian elimination with partial
*  pivoting and implicit row scaling.
*
*  The parameter LAMBDA is included in the routine so that DLAGTF may
*  be used, in conjunction with DLAGTS, to obtain eigenvectors of T by
*  inverse iteration.
*  @param n input int The order of the matrix T.
*  @param a (input/output) double[] of dimension n.   On entry, a must contain the diagonal elements of T.
*          On exit, a is overwritten by the n diagonal elements of the
*          upper triangular matrix U of the factorization of T.
*  @param lambda input double
   @param b (input/output) double[] of dimension n-1.
           On entry, B must contain the (n-1) super-diagonal elements of
*          T.
*
*          On exit, B is overwritten by the (n-1) super-diagonal
*          elements of the matrix U of the factorization of T.
*  @param c (input/output) of dimension (n-1).
*          On entry, C must contain the (n-1) sub-diagonal elements of
*          T.
*
*          On exit, C is overwritten by the (n-1) sub-diagonal elements
*          of the matrix L of the factorization of T.
*  @param tol input double   On entry, a relative tolerance used to indicate whether or
*          not the matrix (T - lambda*I) is nearly singular. tol should
*          normally be chose as approximately the largest relative error
*          in the elements of T. For example, if the elements of T are
*          correct to about 4 significant figures, then tol should be
*          set to about 5*10**(-4). If TOL is supplied as less than eps,
*          where eps is the relative machine precision, then the value
*          eps is used in place of tol.
*  @param d output double[] of dimension (n-2).  
*          On exit, d is overwritten by the (n-2) second super-diagonal
*          elements of the matrix U of the factorization of T.
*  @param in output int[] of dimension n.
*          On exit, in contains details of the permutation matrix P. If
*          an interchange occurred at the kth step of the elimination,
*          then in(k) = 1, otherwise in(k) = 0. The element in(n)
*          returns the smallest positive integer j such that
*
*             abs( u(j,j) ).le. norm( (T - lambda*I)(j) )*TOL,
*
*          where norm( A(j) ) denotes the sum of the absolute values of
*          the jth row of the matrix A. If no such j exists then in(n)
*          is returned as zero. If in(n) is returned as positive, then a
*          diagonal element of U is small, indicating that
*          (T - lambda*I) is singular or nearly singular.
*  @param info output int[] of dimension 1.
*           = 0   : successful exit
*           < 0: if info[0] = -k, the kth argument had an illegal value
  */
  private void dlagtf(int n, double a[], double lambda, double b[], double c[],
                      double tol, double d[], int in[], int info[]) {
  double eps;
  double tl;
  double scale1;
  int k;
  double scale2;
  double piv1;
  double piv2;
  double mult;
  double temp;

  info[0] = 0;
  if (n < 0) {
     info[0] = -1;
     MipavUtil.displayError("Error dlagtf had info[0] = " + info[0]);
     return;
  }

  if (n == 0) {
     return;
  }

  a[0] = a[0] - lambda;
  in[n-1] = 0;
  if (n == 1) {
     if (a[0] == 0.0) {
        in[0] = 1;
     }
     return;
  }

  eps = ge.dlamch('E');

  tl = Math.max(tol, eps);
  scale1 = Math.abs(a[0] ) + Math.abs(b[0]);
  for (k = 1; k <= n-1; k++) {
     a[k] = a[k] - lambda;
     scale2 = Math.abs(c[k-1]) + Math.abs(a[k]);
     if (k < (n-1)) {
        scale2 = scale2 + Math.abs(b[k]);
     }
     if (a[k-1] == 0.0) {
        piv1 = 0.0;
     }
     else {
        piv1 = Math.abs(a[k-1]) /scale1;
     }
     if (c[k-1] == 0.0) {
        in[k-1] = 0;
        piv2 = 0.0;
        scale1 = scale2;
        if (k < (n-1)) {
           d[k-1] = 0.0;
        }
     }
     else {
        piv2 = Math.abs(c[k-1]) / scale2;
        if (piv2 <= piv1) {
           in[k-1] = 0;
           scale1 = scale2;
           c[k-1] = c[k-1] / a[k-1];
           a[k] = a[k] - c[k-1]*b[k-1];
           if (k < (n-1)) {
              d[k-1] = 0.0;
           }
        }
        else {
           in[k-1] = 1;
           mult = a[k-1] / c[k-1];
           a[k-1] = c[k-1];
           temp = a[k];
           a[k] = b[k-1] - mult * temp;
           if (k < (n-1)) {
              d[k-1] = b[k];
              b[k] = -mult * d[k-1];
           }
           b[k-1] = temp;
           c[k-1] = mult;
        }
     }
     if ( (Math.max(piv1, piv2) <= tl) && (in[n-1] == 0 ) ) {
        in[n-1] = k;
     }
  } // for (k = 1; k <= n-1; k++) 
  if ( (Math.abs(a[n-1]) <= scale1*tl) && (in[n-1] == 0 ) ) {
     in[n-1] = n;
  }

  return;

  } // dlagtf
  
  /** This is a port of version 3.3.1 LAPACK routine DLAGTS.  The original DLAGTS is created by by Univ. of Tennessee,
  Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. on April, 2011.
  
  dlagts may be used to solve one of the systems of equations
*
*     (T - lambda*I)*x = y   or   (T - lambda*I)**T*x = y,
*
*  where T is an n by n tridiagonal matrix, for x, following the
*  factorization of (T - lambda*I) as
*
*     (T - lambda*I) = P*L*U ,
*
*  by routine dlagtf. The choice of equation to be solved is
*  controlled by the argument job, and in each case there is an option
*  to perturb zero or very small diagonal elements of U, this option
*  being intended for use in applications such as inverse iteration.
*  
*  @param job input int  Specifies the job to be performed by dlagts as follows:
*          =  1: The equations  (T - lambda*I)x = y  are to be solved,
*                but diagonal elements of U are not to be perturbed.
*          = -1: The equations  (T - lambda*I)x = y  are to be solved
*                and, if overflow would otherwise occur, the diagonal
*                elements of U are to be perturbed. See argument TOL
*                below.
*          =  2: The equations  (T - lambda*I)**Tx = y  are to be solved,
*                but diagonal elements of U are not to be perturbed.
*          = -2: The equations  (T - lambda*I)**Tx = y  are to be solved
*                and, if overflow would otherwise occur, the diagonal
*                elements of U are to be perturbed. See argument TOL
*                below.
*  @param n input n The order of the matrix T.
*  @param a input double[] of dimension n.  On entry, a must contain the diagonal elements of U as
*          returned from dlagtf.
*  @param b input double[] of dimension (n-1). On entry, b must contain the first super-diagonal elements of
*          U as returned from dlagtf. 
*  @param c input double[] of dimension (n-1). On entry, c must contain the sub-diagonal elements of L as
*          returned from dlagtf. 
*  @param d input double[] of dimension (n-2). On entry, d must contain the second super-diagonal elements
*          of U as returned from dlagtf.
*  @param in input int[] of dimension n.  On entry, in must contain details of the matrix P as returned
*          from dlagtf.
*  @param y (input/output) double[] of dimension n.
*          On entry, the right hand side vector y.
*          On exit, y is overwritten by the solution vector x.  
   @param tol (input/out) double[] of dimension 1. On entry, with  job < 0, tol should be the minimum
*          perturbation to be made to very small diagonal elements of U.
*          tol should normally be chosen as about eps*norm(U), where eps
*          is the relative machine precision, but if tol is supplied as
*          non-positive, then it is reset to eps*max( abs( u(i,j) ) ).
*          If  job > 0  then tol is not referenced.
*
*          On exit, TOL is changed as described above, only if TOL is
*          non-positive on entry. Otherwise TOL is unchanged.
*  @param info output int[] of dimension 1.
*         = 0   : successful exit
*         < 0: if info[0] = -i, the i-th argument had an illegal value
*         > 0: overflow would occur when computing the INFO(th)
*                  element of the solution vector x. This can only occur
*                  when JOB is supplied as positive and either means
*                  that a diagonal element of U is very small, or that
*                  the elements of the right-hand side vector y are very
*                  large. 
  */
  private void dlagts(int job, int n, double a[], double b[], double c[], double d[],
                      int in[], double y[], double tol[], int info[]) {
  double eps;
  double sfmin;
  double bignum;
  int k;
  double temp;
  double ak;
  double absak;
  double pert;

  info[0] = 0;
  if ( (Math.abs(job) > 2 ) || (job == 0) ) {
     info[0] = -1;
  }
  else if (n < 0) {
     info[0] = -2;
  }
  if (info[0] != 0) {
     MipavUtil.displayError("Error dlagts had info[0] = " + info[0]);
     return;
  }

  if (n == 0) {
      return;
  }

  eps = ge.dlamch('E');
  sfmin = ge.dlamch('S');
  bignum = 1.0 / sfmin;

  if (job < 0) {
     if (tol[0] <= 0.0) {
        tol[0] = Math.abs(a[0]);
        if (n > 1) {
           tol[0] = Math.max(tol[0], Math.max(Math.abs(a[1]), Math.abs(b[0])) );
        }
        for (k = 3; k <= n; k++) {
           tol[0] = Math.max(tol[0],Math.max(Math.abs(a[k-1]), Math.max(Math.abs(b[k-2]),
                 Math.abs(d[k-3]))) );
        } // for (k = 3; k <= n; k++)
        tol[0] = tol[0]*eps;
        if (tol[0] == 0.0) {
           tol[0] = eps;
        }
     } // if (tol[0] <= 0.0) 
  } // if (job < 0)

  if (Math.abs(job) == 1 ) {
     for (k = 2; k <= n; k++) {
        if (in[k-2] == 0) {
           y[k-1] = y[k-1] - c[k-2]*y[k-2];
        }
        else {
           temp = y[k-2];
           y[k-2] = y[k-1];
           y[k-1] = temp - c[k-2]*y[k-1];
        }
     } // for (k = 2; k <= n; k++) 
     if (job == 1) {
        for (k = n; k >= 1; k--) {
           if (k <= n-2) {
              temp = y[k-1] - b[k-1]*y[k] - d[k-1]*y[k+1];
           }
           else if (k == n-1) {
              temp = y[k-1] - b[k-1]*y[k];
           }
           else {
              temp = y[k-1];
           }
           ak = a[k-1];
           absak = Math.abs(ak);
           if (absak < 1.0) {
              if (absak < sfmin) {
                 if (absak == 0.0 || Math.abs(temp)*sfmin > absak) {
                    info[0] = k;
                    return;
                 }
                 else {
                    temp = temp * bignum;
                    ak = ak * bignum;
                 }
              } // if (absak < sfmin)
              else if (Math.abs(temp) > absak*bignum) {
                 info[0] = k;
                 return;
              } // else if (Math.abs(temp) > absak*bignum)
           } // if (absak < 1.0)
           y[k-1] = temp / ak;
        } // for (k = n; k >= 1; k--)
     } // if (job == 1)
     else { // job == -1
        for (k = n; k >= 1; k--) {
           if (k <= n-2) {
              temp = y[k-1] - b[k-1]*y[k] - d[k-1]*y[k+1];
           }
           else if (k == n-1) {
              temp = y[k-1] - b[k-1]*y[k];
           }
           else {
              temp = y[k-1];
           }
           ak = a[k-1];
           if (ak >= 0.0) {
               pert = Math.abs(tol[0]);
           }
           else {
               pert = -Math.abs(tol[0]);
           }
           
           while (true) {
               absak = Math.abs(ak);
               if (absak < 1.0) {
                  if (absak < sfmin) {
                     if (absak == 0.0 || Math.abs(temp)*sfmin > absak) {
                        ak = ak + pert;
                        pert = 2.0 * pert;
                        continue;
                     } // if (absak == 0.0 || Math.abs(temp)*sfmin > absak)
                     else {
                        temp = temp * bignum;
                        ak = ak * bignum;
                        break;
                     }
                  } // if (absak < sfmin)
                  else if (Math.abs(temp) > absak * bignum) {
                     ak = ak + pert;
                     pert = 2.0 * pert;
                     continue;
                  } // else if (Math.abs(temp) > absak * bignum)
                  else {
                      break;
                  }
               } // if (absak < 1.0)
               else {
                   break;
               }
           } // while (true)
           y[k-1] = temp/ak;
        } // for (k = n; k >= 1; k--)
     } // else job == -1
  } // if (Math.abs(job) == 1 )
  else { // job = 2 or -2

     // Come to here if  JOB = 2 or -2

     if (job == 2) {
        for (k = 1; k <= n; k++) {
           if (k >= 3) {
              temp = y[k-1] - b[k-2]*y[k-2] - d[k-3]*y[k-3];
           }
           else if (k == 2) {
              temp = y[k-1] - b[k-2]*y[k-2];
           }
           else {
              temp = y[k-1];
           }
           ak = a[k-1];
           absak = Math.abs(ak);
           if (absak < 1.0) {
              if (absak < sfmin) {
                 if (absak == 0.0 || Math.abs(temp)*sfmin > absak) {
                    info[0] = k;
                    return;
                 }
                 else {
                    temp = temp*bignum;
                    ak = ak*bignum;
                 }
              }
              else if (Math.abs(temp) > absak*bignum) {
                 info[0] = k;
                 return;
              }
           } // if (absak < 1.0)
           y[k-1] = temp / ak;
        } // for (k = 1; k <= n; k++)
     } // if (job == 2)
     else { // job == -2
        for (k = 1; k <= n; k++) {
           if (k >= 3) {
              temp = y[k-1] - b[k-2]*y[k-2] - d[k-3]*y[k-3];
           }
           else if (k == 2) {
              temp = y[k-1] - b[k-2]*y[k-2];
           }
           else {
              temp = y[k-1];
           }
           ak = a[k-1];
           if (ak >= 0.0) {
               pert = Math.abs(tol[0]);
           }
           else {
               pert = -Math.abs(tol[0]);
           }
           while (true) {
               absak = Math.abs(ak);
               if (absak < 1.0) {
                   if (absak < sfmin) {
                     if (absak == 0.0 || Math.abs(temp)*sfmin > absak) {
                        ak = ak + pert;
                        pert = 2.0 * pert;
                        continue;
                     } // if (absak == 0.0 || Math.abs(temp)*sfmin > absak) 
                     else {
                        temp = temp*bignum;
                        ak = ak*bignum;
                        break;
                     }
                   } // if (absak < sfmin)
                   else if (Math.abs(temp) > absak*bignum) {
                     ak = ak + pert;
                     pert = 2.0*pert;
                     continue;
                   } // else if (Math.abs(temp) > absak*bignum)
                   else {
                       break;
                   }
               } // if (absak < 1.0)
               else {
                   break;
               }
           } // while (true)
           y[k-1] = temp/ak;
        } // for (k = 1; k <= n; k++)
     } // else job == -2

     for (k = n; k >= 2; k--) {
        if (in[k-2] == 0 ) {
           y[k-2] = y[k-2] - c[k-2]*y[k-1];
        }
        else {
           temp = y[k-2];
           y[k-2] = y[k-1];
           y[k-1] = temp - c[k-2]*y[k-1];
        }
     } // for (k = n; k >= 2; k--)
  } // else job = 2 or -2
  return;
  } // dlagts
  
  /** This is a port of version 3.2 LAPACK routine DORMTR.  The original DORMTR is created by by Univ. of Tennessee,
  Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. on November, 2006.
  
   DORMTR overwrites the general real M-by-N matrix C with
*
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'T':      Q**T * C       C * Q**T
*
*  where Q is a real orthogonal matrix of order nq, with nq = m if
*  SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
*  nq-1 elementary reflectors, as returned by DSYTRD:
*
*  if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
*
*  if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
*  
*  @param side input char
*          = 'L': apply Q or Q**T from the Left;
*          = 'R': apply Q or Q**T from the Right.
*  @param uplo input char
*         = 'U': Upper triangle of A contains elementary reflectors from dsytrd;
*         = 'L': Lower triangle of A contains elementary reflectors from dsytrd.
   @param trans input char
          = 'N':  No transpose, apply Q;
          = 'T':  Transpose, apply Q**T.
   @param m input int  The number of rows of the matrix C. m >= 0. 
   @param n input int  The number of columns of the matrix C. n >= 0.
   @param A input double[][] of dimension
             (lda,m) if side = 'L'
*            (lda,n) if side = 'R'
*            The vectors which define the elementary reflectors, as
*            returned by dsytrd.
*  @param lda input int The leading dimension of the array A.
*          lda >= max(1,m) if side = 'L'; lda >= max(1,n) if side = 'R'.
*  @param tau input double[] of dimension
*          (m-1) if side = 'L'
*          (n-1) if side = 'R'
*          tau[i] must contain the scalar factor of the elementary
*          reflector H(i), as returned by dsytrd.
*  @param C (input/output) double of dimension (ldc,n).
*          On entry, the m-by-n matrix C.
*          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
*  @param ldc input int The leading dimension of the array C. ldc >= max(1,m). 
*  @param work (workspace/output) of dimension max(1, lwork)
*          On exit, if info[0] = 0, work[0] returns the optimal lwork.
*  @param lwork input int
*          The dimension of the array work.
*          If side = 'L', lwork >= max(1,n);
*          if side = 'R', lwork >= max(1,m).
*          For optimum performance lwork >= n*nb if side = 'L', and
*          lwork >= m*nb if side = 'R', where nb is the optimal blocksize.
*          
*          If lwork = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the work array, returns
*          this value as the first entry of the work array, and no error
*          message related to lwork is issued.
* @param info output int[] of dimension 1.
*           = 0:  successful exit
*           < 0:  if info[0] = -i, the i-th argument had an illegal value
  */
  private void dormtr(char side, char uplo, char trans, int m, int n, double A[][],
                      int lda, double tau[], double C[][], int ldc, double work[], 
                      int lwork, int info[]) {
  boolean left;
  boolean upper;
  boolean lquery;
  int nq;
  int nw;
  char optsC[] = new char[2];
  String opts;
  int nb;
  int lwkopt = 0;
  int mi;
  int ni;
  int i1;
  int i2;
  double array1[][];
  double array2[][];
  int i;
  int j;
  int iinfo[] = new int[1];
  int firstDim;

  // Test the input arguments

  info[0] = 0;
  left = ((side == 'L' ) || (side == 'l'));
  upper = ((uplo == 'U' ) || (uplo == 'u'));
  lquery = (lwork == -1);

  // nq is the order of Q and nw is the minimum dimension of WORK

  if (left) {
     nq = m;
     nw = n;
  }
  else {
     nq = n;
     nw = m;
  }
  if (!left && !((side == 'R') || (side == 'r')) ) {
     info[0] = -1;
  }
  else if (!upper && !((uplo == 'L') || (uplo == 'l'))) {
     info[0] = -2;
  }
  else if (!((trans == 'N') || (trans == 'n')) && !((trans == 'T') || (trans == 't'))) {
     info[0] = -3;
  }
  else if (m < 0) {
     info[0] = -4;
  }
  else if (n < 0) {
     info[0] = -5;
  }
  else if (lda < Math.max(1, nq)) {
     info[0] = -7;
  }
  else if (ldc < Math.max(1, m)) {
     info[0] = -10;
  }
  else if (lwork < Math.max(1, nw) && !lquery) {
     info[0] = -12;
  }

  
  if (info[0] == 0) {
      optsC[0] = side;
      optsC[1] = trans;
      opts = new String(optsC);
     if (upper) {
        if (left) {
           nb = ge.ilaenv(1, "DORMQL", opts, m-1, n, m-1, -1 );
        }
        else {
           nb = ge.ilaenv(1, "DORMQL", opts, m, n-1, n-1, -1 );
        }
     } // if (upper)
     else {
        if (left) {
           nb = ge.ilaenv(1, "DORMQR", opts, m-1, n, m-1, -1 );
        }
        else {
           nb = ge.ilaenv(1, "DORMQR", opts, m, n-1, n-1, -1 );
        }
     } // else
     lwkopt = Math.max(1, nw)*nb;
     work[0] = lwkopt;
  } // if (info[0] == 0)

  if (info[0] != 0) {
     MipavUtil.displayError("Error DORMTR had info[0] = " + info[0]);
     return;
  }
  else if (lquery) {
     return;
  }

  // Quick return if possible

  if (m == 0 || n == 0 || nq == 1) {
     work[0] = 1;
     return;
  }

  if (left) {
     mi = m - 1;
     ni = n;
  }
  else {
     mi = m;
     ni = n - 1;
  }

  if (upper) {

     // Q was determined by a call to dsytrd with uplo = 'U'

     array1 = new double[lda][nq-1];
     for (i = 0; i < lda; i++) {
         for (j = 0; j < nq-1; j++) {
             array1[i][j] = A[i][j+1];
         }
     }
     dormql(side,  trans, mi, ni, nq-1, array1, lda, tau, C,
                  ldc, work, lwork, iinfo);
  }
  else {

     // Q was determined by a call to dsytrd with uplo = 'L'

     if (left) {
        i1 = 2;
        i2 = 1;
     }
     else {
        i1 = 1;
        i2 = 2;
     }
     if (left) {
         firstDim = Math.max(1,mi);
     }
     else {
         firstDim = Math.max(1,ni);
     }
     array1 = new double[firstDim][nq-1];
     for (i = 0; i < firstDim; i++) {
         for (j = 0; j < nq-1; j++) {
             array1[i][j] = A[i+1][j];
         }
     }
     array2 = new double[Math.max(1,mi)][ni];
     for (i = 0; i < Math.max(1, mi); i++) {
         for (j = 0; j < ni; j++) {
             array2[i][j] = C[i1-1+i][i2-1+j];
         }
     }
     ge.dormqr(side, trans, mi, ni, nq-1, array1, firstDim, tau,
                  array2, Math.max(1,mi), work, lwork, iinfo);
     for (i = 0; i < Math.max(1, mi); i++) {
         for (j = 0; j < ni; j++) {
             C[i1-1+i][i2-1+j] = array2[i][j];
         }
     }
  }
  work[0] = lwkopt;
  return;

  } // dormtr
  
  /** This is a port of version 3.3.1 LAPACK routine DORMQL.  The original DORMQL is created by by Univ. of Tennessee,
  Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd. on April, 2011.
  
  dormql overwrites the general real M-by-N matrix C with
*
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'T':      Q**T * C       C * Q**T
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(k) . . . H(2) H(1)
*
*  as returned by DGEQLF. Q is of order M if SIDE = 'L' and of order N
*  if SIDE = 'R'.
*  
*  @param side input char
*         = 'L': apply Q or Q**T from the Left;
*         = 'R': apply Q or Q**T from the Right.
*  @param trans input char
*         = 'N':  No transpose, apply Q;
*         = 'T':  Transpose, apply Q**T.
*  @param m input int  The number of rows of the matrix C. m >= 0.
*  @param n input int  The number of columns of the matrix C. n >= 0.
*  @param k input int  The number of elementary reflectors whose product defines the matrix Q.
*          If side = 'L', m >= k >= 0;
*          if side = 'R', n >= k >= 0.
*  @param A input double[][] of dimension (lda, k)
*         The i-th column must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          dgeqlf in the last k columns of its array argument A.
*          A is modified by the routine but restored on exit
*  @param lda input int  The leading dimension of the array A.
*          If side = 'L', lda >= max(1,m);
*          if side = 'R', lda >= max(1,n).
*  @param tau[] input double[] of dimension k
*          tau[i] must contain the scalar factor of the elementary
*          reflector H(i), as returned by dgeqlf.
*  @param C (input/output) double[][] of dimension (ldc, n)
*          On entry, the m-by-n matrix C.
*          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
*  @param ldc input int The leading dimension of the array C. ldc >= max(1,m).
*  @param work (workspace/output) of dimension max(1, lwork).
*          On exit, if info[0] = 0, work[0] returns the optimal lwork.
*  @param lwork input int The dimension of the array work.
*          If side = 'L', lwork >= max(1,n);
*          if side = 'R', lwork >= max(1,m).
*          For optimum performance lwork >= n*nb if side = 'L', and
*          lwork >= m*nb if side = 'R', where nb is the optimal
*          blocksize.
*
*          If lwork = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the work array, returns
*          this value as the first entry of the work array, and no error
*          message related to lwork is issued.
*  @param info output int[] of dimension 1.
*         = 0:  successful exit
*         < 0:  if info[0] = -i, the i-th argument had an illegal value
  */
  private void dormql(char side, char trans, int m, int n, int k, double A[][], int lda, double tau[],
          double C[][], int ldc, double work[], int lwork, int info[]) {
  int nbmax = 64;
  int ldt = nbmax + 1;
  boolean left;
  boolean notran;
  boolean lquery;
  int nq;
  int nw;
  int lwkopt = 0;
  int nb = 0;
  char optsC[] = new char[2];
  String opts;
  int nbmin;
  int ldwork;
  int iws;
  int iinfo[] = new int[1];
  int i1;
  int i2;
  int i3;
  int mi = 0;
  int ni = 0;
  int i;
  int ib;
  double array1[][];
  double array2[][];
  double vec1[];
  int j;
  int p;
  double T[][] = new double[ldt][nbmax];

  // Test the input arguments

  info[0] = 0;
  left = ((side == 'L') || (side == 'l'));
  notran = ((trans == 'N') || (trans == 'n'));
  lquery = (lwork == -1);

  // nq is the order of Q and nw is the minimum dimension of work

  if (left) {
     nq = m;
     nw = Math.max(1, n);
  }
  else {
     nq = n;
     nw = Math.max(1, m);
  }
  if (!left && !((side == 'R') || (side == 'r'))) {
     info[0] = -1;
  }
  else if (!notran && !((trans == 'T') || (trans == 't'))) {
     info[0] = -2;
  }
  else if (m < 0) {
     info[0] = -3;
  }
  else if (n < 0) {
     info[0] = -4;
  }
  else if (k < 0 || k > nq) {
     info[0] = -5;
  }
  else if (lda < Math.max(1, nq)) {
     info[0] = -7;
  }
  else if (ldc < Math.max(1, m)) {
     info[0] = -10;
  }

  optsC[0] = side;
  optsC[1] = trans;
  opts = new String(optsC);
  
  if (info[0] == 0) {
     if (m == 0 || n == 0) {
        lwkopt = 1;
     }
     else {

        // Determine the block size.  nb may be at most nbmax, where
        // nbmax is used to define the local array T.

        nb = Math.min(nbmax, ge.ilaenv(1, "DORMQL", opts, m, n, k, -1 ) );
        lwkopt = nw*nb;
     } // else
     work[0] = lwkopt;

     if (lwork < nw && !lquery) {
        info[0] = -12;
     }
  } // if (info[0] == 0)

  if (info[0] != 0) {
     MipavUtil.displayError("Error dormql had info[0] = " + info[0]);
     return;
  }
  else if (lquery) {
     return;
  }

  // Quick return if possible

  if (m == 0 || n == 0) {
     return;
  }

  nbmin = 2;
  ldwork = nw;
  if (nb > 1 && nb < k) {
     iws = nw*nb;
     if (lwork < iws) {
        nb = lwork / ldwork;
        nbmin = Math.max( 2, ge.ilaenv(2, "DORMQL", opts, m, n, k, -1 ) );
     }
  }
  else {
     iws = nw;
  }

  if (nb < nbmin || nb >= k) {

     // Use unblocked code

     ge.dorm2l(side, trans, m, n, k, A, lda, tau, C, ldc, work, iinfo);
  }
  else {

     // Use blocked code

     if ( (left && notran) || (!left && !notran) ) {
        i1 = 1;
        i2 = k;
        i3 = nb;
     }
     else {
        i1 = ( ( k-1 ) / nb )*nb + 1;
        i2 = 1;
        i3 = -nb;
     }

     if (left) {
        ni = n;
     }
     else {
        mi = m;
     }

     if (i3 == nb) {
         for (i = i1; i <= i2; i += nb) {
            ib = Math.min(nb, k-i+1);
    
            // Form the triangular factor of the block reflector
            // H = H(i+ib-1) . . . H(i+1) H(i)
            array1 = new double[lda][ib];
            for (j = 0; j < lda; j++) {
                for (p = 0; p < ib; p++) {
                    array1[j][p] = A[j][i-1+p];
                }
            }
            vec1 = new double[ib];
            for (j = 0; j < ib; j++) {
                vec1[j] = tau[i-1+j];
            }
            ge.dlarft('B', 'C', nq-k+i+ib-1, ib, array1, lda, vec1, T, ldt);
            
            if (left) {
    
              // H or H**T is applied to C(1:m-k+i+ib-1,1:n)
    
               mi = m - k + i + ib - 1;
            }
            else {
    
               // H or H**T is applied to C(1:m,1:n-k+i+ib-1)
    
               ni = n - k + i + ib - 1;
            }
    
            // Apply H or H**T
    
            array2 = new double[ldwork][ib];
            ge.dlarfb(side, trans, 'B', 'C', mi, ni, ib, array1, lda,
                      T, ldt, C, ldc, array2, ldwork);
            for (j = 0; j < lda; j++) {
                for (p = 0; p < ib; p++) {
                    A[j][i-1+p] = array1[j][p];
                }
            }
         } // for (i = i1; i <= i2; i += nb)
     } // if (i3 == nb)
     else { // i3 == -nb
         for (i = i1; i >= i2; i -= nb) {
             ib = Math.min(nb, k-i+1);
     
             // Form the triangular factor of the block reflector
             // H = H(i+ib-1) . . . H(i+1) H(i)
             array1 = new double[lda][ib];
             for (j = 0; j < lda; j++) {
                 for (p = 0; p < ib; p++) {
                     array1[j][p] = A[j][i-1+p];
                 }
             }
             vec1 = new double[ib];
             for (j = 0; j < ib; j++) {
                 vec1[j] = tau[i-1+j];
             }
             ge.dlarft('B', 'C', nq-k+i+ib-1, ib, array1, lda, vec1, T, ldt);
             
             if (left) {
     
               // H or H**T is applied to C(1:m-k+i+ib-1,1:n)
     
                mi = m - k + i + ib - 1;
             }
             else {
     
                // H or H**T is applied to C(1:m,1:n-k+i+ib-1)
     
                ni = n - k + i + ib - 1;
             }
     
             // Apply H or H**T
     
             array2 = new double[ldwork][ib];
             ge.dlarfb(side, trans, 'B', 'C', mi, ni, ib, array1, lda,
                       T, ldt, C, ldc, array2, ldwork);
             for (j = 0; j < lda; j++) {
                 for (p = 0; p < ib; p++) {
                     A[j][i-1+p] = array1[j][p];
                 }
             }
          } // for (i = i1; i >= i2; i -= nb)    
     } // else i3 == -nb
  }
  work[0] = lwkopt;
  return;

  } // dormql












}