package gov.nih.mipav.view.renderer.WildMagic.AAM;


import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;

import gov.nih.mipav.view.ViewUserInterface;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;


/**
 * 
 * AAM-API LICENSE - file: license.txt
 * 
 * This software is freely available for non-commercial use such as research and education. Please see the full
 * disclaimer below.
 * 
 * All publications describing work using this software should cite the reference given below.
 * 
 * Copyright (c) 2000-2003 Mikkel B. Stegmann, mbs@imm.dtu.dk
 * 
 * 
 * IMM, Informatics & Mathematical Modelling DTU, Technical University of Denmark Richard Petersens Plads, Building 321
 * DK-2800 Lyngby, Denmark
 * 
 * http://www.imm.dtu.dk/~aam/
 * 
 * 
 * 
 * REFERENCES
 * 
 * Please use the reference below, when writing articles, reports etc. where the AAM-API has been used. A draft version
 * the article is available from the homepage.
 * 
 * I will be happy to receive pre- or reprints of such articles.
 * 
 * /Mikkel
 * 
 * 
 * ------------- M. B. Stegmann, B. K. Ersboll, R. Larsen, "FAME -- A Flexible Appearance Modelling Environment", IEEE
 * Transactions on Medical Imaging, IEEE, 2003 (to appear) -------------
 * 
 * 
 * 
 * 3RD PART SOFTWARE
 * 
 * The software is partly based on the following libraries:
 * 
 * - The Microsoft(tm) Vision Software Developers Kit, VisSDK - LAPACK
 * 
 * 
 * DISCLAIMER
 * 
 * This software is provided 'as-is', without any express or implied warranty. In no event will the author be held
 * liable for any damages arising from the use of this software.
 * 
 * Permission is granted to anyone to use this software for any non-commercial purpose, and to alter it, subject to the
 * following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software.
 * 
 * 2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original
 * software.
 * 
 * 3. This notice may not be removed or altered from any source distribution.
 * 
 * --
 * 
 * No guarantees of performance accompany this software, nor is any responsibility assumed on the part of the author or
 * IMM.
 * 
 * This software is provided by Mikkel B. Stegmann and IMM ``as is'' and any express or implied warranties, including,
 * but not limited to, the implied warranties of merchantability and fitness for a particular purpose are disclaimed. In
 * no event shall IMM or Mikkel B. Stegmann be liable for any direct, indirect, incidental, special, exemplary, or
 * consequential damages (including, but not limited to, procurement of substitute goods or services; loss of use, data,
 * or profits; or business interruption) however caused and on any theory of liability, whether in contract, strict
 * liability, or tort (including negligence or otherwise) arising in any way out of the use of this software, even if
 * advised of the possibility of such damage.
 * 
 * 
 * 
 * 
 * $Revision: 1.4 $ $Date: 2003/04/23 14:49:15 $
 * 
 * 
 * @author Ruida Cheng
 * 
 */
public class aamc {

    public static void main(final String[] args) {
        System.setProperty("sun.awt.noerasebackground", "true");

        // MipavMain mipav = new MipavMain();
        // int initArg = mipav.parseArguments(args, 0); //process static command line arguments

        final ViewUserInterface ui = ViewUserInterface.create();
        // ui.parseArguments(args, initArg); //process command line arguments that require mipav objects

        /*
         * if (ui.isAppFrameVisible() && Preferences.is(Preferences.PREF_SHOW_SPLASH)) { ui.showSplashGraphics(); }
         */
        ui.setVisible(ui.isAppFrameVisible());
        final int argc = args.length;
        final CAAMConsole c = new CAAMConsole(argc, args);
        /*
         * TestRun t = new TestRun(); t.runLapack(); t.runJama(); t.runJTEM();
         */
    }

}

class TestRun {
    final double[][] A = new double[3][3];

    public TestRun() {

        A[0][0] = 0.064297914100621;
        A[0][1] = -0.035924465561849;
        A[0][2] = -0.028373448538772;
        A[1][0] = -0.035924465561849;
        A[1][1] = 0.035366528421754;
        A[1][2] = 0.00055793714009496;
        A[2][0] = -0.028373448538772;
        A[2][1] = 0.00055793714009496;
        A[2][2] = 0.027815511398677;

        /*
         * A[0][0] = 1; A[0][1] = 2; A[0][2] = 3; A[1][0] = 4; A[1][1] = 5; A[1][2] = 6; A[2][0] = 7; A[2][1] = 8;
         * A[2][2] = 9;
         */
    }

    public void runJTEM() {
        System.err.println("\n\nrun JTEM");

        double[] eigenvalue;
        final double[][] V;
        double[][] eigenvector;
        final double[] e1;
        try {
            eigenvalue = new double[A.length];
            eigenvector = new double[A.length][A.length];
        } catch (final OutOfMemoryError e) {
            System.gc();
            e.printStackTrace();
            return;
        }
        Eigenvalue.decompose(A, eigenvector, eigenvalue);

        final CDVector m_vEigenValues = new CDVector();
        final CDMatrix e_i = new CDMatrix();
        m_vEigenValues.Resize(3);
        e_i.Resize(3, 3);
        m_vEigenValues.assign(eigenvalue);
        e_i.assign(eigenvector);
        System.err.println("eigen value");
        m_vEigenValues.ToString(true);
        System.err.println("eigen vector");
        e_i.ToString();

    }

    public void runJama() {
        System.err.println("\n\nrun Jama");

        final EigenvalueDecomposition ed = new EigenvalueDecomposition(A);
        CDVector m_vEigenValues = new CDVector();
        CDMatrix e_i = new CDMatrix();
        m_vEigenValues.Resize(3);
        e_i.Resize(3, 3);
        m_vEigenValues = ed.getRealEigenvalues();
        e_i = ed.getV();

        System.err.println("eigen value");
        m_vEigenValues.ToString(true);
        System.err.println("eigen vector");
        e_i.ToString();

    }

    public void runLapack() {

        System.err.println("run Lapack");
        final int itype = 1;
        final char jobz = 'V';
        final char uplo = 'U';
        final int n = 3;

        final int lda = 3;
        final double[][] B = new double[3][3];
        B[0][0] = 1.0;
        B[0][1] = 0.0;
        B[0][2] = 0.0;
        B[1][0] = 0.0;
        B[1][1] = 1.0;
        B[1][2] = 0.0;
        B[2][0] = 0.0;
        B[2][1] = 0.0;
        B[2][2] = 1.0;

        final int ldb = 3;
        final double[] w = new double[3];
        final double[] work = new double[100];
        final int lwork = 100;
        final int[] info = new int[1];
        final GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
        ge.dsygv(itype, jobz, uplo, n, A, lda, B, ldb, w, work, lwork, info);

        if (info[0] != 0) {

            return;
        }
        System.err.println("Eigenvalues in ascending order are:\n");
        System.err.println(w[0] + "  " + w[1] + "  " + w[2] + "\n");
        System.err.print(A[0][0] + "\t");
        System.err.print(A[0][1] + "\t");
        System.err.print(A[0][2] + "\n");
        System.err.print(A[1][0] + "\t");
        System.err.print(A[1][1] + "\t");
        System.err.print(A[1][2] + "\n");
        System.err.print(A[2][0] + "\t");
        System.err.print(A[2][1] + "\t");
        System.err.print(A[2][2] + "\n");

        return;
    }

}
