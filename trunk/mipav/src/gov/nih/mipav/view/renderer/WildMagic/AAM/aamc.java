package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.view.ViewUserInterface;
import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;
import gov.nih.mipav.model.structures.jama.*;

public class aamc {
	
	public static void main(String[] args) {
		System.setProperty("sun.awt.noerasebackground", "true");

        // MipavMain mipav = new MipavMain();
        // int initArg = mipav.parseArguments(args, 0); //process static command line arguments
        
        ViewUserInterface ui = ViewUserInterface.create();
        // ui.parseArguments(args, initArg); //process command line arguments that require mipav objects

         /*
        if (ui.isAppFrameVisible() && Preferences.is(Preferences.PREF_SHOW_SPLASH)) {
            ui.showSplashGraphics();
        }
         */
        ui.setVisible(ui.isAppFrameVisible());
		int argc = args.length;
		CAAMConsole c = new CAAMConsole(argc, args);
		/*
		TestRun t = new TestRun();
		t.runLapack();
		t.runJama();
		t.runJTEM();
		*/
	}
	
}


class TestRun {
	final double[][] A = new double[3][3];
	
	public TestRun () {
		
        A[0][0] = 0.064297914100621; A[0][1] = -0.035924465561849; A[0][2] = -0.028373448538772;
        A[1][0] = -0.035924465561849; A[1][1] = 0.035366528421754; A[1][2] = 0.00055793714009496;
        A[2][0] = -0.028373448538772; A[2][1] = 0.00055793714009496; A[2][2] = 0.027815511398677;
        
        /*
		 A[0][0] = 1; A[0][1] = 2; A[0][2] = 3;
	     A[1][0] = 4; A[1][1] = 5; A[1][2] = 6;
	     A[2][0] = 7; A[2][1] = 8; A[2][2] = 9;
		*/
	}
	
	public void runJTEM() {
		System.err.println("\n\nrun JTEM");
		
        
        double[] eigenvalue;
        double[][] V;
        double[][] eigenvector;
        double[] e1;
        try {
            eigenvalue = new double[A.length];
            eigenvector= new double[A.length][A.length];
        } catch (OutOfMemoryError e) {
            System.gc();
            e.printStackTrace();
            return;
        }
        Eigenvalue.decompose( A, eigenvector, eigenvalue);
	    
        CDVector m_vEigenValues = new CDVector();
        CDMatrix e_i = new CDMatrix();
        m_vEigenValues.Resize( 3 );
	    e_i.Resize( 3, 3 );	
	    m_vEigenValues.assign(eigenvalue);
        e_i.assign(eigenvector);
        System.err.println("eigen value");
	    m_vEigenValues.ToString(true);
	    System.err.println("eigen vector");
	    e_i.ToString();
        
        
	}
	
	public void runJama() {
		System.err.println("\n\nrun Jama");
		
        EigenvalueDecomposition ed = new EigenvalueDecomposition(A);
        CDVector m_vEigenValues = new CDVector();
        CDMatrix e_i = new CDMatrix();
        m_vEigenValues.Resize( 3 );
	    e_i.Resize( 3, 3 );	
        m_vEigenValues = ed.getRealEigenvalues();
	    e_i = ed.getV();
	    
	    System.err.println("eigen value");
	    m_vEigenValues.ToString(true);
	    System.err.println("eigen vector");
	    e_i.ToString();
        
        
		
	}
	
	public void runLapack() {
	    
		    System.err.println("run Lapack");
	        int itype = 1;
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
	        GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
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