package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.structures.jama.*;


/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation. 
 * 
  * AAM-API LICENSE  -  file: license.txt
 * 
 * This software is freely available for non-commercial use such as
 * research and education. Please see the full disclaimer below. 
 * 
 * All publications describing work using this software should cite 
 * the reference given below. 
 * 	
 * Copyright (c) 2000-2003 Mikkel B. Stegmann, mbs@imm.dtu.dk
 * 
 * 
 * IMM, Informatics & Mathematical Modelling
 * DTU, Technical University of Denmark
 * Richard Petersens Plads, Building 321
 * DK-2800 Lyngby, Denmark
 * 
 * http://www.imm.dtu.dk/~aam/
 * 
 * 
 * 
 * REFERENCES
 * 
 * Please use the reference below, when writing articles, reports etc. where 
 * the AAM-API has been used. A draft version the article is available from 
 * the homepage. 
 * 
 * I will be happy to receive pre- or reprints of such articles.
 * 
 * /Mikkel
 * 
 * 
 * -------------
 * M. B. Stegmann, B. K. Ersboll, R. Larsen, "FAME -- A Flexible Appearance 
 * Modelling Environment", IEEE Transactions on Medical Imaging, IEEE, 2003
 * (to appear)
 * -------------
 * 
 *
 * 
 * 3RD PART SOFTWARE
 * 
 * The software is partly based on the following libraries:
 * 
 * - The Microsoft(tm) Vision Software Developers Kit, VisSDK
 * - LAPACK
 * 
 *
 * DISCLAIMER
 * 
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the author be held liable for any damages arising from the
 * use of this software.
 * 
 * Permission is granted to anyone to use this software for any non-commercial 
 * purpose, and to alter it, subject to the following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not claim
 *  that you wrote the original software. 
 *
 * 2. Altered source versions must be plainly marked as such, and must not be 
 *  misrepresented as being the original software.
 * 
 * 3. This notice may not be removed or altered from any source distribution.
 * 
 * --
 *
 * No guarantees of performance accompany this software, nor is any 
 * responsibility assumed on the part of the author or IMM. 
 * 
 * This software is provided by Mikkel B. Stegmann and IMM ``as is'' and any 
 * express or implied warranties, including, but not limited to, the implied 
 * warranties of merchantability and fitness for a particular purpose are 
 * disclaimed. In no event shall IMM or Mikkel B. Stegmann be liable for any 
 * direct, indirect, incidental, special, exemplary, or consequential damages
 * (including, but not limited to, procurement of substitute goods or services;
 * loss of use, data, or profits; or business interruption) however caused and 
 * on any theory of liability, whether in contract, strict liability, or tort 
 * (including negligence or otherwise) arising in any way out of the use of 
 * this software, even if advised of the possibility of such damage.
 * 
 * 
 * 
 *
 * $Revision: 1.4 $ 
 * $Date: 2003/04/23 14:49:15 $ 
 * 
 * 
 * Performs multi-variate linear regression on a set of experiments.
 * 
 * @author Ruida Cheng
 *
 */
public class CAAMLinearReg extends CAAMObject {
	
	/** number of experiments. */ 
	private int m_nExperiments;
	
	/** number of pixel difference.  */ 
	private int m_nPixelsDiffs;
	
	/** number of parameters. */ 
	private int m_nParams;
	
	/** eigen value vector. */ 
	private CDVector m_vEigenVal = new CDVector();
	
	/** eigen value matrix. */
	private CDMatrix m_mEigenVec = new CDMatrix();
	

	/**
	 * constructor
	*/
	public CAAMLinearReg() {
		
	}
	

	/**
	 * dispose memory 
	 */
	public void dispose() {
		
		
	}

	

	/**
      * Performs k-estimation.  
	  * @param    C			The parameter matrix.
	  * @param    EigenVec_k	Output k eigenvectors.
	  * @param	EigenVal_k  Output k eigenvalues.
	  * @return   k
	  */
	private int EstimateK( final CDMatrix C, CDMatrix EigenVec_k, CDMatrix EigenVal_k ) {

		int k;
		// estimate optimal k value
		int k_min = m_nParams;
		double Ek, Ek_min = 1.0e308;
		CDMatrix delta_C = new CDMatrix( C.NRows(), C.NCols() );    
	    CDVector c_j = new CDVector( C.NRows() );
	    int k_st, k_end;	

	    if (true) {

	        // estimate k
		    k_st	= m_nParams;
		    k_end	= m_nExperiments;
	    } else {

	        // use hard-wired k
		    // k_st	= m_nExperiments/2;
		    // k_end	= m_nExperiments/2;
	    }

		final double eps = 1e-10;	

		// naive linear search for the best k
		for(k=k_st;k<=k_end;k++) {

			EigenVec_k.Resize( m_nExperiments, k );
	        CDVector b_kj = new CDVector( k );

			// extract the k largest eigen vectors.
			m_mEigenVec.Submatrix( m_nExperiments, k, EigenVec_k, 0, m_mEigenVec.NCols()-k );        

	        // calc Ek
			delta_C.assign(C.mult(EigenVec_k).mult(EigenVec_k.Transposed()));
	        delta_C.sub_into(C);
			
			Ek = .0;
			for(int j=0;j<delta_C.NCols();j++) {

				delta_C.Col( j, c_j );

				// calc a_kj			
				EigenVec_k.Row(j, b_kj); // equal to: b_kj = EigenVec_k.Transposed().Column( j );
				double a_kj = b_kj.Norm2();
				a_kj*=a_kj;

				// calc f_kj
				if ( Math.abs(1.-a_kj) > eps ) {

	                delta_C.Col( j, c_j );				
	                double f_kj = 1./(1.-a_kj);
	                double v=c_j.Norm2();
					Ek += f_kj*f_kj*v*v;				
				} else{

	                C.Col( j, c_j );
					double v=c_j.Norm2();	
					Ek += v*v;				
				}			
			}		
			if (Ek<Ek_min) {
				            
				Ek_min = Ek;
				k_min = k;
			}
		}
		k=k_min;   

		// extract the k largest eigen vectors.
	    EigenVec_k.Resize( m_nExperiments, k );		
		EigenVec_k.assign(m_mEigenVec.Submatrix( m_nExperiments, k, 0, m_mEigenVec.NCols()-k ));
	    // swap cols
		
		for(int r=0;r<EigenVec_k.NCols()/2;r++) {
			CDVector tmp = new CDVector( EigenVec_k.NRows() );
			
			tmp.assign(EigenVec_k.Column( r ));
			int end = EigenVec_k.NCols()-r-1;

			EigenVec_k.SetColumn( r, EigenVec_k.Column( end ) );
			EigenVec_k.SetColumn( end, tmp );
		}

	    
		// extract the k largest eigen values
		CDVector vEigenVal_k = new CDVector( k );
		for(int i=0;i<k;i++) 
			vEigenVal_k.m_data[i] = m_vEigenVal.m_data[m_vEigenVal.Length()-i-1];

		// insert them into a diagonal matrix
		EigenVal_k.Resize( k, k );
		EigenVal_k.Diag( vEigenVal_k );

		return k;
	}

	
	/**
	 * Calculates the regression matrix, R from the set of experiments
	 * in C and X, obtaining the relationship: C = RX.
	 * @param    C	In the AAM case: the input parameters displacements.
	 * @param    X	In the AAM case: the input normalized pixel differences.
	 * @param	R	The output regression matrix.
	 * @return   k
	 */
	public int DoRegression( final CDMatrix C, final CDMatrix X, CDMatrix R ) {

		assert( C.NCols()==C.NCols() );
		
		m_nExperiments = X.NCols();
		m_nPixelsDiffs = X.NRows();
		m_nParams      = C.NRows();

		CDMatrix tempMatrix = new CDMatrix(X.NRows(), X.NCols());
		
		assert( m_nExperiments>m_nParams );

		// setup matrices
		CDMatrix EigenVec_k = new CDMatrix(); 
		CDMatrix EigenVal_k = new CDMatrix();	

	    // debug
	    System.err.println("sizeof(X) = " + (m_nExperiments*m_nPixelsDiffs/(1024*1024.0))+ " MB" );
	  
	    
	    // calc eigen vectors and values
	    System.err.println("Doing eigenvalue decomposition...");
	    m_vEigenVal.Resize( m_nExperiments );
		m_mEigenVec.Resize( m_nExperiments, m_nExperiments );        
	    boolean useSVD = false;
	    if (useSVD) {

	        /////////////////////////////////////////////////////
		    // use SVD: extract the right singular vectors of X
	        //          (notice the sorting of singular vectors
	        //           is reversed to that of eigen values)
	        /////////////////////////////////////////////////////
	        CDMatrix dummy = new CDMatrix();
	        tempMatrix.VisDMatrixSVD(X, m_vEigenVal, dummy, m_mEigenVec, 0, 1 );   // ??????????????????
	        
	        m_vEigenVal.Sqr();
	        m_vEigenVal.Reverse();
	        m_mEigenVec.FlipLR();
            
	        // SingularValueDecomposition svd = new SingularValueDecomposition(X);
	        // m_vEigenVal = (CDVector)svd.getS();
	        // m_mEigenVec = (CDMatrix)svd.getV();
	        
	        /*
	        char jobu = 'S'; //  the first min(m,n) columns of U (the left singular
            // vectors) are returned in the array U;
			char jobvt = 'A'; // all N rows of V**T are returned in the array VT;
			int m = X.NRows(); // The number of rows of the input matrix X.  m >= 0.
			int n = X.NCols(); // The number of columns of the input matrix X.  n >= 0.
			// On exit the contents of X are destroyed
			int ldx = Math.max(1, m); // The leading dimension of array X
			double s[] = new double[Math.min(m, n)]; // The singular values of X, sorted so
			                                    // that s[i] >= s[i+1].
			double U[][] = new double[m][n]; // if jobu = 'S', U contains the first min(m,n) columns of U
			                            // (the left singular vectors, stored columnwise);
			int ldu = m; //  The leading dimension of the array U.  ldu >= 1; if
			        //  jobu = 'S' or 'A', ldu >= m.
			double VT[][] = new double[n][n]; // If jobvt = 'A', VT contains the n-by-n orthogonal matrix V**T;
			int ldvt = n; // The leading dimension of the array VT.  ldvt >= 1; if
			         // jobvt = 'A', ldvt >= n; if jobvt = 'S', ldvt >= min(m,n).
			int nmax = 132;
			int lwork = (nmax * ((5 * nmax) + 5)) + 1;
			double[] work = new double[lwork];
			int[] info = new int[1]; //  = 0:  successful exit.
			                    // < 0:  if info[0] = -i, the i-th argument had an illegal value.
			                    // > 0:  if dbdsqr did not converge, info[0] specifies how many
			                    // superdiagonals of an intermediate bidiagonal form B
			                    // did not converge to zero.
			
			                    // if info[0] > 0, work[1:min(n,m)-1] contains the unconverged
			                    // superdiagonal elements of an upper bidiagonal matrix B
			                    // whose diagonal is in s (not necessarily sorted). B
			                    // satisfies A = U * B * VT, so it has the same singular values
			                    // as A, and singular vectors related by U and VT.
			SVD svd = new SVD();
			svd.dgesvd(jobu, jobvt, m, n, X.m_data, ldx, s, U, ldu, VT, ldvt, work, lwork, info);
			if (info[0] < 0) {
			  System.err.println("In svd.dgesvd argument " + (-info[0]) + " had an illegal value");
			  return 0;
			}
			if (info[0] > 0) {
				System.err.println("in svd.dgesvd dbdsqr did not converge.");
			    System.err.println(info[0] + " superdiagonals of an intermediate form B did not converge to zero.");
			     return 0;
			}
			m_vEigenVal = new CDVector(s.length, s);
		    Matrix vTMat = new Matrix(VT);
			Matrix vMat = vTMat.transpose();
			double V[][] = vMat.getArray();
			m_mEigenVec = new CDMatrix(n, n, V);
	        
	        
	        m_vEigenVal.Sqr();
	        m_vEigenVal.Reverse();
	        m_mEigenVec.FlipLR();
            */
	    } else {

	        /////////////////////////////////////
	        // use ordinary eigen analysis
	        /////////////////////////////////////                  
	       
	        // calc X^t by hand to avoid large VisSDK temporary objects
	        int i, j, n = X.NRows(), m = X.NCols();
	        CDMatrix X_t = new CDMatrix(m, n);    
	        for (i = 0; i < m; i++)
	            for (j = 0; j < n; j++)
	                X_t.m_data[i][j] = X.m_data[j][i];  // not very cache-coherent on X :-(                      
	        
	        // do eigen analysis
	        VisDMatrixSymmetricEigen( X_t.mult(X), m_vEigenVal, m_mEigenVec );
	        // EigenvalueDecomposition ed = new EigenvalueDecomposition(X_t.mult(X));
	        // m_vEigenVal = (CDVector)ed.getRealEigenvalues();
	        // m_mEigenVec = (CDMatrix)ed.getV();
	        
	        /*
	        CDMatrix T = new CDMatrix(m,m);
	        T.assign(X_t.mult(X));
	        int itype = 1;
	        final char jobz = 'V';
	        final char uplo = 'U';
	        
	       
	        final int lda = n;
	        CDMatrix B = new CDMatrix();
	        B.Resize( n, n );
	        B.Eye();
	        
	        final int ldb = n;
	        final double[] w = new double[n];
	        final double[] work = new double[100];
	        final int lwork = 100;
	        final int[] info = new int[1];
	        GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
	        ge.dsygv(itype, jobz, uplo, n, T.m_data, lda, B.m_data, ldb, w, work, lwork, info);
            // ge.dsyev(jobz, uplo, n, T.m_data, lda, w, work, lwork, info);
	        
	        if (info[0] != 0) {
	         	   System.err.println("info[0] = 0");
	        }
	        m_vEigenVal.assign(w);
	        m_mEigenVec.assign(T);
	        */
	    }    
			
		// estimate optimal k value and calc 'EigenVec_k' and 'EigenVal_k'
	    System.err.println( "Estimating k... " );    
		int k = EstimateK( C, EigenVec_k, EigenVal_k );
	    System.err.println("k = " + k);

		// calc R_k 
		R.Resize( m_nParams, m_nPixelsDiffs );

		// the line below is simlar to:
	    //
	    // R = C*EigenVec_k*(EigenVal_k.Inverted()*EigenVec_k.Transposed()*X.Transposed());
	    //
	    R.assign(C.mult(EigenVec_k).mult((EigenVal_k.Inverted().mult((X.mult(EigenVec_k)).Transposed()))));
	    
		return k;
	}
	
	/**
	 * Symmetric eigen analysis 
	 * @param A
	 * @param vals
	 * @param vects
	 * @return
	 */
	public boolean VisDMatrixSymmetricEigen(final CVisDMatrix A, CVisDVector vals, CVisDMatrix vects) {
		CVisDMatrix tmp = A;
		int M = A.NRows();
		assert(M == A.NCols());
		assert(vals.Length() == M);
		assert(vects.NRows() == M);
		assert(vects.NCols() == M);
		int info = Dsyev(M, tmp.m_data, vals.m_data, vects.m_data);
		if ( info != 0 ) {
			// MipavUtil.displayError("Cannot compute eigenvalue and eigenvectors in VisDMatrixSymmetricEigen");
			return false;
		}
		return true;
	}

	/**
	 * Call lapack routine
	 * @param m
	 * @param a
	 * @param vals
	 * @param vects
	 * @return
	 */
	public int Dsyev(final int m, double[][]a, double []vals, double[][] vects) {
		if (m < 1) return 1;	
		char jobz;	char uplo = 'U';	
		int []info = new int[1]; 
		int lda = m;	
		int lwork = 10*m; // should be >= max(1,3*m-1)	
		double []work = new double[lwork];	
		double [][]V;	
		jobz = (vects != null ? 'V' : 'N');	
		V = new double[m][m];	
		
		for ( int i = 0; i < m; i++ ) 
			for ( int j = 0; j < m; j++ ) 
		          V[i][j] = a[i][j];
		
		GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
	    ge.dsyev(jobz, uplo, m, V, lda, vals, work, lwork, info);
		
		if (vects != null) {		
			for (int i = 0; i < m; i++)			
				for (int j = 0; j < m; j++)				
					vects[i][j] = V[j][i];	
		}	
		work = null;	
		V = null;	
		return info[0];
	}
	
	
	
}