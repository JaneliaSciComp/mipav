package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.structures.jama.*;
import gov.nih.mipav.view.*;
import Jama.Matrix;

/**
 *  * AAM-API LICENSE  -  file: license.txt
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
 * NAME // VisDMatrix.h -- double precision matrix/vector operations 
 * DESCRIPTION 
 * 
 * CLASS:
 *      CVisDMatrix
 *  
 * 
 * The CVisDMatrix class provides some basic matrix operations,
 * using calls to external software (IMSL for the moment) to perform the more
 * complicated operations.
 * 
 * To take advantage of the IMSL numerical analysis routines, define VIS_USE_IMSL 
 * in the Build Settings C/C++ property page. 
 *  
 * Copyright (c) 1996-2000 Microsoft Corporation, All Rights Reserved 
 * 
 * Use partial routine for matrix manipulation for automatic prostate segmentation. 
 * 
 * @author Ruida Cheng
 * 
 */
public class CVisDMatrix {
	
	protected int m_nRows;        // number of rows in matrix
    protected int m_nCols;        // number of columns in matrix
    public double [][]m_data;    // Iliffe vector (array of pointers to data)
	protected boolean m_fExternalStorage;

	public int FieldType;
	public static int eftName = 0;
	public static int eftDims = 1;
	public static int eftData = 2;
	public static int eftEnd = 3;

	public static boolean s_fVerboseOutput;
	
	public CVisDMatrix() {
	    m_nRows = 0;
	    m_nCols = 0;
	    m_data = null; 
	    m_fExternalStorage = false;
	}
	
	
	public CVisDMatrix(int rows, int cols) {
		SetSize(rows, cols, null);
	}
	
	
	public CVisDMatrix(int rows, int cols, double []storage) {
		assert(rows >= 0);
		assert(cols >= 0);
		m_nRows = rows;
		m_nCols = cols;
		
		
		if ((rows > 0) && (cols > 0) && storage != null)
		{
			this.m_data = new double[rows][cols];
			int index = 0;
			for (int i = 0; i < m_nRows; i++ ) {
				for ( int j = 0; j < m_nCols; j++ ) {
					this.m_data[i][j] = storage[index];
					index++;
				}
			}
		}
	}
	
	public CVisDMatrix(int rows, int cols, double [][]storage)
	{
	    SetSize(rows, cols, storage);
	}

	public CVisDMatrix(final CVisDMatrix refmatrix)
	{
	    SetSize(refmatrix.NRows(), refmatrix.NCols());
		this.assign(refmatrix);
	}

	public void dispose()
	{
		FreeBuffer();
	}

	public boolean not_equals(final CVisDMatrix refmatrix) 
	{
	    return !( this.equals(refmatrix));
	}

	// operator+
	public CVisDMatrix add(final CVisDMatrix refmatrix)
	{
	    CVisDMatrix matrixRet = new CVisDMatrix(this);

	    return matrixRet.EqSum(this, refmatrix);
	}

	// operator-
	public CVisDMatrix sub(final CVisDMatrix refmatrix)
	{
	    CVisDMatrix matrixRet = new CVisDMatrix(this);

	    return matrixRet.EqDiff(this, refmatrix);
	}

	// operator*
	public CVisDVector mult(final CVisDVector refvector)
	{
	    CVisDVector vectorRet = new CVisDVector(NRows());

		return vectorRet.EqProd(this, refvector);
	}

	// operator*
	public CVisDMatrix mult(final CVisDMatrix refmatrix)
	{
	    CVisDMatrix matrixRet = new CVisDMatrix(NRows(), refmatrix.NCols());

	    return matrixRet.EqProd(this, refmatrix);
	}

	// operator*
	public CVisDMatrix mult(double dbl)
	{
	    CVisDMatrix matrixRet = new CVisDMatrix(this);

	    return (matrixRet.mult_into(dbl));
	}

	// operator/
	public CVisDMatrix div(double dbl)
	{
	    CVisDMatrix matrixRet = new CVisDMatrix(this);

	    return (matrixRet.div_into(dbl));
	}


	public CVisDMatrix Transpose()
	{
		this.assign(Transposed());

		return this;
	}

	public CVisDMatrix Submatrix(final int newRows, final int newCols, CVisDMatrix submat) {
		return Submatrix(newRows, newCols, submat, 0, 0 );
	}
	
	public CVisDMatrix Submatrix(final int newRows, final int newCols,
			final int i0, final int j0)
	{
		CVisDMatrix submat = new CVisDMatrix(newRows, newCols);
		Submatrix(newRows, newCols, submat, i0, j0);
		return submat;
	}

	public CVisDVector VisCrossProduct(final CVisDVector v1, final CVisDVector v2)
	{
		CVisDVector vOut = new CVisDVector();
		VisCrossProduct(v1, v2, vOut);
		return vOut;
	}
	
	
	public int NRows() { 
		return m_nRows; 
	}
	    
	public int NCols() { 
		return m_nCols; 
	}
	
	public void FreeBuffer()
	{
		assert(((m_nRows > 0) && (m_nCols > 0)) || (m_data == null));
		if (m_data != null)
		{
			m_data = null;
		}
	}
	
	public void SetSize(int rows, int cols) {
		SetSize(rows, cols, null);
	}
	
	
	public void SetSize(int rows, int cols, double[][] storage)
	{
		// This method is called by the constructors and the Resize method.
		// It assumes that the current buffer has already been freed.
		assert(rows >= 0);
		assert(cols >= 0);
		m_nRows = rows;
		m_nCols = cols;
		
		// Get a new buffer.
		// LATER:  We could try to be tricky here and use one memory allocation
		// to get the Iliffe vector and matrix data in a single memory block.
		// (We'd need to use type casts and calcualte the memory block size in bytes.)
		if ((rows > 0) && (cols > 0))
		{
			/*
			double []pdbl;

			m_fExternalStorage = (storage != null);
			if (!m_fExternalStorage)
			{
				int cdbl = rows * cols;
				pdbl = new double[cdbl];
			}
			else
			{
				pdbl = storage;
			}

			// Fill in the Iliffe vector.
			
			for (int iRow = 0; iRow < rows; ++iRow, pdbl += cols)
				m_data[iRow] = pdbl;
		      */
			m_fExternalStorage = (storage != null);
			if (!m_fExternalStorage)
			{
				this.m_data = new double[rows][cols];
			} else {
				this.m_data = storage;
			}
		}
		else
		{
			assert(m_nRows == 0);
			assert(m_nCols == 0);
			m_data = null;
			m_fExternalStorage = false;
		}
	}
	
	
	public void Resize(int rows, int cols) {
		Resize(rows, cols, null);
	}
	
	public void Resize(int rows, int cols, double[][] storage)
	{
		if ((m_nRows != rows) || (m_nCols != cols) || (storage != null)
				|| (m_fExternalStorage))
		{
			// Delete the old buffer, if needed.
			FreeBuffer();

			// Find the new buffer.
			SetSize(rows, cols, storage);
		}
	}
	

	////////////////////////////////////////////////////////////////////////////
	//
	//FUNCTION:        operator=
	//
	//DECLARATION:
	//CVisDMatrix& CVisDMatrix::operator=(const CVisDMatrix &mat);
	//
	//RETURN VALUE:
	//reference to l.h.s.
	//INPUT:
	//&mat (const CVisDMatrix) - matrix being copied
	//
	//DISCRIPTION:
	//Assignment operator
	//
	////////////////////////////////////////////////////////////////////////////
	// CVisDMatrix::operator=
	public CVisDMatrix assign(final CVisDMatrix mat)
	{
		// This "if" statement was added on Nov. 3, 1999 to allow templated
		// array classes to copy elements when resizing the array.
		if (NRows() == 0 && NCols() == 0)
		Resize(mat.NRows(), mat.NCols());
		
		assert(NRows() == mat.NRows() && NCols() == mat.NCols());
		
		if (m_nRows != 0)
		{
			assert(m_nRows > 0);
			assert(m_nCols > 0);
			// System.arraycopy(mat.m_data, 0, m_data, 0, m_nRows*m_nCols);
			for (int i = 0; i < m_nRows; i++ ) {
				for ( int j = 0; j < m_nCols; j++ ) {
					this.m_data[i][j] = mat.m_data[i][j];
				}
			}
		}
		
		return this;
	}
	
	////////////////////////////////////////////////////////////////////////////
	//  
	//  FUNCTION:        operator=
	//  
	//  DECLARATION:
	//          CVisDMatrix& CVisDMatrix::operator=(double value);
	//  
	//  RETURN VALUE:
	//      reference to l.h.s.
	//  INPUT:
	//      value (double) - fill value
	//  
	//  DISCRIPTION:
	//      Assignment operator
	//  
	////////////////////////////////////////////////////////////////////////////
	// CVisDMatrix::operator=
	public CVisDMatrix assign(double value)
	{
		if ((value == 0.0) && (m_nRows != 0))
		{
			assert(m_nRows > 0);
			assert(m_nCols > 0);
	
			// IEEE float
	        // memset(m_data[0], 0, m_nRows*m_nCols*sizeof(double));
			m_data = new double[m_nRows][m_nCols];
		}
		else
		{
			// int n = m_nRows * m_nCols;
			// double []p = m_data;
			for (int i = 0; i < m_nRows; i++) {
				for ( int j = 0; j < m_nCols; j++ ) {
					// m_data[i] = value;
					this.m_data[i][j] = value;
				}
			}
		}
	
		return this;
	}
	
	
	// CVisDMatrix::operator==
	public boolean equals(final CVisDMatrix refmatrix)
	{
	    if ((NRows() == refmatrix.NRows())
				&& (NCols() == refmatrix.NCols()))
		{
			if ((NRows() == 0) || (NCols() == 0))
				return true;

			// int cbRow = NCols();
			for (int iRow = 0; iRow < NRows(); ++iRow)
			{
				for ( int iCol = 0; iCol < NCols(); ++iCol) {
				/*
				if (m_data[iRow] != refmatrix.m_data[iRow])
					return false;
				*/
					if( m_data[iRow][iCol] != refmatrix.m_data[iRow][iCol])
					        return false;
				}
				
			}
			
			return true;
		}


	    return false; 
	}

	
	// CVisDMatrix::operator<
	public boolean lessThan(final CVisDMatrix refmatrix)
	{
	    if (NRows() == refmatrix.NRows())
		{
			if (NCols() == refmatrix.NCols())
			{
				if ((NRows() == 0) || (NCols() == 0))
					return false;

				for (int iRow = 0; iRow < NRows(); ++iRow)
				{
					for ( int iCol = 0; iCol < NCols(); ++ iCol) {
					   if ( m_data[iRow][iCol] < refmatrix.m_data[iRow][iCol] ) 
						   return true;
					}		
				}
				
				return false;
			}
	    
			return (NCols() < refmatrix.NCols()); 
		}

	    return (NRows() < refmatrix.NRows()); 
	}

	// CVisDMatrix::operator+=
	public CVisDMatrix add_into(final CVisDMatrix refmatrix)
	{
		assert((NRows() == refmatrix.NRows())
				&& (NCols() == refmatrix.NCols()));

	    for (int i = 0; i < NRows(); i++)
	        for (int j = 0; j < NCols(); j++)
	            this.m_data[i][j] += refmatrix.m_data[i][j];

	    return this;
	}

	
	//CVisDMatrix::operator-=
	public CVisDMatrix sub_into(final CVisDMatrix refmatrix)
	{
		assert((NRows() == refmatrix.NRows())
				&& (NCols() == refmatrix.NCols()));

	    for (int i = 0; i < NRows(); i++)
	        for (int j = 0; j < NCols(); j++)
	            this.m_data[i][j] -= refmatrix.m_data[i][j];

	    return this;
	}
	
	// CVisDMatrix::operator*=
	public CVisDMatrix mult_into(final CVisDMatrix refmatrix)
	{
		// LATER:  Add an optimization to avoid  allocating memory for small matrices.
		CVisDMatrix matrixT = new CVisDMatrix(this);

	    assert(matrixT.NCols() == refmatrix.NRows());

		if (refmatrix.NCols() != refmatrix.NRows())
			Resize(NRows(), refmatrix.NCols());

	    for (int i = 0; i < matrixT.NRows(); i++)
		{
	        for (int j = 0; j < refmatrix.NCols(); j++)
			{
	            double sum = 0.0;
	            for (int k = 0; k < NCols(); k++)
	                sum += matrixT.m_data[i][k] * refmatrix.m_data[k][j];
	            this.m_data[i][j] = sum;
	        }
	    }

		return this;
	}

	// CVisDMatrix::operator*=
	public CVisDMatrix mult_into(double dbl)
	{
	    for (int i = 0; i < NRows(); i++)
	        for (int j = 0; j < NCols(); j++)
	            this.m_data[i][j] *= dbl;

	    return this;
	}

	// CVisDMatrix::operator/=
	public CVisDMatrix div_into(double dbl)
	{
		assert(dbl != 0);

	    for (int i = 0; i < NRows(); i++)
	        for (int j = 0; j < NCols(); j++)
	            this.m_data[i][j] /= dbl;

	    return this;
	}

	//CVisDMatrix::operator-
	public CVisDMatrix neg()
	{
	    CVisDMatrix matrixRet = new CVisDMatrix(NRows(), NCols());

	    for (int i = 0; i < NRows(); i++)
	        for (int j = 0; j < NCols(); j++)
	            matrixRet.m_data[i][j] = - this.m_data[i][j];

	    return matrixRet;
	}

	
	public CVisDMatrix EqSum(final CVisDMatrix refmatrixA, final CVisDMatrix refmatrixB)
	{
	    int nRows = NRows(); 
	    int nCols = NCols(); 
	    assert(nRows == refmatrixA.NRows());
	    assert(nCols == refmatrixA.NCols());
	    assert(nRows == refmatrixB.NRows());
	    assert(nCols == refmatrixB.NCols());

	    for (int i = 0; i < nRows; i++)
	        for (int j = 0; j < nCols; j++)
	            this.m_data[i][j] = refmatrixA.m_data[i][j] + refmatrixB.m_data[i][j];

	    return this;
	}

	public CVisDMatrix EqDiff(final CVisDMatrix refmatrixA, final CVisDMatrix refmatrixB)
	{
	    int nRows = NRows(); 
	    int nCols = NCols(); 
	    assert(nRows == refmatrixA.NRows());
	    assert(nCols == refmatrixA.NCols());
	    assert(nRows == refmatrixB.NRows());
	    assert(nCols == refmatrixB.NCols());

	    for (int i = 0; i < nRows; i++)
	        for (int j = 0; j < nCols; j++)
	            this.m_data[i][j] = refmatrixA.m_data[i][j] - refmatrixB.m_data[i][j];

	    return this;
	}

	public CVisDMatrix EqProd(final CVisDMatrix refmatrixA, final CVisDMatrix refmatrixB)
	{
	    int nRows = NRows(); 
	    int nCols = NCols(); 
	    int nTerms = refmatrixA.NCols(); 
	    assert(nRows == refmatrixA.NRows());
	    assert(nTerms == refmatrixB.NRows());
	    assert(refmatrixB.NCols() == nCols);

	    for (int i = 0; i < nRows; i++) {
	        for (int j = 0; j < nCols; j++) {
	            double sum = 0.0;
	            for (int k = 0; k < nTerms; k++)
	                sum += refmatrixA.m_data[i][k] * refmatrixB.m_data[k][j];
	            this.m_data[i][j] = sum;
	        }
	    }

	    return this;
	}

	

	//
	//  Matrix inverse
	//
    
	// compute the inverse of a square matrix 'a', and its determinant if p_det != 0
		// if 'a' is not inversible, then p_det will get 0, or returns a nonzero value 
		// if p_det = 0
		// note that 'a' will be overwritten,  lapack
		public int Dgetrf( final int m, double[][] a, double[] p_det) {
			int []info = new int[1];
			int []ipiv = new int[m];
			LinearEquations2  le = new LinearEquations2();
			
			// compute the LU factorization
			le.dgetrf(m, m, a, m, ipiv, info);
			
			if ( info[0] > 0 ) { // not inversible
				if ( p_det[0] != 0 ) {
					p_det[0] = 0;
				}
				ipiv = null;
				return info[0];
			}
			
			// compute the determinant
			if ( p_det[0] != 0 ) {
				p_det[0] = 1.0;
				int i;
				for ( i = 0; i < m; i++ ) {
					p_det[0] *= a[i][i];  // product of diagonal elements
				}
				// we have to multiply by the signature of permutation in ipiv
				// to get the correct sign
				for ( i = 0; i < m; i++ ) {
					if ( ipiv[i] != (i+1) ) {
						p_det[0] *= -1.0;
					}
				}
			}
			
			// compute the inverse
			int lwork = m * 5;
			double[] work = new double[lwork];
			le.dgetri(m, a, m, ipiv, work, lwork, info);
		    ipiv = null;
		    work = null;
		    return info[0];
			
		}
	
	public CVisDMatrix Invert()
	{
	    assert ((NRows() == NCols()) && (NRows() > 0));
	    double []det = new double[1];
		// With CLAPACK, we have the code in the Inverted method.
		LinearEquations2 le = new LinearEquations2();
	    int info = Dgetrf(NRows(), this.m_data, det);
	    if ( info != 0 ) {
	        MipavUtil.displayError("Matrix is not inversible.");
	    }
	    return this;
	  /* Bill's version of invert
	   int ipiv[] = new int[Math.min(m_data.length, m_data[0].length)];
	   int info[] = new int[1];
	   le.dgetrf(m_data.length, m_data[0].length, m_data, m_data.length, ipiv, info);
	   if (info[0] < 0) {
	         MipavUtil.displayError("The argument " + (-info[0])  + " had an illegal value");
	   } else if (info[0] > 0) {
	        int i = info[0] - 1;
	        MipavUtil.displayError("U["+i+"]["+i+"] is exactly zero.  The factorization has been completed, but the factor U is exactly singular, and division by zero will occur if it is used to solve a system of equations");
	   }
	
	   // this.assign(m_data);
		return this;
		*/
	}
	
	public CVisDMatrix Inverted() {
		assert ((NRows() == NCols()) && (NRows() > 0));
        CVisDMatrix copy = new CVisDMatrix(this);
        return copy.Invert();
	}
	
	/*
	public CVisDMatrix Inverted()
	{
		assert ((NRows() == NCols()) && (NRows() > 0));
			// Without CLAPACK, we have the code in the Inverted method.
			CVisDMatrix matrixInverse = new CVisDMatrix(NRows(), NCols());
	
			// Use Gauss-Jordan elimination
			int i, j, n = NRows();
			CVisDMatrix matrixT = new CVisDMatrix(n, 2*n);
			for (i = 0; i < n; i++) {       // copy into a single system
				for (j = 0; j < n; j++) {
					matrixT.m_data[i][j] = this.m_data[i][j];
					matrixT.m_data[i][j+n] = (i == j ? 1 : 0);
				}
			}
			VisGaussJordanSolveDMatrix(matrixT);
			for (i = 0; i < n; i++)
				for (j = 0; j < n; j++)
					matrixInverse.m_data[i][j] = matrixT.m_data[i][j+n];
			return matrixInverse;
	}
	*/
	
	 public double Determinant() {
	    	assert ((NRows() == NCols()) && (NRows() > 0));
	    	double[] det = new double[1];
	    	CVisDMatrix matrixInverse = new CVisDMatrix(NRows(), NCols(), this.m_data);
	    	int info = Dgetrf(NRows(), matrixInverse.m_data, det);
	    	return det[0];
	    	
	    	/* version 2: Bill's porting, which is missing CLAPACK calling context
	    	assert ((NRows() == NCols()) && (NRows() > 0));
	    	double[] det = new double[1];
	    	
	    	CVisDMatrix matrixInverse = new CVisDMatrix(NRows(), NCols(), m_data);
	    	double[][] inverseData = matrixInverse.m_data;
	    	
		   LinearEquations2 le = new LinearEquations2();
		   int ipiv[] = new int[Math.min(inverseData.length, inverseData[0].length)];
		   int info[] = new int[1];
		   
		   le.dgetrf(inverseData.length, inverseData[0].length, inverseData, inverseData.length, ipiv, info);
		   if (info[0] < 0) {
		       MipavUtil.displayError("The argument " + (-info[0])  + " had an illegal value");
		       return -1;
		   }
		   else if (info[0] > 0) {
		       int i = info[0] - 1;
		       MipavUtil.displayError("U["+i+"]["+i+"] is exactly zero.  The factorization has been completed, but the factor U is exactly singular, and division by zero will occur if it is used to solve a system of equations");
		       return -1;
		   }

	    	
	       return det[0];
	       */  
	    	
	    	/*  version 3
	    	double result = 0;
	    	for ( int i = 0; i < m[0].length; i++ ) {
	    		double temp[][] = new double[m.length - 1][m[0].length - 1];

	    		for(int j = 1; j < m.length; j++) {
		    		System.arraycopy(m[j], 0, temp[j-1], 0, i);
		    		System.arraycopy(m[j], i+1, temp[j-1], i, m[0].length-i-1);
	    		}

	    		result += m[0][i] * Math.pow(-1, i) * Determinant(temp); 
	    	}
	    	return result;
	    	*/
	    }
	    
	 
	public double Determinant(CVisDMatrix matrixInverse) {
		double[] det = new double[1];
		matrixInverse = new CVisDMatrix(NRows(), NCols(), this.m_data);
		int info = Dgetrf(NRows(), matrixInverse.m_data, det);	
		return det[0];
	}
     
	public CVisDMatrix Transposed()
	{
	    int i, j, n = NRows(), m = NCols();
	    CVisDMatrix matrixTranspose = new CVisDMatrix(m, n);

	    assert(m > 0 && n > 0);

	    for (i = 0; i < m; i++)
	        for (j = 0; j < n; j++)
	            matrixTranspose.m_data[i][j] = this.m_data[j][i];

		return matrixTranspose;
	}

	public boolean IsSymmetric()
	{
	    if (NRows() != NCols())
	        return false;

	    for (int i = 0; i < NRows(); i++)
	        for (int j = i+1; j < NCols(); j++)
	            if (this.m_data[i][j] != this.m_data[j][i])
	                return false;

	    return true;
	}

	////////////////////////////////////////////////////////////////////////////
	//  
	//  FUNCTION:     VisGaussJordanSolveDMatrix   
	//  
	//  DECLARATION:
	//          static void VisGaussJordanSolveDMatrix(CVisDMatrix& A)
	//  
	//  INPUT:
	//    CVisDMatrix& A - a matrix of coefficients.
	//  
	//  DISCRIPTION:
	//
	// Gauss-Jordan elimination, no pivoting:  not very stable or accurate
	//
	//  
	////////////////////////////////////////////////////////////////////////////
	
	
	public void VisGaussJordanSolveDMatrix(CVisDMatrix A)
	{
	    int n = A.NRows(), m = A.NCols(), i, j, k;
	    
	    // Reduce to triangular form
	    for (i = 0; i < n; i++) {
	        
	        // Make sure diagonal entry is non-zero
	        if (A.m_data[i][i] == 0.0) {
	            for (j = i+1; j < n; j++)
	                if (A.m_data[j][i] != 0.0)   // find non-zero entry
	                    break;
	            if (j >= n)      // if failed, matrix is singular
	                System.err.println("matrix is singular" + "VisGaussJordanSolveDMatrix()");
	            for (k = i; k < m; k++)
	                A.m_data[i][k] += A.m_data[j][k];
	        }
	
	        // VisDMatrixScale this row to unity
	        double aii_inv = 1.0 / A.m_data[i][i];
	        A.m_data[i][i] = 1.0;
	        for (k = i+1; k < m; k++)
	            A.m_data[i][k] *= aii_inv;
	
	        // Subtract from other rows
	        for (j = i+1; j < n; j++) {
	            double aij = A.m_data[j][i];
	            A.m_data[j][i] = 0.0;
	            for (k = i+1; k < m; k++)
	                A.m_data[j][k] -= aij * A.m_data[i][k];
	        }
	    }
	
	    // Back substitute
	    for (i = n-1; i > 0; i--) {
	        for (j = 0; j < i; j++)  {
	            double aji = A.m_data[j][i];
	            A.m_data[j][i] = 0.0;
	            for (k = n; k < m; k++)
	                A.m_data[j][k] -= aji * A.m_data[i][k];
	        }
	    }
	}
	
	public CVisDMatrix VisDMatrixSqrtInverse(CVisDMatrix A) {
		CVisDMatrix result = new CVisDMatrix(A.NRows(), A.NCols());
		VisDMatrixSqrtInverse(A, result);
		return result;
	}
	
	public void VisDMatrixSqrtInverse(CVisDMatrix A, CVisDMatrix AsqrtInv) {
		   assert(A.NRows() == A.NCols() && A.NRows() > 0);
		    assert(A.NRows() == AsqrtInv.NRows());
		    assert(A.NCols() == AsqrtInv.NCols());

		    int i, j, n = A.NRows();
		    CVisDMatrix B = new CVisDMatrix(n, n);

		    B = A.Transposed().mult(A);

		    CVisDVector Eval = new CVisDVector(n);
		    CVisDMatrix Evec = new CVisDMatrix(n,n);
		    
			VisDMatrixSymmetricEigen(B, Eval, Evec);

			for (i=0; i<n; i++){
				assert(Eval.m_data[i] > 0.0);
				Eval.m_data[i] = 1.0f/Math.sqrt(Eval.m_data[i]); // can be easily modified for sqrt
			}

			for (i = 0; i < n; i++){
				for (j = 0; j < n; j++){ 
					AsqrtInv.m_data[i][j] = 0.0; 
					for (int k=0; k<n; k++)
						AsqrtInv.m_data[i][j] += Eval.m_data[k] * Evec.m_data[i][k] * Evec.m_data[j][k];
				}
			}
		    
	
	}

	public boolean VisDMatrixSymmetricEigen(final CVisDMatrix A, CVisDVector vals, CVisDMatrix vects) {
		CVisDMatrix tmp = A;
		int M = A.NRows();
		assert(M == A.NCols());
		assert(vals.Length() == M);
		assert(vects.NRows() == M);
		assert(vects.NCols() == M);
		int info = Dsyev(M, tmp.m_data, vals.m_data, vects.m_data);
		if ( info != 0 ) {
			return false;
			// MipavUtil.displayError("Cannot compute eigenvalue and eigenvectors in VisDMatrixSymmetricEigen");
		}
		return true;
	}
	
	// compute all eigenvalues, and optionally (if 'vects' != 0), eigenvectors,
	// of a real symmetric matrix
	// the eigenvalues are in ascending order
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
	
	
	// extract the submatrix of `newRows'x`newCols' from (i0,j0), 
//	     and put it to `submat' whose reference is also returned
	// added by zhang on June 9, 1998
	public CVisDMatrix Submatrix(final int newRows, final int newCols, CVisDMatrix submat, final int i0,  final int j0) 
	{
		assert(newRows > 0);
	    assert(newCols > 0);
		assert(i0 >= 0);
		assert(j0 >= 0);
	    assert(i0 + newRows <= NRows());
	    assert(j0 + newCols <= NCols());
	    submat.Resize(newRows, newCols);
	    // this procedure assumes that a matrix is stored in rows 
	    for (int i = 0; i < newRows; i++) {
			// memcpy(&submat[i][0], &(*this)[i0+i][j0], newCols*sizeof(double));
	    	for ( int j = 0; j < newCols; j++ ) {
	    		// System.arraycopy(this.m_data[i0+i], 0, submat.m_data[i],0, newCols);
	    		submat.m_data[i][j] = this.m_data[i0+i][j0+j];
	    	}
	    }
	    return submat;
	}

	// return the r-th row
	// added by zhang on Aug. 14, 1998
	public CVisDVector Row(int r) 
	{
		assert(r < NRows());
		assert(r >= 0);
		CVisDVector row = new CVisDVector(NCols());
		// this procedure assumes that a matrix is stored in rows
		// memcpy(&row[0], &(*this)[r][0], NCols()*sizeof(double));
		// System.arraycopy(this.m_data[r], 0, row.m_data, 0, NCols());
		for ( int i = 0; i < NCols(); i++ ) {
			row.m_data[i] = this.m_data[r][i];
		}
		return row;
	}

	// return the c-th column
	// added by zhang on Aug. 14, 1998
	public CVisDVector Column(int c)
	{
		assert(c < NCols());
		assert(c >= 0);
		CVisDVector col = new CVisDVector(NRows());
		for (int i = 0; i < NRows(); i++)
			col.m_data[i] = this.m_data[i][c];
		return col;
	}

	// set the r-th row
	// added by zhang on Feb. 2, 1999
	public void SetRow(int r, final CVisDVector v)
	{
		int N = NCols();
		if (N > v.Length())
			N = v.Length();
	    // this procedure assumes that a matrix is stored in rows
		// memcpy(&(*this)[r][0], &v[0], N*sizeof(double));
		// System.arraycopy(v.m_data, 0, this.m_data[r], 0, N);
		for ( int i = 0; i < N; i++ ) {
			this.m_data[r][i] = v.m_data[i];
		}
	}

	// set the c-th column
	// added by zhang on Feb. 2, 1999
	public void SetColumn(int c, final CVisDVector v)
	{
		int N = NRows();
		if (N > v.Length())
			N = v.Length();
		for (int i = 0; i < N; i++)
			this.m_data[i][c] = v.m_data[i];
	}

	// outer product of two vectors
	// added by zhang on Sept. 4, 1998
	public CVisDMatrix VisOuterProduct(final CVisDVector v1, final CVisDVector v2)
	{
		int M = v1.Length();
	    int N = v2.Length();
	    CVisDMatrix A = new CVisDMatrix(M, N);
		for (int j = 0; j < N; j++)
		    for (int i = 0; i < M; i++)
				A.m_data[i][j] = v1.m_data[i] * v2.m_data[j];
	    return A;
	}

	// cross product of two vectors
	// added by zhang on 30 sept. 1998
	public void VisCrossProduct( final CVisDVector x, final CVisDVector y, CVisDVector z)
	{
	    int N = x.Length();
	    if (N < 2 || N != y.Length()) {
			System.err.println("CrossProduct only works for two vectors of same dimension (>= 2)" + "VisCrossProduct");
	    }
	    
	    int size = N * (N - 1) / 2 - 1;

	    z.Resize(size+1);

	    int inc = 0;
	    double cross;
	    for (int i = size; i > 0; i--)
	        for (int j = (i - 1); j >= 0; j--) {
			    cross = x.m_data[i] * y.m_data[j] - y.m_data[i] * x.m_data[j];
	            z.m_data[inc] = (((i-j)%2)==0) ? cross : -cross;
	            inc++;
	        }
	}

	////////////////////////////////////////////////////////////////////////////
	//
	//FUNCTION:        VisDMatrixLeastSquares
	//
	//DECLARATION:
	//CVisDMatrix VisDMatrixLeastSquares(const CVisDMatrix& A,
	//const CVisDMatrix& B);
	//void VisDMatrixLeastSquares(const CVisDMatrix& A,
	//const CVisDMatrix& B, CVisDMatrix& X);
	//CVisDVector VisDMatrixLeastSquares(const CVisDMatrix& A,
	//const CVisDVectir& b);
	//void VisDMatrixLeastSquares(const CVisDMatrix& A,
	//const CVisDVector& b, CVisDVector& x);
	//
	//INPUT:
	//A (const CVisDMatrix&) - input matrix (l.h.s. of linear system)
	//
	//B (const CVisDMatrix&) - input matrix (r.h.s. of linear system)
	//
	//X (CVisDMatrix&) - output matrix (solution to linear system)
	//
	//b (const CVisDVector&) - input vector (r.h.s. of linear system)
	//
	//x (CVisDVector&) - output vector (solution to linear system)
	//
	//DISCRIPTION:
	//Least squares system solution using Householder transforms
	//(with IMSL), or using normal matrix solution (without)
	//
	//
	////////////////////////////////////////////////////////////////////////////
	public CVisDMatrix VisDMatrixLeastSquares(final CVisDMatrix A, final CVisDMatrix B)
	{
		CVisDMatrix result = new CVisDMatrix(B.NRows(), B.NCols());
		VisDMatrixLeastSquares(A, B, result);
		return result;
	}
	
	public CVisDVector VisDMatrixLeastSquares(final CVisDMatrix A, final CVisDVector b)
	{
	    CVisDVector result = new CVisDVector(A.NCols());
	    VisDMatrixLeastSquares(A, b, result);
	    return result;
	}

	public void VisDMatrixLeastSquares(final CVisDMatrix A, final CVisDVector b, CVisDVector x)
	{
	    CVisDMatrix B = new CVisDMatrix(b.Length(), 1, b.m_data);
	    CVisDMatrix X = new CVisDMatrix(x.Length(), 1, x.m_data);
	    VisDMatrixLeastSquares(A, B, X);
	}

	public void VisDMatrixLeastSquares(final CVisDMatrix A, final CVisDMatrix B, CVisDMatrix X)
	{
	    assert(A.NRows() == B.NRows() && A.NRows() > 0);
		// Form the normal equations
		CVisDMatrix An = new CVisDMatrix(A.NCols(), A.NCols());
		CVisDMatrix Bn = new CVisDMatrix(B.NCols(), A.NCols());
		An.assign(0.0);
		Bn.assign(0.0);
		for (int i = 0; i < A.NRows(); i++) {
			for (int j = 0; j < A.NCols(); j++) {
				for (int k = 0; k < A.NCols(); k++)
					An.m_data[j][k] += A.m_data[i][j] * A.m_data[i][k];
				for (int k = 0; k < B.NCols(); k++)
					Bn.m_data[k][j] += A.m_data[i][j] * B.m_data[i][k];
			}
		}

		// Solve for each column of X independently
		CVisDVector b = new CVisDVector(X.NRows());
		for (int j = 0; j < X.NCols(); j++) {
			// Copy the input (rhs)
			for (int i = 0; i < X.NRows(); i++)
				b.m_data[i] = Bn.m_data[i][j];

			// Solve the system
			CVisDVector x = VisDMatrixSolveSPD(An, b);
        
			// Copy to the output (solution)
			for (int i = 0; i < X.NRows(); i++)
				X.m_data[i][j] = x.m_data[i];
		}
		
	}
	
	public int VisDMatrixLLS(CVisDMatrix A, CVisDMatrix B, CVisDMatrix X) {
		int m = A.NRows();
		int n = A.NCols();
		int nrhs = B.NCols();
		CVisDVector sVals = new CVisDVector((m < n) ? m : n);
		CVisDMatrix AT = A.Transposed();
		CVisDMatrix BT = B.Transposed();
		
		int[] rank = new int[1];
		int info = Dgelss(m, n, AT.m_data, nrhs, BT.m_data, sVals.m_data, rank, X.m_data);
		if (info != 0 ) 
			System.err.println("Cannot compute LLS A X = B" + "VisDMatrixLLS");
             
		return rank[0];
		
	}
	
	// Computes the minimum norm least squares solution to an over-
	// or under-determined system of linear equations A X=B, using
	// the singular value decomposition of A.
	public int Dgelss(int m, int n, double [][]AT, int nrhs, double[][] BT, double[] sVals, int[] pRank, double[][] X) {
		
		double rcond = -1.0; // using machine precision
		int[] info = new int[1];
		info[0] = 0;
		int lwork = 5*m*n + nrhs; // larger than necessary
		double[] work = new double[lwork];
		int ldb = (m > n) ? m : n;
		double [][]mat = new double[ldb][nrhs];
		
		for (int j = 0; j < nrhs; j++)
			for (int i = 0; i < m; i++)
				mat[i][j] = BT[i][j];
		
		GeneralizedInverse2 ge2 = new GeneralizedInverse2();
		ge2.dgelss(m, n, nrhs, AT, m, mat, ldb, sVals, rcond, pRank, work, lwork, info);
		
		for (int k = 0; k < nrhs; k++)
			for (int l = 0; l < n; l++)
			    X[l][k] = mat[k][l];	
		
		mat = null;
		work = null;
		
		return info[0];
	}
	

	// Symmetric Positive Definite matrix A:

	////////////////////////////////////////////////////////////////////////////
	//  
	//  FUNCTION:        VisDMatrixSolveSPD
	//  
	//  DECLARATION:
//	          CVisDVector VisDMatrixSolveSPD(const CVisDMatrix& A,
//	                        const CVisDVector& b);
	//  
	//  RETURN VALUE:
//	      result of linear system solution
	//  INPUT:
//	      A (const CVisDMatrix&) - input matrix
	//  
//	      b (const CVisDVector&) - input vector (r.h.s. of linear system)
//	                  
	//  
	//  DISCRIPTION:
//	      Linear system solution of a symmetric positive definite matrix
//	      
	//  
	////////////////////////////////////////////////////////////////////////////
	public CVisDVector VisDMatrixSolveSPD(final CVisDMatrix A, final CVisDVector b)
	{
		int n = -1;
	    CVisDVector result = new CVisDVector(b.Length());
	    VisDMatrixSolveSPD(A, b, result, n);
	    return result;
	}

	
	////////////////////////////////////////////////////////////////////////////
	//  
	//  FUNCTION:        VisDMatrixSolveSPD
	//  
	//  DECLARATION:
	//          void VisDMatrixSolveSPD(const CVisDMatrix& A, const CVisDVector& b,
	//                      CVisDVector& x, int n);
	//  
	//  INPUT:
	//      A (const CVisDMatrix&) - input matrix (l.h.s. of linear system)
	//                  
	//  
	//      b (const CVisDVector&) - input vector (r.h.s. of linear system)
	//                  
	//  
	//      x (CVisDVector&) - output vector (solution to linear system)
	//                  
	//  
	//      n (int) - size of linear system (may be smaller than size of A)
	//                  
	//  
	//  DISCRIPTION:
	//      Linear system solution of a symmetric positive definite matrix
	//      
	//  
	////////////////////////////////////////////////////////////////////////////
	public void VisDMatrixSolveSPD(final CVisDMatrix A, final CVisDVector b, CVisDVector x, int n) {
		if (n < 1) // optionally solve a sub-system (faster)
			n = A.NRows();
		assert (A.NRows() == A.NCols() && A.NRows() > 0);
		assert (A.NRows() == b.Length());
		
		CVisDMatrix tmp = new CVisDMatrix(n, n);
		if ( n != A.NRows() ) {
			A.Submatrix(n, n, tmp);
		} else {
			tmp = A;
		}
		
		double x2D[][] = new double[n][1];
		for (int i = 0; i < n; i++) {
			x2D[i][0] = x.m_data[i];
		}
		int info = Dposv(n, tmp.m_data, 1, b.m_data, x2D);
		for (int i = 0; i < n; i++) {
			x.m_data[i] = x2D[i][0];
		}
		if ( info != 0 ) {
		   MipavUtil.displayError("Cannot solve A x = b with Symmetric A in VisDMatrixSolveSPD");
		}
		
		/*
		// Use Gauss-Jordan elimination
		int i, j;
		CVisDMatrix B = new CVisDMatrix(n, n + 1);
		for (i = 0; i < n; i++) { // copy into a single system
			for (j = 0; j < n; j++)
				B.m_data[i][j] = A.m_data[i][j];
			B.m_data[i][n] = b.m_data[i];
		}
		VisGaussJordanSolveDMatrix(B);
		for (i = 0; i < n; i++)
			x.m_data[i] = B.m_data[i][n];
        */
	}

	// lapack
	public int Dposv(final int n, double[][] A, final int nrhs, final double[] BT, double[][] XT) {
		int[] info = new int[1];
		char uplo = 'U';
		
		for (int i = 0; i < n; i++) {
			XT[i][0] = BT[i];
		}
		LinearEquations le = new LinearEquations();
		le.dposv(uplo, n, nrhs, A, n, XT, n, info);
		return info[0];
	}
	
		//
	//  Linear systems solution
	//
	
	////////////////////////////////////////////////////////////////////////////
	//  
	//  FUNCTION:        VisDMatrixSolve
	//  
	//  DECLARATION:
	//          CVisDVector VisDMatrixSolve(const CVisDMatrix& A,
	//                        const CVisDVector& b);
	//  
	//  RETURN VALUE:
	//      result vector
	//  INPUT:
	//      A (const CVisDMatrix&) - input matrix (l.h.s. of linear system)
	//                  
	//  
	//      b (const CVisDVector&) - input vector (r.h.s. of linear system)
	//                  
	//  
	//  DISCRIPTION:
	//      Linear system solution
	//  
	////////////////////////////////////////////////////////////////////////////
	public CVisDVector VisDMatrixSolve(final CVisDMatrix A, final CVisDVector b)
	{
	    CVisDVector result = new CVisDVector(b.Length());
	    VisDMatrixSolve(A, b, result);
	    return result;
	}
	
	////////////////////////////////////////////////////////////////////////////
	//  
	//  FUNCTION:        VisDMatrixSolve
	//  
	//  DECLARATION:
	//          void VisDMatrixSolve(const CVisDMatrix& A, const CVisDVector& b,
	//                        CVisDVector& x);
	//  
	//  INPUT:
	//      A (const CVisDMatrix&) - input matrix (l.h.s. of linear system)
	//                  
	//  
	//      b (const CVisDVector&) - input vector (r.h.s. of linear system)
	//                  
	//  
	//      x (CVisDVector&) - output vector (solution to linear system)
	//                  
	//  
	//  DISCRIPTION:
	//      Linear system solution
	//  
	////////////////////////////////////////////////////////////////////////////
	public void VisDMatrixSolve(final CVisDMatrix A, final CVisDVector b, CVisDVector x) {
		assert (A.NRows() == A.NCols() && A.NRows() > 0);
		assert (A.NRows() == b.Length());
       
		int n = A.NRows();
		
		CVisDMatrix AT = A.Transpose();
		
		double x2D[][] = new double[n][1];
		for (int i = 0; i < n; i++) {
			x2D[i][0] = x.m_data[i];
		}
		
		int info = Dgesv(A.NRows(), AT.m_data, 1, b.m_data, x2D);
		
		for (int i = 0; i < n; i++) {
			x.m_data[i] = x2D[i][0];
		}
		
		if ( info != 0 ) {
			MipavUtil.displayError("Cannot solve A x = b in VisDMatrixSolve");
		}
		// lapack Dgesv 
		/*
		// Use Gauss-Jordan elimination
		int i, j, n = A.NRows();
		CVisDMatrix B = new CVisDMatrix(n, n + 1);
		for (i = 0; i < n; i++) { // copy into a single system
			for (j = 0; j < n; j++)
				B.m_data[i][j] = A.m_data[i][j];
			B.m_data[i][n] = b.m_data[i];
		}
		VisGaussJordanSolveDMatrix(B);
		for (i = 0; i < n; i++)
			x.m_data[i] = B.m_data[i][n];
        */
	}

	// lapack
	public int Dgesv(final int n, double[][] AT, final int nrhs, final double[] BT, double[][] XT) {

		int []ipiv = new int[n];
		for (int i = 0; i < n; i++) {
			XT[i][0] = BT[i];
		}
		int[] info = new int[1];
		LinearEquations2 le = new LinearEquations2();
		le.dgesv(n, nrhs, AT, n, ipiv, XT, n, info);
		
		return info[0];
	}
	
	
	// compute the SVD of a MxN matrix 'a'
	// jobu = 'N' if U is not required; otherwise, set jobu = 'A'. Same for jobv
	// U should be MxM; V should be NxN; S should be min(M,N) 
	public int Dgesvd(final int M, final int N, double [][]a, final char jobu, final char jobv, double[][] U, double[] S, double[][] V) { 
		double[][] u;
		double[][] vt;
		int ldu, ldvt;
		
		// note Fortran matrix is the transpose of C matrix
		// So, U = v, V = u;
		
		if ( jobv == 'N') {
			ldu = 1;
			u = null;
		} else {
			ldu = N;
			u = new double[N][N];
		}
		
		if ( jobu == 'N') {
			ldvt = 1;
			vt = null;
		} else {
			ldvt = M;
			vt = U;
		}
		
		int MN = (M < N) ? M : N;    
		double []s = S;
		
		// int lwork = 6*MN + M + N; // larger than necessary	
		int nmax = 132;
		int lwork = (nmax * ((5 * nmax) + 5)) + 1;
		double []work = new double[lwork];
		
		int lda = N;        
		
		int[] info = new int[1];
		info[0] = 0;
		
		SVD svd = new SVD();
		svd.dgesvd(jobv, jobu, N, M, a, lda, s, u, ldu, vt, ldvt, work, lwork, info);
		work = null;
		
		if ( info[0] == 0 ) { 
			// LAPACK outpouts the transpose of U
			if ( jobv != 'N') {
				for ( int i = 0; i < N; i++ ) 
					for ( int j = 0; j < N; j++ ) 
					          V[i][j] = u[j][i];	
					u = null;
			}
			return 0;
		}
		return 1;
	}
	
	public void VisDMatrixSVD(final CVisDMatrix A, CVisDVector s, CVisDMatrix U, CVisDMatrix V) {
		int compute_left = 1;
		int compute_right = 1;
		this.VisDMatrixSVD(A, s, U, V, compute_left, compute_right);
	}
	
	// public void VisDMatrixSVD(CVisDMatrix A, CVisDVector sV, CVisDMatrix UT, CVisDMatrix V, int compute_left, int compute_right) {
	public void VisDMatrixSVD(final CVisDMatrix A, CVisDVector s, CVisDMatrix U, CVisDMatrix V, int compute_left, int compute_right) {
		
		int nr = A.NRows(); 
		int nc = A.NCols();
		char jobu = compute_left != 0 ? 'A' : 'N';
		char jobv = compute_right != 0? 'A' : 'N';
		char jobvt = 'A';
		CVisDMatrix tmp = A;
		CVisDMatrix tmpU = new CVisDMatrix(compute_left != 0 ? nr : 1, compute_left != 0 ? nr : 1);

		SVD svd = new SVD();
		
		int info = Dgesvd(nr, nc, A.m_data, jobu, jobv, tmpU.m_data, s.m_data, V.m_data);
		if ( info != 0 ) {
			MipavUtil.displayError("Singular value decomposition failed in VisDMatrixSVD");
		}
		// Dgesvd(nr, nc, &tmp[0][0], jobu, jobv, &tmpU[0][0], &s[0], &V[0][0]);
		
		  /* Bill's porting.
		  // char jobu = 'S'; //  the first min(m,n) columns of U (the left singular
          // vectors) are returned in the array U;
		 // char jobvt = 'A'; // all N rows of V**T are returned in the array VT;
			int m = A.NRows(); // The number of rows of the input matrix X.  m >= 0.
			int n = A.NCols(); // The number of columns of the input matrix X.  n >= 0.
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
			svd.dgesvd(jobu, jobvt, m, n, A.m_data, ldx, s, U, ldu, VT, ldvt, work, lwork, info);
			if (info[0] < 0) {
			  System.err.println("In svd.dgesvd argument " + (-info[0]) + " had an illegal value");
			  return;
			}
			if (info[0] > 0) {
				System.err.println("in svd.dgesvd dbdsqr did not converge.");
			    System.err.println(info[0] + " superdiagonals of an intermediate form B did not converge to zero.");
			     return;
			}
			// sV = new CDVector(s.length, s);
			sV.SetSize(s.length, s);
		
		    Matrix vTMat = new Matrix(VT);
			Matrix vMat = vTMat.transpose();
			double vTemp[][] = vMat.getArray();
			V.SetSize(n, n, vTemp);
			// m_mEigenVec = new CDMatrix(n, n, vTemp);
	      
	       
	     // fill the remaining s if necessary
	        int i, j;
			for (i = nr; i < nc; i++)
				sV.set(i, 0.0);
			// copy matrix U if necessary
			if (compute_left == 1) {
				int nc2 = Math.min(nr, nc);
				for (i = 0; i < nr; i++) 
					for (j = 0; j < nc2; j++)
						UT.m_data[i][j] = U[i][j];
				// if necessary, arbitrarily fill the matrix
				for (j = nr; j < nc; j++) 
					for (i = 0; i < nr; i++)
						UT.m_data[i][j] = (nc - j == i) ? 1 : 0; 
			}
	          */
	
	    
		// fill the remaining s if necessary
		int i, j;
		for (i = nr; i < nc; i++)
			s.set(i, 0.0);
		// copy matrix U if necessary
		if (compute_left != 0 ) {
			int nc2 = Math.min(nr, nc);
			for (i = 0; i < nr; i++) 
				for (j = 0; j < nc2; j++)
					U.m_data[i][j] = tmpU.m_data[i][j];
			// if necessary, arbitrarily fill the matrix
			for (j = nr; j < nc; j++) 
				for (i = 0; i < nr; i++)
					U.m_data[i][j] = ((nc - j == i) ? 1 : 0);
		}
		  
	}
	
}