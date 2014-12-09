package gov.nih.mipav.view.renderer.WildMagic.AAM;
import java.util.Arrays;

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
 * NAME VisDMatrix -- double precision matrix/vector operations 
 * DESCRIPTION 
 * 
 * CLASS:
 *      CVisDVector
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
 * Java modification from the original cpp file. 
 * Use partial routine for matrix manipulation for automatic prostate segmentation. 
 * 
 * @author Ruida Cheng
 * 
 */
public class CVisDVector {

	// pointer to data
	public double[] m_data;

	// number of elements
	protected int m_length;

	protected boolean m_fExternalStorage;

	public CVisDVector() {
		m_length = 0;
		m_data = null;
		m_fExternalStorage = false;
	}

	public CVisDVector(int length) {
		SetSize(length, null);
	}

	public CVisDVector(int length, double[] storage) {
		SetSize(length, storage);
	}

	public CVisDVector(final CVisDVector refvector) {
		SetSize(refvector.Length());
		this.assign(refvector);
	}

	public void dispose() {
		FreeBuffer();
	}

	// operator!=
	public boolean not_equals(final CVisDVector refvector) {
		return !(this.equals(refvector));
	}

	// CVisDVector::operator+=
	public CVisDVector add_into(final CVisDVector refvector) {
		return EqSum(this, refvector);
	}

	// CVisDVector::operator-=
	public CVisDVector sub_into(final CVisDVector refvector) {
		return EqDiff(this, refvector);
	}

	// CVisDVector::operator+
	public CVisDVector add(final CVisDVector refvector) {
		assert (Length() == refvector.Length());
		CVisDVector vectorRet = new CVisDVector(this);

		return (vectorRet.add_into(refvector));
	}

	// CVisDVector::operator-
	public CVisDVector sub(final CVisDVector refvector) {
		assert (Length() == refvector.Length());
		CVisDVector vectorRet = new CVisDVector(this);

		return (vectorRet.sub_into(refvector));
	}

	public CVisDVector VisCrossProduct(final CVisDVector v1,
			final CVisDVector v2) {
		CVisDVector vOut = new CVisDVector();
		VisCrossProduct(v1, v2, vOut);
		return vOut;
	}

	public CVisDVector EqSum(final CVisDVector refvectorA,
			final CVisDVector refvectorB) {
		int n = Length();
		assert (n == refvectorA.Length());
		assert (n == refvectorB.Length());

		for (int i = 0; i < n; i++)
			this.m_data[i] = refvectorA.m_data[i] + refvectorB.m_data[i];

		return this;
	}

	public CVisDVector EqDiff(final CVisDVector refvectorA,
			final CVisDVector refvectorB) {
		int n = Length();
		assert (n == refvectorA.Length());
		assert (n == refvectorB.Length());

		for (int i = 0; i < n; i++)
			this.m_data[i] = refvectorA.m_data[i] - refvectorB.m_data[i];

		return this;
	}

	public CVisDVector EqProd(final CVisDMatrix refmatrixA,
			final CVisDVector refvectorB) {
		int nRows = refmatrixA.NRows();
		int nCols = refmatrixA.NCols();
		assert (nRows == Length());
		assert (refmatrixA.NCols() == refvectorB.Length());

		for (int i = 0; i < nRows; i++) {
			double dblSum = 0.0;
			for (int j = 0; j < nCols; j++)
				dblSum += refmatrixA.m_data[i][j] * refvectorB.m_data[j];
			this.m_data[i] = dblSum;
		}

		return this;
	}

	// CVisDVector::operator*
	public CVisDVector mult(double dbl) {
		CVisDVector vectorRet = new CVisDVector(this);

		return (vectorRet.mult_into(dbl));
	}

	// CVisDVector::operator/
	public CVisDVector div(double dbl) {
		assert (dbl != 0);

		CVisDVector vectorRet = new CVisDVector(this);

		return (vectorRet.div_into(dbl));
	}

	public double get(int index) {
		return m_data[index];
	}

	public void set(int index, double value) {
		m_data[index] = value;
	}

	public void FreeBuffer() {
		m_data = null;
	}

	public void SetSize(int length) {
		SetSize(length, null);
	}

	public void SetSize(int length, double[] storage) {
		// This method is called by the constructors and the Resize method.
		// It assumes that the current buffer has already been freed.
		assert (length >= 0);
		m_length = length;

		// Get a new buffer.
		if (m_length > 0) {
			m_fExternalStorage = (storage != null);
			if (!m_fExternalStorage) {
				m_data = new double[m_length];
			} else {
				m_data = storage;
			}
		} else {
			m_data = null;
			m_fExternalStorage = false;
		}
	}

	// Vector length
	public int Length() {
		return m_length;
	}

	public void Resize(int length) {
		this.Resize(length, null);
	}

	public void Resize(int length, double[] storage) {
		if ((m_length != length) || (storage != null) || (m_fExternalStorage)) {
			// Delete the old buffer, if needed.
			FreeBuffer();

			// Find the new buffer.
			SetSize(length, storage);
		}
	}

	public void Resize(int length, double[] storage, boolean flag) {
		if ((m_length != length) || (storage != null) || (m_fExternalStorage)) {
			// Delete the old buffer, if needed.
			FreeBuffer();

			// Find the new buffer.
			SetSize(length, storage);
		}
	}

	//
	// Assignment
	//

	// //////////////////////////////////////////////////////////////////////////
	//
	// FUNCTION: operator=
	//
	// DECLARATION:
	// CVisDVector& CVisDVector::operator=(const CVisDVector &vec);
	//
	// RETURN VALUE:
	// vector being copied
	// INPUT:
	// &vec (const CVisDVector) - vector being copied
	//
	// DISCRIPTION:
	// assignment operator
	//
	// //////////////////////////////////////////////////////////////////////////
	// CVisDVector& CVisDVector::operator=(const CVisDVector &vec)
	public CVisDVector assign(final CVisDVector vec) {
		// This "if" statement was added on Nov. 3, 1999 to allow templated
		// array classes to copy elements when resizing the array.
		if (Length() == 0)
			Resize(vec.Length());

		assert (Length() == vec.Length());

		if (m_length != 0) {
			assert (m_length > 0);
			System.arraycopy(vec.m_data, 0, m_data, 0, m_length);
			/*
			 * for ( int i = 0; i < m_length; i++ ) { m_data[i] = vec.m_data[i];
			 * }
			 */
		}

		return this;
	}

	// //////////////////////////////////////////////////////////////////////////
	//
	// FUNCTION: operator=
	//
	// DECLARATION:
	// CVisDVector& CVisDVector::operator=(double value);
	//
	// RETURN VALUE:
	// reference to l.h.s.
	// INPUT:
	// value (double) - fill value
	//
	// DISCRIPTION:
	// Fill vector with constant value
	//
	// //////////////////////////////////////////////////////////////////////////
	public CVisDVector assign(double value) {
		if ((value == 0.0) && (m_length != 0)) {
			assert (m_length > 0);

			m_data = new double[m_length];
		} else {
			for (int i = 0; i < m_length; i++)
				m_data[i] = value;
		}

		return this;
	}

	//
	// Comparison operators
	//

	public boolean equals(final CVisDVector refvector) {
		if (Length() == refvector.Length()) {
			if (Length() == 0)
				return true;

			if (Arrays.equals(m_data, refvector.m_data)) {
				return true;
			}
		}

		return false;
	}

	public boolean less_than(final CVisDVector refvector) {
		if (Length() == refvector.Length()) {
			if (Length() == 0)
				return false;

			/*
			 * return (memcmp(m_data, refvector.m_data, Length() *
			 * sizeof(double)) < 0);
			 */
			int len = Length();
			for (int i = 0; i < len; i++) {
				if (m_data[i] < refvector.m_data[i])
					return true;
			}
		}

		return (Length() < refvector.Length());
	}

	// operator*=
	public CVisDVector mult_into(double dbl) {
		for (int i = 0; i < Length(); i++)
			this.m_data[i] *= dbl;

		return this;
	}

	// operator/=
	public CVisDVector div_into(double dbl) {
		assert (dbl != 0);

		for (int i = 0; i < Length(); i++)
			this.m_data[i] /= dbl;

		return this;
	}

	public double Norm2() {
		double[] pData = null;
		double norm = 0;
		int len = Length();

		pData = this.m_data;

		for (int c = 0; c < len; c++) {
			norm += pData[c] * pData[c];
		}
		return Math.sqrt(norm);
	}

	// operator-
	public CVisDVector neg() {
		CVisDVector vectorRet = new CVisDVector(Length());

		for (int i = 0; i < Length(); i++)
			vectorRet.m_data[i] = -this.m_data[i];

		return vectorRet;
	}

	// operator*
	public double mult(final CVisDVector refvector) {
		double sum = 0.0;
		assert (Length() == refvector.Length());

		for (int i = 0; i < Length(); i++)
			sum += this.m_data[i] * refvector.m_data[i];

		return sum;
	}

	// cross product of two vectors
	// added by zhang on 30 sept. 1998
	public void VisCrossProduct(final CVisDVector x, final CVisDVector y,
			CVisDVector z) {
		int N = x.Length();

		if (N < 2 || N != y.Length()) {
			/*
			 * CVisError(
			 * "CrossProduct only works for two vectors of same dimension (>= 2)"
			 * , eviserrorOpFailed, "VisCrossProduct", __FILE__, __LINE__);
			 */
			System.err
					.println("CrossProduct only works for two vectors of same dimension (>= 2)"
							+ "VisCrossProduct");
		}
		int size = N * (N - 1) / 2 - 1;

		z.Resize(size + 1);

		int inc = 0;
		double cross;
		for (int i = size; i > 0; i--)
			for (int j = (i - 1); j >= 0; j--) {
				cross = x.m_data[i] * y.m_data[j] - y.m_data[i] * x.m_data[j];
				z.m_data[inc] = (((i - j) % 2) == 0) ? cross : -cross;
				inc++;
			}
	}

}