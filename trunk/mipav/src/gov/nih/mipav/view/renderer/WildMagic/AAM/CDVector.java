package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
import java.util.*;

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
 * Copyright (c) 1999-2001 by Informatics & Mathmatical Modelling, Section for
 * Image Analysis
 * 
 * IMM, Informatics & Mathmatical Modelling Technical University of Denmark
 * Richard Petersens Plads Building 321 DK-2800 Lyngby, Denmark
 * http://www.imm.dtu.dk/~diva/
 * 
 * Original author: Rune Fisker
 * 
 * Current author: Mikkel B. Stegmann - mbs@imm.dtu.dk
 * 
 * Contributions: Lars Pedersen - lap@imm.dtu.dk, Henrik Aanaes -
 * haa@imm.dtu.dk, + several other peoples at IMM.
 * 
 * Vector operations for AAM model.
 * 
 * @author Ruida Cheng
 * 
 */
public class CDVector extends CVisDVector {

	public static int RAND_MAX = 32767;

	/**
	 * constructor
	 */
	public CDVector() {
		super();
	}

	public CDVector(int length) {
		super(length);
	}

	public CDVector(int length, double[] storage) {
		super(length, storage);
	}

	public CDVector(final CDVector vec) {
		super(vec);
	}

	public CDVector(final CVisDVector vec) {
		super(vec);
	}

	/**
	 * Assignment operator.
	 * 
	 * @param vIn
	 *            Input vector.
	 * @return This.
	 */
	public CDVector assign(final CVisDVector vIn) {

		if (Length() == 0) {
			Resize(vIn.Length());
		}

		int len = vIn.m_data.length;

		for (int i = 0; i < len; i++) {
			this.m_data[i] = vIn.m_data[i];
		}
		return this;
	}

	/**
	 * Assignment operator.
	 * 
	 * @param vIn
	 *            Input vector.
	 * @return This.
	 */
	public CDVector assign(final CDVector vIn) {

		if (Length() == 0) {
			Resize(vIn.Length());
		}

		int len = vIn.m_data.length;
		for (int i = 0; i < len; i++) {
			// System.err.println("i = " + i);
			this.m_data[i] = vIn.m_data[i];
		}

		return this;
	}

	/**
	 * Assignment operator. Sets all element to the input value.
	 * 
	 * @param vIn
	 *            Input double.
	 * @return This.
	 */
	public CDVector assign(double value) {

		for (int i = 0; i < m_length; i++) {
			this.m_data[i] = value;
		}
		return this;

	}

	/**
	 * Assignment operator.
	 * 
	 * @param value
	 * @return this
	 */
	public CDVector assign(double[] value) {

		int len = this.m_data.length;
		for (int i = 0; i < len; i++) {
			this.m_data[i] = value[i];
		}
		return this;
	}

	/**
	 * Finds the maximum element in the vector.
	 * 
	 * @return The maximum value of the elements.
	 */
	public double Max() {
		double dMax = this.get(0);

		for (int i = 1; i < Length(); i++) {
			dMax = Math.max(dMax, this.get(i));
		}

		return dMax;
	}

	/**
	 * Finds the minimum element in the vector.
	 * 
	 * @return The minimum value of the elements.
	 */
	public double Min() {
		double dMin = this.get(0);

		for (int i = 1; i < Length(); i++) {
			dMin = Math.min(dMin, this.get(i));
		}

		return dMin;
	}

	/**
	 * Finds the minimum element in the vector and its position.
	 * 
	 * @return The minimum value of the elements and its position.
	 */
	public double Min(int[] iPos) {
		double dMin = this.get(0);
		iPos[0] = 0;

		for (int i = 1; i < Length(); i++) {
			if (dMin > this.get(i)) {
				dMin = this.get(i);
				iPos[0] = i;
			}
		}

		return dMin;
	}

	/**
	 * Calculates the mean value of the vector.
	 * 
	 * @return The mean value.
	 */
	public double Mean() {
		return Sum() / Length();
	}

	/**
	 * Calculates the skewness the vector.
	 * 
	 * @return The skewness.
	 */
	public double Skewness() {
		double dSkewness = 0;
		double dMean = Mean();
		double dTmp;

		for (int i = 0; i < Length(); i++) {
			dTmp = this.get(i) - dMean;
			dSkewness += dTmp * dTmp * dTmp;
		}

		double dStd = Std();
		return dSkewness / (Length() * dStd * dStd * dStd);
	}

	/**
	 * Calculates the standard deviation the vector.
	 * 
	 * @return The skewness.
	 */
	public double Std() {
		return Math.sqrt(Var());
	}

	/**
	 * Calculates the sum of the vector.
	 * 
	 * @return The sum.
	 */
	public double Sum() {
		double dSum = 0;

		for (int i = 0; i < Length(); i++) {
			dSum += this.get(i);
		}

		return dSum;
	}

	public double Var() {
		return this.Var(null);
	}

	/**
	 * Calculates the variance the vector.
	 * 
	 * @param pMean
	 *            Optional pointer, where the mean is returned.
	 * @return The variance.
	 */
	public double Var(double[] pMean) {
		double dVar = 0;
		double dMean = Mean();

		for (int i = 0; i < Length(); i++) {
			dVar += (this.get(i) - dMean) * (this.get(i) - dMean);
		}

		if (pMean != null) {

			pMean[0] = dMean;
		}

		return dVar / Length();
	}

	/**
	 * static compare functions for the Sort() function
	 */
	public static int __dbl_cmp_asc(final double[] arg1, final double[] arg2) {

		double v1 = arg1[0];
		double v2 = arg2[0];

		if (v1 < v2)
			return -1;
		if (v1 > v2)
			return 1;

		return 0;
	}

	public static int __dbl_cmp_des(final double[] arg1, final double[] arg2) {

		double v1 = arg1[0];
		double v2 = arg2[0];

		if (v1 > v2)
			return -1;
		if (v1 < v2)
			return 1;

		return 0;
	}

	public void Sort() {
		Sort(true);
	}

	/**
	 * Sorts the vector in either ascending (default) or descending order.
	 * Implemented using the standard array sort().
	 * 
	 * @param ascending
	 *            If true (default) the vector is sorted in ascending order. If
	 *            false its sorted in descending order.
	 */
	public void Sort(boolean ascending) {
		Arrays.sort(this.m_data);
	}

	/**
	 * Squares each element.
	 */
	public void Sqr() {

		for (int i = 0; i < Length(); i++) {
			this.m_data[i] *= this.m_data[i];
		}
	}

	/**
	 * Takes the square root of each element.
	 */
	public void Sqrt() {

		for (int i = 0; i < Length(); i++) {
			this.m_data[i] = Math.sqrt(this.m_data[i]);
		}
	}

	/**
	 * Takes the power 'dP' of each element.
	 * 
	 * @param dP
	 *            The exponent.
	 */
	public void Pow(double dP) {
		for (int i = 0; i < Length(); i++) {
			this.m_data[i] = Math.pow(this.m_data[i], dP);
		}
	}

	/**
	 * Multiplies two vectors element-wise. Corresponding MatLab operator ".*".
	 * 
	 * @param vector
	 *            The input vector to multiply this vector with.
	 */
	public void ElementMultiply(final CDVector vector) {
		assert (Length() == vector.Length());

		for (int i = 0; i < Length(); i++) {
			this.m_data[i] *= vector.m_data[i];
		}
	}

	/**
	 * Divide two vectors element-wise. Corresponding MatLab operator "./".
	 * 
	 * @param vector
	 *            The input vector to divide this vector with.
	 */
	public void ElementDivide(final CDVector vector) {
		assert (Length() == vector.Length());

		for (int i = 0; i < Length(); i++) {
			this.m_data[i] /= vector.m_data[i];
		}
	}

	/**
	 * Returns a string representing the vector.
	 * 
	 * @param fNewline
	 *            If true, the string is terminated with a new line.
	 * @return The output string.
	 */
	public String ToString(final boolean fNewline) {
		String strOut = new String("");

		String strTmp;

		for (int i = 0; i < Length(); i++) {
			System.err.print(this.m_data[i] + "\t");
		}

		if (fNewline)
			System.err.println();

		return strOut;
	}

	/**
	 * Calculates the one-norm (L1) of the vector. Also known as the city block
	 * metric.
	 * 
	 * L1(v) = |x_1| + |x_2| .. + |x_n|
	 * 
	 * @return The L1 norm.
	 */
	public double Norm1() {
		double[] pData = null;
		double norm = 0;
		int len = Length();

		pData = this.m_data;

		for (int c = 0; c < len; c++) {
			norm += Math.abs(pData[c]);
		}
		return norm;
	}

	/**
	 * Calculates the two-norm (L2) of the vector. Also known as the Euclidean
	 * length.
	 * 
	 * L2(v) = sqrt( x_1^2 + x_2^2 .. + x_n^2 )
	 * 
	 * @return The L2 norm.
	 */
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

	/**
	 * Calculates the infinity-norm (Linf) of the vector. Also known as the
	 * Chebyshev Norm.
	 * 
	 * Linf(v) = max( |x_1|, |x_2| ... , |x_n| )
	 * 
	 * @return The L2 norm.
	 */
	public double NormInf() {
		double val;
		double[] pData = null;
		double max = -1e306;
		int len = Length();

		pData = this.m_data;

		for (int c = 0; c < len; c++) {

			val = Math.abs(pData[c]);
			max = val > max ? val : max;
		}
		return max;
	}

	/**
	 * Normalizes the vector to unit length, using the 2-norm.
	 * 
	 * @return Nothing.
	 */
	public void Normalize2() {
		double norm;
		int len = this.Length();

		norm = this.Norm2();

		for (int c = 0; c < len; c++) {
			this.m_data[c] /= norm;
		}

	}

	/**
	 * Writes the vector to disk in MatLab (.m) format. To read the vector into
	 * MatLab write e.g. 'my_vector.m' at the MatLab prompt.
	 * 
	 * Notice that this should be used for storage a (really) large vectors, due
	 * to the computational and i/o overhead induced by the simple MatLab text
	 * format.
	 * 
	 * Also, remember that MatLab can't read (.m) files with lines longer than
	 * 4096 bytes.
	 * 
	 * If no communication with MatLab is needed, but merely to/from disk
	 * functionality within a DIVA program, it is suggested to use the fast
	 * binary i/o methods ToFile() and FromFile().
	 * 
	 * @param sFilename
	 *            Output file name. Should have the extension '.m'.
	 * @param sName
	 *            Name of destination matlab variable.
	 * @param sComment
	 *            Optional comment inside the file.
	 * @param fAppend
	 *            If true, the vector is appended to the file 'sFilename'.
	 */
	public void ToMatlab(final String sFilename, final String sName,
			final String sComment, boolean fAppend) {
		assert (Length() > 0);

		CDMatrix mOut = new CDMatrix(Length(), 1);

		int len = Length();
		for (int i = 0; i < len; i++) {
			mOut.m_data[i][0] = this.m_data[i];
		}
		mOut.ToMatlab(sFilename, sName, sComment, fAppend);
	}

	/**
	 * Writes the vector to disk in binary format. Use 'readbin.m' to load such
	 * a file into MatLab (placed in the diva/matlab dir).
	 * 
	 * @param sFilename
	 *            Output file name.
	 */
	public void ToFile(final String sFilename) {
		assert (Length() > 0);

		CDMatrix mOut = new CDMatrix(1, Length());

		for (int i = 0; i < m_data.length; i++) {
			mOut.m_data[0][i] = m_data[i];
		}

		mOut.ToFile(sFilename);
	}

	/**
	 * Reads a vector from disk in binary format.
	 * 
	 * @param sFilename
	 *            Input file name.
	 */
	public void FromFile(final String sFilename) {
		CDMatrix mOut = new CDMatrix();
		mOut.FromFile(sFilename);

		Resize(mOut.NCols());
		for (int i = 0; i < m_data.length; i++) {
			m_data[i] = mOut.m_data[0][i];
		}
	}

	/**
	 * Writes the vector to disk in binary format. Use 'readbin.m' to load such
	 * a file into MatLab (placed in the diva/matlab dir).
	 * 
	 * @param fh
	 *            Open file handle (opened using fopen())
	 */

	public void ToFile(DataOutputStream fh) {
		assert (Length() > 0);

		CDMatrix mOut = new CDMatrix(1, Length());

		for (int i = 0; i < m_data.length; i++) {
			mOut.m_data[0][i] = m_data[i];
		}

		mOut.ToFile(fh);
	}

	/**
	 * Reads a vector from disk in binary format.
	 * 
	 * @param fh
	 *            Open file handle (opened using fopen())
	 */

	public void FromFile(DataInputStream fh) {
		CDMatrix mOut = new CDMatrix();
		mOut.FromFile(fh);

		Resize(mOut.NCols());
		for (int i = 0; i < m_data.length; i++) {
			m_data[i] = mOut.m_data[0][i];
		}

	}

	/**
	 * Equal - compares a vector and a double. Compares of a vector and a double
	 * and returns the result as a binary vector i.e.:
	 * 
	 * C(i) = 1 if A(i) == B C(i) = 0 else
	 * 
	 * Corresponding MatLab function EQ()
	 * 
	 * @param B
	 *            Input double.
	 * @param C
	 *            Output result vector.
	 */
	public void Eq(final double B, CDVector C) {
		// CHECK: A and C must have same length
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] == B) ? 1 : 0;
	}

	/**
	 * Equal - compares a vector and a vector. Compares of a vector and a vector
	 * and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) == B C(i) = 0 else
	 * 
	 * Corresponding MatLab function EQ()
	 * 
	 * @param B
	 *            Input vector.
	 * @param C
	 *            Output result vector.
	 */
	public void Eq(final CDVector B, CDVector C) {
		// CHECK: A, B and C must have same length
		assert (Length() == B.Length());
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] == B.m_data[r]) ? 1 : 0;
	}

	/**
	 * Not Equal - compares a double and a vector. Compares of a double and a
	 * vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) != B C(i) = 0 else
	 * 
	 * Corresponding MatLab function NE()
	 * 
	 * @param B
	 *            Input double.
	 * @param C
	 *            Output result vector.
	 */
	public void Ne(final double B, CDVector C) {
		// CHECK: A and C must have same length
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] != B) ? 1 : 0;
	}

	/**
	 * Not Equal - compares a vector and a vector. Compares of a vector and a
	 * vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) != B C(i) = 0 else
	 * 
	 * Corresponding MatLab function NE()
	 * 
	 * @param B
	 *            Input vector.
	 * @param C
	 *            Output result vector.
	 */
	public void Ne(final CDVector B, CDVector C) {
		// CHECK: A, B and C must have same length
		assert (Length() == B.Length());
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] != B.m_data[r]) ? 1 : 0;
	}

	/**
	 * Less Than - compares a double and a vector. Compares of a double and a
	 * vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) < B C(i) = 0 else
	 * 
	 * Corresponding MatLab function LT()
	 * 
	 * @param B
	 *            Input double.
	 * @param C
	 *            Output result vector.
	 */
	public void Lt(final double B, CDVector C) {
		// CHECK: A and C must have same length
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] < B) ? 1 : 0;
	}

	/**
	 * Less Than - compares a vector and a vector. Compares of a vector and a
	 * vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) < B C(i) = 0 else
	 * 
	 * Corresponding MatLab function LT()
	 * 
	 * @param B
	 *            Input vector.
	 * @param C
	 *            Output result vector.
	 */
	public void Lt(final CDVector B, CDVector C) {
		// CHECK: A, B and C must have same length
		assert (Length() == B.Length());
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] < B.get(r)) ? 1 : 0;
	}

	/**
	 * Less Than or Equal - compares a double and a vector. Compares of a double
	 * and a vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) <= B C(i) = 0 else
	 * 
	 * Corresponding MatLab function LE()
	 * 
	 * @param B
	 *            Input double.
	 * @param C
	 *            Output result vector.
	 */
	public void Le(final double B, CDVector C) {
		// CHECK: A and C must have same length
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] <= B) ? 1 : 0;
	}

	/**
	 * Less Than or Equal - compares a vector and a vector. Compares of a vector
	 * and a vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) <= B C(i) = 0 else
	 * 
	 * Corresponding MatLab function LE()
	 * 
	 * @param B
	 *            Input vector.
	 * @param C
	 *            Output result vector.
	 */
	public void Le(final CDVector B, CDVector C) {
		// CHECK: A, B and C must have same length
		assert (Length() == B.Length());
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] <= B.get(r) ? 1 : 0);
	}

	/**
	 * Greater Than - compares a double and a vector. Compares of a double and a
	 * vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) > B C(i) = 0 else
	 * 
	 * Corresponding MatLab function GT()
	 * 
	 * @param B
	 *            Input vector.
	 * @param C
	 *            Output result vector.
	 */
	public void Gt(final double B, CDVector C) {
		// CHECK: A and C must have same length
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] > B) ? 1 : 0;
	}

	/**
	 * Greater Than - compares a vector and a vector. Compares of a vector and a
	 * vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) > B C(i) = 0 else
	 * 
	 * Corresponding MatLab function GT()
	 * 
	 * @param B
	 *            Input vector.
	 * @param C
	 *            Output result vector.
	 */
	public void Gt(final CDVector B, CDVector C) {
		// CHECK: A, B and C must have same length
		assert (Length() == B.Length());
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] > B.m_data[r]) ? 1 : 0;
	}

	/**
	 * Greater Than or Equal - compares a double and a vector. Compares of a
	 * double and a vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) >= B C(i) = 0 else
	 * 
	 * Corresponding MatLab function GE()
	 * 
	 * @param B
	 *            Input double.
	 * @param C
	 *            Output result vector.
	 */
	public void Ge(final double B, CDVector C) {
		// CHECK: A and C must have same length
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] >= B) ? 1 : 0;
	}

	/**
	 * Greater Than or Equal - compares a vector and a vector. Compares of a
	 * vector and a vector and returns the result as a binary matrix i.e.:
	 * 
	 * C(i) = 1 if A(i) >= B C(i) = 0 else
	 * 
	 * Corresponding MatLab function GE()
	 * 
	 * @param B
	 *            Input vector.
	 * @param C
	 *            Output result vector.
	 */
	public void Ge(final CDVector B, CDVector C) {
		// CHECK: A, B and C must have same length
		assert (Length() == B.Length());
		assert (Length() == C.Length());

		int n = Length();

		for (int r = 0; r < n; r++)
			C.m_data[r] = (this.m_data[r] >= B.m_data[r]) ? 1 : 0;
	}

	/**
	 * Takes the natural logarithm of each element.
	 */
	public void Log() {
		for (int j = 0; j < Length(); j++)
			this.m_data[j] = Math.log(this.m_data[j]);
	}

	/**
	 * Takes the absolute valueof each element.
	 */
	public void Abs() {

		for (int j = 0; j < Length(); j++) {

			this.m_data[j] = Math.abs(this.m_data[j]);
		}
	}

	/**
	 * Reverses the vector.
	 */
	public void Reverse() {

		double t;
		int len = Length();

		for (int i = 0; i < len / 2; i++) {

			// swap elements
			t = this.m_data[i];
			this.m_data[i] = this.m_data[len - i - 1];
			this.m_data[len - i - 1] = t;
		}
	}

	/**
	 * Uniformly distributed random numbers. Inserts uniformly distributed
	 * random numbers in the range [0;1].
	 */
	public void Rand() {

		int len = Length();

		for (int i = 0; i < len; i++) {

			this.m_data[i] = Math.random() / (double) RAND_MAX;
		}
	}

	/**
	 * Uniformly distributed integer random numbers. Inserts uniformly
	 * distributed integer random numbers in the range [st;end].
	 */
	public void Rand(final int st, final int end) {

		int len = Length();
		double width = end - st;

		for (int i = 0; i < len; i++) {

			this.m_data[i] = (int) (.5 + width
					* (Math.random() / (double) RAND_MAX) + st);
		}
	}

	/**
	 * Forms the cross product of two vectors and store the result in this. If
	 * the vector does have the correct size, it will be resized.
	 * 
	 * @param v1
	 *            Input vector.
	 * @param v2
	 *            Input vector.
	 */
	public void CrossProduct(final CDVector v1, final CDVector v2) {

		if (this.Length() != v1.Length()) {

			this.Resize(v1.Length());
		}
		VisCrossProduct(v1, v2, this);
	}

	/**
	 * Takes ceil() of each element.
	 */
	public void Ceil() {

		int len = Length();
		for (int i = 0; i < len; i++) {

			this.m_data[i] = Math.ceil(this.m_data[i]);
		}
	}

	/**
	 * Takes floor() of each element.
	 */
	public void Floor() {

		int len = Length();
		for (int i = 0; i < len; i++) {

			this.m_data[i] = Math.floor(this.m_data[i]);
		}
	}

	/**
	 * Takes round() of each element.
	 */
	public void Round() {

		int len = Length();
		for (int i = 0; i < len; i++) {

			this.m_data[i] = (int) (.5 + this.m_data[i]);
		}
	}

	/**
	 * Generates a vector of linearly equally spaced points between x1 and x2
	 * (inclusive).
	 * 
	 * @param x1
	 *            Starting point.
	 * @param x2
	 *            Ending point.
	 * @param n
	 *            Number of points.
	 */
	public void Linspace(final double x1, final double x2, final int n) {

		assert (n > 0);
		assert (x1 < x2);

		Resize(n);
		double mul = (x2 - x1) / (n - 1.);
		for (int i = 0; i < n; i++) {

			this.m_data[i] = x1 + i * mul;
		}
	}

	/**
	 * Returns a sub range of a vector.
	 * 
	 * @param st
	 *            Starting posistion.
	 * @param end
	 *            End postion.
	 * @return A sub range in a vector.
	 */
	public CDVector Range(final int st, final int end) {

		assert (st < end);
		assert (end < Length());

		int len = end - st + 1;
		CDVector range = new CDVector(len);
		for (int i = 0; i < len; i++) {

			range.m_data[i] = this.m_data[i + st];
		}

		return range;
	}

	/**
	 * Converts this vector to a matrix, either by row (default) or column.
	 * 
	 * @param nRows
	 *            Number of rows in the matrix.
	 * @param nCols
	 *            Number of cols in the matrix.
	 * @param m
	 *            Output matrix.
	 * @param rowWise
	 *            If true (default) vector data is extracted into rows. If false
	 *            vector data is extracted into cols.
	 */
	public void ToMatrix(final int nRows, final int nCols, CDMatrix m,
			boolean rowWise) {

		assert (nRows > 0 && nCols > 0);
		assert (nRows * nCols == this.Length());

		int i = 0;
		m.Resize(nRows, nCols);
		if (rowWise) {

			for (int r = 0; r < nRows; r++) {
				for (int c = 0; c < nCols; c++) {

					m.m_data[r][c] = this.m_data[i++];
				}
			}
		} else {

			for (int c = 0; c < nCols; c++) {
				for (int r = 0; r < nRows; r++) {

					m.m_data[r][c] = this.m_data[i++];
				}
			}
		}
	}

	/**
	 * Concatenates a vector to the end of this vector.
	 * 
	 * @param v
	 *            Input vector.
	 * @return Concatenated vector.
	 */
	public CDVector VecCat(final CDVector v) {

		int tlen = this.Length();
		CDVector out = new CDVector(tlen + v.Length());

		for (int i = 0; i < out.Length(); i++) {

			out.m_data[i] = i < tlen ? this.m_data[i] : v.m_data[i - tlen];
		}

		return out;
	}

	/**
	 * Clamps the vector to [min,max].
	 * 
	 * @param min
	 *            Minumim value.
	 * @param max
	 *            Maximum value.
	 */
	public void Clamp(final double min, final double max) {

		int len = this.Length();
		double val;

		for (int i = 0; i < len; i++) {

			val = this.m_data[i];
			// *val = *val<min ? min : (*val>max ? max : *val);
			this.m_data[i] = val < min ? min : (val > max ? max : val);
			// this.m_data[i] = this.m_data[i]<min ? min : (this.m_data[i]>max ?
			// max : this.m_data[i]);
		}
	}

	public void AlignTo(final CDVector v) {
		AlignTo(v, null, null);

	}

	/**
	 * Linear alignment of this vector to another vector using the L2 norm.
	 * 
	 * @param v
	 *            Vector that this vector is being least squares fitted to.
	 * @param a
	 *            Optional pointer to store the resulting transformation in.
	 * @param b
	 *            Optional pointer to store the resulting transformation in.
	 */
	public void AlignTo(final CDVector v, double[] pA, double[] pB) {

		assert (this.Length() == v.Length());

		int n = this.Length();
		assert (n == v.Length());

		// calc sums
		double x, y, a, b, Sx = .0, Sy = .0, Sxx = .0, Sxy = .0;
		for (int i = 0; i < n; i++) {

			x = this.m_data[i];
			y = v.m_data[i];
			Sx += x;
			Sy += y;
			Sxx += x * x;
			Sxy += x * y;
		}

		// in an L2 sense, the optimal scaling and offset of this vector
		double d = n * Sxx - Sx * Sx;
		a = (n * Sxy - Sx * Sy) / d;
		b = (Sy * Sxx - Sx * Sxy) / d;

		// apply tranformation
		for (int i = 0; i < n; i++) {

			this.m_data[i] = a * this.m_data[i] + b;
		}

		if (pA != null) {
			pA[0] = a;
		}
		if (pB != null) {
			pB[0] = b;
		}
	}

	/**
	 * Calculates the autocorrelation of the vector with a given lag (default
	 * lag is 1).
	 * 
	 * @param lag
	 *            The lag.
	 * @return The autocorrelation.
	 */
	public double AutoCorrelation(final int lag) {

		int n = this.Length();
		double[] mean = new double[1];
		mean[0] = .0;
		double d = n * this.Var(mean);
		double s = .0;

		for (int i = 0; i < n - lag; i++) {

			s += (this.m_data[i] - mean[0]) * (this.m_data[i + lag] - mean[0]);
		}

		return s / d;
	}

	/**
	 * Shuffles (randomizes) the vector.
	 */
	public void Shuffle() {

		double tmp;
		int n = Length();

		int shuffle_amount = 2;

		for (int s = 0; s < shuffle_amount; s++) {

			for (int i = 0; i < n; i++) {

				int index = (int) (.5 + (n - 1) * Math.random()
						/ (double) RAND_MAX);

				// swap 'i' and 'index'
				tmp = this.m_data[i];
				this.m_data[i] = this.m_data[index];
				this.m_data[index] = tmp;
			}
		}
	}

	/* ************************************************************************** */
	/*
	 * This Quickselect routine is based on the algorithm described in
	 * "Numerical recipes in C", Second Edition, Cambridge University Press,
	 * 1992, Section 8.5, ISBN 0-521-43108-5 This code by Nicolas Devillard -
	 * 1998. Public domain.
	 */

	// #define ELEM_SWAP(a,b) { register double t=(a);(a)=(b);(b)=t; }

	double quick_select(double arr[], int n) {
		int low, high;
		int median;
		int middle, ll, hh;
		double t;
		low = 0;
		high = n - 1;
		median = (low + high) / 2;
		for (;;) {
			if (high <= low) /* One element only */
				return arr[median];

			if (high == low + 1) { /* Two elements only */
				if (arr[low] > arr[high]) {
					// ELEM_SWAP(arr[low], arr[high]) ;
					t = arr[low];
					arr[low] = arr[high];
					arr[high] = t;
				}
				return arr[median];
			}

			/* Find median of low, middle and high items; swap into position low */
			middle = (low + high) / 2;
			if (arr[middle] > arr[high]) {
				// ELEM_SWAP(arr[middle], arr[high]) ;
				t = arr[middle];
				arr[middle] = arr[high];
				arr[high] = t;
			}
			if (arr[low] > arr[high]) {
				// ELEM_SWAP(arr[low], arr[high]) ;
				t = arr[low];
				arr[low] = arr[high];
				arr[high] = t;
			}
			if (arr[middle] > arr[low]) {
				// ELEM_SWAP(arr[middle], arr[low]) ;
				t = arr[middle];
				arr[middle] = arr[low];
				arr[low] = t;
			}

			/* Swap low item (now in position middle) into position (low+1) */
			// ELEM_SWAP(arr[middle], arr[low+1]) ;
			t = arr[middle];
			arr[middle] = arr[low + 1];
			arr[low + 1] = t;

			/* Nibble from each end towards middle, swapping items when stuck */
			ll = low + 1;
			hh = high;
			for (;;) {
				do
					ll++;
				while (arr[low] > arr[ll]);
				do
					hh--;
				while (arr[hh] > arr[low]);

				if (hh < ll)
					break;

				// ELEM_SWAP(arr[ll], arr[hh]) ;
				t = arr[ll];
				arr[ll] = arr[hh];
				arr[hh] = t;
			}

			/* Swap middle item (in position low) back into correct position */
			// ELEM_SWAP(arr[low], arr[hh]) ;
			t = arr[low];
			arr[low] = arr[hh];
			arr[hh] = t;

			/* Re-set active partition */
			if (hh <= median)
				low = ll;
			if (hh >= median)
				high = hh - 1;
		}
	}

	/**
	 * Calculates the median.
	 * 
	 * @return The median.
	 */
	public double Median() {

		CDVector copy = new CDVector(this);
		double median;
		boolean doSorting = false;

		if (doSorting) {

			copy.Sort();
			final int len = copy.Length();
			final boolean isOdd = len % 2 != 0;
			median = isOdd ? copy.m_data[len / 2]
					: .5 * (copy.m_data[len / 2 - 1] + copy.m_data[len / 2]);
		} else {

			// *much* faster alternative kindly provided by Nicolas Devillard
			// see code snip above
			median = quick_select(copy.m_data, copy.Length());
		}

		return median;
	}

	/**
	 * Removes the extreme 'percentage' part of the vector. E.g. v.Trim( .10 )
	 * removes the 5% of the lower tail and 5% of the upper tail.
	 * 
	 * By convention Trim( 1. ) returns the median.
	 * 
	 * @param percentage
	 *            The amount of data to strip from the vector.
	 * @return A trimmed version of the vector.
	 */
	public CDVector Trim(final double percentage) {

		assert (percentage >= .0 && percentage <= 1.);

		if (percentage >= 1.) {

			// by convention
			CDVector v = new CDVector(1);
			v.m_data[0] = this.Median();
			return v;

		} else {

			// remove tails
			final int len = this.Length();
			final int head_tail_len = (int) (.5 + len * percentage / 2.);

			CDVector trimmedVec = new CDVector(this);
			trimmedVec.Sort();
			return trimmedVec.Range(head_tail_len, len - head_tail_len - 1);
		}
	}

	/**
	 * Calculates the trimmed mean. Removes the extreme 'percentage' part of the
	 * vector and calculates the mean of the remaining data.
	 * 
	 * @param percentage
	 *            The amount of data to strip from the vector.
	 * @return The trimmed mean.
	 */
	public double TrimmedMean(final double percentage) {

		CDVector trimmedVec = this.Trim(percentage);
		return trimmedVec.Mean();
	}

	/**
	 * Calculates the trimmed variance. Removes the extreme 'percentage' part of
	 * the vector and calculates the variance of the remaining data.
	 * 
	 * @param percentage
	 *            The amount of data to strip from the vector.
	 * @return The trimmed mean.
	 */
	public double TrimmedVar(final double percentage) {

		CDVector trimmedVec = this.Trim(percentage);
		return trimmedVec.Var();
	}

	/**
	 * Calculates the trimmed standard deviation. Removes the extreme
	 * 'percentage' part of the vector and calculates the standard deviation of
	 * the remaining data.
	 * 
	 * @param percentage
	 *            The amount of data to strip from the vector.
	 * @return The trimmed mean.
	 */
	public double TrimmedStd(final double percentage) {

		CDVector trimmedVec = this.Trim(percentage);
		return trimmedVec.Std();
	}

}