package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
import java.text.*;

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
 * Base matrix operations within AAM model.
 * 
 * @author Ruida Cheng
 * 
 */
public class CDMatrix extends CVisDMatrix {

	public static int RAND_MAX = 32767;

	/**
	 * Constructors
	 */
	public CDMatrix() {
		super();
	}

	public CDMatrix(int rows, int cols) {
		super(rows, cols);
	}

	public CDMatrix(int rows, int cols, double[][] storage) {
		super(rows, cols, storage);
	}

	public CDMatrix(final CDMatrix mat) {
		super(mat);
	}

	public CDMatrix(final CVisDMatrix mat) {
		super(mat);
	}

	/**
	 * Assignment operator (CVisDMatrix).
	 * 
	 * @param mat
	 *            Input matrix.
	 * @return This.
	 */
	public CDMatrix assign(final CVisDMatrix mat) {

		if (NRows() == 0 && NCols() == 0)
			Resize(mat.NRows(), mat.NCols());

		assert (NRows() == mat.NRows() && NCols() == mat.NCols());

		if (m_nRows != 0) {
			assert (m_nRows > 0);
			assert (m_nCols > 0);
			// System.arraycopy(mat.m_data, 0, m_data, 0, m_nRows*m_nCols);
			for (int i = 0; i < m_nRows; i++) {
				for (int j = 0; j < m_nCols; j++) {
					this.m_data[i][j] = mat.m_data[i][j];
				}
			}
		}
		return this;
	}

	/**
	 * Assignment operator (CDMatrix).
	 * 
	 * @param mat
	 *            Input matrix.
	 * @return This.
	 */
	public CDMatrix assign(final CDMatrix mat) {

		if (NRows() == 0 && NCols() == 0)
			Resize(mat.NRows(), mat.NCols());

		assert (NRows() == mat.NRows() && NCols() == mat.NCols());

		if (m_nRows != 0) {
			assert (m_nRows > 0);
			assert (m_nCols > 0);
			// System.arraycopy(mat.m_data, 0, m_data, 0, m_nRows*m_nCols);
			for (int i = 0; i < m_nRows; i++) {
				for (int j = 0; j < m_nCols; j++) {
					this.m_data[i][j] = mat.m_data[i][j];
				}
			}
		}
		return this;
	}

	/**
	 * Assignment operator (double). Set all values equal to 'value'.
	 * 
	 * @param value
	 *            Input double.
	 * @return This.
	 */
	public CDMatrix assign(double value) {

		if ((value == 0.0) && (m_nRows != 0)) {
			assert (m_nRows > 0);
			assert (m_nCols > 0);

			m_data = new double[m_nRows][m_nCols];
		} else {
			for (int i = 0; i < m_nRows; i++) {
				for (int j = 0; j < m_nCols; j++) {
					m_data[i][j] = value;
				}
			}
		}
		return this;
	}

	/**
	 * Assignment operator
	 * 
	 * @param value
	 *            double matrix
	 * @return this.
	 */
	public CDMatrix assign(double[][] value) {

		m_data = new double[m_nRows][m_nCols];
		for (int i = 0; i < m_nRows; i++) {
			for (int j = 0; j < m_nCols; j++) {
				m_data[i][j] = value[i][j];
			}
		}
		return this;
	}

	/**
	 * Calc the sum of each column into a vector. The vector will be resized if
	 * it doesn't have the right length.
	 * 
	 * @param vSum
	 *            Output vector.
	 */
	public void SumCol(CDVector vSum) {
		if (vSum.Length() != NCols()) {

			vSum.Resize(NCols());
		}

		vSum.assign(0);
		for (int r = 0; r < NRows(); r++) {
			for (int c = 0; c < NCols(); c++) {
				vSum.m_data[c] += this.m_data[r][c];

			}
		}
	}

	/**
	 * Calc the mean of each column into a vector. The vector will be resized if
	 * it doesn't have the right length.
	 * 
	 * @param vMean
	 *            Output vector.
	 */
	public void MeanCol(CDVector vMean) {
		if (vMean.Length() != NCols()) {

			vMean.Resize(NCols());
		}
		SumCol(vMean);

		vMean.div_into(NRows());
	}

	/**
	 * Calc the variance of each column into a vector. The vector will be
	 * resized if it doesn't have the right length.
	 * 
	 * @param vVar
	 *            Output vector.
	 */
	public void VarCol(CDVector vVar) {
		assert (NCols() == vVar.Length());

		CDVector vCol = new CDVector(NRows());

		for (int c = 0; c < NCols(); c++) {
			Col(c, vCol);
			vVar.m_data[c] = vCol.Var();
		}
	}

	/**
	 * Calc the standard deviation of each column into a vector. The vector will
	 * be resized if it doesn't have the right length.
	 * 
	 * @param vStd
	 *            Output vector.
	 */
	public void StdCol(CDVector vStd) {
		VarCol(vStd);
		vStd.Sqrt();
	}

	/**
	 * Calc the total sum of the matrix.
	 * 
	 * @return The sum.
	 */
	public double Sum() {
		CDVector vTmp = new CDVector(NCols());

		SumCol(vTmp);

		return vTmp.Sum();
	}

	/**
	 * Calcs the mean value of the matrix.
	 * 
	 * @return The mean value.
	 */
	public double Mean() {
		return Sum() / (NRows() * NCols());
	}

	/**
	 * Calc the variance of the matrix.
	 * 
	 * @return The variance value.
	 */
	public double Var() {
		double dMean = Mean();
		double dVar = 0;

		for (int r = 0; r < NRows(); r++) {
			for (int c = 0; c < NCols(); c++) {
				dVar += Math.pow(this.m_data[r][c] - dMean, 2);
			}
		}

		return dVar / (NRows() * NCols());
	}

	/**
	 * Calc the standard diviation of the matrix.
	 * 
	 * @return The standard diviation value.
	 */
	public double Std() {
		return Math.sqrt(Var());
	}

	/**
	 * One-way analysis of variance (ANOVA). Determination of the fluctuations
	 * observed in a sample, and their dependencies in the form of a one-way
	 * analysis of variance (ANOVA).
	 * 
	 * @param dZ
	 * @param nDFModel
	 * @param nDFError
	 */
	public void OneWayANOVA(double[] dZ, int[] nDFModel, int[] nDFError) {
		int nN = NRows() * NCols();
		int nK = NCols();

		// calc. variance within groups
		CDVector vVarCol = new CDVector(NCols());
		VarCol(vVarCol);

		double dSSError = NRows() * vVarCol.Sum();

		// calc. variance between groups
		CDVector vMeanCol = new CDVector(NCols());
		CDVector vMean = new CDVector(NCols());
		vMean.assign(Mean());
		MeanCol(vMeanCol);

		vMean.sub_into(vMeanCol);
		vMean.Sqr();

		double dSSModel = NRows() * vMean.Sum();

		// calc. degrees of freedom
		nDFError[0] = nN - nK;
		nDFModel[0] = nK - 1;

		// calc. test
		dZ[0] = (dSSModel / nDFModel[0]) / (dSSError / nDFError[0]);
	}

	/**
	 * Student's T-test.
	 * 
	 * @param iCol1
	 * @param iCol2
	 * @param dZ
	 * @param nDF
	 * @return Nothing.
	 */
	public void TTest(final int iCol1, final int iCol2, double[] dZ, int[] nDF) {
		assert (iCol1 < NCols());
		assert (iCol2 < NCols());

		// calc. column mean
		CDVector vMean = new CDVector(NCols());
		MeanCol(vMean);

		// degress of freedom
		nDF[0] = NCols() * NRows() - NCols();

		// calc. pooled variance for each gruop of obs. (MSE)
		CDVector vVarCol = new CDVector(NCols());
		VarCol(vVarCol);

		double dSSError = NRows() * vVarCol.Sum();
		double dRootMSE = Math.sqrt(dSSError / nDF[0]);

		// calc. test
		double dTop = vMean.m_data[iCol1] - vMean.m_data[iCol2];
		double dBottom = dRootMSE * Math.sqrt(2.0 / NRows());

		dZ[0] = dTop / dBottom;
	}

	/**
	 * Return the i-th row of the matrix. The vector will be resized if it
	 * doesn't have the right length.
	 * 
	 * Notice that due to the row major nature of matrices, row read/writes are
	 * *much* faster than col read/writes.
	 * 
	 * Alternatively, one could use the more costly CVisDVector
	 * CVisDMatrix::Row(int r) method.
	 * 
	 * To set the i-th row use: void CVisDMatrix::SetRow(int r, const
	 * CVisDVector &v)
	 * 
	 * @param i
	 *            The row number.
	 * @param vRow
	 *            Output vector;
	 */
	public void Row(int i, CDVector vRow) {
		int nc = NCols();

		if (nc != vRow.Length()) {

			vRow.Resize(nc);
		}

		// assumes that matrices are row major
		// System.arraycopy(this.m_data[i], 0, vRow.m_data, 0, nc);
		for (int j = 0; j < nc; j++) {
			vRow.m_data[j] = this.m_data[i][j];
		}
	}

	/**
	 * Return the i-th column of the matrix. The vector will be resized if it
	 * doesn't have the right length.
	 * 
	 * @param i
	 *            The column number.
	 * @param vCol
	 *            Output vector;
	 */
	public void Col(int i, CDVector vCol) {
		int nr = NRows();

		if (nr != vCol.Length()) {

			vCol.Resize(nr);
		}

		for (int r = 0; r < nr; r++) {
			vCol.m_data[r] = this.m_data[r][i];
		}
	}

	/**
	 * Make the matrix a diagonal matrix containing 'vec'. This function
	 * transforms the matrix into a diagonal matrix, with the values of 'vec' in
	 * the diagonal.
	 * 
	 * @param vec
	 *            Vector to place in the diagonal.
	 */
	public void Diag(final CDVector vec) {
		// The maximum amount of values it is possible to transfer.
		int stop = Math.min(Math.min(NRows(), NCols()), vec.Length());

		this.assign(0.0);
		for (int i = 0; i < stop; i++) {
			this.m_data[i][i] = vec.m_data[i];
		}
	}

	/**
	 * Returns a string representing the matrix.
	 * 
	 * @return A string.
	 */
	public String ToString() {
		String strOut = new String("");

		String strTmp = new String();

		for (int i = 0; i < NRows(); i++) {
			for (int j = 0; j < NCols(); j++) {
				System.err.print(this.m_data[i][j] + "\t");
			}
			System.err.println();
		}

		return strOut;
	}

	/**
	 * Composes the matrix of Top and Bottom (on top of each other).
	 * 
	 * @param Top
	 *            Matrix to be placed in the top of this matrix.
	 * @param Bottom
	 *            Matrix to be placed in the bottom of this matrix.
	 */
	public void CombVert(CVisDMatrix Top, CVisDMatrix Bottom) {
		assert (Top.NCols() == Bottom.NCols());
		assert (Top.NCols() == this.NCols());
		assert (Top.NRows() + Bottom.NRows() == this.NRows());

		int j, i;

		for (j = 0; j < Top.NRows(); j++) {
			for (i = 0; i < this.NCols(); i++) {
				this.m_data[j][i] = Top.m_data[j][i];
			}
		}
		for (; j < this.NRows(); j++) {
			for (i = 0; i < this.NCols(); i++) {
				this.m_data[j][i] = Bottom.m_data[j - Top.NRows()][i];
			}
		}
	}

	/**
	 * Writes the matrix to disk in MatLab (.m) format. To read the matrix into
	 * MatLab write e.g. 'my_matrix.m' at the MatLab prompt.
	 * 
	 * Notice that this should be used for storage a (really) large matrices,
	 * due to the computational and i/o overhead induced by the simple MatLab
	 * text format.
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
		try {
			String strTmp = new String();
			if (fAppend) {
				// ofstream fstreamOut(sFilename,ios::app); //open stream
				PrintWriter fstreamOut = new PrintWriter(sFilename);

				if (!sComment.equals("")) {
					fstreamOut.print("%");
					fstreamOut.print(sComment);
					fstreamOut.println();

				}

				fstreamOut.print(sName); // Write the name of the matrix.
				fstreamOut.print("=[");
				fstreamOut.println();

				DecimalFormat floatFormat = new DecimalFormat("######.######");
				// Writes the matrix contents to sToStr.
				for (int i = 0; i < NRows(); i++) {
					for (int j = 0; j < NCols(); j++) {

						strTmp = floatFormat.format(this.m_data[i][j]);
						// System.err.println(strTmp);
						fstreamOut.print(strTmp);
					}
					fstreamOut.print(";");
					fstreamOut.println();
				}
				fstreamOut.print("];");
				fstreamOut.println();
				fstreamOut.close();
			} else {
				// ofstream fstreamOut(sFilename,ios::trunc); //open stream
				PrintWriter fstreamOut = new PrintWriter(sFilename);

				if (!sComment.equals("")) {
					fstreamOut.print("%");
					fstreamOut.print(sComment);
					fstreamOut.println();

				}

				fstreamOut.print(sName); // Write the name of the matrix.
				fstreamOut.print("=[");
				fstreamOut.println();

				DecimalFormat floatFormat = new DecimalFormat("######.######");

				// Writes the matrix contents to sToStr.
				for (int i = 0; i < NRows(); i++) {
					for (int j = 0; j < NCols(); j++) {
						strTmp = floatFormat.format(this.m_data[i][j]);
						System.err.println(strTmp);
						fstreamOut.print(strTmp);
					}
					fstreamOut.print(";");
					fstreamOut.println();
				}
				fstreamOut.print("];");
				fstreamOut.println();
				fstreamOut.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	/**
	 * Takes the square root of each element.
	 */
	public void Sqrt() {
		for (int j = 0; j < this.NRows(); j++) {
			for (int i = 0; i < this.NCols(); i++) {
				this.m_data[j][i] = Math.sqrt(this.m_data[j][i]);
			}
		}

	}

	/**
	 * Takes the power of two of each element.
	 */
	public void Sqr() {
		for (int r = 0; r < NRows(); r++) {
			for (int c = 0; c < NCols(); c++) {
				this.m_data[r][c] *= this.m_data[r][c];
			}
		}
	}

	/**
	 * Element-wise matrix multiply. Multiply each element in the matrix with
	 * the corresponding element of the input matrix.
	 */
	public void ElementMultiply(final CDMatrix matrix) {
		assert (NCols() == matrix.NCols());
		assert (NRows() == matrix.NRows());

		for (int r = 0; r < NRows(); r++) {
			for (int c = 0; c < NCols(); c++) {
				this.m_data[r][c] *= matrix.m_data[r][c];
			}
		}
	}

	/**
	 * Element-wise matrix division. Divide each element in the matrix with the
	 * corresponding element of the input matrix.
	 */
	public void ElementDivide(final CDMatrix matrix) {
		assert (NCols() == matrix.NCols());
		assert (NRows() == matrix.NRows());

		for (int r = 0; r < NRows(); r++) {
			for (int c = 0; c < NCols(); c++) {
				this.m_data[r][c] /= matrix.m_data[r][c];
			}
		}
	}

	/**
	 * Writes the matrix to disk in binary format. The dimensions are saved as
	 * two doubles (!!?) (rows,cols) in the start.
	 * 
	 * @param sFilename
	 *            Input file name.
	 */
	public void ToFile(final String sFilename) {
		try {
			DataOutputStream Stream; // Stream for output.
			Stream = new DataOutputStream(new FileOutputStream(sFilename));

			ToFile(Stream);

			Stream.close(); // Close the stream.
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Writes the matrix to disk in binary format. The dimensions are saved as
	 * two doubles (!!?) (rows,cols) in the start. The matrix is written to the
	 * binary file 'fh' at the current position of the file pointer.
	 * 
	 * @param fh
	 *            Open file handle.
	 */

	public void ToFile(DataOutputStream fh) {

		try {
			DataOutputStream Stream; // Stream for output.
			int cIndex = 2; // Counter for elements in the matrix.
			int cSize = NCols() * NRows() + 2; // The number of elements to
												// save.
			double[] Elem; // Pointer to all the element to save.

			assert (fh != null);
			Stream = fh;

			Elem = new double[cSize];

			Elem[0] = NRows(); // Save the dimensions of the matrix.
			Elem[1] = NCols();

			for (int cI = 0; cI < NRows(); cI++) // Write the elements of the
													// matrix to Elem.
			{
				for (int cJ = 0; cJ < NCols(); cJ++) {
					Elem[cIndex] = this.m_data[cI][cJ];
					cIndex++;
				}
			}

			for (int i = 0; i < Elem.length; i++) {
				fh.writeDouble(Elem[i]);
			}
			Elem = null;
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	/**
	 * Reads a matrix from disk in binary format, as written from ToFile(). The
	 * matrix is resizew if it doesn't fit the disk matrix.
	 * 
	 * @param sFilename
	 *            Input file name.
	 */
	public void FromFile(final String sFilename) {
		try {
			DataInputStream Stream; // Stream for output.

			Stream = new DataInputStream(new FileInputStream(sFilename)); // Open
																			// the
																			// stream

			FromFile(Stream);

			Stream.close(); // Close the stream.
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	/**
	 * Reads a matrix from disk in binary format (from the current position of
	 * the file pointer), as written from ToFile().
	 * 
	 * The matrix is resized if it doesn't fit the disk matrix.
	 * 
	 * @param fh
	 *            Open file handle.
	 */

	public void FromFile(DataInputStream fh) {

		DataInputStream Stream; // Stream for output.
		int cIndex = 0; // Counter for elements in the matrix.
		int[] Dim = new int[2]; // The dimensions of the loaded matrix.
		double[] Elem; // Pointer to the elements the matrix.

		try {
			assert (fh != null);
			Stream = fh;

			Dim[0] = (int) fh.readDouble();
			Dim[1] = (int) fh.readDouble();

			Resize(Dim[0], Dim[1]); // Resize the matrix to fit the loaded data.

			// memory to retrieve the data.
			Elem = new double[Dim[0] * Dim[1]];

			int len = Dim[0] * Dim[1];
			for (int i = 0; i < len; i++) {
				Elem[i] = fh.readDouble();
			}

			for (int cI = 0; cI < NRows(); cI++) // Put the elements into the
													// matrix.
			{
				for (int cJ = 0; cJ < NCols(); cJ++) {
					this.m_data[cI][cJ] = Elem[cIndex];
					cIndex++;
				}
			}

			Elem = null;
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Forms the Kronecker tensor product of two matrices. The result is placed
	 * in this.
	 */
	public void Kron(CDMatrix mX, CDMatrix mY) {
		Resize(mX.NRows() * mY.NRows(), mX.NCols() * mY.NCols());

		for (int cI = 0; cI < NRows(); cI++) {
			for (int cJ = 0; cJ < NCols(); cJ++) {
				this.m_data[cI][cJ] = mX.m_data[cI / mY.NRows()][cJ
						/ mY.NCols()]
						* mY.m_data[cI % mY.NRows()][cJ % mY.NCols()];
			}
		}
	}

	/**
	 * Converts the matrix in the identity matrix - i.e. zeros all-over except
	 * the ones in the diagonal.
	 */
	public void Eye() {
		this.assign(0);

		int t = NRows() < NCols() ? NRows() : NCols();

		for (int r = 0; r < t; r++) {
			this.m_data[r][r] = 1;
		}
	}

	/**
	 * Extract upper triangular part of matrix. Extracts the elements on and
	 * above the K-th diagonal. K = 0 is the main diagonal, K > 0 is above the
	 * main diagonal and K < 0 is below the main diagonal.
	 */
	public void TriU(CDMatrix matrix, final int K) {

		assert ((NRows() == matrix.NRows()) & (NCols() == matrix.NCols()));

		matrix.assign(this); // copy this matrix

		int M = NRows();
		int N = NCols();

		for (int r = 0; r < M; r++) { // for each row
			for (int c = 0; (c < r - K) & (c < N); c++) { // for each column
															// from left (0) to
															// the diagonal - 1
				matrix.m_data[r][c] = 0; // is set to zero
			}
		}
	}

	/**
	 * Extract lower triangular part of matrix. Extracts the elements on and
	 * below the K-th diagonal. K = 0 is the main diagonal, K > 0 is above the
	 * main diagonal and K < 0 is below the main diagonal.
	 */
	public void TriL(CDMatrix matrix, final int K) {

		assert ((NRows() == matrix.NRows()) & (NCols() == matrix.NCols()));

		matrix.assign(this); // copy this matrix

		int M = NRows();
		int N = NCols();

		for (int r = 0; r < M; r++) { // for each row
			for (int c = r + K + 1; c < N; c++) { // for each column from the
													// diagonal + 1 to right (N)
				matrix.m_data[r][c] = 0; // is set to zero
			}
		}
	}

	/**
	 * Element wise equal to. Comparison of a matrix and a double. Compares each
	 * element in this matrix (A) with double B. Result matrix C has elements
	 * with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) == B C(i,j) = 0 else
	 */
	public void Eq(final double B, CDMatrix C) {
		// CHECK: A and C must have same dimensions
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] == B) ? 1 : 0;
	}

	/**
	 * Element wise equal to. Comparison of two matrices. Compares each element
	 * in this matrix (A) with corresponding element in B. Result matrix C has
	 * elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) == B(i,j) C(i,j) = 0 else
	 */
	public void Eq(final CDMatrix B, CDMatrix C) {
		// CHECK: A, B and C must have same dimensions
		assert ((NRows() == B.NRows()) & (NCols() == B.NCols()));
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] == B.m_data[r][c]) ? 1 : 0;
	}

	/**
	 * Element wise not equal to. Comparison of a matrix and a double. Compares
	 * each element in this matrix (A) with double B. Result matrix C has
	 * elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) != B C(i,j) = 0 else
	 */
	public void Ne(final double B, CDMatrix C) {
		// CHECK: A and C must have same dimensions
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] != B) ? 1 : 0;
	}

	/**
	 * Element wise not equal to. Comparison of two matrices. Compares each
	 * element in this matrix (A) with corresponding element in B. Result matrix
	 * C has elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) != B(i,j) C(i,j) = 0 else
	 */
	public void Ne(final CDMatrix B, CDMatrix C) {
		// CHECK: A, B and C must have same dimensions
		assert ((NRows() == B.NRows()) & (NCols() == B.NCols()));
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] != B.m_data[r][c]) ? 1 : 0;
	}

	/**
	 * Element wise less than. Comparison of a matrix and a double. Compares
	 * each element in this matrix (A) with double B. Result matrix C has
	 * elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) < B C(i,j) = 0 else
	 */
	public void Lt(final double B, CDMatrix C) {
		// CHECK: A and C must have same dimensions
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] < B) ? 1 : 0;
	}

	/**
	 * Element wise less than. Comparison of two matrices. Compares each element
	 * in this matrix (A) with corresponding element in B. Result matrix C has
	 * elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) < B(i,j) C(i,j) = 0 else
	 */
	public void Lt(final CDMatrix B, CDMatrix C) {
		// CHECK: A, B and C must have same dimensions
		assert ((NRows() == B.NRows()) & (NCols() == B.NCols()));
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] < B.m_data[r][c]) ? 1 : 0;
	}

	/**
	 * Element wise less than or equal. Comparison of a matrix and a double.
	 * Compares each element in this matrix (A) with double B. Result matrix C
	 * has elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) <= B C(i,j) = 0 else
	 */
	public void Le(final double B, CDMatrix C) {
		// CHECK: A and C must have same dimensions
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] <= B) ? 1 : 0;
	}

	/**
	 * Element wise less than or equal. Comparison of two matrices. Compares
	 * each element in this matrix (A) with corresponding element in B. Result
	 * matrix C has elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) <= B(i,j) C(i,j) = 0 else
	 */
	public void Le(final CDMatrix B, CDMatrix C) {
		// CHECK: A, B and C must have same dimensions
		assert ((NRows() == B.NRows()) & (NCols() == B.NCols()));
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] <= B.m_data[r][c]) ? 1 : 0;
	}

	/**
	 * Element wise greater than. Comparison of a matrix and a double. Compares
	 * each element in this matrix (A) with double B. Result matrix C has
	 * elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) > B C(i,j) = 0 else
	 */
	public void Gt(final double B, CDMatrix C) {
		// CHECK: A and C must have same dimensions
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] > B) ? 1 : 0;
	}

	/**
	 * Element wise greater than. Comparison of two matrices. Compares each
	 * element in this matrix (A) with corresponding element in B. Result matrix
	 * C has elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) > B(i,j) C(i,j) = 0 else
	 */
	public void Gt(final CDMatrix B, CDMatrix C) {
		// CHECK: A, B and C must have same dimensions
		assert ((NRows() == B.NRows()) & (NCols() == B.NCols()));
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] > B.m_data[r][c]) ? 1 : 0;
	}

	/**
	 * Element wise greater than or equal. Comparison of a matrix and a double.
	 * Compares each element in this matrix (A) with double B. Result matrix C
	 * has elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) >= B C(i,j) = 0 else
	 */
	public void Ge(final double B, CDMatrix C) {
		// CHECK: A and C must have same dimensions
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] >= B) ? 1 : 0;
	}

	/**
	 * Element wise greater than or equal. Comparison of two matrices. Compares
	 * each element in this matrix (A) with corresponding element in B. Result
	 * matrix C has elements with values 1 or 0.
	 * 
	 * C(i,j) = 1 if A(i,j) <= B(i,j) C(i,j) = 0 else
	 */
	public void Ge(final CDMatrix B, CDMatrix C) {
		// CHECK: A, B and C must have same dimensions
		assert ((NRows() == B.NRows()) & (NCols() == B.NCols()));
		assert ((NRows() == C.NRows()) & (NCols() == C.NCols()));

		int n = NRows();
		int m = NCols();

		for (int r = 0; r < n; r++)
			for (int c = 0; c < m; c++)
				C.m_data[r][c] = (this.m_data[r][c] >= B.m_data[r][c]) ? 1 : 0;
	}

	/**
	 * Takes the natural logarithm (base e=2.71..) of each matrix element.
	 */
	public void Log() {
		for (int i = 0; i < NRows(); i++)
			for (int j = 0; j < NCols(); j++)
				this.m_data[i][j] = Math.log(this.m_data[i][j]);
	}

	/**
	 * FlipLR modifies the matrix with row preserved and columns flipped in the
	 * left/right direction.
	 */
	public void FlipLR() {

		int cols = NCols();
		int rows = NRows();
		double tmp;
		int c2 = cols / 2;

		for (int c = 0; c < c2; c++) {
			for (int r = 0; r < rows; r++) {
				tmp = this.m_data[r][c];
				this.m_data[r][c] = this.m_data[r][cols - c - 1];
				this.m_data[r][cols - c - 1] = tmp;
			}
		}
	}

	/**
	 * Flip matrix in up/down direction. FlipUD(X) modifies the matrix with
	 * columns preserved and rows flipped in the up/down direction.
	 */
	public void FlipUD() {

		int cols = NCols();
		int rows = NRows();
		double[] e_top = null;
		double[] e_bottom = null;
		int r2 = rows / 2;
		int row_size = cols;
		double[] tmp_row = new double[cols];

		for (int r = 0; r < r2; r++) {

			e_top = this.m_data[r];
			e_bottom = this.m_data[rows - r - 1];

			// memcpy( tmp_row, e_top, row_size );
			System.arraycopy(e_top, 0, tmp_row, 0, row_size);
			// memcpy( e_top, e_bottom, row_size );
			System.arraycopy(e_bottom, 0, e_top, 0, row_size);
			// memcpy( e_bottom, tmp_row, row_size );
			System.arraycopy(tmp_row, 0, e_bottom, 0, row_size);
		}

	}

	/**
	 * Uniformly distributed random numbers. Inserts uniformly distributed
	 * random numbers in the range [0;1].
	 */
	public void Rand() {

		int cols = NCols();
		int rows = NRows();

		for (int r = 0; r < rows; r++) {

			for (int c = 0; c < cols; c++) {

				this.m_data[r][c] = Math.random() / (double) RAND_MAX;
			}
		}
	}

	/**
	 * Forms the outer product of two vectors and store the result in this. If
	 * the matrix does have the correct size, it will be resized.
	 * 
	 * @param v1
	 *            Input vector.
	 * @param v2
	 *            Input vector.
	 */
	public void OuterProduct(final CDVector v1, final CDVector v2) {

		if (NRows() != v1.Length() || NCols() != v2.Length()) {

			this.Resize(v1.Length(), v2.Length());
		}

		this.assign(VisOuterProduct(v1, v2));
	}

	/**
	 * Calculates the determinant of a square matrix up to the 3rd order.
	 * 
	 * @return The determinant.
	 */
	public double Det() {

		double d = 0;

		assert (NRows() == NCols());

		if (NRows() == 3) {

			// 3x3
			d = this.m_data[0][0] * this.m_data[1][1] * this.m_data[2][2]
					+ this.m_data[0][1] * this.m_data[1][2] * this.m_data[2][0]
					+ this.m_data[0][2] * this.m_data[1][0] * this.m_data[2][1]
					- this.m_data[0][0] * this.m_data[1][2] * this.m_data[2][1]
					- this.m_data[0][1] * this.m_data[1][0] * this.m_data[2][2]
					- this.m_data[0][2] * this.m_data[1][1] * this.m_data[2][0];
		}
		if (NRows() == 2) {

			// 2x2
			d = this.m_data[0][0] * this.m_data[1][1] - this.m_data[1][0]
					* this.m_data[0][1];
		}
		if (NRows() == 1) {

			// 1x1
			d = this.m_data[0][0];
		}
		if (NRows() > 3) {
			// NxN
			d = Determinant();
		}

		return d;
	}

	/**
	 * Calculates the trace - i.e. the sum of diagonal elements.
	 * 
	 * @return The trace.
	 */
	public double Trace() {

		double trace = .0;

		assert (NRows() == NCols());
		for (int i = 0; i < NRows(); i++) {

			trace += this.m_data[i][i];
		}

		return trace;
	}

	/**
	 * Converts this matrix to a vector, either by row (default) or column.
	 * 
	 * @param v
	 *            Output vector
	 * @param rowWise
	 *            If true (default) matrix data is extracted row-wise. If false
	 *            matrix data is extracted column-wise.
	 */
	public void ToVector(CDVector v, boolean rowWise) {

		int i = 0;
		v.Resize(NRows() * NCols());
		if (rowWise) {

			for (int r = 0; r < NRows(); r++) {
				for (int c = 0; c < NCols(); c++) {

					v.m_data[i++] = this.m_data[r][c];
				}
			}
		} else {

			for (int c = 0; c < NCols(); c++) {
				for (int r = 0; r < NRows(); r++) {

					v.m_data[i++] = this.m_data[r][c];
				}
			}
		}
	}

}