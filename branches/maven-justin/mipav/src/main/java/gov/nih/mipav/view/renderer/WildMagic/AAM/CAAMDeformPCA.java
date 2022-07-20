package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import java.util.*;
import java.io.*;

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
 * Performs Principal Component Analysis on a set of data vectors. The PCA basis
 * can then be used for shape deformation.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMDeformPCA extends CAAMDeform {

	private CDMatrix m_mEigenVectors = new CDMatrix();
	private CDVector m_vEigenValues = new CDVector();
	private CDVector m_vEigenValuesOrg = new CDVector();
	private Vector<CDVector> m_vvData = new Vector<CDVector>();
	private CDVector m_vDataMean = new CDVector();

	/**
	 * Returns the eigen values in vector form.
	 * 
	 * @return eigen values
	 */
	public CDVector EigenValues() {
		return m_vEigenValues;
	}

	/**
	 * Returns the orignal eigen values in vector form.
	 * 
	 * @return eigen values original
	 */
	public final CDVector EigenValuesOrg() {
		return m_vEigenValuesOrg;
	}

	/**
	 * Returns the eigen vectors in matrix form.
	 * 
	 * @return eigen vectors
	 */
	public CDMatrix EigenVectors() {
		return m_mEigenVectors;
	}

	/**
	 * Returns the number of principal parameters.
	 * 
	 * @return length of eigen values
	 */
	public int NParameters() {
		return m_vEigenValues.Length();
	}

	/**
	 * Returns the number of princal parameters before any truncation.
	 * 
	 * @return eigen values original length
	 */
	public int NParametersOrg() {
		return m_vEigenValuesOrg.Length();
	}

	/**
	 * Deletes the data matrix.
	 */
	public void ClearDataItems() {
		m_vvData.clear();
	}

	/**
	 * Returns the number of data items this basis is based on.
	 * 
	 * @return data matrix size
	 */
	public final int NDataItems() {
		return m_vvData.size();
	}

	/**
	 * Returns the eigenvalue of the i-th parameter in absolute numbers or as
	 * percentage.
	 * 
	 * @param i
	 *            index
	 * @param asPercentage
	 *            boolean flag to indicate % return or abs number
	 * @return ret the eigenvalue.
	 */
	public double ParameterWeight(final int i, boolean asPercentage) {

		double ret = m_vEigenValues.m_data[i];

		if (asPercentage) {

			ret /= m_vEigenValues.Sum();
		}

		return ret;
	}

	/**
	 * Returns the eigenvalue of the i-th parameter in absolute numbers or as
	 * percentage.
	 * 
	 * @param i
	 *            index
	 * @param asPercentage
	 *            boolean flag to indicate % return or abs number
	 * @return ret the eigenvalue.
	 */
	public double ParameterWeightOrg(final int i, boolean asPercentage) {

		double ret = m_vEigenValuesOrg.m_data[i];

		if (asPercentage) {

			ret /= m_vEigenValuesOrg.Sum();
		}

		return ret;
	}

	/**
	 * Inserts a data vector
	 * 
	 * @param v
	 *            a data vector
	 */
	public void InsertDataItem(final CDVector v) {

		m_vvData.add(v);
	}

	/**
	 * Makes the object use an identity basis instead of PCA basis, i.e.
	 * essentially a by-pass of this object.
	 */
	public void UseIdentityTransformation() {

		// get nb samples in each data item
		int nbSamples = m_vvData.get(0).Length();
		int nbDataItems = m_vvData.size();

		// we need at least one data item to do this
		assert (nbDataItems > 0);

		// calc the mean data item
		m_vDataMean.Resize(nbSamples);
		m_vDataMean.assign(0);
		for (int i = 0; i < nbDataItems; i++) {

			m_vDataMean.add_into(m_vvData.get(i));
		}
		m_vDataMean.div_into(nbDataItems);

		CAAMMathUtil.CalcElementVar(m_vvData, m_vEigenValues);
		m_vEigenValuesOrg.assign(m_vEigenValues);

		// set the eigenvectors to the identity matrix
		m_mEigenVectors.Resize(nbSamples, nbSamples);
		m_mEigenVectors.Eye();

		// everything is now ready
		m_bValid = true;

		// return without further ado
		return;
	}

	/**
	 * Apply PCA analysis
	 * 
	 * @return success or not.
	 */
	public boolean DoPCA() {
		return DoPCA(false);
	}

	/**
	 * Performs the principal component analysis on the data items. Uses the
	 * Eckhart-Young theorem if necessary for reduced memory and computational
	 * requirements.
	 * 
	 * @param bWriteCoVarMatrix
	 *            If true the covariance matrix is written to the current dir.
	 *            NOTE: Only in the case of more data items than dimensions
	 *            (samples).
	 * @return success or not
	 */
	public boolean DoPCA(boolean bWriteCoVarMatrix) {

		// get nb samples in each data item
		int nbSamples = m_vvData.get(0).Length();
		int nbDataItems = m_vvData.size();

		// we need at least one data item to do this
		assert (nbDataItems > 0);

		// handle the training set of size 1 situation
		//
		// in the AAM case this would lead to a model
		// containing pure rigid-body deformation
		// parameters (i.e. only pose parameters would
		// be present)
		//
		if (nbDataItems == 1) {

			// the hacker solution:
			// to add one 'null'-mode that don't deform the data
			//
			// this is due to the fact that all code expects
			// at least one deformation parameter
			//
			// sorry about this hack, but this seem as the
			// most clean solution anyway, otherwise all
			// code would be bloated with checks on the number
			// of parameters in the deformation model
			//
			m_vEigenValues.Resize(1);
			m_vEigenValuesOrg.Resize(1);

			m_vEigenValues.assign(1);
			m_vEigenValuesOrg.assign(1);

			m_vDataMean.assign(m_vvData.get(0));

			m_mEigenVectors.Resize(nbSamples, 1);
			m_mEigenVectors.assign(.0);

			// everything is now ready
			m_bValid = true;

			// return without further ado
			return true;
		}

		// calc the mean data item
		m_vDataMean.Resize(nbSamples);
		m_vDataMean.assign(0);
		for (int i = 0; i < nbDataItems; i++) {

			m_vDataMean.add_into(m_vvData.get(i));
		}
		m_vDataMean.div_into(nbDataItems);

		// my apologies for the difference in coding
		// in the two if-branches below, they were
		// originally from two different classes
		// I'll rewrite it some day... :-)
		if (nbDataItems > nbSamples) {

			// more observations than data dimensions
			// -> do a normal PCA

			// calculate the covariance matrix

			CDMatrix covarMatrix = new CDMatrix(nbSamples, nbSamples);
			covarMatrix.assign(.0);
			for (int i = 0; i < nbDataItems; i++) {
				CDVector x_i = new CDVector(nbSamples);
				CDMatrix dx_i = new CDMatrix(nbSamples, 1);
				x_i.assign(m_vvData.get(i).sub(m_vDataMean));
				dx_i.SetColumn(0, x_i);
				covarMatrix.add_into(dx_i.mult(dx_i.Transposed()));
			}
			covarMatrix.div_into(nbDataItems);

			// write the covariance matrix to the current dir
			if (bWriteCoVarMatrix) {

				covarMatrix.ToMatlab("pca_covar.m", "pca_cv",
						"The covariance matrix of a training set.", false);
			}

			// calculate eigenvalues and eigenvectors
			m_vEigenValues.Resize(nbSamples);
			m_mEigenVectors.Resize(covarMatrix.NRows(), covarMatrix.NCols());

			boolean isValid = VisDMatrixSymmetricEigen(covarMatrix,
					m_vEigenValues, m_mEigenVectors);
			if (isValid == false)
				return false;

			// VisDMatrixSymmetricEigen( covarMatrix, m_vEigenValues,
			// m_mEigenVectors);
			// EigenvalueDecomposition ed = new
			// EigenvalueDecomposition(covarMatrix);
			// m_vEigenValues = (CDVector)ed.getRealEigenvalues();
			// m_mEigenVectors = (CDMatrix)ed.getV();
			/*
			 * int itype = 1; final char jobz = 'V'; final char uplo = 'U';
			 * final int n = nbSamples;
			 * 
			 * final int lda = n; CDMatrix B = new CDMatrix(); B.Resize( n, n );
			 * B.Eye();
			 * 
			 * final int ldb = n; final double[] w = new double[n]; final
			 * double[] work = new double[100]; final int lwork = 100; final
			 * int[] info = new int[1]; GeneralizedEigenvalue ge = new
			 * GeneralizedEigenvalue(); // ge.dsygv(itype, jobz, uplo, n,
			 * covarMatrix.m_data, lda, B.m_data, ldb, w, work, lwork, info);
			 * ge.dsyev(jobz, uplo, n, covarMatrix.m_data, lda, w, work, lwork,
			 * info); if (info[0] != 0) { return; } m_vEigenValues.assign(w);
			 * m_mEigenVectors.assign(covarMatrix);
			 */

		} else {

			// more observations than data dimensions
			// -> use the Eckhart-Youngh theorem in the PCA
			// see also the appendix of Cootes' AAM-report
			CDMatrix D = new CDMatrix();
			CDMatrix T = new CDMatrix();
			CDMatrix e_i = new CDMatrix();

			// build D
			D.Resize(nbSamples, nbDataItems);

			for (int i = 0; i < nbDataItems; i++) {
				CDVector x_i = new CDVector();
				x_i.Resize(nbSamples);
				x_i.assign(m_vvData.get(i).sub(m_vDataMean));
				D.SetColumn(i, x_i);
			}

			// build T
			T.Resize(nbDataItems, nbDataItems);
			T.assign(D.Transposed().mult(D));
			T.div_into(nbDataItems);

			// calculate eigenvalues and eigenvectors of the 'small' matrix T
			m_vEigenValues.Resize(nbDataItems);
			e_i.Resize(nbDataItems, nbDataItems);

			boolean isValid = VisDMatrixSymmetricEigen(T, m_vEigenValues, e_i);
			if (isValid == false)
				return false;
			/*
			 * int itype = 1; final char jobz = 'V'; final char uplo = 'U';
			 * final int n = nbDataItems;
			 * 
			 * final int lda = n; CDMatrix B = new CDMatrix(); B.Resize( n, n );
			 * B.Eye();
			 * 
			 * final int ldb = n; final double[] w = new double[n]; final
			 * double[] work = new double[100]; final int lwork = 100; final
			 * int[] info = new int[1]; GeneralizedEigenvalue ge = new
			 * GeneralizedEigenvalue(); // ge.dsygv(itype, jobz, uplo, n,
			 * T.m_data, lda, B.m_data, ldb, w, work, lwork, info);
			 * ge.dsyev(jobz, uplo, n, T.m_data, lda, w, work, lwork, info); if
			 * (info[0] != 0) { return; } m_vEigenValues.assign(w);
			 * e_i.assign(T);
			 */

			// transform into the eigen vectors of 'm_mEigenVectors'
			m_mEigenVectors.Resize(nbSamples, nbDataItems);
			m_mEigenVectors.assign(D.mult(e_i));

			// normalize the pseudo eigenvectors in 'm_mEigenVectors'

			for (int i = 0; i < nbDataItems; i++) {
				CDVector tmp = new CDVector(nbSamples);
				m_mEigenVectors.Col(i, tmp);
				tmp.Normalize2();
				m_mEigenVectors.SetColumn(i, tmp);
			}
		}

		// remove 'zero' eigen-values
		// that is eigenvalues that contribute
		// less than 0.01% to the total variation
		//
		// this is *not* considered a truncation since
		// eigenvalues are *very* close to zero
		//
		double limit = .0001 * m_vEigenValues.Sum();
		int i;
		for (i = 0; i < m_vEigenValues.Length(); i++) {

			if (m_vEigenValues.m_data[i] > limit)
				break;
		}
		int NZeroEV = i;
		int NNonZeroEV = m_vEigenValues.Length() - NZeroEV;

		CDVector v = new CDVector(NNonZeroEV);
		for (i = 0; i < NNonZeroEV; i++) {

			v.m_data[i] = m_vEigenValues.m_data[i + NZeroEV];
		}

		if (v.m_data == null || v.m_data.length == 0)
			return false;

		m_vEigenValues.Resize(NNonZeroEV);
		m_vEigenValues.assign(v);

		CDMatrix truncEVEC = new CDMatrix();
		truncEVEC.Resize(m_mEigenVectors.NRows(), NNonZeroEV);
		truncEVEC.assign(m_mEigenVectors.Submatrix(truncEVEC.NRows(),
				truncEVEC.NCols(), 0, NZeroEV));
		m_mEigenVectors.Resize(truncEVEC.NRows(), truncEVEC.NCols());
		m_mEigenVectors.assign(truncEVEC);

		// copy to org eigenvalues -> no VEL truncation (yet)
		m_vEigenValuesOrg.Resize(m_vEigenValues.Length());
		m_vEigenValuesOrg.assign(m_vEigenValues);

		// everything is now ready
		m_bValid = true;
		return true;
	}

	/**
	 * Perform symmetric eigen analysis.
	 * 
	 * @param A
	 * @param vals
	 * @param vects
	 * @return
	 */
	public boolean VisDMatrixSymmetricEigen(final CVisDMatrix A,
			CVisDVector vals, CVisDMatrix vects) {
		CVisDMatrix tmp = A;
		int M = A.NRows();
		assert (M == A.NCols());
		assert (vals.Length() == M);
		assert (vects.NRows() == M);
		assert (vects.NCols() == M);
		int info = Dsyev(M, tmp.m_data, vals.m_data, vects.m_data);
		if (info != 0) {
			// MipavUtil.displayError("Cannot compute eigenvalue and eigenvectors in VisDMatrixSymmetricEigen");
			return false;
		}
		return true;
	}

	/**
	 * Call the lapack routine
	 * 
	 * @param m
	 * @param a
	 * @param vals
	 * @param vects
	 * @return
	 */
	public int Dsyev(final int m, double[][] a, double[] vals, double[][] vects) {
		if (m < 1)
			return 1;
		char jobz;
		char uplo = 'U';
		int[] info = new int[1];
		int lda = m;
		int lwork = 10 * m; // should be >= max(1,3*m-1)
		double[] work = new double[lwork];
		double[][] V;
		jobz = (vects != null ? 'V' : 'N');
		V = new double[m][m];

		for (int i = 0; i < m; i++)
			for (int j = 0; j < m; j++)
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

	/**
	 * Returns the mean of all sample vectors.
	 * 
	 * @return A mean vector.
	 */
	public final CDVector MeanDataItem() {
		return m_vDataMean;
	}

	/**
	 * @author Mikkel B. Stegmann
	 * @version 10-18-2000
	 * @memo Writes a PCA object to file.
	 * @doc Writes a PCA object to a binary file.
	 * @see FromFile
	 * @param fh
	 *            File handle to binary file open for writing.
	 * @return Nothing.
	 */

	/**
	 * Writes a PCA object to a binary file.
	 * 
	 * @param fh
	 *            file handler to output stream
	 */
	public void ToFile(DataOutputStream fh) {

		if (m_bValid == false) {

			System.err.println("Can't save data.  Call DoPCA() first." + "");

		}

		if (fh == null) {

			System.err.println("fh == null " + " ToFile ");

		}

		m_mEigenVectors.ToFile(fh);
		m_vEigenValues.ToFile(fh);
		m_vEigenValuesOrg.ToFile(fh);
		m_vDataMean.ToFile(fh);
	}

	/**
	 * Reads a PCA object from a binary file.
	 * 
	 * @param fh
	 *            file handler to open binary file for reading
	 */
	public void FromFile(DataInputStream fh) {

		if (fh == null) {
			System.err.println("fh == null" + " FromFile"
					+ " CAAMDeformPCA.java");
		}

		m_mEigenVectors.FromFile(fh);
		m_vEigenValues.FromFile(fh);
		m_vEigenValuesOrg.FromFile(fh);
		m_vDataMean.FromFile(fh);

		m_bValid = true;
	}

	/**
	 * Debug method that provides a human readable file dump of the object data.
	 * Writes eigen vectors and eigen values to matlab text files:
	 * pca_eigenvectors.m pca_eigenvalues.m pca_eigenvalues_org.m
	 * 
	 * @param szPath
	 *            Path including terminating backslash where the data is dumped.
	 */
	public void Dump(final String szPath) {

		String str;

		str = szPath.concat("pca_eigenvectors.m");
		m_mEigenVectors.ToMatlab(str, "pca_evec",
				"Column eigenvectors from CAAMDeformPCA.", false);

		str = szPath.concat("pca_eigenvalues.m");
		m_vEigenValues.ToMatlab(str, "pca_ev",
				"Eigenvalues from CAAMDeformPCA.", false);

		str = szPath.concat("pca_eigenvalues_org.m");
		m_vEigenValuesOrg.ToMatlab(str, "pca_ev_org",
				"Original eigenvalues from CAAMDeformPCA.", false);
	}

	/**
	 * Projects a set of PC scores to the original space.
	 * 
	 * @param params
	 *            PC scores, i.e. the model paramerisation.
	 * @param object
	 *            Resulting projection into the original space, e.g. a shape.
	 * @return Nothing.
	 */
	public void Deform(final CDVector params, CDVector object) {
		object.Resize(m_mEigenVectors.NRows());
		object.assign(m_vDataMean.add(m_mEigenVectors.mult(params)));
	}

	/**
	 * Returns the Mahalanobis distance of a set of PCA parameters.
	 * 
	 * @param params
	 *            A set of PCA parameters.
	 * @return The Mahalanobis distance.
	 */
	public double MahalanobisDistance(final CDVector params) {

		double maha = .0;

		assert (params.Length() == m_vEigenValues.Length());

		for (int i = 0; i < params.Length(); i++) {

			if (!(m_vEigenValues.m_data[i] < 1e-15)) { // exclude near-zero
														// eigenvalues

				maha += params.m_data[i] * params.m_data[i]
						/ m_vEigenValues.m_data[i];
			}

		}

		return maha;
	}

	/**
	 * Shuffle each dimension in all data items over all observations.
	 */
	public void ShuffleData() {

		int p = m_vvData.get(0).Length();
		int n = (int) m_vvData.size();
		CDVector indicies = new CDVector();
		CDVector shuffledIndicies = new CDVector(n);
		indicies.Linspace(0, n - 1, n);
		double tmp;

		// for each dimension
		for (int i = 0; i < p; i++) {

			// make shuffled indicies
			shuffledIndicies.assign(indicies);
			shuffledIndicies.Shuffle();

			// apply shuffling over all observations
			for (int j = 0; j < n; j++) {

				// suffled index
				int jj = (int) shuffledIndicies.m_data[j];

				// swap 'j' and 'jj'
				tmp = m_vvData.get(j).m_data[i];
				m_vvData.get(j).m_data[i] = m_vvData.get(jj).m_data[i];
				m_vvData.get(jj).m_data[i] = tmp;
			}
		}
	}

	/**
	 * Truncate eigenvectors and eigenvalues using parallel analysis. Also known
	 * as Horn's parallel analysis or Humphrey-Ilgen parallel analysis. Assumes
	 * the data items have not been cleared.
	 * 
	 * @return The number of parameters in the truncated basis.
	 */
	public int TruncateParallel() {

		//
		// below: a poor mans version of PA, i.e. we don't keep
		// keep track of the permutations, instead we are
		// just doing random samples within each dimension
		// across all data items
		//
		assert (m_vvData.size() > 0);

		int p = m_vvData.get(0).Length();
		int n = (int) m_vvData.size();
		int nEV = m_vEigenValues.Length();
		CDVector v = new CDVector(p);
		CDVector simulatedEV = new CDVector(nEV);
		simulatedEV.assign(.0);

		// set-up the number of Monte Carlo simulations
		int nSimulations = 10;

		for (int s = 0; s < nSimulations; s++) {

			// create PCA object
			CAAMDeformPCA pca = new CAAMDeformPCA();

			// copy data
			// pca.m_vvData = this.m_vvData;
			for (int q = 0; q < n; q++) {
				CDVector temp = new CDVector();
				temp.assign(this.m_vvData.get(q));
				pca.m_vvData.add(temp);
			}

			// shuffle data
			pca.ShuffleData();

			// calc PCA
			pca.DoPCA();
			pca.ClearDataItems();

			// accumulate simulated eigenvalue into 'simulatedEV'
			final CDVector simEV = pca.EigenValues();
			for (int e = 0; e < Math.min(simEV.Length(), nEV); e++) {

				simulatedEV.m_data[e] += simEV.m_data[e];
			}
		}

		// calc mean eigenvalues
		simulatedEV.div_into(nSimulations);

		// dermine PA cut-off (largest eigenvalues are last)
		int paCutOff = 0;
		for (int i = nEV - 1; i >= 0; i--) {

			if (m_vEigenValues.m_data[i] >= simulatedEV.m_data[i]) {

				++paCutOff;
			} else {

				break;
			}
		}

		// force at least one mode
		paCutOff = paCutOff == 0 ? 1 : paCutOff;

		// debug
		m_vEigenValues.ToMatlab(CAAMUtil.FindVacantFilename("data_ev.m"),
				"dev", "", false);
		simulatedEV.ToMatlab(CAAMUtil.FindVacantFilename("sim_ev.m"), "sev",
				"", false);

		// truncate eigenvectors/values
		this.Truncate(paCutOff);

		// return the retain number of modes
		return paCutOff;
	}

	/**
	 * Truncate eigenvectors and eigenvalues to retain 'retained_variance'. I.e.
	 * if retained_variance = .95 this function calculates how many eigenvectors
	 * we need to explain 95% of the total variation in the PCA training set.
	 * 
	 * @param retained_variance
	 *            Amount of variance to retain [0<x<1].
	 * @return The number of parameters in the truncated basis.
	 */
	public int TruncateVar(final double retained_variance) {

		// truncate eigenvectors and eigenvalues to
		// satisfy the variance explanation level constraint
		int n = CalcNParam(retained_variance);

		Truncate(n);

		return n;
	}

	/**
	 * Truncate eigenvectors and eigenvalues to retain the 'n' largest.
	 * 
	 * @param n
	 *            Amount of eigenvectors/values to retain.
	 */
	public void Truncate(final int n) {

		// truncate eigenvectors and eigenvalues
		int cufoff = NParameters() - n;

		CDMatrix truncEVEC = new CDMatrix(m_mEigenVectors.NRows(), n);
		truncEVEC.assign(m_mEigenVectors.Submatrix(truncEVEC.NRows(),
				truncEVEC.NCols(), 0, cufoff));
		m_mEigenVectors.Resize(truncEVEC.NRows(), truncEVEC.NCols());
		m_mEigenVectors.assign(truncEVEC);

		CDVector truncEV = new CDVector(n);
		for (int i = 0; i < n; i++) {

			truncEV.m_data[i] = m_vEigenValues.m_data[i + cufoff];
		}
		m_vEigenValues.Resize(truncEV.Length());
		m_vEigenValues.assign(truncEV);
	}

	/**
	 * Calulates the needed number of parameters to retain 'retained_variance'.
	 * I.e. if retained_variance = .95 then this function calculates how many
	 * eigenvectors we need to explain 95% of the total variation in the PCA
	 * training set. Assumes that no eigenvalue cutoff has been done prior to
	 * the call.
	 * 
	 * @param retained_variance
	 *            Amount of variance to retain [0<x<1].
	 * @return The number of model parameters.
	 */
	public int CalcNParam(final double retained_variance) {

		double sum = EigenValues().Sum();
		double ps = .0;
		int np = 0;

		for (int i = NParameters() - 1; i >= 0; i--) {

			ps += EigenValues().m_data[i];
			++np;
			if (ps / sum >= retained_variance)
				break;
		}

		return np;
	}

	/**
	 * Projects an observation into the PCA space. Notice: Costly, due to the
	 * non-cached transpose of the eigenvectors.
	 * 
	 * @param obs
	 *            Input observation.
	 * @param param
	 *            Output model parameters of the (possibly truncated) basis.
	 * @return Nothing.
	 */
	public void Project(final CDVector obs, CDVector param) {
		param.Resize(NParameters());
		param.assign(this.m_mEigenVectors.Transposed().mult(
				(obs.sub(m_vDataMean))));
	}

	/**
	 * Back projects a set of PCA model parameters into the original space.
	 * 
	 * @param param
	 *            Input PCA model parameters.
	 * @param synth_obs
	 *            Synthesized output observation.
	 */
	public void BackProject(final CDVector param, CDVector synth_obs) {
		final int n = m_vDataMean.Length();
		synth_obs.Resize(n);
		synth_obs.assign(m_vDataMean);
		synth_obs.add_into(m_mEigenVectors.mult(param));
	}

	/**
	 * Projects an observation into the PCA space and back. Notice: Costly, due
	 * to the non-cached transpose of the eigenvectors in the project call.
	 * 
	 * @param obs
	 *            Input observation.
	 * 
	 */
	public void Filter(CDVector obs) {

		CDVector param = new CDVector();

		this.Project(obs, param);
		this.BackProject(param, obs);
	}

}