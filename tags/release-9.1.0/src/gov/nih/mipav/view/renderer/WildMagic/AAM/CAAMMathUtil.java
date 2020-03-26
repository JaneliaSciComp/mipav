package gov.nih.mipav.view.renderer.WildMagic.AAM;

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
 * Utility math/statistical methods for the AAM project. This is the
 * mathematical / statistical garbage bin. It consists of methods that for some
 * reason haven't found its way into other (and more meaningful) classes.
 * methods are static, so there is never need for an instantiation of this
 * class.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMMathUtil {

	/**
	 * Calculates the histogram of a vector within [min;max] with optional
	 * nomalization.
	 * 
	 * @param v
	 *            Input vector.
	 * @param min
	 *            Value for the first bin (values lower are clamped to min).
	 * @param max
	 *            Value for the last bin (values lower are clamped to max).
	 * @param hist
	 *            Output histogram vector.
	 * @param nbins
	 *            The number of bins. Default 256.
	 * @param normalize
	 *            Optional normalization, i.e. make the histogram entries sum to
	 *            1. Default true.
	 * @param transform
	 *            Optional transformation of v into 'nbins' integer intervals
	 *            between [min;max]. Default false.
	 */
	public static void Hist(CDVector v, final double min, final double max,
			CDVector hist, final int nbins, boolean normalize, boolean transform) {

		hist.Resize(nbins);

		hist.assign(.0);
		double val, mag = max - min;
		int bin, len = v.Length();
		for (int i = 0; i < len; i++) {

			val = v.m_data[i];
			val = val > max ? max : (val < min ? min : val);
			bin = (int) (.5 + (nbins - 1) * (val - min) / mag);
			++hist.m_data[bin];
			if (transform)
				v.m_data[i] = bin;
		}

		if (normalize) {

			hist.div_into(len);
		}
	}

	/**
	 * Cumulative sum of elements.
	 * 
	 * @param v
	 *            Input vector.
	 * @param cumsum
	 *            Output vector containing the cumulative sum of elements.
	 * @param normalize
	 *            Optional normalization of output, i.e. cumsum[end] = 1.0.
	 *            Default true.
	 */
	public void CumSum(final CDVector v, CDVector cumsum, boolean normalize) {

		int len = v.Length();
		cumsum.Resize(len);
		cumsum.m_data[0] = v.m_data[0];
		for (int i = 1; i < len; i++) {

			cumsum.m_data[i] = cumsum.m_data[i - 1] + v.m_data[i];
		}

		if (normalize) {

			cumsum.div_into(cumsum.m_data[len - 1]);
		}
	}

	/**
	 * Calulates the Mutual Information (MI) between two vectors. See e.g. C.
	 * Studholme et al. or Viola et al.
	 * 
	 * @param v1
	 *            Signal 1.
	 * @param v2
	 *            Signal 2.
	 * @param nbins
	 *            The number of bins in the single and joint histograms. Default
	 *            256.
	 * @return The Mutual Information (MI).
	 */
	public static double MutualInformation(final CDVector v1,
			final CDVector v2, int nbins) {

		assert (v1.Length() == v2.Length());

		double v1min, v1max, v2min, v2max;
		v1min = v1.Min();
		v1max = v1.Max();
		v2min = v2.Min();
		v2max = v2.Max();
		int v1bin, v2bin, len = v1.Length();

		// creat hists and reset them
		CDVector hist1 = new CDVector(nbins);
		CDVector hist2 = new CDVector(nbins);
		CDMatrix hist12 = new CDMatrix(nbins, nbins);
		hist1.assign(.0);
		hist2.assign(.0);
		hist12.assign(.0);

		// make and normalize hists
		double mag1 = v1max - v1min;
		double mag2 = v2max - v2min;
		for (int i = 0; i < len; i++) {

			v1bin = (int) (.5 + (nbins - 1) * (v1.m_data[i] - v1min) / mag1);
			v2bin = (int) (.5 + (nbins - 1) * (v2.m_data[i] - v2min) / mag2);

			++hist1.m_data[v1bin];
			++hist2.m_data[v2bin];
			++hist12.m_data[v1bin][v2bin];
		}
		hist1.div_into(len);
		hist2.div_into(len);
		hist12.div_into(len);

		// calculate entropy of the signals
		double eps = 2.220446049250313e-016;
		double hi, H1 = .0, H2 = .0, log2 = Math.log(2);
		for (int i = 0; i < nbins; i++) {

			hi = hist1.m_data[i];
			H1 -= hi * Math.log(hi + eps) / log2;
			hi = hist2.m_data[i];
			H2 -= hi * Math.log(hi + eps) / log2;
		}
		// calculate joint entropy
		double H12 = .0;
		for (int i = 0; i < nbins; i++) {
			for (int j = 0; j < nbins; j++) {

				hi = hist12.m_data[i][j];
				H12 -= hi * Math.log(hi + eps) / log2;
			}
		}

		// calculate the mutual information
		return H1 + H2 - H12;

	}

	/**
	 * Maps the distibution of v into an approximate Gaussian distribution.
	 * 
	 * @param v
	 *            Input vector.
	 * @param out
	 *            Output vector.
	 * @param nbins
	 *            Number of bins used to approx the distribution. Default 256.
	 * @param gaussLUT
	 *            If not null, the calulated LUT is returned here.
	 * @return Nothing.
	 */
	public void GaussianHistogramMatching(final CDVector v, CDVector out,
			final int nbins, CDVector gaussLUT) {

		int v_len = v.Length();
		out.Resize(v_len);
		CDVector gauss = new CDVector(nbins);
		CDVector gauss_accum = new CDVector(nbins);
		CDVector x = new CDVector(nbins);

		// calc gauss frq
		double k = 1.0 / Math.sqrt(2 * Math.PI);
		double tail = 3.5;
		for (int i = 0; i < nbins; i++) {

			x.m_data[i] = tail * (2. * i / (nbins - 1.) - 1.);
			gauss.m_data[i] = k * Math.exp(-.5 * x.m_data[i] * x.m_data[i]);
		}

		// calc accum gauss dist
		CumSum(gauss, gauss_accum, true);

		// make hist of incoming vector
		CDVector hist = new CDVector();
		CDVector accum_hist = new CDVector();
		out.assign(v);
		Hist(out, out.Min(), out.Max(), hist, nbins, true, true);
		CumSum(hist, accum_hist, true);

		// make a lookup table that maps 'hist' into
		// a gaussian distribution
		CDVector gausslut = new CDVector(nbins);
		int binhit = 0;
		for (int i = 0; i < nbins; i++) {

			// use monotonicity and find the closest class in
			// the accumulative gaussian distribution
			while (Math.abs(accum_hist.m_data[i]
					- gauss_accum.m_data[binhit + 1]) < Math
					.abs(accum_hist.m_data[i] - gauss_accum.m_data[binhit])) {

				++binhit;
			}

			// map the i-th class of v into x
			gausslut.m_data[i] = x.m_data[binhit];
		}

		// transform
		for (int i = 0; i < v_len; i++) {

			out.m_data[i] = gausslut.m_data[(int) (out.m_data[i])];
		}

		// copy gausslut (if specified)
		if (gaussLUT != null) {

			gaussLUT.Resize(gausslut.Length());
			gaussLUT.assign(gausslut);
		}
	}

	/**
	 * This method expands a matrix to have dynamic size, i.e. nrows and ncols
	 * that are powers of two. Expansion are done using zero-padding.
	 * 
	 * @param m
	 *            Input matrix.
	 * @param dyad
	 *            Output dynamic version.
	 */
	public void ExpandMatrix2DyadicSize(final CDMatrix m, CDMatrix dyad) {

		// calc output size
		int new_r, new_c;
		new_c = (int) Math.pow(2, Math.ceil(Math.log(m.NCols()) / Math.log(2)));
		new_r = (int) Math.pow(2, Math.ceil(Math.log(m.NRows()) / Math.log(2)));
		dyad.Resize(new_r, new_c);
		dyad.assign(.0);

		// copy data
		for (int r = 0; r < m.NRows(); r++) {

			for (int c = 0; c < m.NCols(); c++) {

				dyad.m_data[r][c] = m.m_data[r][c];
			}
		}
	}

	/**
	 * Wrapper to calculate element variables.
	 * 
	 * @param vVectors
	 *            Input set of vectors.
	 * @param varVec
	 *            A vector containing the variance of each component in
	 *            cVectors.
	 */
	public static void CalcElementVar(final Vector<CDVector> vVectors,
			CDVector varVec) {

		CalcElementVar(vVectors, varVec, null);

	}

	/**
	 * Calculates the variance of each component in a set of vectors.
	 * 
	 * @param cVectors
	 *            Input set of vectors.
	 * @param varVec
	 *            A vector containing the variance of each component in
	 *            cVectors.
	 * @param vpMean
	 *            Optional vector pointer to return the mean vector in.
	 */
	public static void CalcElementVar(final Vector<CDVector> vVectors,
			CDVector varVec, CDVector vpMean) {

		assert (vVectors.size() > 0);
		if (vpMean != null) {

			vpMean.Resize(vVectors.get(0).Length());
		}

		CDVector elemVec = new CDVector(vVectors.size());
		varVec.Resize(vVectors.get(0).Length());

		for (int elem = 0; elem < vVectors.get(0).Length(); elem++) {

			for (int vecNb = 0; vecNb < vVectors.size(); vecNb++) {

				elemVec.m_data[vecNb] = vVectors.get(vecNb).m_data[elem];
			}
			double[] mean = new double[1];
			varVec.m_data[elem] = elemVec.Var(mean);
			if (vpMean != null) {

				vpMean.m_data[elem] = mean[0];
			}
		}
	}

	/**
	 * Maps a vector linearly from [min;max] to [new_min;new_max].
	 * 
	 * @param v
	 *            Input vector.
	 * @param new_min
	 *            Desired minimum.
	 * @param new_max
	 *            Desired maximum.
	 * 
	 */
	public static void LinearStretchMinMax(CDVector v, final double new_min,
			final double new_max) {

		double min = v.Min();
		double max = v.Max();

		if (max == min) {

			// the vector is constant -> do nothing
			return;
		}

		// map [min;max] linear to [new_min;new_max]
		double mul = (new_max - new_min) / (max - min);
		for (int i = 0; i < v.Length(); i++) {

			v.m_data[i] = mul * (v.m_data[i] - min) + new_min;
		}
	}

	/**
	 * Maps a vector linearly from [x1;x2] to [new_min;new_max]. Values outside
	 * [x1;x2] are clamped to x1, x2 respectively.
	 * 
	 * @param v
	 *            Input vector.
	 * @param x1
	 *            Starting point of stretch.
	 * @param x2
	 *            Ending point of stretch.
	 * @param new_min
	 *            Desired minimum.
	 * @param new_max
	 *            Desired maximum.
	 */
	public void LinearStretchClamp(CDVector v, final double x1,
			final double x2, final double new_min, final double new_max) {

		assert (x1 != x2);

		if (x1 == x1) {

			// constant vector is reqested -> do nothing
			return;
		}

		// map the vector linearly from [x1;x2] to [new_min;new_max]
		double mul = (new_max - new_min) / (x2 - x1);
		double val;
		for (int i = 0; i < v.Length(); i++) {

			val = v.m_data[i];
			v.m_data[i] = val > x2 ? new_max : val < x1 ? new_min : (val - x1)
					* mul + new_min;
		}
	}

	/**
	 * Performs a 3x3 mean filtering of a matrix.
	 * 
	 * @param in
	 *            Input matrix.
	 * @param out
	 *            Output mean filtered matrix.
	 */
	public void MeanFilter(final CDMatrix in, CDMatrix out) {

		int r, c;
		out.Resize(in.NRows(), in.NCols());
		out.assign(.0);

		// top
		for (c = 1; c < out.NCols() - 1; c++) {

			r = 0;
			out.m_data[r][c] += in.m_data[r][c - 1];
			out.m_data[r][c] += in.m_data[r][c];
			out.m_data[r][c] += in.m_data[r][c + 1];
			out.m_data[r][c] += in.m_data[r + 1][c - 1];
			out.m_data[r][c] += in.m_data[r + 1][c];
			out.m_data[r][c] += in.m_data[r + 1][c + 1];
			out.m_data[r][c] /= 6;
		}
		// bottom
		for (c = 1; c < out.NCols() - 1; c++) {

			r = out.NRows() - 1;
			out.m_data[r][c] += in.m_data[r - 1][c - 1];
			out.m_data[r][c] += in.m_data[r - 1][c];
			out.m_data[r][c] += in.m_data[r - 1][c + 1];
			out.m_data[r][c] += in.m_data[r][c - 1];
			out.m_data[r][c] += in.m_data[r][c];
			out.m_data[r][c] += in.m_data[r][c + 1];
			out.m_data[r][c] /= 6;
		}
		// left
		for (r = 1; r < out.NRows() - 1; r++) {

			c = 0;
			out.m_data[r][c] += in.m_data[r - 1][c];
			out.m_data[r][c] += in.m_data[r - 1][c + 1];
			out.m_data[r][c] += in.m_data[r][c];
			out.m_data[r][c] += in.m_data[r][c + 1];
			out.m_data[r][c] += in.m_data[r + 1][c];
			out.m_data[r][c] += in.m_data[r + 1][c + 1];
			out.m_data[r][c] /= 6;
		}
		// right
		for (r = 1; r < out.NRows() - 1; r++) {

			c = out.NCols() - 1;
			out.m_data[r][c] += in.m_data[r - 1][c - 1];
			out.m_data[r][c] += in.m_data[r - 1][c];
			out.m_data[r][c] += in.m_data[r][c - 1];
			out.m_data[r][c] += in.m_data[r][c];
			out.m_data[r][c] += in.m_data[r + 1][c - 1];
			out.m_data[r][c] += in.m_data[r + 1][c];
			out.m_data[r][c] /= 6;
		}
		// rest
		for (r = 1; r < out.NRows() - 1; r++) {
			for (c = 1; c < out.NCols() - 1; c++) {

				out.m_data[r][c] += in.m_data[r - 1][c - 1];
				out.m_data[r][c] += in.m_data[r - 1][c];
				out.m_data[r][c] += in.m_data[r - 1][c + 1];
				out.m_data[r][c] += in.m_data[r][c - 1];
				out.m_data[r][c] += in.m_data[r][c];
				out.m_data[r][c] += in.m_data[r][c + 1];
				out.m_data[r][c] += in.m_data[r + 1][c - 1];
				out.m_data[r][c] += in.m_data[r + 1][c];
				out.m_data[r][c] += in.m_data[r + 1][c + 1];
				out.m_data[r][c] /= 9;
			}
		}
	}

	/**
	 * Normalises a vector to zero mean and unit length.
	 * 
	 * @param v
	 *            Input vector.
	 * @return Nothing. The result is returned in v.
	 */
	public static void ZeroMeanUnitLength(CDVector v) {

		double sqsum = .0, mean = v.Mean();
		int n = v.Length();

		for (int i = 0; i < n; i++) {

			v.m_data[i] -= mean;
			sqsum += v.m_data[i] * v.m_data[i];
		}
		double a = 1. / Math.sqrt(sqsum);

		for (int i = 0; i < n; i++) {

			v.m_data[i] *= a;
		}
	}

	/**
	 * Calculates the pseudo inverse of a matrix.
	 * 
	 * @param A
	 *            Input matrix.
	 * @param P
	 *            Pseudo inverse of A.
	 * @return Nothing.
	 */
	public void PseudoInv(final CDMatrix A, CDMatrix P) {

		int m = A.NRows();
		int n = A.NCols();
		CDMatrix U = new CDMatrix(m, n);
		CDMatrix V = new CDMatrix(n, n);
		CDMatrix SI = new CDMatrix();
		CDVector s = new CDVector(n);

		// VisDMatrixSVD( A, s, U, V, 1, 1 );
		SingularValueDecomposition svd = new SingularValueDecomposition(A);
		s = (CDVector) svd.getS();
		U = (CDMatrix) svd.getU();
		V = (CDMatrix) svd.getV();
		// construct SI
		SI.Resize(n, n);
		SI.assign(.0);
		double si, eps = 1e-16;

		// a tolerance setting that fell down from matlab heaven
		double tolerance = Math.max(m, n) * s.Max() * eps;

		for (int i = 0; i < n; i++) {

			si = s.m_data[i];
			SI.m_data[i][i] = si > tolerance ? 1. / si : .0;
		}

		// calc pinv(A)
		//
		// beware: potentially very large temporary objects ahead(!)
		// any zero-multiplications below could also be avoided
		//
		P.assign(V.mult(SI).mult(U.Transposed()));
	}

}