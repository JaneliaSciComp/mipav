package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;
import gov.nih.mipav.model.structures.*;

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
 * Shape collection container and shape-aligner. This class act as a container
 * for a set of shapes. Secondary it can align the set of shapes to a normalised
 * reference frame with respect to position, scale and orientation.
 * 
 * @author Ruida Cheng
 */
public class CAAMShapeCollection extends Vector<CAAMShape> {

	/**
	 * The meanshape prior to tangent space projection (calculated during
	 * alignment).
	 */
	private CAAMShape m_MeanShapeBeforeTS = new CAAMShape();

	/** The average shape size (calculated during alignment). */
	private double m_dAvgSize;

	/** Path to the .asf files (if any). */
	private String m_szPath = new String();

	/** The number of shapes in the collection. */
	public int NShapes() {
		return size();
	}

	/** The number of points in each of shape. */
	public int NPoints() {
		return NShapes() > 0 ? this.get(0).NPoints() : 0;
	}

	/** Returns the path of all shapes. */
	public String Path() {
		return m_szPath;
	}

	/** Returns the average shape size *before* the aligment process. */
	public double MeanSize() {
		return m_dAvgSize;
	}

	/**
	 * Constructor.
	 */
	public CAAMShapeCollection() {
		super();
	}

	/**
	 * Assignment operator
	 * 
	 * @param shapeCollection
	 *            shape collection reference
	 */
	public void assign(CAAMShapeCollection shapeCollection) {

		int len = shapeCollection.size();
		// this = (CAAMShapeCollection)shapeCollection.clone();
		this.clear();
		for (int i = 0; i < len; i++) {
			CAAMShape orig = shapeCollection.get(i);
			CAAMShape newObject = new CAAMShape();
			newObject.assign(orig);
			this.add(i, newObject);
		}

	}

	/**
	 * dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Inserts a shape into the collection.
	 * 
	 * @param s
	 *            The input shape.
	 */
	public void Insert(final CAAMShape s) {
		add(s);
	}

	/**
	 * Writes the shapes in a (NShapes x 2*NPoints) matrix in Matlab (*.m)
	 * format. The i-th row thus contains the i-th shape in xxxyy format.
	 * 
	 * @param sFilename
	 *            Filename including path to be written.
	 * @param sName
	 *            The matlab variable name of the matrix.
	 * @param sComment
	 *            An optional comment.
	 * @param fAppend
	 *            Addend to an existing file or overwrite.
	 * @return Zero on sucess, non-zero if no shapes are stored in the
	 *         collection.
	 */
	public int ToMatlab(final String sFilename, final String sName,
			final String sComment, boolean fAppend) {
		assert (NShapes() > 0);

		if (NShapes() == 0)
			return -1;

		CDMatrix m = new CDMatrix(NShapes(), 2 * NPoints());

		for (int i = 0; i < NShapes(); i++) {

			m.SetRow(i, this.get(i));
		}
		m.ToMatlab(sFilename, sName, sComment, fAppend);

		return 0;
	}

	/**
	 * Alight shapes
	 * 
	 * @return success or not.
	 */
	public int AlignShapes() {
		boolean use_tangentspace = true;
		return AlignShapes(use_tangentspace);
	}

	/**
	 * Normalizes all shapes with respect to position, scale and orientation.
	 * Position normalization are done by a translation of the center of gravity
	 * to orig. Scale normalization are done by a scaling of 1/<norm2>. Rotation
	 * normalization are done by minimizing the sum of squared point distances,
	 * as described by all using Singular Value Decomposition (SVD).
	 * 
	 * @return Zero on success.
	 */
	public int AlignShapes(boolean use_tangentspace) {

		// check if we have any shapes at all
		if (NShapes() < 1)
			return 0;

		// calculate the average shape size according to the 2-norm
		m_dAvgSize = .0;
		for (int i = 0; i < NShapes(); i++) {

			m_dAvgSize += this.get(i).Normalize(); // move to origo and scale to
													// unit size
		}
		m_dAvgSize /= NShapes();

		// the initial estimate of the mean shape is
		// set to the first shape
		CAAMShape mean_est = new CAAMShape();
		mean_est.assign(this.get(0));

		// setup
		boolean verbose = false;
		boolean forceMeanOrientation = true;

		// do a number number of alignment iterations
		// until the mean shape estimate is stable
		double diff, diff_max = 0.001; // diff must be less than 0.1%
		int max_iter = 30;
		CAAMShape mean_est_old = new CAAMShape();
		int iter = 1;
		double[] theta = new double[1];
		do {

			// normalize and align all other shapes to the mean shape estimate
			CDVector rot = new CDVector(NShapes());
			for (int i = 0; i < NShapes(); i++) {

				// align the i-th shape to the estimate of the mean shape
				this.get(i).AlignTo(mean_est, theta);
				rot.m_data[i] = theta[0]; // record the rotation

				// re-scale to unit size to avoid the so-called 'shrinking
				// effect'
				// i.e. that that the alignment error goes towards zero, when
				// the shapes are downscaled
				this.get(i).Scale(1.0 / this.get(i).Norm2());
			}
			mean_est_old.assign(mean_est);

			// estimate the new mean shape
			MeanShape(mean_est);

			// if this is the first iteration, correct the
			// orientation of the mean shape, so that
			// rotation of the training set to fit the mean
			// shape is -- on average -- zero
			//
			// or put more clearly:
			//
			// "make the meanshape have a mean orientation"
			//
			if (forceMeanOrientation && iter == 1) {

				mean_est.Rotate(-rot.Mean());
			}

			diff = (mean_est_old.sub(mean_est)).Norm2();

			if (verbose) {
				System.err.println("Alignment iteration #" + iter
						+ ", mean shape est. diff. = " + diff);
			}

			++iter;

		} while (Math.abs(diff) / mean_est.Norm2() > diff_max
				&& iter < max_iter);

		// save the mean shape before tangent space projection
		m_MeanShapeBeforeTS.assign(mean_est);

		// project into tangent space to avoid non-linearity in point movements
		if (use_tangentspace) {

			// CDVector ts = new CDVector(1);

			double ts;

			double scale;

			CDMatrix m_MeanShapeMatTransposed = new CDMatrix(1,
					m_MeanShapeBeforeTS.Length());
			m_MeanShapeMatTransposed.SetRow(0, m_MeanShapeBeforeTS);

			for (int i = 0; i < NShapes(); i++) {

				// ts.assign((CDVector)m_MeanShapeMatTransposed.mult(this.get(i)));
				ts = m_MeanShapeMatTransposed.mult(this.get(i)).get(0);
				scale = 1. / ts;
				this.get(i).Scale(scale);
			}
		}

		// Success
		return 0;
	}

	/**
	 * Calcs the mean shape of all shapes.
	 * 
	 * @param meanShape
	 *            The output mean shape.
	 */
	public void MeanShape(CAAMShape meanShape) {

		if (meanShape.NPoints() != NPoints()) {
			meanShape.Resize(2 * NPoints());
		}

		// copy point aux data
		meanShape.assign(this.get(0));

		meanShape.assign(0.0);
		for (int i = 0; i < NShapes(); i++) {

			meanShape.add_into(this.get(i));
		}
		meanShape.div_into(NShapes());
	}

	/**
	 * Calcs the mean shape of all aligned shapes and size it to mean size.
	 * 
	 * @param refShape
	 *            The output reference shape.
	 */
	public void ReferenceShape(CAAMShape refShape) {

		MeanShape(refShape);
		refShape.Scale(this.MeanSize(), true);
	}

	/**
	 * Find the minimum x component of all shapes.
	 * 
	 * @return The x-minimum.
	 */
	public double MinX() {

		double val, min = 1.7E+308;

		for (int i = 0; i < NShapes(); i++) {

			val = this.get(i).MinX();
			min = val < min ? val : min;
		}
		return min;
	}

	/**
	 * Find the maximum x component of all shapes.
	 * 
	 * @return The x-maximum.
	 */
	public double MaxX() {

		double val, max = -1.7E+308;

		for (int i = 0; i < NShapes(); i++) {

			val = this.get(i).MaxX();
			max = val > max ? val : max;
		}
		return max;
	}

	/**
	 * Find the minimum x component of all shapes.
	 * 
	 * @return The x-minimum.
	 */
	public double MinY() {

		double val, min = 1.7E+308;

		for (int i = 0; i < NShapes(); i++) {

			val = this.get(i).MinY();
			min = val < min ? val : min;
		}
		return min;
	}

	/**
	 * Find the maximum y component of all shapes.
	 * 
	 * @return The y-maximum.
	 */
	public double MaxY() {

		double val, max = -1.7E+308;

		for (int i = 0; i < NShapes(); i++) {

			val = this.get(i).MaxY();
			max = val > max ? val : max;
		}
		return max;
	}

	/**
	 * Wrapper to scale all shape
	 * 
	 * @param s
	 *            scale factor
	 */
	public void Scale(final double s) {
		boolean aroundCOG = false;
		Scale(s, aroundCOG);
	}

	/**
	 * Scale the shapes.
	 * 
	 * @param s
	 *            Scale factor.
	 * @param aroundCOG
	 *            If true the scale is being done around the cog of the shape
	 *            instead of around the global center.
	 */
	public void Scale(final double s, final boolean aroundCOG) {

		for (int i = 0; i < NShapes(); i++) {

			this.get(i).Scale(s, aroundCOG);
		}
	}

	/**
	 * Expands all shapes (contraction can be done by using a negative nPixels).
	 * xpands all shapes by moving each model point 'nPixels' perpendicular to
	 * the shape contour (that is: along the model point normal).
	 * 
	 * This function will expand each outer (closed) path of the shape.
	 * 
	 * No tests for crossing contours are being made as of now.
	 * 
	 * @param nPixels
	 *            The number of pixel to expand the shape with.
	 */
	public void Expand(int nPixels) {

		for (int i = 0; i < NShapes(); i++) {

			this.get(i).Expand(nPixels);
		}
	}

	/**
	 * Wrapper to read shapes from asf files
	 * 
	 * @param asfFiles
	 *            asf file vector
	 * @return success or not
	 */
	public boolean ReadShapes(final Vector<String> asfFiles) {
		return ReadShapes(asfFiles, false);
	}

	/**
	 * Wrapper to read shape from given image and VOIs vector
	 * 
	 * @param modelImageVector
	 *            image and voi vector
	 * @return success or not.
	 */
	public boolean ReadImages(final Vector<ModelImage> modelImageVector) {
		return ReadImages(modelImageVector, false);
	}

	/**
	 * Reads a set of shapes in the order given in the vector of strings.
	 * 
	 * @param asfFiles
	 *            Vector of asf filenames.
	 * @param validate
	 *            Validates that all shapes have the same number of points.
	 * @return True on a valid training set - otherwise false.
	 */
	public boolean ReadShapes(final Vector<String> asfFiles, boolean validate) {

		int npoints = -1;
		boolean valid = true;

		for (int i = 0; i < asfFiles.size(); i++) {

			CAAMShape shape = new CAAMShape();
			String fn = asfFiles.get(i);
			shape.ReadASF(fn);

			if (i == 0) {

				npoints = shape.NPoints();
			}

			if (valid) {

				// check nb points
				if (shape.NPoints() != npoints) {

					valid = false;
					System.err
							.printf("Error: Shape number %i has %i points. Training point size: %i.\n",
									i, shape.NPoints(), npoints);
				}
			}

			Insert(shape);
		}

		// set the path member
		if (asfFiles.size() > 0) {

			m_szPath = CAAMUtil.GetPath(asfFiles.get(0));
		}

		return valid;
	}

	/**
	 * Reads a set of shapes in the order given in the vector of strings.
	 * 
	 * @param asfFiles
	 *            Vector of asf filenames.
	 * @param validate
	 *            Validates that all shapes have the same number of points.
	 * @return True on a valid training set - otherwise false.
	 */
	public boolean ReadImages(final Vector<ModelImage> modelImageVector,
			boolean validate) {

		int npoints = -1;
		boolean valid = true;

		int size = modelImageVector.size();

		for (int i = 0; i < size; i++) {

			CAAMShape shape = new CAAMShape();

			shape.ReadASFfromVOI(modelImageVector.get(i));

			if (i == 0) {

				npoints = shape.NPoints();
			}

			if (valid) {

				// check nb points
				if (shape.NPoints() != npoints) {

					valid = false;
					System.err.printf("Error: Shape number " + i + " has "
							+ shape.NPoints()
							+ " points. Training point size: " + npoints);
				}
			}

			Insert(shape);
		}

		return valid;
	}

	/**
	 * Convert shape from relative coordinates to abs coordinates
	 */
	public void Rel2Abs() {
		Rel2Abs(1);
	}

	/**
	 * Converts all shapes with relative coordinates to absolute. Unfortunately
	 * this requires to read the headers of all host images. VisSDK does not
	 * provide any operation for this. Thus, all images are one by one read into
	 * memory and discarded again to obtain height and width. Very costly :-(
	 * 
	 * @param rfactor
	 *            Optional reduction factor. Performs a scaling of the the shape
	 *            by 1/rfactor. Default 1 i.e. no scaling.
	 */
	public void Rel2Abs(int rfactor) {

		for (int i = 0; i < this.size(); i++) {

			if (this.get(i).IsAbs() == false) {

				// convert to absolute coordinates
				this.get(i).Rel2Abs(m_szPath);

				if (rfactor != 1) {

					this.get(i).Scale(1.0 / (double) rfactor);
				}
			}
		}
	}

}