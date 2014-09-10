package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
import java.util.*;
import java.text.*;

import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.*;

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
 * The core Active Appearance Model object that hold all eigenmodels, prediction
 * matrices etc. Build by a CAAMBuilder.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMModel extends CAAMObject {

	/** AAM synthesize analysis. */
	protected CAAMAnalyzeSynthesize m_pAnalyzeSynthesize;

	/** AAM reference frame. */
	protected CAAMReferenceFrame m_pReferenceFrame;

	/** The shape basis. */
	protected CAAMDeform m_pShapeBasis;

	/** The texture basis. */
	protected CAAMDeform m_pTextureBasis;

	/** The shape PCA. */
	protected CAAMDeformPCA m_ShapePCA = new CAAMDeformPCA();

	/** The texture PCA. */
	protected CAAMDeformPCA m_TexturePCA = new CAAMDeformPCA();

	/** The combined PCA. */
	protected CAAMDeformPCA m_CombinedPCA = new CAAMDeformPCA();

	/** The shape-to-pixel weights. */
	protected CDMatrix m_mShape2PixelWeights = new CDMatrix();

	/** The AMF format version. */
	protected double m_dAMFVersion;

	/** The shape part of the combined eigenvectors. */
	protected CDMatrix m_mQsEV = new CDMatrix();

	/** The texture part of the combined eigenvectors. */
	protected CDMatrix m_mQgEV = new CDMatrix();

	/** The mean texture. */
	protected CDVector m_vMeanTexture = new CDVector();

	/** the mean texture original. */
	protected CDVector m_vMeanTextureOrg = new CDVector();

	/** Cached mean shape. */
	protected CAAMShape m_sMeanAShape = new CAAMShape();

	/** Cache shape matrix. */
	protected CDMatrix m_mShapeInstance = new CDMatrix();

	/** Cache texture matrix. */
	protected CDMatrix m_mTextureInstance = new CDMatrix();

	/** The variance of each normalized pixel in the texture model. */
	protected CDVector m_vTextureVar = new CDVector();

	/** The number of texture samples in the model. */
	protected int m_iTextureSamples;

	/** the number of shapes. */
	protected int m_iNShapes;

	/** mean shape size. */
	protected double m_dMeanShapeSize;

	/** The parameter prediction matrix */
	protected CDMatrix m_R_c = new CDMatrix();
	protected CDMatrix m_R_t = new CDMatrix();

	/** The texture transfer function. */
	protected CAAMTransferFunction m_pTextureTF;

	/** flag for model reduction. */
	protected int m_iModelReduction;

	/** flag to add extents. */
	protected double m_dAddExtents;

	/** flag to use convexhull. */
	protected boolean m_bUseConvexHull;

	/** flag to use targent space. */
	protected boolean m_bUseTangentSpace;

	/** which learning method to use. */
	protected int m_iLearningMethod;

	/** shape truncation. */
	protected int m_iShapeTrunc;

	/** texture truncation. */
	protected int m_iTextureTrunc;

	/** combined truncation. */
	protected int m_iCombinedTrunc;

	/** Current analyzer name. */
	protected String m_sCurrentAnalyzeId = new String();

	/** model build time. */
	protected double m_dBuildTime;

	/** pose parameters update. */
	protected CDVector m_vPoseParameterUpdateConstraints = new CDVector();

	/** shape parameters upate. */
	protected CDVector m_vShapeParameterConstraintsMean = new CDVector();
	protected CDVector m_vShapeParameterConstraintsSD = new CDVector();

	/** deform displacement PCA. */
	protected CAAMDeformPCA m_DisplacementPCA = new CAAMDeformPCA();

	/**
	 * Texture parameter update prediction matrix.
	 * 
	 * @return texture prediction matrix
	 */
	public CDMatrix Rc() {
		return m_R_c;
	}

	/**
	 * Pose parameter update prediction matrix.
	 * 
	 * @return pose prediction matrix
	 */
	public CDMatrix Rt() {
		return m_R_t;
	}

	/**
	 * The texture part of the combined PCA eigenvectors.
	 * 
	 * @return combined texture PCA
	 */
	public CDMatrix Qg() {
		return m_mQgEV;
	}

	/**
	 * The shape part of the combined PCA eigenvectors.
	 * 
	 * @return combined shape PCA
	 */
	public CDMatrix Qs() {
		return m_mQsEV;
	}

	/**
	 * Returns the reference frame of the model.
	 * 
	 * @return refrence frame
	 */
	public CAAMReferenceFrame ReferenceFrame() {
		return m_pReferenceFrame;
	}

	/**
	 * Returns the number of bands in the model
	 * 
	 * @return single channel to represent the gray scal intensity.
	 */
	public int NBands() {
		return AAMdef.BANDS;
	}

	/**
	 * Return texture transfer function.
	 * 
	 * @return return texture transfer function
	 */
	public CAAMTransferFunction TextureTF() {
		return m_pTextureTF;
	}

	/**
	 * Returns the number of samples in the texture model.
	 * 
	 * @return sample numbers
	 */
	public int NTextureSamples() {
		return m_iTextureSamples;
	}

	/**
	 * Returns true if the texture model is based on the convex hull.
	 * 
	 * @return ture convex hull, false not
	 */
	public boolean IsConvexHullUsed() {
		return m_bUseConvexHull;
	}

	/**
	 * Returns the amount shape extents added (warning: shape extents will be
	 * remove in later versions).
	 * 
	 * @return shape extents
	 */
	public double AddExtents() {
		return m_dAddExtents;
	}

	/**
	 * Returns the mean shape.
	 * 
	 * @return mean shape
	 */
	public CAAMShape MeanShape() {
		return m_sMeanAShape;
	}

	/**
	 * Returns the mean texture.
	 * 
	 * @return mean texture
	 */
	public CDVector MeanTexture() {
		return m_vMeanTexture;
	}

	/**
	 * Return shape to pixel weight.
	 * 
	 * @return weight
	 */
	public CDMatrix Shape2PixelWeights() {
		return m_mShape2PixelWeights;
	};

	/**
	 * Returns the shape PCA.
	 * 
	 * @return shape PCA
	 */
	public final CAAMDeformPCA ShapePCA() {
		return m_ShapePCA;
	}

	/**
	 * Returns the texture PCA.
	 * 
	 * @return texture PCA
	 */
	public final CAAMDeformPCA TexturePCA() {
		return m_TexturePCA;
	}

	/**
	 * Returns the shape PCA.
	 * 
	 * @return shape PCA.
	 */
	public CAAMDeform ShapeBasis() {
		return m_pShapeBasis;
	}

	/**
	 * Returns the texture PCA.
	 * 
	 * @return texture PCA.
	 */
	public CAAMDeform TextureBasis() {
		return m_pTextureBasis;
	}

	/**
	 * Returns the combined PCA.
	 * 
	 * @return combined PCA.
	 */
	public CAAMDeformPCA CombinedPCA() {
		return m_CombinedPCA;
	}

	/**
	 * Return model reduction.
	 * 
	 * @return flag for model reduction.
	 */
	public int ModelReduction() {
		return m_iModelReduction;
	}

	/**
	 * Returns the mean shape size, i.e. the size of the reference shape.
	 * 
	 * @return mean shape size.
	 */
	public double MeanShapeSize() {
		return m_dMeanShapeSize;
	}

	/**
	 * Constructor. Set up the default settings.
	 */
	public CAAMModel() {

		// setup defaults
		m_iModelReduction = 1;
		m_dAddExtents = 0.0;
		m_bUseConvexHull = false;
		m_bUseTangentSpace = true;
		m_iLearningMethod = 1;
		m_iShapeTrunc = 95;
		m_iTextureTrunc = 95;
		m_iCombinedTrunc = 95;

		m_iTextureSamples = 0;
		m_iNShapes = 0;
		m_dMeanShapeSize = 0;
		m_dBuildTime = .0;

		// current amf format version
		m_dAMFVersion = .99;

		// setup texture transfer function
		// use the identify mapping as default for textures
		m_pTextureTF = new CAAMTFIdentity();

		m_pAnalyzeSynthesize = null;
		m_pReferenceFrame = null;
		m_sCurrentAnalyzeId = "__none__";
	}

	/**
	 * dispose memory
	 */
	public void dispose() {

		// deallocate
		if (m_pTextureTF != null) {
			m_pTextureTF = null;
		}

		if (m_pReferenceFrame != null) {
			m_pReferenceFrame = null;
		}

		if (m_pAnalyzeSynthesize != null) {
			m_pAnalyzeSynthesize = null;
		}
	}

	/**
	 * Doing model approximation of an (unseen) example. Synthesizes an unseen
	 * example by projecting the shape and (normalized) texture into c-parameter
	 * space, generating a model instance and assigning it the appropriate pose
	 * (incl. denormalization).
	 * 
	 * @param filename
	 *            The base filename of an annotation. Ex. "scan.asf"
	 * 
	 * @param outImg
	 *            The output image where the model approximation has been
	 *            overlaid.
	 * 
	 * @return true on success, false if the image and/or annotation could not
	 *         be read.
	 */
	public boolean ApproxExample(final String filename, ModelSimpleImage outImg) {

		CAAMShape shape = new CAAMShape();

		// load image and shape
		outImg = CAAMUtil.ReadExample(filename, outImg, shape,
				m_iModelReduction);

		// obtain the combined model parameters, c, and the normalisation
		CDVector c = new CDVector();
		Shape2Combined(shape, outImg, c);

		//
		// generate model instance (match pose to 'shape')
		// and render the instance into the image 'img'
		//
		ModelImage(c, outImg, shape);

		// everything went fine
		return true;
	}

	/**
	 * Sets constraints on the pose parameter updates.
	 * 
	 * NOTICE: Currently, these are *not* saved along with the model and thus
	 * must be set each time a model is loaded.
	 * 
	 * @param pc
	 *            Parameter constraints (in absolute numbers).
	 * @return Nothing
	 */
	public void SetPoseParameterUpdateConstraints(final CDVector pc) {
		m_vPoseParameterUpdateConstraints.assign(pc);
	}

	/**
	 * Sets user-specified shape parameter update constraints. NOTICE: These are
	 * currently not saved with the model.
	 * 
	 * @param mean
	 *            Some shape parameter configuration.
	 * @param sd
	 *            The standard diviations of the 'mean'.
	 * @return Nothing.
	 */
	public void SetShapeParameterUpdateConstraints(final CDVector mean,
			final CDVector sd) {

		m_vShapeParameterConstraintsMean.assign(mean);
		m_vShapeParameterConstraintsSD.assign(sd);
	}

	/**
	 * Constrain the pose and model parameters to be within some reasonable
	 * limits.
	 * 
	 * @param c
	 *            The model parameters.
	 * @param pose
	 *            The pose parameters.
	 * @return Nothing.
	 */
	public void ConstrainSearchParameters(CDVector c, CDVector pose) {

		if (m_vPoseParameterUpdateConstraints.Length() != 4) {

			// use old hard-wired constraints

			// scale
			double scale_limit = .1d;
			if (Math.abs(pose.get(0)) > scale_limit) {

				int sgn = (int) (pose.get(0) / Math.abs(pose.get(0)));
				pose.set(0, sgn * scale_limit);
			}

			// theta
			double theta = 5. * Math.PI / 180.;
			if (Math.abs(pose.get(1)) > theta) {

				int sgn = (int) (pose.get(1) / Math.abs(pose.get(1)));
				pose.set(1, sgn * theta);
			}

			// x
			double xlimit = this.ReferenceFrame().RefImageWidth() / 10.;
			if (Math.abs(pose.get(2)) > xlimit) {

				int sgn = (int) (pose.get(2) / Math.abs(pose.get(2)));
				pose.set(2, sgn * xlimit);
			}

			// y
			double ylimit = this.ReferenceFrame().RefImageHeight() / 10.;
			if (Math.abs(pose.get(3)) > ylimit) {

				int sgn = (int) (pose.get(3) / Math.abs(pose.get(3)));
				pose.set(3, sgn * ylimit);
			}
		} else {

			// use user-selectable constraints
			for (int i = 0; i < 4; i++) {

				if (Math.abs(pose.get(i)) > m_vPoseParameterUpdateConstraints
						.get(i)) {

					// clamp
					int sgn = (int) (pose.get(i) / Math.abs(pose.get(i)));
					pose.set(i, sgn * m_vPoseParameterUpdateConstraints.get(i));
				}
			}
		}

		if (m_vShapeParameterConstraintsMean.Length() == 0) {

			// model parameters
			int np = c.Length();
			double val;
			for (int i = 0; i < np; i++) {

				val = c.get(i);
				double limit = 3. * Math.sqrt(m_CombinedPCA.EigenValues()
						.get(i));

				if (Math.abs(val) > limit) {

					int sgn = (int) (val / Math.abs(val));
					c.set(i, sgn * limit);
				}
			}
		} else {

			// printf("  constraining shape");
			CDVector b_s = new CDVector();
			CDVector b_g = new CDVector();
			this.Combined2ShapeParam(c, b_s);
			this.Combined2TexParam(c, b_g);

			boolean constrained = false;
			for (int i = 0; i < b_s.Length(); i++) {

				double limit = 0. * m_vShapeParameterConstraintsSD.get(i);
				double val = b_s.get(i)
						- m_vShapeParameterConstraintsMean.get(i);

				if (Math.abs(val) > limit) {

					// printf(".");
					int sgn = (int) (val / Math.abs(val));
					b_s.set(i, m_vShapeParameterConstraintsMean.get(i) + sgn
							* limit);
					constrained = true;
				}
			}
			if (constrained) {

				// printf("c = %s\n", c.ToString() );
				this.ShapeTexParam2Combined(b_s, b_g, c);
				// printf("c = %s\n", c.ToString() );
			}

			// printf("\n");
		}
	}

	/**
	 * Estimate the pose of a shape using the pose regression matrix.
	 * 
	 * @param image
	 *            The image to search in.
	 * @param shape
	 *            The shape to determine the pose from.
	 * @param pose
	 *            The output pose vector.
	 * @return True is ok, false if the shape is outside the image.
	 */
	public boolean EstimatePose(final ModelSimpleImage image,
			final CAAMShape shape, CDVector pose) {

		CDVector c = new CDVector();
		CDVector g_s = new CDVector();
		CDVector g_m = new CDVector();

		// make image instance
		int nSamples = SampleShape(image, shape, g_s);
		if (nSamples == 0)
			return false;

		// align shape
		CAAMShape alignedShape = new CAAMShape(shape);
		alignedShape.AlignTo(m_sMeanAShape);

		// make model param
		ShapeTex2Combined(alignedShape, g_s, c);

		// make model instance
		TextureInstance(c, g_m);

		// form difference
		g_s.sub_into(g_m);

		// estimate pose
		pose.assign(m_R_t.neg().mult(g_s));

		return true;
	}

	/**
	 * Generate model instance (match pose to 'shape') and render the instance
	 * into the image 'img'.
	 * 
	 * @param c
	 *            combined model parameters
	 * @param outImg
	 *            result image
	 * @param matchPose
	 *            matched shape
	 */
	public void ModelImage(final CDVector c, ModelSimpleImage outImg,
			final CAAMShape matchPose) {
		ModelImage(c, outImg, matchPose, true);
	}

	/**
	 * Generates a model image based on the parameters in 'c' with various
	 * options.
	 * 
	 * @param c
	 *            A set of model parameters.
	 * @param outImg
	 *            The output imge.
	 * @param matchPose
	 *            A pointer to a shape. If not NULL the model generated by the
	 *            set of c-parameters will be aligned wrt. pose to this shape.
	 * @param fitTexture
	 *            If matchPose is not NULL, the de-mapped and de-normalized
	 *            texture will be fitted in a least squares sense to the texture
	 *            in outImg given by the shape. Default true.
	 */
	public void ModelImage(final CDVector c, ModelSimpleImage outImg,
			final CAAMShape matchPose, final boolean fitTexture) {

		// generate the texture instance from the model
		CDVector texture = new CDVector();
		TextureInstance(c, texture);

		// generate the shape instance from the model
		CAAMShape shape = new CAAMShape();

		ShapeInstance(c, shape);

		if (matchPose != null) {

			shape.AlignTo(matchPose);
		} else {

			shape.Scale(m_dMeanShapeSize);
		}

		// generate the model image
		ModelImageEx(shape, texture, outImg, matchPose != null, fitTexture);
	}

	/**
	 * Generates a synthetic image from the AAM using the model parameters in
	 * 'c'.
	 * 
	 * @param shape
	 *            The shape the texture vector should be mapped to.
	 * @param textureSamples
	 *            The texture vector to be warped into an image. Will be
	 *            de-mapped and de-normalized.
	 * @param outImg
	 *            The output image (sized correctly inside this method, if
	 *            renderInImage is false).
	 * @param renderImage
	 *            If true:
	 * 
	 *            1) outImg is expected to be allocated.
	 * 
	 *            2) the model is rendered with it's pose unchanged, thus the
	 *            shape is expected to lie within the outImg
	 * 
	 *            This option is used for drawing the final optimization into
	 *            the image or to draw a model approximation into the image
	 *            'outImg'.
	 * 
	 *            Default false.
	 * @param fitTexture
	 *            If renderImage is true, the de-mapped and de-normalized
	 *            texture will be fitted in a least squares sense to the texture
	 *            in outImg given by the shape. Default true.
	 */
	public void ModelImageEx(final CAAMShape shape, final CDVector texture,
			ModelSimpleImage outImg, final boolean renderImage,
			final boolean fitTexture) {

		CDVector tex = new CDVector(texture);
		TextureTF().DeMap(tex);

		// fit texture to image (if requested)
		if (renderImage && fitTexture
				&& CAAMUtil.ShapeInsideImage(shape, outImg)) {

			// sample image texture
			CDVector imgTexture = new CDVector();
			this.SampleShape(outImg, shape, imgTexture, false, true, false);

			// least squares fit to image texture
			tex.AlignTo(imgTexture);

			// clamp
			tex.Clamp(0., 255.);

		} else {

			// we don't know anything -> use full byte range
			CAAMMathUtil.LinearStretchMinMax(tex, 0, 255);
		}

		// render the final texture into outImg
		m_pAnalyzeSynthesize.Synthesize(shape, tex, outImg, renderImage);
	}

	/**
	 * Wrapper to generates a shape free image using a vector of texture
	 * samples.
	 * 
	 * @param textureSamples
	 *            texture samples
	 * @param outImg
	 *            result image
	 * @return shape free image
	 */
	public ModelSimpleImage ShapeFreeImage(final CDVector textureSamples,
			ModelSimpleImage outImg) {
		return ShapeFreeImage(textureSamples, outImg, true);
	}

	/**
	 * Generates a shape free image (that is; a mean shape image) using a vector
	 * of texture samples.
	 * 
	 * @param textureSamples
	 *            The texture vector to be warped into an image. De-normalizes
	 *            (and demaps) inside this method.
	 * @param outImg
	 *            A reference to an output image. Resize of the image is done
	 *            inside this method.
	 * @return shape free image.
	 */
	public ModelSimpleImage ShapeFreeImage(final CDVector textureSamples,
			ModelSimpleImage outImg, final boolean deMap) {

		// demap
		CDVector texture = new CDVector(textureSamples);

		if (deMap) {

			TextureTF().DeMap(texture);
		}

		CAAMMathUtil.LinearStretchMinMax(texture, 0, 255);

		// convert to shape-free image
		return m_pReferenceFrame.Vector2Image(texture, outImg);
	}

	/**
	 * Generates a shape free image (that is; a mean shape image) using a vector
	 * of texture samples.
	 * 
	 * @param textureSamples
	 *            The texture vector to be warped into an image. De-normalizes
	 *            (and demaps) inside this method.
	 * @param m
	 *            A reference to an output image on matrix form. Resize of the
	 *            matrix is done inside this method.
	 * @param deMap
	 */
	public void ShapeFreeImage(final CDVector textureSamples, CDMatrix m,
			final boolean deMap, final boolean normalize) {

		// demap
		CDVector texture = new CDVector(textureSamples);

		if (deMap) {

			TextureTF().DeMap(texture);
		}

		if (normalize) {

			CAAMMathUtil.LinearStretchMinMax(texture, 0, 255);
		}

		// convert to shape-free image
		m_pReferenceFrame.Vector2Matrix(texture, m);
	}

	/**
	 * Wrapper to calculate the pixel difference from a model instance and an
	 * image.
	 * 
	 * @param image
	 *            model image
	 * @param c
	 *            parameter vector
	 * @param estimate
	 *            estimaed shape
	 * @param diff
	 *            difference vector
	 * @param similaritym
	 *            similarity measure type
	 * @return similarity measure
	 */
	public double ModelEstimateTexDiff(final ModelSimpleImage image,
			final CDVector c, CAAMShape estimate, CDVector diff,
			final int similaritym) {
		return ModelEstimateTexDiff(image, c, estimate, diff, similaritym, true);
	}

	/**
	 * Calculates the pixel difference from a model instance and an image.
	 * 
	 * @param image
	 *            The image (input)
	 * @param c
	 *            Model parameters (in/output).
	 * @param estimate
	 *            The shape estimate (in/output).
	 * @param diff
	 *            The pixel difference vector (output) Resized inside.
	 * @param similaritym
	 *            Set the used similarity measure:
	 * 
	 *            0 Non-normalised L_2 norm (default)
	 * 
	 *            1 The "Mahalanobis" distance (texture samples are regarded
	 *            independent to increase performance).
	 * 
	 *            2 The Lorentzian error norm.
	 * 
	 *            3 Absolute auto correlation of the residuals.
	 * @return The similarity measure.
	 */
	public double ModelEstimateTexDiff(final ModelSimpleImage image,
			final CDVector c, CAAMShape estimate, CDVector diff,
			final int similaritym, final boolean useInterpolation) {

		CDVector g_m = new CDVector();
		CDVector g_s = diff; // a bit confusing, but this saves
								// an extra texture vector
		CAAMShape model_shape = new CAAMShape();

		// generate model texture
		TextureInstance(c, g_m);

		// generate model shape
		ShapeInstance(c, model_shape);

		// Align X_shape to the annotated shape
		model_shape.AlignTo(estimate);

		int nSamples = SampleShape(image, model_shape, g_s, true,
				useInterpolation);
		// remember g_s = diff

		if (nSamples == 0) {

			// we're outside
			return -1.;
		}

		// update estimate
		estimate.assign(model_shape);

		// calc pixel difference: g_s - g_m (remember g_s = diff)
		diff.sub_into(g_m);

		// obtain the number of texture samples
		int n = g_m.Length();

		// calc the similarity between the two texture vectors
		int i;
		double sm, tmpval;
		CDVector tmp = new CDVector();
		switch (similaritym) {

		case 0: // unnormalized L_2 norm
			tmpval = diff.Norm2();
			sm = tmpval * tmpval;
			break;

		case 1: // independent "Mahalanobis" distance
			tmp.Resize(n);
			for (i = 0; i < n; i++) {

				tmp.set(i, diff.get(i) * diff.get(i) / m_vTextureVar.get(i));
			}
			sm = tmp.Sum() / (double) n;
			break;

		case 2: // lorentzian norm
			tmp.Resize(n);
			for (i = 0; i < n; i++) {

				tmp.set(i,
						Math.log(1.0 + diff.get(i) * diff.get(i)
								/ (2.0 * Math.sqrt(m_vTextureVar.get(i)))));
			}
			sm = tmp.Sum() / (double) n;
			break;

		case 3: // absolute auto correlation of the residuals
		{
			double mean = diff.Mean();
			double cov = .0;
			for (i = 0; i < n - 1; i++) {

				cov += (diff.get(i) - mean) * (diff.get(i + 1) - mean);
			}
			cov /= n - 1;
			sm = Math.abs(cov / diff.Var());
		}
			break;

		case 4: // mutual information
			sm = CAAMMathUtil.MutualInformation(g_m, g_m, 64)
					- CAAMMathUtil.MutualInformation(g_s, g_m, 64);
			break;

		default:
			System.err
					.println("ModelEstimateTexDiff: Unknown similarity measure, "
							+ similaritym);
			sm = .0d;
		}

		// return the used similarity measure
		return sm;
	}

	/**
	 * Normalizes a texture vector.
	 * 
	 * @param texture
	 *            Texture to be normalized.
	 */
	public void NormalizeTexture(CDVector texture) {

		boolean remove_mean_only;
		remove_mean_only = false;
		if (remove_mean_only) {

			double mean = texture.Mean();
			for (int i = 0; i < texture.Length(); i++) {

				texture.set(i, texture.get(i) - mean);
			}
			return;
		} else {

			CAAMMathUtil.ZeroMeanUnitLength(texture);
			double dp = texture.mult(m_vMeanTextureOrg);

			if (dp != .0) {

				texture.mult_into(1. / dp);
			}
			// if ( _DEBUG )
			if (dp == .0) {
				System.err
						.println("CAAMModel::NormalizeTexture() tried to normalize a zero texture vector.");
			}

		}
	}

	/**
	 * Wrapper for model optimization.
	 * 
	 * @param image
	 *            search image
	 * @param s
	 *            init shape
	 * @param c
	 *            init model parameters
	 * @return optimization result
	 */
	public CAAMOptRes OptimizeModel(final ModelSimpleImage image, CAAMShape s,
			CDVector c) {
		return OptimizeModel(image, s, c, 30, null, false);
	}

	/**
	 * Wrapper for model optimization
	 * 
	 * @param image
	 *            search image
	 * @param s
	 *            init shape
	 * @param c
	 *            init parameters
	 * @param maxIterations
	 *            max iterations
	 * @param pOptStates
	 *            optimization state
	 * @return optimization result
	 */
	public CAAMOptRes OptimizeModel(final ModelSimpleImage image, CAAMShape s,
			CDVector c, final int maxIterations, Vector<CAAMOptState> pOptStates) {
		return OptimizeModel(image, s, c, maxIterations, pOptStates, false);
	}

	/**
	 * Performs AAM optimization of a shape containing initial pose and a set of
	 * model parameters (c).
	 * 
	 * @param image
	 *            The image to search in.
	 * @param s
	 *            The initial shape (also containing the inital pose, thus; not
	 *            a normalized shape). Actually only the pose of 's' is used (to
	 *            align the reference shape as the initial shape).
	 * 
	 *            NOTE: The optimal shape is returned in 's' after execution.
	 * @param c
	 *            The initial model parameters. If this vector is empty, it is
	 *            resized correctly and set equal to zero, thus the mean model.
	 * 
	 *            NOTE: The optimal model parameters are returned in 'c' after
	 *            execution.
	 * @param maxIterations
	 *            The maximum iterations allowed.
	 * @param pOptStates
	 *            Optional parameter all convergence info can be returned in.
	 *            See CAAMOptState.
	 * @param disableDamping
	 *            Disables the damping steps (default false).
	 * @return The results of the optimization in the form of a 'CAAMOptRes'
	 *         instance.
	 */
	public CAAMOptRes OptimizeModel(final ModelSimpleImage image, CAAMShape s,
			CDVector c, final int maxIterations,
			Vector<CAAMOptState> pOptStates, boolean disableDamping) {

		int iter = 0, np = m_CombinedPCA.NParameters();
		CDVector delta_g = new CDVector(m_iTextureSamples);
		CDVector delta_c = new CDVector(np);
		CDVector delta_t = new CDVector(4);
		CDVector delta_t_corr = new CDVector(4);
		CDVector c_est = new CDVector(np);
		double E_previous, E;
		Vector<CAAMOptState> optStates = new Vector<CAAMOptState>();
		CAAMShape s_est = new CAAMShape();
		double refSize = ReferenceShape().ShapeSize();

		// constants
		final double covergence_level = 0.01; // point at which we declare
												// convergence
		final int ndamps = 3;
		// final double k_values[] = {1.};
		// final double k_values[] = { 0.02, 0.01, 0.05 };
		final double k_values[] = {1., 0.5, 0.25};
		// final double k_values[] = {1., .95, 1.05};
		// final double k_values[] = {1.0, 0.5, -0.5, 0.25, -0.25};
		// config
		boolean useInterpolation = true;
		boolean opt_timings = false;

		// setup timers
		long timerModelEstimateTexDiff_start = 0;
		long timerRt_start = 0;
		long timerRc_start = 0;
		long timerModelEstimateTexDiff_stop = 0;
		long timerRt_stop = 0;
		long timerRc_stop = 0;
		long timerTotal_stop = 0;

		// setup
		if (c.Length() != np) {

			c.Resize(np); // resize appropriately
			c.assign(.0); // set to mean model
		}

		// calculate the initial error vector between the model and the image
		if (opt_timings)
			timerModelEstimateTexDiff_start = System.currentTimeMillis();
		E = ModelEstimateTexDiff(image, c, s, delta_g, 1, useInterpolation);
		if (opt_timings)
			timerModelEstimateTexDiff_start = System.currentTimeMillis();
		if (E == -1.) {
			// System.err.println("shape is outside ??????????????????????????????????????????????????????????");
			// return new CAAMOptRes( .0, 0, -1. ); /* shape is outside */
			return null;
		}

		// save initial state
		optStates.add(new CAAMOptState(E, s, c, 0)); // save state

		// iterate
		do {
			E_previous = E; // save old error

			// predict pose and model parameter displacement
			if (opt_timings)
				timerRt_start = System.currentTimeMillis();
			delta_t.assign(m_R_t.mult(delta_g)); // pose
			if (opt_timings)
				timerRt_stop = System.currentTimeMillis();

			if (opt_timings)
				timerRc_start = System.currentTimeMillis();
			delta_c.assign(m_R_c.mult(delta_g)); // model
			if (opt_timings)
				timerRc_stop = System.currentTimeMillis();

			// if the prediction above didn't improve th fit,
			// try amplify and later damp the prediction
			int i;
			// System.err.println("\n");
			for (i = 0; i < (disableDamping ? 1 : ndamps); i++) {
				// System.err.println(i + "\t");
				// make damped c prediction
				c_est.assign(c.add(delta_c.mult(k_values[i])));

				// convert pose prediction to actual size
				double sizeRatio = s.ShapeSize() / refSize;
				delta_t_corr.assign(delta_t); // copy prediction
				delta_t_corr.m_data[2] *= sizeRatio;
				delta_t_corr.m_data[3] *= sizeRatio;
				delta_t_corr.mult_into(k_values[i]); // damp

				// constrain the updates
				ConstrainSearchParameters(c_est, delta_t_corr);

				// update pose (damped)
				s_est.assign(s);
				s_est.Displace(delta_t_corr); // ????????????????????????????????????????

				// calculate the error vector between the predicted model and
				// the image
				if (opt_timings)
					timerModelEstimateTexDiff_start = System
							.currentTimeMillis();
				E = ModelEstimateTexDiff(image, c_est, s_est, delta_g, 1,
						useInterpolation);
				if (opt_timings)
					timerModelEstimateTexDiff_stop = System.currentTimeMillis();
				if (E != -1. && E < E_previous) {
					break;
				} // error improved -> make new prediction
			}

			if (E != -1. && E < E_previous) {

				// the error improved, accept the new c and s
				c.assign(c_est);
				s.assign(s_est);
				
				// ModelImage tempImg = new ModelImage(image, "dampFactor");
				// s.generateVOImesh(tempImg);
				// new ViewJFrameImage(tempImg); 
				
				optStates.add(new CAAMOptState(E, s, c, i)); // save state
			}

			++iter;

		} while (E != -1. && E_previous - E > covergence_level * E
				&& iter < maxIterations);

		int last = optStates.size() - 1; // this is the last good iteration
		s.assign(optStates.get(last).shape); // return the best shape in s
		c.assign(optStates.get(last).c); // return the best model param in c

		// return convergence info (if requested)
		// ??????????????????????????????????????????
		if (pOptStates != null) {
			int len = optStates.size();
			pOptStates.setSize(len);
			pOptStates.clear();
			for (int i = 0; i < len; i++) {
				CAAMOptState temp = new CAAMOptState();
				temp.assign(optStates.get(i));
				pOptStates.add(temp);
				// pOptStates.get(i).assign(optStates.get(i));
			}
		}

		// calculate optimization results
		CAAMOptRes results = new CAAMOptRes(
				m_CombinedPCA.MahalanobisDistance(c), iter, E);

		if (opt_timings) {

			timerTotal_stop = System.currentTimeMillis();

			System.err.println("OptimizeModel() timings:");
			System.err
					.println("  ModelEstimateTexDiff()   : "
							+ (timerModelEstimateTexDiff_stop - timerModelEstimateTexDiff_start)
							+ " ms");
			System.err.println("  Rt multiplication        : "
					+ (timerRt_stop - timerRt_start) + " ms");
			System.err.println("  Rc multiplication        : "
					+ (timerRc_stop - timerRc_start) + " ms");
			System.err.println("  Total time               : "
					+ (timerTotal_stop - timerRc_start) + " ms");
		}

		// return optimization results
		return results;
	}

	/**
	 * Perform general-purpose optimization of the AAM using simulated
	 * annealing, conjugate gradient, steepest descent, BGFS or pattern search.
	 * 
	 * @param image
	 *            The image beeing searched in.
	 * @param s
	 *            The inital shape pose.
	 * @param c
	 *            The initial model parameters.
	 * @param maxIterations
	 *            The maximum allowed number of iterations.
	 * @param similaritym
	 *            The used similarity measure for the optimization:
	 * 
	 *            0 Non-normalized L_2 norm (default).
	 * 
	 *            1 The "Mahalanobis" distance (texture samples are regarded
	 *            independent to increase performance).
	 * 
	 *            2 The Lorentzian error norm.
	 * @param optimizer
	 *            Sets the optimer to use:
	 * 
	 *            1 Steepest Descent (default) 2 Conjugate Gradient 3
	 *            Quasi-Newton, BFGS 4 Pattern search 5 Simulated annealing
	 * @return The final fit.
	 */
	public CAAMOptRes OptimizeModelByFineTuning(final ModelSimpleImage image,
			CAAMShape s, CDVector c, final int maxIterations,
			final int similaritym, final int optimizer) {

		// make an aam optimizer
		CAAMOptimize AAMOpt = new CAAMOptimize(this, s, image, similaritym);

		int n = m_CombinedPCA.NParameters();

		// setup the optimization vector
		int nVx;

		nVx = n + 4;

		CDVector vX = new CDVector(nVx);
		CDVector GradStepVec = new CDVector(nVx);

		for (int i = 0; i < n; i++) {

			vX.m_data[i] = c.m_data[i]; // initial model parameters
			GradStepVec.m_data[i] = Math.abs(c.m_data[i]) * .05d; // step size
		}
		for (int i = n; i < n + 4; i++) {

			vX.m_data[i] = 0; // displacement is zero intially
		}
		GradStepVec.m_data[n] = .01; // scale
		GradStepVec.m_data[n + 1] = CAAMUtil.Deg2Rad(1); // radians
		GradStepVec.m_data[n + 2] = ReferenceShape().Width() * .01; // pixels
		GradStepVec.m_data[n + 3] = ReferenceShape().Height() * .01; // pixels

		// do the fine-tuning
		CDOptimizeBase pOptimizer = null;
		try {

			switch (optimizer) {

			case 1:
				pOptimizer = new CDOptimizeSD();
				break;
			case 2:
				pOptimizer = new CDOptimizeCG();
				break;
			case 3:
				pOptimizer = new CDOptimizeBFGS();
				break;
			case 4:
				pOptimizer = new CDOptimizePS();
				break;
			case 5:
				pOptimizer = new CDOptimizeSA();
				break;
			default:
				System.err.println("Unknown optimizer type=" + optimizer);
				break;
			}

			// set the maximum number of iterations
			pOptimizer.SetMaxFuncEval(maxIterations);
			pOptimizer.SetMaxIterations(maxIterations);

			// do the optimization (using numerical gradidents)
			pOptimizer.MinimizeNum(vX, AAMOpt, GradStepVec);

		} catch (Exception e) {
			e.printStackTrace();
			System.err.println("Error allocating general purpose optimizer!");
		}

		// get results
		double[] E = new double[1];
		AAMOpt.OptResults(c, s, E);

		// calculate optimization results
		CAAMOptRes results = new CAAMOptRes(
				m_CombinedPCA.MahalanobisDistance(c), maxIterations, E[0]);

		// deallocate
		pOptimizer = null;

		return results;
	}

	/**
	 * 
	 * Reads the complete AAMModel from disk.
	 * 
	 * @param filename
	 *            Input filename without any extension. E.g. if the files on
	 *            disk are
	 * 
	 *            'model.txt' & 'model.amf'
	 * 
	 *            -> filename = 'model'
	 * @return true on success, false on file errors.
	 */
	public boolean ReadModel(final String filename) {

		try {
			// fh = fopen( CAAMUtil.ForceExt( filename, "amf" ), "rb" );
			DataInputStream fh = new DataInputStream(new FileInputStream(
					filename));

			double version;
			// fread( &version, sizeof(double), 1, fh );
			version = fh.readDouble();

			if (version != m_dAMFVersion) {

				System.err.println("Error: Wrong .amf format version, v "
						+ version + ". Ver. " + m_dAMFVersion
						+ "was expected.\n");
				fh.close();
				return false;
			}

			// read misc settings
			// fread( &m_iModelReduction, sizeof(int), 1, fh );
			m_iModelReduction = fh.readInt();
			// fread( &m_dAddExtents, sizeof(double), 1, fh );
			m_dAddExtents = fh.readDouble();
			int use_hull;
			// fread( &use_hull, sizeof(int), 1, fh );
			use_hull = fh.readInt();

			m_bUseConvexHull = use_hull == 1;

			// PCA
			m_ShapePCA.FromFile(fh);
			m_TexturePCA.FromFile(fh);
			m_CombinedPCA.FromFile(fh);

			m_mShape2PixelWeights.FromFile(fh);
			m_mQsEV.FromFile(fh);
			m_mQgEV.FromFile(fh);
			m_vMeanTexture.FromFile(fh);
			m_vMeanTextureOrg.FromFile(fh);
			m_sMeanAShape.FromFile(fh);

			CDVector aux = new CDVector(7);
			aux.FromFile(fh);
			m_iTextureSamples = (int) aux.get(0);
			m_iNShapes = (int) aux.get(1);
			m_dMeanShapeSize = aux.get(2);
			m_iLearningMethod = (int) aux.get(3);
			m_iShapeTrunc = (int) aux.get(4);
			m_iTextureTrunc = (int) aux.get(5);
			m_iCombinedTrunc = (int) aux.get(6);

			// Regression matrices.
			m_R_c.FromFile(fh);
			m_R_t.FromFile(fh);

			m_vTextureVar.FromFile(fh);

			// load texture transfer function
			if (m_pTextureTF != null) {

				m_pTextureTF = null;
			}
			m_pTextureTF = CAAMTransferFunction.AAMLoadTransferFunction(fh,
					this);

			// initialize the ReferenceFrame and AnalyzeSynthesize objects
			if (m_pReferenceFrame != null) {
				m_pReferenceFrame = null;
			}
			m_pReferenceFrame = new CAAMReferenceFrame();
			m_pReferenceFrame.FromFile(fh);

			if (m_pAnalyzeSynthesize != null) {
				m_pAnalyzeSynthesize = null;
			}
			m_pAnalyzeSynthesize = CAAMAnalyzeSynthesize
					.AAMLoadAnalyzerSynthesizer(fh, this.m_pReferenceFrame);

			// close file
			// if ( 0!=fclose(fh) ) return false;
			fh.close();
			return true;

		} catch (IOException e) {

			e.printStackTrace();
			System.err
					.println("An error occured during CAAMModel::ReadModel()");
			return false;
		}

		// return true;
	}

	/**
	 * Returns the reduction factor of the training set that this model was
	 * 
	 * @return model reduction
	 */
	public int ReductionFactor() {
		return m_iModelReduction;
	}

	/**
	 * Wrapper to build a texture vector from an image and a shape.
	 * 
	 * @param image
	 *            model image
	 * @param shape
	 *            voi
	 * @param textureSamples
	 *            texture samples
	 * @return shape inside or outside the image
	 */
	public int SampleShape(final ModelSimpleImage image, final CAAMShape shape,
			CDVector textureSamples) {
		return SampleShape(image, shape, textureSamples, true, true, true);
	}

	/**
	 * Wrapper to build a texture vector from an image and a shape.
	 * 
	 * @param image
	 *            model image
	 * @param shape
	 *            VIO
	 * @param textureSamples
	 *            texture samples
	 * @param normalize
	 *            normalization
	 * @return shape inside or outside the image
	 */
	public int SampleShape(final ModelSimpleImage image, final CAAMShape shape,
			CDVector textureSamples, final boolean normalize) {
		return SampleShape(image, shape, textureSamples, normalize, true, true);
	}

	/**
	 * Wrapper to build a texture vector from an image and a shape.
	 * 
	 * @param image
	 *            model image
	 * @param shape
	 *            VIO
	 * @param textureSamples
	 *            texture samples
	 * @param normalize
	 *            normalization
	 * @param useInterpolation
	 *            interpolation or not.
	 * @return shape inside or outside the image
	 */
	public int SampleShape(final ModelSimpleImage image, final CAAMShape shape,
			CDVector textureSamples, final boolean normalize,
			final boolean useInterpolation) {
		return SampleShape(image, shape, textureSamples, normalize,
				useInterpolation, true);
	}

	/**
	 * Builds a texture vector from an image and a shape.
	 * 
	 * @param image
	 *            The image to sample in.
	 * @param textureSamples
	 *            The normalized destination texture vector.
	 * @param shape
	 *            The shape to sample from (in image coordinates).
	 * @param normalize
	 *            Perform normalization after sampling. Default true.
	 * @param map
	 *            Perform mapping after sampling. Default true.
	 * @return The number of samples done (zero if the shape is outside the
	 *         image).
	 */
	public int SampleShape(final ModelSimpleImage image, final CAAMShape shape,
			CDVector textureSamples, final boolean normalize,
			final boolean useInterpolation, final boolean map) {

		// test shape
		boolean inside = CAAMUtil.ShapeInsideImage(shape, image);
		if (!inside) {

			// the shape is outside the image
			// System.err.println("shape is outside the image, 2 ***********************");
			return 0;
		}

		// set input image
		//
		// For the OpenGL version this is a *very*
		// expensive operation and should only be done
		// if really needed, theirfore the small trick below.
		//
		if (m_sCurrentAnalyzeId != image.getName()) {

			// set image
			m_pAnalyzeSynthesize.SetAnalyzeImage(image);

			// set unique id
			String id = new String();
			id = ("SetAnalyzeImage id=" + (int) System.currentTimeMillis());
			// System.err.println("id = " + id);
			((ModelSimpleImage) image).SetName(id);

			// set current analyze image id
			((CAAMModel) this).m_sCurrentAnalyzeId = id;
		}

		// sample texture
		m_pAnalyzeSynthesize.Analyze(shape, textureSamples, useInterpolation);
		int nSamples = m_pReferenceFrame.NTextureSamples();

		// sample whiskers
		//
		// usage:
		//
		// _putenv( "SAMPLE_WHISKERS=1" );
		//
		// default 0
		//
		boolean sampleWhiskers;
		/*
		 * if (USEAAMENV) { String sampleWhiskersStr = getenv( "SAMPLE_WHISKERS"
		 * ); sampleWhiskers = (sampleWhiskersStr.equals("1") ? false : true); }
		 * else {
		 */
		sampleWhiskers = false;
		// }
		if (sampleWhiskers) {

			// an extremely inefficient whiskers implementation
			// made real quick
			//
			// LATER: implement proper weigthning of texture samples
			//
			double scale = shape.ShapeSize() / ReferenceShape().ShapeSize();
			int np = shape.NPoints();
			double weight = 2.;
			int w_samples = (int) (.5 + (weight * nSamples) / np); // number of
																	// samples
																	// per
																	// whisker

			double[] x1 = new double[1];
			double[] x2 = new double[1];
			double[] y1 = new double[1];
			double[] y2 = new double[1];
			ReferenceShape().GetPointUS(0, x1, y1);
			ReferenceShape().GetPointUS(1, x2, y2);
			double whiskerLength = 3. * Math.sqrt((x1[0] - x2[0])
					* (x1[0] - x2[0]) + (y1[0] - y2[0]) * (y1[0] - y2[0]));
			double wl = scale * whiskerLength; // in 'shape' pixels
			// printf("whiskerLength=%.1f, wl=%.1f, w_samples=%i, nSamples=%i\n",
			// whiskerLength, wl, w_samples, nSamples );
			CAAMPoint p = new CAAMPoint();
			CAAMPoint p_outside = new CAAMPoint();
			CAAMPoint p_inside = new CAAMPoint();
			CDVector whiskerSamples = new CDVector(np * w_samples
					* AAMdef.BANDS);
			double pix = 0;
			int iw = image.Width();
			int ih = image.Height();
			int s = 0;
			for (int i = 0; i < np; i++) {

				p = shape.GetPoint(i);
				shape.Normal(i, p_outside, p_inside, wl);

				// sample
				double t, x, y;
				for (int j = 0; j < w_samples; j++) {

					t = j / (w_samples - 1.);
					x = (1. - t) * p.x + t * p_outside.x;
					y = (1. - t) * p.y + t * p_outside.y;

					if (x > 0 && x < iw - 1 && y > 0 && y < ih - 1) {
						// image.Pixel1( x, y, pix );
						pix = image.getValue((int) x, (int) y);
					}
					// for(int k=0;k<AAMdef.BANDS;k++) {
					whiskerSamples.m_data[s++] = pix;
					// }
				}
			}

			nSamples += whiskerSamples.Length();

			CDVector tmp = textureSamples.VecCat(whiskerSamples);
			textureSamples.Resize(tmp.Length());
			textureSamples.assign(tmp);
		}

		// normalize the texture vector
		if (normalize) {
			NormalizeTexture(textureSamples);
		}

		// apply the current texture transfer function
		if (map) {
			TextureTF().Map(textureSamples);
		}

		// return
		return nSamples;
	}

	/**
	 * Generates a shape based on a set of model parameters.
	 * 
	 * @param c
	 *            Input model parameters.
	 * @param outShape
	 *            The generated shape (resizing are done inside this method).
	 */
	public void ShapeInstance(final CDVector c, CAAMShape outShape) {

		int rows = m_sMeanAShape.Length();
		int cols = m_CombinedPCA.NParameters();

		if (m_mShapeInstance.NCols() != cols
				|| m_mShapeInstance.NRows() != rows) {

			// cache this very expensive matrix product
			((CAAMModel) this).m_mShapeInstance.Resize(rows, cols);
			((CAAMModel) this).m_mShapeInstance.assign(m_ShapePCA
					.EigenVectors().mult(m_mShape2PixelWeights.Inverted())
					.mult(m_mQsEV));
		}

		outShape.assign(m_sMeanAShape);
		outShape.add_into(m_mShapeInstance.mult(c));
	}

	/**
	 * Generates a texture based on a set of model parameters.
	 * 
	 * @param c
	 *            Input model parameters.
	 * @param outShape
	 *            The generated texture (resizing are done inside this method).
	 */
	public void TextureInstance(final CDVector c, CDVector outTexture) {

		final int rows = m_vMeanTexture.Length();
		final int cols = m_CombinedPCA.NParameters();

		final int m = rows;
		final int kc = cols;
		final int kt = m_TexturePCA.NParameters();
		boolean doCache = m * (kc - kt) < kt * kc; // See FAME paper

		outTexture.Resize(m_vMeanTexture.Length());
		outTexture.assign(m_vMeanTexture);
		if (doCache) {

			if (m_mTextureInstance.NCols() != cols
					|| m_mTextureInstance.NRows() != rows) {

				// cache this expensive matrix product
				((CAAMModel) this).m_mTextureInstance.Resize(rows, cols);
				((CAAMModel) this).m_mTextureInstance.assign(m_TexturePCA
						.EigenVectors().mult(m_mQgEV));
			}
			outTexture.add_into(m_mTextureInstance.mult(c));
		} else {

			outTexture.add_into(m_TexturePCA.EigenVectors().mult(
					(m_mQgEV.mult(c))));
		}
	}

	/**
	 * Generates a shape based on a set of shape b-parameters.
	 * 
	 * @param b_s
	 *            Shape parameters
	 * @param outShape
	 *            Output shape (resized inside method).
	 */
	public void ShapePCAInstance(final CDVector b_s, CAAMShape outShape) {

		// since the CAAMDeformPCA only handles vectors
		// all additional data such as point aux info etc.
		// are lost in the pca-deformation
		//
		// hence we need to copy it into the 'outShape'
		// object explicitly
		//
		outShape.assign(m_sMeanAShape);

		// do deformation
		m_ShapePCA.Deform(b_s, outShape);
	}

	/**
	 * Projects the shape and texture into c-space i.e. the combined model
	 * parameters.
	 * 
	 * @param shape
	 *            The input shape aligned to the (aligned) mean shape.
	 * @param texture
	 *            The corresponding normalized texture.
	 * @param c
	 *            The resulting model parameters.
	 */
	public void ShapeTex2Combined(final CAAMShape shape,
			final CDVector texture, CDVector c) {

		CDVector b = new CDVector();
		ShapeTex2Param(shape, texture, b);
		ShapeTexParam2Combined(b, c);
	}

	/**
	 * Extract the b-parameters from a shape and corresponding texture by
	 * inverting the shape and texture pca projection.
	 * 
	 * Assumes that the shape and texture PCA are done beforehand.
	 * 
	 * @param shape
	 *            The input shape aligned to the (aligned) mean shape.
	 * @param texture
	 *            The corresponding normalized texture.
	 * @param b
	 *            The resulting concatenated b vector.
	 */
	public void ShapeTex2Param(final CAAMShape shape, final CDVector texture,
			CDVector b) {

		int j;
		int nbTexParam = m_TexturePCA.NParameters();
		int nbShapeParam = m_ShapePCA.NParameters();

		CDVector b_s = new CDVector(nbShapeParam);
		CDVector b_g = new CDVector(nbTexParam);
		b.Resize(nbTexParam + nbShapeParam);

		b_s.assign(m_mShape2PixelWeights.mult(m_ShapePCA.EigenVectors()
				.Transposed().mult((shape.sub(m_sMeanAShape)))));

		b_g.assign(m_TexturePCA.EigenVectors().Transposed()
				.mult((texture.sub(m_vMeanTexture))));

		// concat vectors
		for (j = 0; j < nbShapeParam; j++)
			b.m_data[j] = b_s.m_data[j];
		for (j = nbShapeParam; j < b.Length(); j++)
			b.m_data[j] = b_g.m_data[j - nbShapeParam];
	}

	/**
	 * Transforms the concatenated b-parameters into combined model parameters.
	 * 
	 * @see ShapeTex2Param
	 * @param b
	 *            Concatenated shape (weighted) and texture parameters
	 * @param c
	 *            Combined model parameters (resized inside function).
	 * @return Nothing.
	 */
	public void ShapeTexParam2Combined(final CDVector b, CDVector c) {
		c.Resize(m_CombinedPCA.NParameters());
		c.assign(m_CombinedPCA.EigenVectors().Transposed().mult(b));
	}

	/**
	 * Write the AAM model to text file or binary file.
	 * 
	 * @param filename
	 *            file name
	 * @return success or not
	 */
	public boolean WriteModel(final String filename) {
		return WriteModel(filename, false);
	}

	/**
	 * Writes the complete AAMModel to disk as a .txt and an .amf file.
	 * 
	 * @param filename
	 *            Output filename without any extension.
	 * @param txt_only
	 *            If true binary model data is not written.
	 * @return true on success, false on file errors.
	 */
	public boolean WriteModel(final String filename, final boolean txt_only) {

		boolean success = true;

		PrintWriter fh;

		try {
			// ///////////////////////////////////////////////////
			// write the human readable txt file
			// ///////////////////////////////////////////////////
			fh = new PrintWriter(filename + ".txt");

			fh.println("######################################################################");
			fh.println("Active Appearance Model File ");

			String pattern = "yyyy.MMMMM.dd GGG hh:mm aaa";
			Locale currentLocale = new Locale("en", "US");
			Date today;
			SimpleDateFormat formatter;
			String output;

			formatter = new SimpleDateFormat(pattern, currentLocale);
			today = new Date();
			output = formatter.format(today);

			// fh.println("\nWritten                   : " + s);
			fh.println(pattern + "   " + output);
			fh.println("Format version            : " + m_dAMFVersion);
			fh.println("Build time                : "
					+ CAAMUtil.Secs2Mins(m_dBuildTime) + "(" + m_dBuildTime
					+ " secs)");

			fh.println("Shapes                    : " + m_iNShapes);
			fh.println("Shape points              : " + m_sMeanAShape.NPoints());
			fh.println("Texture Bands             : " + this.NBands());
			String szPerBand = new String();
			if (NBands() != 1)
				szPerBand = " (" + (m_iTextureSamples / this.NBands())
						+ " samples/band";

			fh.println("Texture samples           : " + m_iTextureSamples
					+ szPerBand);

			fh.println("Texture TF                : " + m_pTextureTF.TypeName());
			String str = m_pTextureTF.TypeInfo();
			if (str.length() > 0) {

				fh.println("Texture TF info           : " + str);
			}

			fh.println("Model reduction           : " + m_iModelReduction);
			fh.println("Add Shape Extents         : " + m_dAddExtents);
			fh.println("Convex hull used          : "
					+ (m_bUseConvexHull ? "Yes" : "No"));
			fh.println("Tangent space used        : "
					+ (m_bUseTangentSpace ? "Yes" : "No"));
			fh.println("Learning method           : " + m_iLearningMethod);
			fh.println("Shape truncation level    : " + m_iShapeTrunc
					+ " (variance: " + m_ShapePCA.EigenValues().Sum() + "/"
					+ m_ShapePCA.EigenValuesOrg().Sum() + ")%");
			fh.println("Texture truncation level  : " + m_iTextureTrunc
					+ " (variance: " + m_TexturePCA.EigenValues().Sum() + "/"
					+ m_TexturePCA.EigenValuesOrg().Sum() + ")%");
			fh.println("Combined truncation level : " + m_iCombinedTrunc
					+ " (variance: " + m_CombinedPCA.EigenValues().Sum() + "/"
					+ m_CombinedPCA.EigenValuesOrg().Sum() + ")%");
			fh.println("Parameters used           : "
					+ m_CombinedPCA.NParameters());
			fh.println("Mean shape area           : "
					+ ReferenceShape().Area(m_bUseConvexHull));
			double acc;
			int n;

			// modes of variation in the combined model
			fh.println("Combined mode variation : ");
			acc = 0;
			n = m_CombinedPCA.NParametersOrg();
			for (int i = 1; i <= m_CombinedPCA.NParameters(); i++) {

				acc += 100. * m_CombinedPCA.ParameterWeightOrg(n - i, true);
				fh.println("\t" + i + "\t" + 100.
						* m_CombinedPCA.ParameterWeightOrg(n - i, true)
						+ " % \t(" + acc + "%)");
			}

			// modes of variation in the shape model
			fh.println("Shape mode variation : ");
			acc = 0;
			n = m_ShapePCA.NParametersOrg();
			for (int i = 1; i <= m_ShapePCA.NParameters(); i++) {

				acc += 100. * m_ShapePCA.ParameterWeightOrg(n - i, true);
				fh.println("\t" + i + "\t" + 100.
						* m_ShapePCA.ParameterWeightOrg(n - i, true)
						+ "% + \t(" + acc + "%)");
			}

			// modes of variation in the texture model
			fh.println("Texture mode variation : ");
			acc = 0;
			n = m_TexturePCA.NParametersOrg();
			for (int i = 1; i <= m_TexturePCA.NParameters(); i++) {

				acc += 100. * m_TexturePCA.ParameterWeightOrg(n - i, true);
				fh.println("\t" + i + " \t" + 100.
						* m_TexturePCA.ParameterWeightOrg(n - i, true)
						+ "% \t(" + acc + "%)");
			}

			fh.println();
			fh.println("######################################################################");

			fh.close();

			// ///////////////////////////////////////////////////
			// write the binary data file
			// ///////////////////////////////////////////////////
			if (txt_only != true) {

				// fh = fopen( filename+CString(".amf"), "wb" );
				DataOutputStream os = new DataOutputStream(
						new FileOutputStream(filename + ".amf"));

				// write version number
				// fwrite( &m_dAMFVersion, sizeof(double), 1, fh );
				os.writeDouble(m_dAMFVersion);

				// write misc settings
				// fwrite( &m_iModelReduction, sizeof(int), 1, fh );
				os.writeInt(m_iModelReduction);

				// fwrite( &m_dAddExtents, sizeof(double), 1, fh );
				os.writeDouble(m_dAddExtents);

				int use_hull = m_bUseConvexHull ? 1 : 0;
				// fwrite( &use_hull, sizeof(int), 1, fh );
				os.writeInt(use_hull);

				// PCA
				m_ShapePCA.ToFile(os);
				m_TexturePCA.ToFile(os); // big object: nbTexSamples *
											// nbTParam+1 * 8 bytes
				m_CombinedPCA.ToFile(os);

				m_mShape2PixelWeights.ToFile(os);
				m_mQsEV.ToFile(os); // cache matrix: could be extracted from
									// m_CombinedPCA
				m_mQgEV.ToFile(os); // cache matrix: could be extracted from
									// m_CombinedPCA

				m_vMeanTexture.ToFile(os); // big vector: nbTexSamples * 8 bytes
				m_vMeanTextureOrg.ToFile(os); // big vector: nbTexSamples * 8
												// bytes
				m_sMeanAShape.ToFile(os);

				CDVector aux = new CDVector(7);
				aux.m_data[0] = m_iTextureSamples;
				aux.m_data[1] = m_iNShapes;
				aux.m_data[2] = m_dMeanShapeSize;
				aux.m_data[3] = m_iLearningMethod;
				aux.m_data[4] = m_iShapeTrunc;
				aux.m_data[5] = m_iTextureTrunc;
				aux.m_data[6] = m_iCombinedTrunc;
				aux.ToFile(os);

				// Regression matrices.
				m_R_c.ToFile(os); // big matrix: nbTexSamples * nbCParam * 8
									// bytes
				m_R_t.ToFile(os); // big matrix: nbTexSamples * 4 * 8 bytes

				m_vTextureVar.ToFile(os); // big vector: nbTexSamples * 8 bytes

				m_pTextureTF.ToFile(os);

				m_pReferenceFrame.ToFile(os);
				m_pAnalyzeSynthesize.ToFile(os);

				// close file
				// if ( fclose(fh) ) return false;
				os.close();
				return false;

			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		return success;
	}

	/**
	 * Get the the number of eigen values in PCA.
	 * 
	 * @return number of eigen values in PCA
	 */
	public int getNumberShapeParameters() {
		return m_ShapePCA.NParameters();
	}

	/**
	 * Plots the variance of each pixel in the model over the training set into
	 * the mean shape and saves the image.
	 * 
	 * @param filename
	 *            Output image filename.
	 */
	public void WriteVarianceMap(final String filename) {

		CDVector vByteVariances = new CDVector();
		vByteVariances.Resize(m_vTextureVar.m_length);
		vByteVariances.assign(m_vTextureVar);

		// make variance map
		ModelSimpleImage image = new ModelSimpleImage();
		image = ShapeFreeImage(vByteVariances, image);
		// image.WriteBandedFile( filename );
		ModelImage img = new ModelImage(image, filename);
		String dir = "";
		String name = filename;

		img.saveImage(dir, name, FileUtility.XML, false);
	}

	/**
	 * Returns the reference shape where all texture sampling and comparison
	 * should be done.
	 * 
	 * The reference shape is defined as the mean shape size to mean size and
	 * moved to the fourth quadrant.
	 * 
	 * @return The reference shape.
	 */
	public final CAAMShape ReferenceShape() {

		return m_pReferenceFrame.RefShape();
	}

	/**
	 * Converts a set of b-vectors to combined model parameters (c-vectors).
	 * 
	 * @param bVectors
	 *            The input b-vectors.
	 * @param cVectors
	 *            The output c-vectors.
	 * @return Nothing.
	 */
	public void ShapeTexParam2Combined(final Vector<CDVector> bVectors,
			Vector<CDVector> cVectors) {

		cVectors.clear();

		for (int i = 0; i < bVectors.size(); i++) {
			CDVector c = new CDVector();
			ShapeTexParam2Combined(bVectors.get(i), c);

			cVectors.add(c);
		}
	}

	/**
	 * Copy constructor.
	 * 
	 * @param m
	 *            Object to copy from.
	 */
	public CAAMModel(final CAAMModel m) {

		m_pTextureTF = null;
		m_pReferenceFrame = null;
		m_pAnalyzeSynthesize = null;

		this.assign(m);
	}

	/**
	 * Assignment operator.
	 * 
	 * @param m
	 *            Object to copy from.
	 * @return This;
	 */
	public CAAMModel assign(final CAAMModel m) {

		// assignment copy of all members
		m_ShapePCA = m.m_ShapePCA;
		m_TexturePCA = m.m_TexturePCA;
		m_CombinedPCA = m.m_CombinedPCA;
		m_mShape2PixelWeights.assign(m.m_mShape2PixelWeights);
		m_dAMFVersion = m.m_dAMFVersion;
		m_mQsEV.assign(m.m_mQsEV);
		m_mQgEV.assign(m.m_mQgEV);
		m_vMeanTexture.assign(m.m_vMeanTexture);
		m_vMeanTextureOrg.assign(m.m_vMeanTextureOrg);
		m_sMeanAShape.assign(m.m_sMeanAShape);
		m_mShapeInstance.assign(m.m_mShapeInstance);
		m_mTextureInstance.assign(m.m_mTextureInstance);
		m_vTextureVar.assign(m.m_vTextureVar);
		m_iTextureSamples = m.m_iTextureSamples;
		m_iNShapes = m.m_iNShapes;
		m_dMeanShapeSize = m.m_dMeanShapeSize;
		m_R_c.assign(m.m_R_c);
		m_R_t.assign(m.m_R_t);
		m_iModelReduction = m.m_iModelReduction;
		m_dAddExtents = m.m_dAddExtents;
		m_bUseConvexHull = m.m_bUseConvexHull;
		m_bUseTangentSpace = m.m_bUseTangentSpace;
		m_iLearningMethod = m.m_iLearningMethod;

		m_iShapeTrunc = m.m_iShapeTrunc;
		m_iTextureTrunc = m.m_iTextureTrunc;
		m_iCombinedTrunc = m.m_iCombinedTrunc;

		m_dBuildTime = m.m_dBuildTime;

		// deallocate
		if (m_pTextureTF != null) {

			// delete m_pTextureTF;
			m_pTextureTF = null;
		}
		if (m.m_pTextureTF != null) {

			m_pTextureTF = (CAAMTransferFunction) (m.m_pTextureTF.Clone());
		}

		if (m_pReferenceFrame != null) {

			// delete m_pReferenceFrame;
			m_pReferenceFrame = null;
		}

		if (m.m_pReferenceFrame != null) {

			m_pReferenceFrame = new CAAMReferenceFrame();
			m_pReferenceFrame.assign(m.m_pReferenceFrame);
		}

		if (m_pAnalyzeSynthesize != null) {

			m_pAnalyzeSynthesize = null;
		}

		if (m.m_pAnalyzeSynthesize != null) {
			/******************************* assign *************************************/
			m_pAnalyzeSynthesize = m.m_pAnalyzeSynthesize
					.Clone(this.m_pReferenceFrame);
		}

		return this;
	}

	/**
	 * Projects a shape into a set of c parameters.
	 * 
	 * @param shape
	 *            The input shape (assumed to be in abs coordinates).
	 * @param image
	 *            The image where the shape is placed.
	 * @param c
	 *            The output combined model parameters.
	 */
	public void Shape2Combined(final CAAMShape shape,
			final ModelSimpleImage image, CDVector c) {

		// build texture vector from annotation and host image
		CDVector texture = new CDVector();

		// sample texture
		SampleShape(image, shape, texture, true);

		// ///////////////////////////////////////////////////
		// approx the model (c) parameters for this example
		// ///////////////////////////////////////////////////

		// copy the input shape
		CAAMShape shapeCopy = new CAAMShape(shape);

		// align the shape to the mean aligned shape
		shapeCopy.AlignTo(m_sMeanAShape);

		// extract model parameters from shape and texture
		CDVector b = new CDVector();
		ShapeTex2Param(shapeCopy, texture, b);
		ShapeTexParam2Combined(b, c);
	}

	/**
	 * Projects a shape into a set of shape parameters,
	 * 
	 * @param shape
	 *            The input shape (assumed to be in abs parameters).
	 * @param b
	 *            The output shape model parameters.
	 */
	public void Shape2Param(final CAAMShape shape, CDVector b_s) {

		CAAMShape alignedShape = new CAAMShape(shape);
		alignedShape.AlignTo(m_sMeanAShape);

		b_s.Resize(m_ShapePCA.NParameters());
		b_s.assign(m_ShapePCA.EigenVectors().Transposed()
				.mult((alignedShape.sub(m_sMeanAShape))));
	}

	/**
	 * Converts combined model parameters to shape parameters.
	 * 
	 * @param c
	 *            Combined model parameters.
	 * @param b_s
	 *            Output non-weighted shape parameters.
	 */
	public void Combined2ShapeParam(final CDVector c, CDVector b_s) {

		b_s.Resize(m_ShapePCA.NParameters());
		b_s.assign(m_mShape2PixelWeights.Inverted().mult(m_mQsEV.mult(c)));
	}

	/**
	 * Converts combined model parameters to texture parameters.
	 * 
	 * @param c
	 *            Combined model parameters.
	 * @param b_g
	 *            Output texture parameters.
	 */
	public void Combined2TexParam(final CDVector c, CDVector b_g) {

		b_g.Resize(m_TexturePCA.NParameters());
		b_g.assign(m_mQgEV.mult(c));
	}

	/**
	 * Projects shape and texture parameters into the combined eigenspace.
	 * 
	 * @param b_s
	 *            Input shape parameters.
	 * @param b_g
	 *            Input texture parameters.
	 * @param c
	 *            Resulting combined model parameters.
	 */
	public void ShapeTexParam2Combined(final CDVector b_s, final CDVector b_g,
			CDVector c) {

		int ns = b_s.Length();
		int nt = b_g.Length();

		CDVector b_sw = new CDVector(ns);
		CDVector b = new CDVector(ns + nt);
		b_sw.assign(m_mShape2PixelWeights.mult(b_s));
		b.assign(b_sw.VecCat(b_g));
		this.ShapeTexParam2Combined(b, c);
	}

}
