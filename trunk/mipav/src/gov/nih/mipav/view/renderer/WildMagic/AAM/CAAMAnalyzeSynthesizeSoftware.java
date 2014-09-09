package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;
import gov.nih.mipav.model.structures.*;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation. 
 * 
 * Copyright © 2000, 2001, 2002, 2003 by Mikkel B.
 * Stegmann IMM, Informatics & Mathmatical Modelling Technical University of
 * Denmark Richard Petersens Plads Building 321 DK-2800 Lyngby, Denmark
 * http://www.imm.dtu.dk/
 * 
 * Author: Mikkel B. Stegmann - http://www.imm.dtu.dk/~aam/ - aam@imm.dtu.dk
 * 
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
 * 
 * Concrete class for software implementation of the analysis/synthesis
 * functions.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMAnalyzeSynthesizeSoftware extends CAAMAnalyzeSynthesize {

	// warp cache entry
	class sWarpEntry {
		int[] triangle = new int[1];
		double[] alpha = new double[1];
		double[] beta = new double[1];
		double[] gamma = new double[1];
	};

	/** warp table */
	private Vector<sWarpEntry> m_WarpTable = new Vector<sWarpEntry>();

	/** reference to analyze image */
	private ModelSimpleImage m_pAnalyzeImage;

	/**
	 * Constructor
	 * 
	 * @param rf
	 *            The reference frame to analyze and synthesize.
	 */
	public CAAMAnalyzeSynthesizeSoftware(final CAAMReferenceFrame rf) {
		super(rf);
		m_Id = asSoftware;
		BuildWarpTable();
	}

	/**
	 * Dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Cache method, that caches triangle info. This method cache triangle
	 * information that does not change.
	 */
	public void BuildWarpTable() {

		final ModelSimpleImage mask = m_pReferenceFrame.MaskImage();
		int width = mask.Width();
		int height = mask.Height();

		boolean found;

		// scan mask image
		// NOTE: this is potentially unsafe, since we relies
		// on CAAMReferenceFrame doing the exact same
		// scanning
		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {

				if (mask.getValue(x, y) == 255) {

					sWarpEntry e = new sWarpEntry();
					// we're inside mask
					found = m_pReferenceFrame.FindTriangle(new CAAMPoint(x, y),
							e.triangle, e.alpha, e.beta, e.gamma);
					assert (found);
					if (found) {
						m_WarpTable.add(e);
					}
				}
			}
		}

		assert (m_WarpTable.size() == m_pReferenceFrame.NTextureSamples()
				/ AAMdef.BANDS);
	}

	/**
	 * Sets the image to be analyzed.
	 * 
	 * @param img
	 *            Image to be analyzed.
	 */
	public void SetAnalyzeImage(final ModelSimpleImage img) {

		m_pAnalyzeImage = img;
	}

	/**
	 * Warpper to analyze the image. Sample image texture under a given shape.
	 * 
	 * @param shape
	 *            VOI shape
	 * @param texture
	 *            image texture
	 * @return true if the shape is inside the image
	 */
	public boolean Analyze(final CAAMShape shape, CDVector texture) {
		return Analyze(shape, texture, true);
	}

	/**
	 * Warpper to analyze the image. Sample image texture under a given shape.
	 * 
	 * @param shape
	 *            VOI shape
	 * @param texture
	 *            image texture with MIPAV simple image type
	 * @return true if the shape is inside the image
	 */
	public boolean Analyze(final CAAMShape shape, ModelSimpleImage refImg) {
		return Analyze(shape, refImg, true);
	}

	/**
	 * This method samples the image intensities under a user-supplied shape
	 * into a texture vector.
	 * 
	 * @param shape
	 *            VOI shape
	 * @param texture
	 *            image texture
	 * @param useInterpolation
	 *            If true bilinear interpolation is used (default). Otherwise
	 *            the faster nearest neighbor interpolation is used.
	 * @return true if the shape is inside the image
	 */
	public boolean Analyze(final CAAMShape shape, CDVector texture,
			final boolean useInterpolation) {

		// setup
		double p;
		double iP;
		double x, y;
		double[] x1 = new double[1];
		double[] x2 = new double[1];
		double[] x3 = new double[1];
		double[] y1 = new double[1];
		double[] y2 = new double[1];
		double[] y3 = new double[1];
		sWarpEntry e;
		CAAMTriangle tri;
		final Vector<CAAMTriangle> vTriangles = m_pReferenceFrame.RefMesh()
				.Triangles();
		int np = m_pReferenceFrame.NTextureSamples();
		int c = 0, n = m_WarpTable.size();
		if (texture.Length() != np) {
			texture.Resize(np);
		}
		assert (np / AAMdef.BANDS == n);

		// warp
		for (int i = 0; i < n; i++) {

			// for each entry in the warp table
			e = m_WarpTable.get(i);

			// get triangle
			tri = vTriangles.get(e.triangle[0]);

			// calculate position in the analyze image
			shape.GetPointUS(tri.V1(), x1, y1);
			shape.GetPointUS(tri.V2(), x2, y2);
			shape.GetPointUS(tri.V3(), x3, y3);
			x = e.alpha[0] * x1[0] + e.beta[0] * x2[0] + e.gamma[0] * x3[0];
			y = e.alpha[0] * y1[0] + e.beta[0] * y2[0] + e.gamma[0] * y3[0];

			if (useInterpolation) {
				// do bilinear sampling
				p = m_pAnalyzeImage.Pixel1((float) x, (float) y);
				texture.set(c++, p);
			} else {
				// nearest neighbor
				iP = m_pAnalyzeImage.getValue((int) (x + .5), (int) (y + .5));
				texture.set(c++, iP);
			}
		}

		return true;
	}

	/**
	 * This method samples the image intensities under a user-supplied shape
	 * into a texture vector.
	 * 
	 * @param shape
	 *            VOI shape
	 * @param texture
	 *            image texture with MIPAV simple image type
	 * @param useInterpolation
	 *            If true bilinear interpolation is used (default). Otherwise
	 *            the faster nearest neighbor interpolation is used.
	 * @return true if the shape is inside the image
	 */
	public boolean Analyze(final CAAMShape shape, ModelSimpleImage refImg,
			final boolean useInterpolation) {

		// setup
		CDVector texture = new CDVector();

		Analyze(shape, texture);

		// convert texture vector to reference image
		refImg = m_pReferenceFrame.Vector2Image(texture, refImg);

		return true;
	}

	/**
	 * Renders a texture vector into a shape. This method renders a texture
	 * vector into a shape defined in image coordinates.
	 * 
	 * @param shape
	 *            The shape to synthesize into.
	 * @param texture
	 *            The input image texture.
	 * @param destImage
	 *            Destination image
	 * @param renderOntoImage
	 *            If true the synthesization is done on top of the existing
	 *            image.
	 * @return True on success.
	 */
	public final boolean Synthesize(final CAAMShape shape,
			final CDVector texture, ModelSimpleImage destImage,
			boolean renderOntoImage) {

		ModelSimpleImage maskImg = m_pReferenceFrame.MaskImage();
		int[] extents = new int[2];
		extents[0] = maskImg.Width();
		extents[1] = maskImg.Height();

		ModelSimpleImage refImage = new ModelSimpleImage(extents);

		// make shape free image
		refImage = m_pReferenceFrame.Vector2Image(texture, refImage);

		// mirror the edge to avoid warped points that falls
		// just outside the reference image mask
		CAAMUtil.MirrorEdge(refImage, m_pReferenceFrame.MaskImage(), 1);

		double shapeWidth = shape.Width();
		double shapeHeight = shape.Height();
		if (!renderOntoImage) {

			// init destination image
			int[] dimExtents = new int[2];
			dimExtents[0] = (int) (shapeWidth + 1);
			dimExtents[1] = (int) (shapeHeight + 1);
			destImage = new ModelSimpleImage(dimExtents);
		}

		// config
		// bool add_noise = renderOntoImage;
		boolean add_noise = false;
		double noise_amplitude = 0;
		if (add_noise) {
			noise_amplitude = .1 * texture.Mean(); // add 10% of the texture
													// mean
		}

		// setup warp
		CAAMWarpLinear warp = new CAAMWarpLinear(false);

		warp.UseConvexHull(m_pReferenceFrame.UseConvexHull());
		warp.SetSrcShape(shape);
		warp.SetDestShape(m_pReferenceFrame.RefShape());

		// misc setup
		CAAMPoint out = new CAAMPoint();

		double xmin = shape.MinX(), ymin = shape.MinY();
		int refWidth = refImage.Width();
		int refHeight = refImage.Height();
		int offsetX = renderOntoImage ? AAMdef.AAMRound(xmin) : 0;
		int offsetY = renderOntoImage ? AAMdef.AAMRound(ymin) : 0;

		// correct for subpixel placement of shape in the destination image
		int srcOffsetX = (int) (renderOntoImage ? xmin - (xmin - offsetX)
				: xmin);
		int srcOffsetY = (int) (renderOntoImage ? ymin - (ymin - offsetY)
				: ymin);

		// warp pixels
		for (int y = 0; y <= shapeHeight; y++) {

			for (int x = 0; x <= shapeWidth; x++) {

				boolean ok = warp.Warp(new CAAMPoint(x + srcOffsetX, y
						+ srcOffsetY), out);
				if (ok) {

					// pixel is inside shape

					// skip pixels at the edge to save
					// the bilinear interpolation from a slow
					// and painful death
					if ((int) (out.x) >= refWidth - 1
							|| (int) (out.y) >= refHeight - 1
							|| (int) (out.x) == 0 || (int) (out.y) == 0) {

						// skip
						continue;
					}

					double[] p = new double[1];
					// do bilinear sampling
					p[0] = refImage.Pixel1((float) out.x, (float) out.y);

					if (add_noise && renderOntoImage) {
						// add monochromatic noise
						double r = noise_amplitude
								* (Math.random() / (double) CDMatrix.RAND_MAX - .5);
						double val = p[0] + r;
						p[0] = val > 255. ? 255. : (val < 0. ? 0. : val);
					}

					// write the result back to the destination image
					destImage.setValue(x + offsetX, y + offsetY, (float) p[0]);
				}
			}
		}

		return true;
	}

	/**
	 * Clones itself.
	 * 
	 * @param rf
	 *            The reference frame of the cloned object.
	 * @return A cloned object created on the heap.
	 */
	public CAAMAnalyzeSynthesize Clone(final CAAMReferenceFrame rf) {

		CAAMAnalyzeSynthesizeSoftware obj;

		obj = new CAAMAnalyzeSynthesizeSoftware(rf);

		return obj;
	}

};
