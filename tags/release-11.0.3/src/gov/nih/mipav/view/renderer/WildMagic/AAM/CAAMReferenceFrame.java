package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;
import java.io.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.FileUtility;

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
 * The geometrical reference frame (or shape-free frame). This class defined the
 * geomtrical reference frame of an AAM. Hence, it is the spatial layout where
 * all texture sampling takes place.
 * 
 * The main objective of this class is to provide fast conversions from a
 * shape-free image to a texture vector and vice versa.
 * 
 * @author Ruida Cheng
 * 
 */

public class CAAMReferenceFrame {

	/**
	 * defines a sub-part of a horizontal line in an image
	 */
	class sScanLinePart {
		int y, x1, x2; // x1 and x2 are inclusive

		public sScanLinePart() {

		}

		public void assign(sScanLinePart s) {
			this.y = s.y;
			this.x1 = s.x1;
			this.x2 = s.x2;
		}

	}

	/** reference shape. */
	private CAAMShape m_pReferenceShape;

	/** reference mesh. */
	;
	private CAAMMesh m_pReferenceMesh;

	/** mask image. */
	private ModelSimpleImage m_pMaskImage;

	/** scan lines. */
	private Vector<sScanLinePart> m_ScanlineParts = new Vector<sScanLinePart>();

	/** number of texture samples. */
	private int m_iTextureSamples;

	/** flag to use convex hull. */
	private boolean m_bUseConvexHull;

	/**
	 * Constructor.
	 * 
	 * @doc Constructor. Sets up an empty reference frame. Use Setup() to make
	 *      such an object any useful.
	 */
	public CAAMReferenceFrame() {

		m_bUseConvexHull = true;
		m_pReferenceShape = null;
		m_pMaskImage = null;
		m_pReferenceMesh = null;
	}

	/**
	 * Assignment operator.
	 * 
	 * @param rf
	 *            Object to copy from.
	 */
	public CAAMReferenceFrame assign(final CAAMReferenceFrame rf) {

		// deallocate members
		if (m_pReferenceShape != null)
			m_pReferenceShape = null;
		if (m_pReferenceMesh != null)
			m_pReferenceMesh = null;
		if (m_pMaskImage != null)
			m_pMaskImage = null;

		// deep copy of source data members
		m_bUseConvexHull = rf.m_bUseConvexHull;
		m_iTextureSamples = rf.m_iTextureSamples;

		ModelSimpleImage maskImg = rf.m_pMaskImage;
		int[] extents = new int[2];
		extents[0] = maskImg.Width();
		extents[1] = maskImg.Height();
		m_pMaskImage = new ModelSimpleImage(extents);
		m_pMaskImage = (ModelSimpleImage) (rf.m_pMaskImage.clone());

		m_pReferenceMesh = new CAAMMesh();
		m_pReferenceMesh.assign(rf.m_pReferenceMesh); // Ruida Cheng:
														// CAAMMesh.assign

		m_pReferenceShape = new CAAMShape();
		m_pReferenceShape.assign(rf.m_pReferenceShape);

		// m_ScanlineParts = rf.m_ScanlineParts;
		m_ScanlineParts.clear();
		int len = rf.m_ScanlineParts.size();
		for (int i = 0; i < len; i++) {
			sScanLinePart temp = new sScanLinePart();
			temp.assign(rf.m_ScanlineParts.get(i));
			m_ScanlineParts.add(temp);
		}

		return this;
	}

	/**
	 * Copy constructor.
	 * 
	 * @param rf
	 *            Reference frame object to be copied by construction.
	 */
	public CAAMReferenceFrame(final CAAMReferenceFrame rf) {

		m_pReferenceShape = null;
		m_pReferenceMesh = null;
		m_pMaskImage = null;

		this.assign(rf);
	}

	/**
	 * Sets up the class with the reference shape and information about how to
	 * determine the extent of the shape.
	 * 
	 * @param referenceShape
	 *            The reference shape.
	 * @param useConvexHull
	 *            If true the convex hull of the reference shape is used to
	 *            determine its extent.
	 */
	public void Setup(final CAAMShape referenceShape,
			final boolean useConvexHull) {

		if (m_pReferenceShape != null)
			m_pReferenceShape = null;
		if (m_pReferenceMesh != null)
			m_pReferenceMesh = null;
		if (m_pMaskImage != null)
			m_pMaskImage = null;

		m_pReferenceShape = new CAAMShape(referenceShape);
		m_bUseConvexHull = useConvexHull;

		// move shape to fourth quadrant
		double xMin = m_pReferenceShape.MinX();
		double yMin = m_pReferenceShape.MinY();
		m_pReferenceShape.Translate(-xMin, -yMin);

		// calculate mask image and scanline info
		CalcMaskImage(m_bUseConvexHull);
		CalcScanLines();
	}

	/**
	 * Writes the class data to disk. This method writes the class data to disk.
	 * Actually it is only the reference shape and the convex hull option that
	 * is written. The rest is reconstructed during read.
	 * 
	 * @param fh
	 *            Open file handle to a binary file.
	 */
	public void ToFile(final DataOutputStream fh) {

		int i = m_bUseConvexHull ? 1 : 0;

		m_pReferenceShape.ToFile(fh);
		try {
			// fwrite( &i, sizeof(int), 1, fh );
			fh.writeInt(i);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Reads the class data form disk. This method reads the class data from
	 * disk. Actually it is only the reference shape and the convex hull option
	 * that is read. The rest is reconstructed.
	 * 
	 * @param fh
	 *            Open file handle to a binary file.
	 */
	public void FromFile(DataInputStream fh) {

		int i = 0;
		CAAMShape shape = new CAAMShape();

		// read data
		shape.FromFile(fh);
		try {
			// fread( &i, sizeof(int), 1, fh );
			i = fh.readInt();
		} catch (IOException e) {
			e.printStackTrace();
		}
		// reconstruct remaining data members
		Setup(shape, i == 1);
	}

	/**
	 * Writes some of the class data to disk.
	 * 
	 * @doc This method writes the reference shape, the mask image and a test of
	 *      the Image2Vector() and Vector2image() to the current directory.
	 */
	public void DebugDump() {

		System.err.println("mask size: " + m_pMaskImage.Width() + "x"
				+ m_pMaskImage.Height());
		m_pReferenceShape.SetHostImage("refImage.bmp");
		m_pReferenceShape.WriteASF("ref.asf", m_pMaskImage.Width(),
				m_pMaskImage.Height());
		// m_pMaskImage.WriteBandedFile( "ref.bmp" );
		String dir = System.getProperties().getProperty("user.dir");
		String filename = "ref.bmp";
		ModelImage refImage = new ModelImage(m_pMaskImage, filename);
		refImage.saveImage(dir, filename, FileUtility.BMP, false);

		CDVector v = new CDVector();
		Image2Vector(m_pMaskImage, v);
		v.ToMatlab("vv.m", "v", "", false);

		int[] extents = new int[2];
		extents[0] = m_pMaskImage.Width();
		extents[1] = m_pMaskImage.Height();
		ModelSimpleImage img2 = new ModelSimpleImage(extents);
		img2 = Vector2Image(v, img2);
		// img2.WriteBandedFile( "img2.bmp" );
		filename = "img2.bmp";
		ModelImage img2Image = new ModelImage(img2, filename);
		img2Image.saveImage(dir, filename, FileUtility.BMP, false);
	}

	/**
	 * Deletes any reference shape, mesh and mask image.
	 * 
	 * @return Nothing.
	 */
	public void dispose() {

		if (m_pReferenceShape != null)
			m_pReferenceShape = null;
		if (m_pReferenceMesh != null)
			m_pReferenceMesh = null;
		if (m_pMaskImage != null)
			m_pMaskImage = null;
	}

	/**
	 * Calculates a reference mask image. This method calculates a reference
	 * mask image, where the shape area is set to 255 and the background to
	 * zero. The resulting image is stored in 'm_pMaskImage'.
	 * 
	 * @param useConvexHull
	 *            If true the convex hull is used to define the extent of the
	 *            reference shape.
	 */
	public void CalcMaskImage(final boolean useConvexHull) {

		// setup
		double[] p255 = new double[1];

		p255[0] = 255;
		if (m_pMaskImage != null)
			m_pMaskImage = null;

		// allocate mask image
		int width = (int) Math.ceil(m_pReferenceShape.MaxX());
		int height = (int) Math.ceil(m_pReferenceShape.MaxY());
		int[] extents = new int[2];
		extents[0] = width;
		extents[1] = height;
		m_pMaskImage = new ModelSimpleImage(extents);

		// set background to black
		m_pMaskImage.FillPixels(0);

		// make reference mesh
		m_pReferenceMesh = new CAAMMesh();
		// Delaunay
		CAAMDelaunay.MakeMesh(m_pReferenceShape, m_pReferenceMesh,
				useConvexHull == true);

		// int c = 0;

		// calc mask image (set shape area to 255)
		for (int y = 0; y < height; y++) {

			for (int x = 0; x < width; x++) {

				if (m_pReferenceMesh.IsInside(new CAAMPoint(x, y))) {
					// System.err.println("x = " + x + "   y = " + y);
					m_pMaskImage.setValue(x, y, 255);
					// ++c;
				}
			}
		}
	}

	/**
	 * Calculates the scanlines of the mask image. This method calculates the
	 * scanlines of the mask image. A scanline is a subpart of a horsontal image
	 * line that is occupied but a subpart of the reference shape.
	 * 
	 * The resulting scanlines are used for quick'n'easy conversion from spatial
	 * to vector layout (and back).
	 * 
	 * Scanlines are stored in 'm_ScanlineParts'.
	 */
	public void CalcScanLines() {

		int width = m_pMaskImage.Width();
		int height = m_pMaskImage.Height();

		// calc scanline vector
		m_iTextureSamples = 0;
		for (int y = 0; y < height; y++) {

			int x = 0;
			do {

				sScanLinePart s = new sScanLinePart();
				s.x1 = -1;
				s.x2 = -1;
				s.y = y;
				// System.err.println(" x = " + x + "  y = " + y);
				while (x < width && m_pMaskImage.getValue(x, y) != 255)
					++x;

				if (x != width) {

					s.x1 = x;
					while (x < width && m_pMaskImage.getValue(x, y) == 255)
						++x;
					s.x2 = x - 1;
				}
				if (s.x1 != -1) {

					m_ScanlineParts.add(s);
					m_iTextureSamples += s.x2 - s.x1 + 1;
				}

			} while (x < width);
		}

		m_iTextureSamples *= AAMdef.BANDS; // one sample per band
	}

	/**
	 * Returns the width of the reference shape.
	 * 
	 * @return The width.
	 */
	public double RefShapeWidth() {

		return m_pReferenceShape.Width();
	}

	/**
	 * Returns the height of the reference shape.
	 * 
	 * @return The height.
	 */
	public double RefShapeHeight() {

		return m_pReferenceShape.Height();
	}

	/**
	 * Conversion from shape-free image to a texture vector. This method
	 * performs a quick conversion from a shape-free image to a texture vector.
	 * 
	 * @param refImg
	 *            Destination reference image.
	 * @param v
	 *            Input texture vector.
	 */
	public void Image2Vector(final ModelSimpleImage refImg, CDVector v) {

		assert (refImg.Width() == m_pMaskImage.Width());
		assert (refImg.Height() == m_pMaskImage.Height());

		if (v.Length() != m_iTextureSamples) {

			v.Resize(m_iTextureSamples);
		}

		boolean safe = false;
		if (safe) {

			// safe version: does not assume anything about the
			// memory layout of the image or the vector (slow)
			int c = 0, n = m_ScanlineParts.size();
			for (int i = 0; i < n; i++) {

				int y = m_ScanlineParts.get(i).y;
				int end = m_ScanlineParts.get(i).x2;

				for (int x = m_ScanlineParts.get(i).x1; x <= end; x++) {

					// for(int k=0;k<AAMdef.BANDS;k++) {

					v.m_data[c++] = refImg.getValue(x, y);
					// }
				}
			}
			assert (c == m_iTextureSamples);

		} else {
			int n = m_ScanlineParts.size();
			// double []pPixels = ((ModelSimpleImage)refImg).Pixel(0,0,0);
			float[] pPixels = new float[refImg.Width() * refImg.Height()];
			refImg.exportData(pPixels, 0, 0);
			// double []pScanline;
			int scanLineWidth = refImg.Width();
			double[] pVec = v.m_data;
			int pVec_index = 0;
			for (int i = 0; i < n; i++) {

				int x = m_ScanlineParts.get(i).x1;
				int w = (m_ScanlineParts.get(i).x2 - x + 1);
				// pScanline =
				// pPixels[m_ScanlineParts.get(i).y*scanLineWidth+x];
				int pScanline_index = m_ScanlineParts.get(i).y * scanLineWidth
						+ x;
				int j;
				for (j = 0; j < w; j++) {

					// the expensive part here is to access the
					// rather large amount of data in pVec
					//
					// cache prefecting has been tried on the
					// Athlon FB 1.1 GHz with little effect :-(
					pVec[j + pVec_index] = pPixels[j + pScanline_index];
				}
				pVec_index += j;
			}
		}
	}

	/**
	 * Conversion from a texture vector to a shape-free image. This method
	 * performs a quick conversion from a texture vector to a shape-free image.
	 * 
	 * @param refImg
	 *            Destination reference image.
	 * @param v
	 *            Input texture vector.
	 */
	public ModelSimpleImage Vector2Image(final CDVector v,
			ModelSimpleImage outImg) {

		if (outImg.Width() != m_pMaskImage.Width()
				|| outImg.Height() != m_pMaskImage.Height()) {

			int[] extents = new int[2];
			extents[0] = m_pMaskImage.Width();
			extents[1] = m_pMaskImage.Height();

			outImg = new ModelSimpleImage(extents);

		}

		int c = 0, n = m_ScanlineParts.size();
		for (int i = 0; i < n; i++) {

			int y = m_ScanlineParts.get(i).y;
			int end = m_ScanlineParts.get(i).x2;

			for (int x = m_ScanlineParts.get(i).x1; x <= end; x++) {

				// for(int k=0;k<AAMdef.BANDS;k++) {

				outImg.setValue(x, y, (float) v.m_data[c++]);
				// }
			}
		}

		assert (c == m_iTextureSamples);
		// Ruida

		// ModelImage temp = new ModelImage(outImg, "temp");
		// new ViewJFrameImage(temp);

		return outImg;
	}

	/**
	 * Conversion from a texture vector to a shape-free matrix. This method
	 * performs a quick conversion from a texture vector to a shape-free image
	 * on matrix form.
	 * 
	 * Will currently only work on single band AAMs.
	 * 
	 * @param refImg
	 *            Destination reference image.
	 * @param v
	 *            Input texture vector.
	 */
	public void Vector2Matrix(final CDVector v, CDMatrix m) {

		assert (AAMdef.BANDS == 1);

		m.Resize(m_pMaskImage.Height(), m_pMaskImage.Width());

		int c = 0, n = m_ScanlineParts.size();
		for (int i = 0; i < n; i++) {

			int y = m_ScanlineParts.get(i).y;
			int end = m_ScanlineParts.get(i).x2;

			for (int x = m_ScanlineParts.get(i).x1; x <= end; x++) {

				m.m_data[y][x] = v.m_data[c++];
			}
		}

		assert (c == m_iTextureSamples);
	}

	/**
	 * Returns the width of the reference image.
	 * 
	 * @return The width.
	 */
	public int RefImageWidth() {

		return m_pMaskImage.Width();
	}

	/**
	 * Returns the height of the reference image.
	 * 
	 * @return The height.
	 */
	public int RefImageHeight() {

		return m_pMaskImage.Height();
	}

	/**
	 * Find the corresponding triangle in the source mesh for a point.
	 * 
	 * @param p
	 *            Input point
	 * @param triangle
	 *            Triangle index.
	 * @param alpha
	 *            Relative position on triangle (barycentric coordinate).
	 * @param beta
	 *            Relative position on triangle (barycentric coordinate).
	 * @param gamma
	 *            Relative position on triangle (barycentric coordinate).
	 * @return True if the point is inside the source mesh.
	 */
	public boolean FindTriangle(final CAAMPoint p, int[] triangle,
			double[] alpha, double[] beta, double[] gamma) {

		int[] t = new int[1];
		boolean ret;

		ret = m_pReferenceMesh.IsInside(p, t, alpha, beta, gamma);
		triangle[0] = t[0];

		return ret;
	}

	/**
	 * Returns true of the convex hull determines the shape extent.
	 * 
	 * @return flag to indiciate to use convexHull or not.
	 */
	public boolean UseConvexHull() {
		return m_bUseConvexHull;
	}

	/**
	 * Returns the reference shape.
	 * 
	 * @return shape
	 */
	public final CAAMShape RefShape() {
		return m_pReferenceShape;
	}

	/**
	 * Returns the number of texture samples in the texture model.
	 * 
	 * @return number of texture samples
	 */
	public int NTextureSamples() {
		return m_iTextureSamples;
	}

	/**
	 * Returns the reference mesh.
	 * 
	 * @return mesh
	 */
	public final CAAMMesh RefMesh() {
		return m_pReferenceMesh;
	}

	/**
	 * Returns the mask image.
	 * 
	 * @return mask image
	 */
	public final ModelSimpleImage MaskImage() {
		return m_pMaskImage;
	}

	/**
	 * Direct access to the scanline parts (not commonly used)
	 * 
	 * @return scanline
	 */
	public final Vector<sScanLinePart> ScanlineParts() {
		return m_ScanlineParts;
	}

}