package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;
import java.io.*;

import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;
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
 * Class that visualizes different aspects of a model. For visualization of e.g.
 * annotations without a model see CAAMUtil.
 * 
 * @author Ruida Cheng
 */
public class CAAMVisualizer {

	/** AAM model reference. */
	private CAAMModel m_pModel;

	/**
	 * Constructor. Constructor that sets the model pointer.
	 * 
	 * @param pModel
	 *            Pointer to the model that you want visualised.
	 */
	public CAAMVisualizer(final CAAMModel pModel) {
		m_pModel = pModel;
	}

	/**
	 * dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Wrapper to visualizes the iterations during a model search.
	 * 
	 * @param optStates
	 *            optimization state vector
	 * @param img
	 *            image
	 * @param filename
	 *            file name.
	 */
	public void OptimizationMovie(final Vector<CAAMOptState> optStates,
			final ModelSimpleImage img, final String filename) {
		OptimizationMovie(optStates, img, filename, 5);
	}

	/**
	 * Visualizes the iterations during a model search. This method visualizes
	 * the iterations during a model search. Output format is an AVI-file that
	 * you can put into e.g. Powerpoint to impress your supervisor with.
	 * 
	 * @param optStates
	 *            An array of optimization states.
	 * @param img
	 *            The image used for model searching.
	 * @param filename
	 *            The output movie filename, e.g. 'search.avi'.
	 * @param frameRate
	 *            The number of frame per second.
	 */
	public void OptimizationMovie(final Vector<CAAMOptState> optStates,
			final ModelSimpleImage img, final String filename,
			final int frameRate) {

		int[] extents = new int[2];
		extents[0] = img.Width();
		extents[1] = img.Height();
		int size = extents[0] * extents[1];

		ModelSimpleImage frame = new ModelSimpleImage(extents);

		int n = optStates.size();

		int[] newExtents = new int[3];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];
		newExtents[2] = n;
		try {
			ModelImage image = new ModelImage(ModelStorageBase.FLOAT,
					newExtents, filename);
			/*
			 * FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[1];
			 * 
			 * fileInfoBases[0] = new FileInfoImageXML("temp.xml", null,
			 * FileUtility.XML);
			 * 
			 * fileInfoBases[0].setEndianess(imageYEnd);
			 * fileInfoBases[i].setUnitsOfMeasure(units);
			 * fileInfoBases[i].setResolutions(redChannelsImageResols);
			 * 
			 * fileInfoBases[i].setOrigin(orig);
			 * 
			 * fileInfoBases[0].setExtents(newExtents);
			 * fileInfoBases[0].setDataType(ModelStorageBase.FLOAT); //
			 * fileInfoBases[i].setFileDirectory(dir);
			 * 
			 * image.setFileInfo(fileInfoBases);
			 */
			// render frames
			for (int i = 0; i < n; i++) {
				float[] buffer = new float[size];
				// img.CopyPixelsTo( frame );
				img.exportData(buffer, 0, size);
				frame.importData(buffer, 0, size);
				m_pModel.ModelImage(optStates.get(i).c, frame,
						optStates.get(i).shape, true);

				float[] temp = new float[size];
				frame.exportData(temp, 0, size);
				image.importData(i * size, temp, false);
				// frame.CopyPixelsToRGBA( rgba );
				// avi.WriteFrame( rgba );
			}
			// close avi
			// avi.Close();
			// new ViewJFrameImage(image);

			image.saveImage("", filename, FileUtility.XML, false);
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	/**
	 * Plots a shape into an image and save it to disk.
	 * 
	 * @param img
	 *            Image to plot shape onto.
	 * @param shape
	 *            Shape in image coordinates.
	 * @param fileman
	 *            Destination image file, e.g. 'result.bmp'.
	 */
	public static void ShapeStill(final ModelSimpleImage img,
			final CAAMShape shape, final String filename) {

		int[] extents = new int[2];
		extents[0] = img.Width();
		extents[1] = img.Height();
		int size = extents[0] * extents[1];
		float[] buffer = new float[size];
		ModelSimpleImage resImage = new ModelSimpleImage(extents);
		img.exportData(buffer, 0, size);
		resImage.importData(buffer, 0, size);

		double[] p255 = new double[AAMdef.BANDS];
		p255[0] = 255;

		CAAMUtil.PlotShapeIntoImage(resImage, shape, p255, p255, false, false,
				false);
		ModelImage image = new ModelImage(resImage, filename);
		image.saveImage("", filename, FileUtility.XML, false);
	}

	/**
	 * Writes the texture eigen modes as shape-free images. This method converts
	 * each column of the texture eigen vectors into corresponding shape-free
	 * images and save them to disk in the BMP format.
	 * 
	 * @return Nothing.
	 */
	public void WriteEigenImages() {

		int nmodes = m_pModel.TexturePCA().NParameters();
		ModelSimpleImage img = new ModelSimpleImage();
		CDVector texture = new CDVector();
		for (int i = 0; i < nmodes; i++) {

			m_pModel.TexturePCA().EigenVectors().Col(nmodes - i - 1, texture); // take
																				// largest
																				// first
			img = m_pModel.ShapeFreeImage(texture, img);
			String str = new String();
			// str.format("eigen_image%02i.bmp", i );
			str = "eigen_image" + i + ".xml";
			ModelImage image = new ModelImage(img, str);
			image.saveImage("", str, FileUtility.XML, false);
		}
	}

	/**
	 * Writes the parameter update matrix as shape-free images. This method
	 * converts each column of the parameter update matrices into corresponding
	 * shape-free images and save them to disk in the BMP format.
	 */
	public void WritePredictionImages() {

		String str = new String();

		ModelSimpleImage img = new ModelSimpleImage();
		CDVector texture = new CDVector();
		int n = m_pModel.Rc().NRows();
		for (int i = 0; i < n; i++) {

			m_pModel.Rc().Row(n - i - 1, texture); // take largest first
			img = m_pModel.ShapeFreeImage(texture, img);
			// str.format("Rc_image%02i.bmp", i );
			str = "Rc_image" + i + ".xml";
			ModelImage image = new ModelImage(img, str);
			image.saveImage("", str, FileUtility.XML, false);
		}
		n = m_pModel.Rt().NRows();
		for (int i = 0; i < n; i++) {

			m_pModel.Rt().Row(i, texture);
			img = m_pModel.ShapeFreeImage(texture, img);
			// str.format("Rt_image%02i.bmp", i );
			str = "Rt_image" + i + ".xml";
			ModelImage image = new ModelImage(img, str);
			image.saveImage("", str, FileUtility.XML, false);
		}
	}

	/**
	 * Generates movies files showing each mode of texture variation by
	 * adjusting the each mode of variation to
	 * 
	 * +/- 'range' times <the mode standard deviation>
	 * 
	 * in 2*'step'+1 steps.
	 * 
	 * @see Shape, Combined
	 * @param filename
	 *            The output filename excl. extension.
	 * @param nbModes
	 *            The number of modes to generate movies from.
	 * @param range
	 *            The rendered range in standard deviations.
	 * @param steps
	 *            The number of frames to generate to reach 'range'. Thus the
	 *            total number of frames will be 2*step+1. (the +1 term is the
	 *            mean texture).
	 * @param whiteBackground
	 *            If true the background is rendered in white. Default is black.
	 * @return true on success, false on file errors.
	 */
	public boolean TextureMovie(final String filename, final int nbModes,
			final double range, final int steps, final boolean whiteBackground) {

		boolean success = true;
		CDVector texVec = new CDVector();
		double min, max, pVal;
		int nbAllModes = m_pModel.TexturePCA().NParameters();
		CDVector b_g = new CDVector(nbAllModes);
		CDVector c = new CDVector(0);
		int nFrames = steps * 2 + 1;

		for (int i = nbAllModes - 1; i >= nbAllModes
				- Math.min(nbModes, nbAllModes); i--) {

			double ev_sqr = Math.sqrt(m_pModel.TexturePCA().EigenValues()
					.get(i));
			min = -range * ev_sqr;
			max = range * ev_sqr;

			String fn = new String();
			// fn.format("%s%02i.avi", filename, nbAllModes-i );
			fn = filename + (nbAllModes - i) + ".xml";

			ModelSimpleImage first = new ModelSimpleImage();

			pVal = (double) 0 / (nFrames - 1) * (max - min) + min;
			b_g.assign(.0);
			b_g.m_data[i] = pVal;

			m_pModel.TexturePCA().Deform(b_g, texVec);
			first = m_pModel.ShapeFreeImage(texVec, first);

			int[] extents = first.extents;
			int[] newExtents = new int[3];
			newExtents[0] = extents[0];
			newExtents[1] = extents[1];
			newExtents[2] = nFrames;

			ModelImage image = new ModelImage(ModelStorageBase.FLOAT,
					newExtents, fn);

			try {

				// avi.Open( fn, OF_CREATE|OF_WRITE );
				for (int x = 0; x < nFrames; x++) {

					pVal = (double) x / (nFrames - 1) * (max - min) + min;
					b_g.assign(.0);
					b_g.m_data[i] = pVal;

					ModelSimpleImage frame = new ModelSimpleImage();
					if (whiteBackground) {
						frame.FillPixels(255);
					}
					m_pModel.TexturePCA().Deform(b_g, texVec);

					int size = extents[0] * extents[1];
					float[] buffer = new float[size];

					// do the sampling
					frame = m_pModel.ShapeFreeImage(texVec, frame);

					frame.exportData(buffer, 0, size);
					image.importData(x * size, buffer, false);
				}
				image.saveImage("", fn, FileUtility.XML, false);
			} catch (IOException e) {

				e.printStackTrace();
				success = false;
			}
		}

		return success;
	}

	/**
	 * Generates movies files showing each mode of shape variation by adjusting
	 * the each mode of variation to
	 * 
	 * +/- 'range' times <the mode standard deviation>
	 * 
	 * in 2*'step'+1 steps.
	 * 
	 * @param filename
	 *            The output filename excl. extension.
	 * @param nbModes
	 *            The number of modes to generate movies from.
	 * @param range
	 *            The rendered range in standard deviations.
	 * @param steps
	 *            The number of frames to generate to reach 'range'. Thus the
	 *            total number of frames will be 2*step+1. (the +1 term is the
	 *            mean shape).
	 * @param whiteBackground
	 *            If true the background is rendered in white. Default is black.
	 * @return true on success, false on file errors.
	 */
	public boolean ShapeMovie(String filename, final int nbModes,
			final double range, final int steps, final boolean whiteBackground) {

		boolean success = true;
		double min, max, pVal;
		int nbAllModes = m_pModel.ShapePCA().NParameters();
		CDVector b_s = new CDVector(nbAllModes);
		CDVector c = new CDVector();
		CAAMShape shape = new CAAMShape();
		CAAMShape s1 = new CAAMShape();
		CAAMShape s2 = new CAAMShape();
		int nFrames = steps * 2 + 1;

		// denormalize mean texture
		CDVector meantex = new CDVector(m_pModel.MeanTexture().Length());
		meantex = m_pModel.MeanTexture();

		// setup
		int border = 0;

		for (int i = nbAllModes - 1; i >= nbAllModes
				- Math.min(nbModes, nbAllModes); i--) {

			double ev_sqr = Math.sqrt(m_pModel.ShapePCA().EigenValues().get(i));
			min = -range * ev_sqr;
			max = range * ev_sqr;

			// calc shape bounding box
			b_s.assign(.0);
			b_s.m_data[i] = min;
			m_pModel.ShapePCAInstance(b_s, s1);
			s1.Scale(m_pModel.MeanShapeSize());
			b_s.m_data[i] = max;
			m_pModel.ShapePCAInstance(b_s, s2);
			s2.Scale(m_pModel.MeanShapeSize());
			double xMin = Math.min(s1.MinX(), s2.MinX());
			double xMax = Math.max(s1.MaxX(), s2.MaxX());
			double yMin = Math.min(s1.MinY(), s2.MinY());
			double yMax = Math.max(s1.MaxY(), s2.MaxY());
			int width = (int) Math.ceil(xMax - xMin) + 2 * border;
			int height = (int) Math.ceil(yMax - yMin) + 2 * border;

			try {

				String fn = new String();
				fn = filename + (nbAllModes - i) + ".xml";

				int[] newExtents = new int[3];
				newExtents[0] = width;
				newExtents[1] = height;
				newExtents[2] = nFrames;

				ModelImage image = new ModelImage(ModelStorageBase.FLOAT,
						newExtents, fn);

				for (int x = 0; x < nFrames; x++) {

					pVal = (double) x / (nFrames - 1) * (max - min) + min;
					b_s.assign(.0);
					b_s.m_data[i] = pVal;

					m_pModel.ShapePCAInstance(b_s, shape);
					shape.Scale(m_pModel.MeanShapeSize());
					shape.Translate(-xMin + border, -yMin + border);

					int[] extents = new int[2];
					extents[0] = width;
					extents[1] = height;
					ModelSimpleImage frame = new ModelSimpleImage(extents);
					if (whiteBackground) {
						frame.FillPixels(255);
					}

					m_pModel.ModelImageEx(shape, meantex, frame, true, false);

					int size = width * height;
					float[] buffer = new float[size];

					frame.exportData(buffer, 0, size);
					image.importData(x * size, buffer, false);
				}
				image.saveImage("", fn, FileUtility.XML, false);
			} catch (IOException e) {

				e.printStackTrace();
				success = false;
			}
		}

		return success;
	}

	/**
	 * Generates movies files showing each mode of combined shape and texture
	 * variation by adjusting the each mode of variation to:
	 * 
	 * +/- 'range' times <the mode standard deviation>
	 * 
	 * in 2*'step'+1 steps.
	 * 
	 * @param filename
	 *            The output filename excl. extension.
	 * @param nbModes
	 *            The number of modes to generate movies from.
	 * @param range
	 *            The rendered range in standard deviations.
	 * @param steps
	 *            The number of frames to generate to reach 'range'. Thus the
	 *            total number of frames will be 2*step+1. (the +1 term is the
	 *            mean shape).
	 * @param whiteBackground
	 *            If true the background is rendered in white. Default is black.
	 * @return true on success, false on file errors.
	 */
	public boolean CombinedMovie(final String filename, final int nbModes,
			final double range, final int steps, final boolean whiteBackground) {

		boolean success = true;
		double min, max, pVal;
		int nbAllModes = m_pModel.CombinedPCA().NParameters();
		CDVector c = new CDVector(nbAllModes);
		int nFrames = steps * 2 + 1;
		CAAMShape shape = new CAAMShape();
		CAAMShape s1 = new CAAMShape();
		CAAMShape s2 = new CAAMShape();

		// setup
		int border = 0;

		for (int i = nbAllModes - 1; i >= nbAllModes
				- Math.min(nbModes, nbAllModes); i--) {

			double ev_sqr = Math.sqrt(m_pModel.CombinedPCA().EigenValues()
					.get(i));
			min = -range * ev_sqr;
			max = range * ev_sqr;

			// calc shape bounding box
			c.assign(.0);
			c.m_data[i] = min;
			m_pModel.ShapeInstance(c, s1);
			s1.Scale(m_pModel.MeanShapeSize());
			c.m_data[i] = max;
			m_pModel.ShapeInstance(c, s2);
			s2.Scale(m_pModel.MeanShapeSize());
			double xMin = Math.min(s1.MinX(), s2.MinX());
			double xMax = Math.max(s1.MaxX(), s2.MaxX());
			double yMin = Math.min(s1.MinY(), s2.MinY());
			double yMax = Math.max(s1.MaxY(), s2.MaxY());
			int width = (int) Math.ceil(xMax - xMin) + 2 * border;
			int height = (int) Math.ceil(yMax - yMin) + 2 * border;

			try {

				String fn = new String();
				fn = filename + (nbAllModes - i) + ".xml";

				int[] newExtents = new int[3];
				newExtents[0] = width;
				newExtents[1] = height;
				newExtents[2] = nFrames;

				ModelImage image = new ModelImage(ModelStorageBase.FLOAT,
						newExtents, fn);

				for (int x = 0; x < nFrames; x++) {

					c.assign(0);
					pVal = (double) x / (nFrames - 1) * (max - min) + min;
					c.m_data[i] = pVal;

					int[] extents = new int[2];
					extents[0] = width;
					extents[1] = height;
					ModelSimpleImage frame = new ModelSimpleImage(extents);
					if (whiteBackground) {
						frame.FillPixels(255);
					}

					m_pModel.ShapeInstance(c, shape);
					shape.Scale(m_pModel.MeanShapeSize());
					shape.Translate(-xMin + border, -yMin + border);

					m_pModel.ModelImage(c, frame, shape, false);

					int size = width * height;
					float[] buffer = new float[size];

					frame.exportData(buffer, 0, size);
					image.importData(x * size, buffer, false);
				}

				image.saveImage("", fn, FileUtility.XML, false);

			} catch (IOException e) {

				e.printStackTrace();
				success = false;
			}
		}

		return success;
	}

	/**
	 * Generates a movie of the registered training set. Generates a movie of
	 * the registered training set by warping each texture to the mean shape and
	 * output this to an AVI-file.
	 * 
	 * @param filename
	 *            The output filename.
	 * @param m_vTexture
	 *            The vector of textures.
	 * @return true on success, false on file errors.
	 */
	public boolean RegistrationMovie(final String filename,
			final Vector<CDVector> vTexture) {

		boolean success = true;

		try {
			// generate shapefree images and write to avi
			int numberImg = vTexture.size();

			ModelSimpleImage first = new ModelSimpleImage();
			CDVector temp = new CDVector(vTexture.get(0));
			first = m_pModel.ShapeFreeImage(temp, first);
			int[] extents = first.extents;
			int[] newExtents = new int[3];
			newExtents[0] = extents[0];
			newExtents[1] = extents[1];
			newExtents[2] = numberImg;

			ModelImage image = new ModelImage(ModelStorageBase.FLOAT,
					newExtents, filename);

			for (int i = 0; i < numberImg; i++) {
				// generate the i-th frame
				ModelSimpleImage frame = new ModelSimpleImage();
				CDVector tmp = new CDVector(vTexture.get(i));
				int size = extents[0] * extents[1];
				float[] buffer = new float[size];
				// do the sampling
				frame = m_pModel.ShapeFreeImage(tmp, frame);
				frame.exportData(buffer, 0, size);
				image.importData(i * size, buffer, false);
			}

			image.saveImage("", filename, FileUtility.XML, false);

		} catch (IOException e) {

			e.printStackTrace();
			success = false;
		}

		return success;
	}

}