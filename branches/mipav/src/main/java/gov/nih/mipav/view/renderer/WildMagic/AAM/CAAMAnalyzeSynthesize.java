package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
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
 * Abstract base class for the analyze/synthesize classes. To core task of such
 * classes is to sample pixels inside a shape placed on an image (the analyze
 * part) and render a texture vector into a shape given in image coordinates
 * (the synthesize part).
 * 
 * @author Ruida Cheng
 * 
 */
public abstract class CAAMAnalyzeSynthesize {

	/** software synthesize */
	public static int asSoftware = 0;

	/** hardware OpenGL synthesize, currently disabled. */
	public static int asOpenGL = 1;

	/** reference frame */
	protected final CAAMReferenceFrame m_pReferenceFrame;

	/** Synthesize ID */
	protected int m_Id;

	/**
	 * Constructor
	 * 
	 * @param rf
	 *            The reference frame to analyze and synthesize
	 */
	public CAAMAnalyzeSynthesize(final CAAMReferenceFrame rf) {
		m_pReferenceFrame = rf;
	}

	/**
	 * Reads an object from disk. Deprecated.
	 * 
	 * @param fh
	 *            input stream
	 * @param _id
	 *            file id
	 */
	public void FromFile(DataInputStream fh, int _id) {

		int id = -1;
		id = _id;
		m_Id = id;
	}

	/**
	 * Write an object to disk.
	 * 
	 * @param fh
	 *            output stream
	 */
	public void ToFile(DataOutputStream fh) {

		int id;
		try {
			id = m_Id;
			fh.writeInt(id);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Analyzer/Synthesizer loader. The function loads an Analyzer/Synthesizer
	 * from a stream, instantiates the correct concrete class and returns a base
	 * class pointer.
	 * 
	 * @param fh
	 *            input stream from disk
	 * @param rf
	 *            reference frame
	 * @return An instance of Analyzer/Synthesize object created on the heap
	 */
	public static CAAMAnalyzeSynthesize AAMLoadAnalyzerSynthesizer(
			DataInputStream fh, final CAAMReferenceFrame rf) {

		CAAMAnalyzeSynthesize pAS = null;
		int id;
		try {
			id = fh.readInt();

			int as_id = id;

			switch (as_id) {

			case 0:
				pAS = new CAAMAnalyzeSynthesizeSoftware(rf); // asSoftware
				break;

			case 1:
				// pAS = new CAAMAnalyzeSynthesizeOpenGL(rf); // disabled OpenGL
				break;

			default:
				System.err
						.println("Error: AAMLoadAnalyzerSynthesizer(): Unknown class id, NULL returned(!)");
				return null;

			}

			pAS.FromFile(fh, as_id);
		} catch (IOException e) {
			e.printStackTrace();
		}

		return pAS;
	}

	/**
	 * Dispose memory
	 */
	public abstract void dispose();

	/**
	 * Set the analyzed image
	 * 
	 * @param img
	 *            MIPAV simple image as the pass in type
	 */
	public abstract void SetAnalyzeImage(final ModelSimpleImage img);

	/**
	 * Analyze image
	 * 
	 * @param shape
	 *            AAM shape, VOI shape
	 * @param texture
	 *            image texture
	 * @param useInterpolation
	 *            interpolation type
	 * @return success or not
	 */
	public abstract boolean Analyze(final CAAMShape shape, CDVector texture,
			final boolean useInterpolation);

	/**
	 * Analyze image.
	 * 
	 * @param shape
	 *            VOI shape converted to AAM shape
	 * @param refImg
	 *            image reference, MIPAV simple image type
	 * @param useInterpolation
	 *            interpolation type
	 * @return success or not
	 */
	public abstract boolean Analyze(final CAAMShape shape,
			ModelSimpleImage refImg, final boolean useInterpolation);

	/**
	 * Image synthesize.
	 * 
	 * @param shape
	 *            VOI shape
	 * @param texture
	 *            image texture
	 * @param destImage
	 *            target image
	 * @param renderOntoImage
	 *            intermediate image
	 * @return success or not
	 */
	public abstract boolean Synthesize(final CAAMShape shape,
			final CDVector texture, ModelSimpleImage destImage,
			boolean renderOntoImage);

	/**
	 * Anyalyze/Synthesize object clone
	 * 
	 * @param rf
	 *            reference frame
	 * @return cloned object
	 */
	public abstract CAAMAnalyzeSynthesize Clone(final CAAMReferenceFrame rf);

}