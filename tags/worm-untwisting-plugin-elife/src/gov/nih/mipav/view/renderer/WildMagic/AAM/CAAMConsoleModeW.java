package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.file.FileUtility;
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
 * Console mode W for write testing.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeW extends CAAMConsoleMode {

	/**
	 * Constructor.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeW(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 2;
		m_NMaxParam = 3;

		m_Name = new String("w");
		m_Usage = new String("<write options> <annotation> [reduction factor]");
		m_Desc = new String(
				"Plots an annotation into it's host image.\n\n"
						+ "<write options>    Set one or several of the following options to get\n"
						+ "                   the desired result (not sensitive to ordering):\n\n"
						+ "                     s   Plots the shape as connecting lines\n"
						+ "                     p   Plots all shape points as cross-hairs\n"
						+ "                     i   Dots the interior of the shape\n"
						+ "                     n   Draws point normals\n"
						+ "                     m   Plots a Delaunay triangulation of the shape\n"
						+ "                     t   Similar to 'm' but removes concave triangles and holes\n\n"
						+ "<annotation>       An .asf-file.\n\n"
						+ "[reduction factor] Optional integer image/annotation reduction factor [2-n]\n\n"
						+ "Example usage:\n\n       " + m_ProgName
						+ " w si c6302.asf\n");
		m_LongDesc = new String("");
	}

	/**
	 * C style anchor to invoke the W console mode. Being called from the AAM
	 * console.
	 * 
	 * @param argc
	 *            number of augments
	 * @param argv
	 *            augments array
	 * @return nothing
	 */
	public int Main(int argc, String[] argv) {

		// call base implementation
		super.Main(argc, argv);
		int rfactor = 1;

		// setup
		ModelSimpleImage image = new ModelSimpleImage();
		CAAMShape shape = new CAAMShape();
		String outFile = new String();
		String options = argv[0];
		String asf_file = argv[1];

		// determine write options
		boolean drawLines = options.equals('s');
		boolean drawPoints = options.equals('p');
		boolean drawInterior = options.equals('i');
		boolean drawNormals = options.equals('n');
		boolean drawDelaunay = options.equals('m');
		boolean drawDelaunayClean = options.equals('t');

		if (argc == 3) {

			// scale factor was incluced
			rfactor = Integer.valueOf(argv[2]);
		}

		// read input
		image = CAAMUtil.ReadExample(asf_file, image, shape, rfactor);

		double[] p255 = new double[1];
		// ModelSimpleImage.SPAWN( p255, 255);
		p255[0] = 255;

		// plot delaunay mesh
		if (drawDelaunay || drawDelaunayClean) {
			CAAMMesh mesh = new CAAMMesh();
			CAAMDelaunay.MakeMesh(shape, mesh, drawDelaunayClean);
			CAAMUtil.PlotMeshIntoImage(image, mesh, p255, p255);
		}

		// plot shape
		CAAMUtil.PlotShapeIntoImage(image, shape, p255, p255, drawNormals,
				drawInterior, drawPoints, drawLines);

		// write the result to disk
		outFile = CAAMUtil.RemoveExt(asf_file) + new String(".ann") + ".bmp";
		// image.WriteBandedFile( outFile, "bmp" );
		String dir = System.getProperties().getProperty("user.dir");
		ModelImage outImage = new ModelImage(image, outFile);
		outImage.saveImage(dir, outFile, FileUtility.BMP, false);

		System.err.println("Annotation rendered and written into '" + outFile
				+ "'.");

		return 0;
	}

}