package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import java.io.*;
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
 * Console mode CM
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeCM extends CAAMConsoleMode {

	/**
	 * Constructor
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeCM(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 5;
		m_NMaxParam = 6;

		m_Name = new String("cm");
		m_Usage = new String(
				"<input dir> <ext> <output avi> <frame rate> <gray|rgb> [comp]");
		m_Desc = new String(
				"Collects a movie file from BMP frames.\n\n"
						+ "If the optional 'comp' is equal to '1' a compression dialog\n"
						+ "is shown. In all other cases the AVI format is uncompressed.\n");
		m_LongDesc = new String("Example usage:\n\n     " + m_ProgName
				+ " cm frames\\ bmp movie.avi 4 rgb 1\n");
	}

	/**
	 * C style anchor to invoke the CM console mode. Being called from the AAM
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

		// setup input parameters
		String inDir = argv[0];
		String inExt = argv[1];
		String outMovie = argv[2];
		// int frameRate = Integer.valueOf(argv[3]);
		String mode = argv[4];
		// boolean showCompressionDlg = argc == 6 ? argv[5].equals("1") : false;

		if (!mode.equals("gray") && !mode.equals("rgb")) {

			System.err.println("\nUnknown destination format '" + mode
					+ "'. Must be either 'gray' or 'rgb'.\n");

			System.exit(-1);
		}

		Vector<String> vFrames = new Vector<String>();

		vFrames = (Vector<String>) CAAMUtil.ScanSortDir(inDir, inExt);

		try {

			// int index = outMovie.lastIndexOf(File.separator);
			// String aviDir = outMovie.substring(0, index);
			// String avi_fileName = outMovie.substring(index + 1, outMovie.length());

			for (int i = 0; i < vFrames.size(); i++) {

				System.err.println("Reading frame #" + i + "/" + vFrames.size()
						+ " '" + vFrames.get(i) + "'");
				FileIO io = new FileIO();
				String absPath = vFrames.get(i);
				int idx = absPath.lastIndexOf(File.separator);
				String dir = absPath.substring(0, idx);
				String filename = absPath.substring(idx + 1, absPath.length());

				// ugly but fast :-(
				if (mode.equals("gray")) {

					ModelImage img = null;

					try {
						img = io.readImage(dir, filename);
					} catch (Exception e) {
						e.printStackTrace();
					}

					// convert and write frame
					// ModelRGB rgbImage = ViewJFrameBase.initRGB(img);
				} else {

					ModelImage img_gray;
					try {
						img_gray = io.readImage(dir, filename);
					    ModelRGB img = ViewJFrameBase.initRGB(img_gray);
					    
					} catch (Exception e) {
						System.exit(-1);
					}

				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		// we're done
		return 0;
	}

}