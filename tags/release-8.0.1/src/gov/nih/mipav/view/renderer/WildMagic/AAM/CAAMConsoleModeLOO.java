package gov.nih.mipav.view.renderer.WildMagic.AAM;

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
 * Console mode loo for leave one out evaluation.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeLOO extends CAAMConsoleMode {

	/**
	 * Constructor
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeLOO(String progname) {

		m_ProgName = progname;
		m_NMinParam = 1;
		m_NMaxParam = 4;

		m_Name = new String("loo");
		m_Usage = new String(
				"<input dir> [acf file] [still|movie|both|none*] [pseudo|auto*]");
		m_Desc = new String("Leave-one-out evaluation of a training set.");
		m_LongDesc = new String(
				"This mode performs cross-validation of a training set.\n\n"
						+ "input dir          : Directory containing images and annotations.\n"
						+ "acf file           : AAM configuration file.\n"
						+ "[still|movie|both] : Write stills of the initial and optimized model\n"
						+ "                     and/or movies of the complete optimization.\n"
						+ "[pseudo|auto*]     : Initialisation method.\n"
						+ "\nOutput is written in the directory 'loo_results'.\n"
						+ "Default settings are marked with an asterisk (*)\n");
	}

	/**
	 * C style anchor to invoke the loo console mode. Being called from the AAM
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
		// CHTimer timer, thisLooTime;

		// timer.start();
		long startTime = System.currentTimeMillis();

		// setup input parameters
		String inDir = argv[0];
		String acf = argc >= 2 ? argv[1] : "";
		String docOutput = argc >= 3 ? argv[2] : "";
		boolean writeMovies = docOutput == "movie" || docOutput == "both";
		boolean writeStills = docOutput == "still" || docOutput == "both";
		boolean initialisation = true;
		if (argc >= 4) {
			initialisation = argv[3].equals("pseudo");
		}
		String resultDir = new String("loo_results");
		String s = new String();

		// scan shapes
		Vector<String> shapefns = (Vector<String>) CAAMUtil.ScanSortDir(inDir,
				"asf");

		// make result dir
		// _mkdir(resultDir);
		File f = new File(resultDir);
		f.mkdir();

		// do loo
		String shapeDestFile = new String();
		String imageDestFile = new String();
		int n = shapefns.size();
		double totalTime = .0, modelParam = .0, modelSize = .0;
		int reduction_factor = -1;
		CDVector ptpt = new CDVector(n);
		CDVector ptcrv = new CDVector(n);
		CDVector iter = new CDVector(n);
		CDVector time = new CDVector(n);
		CAAMEvaluationResults evalResults = new CAAMEvaluationResults();
		CAAMLowerBounds LowerBounds = new CAAMLowerBounds();
		for (int i = 0; i < n; i++) {

			long loop_startTime = System.currentTimeMillis();

			// read shape
			CAAMShape shape = new CAAMShape();
			shape.ReadASF(shapefns.get(i));
			String shapefn = CAAMUtil.GetFilename(shapefns.get(i));
			String image = shape.HostImage();

			C_AAMBUILDER builder = new C_AAMBUILDER();
			C_AAMMODEL model = new C_AAMMODEL();
			CAAMModelSeq modelSeq = new CAAMModelSeq();
			System.err
					.println("\n********************************************************");
			System.err.println("Building AAM - " + (i + 1) + "/"
					+ shapefns.size() + " (leaving out " + shapefn + ").");
			System.err
					.println("********************************************************");

			// make result dir
			s = resultDir + "\\" + shapefn;
			// _mkdir(s);
			File fi = new File(s);
			fi.mkdir();

			// build model - excluding the i-th shape
			String ext = CAAMUtil.GetExt(acf);
			s = resultDir + "\\" + shapefn + "\\" + "loo_model";
			if (ext == "acf") {

				// build model
				builder.BuildFromFiles(model, inDir, acf, 1, i);

				// write model
				model.WriteModel(s, true);

				reduction_factor = model.ReductionFactor();

				// calc stats
				modelSize += model.NTextureSamples();
				modelParam += model.CombinedPCA().NParameters();

			} else {

				if (ext == "sacf") {

					// model sequence
					modelSeq.BuildFromSACF(acf, inDir, i);

					// write
					modelSeq.WriteModels(s, true);

					reduction_factor = model.ReductionFactor();

					// calc stats
					modelSize += modelSeq.FinalModel().NTextureSamples();
					modelParam += modelSeq.FinalModel().CombinedPCA()
							.NParameters();
				} else {

					System.err
							.println("Error: unknown configuration extension.");
					System.exit(-1);
				}
			}

			// copy shape
			shapeDestFile = resultDir + "\\" + shapefn + "\\" + shapefn;

			// copy image
			imageDestFile = resultDir + "\\" + shapefn + "\\" + image;

			// evaluate
			System.err.println("Evaluating AAM on " + shapefn + "...");
			CAAMEvaluationResults res = new CAAMEvaluationResults();
			CAAMLowerBounds lbs = new CAAMLowerBounds();

			if (ext == "acf") {

				res = CAAMTest.EvaluateModel(model, resultDir + "\\" + shapefn
						+ "\\", "results.txt", writeStills, writeMovies,
						initialisation, false, lbs);
			}
			if (ext == "sacf") {

				res = CAAMTest.EvaluateModelSeq(modelSeq, resultDir + "\\"
						+ shapefn + "\\", "results.txt", writeStills,
						writeMovies, initialisation, false, lbs);
			}

			evalResults.AddResults(res);
			LowerBounds.AddGroundtruths(lbs);

			// calc and write timings
			long loop_endTime = System.currentTimeMillis();
			totalTime += (loop_endTime - loop_startTime);
			double estAllLOOs = n * totalTime / (i + 1.);
			double eta = totalTime * (n / (i + 1.) - 1.);

			System.err.println("This experiment: "
					+ ((loop_endTime - loop_startTime) / 1000)
					+ " secs. Total time: " + (estAllLOOs / 60.)
					+ " mins. ETA: " + (eta / 60.) + " mins.");
		}

		// open result file
		try {
			PrintWriter fh = new PrintWriter(resultDir + "\\loo_results.txt");

			evalResults.PrintStatistics(fh);
			evalResults.PrintStatistics();
			LowerBounds.PrintStatistics(resultDir + "\\lbs.txt");

			long endTime = System.currentTimeMillis();
			double totaltime = endTime - startTime;
			fh.println("\n");
			fh.println("Leave-one-out time    " + CAAMUtil.Secs2Mins(totaltime)
					+ " mins");
			fh.println("Mean model parameters " + (modelParam / n));
			fh.println("Mean texture samples  " + (modelSize / n));

			fh.println("\n" + (double) reduction_factor + "\t" + totaltime
					+ "\t" + (modelParam / n) + "\t" + (modelSize / n));

			// close files
			fh.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return 0;
	}

}