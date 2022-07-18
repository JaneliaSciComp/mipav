package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
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
 * Console mode P for n-leval multi-scale AAM
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeP extends CAAMConsoleMode {

	/**
	 * Constructor
	 * 
	 * @param progname
	 *            program name.
	 */
	public CAAMConsoleModeP(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 1;
		m_NMaxParam = 100;

		m_Name = new String("p");
		m_Usage = new String(
				"[b|s|e|loo] <nb. levels> <usual set of parameters>");
		m_Desc = new String("N-level pyramidal (multi-scale) AAM.");

		m_LongDesc = new String(
				"This mode implements the modes b,s,e and loo using an N-level aam.\n"
						+ "Parameters after the mode name are set as in the 1-level modes.\n");
	}

	/**
	 * C style anchor to invoke the P console mode. Being called from the AAM
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
		String mode = argv[0];

		// sanity checks
		if ((!mode.equals("b") && !mode.equals("s") && !mode.equals("e") && !mode
				.equals("loo"))
				|| (mode.equals("b") && (argc < 4 || argc > 5))
				|| (mode.equals("e") && (argc < 4 || argc > 6))
				|| (mode.equals("loo") && (argc < 3 || argc > 6))
				|| (mode.equals("s") && (argc < 4 || argc > 5))) {

			PrintUsage();
			System.exit(-1);
		}

		// the number of levels in the model
		int nLevels = Integer.valueOf(argv[1]);

		// build/use the multi scale AAM
		if (mode.equals("b")) {

			// build multi-scale AAM
			CAAMModelMS multiscale = new CAAMModelMS();
			String inputdir = argv[2];
			String outmodel = argv[3];
			String acf = argv[4];

			long startTime = System.currentTimeMillis();
			multiscale.BuildAllLevels(nLevels, inputdir, acf);
			long endTime = System.currentTimeMillis();
			multiscale.WriteModel(outmodel);
			System.err.println("Total build time for " + nLevels + " levels: "
					+ ((endTime - startTime) / 1000) + " secs.");
			return 0;
		}

		// search multi-scale AAM
		if (mode.equals("s")) {

			CAAMModelMS multiscale = new CAAMModelMS();
			String inmodel = argv[2];
			String imagedir = argv[3];

			multiscale.ReadModel(inmodel);
			Vector<String> fn = new Vector<String>();

			// scan for bmp and files
			fn = (Vector<String>) CAAMUtil.ScanSortDir(imagedir, "bmp");

			for (int i = 0; i < fn.size(); i++) {

				ModelSimpleImage image = new ModelSimpleImage();
				ModelSimpleImage smallImage = new ModelSimpleImage();
				CDVector c = new CDVector();
				image = image.ReadBandedFile(fn.get(i));

				CAAMShape s = new CAAMShape();
				smallImage = image;
				int rf = (int) (Math.pow(2., multiscale.NLevels() - 1));
				if (rf > 1) {
					// smallImage.ReducePyr( rf );
					smallImage.subSample2dBy2();
				}

				boolean cheat = false;
				if (cheat) {

					s.ReadASF(CAAMUtil.RemoveExt(fn.get(i)) + ".asf");
					s.Scale(1. / Math.pow(2., multiscale.NLevels() - 1), false);
					s.Rel2Abs(image.Width(), image.Height());
				} else {

					CAAMInitializeStegmann init = new CAAMInitializeStegmann(
							multiscale.GetSmallest());
					init.Initialize(smallImage, s, c);

					CAAMVisualizer.ShapeStill(smallImage, s, "init.xml");

					// scale result back to level o
					s.Scale(Math.pow(2., multiscale.NLevels() - 1), false);
				}

				long startTime = System.currentTimeMillis();
				multiscale.OptimizeModel(image, s, c);
				long endTime = System.currentTimeMillis();

				CAAMVisualizer.ShapeStill(image, s, "opt.xml");

				System.err.println("Multi-scale search took "
						+ ((endTime - startTime) / 1000) + " secs.");
			}
			return 0;
		}

		// evaluate mul-scale AAM
		if (mode.equals("e")) {

			CAAMModelMS model = new CAAMModelMS();

			// setup input parameters
			String inModel = argv[2];
			String inPath = CAAMUtil.AddBackSlash(argv[3]);
			String docOutput = argc >= 5 ? argv[4] : "";
			boolean writeMovies = docOutput.equals("movie")
					|| docOutput.equals("both");
			boolean writeStills = docOutput.equals("still")
					|| docOutput.equals("both");
			boolean initialisation = true;

			if (argc >= 6) {

				initialisation = argv[5].equals("pseudo");
			}

			// build model
			boolean ok = model.ReadModel(inModel);

			if (!ok) {

				System.err.println("Could not read model file. Exiting...");
				System.exit(1);
			}

			CAAMTest.EvaluateModel((CAAMModelMS) (model), inPath,
					"results.txt", writeStills, writeMovies, initialisation,
					true);
			model = null;
			return 0;
		}

		if (mode.equals("loo")) {

			long startTime = System.currentTimeMillis();

			// setup input parameters
			String inDir = argv[2];
			String acf = argc >= 4 ? argv[3] : "";
			String docOutput = argc >= 5 ? argv[4] : "";
			boolean writeMovies = docOutput.equals("movie")
					|| docOutput.equals("both");
			boolean writeStills = docOutput.equals("still")
					|| docOutput.equals("both");
			boolean initialisation = true;
			if (argc >= 6) {

				initialisation = !(argv[5].equals("pseudo") ? false : true);
			}
			String resultDir = "loo_results";
			String s = new String();

			// scan shapes
			Vector<String> shapefns = (Vector<String>) CAAMUtil.ScanSortDir(
					inDir, "asf");

			// make result dir
			// _mkdir(resultDir);
			File file = new File(resultDir);
			file.mkdir();

			// do loo
			String shapeDestFile = new String();
			String imageDestFile = new String();
			int n = shapefns.size();
			double totalTime = .0, modelParam = .0, modelSize = .0;
			int reduction_factor = 1;
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

				System.err
						.println("\n********************************************************");
				System.err.println("Building AAM - " + (i + 1) + "/"
						+ shapefns.size() + "(leaving out '" + shapefn + "').");
				System.err
						.println("********************************************************");

				// build model - excluding the i-th shape
				CAAMModelMS model = new CAAMModelMS();
				model.BuildAllLevels(nLevels, inDir, acf, 1, i);

				// make result dir
				s = resultDir + "\\" + shapefn;
				// _mkdir(s);
				File fs = new File(s);
				fs.mkdir();

				// write model
				s = resultDir + "\\" + shapefn + "\\loo_model" + i;
				model.WriteModel(s, true);
				reduction_factor = model.ReductionFactor();

				// calc stats
				modelSize += model.NTextureSamples();
				modelParam += model.CombinedPCA().NParameters();

				// copy shape
				shapeDestFile = resultDir + "\\" + shapefn + "\\" + shapefn;
				// CopyFile( shapefns.get(i), shapeDestFile, 0 ); ????????????

				// copy image
				imageDestFile = resultDir + "\\" + shapefn + "\\" + image;
				// CopyFile( inDir+"\\"+image, imageDestFile, 0 ); ??????????

				// evaluate
				System.err.println("Evaluating AAM on " + shapefn + "...");
				CAAMEvaluationResults res = new CAAMEvaluationResults();
				CAAMLowerBounds lbs = new CAAMLowerBounds();
				res = CAAMTest.EvaluateModel(model, resultDir + "\\" + shapefn
						+ "\\", "results.txt", writeStills, writeMovies,
						initialisation, false, lbs);

				evalResults.AddResults(res);
				LowerBounds.AddGroundtruths(lbs);

				// calc and write timings
				long loop_endTime = System.currentTimeMillis();
				totalTime += (loop_endTime - loop_startTime);
				double estAllLOOs = n * totalTime / (i + 1.);
				double eta = totalTime * (n / (i + 1.) - 1.);

				model = null;

				System.err.println("This experiment: "
						+ ((loop_endTime - loop_startTime) / 1000)
						+ " secs. Total time: " + (estAllLOOs / 60.)
						+ " mins. ETA: " + (eta / 60.) + " mins.");
			}

			// open result file
			try {
				PrintWriter fh = new PrintWriter(resultDir + "\\"
						+ "loo_results.txt", "wt");

				evalResults.PrintStatistics(fh);
				evalResults.PrintStatistics();

				LowerBounds.PrintStatistics(resultDir + "\\lower_bounds.txt");

				long endTime = System.currentTimeMillis();
				double totaltime = (endTime - startTime);
				fh.println("\n");
				fh.println("Leave-one-out time    "
						+ CAAMUtil.Secs2Mins(totaltime) + " mins\n");
				fh.println("Mean model parameters " + modelParam / n + "\n");
				fh.println("Mean texture samples  " + modelSize / n + "\n");

				fh.println("\n" + (double) reduction_factor + "\t" + totaltime
						+ "\t" + modelParam / n + "\t" + modelSize / n);

				// close files
				fh.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
			return 0;
		}

		return -1;
	}

}