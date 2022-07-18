package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
import java.util.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity.POINT;

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
 * Container for all sorts of test functions. In this way a history of all
 * mock-up test functions developed during debugging and testing are kept.
 * 
 * All functions are implemented as static functions. Thus no instantiation the
 * CAAMTest are needed.
 * 
 * @author Ruida Cheng
 */

public class CAAMTest extends CAAMObject {

	/**
	 * Constructor.
	 */
	public CAAMTest() {

	}

	/**
	 * dispose memory
	 * */
	public void dispose() {

	}

	/**
	 * Tests CAAMShape:GetRotation() for rotations in the range [0;360].
	 * 
	 * @param s1
	 *            First shape.
	 * @param s2
	 *            Second shape.
	 */
	public static void GetRotationTest(final CAAMShape s1, final CAAMShape s2) {

		CAAMShape _s1 = new CAAMShape();
		CAAMShape _s2 = new CAAMShape();
		CAAMShape _s1_zero = new CAAMShape();

		_s1.assign(s1);
		_s2.assign(s2);

		System.err.println("Shape 1 got " + _s1.NPoints() + " points.");
		System.err.println("Shape 2 got " + _s2.NPoints() + " points.");

		double rad_angle = _s1.GetRotation(_s2);
		System.err.println("\nRotation between shape 1 and 2: "
				+ CAAMUtil.Rad2Deg(rad_angle) + " degrees.");

		System.err.println("Compensation for rotation.");
		_s1.Rotate(-rad_angle);

		rad_angle = _s1.GetRotation(_s2);
		System.err.println("Rotation between shape 1 and 2: "
				+ CAAMUtil.Rad2Deg(rad_angle) + " degrees.\n");

		_s1_zero.assign(_s1);

		for (int i = 0; i <= 360; i += 5) {

			System.err.println("Actual rot. " + i + " degrees. ");

			double angle = CAAMUtil.Deg2Rad(i);

			_s1.assign(_s1_zero);
			_s1.Rotate(angle);
			double rad_rot = _s1.GetRotation(_s2);

			System.err.println("Estimate: s1 is rotated "
					+ CAAMUtil.Rad2Deg(rad_rot) + " degrees w.r.t. s2.");
		}
	}

	/**
	 * Benchmarks the software warping method against the OpenGL.
	 * 
	 * @param refShape
	 *            Reference shape, i.e. mean shape scale to mean size.
	 * @param s
	 *            Input shape in relative or absolute coordinates.
	 * @param image
	 *            Host image of 's'.
	 * @param useConvexHull
	 *            If true the convex hull is used.
	 */
	public static void AnalyzeTest(final CAAMShape refShape, final CAAMShape s,
			final ModelSimpleImage image, final boolean useConvexHull) {

		// setup
		CAAMReferenceFrame rf = new CAAMReferenceFrame();
		rf.Setup(refShape, useConvexHull);
		CDVector hard = new CDVector();
		CDVector soft = new CDVector();

		long startTime = System.currentTimeMillis();
		long endTime = System.currentTimeMillis();

		double software, hardware;
		CAAMShape shape = new CAAMShape(s);
		shape.Rel2Abs(image.Width(), image.Height());

		int n = 30; // number of repetions

		// /////////////////////////
		// hardware analyze
		// /////////////////////////
		// CAAMAnalyzeSynthesizeOpenGL as = new CAAMAnalyzeSynthesizeOpenGL(rf);
		// ????????????????????

		// as.ClockReadPixels(); // test readpixel ????????????????????????????

		hardware = .0;

		startTime = System.currentTimeMillis();
		for (int i = 0; i < n; i++) {

			// as.SetAnalyzeImage(image); ??????????????????????????
		}
		endTime = System.currentTimeMillis();
		hardware = (endTime - startTime) / n;
		System.err.println("OpenGL SetAnalyzeImage() : " + hardware + " ms");

		ModelSimpleImage refImage;
		int refWidth = rf.RefImageWidth();
		int refHeight = rf.RefImageHeight();
		int[] extents = new int[2];
		extents[0] = refWidth;
		extents[1] = refHeight;

		refImage = new ModelSimpleImage(extents);

		hardware = .0;

		startTime = System.currentTimeMillis();
		int i;
		for (i = 0; i < n; i++) {

			// as.Analyze(shape, refImage, true);
			// ?????????????????????????????????
			rf.Image2Vector(refImage, hard);
		}
		endTime = System.currentTimeMillis();
		hardware = (endTime - startTime) / n;

		String filename = "analyze_hard_refimage.bmp";
		String dir = System.getProperties().getProperty("user.dir");
		ModelImage an_refImage = new ModelImage(refImage, filename);
		an_refImage.saveImage(dir, filename, FileUtility.BMP, false);

		refImage = rf.Vector2Image(hard, refImage);
		filename = "analyze_hard.bmp";
		ModelImage an_hardImage = new ModelImage(refImage, filename);
		an_hardImage.saveImage(dir, filename, FileUtility.BMP, false);

		hard.ToMatlab("hard.m", "h", "", false);
		System.err.println("OpenGL Analyze()         : " + hardware + " ms\n");

		// /////////////////////////
		// software analyze
		// /////////////////////////
		CAAMAnalyzeSynthesizeSoftware ass = new CAAMAnalyzeSynthesizeSoftware(
				rf);
		ass.SetAnalyzeImage(image);
		software = .0;

		startTime = System.currentTimeMillis();
		for (i = 0; i < n; i++) {

			ass.Analyze(shape, soft, true);
		}
		endTime = System.currentTimeMillis();
		software = (endTime - startTime) / n;
		refImage = rf.Vector2Image(soft, refImage);

		filename = "analyze_soft.bmp";
		ModelImage an_softImage = new ModelImage(refImage, filename);
		an_softImage.saveImage(dir, filename, FileUtility.BMP, false);

		System.err.println("Software Analyze()       : " + software + " ms");
		soft.ToMatlab("soft.m", "s", "", false);
		System.err.println("Analyze ratio            : " + software / hardware
				+ " (software/hardware)");

		// /////////////////////////
		// hardware synthesize
		// /////////////////////////
		ModelSimpleImage destImage = image;
		hardware = .0;

		startTime = System.currentTimeMillis();
		for (i = 0; i < n; i++) {

			// as.Synthesize(shape, hard, destImage, true);
			// ?????????????????????????
		}
		endTime = System.currentTimeMillis();
		hardware = (endTime - startTime) / n;
		System.err.println("\nOpenGL Synthesize()      : " + hardware + " ms");
		// destImage.WriteBandedFile("synthesize_hard.bmp");
		filename = "synthesize_hard.bmp";
		ModelImage syn_hardImage = new ModelImage(destImage, filename);
		syn_hardImage.saveImage(dir, filename, FileUtility.BMP, false);

		// /////////////////////////
		// software synthesize
		// /////////////////////////
		software = .0;

		startTime = System.currentTimeMillis();
		for (i = 0; i < n; i++) {
			ass.Synthesize(shape, soft, destImage, true);
		}
		endTime = System.currentTimeMillis();
		software += (endTime - startTime) / n;
		System.err.println("Software Synthesize()    : " + software + " ms");
		filename = "synthesize_soft.bmp";
		ModelImage syn_softImage = new ModelImage(destImage, filename);
		syn_softImage.saveImage(dir, filename, FileUtility.BMP, false);

		System.err.println("Synthesize ratio         : " + software / hardware
				+ " (software/hardware)");

		// /////////////////////////
		// image2vector
		// /////////////////////////
		double img2vec = .0;

		startTime = System.currentTimeMillis();
		for (i = 0; i < n; i++) {

			rf.Image2Vector(refImage, hard);
		}
		endTime = System.currentTimeMillis();
		img2vec = (endTime - startTime) / n;
		System.err.printf("\nImage2Vector()           : " + img2vec + " ms");

		// /////////////////////////
		// vector2image
		// /////////////////////////
		double vec2img = .0;

		startTime = System.currentTimeMillis();
		for (i = 0; i < n; i++) {

			refImage = rf.Vector2Image(hard, refImage);
		}
		endTime = System.currentTimeMillis();
		vec2img = (endTime - startTime) / n;
		System.err.println("Vector2Image()           : " + vec2img + " ms");

		// done
		System.err.println("\n# texture samples        : " + soft.Length());
	}

	/**
	 * Tests the prediction matrices ability to predict pose displacements.
	 * 
	 * @doc Output are returned in the form of eight matlab formatted files in
	 *      the current directory.
	 * @param model
	 *            The AAM to test.
	 * @param path
	 *            The path where test images and annotations are placed
	 *            (including terminating backslash).
	 */
	public static void TestPosePrediction(final CAAMModel model,
			final String path) {

		Vector<String> vFilenames = new Vector<String>();

		// read files
		vFilenames = (Vector<String>) CAAMUtil.ScanSortDir(path, "asf");
		int nShapes = vFilenames.size();

		// setup test ranges
		int n = 11;
		double rel_displace = .2;
		double degrees = 15.;
		double scale = .15;
		int nIter = 5;

		CDVector dx = new CDVector();
		CDVector dy = new CDVector();
		CDVector ds = new CDVector();
		CDVector dtheta = new CDVector();
		CDVector dxy_rel = new CDVector();
		double refWidth = model.ReferenceShape().Width();
		double refHeight = model.ReferenceShape().Height();
		double w = rel_displace * refWidth;
		double h = rel_displace * refHeight;
		double w10 = refHeight / 10.;
		double h10 = refHeight / 10.;
		double r10 = 10. * Math.PI / 180.;
		double avgXerr = .0, avgYerr = .0, avgRotErr = .0, avgScaleErr = .0;
		int XerrCount = 0, YerrCount = 0, RotErrCount = 0, ScaleErrCount = 0;
		dxy_rel.Linspace(-rel_displace, rel_displace, n);
		dx.Linspace(-w, w, n);
		dy.Linspace(-h, h, n);
		ds.Linspace(1 - scale, 1 + scale, n);
		dtheta.Linspace(-degrees * Math.PI / 180., degrees * Math.PI / 180., n);

		CDMatrix vDx = new CDMatrix(n, nShapes);
		CDMatrix vDy = new CDMatrix(n, nShapes);
		CDMatrix vDs = new CDMatrix(n, nShapes);
		CDMatrix vDtheta = new CDMatrix(n, nShapes);
		for (int i = 0; i < nShapes; i++) {

			CDVector c = new CDVector();
			CDVector g_m = new CDVector();
			CDVector g_s = new CDVector();
			CDVector g_delta = new CDVector();
			CDVector pose = new CDVector(4);
			ModelSimpleImage image = new ModelSimpleImage();
			double[] s = new double[1];
			double[] t = new double[1];
			double[] t_x = new double[1];
			double[] t_y = new double[1];
			CAAMShape shape = new CAAMShape();

			// read the image and shape
			image = CAAMUtil.ReadExample(vFilenames.get(i), image, shape,
					model.ModelReduction());

			// debug
			System.err.println("Testing " + vFilenames.get(i) + " [" + (i + 1)
					+ "/" + nShapes + "]\n");

			// make x-translation tests
			for (int j = 0; j < dx.Length(); j++) {

				// make curve
				CAAMShape shapeCopy = new CAAMShape(shape);
				shapeCopy.Translate(dx.m_data[j], 0);
				model.EstimatePose(image, shapeCopy, pose);
				CAAMShape.PoseVec2Param(pose, s, t, t_x, t_y);
				vDx.m_data[j][i] = t_x[0];
				if (Math.abs(dx.m_data[j]) < w10) {
					avgXerr += Math.abs(dx.m_data[j] - t_x[0]);
					++XerrCount;
				}
			}

			// make y-translation tests
			for (int j = 0; j < dy.Length(); j++) {

				CAAMShape shapeCopy = new CAAMShape(shape);
				shapeCopy.Translate(0, dy.m_data[j]);
				model.EstimatePose(image, shapeCopy, pose);
				CAAMShape.PoseVec2Param(pose, s, t, t_x, t_y);
				vDy.m_data[j][i] = t_y[0];
				if (Math.abs(dy.m_data[j]) < h10) {
					avgYerr += Math.abs(dy.m_data[j] - t_y[0]);
					++YerrCount;
				}
			}

			// make scale tests
			for (int j = 0; j < ds.Length(); j++) {

				CAAMShape shapeCopy = new CAAMShape(shape);
				shapeCopy.Scale(ds.m_data[j], true);
				model.EstimatePose(image, shapeCopy, pose);
				CAAMShape.PoseVec2Param(pose, s, t, t_x, t_y);
				vDs.m_data[j][i] = s[0];
				if (Math.abs(1 - ds.m_data[j]) < .1) {
					avgScaleErr += Math.abs(ds.m_data[j] - s[0]);
					++ScaleErrCount;
				}
			}

			// make rotation tests
			for (int j = 0; j < dtheta.Length(); j++) {

				CAAMShape shapeCopy = new CAAMShape(shape);
				shapeCopy.Rotate(dtheta.m_data[j], true);
				model.EstimatePose(image, shapeCopy, pose);
				CAAMShape.PoseVec2Param(pose, s, t, t_x, t_y);
				vDtheta.m_data[j][i] = t[0];
				if (Math.abs(dtheta.m_data[j]) < r10) {
					avgRotErr += Math.abs(dtheta.m_data[j] - t[0]);
					++RotErrCount;
				}
			}
		}
		System.err.println("Avg X error (within +/- 10%% width)     : "
				+ (avgXerr / XerrCount) + " pixels ("
				+ (100. * (avgXerr / XerrCount) / refWidth) + " %%)");

		System.err.println("Avg Y error (within +/- 10%% height)    : "
				+ (avgYerr / YerrCount) + " pixels ("
				+ (100. * (avgYerr / YerrCount) / refHeight) + " %%)");
		;
		System.err.println("Avg rot. error (within +/- 10 degrees) : "
				+ (avgRotErr / RotErrCount * 180. / Math.PI) + " degrees");
		System.err.println("Avg scale error (within +/- 10%%)       : "
				+ (100. * avgScaleErr / ScaleErrCount) + " %%");

		// write displacements to disk in matlab format
		dx.ToMatlab("fdx.m", "dx", "", false);
		dy.ToMatlab("fdy.m", "dy", "", false);
		ds.ToMatlab("fds.m", "ds", "", false);
		dxy_rel.ToMatlab("fdxy_rel.m", "dxy_rel", "", false);
		dtheta.ToMatlab("fdtheta.m", "dtheta", "", false);

		// write results to disk in matlab format
		vDx.ToMatlab("fdrx.m", "drx", "", false);
		vDy.ToMatlab("fdry.m", "dry", "", false);
		vDs.ToMatlab("fdrs.m", "drs", "", false);
		vDtheta.ToMatlab("fdrtheta.m", "drtheta", "", false);

	}

	/**
	 * Optimizes a set of images and compares the result to a ground truth
	 * annotation. As initialization the ground truth pose is systematically
	 * displaced (default) or an automatic initialization is performed.
	 * 
	 * @param model
	 *            The model to evaluate.
	 * @param gt_path
	 *            Path to ground truth images and annotations.
	 * @param result_file
	 *            The file to write the results in.
	 * @param writeStills
	 *            If true two model border images are written; one of the
	 *            initialization and one of the optimization.
	 * @param writeMovies
	 *            It true a movie of the whole optimization is written, one
	 *            frame per iteration.
	 * @param autoinit
	 *            If true automatic initialization is performed instead of the
	 *            systematic displacement of the ground truth pose.
	 * @param dump2screen
	 *            If true, results are written to the screen also (default
	 *            true).
	 * @return evaluation results
	 */
	public static CAAMEvaluationResults EvaluateModel(final CAAMModel pModel,
			final String gt_path, final String result_file,
			final boolean writeStills, final boolean writeMovies,
			final boolean autoinit, final boolean dump2screen) {
		return EvaluateModel(pModel, gt_path, result_file, writeStills,
				writeMovies, autoinit, dump2screen, null);
	}

	/**
	 * Optimizes a set of images and compares the result to a ground truth
	 * annotation. As initialization the ground truth pose is systematically
	 * displaced (default) or an automatic initialization is performed.
	 * 
	 * @param model
	 *            The model to evaluate.
	 * @param gt_path
	 *            Path to ground truth images and annotations.
	 * @param result_file
	 *            The file to write the results in.
	 * @param writeStills
	 *            If true two model border images are written; one of the
	 *            initialisation and one of the optimization.
	 * @param writeMovies
	 *            It true a movie of the whole optimization is written, one
	 *            frame per iteration.
	 * @param autoinit
	 *            If true automatic initialization is performed instead of the
	 *            systematic displacement of the ground truth pose.
	 * @param dump2screen
	 *            If true, results are written to the screen also (default
	 *            true).
	 * @param pLB
	 *            Optional pointer to a CAAMLowerBounds object.
	 * @return Evaluation results.
	 */
	public static CAAMEvaluationResults EvaluateModel(final CAAMModel pModel,
			final String gt_path, final String result_file,
			final boolean writeStills, final boolean writeMovies,
			final boolean autoinit, final boolean dump2screen,
			CAAMLowerBounds pLB) {

		Vector<String> vFilenames = new Vector<String>();
		String resDir = new String("__evaluation__");
		String resFile = new String();
		String resPath = new String();
		CAAMVisualizer AAMvis = new CAAMVisualizer(pModel);
		ModelSimpleImage image = new ModelSimpleImage();
		CAAMShape groundtruth = new CAAMShape();
		CAAMOptRes res = new CAAMOptRes();

		Vector<CAAMOptState> optStates = new Vector<CAAMOptState>();

		CAAMEvaluationResults evalRes = new CAAMEvaluationResults();
		CAAMLowerBounds lbs = new CAAMLowerBounds(pModel);

		System.err.println("Evaluating AAM...\n");

		// start total timer
		long startTime = System.currentTimeMillis();

		// make output directory
		resPath = gt_path + resDir + "\\";
		// _mkdir( resPath );
		File file = new File(resPath);
		file.mkdir();

		// test results file
		resFile = resPath + result_file;

		if (false == CAAMUtil.CreateTest(resFile)) {

			System.err.println("Result file " + resFile
					+ " could not be opened.\n");
			return evalRes;
		}

		// find asf files
		vFilenames = (Vector<String>) CAAMUtil.ScanSortDir(gt_path, "asf");
		int nImages = vFilenames.size();

		// setup
		int nExperiments = autoinit ? 1 : 8; // use 4 for xy only and 8 for xy,
												// s, theta
		int totalexp = nExperiments * nImages;
		double timeSum = .0;
		boolean fine_tuning = false;
		boolean writeError = false;

		// evaluate
		for (int i = 0; i < nImages; i++) {

			// read the image and shape
			image = new ModelSimpleImage();
			image = CAAMUtil.ReadExample(vFilenames.get(i), image, groundtruth,
					pModel.ModelReduction());
			String shapeName = CAAMUtil.GetFilename(CAAMUtil
					.RemoveExt(vFilenames.get(i)));
			// String shapeName = "testShape";

			// add shape extents (if requested)
			if (pModel.AddExtents() != 0.0) {
				groundtruth.AddShapeExtends((int) (.5 + pModel.AddExtents()));
			}

			// do displacements and optimizations
			int exp = 0;
			for (exp = 0; exp < nExperiments; exp++) {

				// use mean texture and mean shape size to mean size
				CDVector c = new CDVector();
				c.assign(0);
				CAAMShape shapeCopy = pModel.ReferenceShape();
				if (autoinit) {

					// perform automatic initialization
					CAAMInitializeStegmann Stegmann = new CAAMInitializeStegmann(
							pModel);
					Stegmann.Initialize(image, shapeCopy, c);
				} else {

					// do displacements
					double dtheta = 0.0, scale = 1.0, dx = 0, dy = 0;
					double dp = .2;

					switch (exp) {

					case 0:
						dx = -dp * groundtruth.Width();
						break;
					case 1:
						dx = dp * groundtruth.Width();
						break;
					case 2:
						dy = -dp * groundtruth.Height();
						break;
					case 3:
						dy = dp * groundtruth.Height();
						break;
					case 4:
						dtheta = -10 * Math.PI / 180.0;
						break;
					case 5:
						dtheta = 10 * Math.PI / 180.0;
						break;
					case 6:
						scale = 0.90;
						break;
					case 7:
						scale = 1.10;
						break;
					default:
						System.err.println("No such experiment.\n");
						System.exit(-1);
						break;
					}

					shapeCopy.AlignTo(groundtruth);
					shapeCopy.Translate(dx, dy);
					shapeCopy.Rotate(dtheta, true); // around cog
					shapeCopy.Scale(scale, true); // around cog
				}

				// write initial model points
				if (writeStills) {

					String fn = new String();
					fn = shapeName + "_exp" + exp + "_init.bmp";
					AAMvis.ShapeStill(image, shapeCopy, resPath + fn);
				}

				// do the optimization

				startTime = System.currentTimeMillis();
				res = pModel.OptimizeModel(image, shapeCopy, c, 30, optStates);

				boolean fineTuning;
				fineTuning = false;

				if (fineTuning) {
					System.err.println("*** OptimizeModelByFineTuning() ...");
					res = pModel.OptimizeModelByFineTuning(image, shapeCopy, c,
							1000, 0, 4);
					System.err.println(" Done!");
				}
				long endTime = System.currentTimeMillis();
				double ms = (endTime - startTime);

				// add result to list
				evalRes.AddResult(shapeCopy, groundtruth, ms, res);
				lbs.AddGroundtruth(groundtruth, image);

				// write optimized model points
				if (writeStills) {

					String fn = new String();
					fn = shapeName + "_exp" + exp + "_opt" + ".xml";
					AAMvis.ShapeStill(image, shapeCopy, resPath + fn);
				}

				// write movie
				if (writeMovies) {

					String mn = new String();
					mn = shapeName + "_exp" + exp + ".xml";
					AAMvis.OptimizationMovie(optStates, image, resPath + mn);
				}

				// write error
				if (writeError) {

					String fn = new String();
					fn = shapeName + "_exp" + exp + "_err.txt";
					try {
						PrintWriter fhE = new PrintWriter(resPath + fn);
						for (int j = 0; j < optStates.size(); j++) {

							fhE.println(optStates.get(j).error + "\t"
									+ optStates.get(j).damps + "\n");
						}
						fhE.close();
					} catch (IOException error) {
						error.printStackTrace();
					}
				}

				// write model parameters
				boolean writeParameters = true;
				if (writeParameters) {

					String fn = new String();
					fn = shapeName + "_exp" + exp + "_c.m";
					c.ToMatlab(resPath + fn, "c",
							"Recovered combined model parameters", false);

					CDVector b_g = new CDVector();
					CDVector b_s = new CDVector();
					pModel.Combined2ShapeParam(c, b_s);
					pModel.Combined2TexParam(c, b_g);

					fn = shapeName + "_exp" + exp + "_b_s.m";
					b_s.ToMatlab(resPath + fn, "b_s",
							"Recovered shape model parameters", false);

					fn = shapeName + "_exp" + exp + "_b_g.m";
					b_g.ToMatlab(resPath + fn, "b_g",
							"Recovered texture model parameters", false);
				}

				// write the shape result
				String resASF = new String();
				resASF = "testShape" + "_exp" + exp + ".asf";
				CAAMShape s = new CAAMShape(shapeCopy);
				s.SetHostImage(groundtruth.HostImage());
				s.Scale(pModel.ModelReduction());
				s.WriteASF(resPath + resASF, image.Width(), image.Height());
				// s.generateVOIs(targetImageSlice);
			} // end for exp loop
		} // end for i loop

		// print statistics to screen and file
		if (dump2screen)
			evalRes.PrintStatistics();
		evalRes.PrintStatistics(resFile);

		if (dump2screen)
			lbs.PrintStatistics();
		if (pLB != null)
			pLB = lbs;

		long endTime = System.currentTimeMillis();
		// print info
		System.err.println("Model evaluation took: " + (endTime - startTime)
				/ 1000d + " secs.\n");

		// return the evaluation results
		return evalRes;
	}

	/**
	 * Wrapper to optimizes a set of images and compares the result to a ground
	 * truth annotation. Optimizes a set of images and compares the result to a
	 * ground truth annotation. As initialization the ground truth pose is
	 * systematically displaced (default) or an automatic initialization is
	 * performed.
	 * 
	 * @param model
	 *            The model to evaluate.
	 * @param targeImageSlice
	 *            target image 2D slice
	 * @param sampleImage
	 *            reference sample image
	 * @param gt_path
	 *            Path to ground truth images and annotations.
	 * @param result_file
	 *            The file to write the results in.
	 * @param writeStills
	 *            If true two model border images are written; one of the
	 *            initialisation and one of the optimization.
	 * @param writeMovies
	 *            It true a movie of the whole optimization is written, one
	 *            frame per iteration.
	 * @param autoinit
	 *            If true automatic initialization is performed instead of the
	 *            systematic displacement of the ground truth pose.
	 * @param dump2screen
	 *            If true, results are written to the screen also (default
	 *            true).
	 * @return Evaluation results.
	 */
	public static CAAMEvaluationResults EvaluateModel(final CAAMModel pModel,
			ModelImage targetImageSlice, ModelImage sampleImage,
			final String gt_path, final String result_file,
			final boolean writeStills, final boolean writeMovies,
			final boolean autoinit, final boolean dump2screen) {
		return EvaluateModel(pModel, targetImageSlice, sampleImage, gt_path,
				result_file, writeStills, writeMovies, autoinit, dump2screen,
				null);
	}

	/**
	 * Wrapper to optimizes a set of images and compares the result to a ground
	 * truth annotation. Optimizes a set of images and compares the result to a
	 * ground truth annotation. As initialization the ground truth pose is
	 * systematically displaced (default) or an automatic initialization is
	 * performed.
	 * 
	 * @param model
	 *            The model to evaluate.
	 * @param targeImageSlice
	 *            target image 2D slice
	 * @param gt_path
	 *            Path to ground truth images and annotations.
	 * @param result_file
	 *            The file to write the results in.
	 * @param writeStills
	 *            If true two model border images are written; one of the
	 *            initialisation and one of the optimization.
	 * @param writeMovies
	 *            It true a movie of the whole optimization is written, one
	 *            frame per iteration.
	 * @param autoinit
	 *            If true automatic initialization is performed instead of the
	 *            systematic displacement of the ground truth pose.
	 * @param dump2screen
	 *            If true, results are written to the screen also (default
	 *            true).
	 * @return Evaluation results.
	 */
	public static CAAMEvaluationResults EvaluateModel(final CAAMModel pModel,
			ModelImage targetImageSlice, final String gt_path,
			final String result_file, final boolean writeStills,
			final boolean writeMovies, final boolean autoinit,
			final boolean dump2screen) {
		return EvaluateModel(pModel, targetImageSlice, gt_path, result_file,
				writeStills, writeMovies, autoinit, dump2screen, null);
	}

	/**
	 * Optimizes a set of images and compares the result to a ground truth
	 * annotation. As initialization the ground truth pose is systematically
	 * displaced (default) or an automatic initialisation is performed.
	 * 
	 * @param model
	 *            The model to evaluate.
	 * @param targeImageSlice
	 *            target image 2D slice
	 * @param sampleImage
	 *            reference sample image
	 * @param gt_path
	 *            Path to ground truth images and annotations.
	 * @param result_file
	 *            The file to write the results in.
	 * @param writeStills
	 *            If true two model border images are written; one of the
	 *            initialisation and one of the optimization.
	 * @param writeMovies
	 *            It true a movie of the whole optimization is written, one
	 *            frame per iteration.
	 * @param autoinit
	 *            If true automatic initialization is performed instead of the
	 *            systematic displacement of the ground truth pose.
	 * @param dump2screen
	 *            If true, results are written to the screen also (default
	 *            true).
	 * @param pLB
	 *            Optional pointer to a CAAMLowerBounds object.
	 * @return Evaluation results.
	 */
	public static CAAMEvaluationResults EvaluateModel(final CAAMModel pModel,
			ModelImage targetImageSlice, ModelImage sampleImage,
			final String gt_path, final String result_file,
			final boolean writeStills, final boolean writeMovies,
			final boolean autoinit, final boolean dump2screen,
			CAAMLowerBounds pLB) {

		long endTime;
		Vector<String> vFilenames = new Vector<String>();
		String resDir = new String("__evaluation__");
		String resFile = new String();
		String resPath = new String();
		CAAMVisualizer AAMvis = new CAAMVisualizer(pModel);
		ModelSimpleImage image = new ModelSimpleImage();
		CAAMShape groundtruth = new CAAMShape();
		CAAMShape sampleShape = new CAAMShape();
		CAAMOptRes res = new CAAMOptRes();

		Vector<CAAMOptState> optStates = new Vector<CAAMOptState>();

		CAAMEvaluationResults evalRes = new CAAMEvaluationResults();
		CAAMLowerBounds lbs = new CAAMLowerBounds(pModel);

		System.err.println("Evaluating AAM...\n");

		// start total timer
		long startTime = System.currentTimeMillis();

		// make output directory
		resPath = gt_path + resDir + "\\";
		// _mkdir( resPath );
		File file = new File(resPath);
		file.mkdir();

		// test results file
		resFile = resPath + result_file;

		if (false == CAAMUtil.CreateTest(resFile)) {

			System.err.println("Result file " + resFile
					+ " could not be opened.\n");
			return evalRes;
		}

		// find asf files
		// vFilenames = (Vector<String>)CAAMUtil.ScanSortDir(gt_path, "asf");
		int nImages = 1; // / vFilenames.size();

		// setup
		int nExperiments = autoinit ? 1 : 8; // use 4 for xy only and 8 for xy,
												// s, theta
		int totalexp = nExperiments * nImages;
		double timeSum = .0;
		boolean fine_tuning = false;
		boolean writeError = false;

		// read the image and shape
		image = new ModelSimpleImage();
		image = CAAMUtil.ReadExample_init(targetImageSlice, image, groundtruth,
				pModel.ModelReduction());
		sampleShape.ReadASFfromVOI(sampleImage);

		String shapeName = "testShape";

		// add shape extents (if requested)
		if (pModel.AddExtents() != 0.0) {
			groundtruth.AddShapeExtends((int) (.5 + pModel.AddExtents()));
		}

		// do displacements and optimizations
		int exp = 5;
		// for (exp = 0; exp < nExperiments; exp++) {
		CAAMInitializeStegmann Stegmann = null;
		// use mean texture and mean shape size to mean size
		CDVector c = new CDVector();
		c.assign(0);
		CAAMShape shapeCopy = pModel.ReferenceShape();
		if (autoinit) {

			// perform automatic initialization
			Stegmann = new CAAMInitializeStegmann(pModel, targetImageSlice,
					sampleImage, sampleShape);
			Stegmann.Initialize(image, shapeCopy, c, targetImageSlice);
		} else {

			// do displacements, ruida3
			double dtheta = 0.0, scale = 1.0, dx = 0, dy = 0;
			double dp = .1;

			switch (exp) {

			case 0:
				dx = -dp * groundtruth.Width();
				break;
			case 1:
				dx = dp * groundtruth.Width();
				break;
			case 2:
				dy = -dp * groundtruth.Height();
				break;
			case 3:
				dy = dp * groundtruth.Height();
				break;
			case 4:
				dtheta = -5 * Math.PI / 180.0;
				break;
			case 5:
				dtheta = 5 * Math.PI / 180.0;
				break;
			case 6:
				scale = 0.95;
				break;
			case 7:
				scale = 1.05;
				break;
			default:
				System.err.println("No such experiment.\n");
				System.exit(-1);
				break;
			}

			shapeCopy.AlignTo(groundtruth);
			shapeCopy.Translate(dx, dy);
			shapeCopy.Rotate(dtheta, true); // around cog
			shapeCopy.Scale(scale, true); // around cog

		}

		// write initial model points
		if (writeStills) {

			String fn = new String();
			fn = shapeName + "_exp" + exp + "_init.bmp";
			AAMvis.ShapeStill(image, shapeCopy, resPath + fn);
		}

		// do the optimization

		startTime = System.currentTimeMillis();
		res = pModel.OptimizeModel(image, shapeCopy, c, 30, optStates);

		boolean fineTuning;

		fineTuning = false;

		if (fineTuning) {
			System.err.println("*** OptimizeModelByFineTuning() ...");
			res = pModel.OptimizeModelByFineTuning(image, shapeCopy, c, 1000,
					0, 4);
			System.err.println(" Done!");
		}

		// write optimized model points
		if (writeStills) {

			String fn = new String();
			fn = shapeName + "_exp" + exp + "_opt" + ".xml";
			AAMvis.ShapeStill(image, shapeCopy, resPath + fn);
		}

		// write movie
		if (writeMovies) {

			String mn = new String();
			mn = shapeName + "_exp" + exp + ".xml";
			AAMvis.OptimizationMovie(optStates, image, resPath + mn);
		}

		// write error
		if (writeError) {

			String fn = new String();
			fn = shapeName + "_exp" + exp + "_err.txt";
			try {
				PrintWriter fhE = new PrintWriter(resPath + fn);
				for (int j = 0; j < optStates.size(); j++) {

					fhE.println(optStates.get(j).error + "\t"
							+ optStates.get(j).damps + "\n");
				}
				fhE.close();
			} catch (IOException error) {
				error.printStackTrace();
			}
		}

		// write model parameters
		boolean writeParameters = false;
		if (writeParameters) {

			String fn = new String();
			fn = shapeName + "_exp" + exp + "_c.m";
			c.ToMatlab(resPath + fn, "c",
					"Recovered combined model parameters", false);

			CDVector b_g = new CDVector();
			CDVector b_s = new CDVector();
			pModel.Combined2ShapeParam(c, b_s);
			pModel.Combined2TexParam(c, b_g);

			fn = shapeName + "_exp" + exp + "_b_s.m";
			b_s.ToMatlab(resPath + fn, "b_s",
					"Recovered shape model parameters", false);

			fn = shapeName + "_exp" + exp + "_b_g.m";
			b_g.ToMatlab(resPath + fn, "b_g",
					"Recovered texture model parameters", false);
		}

		// write the shape result
		String resASF = new String();
		resASF = "testShape" + "_exp" + exp + ".asf";
		CAAMShape s = new CAAMShape(shapeCopy);
		s.SetHostImage(groundtruth.HostImage());
		s.Scale(pModel.ModelReduction());

		s.generateVOIs(targetImageSlice);

		VOIVector voiVector = targetImageSlice.getVOIs();
		VOI voi = voiVector.VOIAt(0);

		VOIBaseVector va = voi.getCurves();
		VOIBase v = va.get(0);
		VOIBase vTemp = (VOIBase) v.clone();
		CAAMShape polygon1Shape = new CAAMShape();
		polygon1Shape.ConvertASFfromVOI(v);
		float polygon1Area = (float) polygon1Shape.ShapeSize();

		int nPts = vTemp.size();

		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];
		vTemp.exportArrays(xPts, yPts, zPts);
		counterClockwise(xPts, yPts, zPts, nPts);

		poly polygon = new poly(nPts);
		polygon.n = nPts;

		for (int z = 0; z < nPts; z++) {
			polygon.pt[z] = new POINT();
			polygon.pt[z].x = xPts[z];
			polygon.pt[z].y = yPts[z];
		}
		ShapeSimilarity shapeCompare = new ShapeSimilarity();
		double[] result = new double[6];
		shapeCompare.comparePolygon(polygon, Stegmann.meanShapePolygon, result);
		System.err.println("result[0] = " + result[0]);

		System.err.println("result[0] = " + result[0] + "\t" + result[1] + "\t"
				+ result[2] + "\t" + result[3] + "\t" + result[4] + "\t"
				+ result[5]);

		double areaDiff = Math.abs(s.ShapeSize()
				- Stegmann.m_sCentredRefshape.ShapeSize());
		System.err.println("areaDiff = " + areaDiff);
		if (result[0] > 3 || result[2] > 30 || result[3] > 30 || areaDiff >= 65) {
			Stegmann.m_sCentredRefshape.generateVOIs(targetImageSlice);
		}

		// print statistics to screen and file
		if (dump2screen)
			evalRes.PrintStatistics();
		// evalRes.PrintStatistics(resFile);

		if (dump2screen)
			lbs.PrintStatistics();
		// lbs.PrintStatistics(resPath + "lower_bounds.txt");
		if (pLB != null)
			pLB = lbs;

		endTime = System.currentTimeMillis();
		// print info
		// System.err.println("Results are written in " + resFile + "\n");
		System.err.println("Model evaluation took: " + (endTime - startTime)
				/ 1000d + " secs.\n");

		// return the evaluation results
		return evalRes;
	}

	/**
	 * Change the VOI points to counter clock wise order.
	 * 
	 * @param x
	 *            x coordinates
	 * @param y
	 *            y coordinates
	 * @param z
	 *            z coordinates
	 * @param nPts
	 *            number of points
	 */
	private static void counterClockwise(float[] x, float[] y, float[] z,
			int nPts) {
		float[] x1 = new float[nPts];
		float[] y1 = new float[nPts];
		float[] z1 = new float[nPts];
		int index = 0;
		for (int i = nPts - 1; i >= 0; i--) {
			x1[index] = x[i];
			y1[index] = y[i];
			z1[index] = z[i];
			index++;
		}

		for (int i = 0; i < nPts; i++) {
			x[i] = x1[i];
			y[i] = y1[i];
			z[i] = z1[i];
		}

	}

	/**
	 * Wrapper to optimizes a set of images and compares the result to a ground
	 * truth annotation. Optimizes a set of images and compares the result to a
	 * ground truth annotation. As initialization the ground truth pose is
	 * systematically displaced (default) or an automatic initialization is
	 * performed.
	 * 
	 * @param model
	 *            The model to evaluate.
	 * @param targeImageSlice
	 *            target image 2D slice
	 * @param gt_path
	 *            Path to ground truth images and annotations.
	 * @param result_file
	 *            The file to write the results in.
	 * @param writeStills
	 *            If true two model border images are written; one of the
	 *            initialisation and one of the optimization.
	 * @param writeMovies
	 *            It true a movie of the whole optimization is written, one
	 *            frame per iteration.
	 * @param autoinit
	 *            If true automatic initialization is performed instead of the
	 *            systematic displacement of the ground truth pose.
	 * @param dump2screen
	 *            If true, results are written to the screen also (default
	 *            true).
	 * @param pLB
	 *            Optional pointer to a CAAMLowerBounds object.
	 * @return Evaluation result
	 * */
	public static CAAMEvaluationResults EvaluateModel(final CAAMModel pModel,
			ModelImage targetImageSlice, final String gt_path,
			final String result_file, final boolean writeStills,
			final boolean writeMovies, final boolean autoinit,
			final boolean dump2screen, CAAMLowerBounds pLB) {

		long endTime;
		Vector<String> vFilenames = new Vector<String>();
		String resDir = new String("__evaluation__");
		String resFile = new String();
		String resPath = new String();
		CAAMVisualizer AAMvis = new CAAMVisualizer(pModel);
		ModelSimpleImage image = new ModelSimpleImage();
		CAAMShape groundtruth = new CAAMShape();
		CAAMShape sampleShape = new CAAMShape();
		CAAMOptRes res = new CAAMOptRes();

		Vector<CAAMOptState> optStates = new Vector<CAAMOptState>();

		CAAMEvaluationResults evalRes = new CAAMEvaluationResults();
		CAAMLowerBounds lbs = new CAAMLowerBounds(pModel);

		System.err.println("Evaluating AAM...\n");

		// start total timer
		long startTime = System.currentTimeMillis();

		// make output directory
		resPath = gt_path + resDir + "\\";
		// _mkdir( resPath );
		File file = new File(resPath);
		file.mkdir();

		// test results file
		resFile = resPath + result_file;

		if (false == CAAMUtil.CreateTest(resFile)) {

			System.err.println("Result file " + resFile
					+ " could not be opened.\n");
			return evalRes;
		}

		int nImages = 1; // / vFilenames.size();

		// setup
		int nExperiments = autoinit ? 1 : 8; // use 4 for xy only and 8 for xy,
												// s, theta
		int totalexp = nExperiments * nImages;
		double timeSum = .0;
		boolean fine_tuning = false;
		boolean writeError = false;

		// read the image and shape
		image = new ModelSimpleImage();
		image = CAAMUtil.ReadExample(targetImageSlice, image, groundtruth,
				pModel.ModelReduction());

		String shapeName = "testShape";

		// add shape extents (if requested)
		if (pModel.AddExtents() != 0.0) {
			groundtruth.AddShapeExtends((int) (.5 + pModel.AddExtents()));
		}

		// do displacements and optimizations
		int exp = 5;
		// for (exp = 0; exp < nExperiments; exp++) {

		// use mean texture and mean shape size to mean size
		CDVector c = new CDVector();
		c.assign(0);
		CAAMShape shapeCopy = pModel.ReferenceShape();
		if (autoinit) {

			// perform automatic initialization
			CAAMInitializeStegmann Stegmann = new CAAMInitializeStegmann(
					pModel, targetImageSlice, null, sampleShape);
			Stegmann.Initialize(image, shapeCopy, c, targetImageSlice);
		} else {

			// do displacements, ruida3
			double dtheta = 0.0, scale = 1.0, dx = 0, dy = 0;
			double dp = .1;

			/*
			 * switch (exp) {
			 * 
			 * case 0: dx = -dp * groundtruth.Width(); break; case 1: dx = dp *
			 * groundtruth.Width(); break; case 2: dy = -dp *
			 * groundtruth.Height(); break; case 3: dy = dp *
			 * groundtruth.Height(); break; case 4: dtheta = -5 * Math.PI /
			 * 180.0; break; case 5: dtheta = 5 * Math.PI / 180.0; break; case
			 * 6: scale = 0.95; break; case 7: scale = 1.05; break; default:
			 * System.err.println("No such experiment.\n"); System.exit(-1);
			 * break; }
			 */

			shapeCopy.AlignTo(groundtruth);
			shapeCopy.Translate(dx, dy);
			shapeCopy.Rotate(dtheta, true); // around cog
			shapeCopy.Scale(scale, true); // around cog

		}

		// write initial model points
		if (writeStills) {

			String fn = new String();
			fn = shapeName + "_exp" + exp + "_init.bmp";
			AAMvis.ShapeStill(image, shapeCopy, resPath + fn);
		}

		// do the optimization
		shapeCopy.generateVOIs(targetImageSlice);
		// new ViewJFrameImage(targetImageSlice);
		// pause();

		startTime = System.currentTimeMillis();
		res = pModel.OptimizeModel(image, shapeCopy, c, 30, optStates);

		boolean fineTuning;

		fineTuning = false;
		if (fineTuning) {
			System.err.println("*** OptimizeModelByFineTuning() ...");
			res = pModel.OptimizeModelByFineTuning(image, shapeCopy, c, 1000,
					0, 4);
			System.err.println(" Done!");
		}

		// write optimized model points
		if (writeStills) {

			String fn = new String();
			fn = shapeName + "_exp" + exp + "_opt" + ".bmp";
			AAMvis.ShapeStill(image, shapeCopy, resPath + fn);
		}

		// write movie
		if (writeMovies) {

			String mn = new String();
			mn = shapeName + "_exp" + exp + ".xml";
			AAMvis.OptimizationMovie(optStates, image, resPath + mn);
		}

		// write error
		if (writeError) {

			String fn = new String();
			fn = shapeName + "_exp" + exp + "_err.txt";
			try {
				PrintWriter fhE = new PrintWriter(resPath + fn);
				for (int j = 0; j < optStates.size(); j++) {

					fhE.println(optStates.get(j).error + "\t"
							+ optStates.get(j).damps + "\n");
				}
				fhE.close();
			} catch (IOException error) {
				error.printStackTrace();
			}
		}

		// write model parameters
		boolean writeParameters = false;
		if (writeParameters) {

			String fn = new String();
			fn = shapeName + "_exp" + exp + "_c.m";
			c.ToMatlab(resPath + fn, "c",
					"Recovered combined model parameters", false);

			CDVector b_g = new CDVector();
			CDVector b_s = new CDVector();
			pModel.Combined2ShapeParam(c, b_s);
			pModel.Combined2TexParam(c, b_g);

			fn = shapeName + "_exp" + exp + "_b_s.m";
			b_s.ToMatlab(resPath + fn, "b_s",
					"Recovered shape model parameters", false);

			fn = shapeName + "_exp" + exp + "_b_g.m";
			b_g.ToMatlab(resPath + fn, "b_g",
					"Recovered texture model parameters", false);
		}

		// write the shape result
		String resASF = new String();
		resASF = "testShape" + "_exp" + exp + ".asf";
		CAAMShape s = new CAAMShape(shapeCopy);
		s.SetHostImage(groundtruth.HostImage());
		s.Scale(pModel.ModelReduction());
		s.generateVOIs(targetImageSlice);

		// print statistics to screen and file
		if (dump2screen)
			evalRes.PrintStatistics();
		// evalRes.PrintStatistics(resFile);

		if (dump2screen)
			lbs.PrintStatistics();
		// lbs.PrintStatistics(resPath + "lower_bounds.txt");
		if (pLB != null)
			pLB = lbs;

		endTime = System.currentTimeMillis();
		// print info
		// System.err.println("Results are written in " + resFile + "\n");
		System.err.println("Model evaluation took: " + (endTime - startTime)
				/ 1000d + " secs.\n");

		// return the evaluation results
		return evalRes;
	}

	/**
	 * debug function
	 */
	public static void pause() {
		int count = 0;
		System.err.println("enter to continue: ");
		try {
			// eat any pending characters
			for (int av = System.in.available(); av > 0; av--) {
				System.in.read();
			}
			System.in.read();// wait for user to hit Enter, discard result
		} catch (IOException e) {
			System.err.println("keyboard failed: " + e);
		}

	}

	/**
	 * Wrapper to optimize a set of images and compares the result to a ground
	 * truth annotation using a sequence of AAMs. As initialization the ground
	 * truth pose is systematically displaced (default) or an automatic
	 * initialization is performed.
	 * 
	 * @param modelSeq
	 *            A sequence of models.
	 * @param gt_path
	 *            Path to ground truth images and annotations.
	 * @param result_file
	 *            The file to write the results in.
	 * @param writeStills
	 *            If true two model border images are written; one of the
	 *            initialisation and one of the optimization.
	 * @param writeMovies
	 *            It true a movie of the whole optimization is written, one
	 *            frame per iteration.
	 * @param autoinit
	 *            If true automatic initialization is performed instead of the
	 *            systematic displacement of the ground truth pose.
	 * @param dump2screen
	 *            If true, results are written to the screen also (default
	 *            true).
	 * @return Evaluation results.
	 */
	public static CAAMEvaluationResults EvaluateModelSeq(
			final CAAMModelSeq modelSeq, final String gt_path,
			final String result_file, final boolean writeStills,
			final boolean writeMovies, final boolean autoinit,
			final boolean dump2screen) {
		return EvaluateModelSeq(modelSeq, gt_path, result_file, writeStills,
				writeMovies, autoinit, dump2screen, null);
	}

	/**
	 * Optimizes a set of images and compares the result to a ground truth
	 * annotation using a sequence of AAMs. As initialization the ground truth
	 * pose is systematically displaced (default) or an automatic initialisation
	 * is performed.
	 * 
	 * @param modelSeq
	 *            A sequence of models.
	 * @param gt_path
	 *            Path to ground truth images and annotations.
	 * @param result_file
	 *            The file to write the results in.
	 * @param writeStills
	 *            If true two model border images are written; one of the
	 *            initialisation and one of the optimization.
	 * @param writeMovies
	 *            It true a movie of the whole optimization is written, one
	 *            frame per iteration.
	 * @param autoinit
	 *            If true automatic initialization is performed instead of the
	 *            systematic displacement of the ground truth pose.
	 * @param dump2screen
	 *            If true, results are written to the screen also (default
	 *            true).
	 * @param pLBS
	 *            Optional pointer to a CAAMLBShapeModel object.
	 * @return Evaluation results.
	 */
	public static CAAMEvaluationResults EvaluateModelSeq(
			final CAAMModelSeq modelSeq, final String gt_path,
			final String result_file, final boolean writeStills,
			final boolean writeMovies, final boolean autoinit,
			final boolean dump2screen, CAAMLowerBounds pLB) {

		// LATER: Hver model burde visualiseres rigtigt (egentligt ikke noget
		// problem)

		Vector<String> vFilenames = new Vector<String>();
		String resDir = new String("__evaluation__");
		String resFile = new String();
		String resPath = new String();

		CAAMVisualizer AAMvis = new CAAMVisualizer(modelSeq.FinalModel());
		CAAMShape groundtruth = new CAAMShape();
		CAAMOptRes res = new CAAMOptRes();
		Vector<CAAMOptState> optStates = new Vector<CAAMOptState>();

		CAAMEvaluationResults evalRes = new CAAMEvaluationResults();
		CAAMLowerBounds lbs = new CAAMLowerBounds(modelSeq.FinalModel());
		CDVector initTime = new CDVector();

		System.err.println("Evaluating AAM...\n");

		// start total timer
		long startTime = System.currentTimeMillis();

		// make output directory
		resPath = gt_path + resDir + "\\";
		// _mkdir( resPath );
		File file = new File(resPath);
		file.mkdir();

		// test results file
		resFile = resPath + result_file;
		if (false == CAAMUtil.CreateTest(resFile)) {

			System.err.println("Result file " + resFile
					+ " could not be opened.\n");
			return evalRes;
		}

		// find asf files, ruida
		vFilenames = (Vector<String>) CAAMUtil.ScanSortDir(gt_path, "asf");
		int nImages = vFilenames.size();
		// setup
		int nExperiments = autoinit ? 1 : 4; // use 4 for xy only and 8 for xy,
												// s, theta
		int totalexp = nExperiments * nImages;
		double ms, timeSum = .0;
		boolean writeError = false;
		int ninit = 0;

		if (autoinit) {

			initTime.Resize(totalexp);
		}

		// for each image
		for (int i = 0; i < nImages; i++) {

			// read the image and shape
			ModelSimpleImage imageOrg = new ModelSimpleImage();
			ModelSimpleImage imageScaled = new ModelSimpleImage();
			imageOrg = CAAMUtil.ReadExample(vFilenames.get(i), imageOrg,
					groundtruth, 1);
			String shapeName = CAAMUtil.GetFilename(CAAMUtil
					.RemoveExt(vFilenames.get(i)));

			// add shape extents (if requested)
			if (modelSeq.FinalModel().AddExtents() != 0.0) {
				System.err.println("Shape extents not supported.");
				System.exit(-1);
			}

			// scale image (if requested) LATER: this should be improved on
			// later
			imageScaled = imageOrg;
			if (modelSeq.Model(0).ModelReduction() > 1) {

				// imageScaled.ReducePyr(modelSeq.Model(0).ModelReduction());
				imageScaled.subSample2dBy2();
			}

			// do displacements and optimizations
			for (int exp = 0; exp < nExperiments; exp++) {

				// use mean texture and mean shape sized to mean size
				CDVector c = new CDVector(modelSeq.Model(0).CombinedPCA()
						.NParameters());
				c.assign(.0);
				CAAMShape shapeCopy = modelSeq.Model(0).ReferenceShape();
				if (autoinit) {

					// perform automatic initialization
					long init_startTime = System.currentTimeMillis();

					CAAMInitializeStegmann Stegmann = new CAAMInitializeStegmann(
							modelSeq.Model(0));
					Stegmann.Initialize(imageScaled, shapeCopy, c);

					long init_endTime = System.currentTimeMillis();
					initTime.m_data[ninit++] = (init_endTime - init_startTime);

				} else {

					// scale ground truth to this level
					CAAMShape gt_scaled = new CAAMShape(groundtruth);
					gt_scaled.Scale(1. / modelSeq.Model(0).ModelReduction());

					// do displacements
					double dtheta = 0.0, scale = 1.0, dx = 0, dy = 0;
					double dp = .1;
					switch (exp) {

					case 0:
						dx = -dp * gt_scaled.Width();
						break;
					case 1:
						dx = dp * gt_scaled.Width();
						break;
					case 2:
						dy = -dp * gt_scaled.Height();
						break;
					case 3:
						dy = dp * gt_scaled.Height();
						break;
					case 4:
						dtheta = -5 * Math.PI / 180.0;
						break;
					case 5:
						dtheta = 5 * Math.PI / 180.0;
						break;
					case 6:
						scale = 0.95;
						break;
					case 7:
						scale = 1.05;
						break;
					default:
						System.err.println("No such experiment.\n");
						System.exit(-1);
						break;
					}

					shapeCopy.AlignTo(gt_scaled);
					shapeCopy.Translate(dx, dy);
					shapeCopy.Rotate(dtheta, true); // around cog
					shapeCopy.Scale(scale, true); // around cog
				}

				// scale shape to final image coordinates
				modelSeq.ScaleShape2Final(0, shapeCopy);

				// write initial model points
				if (writeStills) {

					String fn = new String();
					fn = shapeName + "_exp" + +exp + "_init.bmp";
					AAMvis.ShapeStill(imageOrg, shapeCopy, resPath + fn);
				}

				long t_startTime = System.currentTimeMillis();
				// for each model
				for (int m = 0; m < modelSeq.NModels(); m++) {

					// scale image
					if (m != 0) {

						long t_endTime = System.currentTimeMillis();
						imageScaled = imageOrg;
						if (modelSeq.Model(m).ModelReduction() > 1) {

							// imageScaled.ReducePyr(modelSeq.Model(m).ModelReduction());
							imageScaled.subSample2dBy2();
						}
						t_startTime = System.currentTimeMillis();
					}

					// scale shape to current image coordinates
					modelSeq.ScaleShape2Model(m, shapeCopy);

					// set initial model parameters
					if (m > 0) {

						// sample texture under shape and project into the
						// current model
						// (this sample should really be used to start the
						// OptimizeModel
						// below, otherwise we're doing the same work twice....
						// )
						modelSeq.Model(m).Shape2Combined(shapeCopy,
								imageScaled, c);
					}

					// do the optimization
					res = modelSeq.Model(m).OptimizeModel(imageScaled,
							shapeCopy, c, 30, optStates);

					boolean fineTuning;
					fineTuning = false;

					if (fineTuning) {

						System.err
								.println("*** OptimizeModelByFineTuning() ...");
						res = modelSeq.Model(m).OptimizeModelByFineTuning(
								imageScaled, shapeCopy, c, 1000, 0, 3);
						System.err.println(" Done!");
					}

					// scale shape result to final image coordinates
					modelSeq.ScaleShape2Final(m, shapeCopy);

					// write optimized model points
					long t_endTime = System.currentTimeMillis();
					if (writeStills) {

						String fn = new String();
						fn = shapeName + "_exp" + exp + "_opt_m" + m + ".bmp";
						AAMvis.ShapeStill(imageOrg, shapeCopy, resPath + fn);
					}
					t_startTime = System.currentTimeMillis();
				}

				// correct ground truth scale
				groundtruth.Scale(1. / modelSeq.FinalModel().ModelReduction());

				long t_endTime = System.currentTimeMillis();
				ms = (t_endTime - t_startTime);

				// add result to list
				evalRes.AddResult(shapeCopy, groundtruth, ms, res);
				lbs.AddGroundtruth(groundtruth, imageOrg);

				// write movie
				if (writeMovies) {

					String mn = new String();
					// mn.format("%s_exp%02i.avi", shapeName, exp);
					mn = shapeName + "_exp" + exp + ".xml";
					AAMvis.OptimizationMovie(optStates, imageOrg, resPath + mn);
				}

				// write error
				if (writeError) {

					String fn = new String();
					fn = shapeName + "_exp" + exp + "i_err.txt";
					try {
						PrintWriter fhE = new PrintWriter(resPath + fn);
						for (int j = 0; j < optStates.size(); j++) {

							fhE.println(optStates.get(j).error + "\t"
									+ optStates.get(j).damps + "\n");
						}
						fhE.close();
					} catch (IOException error) {
						error.printStackTrace();
					}
				}

				// write model parameters
				boolean writeParameters = true;
				if (writeParameters) {

					String fn = new String();
					fn = shapeName + "_exp" + exp + "_c.m";
					c.ToMatlab(resPath + fn, "c",
							"Recovered combined model parameters", false);

					CDVector b_g = new CDVector();
					CDVector b_s = new CDVector();
					modelSeq.FinalModel().Combined2ShapeParam(c, b_s);
					modelSeq.FinalModel().Combined2TexParam(c, b_g);

					fn = shapeName + "_exp" + exp + "_b_s.m";
					b_s.ToMatlab(resPath + fn, "b_s",
							"Recovered shape model parameters", false);

					fn = shapeName + "_exp" + exp + "_b_g.m";
					b_g.ToMatlab(resPath + fn, "b_g",
							"Recovered texture model parameters", false);
				}

				// write the shape result
				String resASF = new String();
				resASF = shapeName + "_exp" + exp + ".asf";
				CAAMShape s = new CAAMShape(shapeCopy);
				s.SetHostImage(groundtruth.HostImage());
				s.WriteASF(resPath + resASF, imageOrg.Width(),
						imageOrg.Height());

			} // for each experiment
		} // for each image

		// print statistics to screen and file
		if (dump2screen)
			evalRes.PrintStatistics();

		// FILE *fh = fopen( resFile, "wt" );
		try {
			PrintWriter fh = new PrintWriter(resFile);
			evalRes.PrintStatistics(fh);
			if (autoinit) {

				fh.println("Initialisation time: " + initTime.Mean() + " ("
						+ initTime.Std() / Math.sqrt(initTime.Length()) + ")  "
						+ initTime.Median() + " " + initTime.Min() + " "
						+ initTime.Max());
			}
			fh.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		if (dump2screen)
			lbs.PrintStatistics();
		if (pLB != null)
			pLB = lbs;

		// print info
		long endTime = System.currentTimeMillis();
		// System.err.println("Results are written in " + resFile + "\n");
		System.err.println("Model evaluation took: "
				+ ((endTime - startTime) / 1000d) + " secs.\n");

		// return the evaluation results
		return evalRes;
	}

}