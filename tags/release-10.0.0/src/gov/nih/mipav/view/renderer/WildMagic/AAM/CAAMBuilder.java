package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;
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
 * Factory object that produces CAAMModel objects. Main tasks are the estimation
 * of parameter update matrices and verbose dumping of model information. Most
 * other tasks are simple calls into CAAMModel, CAAMShape etc.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMBuilder extends CAAMObject {

	/** The raw (unaligned) shapes. */
	protected CAAMShapeCollection m_Shapes = new CAAMShapeCollection();

	/** The aligned shapes. */
	protected CAAMShapeCollection m_AlignedShapes = new CAAMShapeCollection();

	/** The texture samples. */
	protected Vector<CDVector> m_vTexture = new Vector<CDVector>();

	/** current default to false, not used */
	protected boolean m_bVerbose;
	protected boolean m_bWriteRegMovie;
	protected boolean m_bWriteVarImage;
	protected boolean m_bMakeDocumentation;
	protected int m_iTSSubsampling;
	protected int m_iWarpMethod;

	/** the model we're building. */
	protected CAAMModel m_pModel;

	/**
	 * Build the model from text based file
	 * 
	 * @param model
	 *            model to build
	 * @param inDir
	 *            file instream dir.
	 */
	public void BuildFromFiles(CAAMModel model, final String inDir) {
		BuildFromFiles(model, inDir, "", 1, -1);
	}

	/**
	 * Build the model from text based file and AAM configuration file
	 * 
	 * @param model
	 *            model to build
	 * @param inDir
	 *            text file input dir
	 * @param acf
	 *            aam configuration file
	 */
	public void BuildFromFiles(CAAMModel model, final String inDir,
			final String acf) {
		BuildFromFiles(model, inDir, acf, 1, -1);
	}

	/**
	 * Build the model from prostate images and VOIs
	 * 
	 * @param model
	 *            model to build
	 * @param modelImageVector
	 *            array of prostate image and VOIs
	 * @return build success or not
	 */
	public boolean BuildModel(CAAMModel model,
			Vector<ModelImage> modelImageVector) {
		return BuildModel(model, modelImageVector, 1, -1);
	}

	/**
	 * Diver method for model generation. This method automates the model
	 * generation as much as possible by using the various class methods for all
	 * the sequences in the task of producing a model.
	 * 
	 * @param model
	 *            The generated model.
	 * @param inDir
	 *            Input directory where annotations (.asf) resides.
	 * @param acf
	 *            Filename of an AAM configuration file. If omitted defaults are
	 *            used.
	 * @param modelReduction
	 *            Model reduction multiplier. Default off == 1. Useful when
	 *            building multi-scale AAMs.
	 * @param excludeShape
	 *            Excludes one shape number 'excludeShape' from the input
	 *            directory. Default -1, i.e. no shapes are removed. Used to
	 *            perform leave-one-out testing.
	 * @return Nothing.
	 */
	public void BuildFromFiles(CAAMModel model, final String inDir,
			final String acf, final int modelReduction, final int excludeShape) {
		Vector<String> asfFiles = new Vector<String>();
		asfFiles = (Vector<String>) CAAMUtil.ScanSortDir(
				CAAMUtil.AddBackSlash(inDir), "asf");
		this.BuildFromFiles(model, asfFiles, acf, modelReduction, excludeShape);
	}

	/**
	 * Build the model in supervised way, with given prostate images and VOIs.
	 * 
	 * @param model
	 *            The generated model.
	 * @param modelImageVector
	 *            vector for prostate images and VOIs
	 * @param modelReduction
	 *            Model reduction multiplier. Default off == 1. Useful when
	 *            building multi-scale AAMs.
	 * @param excludeShape
	 *            Excludes one shape number 'excludeShape' from the input
	 *            directory. Default -1, i.e. no shapes are removed. Used to
	 *            perform leave-one-out testing.
	 * @return nothing
	 */
	public boolean BuildModel(CAAMModel model,
			Vector<ModelImage> modelImageVector, int modelReduction,
			final int excludeShape) {
		// set the internal model pointer that all
		// private methods work upon.
		boolean isOutsideImage = false;
		m_pModel = model;

		// set up
		long startTimer = System.currentTimeMillis();

		// setup configuration
		setACF();

		// set the model reduction (if requested)
		if (modelReduction != 1) {
			m_pModel.m_iModelReduction = modelReduction;
		}

		// read images and shapes
		System.err.println("Reading images and annotations...");
		boolean addCompleteImage = false;
		boolean valid_shapes = LoadShapes(m_Shapes, modelImageVector,
				m_pModel.m_iModelReduction, addCompleteImage,
				m_pModel.m_dAddExtents, excludeShape);
		if (valid_shapes == false) {
			System.err.println("The training set is not valid. Exiting.");
			return false;
		}

		// align shapes
		DoShapeAlignment(m_pModel.m_bUseTangentSpace);

		// use a different set of shapes for the shape PCA
		boolean shapePCAoverride;
		String shapePCAoverideDir;

		shapePCAoverideDir = new String();
		shapePCAoverride = false;

		CAAMShapeCollection orgShapes = new CAAMShapeCollection();
		CAAMShapeCollection orgAlignedShapes = new CAAMShapeCollection();
		if (shapePCAoverride) {

			// backup shape collections
			orgShapes.assign(m_Shapes);
			orgAlignedShapes.assign(m_AlignedShapes);

			// find files
			Vector<String> shapeAsfFiles = new Vector<String>();
			shapeAsfFiles = (Vector<String>) CAAMUtil.ScanSortDir(
					CAAMUtil.AddBackSlash(shapePCAoverideDir), "asf");

			// load shapes
			valid_shapes = LoadShapes(shapeAsfFiles, m_Shapes,
					m_pModel.m_iModelReduction, addCompleteImage,
					m_pModel.m_dAddExtents, excludeShape);

			if (valid_shapes == false) {

				System.err
						.println("The shape PCA training set is not valid. Exiting.");
				System.exit(-1);
			}

			System.err.println("INFO: Shape PCA overidden using "
					+ m_Shapes.NShapes() + " shapes from:\n  "
					+ shapePCAoverideDir);

			// NOTICE
			// using this feature the texture pca will also be slightly
			// affected, since the referenceFrame is using the reference
			// shape from the new shape PCA
			//
			DoShapeAlignment(m_pModel.m_bUseTangentSpace);

			// set to number of shape to the number of traning textures
			m_pModel.m_iNShapes = orgShapes.NShapes();
		}

		// shape PCA
		System.err.println("Doing PCA on the shape data...");
		for (int s = 0; s < m_AlignedShapes.NShapes(); s++) {

			m_pModel.m_ShapePCA.InsertDataItem(m_AlignedShapes.get(s));
		}
		boolean isPCAsuccess = m_pModel.m_ShapePCA.DoPCA(m_bMakeDocumentation);
		if (isPCAsuccess == false)
			return false;
		// truncate eigenvectors and eigenvalues to
		// satisfy the variance explanation level
		// constraint (vlec) or by using parallel analysis
		if (m_pModel.m_iShapeTrunc == -1) {

			m_pModel.m_ShapePCA.TruncateParallel();
			m_pModel.m_ShapePCA.ClearDataItems();
		} else {
			m_pModel.m_ShapePCA.ClearDataItems();
			m_pModel.m_ShapePCA.TruncateVar(m_pModel.m_iShapeTrunc / 100.);
		}

		// use a different set of shapes for the shape PCA
		if (shapePCAoverride) {

			// restore shape collections
			m_Shapes.assign(orgShapes);
			m_AlignedShapes.assign(orgAlignedShapes);
		}

		// initialize the ReferenceFrame object
		CAAMShape rs = new CAAMShape();
		m_AlignedShapes.ReferenceShape(rs);
		m_pModel.m_pReferenceFrame = new CAAMReferenceFrame();
		m_pModel.m_pReferenceFrame.Setup(rs, m_pModel.m_bUseConvexHull);

		// initialize the AnalyzeSynthesize object
		switch (m_iWarpMethod) {

		case 0:
		case 1: // software
			m_pModel.m_pAnalyzeSynthesize = new CAAMAnalyzeSynthesizeSoftware(
					m_pModel.m_pReferenceFrame);
			break;

		case 2: // hardware accelerated OpenGL, diabled
			/*
			 * m_pModel.m_pAnalyzeSynthesize = new CAAMAnalyzeSynthesizeOpenGL(
			 * m_pModel.m_pReferenceFrame);
			 */
			break;

		default:
			System.err.println("Wrong warping method (method==" + m_iWarpMethod
					+ "). Aborting!\n");
			System.exit(-1);
			break;
		}

		// build texture vectors
		System.err.println("Building texture vectors...");
		BuildTextureVectorsFromImage();

		// normalize textures
		System.err.println("Normalizing texture vectors...");
		NormalizeTextureVectors();

		// apply mappings to the texture vectors
		MapTextures();

		// calc texture variances
		// (we can't use the diagonal of the covariance matrix due to the Q-R
		// mode trick...)
		System.err.println("Calculating texture variances...");
		CAAMMathUtil.CalcElementVar(m_vTexture, m_pModel.m_vTextureVar);

		// dump var image
		if (m_bWriteVarImage) {

			String fn = new String("modelvar.xml");
			System.err.println("Writing variance image " + fn + "...");
			m_pModel.WriteVarianceMap(fn);
		}

		// dump registration movie
		if (m_bWriteRegMovie) {

			String movieName = new String("registration.xml");
			CAAMVisualizer AAMvis = new CAAMVisualizer(m_pModel);
			AAMvis.RegistrationMovie(movieName, m_vTexture);
		}

		// texture PCA
		System.err.println("Doing PCA on the texture data...");
		for (int t = 0; t < m_vTexture.size(); t++) {

			m_pModel.m_TexturePCA.InsertDataItem(m_vTexture.get(t));
		}
		boolean isValid = m_pModel.m_TexturePCA.DoPCA();
		if (isValid == false)
			return false;

		// truncate eigenvectors and eigenvalues to
		// satisfy the variance explanation level
		// constraint (vlec) or by using parallel analysis
		if (m_pModel.m_iTextureTrunc == -1) {

			m_pModel.m_TexturePCA.TruncateParallel();
			m_pModel.m_TexturePCA.ClearDataItems();
		} else {
			m_pModel.m_TexturePCA.ClearDataItems();
			m_pModel.m_TexturePCA.TruncateVar(m_pModel.m_iTextureTrunc / 100.);
		}

		// do the combined PCA
		System.err.println("Doing combined PCA...");
		Vector<CDVector> bVectors = new Vector<CDVector>();
		bVectors = DoCombinedPCA();

		// build regression/gradient matrices
		if (m_iTSSubsampling > 0) {

			switch (m_pModel.m_iLearningMethod) {

			case 0:
				System.err.println("Building regression matrices (method=="
						+ m_pModel.m_iLearningMethod + ")...\n");
				EstRegressionMatrices(bVectors, m_iTSSubsampling);
				break;

			case 1:
				System.err.println("Building gradient matrices (method=="
						+ m_pModel.m_iLearningMethod + ")...\n");

				isOutsideImage = EstPredictionMatrices(bVectors,
						m_iTSSubsampling);
				if (isOutsideImage)
					return false;
				break;

			default:
				System.err.println("Wrong learning method (method=="
						+ m_pModel.m_iLearningMethod + "). Aborting!\n");
				System.exit(-1);
				break;
			}
		}

		// make doc
		if (m_bMakeDocumentation) {

			DumpModelDoc(bVectors);
			CAAMVisualizer AAMvis = new CAAMVisualizer(m_pModel);
			AAMvis.WriteEigenImages();
			AAMvis.WritePredictionImages();
		}

		// we're done: notify and write timings
		System.err.println("Done...");
		long endTimer = System.currentTimeMillis();
		// m_pModel.m_dBuildTime = timer.getTime();

		System.err.println("Time spent: "
				+ (double) ((endTimer - startTimer) / 1000d) + " secs");

		// optional pause to check resource consumption etc.
		/*
		 * boolean pause = true; if (pause) {
		 * System.err.println("Press return to continue..."); // getchar();
		 * pause(); }
		 */
		return true;

	}

	/**
	 * Diver method for model generation. This method automates the model
	 * generation as much as possible by using the various class methods for all
	 * the sequences in the task of producing a model.
	 * 
	 * @param model
	 *            The generated model.
	 * @param asfFiles
	 *            Vector of asf filenames.
	 * @param acf
	 *            Filename of an AAM configuration file. If omitted defaults are
	 *            used.
	 * @param modelReduction
	 *            Model reduction multiplier. Default off == 1. Useful when
	 *            building multi-scale AAMs.
	 * @param excludeShape
	 *            Excludes one shape number 'excludeShape' from the input
	 *            directory. Default -1, i.e. no shapes are removed. Used to
	 *            perform leave-one-out testing.
	 * @return Nothing.
	 */

	public void BuildFromFiles(CAAMModel model, final Vector<String> asfFiles,
			final String acf, final int modelReduction, final int excludeShape) {

		// set the internal model pointer that all
		// private methods work upon.
		m_pModel = model;

		// set up
		long startTimer = System.currentTimeMillis();

		// check for config file
		if (!acf.equals("")) {

			// read config file
			boolean ok = ReadACF(acf);

			if (!ok) {
				System.err
						.println("Could not open acf file '%s'. Using defaults."
								+ new String(acf));
			}
		}

		// check if a benchmark is requested
		if (m_iWarpMethod == 0) {

			// perform software/hardware benchmark
			ModelSimpleImage img = new ModelSimpleImage();
			CAAMShapeCollection shapes = new CAAMShapeCollection();

			shapes.ReadShapes(asfFiles);
			shapes.Rel2Abs(m_pModel.m_iModelReduction);

			if (shapes.size() < 2) {

				System.err
						.println("Error: Benchmarking requires a training set of min. two shapes.");
				System.exit(-1);
			}
			String imgfile = CAAMUtil.GetPath(asfFiles.get(0))
					+ shapes.get(1).HostImage();
			System.err.println("\nBenchmarking warping methods on [" + imgfile
					+ "]");
			img = img.ReadBandedFile(imgfile);
			if (m_pModel.m_iModelReduction != 1) {

				// img.ReducePyr(m_pModel.m_iModelReduction);
				img.subSample2dBy2();
			}
			CAAMShape refShape = new CAAMShape(shapes.get(0));
			refShape.Rel2Abs(img.Width(), img.Height());
			CAAMTest.AnalyzeTest(refShape, shapes.get(1), img,
					m_pModel.m_bUseConvexHull);

			System.err.println("\nBenchmarking done.");
			System.exit(0); // a bit brutal though....
		}

		// set the model reduction (if requested)
		if (modelReduction != 1) {
			m_pModel.m_iModelReduction = modelReduction;
		}

		// read images and shapes
		System.err.println("Reading images and annotations...");
		boolean addCompleteImage = false;
		boolean valid_shapes = LoadShapes(asfFiles, m_Shapes,
				m_pModel.m_iModelReduction, addCompleteImage,
				m_pModel.m_dAddExtents, excludeShape);
		if (valid_shapes == false) {

			System.err.println("The training set is not valid. Exiting.");
			System.exit(-1);
		}

		// align shapes
		DoShapeAlignment(m_pModel.m_bUseTangentSpace);

		//
		// use a different set of shapes for the shape PCA
		//
		// usage:
		//
		// _putenv(
		// "SHAPE_PCA_OVERRIDE=C:\\users\\mbs\\src2\\test\\greyscale\\all" );
		//
		boolean shapePCAoverride;
		String shapePCAoverideDir;

		shapePCAoverideDir = new String();
		shapePCAoverride = false;

		CAAMShapeCollection orgShapes = new CAAMShapeCollection();
		CAAMShapeCollection orgAlignedShapes = new CAAMShapeCollection();
		if (shapePCAoverride) {

			// backup shape collections
			orgShapes.assign(m_Shapes);
			orgAlignedShapes.assign(m_AlignedShapes);

			// find files
			Vector<String> shapeAsfFiles = new Vector<String>();
			shapeAsfFiles = (Vector<String>) CAAMUtil.ScanSortDir(
					CAAMUtil.AddBackSlash(shapePCAoverideDir), "asf");

			// load shapes
			valid_shapes = LoadShapes(shapeAsfFiles, m_Shapes,
					m_pModel.m_iModelReduction, addCompleteImage,
					m_pModel.m_dAddExtents, excludeShape);

			if (valid_shapes == false) {

				System.err
						.println("The shape PCA training set is not valid. Exiting.");
				System.exit(-1);
			}

			System.err.println("INFO: Shape PCA overidden using "
					+ m_Shapes.NShapes() + " shapes from:\n  "
					+ shapePCAoverideDir);

			// NOTICE
			// using this feature the texture pca will also be slightly
			// affected, since the referenceFrame is using the reference
			// shape from the new shape PCA
			//
			DoShapeAlignment(m_pModel.m_bUseTangentSpace);

			// set to number of shape to the number of traning textures
			m_pModel.m_iNShapes = orgShapes.NShapes();
		}

		// shape PCA
		System.err.println("Doing PCA on the shape data...");
		for (int s = 0; s < m_AlignedShapes.NShapes(); s++) {

			m_pModel.m_ShapePCA.InsertDataItem(m_AlignedShapes.get(s));
		}
		m_pModel.m_ShapePCA.DoPCA(m_bMakeDocumentation);

		// truncate eigenvectors and eigenvalues to
		// satisfy the variance explanation level
		// constraint (vlec) or by using parallel analysis
		if (m_pModel.m_iShapeTrunc == -1) {

			m_pModel.m_ShapePCA.TruncateParallel();
			m_pModel.m_ShapePCA.ClearDataItems();
		} else {
			m_pModel.m_ShapePCA.ClearDataItems();
			m_pModel.m_ShapePCA.TruncateVar(m_pModel.m_iShapeTrunc / 100.);
		}

		// use a different set of shapes for the shape PCA
		if (shapePCAoverride) {

			// restore shape collections
			m_Shapes.assign(orgShapes);
			m_AlignedShapes.assign(orgAlignedShapes);
		}

		// System.err.println(m_Shapes);

		// initialize the ReferenceFrame object
		CAAMShape rs = new CAAMShape();
		m_AlignedShapes.ReferenceShape(rs);
		m_pModel.m_pReferenceFrame = new CAAMReferenceFrame();
		m_pModel.m_pReferenceFrame.Setup(rs, m_pModel.m_bUseConvexHull);

		// initialize the AnalyzeSynthesize object
		switch (m_iWarpMethod) {

		case 0:
		case 1: // software
			m_pModel.m_pAnalyzeSynthesize = new CAAMAnalyzeSynthesizeSoftware(
					m_pModel.m_pReferenceFrame);
			break;

		case 2: // hardware accelerated OpenGL
			/*
			 * m_pModel.m_pAnalyzeSynthesize = new CAAMAnalyzeSynthesizeOpenGL(
			 * m_pModel.m_pReferenceFrame);
			 */
			break;

		default:
			System.err.println("Wrong warping method (method==" + m_iWarpMethod
					+ "). Aborting!\n");
			System.exit(-1);
			break;
		}

		// build texture vectors
		System.err.println("Building texture vectors...");
		BuildTextureVectors();

		// normalize textures
		System.err.println("Normalizing texture vectors...");
		NormalizeTextureVectors();

		// apply mappings to the texture vectors
		MapTextures();

		// calc texture variances
		// (we can't use the diagonal of the covariance matrix due to the Q-R
		// mode trick...)
		System.err.println("Calculating texture variances...");
		CAAMMathUtil.CalcElementVar(m_vTexture, m_pModel.m_vTextureVar);

		// dump var image
		if (m_bWriteVarImage) {

			String fn = new String("modelvar.xml");
			System.err.println("Writing variance image " + fn + "...");
			m_pModel.WriteVarianceMap(fn);
		}

		// dump registration movie
		if (m_bWriteRegMovie) {

			String movieName = new String("registration.xml");
			CAAMVisualizer AAMvis = new CAAMVisualizer(m_pModel);
			AAMvis.RegistrationMovie(movieName, m_vTexture);
		}

		// texture PCA
		// (actually rather excessive use of memory....
		// why have two copies of each texture in memory? ...hmmmm...)
		System.err.println("Doing PCA on the texture data...");
		for (int t = 0; t < m_vTexture.size(); t++) {

			m_pModel.m_TexturePCA.InsertDataItem(m_vTexture.get(t));
		}
		m_pModel.m_TexturePCA.DoPCA();

		// truncate eigenvectors and eigenvalues to
		// satisfy the variance explanation level
		// constraint (vlec) or by using parallel analysis
		if (m_pModel.m_iTextureTrunc == -1) {

			m_pModel.m_TexturePCA.TruncateParallel();
			m_pModel.m_TexturePCA.ClearDataItems();
		} else {
			m_pModel.m_TexturePCA.ClearDataItems();
			m_pModel.m_TexturePCA.TruncateVar(m_pModel.m_iTextureTrunc / 100.);
		}

		// do the combined PCA
		System.err.println("Doing combined PCA...");
		Vector<CDVector> bVectors = new Vector<CDVector>();
		bVectors = DoCombinedPCA();

		// build regression/gradient matrices
		if (m_iTSSubsampling > 0) {

			switch (m_pModel.m_iLearningMethod) {

			case 0:
				System.err.println("Building regression matrices (method=="
						+ m_pModel.m_iLearningMethod + ")...\n");
				EstRegressionMatrices(bVectors, m_iTSSubsampling);
				break;

			case 1:
				System.err.println("Building gradient matrices (method=="
						+ m_pModel.m_iLearningMethod + ")...\n");

				EstPredictionMatrices(bVectors, m_iTSSubsampling);
				break;

			default:
				System.err.println("Wrong learning method (method=="
						+ m_pModel.m_iLearningMethod + "). Aborting!\n");
				System.exit(-1);
				break;
			}
		}

		// make doc
		if (m_bMakeDocumentation) {

			DumpModelDoc(bVectors);
			CAAMVisualizer AAMvis = new CAAMVisualizer(m_pModel);
			AAMvis.WriteEigenImages();
			AAMvis.WritePredictionImages();
		}

		// we're done: notify and write timings
		System.err.println("Done...");
		long endTimer = System.currentTimeMillis();
		// m_pModel.m_dBuildTime = timer.getTime();

		System.err.println("Time spent: "
				+ (double) ((endTimer - startTimer) / 1000d) + " secs");

		// optional pause to check resource consumption etc.
		/*
		 * boolean pause = true; if (pause) {
		 * 
		 * System.err.println("Press return to continue..."); // getchar();
		 * pause(); }
		 */

	}

	/**
	 * Pauses the display until the user hits enter.
	 */
	public static void pause() {
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
	 * Loads (and preprocess) all training shapes. Loads (and preprocess) all
	 * training shapes into a CAAMShapeCollection. This could as well be placed
	 * in CAAMUtil actually.
	 * 
	 * @param asfFiles
	 *            An array of asf filenames.
	 * @param destination
	 *            Output shape collection.
	 * @param modelReduction
	 *            Optional size reduction. Default 1, i.e. no reduction.
	 * @param addCompleteImage
	 *            Set this to true if you (for wird reasons) would like add the
	 *            corners of the image to the shape. Default false.
	 * @param addExtents
	 *            Simple and somewhat hacked way to add a shape neighborhood
	 *            (will be removed in later versions).
	 * @param excludeShape
	 *            If != -1 the the 'excludeShape'-th shape will be excluded.
	 *            Zero is the first shape. Used for leave-one-out evaluation.
	 * @return Nothing.
	 */
	public boolean LoadShapes(final Vector<String> asfFiles,
			CAAMShapeCollection destination, int modelReduction,
			boolean addCompleteImage, double addExtents, int excludeShape) {

		destination.clear();

		boolean valid_shapes = destination.ReadShapes(asfFiles);

		if (valid_shapes == false) {

			return false;
		}

		if (excludeShape != -1) {

			System.err.println("Excluding shape #" + excludeShape + ".");
			destination.remove(excludeShape);
		}

		// add shape frame - i.e. the complete image now becomes the model
		if (addCompleteImage) {

			CAAMShape s = new CAAMShape(4);
			double hi = 0.99, lo = 0.01;
			s.SetPoint(0, lo, lo);
			s.SetPoint(1, hi, lo);
			s.SetPoint(2, hi, hi);
			s.SetPoint(3, lo, hi);

			CAAMPointInfo pi = new CAAMPointInfo();
			pi.SetClosed();
			pi.SetHole(false);
			pi.SetOuterEdge(true);
			for (int i = 0; i < destination.NShapes(); i++) {

				destination.get(i).AddPath(s, pi);
				System.err.println("BuildFromFiles: Adding shape frame...");
			}
		}

		// force absolute coordinates
		destination.Rel2Abs(modelReduction);

		// add shape extents (if requested)
		if (addExtents != .0) {

			for (int i = 0; i < destination.NShapes(); i++) {

				destination.get(i).AddShapeExtends((int) (.5 + addExtents));
			}
		}

		return true;
	}

	/**
	 * Load all training shapes from prostate image vector VOIs.
	 * 
	 * @param destination
	 *            Output shape collection.
	 * @param modelImageVector
	 *            prostate image vector with VOIs
	 * @param modelReduction
	 *            Optional size reduction. Default 1, i.e. no reduction.
	 * @param addCompleteImage
	 *            Set this to true if you (for wird reasons) would like add the
	 *            corners of the image to the shape. Default false.
	 * @param addExtents
	 *            Simple and somewhat hacked way to add a shape neighborhood
	 *            (will be removed in later versions).
	 * @param excludeShape
	 *            If != -1 the the 'excludeShape'-th shape will be excluded.
	 *            Zero is the first shape. Used for leave-one-out evaluation.
	 * @return nothing
	 */
	public boolean LoadShapes(CAAMShapeCollection destination,
			Vector<ModelImage> modelImageVector, int modelReduction,
			boolean addCompleteImage, double addExtents, int excludeShape) {

		destination.clear();

		boolean valid_shapes = destination.ReadImages(modelImageVector);

		if (valid_shapes == false) {

			return false;
		}

		if (excludeShape != -1) {

			System.err.println("Excluding shape #" + excludeShape + ".");
			destination.remove(excludeShape);
		}

		// add shape frame - i.e. the complete image now becomes the model
		if (addCompleteImage) {

			CAAMShape s = new CAAMShape(4);
			double hi = 0.99, lo = 0.01;
			s.SetPoint(0, lo, lo);
			s.SetPoint(1, hi, lo);
			s.SetPoint(2, hi, hi);
			s.SetPoint(3, lo, hi);

			CAAMPointInfo pi = new CAAMPointInfo();
			pi.SetClosed();
			pi.SetHole(false);
			pi.SetOuterEdge(true);
			for (int i = 0; i < destination.NShapes(); i++) {

				destination.get(i).AddPath(s, pi);
				System.err.println("BuildFromFiles: Adding shape frame...");
			}
		}

		// force absolute coordinates
		destination.Rel2Abs(modelReduction);

		// add shape extents (if requested)
		if (addExtents != .0) {

			for (int i = 0; i < destination.NShapes(); i++) {

				destination.get(i).AddShapeExtends((int) (.5 + addExtents));
			}
		}

		return true;
	}

	/**
	 * Performs a mapping of all textures in 'm_vTexture'. Performs a mapping of
	 * all textures in 'm_vTexture' using the texture transfer class of the
	 * model.
	 */
	public void MapTextures() {

		int n_tex = m_vTexture.size();
		for (int i = 0; i < n_tex; i++) {

			m_pModel.m_pTextureTF.Map(m_vTexture.get(i));
		}

		// update the number of texture samples
		m_pModel.m_iTextureSamples = m_vTexture.get(0).Length();

		// recalc the mean texture
		RecalcMeanTexture();
	}

	/**
	 * Write additional documentation output.
	 * 
	 * @param bVectors
	 *            Concatenated and weighted shape and texture vectors over the
	 *            training set.
	 */
	public void DumpModelDoc(Vector<CDVector> bVectors) {

		// write additional documentation output
		System.err
				.println("Dumping additional documentation output to current dir...");

		// write the unaligned shapes
		m_Shapes.ToMatlab("shapes.m", "unaligned_shapes", "Unaligned shapes.",
				false);

		// write the aligned shapes
		m_AlignedShapes.ToMatlab("ashapes.m", "aligned_shapes",
				"Aligned shapes.", false);

		// write the mean shape
		m_pModel.m_sMeanAShape.ToMatlab("meanshape.m", "mean_shape",
				"The mean shape.", false);
		m_pModel.m_sMeanAShape.WriteASF("meanshape.asf", 1, 1);

		// write the Delaunay triangulation of the meanshape
		m_pModel.ReferenceFrame().RefMesh().ToMatlab("delaunay.m");

		// write pca values for the training set in 'texture_pc.m', 'shape_pc.m'
		// and 'combined_pc.m'
		DumpPCA(bVectors);

		// dump eigenvalues
		m_pModel.m_CombinedPCA.EigenValues().ToMatlab("combined_ev.m", "c_ev",
				"Combined model eigenvalues.", false);
		m_pModel.m_TexturePCA.EigenValues().ToMatlab("texture_ev.m", "t_ev",
				"Texture model eigenvalues.", false);
		m_pModel.m_ShapePCA.EigenValues().ToMatlab("shape_ev.m", "s_ev",
				"Shape model eigenvalues.", false);

		// dump shape eigenvectors
		m_pModel.m_ShapePCA
				.EigenVectors()
				.ToMatlab(
						"shape_evec.m",
						"s_evec",
						"Shape eigenvectors (nb points x2 rows / nb eigenvalues cols).",
						false);

		// dump inverse alignment transformations, i.e. the transformation
		// needed
		// to align the aligned shapes with the unaligned shapes
		int ns = m_Shapes.NShapes();
		CDMatrix iat = new CDMatrix(ns, 4);
		for (int i = 0; i < ns; i++) {

			double[] scale = new double[1];
			double[] theta = new double[1];
			CAAMPoint translation = new CAAMPoint();
			m_AlignedShapes.get(i).AlignTransformation(m_Shapes.get(i), scale,
					theta, translation);
			iat.m_data[i][0] = scale[0];
			iat.m_data[i][1] = theta[0];
			iat.m_data[i][2] = translation.x;
			iat.m_data[i][3] = translation.y;
		}
		iat.ToMatlab(
				"iat.m",
				"T",
				"the transformations needed to align the aligned shapes with the unaligned shapes",
				false);

		// a more readable version of the above (with stats)
		try {
			PrintWriter fh = new PrintWriter("ts_pose.txt");
			double[] scale = new double[1];
			double[] theta = new double[1];
			double[] xc = new double[1];
			double[] yc = new double[1];
			CAAMPoint translation = new CAAMPoint();
			if (fh != null) {

				final int n = m_Shapes.NShapes();
				CDVector size = new CDVector(n);
				CDVector rotation = new CDVector(n);
				CDVector xcog = new CDVector(n);
				CDVector ycog = new CDVector(n);
				for (int i = 0; i < n; i++) {

					m_AlignedShapes.get(i).AlignTransformation(m_Shapes.get(i),
							scale, theta, translation);
					m_Shapes.get(i).COG(xc, yc);
					size.m_data[i] = scale[0];
					rotation.m_data[i] = CAAMUtil.Rad2Deg(theta[0]);
					xcog.m_data[i] = xc[0];
					ycog.m_data[i] = yc[0];
				}
				size.div_into(size.Mean());
				fh.println("size\trot\tcog_x\tcog_y\n");
				for (int i = 0; i < n; i++) {
					fh.println(size.m_data[i] + "\t" + rotation.m_data[i]
							+ "\t" + xcog.m_data[i] + "\t" + ycog.m_data[i]);
				}
				fh.println("\n[mean/std.dev./min/max]");
				fh.println("size   " + size.Mean() + " \t" + "(" + size.Std()
						+ ")" + " \t" + size.Min() + " \t" + size.Max());
				fh.printf("rot    " + rotation.Mean() + " \t(" + rotation.Std()
						+ ") \t" + rotation.Min() + " \t" + rotation.Max());
				fh.printf("cog_x  " + xcog.Mean() + " \t(" + xcog.Std()
						+ ") \t" + xcog.Min() + " \t" + xcog.Max());
				fh.printf("cog_y  " + ycog.Mean() + " \t(" + ycog.Std()
						+ ") \t" + ycog.Min() + " \t" + ycog.Max());

			}
			fh.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		// expensive (i.e. time consuming) dumps
		boolean do_expensive_dumps = false;
		if (do_expensive_dumps) {

			// dump texture vectors
			int ntex = m_vTexture.size();
			int nsamp = m_pModel.NTextureSamples();
			CDMatrix texvecs = new CDMatrix(nsamp, ntex);
			for (int i = 0; i < ntex; i++) {
				texvecs.SetColumn(i, m_vTexture.get(i));
			}
			texvecs.ToMatlab("texvecs.m", "tex", "textures, one per column",
					false);
			CDMatrix mt = new CDMatrix(nsamp, 1);
			mt.SetColumn(0, m_pModel.MeanTexture());
			mt.ToMatlab("meantex.m", "mt", "mean texture", false);

			// dump texture eigenvectors
			m_pModel.m_TexturePCA
					.EigenVectors()
					.ToMatlab(
							"texture_evec.m",
							"t_evec",
							"Texture eigenvectors (nb pixels rows / nb eigenvalues cols).",
							false);
		}

	}

	/**
	 * Build the regression matrices for pose and parameter prediction. I.e.
	 * calculates the member variables 'regR_c', 'regR_t'. using principal
	 * component regression.
	 * 
	 * @param bVectors
	 *            The b-vectors for the training set the current AAM is built
	 *            upon. [Can be optained from the DoCombinedPCA() call]
	 * @param ts_subsampling
	 *            Controls the sub sampling of the training set - i.e. to use
	 *            every fifth shape to build the regression matrices upon, set
	 *            shape_subsampling = 5;
	 * 
	 *            The motivation for doing this is reduction of model building
	 *            time -- and perhaps most importantly -- conservation of memory
	 *            resources.
	 */
	public void EstRegressionMatrices(final Vector<CDVector> bVectors,
			final int ts_subsampling) {

		// output info
		int nShapes = m_Shapes.NShapes();
		int nbShapeSamples = nShapes % ts_subsampling != 0 ? nShapes
				/ ts_subsampling + 1 : nShapes / ts_subsampling;
		System.err.println("Info: Training set subsampling = " + ts_subsampling
				+ " (" + nbShapeSamples + " shapes used)");

		// convert b to c parameters
		Vector<CDVector> cVectors = new Vector<CDVector>();
		m_pModel.ShapeTexParam2Combined(bVectors, cVectors);

		// generate displacement vectors
		Vector<CDVector> vCDisps = new Vector<CDVector>();
		Vector<CDVector> vPoseDisps = new Vector<CDVector>();
		DisplacementSets(vCDisps, vPoseDisps, cVectors);

		// do model parameter experiments
		{
			CDMatrix X = new CDMatrix();
			CDMatrix C = new CDMatrix();
			DoCParamExperiments(vCDisps, cVectors, X, C, ts_subsampling);
			System.err.println("X = " + X.ToString());
			System.err.println("C = " + C.ToString());
			// estimate Rc using principal component regression
			CAAMLinearReg reg = new CAAMLinearReg();
			reg.DoRegression(C, X, m_pModel.m_R_c);

		}

		// do pose experiments
		{
			CDMatrix X = new CDMatrix();
			CDMatrix C = new CDMatrix();
			DoPoseExperiments(vPoseDisps, cVectors, X, C, ts_subsampling);
			
			// estimate Rt using principal component regression
			CAAMLinearReg reg = new CAAMLinearReg();
			reg.DoRegression(C, X, m_pModel.m_R_t);

		}

		// store negative versions to avoid any sign change at each iteration
		m_pModel.m_R_c.mult_into(-1);
		m_pModel.m_R_t.mult_into(-1);
	}

	/**
	 * Performs a set of pose parameter displacement experiments on the training
	 * set given a set of displacment vectors.
	 * 
	 * @param vPoseDisps
	 *            A vector of displacement vectors as obtained from
	 *            PoseDispVectors() or DisplacementSets().
	 * @param cVectors
	 *            The set of optimum c vectors for the training examples.
	 * @param X
	 *            Output matrix containing the texture difference vectors
	 *            obtained from the displacements.
	 * @param Y
	 *            Output matrix containing the model parameter displacements
	 *            carried out.
	 * @param ts_subsampling
	 *            Subsampling factor, i.e. ts_subsampling==n will carry out
	 *            displacements on every n-th example in the training set.
	 * @return Nothing.
	 */
	public boolean DoPoseExperiments(final Vector<CDVector> vPoseDisps,
			final Vector<CDVector> cVectors, CDMatrix X, CDMatrix C,
			final int ts_subsampling) {

		boolean isOutsideImage;
		// setup
		int nShapes = m_Shapes.NShapes();

		// determine subsampling of the training set
		int nbShapeSamples = nShapes % ts_subsampling != 0 ? nShapes
				/ ts_subsampling + 1 : nShapes / ts_subsampling;
		int totalExp = vPoseDisps.size() * nbShapeSamples;
		int nExperiment = 0;
		X.Resize(m_pModel.m_iTextureSamples, totalExp);
		C.Resize(4, totalExp);

		// for each training example in the (subsampled) training set
		for (int nShape = 0; nShape < nShapes; nShape += ts_subsampling) {

			// get the shape image
			ModelSimpleImage image = new ModelSimpleImage();
			// debug
			// image = m_Shapes.get(nShape).GetHostImage(image, m_Shapes.Path(),
			// m_pModel.m_iModelReduction);
			image = m_Shapes.get(nShape).GetHostImage(
					m_Shapes.get(nShape).getHostImage(),
					m_pModel.m_iModelReduction);
			for (int i = 0; i < vPoseDisps.size(); i++) {
				CDVector delta_g = new CDVector();

				// ***************************** isOutsideImage
				// *********************
				// do displacement measures
				isOutsideImage = PoseDisplacement(image, m_Shapes.get(nShape),
						cVectors.get(nShape), vPoseDisps.get(i), delta_g);

				if (isOutsideImage) {
					return true;
				}
				// insert the results into X and C
				X.SetColumn(nExperiment, delta_g);
				C.SetColumn(nExperiment, vPoseDisps.get(i));
				++nExperiment;
			}
			System.err.println("Experiment " + nExperiment + " of " + totalExp
					+ " done (pose)...");

		}
		return false;
	}

	/**
	 * Performs a set of model parameter displacement experiments on the
	 * training set given a set of displacment vectors.
	 * 
	 * @param vCDisps
	 *            A vector of displacement vectors as obtained from
	 *            CParamDispVectors() or DisplacementSets().
	 * @param cVectors
	 *            The set of optimum c vectors for the training examples.
	 * @param X
	 *            Output matrix containing the texture difference vectors
	 *            obtained from the displacements.
	 * @param Y
	 *            Output matrix containing the model parameter displacements
	 *            carried out.
	 * @param ts_subsampling
	 *            Subsampling factor, i.e. ts_subsampling==n will carry out
	 *            displacements on every n-th example in the training set.
	 * @return Nothing.
	 */
	public boolean DoCParamExperiments(final Vector<CDVector> vCDisps,
			final Vector<CDVector> cVectors, CDMatrix X, CDMatrix C,
			final int ts_subsampling) {

		boolean isOutsideImage;
		// setup
		int nShapes = m_Shapes.NShapes();
		int np = m_pModel.m_CombinedPCA.NParameters();

		// determine subsampling of the training set
		int nbShapeSamples = nShapes % ts_subsampling != 0 ? nShapes
				/ ts_subsampling + 1 : nShapes / ts_subsampling;
		int totalExp = vCDisps.size() * nbShapeSamples;
		int nExperiment = 0;
		X.Resize(m_pModel.m_iTextureSamples, totalExp);
		C.Resize(np, totalExp);
		// CDVector delta_g = new CDVector();

		// for each training example in the (subsampled) training set
		for (int nShape = 0; nShape < nShapes; nShape += ts_subsampling) {

			// get the shape image
			ModelSimpleImage image = new ModelSimpleImage();
			// debug
			// image = m_Shapes.get(nShape).GetHostImage(image, m_Shapes.Path(),
			// m_pModel.m_iModelReduction);
			image = m_Shapes.get(nShape).GetHostImage(
					m_Shapes.get(nShape).getHostImage(),
					m_pModel.m_iModelReduction);
			for (int i = 0; i < vCDisps.size(); i++) {
				CDVector delta_g = new CDVector();

				// *****************************isOutsideImage
				// *********************
				// do displacement measures
				isOutsideImage = ModelDisplacement(image, m_Shapes.get(nShape),
						cVectors.get(nShape), vCDisps.get(i), delta_g);
				if (isOutsideImage)
					return true;

				// insert the results into X and C
				X.SetColumn(nExperiment, delta_g);
				C.SetColumn(nExperiment, vCDisps.get(i));
				++nExperiment;
			}
			System.err.println("Experiment " + nExperiment + " of " + totalExp
					+ " done (c)...");

		}
		return false;
	}

	/**
	 * @doc Generates model parameter and pose displacement sets.
	 * @param vCDisps
	 *            Resulting model parameter displacement set.
	 * @param vPoseDisps
	 *            Resulting pose parameter displacement set.
	 * @param cVectors
	 *            The set of c vectors over the training set.
	 */
	public void DisplacementSets(Vector<CDVector> vCDisps,
			Vector<CDVector> vPoseDisps, final Vector<CDVector> cVectors) {

		// //////////////////////////////////////////////////////////////////
		//
		// NOTICE: sets must *always* be in anti-symmetric pairs with the
		// largest displacements first, e.g. [ -.5, .5, -.25, .25 ]
		//
		// //////////////////////////////////////////////////////////////////

		// generate c displacement sets
		CDVector vStdDisp = new CDVector(4);

		// MBS master's thesis displacement set
		vStdDisp.m_data[0] = -.5;
		vStdDisp.m_data[1] = .5;
		vStdDisp.m_data[2] = -.25;
		vStdDisp.m_data[3] = .25;

		// vCDisps = CParamDispVectors(vStdDisp, cVectors);
		Vector<CDVector> temp1 = CParamDispVectors(vStdDisp, cVectors);
		for (int i = 0; i < temp1.size(); i++) {
			vCDisps.add(i, temp1.get(i));
		}

		// generate pose displacement sets
		CDVector vXYDisp = new CDVector(6);
		CDVector vScaleDisp = new CDVector(6);
		CDVector vRotDisp = new CDVector(6);

		// relative displacement set
		vXYDisp.Resize(8);
		vXYDisp.m_data[0] = -.05;
		vXYDisp.m_data[1] = .05;
		vXYDisp.m_data[2] = -.10;
		vXYDisp.m_data[3] = .10;
		vXYDisp.m_data[4] = -.15;
		vXYDisp.m_data[5] = .15;
		vXYDisp.m_data[6] = -.20;
		vXYDisp.m_data[7] = .20;

		/*
		 * debug vScaleDisp.Resize(8); vScaleDisp.m_data[0] = .85;
		 * vScaleDisp.m_data[1] = 1.15; vScaleDisp.m_data[2] = .95;
		 * vScaleDisp.m_data[3] = 1.05; vScaleDisp.m_data[4] = .90;
		 * vScaleDisp.m_data[5] = 1.10; vScaleDisp.m_data[6] = .80;
		 * vScaleDisp.m_data[7] = 1.20;
		 */

		vScaleDisp.Resize(8);
		vScaleDisp.m_data[0] = .95;
		vScaleDisp.m_data[1] = 1.05;
		vScaleDisp.m_data[2] = 0.9;
		vScaleDisp.m_data[3] = 1.1;
		vScaleDisp.m_data[4] = 0.8;
		vScaleDisp.m_data[5] = 1.2;
		vScaleDisp.m_data[6] = 0.7;
		vScaleDisp.m_data[7] = 1.3;

		vRotDisp.Resize(4);
		// vRotDisp.m_data[0] = -15.; // degrees
		// vRotDisp.m_data[1] = 15.;
		// vRotDisp.m_data[2] = -5.;
		// vRotDisp.m_data[3] = 5.;

		vRotDisp.m_data[0] = -5.; // degrees
		vRotDisp.m_data[1] = 5.;
		vRotDisp.m_data[2] = -10.;
		vRotDisp.m_data[3] = 10.;
		// vRotDisp.m_data[4] = -6.;
		// vRotDisp.m_data[5] = 6.;

		CDVector vXDisp = new CDVector(vXYDisp.Length());
		CDVector vYDisp = new CDVector(vXYDisp.Length());
		vXDisp.assign(vXYDisp.mult(m_pModel.ReferenceShape().Width()));
		vYDisp.assign(vXYDisp.mult(m_pModel.ReferenceShape().Height()));

		// vPoseDisps = PoseDispVectors(vXDisp, vYDisp, vScaleDisp, vRotDisp);
		Vector<CDVector> temp2 = PoseDispVectors(vXDisp, vYDisp, vScaleDisp,
				vRotDisp);
		for (int i = 0; i < temp2.size(); i++) {
			vPoseDisps.add(i, temp2.get(i));
		}

	}

	/**
	 * Wrapper to generate damp displacement vectors
	 * 
	 * @param vStdDisp
	 *            displacement vector
	 * @param cVectors
	 *            c parameters
	 * @return displacement vector
	 */
	public Vector<CDVector> CParamDispVectors(final CDVector vStdDisp,
			final Vector<CDVector> cVectors) {
		return CParamDispVectors(vStdDisp, cVectors, 0, 0);
	}

	/**
	 * Generates a set combined model parameter displacement vectors where each
	 * parameter is displaced at a time according to the values in vStdDisp.
	 * 
	 * @param vStdDisp
	 *            A vector of parameter displacements in standard deviations of
	 *            the corresponding parameter.
	 * @param cVectors
	 *            The set of c vectors over the training set.
	 * @param pStart
	 *            The first parameter to displace. (default 0).
	 * @param pLen
	 *            The number of parameters to displace. (default 0, which means
	 *            all parameters).
	 * @return A vector of displacement vectors.
	 */
	public Vector<CDVector> CParamDispVectors(final CDVector vStdDisp,
			final Vector<CDVector> cVectors, final int pStart, final int pLen) {

		Vector<CDVector> cDisplacements = new Vector<CDVector>();
		int np = m_pModel.m_CombinedPCA.NParameters();

		// sanity checks
		assert (pStart >= 0);
		assert (pLen == 0 || (pStart + pLen) <= np);

		// calc var
		CDVector paramVar = new CDVector();
		CAAMMathUtil.CalcElementVar(cVectors, paramVar);

		// make displacement vectors
		int len = pLen == 0 ? np : pLen;
		for (int i = pStart; i < pStart + len; i++) {

			// for each parameter
			double std = Math.sqrt(paramVar.m_data[i]); // get standard
														// deviation for this
														// parameter
			for (int j = 0; j < vStdDisp.Length(); j++) {

				// for each displacement
				CDVector dC = new CDVector(np);
				dC.assign(.0);
				dC.m_data[i] = vStdDisp.m_data[j] * std;
				cDisplacements.add(dC);
			}
		}

		return cDisplacements;
	}

	/**
	 * Generates a set pose displacement vectors.
	 * 
	 * @param vXDisp
	 *            A vector of x displacements in pixels.
	 * @param vYDisp
	 *            A vector of y displacements in pixels.
	 * @param vScaleDisp
	 *            A vector of scale displacements (1.0=no scaling).
	 * @param vRotDisp
	 *            A vector of rotation displacements in degrees.
	 * @return A vector of displacement vectors.
	 */
	public Vector<CDVector> PoseDispVectors(final CDVector vXDisp,
			final CDVector vYDisp, final CDVector vScaleDisp,
			final CDVector vRotDisp) {

		Vector<CDVector> poseDisplacements = new Vector<CDVector>();

		// add x displacements
		for (int i = 0; i < vXDisp.Length(); i++) {

			CDVector dT = new CDVector(4);
			dT.assign(.0);
			CAAMShape.Param2PoseVec(1., .0, vXDisp.m_data[i], .0, dT);
			poseDisplacements.add(dT);
		}

		int i;
		// add y displacements
		for (i = 0; i < vYDisp.Length(); i++) {

			CDVector dT = new CDVector(4);
			dT.assign(.0);
			CAAMShape.Param2PoseVec(1., .0, .0, vYDisp.m_data[i], dT);
			poseDisplacements.add(dT);
		}

		// add scale displacements
		for (i = 0; i < vScaleDisp.Length(); i++) {

			CDVector dT = new CDVector(8);
			dT.assign(.0);
			CAAMShape.Param2PoseVec(vScaleDisp.m_data[i], .0, .0, .0, dT);
			poseDisplacements.add(dT);
		}

		// add rotation displacements
		for (i = 0; i < vRotDisp.Length(); i++) {

			CDVector dT = new CDVector(6);
			dT.assign(.0);
			CAAMShape.Param2PoseVec(1., vRotDisp.m_data[i] * Math.PI / 180.,
					.0, .0, dT);
			poseDisplacements.add(dT);
		}

		return poseDisplacements;
	}

	/**
	 * Samples all shapes in the training set and build the corresponding
	 * texture vectores. Initializes the private member: 'm_vTexture' by
	 * sampling all shapes using a warp function. Note that this method
	 * calculates the mean texture.
	 */
	public void BuildTextureVectors() {

		int nSamples, nbShapes = m_Shapes.NShapes();

		for (int i = 0; i < nbShapes; i++) {
			CDVector vTexture = new CDVector();
			// get the shape image
			ModelSimpleImage image = new ModelSimpleImage();
			image = m_Shapes.get(i).GetHostImage(image, m_Shapes.Path(),
					m_pModel.m_iModelReduction);

			// sample the texture of the i-th shape into 'vTexture'
			nSamples = m_pModel.SampleShape(image, m_Shapes.get(i), vTexture,
					false);

			if (i == 0) {

				m_pModel.m_iTextureSamples = nSamples;
			}
			m_vTexture.add(vTexture);
		}
	}

	/**
	 * Transform image to texture vector
	 */
	public void BuildTextureVectorsFromImage() {

		int nSamples, nbShapes = m_Shapes.NShapes();

		for (int i = 0; i < nbShapes; i++) {
			CDVector vTexture = new CDVector();
			// get the shape image
			ModelSimpleImage image = new ModelSimpleImage();
			image = m_Shapes.get(i).GetHostImage(
					m_Shapes.get(i).getHostImage(), m_pModel.m_iModelReduction);

			// sample the texture of the i-th shape into 'vTexture'
			nSamples = m_pModel.SampleShape(image, m_Shapes.get(i), vTexture,
					false);

			if (i == 0) {

				m_pModel.m_iTextureSamples = nSamples;
			}
			m_vTexture.add(vTexture);
		}
	}

	/**
	 * Constructor. Sets up default values for the settings usally given by an
	 * acf file.
	 */
	public CAAMBuilder() {
		m_bVerbose = false;
		m_bWriteRegMovie = false;
		m_bWriteVarImage = false;
		m_bMakeDocumentation = false;
		m_iTSSubsampling = 1;
		m_iWarpMethod = 1;
		m_pModel = null;
	}

	/**
	 * Dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Calculates the pixel-to-shape weights used in the combined PCA. Currently
	 * the simple 'split even' strategy is employed, i.e. normalise shape and
	 * texture variance to be equal.
	 */
	public void CalcPixel2ShapeWeights() {

		int nbShapeParam = m_pModel.m_ShapePCA.NParameters();

		//
		// well, since we are using this very simple weightning
		// scheme it could be hard-coded in the CAAMModel,
		// providing clarity and smaller model files, etc.
		//
		// however, for now we still retain the full matrix
		// formulation, not even exploiting that it is a
		// diagonal matrix
		//
		// since the usage of this matrix is cached inside
		// CAAMModel it does not slow it down....
		//
		m_pModel.m_mShape2PixelWeights.Resize(nbShapeParam, nbShapeParam);
		m_pModel.m_mShape2PixelWeights.Eye();

		// split even
		double val = m_pModel.m_TexturePCA.EigenValues().Sum()
				/ m_pModel.m_ShapePCA.EigenValues().Sum();

		m_pModel.m_mShape2PixelWeights.mult_into(Math.sqrt(val));
	}

	/**
	 * Performs one pose regression experiment.
	 * 
	 * @param image
	 *            The image corresponding to the equibrilium shape.
	 * @param shape
	 *            The equibrilium shape.
	 * @param c0
	 *            The equibrilium model parameters.
	 * @param t
	 *            The pose displacement parameters.
	 * @param pixel_diff
	 *            The normalized pixel differences resulting from the pose
	 *            displacement.
	 * @return Nothing.
	 */
	public boolean PoseDisplacement(final ModelSimpleImage image,
			final CAAMShape shape, final CDVector c0, final CDVector t,
			CDVector pixel_diff) {

		boolean isOutside = false;
		CDVector g_m = new CDVector();
		CDVector g_s = new CDVector();
		CAAMShape X_shape = new CAAMShape();

		// generate model texture
		m_pModel.TextureInstance(c0, g_m);

		// generate model shape
		m_pModel.ShapeInstance(c0, X_shape);

		// calc alignment from X_shape to the annotated shape
		CAAMPoint translation = new CAAMPoint();
		double[] scale = new double[1];
		double[] theta = new double[1];
		X_shape.AlignTransformation(shape, scale, theta, translation);

		// align displaced shape onto the image domain
		X_shape.Scale(scale[0]);

		// calc ratio for this shape
		double sizeRatio = X_shape.ShapeSize()
				/ m_pModel.ReferenceShape().ShapeSize();

		// impose the right pose displacement on X by using t
		CDVector tSizeNormalized = new CDVector(t);
		tSizeNormalized.m_data[2] *= sizeRatio;
		tSizeNormalized.m_data[3] *= sizeRatio;
		X_shape.Displace(tSizeNormalized);

		// align the displaced shape onto the image domain
		X_shape.Rotate(theta[0]);
		X_shape.Translate(translation);

		// sample the shape
		int nSamples = m_pModel.SampleShape(image, X_shape, g_s);
		if (nSamples == 0) {

			// LATER
			System.err
					.println("Pose: Shape was outside image - texture set to zero.");
			g_s.Resize(g_m.Length());
			g_s.assign(0);
			isOutside = true;
			return isOutside;
		}

		// calc pixel difference
		pixel_diff.Resize(g_m.Length());
		pixel_diff.assign(g_s);
		pixel_diff.sub_into(g_m);
		isOutside = false;
		return isOutside;
	}

	/**
	 * Performs one model parameter regression experiment.
	 * 
	 * @param image
	 *            The image corresponding to the equibrilium shape.
	 * @param shape
	 *            The equibrilium shape.
	 * @param c0
	 *            The equibrilium model parameters.
	 * @param delta_c
	 *            The model parameter displacements.
	 * @param pixel_diff
	 *            The normalized pixel differences resulting from the pose
	 *            displacement.
	 * @return Nothing.
	 */
	public boolean ModelDisplacement(final ModelSimpleImage image,
			final CAAMShape shape, final CDVector c0, final CDVector delta_c,
			CDVector pixel_diff) {

		boolean isOutside = false;
		CDVector c = new CDVector(c0.Length());
		CDVector g_m = new CDVector();
		CDVector g_s = new CDVector();
		CAAMShape X_shape = new CAAMShape();

		c.assign(delta_c.add(c0));

		// generate model texture
		m_pModel.TextureInstance(c, g_m);

		// generate model shape
		m_pModel.ShapeInstance(c, X_shape);

		// Align X_shape to the annotated shape
		X_shape.AlignTo(shape);

		// sample the shape
		int nSamples = m_pModel.SampleShape(image, X_shape, g_s);

		if (nSamples == 0) {
			System.err
					.println("Model: Shape was outside image - texture set to zero.");
			g_s.Resize(g_m.Length());
			g_s.assign(0);
			isOutside = true;
			return isOutside;
		}

		// calc pixel difference
		pixel_diff.Resize(g_m.Length());
		pixel_diff.assign(g_s);
		pixel_diff.sub_into(g_m);
		isOutside = false;
		return isOutside;
	}

	/**
	 * Alignes shapes and calc mean- and reference-shape. I.e. initializes
	 * 'm_AlignedShapes', 'm_sMeanAShape' and 'm_sReferenceShape'.
	 * 
	 * @param fUseTangentSpace
	 *            Use the tangent space projection (bool).
	 */
	public void DoShapeAlignment(final boolean fUseTangentSpace) {

		// copy the unaligned shapes
		m_AlignedShapes.assign(m_Shapes);

		// align shape with respect to position, scale and rotation
		m_AlignedShapes.AlignShapes(fUseTangentSpace);

		// calculate the cached mean shape of the aligned shapes
		m_AlignedShapes.MeanShape(m_pModel.m_sMeanAShape);

		// set the mean shape size and the number of shapes
		m_pModel.m_dMeanShapeSize = m_AlignedShapes.MeanSize();
		m_pModel.m_iNShapes = m_Shapes.NShapes();
	}

	/**
	 * Dumps the PC scores of the shape, texture and combined PCA. These are
	 * written to the current directory in Matlab format as shape_pc.m,
	 * texture_pc.m and combined_pc, respectively.
	 * 
	 * param bVectors The b-parameters for all training examples. As obtained
	 * from DoCombinedPCA().
	 */
	public void DumpPCA(final Vector<CDVector> bVectors) {

		int nbTrainingExamples = m_vTexture.size();
		int nbTexParam = m_pModel.m_TexturePCA.NParameters();
		int nbShapeParam = m_pModel.m_ShapePCA.NParameters();
		int nbCombinedParam = m_pModel.m_CombinedPCA.NParameters();

		CDMatrix shapePC = new CDMatrix(nbShapeParam, nbTrainingExamples);
		CDMatrix texturePC = new CDMatrix(nbTexParam, nbTrainingExamples);
		CDMatrix combinedPC = new CDMatrix(nbCombinedParam, nbTrainingExamples);

		// ////////////////////////////////////////////////////////
		// generate c-vectors for all elements in the trainingset
		// ////////////////////////////////////////////////////////
		Vector<CDVector> cVectors = new Vector<CDVector>();
		m_pModel.ShapeTexParam2Combined(bVectors, cVectors);

		shapePC.assign(.0);
		texturePC.assign(.0);
		// generate b-vectors for all elements in the trainingset
		for (int i = 0; i < nbTrainingExamples; i++) {

			// the format of bVectors[i] is [ <shape pc> <tex pc>]

			// set the shape row
			for (int r = 0; r < nbShapeParam; r++) {

				shapePC.m_data[r][i] = bVectors.get(i).m_data[r];
			}

			// set the texture row
			for (int r = 0; r < nbTexParam; r++) {

				texturePC.m_data[r][i] = bVectors.get(i).m_data[r
						+ nbShapeParam];
			}

			// set the combined row
			for (int r = 0; r < nbCombinedParam; r++) {

				combinedPC.m_data[r][i] = cVectors.get(i).m_data[r];
			}
		}

		// remove the shape2pixel weights from the b_s
		CDMatrix w = m_pModel.Shape2PixelWeights();
		shapePC.assign(w.Inverted().mult(shapePC));

		shapePC.ToMatlab(
				"shape_pc.m",
				"s_pc",
				"PC-projection of training shapes. Each example occupy one column. Largest pc (pc1) in last row.",
				false);
		texturePC
				.ToMatlab(
						"texture_pc.m",
						"t_pc",
						"PC-projection of training textures. Each example occupy one column. Largest pc (pc1) in last row.",
						false);
		combinedPC
				.ToMatlab(
						"combined_pc.m",
						"c_pc",
						"PC-projection of training shapes and textures. Each example occupy one column. Largest pc (pc1) in last row.",
						false);
	}

	/**
	 * Iterative normalization of the texture samples. Performs normalization of
	 * the texture vectors as described by Cootes et al. in
	 * "Active Appearance Models" sec. 2. Recalculates the mean texture.
	 */
	public void NormalizeTextureVectors() {

		// get sizes
		double diff = 1e306;
		int nbTextures = m_vTexture.size();
		assert (m_vTexture.size() > 0);
		CDVector lastMeanEstimate = new CDVector();

		// iterate until all texture vectors
		// are normalized using a stable estimate
		// of the mean texture
		m_pModel.m_vMeanTexture = new CDVector(m_vTexture.get(0).Length());
		m_pModel.m_vMeanTexture.assign(m_vTexture.get(0));
		CAAMMathUtil.ZeroMeanUnitLength(m_pModel.m_vMeanTexture);
		m_pModel.m_vMeanTextureOrg = new CDVector(
				m_pModel.m_vMeanTexture.Length());
		m_pModel.m_vMeanTextureOrg.assign(m_pModel.m_vMeanTexture);
		lastMeanEstimate.SetSize(m_pModel.m_vMeanTexture.Length());
		int iter = 0;
		while (diff > 1e-5 && iter < 10) {

			// save last mean estimate
			lastMeanEstimate.assign(m_pModel.m_vMeanTexture);

			// normalize all texture vectors to the new mean estimate
			for (int i = 0; i < nbTextures; i++) {

				m_pModel.NormalizeTexture(m_vTexture.get(i));
			}

			// recalc mean texture
			RecalcMeanTexture();
			m_pModel.m_vMeanTextureOrg.assign(m_pModel.m_vMeanTexture);

			// test if the mean estimate has converged
			diff = (m_pModel.m_vMeanTexture.sub(lastMeanEstimate)).Norm2();

			++iter;
		}
		m_pModel.m_vMeanTextureOrg.assign(m_pModel.m_vMeanTexture);
	}

	/**
	 * Reads and parses an ACF file. In this way the AAM can be configured using
	 * different setting for model generation. Note that all parsing is very
	 * primitive and in no way robust :-( So be careful about the configuration
	 * files.
	 * 
	 * @param filename
	 *            The acf file to open.
	 * @return true on success, false on file errors.
	 */
	public boolean ReadACF(final String filename) {

		CAAMPropsReader r = new CAAMPropsReader(filename);
		int i;
		if (!r.IsValid())
			return false;

		// read settings
		i = r.Sync();
		m_pModel.m_iModelReduction = i;

		i = r.Sync();
		m_pModel.m_dAddExtents = i;

		i = r.Sync();
		m_pModel.m_bUseConvexHull = i != 0;

		i = r.Sync();
		m_bVerbose = i != 0;

		i = r.Sync();
		m_bWriteRegMovie = i != 0;

		i = r.Sync();
		m_bWriteVarImage = i != 0;

		i = r.Sync();
		m_bMakeDocumentation = i != 0;

		i = r.Sync();
		m_pModel.m_bUseTangentSpace = i != 0;

		i = r.Sync();
		m_pModel.m_iLearningMethod = i;

		i = r.Sync();
		m_pModel.m_iShapeTrunc = i;

		i = r.Sync();
		m_pModel.m_iTextureTrunc = i;

		i = r.Sync();
		m_pModel.m_iCombinedTrunc = i;

		i = r.Sync();
		m_iTSSubsampling = i;

		i = r.Sync();
		m_iWarpMethod = i;

		r.dispose();

		return true;
	}

	/**
	 * Reads and parses an ACF file. In this way the AAM can be configured using
	 * different setting for model generation. Note that all parsing is very
	 * primitive and in no way robust :-( So be careful about the configuration
	 * files.
	 * 
	 * @param filename
	 *            The acf file to open.
	 * @return true on success, false on file errors.
	 */
	public boolean setACF() {

		m_pModel.m_iModelReduction = 1;

		m_pModel.m_dAddExtents = 0;

		m_pModel.m_bUseConvexHull = true;

		m_bVerbose = false;

		m_bWriteRegMovie = false;

		m_bWriteVarImage = false; 

		m_bMakeDocumentation = false;

		m_pModel.m_bUseTangentSpace = (1 != 0);

		m_pModel.m_iLearningMethod = 1;

		m_pModel.m_iShapeTrunc = 90;

		m_pModel.m_iTextureTrunc = 90;

		m_pModel.m_iCombinedTrunc = 90;

		m_iTSSubsampling = 1;

		m_iWarpMethod = 1;

		return true;
	}

	/**
	 * Recalculates the mean texture vector 'm_vMeanTexture'.
	 */
	public void RecalcMeanTexture() {

		int nbTextures = m_vTexture.size();
		assert (nbTextures > 0);
		int nbTexSamples = m_vTexture.get(0).Length();

		// calc the mean texture
		m_pModel.m_vMeanTexture.Resize(nbTexSamples);
		m_pModel.m_vMeanTexture.assign(.0);
		for (int i = 0; i < nbTextures; i++) {

			m_pModel.m_vMeanTexture.add_into(m_vTexture.get(i));
		}
		m_pModel.m_vMeanTexture.div_into(nbTextures);
	}

	/**
	 * Performs principal component analysis on the shape and the texture data.
	 * Uses the Eckhart-Young theorem described in appendix A of
	 * "Statistical Models of Appearance of Computer Vision" by T.F. Cootes et
	 * al. if we have fewer samples than dimensions which is typically the case.
	 * 
	 * @return The b-parameters for all training examples. The format is a
	 *         vector of b-parameter vectors. This vector is used in the
	 *         BuildRegressionMatrices() call.
	 */
	public Vector<CDVector> DoCombinedPCA() {

		Vector<CDVector> bVectors = new Vector<CDVector>();
		int nbTextures = m_vTexture.size();
		assert (nbTextures > 0);
		int nbShapeParam = m_pModel.m_ShapePCA.NParameters();
		int nbTexParam = m_pModel.m_TexturePCA.NParameters();

		// ////////////////////////////////////////////////////////
		// generate b-vectors for all elements in the trainingset
		// ////////////////////////////////////////////////////////

		// calc the pixel to shape weights
		CalcPixel2ShapeWeights();

		// generate b-vectors for all elements in the trainingset
		for (int i = 0; i < nbTextures; i++) {
			CDVector b = new CDVector();

			// calc parameters
			m_pModel.ShapeTex2Param(m_AlignedShapes.get(i), m_vTexture.get(i),
					b);

			// add
			m_pModel.m_CombinedPCA.InsertDataItem(b);
			bVectors.add(b);
		}
		if (m_pModel.m_iCombinedTrunc == -2) {

			// just concatenate vectors
			m_pModel.m_CombinedPCA.UseIdentityTransformation();
		} else {

			// do normal pca
			m_pModel.m_CombinedPCA.DoPCA();

			// truncate eigenvectors and eigenvalues to
			// satisfy the variance explanation level
			// constraint (velc) or by using parallel analysis
			if (m_pModel.m_iCombinedTrunc == -1) {

				m_pModel.m_CombinedPCA.TruncateParallel();
				m_pModel.m_CombinedPCA.ClearDataItems();
			} else {
				m_pModel.m_CombinedPCA.ClearDataItems();
				m_pModel.m_CombinedPCA
						.TruncateVar(m_pModel.m_iCombinedTrunc / 100.);
			}
		}

		// extract the shape part of the combined eigen vectors
		m_pModel.m_mQsEV.Resize(nbShapeParam, m_pModel.m_CombinedPCA
				.EigenVectors().NCols());
		m_pModel.m_mQsEV.assign(m_pModel.m_CombinedPCA.EigenVectors()
				.Submatrix(m_pModel.m_mQsEV.NRows(), m_pModel.m_mQsEV.NCols(),
						0, 0));

		// extract the texture part of the combined eigen vectors
		m_pModel.m_mQgEV.Resize(nbTexParam, m_pModel.m_CombinedPCA
				.EigenVectors().NCols());
		m_pModel.m_mQgEV.assign(m_pModel.m_CombinedPCA.EigenVectors()
				.Submatrix(m_pModel.m_mQgEV.NRows(), m_pModel.m_mQgEV.NCols(),
						nbShapeParam, 0));

		return bVectors;
	}

	/**
	 * Build the prediction matrices for pose and parameter prediction. I.e.
	 * calculates the member variables 'regR_c', 'regR_t'. using estimates of
	 * the gradient matrices.
	 * 
	 * @param bVectors
	 *            The b-vectors for the training set the current AAM is built
	 *            upon. [Can be optained from the DoCombinedPCA() call]
	 * @param ts_subsampling
	 *            Controls the sub sampling of the training set - i.e. to use
	 *            every fifth shape to build the regression matrices upon, set
	 * 
	 *            shape_subsampling = 5;
	 * 
	 *            The motivation for doing this is reduction of model building
	 *            time.
	 * @return Nothing.
	 */
	public boolean EstPredictionMatrices(final Vector<CDVector> bVectors,
			final int ts_subsampling) {

		boolean isOutsideImage = false;
		// output info
		int nShapes = m_Shapes.NShapes();
		int nbShapeSamples = nShapes % ts_subsampling != 0 ? nShapes
				/ ts_subsampling + 1 : nShapes / ts_subsampling;
		System.err.println("Info: Training set subsampling = " + ts_subsampling
				+ " + (" + nbShapeSamples + " shapes used)");

		// convert b to c parameters
		Vector<CDVector> cVectors = new Vector<CDVector>();
		m_pModel.ShapeTexParam2Combined(bVectors, cVectors);

		// generate displacement vectors
		Vector<CDVector> vCDisps = new Vector<CDVector>();
		Vector<CDVector> vPoseDisps = new Vector<CDVector>();
		DisplacementSets(vCDisps, vPoseDisps, cVectors);

		//
		// NOTICE: assumes that the displacement sets only are
		// displacing one parameter at the time

		// do model parameter experiments
		{
			CDMatrix Gparam = new CDMatrix();
			isOutsideImage = EstCParamGradientMatrix(vCDisps, cVectors, Gparam,
					ts_subsampling);
			if (isOutsideImage)
				return true;
			// estimate Rc

			// calc pseudo-inverse
			//
			// this is done much more stable using svd,
			// but this requires to much memory, hence
			// a simple inversion is used
			//
			CDMatrix Gt = new CDMatrix();
			Gt.assign(Gparam.Transposed());
			m_pModel.m_R_c.assign((Gt.mult(Gparam)).Inverted().mult(Gt));
		}

		// do pose experiments
		{
			CDMatrix Gpose = new CDMatrix();
			isOutsideImage = EstPoseGradientMatrix(vPoseDisps, cVectors, Gpose,
					ts_subsampling);
			if (isOutsideImage)
				return true;
			// estimate Rt

			// calc pseudo-inverse
			//
			// this is done much more stable using svd,
			// but this requires to much memory, hence
			// a simple inversion is used
			//
			CDMatrix Gt = new CDMatrix();
			Gt.assign(Gpose.Transposed());
			m_pModel.m_R_t.assign((Gt.mult(Gpose)).Inverted().mult(Gt));
		}

		// store negative versions to avoid any sign change at each iteration
		m_pModel.m_R_c.mult_into(-1);
		m_pModel.m_R_t.mult_into(-1);
		return false;
	}

	/**
	 * Estimates the Jacobian of the pose parameters given a set of displacement
	 * vectors and the optimum model parameters for the training set.
	 * 
	 * @param vPoseDisps
	 *            A vector of displacement vectors as obtained from
	 *            PoseDispVectors() or DisplacementSets().
	 * @param cVectors
	 *            The set of optimum c vectors for the training examples.
	 * @param Gpose
	 *            The output Jacobian matrix (or gradient matrix if you like).
	 * @param ts_subsampling
	 *            Subsampling factor, i.e. ts_subsampling==n will carry out
	 *            displacements on every n-th example in the training set.
	 * @return Nothing.
	 */
	public boolean EstPoseGradientMatrix(final Vector<CDVector> vPoseDisps,
			final Vector<CDVector> cVectors, CDMatrix Gpose,
			final int ts_subsampling) {

		boolean isOutsideImage = false;
		// setup
		int nShapes = m_Shapes.NShapes();

		// determine subsampling of the training set
		int nbShapeSamples = nShapes % ts_subsampling != 0 ? nShapes
				/ ts_subsampling + 1 : nShapes / ts_subsampling;
		int totalExp = (int) (vPoseDisps.size() / 2. * nbShapeSamples);
		int nExperiment = 0;

		int nParam = vPoseDisps.get(0).Length();
		Gpose.Resize(m_pModel.m_iTextureSamples, nParam);
		CDVector dg1 = new CDVector();
		CDVector dg2 = new CDVector();

		CDVector normFactors = new CDVector(nParam);

		// reset gradient matrix
		Gpose.assign(.0);

		// for each training example in the (subsampled) training set
		for (int nShape = 0; nShape < nShapes; nShape += ts_subsampling) {

			// get the shape image
			ModelSimpleImage image = new ModelSimpleImage();
			// image = m_Shapes.get(nShape).GetHostImage(image, m_Shapes.Path(),
			// m_pModel.m_iModelReduction); // ruida 2
			image = m_Shapes.get(nShape).GetHostImage(
					m_Shapes.get(nShape).getHostImage(),
					m_pModel.m_iModelReduction);

			for (int i = 0; i < vPoseDisps.size(); i += 2) {

				// find the index of the displaced parameter
				int j = 0;
				while (vPoseDisps.get(i).m_data[j] == 0) {
					++j;
				}

				// do negative displacement
				isOutsideImage = PoseDisplacement(image, m_Shapes.get(nShape),
						cVectors.get(nShape), vPoseDisps.get(i), dg1);
				if (isOutsideImage) {
					return true;
				}

				// do positive displacement
				isOutsideImage = PoseDisplacement(image, m_Shapes.get(nShape),
						cVectors.get(nShape), vPoseDisps.get(i + 1), dg2);
				if (isOutsideImage) {
					return true;
				}

				CDVector cDiff = new CDVector();
				cDiff.Resize(dg1.m_length);
				// form central difference
				cDiff.assign((dg2.sub(dg1)).div((vPoseDisps.get(i + 1).m_data[j] - vPoseDisps
						.get(i).m_data[j])));

				// accumulate the results into the j-th column
				CDVector Gj = new CDVector(m_pModel.m_iTextureSamples);
				Gpose.Col(j, Gj);
				Gj.add_into(cDiff);
				Gpose.SetColumn(j, Gj);
				++nExperiment;

				// increment the normalisation factor
				normFactors.m_data[j] = normFactors.m_data[j] + 1;

			}
			System.err.println("Experiment " + (2 * nExperiment) + " of "
					+ (2 * totalExp) + " done (pose)...");

		}

		// normalize
		for (int j = 0; j < Gpose.NCols(); j++) {
			CDVector Gj = new CDVector(m_pModel.m_iTextureSamples);
			Gpose.Col(j, Gj);
			Gj.div_into(normFactors.m_data[j]);
			Gpose.SetColumn(j, Gj);
		}
		return false;
	}

	/**
	 * Estimates the Jacobian of the model parameters given a set of
	 * displacement vectors and the optimum model parameters for the training
	 * set.
	 * 
	 * @param vCDisps
	 *            A vector of displacement vectors as obtained from
	 *            CParamDispVectors() or DisplacementSets().
	 * @param cVectors
	 *            The set of optimum c vectors for the training examples.
	 * @param Gparam
	 *            The output Jacobian matrix (or gradient matrix if you like).
	 * @param ts_subsampling
	 *            Subsampling factor, i.e. ts_subsampling==n will carry out
	 *            displacements on every n-th example in the training set.
	 * @return Nothing.
	 */
	public boolean EstCParamGradientMatrix(final Vector<CDVector> vCDisps,
			final Vector<CDVector> cVectors, CDMatrix Gparam,
			final int ts_subsampling) {

		boolean isOutsideImage = false;
		// setup
		int nShapes = m_Shapes.NShapes();

		// determine subsampling of the training set
		int nbShapeSamples = nShapes % ts_subsampling != 0 ? nShapes
				/ ts_subsampling + 1 : nShapes / ts_subsampling;
		int totalExp = (int) (vCDisps.size() / 2. * nbShapeSamples);
		int nExperiment = 0;

		int nParam = vCDisps.get(0).Length();
		Gparam.Resize(m_pModel.m_iTextureSamples, nParam);
		CDVector dg1 = new CDVector();
		CDVector dg2 = new CDVector();

		CDVector normFactors = new CDVector(nParam);

		// reset gradient matrix
		Gparam.assign(.0);

		// for each training example in the (subsampled) training set
		for (int nShape = 0; nShape < nShapes; nShape += ts_subsampling) {

			// get the shape image
			ModelSimpleImage image = new ModelSimpleImage();
			// image = m_Shapes.get(nShape).GetHostImage(image, m_Shapes.Path(),
			// m_pModel.m_iModelReduction); // ruida 1
			image = m_Shapes.get(nShape).GetHostImage(
					m_Shapes.get(nShape).getHostImage(),
					m_pModel.m_iModelReduction);

			for (int i = 0; i < vCDisps.size(); i += 2) {

				// find the index of the displaced parameter
				int j = 0;
				while (vCDisps.get(i).m_data[j] == 0) {
					++j;
				}

				// do negative displacement
				isOutsideImage = ModelDisplacement(image, m_Shapes.get(nShape),
						cVectors.get(nShape), vCDisps.get(i), dg1);
				if (isOutsideImage)
					return true;

				// do positive displacement
				isOutsideImage = ModelDisplacement(image, m_Shapes.get(nShape),
						cVectors.get(nShape), vCDisps.get(i + 1), dg2);
				if (isOutsideImage)
					return true;

				CDVector cDiff = new CDVector();
				cDiff.Resize(dg1.m_length);
				// form central difference
				cDiff.assign((dg2.sub(dg1)).div((vCDisps.get(i + 1).m_data[j] - vCDisps
						.get(i).m_data[j])));

				// accumulate the results into the j-th column
				CDVector Gj = new CDVector(m_pModel.m_iTextureSamples);
				Gparam.Col(j, Gj);
				Gj.add_into(cDiff);
				Gparam.SetColumn(j, Gj);
				++nExperiment;

				// increment the normalisation factor
				normFactors.m_data[j] = normFactors.m_data[j] + 1;
			}
			System.err.println("Experiment " + (2 * nExperiment) + " of "
					+ (2 * totalExp) + " done (c)...");

		}

		// normalize
		for (int j = 0; j < Gparam.NCols(); j++) {
			CDVector Gj = new CDVector(m_pModel.m_iTextureSamples);
			Gparam.Col(j, Gj);
			Gj.div_into(normFactors.m_data[j]);
			Gparam.SetColumn(j, Gj);
		}
		return false;
	}

}