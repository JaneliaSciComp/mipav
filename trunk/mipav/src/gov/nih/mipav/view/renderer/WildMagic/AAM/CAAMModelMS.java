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
 * Multi-scale derivation of CAAModel. Smaller models are stored in m_vModels.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMModelMS extends CAAMModel {

	/** Model vector. */
	protected Vector<CAAMModel> m_vModels = new Vector<CAAMModel>();

	/** Image VOIs vector. */
	protected Vector<ModelSimpleImage> m_vImagePyr = new Vector<ModelSimpleImage>();

	/**
	 * Default multi-scale constructor.
	 */
	public CAAMModelMS() {
		super();
	}

	/**
	 * dispose memory
	 */
	public void dispose() {

		// delete the scale images
		for (int i = 1; i < m_vImagePyr.size(); i++) { // notice that level 0 is
														// handled elsewhere...

			m_vImagePyr.set(i, null);
		}
		m_vImagePyr = null;
	}

	/**
	 * Wrapper for automate model generation.
	 * 
	 * @param nLevels
	 *            number level in multi-scale
	 * @param inDir
	 *            input dir
	 * @param acf
	 *            aam configuration file
	 */
	public void BuildAllLevels(final int nLevels, final String inDir,
			final String acf) {
		BuildAllLevels(nLevels, inDir, acf, 1, -1);
	}

	/**
	 * Driver method for model generation.
	 * 
	 * This method automates the model generation as much as possible by using
	 * the various class methods for all the sequences in the task of producing
	 * a model.
	 * 
	 * @param nLevels
	 *            The number of levels the multi-scale AAM should be in. Default
	 *            3.
	 * @param inDir
	 *            Input directory where annotations (.asf) resides.
	 * @param acf
	 *            Filename of an AAM configuration file. If omitted defaults are
	 *            used.
	 * @param modelReduction
	 *            Model reduction multiplier. Default off == 1. This sets the
	 *            size of the lowest level in the model pyramid, i.e. level 0.
	 * @param excludeShape
	 *            Excludes one shape number 'excludeShape' from the input
	 *            directory. Default -1, i.e. no shapes are removed.
	 * 
	 *            Used to perform leave-one-out testing.
	 */
	public void BuildAllLevels(final int nLevels, final String inDir,
			final String acf, final int modelReduction, final int excludeShape) {

		Vector<String> asfFiles = new Vector<String>();
		asfFiles = (Vector<String>) CAAMUtil.ScanSortDir(
				CAAMUtil.AddBackSlash(inDir), "asf");
		this.BuildAllLevels(nLevels, asfFiles, acf, modelReduction,
				excludeShape);
	}

	/**
	 * Driver method for model generation. This method automates the model
	 * generation as much as possible by using the various class methods for all
	 * the sequences in the task of producing a model.
	 * 
	 * @param nLevels
	 *            The number of levels the multi-scale AAM should be in. Default
	 *            3.
	 * @param asfFiles
	 *            Vector of asf filenames.
	 * @param acf
	 *            Filename of an AAM configuration file. If omitted defaults are
	 *            used.
	 * @param modelReduction
	 *            Model reduction multiplier. Default off == 1. This sets the
	 *            size of the lowest level in the model pyramid, i.e. level 0.
	 * @param excludeShape
	 *            Excludes one shape number 'excludeShape' from the input
	 *            directory. Default -1, i.e. no shapes are removed.
	 * 
	 *            Used to perform leave-one-out testing.
	 */
	public void BuildAllLevels(final int nLevels,
			final Vector<String> asfFiles, final String acf,
			final int modelReduction, final int excludeShape) {

		int reductionMultiplier = modelReduction;

		// delete any old models
		m_vModels.clear();
		// CAAMModel emptyModel = new CAAMModel();
		// this = (CAAMModel)emptyModel;
		this.assign(new CAAMModel());

		for (int i = 0; i < nLevels; i++) {

			System.err.printf("\n*** Building model for level " + (i + 1) + "/"
					+ nLevels + "in the multi-scale AAM (re="
					+ reductionMultiplier + "). ***");

			// build model and add to the level list
			CAAMBuilder builder = new CAAMBuilder();
			CAAMModel pModel = null;

			if (i == 0) {

				// we're at the lowest level
				pModel = this;
			} else {

				// any higher level
				pModel = new CAAMModel();
			}

			builder.BuildFromFiles(pModel, asfFiles, acf, reductionMultiplier,
					excludeShape);

			if (i > 0) {

				m_vModels.add(pModel); // add a copy to the list
			}

			// delete model
			if (pModel != this) {

				pModel = null;
			}

			// move one level up in the scale pyramid -- i.e. scale by 50%
			reductionMultiplier *= 2;
		}
	}

	/**
	 * Writes the complete multi-scale AAMModel to disk as a set of .txt and an
	 * .amf files.
	 * 
	 * @param filename
	 *            Output filename without any extension. Multi-scale prefixes
	 *            will be added.
	 * @param txt_only
	 *            If true binary model data is not written.
	 * @return true on success, false on file errors.
	 */
	public boolean WriteModel(final String filename, final boolean txt_only) {

		String modelname = new String();
		boolean res = true;

		for (int i = 0; i < NLevels() && res; i++) {

			System.err.println(modelname + filename + "_msl " + i);
			res = Model(i).WriteModel(modelname, txt_only);
		}

		return res;
	}

	/**
	 * Reads the complete AAMModel from disk.
	 * 
	 * @param filename
	 *            Input filename without any extension and scale prefixes.
	 * 
	 *            E.g. if the files on disk are
	 * 
	 *            'model_msl<level>.txt' & 'model_msl<level>.amf'
	 * 
	 *            -> filename = 'model'
	 * @return true on success, false on file errors.
	 */
	public boolean ReadModel(final String basename) {

		// delete any old models
		m_vModels.clear();

		String modelname = new String();
		boolean result = true;
		int i = 0;
		while (result) {

			modelname = basename + "_msl" + i;
			CAAMModel model = new CAAMModel();

			System.err.println("Trying to read model '" + modelname + "... ");
			result = model.ReadModel(modelname);

			if (result) {

				if (i == 0) {

					// *(CAAMModel*)this = model;
					this.assign(model);
				} else {

					m_vModels.add(model);
				}
			}
			System.err.println((result ? "succes.\n" : "failure.\n"));

			++i;
		}
		System.err.printf("\nMulti-scale AAM containing " + (--i)
				+ " levels succesfully read.\n");

		return i > 0;

	}

	/**
	 * Performs AAM optimization of a shape containing initial pose using a
	 * model pyramid.
	 * 
	 * @param image
	 *            The image to search in (in size corresponding to level zero).
	 * @param s
	 *            The initial shape (also containing the inital pose, thus; not
	 *            a normalized shape). Actually only the pose of 's' is used (to
	 *            align the reference shape as the initial shape).
	 * 
	 *            NOTE: The optimal shape is returned in 's' after execution.
	 * 
	 *            NOTE: 's' is defined at level 0, i.e. shapes should have the
	 *            same size as when calling CAAMModel::OptimizeModel() directly.
	 * @param c
	 *            The optimal model parameters at level zero. Unlike
	 *            CAAMModel::OptimizeModel() the content of this vector is not
	 *            used, since the optimisation is started the smallest (i.e.
	 *            top) level of the pyramid.
	 * 
	 *            NOTE: The optimal model parameters are returned in 'c' after
	 *            execution.
	 * @param maxIterations
	 *            The maximum iterations allowed at each level.
	 * @param pOptStates
	 *            Optional parameter convergence info (at level zero) can be
	 *            returned in. See CAAMOptState.
	 * @param disableDamping
	 *            Disables the damping steps (default false).
	 * @return The results of the optimization in the form of a 'CAAMOptRes'
	 *         instance.
	 */
	public CAAMOptRes OptimizeModel(final ModelSimpleImage image, CAAMShape s,
			CDVector c, final int maxIterations,
			Vector<CAAMOptState> pOptStates, boolean disableDamping) {

		CAAMOptRes res = new CAAMOptRes();

		int st_level = NLevels() - 1; // start at the smallest level
										// i.e. the top of the pyramid

		// setup
		boolean writeLevelResults = false;

		// make scaled images
		BuildPyr(image);

		// scale shape to fit lowest level
		s.Scale(1. / Math.pow(2., NLevels() - 1), false);

		// do the optimisation
		for (int i = st_level; i >= 0; i--) {

			if (i != st_level) {

				// size the last result around (0,0)
				s.Scale(2., false);

				boolean project = true;
				if (project && CAAMUtil.ShapeInsideImage(s, m_vImagePyr.get(i))) {

					// sample texture under shape and project into the current
					// model
					// (this sample should really be used to start the
					// OptimizeModel
					// below, otherwise we're doing the same work twice.... )
					Model(i).Shape2Combined(s, m_vImagePyr.get(i), c);
				} else {

					// reset
					c.Resize(Model(i).CombinedPCA().NParameters());
					c.assign(0);
				}
			}
			// printf("Optimising at level %i...\n", i );

			// optimize model
			res = Model(i).OptimizeModel(m_vImagePyr.get(i), s, c,
					maxIterations, pOptStates, disableDamping);

			// write results for this level
			if (writeLevelResults) {

				String fn = new String();
				// fn.format( "level%02i_opt.bmp", i );
				fn = "level" + i + "_opt.xml";
				CAAMVisualizer.ShapeStill(m_vImagePyr.get(i), s, fn);
			}
		}

		return res;
	}

	/**
	 * Builds an image pyramid (if its not cached beforehand). The pyramid is
	 * stored in m_VImagePyr.
	 * 
	 * @param image
	 *            Input image that should be convertet to a pyramid. This image
	 *            is assumed to live to the *complete* usage of the pyramid,
	 *            since level 0 is a pointer to this image.
	 */
	public void BuildPyr(final ModelSimpleImage image) {

		boolean cachePyr = m_vImagePyr.size() > 0
				&& m_vImagePyr.size() == NLevels()
				&& image.getName() == m_vImagePyr.get(0).getName();

		// non-const this for caching
		CAAMModelMS thisNC = (CAAMModelMS) (this);

		if (!cachePyr) {

			// printf("Can't use cache. Building image pyr.\n");

			// make scaled images
			thisNC.m_vImagePyr.clear();
			thisNC.m_vImagePyr.setSize(NLevels());
			thisNC.m_vImagePyr.set(0, (ModelSimpleImage) (image));
			for (int i = 1; i < NLevels(); i++) {

				int[] extents = new int[2];
				extents[0] = image.Width();
				extents[1] = image.Height();
				ModelSimpleImage pImage = new ModelSimpleImage(extents);
				pImage = m_vImagePyr.get(i - 1); // silly copy only needed due
													// to the design of
													// reducepyr :-(

				// pImage.ReducePyr( 2 ); // reduce image by 50%
				pImage.subSample2dBy2();
				thisNC.m_vImagePyr.set(i, pImage); // add image
			}
		}
	}

	/**
	 * Get the model at level i.
	 * 
	 * @param i
	 *            level
	 * @return model
	 */
	public final CAAMModel Model(final int i) {
		assert (i >= 0 && i < NLevels());
		return i == 0 ? (CAAMModel) this : m_vModels.get(i - 1);
	}

	/**
	 * Get the number of levels.
	 * 
	 * @return levels number
	 */
	public final int NLevels() {
		return m_vModels.size() + 1;
	}

	/**
	 * Get the smallest model
	 * 
	 * @return model
	 */
	public final CAAMModel GetSmallest() {
		return Model(NLevels() - 1);
	}

};
