package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.structures.*;
import java.util.*;

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
 * Console mode B
 * 
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
 * 
 * @author ruida
 * 
 */
public class CAAMConsoleModeB extends CAAMConsoleMode {

	/** AAM model reference. */
	private C_AAMMODEL model;

	/**
	 * Constructor
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeB(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 2;
		m_NMaxParam = 3;

		m_Name = "b";
		m_Usage = "<input dir> <out model> [acf file]";
		m_Desc = "Builds an Active Appearance Model.";
		m_LongDesc = "In this mode the principal component analysis, parameter optimization training\n"
				+ "etc. are done.\n\n"
				+ "input dir : Directory containing images and annotations.\n"
				+ "out model : Filename (and path) of the output model file. Ex. 'model42'.\n"
				+ "acf file  : AAM configuration file.\n";
	}

	/**
	 * Constructor
	 */
	public CAAMConsoleModeB() {

		m_ProgName = "b";
		m_NMinParam = 2;
		m_NMaxParam = 3;

		m_Name = "b";
		m_Usage = "<input dir> <out model> [acf file]";
		m_Desc = "Builds an Active Appearance Model.";
		m_LongDesc = "In this mode the principal component analysis, parameter optimization training\n"
				+ "etc. are done.\n\n"
				+ "input dir : Directory containing images and annotations.\n"
				+ "out model : Filename (and path) of the output model file. Ex. 'model42'.\n"
				+ "acf file  : AAM configuration file.\n";
	}

	/**
	 * Build the AAM model from the given image and VOIs vector
	 * 
	 * @param modelImageVector
	 *            image and VOIs vector
	 * @param modelDirectory
	 *            saved model directory
	 * @param modelName
	 *            saved model name
	 * @return nothing
	 */
	public boolean buildModel(Vector<ModelImage> modelImageVector,
			String modelDirectory, String modelName) {
		boolean success = false;
		C_AAMBUILDER builder = new C_AAMBUILDER();
		model = new C_AAMMODEL();
		System.err.println("Building Active Appearence Model.");

		// build model
		success = builder.BuildModel(model, modelImageVector);
		if (success == false)
			return false;

		// write
		if (modelName.equals("")) {
			modelName = "model";
		}

		if (model.getNumberShapeParameters() >= 2) {
			model.WriteModel(modelDirectory + modelName);
		}

		return true;
	}

	/**
	 * Build the model with the given model image and VOIs vector along
	 * 
	 * @param modelImageVector
	 *            image VOIs vector
	 * @return nothing
	 */
	public boolean buildModel(Vector<ModelImage> modelImageVector) {
		boolean success = false;
		C_AAMBUILDER builder = new C_AAMBUILDER();
		model = new C_AAMMODEL();
		System.err.println("Building Active Appearence Model.");

		// build model
		success = builder.BuildModel(model, modelImageVector);
		if (success == false)
			return false;
		return true;
	}

	/**
	 * Get the built AAM model
	 * 
	 * @return aam model
	 */
	public C_AAMMODEL getModel() {
		return model;
	}

	/**
	 * C style anchor to invoke the B console mode. Being called from the AAM
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

		C_AAMBUILDER builder = new C_AAMBUILDER();
		C_AAMMODEL model = new C_AAMMODEL();
		System.err.println("Building Active Appearence Model.");

		// setup input parameters
		String inDir = argv[1];
		String outModel = argv[2];
		String acf = argv[3];

		String ext = CAAMUtil.GetExt(acf);
		if (ext.equals("acf") || acf.equals("")) {

			// build model
			builder.BuildFromFiles(model, inDir, acf);

			// write
			model.WriteModel(outModel);

		} else {

			if (ext.equals("sacf")) {

				// model sequence
				CAAMModelSeq modelSeq = new CAAMModelSeq();
				modelSeq.BuildFromSACF(acf, inDir);

				// write
				modelSeq.WriteModels(outModel);

			} else {

				System.err.println("Error: unknown configuration extension.");
				System.exit(-1);
			}
		}

		// we're done
		return 0;
	}

}