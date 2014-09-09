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
 * Console mode E for evaluation.
 * 
 * Author: Mikkel B. Stegmann - http://www.imm.dtu.dk/~aam/ - aam@imm.dtu.dk
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeE extends CAAMConsoleMode {

	/**
	 * Constructor. Console mode E for evaluation.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeE(String progname) {

		m_ProgName = progname;
		m_NMinParam = 2;
		m_NMaxParam = 4;

		m_Name = "e";
		m_Usage = "<model> <dir> [still|movie|both|none*] [pseudo|auto*]";
		m_Desc = "Evaluates an Active Appearance Model.";
		m_LongDesc = "model              : The model .amf that should be evaluated.\n"
				+ "dir                : Directory containing images and ground truth annotations.\n"
				+ "[still|movie|both] : Write stills of the initial and optimized model\n"
				+ "                     and/or movies of the complete optimization.\n"
				+ "[pseudo|auto*]     : Initialisation method.\n"
				+ "\nOutput is written in the input dir in the file 'results.txt'\n"
				+ "Default settings are marked with an asterisk (*)\n";
	}

	/**
	 * Constructor.
	 */
	public CAAMConsoleModeE() {

		m_ProgName = "e";
		m_NMinParam = 2;
		m_NMaxParam = 4;

		m_Name = "e";
		m_Usage = "<model> <dir> [still|movie|both|none*] [pseudo|auto*]";
		m_Desc = "Evaluates an Active Appearance Model.";
		m_LongDesc = "model              : The model .amf that should be evaluated.\n"
				+ "dir                : Directory containing images and ground truth annotations.\n"
				+ "[still|movie|both] : Write stills of the initial and optimized model\n"
				+ "                     and/or movies of the complete optimization.\n"
				+ "[pseudo|auto*]     : Initialisation method.\n"
				+ "\nOutput is written in the input dir in the file 'results.txt'\n"
				+ "Default settings are marked with an asterisk (*)\n";
	}

	/**
	 * C style anchor to invoke the E console mode, evaluation mode. Being
	 * called from the AAM console.
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
		String inModel = argv[1];
		String inPath = CAAMUtil.AddBackSlash(argv[2]);
		String docOutput = argc >= 3 ? argv[3] : "";
		boolean writeMovies = docOutput.equals("movie")
				|| docOutput.equals("both");
		boolean writeStills = docOutput.equals("still")
				|| docOutput.equals("both");
		boolean initialisation = false;

		if (argc >= 4) {

			initialisation = !(argv[4].equals("pseudo"));
		}

		String ext = CAAMUtil.GetExt(inModel);
		if (ext.equals("amf")) {

			C_AAMMODEL model = new C_AAMMODEL();

			// read model
			boolean ok = model.ReadModel(inModel);

			if (!ok) {

				System.err.println("Could not read model file. Exiting...");
				System.exit(1);
			}
			CAAMTest.EvaluateModel(model, inPath, "results.txt", writeStills,
					writeMovies, initialisation, true);

		} else {

			if (ext.equals("samf")) {

				CAAMModelSeq modelSeq = new CAAMModelSeq();

				// read model sequence
				boolean ok = modelSeq.ReadModels(inModel);

				if (!ok) {

					System.err.println("Could not read model file. Exiting...");
					System.exit(1);
				}

				CAAMTest.EvaluateModelSeq(modelSeq, inPath, "results.txt",
						writeStills, writeMovies, initialisation, true);

			} else {

				System.err.println("Error: unknow model extension.");
				System.exit(-1);
			}
		}

		// we're done
		return 0;
	}

	/**
	 * Classification testing routine.
	 * 
	 * @param model
	 *            aam model reference
	 * @param targetImageSlice
	 *            target image slice 2D
	 * @param sampleImage
	 *            sample image
	 * @return
	 */
	public int classification(C_AAMMODEL model, ModelImage targetImageSlice,
			ModelImage sampleImage) {

		// setup input parameters
		// String inModel = "model.amf";
		String inPath = "unseen\\";
		boolean writeMovies = false;
		boolean writeStills = false;
		boolean initialisation = true;

		CAAMTest.EvaluateModel(model, targetImageSlice, sampleImage, inPath,
				"results.txt", writeStills, writeMovies, initialisation, false);

		return 0;
	}

	/**
	 * Classification testing routine.
	 * 
	 * @param model
	 *            AAM model reference
	 * @param targetImageSlice
	 *            target image slice 2D
	 * @return
	 */
	public int classification(C_AAMMODEL model, ModelImage targetImageSlice) {

		// setup input parameters
		// String inModel = "model.amf";
		String inPath = "unseen\\";
		boolean writeMovies = false;
		boolean writeStills = false;
		boolean initialisation = false;

		CAAMTest.EvaluateModel(model, targetImageSlice, inPath, "results.txt",
				writeStills, writeMovies, initialisation, false);

		return 0;
	}
}