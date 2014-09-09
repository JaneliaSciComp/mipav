package gov.nih.mipav.view.renderer.WildMagic.AAM;

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
 * Console mode M for movies.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeM extends CAAMConsoleMode {

	/**
	 * Constructor. Console mode M for movies.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeM(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 2;
		m_NMaxParam = 6;

		m_Name = new String("m");
		m_Usage = new String(
				"<model.amf> <all|shape|texture|combined> [#modes] [#frames] [white|black*] [range]");
		m_Desc = new String("Writes Active Appearance Model mode movies.");
		m_LongDesc = new String(
				"This mode documents the given AAM by generating movies showing the shape,\n"
						+ "texture and combined variation resulting from the PCAs.\n"
						+ "\nOutput are written in current dir.\n\n"
						+ "[#modes]        : The number of model modes to render (3).\n"
						+ "[#frames]       : The frames to render each mode in (10).\n"
						+ "[white|black]   : Background colour (black).\n"
						+ "[range]         : Parameter range (3).\n");
	}

	/**
	 * C style anchor to invoke the M console mode. Being called from the AAM
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

		boolean writeMovie = false, writeImages = true;

		// setup input parameters
		String inModel = argv[1];
		String movieType = argv[2];
		int nModes = argc >= 3 ? Integer.valueOf(argv[3]) : 3;
		int nSteps = argc >= 4 ? Integer.valueOf(argv[4]) / 2 : 10;
		boolean bWhiteBG = argc >= 5 ? argv[5].equals("white") : false;
		double range = argc >= 6 ? Double.valueOf(argv[6]) : 3;

		// test movie type
		if (!(movieType.equals("all") || movieType.equals("shape")
				|| movieType.equals("texture") || movieType.equals("combined"))) {

			System.err.println("Error: Movie type '" + movieType
					+ "' is not supported.\n");
			PrintUsage();
			return 1;
		}

		C_AAMMODEL aam = new C_AAMMODEL();

		// read model
		boolean ok = aam.ReadModel(inModel);

		if (!ok) {

			System.err.println("Could not read model file '" + inModel
					+ "'. Exiting...");
			System.exit(1);
		}

		System.err.printf("Generating movie type '" + movieType + "'...");

		// make movie object
		CAAMVisualizer AAMvis = new CAAMVisualizer(aam);

		if (movieType.equals("texture") || movieType.equals("all")) {

			AAMvis.TextureMovie("texture", nModes, range, nSteps, bWhiteBG);
		}

		if (movieType.equals("shape") || movieType.equals("all")) {

			AAMvis.ShapeMovie("shape", nModes, range, nSteps, bWhiteBG);
		}

		if (movieType.equals("combined") || movieType.equals("all")) {

			AAMvis.CombinedMovie("combined", nModes, range, nSteps, bWhiteBG);
		}

		// we're done
		return 0;
	}

}