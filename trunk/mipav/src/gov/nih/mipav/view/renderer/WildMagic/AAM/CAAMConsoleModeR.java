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
 * Console mode R for regression testing on AAM.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeR extends CAAMConsoleMode {

	/**
	 * Constructor.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeR(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 2;
		m_NMaxParam = 2;

		m_Name = new String("r");
		m_Usage = new String("<model.amf> <input dir>");
		m_Desc = new String("Tests the regression prediction in an AAM.");
		m_LongDesc = new String(
				"Output is written in the current dir in matlab format.");
	}

	/**
	 * C style anchor to invoke the R console mode. Being called from the AAM
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
		String inModel = argv[0];
		String inDir = argv[1];

		C_AAMMODEL aam = new C_AAMMODEL();

		// read model
		boolean ok = aam.ReadModel(inModel);

		if (!ok) {

			System.err.println("Could not read model file '" + inModel
					+ "'. Exiting...");
			System.exit(1);
		}

		// test the prediction accuracy
		CAAMTest.TestPosePrediction(aam, inDir);

		// we're done
		return 0;
	}

}