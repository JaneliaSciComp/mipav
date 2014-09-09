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
 * Console mode REG for shape based registration test.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeREG extends CAAMConsoleMode {

	/**
	 * Constructor.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeREG(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 1;
		m_NMaxParam = 2;

		m_Name = new String("reg");
		m_Usage = new String("<input dir>");
		m_Desc = new String("Registration from a set of shapes.");
		m_LongDesc = new String(
				"This mode creates a registration movie from a set of shapes.\n\n"
						+ "input dir          : Directory containing images and annotations.\n"
						+ "<convex|no-convex*>: Usage of the convex hull as shape extent.\n");
	}

	/**
	 * C style anchor to invoke the REG console mode. Being called from the AAM
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

		String asfPath = argv[0];
		boolean useConvexHull = argc >= 2 ? argv[1].equals("convex") : false;
		boolean writeRefShape = false;

		CAAMUtil.RegistrationMovie("registration.avi", asfPath, useConvexHull,
				writeRefShape);

		return 0;
	}

}