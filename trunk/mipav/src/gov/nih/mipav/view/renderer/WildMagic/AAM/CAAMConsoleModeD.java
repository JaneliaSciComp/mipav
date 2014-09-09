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
 * Console mode D for debug
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeD extends CAAMConsoleMode {

	/**
	 * Constructor, Debug/Test console mode, not used.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeD(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 0;
		m_NMaxParam = 100;

		m_Name = new String("d");
		m_Usage = new String("");
		m_Desc = new String("Debug/Test console mode. Don't use!");
		m_LongDesc = new String("");
	}

	/**
	 * Debug function. C style anchor to invoke the D console mode. Being called
	 * from the AAM console.
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

		CAAMModelSeq seq = new CAAMModelSeq();

		// seq.BuildFromSACF( "test.sacf", "data" );

		// seq.WriteModels( "seqtest" );

		seq.ReadModels("seqtest.samf");

		// we're done
		return 0;
	}

}