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
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
 * 
 * abstract class for AAM console mode.
 * 
 * @author Ruida Cheng
 * 
 */
public abstract class CAAMConsoleMode {

	/** min number of parameters */
	protected int m_NMinParam;

	/** max number of parameters */
	protected int m_NMaxParam;

	/** console mode name */
	protected String m_Name = new String();

	/** console mode usage */
	protected String m_Usage = new String();

	/** console mode description */
	protected String m_Desc = new String();

	/** console mode long description */
	protected String m_LongDesc = new String();

	/** console mode programm name */
	protected String m_ProgName = new String();

	/**
	 * constructor
	 */
	public CAAMConsoleMode() {
		m_ProgName = "";
	}

	/**
	 * Constructor
	 * 
	 * @param progname
	 *            console mode program name
	 */
	public CAAMConsoleMode(final String progname) {
		m_ProgName = progname;
	}

	/**
	 * Get the console mode name
	 * 
	 * @return console mode name
	 */
	public String Name() {
		return m_Name;
	}

	/**
	 * Get the console mode usage
	 * 
	 * @return console mode usage
	 */
	public String Usage() {
		return m_Usage;
	}

	/**
	 * Get the console mode description
	 * 
	 * @return console mode description
	 */
	public String Desc() {
		return m_Desc;
	}

	/**
	 * Get the console mode long description
	 * 
	 * @return console mode long description
	 */
	public String LongDesc() {
		return m_LongDesc;
	}

	/**
	 * C style anchor for console mode invocation
	 * 
	 * @param argc
	 *            number of augments
	 * @param argv
	 *            augments array
	 * @return nothing
	 */
	public int Main(int argc, String[] argv) {

		// check for help param
		if (argc == 1) {

			if (CAAMConsole.IsHelpSwitch(argv[0])) {

				PrintUsage();
				System.exit(0);
			}
		}

		// check param amount
		if (!(argc >= m_NMinParam && argc <= m_NMaxParam)) {

			PrintUsage();
			System.exit(1);
		}

		return 0;
	}

	/**
	 * print console mode usage.
	 */
	public void PrintUsage() {

		System.err.println("USAGE:\n      " + m_ProgName + " " + m_Name + " "
				+ m_Usage);
		System.err.println("DESCRIPTION:\n\n" + m_Desc + "\n\n" + m_LongDesc);
	}

}