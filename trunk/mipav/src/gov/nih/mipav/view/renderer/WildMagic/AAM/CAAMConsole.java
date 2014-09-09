package gov.nih.mipav.view.renderer.WildMagic.AAM;

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
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
 * 
 * AAM-API console interface. Current AAM model accessing is still command line
 * console. Will improve the access interface in the future.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsole {

	/** C style argc */
	// private int m_argc;

	/** C style argv */
	private String[] m_argv;

	/** Console mode vector */
	private Vector<CAAMConsoleMode> m_Modes = new Vector<CAAMConsoleMode>();

	/**
	 * Console interface to pass the augments to varies modes
	 * 
	 * @param argc
	 *            number of augments
	 * @param argv
	 *            augments array
	 */
	public CAAMConsole(int argc, String[] argv) {

		// store command line
		m_argv = argv;
		// m_argc = argc;

		CAAMConsoleModeB b = new CAAMConsoleModeB(m_argv[0]);
		m_Modes.add(b);
		CAAMConsoleModeE e = new CAAMConsoleModeE(m_argv[0]);
		m_Modes.add(e);
		CAAMConsoleModeLOO loo = new CAAMConsoleModeLOO(m_argv[0]);
		m_Modes.add(loo);
		CAAMConsoleModeM m = new CAAMConsoleModeM(m_argv[0]);
		m_Modes.add(m);
		CAAMConsoleModeP p = new CAAMConsoleModeP(m_argv[0]);
		m_Modes.add(p);
		CAAMConsoleModeR r = new CAAMConsoleModeR(m_argv[0]);
		m_Modes.add(r);
		CAAMConsoleModeREG reg = new CAAMConsoleModeREG(m_argv[0]);
		m_Modes.add(reg);
		CAAMConsoleModeS s = new CAAMConsoleModeS(m_argv[0]);
		m_Modes.add(s);
		CAAMConsoleModeT t = new CAAMConsoleModeT(m_argv[0]);
		m_Modes.add(t);
		CAAMConsoleModeU u = new CAAMConsoleModeU(m_argv[0]);
		m_Modes.add(u);
		CAAMConsoleModeW w = new CAAMConsoleModeW(m_argv[0]);
		m_Modes.add(w);
		CAAMConsoleModeSM sm = new CAAMConsoleModeSM(m_argv[0]);
		m_Modes.add(sm);
		CAAMConsoleModeCM cm = new CAAMConsoleModeCM(m_argv[0]);
		m_Modes.add(cm);
		CAAMConsoleModeD d = new CAAMConsoleModeD(m_argv[0]);
		m_Modes.add(d);

		// parse command line
		if (argc < 2) {

			// we need at least two arguments to do something
			PrintUsage();
			System.exit(1);
		}

		// print help if requested
		if (IsHelpSwitch(argv[0])) {

			PrintHelp();
			System.exit(0);
		}

		if (argv[1].equals("-full")) {

			// print full help
			PrintFullHelp();
			System.exit(0);
		}

		int modeID = IsModeAllowed(argv[0]);

		if (modeID == -1) {

			// the specified mode was not allowed
			PrintUsage();
			System.exit(1);
		}

		// main call
		String pnuc = new String(m_argv[0]);
		pnuc.toUpperCase();
		try {

			// remove the mode info from the command line
			// and run the mode
			m_Modes.get(modeID).Main(argc - 1, argv);

		} catch (Exception ex) {
			ex.printStackTrace();
			System.err.println(pnuc + ": Exception thrown. The message was:"
					+ ex.toString());
			System.exit(-1);
		}
		// the mode was handled successfully if we reach this point
		System.exit(0);
	}

	/**
	 * Parse help augments
	 * 
	 * @param arg
	 *            help related augments
	 * @return
	 */
	public static boolean IsHelpSwitch(final String arg) {

		return arg.equals("-help") || arg.equals("--h") || arg.equals("-h")
				|| arg.equals("-?") || arg.equals("?");
	}

	/**
	 * Print short help info
	 */
	private void PrintHelp() {

		System.err.println("The tool '" + m_argv[0]
				+ "' is the console interface to the AAM-API.");
	}

	/**
	 * Print full help info
	 */
	private void PrintFullHelp() {

		PrintUsage();
		PrintHelp();

		for (int i = 0; i < m_Modes.size(); i++) {

			// for each mode
			System.err
					.println("\n---------------------------------------------------------------------");
			System.err.println("MODE: " + m_Modes.get(i).Name() + " - "
					+ m_Modes.get(i).Desc());
			System.err
					.println("---------------------------------------------------------------------\n");
			m_Modes.get(i).PrintUsage();
		}
	}

	/**
	 * Checks if the given mode is allowed.
	 * 
	 * @param modeStr
	 *            mode string passed in
	 * @return the mode id
	 */
	private int IsModeAllowed(final String modeStr) {

		for (int i = 0; i < m_Modes.size(); i++) {

			if (modeStr.equals(m_Modes.get(i).Name())) {
				// the mode was found
				return i;
			}
		}

		// we never found the mode
		return -1;
	}

	/**
	 * Print usage
	 */
	private void PrintUsage() {

		String pnuc = new String(m_argv[0]);
		pnuc.toUpperCase();
		System.err
				.println(pnuc
						+ " - Active Appearance Model Console Interface - version 0.98");
		System.err
				.println("Copyright (c) Mikkel B. Stegmann 2003, mbs@imm.dtu.dk. All rights reserved.");
		System.err
				.println("For further information see http://www.imm.dtu.dk/~aam/\n");
		System.err.println("This version of " + pnuc + "was built on: \n");
		System.err.println("USAGE:\n");
		System.err.println("       " + m_argv[0] + " <");
		System.err.println("> [additional mode arguments]\n");

		System.err.println("MODES:");
		for (int i = 0; i < m_Modes.size(); i++)
			System.err.println("       " + m_argv[0] + " "
					+ m_Modes.get(i).Name() + " " + m_Modes.get(i).Usage());

		System.err.println("\nFor futher help write: '" + m_argv[0]
				+ " -help', '" + m_argv[0] + "  -full' or '" + m_argv[0]
				+ "  <modename> -help");
	}

}