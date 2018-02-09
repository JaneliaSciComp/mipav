package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;

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