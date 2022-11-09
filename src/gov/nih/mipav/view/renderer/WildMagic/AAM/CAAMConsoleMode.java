package gov.nih.mipav.view.renderer.WildMagic.AAM;

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