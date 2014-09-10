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
 * AAMDef defines a few global central math functions.
 * 
 * @author Ruida Cheng
 * 
 */
public class AAMdef {

	/**
	 * define the number of bands used in the AAM model. 1 for single intensity
	 * channel of MRI prostate grayscale image. 3 for the RGB channels.
	 * */
	public static int BANDS = 1;

	/** define the max function with two given double values. */

	/**
	 * define the max function with two given double values.
	 * 
	 * @param a
	 *            first number
	 * @param b
	 *            second number
	 * @return the max of the two given numbers
	 */
	public static double AAM_MAX2(double a, double b) {
		return a > b ? a : b;
	}

	/**
	 * define the max function for three given double values.
	 * 
	 * @param a
	 *            first number
	 * @param b
	 *            second number
	 * @param c
	 *            third number
	 * @return max value from the three
	 */
	public static double AAM_MAX3(double a, double b, double c) {
		return AAM_MAX2(AAM_MAX2(a, b), c);
	}

	/**
	 * define the min function for two given double values.
	 * 
	 * @param a
	 *            first number
	 * @param b
	 *            second number
	 * @return min value from the two
	 */
	public static double AAM_MIN2(double a, double b) {
		return a < b ? a : b;
	}

	/**
	 * define the min function for three given double values.
	 * 
	 * @param a
	 *            first number
	 * @param b
	 *            second number
	 * @param c
	 *            third number
	 * @return min value from the three
	 */
	public static double AAM_MIN3(double a, double b, double c) {
		return AAM_MIN2(AAM_MIN2(a, b), c);
	}

	/**
	 * Round up function
	 * 
	 * @param x
	 *            double precision value
	 * @return round up integer value
	 */
	public static int AAMRound(double x) {
		return (int) (.5 + x);
	}

	/**
	 * Set the bands for the AAM model.
	 * 
	 * @param AAM_3BAND
	 *            true == color RGB channel, false == intensity channel
	 */
	public void setBands(boolean AAM_3BAND) {
		if (AAM_3BAND == true) {
			BANDS = 3;
		} else {
			BANDS = 1;
		}
	}
}