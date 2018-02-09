package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;

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
 * Transfer function that implements a lookup table, which is useful for
 *      e.g. histogram equalisations.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMTFLookUp extends CAAMTransferFunction {

	/**
	 * Transfer function lookup table. 
	 */
	private CDVector m_LUT = new CDVector();
	
	/**
	 * Inverse LUT function. 
	 */
	private CDVector m_InvLUT = new CDVector();

	/**
	 * Constructor.
	 */
	public CAAMTFLookUp() {
		m_Id = tfLookUp;
	}

	/**
	 * dispose memory
	 */
	public void dispose() {

	}

	// / Clone function (conveys type info).
	/**
	 * Clone this instance. 
	 * @return lookup   new created transfer function lookup table. 
	 */
	public CAAMTransferFunction Clone() {
		CAAMTFLookUp lookup = new CAAMTFLookUp();
		lookup.m_LUT = this.m_LUT;
		lookup.m_InvLUT = this.m_InvLUT;
		return lookup;
	}

	/**
     *Returns the clear-text type name 
	 */
	public final String TypeName() {
		return new String("lookup table");
	}

	/**
	 * Loads the lookup table and generates the inverse, i.e. assumes that
	 *      the LUT monotonic.
	 * @param lut
	 *            A lookup table in vector form.
	 */
	public void LoadLUT(final CDVector lut) {

		int len = lut.Length();
		m_LUT.Resize(len);
		m_LUT.assign(lut);

		// approximate inverse lut
		double min = m_LUT.Min();
		double max = m_LUT.Max();
		double mag = max - min;

		double val, min_dist, dist;
		int min_pos = 0;
		m_InvLUT.Resize(len);
		for (int i = 0; i < len; i++) {

			val = mag * i / (len - 1.0) + min;
			min_dist = 1e306;
			for (int j = i; j < len; j++) {

				dist = Math.abs(m_LUT.get(j) - val);
				if (dist < min_dist) {
					min_pos = j;
					min_dist = dist;
				}
			}
			m_InvLUT.m_data[i] = min_pos;
		}

		m_InvLUT.ToMatlab("invlut.m", "il", "", false);
	}

	/**
	 * Maps an input vector using the current lookup table.
	 * @param v
	 *            Input vector. The result is returned in v.
	 */
	public void Map(CDVector v) {

		// transform v from [min;max] into (int)[0, 256], hist is not used
		// (actually *very* costly compared to the mapping...)
		CDVector hist = new CDVector();
		CAAMMathUtil.Hist(v, v.Min(), v.Max(), hist, 256, false, true);

		// do LUT mapping
		if (m_LUT.Length() > 0) {

			int v_len = v.Length();
			for (int i = 0; i < v_len; i++) {

				v.m_data[i] = m_LUT.get((int) v.get(i));
			}
		} else {
			System.err
					.println("Warning: CAAMTFLookUp is not initialized. Using [min;max]->[0,256] map only.");
		}
	}

	/**
	 * The inverse of the lookup table transform. The input vector is
	 *      transformed into [0;255] and the the inverse LUT is applied.
	 *@param v
	 *            Input vector to de-map. The result is returned in v.
	 */
	public void DeMap(CDVector v) {

		// transform v from [min;max] into (int)[0, 256], hist is not used
		CAAMMathUtil.LinearStretchMinMax(v, 0, 255);

		// do inverse LUT mapping
		int v_len = v.Length();
		for (int i = 0; i < v_len; i++) {

			v.m_data[i] = m_InvLUT.get((int) v.get(i));
		}
	}

	/**
	 * Reads the class info from file.
	 * @param fh
	 *            Open binary file handle.
	 */
	public void FromFile(DataInputStream fh) {

		super.FromFile(fh, 0);

		m_LUT.FromFile(fh);
		m_InvLUT.FromFile(fh);
	}

	/**
	 * Writes the class info to file.
	 * @param fh
	 *            Open binary file handle.
	 */
	public void ToFile(DataOutputStream fh) {

		super.ToFile(fh);

		m_LUT.ToFile(fh);
		m_InvLUT.ToFile(fh);
	}

};
