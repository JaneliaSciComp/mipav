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
 * Abstract base class for all transfer functions. This class defines an
 * interface for the so-called transfer functions, which are nothing but
 * differnt mapping from one vector space to another.
 * 
 * @author Ruida Cheng
 * 
 */
public abstract class CAAMTransferFunction extends CAAMObject {

	/** Transfer function types. */
	public int eTFid;
	public static int tfBase = 0;
	public static int tfIdentity = 1;
	public static int tfLookUp = 2;
	public static int tfUniformStretch = 3;
	public static int tfWavelet = 4;

	/** Transfer function id. */
	protected int m_Id;

	/**
	 * Contructor
	 */
	public CAAMTransferFunction() {
		m_Id = tfBase;
	}

	/**
	 * dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Get transfer function id.
	 * 
	 * @return
	 */
	public int Type() {
		return m_Id;
	}

	/**
	 * Type name.
	 * 
	 * @return
	 */
	public abstract String TypeName();

	/**
	 * Get type info
	 * 
	 * @return type info
	 */
	public String TypeInfo() {
		String s = new String();
		return s;
	}

	public abstract void Map(CDVector v);

	public abstract void DeMap(CDVector v);

	public abstract CAAMTransferFunction Clone();

	/**
	 * Not used
	 * 
	 * @param fh
	 * @param _id
	 */
	public void FromFile(DataInputStream fh, int _id) {

		int id = 0;
		// try {
		// fread(id,sizeof(unsigned int),1,fh);
		id = _id; // fh.readInt();
		// } catch (IOException e ) {
		// e.printStackTrace();
		// }
		m_Id = id;
	}

	/**
	 * Not used
	 * 
	 * @param fh
	 */
	public void ToFile(DataOutputStream fh) {

		int id;

		id = m_Id;
		try {
			fh.writeInt(id);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * The function loads a transfer function from a stream, instantiates the
	 * correct concrete class and returns a base class pointer.
	 * 
	 * @param fh
	 *            Open binary stream.
	 * @param pModel
	 *            Model pointer (currently not used).
	 * @return A pointer to a transfer function created on the heap.
	 */
	public static CAAMTransferFunction AAMLoadTransferFunction(
			DataInputStream fh, CAAMModel pModel) {

		CAAMTransferFunction pTF = null;
		int id;

		try {
			id = fh.readInt();
			int tf_id = id;

			switch (tf_id) {

			case 1:
				pTF = new CAAMTFIdentity();
				break;

			case 2:
				pTF = new CAAMTFLookUp();
				break;

			case 3:
				pTF = new CAAMTFUniformStretch();
				break;

			default:
				System.err
						.println("Error: LoadTF(): Unknown transfer function.");
				System.exit(-1);
				break;
			}

			pTF.FromFile(fh, tf_id);
		} catch (IOException e) {
			e.printStackTrace();
		}
		return pTF;
	}

}