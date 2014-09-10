package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

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
 * Auxiliary point data. Needed as a part of the enhanced shape representation
 * extension [Which explains why it's layered upon the CAAMShape class and not
 * merged into it].
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMPointInfo {

	/** shape path ID to indicate which shape. */
	public int m_iPathID;

	/** point type flag. */
	public int m_iTypeFlags;

	/** point connect from index. */
	public int m_iConnectFrom;

	/** point connect to index. */
	public int m_iConnectTo;

	/**
	 * Constructor
	 */
	public CAAMPointInfo() {
		m_iPathID = 0;
		m_iTypeFlags = 0;
		m_iConnectFrom = 0;
		m_iConnectTo = 0;
	}

	/**
	 * Constructor
	 * 
	 * @param _m_iPathID
	 *            shape path ID
	 * @param _m_iTypeFlags
	 *            point type
	 * @param _m_iConnectFrom
	 *            connect from index
	 * @param _m_iConnectTo
	 *            connnct to index
	 */
	public CAAMPointInfo(int _m_iPathID, int _m_iTypeFlags,
			int _m_iConnectFrom, int _m_iConnectTo) {
		this.m_iPathID = _m_iPathID;
		this.m_iTypeFlags = _m_iTypeFlags;
		this.m_iConnectFrom = _m_iConnectFrom;
		this.m_iConnectTo = _m_iConnectTo;
	}

	/**
	 * Read point info from a file.
	 * 
	 * @param fh
	 *            input file handler
	 */
	public void FromFile(DataInputStream fh) {
		try {
			m_iPathID = fh.readInt();
			m_iTypeFlags = fh.readInt();
			m_iConnectFrom = fh.readInt();
			m_iConnectTo = fh.readInt();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Write point info to a file
	 * 
	 * @param fh
	 *            output file handler
	 */
	public void ToFile(DataOutputStream fh) {
		try {
			fh.writeInt(m_iPathID);
			fh.writeInt(m_iTypeFlags);
			fh.writeInt(m_iConnectFrom);
			fh.writeInt(m_iConnectTo);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Assignment operator.
	 * 
	 * @param pInfo
	 *            point info
	 */
	public void assign(CAAMPointInfo pInfo) {
		this.m_iPathID = pInfo.m_iPathID;
		this.m_iTypeFlags = pInfo.m_iTypeFlags;
		this.m_iConnectFrom = pInfo.m_iConnectFrom;
		this.m_iConnectTo = pInfo.m_iConnectTo;
	}

	/**
	 * Point info method, is outer edge or not.
	 * 
	 * @return outer edge or not.
	 */
	public boolean IsOuterEdge() {
		return !((m_iTypeFlags & 0x1) == 1 ? true : false);
	}

	/**
	 * Point info method.
	 * 
	 * @return is original point.
	 */
	public boolean IsOriginal() {
		return !((m_iTypeFlags & 0x2) == 1 ? true : false);
	}

	/**
	 * Point info method, is closed point
	 * 
	 * @return is closed point
	 */
	public boolean IsClosed() {
		return !((m_iTypeFlags & 0x4) == 1 ? true : false);
	}

	/**
	 * Point info method.
	 * 
	 * @return is a hole.
	 */
	public boolean IsHole() {
		return 0x8 == (m_iTypeFlags & 0x8);
	}

	/**
	 * Set outer edge.
	 */
	public void SetOuterEdge() {
		SetOuterEdge(true);
	}

	/**
	 * Sets whether the point belongs to an outer edge or not.
	 * 
	 * @param enable
	 *            true outer edge, false not.
	 */
	public void SetOuterEdge(boolean enable) {
		if (!enable)
			m_iTypeFlags |= 0x1;
		else
			m_iTypeFlags &= Character.MAX_VALUE - 0x1;
	}

	/**
	 * Set original point.
	 */
	public void SetOriginal() {
		SetOriginal(true);
	}

	/**
	 * Sets whether the point is an original model point or an artificial
	 * generated point.
	 * 
	 * @param enable
	 *            true original point, false not.
	 */
	public void SetOriginal(boolean enable) {
		if (!enable)
			m_iTypeFlags |= 0x2;
		else
			m_iTypeFlags &= Character.MAX_VALUE - 0x2;
	}

	/**
	 * Set the point to closed point.
	 */
	public void SetClosed() {
		SetClosed(true);
	}

	/**
	 * Sets whether the point belongs to a closed or opened path.
	 * 
	 * @param enable
	 *            true belong to path, false not.
	 */
	public void SetClosed(boolean enable) {
		if (!enable)
			m_iTypeFlags |= 0x4;
		else
			m_iTypeFlags &= Character.MAX_VALUE - 0x4;
	}

	/**
	 * Sets the point to represent a hole
	 */
	public void SetHole() {
		SetHole(true);
	}

	/**
	 * Sets whether the point belongs to an hole in the model.
	 * 
	 * @param enable
	 *            true is hole, false not.
	 */
	public void SetHole(boolean enable) {
		if (enable)
			m_iTypeFlags |= 0x8;
		else
			m_iTypeFlags &= Character.MAX_VALUE - 0x8;
	}

}