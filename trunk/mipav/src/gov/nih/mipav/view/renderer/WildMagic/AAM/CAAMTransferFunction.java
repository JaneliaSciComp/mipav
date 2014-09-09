package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation.
 * 
 * Copyright © 2000, 2001, 2002, 2003 by Mikkel B. Stegmann IMM, Informatics &
 * Mathmatical Modelling Technical University of Denmark Richard Petersens Plads
 * Building 321 DK-2800 Lyngby, Denmark http://www.imm.dtu.dk/
 * 
 * Author: Mikkel B. Stegmann - http://www.imm.dtu.dk/~aam/ - aam@imm.dtu.dk
 * 
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
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