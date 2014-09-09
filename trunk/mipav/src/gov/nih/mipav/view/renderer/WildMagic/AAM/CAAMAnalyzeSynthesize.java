package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
import gov.nih.mipav.model.structures.*;

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
 * Abstract base class for the analyze/synthesize classes. To core task of such
 * classes is to sample pixels inside a shape placed on an image (the analyze
 * part) and render a texture vector into a shape given in image coordinates
 * (the synthesize part).
 * 
 * @author Ruida Cheng
 * 
 */
public abstract class CAAMAnalyzeSynthesize {

	/** software synthesize */
	public static int asSoftware = 0;

	/** hardware OpenGL synthesize, currently disabled. */
	public static int asOpenGL = 1;

	/** reference frame */
	protected final CAAMReferenceFrame m_pReferenceFrame;

	/** Synthesize ID */
	protected int m_Id;

	/**
	 * Constructor
	 * 
	 * @param rf
	 *            The reference frame to analyze and synthesize
	 */
	public CAAMAnalyzeSynthesize(final CAAMReferenceFrame rf) {
		m_pReferenceFrame = rf;
	}

	/**
	 * Reads an object from disk. Deprecated.
	 * 
	 * @param fh
	 *            input stream
	 * @param _id
	 *            file id
	 */
	public void FromFile(DataInputStream fh, int _id) {

		int id = -1;
		id = _id;
		m_Id = id;
	}

	/**
	 * Write an object to disk.
	 * 
	 * @param fh
	 *            output stream
	 */
	public void ToFile(DataOutputStream fh) {

		int id;
		try {
			id = m_Id;
			fh.writeInt(id);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Analyzer/Synthesizer loader. The function loads an Analyzer/Synthesizer
	 * from a stream, instantiates the correct concrete class and returns a base
	 * class pointer.
	 * 
	 * @param fh
	 *            input stream from disk
	 * @param rf
	 *            reference frame
	 * @return An instance of Analyzer/Synthesize object created on the heap
	 */
	public static CAAMAnalyzeSynthesize AAMLoadAnalyzerSynthesizer(
			DataInputStream fh, final CAAMReferenceFrame rf) {

		CAAMAnalyzeSynthesize pAS = null;
		int id;
		try {
			id = fh.readInt();

			int as_id = id;

			switch (as_id) {

			case 0:
				pAS = new CAAMAnalyzeSynthesizeSoftware(rf); // asSoftware
				break;

			case 1:
				// pAS = new CAAMAnalyzeSynthesizeOpenGL(rf); // disabled OpenGL
				break;

			default:
				System.err
						.println("Error: AAMLoadAnalyzerSynthesizer(): Unknown class id, NULL returned(!)");
				return null;

			}

			pAS.FromFile(fh, as_id);
		} catch (IOException e) {
			e.printStackTrace();
		}

		return pAS;
	}

	/**
	 * Dispose memory
	 */
	public abstract void dispose();

	/**
	 * Set the analyzed image
	 * 
	 * @param img
	 *            MIPAV simple image as the pass in type
	 */
	public abstract void SetAnalyzeImage(final ModelSimpleImage img);

	/**
	 * Analyze image
	 * 
	 * @param shape
	 *            AAM shape, VOI shape
	 * @param texture
	 *            image texture
	 * @param useInterpolation
	 *            interpolation type
	 * @return success or not
	 */
	public abstract boolean Analyze(final CAAMShape shape, CDVector texture,
			final boolean useInterpolation);

	/**
	 * Analyze image.
	 * 
	 * @param shape
	 *            VOI shape converted to AAM shape
	 * @param refImg
	 *            image reference, MIPAV simple image type
	 * @param useInterpolation
	 *            interpolation type
	 * @return success or not
	 */
	public abstract boolean Analyze(final CAAMShape shape,
			ModelSimpleImage refImg, final boolean useInterpolation);

	/**
	 * Image synthesize.
	 * 
	 * @param shape
	 *            VOI shape
	 * @param texture
	 *            image texture
	 * @param destImage
	 *            target image
	 * @param renderOntoImage
	 *            intermediate image
	 * @return success or not
	 */
	public abstract boolean Synthesize(final CAAMShape shape,
			final CDVector texture, ModelSimpleImage destImage,
			boolean renderOntoImage);

	/**
	 * Anyalyze/Synthesize object clone
	 * 
	 * @param rf
	 *            reference frame
	 * @return cloned object
	 */
	public abstract CAAMAnalyzeSynthesize Clone(final CAAMReferenceFrame rf);

}