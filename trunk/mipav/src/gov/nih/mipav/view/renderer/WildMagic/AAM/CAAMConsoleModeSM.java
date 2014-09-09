package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
import gov.nih.mipav.model.file.*;
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
 * Console mode SM to split movie files into frames.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeSM extends CAAMConsoleMode {

	/**
	 * Constructor.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeSM(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 1;
		m_NMaxParam = 2;

		m_Name = new String("sm");
		m_Usage = new String("<input movie> [output extension]");
		m_Desc = new String("Split movie file (.avi) into frames.");
		m_LongDesc = "Example usage:\n\n     " + m_ProgName
				+ " sm shape01.avi bmp";
	}

	/**
	 * C style anchor to invoke the SM console mode. Being called from the AAM
	 * console.
	 * 
	 * @param argc
	 *            number of augments
	 * @param argv
	 *            augments array
	 * @return nothing
	 */
	public int Main(int argc, String[] argv) {
		// call base implementation
		super.Main(argc, argv);

		// setup input parameters
		String inMovie = argv[0];
		String ext = argc >= 2 ? argv[1] : "bmp";

		FileIO io = new FileIO();
		int index = inMovie.lastIndexOf(File.separator);
		String dir = inMovie.substring(0, index);
		String fileName = inMovie.substring(index + 1, inMovie.length());
		ModelImage aviImage = io.readImage(dir, fileName);
		// write image as XML file.
		aviImage.saveImage(dir, fileName + ".xml", FileUtility.XML, false);

		// we're done
		return 0;
	}

}