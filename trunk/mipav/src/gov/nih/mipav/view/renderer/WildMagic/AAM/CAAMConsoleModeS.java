package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;
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
 * Console mode S for AAM search testing.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeS extends CAAMConsoleMode {

	/**
	 * Constructor.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeS(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 2;
		m_NMaxParam = 3;

		m_Name = new String("s");
		m_Usage = new String("<input model.amf> <input dir> [movie filename]");
		m_Desc = new String("Active Appearance Model search.");
		m_LongDesc = new String(
				"Search output is written in the input dir.\nAutomatic initialisation is performed on all BMP images.\n");
	}

	/**
	 * C style anchor to invoke the s console mode. Being called from the AAM
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

		boolean writeMovie = false;
		boolean writeImages = true;

		C_AAMMODEL aam = new C_AAMMODEL();

		// setup input parameters
		String inModel = argv[0];
		String inPath = argv[1];
		writeMovie = (argc == 3);
		String movieName = "";
		if (argc == 5)
			movieName = argv[2];

		String outPath = CAAMUtil.AddBackSlash(inPath);

		// read model
		boolean ok = aam.ReadModel(inModel);

		if (!ok) {

			System.err.println("Could not read model file '" + inModel
					+ "'. Exiting...");
			System.exit(1);
		}

		Vector<String> vFilenames = (Vector<String>) CAAMUtil.ScanSortDir(
				inPath, "bmp");
		int nImages = vFilenames.size();

		for (int i = 0; i < nImages; i++) {

			long startTime = System.currentTimeMillis();

			System.err.println("Performing search using auto init on '"
					+ vFilenames.get(i) + "'.");

			// do search
			CDVector c = new CDVector(aam.CombinedPCA().NParameters());
			ModelSimpleImage image = new ModelSimpleImage();

			c.assign(0);

			// read image
			try {
				image = image.ReadBandedFile(vFilenames.get(i));
			} catch (Exception e) {
				e.printStackTrace();
				return -1;
			}

			// reduce to fit the model
			int rc = aam.ReductionFactor();
			if (rc > 1) {
				// image.ReducePyr( rc );
				image.subSample2dBy2();
			}

			// get meanshape meansize
			CAAMShape s = aam.ReferenceShape();

			// initialize
			CAAMInitializeStegmann Stegmann = new CAAMInitializeStegmann(aam);

			Stegmann.Initialize(image, s, c);

			if (writeImages) {

				String initfile = outPath
						+ CAAMUtil.GetFilename(vFilenames.get(i)) + "_init.xml";
				CAAMVisualizer.ShapeStill(image, s, initfile);
			}

			// do the actual optimization
			Vector<CAAMOptState> optStates = new Vector<CAAMOptState>();
			aam.OptimizeModel(image, s, c, 30, optStates);

			// write movie
			if (writeMovie) {

				CAAMVisualizer AAMvis = new CAAMVisualizer(aam);
				String moviefile = CAAMUtil.ForceExt(
						outPath + CAAMUtil.GetFilename(vFilenames.get(i))
								+ movieName, "avi");
				AAMvis.OptimizationMovie(optStates, image, moviefile);
			}

			// write asf
			String asffile = outPath + CAAMUtil.GetFilename(vFilenames.get(i))
					+ "_opt.asf";
			s.WriteASF(asffile, image.Width(), image.Height());

			long endTime = System.currentTimeMillis();
			double secs = (endTime - startTime) / 1000;

			System.err.println("Time spent:  " + (secs / 60.0) + " mins. (="
					+ secs + " secs.)");

			if (writeImages) {

				String optfile = outPath
						+ CAAMUtil.GetFilename(vFilenames.get(i)) + "_opt.xml";
				CAAMVisualizer.ShapeStill(image, s, optfile);
			}
		}

		// we're done
		return 0;
	}

}