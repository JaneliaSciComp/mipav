package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.file.FileUtility;
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
 * Console mode W for write testing.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeW extends CAAMConsoleMode {

	/**
	 * Constructor.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeW(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 2;
		m_NMaxParam = 3;

		m_Name = new String("w");
		m_Usage = new String("<write options> <annotation> [reduction factor]");
		m_Desc = new String(
				"Plots an annotation into it's host image.\n\n"
						+ "<write options>    Set one or several of the following options to get\n"
						+ "                   the desired result (not sensitive to ordering):\n\n"
						+ "                     s   Plots the shape as connecting lines\n"
						+ "                     p   Plots all shape points as cross-hairs\n"
						+ "                     i   Dots the interior of the shape\n"
						+ "                     n   Draws point normals\n"
						+ "                     m   Plots a Delaunay triangulation of the shape\n"
						+ "                     t   Similar to 'm' but removes concave triangles and holes\n\n"
						+ "<annotation>       An .asf-file.\n\n"
						+ "[reduction factor] Optional integer image/annotation reduction factor [2-n]\n\n"
						+ "Example usage:\n\n       " + m_ProgName
						+ " w si c6302.asf\n");
		m_LongDesc = new String("");
	}

	/**
	 * C style anchor to invoke the W console mode. Being called from the AAM
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
		int rfactor = 1;

		// setup
		ModelSimpleImage image = new ModelSimpleImage();
		CAAMShape shape = new CAAMShape();
		String outFile = new String();
		String options = argv[0];
		String asf_file = argv[1];

		// determine write options
		boolean drawLines = options.equals('s');
		boolean drawPoints = options.equals('p');
		boolean drawInterior = options.equals('i');
		boolean drawNormals = options.equals('n');
		boolean drawDelaunay = options.equals('m');
		boolean drawDelaunayClean = options.equals('t');

		if (argc == 3) {

			// scale factor was incluced
			rfactor = Integer.valueOf(argv[2]);
		}

		// read input
		image = CAAMUtil.ReadExample(asf_file, image, shape, rfactor);

		double[] p255 = new double[1];
		// ModelSimpleImage.SPAWN( p255, 255);
		p255[0] = 255;

		// plot delaunay mesh
		if (drawDelaunay || drawDelaunayClean) {
			CAAMMesh mesh = new CAAMMesh();
			CAAMDelaunay.MakeMesh(shape, mesh, drawDelaunayClean);
			CAAMUtil.PlotMeshIntoImage(image, mesh, p255, p255);
		}

		// plot shape
		CAAMUtil.PlotShapeIntoImage(image, shape, p255, p255, drawNormals,
				drawInterior, drawPoints, drawLines);

		// write the result to disk
		outFile = CAAMUtil.RemoveExt(asf_file) + new String(".ann") + ".bmp";
		// image.WriteBandedFile( outFile, "bmp" );
		String dir = System.getProperties().getProperty("user.dir");
		ModelImage outImage = new ModelImage(image, outFile);
		outImage.saveImage(dir, outFile, FileUtility.BMP, false);

		System.err.println("Annotation rendered and written into '" + outFile
				+ "'.");

		return 0;
	}

}