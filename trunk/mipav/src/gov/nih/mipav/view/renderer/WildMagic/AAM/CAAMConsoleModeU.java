package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.IOException;
import java.util.*;
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
 * Console mode U for utilities functions.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeU extends CAAMConsoleMode {

	/**
	 * Constructor.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeU(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 1;
		m_NMaxParam = 50;

		m_Name = new String("u");
		m_Usage = new String("<utility function> arguments ...");
		m_Desc = new String("Utility functions");
		m_LongDesc = new String("\nPrint area utility\n\nUsage:\n\n     "
				+ m_ProgName + " u printarea\n");
	}

	/**
	 * C style anchor to invoke the U console mode. Being called from the AAM
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
		String utility = argv[0];
		// CString ext = argc>=2 ? argv[1] : "bmp";

		if (utility.equals("printarea")) {

			// scan the current dir for all annotations
			Vector<String> annotations = new Vector<String>();

			annotations = (Vector<String>) CAAMUtil.ScanSortDir(".\\", "asf");

			CAAMShapeCollection collection = new CAAMShapeCollection();

			// calculate and print area for all annotations
			// in the current dir
			for (int i = 0; i < annotations.size(); i++) {

				// read shape
				CAAMShape shape = new CAAMShape();
				shape.ReadASF(annotations.get(i));
				collection.Insert(shape);
			}
			collection.Rel2Abs(); // convert to absolute coordinates
			System.err.println("Annotation name \tArea");
			for (int i = 0; i < annotations.size(); i++) {

				// print name and area
				System.err.println(annotations.get(i) + "\t"
						+ collection.get(i).Area());

				// print the area of each path (if more than one)
				Vector<Integer> paths = collection.get(i).GetPaths();
				for (int p = 0; p < paths.size() && paths.size() > 1; p++) {

					CAAMShape s = collection.get(i).ExtractPath(paths.get(p));
					System.err.printf("\t" + s.Area());
				}
				System.err.println();
			}

			// ended with sucess -> return
			return 0;
		}

		if (utility == "comicstrip") {

			// scan the current dir for all bmp files
			Vector<String> images = new Vector<String>();

			images = (Vector<String>) CAAMUtil.ScanSortDir(".\\", "bmp");

			final int n = images.size();
			final int nCols = Integer.valueOf(argv[1]);
			final int nRows = (int) Math.ceil((double) n / nCols);
			int w, h;
			{
				FileIO io = new FileIO();
				ModelImage image = io.readImage(images.get(0));
				int[] extents = image.getExtents();
				w = extents[0];
				h = extents[1];
			}

			// CVisImage outImage = new CVisImage( nCols*w, nRows*h, 1,
			// CVisImage.evisimoptDontAlignMemory );
			int[] ext = new int[2];
			ext[0] = nCols * w;
			ext[1] = nRows * h;
			ModelImage outImage = new ModelImage(FileUtility.BMP, ext,
					"outImage");

			// outImage.FillPixels( 255 );
			int size = ext[0] * ext[1];
			for (int i = 0; i < size; i++) {
				outImage.set(i, 255);
			}

			System.err
					.println("Creating " + nRows + "x" + nCols
							+ " image matrix (" + ext[0] + "x" + ext[1]
							+ " pixels)...");

			int i = 0;
			try {
				for (int r = 0; r < nRows; r++) {

					for (int c = 0; c < nCols; c++) {

						if (i >= images.size())
							break;

						System.err.println("reading image #" + i + ": "
								+ images.get(i));

						FileIO fio = new FileIO();
						ModelImage image = fio.readImage(images.get(i));
						float[] data = new float[w * h];
						image.exportData(0, w * h, data);
						outImage.importData((c * w) * (r * h), data, false);

						++i;
					}

				}
			} catch (IOException e) {
				e.printStackTrace();
			}

			// outImage.WriteFile( "comicstrip.bmp" );
			String filename = "comicstrip.bmp";
			String dir = System.getProperties().getProperty("user.dir");
			outImage.saveImage(dir, filename, FileUtility.BMP, false);
			System.err.println("Result is written as 'comicstrip.bmp'.");

			// ended with sucess -> return
			return 0;
		}

		// unknown utility
		System.err.printf("The utility '" + utility + "' is unknown.");
		return -1;
	}

}