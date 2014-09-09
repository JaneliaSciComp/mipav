package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;

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
 * Console mode T for tracking movie. Currently, disabled.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMConsoleModeT extends CAAMConsoleMode {

	/**
	 * Constructor.
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeT(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 2;
		m_NMaxParam = 2;

		m_Name = new String("t");
		m_Usage = new String("<model.amf> <input movie>");
		m_Desc = new String("Performs tracking in a movie file (.avi).");
		m_LongDesc = new String(
				"This mode performs a through initialisation of the AAM in the first frame and\n"
						+ "uses the convergened position as initial pose in the next frame (and so on...).\n\n"
						+ "Example usage:\n\n     "
						+ m_ProgName
						+ " t mymodel.amf hand.avi\n");
	}

	public int Main(int argc, String[] argv) {

		// call base implementation
		super.Main(argc, argv);

		// setup input parameters
		String inModel = argv[0];
		String inMovie = argv[1];

		C_AAMMODEL aam = new C_AAMMODEL();

		// read model
		boolean ok = aam.ReadModel(inModel);

		if (!ok) {

			System.err.printf("Could not read model file '%s'. Exiting...\n",
					inModel);
			System.exit(1);
		}

		// read movie
		// CAAMMovieAVI avi = new CAAMMovieAVI();
		// CAAMMovieAVI aviOut = new CAAMMovieAVI();

		CDVector c = new CDVector();
		CAAMShape shape = new CAAMShape();

		shape.assign(aam.ReferenceShape());

		CAAMInitializeStegmann Stegmann = new CAAMInitializeStegmann(aam);

		try {

			FileIO io = new FileIO();
			ModelImage avi_img = io.readImage(inMovie);

			// open the putput movie
			ModelImage aviOut_img = io.readImage(CAAMUtil.RemoveExt(inMovie)
					+ ".track.avi");

			// do tracking
			// FILE *fh = fopen("tracking_results.txt", "wt");
			PrintWriter fh = new PrintWriter("tracking_results.txt");

			int[] extents = avi_img.getExtents();
			int w = extents[0];
			int h = extents[1];
			int zDim = extents[2];
			/*
			 * for(int i=0;i<zDim;i++) {
			 * 
			 * // CVisImageRGB image = new CVisImageRGB(); ModelImage image = //
			 * int ok=avi.ReadFrame( image, i ); if (!ok) break; // int w =
			 * image.Width(); // int h = image.Height();
			 * 
			 * // convert to multiband // ModelSimpleImage gray = new
			 * ModelSimpleImage( w, h, AAMdef.BANDS,
			 * CVisImage.evisimoptDontAlignMemory ); int[] ext = new int[2];
			 * ext[0] = w; ext[1] = h; ModelImage gray = new
			 * ModelImage(ModelStorageBase.FLOAT, ext, "_tempImage"); for(int
			 * y=0;y<h;y++) { for(int x=0;x<w;x++) { double[] pgray = new
			 * double[AAMdef.BANDS]; gray.FromRGB( image.Pixel(x,y), pgray);
			 * gray.set_pixel(x,y,pgray); } }
			 * 
			 * if (i==0) {
			 * 
			 * // it's the first frame -> initialize Stegmann.Initialize( gray,
			 * shape, c );
			 * 
			 * // write initial model points { // make a copy of the multiband
			 * gray image ModelSimpleImage grayCopy = new ModelSimpleImage( w,
			 * h, AAMdef.BANDS, CVisImage.evisimoptDontAlignMemory );
			 * gray.CopyPixelsTo( grayCopy );
			 * 
			 * // plot into gray image double[] p255 = new double[AAMdef.BANDS];
			 * ModelSimpleImage.SPAWN( p255, 255); CAAMUtil.PlotShapeIntoImage(
			 * grayCopy, shape, p255, p255, false, false, false );
			 * 
			 * // convert to rgba CVisImageRGB rgba = new CVisImageRGB( w, h, 1,
			 * CVisImage.evisimoptDontAlignMemory ); grayCopy.CopyPixelsToRGBA(
			 * rgba );
			 * 
			 * // write frame aviOut.WriteFrame( rgba );
			 * 
			 * // write still grayCopy.WriteBandedFile("init.bmp"); } }
			 * 
			 * 
			 * // debug output System.err.printf("Tracking frame %3i... ", i );
			 * 
			 * // do the optimization CAAMOptRes res = new CAAMOptRes(); res =
			 * aam.OptimizeModel( gray, shape, c );
			 * 
			 * // debug output double[] cog_x = new double[1]; double[] cog_y =
			 * new double[1]; shape.COG( cog_x, cog_y ); System.err.printf(
			 * "done, used %2i iterations, E=%9.2e, COG=(%3.0f, %3.0f)\n",
			 * res.NIter(), res.SimilarityMeasure(), cog_x[0], cog_y[0] );
			 * 
			 * // dump output String asf = new String(); asf.format(
			 * "%s.%04i.asf", CAAMUtil.RemoveExt(CAAMUtil.GetFilename( inMovie
			 * )), i ); shape.WriteASF( asf, gray.Width(), gray.Height() );
			 * 
			 * fh.printf("%i\t%i\t%f\t%f\t%f\t%f\n", i, res.NIter(),
			 * res.SimilarityMeasure(), cog_x, cog_y, res.Mahalanobis() );
			 * fh.flush();
			 * 
			 * 
			 * // write optimized model points { // plot into gray image
			 * double[] p255 = new double[AAMdef.BANDS]; ModelSimpleImage.SPAWN(
			 * p255, 255); CAAMUtil.PlotShapeIntoImage( gray, shape, p255, p255,
			 * false, false, false );
			 * 
			 * // convert to rgba CVisImageRGB rgba = new CVisImageRGB( w, h, 1,
			 * CVisImage.evisimoptDontAlignMemory ); gray.CopyPixelsToRGBA( rgba
			 * );
			 * 
			 * // write frame aviOut.WriteFrame( rgba );
			 * 
			 * // write still //CString str; str.Format("track.%04i.bmp", i );
			 * //gray.WriteBandedFile(str); } }
			 */
			// close movies and files
			// avi.Close();
			// aviOut.Close();
			// fclose(fh);
			fh.close();
		} catch (IOException e) {
			e.printStackTrace();
			// AfxMessageBox( e.FullMessage() );
		}

		// we're done
		return 0;
	}

}