package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
import java.util.*;
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
 * Simple lo-fi property reader. Used as naive parser/scanner for the .asf, acf
 * files.
 * 
 * @author Ruida Cheng
 */
public class CAAMPropsReader extends CAAMObject {

	/** File reader */
	public FileReader reader;

	/** Java Scanner API. */
	public Scanner in;

	public final char CR;
	public final char LF;
	public final char COMMENT_CHAR;
	private String previousLine;

	/**
	 * Constructor. Opens the file.
	 * 
	 * @param filename
	 *            The file to open.
	 */
	public CAAMPropsReader(final String filename) {
		CR = 0x0a;
		LF = 0x0d;
		COMMENT_CHAR = '#';

		try {
			reader = new FileReader(filename);
			in = new Scanner(reader);
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	/**
	 * Process point info from the in stream line
	 * 
	 * @param path_id
	 *            path id
	 * @param type
	 *            point type
	 * @param x_rel
	 *            x coordinate
	 * @param y_rel
	 *            y coordinate
	 * @param point_nb
	 *            point number
	 * @param from
	 *            from index
	 * @param to
	 *            to index
	 */
	public void processCoordinatePoints(int[] path_id, int[] type,
			float[] x_rel, float[] y_rel, int[] point_nb, int[] from, int[] to) {
		if (in.hasNextLine()) {
			String line;
			if (previousLine != null) {
				line = previousLine;
			} else {
				line = in.nextLine();
			}
			// System.err.println(line);
			StringTokenizer token = new StringTokenizer(line);

			path_id[0] = Integer.valueOf(token.nextToken());
			type[0] = Integer.valueOf(token.nextToken());
			x_rel[0] = Float.valueOf(token.nextToken());
			y_rel[0] = Float.valueOf(token.nextToken());
			point_nb[0] = Integer.valueOf(token.nextToken());
			from[0] = Integer.valueOf(token.nextToken());
			to[0] = Integer.valueOf(token.nextToken());
			// System.err.println("test");
			previousLine = null;
		}
		// System.err.println("test process points");
	}

	/**
	 * Process image file.
	 * 
	 * @param fileName
	 *            image file name
	 */
	public void processImageName(String[] fileName) {
		if (previousLine != null) {
			fileName[0] = previousLine;
		}
		if (in.hasNextLine()) {
			fileName[0] = in.nextLine();
		}
	}

	/**
	 * Sync to read number.
	 * 
	 * @return -1 fail, otherwise, return int number.
	 */
	public int Sync() {
		int value;
		while (in.hasNextLine()) {
			String line = in.nextLine();
			if (!line.startsWith("#") && !(line.length() == 0)) {
				StringTokenizer token = new StringTokenizer(line);
				value = Integer.valueOf(token.nextToken());
				return value;
			}
		}
		return -1;

	}

	/**
	 * Skip comment line or empty line
	 */
	public void SkipLines() {
		
		while (in.hasNextLine()) {

			previousLine = in.nextLine();
			if (!previousLine.startsWith("#") && !(previousLine.length() == 0)) {
				break;
			}

		}

	}

	/**
	 * Returns true if the file is valid.
	 * 
	 * @return if in stream is valid
	 */
	public boolean IsValid() {
		return in != null;
	}

	/**
	 * dispose memory
	 */
	public void dispose() {

		if (in != null) {
			in.close();
			in = null;
		}

		if (reader != null) {
			try {
				reader.close();
				reader = null;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Expands an image to have dynamic size.
	 * 
	 * @doc The method expands an image by using zero-padding so that the
	 *      resulting image ´has width and height that are powers of two.
	 * @param img
	 *            Input image.
	 * @param out
	 *            Output dyadic image.
	 */
	public void ExpandImg2DyadicSize(final ModelSimpleImage img,
			ModelSimpleImage out) {

		// calc output size
		int new_w, new_h;
		new_w = (int) Math.pow(2,
				Math.ceil(Math.log(img.Width()) / Math.log(2)));
		new_h = (int) Math.pow(2,
				Math.ceil(Math.log(img.Height()) / Math.log(2)));
		int[] extents = new int[2];
		extents[0] = new_w;
		extents[1] = new_h;
		out = new ModelSimpleImage(extents);

		// copy pixels
		// TAAMPixel[] pixel = new TAAMPixel[AAMdef.BANDS];
		float pixel;
		for (int y = 0; y < img.Height(); y++) {

			for (int x = 0; x < img.Width(); x++) {

				// ((ModelSimpleImage<TAAMPixel>)img).get_pixel( x, y, pixel );
				pixel = img.getValue(x, y);
				// out.set_pixel( x, y, pixel);
				out.setValue(x, y, pixel);
			}
		}
	}

	/**
	 * Mirrors the edge of an arbitrary shape mask in a matrix using poor-mans
	 * mirroring.
	 * 
	 * @param img
	 *            Input matrix. Overwritten by mirrored version.
	 * @param mask
	 *            Image mask defining the shape.
	 * @param edgeWidth
	 *            The width of the edge.
	 */
	public void MirrorEdge(CDMatrix m, final ModelSimpleImage mask,
			int edgeWidth) {

		int width = m.NCols();
		int height = m.NRows();
		int x, y;

		// left horisontal edge
		for (y = 0; y < height; y++) {

			x = 0;
			while (x < width && mask.getValue(x, y) == 0)
				x++;
			if (x == width)
				continue;

			for (int i = 1; i <= edgeWidth; i++) {

				if (x - i < 0)
					break;
				if (x + i >= width)
					break;

				m.m_data[y][x - i] = m.m_data[y][x + i];
			}
		}

		// right horisontal edge
		for (y = 0; y < height; y++) {

			x = width - 1;
			while (x > 0 && mask.getValue(x, y) == 0)
				x--;
			if (x == 0)
				continue;

			for (int i = 1; i <= edgeWidth; i++) {

				if (x - i < 0)
					break;
				if (x + i >= width)
					break;

				m.m_data[y][x + i] = m.m_data[y][x - i];
			}
		}

		// left vertical edge
		for (x = 0; x < width; x++) {

			y = 0;
			while (y < height && mask.getValue(x, y) == 0)
				y++;
			if (y == height)
				continue;

			for (int i = 1; i <= edgeWidth; i++) {

				if (y - i < 0)
					break;
				if (y + i >= height)
					break;

				m.m_data[y - i][x] = m.m_data[y + i][x];
			}
		}

		// right vertical edge
		for (x = 0; x < width; x++) {

			y = height - 1;
			while (y > 0 && mask.getValue(x, y) == 0)
				y--;
			if (x == 0)
				continue;

			for (int i = 1; i <= edgeWidth; i++) {

				if (y - i < 0)
					break;
				if (y + i >= height)
					break;

				m.m_data[y + i][x] = m.m_data[y - i][x];
			}
		}
	}

	/**
	 * Mirrors the edge of an arbitrary shape mask in an image using poor-mans
	 * mirroring.
	 * 
	 * @param img
	 *            Input image. Overwritten by mirrored version.
	 * @param mask
	 *            Image mask defining the shape.
	 * @param edgeWidth
	 *            The width of the edge.
	 */
	public void MirrorEdge(ModelSimpleImage img, final ModelSimpleImage mask,
			int edgeWidth) {

		int width = img.Width();
		int height = img.Height();
		// TAAMPixel[] pixel = new TAAMPixel[AAMdef.BANDS];
		float pixel;
		int x, y;

		// left horisontal edge
		for (y = 0; y < height; y++) {

			x = 0;
			while (x < width && mask.getValue(x, y) == 0)
				x++;
			if (x == width)
				continue;

			for (int i = 1; i <= edgeWidth; i++) {

				if (x - i < 0)
					break;
				if (x + i >= width)
					break;

				pixel = img.getValue(x + i, y);
				img.setValue(x - i, y, pixel);
			}
		}

		// right horisontal edge
		for (y = 0; y < height; y++) {

			x = width - 1;
			while (x > 0 && mask.getValue(x, y) == 0)
				x--;
			if (x == 0)
				continue;

			for (int i = 1; i <= edgeWidth; i++) {

				if (x - i < 0)
					break;
				if (x + i >= width)
					break;

				pixel = img.getValue(x - i, y);
				img.setValue(x + i, y, pixel);
			}
		}

		// left vertical edge
		for (x = 0; x < width; x++) {

			y = 0;
			while (y < height && mask.getValue(x, y) == 0)
				y++;
			if (y == height)
				continue;

			for (int i = 1; i <= edgeWidth; i++) {

				if (y - i < 0)
					break;
				if (y + i >= height)
					break;

				pixel = img.getValue(x, y + i);
				img.setValue(x, y - i, pixel);
			}
		}

		// right vertical edge
		for (x = 0; x < width; x++) {

			y = height - 1;
			while (y > 0 && mask.getValue(x, y) == 0)
				y--;
			if (x == 0)
				continue;

			for (int i = 1; i <= edgeWidth; i++) {

				if (y - i < 0)
					break;
				if (y + i >= height)
					break;

				pixel = img.getValue(x, y - i);
				img.setValue(x, y + i, pixel);
			}
		}
	}

	/**
	 * Tests if a shape is fully inside an image.
	 * 
	 * @param s
	 *            Input shape.
	 * @param img
	 *            Input image.
	 * @return True if the shape is fully inside the image.
	 */
	public boolean ShapeInsideImage(final CAAMShape s,
			final ModelSimpleImage img) {

		boolean outside = s.MinX() < 0 || s.MaxX() > img.Width() - 1
				|| s.MinY() < 0 || s.MaxY() > img.Height() - 1;

		return !outside;
	}

	/**
	 * Calculates optimization results.
	 * 
	 * @param optimized
	 *            Model shape.
	 * @param groundTruth
	 *            Ground truth shape.
	 * @param ptpt
	 *            Average point to point landmark error.
	 * @param ptcrv
	 *            Average point to curve landmark error. NOTICE: This is not a
	 *            symmetric measure!
	 */
	public void CalcShapeDistances(final CAAMShape optimized,
			final CAAMShape groundTruth, double[] ptpt, double[] ptcrv,
			CDVector pvDists) {

		ptpt[0] = CAAMUtil.DistEuclidianPoints(optimized, groundTruth);
		ptcrv[0] = CAAMUtil.DistEuclidianAssBorder(optimized, groundTruth,
				pvDists);
	}

	/**
	 * Converts seconds to a MM:SS string.
	 * 
	 * @param secs
	 *            Time in seconds.
	 * @return The time in MM:SS.
	 */
	public String Secs2Mins(double secs) {

		String ret = new String();
		ret = (int) (secs / 60) + ":" + ((int) (.5 + secs) % 60);
		return ret;
	}

	/**
	 * Finds a file name that is not 'occupied'. This method finds a file name
	 * that is not 'occupied' by adding at number to the base part of the
	 * suggested file name.
	 * 
	 * @param filename_suggestion
	 *            Suggestion including extension.
	 * @return An unused filename resembling the suggstion.
	 */
	public String FindVacantFilename(final String filename_suggestion) {
		File file = new File(filename_suggestion);
		if (!file.exists()) {
			return filename_suggestion;
		}

		String base = CAAMUtil.RemoveExt(filename_suggestion);
		String ext = CAAMUtil.GetExt(filename_suggestion);
		// String fn;

		for (int i = 0; i < 1000; i++) {

			String fn = new String(base + i + "." + ext); // format("%s%03i.%s",
															// base, i, ext );
			boolean exists = CAAMUtil.FileExists(fn);

			if (!exists) {

				return fn;
			}
		}

		return "";
	}

	/**
	 * Samples a set of texture vectors given a set of shape in absolute (i.e.
	 * image) coordinates.
	 * 
	 * @param unalignedShapes
	 *            Shapes in absolute coordinates.
	 * @param vTextures
	 *            The set of textures.
	 * @param outputRF
	 *            The output reference frame generated for sampling the
	 *            textures.
	 * @param removeMean
	 *            If true the mean from each texture vector (i.e. the DC) is
	 *            removed.
	 * @param useTSP
	 *            Use tangent space projection to align the shapes.
	 * @param useConvexHull
	 *            If true the convex hull is used to determine the extent of a
	 *            shape.
	 */
	public void SampleTextures(final CAAMShapeCollection unalignedShapes,
			Vector<CDVector> vTextures, CAAMReferenceFrame outputRF,
			final int imageReduction, final boolean removeMean,
			final boolean useTSP, final boolean useConvexHull) {

		// /////////////////////////////////////
		// align shapes
		// /////////////////////////////////////

		// copy the unaligned shapes
		CAAMShapeCollection alignedShapes = new CAAMShapeCollection();
		alignedShapes.assign(unalignedShapes);

		// align shape with respect to position, scale and rotation
		alignedShapes.AlignShapes(useTSP);

		// calculate the cached reference shape of the unaligned shapes
		CAAMShape refShape = new CAAMShape();
		alignedShapes.ReferenceShape(refShape);

		// /////////////////////////////////////
		// make reference frame and analyzer
		// /////////////////////////////////////
		outputRF.Setup(refShape, useConvexHull);
		CAAMAnalyzeSynthesizeSoftware as = new CAAMAnalyzeSynthesizeSoftware(
				outputRF);

		// /////////////////////////////////////
		// sample texture vectors
		// /////////////////////////////////////
		vTextures.clear();
		for (int i = 0; i < unalignedShapes.NShapes(); i++) {

			// get the shape image
			ModelSimpleImage image = new ModelSimpleImage();
			image = unalignedShapes.get(i).GetHostImage(image,
					unalignedShapes.Path(), imageReduction);

			// sample the texture of the i-th shape into 'vTexture'
			CDVector tex = new CDVector();
			as.SetAnalyzeImage(image);
			as.Analyze(unalignedShapes.get(i), tex, true);

			if (removeMean) {

				// remove mean
				double mean = tex.Mean();
				for (i = 0; i < tex.Length(); i++) {

					tex.set(i, tex.get(i) - mean);
				}
			}

			// store texture
			vTextures.add(tex);
		}
	}

	/**
	 * Writes a movie file containing all shapes from a directory warped to
	 * their mean shape.
	 * 
	 * @param filename
	 *            Output movie filename.
	 * @param asfPath
	 *            Path to annotation files.
	 * @param useConvexHull
	 *            If true the convex hull is used to determine the extent of a
	 *            shape.
	 * @param writeRefShape
	 *            If true the reference shape corresponding to the movie file is
	 *            written as "regshape.asf".
	 */
	public void RegistrationMovie(final String filename, final String asfPath,
			final boolean useConvexHull, final boolean writeRefShape) {

		Vector<String> asfFiles = new Vector<String>();
		CAAMShapeCollection shapes = new CAAMShapeCollection();
		asfFiles = (Vector<String>) CAAMUtil.ScanSortDir(asfPath, "asf");
		shapes.ReadShapes(asfFiles, true);
		shapes.Rel2Abs(1);

		CAAMReferenceFrame rf = new CAAMReferenceFrame();
		Vector<CDVector> vTextures = new Vector<CDVector>();
		SampleTextures(shapes, vTextures, rf, 1, false, true, useConvexHull);

		CAAMUtil.RegistrationMovie(filename, vTextures, rf);

		if (writeRefShape) {

			CAAMShape shape = new CAAMShape(rf.RefShape());
			shape.SetHostImage(filename);
			shape.WriteASF("regshape.asf", rf.RefImageWidth(),
					rf.RefImageHeight());
		}
	}

	/**
	 * Calculates the overlap between two shapes as specified in
	 * "Active Shape Model Segmentation With Optimal Features" Bram van Ginneken
	 * et al., IEEE TMI 21(8) Aug. 2002.
	 * 
	 * Notice that this only makes sense for one-path closed shapes.
	 * 
	 * @param model
	 *            Model shape
	 * @param gt
	 *            Ground truth shape
	 * @return The shape overlap (1 = perfect match, 0 = no overlap).
	 */
	public double ShapeOverlap(final CAAMShape model, final CAAMShape gt) {

		double overlap, TP, FP, FN;

		double minX = Math.min(model.MinX(), gt.MinX());
		double maxX = Math.max(model.MaxX(), gt.MaxX());
		double minY = Math.min(model.MinY(), gt.MinY());
		double maxY = Math.max(model.MaxY(), gt.MaxY());

		// calculate overlap in a one-unit spaced grid
		TP = .0;
		FP = .0;
		FN = .0;
		for (double x = minX; x < maxX; x = x + 1.) {

			for (double y = minY; y < maxY; y = y + 1.) {

				boolean insideModel = model.IsInside(new CAAMPoint(x, y));
				boolean insideGT = gt.IsInside(new CAAMPoint(x, y));

				if (insideModel || insideGT) {

					TP = (insideGT && insideModel) ? TP + 1. : TP;
					FP = (insideModel && !insideGT) ? FP + 1. : FP;
					FN = (!insideModel && insideGT) ? FN + 1. : FN;
				}

			}
		}

		overlap = TP / (TP + FP + FN);

		return overlap;
	}

}