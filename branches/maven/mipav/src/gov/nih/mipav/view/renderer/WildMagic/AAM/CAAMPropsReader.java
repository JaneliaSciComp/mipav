package gov.nih.mipav.view.renderer.WildMagic.AAM;


import gov.nih.mipav.model.structures.ModelSimpleImage;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;
import java.util.StringTokenizer;
import java.util.Vector;


/**
 * This is the Java modified version of C++ active appearance model API (AAM_API). It is modified with a subset of
 * required functions for automatic MRI prostate segmentation.
 * 
 * AAM-API LICENSE - file: license.txt
 * 
 * This software is freely available for non-commercial use such as research and education. Please see the full
 * disclaimer below.
 * 
 * All publications describing work using this software should cite the reference given below.
 * 
 * Copyright (c) 2000-2003 Mikkel B. Stegmann, mbs@imm.dtu.dk
 * 
 * 
 * IMM, Informatics & Mathematical Modelling DTU, Technical University of Denmark Richard Petersens Plads, Building 321
 * DK-2800 Lyngby, Denmark
 * 
 * http://www.imm.dtu.dk/~aam/
 * 
 * 
 * 
 * REFERENCES
 * 
 * Please use the reference below, when writing articles, reports etc. where the AAM-API has been used. A draft version
 * the article is available from the homepage.
 * 
 * I will be happy to receive pre- or reprints of such articles.
 * 
 * /Mikkel
 * 
 * 
 * ------------- M. B. Stegmann, B. K. Ersboll, R. Larsen, "FAME -- A Flexible Appearance Modelling Environment", IEEE
 * Transactions on Medical Imaging, IEEE, 2003 (to appear) -------------
 * 
 * 
 * 
 * 3RD PART SOFTWARE
 * 
 * The software is partly based on the following libraries:
 * 
 * - The Microsoft(tm) Vision Software Developers Kit, VisSDK - LAPACK
 * 
 * 
 * DISCLAIMER
 * 
 * This software is provided 'as-is', without any express or implied warranty. In no event will the author be held
 * liable for any damages arising from the use of this software.
 * 
 * Permission is granted to anyone to use this software for any non-commercial purpose, and to alter it, subject to the
 * following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software.
 * 
 * 2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original
 * software.
 * 
 * 3. This notice may not be removed or altered from any source distribution.
 * 
 * --
 * 
 * No guarantees of performance accompany this software, nor is any responsibility assumed on the part of the author or
 * IMM.
 * 
 * This software is provided by Mikkel B. Stegmann and IMM ``as is'' and any express or implied warranties, including,
 * but not limited to, the implied warranties of merchantability and fitness for a particular purpose are disclaimed. In
 * no event shall IMM or Mikkel B. Stegmann be liable for any direct, indirect, incidental, special, exemplary, or
 * consequential damages (including, but not limited to, procurement of substitute goods or services; loss of use, data,
 * or profits; or business interruption) however caused and on any theory of liability, whether in contract, strict
 * liability, or tort (including negligence or otherwise) arising in any way out of the use of this software, even if
 * advised of the possibility of such damage.
 * 
 * 
 * 
 * 
 * $Revision: 1.4 $ $Date: 2003/04/23 14:49:15 $
 * 
 * Simple lo-fi property reader. Used as naive parser/scanner for the .asf, acf files.
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
     * @param filename The file to open.
     */
    public CAAMPropsReader(final String filename) {
        CR = 0x0a;
        LF = 0x0d;
        COMMENT_CHAR = '#';

        try {
            reader = new FileReader(filename);
            in = new Scanner(reader);
        } catch (final IOException e) {
            e.printStackTrace();
        }

    }

    /**
     * Process point info from the in stream line
     * 
     * @param path_id path id
     * @param type point type
     * @param x_rel x coordinate
     * @param y_rel y coordinate
     * @param point_nb point number
     * @param from from index
     * @param to to index
     */
    public void processCoordinatePoints(final int[] path_id, final int[] type, final float[] x_rel, final float[] y_rel, final int[] point_nb,
            final int[] from, final int[] to) {
        if (in.hasNextLine()) {
            String line;
            if (previousLine != null) {
                line = previousLine;
            } else {
                line = in.nextLine();
            }
            // System.err.println(line);
            final StringTokenizer token = new StringTokenizer(line);

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
     * @param fileName image file name
     */
    public void processImageName(final String[] fileName) {
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
            final String line = in.nextLine();
            if ( !line.startsWith("#") && ! (line.length() == 0)) {
                final StringTokenizer token = new StringTokenizer(line);
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
            if ( !previousLine.startsWith("#") && ! (previousLine.length() == 0)) {
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
            } catch (final IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Expands an image to have dynamic size.
     * 
     * @doc The method expands an image by using zero-padding so that the resulting image has width and height that are
     *      powers of two.
     * @param img Input image.
     * @param out Output dyadic image.
     */
    public void ExpandImg2DyadicSize(final ModelSimpleImage img, ModelSimpleImage out) {

        // calc output size
        int new_w, new_h;
        new_w = (int) Math.pow(2, Math.ceil(Math.log(img.Width()) / Math.log(2)));
        new_h = (int) Math.pow(2, Math.ceil(Math.log(img.Height()) / Math.log(2)));
        final int[] extents = new int[2];
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
     * Mirrors the edge of an arbitrary shape mask in a matrix using poor-mans mirroring.
     * 
     * @param img Input matrix. Overwritten by mirrored version.
     * @param mask Image mask defining the shape.
     * @param edgeWidth The width of the edge.
     */
    public void MirrorEdge(final CDMatrix m, final ModelSimpleImage mask, final int edgeWidth) {

        final int width = m.NCols();
        final int height = m.NRows();
        int x, y;

        // left horisontal edge
        for (y = 0; y < height; y++) {

            x = 0;
            while (x < width && mask.getValue(x, y) == 0) {
                x++;
            }
            if (x == width) {
                continue;
            }

            for (int i = 1; i <= edgeWidth; i++) {

                if (x - i < 0) {
                    break;
                }
                if (x + i >= width) {
                    break;
                }

                m.m_data[y][x - i] = m.m_data[y][x + i];
            }
        }

        // right horisontal edge
        for (y = 0; y < height; y++) {

            x = width - 1;
            while (x > 0 && mask.getValue(x, y) == 0) {
                x--;
            }
            if (x == 0) {
                continue;
            }

            for (int i = 1; i <= edgeWidth; i++) {

                if (x - i < 0) {
                    break;
                }
                if (x + i >= width) {
                    break;
                }

                m.m_data[y][x + i] = m.m_data[y][x - i];
            }
        }

        // left vertical edge
        for (x = 0; x < width; x++) {

            y = 0;
            while (y < height && mask.getValue(x, y) == 0) {
                y++;
            }
            if (y == height) {
                continue;
            }

            for (int i = 1; i <= edgeWidth; i++) {

                if (y - i < 0) {
                    break;
                }
                if (y + i >= height) {
                    break;
                }

                m.m_data[y - i][x] = m.m_data[y + i][x];
            }
        }

        // right vertical edge
        for (x = 0; x < width; x++) {

            y = height - 1;
            while (y > 0 && mask.getValue(x, y) == 0) {
                y--;
            }
            if (x == 0) {
                continue;
            }

            for (int i = 1; i <= edgeWidth; i++) {

                if (y - i < 0) {
                    break;
                }
                if (y + i >= height) {
                    break;
                }

                m.m_data[y + i][x] = m.m_data[y - i][x];
            }
        }
    }

    /**
     * Mirrors the edge of an arbitrary shape mask in an image using poor-mans mirroring.
     * 
     * @param img Input image. Overwritten by mirrored version.
     * @param mask Image mask defining the shape.
     * @param edgeWidth The width of the edge.
     */
    public void MirrorEdge(final ModelSimpleImage img, final ModelSimpleImage mask, final int edgeWidth) {

        final int width = img.Width();
        final int height = img.Height();
        // TAAMPixel[] pixel = new TAAMPixel[AAMdef.BANDS];
        float pixel;
        int x, y;

        // left horisontal edge
        for (y = 0; y < height; y++) {

            x = 0;
            while (x < width && mask.getValue(x, y) == 0) {
                x++;
            }
            if (x == width) {
                continue;
            }

            for (int i = 1; i <= edgeWidth; i++) {

                if (x - i < 0) {
                    break;
                }
                if (x + i >= width) {
                    break;
                }

                pixel = img.getValue(x + i, y);
                img.setValue(x - i, y, pixel);
            }
        }

        // right horisontal edge
        for (y = 0; y < height; y++) {

            x = width - 1;
            while (x > 0 && mask.getValue(x, y) == 0) {
                x--;
            }
            if (x == 0) {
                continue;
            }

            for (int i = 1; i <= edgeWidth; i++) {

                if (x - i < 0) {
                    break;
                }
                if (x + i >= width) {
                    break;
                }

                pixel = img.getValue(x - i, y);
                img.setValue(x + i, y, pixel);
            }
        }

        // left vertical edge
        for (x = 0; x < width; x++) {

            y = 0;
            while (y < height && mask.getValue(x, y) == 0) {
                y++;
            }
            if (y == height) {
                continue;
            }

            for (int i = 1; i <= edgeWidth; i++) {

                if (y - i < 0) {
                    break;
                }
                if (y + i >= height) {
                    break;
                }

                pixel = img.getValue(x, y + i);
                img.setValue(x, y - i, pixel);
            }
        }

        // right vertical edge
        for (x = 0; x < width; x++) {

            y = height - 1;
            while (y > 0 && mask.getValue(x, y) == 0) {
                y--;
            }
            if (x == 0) {
                continue;
            }

            for (int i = 1; i <= edgeWidth; i++) {

                if (y - i < 0) {
                    break;
                }
                if (y + i >= height) {
                    break;
                }

                pixel = img.getValue(x, y - i);
                img.setValue(x, y + i, pixel);
            }
        }
    }

    /**
     * Tests if a shape is fully inside an image.
     * 
     * @param s Input shape.
     * @param img Input image.
     * @return True if the shape is fully inside the image.
     */
    public boolean ShapeInsideImage(final CAAMShape s, final ModelSimpleImage img) {

        final boolean outside = s.MinX() < 0 || s.MaxX() > img.Width() - 1 || s.MinY() < 0 || s.MaxY() > img.Height() - 1;

        return !outside;
    }

    /**
     * Calculates optimization results.
     * 
     * @param optimized Model shape.
     * @param groundTruth Ground truth shape.
     * @param ptpt Average point to point landmark error.
     * @param ptcrv Average point to curve landmark error. NOTICE: This is not a symmetric measure!
     */
    public void CalcShapeDistances(final CAAMShape optimized, final CAAMShape groundTruth, final double[] ptpt, final double[] ptcrv, final CDVector pvDists) {

        ptpt[0] = CAAMUtil.DistEuclidianPoints(optimized, groundTruth);
        ptcrv[0] = CAAMUtil.DistEuclidianAssBorder(optimized, groundTruth, pvDists);
    }

    /**
     * Converts seconds to a MM:SS string.
     * 
     * @param secs Time in seconds.
     * @return The time in MM:SS.
     */
    public String Secs2Mins(final double secs) {

        String ret = new String();
        ret = (int) (secs / 60) + ":" + ((int) (.5 + secs) % 60);
        return ret;
    }

    /**
     * Finds a file name that is not 'occupied'. This method finds a file name that is not 'occupied' by adding at
     * number to the base part of the suggested file name.
     * 
     * @param filename_suggestion Suggestion including extension.
     * @return An unused filename resembling the suggstion.
     */
    public String FindVacantFilename(final String filename_suggestion) {
        final File file = new File(filename_suggestion);
        if ( !file.exists()) {
            return filename_suggestion;
        }

        final String base = CAAMUtil.RemoveExt(filename_suggestion);
        final String ext = CAAMUtil.GetExt(filename_suggestion);
        // String fn;

        for (int i = 0; i < 1000; i++) {

            final String fn = new String(base + i + "." + ext); // format("%s%03i.%s",
            // base, i, ext );
            final boolean exists = CAAMUtil.FileExists(fn);

            if ( !exists) {

                return fn;
            }
        }

        return "";
    }

    /**
     * Samples a set of texture vectors given a set of shape in absolute (i.e. image) coordinates.
     * 
     * @param unalignedShapes Shapes in absolute coordinates.
     * @param vTextures The set of textures.
     * @param outputRF The output reference frame generated for sampling the textures.
     * @param removeMean If true the mean from each texture vector (i.e. the DC) is removed.
     * @param useTSP Use tangent space projection to align the shapes.
     * @param useConvexHull If true the convex hull is used to determine the extent of a shape.
     */
    public void SampleTextures(final CAAMShapeCollection unalignedShapes, final Vector<CDVector> vTextures, final CAAMReferenceFrame outputRF,
            final int imageReduction, final boolean removeMean, final boolean useTSP, final boolean useConvexHull) {

        // /////////////////////////////////////
        // align shapes
        // /////////////////////////////////////

        // copy the unaligned shapes
        final CAAMShapeCollection alignedShapes = new CAAMShapeCollection();
        alignedShapes.assign(unalignedShapes);

        // align shape with respect to position, scale and rotation
        alignedShapes.AlignShapes(useTSP);

        // calculate the cached reference shape of the unaligned shapes
        final CAAMShape refShape = new CAAMShape();
        alignedShapes.ReferenceShape(refShape);

        // /////////////////////////////////////
        // make reference frame and analyzer
        // /////////////////////////////////////
        outputRF.Setup(refShape, useConvexHull);
        final CAAMAnalyzeSynthesizeSoftware as = new CAAMAnalyzeSynthesizeSoftware(outputRF);

        // /////////////////////////////////////
        // sample texture vectors
        // /////////////////////////////////////
        vTextures.clear();
        for (int i = 0; i < unalignedShapes.NShapes(); i++) {

            // get the shape image
            ModelSimpleImage image = new ModelSimpleImage();
            image = unalignedShapes.get(i).GetHostImage(image, unalignedShapes.Path(), imageReduction);

            // sample the texture of the i-th shape into 'vTexture'
            final CDVector tex = new CDVector();
            as.SetAnalyzeImage(image);
            as.Analyze(unalignedShapes.get(i), tex, true);

            if (removeMean) {

                // remove mean
                final double mean = tex.Mean();
                for (i = 0; i < tex.Length(); i++) {

                    tex.set(i, tex.get(i) - mean);
                }
            }

            // store texture
            vTextures.add(tex);
        }
    }

    /**
     * Writes a movie file containing all shapes from a directory warped to their mean shape.
     * 
     * @param filename Output movie filename.
     * @param asfPath Path to annotation files.
     * @param useConvexHull If true the convex hull is used to determine the extent of a shape.
     * @param writeRefShape If true the reference shape corresponding to the movie file is written as "regshape.asf".
     */
    public void RegistrationMovie(final String filename, final String asfPath, final boolean useConvexHull, final boolean writeRefShape) {

        Vector<String> asfFiles = new Vector<String>();
        final CAAMShapeCollection shapes = new CAAMShapeCollection();
        asfFiles = (Vector<String>) CAAMUtil.ScanSortDir(asfPath, "asf");
        shapes.ReadShapes(asfFiles, true);
        shapes.Rel2Abs(1);

        final CAAMReferenceFrame rf = new CAAMReferenceFrame();
        final Vector<CDVector> vTextures = new Vector<CDVector>();
        SampleTextures(shapes, vTextures, rf, 1, false, true, useConvexHull);

        CAAMUtil.RegistrationMovie(filename, vTextures, rf);

        if (writeRefShape) {

            final CAAMShape shape = new CAAMShape(rf.RefShape());
            shape.SetHostImage(filename);
            shape.WriteASF("regshape.asf", rf.RefImageWidth(), rf.RefImageHeight());
        }
    }

    /**
     * Calculates the overlap between two shapes as specified in "Active Shape Model Segmentation With Optimal Features"
     * Bram van Ginneken et al., IEEE TMI 21(8) Aug. 2002.
     * 
     * Notice that this only makes sense for one-path closed shapes.
     * 
     * @param model Model shape
     * @param gt Ground truth shape
     * @return The shape overlap (1 = perfect match, 0 = no overlap).
     */
    public double ShapeOverlap(final CAAMShape model, final CAAMShape gt) {

        double overlap, TP, FP, FN;

        final double minX = Math.min(model.MinX(), gt.MinX());
        final double maxX = Math.max(model.MaxX(), gt.MaxX());
        final double minY = Math.min(model.MinY(), gt.MinY());
        final double maxY = Math.max(model.MaxY(), gt.MaxY());

        // calculate overlap in a one-unit spaced grid
        TP = .0;
        FP = .0;
        FN = .0;
        for (double x = minX; x < maxX; x = x + 1.) {

            for (double y = minY; y < maxY; y = y + 1.) {

                final boolean insideModel = model.IsInside(new CAAMPoint(x, y));
                final boolean insideGT = gt.IsInside(new CAAMPoint(x, y));

                if (insideModel || insideGT) {

                    TP = (insideGT && insideModel) ? TP + 1. : TP;
                    FP = (insideModel && !insideGT) ? FP + 1. : FP;
                    FN = ( !insideModel && insideGT) ? FN + 1. : FN;
                }

            }
        }

        overlap = TP / (TP + FP + FN);

        return overlap;
    }

}
