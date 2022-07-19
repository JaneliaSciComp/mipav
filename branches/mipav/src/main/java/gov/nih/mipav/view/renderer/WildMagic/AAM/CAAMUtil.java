package gov.nih.mipav.view.renderer.WildMagic.AAM;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelSimpleImage;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;
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
 * 
 * Utility methods for the AAM project. This class consists of methods that are self-containing and could not logically
 * fit into any other AAM class.
 * 
 * Thus all methods are static, so remember that there is never need for an instantiation of this class.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMUtil extends CAAMObject {

    /** File name vector */
    public static List<String> vFilenames = new Vector<String>();

    /**
     * Plots a mesh into an image.
     * 
     * @param image The image in which the shape should be plotted.
     * @param shape The shape to be plotted.
     * @param point_color The point color of the mesh.
     * @param line_color The line color of the mesh.
     */
    public static void PlotMeshIntoImage(final ModelSimpleImage image, final CAAMMesh mesh, final double[] point_color, final double[] line_color) {

        // plot triangles
        for (int i = 0; i < mesh.NTriangles(); i++) {

            final CAAMTriangle tri = mesh.Triangles().get(i);

            // get points
            final CAAMPoint p1 = mesh.Points().get(tri.V1());
            final CAAMPoint p2 = mesh.Points().get(tri.V2());
            final CAAMPoint p3 = mesh.Points().get(tri.V3());

            final int[] x1 = new int[1];
            final int[] y1 = new int[1];
            final int[] x2 = new int[1];
            final int[] y2 = new int[1];

            // plot lines

            x1[0] = (int) (.5 + p1.x);
            y1[0] = (int) (.5 + p1.y);
            x2[0] = (int) (.5 + p2.x);
            y2[0] = (int) (.5 + p2.y);

            x1[0] = (int) (.5 + p2.x);
            y1[0] = (int) (.5 + p2.y);
            x2[0] = (int) (.5 + p3.x);
            y2[0] = (int) (.5 + p3.y);

            x1[0] = (int) (.5 + p3.x);
            y1[0] = (int) (.5 + p3.y);
            x2[0] = (int) (.5 + p1.x);
            y2[0] = (int) (.5 + p1.y);

            // plot points
            image.setValue((int) (p1.x + .5), (int) (p1.y + .5), (float) point_color[0]);
            image.setValue((int) (p2.x + .5), (int) (p2.y + .5), (float) point_color[0]);
            image.setValue((int) (p3.x + .5), (int) (p3.y + .5), (float) point_color[0]);
        }
    }

    /**
     * Wrapper to draw shape onto image
     * 
     * @param image The image in which the shape should be plotted.
     * @param shape The shape to be plotted.
     * @param point_color The point color of the shape.
     * @param line_color The line color of the shape.
     * @param drawNormals If true the point normals are drawn.
     * @param drawArea If true the inside area of the shape is drawn.
     * @param drawPoints If true the points of the shape is drawn.
     */
    public static void PlotShapeIntoImage(final ModelSimpleImage image, final CAAMShape shape, final double[] point_color, final double[] line_color,
            final boolean drawNormals, final boolean drawArea, final boolean drawPoints) {
        PlotShapeIntoImage(image, shape, point_color, line_color, drawNormals, drawArea, drawPoints, true);
    }

    /**
     * Draws the shape into an image with a given color.
     * 
     * @param image The image in which the shape should be plotted.
     * @param shape The shape to be plotted.
     * @param point_color The point color of the shape.
     * @param line_color The line color of the shape.
     * @param drawNormals If true the point normals are drawn.
     * @param drawArea If true the inside area of the shape is drawn.
     * @param drawPoints If true the points of the shape is drawn.
     * @param drawLines If true the lines of the shape is drawn.
     */
    public static void PlotShapeIntoImage(final ModelSimpleImage image, final CAAMShape shape, final double[] point_color, final double[] line_color,
            final boolean drawNormals, final boolean drawArea, final boolean drawPoints, final boolean drawLines) {

        int w, h;
        int path_id = -1;
        final double[] x = new double[1];
        final double[] y = new double[1];
        w = image.Width();
        h = image.Height();

        final boolean drawOriginalPointsOnly = false;

        // make inside test
        if (shape.MinX() < 3 || shape.MinY() < 3 || shape.MaxX() >= w - 4 || shape.MaxY() >= h - 4) {

            return;
        }

        int k;
        final double[] invpcolor = new double[1];
        final double[] invlcolor = new double[1];
        for (k = 0; k < AAMdef.BANDS; k++) {
            invpcolor[k] = 255 - point_color[k];
            invlcolor[k] = 255 - line_color[k];
        }

        if (drawLines) {

            // draw lines
            for (int i = 0; i < shape.NPoints(); i++) {

                final CAAMPoint p1 = shape.GetPoint(i);
                final CAAMPointInfo pi = shape.PointAux().get(i);
                final CAAMPoint p2 = shape.GetPoint(pi.m_iConnectTo);

                if (path_id != pi.m_iPathID) {

                    // new path reached
                    path_id = pi.m_iPathID;
                }

                if (pi.m_iConnectTo != pi.m_iConnectFrom && (drawOriginalPointsOnly == false || pi.IsOriginal())) {

                    final int[] x1 = new int[1];
                    final int[] y1 = new int[1];
                    final int[] x2 = new int[1];
                    final int[] y2 = new int[1];

                    x1[0] = (int) (.5 + p1.x);
                    y1[0] = (int) (.5 + p1.y);
                    x2[0] = (int) (.5 + p2.x);
                    y2[0] = (int) (.5 + p2.y);
                    // draw line
                    DrawLine(x1, y1, x2, y2, line_color, false, image);
                }

                if (i == pi.m_iConnectTo && pi.m_iConnectTo == pi.m_iConnectFrom) {

                    // force drawing of isolated points
                    final int xi = (int) (.5 + p1.x);
                    final int yi = (int) (.5 + p1.y);

                    // draw isolated point as a cross
                    image.setValue(xi, yi, (float) invpcolor[0]);
                    image.setValue(xi + 1, yi, (float) point_color[0]);
                    image.setValue(xi - 1, yi, (float) point_color[0]);
                    image.setValue(xi + 2, yi, (float) point_color[0]);
                    image.setValue(xi - 2, yi, (float) point_color[0]);
                    image.setValue(xi, yi - 1, (float) point_color[0]);
                    image.setValue(xi, yi + 1, (float) point_color[0]);
                    image.setValue(xi, yi - 2, (float) point_color[0]);
                    image.setValue(xi, yi + 2, (float) point_color[0]);
                }
            }
        }

        // draw points
        if (drawPoints) {

            for (int i = 0; i < shape.NPoints(); i++) {

                shape.GetPoint(i, x, y);

                if (x[0] >= 0 && x[0] < w && y[0] >= 0 && y[0] < h) {

                    final int xi = (int) (.5 + x[0]);
                    final int yi = (int) (.5 + y[0]);

                    // draw cross
                    image.setValue(xi, yi, (float) invpcolor[0]);
                    image.setValue(xi + 1, yi, (float) point_color[0]);
                    image.setValue(xi - 1, yi, (float) point_color[0]);
                    image.setValue(xi + 2, yi, (float) point_color[0]);
                    image.setValue(xi - 2, yi, (float) point_color[0]);
                    image.setValue(xi, yi - 1, (float) point_color[0]);
                    image.setValue(xi, yi + 1, (float) point_color[0]);
                    image.setValue(xi, yi - 2, (float) point_color[0]);
                    image.setValue(xi, yi + 2, (float) point_color[0]);
                }
            }
        }

        // draw normals
        if (drawNormals) {

            for (int i = 0; i < shape.NPoints(); i++) {

                shape.GetPoint(i, x, y);

                // draw normal
                final CAAMPoint p1 = new CAAMPoint();
                final CAAMPoint p2 = new CAAMPoint();
                shape.Normal(i, p1, p2, 10);

                if (p1.x >= 0 && p1.x <= w - 1 && p1.y >= 0 && p1.y <= h - 1 && p2.x >= 0 && p2.x <= w - 1 && p2.y >= 0 && p2.y <= h - 1) {

                    final int[] x1 = new int[1];
                    final int[] y1 = new int[1];
                    final int[] x2 = new int[1];
                    final int[] y2 = new int[1];

                    x1[0] = (int) (.5 + x[0]);
                    y1[0] = (int) (.5 + y[0]);
                    x2[0] = (int) (.5 + p2.x);
                    y2[0] = (int) (.5 + p2.y);

                    DrawLine(x1, y1, x2, y2, invlcolor, false, image);

                    x1[0] = (int) (.5 + p1.x);
                    y1[0] = (int) (.5 + p1.y);
                    x2[0] = (int) (.5 + x[0]);
                    y2[0] = (int) (.5 + y[0]);

                    DrawLine(x1, y1, x2, y2, line_color, false, image);
                }
            }
        }

        // draw the inside area of the shape
        // signaled by white and black dots
        if (drawArea) {

            final int maxX = (int) (.5 + shape.MaxX());
            final int maxY = (int) (.5 + shape.MaxY());
            final int minX = (int) (.5 + shape.MinX());
            final int minY = (int) (.5 + shape.MinY());

            final double[] p255 = new double[1];
            final double[] p0 = new double[1];
            // ModelSimpleImage.SPAWN( p0, 0);
            p0[0] = 0;
            // ModelSimpleImage.SPAWN( p255, 255);
            p255[0] = 255;

            for (int y1 = minY; y1 < maxY; y1 += 5) {

                for (int x1 = minX; x1 < maxX; x1 += 5) {

                    if (shape.IsInside(new CAAMPoint(x1, y1))) {
                        image.setValue(x1, y1, (float) p255[0]);
                        image.setValue(x1 + 1, y1, (float) p0[0]);
                    }
                }
            }
        }

    }

    /**
     * Check the point is inside the image or not
     * 
     * @param x point x coordinate
     * @param y point y coordinate
     * @param image model image reference.
     * @return true inside, false outside.
     */
    public static boolean ContainsPoint(final int x, final int y, final ModelSimpleImage image) {
        final int xDim = image.extents[0];
        final int yDim = image.extents[1];
        return (x >= 0 && x < xDim && y >= 0 && y < yDim);
    }

    /**
     * Swap values
     * 
     * @param a
     * @param b
     */
    public static void Swap(final int[] a, final int[] b) {
        final int temp = a[0];
        a[0] = b[0];
        b[0] = temp;
    }

    /**
     * Draw lines
     * 
     * @param x1
     * @param y1
     * @param x2
     * @param y2
     * @param tVal
     * @param fInsideCheck
     * @param image
     */
    public static void DrawLine(final int[] x1, final int[] y1, final int[] x2, final int[] y2, final double[] tVal, final boolean fInsideCheck,
            final ModelSimpleImage image) {
        boolean flip = true;

        if (Math.abs(x2[0] - x1[0]) < Math.abs(y2[0] - y1[0])) {
            Swap(x1, y1);
            Swap(x2, y2);
            flip = false;
        }

        if (x1[0] > x2[0]) {
            Swap(x1, x2);
            Swap(y1, y2);
        }

        int yincr;

        if (y2[0] > y1[0]) {
            yincr = 1;
        } else {
            yincr = -1;
        }

        final double dx = x2[0] - x1[0];
        final double dy = Math.abs(y2[0] - y1[0]);
        double d = 2 * dy - dx;
        final int aincr = (int) (2 * (dy - dx));
        final int bincr = (int) (2 * dy);
        int xx = x1[0];
        int yy = y1[0];
        final int nPoints = 0;

        if (fInsideCheck) {
            if ( !flip) {
                if (ContainsPoint(yy, xx, image)) {
                    image.setValue(yy, xx, (float) tVal[0]);
                }
            } else {
                if (ContainsPoint(xx, yy, image)) {
                    image.setValue(xx, yy, (float) tVal[0]);
                }
            }

            for (xx = x1[0] + 1; xx <= x2[0]; xx++) {
                if (d >= 0) {
                    yy += yincr;
                    d += aincr;
                } else {
                    d += bincr;
                }

                if ( !flip) {
                    if (ContainsPoint(yy, xx, image)) {
                        image.setValue(yy, xx, (float) tVal[0]);
                    }
                } else {
                    if (ContainsPoint(xx, yy, image)) {
                        image.setValue(xx, yy, (float) tVal[0]);
                    }
                }
            }
        } else {
            if ( !flip) {
                image.setValue(yy, xx, (float) tVal[0]);
            } else {
                image.setValue(xx, yy, (float) tVal[0]);
            }

            for (xx = x1[0] + 1; xx <= x2[0]; xx++) {
                if (d >= 0) {
                    yy += yincr;
                    d += aincr;
                } else {
                    d += bincr;
                }

                if ( !flip) {
                    image.setValue(yy, xx, (float) tVal[0]);
                } else {
                    image.setValue(xx, yy, (float) tVal[0]);
                }
            }

        }
    }

    /**
     * Concatenates three vectors.
     * 
     * @param dest Output vector.
     * @param v1 First input vector.
     * @param v2 Second input vector.
     * @param v3 Third input vector.
     */
    public static void VecCat3(final CDVector dest, final CDVector v1, final CDVector v2, final CDVector v3) {

        assert (dest.Length() == (v1.Length() + v2.Length() + v3.Length()));
        int j;
        final int v1_end = v1.Length();
        final int v2_end = v1_end + v2.Length();
        final int v3_end = dest.Length();
        for (j = 0; j < v1_end; j++) {
            dest.m_data[j] = v1.m_data[j];
        }
        for (j = v1_end; j < v2_end; j++) {
            dest.m_data[j] = v2.m_data[j - v1_end];
        }
        for (j = v2_end; j < v3_end; j++) {
            dest.m_data[j] = v3.m_data[j - v2_end];
        }
    }

    /**
     * Scans and sorts a directory for files with a specific extension.
     * 
     * @param path Path to read from.
     * @param extension The file extension to search for. ex. "hips".
     * @return The filenames found without any path.
     */
    public static List<String> ScanSortDir(final String path, final String extension) {

        final String searchPath;
        final int nbFiles;

        final String pathBS = path;

        // build and sort list of filenames
        processDir(pathBS, extension);

        // sort the filenames
        Collections.sort(vFilenames, String.CASE_INSENSITIVE_ORDER);

        // return
        return vFilenames;
    }

    /**
     * Process directory.
     * 
     * @param pathBS file path
     * @param ext file extension
     */
    private static void processDir(final String pathBS, final String ext) {
        final File fileDir = new File(pathBS);

        final String[] children = fileDir.list();
        if (children != null) {
            for (int i = 0; i < children.length; i++) {
                final String fileName = children[i];
                if (fileName.endsWith(ext)) {
                    vFilenames.add(pathBS + File.separator + fileName);
                }
            }
        }

    }

    /**
     * Wrapper to read image and VOIs
     * 
     * @param filename file name
     * @param img image
     * @param shape voi
     * @return
     */
    public static ModelSimpleImage ReadExample(final String filename, ModelSimpleImage img, final CAAMShape shape) {
        img = ReadExample(filename, img, shape, 1);
        return img;
    }

    /**
     * @author Mikkel B. Stegmann
     * @version 3-10-2000
     * @memo Reads an image and a shape.
     * @doc Reads an image and a shape (in the corresponding .m-file).
     * @see
     * @param filename The filename of an annotation. Ex. "horse.asf".
     * @param img Output image.
     * @param shape Output shape.
     * @param rfactor The reduction factor. Performs a scaling of the input of 1/rfactor.
     * @return img image
     */
    public static ModelSimpleImage ReadExample(final String filename, ModelSimpleImage img, final CAAMShape shape, final int rfactor) {

        // load annotation
        final boolean ok = shape.ReadASF(filename);

        // check for any errors
        if ( !ok) {
            System.err.println("The requested .asf file '" + "' does not exist." + "ReadExample " + "AAMUtil.cpp");
        }

        // load image
        if (shape.HostImage() != "") {

            img = img.ReadBandedFile(CAAMUtil.GetPath(filename) + shape.HostImage());
            img.SetName(shape.HostImage());

        } else {

            System.err.println("No host image for shape." + " ReadExample");
        }

        // convert to absolute coordinates
        shape.Rel2Abs(img.Width(), img.Height());

        // scale if requested
        if (rfactor != 1) {

            shape.Scale(1.0 / rfactor);
            // img.ReducePyr( rfactor );
            img.subSample2dBy2();
        }
        return img;
    }

    /**
     * Read example from target image, scale down if necessary.
     * 
     * @param targetImageSlice target image slice
     * @param img sample image
     * @param shape voi
     * @param rfactor subsample factor
     * @return image
     */
    public static ModelSimpleImage ReadExample(final ModelImage targetImageSlice, ModelSimpleImage img, final CAAMShape shape, final int rfactor) {

        // load annotation
        final boolean ok = shape.ReadASFfromVOI(targetImageSlice);

        // load image
        if (shape.HostImage() != "") {

            img = shape.GetHostImage(shape.getHostImage(), rfactor);
            img.SetName(shape.HostImage());

        } else {

            System.err.println("No host image for shape." + " ReadExample");
        }

        // scale if requested
        if (rfactor != 1) {

            shape.Scale(1.0 / rfactor);
            img.subSample2dBy2();
        }
        return img;
    }

    /**
     * 
     * Read example from target image, scale down if necessary.
     * 
     * @param targetImageSlice target image slice
     * @param img sample image
     * @param shape voi
     * @param rfactor subsample factor
     * @return image
     */
    public static ModelSimpleImage ReadExample_init(final ModelImage targetImageSlice, ModelSimpleImage img, final CAAMShape shape, final int rfactor) {

        // load annotation
        final boolean ok = shape.ReadASFfromVOI_init(targetImageSlice);

        // load image
        if (shape.HostImage() != "") {

            // shape.HostImage());
            img = shape.GetHostImage(shape.getHostImage(), rfactor);
            img.SetName(shape.HostImage());

        } else {

            System.err.println("No host image for shape." + " ReadExample");
        }

        // scale if requested
        if (rfactor != 1) {

            shape.Scale(1.0 / rfactor);

            img.subSample2dBy2();
        }
        return img;
    }

    /**
     * Calculates the point to point error between two shapes.
     * 
     * @param s1 Shape 1.
     * @param s2 Shape 2.
     * @return The average point to point error.
     */
    public static double DistEuclidianPoints(final CAAMShape s1, final CAAMShape s2) {

        final double[] x = new double[1];
        final double[] y = new double[1];

        assert (s1.NPoints() == s2.NPoints());

        // convert point format
        final CDVector vX1 = new CDVector(s1.NPoints());
        final CDVector vY1 = new CDVector(s1.NPoints());
        final CDVector vX2 = new CDVector(s2.NPoints());
        final CDVector vY2 = new CDVector(s2.NPoints());
        for (int i = 0; i < s1.NPoints(); i++) {

            s1.GetPoint(i, x, y);
            vX1.m_data[i] = x[0];
            vY1.m_data[i] = y[0];

            s2.GetPoint(i, x, y);
            vX2.m_data[i] = x[0];
            vY2.m_data[i] = y[0];
        }

        return DistEuclidianPoints(vX1, vY1, vX2, vY2);
    }

    /**
     * Wrapper to calculate the point to curve error.
     * 
     * @param s1 shape 1
     * @param s2 shape 2
     * @return the average point to curve error.
     */
    public static double DistEuclidianAssBorder(final CAAMShape s1, final CAAMShape s2) {
        return DistEuclidianAssBorder(s1, s2, null);
    }

    /**
     * Calculates the point to curve error between two shapes.
     * 
     * @param s1 Shape 1.
     * @param s2 Shape 2.
     * @param pvDist Optional pointer to an output vector containing all point to curve distances (one for each
     *            landmark).
     * @return The average point to curve error.
     */
    public static double DistEuclidianAssBorder(final CAAMShape s1, final CAAMShape s2, final CDVector pvDist) {

        final double[] x = new double[1];
        final double[] y = new double[1];

        assert (s1.NPoints() == s2.NPoints());

        // convert point format
        final CDVector vX1 = new CDVector(s1.NPoints());
        final CDVector vY1 = new CDVector(s1.NPoints());
        final CDVector vX2 = new CDVector(s2.NPoints());
        final CDVector vY2 = new CDVector(s2.NPoints());
        for (int i = 0; i < s1.NPoints(); i++) {

            s1.GetPoint(i, x, y);
            vX1.m_data[i] = x[0];
            vY1.m_data[i] = y[0];

            s2.GetPoint(i, x, y);
            vX2.m_data[i] = x[0];
            vY2.m_data[i] = y[0];
        }

        return DistEuclidianAssBorder(vX1, vY1, vX2, vY2, pvDist);
    }

    /**
     * Calculates the point to point error between two shapes.
     * 
     * @param vX1 X-positions of shape 1.
     * @param vY1 Y-positions of shape 1.
     * @param vX2 X-positions of shape 2.
     * @param vY2 Y-positions of shape 2.
     * @return The average point to point error.
     */
    public static double DistEuclidianPoints(final CDVector vX1, final CDVector vY1, final CDVector vX2, final CDVector vY2) {

        assert (vX1.Length() == vY1.Length());
        assert (vX2.Length() == vY2.Length());
        assert (vX1.Length() == vX2.Length());

        double dDist = 0;

        // euclidian distance with offset as reparameterization
        // Staib & Duncan, PAMI, pp. 1061-1069, 1992
        for (int i = 0; i < vX1.Length(); i++) {
            dDist += Math.sqrt( (vX1.m_data[i] - vX2.m_data[i]) * (vX1.m_data[i] - vX2.m_data[i]) + (vY1.m_data[i] - vY2.m_data[i])
                    * (vY1.m_data[i] - vY2.m_data[i]));
        }

        dDist /= vX1.Length();

        return dDist;
    }

    /**
     * Calculates the point to curve error between two shapes.
     * 
     * @param vX1 X-positions of shape 1.
     * @param vY1 Y-positions of shape 1.
     * @param vX2 X-positions of shape 2.
     * @param vY2 Y-positions of shape 2.
     * @param pvDist Optional pointer to an output vector containing all point to curve distances (one for each
     *            landmark).
     * @return The average point to curve error.
     */
    public static double DistEuclidianAssBorder(final CDVector vX1, final CDVector vY1, final CDVector vX2, final CDVector vY2, final CDVector pvDist) {

        final CDVector vXBorder = new CDVector(vX1.Length());
        final CDVector vYBorder = new CDVector(vX1.Length());
        final CDVector vDist = new CDVector(vX1.Length());

        PointOnAssBorder(vX1, vY1, vX2, vY2, vXBorder, vYBorder, vDist);

        if (pvDist != null) {

            pvDist.Resize(vDist.Length());
            pvDist.assign(vDist);
        }

        return vDist.Mean();
    }

    /**
     * Calculates the point to curve error between two shapes.
     * 
     * @param vX1 X-positions of shape 1.
     * @param vY1 Y-positions of shape 1.
     * @param vX2 X-positions of shape 2.
     * @param vY2 Y-positions of shape 2.
     * @param vXBorder Output x border.
     * @param vYBorder Output y border.
     * @param vDist Point to curve distance for each landmark.
     */
    public static void PointOnAssBorder(final CDVector vX1, final CDVector vY1, final CDVector vX2, final CDVector vY2, final CDVector vXBorder,
            final CDVector vYBorder, final CDVector vDist) {

        final int nStart = 0;
        final int nEnd = vX2.Length();

        // find associated point on border for all coordinates
        for (int iFrom = 0; iFrom < vX1.Length(); iFrom++) {
            double dDist;
            int iDistMin = 0;

            vDist.m_data[iFrom] = Double.MAX_VALUE;

            // find closest existing point on associated border
            for (int iTo = nStart; iTo < nEnd; iTo++) {
                dDist = Math.sqrt(Math.pow(vX1.m_data[iFrom] - vX2.m_data[iTo], 2) + Math.pow(vY1.m_data[iFrom] - vY2.m_data[iTo], 2));

                if (dDist < vDist.m_data[iFrom]) {
                    vDist.m_data[iFrom] = dDist;
                    iDistMin = iTo;
                }
            }

            // set min distance
            vXBorder.m_data[iFrom] = vX2.m_data[iDistMin];
            vYBorder.m_data[iFrom] = vY2.m_data[iDistMin];

            // find project on line before
            final int iBefore = (iDistMin - 1 + vX2.Length()) % vX2.Length(); // cyclic
            double dDistBefore;
            final double[] dXProjBefore = new double[1];
            final double[] dYProjBefore = new double[1];

            // dDistBefore = ProjPointOnLine(3, 3, 5, 1, 5, 3, dXProjBefore,
            // dYProjBefore);
            dDistBefore = ProjPointOnLine(vX2.m_data[iBefore], vY2.m_data[iBefore], vX2.m_data[iDistMin], vY2.m_data[iDistMin], vX1.m_data[iFrom],
                    vY1.m_data[iFrom], dXProjBefore, dYProjBefore);

            // update min distance
            if (dDistBefore < vDist.m_data[iFrom]) {
                vDist.m_data[iFrom] = dDistBefore;
                vXBorder.m_data[iFrom] = dXProjBefore[0];
                vYBorder.m_data[iFrom] = dYProjBefore[0];
            }

            // find project on line after
            final int iAfter = (iDistMin + 1) % vX2.Length(); // cyclic
            double dDistAfter;
            final double[] dXProjAfter = new double[1];
            final double[] dYProjAfter = new double[1];

            dDistAfter = ProjPointOnLine(vX2.m_data[iAfter], vY2.m_data[iAfter], vX2.m_data[iDistMin], vY2.m_data[iDistMin], vX1.m_data[iFrom],
                    vY1.m_data[iFrom], dXProjAfter, dYProjAfter);

            // update min distance
            if (dDistAfter < vDist.m_data[iFrom]) {
                vDist.m_data[iFrom] = dDistAfter;
                vXBorder.m_data[iFrom] = dXProjAfter[0];
                vYBorder.m_data[iFrom] = dYProjAfter[0];
            }
        }
    }

    /**
     * Find the projection dProj of the point dP on the line through dL1 and dL2. Returns the distance between dProj and
     * dP, if the dProj lays between dL1 and dL2 otherwise DBL_MAX.
     * 
     * dXL1,dYL1,dXL2,dYL2: points on line
     * 
     * dXP,dYP: points to project
     * 
     * dXProj,dYProj: projected point on the line
     * 
     * @return The distance.
     */
    public static double ProjPointOnLine(final double dXL1, final double dYL1, final double dXL2, final double dYL2, final double dXP, final double dYP,
            final double[] dXProj, final double[] dYProj) {
        // handle special cases
        if (dXL2 == dXL1) {
            dXProj[0] = dXL1;
            dYProj[0] = dYP;
        } else if (dYL2 == dYL1) {
            dXProj[0] = dXP;
            dYProj[0] = dYL1;
        } else {
            // get line from point dL1 to dL2
            assert (dXL2 != dXL1);
            final double dAL = (dYL2 - dYL1) / (dXL2 - dXL1);
            final double dBL = dYL1 - dAL * dXL1;

            // find perpendicular line through dP
            assert (dAL != 0);
            final double dAP = -1 / dAL;
            final double dBP = dYP - dAP * dXP;

            // find line crossing (projection)
            assert ( (dAP - dAL) != 0);
            dXProj[0] = (dBP - dBL) / (dAL - dAP);
            dYProj[0] = dAL * dXProj[0] + dBL;
        }

        double dDist;
        // to be legal the projected point should lay on the line between dL1
        // and dL2
        if ( ( ( (dXL1 <= dXProj[0]) && (dXL2 >= dXProj[0])) || ( (dXL1 >= dXProj[0]) && (dXL2 <= dXProj[0])))
                && ( ( (dYL1 <= dYProj[0]) && (dYL2 >= dYProj[0])) || ( (dYL1 >= dYProj[0]) && (dYL2 <= dYProj[0])))) {
            dDist = Math.sqrt(Math.pow(dXP - dXProj[0], 2) + Math.pow(dYP - dYProj[0], 2));
        } else {
            dDist = Double.MAX_VALUE;
        }

        return dDist;
    }

    /**
     * Removes the extension of a file name.
     * 
     * @param s Input file name.
     * @return File name without extension.
     */
    public static String RemoveExt(final String s) {
        final int index = s.indexOf(".");
        return s.substring(0, index);
    }

    /**
     * Returns the extension of a file name.
     * 
     * @param s Input filename
     * @return The extension.
     */
    public static String GetExt(final String s) {
        final int index = s.indexOf(".");
        return s.substring(index + 1, s.length());
    }

    /**
     * Tests if a file can be created for writing.
     * 
     * @param file Input file name.
     * @return True, if the file could be created (and deleted again).
     */
    public static boolean CreateTest(final String file) {

        try {
            final PrintWriter fh = new PrintWriter(file);

            if (fh != null) {
                fh.close();
                return true;

            }
        } catch (final IOException e) {
            e.printStackTrace();
        }
        // could not be created
        return false;
    }

    /**
     * Ensures that a string is terminated with a backslash. If the already has a terminating backslash, nothing is
     * done.
     * 
     * @param path Input string.
     * @return Backslash-terminated output string.
     */
    public static String AddBackSlash(final String path) {

        final int len = path.length();

        if (len > 0) {

            if (path.charAt(len - 1) == '\\') {

                return path;
            }
        } else {

            return path;
        }

        return path.concat("\\");
    }

    /**
     * Returns the path of a filename.
     * 
     * @param fullfilename Filename including any path.
     * @return The path to the filename.
     */
    public static String GetPath(final String fullfilename) {

        final int index = fullfilename.lastIndexOf(File.separator);
        return fullfilename.substring(0, index);
    }

    /**
     * Returns the file name of a path+file name string.
     * 
     * @param filename Full qualified filename including path.
     * @return The file name including any extension, but without any path.
     */
    public static String GetFilename(final String filename) {

        final int index = filename.lastIndexOf(File.separator);
        return filename.substring(index + 1, filename.length());
    }

    /**
     * Forces an extension onto a file name. The method tests the extension of a filename. If the wanted extension is
     * presented nothing is done. If not, the extension is appended.
     * 
     * @param filename Input file name.
     * @param ext Wanted extension.
     * @return The file name including the wanted extension.
     */
    public static String ForceExt(final String filename, final String ext) {

        return ext != GetExt(filename) ? filename + "." + ext : filename;
    }

    /**
     * Tests if a file exists.
     * 
     * @param filename File name to test.
     * @return True if the file exists.
     */
    public static boolean FileExists(final String filename) {
        final File fh = new File(filename);
        return fh.exists();
    }

    /**
     * Expands an image to have dyadic size. The method expands an image by using zero-padding so that the resulting
     * image has width and height that are powers of two.
     * 
     * @param img Input image.
     * @param out Output dyadic image.
     */
    public static void ExpandImg2DyadicSize(final ModelSimpleImage img, ModelSimpleImage out) {

        // calc output size
        int new_w, new_h;
        new_w = (int) Math.pow(2, Math.ceil(Math.log(img.Width()) / Math.log(2)));
        new_h = (int) Math.pow(2, Math.ceil(Math.log(img.Height()) / Math.log(2)));
        final int[] extents = new int[2];
        extents[0] = new_w;
        extents[1] = new_h;
        out = new ModelSimpleImage(extents);

        // copy pixels
        double pixel;
        for (int y = 0; y < img.Height(); y++) {

            for (int x = 0; x < img.Width(); x++) {

                pixel = img.getValue(x, y);
                out.setValue(x, y, (float) pixel);
            }
        }
    }

    /**
     * Mirrors the edge of an arbitrary shape. Mirrors the edge of an arbitrary shape mask in a matrix using poor-mans
     * mirroring.
     * 
     * @param img Input matrix. Overwritten by mirrored version.
     * @param mask Image mask defining the shape.
     * @param edgeWidth The width of the edge.
     */
    public static void MirrorEdge(final CDMatrix m, final ModelSimpleImage mask, final int edgeWidth) {

        final int width = m.NCols();
        final int height = m.NRows();
        int y;

        // left horisontal edge
        for (y = 0; y < height; y++) {

            int x = 0;
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

            int x = width - 1;
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
        for (int x = 0; x < width; x++) {

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
        for (int x = 0; x < width; x++) {

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
     * Mirrors the edge of an arbitrary shape. Mirrors the edge of an arbitrary shape mask in an image using poor-mans
     * mirroring.
     * 
     * @param img Input image. Overwritten by mirrored version.
     * @param mask Image mask defining the shape.
     * @param edgeWidth The width of the edge.
     * @return Nothing.
     */
    public static void MirrorEdge(final ModelSimpleImage img, final ModelSimpleImage mask, final int edgeWidth) {

        final int width = img.Width();
        final int height = img.Height();
        double pixel;
        int y;
        // left horisontal edge
        for (y = 0; y < height; y++) {

            int x = 0;
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
                img.setValue(x - i, y, (float) pixel);
            }
        }

        // right horisontal edge
        for (y = 0; y < height; y++) {

            int x = width - 1;
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
                img.setValue(x + i, y, (float) pixel);
            }
        }

        // left vertical edge
        for (int x = 0; x < width; x++) {

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
                img.setValue(x, y - i, (float) pixel);
            }
        }

        // right vertical edge
        for (int x = 0; x < width; x++) {

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
                img.setValue(x, y + i, (float) pixel);
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
    public static boolean ShapeInsideImage(final CAAMShape s, final ModelSimpleImage img) {

        final boolean outside = s.MinX() < 0 || s.MaxX() > img.Width() - 1 || s.MinY() < 0 || s.MaxY() > img.Height() - 1;

        return !outside;
    }

    /**
     * Converts from degrees to radians.
     * 
     * @param deg degree
     * @return radian
     */
    public static double Deg2Rad(final double deg) {
        return deg * Math.PI / 180.0;
    }

    /**
     * Convert from radians to degrees.
     * 
     * @param rad radian
     * @return degree
     */
    public static double Rad2Deg(final double rad) {
        return rad * 180.0 / Math.PI;
    }

    /**
     * Calculates optimization results.
     * 
     * @param optimized Model shape.
     * @param groundTruth Ground truth shape.
     * @param ptpt Average point to point landmark error.
     * @param ptcrv Average point to curve landmark error. NOTICE: This is not a symmetric measure!
     */
    public static void CalcShapeDistances(final CAAMShape optimized, final CAAMShape groundTruth, final double[] ptpt, final double[] ptcrv,
            final CDVector pvDists) {

        ptpt[0] = DistEuclidianPoints(optimized, groundTruth);
        ptcrv[0] = DistEuclidianAssBorder(optimized, groundTruth, pvDists);
    }

    /**
     * Converts seconds to a MM:SS string.
     * 
     * @param secs Time in seconds.
     * @return The time in MM:SS.
     */
    public static String Secs2Mins(final double secs) {

        String ret = new String();
        ret = (int) (secs / 60) + ":" + (int) (.5 + secs) % 60;
        return ret;
    }

    /**
     * Finds a file name that is not 'occupied'.
     * 
     * @This method finds a file name that is not 'occupied' by adding at number to the base part of the suggested file
     *       name.
     * @param filename_suggestion Suggestion including extension.
     * @return An unused filename resembling the suggestion.
     */
    public static String FindVacantFilename(final String filename_suggestion) {

        if (FileExists(filename_suggestion) == false) {

            return filename_suggestion;
        }

        final String base = RemoveExt(filename_suggestion);
        final String ext = GetExt(filename_suggestion);
        final String fn = new String();

        for (int i = 0; i < 1000; i++) {

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
    public static void SampleTextures(final CAAMShapeCollection unalignedShapes, final Vector<CDVector> vTextures, final CAAMReferenceFrame outputRF,
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
                for (int j = 0; j < tex.Length(); j++) {

                    tex.m_data[j] -= mean;
                }
            }

            // store texture
            vTextures.add(tex);
        }
    }

    /**
     * Writes a movie file containing a set of textures warped to their mean shape. Curently diabled.
     * 
     * @param filename Output movie filename.
     * @param vTexture The set of textures.
     * @param rf The reference frame used for sampling the textures.
     */
    public static void RegistrationMovie(final String filename, final Vector<CDVector> vTexture, final CAAMReferenceFrame rf) {

        /*
         * try { // generate shapefree images and write to avi CAAMMovieAVI avi = new CAAMMovieAVI(); avi.Open(
         * filename, OF_CREATE|OF_WRITE ); for(int i=0;i<vTexture.size();i++) {
         * 
         * // generate the i-th frame ModelSimpleImage frame = new ModelSimpleImage();
         * 
         * // do the sampling rf.Vector2Image( vTexture.get(i), frame );
         * 
         * // convert and write frame CVisImageRGB rgbImage = new CVisImageRGB( frame.MemoryRect() );
         * frame.CopyPixelsToRGBA( rgbImage ); avi.WriteFrame( rgbImage ); } } catch (CVisFileIOError e) {
         * 
         * AfxMessageBox( e.FullMessage() ); }
         */
    }

    /**
     * Writes a movie file containing all shapes from a directory warped to their mean shape.
     * 
     * @param filename Output movie filename.
     * @param asfPath Path to annotation files.
     * @param useConvexHull If true the convex hull is used to determine the extent of a shape.
     * @param writeRefShape If true the reference shape corresponding to the movie file is written as "regshape.asf".
     */
    public static void RegistrationMovie(final String filename, final String asfPath, final boolean useConvexHull, final boolean writeRefShape) {

        Vector<String> asfFiles = new Vector<String>();
        final CAAMShapeCollection shapes = new CAAMShapeCollection();
        asfFiles = (Vector<String>) ScanSortDir(asfPath, "asf");
        shapes.ReadShapes(asfFiles, true);
        shapes.Rel2Abs(1);

        final CAAMReferenceFrame rf = new CAAMReferenceFrame();
        final Vector<CDVector> vTextures = new Vector<CDVector>();
        SampleTextures(shapes, vTextures, rf, 1, false, true, useConvexHull);

        RegistrationMovie(filename, vTextures, rf);

        if (writeRefShape) {

            final CAAMShape shape = new CAAMShape(rf.RefShape());
            shape.SetHostImage(filename);
            shape.WriteASF("regshape.asf", rf.RefImageWidth(), rf.RefImageHeight());
        }
    }

    /**
     * Converts AAM-API shape files (.asf) to the ISBE .pts format. Output is written in the directory 'pts'.
     * 
     * @param path Path to .asf files.
     */
    public static void ASF2PTS(final String path) {

        // find .asfs
        final Vector<String> asf_files = (Vector<String>) ScanSortDir(path, "asf");

        final String ptsDir = path + File.separator + "pts";
        final File dDir = new File(ptsDir);
        // _mkdir( AddBackSlash(path)+"pts" );
        dDir.mkdir();
        for (int i = 0; i < asf_files.size(); i++) {

            final CAAMShape shape = new CAAMShape();
            ModelSimpleImage img = new ModelSimpleImage();
            img = CAAMUtil.ReadExample(asf_files.get(i), img, shape);
            shape.Rel2Abs(img.Width(), img.Height());

            // String fn = ( RemoveExt( GetFilename(asf_files.get(i)) )+".pts"
            // );
            final String fn = asf_files.get(i) + ".pts";

            // FILE *fh = fopen( AddBackSlash(path)+"pts/"+fn, "wt" );
            final String absFileDir = path + File.separator + "pts" + File.separator + fn;
            try {
                final PrintWriter fh = new PrintWriter(new File(absFileDir));

                fh.println("version: 1");
                fh.println("// Originally for image " + shape.HostImage());
                fh.println("image_size_x: " + img.Width());
                fh.println("image_size_y: " + img.Height());
                fh.println("n_points: " + shape.NPoints());
                fh.println("{");
                for (int j = 0; j < shape.NPoints(); j++) {

                    final CAAMPoint p = shape.GetPoint(j);
                    fh.println(p.x + " " + p.y);
                }
                fh.println("}");
                System.err.println(fn + " : " + shape.HostImage());
                fh.close();
            } catch (final IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * A symmetric poin- to-curve measure, i.e.
     * 
     * SymmetricPtCrv(a,b) == SymmetricPtCrv( b,a );
     * 
     * @param s1 First shape
     * @param s2 Second shape
     * @return The average symmetric point-to-curve error over all landmarks.
     */
    public static double SymmetricPtCrv(final CAAMShape s1, final CAAMShape s2) {

        final double s12 = DistEuclidianAssBorder(s1, s2);
        final double s21 = DistEuclidianAssBorder(s2, s1);

        // return the mean
        return .5 * (s12 + s21);
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
    public static double ShapeOverlap(final CAAMShape model, final CAAMShape gt) {

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

    /**
     * Reads a file into an array of lines.
     * 
     * @param filename Name of input file.
     * @return A vector of text lines.
     */
    public static Vector<String> ReadLines(final String filename) {

        final Vector<String> lines = new Vector<String>();
        try {
            final FileReader reader = new FileReader(filename);
            final Scanner in = new Scanner(reader);

            while (in.hasNext()) {
                lines.add(in.nextLine());
            }
            in.close();

        } catch (final IOException e) {
            e.printStackTrace();
        }

        return lines;
    }

}
