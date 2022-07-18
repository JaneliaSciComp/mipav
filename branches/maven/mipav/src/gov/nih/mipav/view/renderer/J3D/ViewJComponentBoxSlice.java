package gov.nih.mipav.view.renderer.J3D;


import java.awt.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Four lines that are the border of a slice in the surface renderer. Color of the lines can be changed.
 */
public class ViewJComponentBoxSlice extends IndexedLineArray {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** This is an x slice. */
    public static final int X_SLICE = 0;

    /** This is an y slice. */
    public static final int Y_SLICE = 1;

    /** This is an z slice. */
    public static final int Z_SLICE = 2;

    /** This is an x clip slice. */
    public static final int X_CLIPSLICE = 4;

    /** This is an y clip slice. */
    public static final int Y_CLIPSLICE = 5;

    /** This is an z clip slice. */
    public static final int Z_CLIPSLICE = 6;

    /** This is an -x clip slice. */
    public static final int X_CLIPSLICE_NEG = 7;

    /** This is an -y clip slice. */
    public static final int Y_CLIPSLICE_NEG = 8;

    /** This is an -z clip slice. */
    public static final int Z_CLIPSLICE_NEG = 9;

    /** This is an x clip slice. */
    public static final int A_CLIPSLICE = 10;

    /** This is an s clip slice. */
    public static final int S_CLIPSLICE = 11;

    /** This is an s clip slice. */
    public static final int S_CLIPSLICE_NEG = 12;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Color - can change if user chooses different one. */
    Color3f[] colors;

    /** Connections between verticies, won't change. */
    int[] pntsIndex;


    /** Verticies - can change if user slides slice. */
    Point3f[] verts;

    /** type of box (X_SLICE, Y_SLICE, Z_SLICE). */
    private int m_iMode;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new frame around slice.
     *
     * @param  x     X dimension to use.
     * @param  y     Y dimension to use.
     * @param  z     Z dimension to use.
     * @param  mode  One of X_SLICE, Y_SLICE, or Z_SLICE.
     */
    public ViewJComponentBoxSlice(float x, float y, float z, int mode) {
        super(4, GeometryArray.COORDINATES | GeometryArray.COLOR_3, 8);
        setCapability(ALLOW_COORDINATE_WRITE);
        setCapability(ALLOW_COORDINATE_INDEX_WRITE);
        setCapability(ALLOW_COLOR_WRITE);
        verts = new Point3f[4];
        colors = new Color3f[4];
        pntsIndex = new int[8];
        m_iMode = mode;

        if (mode == X_SLICE) {

            // Yellow
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(1.0f, 1.0f, 0.0f);
            }
        } else if (mode == Y_SLICE) {

            // Green
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(0.0f, 1.0f, 0.0f);
            }
        } else if (mode == Z_SLICE) {

            // Blue
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(1.0f, 0.0f, 0.0f);
            }
        } else if (mode == X_CLIPSLICE) {

            // Yellow
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(1.0f, 1.0f, 0.0f);
            }
        } else if (mode == Y_CLIPSLICE) {

            // Green
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(0.0f, 1.0f, 0.0f);
            }
        } else if (mode == Z_CLIPSLICE) {

            // Blue
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(1.0f, 0.0f, 0.0f);
            }
        } else if (mode == X_CLIPSLICE_NEG) {

            // Yellow
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(1.0f, 1.0f, 0.0f);
            }
        } else if (mode == Y_CLIPSLICE_NEG) {

            // Green
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(0.0f, 1.0f, 0.0f);
            }
        } else if (mode == Z_CLIPSLICE_NEG) {

            // Blue
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(1.0f, 0.0f, 0.0f);
            }
        } else if (mode == A_CLIPSLICE) {

            // light blue
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(0.73f, 0.70f, 0.86f);
            }
        } else if (mode == S_CLIPSLICE) {

            // Orange
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(1.0f, 0.64f, 0f);
            }
        } else if (mode == S_CLIPSLICE_NEG) {

            // Orange
            for (int i = 0; i < 4; i++) {
                colors[i] = new Color3f(1.0f, 0.64f, 1.0f);
            }
        }

        setSlices(x, y, z, mode);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns the color of the slice:
     *
     * @return  DOCUMENT ME!
     */
    public Color3f getColor() {
        return colors[0];
    }

    /**
     * Returns the type of box being drawn, x,y, or z slice:
     *
     * @return  DOCUMENT ME!
     */
    public int getMode() {
        return m_iMode;
    }

    /**
     * Returns a reference to the box vertices:
     *
     * @return  DOCUMENT ME!
     */
    public Point3f[] getVertices() {
        return verts;
    }

    /**
     * Sets color of slice.
     *
     * @param  color  Color to set to.
     */
    public void setColor(Color color) {

        for (int i = 0; i < 4; i++) {
            colors[i] = new Color3f(color);
        }

        setColors(0, colors);
    }

    /**
     * Sets verticies based on dimensions and mode. Whichever mode this is, that dimension will be the same for all four
     * verticies so it's in the proper plane.
     *
     * @param  x     X dimension to use.
     * @param  y     Y dimension to use.
     * @param  z     Z dimension to use.
     * @param  mode  One of X_SLICE, Y_SLICE, or Z_SLICE.
     */
    public void setSlices(float x, float y, float z, int mode) {

        if (mode == X_SLICE) {

            // X slice bounds
            /* The order of the points was changed so that the points were
             * consistent with the other slices: verts[0] ---- verts[1]  |             |  |             |  |
             * | verts[3] ---- verts[2]
             *
             * For the X_SLICE the z values are considered to be inverted (negative z is out of the screen).
             */
            verts[0] = new Point3f(x, y, z);
            verts[1] = new Point3f(x, y, -z);
            verts[2] = new Point3f(x, -y, -z);
            verts[3] = new Point3f(x, -y, z);
        } else if (mode == Y_SLICE) {

            // Y slice bounds
            verts[0] = new Point3f(-x, y, z);
            verts[1] = new Point3f(x, y, z);
            verts[2] = new Point3f(x, y, -z);
            verts[3] = new Point3f(-x, y, -z);
        } else if (mode == Z_SLICE) {

            // Z slice bounds
            verts[0] = new Point3f(-x, y, z);
            verts[1] = new Point3f(x, y, z);
            verts[2] = new Point3f(x, -y, z);
            verts[3] = new Point3f(-x, -y, z);
        } else if (mode == X_CLIPSLICE) {

            // X slice bounds
            verts[0] = new Point3f(x, y, z);
            verts[1] = new Point3f(x, -y, z);
            verts[2] = new Point3f(x, -y, -z);
            verts[3] = new Point3f(x, y, -z);
        } else if (mode == Y_CLIPSLICE) {

            // Y slice bounds
            verts[0] = new Point3f(-x, y, z);
            verts[1] = new Point3f(x, y, z);
            verts[2] = new Point3f(x, y, -z);
            verts[3] = new Point3f(-x, y, -z);
        } else if (mode == Z_CLIPSLICE) {

            // Z slice bounds
            verts[0] = new Point3f(-x, y, z);
            verts[1] = new Point3f(x, y, z);
            verts[2] = new Point3f(x, -y, z);
            verts[3] = new Point3f(-x, -y, z);
        } else if (mode == X_CLIPSLICE_NEG) {

            // X slice bounds
            verts[0] = new Point3f(x, y, z);
            verts[1] = new Point3f(x, -y, z);
            verts[2] = new Point3f(x, -y, -z);
            verts[3] = new Point3f(x, y, -z);
        } else if (mode == Y_CLIPSLICE_NEG) {

            // Y slice bounds
            verts[0] = new Point3f(-x, y, z);
            verts[1] = new Point3f(x, y, z);
            verts[2] = new Point3f(x, y, -z);
            verts[3] = new Point3f(-x, y, -z);
        } else if (mode == Z_CLIPSLICE_NEG) {

            // Z slice bounds
            verts[0] = new Point3f(-x, y, z);
            verts[1] = new Point3f(x, y, z);
            verts[2] = new Point3f(x, -y, z);
            verts[3] = new Point3f(-x, -y, z);
        } else if (mode == A_CLIPSLICE) {

            // A slice bounds
            verts[0] = new Point3f(x, y, z);
            verts[1] = new Point3f(x, -y, z);
            verts[2] = new Point3f(x, -y, -z);
            verts[3] = new Point3f(x, y, -z);
        } else if (mode == S_CLIPSLICE) {

            // S slice bounds
            verts[0] = new Point3f(-x, y, z);
            verts[1] = new Point3f(x, y, z);
            verts[2] = new Point3f(x, -y, z);
            verts[3] = new Point3f(-x, -y, z);
        } else if (mode == S_CLIPSLICE_NEG) {

            // S slice bounds
            verts[0] = new Point3f(-x, y, z);
            verts[1] = new Point3f(x, y, z);
            verts[2] = new Point3f(x, -y, z);
            verts[3] = new Point3f(-x, -y, z);
        }

        pntsIndex[0] = 0;
        pntsIndex[1] = 1;

        pntsIndex[2] = 1;
        pntsIndex[3] = 2;

        pntsIndex[4] = 2;
        pntsIndex[5] = 3;

        pntsIndex[6] = 3;
        pntsIndex[7] = 0;

        setCoordinates(0, verts);
        setCoordinateIndices(0, pntsIndex);
        setColors(0, colors);
    }

}
