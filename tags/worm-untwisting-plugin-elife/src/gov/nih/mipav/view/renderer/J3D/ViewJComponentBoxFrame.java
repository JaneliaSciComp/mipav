package gov.nih.mipav.view.renderer.J3D;


import java.awt.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * The red bounding box of the surface renderer. More comments
 */
public class ViewJComponentBoxFrame extends IndexedLineArray {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Save the colors (typically the same for all vertices )of the bounding box. */
    private Color3f[] colors;

    /** Stores the 8 vertices of the bounding box. */
    private Point3f[] verts;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates 8 verticies with 12 edges, a rectangular cube, that represents the bounding box frame of the surface
     * renderer.
     *
     * @param  xBox  = (xDim-1)*xRes/((dim-1)*res)max Ruida: This comments are wrong !!!!!!
     * @param  yBox  = (yDim-1)*yRes/((dim-1)*res)max
     * @param  zBox  = (zDim-1)*zRes/((dim-1)*res)max
     */
    public ViewJComponentBoxFrame(float xBox, float yBox, float zBox) {
        super(8, GeometryArray.COORDINATES | GeometryArray.COLOR_3, 24);
        setCapability(ALLOW_COLOR_WRITE);

        verts = new Point3f[8];
        colors = new Color3f[8];

        // Bounding Box
        verts[0] = new Point3f(-xBox, yBox, zBox);
        verts[1] = new Point3f(xBox, yBox, zBox);
        verts[2] = new Point3f(xBox, -yBox, zBox);
        verts[3] = new Point3f(-xBox, -yBox, zBox);
        verts[4] = new Point3f(-xBox, -yBox, -zBox);
        verts[5] = new Point3f(xBox, -yBox, -zBox);
        verts[6] = new Point3f(xBox, yBox, -zBox);
        verts[7] = new Point3f(-xBox, yBox, -zBox);

        // Red for the bounding box.
        for (int i = 0; i < 8; i++) {
            colors[i] = new Color3f(1.0f, 0.0f, 0.0f);
        }

        int[] pntsIndex = new int[24];

        // Bounding box connections.
        pntsIndex[0] = 0;
        pntsIndex[1] = 1;

        pntsIndex[2] = 1;
        pntsIndex[3] = 2;

        pntsIndex[4] = 2;
        pntsIndex[5] = 3;

        pntsIndex[6] = 3;
        pntsIndex[7] = 4;

        pntsIndex[8] = 4;
        pntsIndex[9] = 5;

        pntsIndex[10] = 5;
        pntsIndex[11] = 6;

        pntsIndex[12] = 6;
        pntsIndex[13] = 7;

        pntsIndex[14] = 7;
        pntsIndex[15] = 0;

        pntsIndex[16] = 0;
        pntsIndex[17] = 3;

        pntsIndex[18] = 4;
        pntsIndex[19] = 7;

        pntsIndex[20] = 2;
        pntsIndex[21] = 5;

        pntsIndex[22] = 1;
        pntsIndex[23] = 6;

        setCoordinates(0, verts);
        setCoordinateIndices(0, pntsIndex);
        setColors(0, colors);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets color of frame.
     *
     * @param  color  Color to set to.
     */
    public void setColor(Color color) {

        for (int i = 0; i < 8; i++) {
            colors[i] = new Color3f(color);
        }

        setColors(0, colors);
    }

}
