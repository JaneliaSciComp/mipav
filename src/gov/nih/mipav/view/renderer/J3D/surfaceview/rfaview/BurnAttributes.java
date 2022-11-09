package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Class that holds the information about each surface; the BranchGroup which holds the surface subtree, the name of the
 * surface in the dialog, the color of the surface, the shininess of the surface, the level of detail (for clod meshes),
 * the number of triangles (changes with level of detail), the polygon mode (fill, line, or point), and a flag
 * indicating if this is a clod mesh.
 */
public class BurnAttributes {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** burn subtree, holds all the Shape3D objects that make up the surface. */
    public BranchGroup burnBG;

    /** burning time. */
    public float burningTime;

    /** BitSet burning sphere or ellposoid mask. */
    public BitSet burnMask;

    /** Burn center coordinate. */
    public Vector3f burnPoint;

    /** burn center. */
    public Point3f center;

    /** burn surface clipped or not. */
    public boolean clipping;

    /**
     * Color of burn. The element type is Color4f, although currently the alpha channel does not appear to be supported
     * by Java3D materials (even though the Material API allows the alpha channel to be set for diffuse colors).
     */
    public Color4f color;

    /** Back face culling enabled or not. */
    public boolean culling;

    /** entry point coordinate of the specified probe. */
    public Point3f entryPoint;

    /** Name of surface displayed in list box. */
    public String name;

    /** opacity of burn. */
    public float opacity;

    /** burn surface pickabel or not. */
    public boolean pickable;

    /** burn diameter. */
    public Point3f radius;

    /** probe transform. */
    public Transform3D transform;

    /** Volume of triangle mesh displayed associated with the burn. Changes with level of detail. */
    public float volume;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new attributes structure to hold information needed for displaying each burn.
     *
     * @param  burnBG       burnBG subtree.
     * @param  name         Name of surface.
     * @param  color        Color of surface.
     * @param  volume       Volume of mesh.
     * @param  opacity      opacity of the surface
     * @param  radius       sphere radius
     * @param  burningTime  ablation time
     */
    public BurnAttributes(BranchGroup burnBG, String name, Color4f color, float volume, float opacity, Point3f radius,
                          float burningTime) {

        this.burnBG = burnBG;
        this.name = name;
        this.color = color;
        this.volume = volume;
        this.opacity = opacity;
        this.radius = radius;
        this.burningTime = burningTime;
    }

    /**
     * Constructs new attributes structure to hold information needed for displaying each surface.
     *
     * @param  burnBG       burnBG subtree.
     * @param  name         Name of surface.
     * @param  color        Color of surface.
     * @param  volume       Volume of mesh.
     * @param  opacity      opacity of the surface.
     * @param  diameter     diameter, a point to represent semiX, Y, Z
     * @param  burningTime  Burning time
     * @param  center       burning center.
     * @param  pickable     burning surface pickable or not
     * @param  clipping     burning surface clipping or not.
     * @param  culling      burning point surface back face culling or not
     * @param  burnMask     burning sphere mask.
     * @param  entryPoint   entry point coordinate
     * @param  burnPoint    burn center point coordinate
     * @param  transform    probe transform
     */
    public BurnAttributes(BranchGroup burnBG, String name, Color4f color, float volume, float opacity, Point3f diameter,
                          float burningTime, Point3f center, boolean pickable, boolean clipping, boolean culling,
                          BitSet burnMask, Point3f entryPoint, Vector3f burnPoint, Transform3D transform) {
        this(burnBG, name, color, volume, opacity, diameter, burningTime);
        this.center = center;
        this.pickable = pickable;
        this.clipping = clipping;
        this.culling = culling;
        this.burnMask = burnMask;
        this.entryPoint = entryPoint;
        this.burnPoint = burnPoint;
        this.transform = transform;
    }

}
