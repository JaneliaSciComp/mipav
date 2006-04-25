package gov.nih.mipav.view.renderer.surfaceview;


import gov.nih.mipav.model.structures.*;

import com.sun.j3d.utils.geometry.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Class that holds the information about each surface; the BranchGroup which holds the surface subtree, the name of the
 * surface in the dialog, the color of the surface, the shininess of the surface, the level of detail (for clod meshes),
 * the number of triangles (changes with level of detail), the polygon mode (fill, line, or point), and a flag
 * indicating if this is a clod mesh.
 */
public class SurfaceAttributes {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Area of triangle mesh displayed associated with the surface. Changes with level of detail. */
    public float area;

    /** Surface center. */
    public Point3f center;

    /** Clod mesh. */
    public ModelClodMesh clodMesh;

    /**
     * Color of surface. The element type is Color4f, although currently the alpha channel does not appear to be
     * supported by Java3D materials (even though the Material API allows the alpha channel to be set for diffuse
     * colors).
     */
    public Color4f color;

    /** Detail level of surface; only applies to clod meshes. */
    public int detailLevel;

    /** The full path to the surface file. */
    public String fullPath;

    /**
     * <code>true</code> indicates that this surface is a clod mesh, <code>false</code> that it is a standard triangle
     * mesh; clod meshes can change level of detail, triangle meshes cannot.
     */
    public boolean isClodMesh;

    /** VOI point. */
    public boolean isVOIPt = false;

    /** Name of surface displayed in list box. */
    public String name;

    /** opacity of surface. */
    public float opacity;

    /**
     * Polygon mode of surface, one of PolygonAttribute.POLYGON_FILL, PolygonAttribute.POLYGON_LINE, or
     * PolygonAttribute.POLYGON_POINT.
     */
    public int polygonMode;

    /** Shininess of surface. */
    public int shininess;

    /** Surface subtree, holds all the Shape3D objects that make up the surface. */
    public BranchGroup surface;

    /** Surface volume bit set mask. */
    public BitSet surfaceMask;

    /** Number of triangles displayed associated with the surface. Changes with level of detail. */
    public int triangles;

    /** Volume of triangle mesh displayed associated with the surface. Changes with level of detail. */
    public float volume;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new attributes structure to hold information needed for displaying each surface.
     *
     * @param  surface      Surface subtree.
     * @param  fullPath     The full path to the surface file.
     * @param  name         Name of surface.
     * @param  color        Color of surface.
     * @param  shininess    Shininess of surface.
     * @param  detailLevel  Level of detail.
     * @param  polygonMode  Fill, Line, etc.
     * @param  triangles    Number of triangles in surface.
     * @param  volume       Volume of mesh.
     * @param  area         Area of mesh.
     * @param  isClodMesh   Flag stating if this is a clod mesh.
     * @param  surfaceMask  surface volume mask.
     */
    public SurfaceAttributes(BranchGroup surface, String fullPath, String name, Color4f color, int shininess,
                             int detailLevel, int polygonMode, int triangles, float volume, float area,
                             boolean isClodMesh, BitSet surfaceMask) {

        this(surface, fullPath, name, color, shininess, detailLevel, polygonMode, triangles, volume, area, isClodMesh,
             0.5f, surfaceMask);
    }

    /**
     * Constructs new attributes structure to hold information needed for displaying each surface.
     *
     * @param  surface      Surface subtree.
     * @param  fullPath     The full path to the surface file.
     * @param  name         Name of surface.
     * @param  color        Color of surface.
     * @param  shininess    Shininess of surface.
     * @param  detailLevel  Level of detail.
     * @param  polygonMode  Fill, Line, etc.
     * @param  triangles    Number of triangles in surface.
     * @param  volume       Volume of mesh.
     * @param  area         Area of mesh.
     * @param  isClodMesh   Flag stating if this is a clod mesh.
     * @param  opacity      opacity of the surface
     * @param  surfaceMask  Surface volume bit set mask
     */
    public SurfaceAttributes(BranchGroup surface, String fullPath, String name, Color4f color, int shininess,
                             int detailLevel, int polygonMode, int triangles, float volume, float area,
                             boolean isClodMesh, float opacity, BitSet surfaceMask) {

        this.surface = surface;
        this.fullPath = fullPath;
        this.name = name;
        this.color = color;
        this.shininess = shininess;
        this.detailLevel = detailLevel;
        this.polygonMode = polygonMode;
        this.triangles = triangles;
        this.volume = volume;
        this.area = area;
        this.isClodMesh = isClodMesh;
        this.opacity = opacity;
        this.surfaceMask = surfaceMask;

        // change the object opacity
        BranchGroup root = surface;
        Shape3D shape = (Shape3D) root.getChild(0);
        Appearance appearance = shape.getAppearance();
        TransparencyAttributes tap = new TransparencyAttributes();

        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_READ);

        if ((1 - opacity) == 0) {
            tap.setTransparencyMode(TransparencyAttributes.NONE);
        } else {
            tap.setTransparencyMode(TransparencyAttributes.BLENDED);
        }

        tap.setSrcBlendFunction(TransparencyAttributes.BLEND_SRC_ALPHA);
        tap.setDstBlendFunction(TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA);
        tap.setTransparency(1 - opacity); // 0 = Opaque

        // appearance.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
        appearance.setTransparencyAttributes(tap);
    }


    /**
     * Constructs new attributes structure to hold information needed for displaying each surface.
     *
     * @param  surface      Surface subtree.
     * @param  fullPath     The full path to the surface file.
     * @param  name         Name of surface.
     * @param  color        Color of surface.
     * @param  shininess    Shininess of surface.
     * @param  detailLevel  Level of detail.
     * @param  polygonMode  Fill, Line, etc.
     * @param  triangles    Number of triangles in surface.
     * @param  volume       Volume of mesh.
     * @param  area         Area of mesh.
     * @param  isClodMesh   Flag stating if this is a clod mesh.
     * @param  opacity      opacity of the surface
     * @param  isVOIPt      VOI point or not.
     * @param  surfaceMask  Surface volume bit set mask.
     */
    public SurfaceAttributes(BranchGroup surface, String fullPath, String name, Color4f color, int shininess,
                             int detailLevel, int polygonMode, int triangles, float volume, float area,
                             boolean isClodMesh, float opacity, boolean isVOIPt, BitSet surfaceMask) {

        this.surface = surface;
        this.fullPath = fullPath;
        this.name = name;
        this.color = color;
        this.shininess = shininess;
        this.detailLevel = detailLevel;
        this.polygonMode = polygonMode;
        this.triangles = triangles;
        this.volume = volume;
        this.area = area;
        this.isClodMesh = isClodMesh;
        this.opacity = opacity;
        this.isVOIPt = isVOIPt;
        this.surfaceMask = surfaceMask;

        BranchGroup root = surface;

        for (int i = 0; i < root.numChildren(); i++) {
            Shape3D shape = (Shape3D) (((Sphere) (((TransformGroup) (root.getChild(i))).getChild(0))).getShape());
            Appearance appearance = shape.getAppearance();
            TransparencyAttributes tap = new TransparencyAttributes();

            tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);

            if ((1 - opacity) == 0) {
                tap.setTransparencyMode(TransparencyAttributes.NONE);
            } else {
                tap.setTransparencyMode(TransparencyAttributes.BLENDED);
            }

            tap.setSrcBlendFunction(TransparencyAttributes.BLEND_SRC_ALPHA);
            tap.setDstBlendFunction(TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA);
            tap.setTransparency(1 - opacity); // 0 = Opaque

            // appearance.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
            appearance.setTransparencyAttributes(tap);
        }

    }

    /**
     * Constructs new attributes structure to hold information needed for displaying each surface.
     *
     * @param  surface      Surface subtree.
     * @param  fullPath     The full path to the surface file.
     * @param  name         Name of surface.
     * @param  color        Color of surface.
     * @param  shininess    Shininess of surface.
     * @param  detailLevel  Level of detail.
     * @param  polygonMode  Fill, Line, etc.
     * @param  triangles    Number of triangles in surface.
     * @param  volume       Volume of mesh.
     * @param  area         Area of mesh.
     * @param  isClodMesh   Flag stating if this is a clod mesh.
     * @param  opacity      opacity of the surface
     * @param  center       surface center
     * @param  surfaceMask  surface volume mask
     */
    public SurfaceAttributes(BranchGroup surface, String fullPath, String name, Color4f color, int shininess,
                             int detailLevel, int polygonMode, int triangles, float volume, float area,
                             boolean isClodMesh, float opacity, Point3f center, BitSet surfaceMask) {
        this(surface, fullPath, name, color, shininess, detailLevel, polygonMode, triangles, volume, area, isClodMesh,
             opacity, surfaceMask);
        this.center = center;
    }


}
