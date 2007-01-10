package gov.nih.mipav.view.renderer.surfaceview;


import gov.nih.mipav.model.structures.*;
import java.util.*;
import javax.media.j3d.*;
import javax.vecmath.*;


/**
 * Class that holds the information about each surface; the BranchGroup which
 * holds the surface subtree, the name of the surface in the dialog, the color
 * of the surface, the shininess of the surface, the level of detail (for clod
 * meshes), the number of triangles (changes with level of detail), the
 * polygon mode (fill, line, or point), and a flag indicating if this is a
 * clod mesh.
 */
public class SurfaceAttributes {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Area of triangle mesh displayed associated with the surface. Changes
     * with level of detail. */
    private float mArea;

    /** Surface center. */
    private Point3f mCenter;

    /** Clod mesh. */
    private ModelClodMesh mClodMesh;

    /** ModelTriangleMesh. */
    private ModelTriangleMesh[] mTriangleMesh;

    /** Shape3D[] array */
    private Shape3D[] mSurfaceShape;

    /**
     * Color of surface. The element type is Color4f, although currently the
     * alpha channel does not appear to be supported by Java3D materials (even
     * though the Material API allows the alpha channel to be set for diffuse
     * colors).
     */
    private Color4f mColor;

    /** Detail level of surface; only applies to clod meshes. */
    private int mLevelDetail;

    /** The full path to the surface file. */
    private String mFullPath;

    /**
     * <code>true</code> indicates that this surface is a clod mesh,
     * <code>false</code> that it is a standard triangle mesh; clod meshes can
     * change level of detail, triangle meshes cannot.
     */
    private boolean mIsClodMesh;

    /** VOI point. */
    private boolean mIsVOIPt = false;

    /** Name of surface displayed in list box. */
    private String mName;

    /** opacity of surface. */
    private float mOpacity;

    /**
     * Polygon mode of surface, one of PolygonAttribute.POLYGON_FILL,
     * PolygonAttribute.POLYGON_LINE, or PolygonAttribute.POLYGON_POINT.
     */
    private int mPolygonMode;

    /** Shininess of surface. */
    private int mShininess;

    /** Surface subtree, holds all the Shape3D objects that make up the
     * surface. */
    private BranchGroup mSurfaceBranchGroup;

    /** Surface volume bit set mask. */
    private BitSet mSurfaceMask;

    /** Number of triangles displayed associated with the surface. Changes
     * with level of detail. */
    private int mNumberTriangles;

    /** Volume of triangle mesh displayed associated with the surface. Changes
     * with level of detail. */
    private float mVolume;

    /** Surface Material object. */
    private Material mMaterial;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /** Constructor. Most common usage of SurfaceAttributes. 
     * @param mesh, array of ModelTriangleMesh objects the SurfaceAttributes describes
     * @param fullPath, directory/file name containing the triangle mesh
     * @param name, file name
     * @param triangles, mesh triangle count 
     * @param volume, volume enclosed in mesh
     * @param area, mesh surface area
     * @param center, 3D center point of mesh.
     */
    public SurfaceAttributes( ModelTriangleMesh[] mesh,
                              String fullPath, String name,
                              int triangles, float volume, float area, Point3f center )
    {
        this.mTriangleMesh = mesh;
        this.mClodMesh = null;
        this.mSurfaceBranchGroup = null;
        this.mFullPath = fullPath;
        this.mName = name;
        this.mShininess = 64;
        this.mLevelDetail = 100;
        this.mPolygonMode = PolygonAttributes.POLYGON_FILL;
        this.mNumberTriangles = triangles;
        this.mVolume = volume;
        this.mArea = area;
        this.mCenter = center;
        this.mIsVOIPt = false;
        this.mIsClodMesh = false;
        this.mSurfaceMask = null;
        /* sets the default color */
        this.setColor( new Color4f( 1, 0, 0, 1 ) ); 
    }

    // Access functions: 

    /**
     * sets the array of ModelTriangleMesh objects:
     * @param mesh, array of ModelTriangleMesh objects.
     */
    public void setMesh( ModelTriangleMesh[] mesh )
    {
        mTriangleMesh = mesh;
    }

    /**
     * returns the array of ModelTriangleMesh objects.
     * @return mTriangleMesh, the array of ModelTriangleMesh objects.
    */
    public ModelTriangleMesh[] getMesh()
    {
        return mTriangleMesh;
    }

    /**
     * sets the array of Shape3D objects:
     * @param surfaceShape, array of Shape3D objects.
     */
    public void setShape( Shape3D[] surfaceShape )
    {
        mSurfaceShape = surfaceShape;
    }

    /**
     * returns the array of Shape3D objects.
     * @return mSurfaceShape, the array of Shape3D objects.
     */
    public Shape3D[] getShape()
    {
        return mSurfaceShape;
    }

    /**
     * sets the surface Material. the Material diffuse color is used to set
     * the color value as well.
     * @param material, new surface Material.
     */
    public void setMaterial( Material material )
    {
        mMaterial = material;
        if ( mMaterial != null )
        {
            Color3f diffuse = new Color3f();
            mMaterial.getDiffuseColor( diffuse );
            mColor.x = diffuse.x;
            mColor.y = diffuse.y;
            mColor.z = diffuse.z;
        }
    }

    /**
     * returns the Material.
     * @return mMaterial, the surface Material.
     */
    public Material getMaterial()
    {
        return mMaterial;
    }

    /**
     * sets the surface Opacity. 
     * @param opacity, new opacity value.
     */
    public void setOpacity( float opacity )
    {
        mOpacity = opacity;
    }

    /**
     * returns the surface opacity
     * @return mOpacity, surface opacity.
     */
    public float getOpacity()
    {
        return mOpacity;
    }

    /**
     * sets the level of detail for the ClodMesh
     * @param levelDetail, the level of detail
     */
    public void setLevelDetail( int levelDetail )
    {
        mLevelDetail = levelDetail;
    }

    /**
     * returns the level of detail
     * @return mLevelDetail, the level of detail
     */
    public int getLevelDetail()
    {
        return mLevelDetail;
    }

    /**
     * sets the BranchGroup in the scene graph that renders the surface.
     * @param branch, BranchGroup in the scenegraph
     */
    public void setBranch( BranchGroup branch )
    {
        mSurfaceBranchGroup = branch;
    }
    
    /**
     * returns the BranchGroup from the scene graph
     * @return mSurfaceBranchGroup, the BranchGroup containing the mesh
     */
    public BranchGroup getBranch() 
    {
        return mSurfaceBranchGroup;
    }

    /**
     * Sets the volume mask data -- the intersection of the triangle mesh with
     * the volume data.
     * @param mask, the BitSet mask data
     */
    public void setMask( BitSet mask )
    {
        mSurfaceMask = mask;
    }

    /**
     * returns the surface mask data
     * @return mSurfaceMask, intersection of the triangle mesh with the volume data.
     */
    public BitSet getMask()
    {
        return mSurfaceMask;
    }

    /**
     * Returns true is the mesh described in the SurfaceAttributes is a VOI.
     * @return mIsVOIPt, true when the mesh is a VOI, false otherwise.
     */
    public boolean getIsVOIPt()
    {
        return mIsVOIPt;
    }

    /**
     * Sets the clod mesh flag.
     * @param isClod, when true the mesh is of type ClodMesh, when false the
     * mesh is ModelTriangleMesh
     */
    public void setIsClodMesh( boolean isClod )
    {
        mIsClodMesh = isClod;
    }

    /**
     * Returns if the mesh is type ClodMesh or ModelTriangleMesh.
     * @return mIsClodMesh, true when the mesh is type ClodMesh, false when
     * the mesh is type ModelTriangleMesh
     */
    public boolean getIsClodMesh()
    {
        return mIsClodMesh;
    }

    /**
     * Returns the file name of the surface, if the surface was loaded from file.
     * @return mName, the surface file name, or surface name.
     */
    public String getName()
    {
        return mName;
    }
    
    /**
     * Returns the file pathname of the surface, if the surface was loaded from file.
     * @return mFullPath, the surface pathname, or null.
     */
    public String getFullPath()
    {
        return mFullPath;
    }

    /**
     * Restores the ModelTriangleMesh per-vertex colors to the surface
     * color. Used to clear all paint or remove the per-vertex texture-based
     * color.
     */
    public void restoreVertexColors()
    {
        setColor( mColor );
    }

    /**
     * Sets the surface color. Also sets the Material diffuse, specular, and
     * ambient colors.
     * @param color, the new surface Color.
     */
    public void setColor( Color4f color )
    {
        mColor = color;

        if ( mMaterial == null )
        {
            mMaterial = new Material();
            mMaterial.setCapability(Material.ALLOW_COMPONENT_READ);
            mMaterial.setCapability(Material.ALLOW_COMPONENT_WRITE);
            mMaterial.setColorTarget( Material.AMBIENT_AND_DIFFUSE );
            mMaterial.setEmissiveColor( 0f, 0f, 0f );
            mMaterial.setSpecularColor( 0f, 0f, 0f );
        }
        mMaterial.setAmbientColor( mColor.x, mColor.y, mColor.z);
        mMaterial.setDiffuseColor( mColor.x, mColor.y, mColor.z, mColor.w);

        for ( int i = 0; i < mTriangleMesh.length; i++ )
        {
            Color4f[] akColors = new Color4f[ mTriangleMesh[i].getVertexCount() ];
            for ( int j = 0; j < mTriangleMesh[i].getVertexCount(); j++ )
            {
                akColors[j] = new Color4f( color.x, color.y, color.z, 1 - mOpacity );
            }
            mTriangleMesh[i].setColors( 0, akColors );
        }
    }

    /**
     * Gets the surface color.
     * @return mColor, the surface color. 
     */
    public Color4f getColor()
    {
        return mColor;
    }

    /**
     * Gets a Color3f copy of the surface color.
     * @return a Color3f copy of the surface color.
     */
    public Color3f getColor3()
    {
        return new Color3f( mColor.x, mColor.y, mColor.z );
    }

    /**
     * Sets the surface polygon mode. PolygonAttributes.POLYGON_FILL,
     * PolygonAttributes.POLYGON_LINE, or PolygonAttributes.POLYGON_POINT
     * @param mode, PolygonAttributes mode
     */
    public void setPolygonMode( int mode )
    {
        mPolygonMode = mode;
    }

    /**
     * Returns the polgyon mode.
     * @return mPolygonMode PolygonAttributes.POLYGON_FILL,
     * PolygonAttributes.POLYGON_LINE, or PolygonAttributes.POLYGON_POINT
     */
    public int getPolygonMode()
    {
        return mPolygonMode;
    }

    /**
     * Sets the value of the surface volume.
     * @param volume, the surface volume contained in the ModelTriangleMesh objects.
     */
    public void setVolume( float volume )
    {
        mVolume = volume;
    }

    /**
     * Gets the surface volume.
     * @return the volume contained within the ModelTriangleMesh objects.
     */
    public float getVolume()
    {
        return mVolume;
    }

    /**
     * Sets the value of the surface area of the ModelTriangleMesh objects.
     * @param area, the surface area of the ModelTriangleMesh objects.
     */
    public void setArea( float area )
    {
        mArea = area;
    }

    /**
     * Return the surface area of the ModelTriangleMesh objects.
     * @return area, the surface area of the ModelTriangleMesh objects.
     */
    public float getArea()
    {
        return mArea;
    }

    /**
     * Sets the number of triangles of the ModelTriangleMesh objects.
     * @param nTriangles, the number of triangles of the ModelTriangleMesh objects.
     */
    public void setNumberTriangles( int nTriangles )
    {
        mNumberTriangles = nTriangles;
    }

    /**
     * Gets the number of triangles of the ModelTriangleMesh objects.
     * @return nNumberTriangles, the number of triangles of the ModelTriangleMesh objects.
     */
    public int getNumberTriangles()
    {
        return mNumberTriangles;
    }

    /**
     * Gets the center of the ModelTriangleMesh objects.
     * @return mCenter, the center of the ModelTriangleMesh objects.
     */
    public Point3f getCenter()
    {
        return mCenter;
    }
}
