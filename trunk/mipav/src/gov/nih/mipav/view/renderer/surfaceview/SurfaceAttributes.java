package gov.nih.mipav.view.renderer.surfaceview;


import gov.nih.mipav.model.structures.*;
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
     * Color of surface. The element type is Color4f, although currently the alpha channel does not appear to be
     * supported by Java3D materials (even though the Material API allows the alpha channel to be set for diffuse
     * colors).
     */
    private Color4f mColor;

    /** Detail level of surface; only applies to clod meshes. */
    private int mLevelDetail;

    /** The full path to the surface file. */
    private String mFullPath;

    /**
     * <code>true</code> indicates that this surface is a clod mesh, <code>false</code> that it is a standard triangle
     * mesh; clod meshes can change level of detail, triangle meshes cannot.
     */
    private boolean mIsClodMesh;

    /** VOI point. */
    private boolean mIsVOIPt = false;

    /** Name of surface displayed in list box. */
    private String mName;

    /** opacity of surface. */
    private float mOpacity;

    /**
     * Polygon mode of surface, one of PolygonAttribute.POLYGON_FILL, PolygonAttribute.POLYGON_LINE, or
     * PolygonAttribute.POLYGON_POINT.
     */
    private int mPolygonMode;

    /** Shininess of surface. */
    private int mShininess;

    /** Surface subtree, holds all the Shape3D objects that make up the surface. */
    private BranchGroup mSurfaceBranchGroup;

    /** Surface volume bit set mask. */
    private BitSet mSurfaceMask;

    /** Number of triangles displayed associated with the surface. Changes with level of detail. */
    private int mNumberTriangles;

    /** Volume of triangle mesh displayed associated with the surface. Changes with level of detail. */
    private float mVolume;

    private Material mMaterial;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    public SurfaceAttributes( ModelTriangleMesh[] mesh, String fullPath, String name, int triangles, float volume, float area, Point3f center )
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
        this.mOpacity = 0.5f;
        this.mSurfaceMask = null;
        
        this.setColor( new Color4f( 1, 0, 0, 1 ) ); 
    }

    public void setMesh( ModelTriangleMesh[] mesh )
    {
        mTriangleMesh = mesh;
    }


    public ModelTriangleMesh[] getMesh()
    {
        return mTriangleMesh;
    }

    public void setShape( Shape3D[] surfaceShape )
    {
        mSurfaceShape = surfaceShape;
    }

    public Shape3D[] getShape()
    {
        return mSurfaceShape;
    }

    public void setMaterial( Material material )
    {
        mMaterial = material;
        Color3f diffuse = new Color3f();
        mMaterial.getDiffuseColor( diffuse );
        mColor.x = diffuse.x;
        mColor.y = diffuse.y;
        mColor.z = diffuse.z;
    }

    public Material getMaterial()
    {
        return mMaterial;
    }

    public void setOpacity( float opacity )
    {
        mOpacity = opacity;
    }

    public float getOpacity()
    {
        return mOpacity;
    }

    public void setLevelDetail( int levelDetail )
    {
        mLevelDetail = levelDetail;
    }

    public int getLevelDetail()
    {
        return mLevelDetail;
    }


    public void setBranch( BranchGroup branch )
    {
        mSurfaceBranchGroup = branch;
    }
    
    public BranchGroup getBranch() 
    {
        return mSurfaceBranchGroup;
    }

    public void setMask( BitSet mask )
    {
        mSurfaceMask = mask;
    }

    public BitSet getMask()
    {
        return mSurfaceMask;
    }

    public boolean getIsVOIPt()
    {
        return mIsVOIPt;
    }

    public void setIsClodMesh( boolean isClod )
    {
        mIsClodMesh = isClod;
    }

    public boolean getIsClodMesh()
    {
        return mIsClodMesh;
    }

    public String getName()
    {
        return mName;
    }
    
    public String getFullPath()
    {
        return mFullPath;
    }

    public void setColor( Color4f color )
    {
        mColor = color;

        if ( mMaterial == null )
        {
            mMaterial = new Material();
            mMaterial.setCapability(Material.ALLOW_COMPONENT_READ);
            mMaterial.setCapability(Material.ALLOW_COMPONENT_WRITE);
        }
        mMaterial.setDiffuseColor( mColor.x, mColor.y, mColor.z, mColor.w);
        mMaterial.setSpecularColor( mColor.x, mColor.y, mColor.z);
        mMaterial.setAmbientColor( mColor.x, mColor.y, mColor.z);
    }

    public Color4f getColor()
    {
        return mColor;
    }

    public void setPolygonMode( int mode )
    {
        mPolygonMode = mode;
    }

    public int getPolygonMode()
    {
        return mPolygonMode;
    }

    public void setVolume( float volume )
    {
        mVolume = volume;
    }

    public float getVolume()
    {
        return mVolume;
    }

    public void setArea( float area )
    {
        mArea = area;
    }

    public float getArea()
    {
        return mArea;
    }

    public void setNumberTriangles( int nTriangles )
    {
        mNumberTriangles = nTriangles;
    }

    public int getNumberTriangles()
    {
        return mNumberTriangles;
    }

    public Point3f getCenter()
    {
        return mCenter;
    }

}
