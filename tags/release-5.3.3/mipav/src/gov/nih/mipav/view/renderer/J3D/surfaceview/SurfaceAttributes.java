package gov.nih.mipav.view.renderer.J3D.surfaceview;


import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;
import java.util.*;
import javax.media.j3d.*;
import javax.vecmath.*;


/**
 * Class that holds the information about each surface displayed in the
 * SurfaceRenderer. It is the main surface data structure used in the
 * JPanelSurface, FileSurface, and SurfaceMask classes.
 *
 * SurfaceAttributes contains one or more triangle-mesh objects. The
 * triangle-meshes may be of type ClodMesh or of type ModelTriangleMesh. The
 * mIsClodMesh data member, when true indicates that the meshes are of type
 * ClodMesh and when false indicates that the meshes are of type
 * ModelTriangleMesh.
 *
 * Each data member represents global properties of all triangle meshes
 * contained in this class. For example the area and volume values are the
 * sums of the volumes and areas of each mesh contained in the object.
 *
 */
public class SurfaceAttributes {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    /** Clod mesh. */
    //private ModelClodMesh mClodMesh;

    /**
     * <code>true</code> indicates that this surface is a clod mesh,
     * <code>false</code> that it is a standard triangle mesh; clod meshes can
     * change level of detail, triangle meshes cannot.
     */
    private boolean mIsClodMesh;

    /** ModelTriangleMesh. */
    private ModelTriangleMesh[] mTriangleMesh;

    /** mSurfaceMask represents the intersection of the ModelImage volume data
     * with the triangle-meshes. It is a filled-volume mask. */
    private BitSet mSurfaceMask;

    /** Surface subtree, holds all the Shape3D objects that make up the
     * surface. */
    private BranchGroup mSurfaceBranchGroup;

    /** Shape3D[] array */
    private Shape3D[] mSurfaceShape;

    /** Color of surface. */
    private Color4f mColor;

    /** Color of surface. */
    private Color4f[][] mPerVertexColors_Backup;

    /** Surface Material. */
    private Material mMaterial;

    /** opacity of surface. */
    private float mOpacity;

    /** Detail level of surface; only applies to clod meshes. */
    private int mLevelDetail;

    /**
     * Polygon mode of surface, one of PolygonAttribute.POLYGON_FILL,
     * PolygonAttribute.POLYGON_LINE, or PolygonAttribute.POLYGON_POINT.
     */
    private int mPolygonMode;

    /** The full path to the surface file. */
    private String mFullPath;

    /** Name of surface displayed in list box. */
    private String mName;

    /** Total number of triangles in the meshes. Changes with level of
     * detail. */
    private int mNumberTriangles;

    /** Total volume of triangle meshes. Changes with level of detail. */
    private float mVolume;

    /** Total area of triangle meshes. Changes with level of detail. */
    private float mArea;

    /** Surface center. */
    private Point3f mCenter;


    //~ Constructors ---------------------------------------------------------------------------------------------------


    /** Constructor. Most common usage of SurfaceAttributes. 
     * @param mesh array of ModelTriangleMesh objects the SurfaceAttributes describes
     * @param fullPath directory/file name containing the triangle mesh
     * @param name file name
     */
    public SurfaceAttributes( ModelTriangleMesh[] mesh,
                              String fullPath, String name )
    {
        this.mTriangleMesh = mesh;
        //this.mClodMesh = null;
        this.mSurfaceBranchGroup = null;
        this.mFullPath = fullPath;
        this.mName = name;
        this.mLevelDetail = 100;
        this.mPolygonMode = PolygonAttributes.POLYGON_FILL;
        this.mIsClodMesh = false;
        this.mSurfaceMask = null;
        /* sets the default color */
        mColor = new Color4f( 1, 0, 0, 1 ); 

        this.mNumberTriangles = 0;
        this.mVolume = 0;
        this.mArea = 0;
        this.mCenter = new Point3f( 0f, 0f, 0f );
        for ( int i = 0; i < mTriangleMesh.length; i++ )
        {
            this.mNumberTriangles += mTriangleMesh[i].getIndexCount() / 3;
            this.mVolume += mTriangleMesh[i].volume();
            this.mArea += mTriangleMesh[i].area();
            this.mCenter.x += mTriangleMesh[i].center().x;
            this.mCenter.y += mTriangleMesh[i].center().y;
            this.mCenter.z += mTriangleMesh[i].center().z;
        }
        this.mCenter.x /= mTriangleMesh.length;
        this.mCenter.y /= mTriangleMesh.length;
        this.mCenter.z /= mTriangleMesh.length;
        
    }

    // Access functions: 

    /**
     * sets the array of ModelTriangleMesh objects:
     * @param mesh array of ModelTriangleMesh objects.
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
     * @param surfaceShape array of Shape3D objects.
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
     * @param material new surface Material.
     */
    public void setMaterial( Material material )
    {
        mMaterial = copyMaterial( material );
        if ( mMaterial != null )
        {
        	if ( mSurfaceShape != null ) {
	            for ( int i = 0; i < mSurfaceShape.length; i++ )
	            {
	                mSurfaceShape[i].getAppearance().setMaterial( mMaterial );
	            }
        	}
            Color3f diffuse = new Color3f();
            mMaterial.getDiffuseColor( diffuse );
            mColor.x = diffuse.x;
            mColor.y = diffuse.y;
            mColor.z = diffuse.z;
            setColor( mColor );
        }
    }

    /**
     * Stores the per-vertex colors when the material is set -- used to
     * restore per-vertex color from the AdvancedMaterials dialog.
     */
    private void backupColors()
    {
        mPerVertexColors_Backup = new Color4f[ mTriangleMesh.length ][];
        for ( int i = 0; i < mTriangleMesh.length; i++ )
        {
            mPerVertexColors_Backup[i] = new Color4f[mTriangleMesh[i].getVertexCount()];
            for ( int j = 0; j < mTriangleMesh[i].getVertexCount(); j++ )
            {
                mPerVertexColors_Backup[i][j] = new Color4f();
                mTriangleMesh[i].getColorLocal( j, mPerVertexColors_Backup[i][j] );
            }
        }
    }

    /**
     * Get the per-vertex color array. 
     * @return   mPerVertexColors_Backup  surface color array.
     */
    public Color4f[][] getPerVertexColorArray() { 
         return mPerVertexColors_Backup;
    }
    
    
    /**
     * Set the per-vertex color array.
     * @param color    stored per vertex color array reference.  
     */
    public void setPerVertexColorArray(Color4f[] color, int index) {
    	if ( mPerVertexColors_Backup == null ) {
    	    mPerVertexColors_Backup = new Color4f[ mTriangleMesh.length ][];
    	}
        
    	mPerVertexColors_Backup[index] = new Color4f[mTriangleMesh[index].getVertexCount()];
        for ( int j = 0; j < color.length; j++ )
        {
        	mPerVertexColors_Backup[index][j] = color[j];
            mTriangleMesh[index].setColorDelay( j,  mPerVertexColors_Backup[index][j] );
        }
        // updates the geometry all at once (faster):
        mTriangleMesh[index].setColorUpdate( );
    }
    
    /**
     * returns a new copy of the Material.
     * @return mMaterial, a new copy of the surface Material.
     */
    public Material getMaterial()
    {
        // first save per-vertex colors:
        backupColors();

        return copyMaterial( mMaterial );
    }

    private Material copyMaterial( Material material )
    {
        Color3f color = new Color3f();
        Material newMaterial = new Material();
        newMaterial.setCapability(Material.ALLOW_COMPONENT_READ);
        newMaterial.setCapability(Material.ALLOW_COMPONENT_WRITE);
        material.getEmissiveColor( color );
        newMaterial.setEmissiveColor( color );
        material.getSpecularColor( color );
        newMaterial.setSpecularColor( color );
        material.getAmbientColor( color );
        newMaterial.setAmbientColor( color );
        material.getDiffuseColor( color );
        newMaterial.setDiffuseColor( color );
        newMaterial.setShininess( material.getShininess() );
        return newMaterial;
    }

    /**
     * sets the surface Opacity. 
     * @param opacity new opacity value.
     */
    public void setOpacity( float opacity )
    {
        mOpacity = opacity;
        mColor.w = 1 - opacity;

        if ( mSurfaceShape != null ) {
	        for (int i = 0; i < mSurfaceShape.length; i++ )
	        {
	            // 0 = Opaque
	            mSurfaceShape[i].getAppearance().getTransparencyAttributes().setTransparency(opacity);
	        }
        }
        
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
     * @param levelDetail the level of detail
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
     * @param branch BranchGroup in the scenegraph
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
     * @param mask the BitSet mask data
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
     * Sets the clod mesh flag.
     * @param isClod when true the mesh is of type ClodMesh, when false the
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
     * Restores the ModelTriangleMesh per-vertex colors to the saved per-vertex colors. 
     * @param material the material to restore.
     */
    public void restorePerVertexColors( Material material )
    {
        this.setMaterial( material );

        // Sets the color, but does not update the geometry yet (faster):
        for ( int i = 0; i < mTriangleMesh.length; i++ )
        {
            for ( int j = 0; j < mPerVertexColors_Backup[i].length; j++ )
            {
                mTriangleMesh[i].setColorDelay( j,  mPerVertexColors_Backup[i][j] );
            }
            // updates the geometry all at once (faster):
            mTriangleMesh[i].setColorUpdate( );
        }
    }

    /**
     * Sets the surface color. Also sets the Material diffuse, specular, and
     * ambient colors.
     * @param color the new surface Color.
     */
    public void setColor( Color4f color )
    {
        mColor = color;

        if ( mMaterial == null )
        {
            mMaterial = new Material();
            mMaterial.setCapability(Material.ALLOW_COMPONENT_READ);
            mMaterial.setCapability(Material.ALLOW_COMPONENT_WRITE);
            mMaterial.setEmissiveColor( 0f, 0f, 0f );
            mMaterial.setSpecularColor( 0f, 0f, 0f );
            mMaterial.setAmbientColor( mColor.x, mColor.y, mColor.z );
        }
        mMaterial.setDiffuseColor( mColor.x, mColor.y, mColor.z);

        for ( int i = 0; i < mTriangleMesh.length; i++ )
        {
            for ( int j = 0; j < mTriangleMesh[i].getVertexCount(); j++ )
            {
                // Sets the color, but does not update the geometry yet (faster):
                mTriangleMesh[i].setColorDelay( j,  mColor );
            }
            // updates the geometry all at once (faster):
            mTriangleMesh[i].setColorUpdate( );
        }
    }

    /**
     * Gets the surface color.
     * @return mColor, the surface color. 
     */
    public ColorRGBA getColorRGBA()
    {
        return new ColorRGBA( mColor.x, mColor.y, mColor.z, mColor.w );
    }
    

    /**
     * Gets the surface color.
     * @return mColor, the surface color. 
     */
    public Color4f getColor4f()
    {
        return new Color4f( mColor.x, mColor.y, mColor.z, mColor.w );
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
     * @param mode PolygonAttributes mode
     */
    public void setPolygonMode( int mode )
    {
        mPolygonMode = mode;
        for ( int i = 0; i < mSurfaceShape.length; i++ )
        {
            mSurfaceShape[i].getAppearance().getPolygonAttributes().setPolygonMode(mode);
        }

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
     * @param volume the surface volume contained in the ModelTriangleMesh objects.
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
     * @param area the surface area of the ModelTriangleMesh objects.
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
     * @param nTriangles the number of triangles of the ModelTriangleMesh objects.
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
