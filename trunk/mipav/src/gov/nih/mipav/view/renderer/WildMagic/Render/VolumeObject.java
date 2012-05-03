package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.util.Vector;

import gov.nih.mipav.model.structures.ModelImage;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.ObjectSystem.GraphicsObject;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.PolygonOffsetState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.Rendering.ZBufferState;

import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Spatial;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * VolumeObect: abstract base class for all rendered objects in the Volume
 * Tri-Planar view.  Stores the reference to the shared VolumeImage object
 * which contains the volume data and the shared textures needed to render the
 * data.
 *
 * @see VolumeRayCast.java
 * @see VolumeBoundingBox.java
 * @see VolumeClip.java
 * @see VolumeDTI.java
 * @see VolumeOrientationCube.java
 * @see VolumeSlices.java
 */
public abstract class VolumeObject
{
    /** boolean to turn rendering on/off for this object. */
    protected boolean m_bDisplay = false;
    
    
    /** boolean to turn picking on/off for this object. */
    protected boolean m_bPickable = false;
    
    /** the scene-graph node containing the rendered object. */
    protected Node m_kScene = null;

    /** a reference to the VolumeImage containing the shared data and textures for display. */
    protected VolumeImage m_kVolumeImageA;

    /** a reference to the VolumeImage containing the shared data and textures for display. */
    protected VolumeImage m_kVolumeImageB;

    /** local translation in the parent scene-graph. */
    protected Vector3f m_kTranslate = new Vector3f();

    /** Culling of this object (front-face, back-face, none) */
    protected CullState m_kCull;

    /** Alpha blending for this object. */
    protected AlphaState m_kAlpha;
    protected ZBufferState m_kZBuffer;

    /** PolygonOffset blending for this object. */
    protected PolygonOffsetState m_kPolygonOffset;

    /** Alpha blending for this object. */
    protected AlphaState m_kAlphaTransparency;
    /** Zbuffer for this object. */
    protected ZBufferState m_kZBufferTransparency;
    protected boolean m_bTransparent = false;

    /** Wire-frame for this object. */
    protected WireframeState m_kWireframe = null;
    
    /** Volume coordinates of the data (extents * resolutions): */
    protected float m_fX, m_fY, m_fZ;

    /** Surface light shader for rendering objects without volume-texture mapping. */
    protected MipavLightingEffect m_kLightShader = null;
    

    protected Vector<Spatial> m_kDeleteList = new Vector<Spatial>();
    
    /** Create a new VolumeObject with the VolumeImage parameter.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate translation in the scene-graph for this object.
     * @param fX the size of the volume in the x-dimension (extent * resolutions)
     * @param fY the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeObject (VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ)
    {
        this( kImageA, null, kTranslate, fX, fY, fZ);
    }
    

    /** Create a new VolumeObject with the VolumeImage parameter.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     */
    public VolumeObject (VolumeImage kImageA, VolumeImage kImageB)
    {
        m_kVolumeImageA = kImageA;
        m_kVolumeImageB = kImageB;
    }


    /** Create a new VolumeObject with the VolumeImage parameter.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate translation in the scene-graph for this object.
     * @param fX the size of the volume in the x-dimension (extent * resolutions)
     * @param fY the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeObject (VolumeImage kImageA, VolumeImage kImageB, Vector3f kTranslate, float fX, float fY, float fZ)
    {
        m_kVolumeImageA = kImageA;
        m_kVolumeImageB = kImageB;
        m_kTranslate.Copy(kTranslate);
        
        m_fX = fX;
        m_fY = fY;
        m_fZ = fZ;  

        m_kAlphaTransparency = new AlphaState();
        m_kAlphaTransparency.BlendEnabled = true;
        m_kAlphaTransparency.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlphaTransparency.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        
        m_kZBufferTransparency = new ZBufferState();
        m_kZBufferTransparency.Enabled = true;
        m_kZBufferTransparency.Writable = false;
    }
    
    /**
     * Set object blend value.
     * @param fValue blend value.
     */
    public void Blend( @SuppressWarnings("unused")float fValue ) {}

    /** delete local memory. */
    public void dispose(Renderer kRenderer)
    {
        m_kVolumeImageA = null;
        m_kTranslate = null;
        if ( m_kScene != null )
        {
        	kRenderer.ReleaseAllResources(m_kScene);
            m_kScene.dispose();
            m_kScene = null;
        }
        if ( m_kCull != null )
        {
            m_kCull = null;
        }
        if ( m_kAlpha != null )
        {
            m_kAlpha = null;
        }
        if ( m_kLightShader != null )
        {
        	kRenderer.ReleaseResources(m_kLightShader);
            m_kLightShader.dispose();
            m_kLightShader = null;
        }
    }
    /**
     * Paint can support.
     * @param kRecord pick record.
     * @param kPaintColor paint color.
     * @param rkPickPoint, picked point.
     */
    public void Dropper(@SuppressWarnings("unused") PickRecord kRecord,
            @SuppressWarnings("unused") ColorRGBA rkDropperColor, 
            @SuppressWarnings("unused") Vector3f rkPickPoint ) {} 

    /**
     * Erase paint.
     * @param kRenderer Renderer.
     * @param kRecord pick record.
     * @param iBrushSize brush size.
     */
    public void Erase(@SuppressWarnings("unused") Renderer kRenderer, 
            @SuppressWarnings("unused") PickRecord kRecord,
            @SuppressWarnings("unused") int iBrushSize ) {}

    /**
     * Return true if back-face culling is on, false otherwise.
     * @return true if back-face culling is on, false otherwise.
     */
    public boolean GetBackface()
    {
        if ( m_kCull != null )
        {
            return m_kCull.Enabled;
        }
        return false;
    } 

    /**
     * Get the object display either on/off.
     * @return when true display this object, when false do not display the
     * object.
     */
    public boolean GetDisplay()
    {
        return m_bDisplay;
    }

    /**
     * Return the TriMesh object (if, any) associated with this object.
     * @return the TriMesh object (if, any) associated with this object.
     */
    public TriMesh GetMesh() { return null; }
    
    /**
     * Return the name of this object.
     * @return name of this object.
     */
    public String GetName()
    {
        return null;
    }

    /**
     * Get the object display either on/off.
     * @return when true display this object, when false do not display the
     * object.
     */
    public boolean GetPickable()
    {
        return (m_bDisplay&&m_bPickable);
    }
    
    /**
     * Return the Wireframe fill mode (FILL, LINE, POINT).
     * @return the Wireframe fill mode (FILL, LINE, POINT).
     */
    public WireframeState.FillMode GetPolygonMode()
    {
        if ( m_kWireframe != null )
        {
            return m_kWireframe.Fill;
        }
        return WireframeState.FillMode.FM_QUANTITY;
    }
    /**
     * Get the object's parent node in the scene graph.
     * @param m_kScene the Node containing this object.
     */
    public Node GetScene()
    {
        return m_kScene;
    }
    
    /**
     * Return the translation vector.
     * @return translation vector.
     */
    public Vector3f GetTranslate()
    {
        return new Vector3f(m_kTranslate);
    }

    /**
     * Painting support.
     * @param kRenderer Renderer.
     * @param kRecord pick record.
     * @param kPaintColor paint color.
     * @param iBrushSize brush size.
     */
    public void Paint(@SuppressWarnings("unused") Renderer kRenderer,
            @SuppressWarnings("unused") PickRecord kRecord, 
            @SuppressWarnings("unused") ColorRGBA kPaintColor, 
            @SuppressWarnings("unused") int iBrushSize ) {}
    /** 
     * Render the object after all other objects have been rendererd. Useful
     * for screen-space objects such as the eye-clip plane.
     * @param kRenderer the OpenGLRenderer object.
     * @param kCuller the Culler object.
     */
    public void PostRender( @SuppressWarnings("unused") Renderer kRenderer, 
            @SuppressWarnings("unused") Culler kCuller ) {}
    
    /**
     * Render the object.
     * @param kRenderer the OpenGLRenderer object.
     * @param kCuller the Culler object.
     */
    public abstract void Render( Renderer kRenderer, Culler kCuller, boolean bPreRender, boolean bSolid );
    
    /**
     * Set back-face culling on/off.
     * @param bOn on/off.
     */
    public void SetBackface( boolean bOn )
    {
        if ( m_kCull != null )
        {
            m_kCull.Enabled = bOn;
            m_kCull.CullFace = CullState.CullMode.CT_BACK;
        }
    }
    
    /**
     * Set the object color.
     * @param kColor new color.
     */
    public void SetColor( @SuppressWarnings("unused") ColorRGB kColor, @SuppressWarnings("unused") boolean bUpdate ){}
    
    /**
     * Set the object display to on/off.
     * @param bDisplay when true display this object, when false do not
     * display the object.
     */
    public void SetDisplay( boolean bDisplay )
    {
        m_bDisplay = bDisplay;
    }
    /**
     * Sets front-face culling on/off.
     * @param bOn on/off.
     */
    public void SetFrontface( boolean bOn )
    {
        if ( m_kCull != null )
        {
            m_kCull.Enabled = bOn;
            m_kCull.CullFace = CullState.CullMode.CT_FRONT;
        }
    }
    
    /**
     * Sets the light for the EllipsoidsShader.
     * @param kLightType the name of the light to set (Light0, Light1, etc.)
     * @param afType the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        if ( m_kLightShader != null )
        {
            m_kLightShader.SetLight(kLightType, afType);
        }
    }
    
    /**
     * Enables/disables picking for this object.
     * @param bPickable picking on/off.
     */
    public void SetPickable( boolean bPickable )
    {
        m_bPickable = bPickable;
    }
    

    /**
     * Enables/disables wireframe and sets the mode: FILL, LINE, POINT.
     * @param bEnable turns the Wireframe State on/off.
     * @param eType wireframe mode: FILL, LINE, POINT.
     */
    public void SetPolygonMode( boolean bEnable, WireframeState.FillMode eType )
    {
        if ( m_kWireframe != null )
        {
            m_kWireframe.Enabled = bEnable;
            m_kWireframe.Fill = eType;
        }
    }

    /**
     * Copy translation vector.
     * @param kTranslate new translation amount.
     */
    public void SetTranslate(Vector3f kTranslate)
    {
        m_kTranslate.Copy(kTranslate);
    }
    
    /**
     * Add to the object translation vector.
     * @param kTranslate new translation amount.
     */
    public void Translate(Vector3f kTranslate)
    {
        m_kTranslate.Add(kTranslate);
    }
        
    protected void scale( VertexBuffer kVertexBuffer )
    {
        ModelImage kImageA = m_kVolumeImageA.GetImage();
        Vector3f kVolumeScale = new Vector3f(m_kVolumeImageA.GetScaleX(), m_kVolumeImageA.GetScaleY(), m_kVolumeImageA.GetScaleZ()  );
        Vector3f kExtentsScale = new Vector3f(1f/(kImageA.getExtents()[0] - 1), 
                1f/(kImageA.getExtents()[1] - 1), 
                1f/(kImageA.getExtents()[2] - 1)  );

        for ( int i = 0; i < kVertexBuffer.GetVertexQuantity(); i++ )
        {
            Vector3f kPos = new Vector3f();
            kVertexBuffer.GetPosition3(i, kPos);
            kPos.Mult(kExtentsScale);
            if ( kVertexBuffer.GetAttributes().HasTCoord(0) )
            {            	
            	kVertexBuffer.SetTCoord3(0, i, kPos);
            }
            kPos.Mult(kVolumeScale);
            kVertexBuffer.SetPosition3(i, kPos);
        }
    }
}
