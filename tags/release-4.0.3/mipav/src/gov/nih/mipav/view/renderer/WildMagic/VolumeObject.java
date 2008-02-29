package gov.nih.mipav.view.renderer.WildMagic;

import javax.media.opengl.*;
import com.sun.opengl.util.*;
import java.awt.event.*;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Vector;

import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Collision.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

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
    /** Create a new VolumeObject with the VolumeImage parameter.
     * @param kImageA, the VolumeImage containing shared data and textures for
     * rendering.
     */
    public VolumeObject (VolumeImage kImageA)
    {
        m_kVolumeImageA = kImageA;
    }
    
    /** Create a new VolumeObject with the VolumeImage parameter.
     * @param kImageA, the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate, translation in the scene-graph for this object.
     * @param fX, the size of the volume in the x-dimension (extent * resolutions)
     * @param fY, the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ, the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeObject (VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ)
    {
        m_kVolumeImageA = kImageA;
        m_kTranslate = kTranslate;
        
        m_fX = fX;
        m_fY = fY;
        m_fZ = fZ;
    }

    /** delete local memory. */
    public void dispose()
    {
        m_kVolumeImageA = null;
        m_kTranslate = null;
        if ( m_kScene != null )
        {
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
            m_kLightShader.dispose();
            m_kLightShader = null;
        }
    }

    /**
     * PreRender the object, for embedding in the ray-cast volume.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
    public abstract void PreRender( Renderer kRenderer, Culler kCuller );

    /**
     * Render the object.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
    public abstract void Render( Renderer kRenderer, Culler kCuller );

    /** 
     * Render the object after all other objects have been rendererd. Useful
     * for screen-space objects such as the eye-clip plane.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
    public void PostRender( Renderer kRenderer, Culler kCuller ) {}

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
     * Get the object display either on/off.
     * @return when true display this object, when false do not display the
     * object.
     */
    public boolean GetDisplay()
    {
        return m_bDisplay;
    }
    
    /**

     */
    public void SetPickable( boolean bPickable )
    {
        m_bPickable = bPickable;
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
     * Get the object's parent node in the scene graph.
     * @param m_kScene, the Node containing this object.
     */
    public Node GetScene()
    {
        return m_kScene;
    }
    

    /**
     * Sets the light for the EllipsoidsShader.
     * @param kLightType, the name of the light to set (Light0, Light1, etc.)
     * @param afType, the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        if ( m_kLightShader != null )
        {
            m_kLightShader.SetLight(kLightType, afType);
        }
    }


    public void SetPolygonMode( boolean bEnable, WireframeState.FillMode eType )
    {
        if ( m_kWireframe != null )
        {
            m_kWireframe.Enabled = bEnable;
            m_kWireframe.Fill = eType;
        }
    }

    public String GetName()
    {
        return null;
    }

    public void SetBackface( boolean bOn )
    {
        if ( m_kCull != null )
        {
            m_kCull.Enabled = bOn;
        }
    } 

    public void Blend( float fValue ) {}
    
    public void SetColor( ColorRGB kColor ){}

    public void Paint(Renderer kRenderer, PickRecord kRecord, ColorRGBA kPaintColor, int iBrushSize ) {}
    public void Dropper(PickRecord kRecord, ColorRGBA rkDropperColor, Vector3f rkPickPoint ) {}
    public void Erase(Renderer kRenderer, PickRecord kRecord, int iBrushSize ) {}

    /** boolean to turn rendering on/off for this object. */
    protected boolean m_bDisplay = false;
    /** boolean to turn picking on/off for this object. */
    protected boolean m_bPickable = false;
    /** the scene-graph node containing the rendered object. */
    protected Node m_kScene = null;
    /** a reference to the VolumeImage containing the shared data and textures for display. */
    protected VolumeImage m_kVolumeImageA;
    /** local translation in the parent scene-graph. */
    protected Vector3f m_kTranslate;
    /** Culling of this object (front-face, back-face, none) */
    protected CullState m_kCull;
    /** Alpha blending for this object. */
    protected AlphaState m_kAlpha;
    /** Wireframe for this object. */
    protected WireframeState m_kWireframe;
    
    /** Volume coordinates of the data (extents * resolutions): */
    protected float m_fX, m_fY, m_fZ;
    

    protected MipavLightingEffect m_kLightShader = null;
}
