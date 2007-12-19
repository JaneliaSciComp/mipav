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
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

public abstract class VolumeObject
{
    public VolumeObject (VolumeImage kImageA)
    {
        m_kVolumeImageA = kImageA;
    }
    
    public VolumeObject (VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ)
    {
        m_kVolumeImageA = kImageA;
        m_kTranslate = kTranslate;
        
        m_fX = fX;
        m_fY = fY;
        m_fZ = fZ;
    }

    public abstract void PreRender( Renderer kRenderer, Culler kCuller );

    public abstract void Render( Renderer kRenderer, Culler kCuller );

    public void PostRender( Renderer kRenderer, Culler kCuller ) {}

    public void Display( boolean bDisplay )
    {
        m_bDisplay = bDisplay;
    }

    public boolean Display()
    {
        return m_bDisplay;
    }

    public Node GetScene()
    {
        return m_kScene;
    }

    protected boolean m_bDisplay = false;
    protected Node m_kScene = null;
    protected VolumeImage m_kVolumeImageA;
    protected Vector3f m_kTranslate;
    protected CullState m_kCull;
    protected AlphaState m_kAlpha;
    
    protected float m_fX;
    protected float m_fY;
    protected float m_fZ;
}
