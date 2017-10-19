package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.view.WindowLevel;
import gov.nih.mipav.view.renderer.WildMagic.PlaneRender_WM;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;


import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.*;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;
import com.jogamp.opengl.util.Animator;

public class PlaneRenderDTI extends PlaneRender_WM
{

    public PlaneRenderDTI()
    {
        super();
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                           m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );     
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseWheelListener( this );          
    }

    
    public void loadImage( VolumeTriPlanarInterface kParent, Animator kAnimator, 
            VolumeImage kVolumeImageA, VolumeImage kVolumeImageB, int iPlane)
    {
        
        m_kAnimator = kAnimator;
        
        m_kParent = kParent;
    	
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        m_iPlaneOrientation = iPlane;
        
        setOrientation();
        m_kWinLevel = new WindowLevel();
        m_bModified = true;
    }
    
}