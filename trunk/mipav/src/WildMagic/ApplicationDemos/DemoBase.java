// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.  The license is available for reading at
// either of the locations:
//     http://www.gnu.org/copyleft/lgpl.html
//     http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package WildMagic.ApplicationDemos;

import java.awt.event.KeyListener;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibApplications.OpenGLApplication.JavaApplication3D;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Rendering.FrameBuffer;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.Shaders.CompiledProgramCatalog;
import WildMagic.LibGraphics.Shaders.ImageCatalog;
import WildMagic.LibGraphics.Shaders.PixelProgramCatalog;
import WildMagic.LibGraphics.Shaders.VertexProgramCatalog;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public abstract class DemoBase extends JavaApplication3D
implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = -3628112986816799689L;

	protected Node m_spkScene;
    
    protected Culler m_kCuller = new Culler(0,0,null);

    
    protected Animator m_kAnimator;
    
    protected boolean m_bShared;
    public DemoBase( String title )
    {
        super(title,0,0,512,512, new ColorRGBA(0.635294f,0.917647f,1.0f,1.0f));
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                           m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().setSize( m_iWidth, m_iHeight );  
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );     
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       
        
        String kExternalDirs = getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    }

    public DemoBase( String title, FrameBuffer.BufferingType eBuffering )
    {
        super(title,0,0,512,512, new ColorRGBA(0.635294f,0.917647f,1.0f,1.0f));
        m_eBuffering = eBuffering;
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                           m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().setSize( m_iWidth, m_iHeight );  
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );     
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       
        
        String kExternalDirs = getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    }
    
    public DemoBase( String title, GLCanvas canvas )
    {
        super(title,0,0,512,512, new ColorRGBA(0.635294f,0.917647f,1.0f,1.0f));
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                           m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight, canvas );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().setSize( m_iWidth, m_iHeight );  
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );    
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );         
        
        String kExternalDirs = getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    }
    
    public DemoBase( String title, int width, int height )
    {
        super(title,0,0,width, height, new ColorRGBA(0.635294f,0.917647f,1.0f,1.0f));
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                           m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight ); 
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().setSize( m_iWidth, m_iHeight );  
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );     
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       
        
        String kExternalDirs = getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    }

    public DemoBase ( final String acWindowTitle, int iXPosition,
    		int iYPosition, int iWidth, int iHeight,
            FrameBuffer.FormatType eFormat,
            FrameBuffer.DepthType eDepth, FrameBuffer.StencilType eStencil,
            FrameBuffer.BufferingType eBuffering,
            FrameBuffer.MultisamplingType eMultisampling, final ColorRGBA rkBackgroundColor )
    {
    	super(acWindowTitle,iXPosition,iYPosition,iWidth,iHeight, rkBackgroundColor);
    	m_eFormat = eFormat;
    	m_eDepth = eDepth;
    	m_eStencil = eStencil;
    	m_eBuffering = eBuffering;
    	m_eMultisampling = eMultisampling;
    	
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().setSize( m_iWidth, m_iHeight );  
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );     
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );        
        
        String kExternalDirs = getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    }
    
    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    public Node GetScene()
    {
        return m_spkScene;
    }
    public void SetAnimator( Animator animator )
    {
        m_kAnimator = animator;
    }

    protected String getExternalDirs()
    {
    	String dir = new String(System.getProperty("user.dir"));
    	dir = dir + System.getProperty("file.separator") + "src" + System.getProperty("file.separator") + "WildMagic";
    	return dir;
    }
}
