package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import com.sun.opengl.util.*;

import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibRenderers.OpenGLRenderer.*;



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
    }

    
    public void loadImage( VolumeTriPlanarInterface kParent, Animator kAnimator, VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa,
                           VolumeImage kVolumeImageB, ModelImage kImageB, ModelLUT kLUTb,
                           int iPlane, boolean bMemory)
    {
        
        m_kAnimator = kAnimator;
        
        m_kParent = kParent;
    	
        m_kVolumeImageA = kVolumeImageA;
        m_iPlaneOrientation = iPlane;
        m_bMemoryUsage = bMemory;
        
        m_kImageA = kImageA;
        m_kImageB = kImageB;
        m_kImageA.setImageOrder(ModelImage.IMAGE_A);
        
        if (m_kImageB != null) {
            m_kImageB.setImageOrder(ModelImage.IMAGE_B);
        }
        
        setOrientation();
        m_kWinLevel = new WindowLevel();
        m_bModified = true;
    }
    
}
