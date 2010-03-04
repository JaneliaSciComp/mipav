package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceExtractorCubes;

import java.awt.Frame;
import java.awt.event.KeyListener;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Meshes.VETMesh;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.sun.opengl.util.Animator;

public class VolumeImageExtract extends VolumeImageViewer
    implements GLEventListener, KeyListener
{
    /**  */
    private static final long serialVersionUID = 5118167091752659025L;
    private static int ms_iSurface = 0;
    private VolumeCalcEffect m_spkEffect2;
    private GraphicsImage m_kCalcImage;
    private Texture m_pkVolumeCalcTarget;
    private SurfaceExtractImage m_kCalcImage2;
    private Texture m_pkVolumeCalcTarget2;
    private boolean m_bDisplayFirst = true;
    private boolean m_bDisplaySecond = true;
    private VolumeClipEffect m_kClipEffect = null;
    private int[] m_aiNewExtents = new int[3];
    private float[] m_afNewResolutions = new float[3];
    /** Intensity level for GPU-surface extraction. */
    private int m_iExtractLevel = 1;
    
    public VolumeImageExtract( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, VolumeClipEffect kClip, int iLevel  )
    {
        super(kParentFrame, kVolumeImage );
        m_iExtractLevel = iLevel;
        m_kClipEffect = kClip;
    }
    /**
     * @param args
     */
    public static void main( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, VolumeClipEffect kClip, int iLevel )
    {
        VolumeImageExtract kWorld = new VolumeImageExtract(kParentFrame, kVolumeImage, kClip, iLevel);
        Frame frame = new Frame(kWorld.GetWindowTitle());
        frame.add( kWorld.GetCanvas() );
         final Animator animator = new Animator( kWorld.GetCanvas() );
         // setting the frame to be undecorated removes the frame title bar and edges
         // this prevents flashing on-screen.
         frame.setUndecorated(true);
         // frame must be set to visible for the gl canvas to be properly initialized.
         frame.setVisible(true);
         frame.setBounds(0,0,
                 kWorld.GetWidth(), kWorld.GetHeight() );
         frame.setVisible(false);
         kWorld.SetAnimator(animator);
         kWorld.SetFrame(frame);
         animator.start();
    }

    public void display(GLAutoDrawable arg0) {
        if ( m_kAnimator == null )
        {
            return;
        }      
        if ( m_pkRenderer != null )
        {
            ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        }
        while ( m_bDisplayFirst )
        {
            float fZ = ((float)m_iSlice)/(m_aiNewExtents[2] -1);
            UpdateSlice(fZ);
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkRenderer.EndScene();
            }
            m_pkRenderer.FrameBufferToTexSubImage3D( m_pkVolumeCalcTarget, m_iSlice, false );
            //m_pkRenderer.DisplayBackBuffer();
            m_iSlice++; 
            if ( m_iSlice >= m_aiNewExtents[2])
            {
                m_iSlice = 0;
                m_bDisplayFirst = false;
            }
        }

          while ( m_bDisplaySecond )
          {
              float fZ = ((float)m_iSlice)/(m_aiNewExtents[2] -1);
              UpdateSlice(fZ);
              m_pkPlane.DetachAllEffects();
              m_pkPlane.AttachEffect(m_spkEffect2);
              m_kCuller.ComputeVisibleSet(m_spkScene);
              m_pkRenderer.ClearBuffers();
              if (m_pkRenderer.BeginScene())
              {          
                  m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                  m_pkRenderer.EndScene();
                  //writeImage();
              }
              m_pkRenderer.FrameBufferToTexSubImage3D( m_pkVolumeCalcTarget2, m_iSlice, true );
              //m_pkRenderer.DisplayBackBuffer();
              m_iSlice++; 
              if ( m_iSlice >= m_aiNewExtents[2])
              {
                  m_bDisplaySecond = false;
                  m_iSlice = 0;
                  //System.err.println( m_kCalcImage2.Min + " " + m_kCalcImage2.Max + " " + m_kCalcImage2.TriTable.size() );
                  
                  int[] direction = MipavCoordinateSystems.getModelDirections(m_kVolumeImage.GetImage());
                  float[] startLocation = m_kVolumeImage.GetImage().getFileInfo(0).getOrigin();
                  SurfaceExtractorCubes kExtractor = 
                      new SurfaceExtractorCubes(m_aiNewExtents[0], 
                              m_aiNewExtents[1], 
                              m_aiNewExtents[2], m_kCalcImage2.Data,
                              m_afNewResolutions[0], 
                              m_afNewResolutions[1], 
                              m_afNewResolutions[2], direction,
                          startLocation, null);
                  TriMesh kMesh = kExtractor.getLevelSurface(m_iExtractLevel, m_kCalcImage2.TriTable);
                  if ( kMesh != null )
                  {
//                    Get the adjacent triangles:
                      VETMesh kVETMesh = new VETMesh( 2* kMesh.VBuffer.GetVertexQuantity(), .9f,
                              2 * kMesh.IBuffer.GetIndexQuantity(), .9f,
                              2 * kMesh.GetTriangleQuantity(), .9f,
                              kMesh.IBuffer.GetData() );
                      kMesh.IBuffer = new IndexBuffer( kVETMesh.GetTriangles() );
                      TriMesh[] kMeshes = new TriMesh[1];
                      kMeshes[0] = kMesh;
                      if ( kMeshes[0] != null )
                      {
                          m_kParent.getVolumeGPU().displayVolumeRaycast(false);
                          String kSurfaceName = JDialogBase.makeImageName(m_kVolumeImage.GetImage().getImageName(), ms_iSurface + "_extract.sur");
                          kMeshes[0].SetName( kSurfaceName );
                          m_kParent.getSurfacePanel().addSurfaces(kMeshes);
                          m_kParent.getRendererGUI().setDisplaySurfaceCheck( true );
                          m_kParent.getRendererGUI().setDisplayVolumeCheck( false );
                          ms_iSurface++;
                      }
                  }
                  else
                  {
                    
                  }
                  dispose(arg0);
              }
          }
    }

    public void dispose(GLAutoDrawable arg0)
    {
        m_kClipEffect = null;
        m_aiNewExtents = null;
        m_afNewResolutions = null;
        m_pkVolumeCalcTarget.dispose();
        m_kCalcImage.dispose();
        m_pkVolumeCalcTarget2.dispose();
        m_kCalcImage2.dispose();
        m_spkEffect2.dispose();
        super.dispose(arg0);
    }



    protected void CreateScene ()
    {
        CreatePlaneNode();
        m_aiNewExtents[0] = m_kVolumeImage.GetImage().getExtents()[0];
        m_aiNewExtents[1] = m_kVolumeImage.GetImage().getExtents()[1];
        m_aiNewExtents[2] = m_kVolumeImage.GetImage().getExtents()[2];
        m_aiNewExtents[2] = Math.max( Math.max(m_aiNewExtents[0], m_aiNewExtents[1]),
                m_aiNewExtents[2]);
        float fStep = 1.0f/(m_aiNewExtents[2]-1);

        float[] res = m_kVolumeImage.GetImage().getResolutions(0);
        int[] extents = m_kVolumeImage.GetImage().getExtents();
        for ( int i = 0; i < 3; i++ )
        {
            m_afNewResolutions[i] = (extents[i] * res[i]) / m_aiNewExtents[i];
        }
        
        m_spkEffect = new VolumeCalcEffect( m_kVolumeImage, m_kClipEffect, false );
        m_pkPlane.AttachEffect(m_spkEffect);
        m_pkRenderer.LoadResources(m_pkPlane);
        ((VolumeCalcEffect)m_spkEffect).SetStepSize(fStep, fStep, fStep);
        float fIsoVal = (float)((m_iExtractLevel - m_kVolumeImage.GetImage().getMin()) /
        (m_kVolumeImage.GetImage().getMax() - m_kVolumeImage.GetImage().getMin()));
        ((VolumeCalcEffect)m_spkEffect).SetIsoVal( fIsoVal );
        m_pkPlane.DetachAllEffects();

        

        m_kCalcImage2 = new SurfaceExtractImage(GraphicsImage.FormatMode.IT_RGBA8888,
                                         m_iWidth,m_iHeight,
                                         m_aiNewExtents[2], 
                                         new byte[m_iWidth*m_iHeight*m_aiNewExtents[2]*4],
                                         "VolumeExtract2" );
        m_pkVolumeCalcTarget2 = new Texture();
        m_pkVolumeCalcTarget2.SetImage(m_kCalcImage2);
        m_spkEffect2 = new VolumeCalcEffect( "VolumeExtract2", m_pkVolumeCalcTarget2, "SurfaceExtract_P2" );
        m_pkPlane.AttachEffect(m_spkEffect2);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_spkEffect2.SetStepSize(fStep, fStep, fStep);
        m_pkPlane.DetachAllEffects();
        
        
        m_kCalcImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888,
                                         m_iWidth,m_iHeight,
                                         m_aiNewExtents[2], 
                                         new byte[m_iWidth*m_iHeight*m_aiNewExtents[2]*4],
                                         "VolumeExtract" );
        m_pkVolumeCalcTarget = new Texture();
        m_pkVolumeCalcTarget.SetImage(m_kCalcImage);
        m_spkEffect2 = new VolumeCalcEffect( "VolumeExtract", m_pkVolumeCalcTarget, "SurfaceExtract_P2" );
        m_pkPlane.AttachEffect(m_spkEffect2);
        m_pkRenderer.LoadResources(m_pkPlane);
        (m_spkEffect2).SetStepSize(fStep, fStep, fStep);
        m_pkPlane.DetachAllEffects();
        
        

    }
}
