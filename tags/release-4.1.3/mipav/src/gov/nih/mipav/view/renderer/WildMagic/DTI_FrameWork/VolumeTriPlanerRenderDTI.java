package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import com.sun.opengl.util.*;
import java.awt.event.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;

import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Collision.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibRenderers.OpenGLRenderer.*;

public class VolumeTriPlanerRenderDTI extends VolumeTriPlanarRender
{
    public VolumeTriPlanerRenderDTI( )
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

    public void loadImage( VolumeTriPlanarInterfaceDTI kParent, Animator kAnimator, VolumeImage kVolumeImageA, ModelImage kImageA, ModelLUT kLUTa, ModelRGB kRGBTa,
            VolumeImage kVolumeImageB, ModelImage kImageB, ModelLUT kLUTb, ModelRGB kRGBTb  )
    {

        m_kParent = kParent;
        m_kAnimator = kAnimator;
        m_kVolumeImageA = kVolumeImageA;
        m_kImageA = kImageA;
        m_kLUTa = kLUTa;
        m_kRGBTa = kRGBTa;

        m_kVolumeImageB = kVolumeImageB;
        m_kImageB = kImageB;
        m_kLUTb = kLUTb;
        m_kRGBTb = kRGBTb;

        m_kRotate.FromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/18.0f);
    }

    protected void Pick()
    {
        super.Pick();

        Vector3f kPos = new Vector3f(0,0,10);
        Vector3f kDir = new Vector3f(0,0,1);  // the pick ray

        if ( m_bSlicePickPending )
        {
            if (m_spkCamera.GetPickRay(m_iXPick,m_iYPick,GetWidth(),
                    GetHeight(),kPos,kDir))
            {
                m_bSlicePickPending = false;

                for ( int i = 0; i < m_kDisplayList.size(); i++ )
                {
                    if ( m_kDisplayList.get(i).GetPickable() )
                    {

                        m_kPicker.Execute(m_kDisplayList.get(i).GetScene(),kPos,kDir,0.0f, Float.MAX_VALUE);
                        if (m_kPicker.Records.size() > 0)
                        {
                            PickRecord pick = m_kPicker.GetClosestToZero();
                            TriMesh kMesh = (TriMesh)(pick.Intersected);

                            Vector3f kP0 = kMesh.VBuffer.GetPosition3( pick.iV0 ); 
                            kP0.Scale( pick.B0 );
                            Vector3f kP1 = kMesh.VBuffer.GetPosition3( pick.iV1 ); 
                            kP1.Scale( pick.B1 );
                            Vector3f kP2 = kMesh.VBuffer.GetPosition3( pick.iV2 );
                            kP2.Scale( pick.B2 );

                            Vector3f pickPoint = new Vector3f();
                            pickPoint.Add( kP0, kP1 );
                            pickPoint.Add( kP2 );

                            pickPoint.Mult( new Vector3f( 1.0f/m_fX, 1.0f/m_fY, 1.0f/m_fZ ));
                            int[] iExtents = m_kVolumeImageA.GetImage().getExtents();
                            pickPoint.Mult( new Vector3f( iExtents[0], iExtents[1], iExtents[2] ));

                            ((VolumeTriPlanarInterfaceDTI) m_kParent).getParamPanel().diplayTract( (int)pickPoint.X, (int)pickPoint.Y, (int)pickPoint.Z);

                        }


                    }
                }
            }
        }
    }


    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
     */
    public void mousePressed(MouseEvent e)
    {
        super.mousePressed(e);
        if ( e.isControlDown() && m_bSlicePickEnabled )
        {
            m_iXPick = e.getX();
            m_iYPick = e.getY();
            m_bSlicePickPending = true;
            updatingFiberTrack = true;
            ((VolumeTriPlanarInterfaceDTI) m_kParent).getParamPanel().updateCounter();
        }
    }

    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
     */
    public void mouseReleased(MouseEvent e)
    {
        super.mouseReleased(e);
        if ( updatingFiberTrack && m_bSlicePickEnabled ) {
            ((VolumeTriPlanarInterfaceDTI) m_kParent).getParamPanel().addFiberTract();
            updatingFiberTrack = false;
        }
    }

    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
     */
    public void mouseDragged(MouseEvent e)
    {
        if ( !getSculptEnabled() )
        {
            super.mouseDragged(e);
            if ( e.isControlDown() && m_bSlicePickEnabled )
            {
                m_iXPick = e.getX();
                m_iYPick = e.getY();
                m_bSlicePickPending = true;
            }
        }
    }


    /**
     * Enable the tri-planar slice pickable. 
     * @param bEnabled   pickable or not
     */
    public void enableSlicePickable(boolean bEnabled) {
        m_bSlicePickEnabled = bEnabled;

        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i) instanceof VolumeSlices )
            {
                m_kDisplayList.get(i).SetPickable( bEnabled );
            }
        }
    }

    private boolean m_bSlicePickPending = false;
    private boolean m_bSlicePickEnabled = false;
    private boolean updatingFiberTrack = false;

}