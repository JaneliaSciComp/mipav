package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSlices;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;

import java.awt.event.MouseEvent;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

import com.jogamp.opengl.util.Animator;

public class VolumeTriPlanerRenderDTI extends VolumeTriPlanarRender
{
	
    /** */
	private static final long serialVersionUID = 237088312722258458L;
	
	private boolean m_bSlicePickPending = false;


    private boolean m_bSlicePickEnabled = false;

    private boolean updatingFiberTrack = false;


    /**
     * Construct the Volume/Surface/Tri-Planar renderer.
     * @param kParent parent user-interface and frame.
     * @param kAnimator animator used to display the canvas.
     * @param kVolumeImageA volume data and textures for ModelImage A.
     * @param kVolumeImageB volume data and textures for ModelImage B.
     */
    public VolumeTriPlanerRenderDTI( VolumeTriPlanarRender kShared, GLCanvas kCanvas, VolumeTriPlanarInterfaceDTI kParent, Animator kAnimator, 
            VolumeImage kVolumeImageA, VolumeImage kVolumeImageB  )
    {
        super(kShared, kCanvas, kParent, kAnimator, kVolumeImageA, kVolumeImageB );
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

    @Override
	public void init(GLAutoDrawable arg0) {	
    	super.init(arg0);
    	enableSlicePickable(true);
    	setDTIImage( ((VolumeTriPlanarInterfaceDTI)m_kParent).getDTIimage(),
    			((VolumeTriPlanarInterfaceDTI)m_kParent).getEVimage(), ((VolumeTriPlanarInterfaceDTI)m_kParent).getEValueimage());
    }


    /** Rotates the object with a virtual trackball:
     * @param e the MouseEvent
     */
    @Override
	public void mouseDragged(MouseEvent e)
    {
        if ( m_bGeodesicEnabled )
        {
            return;
        }
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

    /** Rotates the object with a virtual trackball:
     * @param e the MouseEvent
     */
    @Override
	public void mousePressed(MouseEvent e)
    {
        super.mousePressed(e);
        if ( m_bGeodesicEnabled )
        {
            return;
        }
        if ( e.isControlDown() && m_bSlicePickEnabled )
        {
            m_iXPick = e.getX();
            m_iYPick = e.getY();
            m_bSlicePickPending = true;
            updatingFiberTrack = true;
        }
    }
    /** Rotates the object with a virtual trackball:
     * @param e the MouseEvent
     */
    @Override
	public void mouseReleased(MouseEvent e)
    {
        super.mouseReleased(e);
        if ( m_bGeodesicEnabled )
        {
            return;
        }
        if ( updatingFiberTrack && m_bSlicePickEnabled ) {
            ((VolumeTriPlanarInterfaceDTI) m_kParent).getParamPanel().closeFiberTractGroup();
            UpdateSceneRotation();
            updatingFiberTrack = false;
        }
    }
    @Override
	protected void Pick()
    {
        super.Pick();

        if ( m_bGeodesicEnabled )
        {
            return;
        }
        
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
                            kP0.scale( pick.B0 );
                            Vector3f kP1 = kMesh.VBuffer.GetPosition3( pick.iV1 ); 
                            kP1.scale( pick.B1 );
                            Vector3f kP2 = kMesh.VBuffer.GetPosition3( pick.iV2 );
                            kP2.scale( pick.B2 );

                            Vector3f pickPoint = Vector3f.add( kP0, kP1 );
                            pickPoint.add( kP2 );

                            if ( m_kDisplayList.get(i) instanceof VolumeSurface )
                            {
                            	pickPoint.sub(m_kDisplayList.get(i).GetTranslate());
                            }
                            
                            pickPoint.scale( 1.0f/m_fX, 1.0f/m_fY, 1.0f/m_fZ );
                            
                            int[] iExtents = m_kVolumeImageA.GetImage().getExtents();
                            pickPoint.scale( iExtents[0]-1, iExtents[1]-1, iExtents[2]-1 );

                            //System.err.println( "Picked " + pickPoint );
                            ((VolumeTriPlanarInterfaceDTI) m_kParent).getParamPanel().diplayTract( (int)pickPoint.X, (int)pickPoint.Y, (int)pickPoint.Z);

                        }


                    }
                }
            }
        }
    }

}
