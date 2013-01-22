package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageCrop;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageExtract;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageSurfaceMask;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeVOI;

import java.awt.Cursor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;

import com.jogamp.opengl.util.Animator;


public class VolumeTriPlanarRender extends VolumeTriPlanarRenderBase
implements GLEventListener, KeyListener, MouseMotionListener,  MouseListener
{

	private static final long serialVersionUID = 4387274360968910837L;
	
	/** Parent user-interface and display frame. */
    protected VolumeTriPlanarInterface m_kParent = null;
    
	/**
	 * Default Constructor.
	 */
	 public VolumeTriPlanarRender()
	{
		 super();
	}


	 /**
	  * Construct the Volume/Surface/Tri-Planar renderer.
	  * @param kParent parent user-interface and frame.
	  * @param kAnimator animator used to display the canvas.
	  * @param kVolumeImageA volume data and textures for ModelImage A.
	  * @param kVolumeImageB volume data and textures for ModelImage B.
	  */
	 public VolumeTriPlanarRender( VolumeTriPlanarInterface kParent, Animator kAnimator, 
			 VolumeImage kVolumeImageA, VolumeImage kVolumeImageB  )
	 {
		 super( kAnimator, kVolumeImageA, kVolumeImageB );
		 m_kParent = kParent;
	 }

	 /**
	  * Construct the Volume/Surface/Tri-Planar renderer.
	  * @param kParent parent user-interface and frame.
	  * @param kAnimator animator used to display the canvas.
	  * @param kVolumeImageA volume data and textures for ModelImage A.
	  * @param kVolumeImageB volume data and textures for ModelImage B.
	  */
	 public VolumeTriPlanarRender( VolumeTriPlanarRender kShared, GLCanvas kCanvas, VolumeTriPlanarInterface kParent, Animator kAnimator, 
			 VolumeImage kVolumeImageA, VolumeImage kVolumeImageB  )
	 {
		 super( kShared, kCanvas, kAnimator, kVolumeImageA, kVolumeImageB);
		 m_kParent = kParent;
	 }

	 /**
	  * Apply the sculpt region to the volume.
	  */
	 public boolean applySculpt(boolean bAll)
	 {
		 if ( super.applySculpt(bAll) )
		 {
			 m_kParent.setModified();
			 return true;
		 }
		 return false;
	 }

	 /* (non-Javadoc)
	  * @see javax.media.opengl.GLEventListener#display(javax.media.opengl.GLAutoDrawable)
	  */
	 @Override
	 public void display(GLAutoDrawable arg0) {

		 super.display(arg0);
		 if ( m_bSurfaceMaskUpdate )
		 {
			 m_bSurfaceMaskUpdate = false;
			 VolumeImageSurfaceMask.main(m_kParent.newSharedCanvas(), m_kParent, m_kVolumeImageA, m_kDisplayList, false);
		 }
		 if ( m_bCrop )
		 {
			 m_bCrop = false;
			 m_kParent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
			 VolumeImageCrop.main(m_kParent.newSharedCanvas(), m_kParent, m_kVolumeImageA, m_kVolumeRayCast.GetClipEffect());
			 if ( m_kVolumeImageB.GetImage() != null )
			 {
				 VolumeImageCrop.main(m_kParent.newSharedCanvas(), m_kParent, m_kVolumeImageB, m_kVolumeRayCast.GetClipEffect());
			 }
			 m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		 }
		 if ( m_bExtract )
		 {
			 m_kParent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
			 VolumeImageExtract.main(m_kParent.newSharedCanvas(), m_kParent, m_kVolumeImageA, m_kVolumeRayCast.GetClipEffect(), m_iExtractLevel);
			 m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			 m_bExtract = false;
		 }
	 }

	 public void dispose(GLAutoDrawable kDrawable)
	 {
		 super.dispose(kDrawable);
		 m_kParent = null;
	 }
	 
	  /**
	   * Part of the KeyListener interface. Pressing 'b' toggles displaying the
	   * proxy-geometry versus the ray-traced volume.
	   * @param e the key event.
	   */
	  public void keyPressed(KeyEvent e) {
		  super.keyPressed(e);
		  m_kParent.setCameraParameters();
		  m_kParent.setObjectParameters();
		  return;
	  }
	  
	  /** Rotates the object with a virtual trackball:
	   * @param e the MouseEvent
	   */
	  @Override
	  public void mouseDragged(MouseEvent e)
	  {
		  super.mouseDragged(e);
		  if ( m_kParent != null ) {
			  m_kParent.setCameraParameters();
			  m_kParent.setObjectParameters();
		  }
	  }

	  /**
	   * Undo applying the sculpt region to the volume.
	   */
	  public void undoSculpt(boolean bAll)
	  {
		  super.undoSculpt(bAll);
		  m_kParent.setModified();
	  }

	  /**
	   * Called by the init() function. Creates and initialized the scene-graph.
	   */
	  protected void CreateScene ()
	  {
		  super.CreateScene();
		  m_kParent.addSlices(m_kSlices);
	  }


	  /**
	   * Picking. If a display list object has picking enabled, find the picked polygon based on the mouse position. 
	   */
	   protected void Pick()
	  {
		  Vector3f kPos = new Vector3f(0,0,10);
		  Vector3f kDir = new Vector3f(0,0,1);  // the pick ray

		  if (m_bPickPending)
		  {
			  if (m_spkCamera.GetPickRay(m_iXPick,m_iYPick,GetWidth(),
					  GetHeight(),kPos,kDir))
			  {
				  m_bPickPending = false;
				  for ( int i = 0; i < m_kDisplayList.size(); i++ )
				  {
					  if ( m_kDisplayList.get(i).GetPickable() )
					  {
						  m_kPicker.Execute(m_kDisplayList.get(i).GetScene(),kPos,kDir,0.0f,
								  Float.MAX_VALUE);
						  if (m_kPicker.Records.size() > 0)
						  {
							  //System.err.println( kPos.X() + " " + kPos.Y() + " " + kPos.Z() );
							  //System.err.println( kDir.X() + " " + kDir.Y() + " " + kDir.Z() );
							  if ( m_bPaintEnabled )
							  {
								  //System.err.println("Picked " + m_kDisplayList.get(i).getClass().getName());
								  if ( m_bPaint )
								  {
									  m_kDisplayList.get(i).Paint( m_pkRenderer, m_kPicker.GetClosestNonnegative(), m_kPaintColor, m_iBrushSize );
								  }
								  else if ( m_bDropper || m_bPaintCan )
								  {
									  ColorRGBA kDropperColor = new ColorRGBA();
									  Vector3f kPickPoint = new Vector3f();
									  m_kDisplayList.get(i).Dropper( m_kPicker.GetClosestNonnegative(), kDropperColor, kPickPoint );
									  m_kParent.setDropperColor( kDropperColor, kPickPoint );
								  } 
								  else if ( m_bErase )
								  {
									  m_kDisplayList.get(i).Erase( m_pkRenderer, m_kPicker.GetClosestNonnegative(), m_iBrushSize );
								  }
							  }
							  if ( m_bGeodesicEnabled )
							  {
								  m_kParent.setGeodesic( m_kDisplayList.get(i).GetMesh(), m_kPicker.GetClosestNonnegative() );
							  }
							  if ( m_bPickCorrespondence )
							  {
								  PickRecord kRecord = m_kPicker.GetClosestNonnegative();
								  m_kParent.PickCorrespondence( kRecord.iV0, kRecord.iV1, kRecord.iV2 );
							  }
						  }
					  }
				  }
			  }
		  }
	  }

	  protected void update4D( boolean bForward )
	  {
		  m_kVolumeImageA.update4D(bForward);
		  m_kParent.setModified();
	  }




	  protected void updateVOIs( VOIVector kVOIs )
	  {
		  boolean bUpdateVOIs = false;
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i) instanceof VolumeVOI )
			  {
				  m_kDisplayList.remove(i);
				  i--;
			  }
		  }
		  for ( int i = 0; i < kVOIs.size(); i++ )
		  {
			  VOI kVOI = kVOIs.get(i);
			  Vector<VOIBase> kCurves = kVOI.getCurves();
			  for ( int k = 0; k < kCurves.size(); k++ )
			  {
				  VOIBase kVOI3D = kCurves.get(k);
				  bUpdateVOIs |= drawVOI( kVOI3D, this, m_kVolumeImageA, m_kTranslate );
			  }
		  } 
		  if ( bUpdateVOIs )
		  {
			  UpdateSceneRotation();
			  m_kParent.setModified();
		  }
	  }
}
