package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.renderer.WildMagic.Navigation.NavigationBehavior;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageCrop;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageExtract;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageSurfaceMask;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeVOI;
import gov.nih.mipav.view.renderer.WildMagic.Navigation.NavigationBehavior;

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
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

import com.jogamp.opengl.util.Animator;


public class VolumeTriPlanarRender extends VolumeTriPlanarRenderBase
implements GLEventListener, KeyListener, MouseMotionListener,  MouseListener, NavigationBehavior.Callback
{

	private static final long serialVersionUID = 4387274360968910837L;

	/** Parent user-interface and display frame. */
	protected VolumeTriPlanarInterface m_kParent = null;

	private NavigationBehavior navigationBehavior;
	
	/**
	 * Default Constructor.
	 */
	public VolumeTriPlanarRender()
	{
		super();
	}


	public VolumeTriPlanarRender(VolumeImage kVolumeImageA)
	{
		super(kVolumeImageA);
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
	public VolumeTriPlanarRender( VolumeTriPlanarRender kShared, GLCanvas kCanvas, VolumeTriPlanarInterface kParent, 
			VolumeImage kVolumeImageA, VolumeImage kVolumeImageB  )
	{
		super( kShared, kCanvas, kVolumeImageA, kVolumeImageB);
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
			//			 VolumeImageSurfaceMask.main(m_kParent.newSharedCanvas(), m_kParent, m_kVolumeImageA, m_kDisplayList, false);
			//
			//			 long startTime = System.currentTimeMillis();
			//			 for ( int i = 1; i < m_kDisplayList.size(); i++ )
			//			 {
			//				 if ( m_kDisplayList.elementAt(i) instanceof VolumeSurface )
			//				 {
			//					 ((VolumeSurface)m_kDisplayList.elementAt(i)).computeSurfaceMask();
			//				 }
			//			 }
			//			 long now = System.currentTimeMillis();
			//			 double elapsedTime = (double) (now - startTime);
			//
			//			 // if elasedTime is invalid, then set it to 0
			//			 if (elapsedTime <= 0) {
			//				 elapsedTime = (double) 0.0;
			//			 }
			//
			//			 System.err.println ("CPU SurfaceMask " + elapsedTime / 1000.0); // return in seconds!!
		}
		if ( m_bCrop )
		{
			m_bCrop = false;
			m_kParent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
			Matrix3f rot = GetSceneRotation();
			Matrix4f rot4 = new Matrix4f( rot.M00, rot.M01, rot.M02, 0,
					rot.M10, rot.M11, rot.M12, 0,
					rot.M20, rot.M21, rot.M22, 0,
					0, 0, 0, 1
					);
			float[] rotMatrix = new float[16];
			rot4.getData(rotMatrix);
			VolumeImageCrop.main(m_kParent.newSharedCanvas(), m_kParent, m_kVolumeImageA, m_kVolumeRayCast.GetClipEffect(), rotMatrix);
			if ( m_kVolumeImageB.GetImage() != null )
			{
				VolumeImageCrop.main(m_kParent.newSharedCanvas(), m_kParent, m_kVolumeImageB, m_kVolumeRayCast.GetClipEffect(), rotMatrix);
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
		if ( m_kParent != null )
		{
			m_kParent.setCameraParameters();
			m_kParent.setObjectParameters();
		}
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
		if ( m_kParent != null )
		{
			m_kParent.addSlices(m_kSlices);
		}
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
	
	/**
	 * Toggle Navigation mode.
	 * 
	 * @param isNavigationEnabled
	 *            is navigation checkbox selected or not.
	 */
	public void toggleNavigation(boolean _isNavigationEnabled) {
		super.toggleNavigation(_isNavigationEnabled);
		if (navigationBehavior == null) {
			setupNavigationScenegraph();
			navigationBehavior = new NavigationBehavior(this, m_spkCamera);
			navigationBehavior.setupCallback(this);
		}
		navigationBehavior.setNaviMode(isNavigationEnabled);

	}

	/**
	 * Setup the Navigation scene graph view for debugging purpose
	 */
	public void setupNavigationScenegraph() {

		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		kAttr.SetCChannels(0, 3);
		StandardMesh kSM = new StandardMesh(kAttr);

		TriMesh kSpherePosition = kSM.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition.VBuffer.SetColor3(0, i, 0, 1, 1);
		}
		kSpherePosition.AttachEffect(new VertexColor3Effect());
		Node kSphereNode = new Node();
		kSphereNode.AttachChild(kSpherePosition);
		kSphereNode.SetName("Camera");
		AddNode(kSphereNode);

		Attributes kAttr1 = new Attributes();
		kAttr1.SetPChannels(3);
		kAttr1.SetNChannels(3);
		kAttr1.SetCChannels(0, 3);
		StandardMesh kSM1 = new StandardMesh(kAttr1);

		TriMesh kSpherePosition1 = kSM1.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition1.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition1.VBuffer.SetColor3(0, i, 0, 1, 0);
		}
		kSpherePosition1.AttachEffect(new VertexColor3Effect());
		Node kSphereNode1 = new Node();
		kSphereNode1.AttachChild(kSpherePosition1);
		kSphereNode1.SetName("StartPoint");
		AddNode(kSphereNode1);

		// end point
		Attributes kAttr2 = new Attributes();
		kAttr2.SetPChannels(3);
		kAttr2.SetNChannels(3);
		kAttr2.SetCChannels(0, 3);
		StandardMesh kSM2 = new StandardMesh(kAttr2);

		TriMesh kSpherePosition2 = kSM2.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition2.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition2.VBuffer.SetColor3(0, i, 1, 0, 0);
		}
		kSpherePosition2.AttachEffect(new VertexColor3Effect());
		Node kSphereNode2 = new Node();
		kSphereNode2.AttachChild(kSpherePosition2);
		kSphereNode2.SetName("EndPoint");
		AddNode(kSphereNode2);

		// treshold point
		Attributes kAttr3 = new Attributes();
		kAttr3.SetPChannels(3);
		kAttr3.SetNChannels(3);
		kAttr3.SetCChannels(0, 3);
		StandardMesh kSM3 = new StandardMesh(kAttr3);

		TriMesh kSpherePosition3 = kSM3.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition3.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition3.VBuffer.SetColor3(0, i, 0, 0, 1);
		}
		kSpherePosition3.AttachEffect(new VertexColor3Effect());
		Node kSphereNode3 = new Node();
		kSphereNode3.AttachChild(kSpherePosition3);
		kSphereNode3.SetName("Threshold");
		AddNode(kSphereNode3);
		
		// tracking point
		Attributes kAttr4 = new Attributes();
		kAttr4.SetPChannels(3);
		kAttr4.SetNChannels(3);
		kAttr4.SetCChannels(0, 3);
		StandardMesh kSM4 = new StandardMesh(kAttr4);

		TriMesh kSpherePosition4 = kSM4.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition4.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition4.VBuffer.SetColor3(0, i, 1, 0, 1);
		}
		kSpherePosition4.AttachEffect(new VertexColor3Effect());
		Node kSphereNode4 = new Node();
		kSphereNode4.AttachChild(kSpherePosition4);
		kSphereNode4.SetName("TrackPoint");
		AddNode(kSphereNode4);

		//******************************   View Frustum Points *******************************
		// --------------------------------- near plane ------------------------------------
		Attributes kAttr_Vbl = new Attributes();
		kAttr_Vbl.SetPChannels(3);
		kAttr_Vbl.SetNChannels(3);
		kAttr_Vbl.SetCChannels(0, 3);
		StandardMesh kSM_Vbl = new StandardMesh(kAttr_Vbl);
		
		TriMesh kSpherePosition_Vbl = kSM_Vbl.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition_Vbl.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition_Vbl.VBuffer.SetColor3(0, i, 0, 1, 0);
		}
		kSpherePosition_Vbl.AttachEffect(new VertexColor3Effect());
		Node kSphereNode_Vbl = new Node();
		kSphereNode_Vbl.AttachChild(kSpherePosition_Vbl);
		kSphereNode_Vbl.SetName("Vbl");
		AddNode(kSphereNode_Vbl);
		
		
		// ---------------------------------------------------------------
		Attributes kAttr_Vtl = new Attributes();
		kAttr_Vtl.SetPChannels(3);
		kAttr_Vtl.SetNChannels(3);
		kAttr_Vtl.SetCChannels(0, 3);
		StandardMesh kSM_Vtl = new StandardMesh(kAttr_Vtl);
		
		TriMesh kSpherePosition_Vtl = kSM_Vtl.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition_Vtl.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition_Vtl.VBuffer.SetColor3(0, i, 0, 1, 0);
		}
		kSpherePosition_Vtl.AttachEffect(new VertexColor3Effect());
		Node kSphereNode_Vtl = new Node();
		kSphereNode_Vtl.AttachChild(kSpherePosition_Vtl);
		kSphereNode_Vtl.SetName("Vtl");
		AddNode(kSphereNode_Vtl);
		
		// -------------------------------------------------------------
		Attributes kAttr_Vbr = new Attributes();
		kAttr_Vbr.SetPChannels(3);
		kAttr_Vbr.SetNChannels(3);
		kAttr_Vbr.SetCChannels(0, 3);
		StandardMesh kSM_Vbr = new StandardMesh(kAttr_Vbr);
		
		TriMesh kSpherePosition_Vbr = kSM_Vbr.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition_Vbr.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition_Vbr.VBuffer.SetColor3(0, i, 0, 1, 0);
		}
		kSpherePosition_Vbr.AttachEffect(new VertexColor3Effect());
		Node kSphereNode_Vbr = new Node();
		kSphereNode_Vbr.AttachChild(kSpherePosition_Vbr);
		kSphereNode_Vbr.SetName("Vbr");
		AddNode(kSphereNode_Vbr);
		
		// --------------------------------------------------------------
		Attributes kAttr_Vtr = new Attributes();
		kAttr_Vtr.SetPChannels(3);
		kAttr_Vtr.SetNChannels(3);
		kAttr_Vtr.SetCChannels(0, 3);
		StandardMesh kSM_Vtr = new StandardMesh(kAttr_Vtr);
		
		TriMesh kSpherePosition_Vtr = kSM_Vtr.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition_Vtr.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition_Vtr.VBuffer.SetColor3(0, i, 0, 1, 0);
		}
		kSpherePosition_Vtr.AttachEffect(new VertexColor3Effect());
		Node kSphereNode_Vtr = new Node();
		kSphereNode_Vtr.AttachChild(kSpherePosition_Vtr);
		kSphereNode_Vtr.SetName("Vtr");
		AddNode(kSphereNode_Vtr);
		
		// --------------------------------  far plane ---------------------
		Attributes kAttr_Wbl = new Attributes();
		kAttr_Wbl.SetPChannels(3);
		kAttr_Wbl.SetNChannels(3);
		kAttr_Wbl.SetCChannels(0, 3);
		StandardMesh kSM_Wbl = new StandardMesh(kAttr_Wbl);
		
		TriMesh kSpherePosition_Wbl = kSM_Wbl.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition_Wbl.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition_Wbl.VBuffer.SetColor3(0, i, 1, 0, 0);
		}
		kSpherePosition_Wbl.AttachEffect(new VertexColor3Effect());
		Node kSphereNode_Wbl = new Node();
		kSphereNode_Wbl.AttachChild(kSpherePosition_Wbl);
		kSphereNode_Wbl.SetName("Wbl");
		AddNode(kSphereNode_Wbl);
		
		// ---------------------------------------------------------------
		Attributes kAttr_Wtl = new Attributes();
		kAttr_Wtl.SetPChannels(3);
		kAttr_Wtl.SetNChannels(3);
		kAttr_Wtl.SetCChannels(0, 3);
		StandardMesh kSM_Wtl = new StandardMesh(kAttr_Wtl);
		
		TriMesh kSpherePosition_Wtl = kSM_Wtl.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition_Wtl.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition_Wtl.VBuffer.SetColor3(0, i, 1, 0, 0);
		}
		kSpherePosition_Wtl.AttachEffect(new VertexColor3Effect());
		Node kSphereNode_Wtl = new Node();
		kSphereNode_Wtl.AttachChild(kSpherePosition_Wtl);
		kSphereNode_Wtl.SetName("Wtl");
		AddNode(kSphereNode_Wtl);
		
		// -----------------------------------------------------------------
		Attributes kAttr_Wbr = new Attributes();
		kAttr_Wbr.SetPChannels(3);
		kAttr_Wbr.SetNChannels(3);
		kAttr_Wbr.SetCChannels(0, 3);
		StandardMesh kSM_Wbr = new StandardMesh(kAttr_Wbr);
		
		TriMesh kSpherePosition_Wbr = kSM_Wbr.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition_Wbr.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition_Wbr.VBuffer.SetColor3(0, i, 1, 0, 0);
		}
		kSpherePosition_Wbr.AttachEffect(new VertexColor3Effect());
		Node kSphereNode_Wbr = new Node();
		kSphereNode_Wbr.AttachChild(kSpherePosition_Wbr);
		kSphereNode_Wbr.SetName("Wbr");
		AddNode(kSphereNode_Wbr);
		
		// ----------------------------------------------------------------
		Attributes kAttr_Wtr = new Attributes();
		kAttr_Wtr.SetPChannels(3);
		kAttr_Wtr.SetNChannels(3);
		kAttr_Wtr.SetCChannels(0, 3);
		StandardMesh kSM_Wtr = new StandardMesh(kAttr_Wtr);
		
		TriMesh kSpherePosition_Wtr = kSM_Wtr.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition_Wtr.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition_Wtr.VBuffer.SetColor3(0, i, 1, 0, 0);
		}
		kSpherePosition_Wtr.AttachEffect(new VertexColor3Effect());
		Node kSphereNode_Wtr = new Node();
		kSphereNode_Wtr.AttachChild(kSpherePosition_Wtr);
		kSphereNode_Wtr.SetName("Wtr");
		AddNode(kSphereNode_Wtr);
		
	}

	/**
	 * Navigation behavior callback handler
	 */
	public void viewChanged(NavigationBehavior behavior, int iEvent) {

		if (isNavigationEnabled) {

			Vector3f cameraLocation = behavior.getViewPoint();
			Vector3f cameraDir = behavior.getViewDirection();
			Vector3f cameraUp = behavior.getViewUp();
			Vector3f cameraRight = Vector3f.unitCross(cameraDir, cameraUp);
			m_spkCamera.SetFrame(cameraLocation, cameraDir, cameraUp, cameraRight);
		
			float fRMin = m_spkCamera.GetRMin(); 
			float fRMax = m_spkCamera.GetRMax(); 
			float fUMin = m_spkCamera.GetUMin(); 
			float fUMax = m_spkCamera.GetUMax(); 
			float fDMin = m_spkCamera.GetDMin();
			float fDMax = m_spkCamera.GetDMax();
			
			m_spkCamera.SetFrustum(fRMin, fRMax, fUMin, fUMax, fDMin, fDMax);
		    
			// **************************  compute frustum points ***********************
			Vector3f dminD = Vector3f.scale(fDMin, cameraDir);
			Vector3f uminU = Vector3f.scale(fUMin, cameraUp);
			Vector3f umaxU = Vector3f.scale(fUMax, cameraUp);
			Vector3f rminR = Vector3f.scale(fRMin, cameraRight);
			Vector3f rmaxR = Vector3f.scale(fRMax, cameraRight);
			
			// near plane
			Vector3f Vbl = Vector3f.add(cameraLocation, Vector3f.add(dminD, Vector3f.add(uminU, rminR)));
			Vector3f Vtl = Vector3f.add(cameraLocation, Vector3f.add(dminD, Vector3f.add(umaxU, rminR)));
			Vector3f Vbr = Vector3f.add(cameraLocation, Vector3f.add(dminD, Vector3f.add(uminU, rmaxR)));
			Vector3f Vtr = Vector3f.add(cameraLocation, Vector3f.add(dminD, Vector3f.add(umaxU, rmaxR)));
			
			// far plane
			Vector3f temp = Vector3f.scale(fDMax/fDMin, Vector3f.add(dminD, Vector3f.add(uminU, rminR)));
			Vector3f Wbl = Vector3f.add(cameraLocation, temp);
			temp = Vector3f.scale(fDMax/fDMin, Vector3f.add(dminD, Vector3f.add(umaxU, rminR)));
			Vector3f Wtl = Vector3f.add(cameraLocation, temp);
			temp = Vector3f.scale(fDMax/fDMin, Vector3f.add(dminD, Vector3f.add(uminU, rmaxR)));
			Vector3f Wbr = Vector3f.add(cameraLocation, temp);
			temp = Vector3f.scale(fDMax/fDMin, Vector3f.add(dminD, Vector3f.add(umaxU, rmaxR)));
			Vector3f Wtr = Vector3f.add(cameraLocation, temp);
		    
			// System.err.println("Vbl = " + Vbl);
			// System.err.println("Vtl = " + Vtl);
			// System.err.println("Vbr = " + Vbr);
			// System.err.println("Vtl = " + Vtl);
			
			// System.err.println("Wbl = " + Wbl);
			// System.err.println("Wtl = " + Wtl);
			// System.err.println("Wbr = " + Wbr);
			// System.err.println("Wtl = " + Wtl);
			
			updateSceneNodePoint("Vbl", Vbl);
			updateSceneNodePoint("Vtl", Vtl);
			updateSceneNodePoint("Vbr", Vbr);
			updateSceneNodePoint("Vtr", Vtr);
			
			updateSceneNodePoint("Wbl", Wbl);
			updateSceneNodePoint("Wtl", Wtl);
			updateSceneNodePoint("Wbr", Wbr);
			updateSceneNodePoint("Wtr", Wtr);
			
			if (m_kVolumeRayCast != null) {
				m_kVolumeRayCast.SetDisplay(m_kParent.getRendererGUI().getVolumeCheck().isSelected());
			}
			if (m_kSlices != null) {
				m_kSlices.SetDisplay(m_kParent.getRendererGUI().getSlicesCheck().isSelected());
			}
			

		}
		updateFrustumPoints();

	}
	
	public void updateSceneNodePoint(String name, Vector3f position) {
		Matrix3f currentRotation = getObjectRotation();
		Matrix3f rotationInverse = Matrix3f.inverse(currentRotation);
		Vector3f location = rotationInverse.mult(position);
		m_kParent.translateSurface(name, location);
	}

	/**
	 * Update the representation of the current viewpoint in the Volume
	 * Tri-Planar renderer.
	 */
	public void updateFrustumPoints() {

		Vector3f trackPoint = navigationBehavior.getTrackingPoint();
		updateSceneNodePoint("TrackPoint", trackPoint);
		
		Vector3f kPositionScaled = navigationBehavior.getViewPoint();
		updateSceneNodePoint("Camera", kPositionScaled);
		
	}
	
}
