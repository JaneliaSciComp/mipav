package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewMenuBar;
import gov.nih.mipav.view.renderer.WildMagic.Navigation.NavigationBehavior;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageCrop;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageExtract;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageSurfaceMask;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeVOI;
import gov.nih.mipav.view.renderer.WildMagic.Navigation.NavigationBehavior;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;
import gov.nih.mipav.util.MipavCoordinateSystems;
import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.TreeMap;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;
import javax.swing.KeyStroke;

import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Line3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

import com.jogamp.opengl.util.Animator;


public class VolumeTriPlanarRender extends VolumeTriPlanarRenderBase
implements GLEventListener, KeyListener, MouseMotionListener,  MouseListener, NavigationBehavior.Callback
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
		if ( m_kParent.is3DSelectionEnabled() )
		{
			Transformation world = m_kVolumeRayCast.getMesh().World;
			switch(e.getKeyCode()) {
			case KeyEvent.VK_UP:
				m_kVolumeRayCast.GetScene().UpdateGS();
				m_kParent.moveSelectedPoint( world.InvertVector(m_spkCamera.GetUVector()) );
				break;
			case KeyEvent.VK_DOWN:
				m_kVolumeRayCast.GetScene().UpdateGS();
				m_kParent.moveSelectedPoint( world.InvertVector(m_spkCamera.GetUVector()).neg() );
				break;
			case KeyEvent.VK_RIGHT:
				m_kVolumeRayCast.GetScene().UpdateGS();
				m_kParent.moveSelectedPoint( world.InvertVector(m_spkCamera.GetRVector()) );
				break;
			case KeyEvent.VK_LEFT:
				m_kVolumeRayCast.GetScene().UpdateGS();
				m_kParent.moveSelectedPoint( world.InvertVector(m_spkCamera.GetRVector()).neg() );
				break;
			case KeyEvent.VK_DELETE:
				m_kParent.deleteSelectedPoint( );
				break;
			}
	        // look for shortcuts now

	        String command = null;
	        final KeyStroke ks = KeyStroke.getKeyStrokeForEvent(e);

	        command = Preferences.getShortcutCommand(ks);

	        // Don't pass the VOI key-commands to the actionPerformed function, this
	        // will be done by the VOIManager which will also get the KeyEvents.
	        if ( command != null ) {
	            m_kParent.actionPerformed(new ActionEvent(ks, 0, command));
	        }
			return;
		}
		
		super.keyPressed(e);
		if ( m_kParent != null )
		{
			m_kParent.setCameraParameters();
			m_kParent.setObjectParameters();
		}
		return;
	}

	public void mousePressed(MouseEvent e) {
		super.mousePressed(e);
		if (e.isControlDown() && (m_kParent.is3DMouseEnabled() || m_kParent.is3DSelectionEnabled())) {
			m_kParent.clear3DSelection();
			m_iXPick = e.getX();
			m_iYPick = e.getY();
			m_bPickPending = true;
		}
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
		if (e.isControlDown() && (m_kParent.is3DMouseEnabled() || m_kParent.is3DSelectionEnabled())) {
			m_iXPick = e.getX();
			m_iYPick = e.getY();
			m_bPickPending = true;
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
				if ( m_kParent.is3DMouseEnabled() || m_kParent.is3DSelectionEnabled() )
				{
					m_kPicker.Execute(m_kVolumeRayCast.GetScene(),kPos,kDir,0.0f,
							Float.MAX_VALUE);

					if ( m_kPicker.Records.size() >= 2 )
					{				        						
						Vector3f firstIntersectionPoint = new Vector3f();
						Vector3f secondIntersectionPoint = new Vector3f();
						
						Vector3f pickedPoints[] = new Vector3f[m_kPicker.Records.size()];
						float distances[] = new float[m_kPicker.Records.size()];
						
						for ( int i = 0; i < m_kPicker.Records.size(); i++ )
						{
							PickRecord kPickPoint = m_kPicker.Records.elementAt(i);
							Vector3f kP0 = m_kVolumeRayCast.getMesh().VBuffer.GetPosition3( kPickPoint.iV0 );
							kP0.scale(kPickPoint.B0);
							Vector3f kP1 = m_kVolumeRayCast.getMesh().VBuffer.GetPosition3( kPickPoint.iV1 );
							kP1.scale( kPickPoint.B1);
							Vector3f kP2 = m_kVolumeRayCast.getMesh().VBuffer.GetPosition3( kPickPoint.iV2 );
							kP2.scale( kPickPoint.B2 );
							Vector3f kPoint = Vector3f.add( kP0, kP1 );
							kPoint.add( kP2 );
							
							m_kVolumeRayCast.localToVolumeCoords( kPoint );
							pickedPoints[i] = kPoint;
							distances[i] = kPickPoint.T;	
						}
						
						if ( m_kPicker.Records.size() == 2 )
						{
							firstIntersectionPoint.copy(pickedPoints[0]);
							secondIntersectionPoint.copy(pickedPoints[1]);
							
							float maxValue = -Float.MAX_VALUE;
							Vector3f maxPt = new Vector3f();
							
							Vector3f p0 = new Vector3f(firstIntersectionPoint);
							Vector3f p1 = new Vector3f(secondIntersectionPoint);
							Vector3f step = Vector3f.sub(p1, p0);
							float numSteps = step.length() + 1;
							step.normalize();
							for ( int i = 0; i < numSteps; i++ )
							{
								p0.add(step);
								float value;
								if ( m_kVolumeImageA.GetImage().isColorImage() )
								{
									boolean useRed = m_kVolumeImageA.GetRGB().getROn();
									boolean useGreen = m_kVolumeImageA.GetRGB().getGOn();
									boolean useBlue = m_kVolumeImageA.GetRGB().getBOn();
									
									float red = 0;
									float green = 0;
									float blue = 0;
									if ( useRed )
									{
										red = m_kVolumeImageA.GetImage().getFloatTriLinearBounds(p0.X, p0.Y, p0.Z, 1);
									}
									if ( useGreen )
									{
										green = m_kVolumeImageA.GetImage().getFloatTriLinearBounds(p0.X, p0.Y, p0.Z, 2);
									}
									if ( useBlue )
									{
										blue = m_kVolumeImageA.GetImage().getFloatTriLinearBounds(p0.X, p0.Y, p0.Z, 3);
									}
									value = Math.max(red, Math.max(green, blue) );
								}
								else 
								{
									value = m_kVolumeImageA.GetImage().getFloatTriLinearBounds(p0.X, p0.Y, p0.Z);
								}
								if ( value > maxValue )
								{
									maxValue = value;
									maxPt.copy(p0);
								}
							}
							
							if ( maxValue != -Float.MAX_VALUE )
							{														
								if ( m_kParent.is3DSelectionEnabled() )
								{
									m_kParent.modify3DMarker( firstIntersectionPoint, secondIntersectionPoint, maxPt );
								}
								else
								{
									short id = (short) m_kVolumeImageA.GetImage().getVOIs().getUniqueID();
									int colorID = 0;
									VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
									VOIText textVOI = new VOIText( );
									textVOI.add( maxPt );
									Transformation world = m_kVolumeRayCast.getMesh().World;
									Vector3f dir = world.InvertVector(m_spkCamera.GetRVector());
									dir.scale(5);
									textVOI.add( Vector3f.add( dir, maxPt) );
									textVOI.setText("origin");
									newTextVOI.getCurves().add(textVOI);
									m_kParent.add3DMarker( newTextVOI );
								}
							}
						}
					}
				}

				else
				{
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
	}

	protected void update4D( boolean bForward )
	{
		m_kVolumeImageA.update4D(bForward);
		m_kParent.setModified();
	}


	public void updateVOIs()
	{
		for ( int i = 0; i < m_kDisplayList.size(); i++ )
		{
			if ( m_kDisplayList.elementAt(i) instanceof VolumeVOI )
			{
				((VolumeVOI)m_kDisplayList.elementAt(i)).needsUpdate(true);
			}
		}
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
		for (int i = kVOIs.size() - 1; i >=0; i--) {
			VOI kVOI = kVOIs.get(i);
			if ( kVOI.getName().equals("MakeSphere") )
			{
				System.err.println( "update 1" + kVOI.getName() );
				Vector<VOIBase> kCurves = kVOI.getCurves();
				for (int k = 0; k < kCurves.size(); k++) {
					Vector3f pt = kCurves.elementAt(k).elementAt(0);
					addSphere( pt, k );
				}

				kVOIs.remove(kVOI);
			}
			else
			{
				Vector<VOIBase> kCurves = kVOI.getCurves();
				for (int k = 0; k < kCurves.size(); k++) {
					VOIBase kVOI3D = kCurves.get(k);
					bUpdateVOIs |= drawVOI(kVOI3D, this, m_kVolumeImageA,
							m_kTranslate);
				}
			}
		}
		if ( bUpdateVOIs )
		{
			UpdateSceneRotation();
			m_kParent.setModified();
		}
	}

	private void addSphere( Vector3f center, int index )
	{
		ColorRGBA[] colors = new ColorRGBA[] {
				new ColorRGBA( 0, 0, 1, 1 ),
				new ColorRGBA( 0, 1, 0, 1 ),
				new ColorRGBA( 0, 1, 1, 1 ),
				new ColorRGBA( 1, 0, 0, 1 ),
				new ColorRGBA( 1, 0, 1, 1 ),
				new ColorRGBA( 1, 1, 0, 1 ),
				new ColorRGBA( 1, 1, 1, 1 )
		};

		int dimX = m_kVolumeImageA.GetImage().getExtents().length > 0 ? m_kVolumeImageA.GetImage().getExtents()[0] : 1;
		int dimY = m_kVolumeImageA.GetImage().getExtents().length > 1 ? m_kVolumeImageA.GetImage().getExtents()[1] : 1;
		int dimZ = m_kVolumeImageA.GetImage().getExtents().length > 2 ? m_kVolumeImageA.GetImage().getExtents()[2] : 1;
		int max = Math.max( dimX, Math.max(dimY, dimZ) );
		float scale = 1f/(float)(max - 1);
		Transformation xfrm = new Transformation();
		xfrm.SetUniformScale( scale );
		Attributes attributes = new Attributes();
		attributes.SetPChannels(3);
		attributes.SetNChannels(3);
		attributes.SetCChannels(0,3);
		StandardMesh std = new StandardMesh(attributes);

		std.SetTransformation ( xfrm );
		TriMesh sphere = std.Sphere(2);
		updateSphere( sphere, center.X, center.Y, center.Z, colors[0] );
		System.err.println( "adding mesh " + index);
		sphere.SetName( "Sphere_" + index);
		m_kParent.addSurface( sphere );
	}

    private static void updateSphere( TriMesh sphere, float x, float y, float z, ColorRGBA c )
    {
    	for ( int i = 0; i < sphere.VBuffer.GetVertexQuantity(); i++ )
    	{
    		Vector3f p = sphere.VBuffer.GetPosition3(i);
    		p.add(x,y,z);
    		sphere.VBuffer.SetPosition3(i, p);
    		sphere.VBuffer.SetColor4( 0, i, c );
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

	
	public void setPathPlanningFlythru(boolean _isPathPlanning) {
		isPathPlanningEnabled = _isPathPlanning;
		navigationBehavior.setPathFlythruMode(_isPathPlanning);
	}
	
	public void setMouseControlFlythru(boolean _isMouseControl) {
		isMouseControlEnabled = _isMouseControl;
		navigationBehavior.setMouseFlythruMode(_isMouseControl);
	}

	public void setAnnotationMode(boolean _isAnnotateEnabled) {
		isAnnotateEnabled = _isAnnotateEnabled;
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
		// AddNode(kSphereNode);

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
		// AddNode(kSphereNode1);

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
		// AddNode(kSphereNode3);

		// tracking point
		Attributes kAttr4 = new Attributes();
		kAttr4.SetPChannels(3);
		kAttr4.SetNChannels(3);
		kAttr4.SetCChannels(0, 3);
		StandardMesh kSM4 = new StandardMesh(kAttr4);

		TriMesh kSpherePosition4 = kSM4.Sphere(10, 10, 0.006f);
		for (int i = 0; i < kSpherePosition4.VBuffer.GetVertexQuantity(); i++) {
			kSpherePosition4.VBuffer.SetColor3(0, i, 0, 1, 0);
		}
		kSpherePosition4.AttachEffect(new VertexColor3Effect());
		Node kSphereNode4 = new Node();
		kSphereNode4.AttachChild(kSpherePosition4);
		kSphereNode4.SetName("TrackPoint");
		// AddNode(kSphereNode4);

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
		// AddNode(kSphereNode_Vbl);


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
		// AddNode(kSphereNode_Vtl);

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
		// AddNode(kSphereNode_Vbr);

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
		// AddNode(kSphereNode_Vtr);

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
		// AddNode(kSphereNode_Wbl);

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
		// AddNode(kSphereNode_Wtl);

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
		// AddNode(kSphereNode_Wbr);

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
		// AddNode(kSphereNode_Wtr);

	}

	/**
	 * Navigation behavior callback handler
	 */
	public void viewChanged(NavigationBehavior behavior, int iEvent) {


		if (isNavigationEnabled) {
           
			Vector3f kCDir = behavior.getViewDirection();
			kCDir.normalize();
			Vector3f kCUp = behavior.getViewUp();
			kCUp.normalize();
			Vector3f kCLoc = behavior.getViewPoint();
			Vector3f kCRight = Vector3f.unitCross(kCDir, kCUp);
			
			if ( isPathPlanningEnabled || isAnnotateEnabled ) {
				Vector3f kPositionScaled = getPositionScaled(kCLoc);
				m_spkCamera.SetFrame(kPositionScaled, kCDir, kCUp, kCRight);
				updateSlicesCenter(kPositionScaled);
			} else if ( isMouseControlEnabled ) {
				m_spkCamera.SetFrame(kCLoc, kCDir, kCUp, kCRight);
			}
			
		}

	}

	/**
     * Scaled coordinates for the current position along the path for viewing.
     * @param   Point in normalized path coordinates.
     * @return  Point3f A new instance created which contains the path position coordinates, scaled to match the
     *          TriMesh in JPanelSurface.
     */
    public Vector3f getPositionScaled( Vector3f kPoint) {

        int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
        float[] afResolutions = m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions();
        float[] afOrigins = m_kVolumeImageA.GetImage().getFileInfo(0).getOrigin();
        int[] aiDirections = m_kVolumeImageA.GetImage().getFileInfo(0).getAxisDirection();

        int xDim = aiExtents[0];
        int yDim = aiExtents[1];
        int zDim = aiExtents[2];

        float xBox = (xDim - 1) * afResolutions[0];
        float yBox = (yDim - 1) * afResolutions[1];
        float zBox = (zDim - 1) * afResolutions[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));
        Vector3f kPointScaled = new Vector3f();
        kPointScaled.X = ((2.0f * (kPoint.X - afOrigins[0]) / aiDirections[0]) -
                          xBox) / (2.0f*maxBox);
        kPointScaled.Y = ((2.0f * (kPoint.Y - afOrigins[1]) / aiDirections[1]) -
                          yBox) / (2.0f*maxBox);
        kPointScaled.Z = ((2.0f * (kPoint.Z - afOrigins[2]) / aiDirections[2]) -
                          zBox) / (2.0f*maxBox);
        return kPointScaled;
    }
	
	/**
	 * Currently only being used to update the picking point
	 * @param name   surface name
	 * @param position  surface location
	 */
	public void updateSceneNodePoint(String name, Vector3f position) {
		Matrix3f currentRotation = getObjectRotation();
		Matrix3f rotationInverse = Matrix3f.inverse(currentRotation);
		Vector3f location = rotationInverse.mult(position);
		m_kParent.translateSurface(name, location);
	}


	/**
	 * Update the bottom 3 planar view center
	 */
	public void updateSlicesCenter(Vector3f loc) {
		Matrix3f currentRotation = getObjectRotation();
		Matrix3f rotationInverse = Matrix3f.inverse(currentRotation);
		Vector3f location = rotationInverse.mult(loc);
		location.sub( m_kTranslate );
		location.X *= 1.0f/m_fX;
		location.Y *= 1.0f/m_fY;
		location.Z *= 1.0f/m_fZ;
		location.X *= (m_kVolumeImageA.GetImage().getExtents()[0]-1);
		location.Y *= (m_kVolumeImageA.GetImage().getExtents()[1]-1);
		location.Z *= (m_kVolumeImageA.GetImage().getExtents()[2]-1);
		m_kParent.setSliceFromPlane(location);
	}

	/**
	 * Update the camera in 3D view window.
	 * @param kCenter   center in image space
	 */
	 public void setCameraCenter(Vector3f kCenter) {
		  
		 if ( isNavigationEnabled ) {
	
			  int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
			  Vector3f tfCenter = new Vector3f( (kCenter.X / (aiExtents[0] -1)),
					  (kCenter.Y / (aiExtents[1] -1)),
					  (kCenter.Z / (aiExtents[2] -1))  );
			  tfCenter.X *= m_fX;
			  tfCenter.Y *= m_fY;
			  tfCenter.Z *= m_fZ;
	
			  tfCenter.add(m_kTranslate);
			  
			  m_spkCamera.SetFrustum(30.0f,m_iWidth/(float)m_iHeight,getNearPlane(),10.0f);
			
			  Matrix3f currentRotation = getObjectRotation();
			  Matrix3f rotationInverse = Matrix3f.inverse(currentRotation);
			  Vector3f location = rotationInverse.multLeft(tfCenter);
			  
			  navigationBehavior.setViewPoint(location);
			  navigationBehavior.setDirection(m_spkCamera.GetDVector());
			  navigationBehavior.setUpVector(m_spkCamera.GetUVector());
			  Vector3f cameraRight = Vector3f.unitCross(m_spkCamera.GetDVector(), m_spkCamera.GetUVector());
			  
			  m_spkCamera.SetFrame(location, m_spkCamera.GetDVector(), m_spkCamera.GetUVector(), cameraRight);			  
		  
		  }
		  
	  }

	 /**
	  * Add the annotation point
	  * @param point
	  */
	 public void addAnnotationPoint(Vector3f point, Vector3f scannerPt) {
		 if ( isNavigationEnabled ) {
			 /*
			  int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
			  Vector3f tfCenter = new Vector3f( (point.X / (aiExtents[0] -1)),
					  (point.Y / (aiExtents[1] -1)),
					  (point.Z / (aiExtents[2] -1))  );
			  tfCenter.X *= m_fX;
			  tfCenter.Y *= m_fY;
			  tfCenter.Z *= m_fZ;
			  tfCenter.add(m_kTranslate);
			  */

				int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
				float[] afResolutions = m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions();
				float[] afOrigins = m_kVolumeImageA.GetImage().getFileInfo(0).getOrigin();
				int[] aiModelDirection = MipavCoordinateSystems.getModelDirections(m_kVolumeImageA.GetImage());
				float[] aiDirections = new float[] { aiModelDirection[0], aiModelDirection[1], aiModelDirection[2] };

				int xDim = aiExtents[0];
				int yDim = aiExtents[1];
				int zDim = aiExtents[2];

				float xBox = (xDim - 1) * afResolutions[0];
				float yBox = (yDim - 1) * afResolutions[1];
				float zBox = (zDim - 1) * afResolutions[2];
				float maxBox = Math.max(xBox, Math.max(yBox, zBox));

				
				Vector3f tfCenter = new Vector3f();
				tfCenter.X = ((2.0f * (scannerPt.X - afOrigins[0]) / aiDirections[0]) - xBox)
						/ (2.0f * maxBox);
				tfCenter.Y = ((2.0f * (scannerPt.Y - afOrigins[1]) / aiDirections[1]) - yBox)
						/ (2.0f * maxBox);
				tfCenter.Z = ((2.0f * (scannerPt.Z - afOrigins[2]) / aiDirections[2]) - zBox)
						/ (2.0f * maxBox);
				
			 
			  createAnnotatePoint(tfCenter);
			  // annotatePtsList.add(tfCenter);
			  annotatePtsList.add(scannerPt);
		 }
	 }
	 
	 /**
	  * Set camera view rotation degree
	  * @param degree
	  */
	  public void setCameraViewRotationDegree(int degree) {
		  navigationBehavior.setCamerViewRotationDegree(degree);
	  }
	  
	  /**
	   * Generate the path planning path. 
	   */
	  public void generatePath() {
		  
		  int len = annotatePtsList.size();
		  Vector3f[] akVec = new Vector3f[len];
		  /*
		  for ( int i = len-1; i >= 0 ; i-- ) {
			  akVec[i] = new Vector3f(annotatePtsList.get(i));
		  }
		  */
		  
		  for ( int i = len-1; i >= 0 ; i-- ) {
			  akVec[len-1-i] = new Vector3f(annotatePtsList.get(i));
		  }
		  
		  m_kFlyPathGraphCurve = new FlyPathGraphCurve(akVec, len-1, 2);
		  
		  kGeometryBranchPath = createBranchPathGeometryScaled(0);
		  kGeometryBranchPath.AttachEffect( new VertexColor3Effect() );
          m_spkScene.AttachChild(kGeometryBranchPath);
          
          kPoly = new Polyline(kGeometryBranchPath.VBuffer, false, true);
          kPoly.AttachEffect( new VertexColor3Effect() );
          
          if ( kPolyNode == null ) {
        	  kPolyNode = new Node();
        	  m_kParent.addNode( kPolyNode );
          }
          kPolyNode.AttachChild(kPoly);
          
          navigationBehavior.setupPath(m_kFlyPathGraphCurve);
	  
	  }
	  
	  
	  /**
	   * Clear the annotated path
	   */
	  public void clearPath() {
		  
		  annotatePtsList.clear();
		  m_spkScene.DetachChild(kGeometryBranchPath);
		  kPolyNode.DetachChild(kPoly);
		  if ( kPoly != null ) {
			  kPoly.dispose();
			  kPoly = null;
		  }
		  if ( kGeometryBranchPath != null ) {
			  kGeometryBranchPath.dispose();
			  kGeometryBranchPath = null;
		  }
		  navigationBehavior.clearPath();
		  m_kFlyPathGraphCurve = null;
		  
		  for ( int i = 0; i < annotatePtsCounter; i++ ) {
			  String pointName = "annotate" + i;
			  RemoveNode(pointName);
		  }
		  
	  }
	  
	  /**
	   * Translate the path points from image coordinate to view volume coordinate.  
	   * @param iBranch
	   * @return
	   */
	  private Polyline createBranchPathGeometryScaled(int iBranch) {
		int iNumVertex = 500;
		Attributes kAttributes = new Attributes();
		kAttributes.SetPChannels(3);
		kAttributes.SetCChannels(0, 3);
		VertexBuffer kVBuffer = new VertexBuffer(kAttributes, iNumVertex);

		Curve3f kCurve = m_kFlyPathGraphCurve.getCurvePosition(iBranch);
		float fStep = kCurve.GetTotalLength() / (iNumVertex - 1);

		int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
		float[] afResolutions = m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions();
		float[] afOrigins = m_kVolumeImageA.GetImage().getFileInfo(0).getOrigin();
		int[] aiModelDirection = MipavCoordinateSystems.getModelDirections(m_kVolumeImageA.GetImage());
		float[] aiDirections = new float[] { aiModelDirection[0], aiModelDirection[1], aiModelDirection[2] };

		int xDim = aiExtents[0];
		int yDim = aiExtents[1];
		int zDim = aiExtents[2];

		float xBox = (xDim - 1) * afResolutions[0];
		float yBox = (yDim - 1) * afResolutions[1];
		float zBox = (zDim - 1) * afResolutions[2];
		float maxBox = Math.max(xBox, Math.max(yBox, zBox));

		for (int iPoint = 0; iPoint < iNumVertex; ++iPoint) {
			float fDist = iPoint * fStep;
			float fTime = kCurve.GetTime(fDist, 100, 1e-02f);
			Vector3f kPoint = kCurve.GetPosition(fTime);
			
			kPoint.X = ((2.0f * (kPoint.X - afOrigins[0]) / aiDirections[0]) - xBox)
					/ (2.0f * maxBox);
			kPoint.Y = ((2.0f * (kPoint.Y - afOrigins[1]) / aiDirections[1]) - yBox)
					/ (2.0f * maxBox);
			kPoint.Z = ((2.0f * (kPoint.Z - afOrigins[2]) / aiDirections[2]) - zBox)
					/ (2.0f * maxBox);
			
			kVBuffer.SetPosition3(iPoint, kPoint);
			kVBuffer.SetColor3(0, iPoint, m_kNormalColorPathUnvisited);
		}

		return new Polyline(kVBuffer, false, true);
	}

  /**
     * Create a new Annotation point at the position specified.
     * @param kPosition position of new annotation point.
     */
    private void createAnnotatePoint(Vector3f kPosition)
    {
     
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetCChannels(0, 3);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh kMesh = kSM.Sphere(10, 10, 0.003f);
        for (int i = 0; i < kMesh.VBuffer.GetVertexQuantity(); i++) {
        	kMesh.VBuffer.SetColor3(0, i, 0, 1, 0);
		}
        kMesh.AttachEffect(new VertexColor3Effect());
        Node annotateNode = new Node();
        annotateNode.AttachChild(kMesh);
        String pointName = "annotate" + annotatePtsCounter;
        annotateNode.SetName(pointName);
        AddNode(annotateNode);
        m_kParent.translateSurface(pointName, kPosition);
        annotatePtsCounter++;
    }
  
}
