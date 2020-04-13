package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmReslice;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelAnnotationAnimation;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceState;
import gov.nih.mipav.view.renderer.WildMagic.Navigation.NavigationBehavior;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageCrop;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageExtract;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeVOI;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.VOIWormAnnotation;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavInitGPU;
import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Image;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Vector;

import com.jogamp.opengl.GLAutoDrawable;
import com.jogamp.opengl.GLEventListener;
import com.jogamp.opengl.awt.GLCanvas;
import javax.swing.KeyStroke;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.Surfaces.TubeSurface;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;


public class VolumeTriPlanarRender extends VolumeTriPlanarRenderBase
implements GLEventListener, KeyListener, MouseMotionListener,  MouseListener, NavigationBehavior.Callback
{

	private static final long serialVersionUID = 4387274360968910837L;

	/** Parent user-interface and display frame. */
	protected VolumeTriPlanarInterface m_kParent = null;
	protected RendererListener configuredListener = null;
	protected boolean rightMousePressed = false;
	protected boolean altPressed = false;
	protected boolean shiftPressed = false;
    private VOILatticeManagerInterface m_kVOIInterface = null;
	/**
	 * Default Constructor.
	 */
	public VolumeTriPlanarRender()
	{
		super();
	}


	public VolumeTriPlanarRender(VolumeImage kVolumeImageA)
	{
		super();
		initShared(null);
		m_pkRenderer = new OpenGLRenderer(m_eFormat, m_eDepth, m_eStencil,
				m_eBuffering, m_eMultisampling, m_iWidth, m_iHeight, newSharedCanvas());
		((OpenGLRenderer) m_pkRenderer).GetCanvas().addGLEventListener(this);
		((OpenGLRenderer) m_pkRenderer).GetCanvas().addKeyListener(this);
		((OpenGLRenderer) m_pkRenderer).GetCanvas().addMouseListener(this);
		((OpenGLRenderer) m_pkRenderer).GetCanvas()
				.addMouseMotionListener(this);
		((OpenGLRenderer) m_pkRenderer).GetCanvas().addMouseWheelListener(this);
		m_pkRenderer.SetExternalDir(MipavInitGPU.getExternalDirs());

		m_kShared = sharedRenderer;
		m_bShared = true;

		m_kAnimator = new Animator(GetCanvas());
		
		m_kVolumeImageA = kVolumeImageA;
		m_kVolumeImageB = new VolumeImage();

		m_kZRotate.fromAxisAngle(Vector3f.UNIT_Z, (float) Math.PI / 18.0f);
		m_kYRotate.fromAxisAngle(Vector3f.UNIT_Y, (float) Math.PI / 18.0f);
		m_kXRotate.fromAxisAngle(Vector3f.UNIT_X, (float) Math.PI / 18.0f);
		m_kParent = null;
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
	 * Construct the Volume/Surface/Tri-Planar renderer.
	 * @param kVolumeImageA volume data and textures for ModelImage A.
	 * @param kVolumeImageB volume data and textures for ModelImage B.
	 */
	public VolumeTriPlanarRender( VolumeTriPlanarInterface kParent, VolumeImage kVolumeImageA, VolumeImage kVolumeImageB  )
	{
		super( );
		initShared(kParent);
		m_pkRenderer = new OpenGLRenderer(m_eFormat, m_eDepth, m_eStencil,
				m_eBuffering, m_eMultisampling, m_iWidth, m_iHeight, newSharedCanvas());
		((OpenGLRenderer) m_pkRenderer).GetCanvas().addGLEventListener(this);
		((OpenGLRenderer) m_pkRenderer).GetCanvas().addKeyListener(this);
		((OpenGLRenderer) m_pkRenderer).GetCanvas().addMouseListener(this);
		((OpenGLRenderer) m_pkRenderer).GetCanvas()
				.addMouseMotionListener(this);
		((OpenGLRenderer) m_pkRenderer).GetCanvas().addMouseWheelListener(this);
		m_pkRenderer.SetExternalDir(MipavInitGPU.getExternalDirs());

		m_kShared = sharedRenderer;
		m_bShared = true;

		m_kAnimator = new Animator(GetCanvas());

		m_kVolumeImageA = kVolumeImageA;
		m_kVolumeImageB = kVolumeImageB;
		m_kZRotate.fromAxisAngle(Vector3f.UNIT_Z, (float) Math.PI / 18.0f);
		m_kYRotate.fromAxisAngle(Vector3f.UNIT_Y, (float) Math.PI / 18.0f);
		m_kXRotate.fromAxisAngle(Vector3f.UNIT_X, (float) Math.PI / 18.0f);
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
	 * @see com.jogamp.opengl.GLEventListener#display(com.jogamp.opengl.GLAutoDrawable)
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
			VolumeImageCrop.main(newSharedCanvas(), m_kParent, m_kVolumeImageA, m_kVolumeRayCast.GetClipEffect(), rotMatrix);
			if ( m_kVolumeImageB.GetImage() != null )
			{
				VolumeImageCrop.main(newSharedCanvas(), m_kParent, m_kVolumeImageB, m_kVolumeRayCast.GetClipEffect(), rotMatrix);
			}
			m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		if ( m_bExtract )
		{
			if ( m_kParent != null )
			{
				m_kParent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
				VolumeImageExtract.main(newSharedCanvas(), m_kParent, m_kVolumeImageA, m_kVolumeRayCast.GetClipEffect(), m_iExtractLevel);
				m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
			m_bExtract = false;
		}

		if ( m_bFirstDisplay && (configuredListener != null) )
		{		
    		configuredListener.rendererConfigured( this );
			m_bFirstDisplay = false;
			m_kSlices.SetDisplay(false);
			m_kVolumeRayCast.SetDisplay(true);
		}
//		if ( wormAnimationStep() != -1 )
		{
			// setup:
			/*
			if (  firstWormAnimation == true )
			{
				String imageName = m_kVolumeImageA.GetImage().getImageName();
				if (imageName.contains("_clone")) {
					imageName = imageName.replaceAll("_clone", "");
				}
				String voiDir = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator + "animation" + File.separator + "seam_cell" + File.separator;
				VOIVector results = new VOIVector();
				LatticeModel.loadAllVOIsFrom( m_kVolumeImageA.GetImage(), voiDir, true, results, true);				
				
				m_kYRotate.fromAxisAngle(Vector3f.UNIT_Y_NEG, (float)Math.PI/180.0f);
				if ( results.size() > 0 )
				{
					annotations = results.elementAt(0);
					addCount = 360/(annotations.getCurves().size());
					for ( int i = 0; i < annotations.getCurves().size(); i++ )
					{
						annotations.getCurves().elementAt(i).createVolumeVOI( m_kVolumeImageA, m_kTranslate );
						annotations.getCurves().elementAt(i).getVolumeVOI().SetDisplay(false);
						if ( annotations.getCurves().elementAt(i) instanceof VOIText )
						{
							VOIText text = (VOIText)annotations.getCurves().elementAt(i);
							if ( text.getText().equalsIgnoreCase("nose") )
							{
								nose = new Vector3f(text.elementAt(0));
							}
						}
					}
				}
				
				allSeamCellsAdded = 0;
				firstWormAnimation = false;
			}
			// rotate:
			if ( rotate )
			{
				Matrix3f kRotate = m_spkScene.Local.GetRotate();
				kRotate.mult(m_kYRotate);
				m_spkScene.Local.SetRotate(kRotate);
				m_spkScene.UpdateGS();
				m_kCuller.ComputeVisibleSet(m_spkScene);
				for (int i = 0; i < m_kDisplayList.size(); i++)
				{
					m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
				}
			}
			
			// add seam cells:
			if ( !seamCellsDone && (animationStep > animationStart) )
			{
				int add = animationStep - animationStart;
				if ( (add % addCount) == 0 )
				{
					for ( int i = 0; i < annotations.getCurves().size(); i++ )
					{
						if ( !annotations.getCurves().elementAt(i).getVolumeVOI().GetDisplay() )
						{
							annotations.getCurves().elementAt(i).getVolumeVOI().SetDisplay(true);
							allSeamCellsAdded++;
							if ( allSeamCellsAdded == annotations.getCurves().size() )
							{
								animationStart = animationStep + 60;
								allPairsAdded = 0;
								seamCellsDone = true;
							}
							break;
						}
					}					
				}
			}
			else if ( seamCellsDone && !pairsDone && (animationStep == animationStart))
			{
				// hide all but 'nose'
				for ( int i = 0; i < annotations.getCurves().size(); i++ )
				{
					if ( annotations.getCurves().elementAt(i) instanceof VOIText )
					{
						VOIText text = (VOIText)annotations.getCurves().elementAt(i);
						if ( text.getText().equalsIgnoreCase("nose") )
						{
							nose = new Vector3f(text.elementAt(0));
						}
						else
						{
							annotations.getCurves().elementAt(i).getVolumeVOI().SetDisplay(false);							
						}
					}
				}
				
				
				String imageName = m_kVolumeImageA.GetImage().getImageName();
				if (imageName.contains("_clone")) {
					imageName = imageName.replaceAll("_clone", "");
				}
				String voiDir = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator + "animation" + File.separator + "pairs" + File.separator;
				VOIVector results = new VOIVector();
				LatticeModel.loadAllVOIsFrom( m_kVolumeImageA.GetImage(), voiDir, true, results, true);

				if ( results.size() > 0 )
				{
					pairs = results.elementAt(0);
					for ( int i = 0; i < pairs.getCurves().size(); i++ )
					{
						pairs.getCurves().elementAt(i).createVolumeVOI( m_kVolumeImageA, m_kTranslate );
						pairs.getCurves().elementAt(i).getVolumeVOI().SetDisplay(false);
					}
					addCount = 360/(pairs.getCurves().size());
				}
				allPairsAdded = 0;
			}
			// add pairs:
			else if ( seamCellsDone && !pairsDone && (animationStep > animationStart) )
			{
				int add = animationStep - animationStart;
				if ( (add % addCount) == 0 )
				{
					for ( int i = 0; i < pairs.getCurves().size(); i++ )
					{
						if ( !pairs.getCurves().elementAt(i).getVolumeVOI().GetDisplay() )
						{
							pairs.getCurves().elementAt(i).getVolumeVOI().SetDisplay(true);
							allPairsAdded++;
							if ( allPairsAdded == pairs.getCurves().size() )
							{
								animationStart = animationStep + 60;
								pairsDone = true;
							}
							break;
						}
					}					
				}
			}
			else if ( seamCellsDone && pairsDone && !latticeDone && (animationStep == animationStart) && (lattice == null) )
			{
				m_kVolumeImageA.GetImage().unregisterAllVOIs();
				
				String imageName = m_kVolumeImageA.GetImage().getImageName();
				if (imageName.contains("_clone")) {
					imageName = imageName.replaceAll("_clone", "");
				}
				String voiDir = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator + "animation" + File.separator + "lattice" + File.separator;
				VOIVector results = new VOIVector();
				LatticeModel.loadAllVOIsFrom( m_kVolumeImageA.GetImage(), voiDir, true, results, false);				
				
				if ( results.size() > 0 )
				{
					lattice = results.elementAt(0);
				}
				else
				{
					short id = (short) m_kVolumeImageA.GetImage().getVOIs().getUniqueID();
					lattice = new VOI(id, "lattice", VOI.POLYLINE, (float) Math.random());
	
					VOIContour left = new VOIContour(false);
					VOIContour right = new VOIContour(false);
					lattice.getCurves().add(left);
					lattice.getCurves().add(right);
					if ( nose != null )
					{
						left.add(nose);
						right.add(nose);
					}
					for ( int i = 0; i < pairs.getCurves().size(); i++ )
					{
						lattice.getCurves().elementAt(0).add(pairs.getCurves().elementAt(i).elementAt(0));
						lattice.getCurves().elementAt(1).add(pairs.getCurves().elementAt(i).elementAt(1));						
					}				
				}
			}
			else if ( !latticeDone && (lattice != null) && (animationStep > animationStart) )
			{
				int add = animationStep - animationStart;
				if ( (add % addCount) == 0 )
				{
					if ( m_kVolumeImageA.GetImage().getVOIs().size() == 0 )
					{
						short id = (short) m_kVolumeImageA.GetImage().getVOIs().getUniqueID();
						VOI temp = new VOI(id, "lattice", VOI.POLYLINE, (float) Math.random());		
						VOIContour left = new VOIContour(false);
						VOIContour right = new VOIContour(false);
						left.update( new ColorRGBA(0, 0, 1, 1 ) );
						right.update( new ColorRGBA(0, 0, 1, 1 ) );
						temp.getCurves().add(left);
						temp.getCurves().add(right);
						temp.setColor( Color.blue );
						m_kVolumeImageA.GetImage().registerVOI(temp);
					}
					else
					{
						VOI temp = m_kVolumeImageA.GetImage().getVOIs().elementAt(0);
						int index = temp.getCurves().elementAt(0).size();

						if ( index < lattice.getCurves().elementAt(0).size() )
						{
							temp.getCurves().elementAt(0).add( lattice.getCurves().elementAt(0).elementAt(index) );
							temp.getCurves().elementAt(1).add( lattice.getCurves().elementAt(1).elementAt(index) );
							temp.getCurves().elementAt(0).update();
							temp.getCurves().elementAt(1).update();


							short id = (short) m_kVolumeImageA.GetImage().getVOIs().getUniqueID();
							VOI pair = new VOI(id, "pair_" + (index+1), VOI.POLYLINE, (float) Math.random());		
							VOIContour rung = new VOIContour(false);
							rung.add( temp.getCurves().elementAt(0).lastElement() );
							rung.add( temp.getCurves().elementAt(1).lastElement() );
							rung.update( new ColorRGBA(1, 1, 0, 1 ) );
							pair.getCurves().add(rung);
							pair.setColor( Color.yellow );
							m_kVolumeImageA.GetImage().registerVOI(pair);
						}
						else
						{
							latticeDone = true;
							animationStart = animationStep + 60;
						}
					}
				}
			}
			else if ( latticeDone && (animationStep == animationStart) && (leftCurve == null) )
			{
				String imageName = m_kVolumeImageA.GetImage().getImageName();
				if (imageName.contains("_clone")) {
					imageName = imageName.replaceAll("_clone", "");
				}
				String voiDir = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator + "animation" + File.separator + "leftLine" + File.separator;
				VOIVector results = new VOIVector();
				LatticeModel.loadAllVOIsFrom( m_kVolumeImageA.GetImage(), voiDir, true, results, false);
				if ( results.size() > 0 )
				{
					leftCurve = results.elementAt(0);
					leftCurveAnimate = new VOI(leftCurve);
					leftCurveAnimate.getCurves().elementAt(0).setClosed(false);
					leftCurveAnimate.getCurves().elementAt(0).removeAllElements();
					leftCurveAnimate.getCurves().elementAt(0).add( leftCurve.getCurves().elementAt(0).remove(0) );
					m_kVolumeImageA.GetImage().registerVOI(leftCurveAnimate);						
				}


				voiDir = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator + "animation" + File.separator + "rightLine" + File.separator;
				results = new VOIVector();
				LatticeModel.loadAllVOIsFrom( m_kVolumeImageA.GetImage(), voiDir, true, results, false);
				if ( results.size() > 0 )
				{
					rightCurve = results.elementAt(0);
					rightCurveAnimate = new VOI(rightCurve);
					rightCurveAnimate.getCurves().elementAt(0).setClosed(false);
					rightCurveAnimate.getCurves().elementAt(0).removeAllElements();
					rightCurveAnimate.getCurves().elementAt(0).add( rightCurve.getCurves().elementAt(0).remove(0) );
					m_kVolumeImageA.GetImage().registerVOI(rightCurveAnimate);				
				}

				voiDir = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator + "animation" + File.separator + "centerLine" + File.separator;
				results = new VOIVector();
				LatticeModel.loadAllVOIsFrom( m_kVolumeImageA.GetImage(), voiDir, true, results, false);
				if ( results.size() > 0 )
				{
					centerCurve = results.elementAt(0);
					centerCurveAnimate = new VOI(centerCurve);
					centerCurveAnimate.getCurves().elementAt(0).setClosed(false);
					centerCurveAnimate.getCurves().elementAt(0).removeAllElements();
					centerCurveAnimate.getCurves().elementAt(0).add( centerCurve.getCurves().elementAt(0).remove(0) );
					m_kVolumeImageA.GetImage().registerVOI(centerCurveAnimate);		
				}
			}
			else if ( !curvesDone && (leftCurve != null) && (rightCurve != null) && (centerCurve != null) )
			{
				if ( leftCurve.getCurves().elementAt(0).size() > 0 )
				{
					leftCurveAnimate.getCurves().elementAt(0).add( leftCurve.getCurves().elementAt(0).remove(0) );
					rightCurveAnimate.getCurves().elementAt(0).add( rightCurve.getCurves().elementAt(0).remove(0) );
					centerCurveAnimate.getCurves().elementAt(0).add( centerCurve.getCurves().elementAt(0).remove(0) );
					if ( leftCurve.getCurves().elementAt(0).size() > 0 )
					{
						leftCurveAnimate.getCurves().elementAt(0).add( leftCurve.getCurves().elementAt(0).remove(0) );
						rightCurveAnimate.getCurves().elementAt(0).add( rightCurve.getCurves().elementAt(0).remove(0) );
						centerCurveAnimate.getCurves().elementAt(0).add( centerCurve.getCurves().elementAt(0).remove(0) );	
					}
					leftCurveAnimate.getCurves().elementAt(0).update();
					rightCurveAnimate.getCurves().elementAt(0).update();
					centerCurveAnimate.getCurves().elementAt(0).update();						
				}
				else
				{
					curvesDone = true;
					animationStart = animationStep + 60;
				}		
			}
			else if ( curvesDone && (animationStep == animationStart) && (wormContours == null) )
			{
				String imageName = m_kVolumeImageA.GetImage().getImageName();
				if (imageName.contains("_clone")) {
					imageName = imageName.replaceAll("_clone", "");
				}
				String voiDir = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator + "animation" + File.separator + "wormContours" + File.separator;
				VOIVector results = new VOIVector();
				LatticeModel.loadAllVOIsFrom( m_kVolumeImageA.GetImage(), voiDir, true, results, false);
				if ( results.size() > 0 )
				{
					wormContours = results.elementAt(0);
					for ( int i = wormContours.getCurves().size() - 1; i <= 0; i -= 2 )
					{
						wormContours.getCurves().remove(i);
					}
					wormContoursAnimate = new VOI(wormContours);
					wormContoursAnimate.getCurves().removeAllElements();
					wormContoursAnimate.getCurves().add( wormContours.getCurves().remove(0) );
					m_kVolumeImageA.GetImage().registerVOI(wormContoursAnimate);
					wormContoursStatic = new VOI(wormContoursAnimate);
					m_kVolumeImageA.GetImage().registerVOI(wormContoursStatic);
				}
				else
				{
					contoursDone = true;
					animationStart = animationStep + 10;
					animationStop = animationStart;
				}
			}
			else if ( !contoursDone && (wormContours != null) && (animationStep > animationStart) )
			{
				int add = animationStep - animationStart;
//				if ( (add % 2) == 0 )
				{
					if ( wormContours.getCurves().size() > 0 )
					{
						wormContoursAnimate.getCurves().add( wormContours.getCurves().remove(0) );
						if ( wormContoursAnimate.getCurves().size() > 15 )
						{
							VOIContour temp = (VOIContour)wormContoursAnimate.getCurves().remove(0);
							if ( (add % 30) == 0 )
							{
								wormContoursStatic.getCurves().add(temp);
							}
						}
					}
					else if ( wormContoursAnimate.getCurves().size() > 0 )
					{
						VOIContour temp = (VOIContour)wormContoursAnimate.getCurves().remove(0);
						if ( wormContoursAnimate.getCurves().size() == 0 )
						{
							wormContoursStatic.getCurves().add(temp);
						}
					}
					else
					{
						contoursDone = true;
						animationStart = animationStep + 60;
					}
				}				
			}
			else if ( contoursDone && !growDone && (animationStep >= animationStart) )
			{
				if ( masks == null )
				{
					String imageName = m_kVolumeImageA.GetImage().getImageName();
					if (imageName.contains("_clone")) {
						imageName = imageName.replaceAll("_clone", "");
					}

					String dir = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator + "output_images" + File.separator + "masks" + File.separator;
					String fileName = imageName + "_0_insideMask.xml";
					FileIO fileIO = new FileIO();
					masks = fileIO.readImage(fileName, dir, true, null); 
					masks.calcMinMax();
					//				new ViewJFrameImage(masks);
					int numVolumes = masks.getExtents().length > 3 ? masks.getExtents()[3] : 1;
					addCount = (int) (360/(1.5*numVolumes));

					m_kVolumeImageA.GetImage().unregisterAllVOIs();
					m_kVolumeImageA.GetImage().registerVOI(wormContoursStatic);
					model = new LatticeModel( m_kVolumeImageA.GetImage() );
					model.setLattice( lattice );
					modelExtents = model.getExtent();
				}
				if ( modelData == null )
				{
					String imageName = m_kVolumeImageA.GetImage().getImageName();
					if (imageName.contains("_clone")) {
						imageName = imageName.replaceAll("_clone", "");
					}

					String dir = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator + "output_images" + File.separator;
					String fileName = imageName + "_model.xml";
					FileIO fileIO = new FileIO();
					modelData = fileIO.readImage(fileName, dir, false, null); 
					modelData.calcMinMax();						
				}
//				else
				{
					int add = animationStep - animationStart;
					Vector3f pos = new Vector3f();
					Vector3f imagePos = new Vector3f();
					int numVolumes = masks.getExtents().length > 3 ? masks.getExtents()[3] : 1;
					if ( (add % addCount) == 0 && (timeStep < numVolumes) )
					{
						int dimX = masks.getExtents().length > 0 ? masks.getExtents()[0] : 0;
						int dimY = masks.getExtents().length > 1 ? masks.getExtents()[1] : 0;
						int dimZ = masks.getExtents().length > 2 ? masks.getExtents()[2] : 0;
						double max = m_kVolumeImageA.GetImage().getMax();
						double min = m_kVolumeImageA.GetImage().getMin();
						for ( int z = 0; z < dimZ; z++ )
						{
							for ( int y = 0; y < dimY; y++ )
							{
								for ( int x = 0; x < dimX; x++ )
								{
									int modelIndex = modelData.getInt(x, y, z);
									if ( modelIndex != 0 )
									{
										pos.copy( centerCurveAnimate.getCurves().elementAt(0).elementAt( modelIndex - 1 ) );
										imagePos.set(x,y,z);
										float dist = imagePos.distance(pos);
										if ( (masks.getFloat(x, y, z, timeStep) > 0) && (dist < modelExtents) )
										{
											m_kVolumeImageA.GetImage().set(x, y, z, Math.max( (max/25f), m_kVolumeImageA.GetImage().getFloat(x, y, z) ) );
										}
										else if ( timeStep == (numVolumes - 1) )
										{
											m_kVolumeImageA.GetImage().set(x, y, z, min );
										}
									}
								}
							}
						}
						m_kVolumeImageA.UpdateData(m_kVolumeImageA.GetImage());
						timeStep++;
						if ( timeStep == (numVolumes) )
						{
							growDone = true;
							animationStart = animationStep + 60;
							masks.disposeLocal();
							masks = null;
						}
					}
				}				
			}
			else if ( growDone && !extractDone && (animationStep == animationStart) )
			{
//				rotate = false;
				sliceContoursStatic = model.getSamplingPlanes(false);
				sliceContours = model.getSamplingPlanes(true);
//				System.err.println( 2 * (model.getExtent()+10) );
				
				final short sID = (short) (m_kVolumeImageA.GetImage().getVOIs().getUniqueID());
				sliceContoursAnimate = new VOI(sID, "samplingPlanes");
				m_kVolumeImageA.GetImage().registerVOI(sliceContoursAnimate);
				wormContoursStatic.getCurves().remove(0);
				wormContoursStatic.getCurves().remove(0);
			}
			else if ( growDone && !extractDone && (animationStep > animationStart) && (sliceContours != null) && (model != null) )
			{
				int add = animationStep - animationStart;
				if ( (wormContoursStatic.getCurves().size()) > 0 && ((add % 30) == 0) )
				{
					wormContoursStatic.getCurves().remove(0);
				}
							
				VOIContour box = null;
				if ( sliceContours.getCurves().size() > 0 )
				{
					sliceContoursAnimate.getCurves().add( sliceContours.getCurves().remove(0) );
					if ( sliceContoursAnimate.getCurves().size() > 1 )
					{
						box = (VOIContour)sliceContoursAnimate.getCurves().remove(0);
					}
				}
				else if ( sliceContoursAnimate.getCurves().size() > 0 )
				{
					box = (VOIContour)sliceContoursAnimate.getCurves().remove(0);
				}
				else
				{
					extractDone = true;
					animationStart = animationStep + 30;
					animationStop = animationStart;
					modelData.disposeLocal();
					modelData = null;
					wormContoursStatic.getCurves().removeAllElements();
				}	
				if ( box != null )
				{
					model.removeDiagonal( m_kVolumeImageA.GetImage(), modelData, sliceContoursStatic, new int[]{2*modelExtents,2*modelExtents}, extractSlice++ );
					m_kVolumeImageA.UpdateData(m_kVolumeImageA.GetImage());
				}
			}
*/
			/*
			if ( !displayStraight && (animationStep == animationStart) )
			{
				rotate = false;
				setClipPlane(5, 0, true);
			}
			if ( !displayStraight && (animationStep > animationStart) )
			{
				int add = animationStep - animationStart;
				System.err.println( add );
				setClipPlane(5, add, true);
				if ( add == m_kVolumeImageA.GetImage().getExtents()[2] )
				{
					animationStart = animationStep + 360;
					animationStop = animationStart;
					rotate = true;

					m_kYRotate.fromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/180.0f);
					displayStraight = true;
					setClipPlane(5, add, false);
					displayClipPlane(5, false, new ColorRGB(0,0,1) );
				}
			}
			*/
			
			/*
			if ( !displaySlice && (animationStep == animationStart) )
			{
				rotate = false;
				displayVolumeRaycast(false);
				displayVolumeSlices(true);
				showBoundingBox(0, true);
				showBoundingBox(1, false);
				showBoundingBox(2, false);
				showSlice(0, true);
				showSlice(1, false);
				showSlice(2, false);
				setBoundingBoxColor(0, new ColorRGB(0,0,1));
				setOrthographicProjection();
				m_kNewCenter.Z = 0;
				m_bUpdateCenterOnDisplay = true;
			}
			if ( !displaySlice && (animationStep > animationStart) )
			{
				int add = animationStep - animationStart;
				System.err.println( add );
				m_kNewCenter.Z = add;
				m_bUpdateCenterOnDisplay = true;
				if ( add == m_kVolumeImageA.GetImage().getExtents()[2] )
				{
					animationStart = animationStep + 1;
					animationStop = animationStart;
					displaySlice = true;
				}
			}
			*/
			/*
			if ( animationStep >= animationStart )
			{
				if ( firstWormAnimation == true )
				{
					firstWormAnimation = false;
					m_kXRotate.fromAxisAngle(Vector3f.UNIT_X, (float)Math.PI/2f);
					Matrix3f kRotate = m_spkScene.Local.GetRotate();
					kRotate.mult(m_kXRotate);
					m_spkScene.Local.SetRotate(kRotate);
					m_spkScene.UpdateGS();
					m_kCuller.ComputeVisibleSet(m_spkScene);
					for (int i = 0; i < m_kDisplayList.size(); i++)
					{
						m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
					}
					m_kXRotate.fromAxisAngle(Vector3f.UNIT_X, (float)Math.PI/180.0f);
				}
				Matrix3f kRotate = m_spkScene.Local.GetRotate();
				kRotate.mult(m_kXRotate);
				m_spkScene.Local.SetRotate(kRotate);
				m_spkScene.UpdateGS();
				m_kCuller.ComputeVisibleSet(m_spkScene);
				for (int i = 0; i < m_kDisplayList.size(); i++)
				{
					m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
				}
				if ( animationStep == 360 )
				{
					straightRotateDone = true;
					animationStart = animationStep + 1;
					animationStop = animationStart;
				}
			}
			
			if ( wormAnimationStep() == 2 )
			{
				if ( screenShots == null )
				{
					screenShots = new Vector<BufferedImage>();
				}
				screenShots.add(m_pkRenderer.Screenshot());
				if ( seamCellsDone && !seamCellsSaved )
				{
					writeData("seamCells");
					seamCellsSaved = true;
					screenShots.clear();
				}
				if ( pairsDone && !pairsSaved )
				{
					writeData("pairs");
					pairsSaved = true;
					screenShots.clear();
				}
				if ( latticeDone && !latticeSaved )
				{
					writeData("lattice");
					latticeSaved = true;
					screenShots.clear();
				}
				if ( curvesDone && !curvesSaved )
				{
					writeData("curves");
					curvesSaved = true;
					screenShots.clear();
				}
				if ( contoursDone && !contoursSaved )
				{
					writeData("contours");
					contoursSaved = true;
					screenShots.clear();
				}
				if ( growDone && !growSaved )
				{
					writeData("grow");
					growSaved = true;
					screenShots.clear();
				}
				if ( extractDone && !extractSaved )
				{
					writeData("extract");
					extractSaved = true;
					screenShots.clear();
				}
				if ( straightRotateDone && !straightRotateSaved )
				{
					writeData("straightRotate");
					straightRotateSaved = true;
					screenShots.clear();
				}
			}
			
			// end animation:
			animationStep++;
			if ( (animationStep == animationStop) && (animationStart == animationStop) )
			{
				System.err.println(animationStep);
				animationStep = 0;
				setWormAnimationStep(-1);
				System.err.println("DONE");
			}
			*/
		}
	}

	public void addConfiguredListener( RendererListener listener )
	{
		configuredListener = listener;
	}
	
	public void dispose(GLAutoDrawable kDrawable)
	{
		super.dispose(kDrawable);
		m_kParent = null;
	}

	public void setVOILatticeManager( VOILatticeManagerInterface newVOIInterface )
	{
		m_kVOIInterface = newVOIInterface;
    	if ( m_kVOIInterface != null )
    	{
    		m_kVOIInterface.setRenderer(this);
    	}
	}
	
    public void clear3DSelection()
    {
    	if ( m_kVOIInterface != null )
    	{
    		m_kVOIInterface.clear3DSelection();
    	}
    }
    
    public String annotationPrefix() 
    {
    	return (m_kVOIInterface == null) ? "A" : m_kVOIInterface.getAnnotationPrefix();
    }
    
    public boolean doAutomaticLabels()
	{
    	return (m_kVOIInterface == null) ? false : m_kVOIInterface.doAutomaticLabels();
	}
    
	public boolean is3DSelectionEnabled()
	{
    	return (m_kVOIInterface == null) ? false : m_kVOIInterface.is3DSelectionEnabled();
	}
	
	public boolean isEditAnnotations()
	{
    	return m_kVOIInterface == null ? false : m_kVOIInterface.isEditAnnotations();
	}
	
	/*
	private boolean firstWormAnimation = true;
	private LatticeModel model;
	private int modelExtents = -1;
	private boolean rotate = true;
	private int animationStep = 0;
	private int animationStart = 0;
	private int animationStop = Integer.MAX_VALUE;
	private int allSeamCellsAdded = 0;
	private int allPairsAdded = 0;
	private int addCount = 0;
	private VOI annotations;
	private VOI pairs;
	private VOI lattice;
	private boolean latticeDone = false, latticeSaved = false;
	private boolean seamCellsDone = false, seamCellsSaved = false;
	private boolean pairsDone = false, pairsSaved = false;
	private boolean curvesDone = false, curvesSaved = false;
	private boolean contoursDone = false, contoursSaved = false;
	private boolean growDone = false, growSaved = false;
	private boolean extractDone = false, extractSaved = false;
	private boolean displayStraight = false;
	private boolean displaySlice = false;
	private boolean straightRotateDone = false, straightRotateSaved = false;
	private Vector3f sliceCenter;
	private int extractSlice = 0;
	private VOI leftCurve, rightCurve, centerCurve, wormContours, sliceContours;
	private VOI leftCurveAnimate, rightCurveAnimate, centerCurveAnimate, wormContoursAnimate, wormContoursStatic, sliceContoursAnimate, sliceContoursStatic;
	private Vector3f nose;
	private ModelImage masks;
	private ModelImage modelData;
	private ModelImage straightImage;
	private Vector<BufferedImage> screenShots;
	private int timeStep = 0;
	
	public int wormAnimationStep()
	{
    	return m_kVOIInterface == null ? -1 : m_kVOIInterface.getAnimationStep();
	}
	
	public void setWormAnimationStep( int value )
	{
    	if ( m_kVOIInterface != null )
    	{
    		m_kVOIInterface.setAnimationStep(value);
    	}
	}
	
	private void writeData( String fileName )
	{
		String imageName = m_kVolumeImageA.GetImage().getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		String directory = m_kVolumeImageA.GetImage().getImageDirectory() + imageName + File.separator;
		//			if ( rotate )
		//			{
		//				directory = directory + "WormAnimationStraight" + File.separator;
		//			}
		//			else
		//			{
		//				directory = directory + "WormAnimationSlice" + File.separator;
		//			}
		directory = directory + "WormAnimation" + File.separator;
		File dir = new File( directory );
		if ( !dir.exists() )
		{
			dir.mkdir();
		}

		System.err.println( "writeData " + screenShots.size() );
		int length = Math.min( 500, screenShots.size() );
		BufferedImage bImage = screenShots.elementAt(0);
		ModelImage movieImage = new ModelImage( ModelStorageBase.ARGB, new int[]{bImage.getWidth(), bImage.getHeight(), length}, fileName );

		for ( int z = 0; z < length; z++ )
		{
			bImage = screenShots.elementAt(z);
			for ( int y = 0; y < bImage.getHeight(); y++ )
			{
				for ( int x = 0; x < bImage.getWidth(); x++ )
				{
					int index = z * bImage.getWidth()*bImage.getHeight() + y * bImage.getWidth() + x;
					int iARGB = bImage.getRGB( x, y );				
					movieImage.set( index * 4 + 0, (byte)((iARGB & 0xff000000) >> 32) );
					movieImage.set( index * 4 + 1, (byte)((iARGB & 0x00ff0000) >> 16) );
					movieImage.set( index * 4 + 2, (byte)((iARGB & 0x0000ff00) >> 8) );
					movieImage.set( index * 4 + 3, (byte)((iARGB & 0x000000ff)) );
				}
			}
		}
		movieImage.setImageName(fileName);
		ModelImage.saveImage(movieImage, movieImage.getImageName() + ".xml", directory, false);
		movieImage.disposeLocal();

		for ( int z = 0; z < length; z++ )
		{
			screenShots.remove(0);
		}
		if ( screenShots.size() > 0 )
		{
			writeData( fileName + "_" + length );
		}
		else
		{
			screenShots.clear();
		}
	}
	*/
    public void deleteSelectedPoint( )
    {
		if ( m_kVOIInterface != null )
    	{
    		m_kVOIInterface.deleteSelectedPoint( );
			m_kVOIInterface.updateDisplay();
    	}    	    	
    }
    
    public void moveSelectedPoint( Vector3f direction )
    {
		if ( m_kVOIInterface != null )
    	{
    		m_kVOIInterface.moveSelectedPoint( direction );
			m_kVOIInterface.updateDisplay();
    	}    	    	
    }
    
    public boolean hasSelectedPoint()
    {
		if ( m_kVOIInterface != null )
    	{
			return m_kVOIInterface.hasSelectedPoint();
    	}    	    	
		return false;
    }
	
	/**
	 * Part of the KeyListener interface. Pressing 'b' toggles displaying the
	 * proxy-geometry versus the ray-traced volume.
	 * @param e the key event.
	 */
	public void keyPressed( KeyEvent e )
	{
		shiftPressed = e.isShiftDown();
    	if ( m_kVOIInterface != null )
    	{
    		m_kVOIInterface.setShiftDown(shiftPressed);
    	}
		altPressed = e.isAltDown();
		if ( is3DSelectionEnabled() )
		{
			Transformation world = m_kVolumeRayCast.getMesh().World;
			switch( e.getKeyCode() )
			{
			case KeyEvent.VK_UP:
				if ( hasSelectedPoint() ) {
					m_kVolumeRayCast.GetScene().UpdateGS();
					moveSelectedPoint( world.InvertVector(m_spkCamera.GetUVector()) );
				}
				else {
					MoveDown();
				}
				break;
			case KeyEvent.VK_DOWN:
				if ( hasSelectedPoint() ) {
					m_kVolumeRayCast.GetScene().UpdateGS();
					moveSelectedPoint( world.InvertVector(m_spkCamera.GetUVector()).neg() );
				}
				else {
					MoveUp();
				}
				break;
			case KeyEvent.VK_RIGHT:
				if ( hasSelectedPoint() ) {
					m_kVolumeRayCast.GetScene().UpdateGS();
					moveSelectedPoint( world.InvertVector(m_spkCamera.GetRVector()) );
				}
				else {
					MoveLeft();
				}
				break;
			case KeyEvent.VK_LEFT:
				if ( hasSelectedPoint() ) {
					m_kVolumeRayCast.GetScene().UpdateGS();
					moveSelectedPoint( world.InvertVector(m_spkCamera.GetRVector()).neg() );
				}
				else {
					MoveRight();
				}
				break;
			case KeyEvent.VK_DELETE:
				deleteSelectedPoint( );
				break;
			case KeyEvent.VK_PAGE_DOWN:
				applyClipFilter(true);
				break;
			case KeyEvent.VK_PAGE_UP:
				applyClipFilter(false);
				break;
			}
	        // look for shortcuts now
			if ( m_kVOIInterface != null )
			{
//				System.err.println( "keyPressed " + e.getKeyChar() );
				if ( e.getKeyChar() == 'f' || e.getKeyChar() == 'F' ||
					e.getKeyChar() == 'h' || e.getKeyChar() == 'H' ||
					e.getKeyChar() == 's' || e.getKeyChar() == 'S' )
				{
					m_kVOIInterface.updateSelectedPoint( Color.green );
				}
				if ( e.getKeyChar() == 'e' || e.getKeyChar() == 'E' ||
					 e.getKeyChar() == 'l' || e.getKeyChar() == 'L' ||
					 e.getKeyChar() == 't' || e.getKeyChar() == 'T' )
				{
					m_kVOIInterface.updateSelectedPoint( Color.red );
				}
				if ( e.getKeyChar() == 'b' || e.getKeyChar() == 'B' )
				{
					m_kVOIInterface.updateSelectedPoint( Color.blue );
				}
				if ( e.getKeyChar() == 'v' || e.getKeyChar() == 'V' )
				{
					displayVolumeRaycast(!m_kVolumeRayCast.GetDisplay());
					displayVolumeSlices( !m_kVolumeRayCast.GetDisplay());
					setVolumeBlend(1.0f);
					setVolumeSamplesMouseReleased(.7f);
					setVolumeSamplesMouseDragged(.7f);
				}
			}

	        String command = null;
	        final KeyStroke ks = KeyStroke.getKeyStrokeForEvent(e);

	        command = Preferences.getShortcutCommand(ks);

	        if ( (m_kVOIInterface != null) && (command != null) )
	        {
	        	m_kVOIInterface.actionPerformed(new ActionEvent(ks, 0, command));
	        }
	        // Don't pass the VOI key-commands to the actionPerformed function, this
	        // will be done by the VOIManager which will also get the KeyEvents.
	        if ( (m_kParent != null) && (command != null) )
	        {
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
	
//	public void mouseClicked(MouseEvent e) {
//		super.mouseClicked(e);
//		clickCount = e.getClickCount();
//		if ( e.getClickCount() > 1 )
//		{
//			clear3DSelection();
//			m_iXPick = e.getX();
//			m_iYPick = e.getY();
//			m_bPickPending = true;
//		}
//	}
	
	public void keyReleased(KeyEvent e) {

    	if ( m_kVOIInterface != null )
    	{
    		m_kVOIInterface.setShiftDown(e.isShiftDown());
    	}
		super.keyReleased(e);
	}

	public void mousePressed(MouseEvent e) {
		super.mousePressed(e);
		if (e.isControlDown() && is3DSelectionEnabled()) {
			rightMousePressed = ((e.getModifiers() & InputEvent.BUTTON3_MASK) != 0);
			altPressed = e.isAltDown();
			shiftPressed = e.isShiftDown();
//			if ( !isEditAnnotations() || !e.isAltDown() ) {
			if ( !isEditAnnotations()  ) {
//				System.err.println("mousePresed clear3DSelection");
				clear3DSelection();
			}
			m_iXPick = e.getX();
			m_iYPick = e.getY();
			m_bPickPending = true;
		}
		if (configuredListener != null )
		{		
    		configuredListener.setActiveRenderer( this );
		}
	}
	public void mouseReleased(MouseEvent e) {
		super.mouseReleased(e);
		rightMousePressed = false;
		altPressed = e.isAltDown();
		shiftPressed = e.isShiftDown();
		m_bMouseDrag = false;
		setDefaultCursor();
	}

    public void setDefaultCursor( )
    {
		if ( m_kParent != null )
		{
			m_kParent.setDefaultCursor();
		}
    }
    
    public boolean select3DMarker( Vector3f startPt, Vector3f endPt, Vector3f pt, boolean rightMouse, boolean multiSelect )
    {
    	if ( m_kVOIInterface != null )
    	{
    		return m_kVOIInterface.select3DMarker(startPt, endPt, pt, rightMouse, multiSelect, shiftPressed);
    	}
    	return false;
    }
    
    public boolean modify3DMarker( Vector3f startPt, Vector3f endPt, Vector3f pt )
    {
    	if ( m_kVOIInterface != null )
    	{
    		return m_kVOIInterface.modify3DMarker(startPt, endPt, pt);
    	}
    	return false;
    }

    public void add3DMarker( VOI textVOI, boolean automaticLabel, boolean multiSelect, boolean isShift )
    {
    	if ( m_kVOIInterface != null )
    	{
    		m_kVOIInterface.add3DMarker(textVOI, automaticLabel, multiSelect, isShift);
    	}    	
    }
    
    public void add3DMarker( VOI textVOI, boolean automaticLabel, boolean multiSelect )
    {
    	add3DMarker(textVOI, automaticLabel, multiSelect, false);
    }

	/** Rotates the object with a virtual trackball:
	 * @param e the MouseEvent
	 */
	@Override
	public void mouseDragged(MouseEvent e)
	{
		if (!e.isControlDown() && ((e.getModifiers() & InputEvent.BUTTON3_MASK) != 0))
		{
			processRightMouseDrag(e);
		}
		else
		{
			super.mouseDragged(e);
		}
		if ( m_kParent != null ) {
			m_kParent.setCameraParameters();
			m_kParent.setObjectParameters();
		}
		if (e.isControlDown() && is3DSelectionEnabled()) {
			m_iXPick = e.getX();
			m_iYPick = e.getY();
			m_bPickPending = true;
			m_bMouseDrag = true;
		}
	}

	 /**
	  * If the right mouse button is pressed and dragged. processRightMouseDrag
	  * updates the HistoLUT window and level (contrast and brightness)
	  *
	  * @param  kEvent  the mouse event generated by a mouse drag
	  */
	 private void processRightMouseDrag(MouseEvent kEvent)
	 {

		 /* Get the coordinates of the mouse position in local coordinates: */
		 float fX = Math.max(0, (float)kEvent.getX() / (float) GetWidth() );
		 fX = Math.min(fX, 1 );
		 float fY = Math.max(0, (float)kEvent.getY() / (float) GetHeight() );
		 fY = Math.min(fY, 1 );

		 float min = (float) m_kVolumeImageA.GetImage().getMin();
		 float max = (float) m_kVolumeImageA.GetImage().getMax();
		 TransferFunction kTransfer = new TransferFunction();
		 kTransfer.removeAll();
		 kTransfer.addPoint(min, 255);
		 kTransfer.addPoint((min + fX * ((max - min) * 2.0f / 3.0f)), 255 * fY);
		 kTransfer.addPoint((min + ((max - min) * 2.0f / 3.0f)), 255 * 0.333f);
		 kTransfer.addPoint(max, 0);
		 m_kVolumeImageA.UpdateImages(kTransfer, 0, null);		 
		 
//		 setWindowLevel( fX,  fY, m_bFirstDrag );
		 m_bFirstDrag = false;
	 }
	 
	 private void setWindowLevel(float fX, float fY, boolean first )
	 {
		 // make the LUT panel the active panel.
		 if ( m_kParent != null )
		 {
			 m_kParent.actionPerformed(new ActionEvent(this, 0, "HistoLUT"));
		 }
		 ModelStorageBase kActiveLookupTable = null;
		 /* Get which image is active, either m_kImageA or m_kImageB: */
		 ModelImage kActiveImage = null;
		 
		 if ( m_kParent != null )
		 {
			 kActiveImage = m_kParent.getHistoLUTActiveImage();
			 if (kActiveImage == null) {
				 kActiveImage = m_kParent.getHistoRGBActiveImage();
			 }
			 kActiveLookupTable = m_kParent.getActiveLookupTable(kActiveImage);
		 }

		 if ( kActiveImage == null )
		 {
			 kActiveImage = m_kVolumeImageA.GetImage();
		 }
		 if ( kActiveLookupTable == null )
		 {
			 if ( kActiveImage.isColorImage() )
			 {
				 kActiveLookupTable = m_kVolumeImageA.GetRGB();
			 }
			 else
			 {
				 kActiveLookupTable = m_kVolumeImageA.GetLUT();
			 }
		 }
		 
		 if ( m_kWinLevel.updateWinLevel( fX, fY, first, kActiveLookupTable, kActiveImage ) )
		 {
			 if ( kActiveImage == m_kVolumeImageA.GetImage() )
			 {
				 if ( m_kParent != null )
				 {
					 m_kParent.getLUTDialog().setLUTA(kActiveLookupTable);
				 }
				 else if ( kActiveImage.isColorImage() )
				 {
					 m_kVolumeImageA.SetRGBT((ModelRGB)kActiveLookupTable);
				 }
				 else
				 {					 
					 m_kVolumeImageA.UpdateImages((ModelLUT)kActiveLookupTable);
				 }
			 }
			 else if ( (m_kVolumeImageB.GetImage() != null) && (kActiveImage == m_kVolumeImageB.GetImage()) )
			 {
				 if ( m_kParent != null )
				 {
					 m_kParent.getLUTDialog().setLUTB(kActiveLookupTable);
				 }
				 else if ( kActiveImage.isColorImage() )
				 {
					 m_kVolumeImageB.SetRGBT((ModelRGB)kActiveLookupTable);
				 }
				 else
				 {
					 m_kVolumeImageB.UpdateImages((ModelLUT)kActiveLookupTable);
				 }
			 }
		 }
		 if (first) {
			 if ( m_kParent != null )
			 {
				 try {
					 Image kImg = MipavUtil.getIconImage("qkwinlevel.gif");
					 Cursor kWinLevelCursor = Toolkit.getDefaultToolkit().createCustomCursor(kImg, new Point(12, 12),
							 "WinLevel");
					 /* Set the cursor icon: */
					 m_kParent.setCursor(kWinLevelCursor);
				 } catch (FileNotFoundException error) { }
			 }
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
	
	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRenderBase#reCreateScene(gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage)
	 */
	public void reCreateScene(VolumeImage image) 
	{
		super.reCreateScene(image);

		if ( m_kParent != null )
		{
			m_kParent.addSlices(m_kSlices);
		}
	}
	
//	public void reCreateScene(VolumeImage imageA, boolean updateListeners) 
//	{
//		super.reCreateScene(imageA);
//
//		if ( m_kParent != null )
//		{
//			m_kParent.addSlices(m_kSlices);
//		}
//		if ( updateListeners && configuredListener != null ) {
//    		configuredListener.algorithmPerformed( new AlgorithmReslice(m_kVolumeImageA.GetImage(), 0) );
//		}
//	}


	/**
	 * Picking. If a display list object has picking enabled, find the picked polygon based on the mouse position. 
	 */
	protected void Pick()
	{
		Vector3f kPos = new Vector3f(0,0,10);
		Vector3f kDir = new Vector3f(0,0,1);  // the pick ray

		if (m_bPickPending)
		{
//			System.err.println("Width = " + GetWidth() + "   height = " + GetHeight() );
			if (m_spkCamera.GetPickRay(m_iXPick,m_iYPick,GetWidth(),
					GetHeight(),kPos,kDir))
			{				
				m_bPickPending = false;
				if ( is3DSelectionEnabled() )
				{
					Vector3f maxPt = new Vector3f();
					if ( m_kSlices.GetDisplay() )
					{
						// pick on the slices
						m_kPicker.Execute(m_kSlices.GetScene(),kPos,kDir,0.0f,
								Float.MAX_VALUE);
						if (m_kPicker.Records.size() > 0)
						{ 
							PickRecord kPickPoint = m_kPicker.GetClosestNonnegative();
							TriMesh kMesh = (TriMesh)kPickPoint.Intersected;
							int iPlane = m_kSlices.whichPlane(kMesh);

							Vector3f kP0 = kMesh.VBuffer.GetPosition3( kPickPoint.iV0 );
							kP0.scale(kPickPoint.B0);
							Vector3f kP1 = kMesh.VBuffer.GetPosition3( kPickPoint.iV1 );
							kP1.scale( kPickPoint.B1);
							Vector3f kP2 = kMesh.VBuffer.GetPosition3( kPickPoint.iV2 );
							kP2.scale( kPickPoint.B2 );
							Vector3f kPoint = Vector3f.add( kP0, kP1 );
							kPoint.add( kP2 );
							m_kVolumeRayCast.localToVolumeCoords( kPoint );
							maxPt.copy(kPoint);
							
							boolean picked = false;
//							System.err.println("mouse drag? " + m_bMouseDrag );
							if ( !m_bMouseDrag ) {
								picked = select3DMarker( null, null, maxPt, rightMousePressed, altPressed );
							}
							else if ( m_bMouseDrag ) {
								picked = modify3DMarker( null, null, maxPt );
							}
							
							if ( picked && (m_kVOIInterface != null) )
							{
								Vector<VOIWormAnnotation> selectedAnnotations = m_kVOIInterface.getPickedAnnotation();
								if ( selectedAnnotations != null ) {
									for ( int i = 0; i < selectedAnnotations.size(); i++ ) {
										selectedAnnotations.elementAt(i).setPlane(iPlane);
									}
								}
							}
							if ( !picked )
							{

								short id = (short) m_kVolumeImageA.GetImage().getVOIs().getUniqueID();
								int colorID = 0;
								VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
								VOIText textVOI = new VOIText( );
								textVOI.add( maxPt );
								textVOI.add( maxPt );
								textVOI.setText(""+id);
								if ( m_kVOIInterface != null )
								{
									if ( doAutomaticLabels() )
									{
										textVOI.setText("" + m_kVOIInterface.getCurrentIndex() );
									}
									else
									{
										textVOI.setText(annotationPrefix() + m_kVOIInterface.getCurrentIndex() );	
									}
								}
								else if ( doAutomaticLabels() )
								{
									textVOI.setText(""+id);										
								}
								else if ( !doAutomaticLabels() )
								{
									textVOI.setText(annotationPrefix() + id);	
								}
								newTextVOI.getCurves().add(textVOI);
								textVOI.setPlane( iPlane );
//								System.err.println("shift down? " + shiftPressed );
								add3DMarker( newTextVOI, doAutomaticLabels(), altPressed );
							}
							m_kVOIInterface.updateDisplay();
							return;
						}
					}
						
					// pick in the volume.
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
							
							Vector3f p0 = new Vector3f(firstIntersectionPoint);
							Vector3f p1 = new Vector3f(secondIntersectionPoint);
							Vector3f step = Vector3f.sub(p1, p0);
							Vector3f test = new Vector3f();
							float numSteps = step.length() + 1;
							step.normalize();
							for ( int i = 0; i < numSteps; i++ )
							{
								// step along the ray and pick the voxel with the highest value:
								p0.add(step);
								// test for clipping:
								if ( m_kVolumeRayCast.GetShaderEffect().isClip() )
								{
									test.copy( p0 );
									Vector3f clip = m_kVolumeRayCast.GetShaderEffect().getClip();  
									clip.scale((m_kVolumeImageA.GetImage().getExtents()[0] - 1), (m_kVolumeImageA.GetImage().getExtents()[1] - 1), (m_kVolumeImageA.GetImage().getExtents()[2] - 1) );
									Vector3f clipInv = m_kVolumeRayCast.GetShaderEffect().getClipInv();
									clipInv.scale((m_kVolumeImageA.GetImage().getExtents()[0] - 1), (m_kVolumeImageA.GetImage().getExtents()[1] - 1), (m_kVolumeImageA.GetImage().getExtents()[2] - 1) );
									
									if ( (test.X < clip.X) || (test.X > clipInv.X) || (test.Y < clip.Y) || (test.Y > clipInv.Y) || (test.Z < clip.Z) || (test.Z > clipInv.Z) )
									{
										continue;
									}
									
								}
								if ( sphereClip != null )
								{
									test.copy( p0 );
									m_kVolumeRayCast.volumeToLocalCoords( test );
//									System.err.println( "Pick " + test + "     " + sphereClipLocal );
//									if ( !ellipsoidClipLocal.Contains(test) ) {
//										continue;
//									}
								}
								float value;
								if ( m_kVolumeImageA.GetImage().isColorImage() )
								{
									boolean useRed = m_kVolumeImageA.GetRGB().getROn();
									boolean useGreen = m_kVolumeImageA.GetRGB().getGOn();
									boolean useBlue = m_kVolumeImageA.GetRGB().getBOn();
									
									float red = 0;
									float green = 0;
									float blue = 0;
									if ( useRed && !(m_kVolumeRayCast.getDisplayGreenAsGray() || m_kVolumeRayCast.getDisplayBlueAsGray())  )
									{
										red = m_kVolumeImageA.GetImage().getFloatTriLinearBounds(p0.X, p0.Y, p0.Z, 1);
									}
									if ( useGreen && !(m_kVolumeRayCast.getDisplayRedAsGray() || m_kVolumeRayCast.getDisplayBlueAsGray()) )
									{
										green = m_kVolumeImageA.GetImage().getFloatTriLinearBounds(p0.X, p0.Y, p0.Z, 2);
									}
									if ( useBlue && !(m_kVolumeRayCast.getDisplayRedAsGray() || m_kVolumeRayCast.getDisplayGreenAsGray())  )
									{
										blue = m_kVolumeImageA.GetImage().getFloatTriLinearBounds(p0.X, p0.Y, p0.Z, 3);
									}
									value = Math.max(red, Math.max(green, blue) );
								}
								else 
								{
									value = m_kVolumeImageA.GetImage().getFloatTriLinearBounds(p0.X, p0.Y, p0.Z);
									if ( (m_kVolumeImageB != null) &&  (m_kVolumeImageB.GetImage() != null))
									{
										float valueB = m_kVolumeImageB.GetImage().getFloatTriLinearBounds(p0.X, p0.Y, p0.Z);
										float blend = getABBlend();
										value = (blend * value + (1 - blend) * valueB);
									}
								}
								if ( value > maxValue )
								{
									maxValue = value;
									maxPt.copy(p0);
								}
							}
							
							if ( maxValue != -Float.MAX_VALUE )
							{					
								boolean picked = false;
//								System.err.println( "mouse drag? " + m_bMouseDrag );
								if ( !m_bMouseDrag ) {
									// select or create a new marker:
									picked = select3DMarker( firstIntersectionPoint, secondIntersectionPoint, maxPt, rightMousePressed, altPressed );
								}
								else if ( m_bMouseDrag ) {
									// modify currently selected, if exists
									picked = modify3DMarker( firstIntersectionPoint, secondIntersectionPoint, maxPt );
								}
								if ( !picked )
								{
									// add a new picked point:
									short id = (short) m_kVolumeImageA.GetImage().getVOIs().getUniqueID();
									int colorID = 0;
									VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
									VOIText textVOI = new VOIText( );
									textVOI.add( maxPt );
									textVOI.add( maxPt );
//									Transformation world = m_kVolumeRayCast.getMesh().World;
//									Vector3f dir = world.InvertVector(m_spkCamera.GetRVector());
//									dir.scale(5);
//									textVOI.add( Vector3f.add( dir, maxPt) );
									textVOI.setText(""+id);
									if ( m_kVOIInterface != null )
									{
										if ( doAutomaticLabels() )
										{
											textVOI.setText("" + m_kVOIInterface.getCurrentIndex() );
										}
										else
										{
											textVOI.setText(annotationPrefix() + m_kVOIInterface.getCurrentIndex() );	
										}
									}
									else if ( doAutomaticLabels() )
									{
										textVOI.setText(""+id);										
									}
									else if ( !doAutomaticLabels() )
									{
										textVOI.setText(annotationPrefix() + id);	
									}
									newTextVOI.getCurves().add(textVOI);
									add3DMarker( newTextVOI, doAutomaticLabels(), altPressed, shiftPressed );
								}
								m_kVOIInterface.updateDisplay();
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
		m_bMouseDrag = false;
	}

	protected void update4D( boolean bForward )
	{
		m_kVolumeImageA.update4D(bForward);
		if ( m_kVolumeImageB != null )
		{
			m_kVolumeImageB.update4D(bForward);			
		}
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
		if ( kVOIs == null )
		{
			return;
		}
		boolean bUpdateVOIs = false;
		boolean clipVOI = false;
		for ( int i = 0; i < m_kDisplayList.size(); i++ )
		{
			if ( m_kDisplayList.get(i) instanceof VolumeVOI )
			{
				VolumeVOI volVOI = (VolumeVOI)m_kDisplayList.elementAt(i);
				if ( volVOI.GetClipped() )
				{
					clipVOI = true;
				}
				m_kDisplayList.remove(i);
				i--;
			}
		}
		for (int i = kVOIs.size() - 1; i >=0; i--) {
			VOI kVOI = kVOIs.get(i);
//			System.err.println("updateVOIs " + kVOI.getName() );
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
				if ( kCurves != null ) {
				for (int k = 0; k < kCurves.size(); k++) {
					VOIBase kVOI3D = kCurves.get(k);
					boolean draw = true;
					if ( latticeClip && kVOI3D instanceof VOIWormAnnotation ) {
						draw = clipAnnotations( (VOIWormAnnotation)kVOI3D);
					}
					if ( draw ) {
						bUpdateVOIs |= drawVOI(kVOI3D, this, m_kVolumeImageA,
								m_kTranslate);
					}
				}
				}
			}
		}
		if ( clipVOI || latticeClip ) {
			applyClipFilter(true);
		}
		else {
			applyClipFilter(false);
		}
		if ( bUpdateVOIs )
		{
			UpdateSceneRotation();
			if ( m_kParent != null )
			{
				m_kParent.setModified();
			}
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
	

	protected void update4DVOIs(boolean bForward) {		
		annotationVOIsUpdate(annotationSpheresIndex + 1);
	}

	private VOIVector annotationPositions;
	private HashMap<String, Float> annotationDiameters;
	private HashMap<String, VolumeSurface> annotationSpheres;
	private HashMap<String, Boolean> annotationSpheresDisplay;
	private int annotationSpheresIndex = -1;
	private HashMap<String, VOI> annotationVOIs;
	private HashMap<String, VOI[]> neuriteVOIs;
	private HashMap<String, VolumeSurface[]> neuriteSurfaces;
	private HashMap<String, Boolean> neuriteDisplay;
	private HashMap<String, ColorRGB> annotationSpheresColors;
	private JPanelAnnotationAnimation annotationAnimationPanel;
	private HashMap<String, Boolean> annotationLabelsDisplay;
	private String[] annotationNames;
	private float sphereScale = 3;
	public void addAnimationVOIs( VOIVector vois, JPanelAnnotationAnimation annotationAnimationPanel )
	{		
		Vector3f origin = new Vector3f();
		for ( int i = 0; i < vois.size(); i++ )
		{
			for ( int j = 0; j < vois.elementAt(i).getCurves().size(); j++ )
			{
				VOIText text = (VOIText) vois.elementAt(i).getCurves().elementAt(j);
				if ( text.size() == 0 )
				{
					System.err.println( text.getText() );
				}
				Vector3f position = text.elementAt(0);
				if ( text.getText().equals("origin" ) )
				{
//					System.err.println( position );
					origin.copy(position);
					break;
				}
			}
			for ( int j = 0; j < vois.elementAt(i).getCurves().size(); j++ )
			{
				VOIText text = (VOIText) vois.elementAt(i).getCurves().elementAt(j);
				text.setUseMarker( false );
				Vector3f position = text.elementAt(0);
				position.sub( origin );
				position.add( m_kVolumeImageA.GetImage().getExtents()[0]/2, m_kVolumeImageA.GetImage().getExtents()[1]/2, 0 );
//				position.add( 4, 0, 0 );
//				System.err.println( text.getText() + "      " + position );
				
				position = text.elementAt(1);
				position.sub(origin);
				position.add( m_kVolumeImageA.GetImage().getExtents()[0]/2, m_kVolumeImageA.GetImage().getExtents()[1]/2, 0 );
			}
		}
		
		
		
		annotationPositions = vois;
		this.annotationAnimationPanel = annotationAnimationPanel;
		Attributes attributes = new Attributes();
		attributes.SetPChannels(3);
		attributes.SetNChannels(3);
		attributes.SetCChannels(0,4);
		StandardMesh std = new StandardMesh(attributes);

		int dimX = m_kVolumeImageA.GetImage().getExtents().length > 0 ? m_kVolumeImageA.GetImage().getExtents()[0] : 1;
		int dimY = m_kVolumeImageA.GetImage().getExtents().length > 1 ? m_kVolumeImageA.GetImage().getExtents()[1] : 1;
		int dimZ = m_kVolumeImageA.GetImage().getExtents().length > 2 ? m_kVolumeImageA.GetImage().getExtents()[2] : 1;
//		System.err.println( dimX + " " + dimY + " " + dimZ );
		sphereScale = 0.05f * Math.min( dimX, Math.min( dimY, dimZ ) );
		sphereScale = Math.max( 3, sphereScale );
//		System.err.println( scale );
		
		Transformation xfrm = new Transformation();
		xfrm.SetUniformScale( sphereScale );
		int maxCount = -1;
		int maxIndex = -1;
		for ( int i = 0; i < vois.size(); i++ )
		{
			if ( vois.elementAt(i).getCurves().size() > maxCount )
			{
				maxCount = vois.elementAt(i).getCurves().size();
				maxIndex = i;
			}
		}

		annotationNames = new String[maxCount];
		annotationSpheres = new HashMap<String, VolumeSurface>();
		annotationLabelsDisplay = new HashMap<String, Boolean>();
		annotationSpheresDisplay = new HashMap<String, Boolean>();
		annotationVOIs = new HashMap<String, VOI>();
		annotationSpheresColors = new HashMap<String, ColorRGB>();
		annotationDiameters = new HashMap<String, Float>();
		for ( int i = 0; i < maxCount; i++ )
		{
			VOIText text = new VOIText( (VOIText)vois.elementAt(maxIndex).getCurves().elementAt(i) );
			text.setUseMarker(false);
			Color c = text.getColor();
			
			annotationNames[i] = new String( text.getText() );

			VOI annotationVOI = new VOI( (short)i, text.getName(), VOI.ANNOTATION, 0 );
			annotationVOI.setColor(c);
			annotationVOI.getCurves().add(text);
			text.createVolumeVOI( m_kVolumeImageA, m_kTranslate );
			m_kVolumeImageA.GetImage().registerVOI( annotationVOI );
			annotationLabelsDisplay.put( text.getText(), true );
			annotationVOIs.put( text.getText(), annotationVOI );
			
			ColorRGBA colorRGBA = new ColorRGBA(c.getRed()/255f,c.getGreen()/255f,c.getBlue()/255f,1);
			
			
			std.SetTransformation( xfrm );
			TriMesh sphere = std.Sphere(2);
			updateSphere( sphere, 0, 0, 0, colorRGBA );
			SurfaceState kSurface = new SurfaceState( sphere, text.getText() );	
			VolumeSurface surface = new VolumeSurface(m_kVolumeImageA,
					m_kVolumeImageB, m_kTranslate, m_fX, m_fY, m_fZ, kSurface, false, true);
			surface.SetDisplay(false);
			m_kDisplayList.add(surface);	
			annotationSpheres.put( text.getText(), surface );
			annotationSpheresDisplay.put( text.getText(), true );
			annotationSpheresColors.put( text.getText(), new ColorRGB(1,1,1) );
			annotationDiameters.put( text.getText(), 1f );
		}
		
		
//		for ( int i = 0; i < m_kDisplayList.size(); i++ )
//		{
//			System.err.println( m_kDisplayList.elementAt(i).GetName() + " " + m_kDisplayList.elementAt(i).GetDisplay() );
//		}
		
		annotationVOIsUpdate(maxIndex);
	}  
	

	public void addSphereVOIs( VOI annotations )
	{		
		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			text.setUseMarker( false );
		}
		
		Attributes attributes = new Attributes();
		attributes.SetPChannels(3);
		attributes.SetNChannels(3);
		attributes.SetCChannels(0,4);
		attributes.SetTChannels(0,3);
		StandardMesh std = new StandardMesh(attributes);

		int dimX = m_kVolumeImageA.GetImage().getExtents().length > 0 ? m_kVolumeImageA.GetImage().getExtents()[0] : 1;
		int dimY = m_kVolumeImageA.GetImage().getExtents().length > 1 ? m_kVolumeImageA.GetImage().getExtents()[1] : 1;
		int dimZ = m_kVolumeImageA.GetImage().getExtents().length > 2 ? m_kVolumeImageA.GetImage().getExtents()[2] : 1;
        float[] afResolutions = m_kVolumeImageA.GetImage().getResolutions(0);
		sphereScale = 12*afResolutions[0];
		System.err.println( sphereScale );
		Transformation xfrm = new Transformation();
		xfrm.SetScale( sphereScale/(2*afResolutions[0]), sphereScale/(2*afResolutions[1]), sphereScale/(2*afResolutions[2]) );

		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIText text = new VOIText( (VOIText)annotations.getCurves().elementAt(i) );
			text.setUseMarker(false);
			Color c = text.getColor();
			
			VOI annotationVOI = new VOI( (short)i, text.getName(), VOI.ANNOTATION, 0 );
			annotationVOI.setColor(c);
			annotationVOI.getCurves().add(text);
			text.createVolumeVOI( m_kVolumeImageA, m_kTranslate );
			m_kVolumeImageA.GetImage().registerVOI( annotationVOI );
			
			ColorRGBA colorRGBA = new ColorRGBA(c.getRed()/255f,c.getGreen()/255f,c.getBlue()/255f,1);
						
			float radius = text.elementAt(0).distance(text.elementAt(1));
			System.err.println(i + " " + radius);
			xfrm.SetScale( radius*sphereScale/(2*afResolutions[0]), radius*sphereScale/(2*afResolutions[1]), radius*sphereScale/(2*afResolutions[2]) );
			std.SetTransformation( xfrm );
			TriMesh sphere = std.Sphere(2);
			updateSphere( sphere, 0, 0, 0, colorRGBA );
			SurfaceState kSurface = new SurfaceState( sphere, text.getText() );	
			VolumeSurface surface = new VolumeSurface(m_kVolumeImageA,
					m_kVolumeImageB, m_kTranslate, m_fX, m_fY, m_fZ, kSurface, false, true);
			surface.SetTranslateVolumeCoords( text.elementAt(0), true );	
			surface.SetDisplay(true);
			m_kDisplayList.add(surface);	
		}
		m_bSurfaceUpdate = true;
	}  
	
	
	
	
	private boolean displayedNeurite( String neuriteName )
	{
		if ( neuriteVOIs == null )
		{
			return false;
		}
		return neuriteVOIs.containsKey(neuriteName);
	}	
	
	private void updateNeurite( String neuriteName, String[] names, ColorRGB color )
	{
		if ( neuriteVOIs == null )
		{
			return;
		}
		if ( !neuriteVOIs.containsKey(neuriteName) )
		{
			return;
		}
		VOI[] neurites = neuriteVOIs.get(neuriteName);
		if ( neurites == null )
		{
			return;
		}

		VolumeSurface[] surfaces = neuriteSurfaces.get(neuriteName);
		for ( int i = 0; i < neurites.length; i++ )
		{
			VOIContour contour = (VOIContour) neurites[i].getCurves().elementAt(0);
			updateContour( contour, annotationPositions.elementAt(i), names );
			if ( surfaces != null )
			{
				m_kDisplayList.remove(surfaces[i]);	
				if ( contour.size() > 1 )
				{
					SurfaceState kSurface = new SurfaceState( createNeuriteSurface(contour, color, sphereScale/3f), "AxL" );	
					surfaces[i] = new VolumeSurface(m_kVolumeImageA,
							m_kVolumeImageB, m_kTranslate, m_fX, m_fY, m_fZ, kSurface, false, true);
					surfaces[i].SetColor( color, false );
					surfaces[i].SetDisplay(false);
					m_kDisplayList.add(surfaces[i]);						
				}
			}
		}
		annotationVOIsUpdate(annotationSpheresIndex);
	}
	
	private void updateContour( VOIContour contour, VOI positions, String[] names )
	{
		contour.clear();
		for ( int i = 0; i < names.length; i++ )
		{
			for ( int j = 0; j < positions.getCurves().size(); j++ )
			{
				VOIText text = (VOIText) positions.getCurves().elementAt(j);
				if ( text.getText().equals( names[i] ) )
				{
					contour.add( text.elementAt(0) );
					break;
				}
			}
		}
		contour.getVolumeVOI().setVOI(contour);
	}

	public void displayNeurite( String neuriteName, boolean display )
	{
		if ( neuriteDisplay == null )
		{
			return;
		}
		neuriteDisplay.put(neuriteName, display);
		annotationVOIsUpdate(annotationSpheresIndex);
	}
	
	public void setNeuriteColor( String neuriteName, ColorRGB color )
	{
		if ( neuriteVOIs != null )
		{
			VOI[] neurites = neuriteVOIs.get( neuriteName );
			VolumeSurface[] surfaces = neuriteSurfaces.get(neuriteName);
			if ( (neurites != null) && (surfaces != null) )
			{
				for ( int j = 0; j < neurites.length; j++ )
				{
					VolumeVOI voi = neurites[j].getCurves().elementAt(0).getVolumeVOI();
					voi.update( new ColorRGBA(color.R, color.G, color.B, 1) );

					if ( surfaces[j] != null )
					{
						surfaces[j].SetColor(color, true );	
					}
					annotationVOIsUpdate(annotationSpheresIndex);			
				}
			}
		}
	}

	private Vector<String> neuriteNames;
	public void addNeurite( String neuriteName, String[] names, ColorRGB color )
	{
		if ( displayedNeurite(neuriteName) )
		{
			updateNeurite(neuriteName, names, color);
			return;
		}

		if ( neuriteVOIs == null )
		{
			neuriteVOIs = new HashMap<String, VOI[]>();
			neuriteDisplay = new HashMap<String, Boolean>();
			neuriteNames = new Vector<String>();
			neuriteSurfaces = new HashMap<String, VolumeSurface[]>();
		}
		VOI[] neurite = new VOI[annotationPositions.size()];
		VolumeSurface[] surfaces = new VolumeSurface[annotationPositions.size()];
		for ( int i = 0; i < annotationPositions.size(); i++ )
		{
			neurite[i] = new VOI( (short) 0, neuriteName, VOI.POLYLINE, 1.0f );
			VOIContour contour = new VOIContour(false);
			for ( int j = 0; j < names.length; j++ )
			{
				for ( int k = 0; k < annotationPositions.elementAt(i).getCurves().size(); k++ )
				{
					VOIText text = (VOIText) annotationPositions.elementAt(i).getCurves().elementAt(k);
					if ( text.getText().equals( names[j] ) )
					{
						contour.add( text.elementAt(0) );
						break;
					}
				}
			}
			neurite[i].getCurves().add(contour);
			if ( contour.size() > 1 )
			{
				SurfaceState kSurface = new SurfaceState( createNeuriteSurface(contour, color, sphereScale/3f), neuriteName + i );	
				surfaces[i] = new VolumeSurface(m_kVolumeImageA,
						m_kVolumeImageB, m_kTranslate, m_fX, m_fY, m_fZ, kSurface, false, true);
				surfaces[i].SetColor( color, false );
				surfaces[i].SetDisplay(false);
				m_kDisplayList.add(surfaces[i]);	
				
			}
			contour.createVolumeVOI( m_kVolumeImageA, m_kTranslate );
			contour.getVolumeVOI().SetDisplay(false);
			m_kVolumeImageA.GetImage().registerVOI( neurite[i] );
		}
		neuriteVOIs.put(neuriteName, neurite);
		neuriteNames.add(neuriteName);
		neuriteDisplay.put(neuriteName, false);
		neuriteSurfaces.put( neuriteName, surfaces );
	}

	public void setAnnotationVOIColor( String name, ColorRGB color )
	{
		VolumeSurface surface = annotationSpheres.get( name );
		if ( surface != null )
		{
			surface.SetColor(color, true );
			annotationSpheresColors.put(name, color);
			annotationVOIsUpdate(annotationSpheresIndex);			
		}
	}
	
	public void setDisplayAnnotation( String name, boolean display )
	{
		Boolean isDisplayed = annotationSpheresDisplay.get( name );
		if ( isDisplayed != display )
		{
			annotationSpheresDisplay.put( name, display );
			annotationVOIsUpdate(annotationSpheresIndex);
		}
	}
	
	public void getAnnotationInfo( String name, boolean[] display, Color[] color, float[] diameter, boolean[] displayLabel, Color[] labelColor )
	{
		display[0] = annotationSpheresDisplay.get( name );
		VolumeSurface surface = annotationSpheres.get( name );
		if ( surface != null )
		{
			ColorRGB colorRGB = surface.GetColor();
			int red = (int) Math.max( 0, Math.min( 255, colorRGB.R * 255) );
			int green = (int) Math.max( 0, Math.min( 255, colorRGB.G * 255) );
			int blue = (int) Math.max( 0, Math.min( 255, colorRGB.B * 255) );
			color[0] = new Color( red, green, blue );
		}
		displayLabel[0] = annotationLabelsDisplay.get(name);
		VOI voi = annotationVOIs.get(name);
		VOIText text = (VOIText) voi.getCurves().elementAt(0);
		labelColor[0] = text.getColor();
		diameter[0] = annotationDiameters.get(name);
	}
	
	public void setAnnotationDiameter( String name, float diameter )
	{
		VolumeSurface surface = annotationSpheres.get( name );
		if ( surface != null )
		{
			m_kDisplayList.remove(surface);

			Attributes attributes = new Attributes();
			attributes.SetPChannels(3);
			attributes.SetNChannels(3);
			attributes.SetCChannels(0,4);
			StandardMesh std = new StandardMesh(attributes);
			
			Transformation xfrm = new Transformation();
			xfrm.SetUniformScale( diameter * sphereScale );
			std.SetTransformation( xfrm );
			TriMesh sphere = std.Sphere(2);
			
			ColorRGB colorRGB = annotationSpheresColors.get(name);
			ColorRGBA colorRGBA = new ColorRGBA(colorRGB.R,colorRGB.G,colorRGB.B,1);				
			updateSphere( sphere, 0, 0, 0, colorRGBA );
			
			SurfaceState kSurface = new SurfaceState( sphere, name );	
			surface = new VolumeSurface(m_kVolumeImageA,
					m_kVolumeImageB, m_kTranslate, m_fX, m_fY, m_fZ, kSurface, false, true);
			surface.SetDisplay(false);
			m_kDisplayList.add(surface);	
			

			annotationSpheres.put( name, surface );
			annotationDiameters.put( name, diameter );
			annotationVOIsUpdate(annotationSpheresIndex);
		
		}
	}
	
	public VOI getSelectedVOI( String name )
	{
		return annotationVOIs.get(name);
	}
	
	public void setDisplayAnnotationLabel( String name, boolean display )
	{
		Boolean isDisplayed = annotationLabelsDisplay.get( name );
		if ( isDisplayed != display )
		{
			annotationLabelsDisplay.put( name, display );
			annotationVOIsUpdate(annotationSpheresIndex);
		}
	}

	private NaturalSpline3 smoothCurve(final VOIContour curve, final float[] time) {
		float totalDistance = 0;
		for (int i = 0; i < curve.size() - 1; i++) {
			totalDistance += curve.elementAt(i).distance(curve.elementAt(i + 1));
		}

		final Vector3f[] akPoints = new Vector3f[curve.size()];
		float distance = 0;
		for (int i = 0; i < curve.size(); i++) {
			if (i > 0) {
				distance += curve.elementAt(i).distance(curve.elementAt(i - 1));
				time[i] = distance / totalDistance;
				akPoints[i] = new Vector3f(curve.elementAt(i));
			} else {
				time[i] = 0;
				akPoints[i] = new Vector3f(curve.elementAt(i));
			}
		}

		return new NaturalSpline3(NaturalSpline3.BoundaryType.BT_FREE, curve.size() - 1, time, akPoints);
	}

	private TubeSurface createTube(VOIContour neurite) {

		float[] time = new float[neurite.size()];
		NaturalSpline3 m_pkSpline = smoothCurve(neurite, time);

		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
//		kAttr.SetTChannels(0,3);
		kAttr.SetCChannels(0,4);

		Vector2f kUVMin = new Vector2f(0.0f,0.0f);
		Vector2f kUVMax = new Vector2f(1.0f,1.0f);
		TubeSurface kTube = new TubeSurface(m_pkSpline, 1f, false,Vector3f.UNIT_Z,
				neurite.size() - 1,8,kAttr,false,false,kUVMin,kUVMax);
//		kTube.Local.SetTranslate( m_kTranslate );

//		if ( kTube.VBuffer.GetAttributes().HasTCoord(0) )
		{
			for ( int i = 0; i < kTube.VBuffer.GetVertexQuantity(); i++ )
			{
				kTube.VBuffer.SetColor4( 0, i, ColorRGBA.WHITE );
//				kTube.VBuffer.SetTCoord3( 0, i, 
//						(kTube.VBuffer.GetPosition3fX(i)) * 1.0f/m_fX,
//						(kTube.VBuffer.GetPosition3fY(i)) * 1.0f/m_fY,
//						(kTube.VBuffer.GetPosition3fZ(i)) * 1.0f/m_fZ);

			}
		}


		return kTube;
	}
	
	private TriMesh createNeuriteSurface( VOIContour neurite, ColorRGB color, float radius )
	{
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		kAttr.SetCChannels(0,4);
		StandardMesh stdMesh = new StandardMesh(kAttr);

		TriMesh[] cylinders = new TriMesh[neurite.size()-1];
//		TriMesh[] cylinders = new TriMesh[1];
		int vCount = 0;
		int iCount = 0;
		for ( int i = 0; i < cylinders.length; i++ )
		{
			Vector3f midPt = Vector3f.add(neurite.elementAt(i), neurite.elementAt(i+1));
			midPt.scale(0.5f);
			
			Vector3f direction = Vector3f.sub( neurite.elementAt(i+1), neurite.elementAt(i) );
			direction.normalize();
			
			Transformation xform = new Transformation();
			Matrix3f rotation = new Matrix3f(false);
			Vector3f axis = Vector3f.cross( Vector3f.UNIT_Z, direction );
			float angle = Vector3f.angle( Vector3f.UNIT_Z, direction );
			axis.normalize();
			rotation.fromAxisAngle( axis, angle );
			xform.SetRotate( rotation );
			xform.SetTranslate(midPt);
			
			stdMesh.SetTransformation( xform );
			
			float dist = neurite.elementAt(i+1).distance( neurite.elementAt(i) );
			cylinders[i] = stdMesh.Cylinder( 8, 8, radius, dist, true );			
			vCount += cylinders[i].VBuffer.GetVertexQuantity();
			iCount += cylinders[i].IBuffer.GetIndexQuantity();
		}
		
		VertexBuffer vBuffer = new VertexBuffer( kAttr, vCount );
		IndexBuffer iBuffer = new IndexBuffer(iCount);
		
		vCount = 0;
		iCount = 0;
		int[] offset = new int[cylinders.length];
		for ( int i = 0; i < cylinders.length; i++ )
		{
			offset[i] = 0;
			for ( int j = i; j > 0; j-- )
			{
				offset[i] += cylinders[j].VBuffer.GetVertexQuantity();
			}
		}
		for ( int i = 0; i < cylinders.length; i++ )
		{
			for ( int j = 0; j < cylinders[i].VBuffer.GetVertexQuantity(); j++ )
			{
				vBuffer.SetVertex( vCount,  cylinders[i].VBuffer.GetVertex(j) );
				vBuffer.SetNormal3( vCount,  cylinders[i].VBuffer.GetNormal3(j) );
				vBuffer.SetColor4(0, vCount, color.R, color.G, color.B, 1);
				vCount++;
			}
			for ( int j = 0; j < cylinders[i].IBuffer.GetIndexQuantity(); j++ )
			{
				iBuffer.GetData()[iCount++] = cylinders[i].IBuffer.GetData()[j] + offset[i];
			}
		}
		
		return new TriMesh( vBuffer, iBuffer );
	}


	public void play4DVOIs(boolean bOn) {
		m_iScreenCaptureCounter = 0;
		annotationVOIsUpdate(0);
		m_bPlay4DVOIs = bOn;
	}
	
	public boolean writeImage()
    {
        super.writeImage();
        if ( annotationPositions != null )
        {
        	if ( m_iScreenCaptureCounter >= annotationPositions.size() )
        	{
        		m_bSnapshot = false;
        		m_bPlay4DVOIs = false;
        	}
        }
        return true;
    }
    
	
	public void annotationVOIsUpdate( int value )
	{
//		for ( int i = 0; i < neuriteSurfaces.length; i++ )
//		{
//			if ( neuriteSurfaces[i][0] != null )
//			{
//				neuriteSurfaces[i][0].SetDisplay(false);
//			}
//			if ( neuriteSurfaces[i][1] != null )
//			{
//				neuriteSurfaces[i][1].SetDisplay(false);
//			}
//		}
		
		for ( int i = 0; i < annotationNames.length; i++ )
		{
			VolumeSurface surface = annotationSpheres.get( annotationNames[i] );
			if ( surface != null )
			{
				surface.SetDisplay(false);
			}
			VOI annotationVOI = annotationVOIs.get( annotationNames[i] );
			if ( annotationVOI != null )
			{
				annotationVOI.getCurves().elementAt(0).getVolumeVOI().SetDisplay(false);
			}
		}
		if ( neuriteVOIs != null )
		{
			for ( int i = 0; i < neuriteNames.size(); i++ )
			{
				VOI[] neurites = neuriteVOIs.get( neuriteNames.elementAt(i) );
				VolumeSurface[] surfaces = neuriteSurfaces.get( neuriteNames.elementAt(i) );
				for ( int j = 0; j < neurites.length; j++ )
				{
					neurites[j].getCurves().elementAt(0).getVolumeVOI().SetDisplay(false);
					if ( surfaces[j] != null )
					{
						surfaces[j].SetDisplay(false);
					}
				}
			}
		}
		//		m_kVolumeImageA.GetImage().unregisterVOI( neuriteVOIs[0] );
//		m_kVolumeImageA.GetImage().unregisterVOI( neuriteVOIs[1] );


		annotationSpheresIndex = value;
    	if ( annotationSpheresIndex >= annotationPositions.size() )
    	{
    		annotationSpheresIndex = 0;
    	}
    	if ( annotationSpheresIndex < 0)
    	{
    		annotationSpheresIndex = 0;
    	}
		if ( annotationSpheresIndex != -1 )
		{
//			neurites[annotationSpheresIndex][0].update( ColorRGBA.WHITE );
//			neurites[annotationSpheresIndex][1].update( ColorRGBA.WHITE );
//			
//			neuriteVOIs[0].getCurves().clear();
//			neuriteVOIs[0].getCurves().add( neurites[annotationSpheresIndex][0] );
//			neuriteVOIs[1].getCurves().clear();
//			neuriteVOIs[1].getCurves().add( neurites[annotationSpheresIndex][1] );
			
			
			int dimX = m_kVolumeImageA.GetImage().getExtents().length > 0 ? m_kVolumeImageA.GetImage().getExtents()[0] : 1;
			int dimY = m_kVolumeImageA.GetImage().getExtents().length > 1 ? m_kVolumeImageA.GetImage().getExtents()[1] : 1;
			int dimZ = m_kVolumeImageA.GetImage().getExtents().length > 2 ? m_kVolumeImageA.GetImage().getExtents()[2] : 1;
			VOI currentTime = annotationPositions.elementAt( annotationSpheresIndex );
//			m_kVolumeImageA.GetImage().registerVOI(currentTime);
//			System.err.println( currentTime.getCurves().size() );
			

			if ( neuriteVOIs != null )
			{
				for ( int i = 0; i < neuriteNames.size(); i++ )
				{
					VOI[] neurites = neuriteVOIs.get( neuriteNames.elementAt(i) );
					if ( neurites[annotationSpheresIndex] != null )
					{
						neurites[annotationSpheresIndex].getCurves().elementAt(0).getVolumeVOI().SetDisplay(neuriteDisplay.get(neuriteNames.elementAt(i)));
					}
					VolumeSurface[] surfaces = neuriteSurfaces.get( neuriteNames.elementAt(i) );
					if ( surfaces[annotationSpheresIndex] != null )
					{
						surfaces[annotationSpheresIndex].SetDisplay(neuriteDisplay.get(neuriteNames.elementAt(i)));
					}
				}
			}

			for ( int i = 0; i < currentTime.getCurves().size(); i++ )
			{
				VOIText text = (VOIText) currentTime.getCurves().elementAt(i);
				VolumeSurface surface = annotationSpheres.get(text.getText());
				if ( surface != null )
				{
					Boolean display = annotationSpheresDisplay.get( text.getText() );
					if ( display )
					{
						Vector3f position = new Vector3f(text.elementAt(0));
						if ( !position.equals( Vector3f.ZERO ) &&
							 (position.X >= 0) && (position.X < dimX) && (position.Y >= 0) && (position.Y < dimY) && 
							 (position.Z >= 0) && (position.Z < dimZ) )
						{
							surface.SetTranslateVolumeCoords( position );							
						}						
					}
					Color color = text.getColor();
					surface.SetColor( new ColorRGB( color.getRed()/255.0f, 
							color.getGreen()/255.0f,
							color.getBlue()/255.0f ), true );
					surface.SetDisplay(display);					
				}
				VOI annotationVOI = annotationVOIs.get(text.getText());
				if ( annotationVOI != null )
				{
					Boolean display = annotationLabelsDisplay.get( text.getText() );
					display &= text.getDisplay();
					VOIText displayedText = (VOIText) annotationVOI.getCurves().elementAt(0);
					if ( displayedText.getVolumeVOI() != null )
					{
						if ( display )
						{
							displayedText.getVolumeVOI().SetBillboardPosition( text.elementAt(1) );				
						}
						displayedText.getVolumeVOI().SetDisplay(display);
					}
				}
			}
//			m_kVolumeImageA.GetImage().registerVOI( neuriteVOIs[0] );
//			m_kVolumeImageA.GetImage().registerVOI( neuriteVOIs[1] );
//
//			if ( neuriteSurfaces[annotationSpheresIndex][0] != null )
//			{
//				neuriteSurfaces[annotationSpheresIndex][0].SetDisplay(true);
//			}
//			if ( neuriteSurfaces[annotationSpheresIndex][1] != null )
//			{
//				neuriteSurfaces[annotationSpheresIndex][1].SetDisplay(true);
//			}

			if (annotationAnimationPanel != null)
			{
				annotationAnimationPanel.setAnnimationSlider( annotationSpheresIndex );
			}
		}
		
		m_bSurfaceUpdate = true;
	}

	protected void UpdateSceneRotation()
	{
		super.UpdateSceneRotation();
		if ( annotationPositions == null )
		{
			return;
		}
		for ( int i = 0; i < annotationNames.length; i++ )
		{
			VOI annotationVOI = annotationVOIs.get( annotationNames[i] );
			if ( annotationVOI != null )
			{
				annotationVOI.getCurves().elementAt(0).getVolumeVOI().GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
				annotationVOI.getCurves().elementAt(0).getVolumeVOI().GetScene().UpdateGS();
			}
		}
		if ( neuriteVOIs != null )
		{
			for ( int i = 0; i < neuriteNames.size(); i++ )
			{
				VOI[] neurites = neuriteVOIs.get( neuriteNames.elementAt(i) );
				VolumeSurface[] surfaces = neuriteSurfaces.get( neuriteNames.elementAt(i) );
				for ( int j = 0; j < neurites.length; j++ )
				{
					neurites[j].getCurves().elementAt(0).getVolumeVOI().GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
					neurites[j].getCurves().elementAt(0).getVolumeVOI().GetScene().UpdateGS();
					if ( surfaces[j] != null )
					{
						surfaces[j].GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
						surfaces[j].GetScene().UpdateGS();
					}
				}
			}
		}
		
		for ( int i = 0; i < annotationPositions.size(); i++ )
		{
			VOI currentTime = annotationPositions.elementAt( i );
			for ( int j = 0; j < currentTime.getCurves().size(); j++ )
			{
				VOIText text = (VOIText) currentTime.getCurves().elementAt(j);
				VolumeVOI vText = text.getVolumeVOI();
				if ( vText != null )
				{
					vText.GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
					vText.GetScene().UpdateGS();
				}
			}
		}
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
