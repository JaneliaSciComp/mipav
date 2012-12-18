package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOIContour;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.ZBufferState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.Surfaces.TubeSurface;

/** Displays the Diffusion Tensor tracts in the VolumeViewer.
 * @see VolumeObject.java
 * @see GPUVolumeRender.java
 */
public class VolumeDTI extends VolumeObject
{

	/** Hashmap for multiple fiber bundles: */
	private HashMap<Integer,Node>  m_kTracts = null;

	/** Hashmap for multiple tube type fiber bundles: */
	private HashMap<Integer,Node>  m_kTubes = null;

	/** Hashmap for multiple fiber bundles: */
	private HashMap<Integer,VolumePreRenderEffect[]>  m_kShaders = null;

	/** Hashmap for multiple fiber bundles: */
	private HashMap<Integer,Vector<VOIContour>> m_kGlyphs = null;

	/** When true display the fiber tracts with ellipsoids instead of lines: */
	private boolean m_bDisplayEllipsoids = false;

	/** When true display the DTI volume with glyphs: */
	private boolean m_bDisplayAllGlyphs = false;

	/** In the display all ellipsoids mode the ellipsoids are displayed every
	 * m_iEllipsoidMod steps. */
	private int m_iEllipsoidMod = 10;


	/** When true display the fiber tracts with cylinders instead of lines: */
	private boolean m_bDisplayCylinders = false;

	/** When true display the fiber tracts with tubes instead of lines: */
	private boolean m_bDisplayTubes = false;

	/** When true display the fiber tracts with arrows instead of lines: */
	private boolean m_bDisplayArrows = false;

	private SurfaceLightingEffect m_kLightShader;


	/** Material properties of the ellipsoids. */
	private MaterialState m_kEllipseMaterial;

	/** Material properties of the Tubes. */
	private MaterialState m_kTubesMaterial;

	/** Material properties of the tract lines. */
	private MaterialState m_kLinesMaterial;

	/** EigenVector values for displaying ellipsoids. */
	ModelImage m_kEigenVector;
	ModelImage m_kEigenValue;
	//private HashMap<Integer,Transformation>  m_kEigenVectors = null;

	/** Ellipsoids is a sphere with a non-uniform scale based on the eigen
	 * vectors and values. */
	private TriMesh m_kSphere;


	/** Cylinders is a sphere with a non-uniform scale based on the eigen
	 * vectors and values. */
	private TriMesh m_kCylinder;   


	/** The DTI volume extents: */
	private int m_iDimX, m_iDimY, m_iDimZ;

	/** Ellipsoids scale factor, based on the DTI volume: */
	private float m_fScale;

	/** Volume-based color for the ellipsoids: */
	private ColorRGB m_kColorEllipse;

	/** maximum number of fiber tracts currently displayed. */
	private int m_iMaxGroups = 0;


	/** flag to indicate to use volume color or not */
	private boolean isUsingVolumeColor = true;

	/** arrow glyphs */
	private Node m_kArrow;

	/** cone glyphs */
	//private TriMesh m_kCone;

	/** Randomly add group color. */
	private HashMap<Integer, ColorRGB> constantColor;



	/** Creates a new VolumeDTI object.
	 * @param kImageA the VolumeImage containing shared data and textures for
	 * rendering.
	 * @param kTranslate translation in the scene-graph for this object.
	 * @param fX the size of the volume in the x-dimension (extent * resolutions)
	 * @param fY the size of the volume in the y-dimension (extent * resolutions)
	 * @param fZ the size of the volume in the z-dimension (extent * resolutions)
	 */
	public VolumeDTI( VolumeImage kVolumeImage, Vector3f kTranslate, float fX, float fY, float fZ )
	{
		super(kVolumeImage,kTranslate,fX,fY,fZ);

		m_iDimX = m_kVolumeImageA.GetImage().getExtents()[0];
		m_iDimY = m_kVolumeImageA.GetImage().getExtents()[1];
		m_iDimZ = m_kVolumeImageA.GetImage().getExtents()[2];
		m_fScale = 1.0f/(Math.max(m_iDimX,Math.max(m_iDimY,m_iDimZ)));

		m_kScene = new Node();
		m_kAlpha = new AlphaState();
		m_kAlpha.BlendEnabled = true;

		m_kZBuffer = new ZBufferState();
		m_kZBuffer.Enabled = true;

		createScene();
	}

	/** Add a polyline to the display. Used to display an approximation fiber tract bundles.
	 * @param kLine new polyline to display.
	 * @param iGroup the group the polyline belongs to.
	 */
	public void addPolyline( VOIContour kContour, Polyline kLine, int iGroup )
	{               
		if ( kLine == null )
		{
			return;
		}
		if ( m_kGlyphs == null )
		{
			m_kTracts = new HashMap<Integer,Node>();
			m_kGlyphs = new HashMap<Integer,Vector<VOIContour>>();
			m_kShaders = new HashMap<Integer,VolumePreRenderEffect[]>();
			m_kTubes = new HashMap<Integer,Node>();
			constantColor = new HashMap<Integer, ColorRGB>();
		}        
		if ( m_iMaxGroups < iGroup )
		{
			m_iMaxGroups = iGroup;
		}


		scale( kLine.VBuffer );
		kLine.Local.SetTranslate(m_kTranslate);

		Integer iIGroup = new Integer(iGroup);
		if ( m_kGlyphs.containsKey( iIGroup ) )
		{
			Vector<VOIContour> kGlyphVector = m_kGlyphs.get(iIGroup);
			kGlyphVector.add(kContour);


			if ( m_kTracts.containsKey( iIGroup ) )
			{
				Node kTractNode = m_kTracts.get(iIGroup);
				kTractNode.AttachChild(kLine);
			}


			if ( m_kTubes.containsKey( iIGroup ) )
			{   
				Node kTubeNode = m_kTubes.get(iIGroup);
				kTubeNode.AttachChild(createTube(kLine));
			}
		}
		else
		{
			Vector<VOIContour> kGlyphVector = new Vector<VOIContour>();
			kGlyphVector.add(kContour);
			m_kGlyphs.put( iIGroup, kGlyphVector );

			Node kTractNode = new Node();
			kTractNode.AttachChild(kLine);
			m_kTracts.put( iIGroup, kTractNode );

			
			VolumePreRenderEffect[] shaders = new VolumePreRenderEffect[4];
			shaders[0] = new VolumePreRenderEffect(true, true, false); // pre-render non transparent
			shaders[1] = new VolumePreRenderEffect(false, true, true); // pre-render transparent
			shaders[2] = new VolumePreRenderEffect();
			m_kShaders.put( iIGroup, shaders );


			Node kTubeNode = new Node();
			kTubeNode.AttachChild(createTube(kLine));
			m_kTubes.put( iIGroup, kTubeNode );

			constantColor.put( iIGroup, new ColorRGB((float)Math.random(), (float)Math.random(), (float)Math.random()));
		}		
	}

	/**
	 * Generate the tube streamline from the given polyline.
	 * @param kTract  polyline of the medial path.
	 * @return  kTube Tube surface generated. 
	 */
	public TubeSurface createTube(Polyline kTract) {
		TubeSurface kTube;
		int iNumCtrlPoints = kTract.VBuffer.GetVertexQuantity();
		Vector3f[] akCtrlPoint = new Vector3f[iNumCtrlPoints];
		for ( int idx =0; idx < iNumCtrlPoints; idx++ ) {
			akCtrlPoint[idx] = kTract.VBuffer.GetPosition3(idx);
		}

		int iDegree = 2;
		BSplineCurve3f m_pkSpline = new BSplineCurve3f(iNumCtrlPoints,akCtrlPoint,iDegree,
				false,true);

		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		kAttr.SetTChannels(0,3);
		kAttr.SetCChannels(0,4);

		Vector2f kUVMin = new Vector2f(0.0f,0.0f);
		Vector2f kUVMax = new Vector2f(1.0f,1.0f);
		kTube = new TubeSurface(m_pkSpline,0.025f, false,Vector3f.UNIT_Z,
				iNumCtrlPoints,8,kAttr,false,false,kUVMin,kUVMax);
		kTube.Local.SetTranslate( m_kTranslate );

		if ( kTube.VBuffer.GetAttributes().HasTCoord(0) )
		{
			for ( int i = 0; i < kTube.VBuffer.GetVertexQuantity(); i++ )
			{
				kTube.VBuffer.SetColor4( 0, i, ColorRGBA.WHITE );
				kTube.VBuffer.SetTCoord3( 0, i, 
						(kTube.VBuffer.GetPosition3fX(i)) * 1.0f/m_fX,
						(kTube.VBuffer.GetPosition3fY(i)) * 1.0f/m_fY,
						(kTube.VBuffer.GetPosition3fZ(i)) * 1.0f/m_fZ);

			}
		}


		return kTube;
	}

	/** Display the DTI volume with glyphs at each voxel. The m_iEllipsMod
	 * value is used to limit the number of ellipsoids displayed.
	public void DisplayAllGlyphs( ModelImage kImage, Renderer kRenderer)
	{
		if ( m_kEigenVectors == null )
		{
			return;
		}

		//kAlpha.BlendEnabled = true;
		//Node kScaleNode = new Node();
		//kScaleNode.Local.SetScale( m_fX, m_fY, m_fZ );
		int iCount = 0;
		int iIndex;
		Integer kKey;
		float fR, fG, fB;
		TriMesh kGlyph = null;
		int iDisplayed = 0;
		Iterator<Integer> kIterator = m_kEigenVectors.keySet().iterator();
		while ( kIterator.hasNext() )
		{
			kKey = (Integer)kIterator.next();
			if ( (iCount%m_iEllipsoidMod) == 0 )
			{                           
				iIndex = kKey.intValue();                          
				if ( kImage.isColorImage() )
				{
					fR = kImage.getFloat( iIndex*4 + 1 )/255.0f;
					fG = kImage.getFloat( iIndex*4 + 2 )/255.0f;
					fB = kImage.getFloat( iIndex*4 + 3 )/255.0f;
					m_kColorEllipse.R = fR;
					m_kColorEllipse.G = fG;
					m_kColorEllipse.B = fB;
				}
				else
				{
					fR = kImage.getFloat( iIndex );
					m_kColorEllipse.R = fR;
					m_kColorEllipse.G = fR;
					m_kColorEllipse.B = fR;
				}
				if ( m_bDisplayEllipsoids )
				{
					kGlyph = m_kSphere;
				}
				else if ( m_bDisplayCylinders )
				{
					kGlyph = m_kCylinder;
				}
				else if ( m_bDisplayArrows )
				{
					m_kArrow.Local.Copy(m_kEigenVectors.get(kKey));

					m_kEllipseMaterial.Ambient = m_kColorEllipse;
					m_kEllipseMaterial.Diffuse = m_kColorEllipse;
					m_kScene.SetChild(0,m_kArrow);
					m_kScene.UpdateGS();
					m_kScene.DetachChild(m_kArrow);
					kRenderer.Draw((TriMesh)m_kArrow.GetChild(0));
					kRenderer.Draw((TriMesh)m_kArrow.GetChild(1));
					iDisplayed++;
				}
				if ( kGlyph != null )
				{
					kGlyph.Local.Copy(m_kEigenVectors.get(kKey));

					m_kEllipseMaterial.Ambient = m_kColorEllipse;
					m_kEllipseMaterial.Diffuse = m_kColorEllipse;

					m_kScene.SetChild(0,kGlyph);
					m_kScene.UpdateGS();
					m_kScene.DetachChild(kGlyph);

					kRenderer.Draw(kGlyph);
					iDisplayed++;
				}
			}
			iCount++;
		}
	}
	 */    

	/**
	 * memory cleanup.
	 */
	public void dispose()
	{
		if ( m_kTracts != null )
		{
			for ( int i = 0; i < m_iMaxGroups; i++ )
			{
				removePolyline(i);
			}
		}

		m_kTracts = null;
		m_kShaders = null;
		m_kGlyphs = null;
		m_kEigenVector = null;
		m_kEigenValue = null;

		if ( m_kEllipseMaterial != null )
		{
			m_kEllipseMaterial.dispose();
			m_kEllipseMaterial = null;
		}
		if ( m_kSphere != null )
		{
			m_kSphere.dispose();
			m_kSphere = null;
		}
		m_kColorEllipse = null;
	}
	
	/** Returns if there are tracts to display.
	 * @return true if there are tracts currently loaded, false otherwise.
	 */
	public boolean GetDisplayTract()
	{
		if ( m_kTracts == null )
		{
			return false;
		}
		return true;
	}

	/** Get the group color with given group ID. 
	 * @param groupID  given group id
	 * @return  ColorRGB group color
	 */
	public ColorRGB getGroupColor(int iGroup) {
		return new ColorRGB(constantColor.get(iGroup));
	}


	public Vector<VOIContour> getPolylines( int iGroup )
	{
		return m_kGlyphs.get( new Integer(iGroup) );
	}

	/** 
	 * Removes the specified polyline tract group.
	 * @param iGroup the group of polylines to remove.
	 */
	public void removePolyline( int iGroup )
	{
		Integer iIGroup = new Integer(iGroup);
		if ( m_kGlyphs.containsKey(iGroup) )
		{
			Vector<VOIContour> kGlyphVector = m_kGlyphs.remove( iIGroup );
			for ( int i = kGlyphVector.size() -1; i >= 0; i-- )
			{
				VOIContour kContour = kGlyphVector.remove(0);
				kContour.dispose();
			}

			m_kDeleteList.add( m_kTracts.remove( iIGroup ) );
			m_kShaders.remove( iIGroup );

			m_kDeleteList.add( m_kTubes.remove( iIGroup ) );
			constantColor.remove( iIGroup);
		}
	}

	/**
	 * Render the object.
	 * @param kRenderer the OpenGLRenderer object.
	 * @param kCuller the Culler object.
	 */
	public void Render( Renderer kRenderer, Culler kCuller, boolean bPreRender, boolean bSolid )
	{
		if ( !m_bDisplay )
		{
			return;
		}
		if ( !bSolid )
		{
			return;
		}


		//int iPassQuantity = m_kLightShader.GetPassQuantity();
		//for (int iPass = 0; iPass < iPassQuantity; iPass++) {
		//	m_kLightShader.LoadPrograms(kRenderer, iPass, kRenderer.GetMaxColors(), kRenderer.GetMaxTCoords(),
		//			kRenderer.GetMaxVShaderImages(), kRenderer.GetMaxPShaderImages());
		//}
		m_kLightShader.SetPerPixelLighting( kRenderer, true );


		AlphaState aTemp = kRenderer.GetAlphaState();
		kRenderer.SetAlphaState(m_kAlpha);

		if ( m_bDisplayEllipsoids || m_bDisplayCylinders ||  m_bDisplayArrows )
		{
			DisplayGlyphs( m_kVolumeImageA.GetImage(), kRenderer, bPreRender );
		}
		else if ( m_bDisplayTubes ) {
			DisplayTubes(m_kVolumeImageA.GetImage(), kRenderer, bPreRender);
		}
		else 
		{
			DisplayTract( kRenderer, bPreRender, bSolid );
		}
		kRenderer.SetAlphaState(aTemp);
	}

	/** Turns on/off displaying all the glyphs.
	 * @param bDisplay when true display all the glyphs in the volume.
	 */
	public void setDisplayAllGlyphs( boolean bDisplay )
	{
		m_bDisplayAllGlyphs = bDisplay;
	}
	/** Turns on/off displaying the fiber bundle tracts with 3d arrows.
	 * @param bDisplay when true display the tracts with arrows.
	 */
	public void setDisplayArrows( boolean bDisplay )
	{
		m_bDisplayArrows = bDisplay;
	}



	/** Turns on/off displaying the fiber bundle tracts with cylinders.
	 * @param bDisplay when true display the tracts with cylinders.
	 */
	public void setDisplayCylinders( boolean bDisplay )
	{
		m_bDisplayCylinders = bDisplay;
	}   



	/** Turns on/off displaying the fiber bundle tracts with ellipsoids.
	 * @param bDisplay when true display the tracts with ellipsods.
	 */
	public void setDisplayEllipsoids( boolean bDisplay )
	{
		m_bDisplayEllipsoids = bDisplay;
	}
	/** Turns on/off displaying the fiber bundle tracts with tubes.
	 * @param bDisplay when true display the tracts with tubes.
	 */
	public void setDisplayTubes( boolean bDisplay )
	{
		m_bDisplayTubes = bDisplay;
	}


	/** Sets the DTI Image for displaying the tensors as ellipsoids.
	 * @param kDTIImage.
	 */
	public void setDTIImage( ModelImage kDTIImage, ModelImage kEigenVectorImage, ModelImage kEigenValueImage, Renderer kRenderer )
	{

		m_kEigenVector = kEigenVectorImage;
		m_kEigenValue = kEigenValueImage;
	}
	
	private void createScene()
	{
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		kAttr.SetTChannels(0,3);

		CullState kCull = new CullState();
		kCull.Enabled = true;
		StandardMesh kSM = new StandardMesh(kAttr);
		m_kSphere = kSM.Sphere(8,8,1f);
		m_kSphere.AttachGlobalState(kCull);

		m_kCylinder = kSM.Cylinder(8,8,1.0f,2f,false);
		m_kCylinder.AttachGlobalState(kCull);

		m_kLightShader = new SurfaceLightingEffect( m_kVolumeImageA, false ); 

		m_kEllipseMaterial = new MaterialState();
		m_kEllipseMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
		m_kEllipseMaterial.Ambient = new ColorRGB(0.24725f,0.2245f,0.0645f);
		m_kEllipseMaterial.Diffuse = new ColorRGB(0.34615f,0.3143f,0.0903f);
		m_kEllipseMaterial.Specular = new ColorRGB(1f,1f,1f);
		m_kEllipseMaterial.Shininess = 500f;
		m_kEllipseMaterial.Alpha = 1f;
		m_kColorEllipse = new ColorRGB(ColorRGB.BLACK);

		m_kTubesMaterial = new MaterialState();
		m_kTubesMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
		m_kTubesMaterial.Ambient = new ColorRGB(0.24725f,0.2245f,0.0645f);
		m_kTubesMaterial.Diffuse = new ColorRGB(0.34615f,0.3143f,0.0903f);
		m_kTubesMaterial.Specular = new ColorRGB(1f,1f,1f);
		m_kTubesMaterial.Alpha = 1f;
		m_kTubesMaterial.Shininess = 500f;

		m_kLinesMaterial = new MaterialState();
		m_kLinesMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
		m_kLinesMaterial.Ambient = new ColorRGB(0.24725f,0.2245f,0.0645f);
		m_kLinesMaterial.Diffuse = new ColorRGB(0.34615f,0.3143f,0.0903f);
		m_kLinesMaterial.Specular = new ColorRGB(1f,1f,1f);
		m_kLinesMaterial.Alpha = 1f;
		m_kLinesMaterial.Shininess = 500f;


		m_kSphere.AttachGlobalState(m_kEllipseMaterial);
		m_kSphere.AttachEffect(m_kLightShader);
		m_kSphere.UpdateRS();

		m_kCylinder.AttachGlobalState(m_kEllipseMaterial);
		m_kCylinder.AttachEffect(m_kLightShader);
		m_kCylinder.UpdateRS();

		MakeArrow();

	}


	/** Set the m_iEllipsoidMod value. 
	 * @param iMod new m_iEllipsoidMod value.
	 */
	public void setEllipseMod( int iMod )
	{
		m_iEllipsoidMod = iMod;
	}


	/**
	 * Sets the light for the EllipsoidsShader.
	 * @param kLightType the name of the light to set (Light0, Light1, etc.)
	 * @param afType the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
	 */
	public void SetLight( String kLightType, float[] afType )
	{
		if ( m_kLightShader != null )
		{
			m_kLightShader.SetLight(kLightType, afType);
		}
	}

	/** Sets the polyline color for the specified fiber bundle tract group. 
	 * @param iGroup the fiber bundle group to set.
	 * @param kColor the new polyline color for the specified fiber bundle tract group. 
	 */
	public void setPolylineColor( int iGroup, ColorRGB kColor )
	{
		Integer kKey = new Integer(iGroup);
		constantColor.remove( kKey );
		constantColor.put( kKey, kColor );
	}

	/**
	 * Set the flag to using volume color or not
	 * @param flag  true or false
	 */
	public void setVolumeColor(boolean flag) {
		isUsingVolumeColor = flag;
	}


	/**
	 * Display a fiber bundle tract with a Glyph.
	 */    
	private void DisplayGlyphs( ModelImage kImage, Renderer kRenderer, boolean bPreRender )
	{
		if ( m_kGlyphs == null )
		{
			return;
		}
		Integer kKey;
		Vector<VOIContour> kGlyphVector;
		VOIContour kContour;
		Vector3f kPos;
		float fR,fG,fB;
		TriMesh kGlyph = null;        
		Iterator<Integer> kIterator = m_kGlyphs.keySet().iterator();
		int iCount = 0;
		m_kLightShader.SetReverseFace(0);
		m_kLightShader.SetSurfaceTexture(false, false, false);
		float[] afVectorData = new float[3];
		Vector3f kAxis = new Vector3f();
		Vector3f kV1 = new Vector3f();
		float fAngle, fLambda1, fLambda2, fLambda3;
		Transformation kTransform;

		float fXDelta = m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[0];
		float fYDelta = m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[1];
		float fZDelta = m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[2];

		float xBox = (m_iDimX - 1) * fXDelta;
		float yBox = (m_iDimY - 1) * fYDelta;
		float zBox = (m_iDimZ - 1) * fZDelta;
		float maxBox = Math.max(xBox, Math.max(yBox, zBox));    
		float fX, fY, fZ;

        Vector3f kExtentsScale = new Vector3f(1f/(float)(m_iDimX - 1), 1f/(float)(m_iDimY - 1), 1f/(float)(m_iDimZ - 1) );
		
		while ( kIterator.hasNext() )
		{
			kKey = (Integer)kIterator.next();
			kGlyphVector = m_kGlyphs.get(kKey);     
			VolumePreRenderEffect[] kShader = m_kShaders.get(kKey);
			for ( int i = 0; i < kGlyphVector.size(); i++ )
			{
				kContour = kGlyphVector.get(i);
				for ( int j = 0; j < kContour.size(); j++ )
				{
					if ( (iCount%m_iEllipsoidMod) == 0 )
					{                           
						kPos = kContour.elementAt(j);
						if ( kImage.isColorImage() )
						{
							if ( !isUsingVolumeColor ) {
								m_kColorEllipse = constantColor.get(kKey);
							} else {
								fR = kImage.getFloatTriLinearBounds( kPos.X, kPos.Y, kPos.Z, 1 )/255.0f;
								fG = kImage.getFloatTriLinearBounds( kPos.X, kPos.Y, kPos.Z, 2 )/255.0f;
								fB = kImage.getFloatTriLinearBounds( kPos.X, kPos.Y, kPos.Z, 3 )/255.0f;
								m_kColorEllipse = new ColorRGB(fR, fG, fB);
							}
						}
						else
						{
							fR = kImage.getFloatTriLinearBounds( kPos.X, kPos.Y, kPos.Z );
							m_kColorEllipse = new ColorRGB(fR, fR, fR);
						}

						// Get the transform:
						for ( int k = 0; k < 3; k++ )
						{
							afVectorData[k] = m_kEigenVector.getFloatTriLinearBoundsTime(kPos.X, kPos.Y, kPos.Z, k);
						}
						kV1.set( afVectorData[0], afVectorData[1], afVectorData[2] );
						kAxis = Vector3f.cross( Vector3f.UNIT_Z, kV1 );
						kAxis.normalize();

						fAngle = kV1.angle(Vector3f.UNIT_Z);
						Matrix3f kRotate = new Matrix3f();
						kRotate.fromAxisAngle( kAxis, fAngle);


						fLambda1 = m_kEigenValue.getFloatTriLinearBounds(kPos.X, kPos.Y, kPos.Z, 1);
						fLambda2 = m_kEigenValue.getFloatTriLinearBounds(kPos.X, kPos.Y, kPos.Z, 2);
						fLambda3 = m_kEigenValue.getFloatTriLinearBounds(kPos.X, kPos.Y, kPos.Z, 3);

						kTransform = new Transformation();
						kTransform.SetRotate( kRotate );

						Vector3f kScale = new Vector3f( fLambda3, fLambda2, fLambda1 );
						kScale.normalize();
						kScale.set( (float)Math.max(.1, kScale.X), (float)Math.max(.1, kScale.Y), (float)Math.max(.1, kScale.Z) );
						kScale.scale(m_fScale);
						kTransform.SetScale( kScale );



						fX = ((2.0f * (kPos.X * fXDelta)) - xBox)/(2.0f*maxBox);
						fY = ((2.0f * (kPos.Y * fYDelta)) - yBox)/(2.0f*maxBox);
						fZ = ((2.0f * (kPos.Z * fZDelta)) - zBox)/(2.0f*maxBox);             

						kTransform.SetTranslate( fX, fY, fZ );




						if ( m_bDisplayEllipsoids )
						{
							kGlyph = m_kSphere;

						}
						else if ( m_bDisplayCylinders )
						{
							kGlyph = m_kCylinder;
						}
						else if ( m_bDisplayArrows )
						{
							kGlyph = (TriMesh)m_kArrow.GetChild(0);
						}
						if ( kGlyph != null )
						{
							kGlyph.Local.Copy(kTransform);
							kGlyph.AttachGlobalState(m_kZBuffer);
							kGlyph.UpdateGS();
							
							if ( bPreRender )
							{
								// use a constant color shader w/above position...
								kGlyph.AttachEffect( kShader[2] );
								kGlyph.UpdateRS();
								Vector3f kPosColor = new Vector3f(kPos);
								kPosColor.mult(kExtentsScale);
								kShader[2].SetColor(kPosColor);
								/*
								if ( !kShader[2].SetColor(kPosColor) )
								{
									kShader[2].LoadPrograms(kRenderer, 0, kRenderer.GetMaxColors(), kRenderer.GetMaxTCoords(),
											kRenderer.GetMaxVShaderImages(), kRenderer.GetMaxPShaderImages());
									kShader[2].SetColor(kPosColor);
								}*/
							}
							else
							{
								// user regular light shader
								kGlyph.AttachEffect( m_kLightShader );
							}
							
							m_kEllipseMaterial.Ambient = m_kColorEllipse;
							m_kEllipseMaterial.Diffuse = m_kColorEllipse;
							m_kScene.SetChild(0,kGlyph);
							m_kScene.UpdateGS();
							m_kScene.UpdateRS();
							m_kScene.DetachChild(kGlyph);
							kRenderer.Draw(kGlyph);							
							
							kGlyph.DetachAllEffects();
						}
					}
					iCount++;
				}
			}
		}
		m_kLightShader.SetReverseFace(0);
	}

	/** Displays a polyline fiber bundle tract with the given shader attached.
	 * @param kInputStader shader to apply to the polyline.
	 */    
	private void DisplayTract( Renderer kRenderer, boolean bPreRender, boolean bSolid  )
	{
		Iterator<Integer> kIterator = m_kTracts.keySet().iterator();
		Integer iKey;
		Node kTractNode;
		Polyline kTract;

		//kAlpha.BlendEnabled = true;
		while ( kIterator.hasNext() )
		{
			iKey = (Integer)kIterator.next();

			kTractNode = m_kTracts.get(iKey);

			ColorRGB kColor1 = null;
			for ( int i = 0; i < kTractNode.GetQuantity(); i++ )
			{
				VolumePreRenderEffect[] kShader = m_kShaders.get(iKey);
				if ( bPreRender )
				{						
					kTract = (Polyline)kTractNode.GetChild(i);
					kTract.DetachAllEffects();
					kTract.AttachEffect( kShader[0] );

					m_kScene.SetChild(0,kTract);
					m_kScene.UpdateGS();
					m_kScene.DetachChild(kTract);
					kRenderer.Draw(kTract);
					kTract.DetachEffect( kShader[0] );						
				}
				else
				{
					if (!isUsingVolumeColor) {
						kColor1 = new ColorRGB(constantColor.get(iKey));

						kTract = (Polyline) kTractNode.GetChild(i);
						kTract.AttachGlobalState(m_kLinesMaterial);
						kTract.DetachAllEffects();
						kTract.AttachEffect(m_kLightShader);

						kTract.UpdateRS();

						m_kLinesMaterial.Ambient = new ColorRGB(kColor1.R, kColor1.G, kColor1.B); 
						m_kLinesMaterial.Diffuse = new ColorRGB(kColor1.R, kColor1.G, kColor1.B);
						m_kLinesMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
						m_kLinesMaterial.Specular = new ColorRGB(ColorRGB.BLACK);
						m_kLinesMaterial.Alpha = 1.0f;
						m_kLinesMaterial.Shininess = 100f;

						m_kScene.SetChild(0, kTract);
						m_kScene.UpdateGS();
						m_kScene.DetachChild(kTract);
						kRenderer.Draw(kTract);
						kTract.DetachEffect(m_kLightShader);

					} 
					else
					{
						kTract = (Polyline)kTractNode.GetChild(i);
						kTract.DetachAllEffects();
						kTract.AttachEffect( kShader[1] );

						m_kScene.SetChild(0,kTract);
						m_kScene.UpdateGS();
						m_kScene.DetachChild(kTract);
						kRenderer.Draw(kTract);
						kTract.DetachEffect( kShader[1] );								
					}
				}
			}
		}
	}
	
	

	/** Displays a tube fiber bundle tract with the given shader attached.
	 * @param kInputStader shader to apply to the tube.
	 */    
	private void DisplayTubes( ModelImage kImage, Renderer kRenderer, boolean bPreRender )
	{
		Node kTubeNode;
		Integer iKey;

		Iterator<Integer> iIterator = m_kTubes.keySet().iterator();

		m_kTubes.keySet();

		TubeSurface kTube;

		while ( iIterator.hasNext() )
		{
			iKey = (Integer)iIterator.next();

			kTubeNode = m_kTubes.get(iKey);   
			for ( int i = 0; i < kTubeNode.GetQuantity(); i++ )
			{
				kTube = (TubeSurface)kTubeNode.GetChild(i);     
				
				VolumePreRenderEffect[] kShader = m_kShaders.get(iKey);
				if ( bPreRender )
				{						
					kTube.AttachEffect(kShader[0]);

					m_kScene.SetChild(0,kTube);
					m_kScene.UpdateGS();
					m_kScene.UpdateRS();
					m_kScene.DetachChild(kTube);
					kRenderer.Draw(kTube);

					kTube.DetachAllEffects();					
				}
				else
				{                      

					ColorRGB kColor1 = ColorRGB.WHITE;
					kColor1 = new ColorRGB(constantColor.get(iKey));

					m_kLightShader.SetSurfaceTexture(isUsingVolumeColor, false, false);

					kTube.AttachGlobalState(m_kTubesMaterial);

					kTube.AttachEffect(m_kLightShader);
					kTube.UpdateRS();

					m_kTubesMaterial.Ambient = new ColorRGB(kColor1);
					m_kTubesMaterial.Diffuse = new ColorRGB(kColor1);
					m_kTubesMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
					m_kTubesMaterial.Specular = new ColorRGB(ColorRGB.WHITE); 
					m_kTubesMaterial.Alpha = 1.0f;
					m_kTubesMaterial.Shininess = 100f;


					m_kScene.SetChild(0,kTube);
					m_kScene.UpdateGS();
					m_kScene.UpdateRS();
					m_kScene.DetachChild(kTube);
					kRenderer.Draw(kTube);

					kTube.DetachAllEffects();
				}
			}
		}
	}

	private void MakeArrow()
	{
		m_kArrow = new Node();

		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		kAttr.SetTChannels(0,3);

		CullState kCull = new CullState();
		kCull.Enabled = true;
		StandardMesh kSM = new StandardMesh(kAttr);

		TriMesh pkMesh = kSM.Cone(8,8,1.0f,1.0f, false);
		m_kArrow.AttachChild(pkMesh);

		TriMesh pkMesh2 = kSM.Cylinder(8,8,0.5f,1.0f,false);

		//m_kArrow.AttachChild(pkMesh2);

		pkMesh.AttachEffect(m_kLightShader);
		pkMesh2.AttachEffect(m_kLightShader);        
		pkMesh.AttachGlobalState(m_kEllipseMaterial);
		pkMesh2.AttachGlobalState(m_kEllipseMaterial);    
		pkMesh.AttachGlobalState(kCull);      
		pkMesh2.AttachGlobalState(kCull);    
		pkMesh.UpdateRS();
		pkMesh2.UpdateRS();
	}


}

