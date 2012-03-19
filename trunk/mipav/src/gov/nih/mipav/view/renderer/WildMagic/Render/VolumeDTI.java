package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.ViewJProgressBar;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
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
import WildMagic.LibGraphics.Shaders.Program;
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
	private HashMap<Integer,ShaderEffect>  m_kShaders = null;


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

	/** Keeps track of the color assigned the polylines. */
	private HashMap<Integer,ColorRGB> m_kEllipseConstantColor;


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

	/** DTI volume data size: */
	private int m_iLen;

	/** Volume-based color for the ellipsoids: */
	private ColorRGB m_kColorEllipse;

	/** maximum number of fiber tracts currently displayed. */
	private int m_iMaxGroups = 0;

	/** Color Shader for rendering the tracts. */
	//private ShaderEffect m_kVertexColor3Shader;

	/** Seeding point index */
	private int centerIndex;

	/** flag to indicate to use volume color or not */
	private boolean isUsingVolumeColor = true;

	/** Group constant color */
	private HashMap<Integer,Integer>  groupConstantColor = null;

	/** current group index */
	private int currentGroupIndex = 0;

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
		m_iLen = m_iDimX*m_iDimY*m_iDimZ;

		m_kScene = new Node();
		//m_kVertexColor3Shader = new VertexColor3Effect();
		m_kAlpha = new AlphaState();
		m_kAlpha.BlendEnabled = true;
		//m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
		//m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
		
        m_kZBuffer = new ZBufferState();
        m_kZBuffer.Enabled = true;

		constantColor = new HashMap<Integer, ColorRGB>();
		for ( int i = 0; i < 100; i++ ) {
			constantColor.put(new Integer(i), new ColorRGB((float)Math.random(), (float)Math.random(), (float)Math.random()));
		}

	}

	/**
	 * advance the group color index.
	 */
	public void addGroupColor() {
		currentGroupIndex++;
	}

	/** Add a polyline to the display. Used to display fiber tract bundles.
	 * @param kLine new polyline to display.
	 * @param iGroup the group the polyline belongs to.
	 */
	public void addPolyline( Polyline kLine, int iGroup )
	{               
		if ( kLine == null )
		{
			return;
		}
		if ( m_kTracts == null )
		{
			m_kTracts = new HashMap<Integer,Node>();
			m_kGlyphs = new HashMap<Integer,Vector<VOIContour>>();
			m_kShaders = new HashMap<Integer,ShaderEffect>();
			m_kEllipseConstantColor = new HashMap<Integer,ColorRGB>();
		}
		if ( m_kTubes == null ) {
			m_kTubes = new HashMap<Integer,Node>();
			groupConstantColor = new HashMap<Integer,Integer>();
		}        
		if ( m_iMaxGroups < iGroup )
		{
			m_iMaxGroups = iGroup;
		}

		VOIContour kContour = new VOIContour(false);
		for ( int i = 0; i < kLine.VBuffer.GetVertexQuantity(); i++ )
		{
			kContour.add( new Vector3f( kLine.VBuffer.GetPosition3(i)));
		}

		/*
		if ( m_kEigenVectors != null )
		{
			for ( int i = 0; i < kLine.VBuffer.GetVertexQuantity(); i++ )
			{
				int iX = (int)kLine.VBuffer.GetPosition3fX(i);
				int iY = (int)kLine.VBuffer.GetPosition3fY(i);
				int iZ = (int)kLine.VBuffer.GetPosition3fZ(i);

				int iIndex = iZ * m_iDimY * m_iDimX + iY * m_iDimX + iX;

				if (  m_kEigenVectors.get( new Integer(iIndex) ) != null )
				{
					aiEllipsoids[i] = iIndex;
				}
				else
				{
					aiEllipsoids[i] = -1;
				}
			}
		}
		*/

		scale( kLine.VBuffer );
		kLine.Local.SetTranslate(m_kTranslate);

		Node kTractNode = null;
		Node kTubeNode = null;

		Integer iIGroup = new Integer(iGroup);
		if ( m_kTracts.containsKey( iIGroup ) )
		{
			kTractNode = m_kTracts.get(iIGroup);
			kTractNode.AttachChild(kLine);
			Vector<VOIContour> kGlyphVector = m_kGlyphs.get(iIGroup);
			kGlyphVector.add(kContour);
			return;
		}

		if ( m_kTubes.containsKey( iIGroup ) )
		{   
			kTubeNode = m_kTubes.get(iIGroup);
			kTubeNode.AttachChild(createTube(kLine));
			kTubeNode.UpdateGS();
			kTubeNode.UpdateRS();
			Vector<VOIContour> kGlyphVector = m_kGlyphs.get(iIGroup);
			kGlyphVector.add(kContour);
			return;                       
		}

		if ( kTractNode == null )
		{
			//System.err.println( "kTractNode null " + iIGroup );
			kTractNode = new Node();
			kTractNode.AttachChild(kLine);
			kTractNode.UpdateGS();
			kTractNode.UpdateRS();
			m_kTracts.put( new Integer(iIGroup), kTractNode );

			Vector<VOIContour> kGlyphVector = new Vector<VOIContour>();
			kGlyphVector.add(kContour);
			m_kGlyphs.put( new Integer(iIGroup), kGlyphVector );
			String kShaderName = new String( "ConstantColor" );
			VertexColor3Effect kPolylineShader = new VertexColor3Effect( kShaderName, true );
			m_kShaders.put( new Integer(iIGroup), kPolylineShader );

		}

		if ( kTubeNode == null ) {
			kTubeNode = new Node();
			kTubeNode.AttachChild(createTube(kLine));
			kTubeNode.UpdateGS();
			kTubeNode.UpdateRS();
			m_kTubes.put( new Integer(iIGroup), kTubeNode );
			groupConstantColor.put(new Integer(iIGroup), new Integer(currentGroupIndex));
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
		m_kEllipseConstantColor = null;

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
		if ( groupConstantColor.get(iGroup-1) != null )
		{
			return new ColorRGB(constantColor.get(groupConstantColor.get(iGroup-1).intValue()));
		}
		return null;
	}

	/** Returns the polyline color for the specified fiber bundle tract group. 
	 * @param iGroup the fiber bundle group to query.
	 * @return the polyline color for the specified fiber bundle tract group. 
	 */
	public ColorRGB getPolylineColor( int iGroup )
	{
		if ( m_kEllipseConstantColor != null )
		{
			return m_kEllipseConstantColor.get( new Integer(iGroup) );
		}
		return null;
	}

	/**
	 * Find the min available group index;
	 */
	public void removeGroupColor() {
		Iterator<Integer> cIterator = groupConstantColor.keySet().iterator();
		Integer cKey;

		currentGroupIndex--;
		while ( cIterator.hasNext() )
		{
			cKey = (Integer)cIterator.next();
			if ( groupConstantColor.get(cKey).intValue() ==  currentGroupIndex ) {
				currentGroupIndex++;
			}
		}

	}

	/** 
	 * Removes the specified polyline tract group.
	 * @param iGroup the group of polylines to remove.
	 */
	public void removePolyline( int iGroup )
	{
		Integer kGroup = new Integer(iGroup);
		if ( m_kTracts == null || !m_kTracts.containsKey(iGroup) )
		{
			return;
		}
		Node kTractNode = m_kTracts.remove(kGroup);
		if ( kTractNode == null )
		{
			return;
		}
		/*
        kTractNode.DetachAllChildren();
        for ( int i = 0; i < kTractNode.GetQuantity(); i++ )
        {
            Polyline kTract = (Polyline)kTractNode.DetachChildAt(i);
            if ( kTract != null )
            { 
                kTract.DetachAllEffects();
                kTract.dispose();
            }
        }
        kTractNode.UpdateGS();
        kTractNode.UpdateRS();
		 */
		kTractNode.dispose();
		kTractNode = null;
		if ( m_kTracts.size() == 0 )
		{
			m_kTracts = null;
		}

		if ( m_kTubes == null ||  !m_kTubes.containsKey(iGroup) )
		{
			return;
		}
		Node kTubeNode = m_kTubes.remove(kGroup);
		groupConstantColor.remove(kGroup);

		if ( kTubeNode == null )
		{
			return;
		}
		/*
        for ( int i = 0; i < kTubeNode.GetQuantity(); i++ )
        {
        	TubeSurface kTube = (TubeSurface)kTubeNode.DetachChildAt(i);
            if ( kTube != null )
            { 
            	kTube.DetachAllEffects();
            	kTube.dispose();
            }
        }
        kTubeNode.UpdateGS();
        kTubeNode.UpdateRS();
		 */
		kTubeNode.dispose();
		kTubeNode = null;
		if ( m_kTubes.size() == 0 )
		{
			m_kTubes = null;
		}

		Vector<VOIContour> kGlyphVector = m_kGlyphs.remove(kGroup);
		if ( kGlyphVector != null )
		{
			kGlyphVector.clear();
		}

		ShaderEffect kShader = m_kShaders.remove(kGroup);
		if ( kShader != null )
		{
			kShader.dispose();
		}
	}

	/**
	 * remove the group color index.
	 */
	public void removeTractColor(Integer iGroup) {

		groupConstantColor.remove(iGroup);

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
		   
		
		int iPassQuantity = m_kLightShader.GetPassQuantity();
		for (int iPass = 0; iPass < iPassQuantity; iPass++) {
			m_kLightShader.LoadPrograms(kRenderer, iPass, kRenderer.GetMaxColors(), kRenderer.GetMaxTCoords(),
					kRenderer.GetMaxVShaderImages(), kRenderer.GetMaxPShaderImages());
		}
		m_kLightShader.SetPerPixelLighting( kRenderer, true );
		
		
		AlphaState aTemp = kRenderer.GetAlphaState();
		kRenderer.SetAlphaState(m_kAlpha);

		if ( m_bDisplayEllipsoids || m_bDisplayCylinders ||  m_bDisplayArrows )
		{
			DisplayGlyphs( m_kVolumeImageA.GetImage(), kRenderer );
		}
		else if ( m_bDisplayTubes ) {
			DisplayTubes(m_kVolumeImageA.GetImage(), kRenderer);
		}
		else 
		{
			DisplayTract(null, kRenderer );
		}
		kRenderer.SetAlphaState(aTemp);
	}
	public void setCenterIndex(int index) {
		centerIndex = index;
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
		
		/*
		ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating ellipse transforms", "", 0, 100, true);

		m_kEigenVectors =
			new HashMap<Integer,Transformation>();
		Matrix3f kMatrix = new Matrix3f();;
		float[] afTensorData = new float[6];
		Matrix3f kEigenValues = new Matrix3f();
		float fLambda1;
		float fLambda2;
		float fLambda3;
		Vector3f kV1 = new Vector3f();
		Vector3f kV2 = new Vector3f();
		Vector3f kV3 = new Vector3f();


		float fXDelta = m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[0];
		float fYDelta = m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[1];
		float fZDelta = m_kVolumeImageA.GetImage().getFileInfo(0).getResolutions()[2];
		
		kEigenValueImage.calcMinMax();
		float fEigenValMax = (float) kEigenValueImage.getMax();
		float fEigenValMin = (float) kEigenValueImage.getMin();
		float fScale = 1 / (fEigenValMax - fEigenValMin);
		System.err.println( fScale + " Max = " + fEigenValMax + " Min = " + fEigenValMin );
		for ( int i = 0; i < m_iLen; i++ )
		{
			kV1.X = kEigenVectorImage.getFloat( i +   m_iLen);
			kV1.Y = kEigenVectorImage.getFloat( i + 1*m_iLen);
			kV1.Z = kEigenVectorImage.getFloat( i + 2*m_iLen);
			
			kV2.X = kEigenVectorImage.getFloat( i + 3*m_iLen);
			kV2.Y = kEigenVectorImage.getFloat( i + 4*m_iLen);
			kV2.Z = kEigenVectorImage.getFloat( i + 5*m_iLen);
			
			kV3.X = kEigenVectorImage.getFloat( i + 6*m_iLen);
			kV3.Y = kEigenVectorImage.getFloat( i + 7*m_iLen);
			kV3.Z = kEigenVectorImage.getFloat( i + 8*m_iLen);
			

			fLambda1 = kEigenValueImage.getFloat( i * 4 + 1 );
			fLambda2 = kEigenValueImage.getFloat( i * 4 + 2 );
			fLambda3 = kEigenValueImage.getFloat( i * 4 + 3 );
			
			
			boolean bAllZero = true;
            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = kDTIImage.getFloat(i + j*m_iLen);
                if ( afTensorData[j] == Float.NaN )
                {
                	System.err.println( "nan" );
                	afTensorData[j] = 0;
                }
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            
            
            
			//Vector3f kScale = new Vector3f( fLambda1, fLambda2, fLambda3 );
			//kScale.Normalize();
			
			//kV1.Normalize();
			//kV1.Scale( kScale.X );
			
			//kV2.Normalize();
			//kV2.Scale( kScale.Y );
			
			//kV3.Normalize();
			//kV3.Scale( kScale.Z );

			//if ( (fLambda1 == fLambda2) && (fLambda1 == fLambda3) )
			//{}
			if ( !bAllZero && (fLambda1 > 0) && (fLambda2 > 0) && (fLambda3 > 0) )
			{
	            
	            kV1.Normalize();
	            kV2.Normalize();
	            kV3.Normalize();
	            
	            Vector3f kAxis = new Vector3f();
	            kAxis.Cross( Vector3f.UNIT_Z, kV1 );
	            kAxis.Normalize();
	            
	            float fAngle = kV1.Angle(Vector3f.UNIT_Z);
	            Matrix3f kRotate = new Matrix3f();
	            kRotate.FromAxisAngle( kAxis, fAngle);
	            
	            Vector3f kTest = new Vector3f();
	            kRotate.Mult( Vector3f.UNIT_Z, kTest );
	            if ( !kTest.IsEqual( kV1 ) )
	            {
	            	System.err.println( kTest + "   ==?  " + kV1 );
	            }
	            
				Transformation kTransform = new Transformation();
				
				kTransform.SetRotate( kRotate );
				
				//kTransform.SetMatrix(new Matrix3f(kV1,kV2,kV3,false));
				Vector3f kScale = new Vector3f( fLambda3, fLambda2, fLambda1 );
				kScale.Normalize();
				kScale.Set( (float)Math.max(.1, kScale.X), (float)Math.max(.1, kScale.Y), (float)Math.max(.1, kScale.Z) );
				kTransform.SetScale( kScale );
				
				//kTransform.SetScale( new Vector3f(kV1) );
				//System.err.println( kTransform.GetScale() );
				
				m_kEigenVectors.put( new Integer(i), kTransform );
			}
			if ( (i%(m_iDimX*m_iDimY)) == 0 )
			{
				int iValue = (int)(100 * (float)(i+1)/m_iLen);
				kProgressBar.updateValueImmed( iValue );
			}
		}
		kProgressBar.dispose();

		Integer kKey;
		int iIndex, iX, iY, iZ;
		float fX, fY, fZ;
		Vector3f kScale;
		Transformation kTransform;
		Transformation kTScale = new Transformation();
		Transformation kTEllipse = new Transformation();
		

		Iterator<Integer> kIterator = m_kEigenVectors.keySet().iterator();
		while ( kIterator.hasNext() )
		{
			kKey = (Integer)kIterator.next();
			iIndex = kKey.intValue();
			iX = iIndex % m_iDimX;
			iIndex -= iX;
			iIndex /= m_iDimX;

			iY = iIndex % m_iDimY;
			iIndex -= iY;
			iIndex /= m_iDimY;

			iZ = iIndex;

			// reset iIndex:
				iIndex = kKey.intValue();


				float xBox = (m_iDimX - 1) * fXDelta;
				float yBox = (m_iDimY - 1) * fYDelta;
				float zBox = (m_iDimZ - 1) * fZDelta;
				float maxBox = Math.max(xBox, Math.max(yBox, zBox));

				fX = ((2.0f * (iX * fXDelta)) - xBox)/(2.0f*maxBox);
				fY = ((2.0f * (iY * fYDelta)) - yBox)/(2.0f*maxBox);
				fZ = ((2.0f * (iZ * fZDelta)) - zBox)/(2.0f*maxBox);           

				kTScale.MakeIdentity();
				kTEllipse.MakeIdentity();

				kTransform = m_kEigenVectors.get(kKey);

				kScale = kTScale.GetScale();
				kScale.Scale( m_fScale );
				kScale.Mult( kTransform.GetScale() );
				kTScale.SetScale(kScale);

				//kTEllipse.SetTranslate( fX - .5f, fY - .5f, fZ - .5f );
				kTEllipse.SetTranslate( fX, fY, fZ );
				kTEllipse.SetMatrixCopy( kTransform.GetMatrix() );


				kTransform.Product( kTEllipse, kTScale );
		}
		kTEllipse = null;
		kTScale = null;
		

		System.out.println( "VolumeDTI " + m_iLen );
		*/

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
		ShaderEffect kShader = m_kShaders.get(kKey);
		if ( kShader == null )
		{
			return;
		}
		Program pkCProgram = kShader.GetCProgram(0);
		if ( pkCProgram == null )
		{
			return;
		}
		if ( kColor == null )
		{
			if ( pkCProgram.GetUC("UseConstantColor") != null )
			{
				pkCProgram.GetUC("UseConstantColor").GetData()[0] = 0;
			}

			m_kEllipseConstantColor.remove( kKey );
			m_kEllipseConstantColor.put( kKey, null );
		}
		else
		{
			if ( pkCProgram.GetUC("ConstantColor") != null )
			{
				pkCProgram.GetUC("ConstantColor").GetData()[0] = kColor.R;
				pkCProgram.GetUC("ConstantColor").GetData()[1] = kColor.G;
				pkCProgram.GetUC("ConstantColor").GetData()[2] = kColor.B;
				pkCProgram.GetUC("ConstantColor").GetData()[3] = 1;
			}
			if ( pkCProgram.GetUC("UseConstantColor") != null )
			{
				pkCProgram.GetUC("UseConstantColor").GetData()[0] = 1;
			}

			m_kEllipseConstantColor.remove( kKey );
			m_kEllipseConstantColor.put( kKey, kColor );

		}
	}

	/** Sets the polyline color for the specified fiber bundle tract group. 
	 * @param iGroup the fiber bundle group to set.
	 * @param kColor the new polyline color for the specified fiber bundle tract group. 
	 */
	public void setTubesGroupColor( int iGroup, ColorRGB kColor ) {
		if ( groupConstantColor.get(iGroup-1) != null )
			constantColor.put(groupConstantColor.get(iGroup-1).intValue(), kColor);
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
	private void DisplayGlyphs( ModelImage kImage, Renderer kRenderer )
	{
		if ( m_kGlyphs == null )
		{
			return;
		}
		Integer kKey;
		Integer cKey;
		Vector<VOIContour> kGlyphVector;
		VOIContour kContour;
		Vector3f kPos;
		float fR,fG,fB;
		TriMesh kGlyph = null;        
		Iterator<Integer> kIterator = m_kGlyphs.keySet().iterator();
		Iterator<Integer> cIterator = groupConstantColor.keySet().iterator();
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
		
		while ( kIterator.hasNext() )
		{
			kKey = (Integer)kIterator.next();
			cKey = (Integer)cIterator.next();
			kGlyphVector = m_kGlyphs.get(kKey);     
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
								if ( cKey == null ) continue;
								m_kColorEllipse = constantColor.get(groupConstantColor.get(cKey).intValue());
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
			            kV1.Set( afVectorData[0], afVectorData[1], afVectorData[2] );
			            kAxis.Cross( Vector3f.UNIT_Z, kV1 );
			            kAxis.Normalize();
			            
			            fAngle = kV1.Angle(Vector3f.UNIT_Z);
			            Matrix3f kRotate = new Matrix3f();
			            kRotate.FromAxisAngle( kAxis, fAngle);
			            

			            fLambda1 = m_kEigenValue.getFloatTriLinearBounds(kPos.X, kPos.Y, kPos.Z, 1);
			            fLambda2 = m_kEigenValue.getFloatTriLinearBounds(kPos.X, kPos.Y, kPos.Z, 2);
			            fLambda3 = m_kEigenValue.getFloatTriLinearBounds(kPos.X, kPos.Y, kPos.Z, 3);
			            
			            kTransform = new Transformation();
						kTransform.SetRotate( kRotate );
						
						Vector3f kScale = new Vector3f( fLambda3, fLambda2, fLambda1 );
						kScale.Normalize();
						kScale.Set( (float)Math.max(.1, kScale.X), (float)Math.max(.1, kScale.Y), (float)Math.max(.1, kScale.Z) );
						kScale.Scale(m_fScale);
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
							m_kArrow.Local.Copy(kTransform);

							m_kEllipseMaterial.Ambient = m_kColorEllipse;
							m_kEllipseMaterial.Diffuse = m_kColorEllipse;
							m_kScene.SetChild(0,m_kArrow);
							m_kScene.UpdateGS();
							m_kScene.DetachChild(m_kArrow);
							kRenderer.Draw((TriMesh)m_kArrow.GetChild(0));
							//kRenderer.Draw((TriMesh)m_kArrow.GetChild(1));
						}
						if ( kGlyph != null )
						{
							kGlyph.Local.Copy(kTransform);
							kGlyph.AttachGlobalState(m_kZBuffer);
							kGlyph.UpdateGS();

							m_kEllipseMaterial.Ambient = m_kColorEllipse;
							m_kEllipseMaterial.Diffuse = m_kColorEllipse;
							m_kScene.SetChild(0,kGlyph);
							m_kScene.UpdateGS();
							m_kScene.DetachChild(kGlyph);
							kRenderer.Draw(kGlyph);
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
	private void DisplayTract( ShaderEffect kInputShader, Renderer kRenderer )
	{
		Iterator<Integer> kIterator = m_kTracts.keySet().iterator();
		Integer iKey;
		Integer cKey;
		Node kTractNode;
		Polyline kTract;
		ShaderEffect kShader;
		Iterator<Integer> cIterator = groupConstantColor.keySet().iterator();

		//kAlpha.BlendEnabled = true;
		while ( kIterator.hasNext() )
		{
			iKey = (Integer)kIterator.next();
			cKey = (Integer)cIterator.next();

			kTractNode = m_kTracts.get(iKey);
			kShader = kInputShader;

			ColorRGB kColor1 = null;

			if ( kShader == null )
			{
				kShader = m_kShaders.get(iKey);
			}
			for ( int i = 0; i < kTractNode.GetQuantity(); i++ )
			{
				if (!isUsingVolumeColor) {
					if ( cKey == null ) continue;
					kColor1 = new ColorRGB(constantColor.get(groupConstantColor.get(cKey).intValue()));

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

				} else {
					kTract = (Polyline)kTractNode.GetChild(i);
					kTract.DetachAllEffects();
					kTract.AttachEffect( kShader );

					m_kScene.SetChild(0,kTract);
					m_kScene.UpdateGS();
					m_kScene.DetachChild(kTract);
					kRenderer.Draw(kTract);
					kTract.DetachEffect( kShader );
				}
			}
		}
	}

	/** Displays a tube fiber bundle tract with the given shader attached.
	 * @param kInputStader shader to apply to the tube.
	 */    
	private void DisplayTubes( ModelImage kImage, Renderer kRenderer )
	{
		Node kTubeNode;
		Integer iKey;
		Integer cKey;

		Iterator<Integer> iIterator = m_kTubes.keySet().iterator();
		Iterator<Integer> cIterator = groupConstantColor.keySet().iterator();

		m_kTubes.keySet();

		TubeSurface kTube;

		while ( iIterator.hasNext() )
		{
			iKey = (Integer)iIterator.next();
			cKey = (Integer)cIterator.next();

			kTubeNode = m_kTubes.get(iKey);                           
			kTube = (TubeSurface)kTubeNode.GetChild(0);                           

			ColorRGB kColor1 = ColorRGB.WHITE;
			if ( cKey != null ) {
				kColor1 = new ColorRGB(constantColor.get(groupConstantColor.get(cKey).intValue()));
			}    

			m_kLightShader.SetSurfaceTexture(isUsingVolumeColor, false, false);

			kTube.AttachGlobalState(m_kTubesMaterial);

			kTube.DetachAllEffects();
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

