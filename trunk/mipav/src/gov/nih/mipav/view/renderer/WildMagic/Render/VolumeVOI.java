package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIText;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.ZBufferState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.Spatial;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibGraphics.SceneGraph.VisibleObject;

/**
 * Displays the VOIBase in the GPU-based VolumeRenderer.
 * Creates the shaders and rendering states necessary for rendering and blending with the volume.
 *
 */
public class VolumeVOI extends VolumeObject
{
	private VOIBase m_kVOI;
	private Polyline m_kVOILine;
	private Polyline m_kVOITicMarks;
	private VolumeVOIEffect m_kVOIShader;
	private VolumePreRenderEffect m_kVOIPreShader;
	private ZBufferState m_kZState = new ZBufferState();
	private Vector3f m_kVolumeScale;
	private ColorRGB m_kColor = new ColorRGB(1,1,0);
	private float m_fOpacity = 1f;

	private VolumeVOIEffect m_kTextEffect = null;

	private float m_fAnnotationScale = 0.02f;
	private Vector3f m_kBillboardPos = null;
	private boolean m_bShowText = false;
	private boolean m_bUpdateDisplay = true;


	/**
	 * Constructor for the VolumeVOI object.
	 * @param kImageA
	 * @param kTranslate
	 * @param kVOI
	 * @param kColor
	 */
	public VolumeVOI ( VolumeImage kImageA, Vector3f kTranslate, VOIBase kVOI, ColorRGBA kColor )
	{
		super(kImageA,kTranslate, kImageA.GetScaleX(),kImageA.GetScaleY(),kImageA.GetScaleZ());
		m_kAlphaTransparency.BlendEnabled = false;
		m_kZBufferTransparency.Enabled = false;
		m_kVOI = kVOI;
		scaleVOI();
		m_bDisplay = true;
		update(kColor);
	}

	/** Delete local memory. */
	public void dispose(Renderer kRenderer)
	{
		m_kVOI = null;
		if ( m_kVOILine != null )
		{
			kRenderer.ReleaseVBuffer(m_kVOILine.VBuffer);
			kRenderer.ReleaseIBuffer(m_kVOILine.IBuffer);
			m_kVOILine.dispose();
			m_kVOILine = null;
		}
		if ( m_kVOITicMarks != null )
		{
			kRenderer.ReleaseVBuffer(m_kVOITicMarks.VBuffer);
			kRenderer.ReleaseIBuffer(m_kVOITicMarks.IBuffer);
			m_kVOITicMarks.dispose();
			m_kVOITicMarks = null;
		}
		if ( m_kVOIShader != null )
		{
			kRenderer.ReleaseResources( m_kVOIShader );
			m_kVOIShader.dispose();
			m_kVOIShader = null;
		}
		if ( m_kVOIPreShader != null )
		{
			kRenderer.ReleaseResources( m_kVOIPreShader );
			m_kVOIPreShader.dispose();
			m_kVOIPreShader = null;
		}
		if ( m_kZState != null )
		{
			m_kZState.dispose();
			m_kZState = null;
		}
		m_kVolumeScale = null;
		m_kColor = null;
		if ( m_kTextEffect != null )
		{
			kRenderer.ReleaseResources( m_kTextEffect );
			m_kTextEffect.dispose();
			m_kTextEffect = null;
		}
		m_kBillboardPos = null;
        super.dispose(kRenderer);
	}  

	/**
	 * Returns the current color of the VOI.
	 * @return
	 */
	public ColorRGBA getColor()
	{
		return new ColorRGBA( m_kColor.R, m_kColor.G, m_kColor.B, 1 );
	}


	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#GetName()
	 */
	public String GetName()
	{
		return m_kVOI.getName();
	}
	
	public boolean needsUpdate()
	{
		return m_bUpdateDisplay;
	}



	/**
	 * Returns the VOIBase object rendered by this VolumVOI object.
	 * @return
	 */
	public VOIBase getVOI()
	{
		return m_kVOI;
	}

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Render(WildMagic.LibGraphics.Rendering.Renderer, WildMagic.LibGraphics.SceneGraph.Culler)
	 */
	public void Render( Renderer kRenderer, Culler kCuller, boolean bPreRender, boolean bSolid )
	{
		if ( !m_bDisplay || !bSolid )
		{
			return;
		}
		m_kVOILine.DetachAllEffects();
		if ( bPreRender )
		{
			m_kVOILine.AttachEffect( m_kVOIPreShader );
		}
		else
		{
			m_kVOILine.AttachEffect( m_kVOIShader );
		}
		m_kScene.DetachGlobalState(GlobalState.StateType.ALPHA);
		m_kScene.DetachGlobalState(GlobalState.StateType.ZBUFFER);
		m_kScene.AttachGlobalState(m_kAlpha);
		m_kScene.AttachGlobalState(m_kZState);
			
		if ( !bPreRender && (m_kVOITicMarks != null) && ((m_kVOI.getType() == VOI.LINE) || (m_kVOI.getType() == VOI.PROTRACTOR)  || (m_kVOI.getType() == VOI.ANNOTATION)) )
		{
			m_kScene.DetachChild(m_kVOITicMarks);
			if ( m_kVOI.isActive() || (m_kVOI.getType() == VOI.ANNOTATION))
			{
				m_kVOITicMarks.DetachAllEffects();
				m_kVOITicMarks.AttachEffect( m_kVOIShader );
				m_kScene.AttachChild(m_kVOITicMarks);
			}
		}

		m_kScene.UpdateGS();
		m_kScene.UpdateRS();
		kCuller.ComputeVisibleSet(m_kScene);              
		kRenderer.DrawScene(kCuller.GetVisibleSet());

		if ( m_bShowText && (m_kBillboardPos != null) )
		{
			Matrix4f kWorld = null;
			if ( kCuller.GetVisibleSet() != null )
			{
				VisibleObject[] akVisible = kCuller.GetVisibleSet().GetVisible();
				if ( akVisible[0].Object != null )
				{
					kWorld = ((Geometry)akVisible[0].Object).HWorld;
				}
			}
			if ( kWorld != null )
			{
				float[] afData = new float[16];
				kRenderer.SetConstantVPMatrix( 0, afData );
				Matrix4f kMat = new Matrix4f(afData, true);
				Matrix4f kWVP = new Matrix4f();
				kWVP.Mult( kWorld, kMat );

				Vector4f kTextPos = new Vector4f();
				kWVP.MultLeft( new Vector4f( m_kBillboardPos.X, m_kBillboardPos.Y, m_kBillboardPos.Z, 1 ), kTextPos );
				kTextPos.Scale( 1f/kTextPos.W );

				String kLabel = ((VOIText)m_kVOI).getText();
				char[] acText = kLabel.toCharArray();
				kRenderer.Draw( kTextPos.X, kTextPos.Y, kTextPos.Z, 
						new ColorRGBA( m_kColor.R, m_kColor.G, m_kColor.B, m_fOpacity),acText);
			}
		}
		m_bUpdateDisplay = false;
	}

	/**
	 * Sets the transparency.
	 * @param fBlend
	 */
	public void setBlend( float fBlend)
	{
		m_kVOIShader.Blend(fBlend);
	}

	/**
	 * Sets the Color.
	 * @param kColor
	 */
	public void setColor(ColorRGB kColor)
	{
		if ( m_kVOI.isActive() )
		{
			m_kColor.Copy(kColor);
			for ( int i = 0; i < m_kVOILine.VBuffer.GetVertexQuantity(); i++ )
			{
				m_kVOILine.VBuffer.SetColor3(0, i, kColor );
			}
			m_kVOILine.VBuffer.Release();
			if ( m_kVOITicMarks != null )
			{
				for ( int i = 0; i < m_kVOITicMarks.VBuffer.GetVertexQuantity(); i++ )
				{
					m_kVOITicMarks.VBuffer.SetColor3(0, i, kColor );
				}
				m_kVOITicMarks.VBuffer.Release();
			}
		}
	}

	/**
	 * Sets the slice information for the shader.
	 * @param bUseSlice
	 * @param iWhichSlice
	 * @param fSlice
	 */
	public void setSlice( boolean bUseSlice, int iWhichSlice, float fSlice )
	{
		//System.err.println( (fSlice == m_kVOILine.VBuffer.GetPosition3fZ(0)) + " " + fSlice + " " + m_kVOILine.VBuffer.GetPosition3fZ(0) );
		m_kVOIShader.SetSlice(bUseSlice, iWhichSlice, fSlice);
	}

	/**
	 * Sets and updates the VOIBase displayed by this VolumeVOI.
	 * @param kVOI
	 */
	public void setVOI( VOIBase kVOI, boolean bForceReload )
	{
		if ( bForceReload )
		{
			m_kVOI = kVOI;
			scaleVOI();
		}
		else
		{
			setVOI(kVOI);
		}
		m_bUpdateDisplay = true;
	}

	/**
	 * Sets and updates the VOIBase displayed by this VolumeVOI.
	 * @param kVOI
	 */
	public void setVOI( VOIBase kVOI )
	{
		m_kVOI = kVOI;
		if ( m_kVOILine == null )
		{
			scaleVOI();
		}
		else if ( kVOI.getType() == VOI.POINT )
		{
			Vector3f kDiff = new Vector3f(m_kVOI.get(0));
			kDiff.Mult(m_kVolumeScale);
			kDiff.Sub( m_kVOILine.VBuffer.GetPosition3(0) );
			for ( int i = 0; i < m_kVOILine.VBuffer.GetVertexQuantity(); i++ )
			{
				Vector3f kPos = m_kVOILine.VBuffer.GetPosition3(i);
				kPos.Add(kDiff);
				m_kVOILine.VBuffer.SetPosition3(i, kPos );
			}
			m_kVOILine.VBuffer.Release();
		}
		else if ( kVOI.size() == m_kVOILine.VBuffer.GetVertexQuantity() )
		{
			for ( int i = 0; i < m_kVOILine.VBuffer.GetVertexQuantity(); i++ )
			{
				Vector3f kPos = new Vector3f( m_kVOI.get(i) );
				kPos.Mult(m_kVolumeScale);
				m_kVOILine.VBuffer.SetPosition3(i, kPos );
			}
			m_kVOILine.VBuffer.Release();
		}
		else 
		{
			m_kScene.DetachChild(m_kVOILine);
			m_kVOILine.dispose();
			m_kVOILine = null;
			scaleVOI();
		}

		if ( (m_kVOI.getType() == VOI.LINE) && !m_kVOI.isSplit() )
		{
			lineAnnotations( m_kVOI.get(0), m_kVOI.get(1) );
		}

		if ( m_kVOI.getType() == VOI.PROTRACTOR )
		{
			protractorAnnotations( m_kVOI.get(0), m_kVOI.get(1), m_kVOI.get(2) );
		}
		if ( m_kVOI.getType() == VOI.ANNOTATION )
		{
			textAnnotations( m_kVOI.get(1), m_kVOI.get(0) );
			m_kBillboardPos = new Vector3f(m_kVOILine.VBuffer.GetPosition3(1));
		}
		m_bUpdateDisplay = true;
	}

	/**
	 * Turns the ZBufferState compare mode on or off.
	 * @param bOn
	 */
	public void setZCompare( boolean bOn )
	{
		if ( bOn )
		{
			m_kZState.Compare = ZBufferState.CompareMode.CF_LEQUAL;
		}
		else
		{
			m_kZState.Compare = ZBufferState.CompareMode.CF_ALWAYS;
		}
	}

	/**
	 * Turns displaying the text on or off.
	 * @param bShow
	 */
	public void showTextBox(boolean bShow)
	{
		m_bShowText = bShow;
	}

	/**
	 * Set the color of the VolumeVOI object.
	 * @param kColor
	 */
	public void update( ColorRGBA kColor )
	{
		m_kColor.Set( kColor.R, kColor.G, kColor.B );
		m_fOpacity = kColor.A;
		setBlend( m_fOpacity );
		for ( int i = 0; i < m_kVOILine.VBuffer.GetVertexQuantity(); i++ )
		{
			m_kVOILine.VBuffer.SetColor3(0, i, m_kColor );
		}
		m_kVOILine.VBuffer.Release();
		if ( m_kVOITicMarks != null )
		{
			for ( int i = 0; i < m_kVOITicMarks.VBuffer.GetVertexQuantity(); i++ )
			{
				m_kVOITicMarks.VBuffer.SetColor3(0, i, m_kColor );
			}
			m_kVOITicMarks.VBuffer.Release();
		}
		if ( (m_kVOI.getType() == VOI.ANNOTATION) && (m_kTextEffect != null) )
		{
			m_kTextEffect.Blend(m_fOpacity*2);
			m_kTextEffect.SetColor(m_kColor);
		}
	}

	/**
	 * Sets up the line annotations.
	 * @param kStart
	 * @param kEnd
	 * @param fraction
	 * @param kVBuffer
	 * @param iPos
	 * @return
	 */
	private int getCoords(Vector3f kStart, Vector3f kEnd, float fraction, VertexBuffer kVBuffer, int iPos) {
		Vector3f kStartTemp = new Vector3f(kStart);
		Vector3f kEndTemp = new Vector3f(kEnd);
		kStartTemp.Mult(m_kVolumeScale);
		kEndTemp.Mult(m_kVolumeScale);

		Vector3f kMidPoint = new Vector3f((kStartTemp.X + kEndTemp.X) / 2, (kStartTemp.Y + kEndTemp.Y) / 2, (kStartTemp.Z + kEndTemp.Z) / 2 );
		if (fraction == .25)
		{
			kMidPoint.Add(kStartTemp);
			kMidPoint.Scale(0.5f);
		} 
		else if (fraction == .75)
		{
			kMidPoint.Add(kEndTemp);
			kMidPoint.Scale(0.5f);
		}

		Vector3f kW = new Vector3f();
		kW.Sub( kEndTemp, kStartTemp );
		kW.Normalize();

		Vector3f kU = new Vector3f();
		Vector3f kV = new Vector3f();
		Vector3f.GenerateComplementBasis( kU, kV, kW );


		kU.Scale(m_fAnnotationScale);      
		Vector3f kNewStart = new Vector3f();
		kNewStart.Add( kMidPoint, kU );
		Vector3f kNewEnd = new Vector3f();
		kNewEnd.Sub( kMidPoint, kU );

		kVBuffer.SetPosition3( iPos++, kNewStart );
		kVBuffer.SetPosition3( iPos++, kNewEnd );


		kV.Scale(m_fAnnotationScale);      
		kNewStart = new Vector3f();
		kNewStart.Add( kMidPoint, kV );
		kNewEnd = new Vector3f();
		kNewEnd.Sub( kMidPoint, kV );

		kVBuffer.SetPosition3( iPos++, kNewStart );
		kVBuffer.SetPosition3( iPos++, kNewEnd );

		return iPos;
	}

	/**
	 * Sets up the line annotations for the end-points of the line (arrows).
	 * @param kStart
	 * @param kEnd
	 */
	private int getEndLines(Vector3f kStart, Vector3f kEnd, VertexBuffer kVBuffer, int iPos) {

		Vector3f kStartTemp = new Vector3f(kStart);
		Vector3f kEndTemp = new Vector3f(kEnd);

		kStartTemp.Mult(m_kVolumeScale);
		kEndTemp.Mult(m_kVolumeScale);

		Vector3f kW = new Vector3f();
		kW.Sub( kEndTemp, kStartTemp );
		kW.Normalize();

		Vector3f kU = new Vector3f();
		Vector3f kV = new Vector3f();
		Vector3f.GenerateComplementBasis( kU, kV, kW );


		Vector3f kTail1 = new Vector3f();
		kTail1.Add( kW, kU );
		kTail1.Scale(0.5f);
		Vector3f kTail3 = new Vector3f();
		kTail3.Neg(kTail1);      

		Vector3f kTail2 = new Vector3f();
		kTail2.Sub( kW, kU );
		kTail2.Scale(0.5f);
		Vector3f kTail4 = new Vector3f();
		kTail4.Neg( kTail2);


		kTail1.Scale(2*m_fAnnotationScale);
		kTail2.Scale(2*m_fAnnotationScale);

		Vector3f kTemp = new Vector3f();
		kTemp.Add( kEndTemp, kTail1 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kEndTemp );
		kTemp.Add( kEndTemp, kTail2 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kEndTemp );


		kTail3.Scale(2*m_fAnnotationScale);
		kTail4.Scale(2*m_fAnnotationScale);

		//System.err.println( kTail1.ToString() + " " + kTail2.ToString() + " " + kTail3.ToString() + " " + kTail4.ToString() );

		kTemp.Add( kStartTemp, kTail3 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kStartTemp );
		kTemp.Add( kStartTemp, kTail4 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kStartTemp );




		kTail1 = new Vector3f();
		kTail1.Add( kW, kV );
		kTail1.Scale(0.5f);
		kTail3 = new Vector3f();
		kTail3.Neg(kTail1);      

		kTail2 = new Vector3f();
		kTail2.Sub( kW, kV );
		kTail2.Scale(0.5f);
		kTail4 = new Vector3f();
		kTail4.Neg( kTail2);


		kTail1.Scale(2*m_fAnnotationScale);
		kTail2.Scale(2*m_fAnnotationScale);

		kTemp = new Vector3f();
		kTemp.Add( kEndTemp, kTail1 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kEndTemp );
		kTemp.Add( kEndTemp, kTail2 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kEndTemp );


		kTail3.Scale(2*m_fAnnotationScale);
		kTail4.Scale(2*m_fAnnotationScale);

		//System.err.println( kTail1.ToString() + " " + kTail2.ToString() + " " + kTail3.ToString() + " " + kTail4.ToString() );

		kTemp.Add( kStartTemp, kTail3 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kStartTemp );
		kTemp.Add( kStartTemp, kTail4 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kStartTemp );

		return iPos;
	}
	/**
	 * Sets up the protractor annotations for the end-points of the protractor (arrows).
	 * @param kStart
	 * @param kEnd
	 */
	private int getEndLines2(Vector3f kStart, Vector3f kEnd, VertexBuffer kVBuffer, int iPos) {
		Vector3f kStartTemp = new Vector3f(kStart);
		Vector3f kEndTemp = new Vector3f(kEnd);
		kStartTemp.Mult(m_kVolumeScale);
		kEndTemp.Mult(m_kVolumeScale);

		Vector3f kW = new Vector3f();
		kW.Sub( kEndTemp, kStartTemp );
		kW.Normalize();

		Vector3f kU = new Vector3f();
		Vector3f kV = new Vector3f();
		Vector3f.GenerateComplementBasis( kU, kV, kW );


		Vector3f kTail1 = new Vector3f();
		kTail1.Add( kW, kU );
		kTail1.Scale(0.5f);
		Vector3f kTail3 = new Vector3f();
		kTail3.Neg(kTail1);      

		Vector3f kTail2 = new Vector3f();
		kTail2.Sub( kW, kU );
		kTail2.Scale(0.5f);
		Vector3f kTail4 = new Vector3f();
		kTail4.Neg( kTail2);


		kTail3.Scale(2*m_fAnnotationScale);
		kTail4.Scale(2*m_fAnnotationScale);

		Vector3f kTemp = new Vector3f();
		kTemp.Add( kEndTemp, kTail3 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kEndTemp );
		kTemp.Add( kEndTemp, kTail4 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kEndTemp );


		kTail1.Add( kW, kV );
		kTail1.Scale(0.5f);
		kTail3.Neg(kTail1);      

		kTail2.Sub( kW, kV );
		kTail2.Scale(0.5f);
		kTail4.Neg( kTail2);        

		kTail3.Scale(2*m_fAnnotationScale);
		kTail4.Scale(2*m_fAnnotationScale);

		kTemp.Add( kEndTemp, kTail3 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kEndTemp );
		kTemp.Add( kEndTemp, kTail4 );        
		kVBuffer.SetPosition3( iPos++, kTemp );
		kVBuffer.SetPosition3( iPos++, kEndTemp );


		/*
        int iPlane = m_kVOI.getPlane();
        float fAngle = (float)Math.PI/2.0f;
        Matrix3f kMat = new Matrix3f(false);

        Vector3f kTail3 = new Vector3f();
        Vector3f kTail4 = new Vector3f();
        Vector3f kAxis;
        if ( iPlane == 0 )
        {
            kAxis = Vector3f.UNIT_X;
        }
        else if ( iPlane == 1 )
        {
            kAxis = Vector3f.UNIT_Y;
        }
        else
        {
            kAxis = Vector3f.UNIT_Z;
        }
        fAngle = (float)(3.0*Math.PI)/4.0f;
        kMat.FromAxisAngle( kAxis, fAngle);
        kMat.Mult(kW, kTail3);         
        kTail3.Scale(2*m_afAnnotationScale);

        kMat.FromAxisAngle( kAxis, -fAngle);
        kMat.Mult(kW, kTail4);         
        kTail4.Scale(2*m_afAnnotationScale);


        Vector3f kTemp = new Vector3f();
        kTemp.Add( kEndTemp, kTail3 );        
        kVBuffer.SetPosition3( iPos++, kTemp );
        kVBuffer.SetPosition3( iPos++, kEndTemp );
        kTemp.Add( kEndTemp, kTail4 );        
        kVBuffer.SetPosition3( iPos++, kTemp );
        kVBuffer.SetPosition3( iPos++, kEndTemp );
		 */
		return iPos;
	}


	/**
	 * Creates the line annotations.
	 * @param kStart
	 * @param kEnd
	 */
	private void lineAnnotations( Vector3f kStart, Vector3f kEnd )
	{
		Attributes kAttributes = new Attributes();
		kAttributes.SetCChannels(0, 3);
		kAttributes.SetPChannels(3);

		VertexBuffer kVBuffer = new VertexBuffer(kAttributes, 28 );
		for ( int i = 0; i < kVBuffer.GetVertexQuantity(); i++ )
		{
			kVBuffer.SetColor3(0, i, m_kColor );
		}
		int iPos = 0;
		iPos = getCoords( kStart, kEnd, 0.5f, kVBuffer, iPos );
		iPos = getCoords( kStart, kEnd, 0.25f, kVBuffer, iPos );
		iPos = getCoords( kStart, kEnd, 0.75f, kVBuffer, iPos );
		iPos = getEndLines( kStart, kEnd, kVBuffer, iPos );
		if ( m_kVOITicMarks == null )
		{
			m_kVOITicMarks = new Polyline( kVBuffer, false, false );
			m_kVOITicMarks.AttachGlobalState(m_kZState);
			m_kVOITicMarks.AttachEffect( m_kVOIShader );
			m_kVOITicMarks.Local.SetTranslate(m_kTranslate);
		}
		else
		{
			m_kVOITicMarks.VBuffer = kVBuffer;
		}

	}


	/**
	 * Creates the protractor annotations.
	 * @param kStart
	 * @param kMiddle
	 * @param kEnd
	 */
	private void protractorAnnotations( Vector3f kStart, Vector3f kMiddle, Vector3f kEnd )
	{
		Attributes kAttributes = new Attributes();
		kAttributes.SetCChannels(0, 3);
		kAttributes.SetPChannels(3);

		VertexBuffer kVBuffer = new VertexBuffer(kAttributes, 32 );
		for ( int i = 0; i < kVBuffer.GetVertexQuantity(); i++ )
		{
			kVBuffer.SetColor3(0, i, m_kColor );
		}
		int iPos = 0;
		iPos = getEndLines2( kMiddle, kStart, kVBuffer, iPos );
		iPos = getEndLines2( kMiddle, kEnd, kVBuffer, iPos );
		if ( m_kVOITicMarks == null )
		{
			m_kVOITicMarks = new Polyline( kVBuffer, false, false );
			m_kVOITicMarks.AttachGlobalState(m_kZState);
			m_kVOITicMarks.AttachEffect( m_kVOIShader );
			m_kVOITicMarks.Local.SetTranslate(m_kTranslate);
		}
		else
		{
			m_kVOITicMarks.VBuffer = kVBuffer;
		}

	}

	/**
	 * Creates the scene graph for displaying the VOIBase object.
	 */
	private void scaleVOI()
	{
		m_kVOIShader = new VolumeVOIEffect(false);
		m_kVOIPreShader = new VolumePreRenderEffect(true, true, false);

		m_kScene = new Node();
		m_kCull = new CullState();
		m_kCull.Enabled = false;
		m_kScene.AttachGlobalState(m_kCull);
		m_kScene.Culling = Spatial.CullingMode.CULL_NEVER;

		m_kAlpha = new AlphaState();
		m_kAlpha.BlendEnabled = true;
		m_kScene.AttachGlobalState(m_kAlpha);

		ModelImage kImageA = m_kVolumeImageA.GetImage();
		m_kVolumeScale = new Vector3f(m_kVolumeImageA.GetScaleX()/(kImageA.getExtents()[0] - 1), 
				m_kVolumeImageA.GetScaleY()/(kImageA.getExtents()[1] - 1), 
				m_kVolumeImageA.GetScaleZ()/(kImageA.getExtents()[2] - 1)  );
		

        Vector3f kVolumeScale = new Vector3f(m_kVolumeImageA.GetScaleX(), m_kVolumeImageA.GetScaleY(), m_kVolumeImageA.GetScaleZ()  );
        Vector3f kExtentsScale = new Vector3f(1f/(kImageA.getExtents()[0] - 1), 
                1f/(kImageA.getExtents()[1] - 1), 
                1f/(kImageA.getExtents()[2] - 1)  );

		Attributes kAttributes = new Attributes();
		kAttributes.SetCChannels(0, 3);
		kAttributes.SetPChannels(3);
		kAttributes.SetTChannels(0, 3);

		VertexBuffer kVBuffer = null;
		if ( m_kVOI.getType() == VOI.POINT )
		{
			kVBuffer = new VertexBuffer(kAttributes, 12 );
			Vector3f kPos = new Vector3f( m_kVOI.get(0) );
			kPos.Mult(m_kVolumeScale);

			kVBuffer.SetPosition3(0, kPos );
			kVBuffer.SetPosition3(1, kPos.X + m_fAnnotationScale, kPos.Y, kPos.Z );            
			kVBuffer.SetPosition3(2, kPos );
			kVBuffer.SetPosition3(3, kPos.X - m_fAnnotationScale, kPos.Y, kPos.Z );
			kVBuffer.SetPosition3(4, kPos );
			kVBuffer.SetPosition3(5, kPos.X, kPos.Y + m_fAnnotationScale, kPos.Z );
			kVBuffer.SetPosition3(6, kPos );
			kVBuffer.SetPosition3(7, kPos.X, kPos.Y - m_fAnnotationScale, kPos.Z );
			kVBuffer.SetPosition3(8, kPos );
			kVBuffer.SetPosition3(9, kPos.X, kPos.Y, kPos.Z + m_fAnnotationScale );
			kVBuffer.SetPosition3(10, kPos );
			kVBuffer.SetPosition3(11, kPos.X, kPos.Y, kPos.Z - m_fAnnotationScale );


			for ( int i = 0; i < kVBuffer.GetVertexQuantity(); i++ )
			{
				kVBuffer.SetColor3(0, i, m_kColor );
			}
		}
		else
		{
			kVBuffer = new VertexBuffer(kAttributes, m_kVOI.size() );
			for ( int i = 0; i < kVBuffer.GetVertexQuantity(); i++ )
			{
				Vector3f kPos = new Vector3f( m_kVOI.get(i) );
	            kPos.Mult(kExtentsScale);
	            kVBuffer.SetTCoord3(0, i, kPos);
	            kPos.Mult(kVolumeScale);
	            
				kVBuffer.SetPosition3(i, kPos );
				kVBuffer.SetColor3(0, i, m_kColor );
			}
		}

		boolean bClosed = m_kVOI.isClosed();
		m_kVOILine = new Polyline( kVBuffer, bClosed, true );
		m_kVOILine.AttachGlobalState(m_kZState);
		m_kVOILine.AttachEffect( m_kVOIShader );
		m_kVOILine.Local.SetTranslate(m_kTranslate);
		m_kScene.AttachChild(m_kVOILine);
		m_kScene.UpdateGS();
		m_kScene.UpdateRS();


		if ( (m_kVOI.getType() == VOI.ANNOTATION) )
		{
			textAnnotations( m_kVOI.get(1), m_kVOI.get(0) );
			m_kBillboardPos = new Vector3f(m_kVOILine.VBuffer.GetPosition3(1));
		}

	}

	/**
	 * Creates the text annotations (arrow)
	 * @param kStart
	 * @param kEnd
	 */
	private void textAnnotations( Vector3f kStart, Vector3f kEnd )
	{
		Attributes kAttributes = new Attributes();
		kAttributes.SetCChannels(0, 3);
		kAttributes.SetPChannels(3);

		VertexBuffer kVBuffer = new VertexBuffer(kAttributes, 16 );
		for ( int i = 0; i < kVBuffer.GetVertexQuantity(); i++ )
		{
			kVBuffer.SetColor3(0, i, m_kColor );
		}
		int iPos = 0;
		iPos = getEndLines2( kStart, kEnd, kVBuffer, iPos );
		if ( m_kVOITicMarks == null )
		{
			m_kVOITicMarks = new Polyline( kVBuffer, false, false );
			m_kVOITicMarks.AttachGlobalState(m_kZState);
			m_kVOITicMarks.AttachEffect( m_kVOIShader );
			m_kVOITicMarks.Local.SetTranslate(m_kTranslate);
		}
		else
		{
			m_kVOITicMarks.VBuffer = kVBuffer;
		}

	}

}
