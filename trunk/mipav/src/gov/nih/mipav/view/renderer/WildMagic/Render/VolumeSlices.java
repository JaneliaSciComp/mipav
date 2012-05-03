package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.structures.ModelRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.PolygonOffsetState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.ZBufferState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * Displays the three orthogonal planes with the volume data.
 * @see GPUVolumeRender.java
 * @see VolumePlaneEffect.java
 * @see VolumeObject.java
 */
public class VolumeSlices extends VolumeObject
{
	private VolumePreRenderEffect[] m_kBoundingBoxShaderSolid;
	private VolumePreRenderEffect[] m_kBoundingBoxShaderTransparent;
	
    /** ShaderEffect for the pre-render stage the bounding-box and the slice share the same pre-render effect. */
    private VolumePreRenderEffect[] m_kVolumePreShader;
    private VolumePreRenderEffect[] m_kVolumePreShaderTransparent;

    /** ShaderEffects for the planes. Each is unique so they can have different alpha values. */
    private VolumePlaneEffect[] m_akPlaneEffect;
    private VolumePlaneEffect[] m_akPlaneEffectTransparent;

    /** The three orthogonal plane TriMeshes. */
    private TriMesh[] m_akPlanes;

    /** Displaying each plane: */
    private boolean[] m_abShowPlanes = new boolean[]{true,true,true};

    /** The three plane bounding-box Polylines. */
    private Polyline[] m_akBoundingBox;
    
    /** Displaying each bounding-box: */
    private boolean[] m_abShowBoundingBox = new boolean[]{true,true,true};
    
    /** Set of colors used to draw the X and Y Bars and the Z box:. */
    private ColorRGB[] m_akColors = { new ColorRGB(1, 0, 0), new ColorRGB(0, 1, 0), new ColorRGB(1, 1, 0) };

    private boolean[] m_abSolid = new boolean[]{true, true, true};
    
    
    /** Create a new VolumeObject with the VolumeImage parameter.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     * @param kVolumeImageB second VolumeImage.
     * @param kTranslate translation in the scene-graph for this object.
     * @param fX the size of the volume in the x-dimension (extent * resolutions)
     * @param fY the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeSlices ( Renderer kRenderer, VolumeImage kImageA, VolumeImage kImageB, Vector3f kTranslate, float fX, float fY, float fZ )
    {
        super(kImageA,kImageB,kTranslate,fX,fY,fZ);

        //System.err.println( kTranslate.ToString() );
        m_kBoundingBoxShaderSolid = new VolumePreRenderEffect[3];
        m_kBoundingBoxShaderTransparent = new VolumePreRenderEffect[3];
        m_akPlaneEffect = new VolumePlaneEffect[3];
        m_akPlaneEffectTransparent = new VolumePlaneEffect[3];
        m_kVolumePreShader = new VolumePreRenderEffect[3];
        m_kVolumePreShaderTransparent = new VolumePreRenderEffect[3];
        for ( int i = 0; i < 3; i++ )
        {
        	m_kBoundingBoxShaderSolid[i] = new VolumePreRenderEffect(false, true, false);
        	m_kBoundingBoxShaderTransparent[i] = new VolumePreRenderEffect(false, true, true);
        	
            m_akPlaneEffect[i] = new VolumePlaneEffect( m_kVolumeImageA, m_kVolumeImageB, true, false );
            m_akPlaneEffectTransparent[i] = new VolumePlaneEffect( m_kVolumeImageA, m_kVolumeImageB, true, true );
            
            m_kVolumePreShader[i] = new VolumePreRenderEffect(true, true, false);
            m_kVolumePreShaderTransparent[i] = new VolumePreRenderEffect(false, true, true);
        }

        CreatePlanes( );
        CreateBoundingBox();
        for ( int i = 0; i < 3; i++ )
        {
            SetBoundingBoxColor( i, m_akColors[i] );
        }
        SetCenter(new Vector3f( .5f, .5f, .5f ) );
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();

        for ( int i = 0; i < 3; i++ )
        {
            m_akPlanes[i].AttachEffect( m_akPlaneEffect[i] );
            kRenderer.LoadResources( m_akPlanes[i] );
            m_akPlanes[i].DetachAllEffects();
            
            m_akPlanes[i].AttachEffect( m_akPlaneEffectTransparent[i] );
            kRenderer.LoadResources( m_akPlanes[i] );
            m_akPlanes[i].DetachAllEffects();
            
            m_akPlanes[i].AttachEffect( m_kVolumePreShader[i] );
            kRenderer.LoadResources( m_akPlanes[i] );
            m_akPlanes[i].DetachAllEffects();
            
            m_akPlanes[i].AttachEffect( m_kVolumePreShaderTransparent[i] );
            kRenderer.LoadResources( m_akPlanes[i] );
            m_akPlanes[i].DetachAllEffects();
        }
        kRenderer.LoadAllResources( m_kScene );
    }

    /** Delete local memory. */
    public void dispose(Renderer kRenderer)
    {
        for ( int i = 0; i < 3; i++ )
        {   

            if ( m_kBoundingBoxShaderSolid != null )
            {
            	if ( m_kBoundingBoxShaderSolid[i] != null )
            	{
                	kRenderer.ReleaseResources(m_kBoundingBoxShaderSolid[i]);
                	m_kBoundingBoxShaderSolid[i].dispose();
                	m_kBoundingBoxShaderSolid[i] = null;            		
            	}
            }
            if ( m_kBoundingBoxShaderTransparent != null )
            {
            	if ( m_kBoundingBoxShaderTransparent[i] != null )
            	{
                	kRenderer.ReleaseResources(m_kBoundingBoxShaderTransparent[i]);
                	m_kBoundingBoxShaderTransparent[i].dispose();
                	m_kBoundingBoxShaderTransparent[i] = null;            		
            	}
            }
            if ( m_kVolumePreShader != null )
            {
                if ( m_kVolumePreShader[i] != null )
                {
                	kRenderer.ReleaseResources(m_kVolumePreShader[i]);
                    m_kVolumePreShader[i].dispose();
                    m_kVolumePreShader[i] = null;
                }
                m_kVolumePreShader = null;
            }
            if ( m_kVolumePreShaderTransparent != null )
            {
                if ( m_kVolumePreShaderTransparent[i] != null )
                {
                	kRenderer.ReleaseResources(m_kVolumePreShaderTransparent[i]);
                    m_kVolumePreShaderTransparent[i].dispose();
                    m_kVolumePreShaderTransparent[i] = null;
                }
                m_kVolumePreShaderTransparent = null;
            }
            if ( m_akPlaneEffect != null )
            {
                if ( m_akPlaneEffect[i] != null )
                {
                	kRenderer.ReleaseResources(m_akPlaneEffect[i]);
                    m_akPlaneEffect[i].dispose();
                    m_akPlaneEffect[i] = null;
                }
                m_akPlaneEffect = null;
            }
            if ( m_akPlaneEffectTransparent != null )
            {
                if ( m_akPlaneEffectTransparent[i] != null )
                {
                	kRenderer.ReleaseResources(m_akPlaneEffectTransparent[i]);
                    m_akPlaneEffectTransparent[i].dispose();
                    m_akPlaneEffectTransparent[i] = null;
                }
                m_akPlaneEffectTransparent = null;
            }
            if ( m_akPlanes != null )
            {
                if ( m_akPlanes[i] != null )
                {
                	kRenderer.ReleaseVBuffer(m_akPlanes[i].VBuffer);
                	kRenderer.ReleaseIBuffer(m_akPlanes[i].IBuffer);
                    m_akPlanes[i].dispose();
                    m_akPlanes[i] = null;
                }
                m_akPlanes = null;
            }
            if ( m_akBoundingBox != null )
            {
                if ( m_akBoundingBox[i] != null )
                {
                	kRenderer.ReleaseVBuffer(m_akBoundingBox[i].VBuffer);
                	kRenderer.ReleaseIBuffer(m_akBoundingBox[i].IBuffer);
                    m_akBoundingBox[i].dispose();
                    m_akBoundingBox[i] = null;
                }
                m_akBoundingBox = null;
            }
            if ( m_akColors != null )
            {
                if ( m_akColors[i] != null )
                {
                    m_akColors[i] = null;
                }
            }
        }
        m_akColors = null;
        m_abShowPlanes = null;
        m_abShowBoundingBox = null;

        super.dispose(kRenderer);
    }

    public Node GetScene()
    {
        m_kScene.DetachAllChildren();
        for ( int i = 0; i < 3; i++ )
        {
            m_kScene.AttachChild(m_akPlanes[i]);
        }
        return m_kScene;
    }
    
    /** Return the current display status for the bounding box for the given plane.
     * @param i the plane index (0-3) in file coordinates.
     * @return true when the bounding box is displayed.
     */
    public boolean GetShowBoundingBox( int i )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        return m_abShowBoundingBox[iIndex];
    }
    /** Return the current display status for the bounding box for the given plane.
     * @param i the plane index (0-3) in file coordinates.
     * @return true when the bounding box is displayed.
     */
    public boolean GetShowSlice( int i )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        return m_abShowPlanes[iIndex];
    }
    
    /**
     * Return the opacity for the given plane.
     * @param i the plane index (0-3) in file coordinates.
     * @return the opacity for the given plane.
     */
    public float GetSliceOpacity( int i )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        return m_akPlaneEffect[iIndex].GetBlend(); 
    }
    
    public float[] GetSliceOpacity()
    {
    	return new float[]{
    			m_akPlaneEffect[0].GetBlend(),
    			m_akPlaneEffect[1].GetBlend(),
    			m_akPlaneEffect[2].GetBlend()
    	};
    }
    
    public void SetSliceOpacity(float[] afAlpha)
    {
    	for ( int i = 0; i < afAlpha.length; i++ )
    	{
    		m_abSolid[i] = afAlpha[i] >= 1.0 ? true : false;
    		m_akPlaneEffect[i].Blend( afAlpha[i] );
    		m_akPlaneEffectTransparent[i].Blend( afAlpha[i] );
    		m_kVolumePreShader[i].Blend(afAlpha[i]);
    		m_kVolumePreShaderTransparent[i].Blend(afAlpha[i]);
    	}
    }



    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Render(WildMagic.LibGraphics.Rendering.Renderer, WildMagic.LibGraphics.SceneGraph.Culler)
     */
    public void Render( Renderer kRenderer, Culler kCuller, boolean bPreRender, boolean bSolid )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        boolean bRender = false;
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlanes[i].DetachAllEffects();
            m_akBoundingBox[i].DetachAllEffects();
            m_kScene.DetachChild( m_akPlanes[i] );
            m_kScene.DetachChild(m_akBoundingBox[i]);
            if ( bSolid == m_abSolid[i] )
            {                
                if ( m_akPlaneEffect[i].GetBlend() != 0 )
                {
                    if ( m_abShowPlanes[i] )
                    {
                        m_kScene.AttachChild( m_akPlanes[i] );
                        bRender = true;
                    }
                    if ( m_abShowBoundingBox[i] )
                    {
                        m_kScene.AttachChild(m_akBoundingBox[i]);
                        bRender = true;
                    }
                }
            }
            if ( bSolid )
            {
                if ( bPreRender )
                {
                    m_akPlanes[i].AttachEffect( m_kVolumePreShader[i] );
                    m_akBoundingBox[i].AttachEffect( m_kVolumePreShader[i] );
                }
                else
                {
                    m_akPlanes[i].AttachEffect( m_akPlaneEffect[i] );
                    m_akBoundingBox[i].AttachEffect( m_kBoundingBoxShaderSolid[i] );
                }
            }
            else
            {
                if ( bPreRender )
                {
                    m_akPlanes[i].AttachEffect( m_kVolumePreShaderTransparent[i] );
                    m_akBoundingBox[i].AttachEffect( m_kVolumePreShaderTransparent[i] );
                }
                else
                {
                    m_akPlanes[i].AttachEffect( m_akPlaneEffectTransparent[i] );
                    m_akBoundingBox[i].AttachEffect( m_kBoundingBoxShaderTransparent[i] );
                }
            }
        }
        if ( !bRender )
        {
            return;
        }
        m_kScene.DetachGlobalState(GlobalState.StateType.ALPHA);
        m_kScene.DetachGlobalState(GlobalState.StateType.ZBUFFER);
        if ( !bSolid )
        {
            m_kScene.AttachGlobalState(m_kAlphaTransparency);
            m_kScene.AttachGlobalState(m_kZBufferTransparency);
            m_kZBufferTransparency.Writable = false;
        }
        else
        {
            m_kScene.AttachGlobalState(m_kAlpha);
            m_kScene.AttachGlobalState(m_kZBuffer);
        }
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        kCuller.ComputeVisibleSet(m_kScene);              
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }    
    
    public void setABBlend( float fBlend )
    {
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlaneEffect[i].setABBlend(fBlend);
            m_akPlaneEffectTransparent[i].setABBlend(fBlend);
        }
    }
    
    /**
     * Sets the background color.
     * @param kColor new background color.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlaneEffect[i].SetBackgroundColor( kColor );
            m_akPlaneEffectTransparent[i].SetBackgroundColor( kColor );
        }
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#PreRender(WildMagic.LibGraphics.Rendering.Renderer, WildMagic.LibGraphics.SceneGraph.Culler)
     
    public void PreRender( Renderer kRenderer, Culler kCuller, boolean bSolid )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlanes[i].DetachAllEffects();
            m_akPlanes[i].AttachEffect( m_kVolumePreShader[i] );
        }
        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }*/
    
    /** Sets the bounding box color for the given plane.
     * @param i the plane index (0-3) in file coordinates.
     * @param kColor the new color.
     */
    public void SetBoundingBoxColor( int i, ColorRGB kColor )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        for ( int j = 0; j < 4; j++ )
        {
            m_akBoundingBox[iIndex].VBuffer.SetColor3(0, j, kColor );
        }
        m_akBoundingBox[iIndex].VBuffer.Release();
    }
    
    

    /** Sets the positions of the three orthogonal planes. 
     * @param kCenter the positions in file coordinates.
     */
    public void SetCenter( Vector3f kCenter )
    {
    	//System.err.println( "SetCenter" );
    	
        float fX = m_fX * kCenter.X;
        float fY = m_fY * kCenter.Y;
        float fZ = m_fZ * kCenter.Z;

        //System.err.println( kCenter.ToString() + " " + fX + " " + fY + " " + fZ );
        
        float fTCX = kCenter.X;
        float fTCY = kCenter.Y;
        float fTCZ = kCenter.Z;

        //System.err.println( fTCX + " " + fTCY + " " + fTCZ );
        
        m_akPlanes[0].VBuffer.SetPosition3( 0, fX, 0, 0 ) ;
        m_akPlanes[0].VBuffer.SetPosition3( 1, fX, 0, m_fZ ) ;
        m_akPlanes[0].VBuffer.SetPosition3( 2, fX, m_fY, 0 ) ;
        m_akPlanes[0].VBuffer.SetPosition3( 3, fX, m_fY, m_fZ ) ;

        m_akPlanes[0].VBuffer.SetColor3( 0, 0, fTCX, 0, 0 ) ;
        m_akPlanes[0].VBuffer.SetColor3( 0, 1, fTCX, 0, 1.0f ) ;
        m_akPlanes[0].VBuffer.SetColor3( 0, 2, fTCX, 1.0f, 0 ) ;
        m_akPlanes[0].VBuffer.SetColor3( 0, 3, fTCX, 1.0f, 1.0f ) ;

        m_akPlanes[0].VBuffer.SetTCoord3( 0, 0, fTCX, 0, 0 ) ;
        m_akPlanes[0].VBuffer.SetTCoord3( 0, 1, fTCX, 0, 1.0f ) ;
        m_akPlanes[0].VBuffer.SetTCoord3( 0, 2, fTCX, 1.0f, 0 ) ;
        m_akPlanes[0].VBuffer.SetTCoord3( 0, 3, fTCX, 1.0f, 1.0f ) ;

        // Y-Slice:
        m_akPlanes[1].VBuffer.SetPosition3( 0, 0, fY, 0 ) ;
        m_akPlanes[1].VBuffer.SetPosition3( 1, 0, fY, m_fZ ) ;
        m_akPlanes[1].VBuffer.SetPosition3( 2, m_fX, fY, 0 ) ;
        m_akPlanes[1].VBuffer.SetPosition3( 3, m_fX, fY, m_fZ ) ;

        m_akPlanes[1].VBuffer.SetColor3( 0, 0, 0, fTCY, 0 ) ;
        m_akPlanes[1].VBuffer.SetColor3( 0, 1, 0, fTCY, 1.0f ) ;
        m_akPlanes[1].VBuffer.SetColor3( 0, 2, 1.0f, fTCY, 0 ) ;
        m_akPlanes[1].VBuffer.SetColor3( 0, 3, 1.0f, fTCY, 1.0f ) ;

        m_akPlanes[1].VBuffer.SetTCoord3( 0, 0, 0, fTCY, 0 ) ;
        m_akPlanes[1].VBuffer.SetTCoord3( 0, 1, 0, fTCY, 1.0f ) ;
        m_akPlanes[1].VBuffer.SetTCoord3( 0, 2, 1.0f, fTCY, 0 ) ;
        m_akPlanes[1].VBuffer.SetTCoord3( 0, 3, 1.0f, fTCY, 1.0f ) ;

        // Z-Slice:
        m_akPlanes[2].VBuffer.SetPosition3( 0, 0, 0, fZ ) ;
        m_akPlanes[2].VBuffer.SetPosition3( 1, m_fX, 0, fZ ) ;
        m_akPlanes[2].VBuffer.SetPosition3( 2, 0, m_fY, fZ ) ;
        m_akPlanes[2].VBuffer.SetPosition3( 3, m_fX, m_fY, fZ ) ;

        m_akPlanes[2].VBuffer.SetColor3( 0, 0, 0, 0, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetColor3( 0, 1, 1.0f, 0, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetColor3( 0, 2, 0, 1.0f, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetColor3( 0, 3, 1.0f, 1.0f, fTCZ ) ;
            
        m_akPlanes[2].VBuffer.SetTCoord3( 0, 0, 0, 0, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetTCoord3( 0, 1, 1.0f, 0, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetTCoord3( 0, 2, 0, 1.0f, fTCZ ) ;
        m_akPlanes[2].VBuffer.SetTCoord3( 0, 3, 1.0f, 1.0f, fTCZ ) ;
        


        //Bounding Boxes:
        m_akBoundingBox[0].VBuffer.SetPosition3( 0, fX, 0, 0 ) ;
        m_akBoundingBox[0].VBuffer.SetPosition3( 1, fX, 0, m_fZ ) ;
        m_akBoundingBox[0].VBuffer.SetPosition3( 2, fX, m_fY, m_fZ ) ;
        m_akBoundingBox[0].VBuffer.SetPosition3( 3, fX, m_fY, 0 ) ;
        m_akBoundingBox[0].VBuffer.SetTCoord3( 0, 0, fTCX, 0, 0 ) ;
        m_akBoundingBox[0].VBuffer.SetTCoord3( 0, 1, fTCX, 0, 1.0f ) ;
        m_akBoundingBox[0].VBuffer.SetTCoord3( 0, 2, fTCX, 1.0f, 1.0f ) ;
        m_akBoundingBox[0].VBuffer.SetTCoord3( 0, 3, fTCX, 1.0f, 0 ) ;

        m_akBoundingBox[1].VBuffer.SetPosition3( 0, m_fX, fY, m_fZ ) ;
        m_akBoundingBox[1].VBuffer.SetPosition3( 1, 0, fY, m_fZ ) ;
        m_akBoundingBox[1].VBuffer.SetPosition3( 2, 0, fY, 0 ) ;
        m_akBoundingBox[1].VBuffer.SetPosition3( 3, m_fX, fY, 0 ) ;
        m_akBoundingBox[1].VBuffer.SetTCoord3( 0, 0, 1.0f, fTCY, 1.0f ) ;
        m_akBoundingBox[1].VBuffer.SetTCoord3( 0, 1, 0, fTCY, 1.0f ) ;
        m_akBoundingBox[1].VBuffer.SetTCoord3( 0, 2, 0, fTCY, 0 ) ;
        m_akBoundingBox[1].VBuffer.SetTCoord3( 0, 3, 1.0f, fTCY, 0 ) ;

        m_akBoundingBox[2].VBuffer.SetPosition3( 0, m_fX, 0, fZ ) ;
        m_akBoundingBox[2].VBuffer.SetPosition3( 1, 0, 0, fZ ) ;
        m_akBoundingBox[2].VBuffer.SetPosition3( 2, 0, m_fY, fZ ) ;
        m_akBoundingBox[2].VBuffer.SetPosition3( 3, m_fX, m_fY, fZ ) ;
        m_akBoundingBox[2].VBuffer.SetTCoord3( 0, 0, 1.0f, 0, fTCZ ) ;
        m_akBoundingBox[2].VBuffer.SetTCoord3( 0, 1, 0, 0, fTCZ ) ;
        m_akBoundingBox[2].VBuffer.SetTCoord3( 0, 2, 0, 1.0f, fTCZ ) ;
        m_akBoundingBox[2].VBuffer.SetTCoord3( 0, 3, 1.0f, 1.0f, fTCZ ) ;

        for ( int i = 0; i < 3; i++ )
        {
            m_akPlanes[i].VBuffer.Release();
            m_akBoundingBox[i].VBuffer.Release();
        }
    }

    public void setRGBTA(ModelRGB RGBT) {
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlaneEffect[i].setRGBTA(RGBT);
            m_akPlaneEffectTransparent[i].setRGBTA(RGBT);
        }
    }
    public void setRGBTB(ModelRGB RGBT) {
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlaneEffect[i].setRGBTB(RGBT);
            m_akPlaneEffectTransparent[i].setRGBTB(RGBT);
        }
    }
    /** Sets the opacity for the given plane.
     * @param i the plane index (0-3) in file coordinates.
     * @param fAlpha the opacity for the given plane.
     */
    public void SetSliceOpacity( int i, float fAlpha )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        m_abSolid[iIndex] = fAlpha >= 1.0 ? true : false;
        m_akPlaneEffect[iIndex].Blend( fAlpha );
        m_akPlaneEffectTransparent[iIndex].Blend( fAlpha );
        m_kVolumePreShader[iIndex].Blend(fAlpha);
        m_kVolumePreShaderTransparent[iIndex].Blend(fAlpha);
        m_kBoundingBoxShaderSolid[iIndex].Blend(fAlpha);
        m_kBoundingBoxShaderTransparent[iIndex].Blend(fAlpha);
    }
    /** Turns on/off displaying the bounding box for the given plane.
     * @param i the plane index (0-3) in file coordinates.
     * @param bShow when true, the bounding box is displayed.
     */
    public void ShowBoundingBox( int i, boolean bShow )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        if ( m_abShowBoundingBox[iIndex] == bShow )
        {
            return;
        }
        m_abShowBoundingBox[iIndex] = bShow;
    }
    /** Turns on/off displaying the given plane.
     * @param i the plane index (0-3) in file coordinates.
     * @param bShow when true, the plane is displayed.
     */
    public void ShowSlice( int i, boolean bShow )
    {
        int iIndex = MipavCoordinateSystems.fileToModel(i, m_kVolumeImageA.GetImage() );
        if ( m_abShowPlanes[iIndex] == bShow )
        {
            return;
        }
        m_abShowPlanes[iIndex] = bShow;
    }
    /**
     * Turns rendering the planes with the surface mask on/off.
     * @param bOn on/off.
     */
    public void ShowSurface( boolean bOn )
    {
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlaneEffect[i].ShowSurface(bOn);
            m_akPlaneEffectTransparent[i].ShowSurface(bOn);
        }
    }
    
    public void SetTranslate(Vector3f kTranslate)
    {
    	super.SetTranslate(kTranslate);
        for ( int i = 0; i < 3; i++ )
        {
            m_akBoundingBox[i].Local.SetTranslate(m_kTranslate);
            m_akPlanes[i].Local.SetTranslate(m_kTranslate);
        }    	
    }

    /** Creates the bounding frames for the planes. */
    private void CreateBoundingBox ( )
    {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetTChannels(0,3);

        float fX = m_fX * .5f;
        float fY = m_fY * .5f;
        float fZ = m_fZ * .5f;
        
        VertexBuffer[] akOutlineSquare = new VertexBuffer[3];
        for ( int i = 0; i < 3; i++ )
        {
            akOutlineSquare[i] = new VertexBuffer(kAttr, 4 );
            for ( int j = 0; j < 4; j++ )
            {
                akOutlineSquare[i].SetColor3( 0, j, m_akColors[i] );
            }
        }

        akOutlineSquare[0].SetPosition3( 0, fX, 0, 0 ) ;
        akOutlineSquare[0].SetPosition3( 1, fX, 0, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 2, fX, m_fY, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 3, fX, m_fY, 0 ) ;

        akOutlineSquare[1].SetPosition3( 0, m_fX, fY, m_fZ ) ;
        akOutlineSquare[1].SetPosition3( 1, 0, fY, m_fZ ) ;
        akOutlineSquare[1].SetPosition3( 2, 0, fY, 0 ) ;
        akOutlineSquare[1].SetPosition3( 3, m_fX, fY, 0 ) ;

        akOutlineSquare[2].SetPosition3( 0, m_fX, 0, fZ ) ;
        akOutlineSquare[2].SetPosition3( 1, 0, 0, fZ ) ;
        akOutlineSquare[2].SetPosition3( 2, 0, m_fY, fZ ) ;
        akOutlineSquare[2].SetPosition3( 3, m_fX, m_fY, fZ ) ;

        //System.err.println( "Bounding box " + m_fX + " " + m_fY + " " + m_fZ );
        
        m_akBoundingBox = new Polyline[3];
        for ( int i = 0; i < 3; i++ )
        {
            m_akBoundingBox[i] = new Polyline( new VertexBuffer(akOutlineSquare[i]), true, true );
            m_akBoundingBox[i].AttachEffect( m_kBoundingBoxShaderSolid[i] );
            m_akBoundingBox[i].Local.SetTranslate(m_kTranslate);
            m_kScene.AttachChild(m_akBoundingBox[i]);
            m_akBoundingBox[i].VBuffer.SetShared(true);
            m_akBoundingBox[i].IBuffer.SetShared(true);
        }
    }

    /** Creates the scene graph. */
    private void CreatePlanes ( )
    {
        m_kScene = new Node();

        m_kCull = new CullState();
        m_kCull.Enabled = false;
        m_kScene.AttachGlobalState(m_kCull);

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kScene.AttachGlobalState(m_kAlpha);
        
        m_kZBuffer = new ZBufferState();
        m_kZBuffer.Enabled = true;
        m_kScene.AttachGlobalState(m_kZBuffer);

        m_kPolygonOffset = new PolygonOffsetState();
        m_kPolygonOffset.FillEnabled = true;
        m_kPolygonOffset.LineEnabled = true;
        m_kPolygonOffset.PointEnabled = true;
        m_kPolygonOffset.Bias = 1;
        m_kScene.AttachGlobalState(m_kPolygonOffset);
        
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetTChannels(0,3);

        StandardMesh kSM = new StandardMesh(kAttr);
        m_akPlanes = new TriMesh[3];
        for ( int i = 0; i < 3; i++ )
        {
            m_akPlanes[i] = kSM.Rectangle(2,2,1.0f,1.0f);
            m_akPlanes[i].Local.SetTranslate(m_kTranslate);
            m_kScene.AttachChild(m_akPlanes[i]);
            m_akPlanes[i].VBuffer.SetShared(true);
            m_akPlanes[i].IBuffer.SetShared(true);
        }
    }
}
