package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.Camera;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/** Displays the Clipping frames in the VolumeViewer.
 * @see VolumeObject.java
 * @see GPUVolumeRender.java
 */
public class VolumeClip extends VolumeObject
{
    public static final int CLIP_X = 0;
    public static final int CLIP_X_INV = 1;
    public static final int CLIP_Y = 2;
    public static final int CLIP_Y_INV = 3;
    public static final int CLIP_Z = 4;
    public static final int CLIP_Z_INV = 5;
    public static final int CLIP_EYE = 6;
    public static final int CLIP_EYE_INV = 7;
    public static final int CLIP_A = 8;
    public static final int MAX_CLIP_PLANES = 9;
    
    /** ShaderEffect for displaying the clip planes. */
    private VertexColor3Effect m_kVertexColor3Shader;

    /** axis-aligned clip plane polylines: */
    private Polyline[] m_akPolyline;

    /** enables/disables displaying clip planes*/
    private boolean[] m_abDisplayPolyline = new boolean[]{false,false,false,false,false,false,false,false,false};

    /** Screen camera for displaying the eye clip planes in screen-coordinates: */
    private Camera m_spkEyeCamera;

    /** Node for rotating the arbitrary clip plane with the mouse trackball: */
    private Node m_kArbRotate = new Node();

    /** Maximum dimension. */
    private float m_fMax;

    /** Creates a new VolumeClip object.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate translation in the scene-graph for this object.
     * @param fX the size of the volume in the x-dimension (extent * resolutions)
     * @param fY the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeClip ( VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ )
    {
        super(kImageA,kTranslate,fX,fY,fZ);

        m_kVertexColor3Shader = new VertexColor3Effect();
        CreateClipPlanes();
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
    }

    /**
     * Scene-graph node for rotating the arbitrary clipping plane.
     * @return Scene-graph node for rotating the arbitrary clipping plane.
     */
    public Node ArbRotate()
    {
        return m_kArbRotate;
    }


    /**
     * Return true if the arbitrary clip plane is current being displayed.
     * @return true if the arbitrary clip plane is current being displayed.
     */
    public boolean DisplayArb()
    {
        return m_abDisplayPolyline[CLIP_A];
    }

    /** Turns displaying the arbitrary clip plane on/off.
     * @param bDisplay when true display the arbitrary clip plane.
     */
    public void DisplayArb(boolean bDisplay)
    {
        m_abDisplayPolyline[CLIP_A] = bDisplay;
        if ( bDisplay )
        {
            m_kScene.AttachChild(m_kArbRotate);
        }
        else
        {
            m_kScene.DetachChild(m_kArbRotate);
        }
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
    }

    /**
     * Called from JPanelClip. Sets the axis-aligned clip plane display on/off.
     * @param iWhich the clip plane to set.
     * @param bDisplay on/off.
     */
    public void displayClipPlane( int iWhich, boolean bDisplay )
    {
        if ( bDisplay != m_abDisplayPolyline[iWhich] )
        {
            
            m_abDisplayPolyline[iWhich] = bDisplay;
            if ( bDisplay )
            {
                m_kScene.AttachChild(m_akPolyline[iWhich]);
            }
            else
            {
                m_kScene.DetachChild(m_akPolyline[iWhich]);
            }
        }
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        m_bDisplay = false;
        for ( int i = 0; i < m_abDisplayPolyline.length; i++ )
        {
            m_bDisplay |= m_abDisplayPolyline[i];
        }
    }

    /** Turns displaying the eye clip plane on/off.
     * @param bDisplay when true display the eye clip plane.
     */
    public void DisplayEye(boolean bDisplay)
    {
        m_abDisplayPolyline[CLIP_EYE] = bDisplay;
    }

    /** Turns displaying the inverse-eye clip plane on/off.
     * @param bDisplay when true display the inverse-eye clip plane.
     */
    public void DisplayEyeInv(boolean bDisplay)
    {
        m_abDisplayPolyline[CLIP_EYE_INV] = bDisplay;
    }

    /** Delete local memory. */
    public void dispose(Renderer kRenderer)
    {
        if ( m_kVertexColor3Shader != null )
        {
        	kRenderer.ReleaseResources(m_kVertexColor3Shader);
            m_kVertexColor3Shader.dispose();
            m_kVertexColor3Shader = null;
        }
        if ( m_kArbRotate != null )
        {
        	kRenderer.ReleaseAllResources(m_kArbRotate);
            m_kArbRotate.dispose();
            m_kArbRotate = null;
        }
        m_abDisplayPolyline = null;
        if ( m_spkEyeCamera != null )
        {
            m_spkEyeCamera.dispose();
            m_spkEyeCamera = null;
        }
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
        	kRenderer.ReleaseVBuffer(m_akPolyline[i].VBuffer);
        	kRenderer.ReleaseIBuffer(m_akPolyline[i].IBuffer);
            m_akPolyline[i].dispose();
            m_akPolyline[i] = null;
        }
        m_akPolyline = null;
        super.dispose(kRenderer);
    }

    /** Returns the value of the specified axis-aligend clip plane.
     * @param iWhich one of the 6 clip planes
     * @return the value of the specified axis-aligend clip plane.
     */
    public float GetValue(int iWhich)
    {
        float fValue = 0;
        if ( iWhich < 2 )
        {
            fValue = m_akPolyline[iWhich].VBuffer.GetPosition3fX( 0 );
            fValue /= m_fX;
        }
        else if ( iWhich < 4 )
        {
            fValue = m_akPolyline[iWhich].VBuffer.GetPosition3fY( 0 );
            fValue /= m_fY;
        }
        else
        {
            fValue = m_akPolyline[iWhich].VBuffer.GetPosition3fZ( 0 );
            fValue /= m_fZ;
        }
        return fValue;
    }

    /** 
     * Render the object after all other objects have been rendererd. Useful
     * for screen-space objects such as the eye-clip plane.
     * @param kRenderer the OpenGLRenderer object.
     * @param kCuller the Culler object.
     */
    public void PostRender( Renderer kRenderer, Culler kCuller )
    {
        if ( m_abDisplayPolyline[CLIP_EYE] || m_abDisplayPolyline[CLIP_EYE_INV] )
        {
            Camera kCamera = kRenderer.GetCamera();

            m_spkEyeCamera.SetLocation(kCamera.GetLocation());
            kRenderer.SetCamera(m_spkEyeCamera);
            if ( m_abDisplayPolyline[CLIP_EYE] )
            {
                kRenderer.Draw(m_akPolyline[CLIP_EYE]);
            }
            if ( m_abDisplayPolyline[CLIP_EYE_INV] )
            {
                kRenderer.Draw(m_akPolyline[CLIP_EYE_INV]);
            }
            kRenderer.SetCamera(kCamera);
        }
    }

    /**
     * Render the object.
     * @param kRenderer the OpenGLRenderer object.
     * @param kCuller the Culler object.
     */
    public void Render( Renderer kRenderer, Culler kCuller, boolean bPreRender, boolean bSolid )
    {
        if ( !m_bDisplay  && !m_abDisplayPolyline[CLIP_A] || !bSolid)
        {
            return;
        }
        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }
    /**
     * Sets the arbitrary clip plane color.
     * @param kColor the new color.
     */
    public void setArbColor( ColorRGB kColor )
    {
        for ( int i = 0; i < 4; i++ )
        {
            m_akPolyline[CLIP_A].VBuffer.SetColor3( 0, i, kColor );
        }
        m_akPolyline[CLIP_A].VBuffer.Release();
    }
    
    /**
     * Set the position of the arbitrary clipping plane, before rotation.
     * @param fX the position of the arbitrary clipping plane, before rotation.
     */
    public void SetArbPlane( float fX )
    {
        m_akPolyline[CLIP_A].VBuffer.SetPosition3( 0, fX, 0, 0 ) ;
        m_akPolyline[CLIP_A].VBuffer.SetPosition3( 1, fX, 0, m_fMax ) ;
        m_akPolyline[CLIP_A].VBuffer.SetPosition3( 2, fX, m_fMax, m_fMax ) ;
        m_akPolyline[CLIP_A].VBuffer.SetPosition3( 3, fX, m_fMax, 0 ) ;
        m_akPolyline[CLIP_A].VBuffer.Release();
        m_kScene.UpdateGS();
    }
    /**
     * Sets the axis-aligned clip plane clipping position.
     * @param iWhich one of the 6 clip planes
     * @param fValue the clipping position.
     */
    public void setClipPlane( int iWhich, float fValue )
    {
        for ( int i = 0; i < 4; i++ )
        {
            if ( iWhich < 2 )
            {
                m_akPolyline[iWhich].VBuffer.SetPosition3(i, 
                                                          fValue*m_fX,
                                                          m_akPolyline[iWhich].VBuffer.GetPosition3fY(i),     
                                                          m_akPolyline[iWhich].VBuffer.GetPosition3fZ(i));
            }
            else if ( iWhich < 4 )
            {
                m_akPolyline[iWhich].VBuffer.SetPosition3(i, 
                                                          m_akPolyline[iWhich].VBuffer.GetPosition3fX(i),     
                                                          fValue*m_fY,
                                                          m_akPolyline[iWhich].VBuffer.GetPosition3fZ(i));
            }
            else
            {
                m_akPolyline[iWhich].VBuffer.SetPosition3(i, 
                                                          m_akPolyline[iWhich].VBuffer.GetPosition3fX(i),     
                                                          m_akPolyline[iWhich].VBuffer.GetPosition3fY(i),
                                                          fValue*m_fZ
                                                          );
            }
        }
        m_akPolyline[iWhich].VBuffer.Release();
        
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();

    }

    /**
     * Sets the axis-aligned clip plane color.
     * @param iWhich one of the 6 clip planes
     * @param kColor the new color.
     */
    public void setClipPlaneColor( int iWhich, ColorRGB kColor )
    {
        for ( int i = 0; i < 4; i++ )
        {
            m_akPolyline[iWhich].VBuffer.SetColor3( 0, i, kColor );
        }
        m_akPolyline[iWhich].VBuffer.Release();
    }
    /**
     * Sets the eye clip plane position.
     * @param f4 clip position (same value as sSlice in JPanelClip)
     * @param bDisplay on/off.
     */
    public void setEyeClipPlane( float fZ )
    {
        m_akPolyline[CLIP_EYE].VBuffer.SetPosition3( 0, 0f, 0f, fZ ) ;
        m_akPolyline[CLIP_EYE].VBuffer.SetPosition3( 1, m_fX, 0f, fZ ) ;
        m_akPolyline[CLIP_EYE].VBuffer.SetPosition3( 2, m_fX, m_fY, fZ ) ;
        m_akPolyline[CLIP_EYE].VBuffer.SetPosition3( 3, 0f, m_fY, fZ ) ;
        m_akPolyline[CLIP_EYE].VBuffer.Release();

        m_akPolyline[CLIP_EYE].UpdateGS();
        m_akPolyline[CLIP_EYE].UpdateRS();
    }

    /**
     * Sets the eye clip plane position.
     * @param f4 clip position (same value as sSlice in JPanelClip)
     * @param bDisplay on/off.
     */
    public void setEyeInvClipPlane( float fZ )
    {
        m_akPolyline[CLIP_EYE_INV].VBuffer.SetPosition3( 0, 0f, 0f, fZ ) ;
        m_akPolyline[CLIP_EYE_INV].VBuffer.SetPosition3( 1, m_fX, 0f, fZ ) ;
        m_akPolyline[CLIP_EYE_INV].VBuffer.SetPosition3( 2, m_fX, m_fY, fZ ) ;
        m_akPolyline[CLIP_EYE_INV].VBuffer.SetPosition3( 3, 0f, m_fY, fZ ) ;
        m_akPolyline[CLIP_EYE_INV].VBuffer.Release();

        m_akPolyline[CLIP_EYE_INV].UpdateGS();
        m_akPolyline[CLIP_EYE_INV].UpdateRS();
    }
    
    /** Creates the clipping planes. */
    private void CreateClipPlanes()
    {
        m_kScene = new Node();

        m_kCull = new CullState();
        m_kCull.Enabled = false;
        m_kScene.AttachGlobalState(m_kCull);

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kScene.AttachGlobalState(m_kAlpha);


        m_akPolyline = new Polyline[9];
        IndexBuffer kIndexBuffer = new IndexBuffer(6);
        int[] aiIndexData = kIndexBuffer.GetData();
        aiIndexData[0] = 0;
        aiIndexData[1] = 1;
        aiIndexData[2] = 2;
        aiIndexData[3] = 0;
        aiIndexData[4] = 2;
        aiIndexData[5] = 3;

        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetCChannels(0,3);

        VertexBuffer[] akOutlineSquare = new VertexBuffer[6];
        for ( int i = 0; i < 6; i++ )
        {
            akOutlineSquare[i] = new VertexBuffer(kAttributes, 4 );
            for ( int j = 0; j < 4; j++ )
            {
                akOutlineSquare[i].SetColor3( 0, j, 1, 0, 0 ) ;
            }
        }
        // neg x clipping:
        akOutlineSquare[0].SetPosition3( 0, 0, 0, 0 ) ;
        akOutlineSquare[0].SetPosition3( 1, 0, 0, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 2, 0, m_fY, m_fZ ) ;
        akOutlineSquare[0].SetPosition3( 3, 0, m_fY, 0 ) ;

        // pos x clipping:
        akOutlineSquare[1].SetPosition3( 0, m_fX, 0, m_fZ ) ;
        akOutlineSquare[1].SetPosition3( 1, m_fX, 0, 0 ) ;
        akOutlineSquare[1].SetPosition3( 2, m_fX, m_fY, 0 ) ;
        akOutlineSquare[1].SetPosition3( 3, m_fX, m_fY, m_fZ ) ;

        // neg y clipping:
        akOutlineSquare[2].SetPosition3( 0, m_fX, 0, m_fZ ) ;
        akOutlineSquare[2].SetPosition3( 1, 0, 0, m_fZ ) ;
        akOutlineSquare[2].SetPosition3( 2, 0, 0, 0 ) ;
        akOutlineSquare[2].SetPosition3( 3, m_fX, 0, 0 ) ;
        // pos y clipping:
        akOutlineSquare[3].SetPosition3( 0, m_fX, m_fY, 0 ) ;
        akOutlineSquare[3].SetPosition3( 1, 0, m_fY, 0 ) ;
        akOutlineSquare[3].SetPosition3( 2, 0, m_fY, m_fZ ) ;
        akOutlineSquare[3].SetPosition3( 3, m_fX, m_fY, m_fZ ) ;

        // neg z clipping:
        akOutlineSquare[4].SetPosition3( 0, m_fX, 0, 0 ) ;
        akOutlineSquare[4].SetPosition3( 1, 0, 0, 0 ) ;
        akOutlineSquare[4].SetPosition3( 2, 0, m_fY, 0 ) ;
        akOutlineSquare[4].SetPosition3( 3, m_fX, m_fY, 0 ) ;

        // pos z clipping:
        akOutlineSquare[5].SetPosition3( 0, 0, 0, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 1, m_fX, 0, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 2, m_fX, m_fY, m_fZ ) ;
        akOutlineSquare[5].SetPosition3( 3, 0, m_fY, m_fZ ) ;

        for ( int i = 0; i < 6; i++ )
        {
            m_akPolyline[i] = new Polyline( new VertexBuffer(akOutlineSquare[i]), true, true );
            m_akPolyline[i].AttachEffect( m_kVertexColor3Shader );
            m_akPolyline[i].Local.SetTranslate(m_kTranslate);
        }

        m_fMax = Math.max( m_fX, Math.max( m_fY, m_fZ ) );

        VertexBuffer kOutlineSquare = new VertexBuffer( kAttributes, 4);
        // arbitrary clipping:
        for ( int i = 0; i < 4; i++ )
        {
            kOutlineSquare.SetColor3( 0, i, 1, 0, 0 ) ;
        }
        kOutlineSquare.SetPosition3( 0, 0f, 0, 0 ) ;
        kOutlineSquare.SetPosition3( 1, 0f, 0, m_fMax ) ;
        kOutlineSquare.SetPosition3( 2, 0f, m_fMax, m_fMax ) ;
        kOutlineSquare.SetPosition3( 3, 0f, m_fMax, 0 ) ;
        m_akPolyline[CLIP_A] = new Polyline( new VertexBuffer(kOutlineSquare), true, true );
        m_akPolyline[CLIP_A].AttachEffect( m_kVertexColor3Shader );
        m_akPolyline[CLIP_A].Local.SetTranslate(m_kTranslate);
        m_kArbRotate.AttachChild( m_akPolyline[CLIP_A] );

        // eye clipping:
        // set up camera
        m_spkEyeCamera = new Camera();
        m_spkEyeCamera.SetFrustum(-0.55f,0.55f,-0.4125f,0.4125f,1.0f,1000.0f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCUp = new Vector3f(0.0f,-1.0f,0.0f);
        Vector3f kCRight = new Vector3f();
        kCRight.Cross( kCDir, kCUp );
        Vector3f kCLoc = new Vector3f(kCDir);
        kCLoc.Scale(-4.0f);
        m_spkEyeCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        for ( int i = 0; i < 4; i++ )
        {
            kOutlineSquare.SetColor3( 0, i, 1, 0, 0 ) ;
        }
        kOutlineSquare.SetPosition3( 0, 0f, 0f, 0 ) ;
        kOutlineSquare.SetPosition3( 1, m_fX, 0f, 0 ) ;
        kOutlineSquare.SetPosition3( 2, m_fX, m_fY, 0 ) ;
        kOutlineSquare.SetPosition3( 3, 0f, m_fY, 0 ) ;
        m_akPolyline[CLIP_EYE] = new Polyline( new VertexBuffer(kOutlineSquare), true, true );
        m_akPolyline[CLIP_EYE].Local.SetTranslate(m_kTranslate);
        m_akPolyline[CLIP_EYE].AttachEffect( m_kVertexColor3Shader );
        m_akPolyline[CLIP_EYE].UpdateGS();
        m_akPolyline[CLIP_EYE].UpdateRS();

        for ( int i = 0; i < 4; i++ )
        {
            kOutlineSquare.SetColor3( 0, i, 1, 0, 0 ) ;
        }
        kOutlineSquare.SetPosition3( 0, 0f, 0f, m_fZ ) ;
        kOutlineSquare.SetPosition3( 1, m_fX, 0f, m_fZ ) ;
        kOutlineSquare.SetPosition3( 2, m_fX, m_fY, m_fZ ) ;
        kOutlineSquare.SetPosition3( 3, 0f, m_fY, m_fZ ) ;
        m_akPolyline[CLIP_EYE_INV] = new Polyline( new VertexBuffer(kOutlineSquare), true, true );
        m_akPolyline[CLIP_EYE_INV].Local.SetTranslate(m_kTranslate);
        m_akPolyline[CLIP_EYE_INV].AttachEffect( m_kVertexColor3Shader );
        m_akPolyline[CLIP_EYE_INV].UpdateGS();
        m_akPolyline[CLIP_EYE_INV].UpdateRS();
    }

}
