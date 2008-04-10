package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.util.*;
import java.nio.*;
import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.model.structures.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibFoundation.System.*;
import WildMagic.LibGraphics.Collision.*;
import WildMagic.LibGraphics.Detail.*;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.SceneGraph.Spatial.CullingMode;

/**
 * Displays the three orthogonal planes with the volume data.
 * @see GPUVolumeRender.java
 * @see VolumePlaneEffect.java
 * @see VolumeObject.java
 */
public class VolumeSurface extends VolumeObject
{
    /** Create a new VolumeObject with the VolumeImage parameter.
     * @param kImageA, the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate, translation in the scene-graph for this object.
     * @param fX, the size of the volume in the x-dimension (extent * resolutions)
     * @param fY, the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ, the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeSurface ( Renderer kRenderer, VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ, TriMesh kMesh )
    {
        this(kRenderer,kImageA, kTranslate, fX, fY, fZ, kMesh, false );
    }
    
    public VolumeSurface ( Renderer kRenderer, VolumeImage kImageA, Vector3f kTranslate, float fX, float fY, float fZ, TriMesh kMesh, boolean bReplace )
    {
        super(kImageA,kTranslate,fX,fY,fZ);

        CreateScene();
        if ( bReplace )
        {
            StandardMesh kSM = new StandardMesh( kMesh.VBuffer.GetAttributes() );
            TriMesh kLocal = kSM.Sphere( 10, 10, .5f );
            m_kMesh = new TriMesh(kLocal);
            //m_kMesh = new HierarchicalTriMesh(kLocal);
        }
        else
        {
            //m_kMesh = new HierarchicalTriMesh(kMesh);
            m_kMesh = kMesh;
        }
        m_kMesh.SetName( new String( kMesh.GetName() ) );
        
        boolean bHasMaterial = true;
        m_kMaterial = (MaterialState)m_kMesh.GetGlobalState( GlobalState.StateType.MATERIAL );
        if ( m_kMaterial == null )
        {
            bHasMaterial = false;
            m_kMaterial = new MaterialState();
            m_kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
            m_kMaterial.Ambient = new ColorRGB(0.2f,0.2f,0.2f);
            m_kMaterial.Diffuse = new ColorRGB(ColorRGB.WHITE);
            m_kMaterial.Specular = new ColorRGB(ColorRGB.WHITE);
            m_kMaterial.Shininess = 32f;
        }
        m_akBackupColor = new ColorRGBA[m_kMesh.VBuffer.GetVertexQuantity()];

        for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            if ( m_kMesh.VBuffer.GetAttributes().HasTCoord(0) )
            {
                m_kMesh.VBuffer.SetTCoord3( 0, i, 
                        (m_kMesh.VBuffer.GetPosition3fX(i)  - kTranslate.X()) * 1.0f/m_fX,
                        (m_kMesh.VBuffer.GetPosition3fY(i)  - kTranslate.Y()) * 1.0f/m_fY,
                        (m_kMesh.VBuffer.GetPosition3fZ(i)  - kTranslate.Z()) * 1.0f/m_fZ);
            }
            m_akBackupColor[i] = new ColorRGBA();
            m_kMesh.VBuffer.GetColor4( 0, i, m_akBackupColor[i]);
        }
        m_kMesh.UpdateMS();

        m_kLightShader = new SurfaceLightingEffect( kImageA );

        if ( !bHasMaterial )
        {
            m_kMesh.AttachGlobalState(m_kMaterial);
        }
        m_kMesh.AttachEffect(m_kLightShader);
        m_kMesh.UpdateRS();
        
        m_kScene.AttachChild(m_kMesh);
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        
//         Date kDate = new Date();
//         long lStart = kDate.getTime();
//         System.err.println("Start build tree " );
//         BoxBVTree kBoxBV = new BoxBVTree(m_kMesh, 4000, true);
//         m_kMesh.SetBoundingVolumeTree(kBoxBV);
//         kDate = new Date();
//         long lEnd = kDate.getTime();
//         System.err.println("End build tree " + (float)(lEnd - lStart)/1000.0f);
    }

    /**
     * PreRender the object, for embedding in the ray-cast volume.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
    public void PreRender( Renderer kRenderer, Culler kCuller )
    {
        if ( !m_bDisplay )
        {
            return;
        }    
        for ( int i = 0; i < m_kScene.GetQuantity(); i++ )
        {
            m_kScene.GetChild(i).DetachAllEffects();
            m_kScene.GetChild(i).AttachEffect( m_kVertexColor3Shader );
        }

        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }

    /**
     * Render the object.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
    public void Render( Renderer kRenderer, Culler kCuller )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        for ( int i = 0; i < m_kScene.GetQuantity(); i++ )
        {
            m_kScene.GetChild(i).DetachAllEffects();
            m_kScene.GetChild(i).AttachEffect( m_kLightShader );
        }
        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }
    
    public String GetName()
    {
        return m_kMesh.GetName();
    }

    public void SetColor( ColorRGB kColor )
    {
        for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMesh.VBuffer.SetColor3( 0, i, kColor );
        }
        m_kMesh.VBuffer.Release();
        m_kMaterial.Diffuse.SetData(kColor);
        m_kMaterial.Ambient.SetData( new ColorRGB(ColorRGB.BLACK) );
        m_kMaterial.Specular.SetData( new ColorRGB(0.5f,0.5f,0.5f) );
        m_kMaterial.Emissive.SetData( new ColorRGB(ColorRGB.BLACK) );
        m_kMaterial.Shininess = 5f;
        
    }
    
    public void Blend( float fValue )
    {
        m_kLightShader.Blend(fValue);
    }
    
    
    public void SetSurfaceTexture( boolean bOn, boolean bUseNewImage, boolean bUseNewLUT )
    {
        m_kLightShader.SetSurfaceTexture(bOn, bUseNewImage, bUseNewLUT);
        m_bTextureOn = bOn;
    }
    
        /**
     * Sets the light for the EllipsoidsShader.
     * @param kLightType, the name of the light to set (Light0, Light1, etc.)
     * @param afType, the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        if ( m_kLightShader != null )
        {
            m_kLightShader.SetLight(kLightType, afType);
        }
    }

    public void SetPerPixelLighting( Renderer kRenderer, boolean bOn )
    {
        if ( m_kLightShader != null )
        {
            m_kLightShader.SetPerPixelLighting(kRenderer, bOn);
        }
    }

    public void SetClipping( boolean bClip )
    {
        m_kLightShader.SetClipping(bClip);
    } 
    
    /** Initializes axis-aligned clipping for the VolumeShaderEffect.
     * @param afClip, the initial clipping parameters for axis-aligned clipping.
     */
    public void InitClip( float[] afClip )
    {
        m_kLightShader.InitClip(afClip);
    }

    /** Sets axis-aligned clipping for the VolumeShaderEffect.
     * @param afClip, the clipping parameters for axis-aligned clipping.
     */
    public void SetClip( int iWhich, float[] data)
    {
        m_kLightShader.SetClip(iWhich, data);
    }

    /** Sets eye clipping for the VolumeShaderEffect.
     * @param afEquation, the eye clipping equation.
     */
    public void SetClipEye( float[] afEquation )
    {
        m_kLightShader.SetClipEye(afEquation);
    }

    /** Sets inverse-eye clipping for the VolumeShaderEffect.
     * @param afEquation, the inverse-eye clipping equation.
     */
    public void SetClipEyeInv( float[] afEquation )
    {
        m_kLightShader.SetClipEyeInv(afEquation);
    }

    /** Sets arbitrary clipping for the VolumeShaderEffect.
     * @param afEquation, the arbitrary-clip plane equation.
     */
    public void SetClipArb( float[] afEquation )
    {
        m_kLightShader.SetClipArb(afEquation);
    }

    public MaterialState GetMaterial()
    {
        return m_kMaterial;
    }     
    
    public void SetMaterial( MaterialState kMaterial)
    {
        for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMesh.VBuffer.SetColor3( 0, i, kMaterial.Diffuse );
        }
        m_kMesh.VBuffer.Release();
        m_kMaterial.Ambient.SetData( kMaterial.Ambient );
        m_kMaterial.Diffuse.SetData( kMaterial.Diffuse );
        m_kMaterial.Specular.SetData( kMaterial.Specular );
        m_kMaterial.Emissive.SetData( kMaterial.Emissive );
        m_kMaterial.Shininess = kMaterial.Shininess;
        m_kScene.UpdateGS();
    }

    public void Paint( Renderer kRenderer, PickRecord kRecord, ColorRGBA kPaintColor, int iBrushSize )
    {
        m_bPainted = true;
        m_kMesh.VBuffer.SetColor4(0, kRecord.iV0, kPaintColor.R(), kPaintColor.G(), kPaintColor.B(), kPaintColor.A() );
        m_kMesh.VBuffer.SetColor4(0, kRecord.iV1, kPaintColor.R(), kPaintColor.G(), kPaintColor.B(), kPaintColor.A() );
        m_kMesh.VBuffer.SetColor4(0, kRecord.iV2, kPaintColor.R(), kPaintColor.G(), kPaintColor.B(), kPaintColor.A() );


        Attributes kIAttr = kRenderer.GetVBufferInputAttributes( m_kMesh.VBuffer );
        if ( kIAttr == null )
        {
            kIAttr = m_kMesh.VBuffer.GetAttributes();
            System.err.println("Attributes NULL");
        }
        if ( iBrushSize > 1 )
        {
            Vector3f kDiff = new Vector3f();
            Vector3f kPos1 = new Vector3f();
            Vector3f kPos2 = new Vector3f();
            m_kMesh.VBuffer.GetPosition3(kRecord.iV0, kPos1 );
            int iMin = -1;
            int iMax = -1;
            for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
            {
                m_kMesh.VBuffer.GetPosition3(i, kPos2 );
                kPos1.sub(kPos2, kDiff );
                if ( kDiff.Length() < ((float)iBrushSize/500.0f) )
                {
                    m_kMesh.VBuffer.SetColor4(0, i, kPaintColor.R(), kPaintColor.G(), kPaintColor.B(), kPaintColor.A() );
                    if ( iMin == -1 )
                    {
                        iMin = i;
                    }
                    if ( i > iMax )
                    {
                        iMax = i;
                    }
                }
            }
            
            float[] afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(kIAttr,
                                                                           iMin, iMax );
            FloatBuffer kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, iMin*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );
        }
        else
        {
            float[] afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(kIAttr, kRecord.iV0,
                                                                           kRecord.iV0);
            FloatBuffer kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, kRecord.iV0*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );

            afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(kIAttr, kRecord.iV1,
                                                                   kRecord.iV1);
            kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, kRecord.iV1*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );

            afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(kIAttr, kRecord.iV2,
                                                                   kRecord.iV2);
            kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, kRecord.iV2*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );
        }
    }


    public void Dropper(PickRecord kRecord, ColorRGBA rkDropperColor, Vector3f rkPickPoint )
    {
        m_kMesh.VBuffer.GetPosition3(kRecord.iV0, rkPickPoint );
        rkPickPoint.multEquals( new Vector3f( 1.0f/m_fX, 1.0f/m_fY, 1.0f/m_fZ ));
        int[] iExtents = m_kVolumeImageA.GetImage().getExtents();
        rkPickPoint.multEquals( new Vector3f( iExtents[0], iExtents[1], iExtents[2] ));
        
        m_kMesh.VBuffer.GetColor4(0, kRecord.iV0, rkDropperColor );
        if ( m_bTextureOn )
        {
            Vector3f kTexCoord = new Vector3f();
            m_kMesh.VBuffer.GetTCoord3( 0, kRecord.iV0, kTexCoord );
            m_kLightShader.Dropper( kTexCoord, rkDropperColor);
        }
    }
    
    public void Erase( Renderer kRenderer, PickRecord kRecord, int iBrushSize )
    {
        if ( !m_bPainted )
        {
            return;
        }
        m_kMesh.VBuffer.SetColor4(0, kRecord.iV0, m_akBackupColor[kRecord.iV0]);
        m_kMesh.VBuffer.SetColor4(0, kRecord.iV1, m_akBackupColor[kRecord.iV1]);
        m_kMesh.VBuffer.SetColor4(0, kRecord.iV2, m_akBackupColor[kRecord.iV2]);

        if ( iBrushSize > 1 )
        {
            Vector3f kDiff = new Vector3f();
            Vector3f kPos1 = new Vector3f();
            Vector3f kPos2 = new Vector3f();
            m_kMesh.VBuffer.GetPosition3(kRecord.iV0, kPos1 );
            int iMin = -1;
            int iMax = -1;
            for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
            {
                m_kMesh.VBuffer.GetPosition3(i, kPos2 );
                kPos1.sub(kPos2, kDiff );
                if ( kDiff.Length() < ((float)iBrushSize/500.0f) )
                {
                    m_kMesh.VBuffer.SetColor4(0, i, m_akBackupColor[i] );
                    if ( iMin == -1 )
                    {
                        iMin = i;
                    }
                    if ( i > iMax )
                    {
                        iMax = i;
                    }
                }
            }
            
            float[] afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(m_kMesh.VBuffer.GetAttributes(),
                                                                           iMin, iMax );
            FloatBuffer kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, iMin*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );
        }
        else
        {
            float[] afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(m_kMesh.VBuffer.GetAttributes(), kRecord.iV0,
                                                                           kRecord.iV0);
            FloatBuffer kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, kRecord.iV0*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );


            afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(m_kMesh.VBuffer.GetAttributes(), kRecord.iV1,
                                                                   kRecord.iV1);
            kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, kRecord.iV1*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );


            afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(m_kMesh.VBuffer.GetAttributes(), kRecord.iV2,
                                                                   kRecord.iV2);
            kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, kRecord.iV2*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );
        }
    }


    public void EraseAllPaint( Renderer kRenderer )
    {
        if ( !m_bPainted )
        {
            return;
        }
        m_bPainted = false;
        int iMin = 0, iMax = m_kMesh.VBuffer.GetVertexQuantity() -1;
        for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMesh.VBuffer.SetColor4(0, i, m_akBackupColor[i]);
        }
        float[] afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(m_kMesh.VBuffer.GetAttributes(),
                iMin, iMax );
        FloatBuffer kData = FloatBuffer.wrap(afCompatible);
        kData.rewind();
        kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, iMin*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );
    }
    
    
    public void SetLUTNew( ModelLUT kLUT, ModelRGB kRGBT )
    {
        m_kLightShader.SetLUTNew(kLUT, kRGBT);
    }    
    
    public void SetImageNew( ModelImage kImage )
    {
        m_kLightShader.SetImageNew(kImage);
    }

    public TriMesh GetMesh()
    {
        return m_kMesh; 
    }
    
    
    public void AddGeodesic( Geometry kMesh, int iGroup )
    {
        //kMesh.Local.SetTranslate(m_kTranslate.add(kMesh.Local.GetTranslate()));
        kMesh.AttachEffect( m_kLightShader );
        if ( m_akGeodesicNodes[iGroup] == null )
        {
            m_akGeodesicNodes[iGroup] = new Node();
            if ( iGroup == m_iCurrentGeodesic )
            {
                m_kScene.AttachChild( m_akGeodesicNodes[iGroup] );
            }
        }
        m_akGeodesicNodes[iGroup].AttachChild( kMesh );
    }  
    
    
    
    public void RemoveGeodesic( int iNode, int iGroup )
    {
        if ( iGroup == -1 )
        {
            for ( int i = 0; i < 3; i++ )
            {
                if ( m_akGeodesicNodes[i] != null )
                {
                    m_akGeodesicNodes[i].DetachChildAt(iNode);
                }
            }    
        }
        else
        {
            if ( m_akGeodesicNodes[iGroup] != null )
            {
                m_akGeodesicNodes[iGroup].DetachChildAt(iNode);
            }
        }
    } 
    
    public void RemoveAllGeodesic()
    {
        for ( int i = 0; i < 3; i++ )
        {
            if ( m_akGeodesicNodes[i] != null )
            {
                m_akGeodesicNodes[i].DetachAllChildren();
                m_kScene.DetachChild( m_akGeodesicNodes[m_iCurrentGeodesic] );
                m_akGeodesicNodes[i] = null;
            }
        }    
    }
    
    public void ReplaceGeodesic( TriMesh kMesh )
    {       
        //kMesh.Local.SetTranslate(m_kTranslate);
        kMesh.UpdateMS();

        kMesh.AttachGlobalState(m_kMaterial);
        kMesh.AttachEffect(m_kLightShader);
        kMesh.UpdateRS();
        
        m_kScene.SetChild(0, kMesh);
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        
        m_kMesh = null;
        m_kMesh = kMesh;
    }
    
    public void ToggleGeodesicPathDisplay( int iWhich )
    {
        if ( m_iCurrentGeodesic == iWhich )
        {
            return;
        }
        if ( m_akGeodesicNodes[m_iCurrentGeodesic] == null )
        {
            return;
        }
        
        m_kScene.DetachChild( m_akGeodesicNodes[m_iCurrentGeodesic] );
        m_iCurrentGeodesic = iWhich;
        m_kScene.AttachChild( m_akGeodesicNodes[m_iCurrentGeodesic] );
    }
    
    public float GetVolume()
    {
        if ( m_fVolume == 0 )
        {
            m_fVolume = ComputeVolume();
        }
        return m_fVolume;
    }

    public float GetSurfaceArea()
    {
        if ( m_fSurfaceArea == 0 )
        {
            m_fSurfaceArea = ComputeSurfaceArea();
        }
        return m_fSurfaceArea;
    }
    

    public Vector3f GetCenter()
    {
        if ( m_kCenter == null )
        {
            ComputeCenter();
        }
        return m_kCenter;
    }

    private void ComputeCenter()
    {
        m_kCenter = new Vector3f();
        Vector3f kPos = new Vector3f();
        for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMesh.VBuffer.GetPosition3( i, kPos );
            m_kCenter.addEquals( kPos );
        }
        m_kCenter.scaleEquals( 1.0f / (float)m_kMesh.VBuffer.GetVertexQuantity() );
    }
    
    
    /**
     * Calculates volume of triangle mesh. The mesh consists of triangle faces and encloses a bounded region. Face j, 0
     * <= j <= n-1 has verticies P0, P1, and P2. The order of the verticies is counterclockwise as you view the face
     * from outside the bounded region. The mesh is closed and manifold in the sense that each edge is shared by exactly
     * two triangles. The volume of the bounded region is:<br>
     *
     * <pre>
             V = 1/6 (Sum from j=0 to n-1 of {P0 dot P1 cross P2})
     *   </pre>
     *
     * The terms of the summation can be positive, negative, or zero. The term is positive if the face is
     * counterclockwise when viewed from the zero vector, or zero if the face appears to be a line segment when viewed
     * from the zero vector. NOTICE THAT THERE ARE 2 DIFFERENT DEFINITIONS OF COUNTERCLOCKWISE, COUNTERCLOCKWISE AS
     * VIEWED FROM OUTSIDE THE BOUNDED REGION AND COUNTERCLOCKWISE AS VIEWED FROM THE ZERO VECTOR.
     *
     * <p>A 3D image on a rectangular lattice contains points (i0, i1, i2) where 0 <= ik < Bk for specified dimension
     * bounds Bk. These are just indicies. The actual physical measurements are provided by scaling factors Dk > 0. For
     * example, a 256x256x256 MRI has B0 = B1 = B2 = 256. If each voxel is 1 mm in x, 1 mm in y, and 5 mm in z, then D0
     * = D1 = 1 and D2 = 5. The 3D image encloses a rectangular region [0,C0] x [0,C1] x [0,C2] in <em>physical
     * space</em> where Ck = Dk*Bk. In the example, C0 = D0*B0 = 256 mm in x, C1 = D1*B1 = 256 mm in y, and C2 = D2*B2 =
     * 1280 mm in z. Volume calculations are required to use physical measurements. In the example, volume will be in
     * cubic millimeters.</p>
     *
     * <p>The surface extraction is performed by mapping [0,C0] x [0,C1] x [0,C2] into [-1,1] x [-1,1] x [-1,1] using
     * uniform scaling. This is done to keep the floating point values within order 1 to avoid the floating point errors
     * that occur if you were to use the index values themselves. The topology of a level surface is invariant under any
     * scaling (not just uniform), but the continuous level of detail algorithm for triangle decimation does edge
     * collapses based on various geometric measurements of the mesh representing the level surface. The geometric
     * measurements are not invariant under nonuniform scaling. Map the image into a cube using uniform scaling so that
     * the triangle collapse order is invariant. The uniform scaling is done so that the largest image dimension [0,M]
     * is mapped to [-1,1]. The other ranges are mapped to intervals of the form [-L,L) where L < 1. If (i0,i1,i2) is in
     * [0,B0) x [0,B1) x [0,B2), the corresponding (x0,x1,x2) in [-1,1) is:<br>
     * </p>
     *
     * <pre>
                     2*Dk*ik - Ck
             xk =    ------------
                     max{C0,C1,C2}
     *   </pre>
     *
     * <p>However, we want to map from [0,Bk) to an inclusive interval [-Rk,Rk], where 0 < Rk < 1 and Rk = 1 -
     * Dk/max{C0,C1,C2}. This ensures that surfaces begin in the center of a voxel rather than at the (0,0,0) corner of
     * the voxel. The problem is easiest to see in the Z direction: a surface that should cover the full Z range will
     * end before the last slice. Therefore, the formula should be:<br>
     * </p>
     *
     * <pre>
                     2*Dk*ik - Ck + Dk
             xk =    -----------------
                       max{C0,C1,C2}
     *   </pre>
     *
     * <p>Once a closed manifold triangle mesh is extracted, the problem is now to compute its volume in physical space.
     * Note that ik are indicies in the original image, but the true physical length that is measured (relative to other
     * index locations) is yk = Dk*ik. Any triangle mesh (x0,x1,x2) must be mapped to (y0,y1,y2) before the volume
     * calculation. The mapping is:<br>
     * </p>
     *
     * <pre>
                     max{C0,C1,C2}*xk + Ck - Dk
             yk =    --------------------------
                                  2
     *   </pre>
     *
     * <p>The volume calculations use the previously mentioned formula where the P points are the (y0,y1,y2) values.</p>
     *
     * @return  The volume of the surface.
     */
    public float ComputeVolume()
    {

        float fSum = 0.0f;
        Vector3f kScale = new Vector3f( 1.0f/m_fX, 1.0f/m_fY, 1.0f/m_fZ );
        
        int iIndexQuantity = m_kMesh.IBuffer.GetIndexQuantity();
        int[] aiConnect = m_kMesh.IBuffer.GetData();
        Vector3f kPos0 = new Vector3f();
        Vector3f kPos1 = new Vector3f();
        Vector3f kPos2 = new Vector3f();
        for (int iT = 0; iT < iIndexQuantity; /**/) {

            // get indices to triangle vertices
            int iV0 = aiConnect[iT++];
            int iV1 = aiConnect[iT++];
            int iV2 = aiConnect[iT++];

            // get vertices
            m_kMesh.VBuffer.GetPosition3(iV0, kPos0);
            m_kMesh.VBuffer.GetPosition3(iV1, kPos1);
            m_kMesh.VBuffer.GetPosition3(iV2, kPos2);

            kPos0.multEquals( kScale );
            kPos1.multEquals( kScale );
            kPos2.multEquals( kScale );

            // Since the differences between the points is in pixels*resolutions,
            // there is no need to map into physical space.

            // compute triple scalar product
            // The scalar triple product of three vectors A, B, and C is denoted
            // [A,B,C] and defined by
            // [A,B,C] = A dot ( B x C)
            // = B dot ( C x A)
            // = C dot ( A x B)
            // = det (A (B C) )
            // = | A1 A2 A3 |
            // | B1 B2 B3 |
            // | C1 C2 C3 |
            // V = 1/6 (Sum from j=0 to n-1 of {P0 dot P1 cross P2})
            // P0 = y0, P1 = y1, P2 = y2
            // fProd = P0 dot (P1 x P2)
            // fSum = sum of fProds
            // volume returned = 1/6 fSum
            float fProd = (kPos0.X() * ((kPos1.Y() * kPos2.Z()) - (kPos1.Z() * kPos2.Y()))) +
                          (kPos0.Y() * ((kPos1.Z() * kPos2.X()) - (kPos1.X() * kPos2.Z()))) +
                          (kPos0.Z() * ((kPos1.X() * kPos2.Y()) - (kPos1.Y() * kPos2.X())));

            fSum += fProd;
        }

        return Math.abs(fSum / 6.0f);
    }


    /**
     * Calculate the surface mesh area. Each surface mesh is composed of triangles. Calculate the surface area from the
     * summation of the each triangle area. Based on the Area by Stokes' Theorem. Area(S) = 1/2 * Normal dot ( Sum from
     * i = 0 to n-1 of ( V1 cross V2) ) ==> Area(S) = 1/2 * ( Sum from i = 0 to n-1 of ( Normal dot ( V1 cross V2) ) )
     *
     * @return  float
     */
    public float ComputeSurfaceArea() {

        float fSum = 0.0f;
        Vector3f kScale = new Vector3f( 1.0f/m_fX, 1.0f/m_fY, 1.0f/m_fZ );
        
        int iIndexQuantity = m_kMesh.IBuffer.GetIndexQuantity();
        int[] aiConnect = m_kMesh.IBuffer.GetData();
        Vector3f kPos0 = new Vector3f();
        Vector3f kPos1 = new Vector3f();
        Vector3f kPos2 = new Vector3f();
        for (int iT = 0; iT < iIndexQuantity; /**/) {

            // get indices to triangle vertices
            int iV0 = aiConnect[iT++];
            int iV1 = aiConnect[iT++];
            int iV2 = aiConnect[iT++];

            // get vertices
            m_kMesh.VBuffer.GetPosition3(iV0, kPos0);
            m_kMesh.VBuffer.GetPosition3(iV1, kPos1);
            m_kMesh.VBuffer.GetPosition3(iV2, kPos2);

            kPos0.multEquals( kScale );
            kPos1.multEquals( kScale );
            kPos2.multEquals( kScale );
            
            // Area of a triangle = || P0 X P1 + P1 X P2 + P2 X P0 ||/2
            // Area = 0.5* (det1 + det2 + det3), where
            // det1, det2, and det3 are 3 by 3 determinants where
            // i, j, and k are unit normal vectors along the x, y, and z axes and
            // |     i        j       k      |
            // det1 = |  kPos0.X()  kPos0.Y()  kPos0.Z()  |
            // |  kPos1.X()  kPos1.Y()  kPos1.Z()  |
            // |     i        j       k      |
            // det2 = |  kPos1.X()  kPos1.Y()  kPos1.Z()  |
            // |  kPos2.X()  kPos2.Y()  kPos2.Z()  |
            // |     i        j       k      |
            // det3 = |  kPos2.X()  kPos2.Y()  kPos2.Z()  |
            // |  kPos0.X()  kPos0.Y()  kPos0.Z()  |
            // vx, vy, and vz are the x, y, and z components of the vector in the area expression
            double vx, vy, vz;
            float triangleArea;
            vx = ((kPos0.Y() * kPos1.Z()) - (kPos1.Y() * kPos0.Z()) + (kPos1.Y() * kPos2.Z()) - (kPos2.Y() * kPos1.Z()) +
                  (kPos2.Y() * kPos0.Z()) - (kPos0.Y() * kPos2.Z()));
            vy = ((kPos0.Z() * kPos1.X()) - (kPos1.Z() * kPos0.X()) + (kPos1.Z() * kPos2.X()) - (kPos2.Z() * kPos1.X()) +
                  (kPos2.Z() * kPos0.X()) - (kPos0.Z() * kPos2.X()));
            vz = ((kPos0.X() * kPos1.Y()) - (kPos1.X() * kPos0.Y()) + (kPos1.X() * kPos2.Y()) - (kPos2.X() * kPos1.Y()) +
                  (kPos2.X() * kPos0.Y()) - (kPos0.X() * kPos2.Y()));
            triangleArea = (float) (0.5 * Math.sqrt((vx * vx) + (vy * vy) + (vz * vz)));
            fSum += triangleArea;
            /* The surface mesh is generated by the triangular isosurfaces.
             * The surface area of the object is just the sum of the area of these triangular isosurfaces. Formula to
             * calculate the area of an arbitrary triangle, assume the length of the sides of an arbitrary triangle are
             * a, b and c. Area = Sqrt(p(p-a)(p-b)(p-c)), where p = 1/2(a + b + c) To find the length of the sides of a
             * triangle, use the vector formula as the vertices of the triangle are in 3D space. Let the vertices of one
             * side of the triangle are (x1, y1, z1) and (x2, y2, z2). The length of the side is equal to  side =
             * Sqrt((x2-x1)^2+(y2-y1)^2+(z2-z1)^2)
             */
            /*double a, b, c, p;
             * a = Math.sqrt((double)((kPos1.X()-kPos0.X())*(kPos1.X()-kPos0.X()) + (kPos1.Y()-kPos0.Y())*(kPos1.Y()-kPos0.Y()) +
             * (kPos1.Z()-kPos0.Z())*(kPos1.Z()-kPos0.Z()))); b = Math.sqrt((double)((kPos2.X()-kPos1.X())*(kPos2.X()-kPos1.X()) +
             * (kPos2.Y()-kPos1.Y())*(kPos2.Y()-kPos1.Y()) + (kPos2.Z()-kPos1.Z())*(kPos2.Z()-kPos1.Z()))); c =
             * Math.sqrt((double)((kPos0.X()-kPos2.X())*(kPos0.X()-kPos2.X()) + (kPos0.Y()-kPos2.Y())*(kPos0.Y()-kPos2.Y()) +
             * (kPos0.Z()-kPos2.Z())*(kPos0.Z()-kPos2.Z()))); p = ( a + b + c ) / 2; float fProd =
             * (float)(Math.sqrt(p*(p-a)*(p-b)*(p-c)));
             *
             *fSum += fProd;*/
        }

        return fSum;
    }

    
    /**
     * Smooth mesh. The formula can be found in "The Visualization Toolkit" by Will Schoeder, Ken Martin, and Bill
     * Lorensen, p. 389. Mesh smoothing moves the verticies of the mesh closer to an average of the points. Each point
     * is moved so that it is the average of the points around it. The formula is:
     *
     * <pre>
            xi+1 = xi + (alpha * (Sum from j=0 to n of {xj - xi}))
     *  </pre>
     *
     * where xi+1 is the new point, xi is the orginal point, and the xjs are points that are connected to xi. Alpha is
     * some smoothing factor between .01 and .10. This formula is run for a number of iterations to obtain a smooth
     * mesh. Usually the iterations will be between 50 and 100.
     *
     * @param  iteration      Number of times to run smoothing formula on data set.
     * @param  alpha          Smoothing factor.
     * @param  volumeLimit    if true stop iterations when the present volume is volumePercent or more different from
     *                        the initial
     * @param  volumePercent  percentage from initial volume for stopping iterations
     */
    public void smoothMesh(int iteration, float alpha, boolean volumeLimit, float volumePercent)
    {
   
        int i;
        int num;
        float initialVolume = 0.0f;
        float presentVolume;
        boolean noVolumeLimit = true;
        float presentPercent;

        HashSet[] connections = buildConnections();
        
        if (volumeLimit) {
            initialVolume = GetVolume();
        }

        // repeat for however many iterations
        for (int k = 0; (k < iteration) && noVolumeLimit; k++) {
            scaleMesh( alpha, connections );
            if (volumeLimit) {
                presentVolume = ComputeVolume();

                // System.out.println("ModelTriangleMesh.smoothMesh -- present volume " + presentVolume);
                presentPercent = Math.abs(100.0f * (presentVolume - initialVolume) / initialVolume);

                if (presentPercent >= volumePercent) {
                    noVolumeLimit = false;
                }
            } // if (doVolumeLimit)
        }

        m_kMesh.UpdateMS();
        m_kMesh.VBuffer.Release();
    }
    /**
     * Derived from the first 2 of the 3 components of AlgorithmBrainExtraction Note that m_fStiffness does not increase
     * and then decrease as in AlgorithmBrainExtraction but instead remains constant. This is because the change in
     * m_fStiffness only applies to the third component of AlgorithmBrainExtraction which uses image intensities.
     *
     * @param  iterations     DOCUMENT ME!
     * @param  m_fStiffness   DOCUMENT ME!
     * @param  volumeLimit    if true stop iterations when the present volume is volumePercent or more different from
     *                        the initial
     * @param  volumePercent  percentage from initial volume for stopping iterations
     */
    public void smoothTwo(int iterations,
                          float fStiffness,
                          boolean volumeLimit,
                          float volumePercent)
    {
        float initialVolume = 0.0f;
        float presentVolume;
        boolean noVolumeLimit = true;
        float presentPercent;

        int iVertexQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        Vector3f[] akVMean = new Vector3f[iVertexQuantity];
        Vector3f[] akVNormal = new Vector3f[iVertexQuantity];
        Vector3f[] akSNormal = new Vector3f[iVertexQuantity];
        Vector3f[] akSTangent = new Vector3f[iVertexQuantity];
        float[] afCurvature = new float[iVertexQuantity];
        UnorderedSetInt[] akAdjacent = new UnorderedSetInt[iVertexQuantity];

        // System.out.println("I am entering smoothTwo");
        for (int i = 0; i < iVertexQuantity; i++) {
            akVMean[i] = new Vector3f();
            akVNormal[i] = new Vector3f();
            akSNormal[i] = new Vector3f();
            akSTangent[i] = new Vector3f();
            akAdjacent[i] = new UnorderedSetInt(6, 1);
        }

        HashMap kEMap = new HashMap();
        Integer kInvalid = new Integer(-1);

        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++)
        {
            int iV0, iV1, iV2;
            int[] aiTris = new int[3];
            if (!m_kMesh.GetTriangle(i, aiTris) )
            {
                continue;
            }
            iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];

            kEMap.put(new Edge(iV0, iV1), kInvalid);
            kEMap.put(new Edge(iV1, iV2), kInvalid);
            kEMap.put(new Edge(iV2, iV0), kInvalid);

            akAdjacent[iV0].insert(iV1);
            akAdjacent[iV0].insert(iV2);
            akAdjacent[iV1].insert(iV0);
            akAdjacent[iV1].insert(iV2);
            akAdjacent[iV2].insert(iV0);
            akAdjacent[iV2].insert(iV1);
        }

        if (volumeLimit) {
            initialVolume = GetVolume();
        }

        for (int i = 1; (i <= iterations) && noVolumeLimit; i++) {

            updateMesh( kEMap, akVNormal, akVMean, akAdjacent, akSTangent, akSNormal, afCurvature, fStiffness );

            if (volumeLimit) {
                presentVolume = ComputeVolume();
                presentPercent = Math.abs(100.0f * (presentVolume - initialVolume) / initialVolume);

                if (presentPercent >= volumePercent) {
                    noVolumeLimit = false;
                }
            } // if (doVolumeLimit)
        }
        

        m_kMesh.UpdateMS();
        m_kMesh.VBuffer.Release();
    }


    /**
     * Smooth mesh. This method smoothes without shrinking by 2 Gaussian smoothing steps. First, a Gaussian smoothing is
     * performed with a positive scale factor lambda. Second, a Gaussian smoothing is performed with a negative scale
     * factor mu, which is greater in magnitude than lambda. To produce a significant smoothing, these steps must be
     * repeated a number of times. 3 references for this smoothing: 1.) Curve and Surface Smoothing Without Shrinkage by
     * Gabriel Taubin, Technical Report RC-19536, IBM Research, April, 1994. (also in Proceedings, Fifth International
     * Conference on Computer Vision, pages 852-857, June, 1995). 2.) A Signal Processing Approach to Fair Surface
     * Design by Gabriel Taubin, Computer Graphics, pages 351-358, August, 1995 (Proceedings SIGGRAPH '95). 3.) Optimal
     * Surface Smoothing as Filter Design by Gabriel Taubin, Tong Zhang, and Gene Golub, IBM Research Report RC-20404
     * (#90237). Usually the iterations will be between 30 and 100.
     *
     * @param  iteration  Number of times to run smoothing formula on data set.
     * @param  lambda     positive scale factor
     * @param  mu         negative scale factor
     * @param  pVisible   if true display progress bar Require: 0 < lambda < -mu (1/lambda) + (1/mu) < 2
     * */
    public void smoothThree(int iteration, float lambda, float mu) {

        HashSet[] connections = buildConnections();

        scaleMesh( lambda, connections );
        scaleMesh( mu, connections );

        m_kMesh.UpdateMS();
        m_kMesh.VBuffer.Release();
    }


    private void scaleMesh( float fValue, HashSet[] connections )
    {
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        VertexBuffer kVBuffer = new VertexBuffer( m_kMesh.VBuffer );

        int num;
        Vector3f kSum = new Vector3f();
        Vector3f kOriginalPos = new Vector3f();
        Vector3f kConnectionPos = new Vector3f();

        // for each coordinate vertex
        for (int i = 0; i < iVQuantity; i++) {

            kSum.SetData(0f,0f,0f);
            num = 0;
            m_kMesh.VBuffer.GetPosition3(i, kOriginalPos);

            // get all the verticies that are connected to this one (at i)
            for (Iterator iter = connections[i].iterator(); iter.hasNext();) {
                int index = ((Integer) iter.next()).intValue();

                m_kMesh.VBuffer.GetPosition3(index, kConnectionPos);

                // Sum of (xj - xi) where j ranges over all the points connected to xi
                // xj = m_kV2; xi = m_kV3
                kSum.addEquals( kConnectionPos.sub( kOriginalPos ) );
                num++;
            }
            // xi+1 = xi + (alpha)*(sum of(points xi is connected to - xi))

            if (num > 1) {
                kSum.scaleEquals( 1.0f / (float)num );
            }

            kSum.scaleEquals( fValue );
            kOriginalPos.addEquals( kSum );
            kVBuffer.SetPosition3(i, kOriginalPos);
        }

        for (int i = 0; i < iVQuantity; i++) {
            m_kMesh.VBuffer.SetPosition3(i, kVBuffer.GetPosition3(i) );
        }

        kVBuffer.dispose();
        kVBuffer = null;
    }


    /**
     * Builds a list of cross-references, so that connections[i] contains all the verticies that are connected to vertex
     * at i.
     */
    private HashSet[] buildConnections() {
        Iterator iter;
        int index;
        boolean addT1, addT2, addT3;

        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        HashSet[] connections = new HashSet[iVQuantity];

        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++)
        {
            int iV0, iV1, iV2;
            int[] aiTris = new int[3];
            if (!m_kMesh.GetTriangle(i, aiTris) )
            {
                continue;
            }
            iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];

            if (connections[iV0] == null) {
                connections[iV0] = new HashSet();
            }

            addT2 = true;
            addT3 = true;

            for (iter = connections[iV0].iterator(); iter.hasNext();) {
                index = ((Integer) iter.next()).intValue();

                if (index == iV1) {
                    addT2 = false;
                } else if (index == iV2) {
                    addT3 = false;
                }
            }

            if (addT2) {
                connections[iV0].add(new Integer(iV1));
            }

            if (addT3) {
                connections[iV0].add(new Integer(iV2));
            }

            if (connections[iV1] == null) {
                connections[iV1] = new HashSet();
            }

            addT1 = true;
            addT3 = true;

            for (iter = connections[iV1].iterator(); iter.hasNext();) {
                index = ((Integer) iter.next()).intValue();

                if (index == iV0) {
                    addT1 = false;
                } else if (index == iV2) {
                    addT3 = false;
                }
            }

            if (addT1) {
                connections[iV1].add(new Integer(iV0));
            }

            if (addT3) {
                connections[iV1].add(new Integer(iV2));
            }

            if (connections[iV2] == null) {
                connections[iV2] = new HashSet();
            }

            addT1 = true;
            addT2 = true;

            for (iter = connections[iV2].iterator(); iter.hasNext();) {
                index = ((Integer) iter.next()).intValue();

                if (index == iV0) {
                    addT1 = false;
                } else if (index == iV1) {
                    addT2 = false;
                }
            }

            if (addT1) {
                connections[iV2].add(new Integer(iV0));
            }

            if (addT2) {
                connections[iV2].add(new Integer(iV1));
            }
        }
        return connections;
    }

    /**
     * Compute the average length of all the edges in the triangle mesh.
     */
    private float computeMeanEdgeLength( HashMap kEMap )
    {
        float fMeanEdgeLength = 0.0f;

        Iterator kEIter = kEMap.entrySet().iterator();
        Map.Entry kEntry = null;
        while (kEIter.hasNext()) {
            kEntry = (Map.Entry) kEIter.next();

            Edge kE = (Edge) kEntry.getKey();

            Vector3f kV0 = m_kMesh.VBuffer.GetPosition3(kE.m_iV0);
            Vector3f kV1 = m_kMesh.VBuffer.GetPosition3(kE.m_iV1);
            Vector3f kEdge = kV1.sub(kV0);
            fMeanEdgeLength += kEdge.Length();
        }

        fMeanEdgeLength /= (float)kEMap.size();
        return fMeanEdgeLength;
    }

    /**
     * Let V[i] be a vertex in the triangle mesh. This function computes VMean[i], the average of the immediate
     * neighbors of V[i]. Define S[i] = VMean[i] - V[i]. The function also computes a surface normal SNormal[i], the
     * component of S[i] in the vertex normal direction. STangent[i] = S[i] - SNormal[i] is computed as an approximation
     * to a tangent to the surface. Finally, Curvature[i] is an approximation of the surface curvature at V[i].
     */
    private void computeVertexInformation( float[] afParams, float fMeanEdgeLength, 
            Vector3f[] akVNormal,
            Vector3f[] akVMean, UnorderedSetInt[] akAdjacent,
            Vector3f[] akSTangent, Vector3f[] akSNormal, float[] afCurvature )
    {
        float fMinCurvature = Float.POSITIVE_INFINITY;
        float fMaxCurvature = Float.NEGATIVE_INFINITY;
        float fInvMeanLength = 1.0f / fMeanEdgeLength;

        int iVertexQuantitaty = m_kMesh.VBuffer.GetVertexQuantity();
        for (int i = 0; i < iVertexQuantitaty; i++) {
            akVMean[i].SetData(0.0f, 0.0f, 0.0f);
        }

        Vector3f kS = new Vector3f();
        
        for (int i = 0; i < iVertexQuantitaty; i++) {

            // compute the mean of the vertex neighbors
            // Point3f kMean = m_akVMean[i];
            UnorderedSetInt kAdj = akAdjacent[i];

            for (int j = 0; j < kAdj.getQuantity(); j++) {
                Vector3f kV0 = m_kMesh.VBuffer.GetPosition3(kAdj.get(j));
                akVMean[i].addEquals(kV0);
            }

            akVMean[i].scaleEquals(1.0f / (float)kAdj.getQuantity());

            // compute the normal and tangential components of mean-vertex
            Vector3f kV0 = m_kMesh.VBuffer.GetPosition3(i);
            akVMean[i].sub( kV0, kS );
            akSNormal[i].SetData( akVNormal[i].scale(kS.Dot(akVNormal[i])));
            kS.sub(akSNormal[i], akSTangent[i]);

            // compute the curvature
            float fLength = akSNormal[i].Length();

            afCurvature[i] = ((2.0f * fLength) * fInvMeanLength) * fInvMeanLength;

            if (afCurvature[i] < fMinCurvature) {
                fMinCurvature = afCurvature[i];
            }

            if (afCurvature[i] > fMaxCurvature) {
                fMaxCurvature = afCurvature[i];
            }
        }

        // compute the fractional function parameters for update2()
        afParams[0] = 0.5f * (fMinCurvature + fMaxCurvature);
        afParams[1] = 6.0f / (fMaxCurvature - fMinCurvature);
    }

    /**
     * Compute the vertex normals of the triangle mesh. Each vertex normal is the unitized average of the non-unit
     * triangle normals for those triangles sharing the vertex.
     */
    private void computeVertexNormals( Vector3f[] akVNormal )
    {
        int iVertexQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        // maintain a running sum of triangle normals at each vertex
        for (int i = 0; i < iVertexQuantity; i++) {
            akVNormal[i].SetData(0.0f, 0.0f, 0.0f);
        }

        Vector3f kEdge1 = new Vector3f();
        Vector3f kEdge2 = new Vector3f();
        Vector3f kNormal = new Vector3f();

        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++)
        {
            int iV0, iV1, iV2;
            int[] aiTris = new int[3];
            if (!m_kMesh.GetTriangle(i, aiTris) )
            {
                continue;
            }
            iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];

            Vector3f kV0 = m_kMesh.VBuffer.GetPosition3(iV0);
            Vector3f kV1 = m_kMesh.VBuffer.GetPosition3(iV1);
            Vector3f kV2 = m_kMesh.VBuffer.GetPosition3(iV2);

            // compute the triangle normal
            kV1.sub( kV0, kEdge1 );
            kV2.sub( kV0, kEdge2 );
            kEdge1.Cross( kEdge2, kNormal );

            // the triangle normal partially contributes to each vertex normal
            akVNormal[iV0].addEquals(kNormal);
            akVNormal[iV1].addEquals(kNormal);
            akVNormal[iV2].addEquals(kNormal);
        }

        for (int i = 0; i < iVertexQuantity; i++) {
            akVNormal[i].Normalize();
        }
    }

    /**
     * Compute the coefficient of the surface normal for the update of the mesh vertex V[i] in the SNormal[i] direction.
     * See BrainExtraction.pdf for a description of the update.
     *
     * @param   i  the index of the vertex to update
     *
     * @return  the coefficient of SNormal[i] for the update
     */
    private float update2(int i, float[] afCurvature, float fStiffness, float[] afParam )
    {
        float fArg = afParam[1] * (afCurvature[i] - afParam[0]);
        float fExpP = (float) Math.exp(fArg);
        float fExpN = (float) Math.exp(-fArg);
        float fTanh = (fExpP - fExpN) / (fExpP + fExpN);
        float fUpdate2 = 0.5f * fStiffness * (1.0f + fTanh);

        return fUpdate2;
    }

    /**
     * The heart of the segmentation. This function is responsible for the evolution of the triangle mesh that
     * approximates the brain surface. The update has a tangential component, a surface normal component, and a vertex
     * normal component for each vertex in the mesh. The first two components control the geometry of the mesh. The last
     * component is based on the MRI data itself. See BrainExtraction.pdf for a detailed description of the update
     * terms.
     */
    private void updateMesh(HashMap kEMap, Vector3f[] akVNormal, Vector3f[] akVMean, UnorderedSetInt[] akAdjacent, Vector3f[] akSTangent, Vector3f[] akSNormal, float[] afCurvature, float fStiffness )
    {
        float fMeanLength = computeMeanEdgeLength(kEMap);
        computeVertexNormals(akVNormal);
        float[] afParams = new float[2];
        computeVertexInformation(afParams, fMeanLength, 
                akVNormal,
                akVMean, akAdjacent,
                akSTangent, akSNormal, afCurvature);

        int iVertexQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        // update the vertices
        for (int i = 0; i < iVertexQuantity; i++) {
            Vector3f kV = m_kMesh.VBuffer.GetPosition3(i);

            // tangential update
            Vector3f kT = akSTangent[i].scale( 0.5f );
            kV.addEquals( kT );

            // normal update
            float fUpdate2 = update2(i, afCurvature, fStiffness, afParams);

            Vector3f kS = akSNormal[i].scale(fUpdate2);
            kV.addEquals( kS );
            m_kMesh.VBuffer.SetPosition3(i, kV);
        }
    }
    
    /** Creates the scene graph. */
    private void CreateScene ( )
    {
        m_kVertexColor3Shader = new VolumePreRenderEffect();
        m_kScene = new Node();

        m_kCull = new CullState();
        m_kScene.AttachGlobalState(m_kCull);

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kScene.AttachGlobalState(m_kAlpha);

        m_kWireframe = new WireframeState();
        m_kScene.AttachGlobalState(m_kWireframe);
    }

    /** Delete local memory. */
    public void dispose()
    {
        if ( m_kVertexColor3Shader != null )
        {
            m_kVertexColor3Shader.dispose();
            m_kVertexColor3Shader = null;
        }
        m_kMesh = null;
    }

    /** ShaderEffect for the plane bounding-boxes. */
    private VolumePreRenderEffect m_kVertexColor3Shader;
    //private HierarchicalTriMesh m_kMesh = null;
    private TriMesh m_kMesh = null;
    private MaterialState m_kMaterial = null;
    private SurfaceLightingEffect m_kLightShader;



    private float m_fVolume = 0;
    private float m_fSurfaceArea = 0;
    private Vector3f m_kCenter = null;
    
    private boolean m_bHasPerVertexColor = false;
    private ColorRGBA[] m_akBackupColor = null;
    private boolean m_bPainted = false;
    private boolean m_bTextureOn = false;
    
    private Node[] m_akGeodesicNodes = new Node[3];
    private int m_iCurrentGeodesic = 0;   
}
