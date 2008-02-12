package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Collision.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.Spatial.CullingMode;

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
        super(kImageA,kTranslate,fX,fY,fZ);

        CreateScene();
        m_kMesh = new HierarchicalTriMesh(kMesh);
        m_kMesh.SetName( new String( kMesh.GetName() ) );
        
        int iUnit = 0;
        if ( m_kMesh.VBuffer.GetAttributes().GetCChannels(1) != 0 )
        {
            iUnit = 1;
        }
        for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMesh.VBuffer.SetPosition3(i, m_kMesh.VBuffer.GetPosition3fX(i) - kTranslate.X(),
                    m_kMesh.VBuffer.GetPosition3fY(i) - kTranslate.Y(), 
                    m_kMesh.VBuffer.GetPosition3fZ(i) - kTranslate.Z() );

            m_kMesh.VBuffer.SetColor3( iUnit, i, 
                    m_kMesh.VBuffer.GetPosition3fX(i) * 1.0f/m_fX,
                    m_kMesh.VBuffer.GetPosition3fY(i) * 1.0f/m_fY,
                    m_kMesh.VBuffer.GetPosition3fZ(i) * 1.0f/m_fZ);

        }
        m_kMesh.Local.SetTranslate(m_kTranslate);
        m_kMesh.UpdateMS();

        m_kLightShader = new SurfaceLightingEffect( );
        m_kMaterial = new MaterialState();
        m_kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        m_kMaterial.Ambient = new ColorRGB(0.2f,0.2f,0.2f);
        m_kMaterial.Diffuse = new ColorRGB(ColorRGB.WHITE);
        m_kMaterial.Specular = new ColorRGB(ColorRGB.WHITE);
        m_kMaterial.Shininess = 32f;

        m_kMesh.AttachGlobalState(m_kMaterial);
        m_kMesh.AttachEffect(m_kLightShader);
        m_kMesh.UpdateRS();
        
        m_kScene.AttachChild(m_kMesh);
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        
        BoxBVTree kBoxBV = new BoxBVTree(m_kMesh, 4000, true);
        m_kMesh.SetBoundingVolumeTree(kBoxBV);
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
        for ( int i = 0; i < 3; i++ )
        {
            m_kMesh.DetachAllEffects();
            m_kMesh.AttachEffect( m_kVertexColor3Shader );
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
        for ( int i = 0; i < 3; i++ )
        {
            m_kMesh.DetachAllEffects();
            m_kMesh.AttachEffect( m_kLightShader );
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
        m_kMaterial.Ambient = kColor;
        m_kMaterial.Diffuse = kColor;
    }
    
    public void Blend( float fValue )
    {
        m_kLightShader.Blend(fValue);
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
        m_kMaterial.Ambient.SetData( kMaterial.Ambient );
        m_kMaterial.Diffuse.SetData( kMaterial.Diffuse );
        m_kMaterial.Specular.SetData( kMaterial.Specular );
        m_kMaterial.Emissive.SetData( kMaterial.Emissive );
        m_kMaterial.Shininess = kMaterial.Shininess;
        m_kScene.UpdateGS();
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

    
    /** Creates the scene graph. */
    private void CreateScene ( )
    {
        m_kVertexColor3Shader = new VertexColor3Effect();
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
    private VertexColor3Effect m_kVertexColor3Shader;
    private HierarchicalTriMesh m_kMesh = null;
    private MaterialState m_kMaterial = null;
    private SurfaceLightingEffect m_kLightShader;



    private float m_fVolume = 0;
    private float m_fSurfaceArea = 0;
}
