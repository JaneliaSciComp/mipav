package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceState;

import java.util.BitSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

import WildMagic.LibFoundation.Intersection.IntrLine3Triangle3f;
import WildMagic.LibFoundation.Intersection.IntrSegment3Triangle3f;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Line3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Triangle3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.System.UnorderedSetInt;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Detail.Edge;
import WildMagic.LibGraphics.Effects.Effect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.BoxBV;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public class VolumeSurface extends VolumeObject
{
    /** ShaderEffect for the plane bounding-boxes. */
    private VolumePreRenderEffect m_kVolumePreShader;
    private VolumePreRenderEffect m_kVolumePreShaderTransparent;
    //private HierarchicalTriMesh m_kMesh = null;
    /** TriMesh */
    private TriMesh m_kMesh = null;
    /** Surface material properties. */
    private MaterialState m_kMaterial = null;
    /** Surface opacity value. */
    private float m_kOpacity = 1;
    /** Surface shader effect. */
    private SurfaceLightingEffect m_kLightShader;
    private SurfaceLightingEffect m_kLightShaderTransparent;
    /** Surface volume. */
    private float m_fVolume = 0;
    /** Surface area. */
    private float m_fSurfaceArea = 0;
    /** Center of surface. */
    private Vector3f m_kCenter = null;
    private Vector3f m_kCenterScanner = null;
    /** Backup of the per-vertex colors. */
    private ColorRGBA[] m_akBackupColor = null;
    
    /** Set to true when the surface has been painted. */
    private boolean m_bPainted = false;
    /** Set to true when texture mapping is on. */
    private boolean m_bTextureOn = false;
    /** Contains geodesic display components. */
    private Node[] m_akGeodesicNodes = new Node[3]; 
    /** Current displayed geodesic component. */
    private int m_iCurrentGeodesic = 0;
    private float m_fBlend = 1.0f;
    private SurfaceState m_kSurfaceState = null;

    /** Create a new VolumeSurface with the VolumeImage parameter.
     * @param kImageA the VolumeImage containing shared data and textures for
     * rendering.
     * @param kVolumeImageB second VolumeImage.
     * @param kTranslate translation in the scene-graph for this object.
     * @param fX the size of the volume in the x-dimension (extent * resolutions)
     * @param fY the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeSurface ( VolumeImage kImageA, VolumeImage kImageB, Vector3f kTranslate, 
            float fX, float fY, float fZ, SurfaceState kSurface )
    {
        super(kImageA,kImageB,kTranslate,fX,fY,fZ);
        m_kSurfaceState = kSurface;
        CreateScene();
        m_kMesh = kSurface.Surface;
        m_kMesh.SetName( new String( kSurface.Name ) );
        
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
        m_kSurfaceState.Material = m_kMaterial;
        m_akBackupColor = new ColorRGBA[m_kMesh.VBuffer.GetVertexQuantity()];

        for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            if ( m_kMesh.VBuffer.GetAttributes().HasTCoord(0) )
            {
                m_kMesh.VBuffer.SetTCoord3( 0, i, 
                        (m_kMesh.VBuffer.GetPosition3fX(i)  - kTranslate.X) * 1.0f/m_fX,
                        (m_kMesh.VBuffer.GetPosition3fY(i)  - kTranslate.Y) * 1.0f/m_fY,
                        (m_kMesh.VBuffer.GetPosition3fZ(i)  - kTranslate.Z) * 1.0f/m_fZ);
                //System.err.println( "Set TexCoord: " + m_kMesh.VBuffer.GetTCoord3(0, i).ToString() );
            }
            m_akBackupColor[i] = new ColorRGBA();
            m_kMesh.VBuffer.GetColor4( 0, i, m_akBackupColor[i]);
        }
        m_kMesh.UpdateMS();

        m_kLightShader = new SurfaceLightingEffect( kImageA, false );
        m_kLightShaderTransparent = new SurfaceLightingEffect( kImageA, true );

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
        
        

    	int iDimX = m_kVolumeImageA.GetImage().getExtents()[0];
    	int iDimY = m_kVolumeImageA.GetImage().getExtents()[1];
    	int iDimZ = m_kVolumeImageA.GetImage().getExtents()[2];
        float[] afResolutions = m_kVolumeImageA.GetImage().getResolutions(0);
        m_kResolutions = new Vector3f( afResolutions[0], afResolutions[1], afResolutions[2] );
        //System.err.println( m_kResolutions );

		float xBox = (iDimX - 1) * afResolutions[0];
		float yBox = (iDimY - 1) * afResolutions[1];
		float zBox = (iDimZ - 1) * afResolutions[2];
		float maxBox = Math.max(xBox, Math.max(yBox, zBox));

		m_kVolumeScale = new Vector3f ( 2f * afResolutions[0], 2f * afResolutions[1], 2f * afResolutions[2] );
		m_kMeshScale = new Vector3f ( 1f/(2f * afResolutions[0]), 1f/(2f * afResolutions[1]), 1f/(2f * afResolutions[2]) );
    	m_kVolumeTrans = new Vector3f(xBox, yBox, zBox);
    	m_fVolumeDiv = 1f/(2.0f*maxBox);
    	m_fVolumeMult = (2.0f*maxBox);
    	
    	m_akUnitsLabel = new String[3];
    	for ( int i = 0; i < 3; i++ )
    	{
    		m_akUnitsLabel[i] = new String( Unit.getUnitFromLegacyNum(m_kVolumeImageA.GetImage().getUnitsOfMeasure()[i]).getAbbrev() );
    	}
    }
    
    private String[] m_akUnitsLabel;
    private Vector3f m_kVolumeScale, m_kVolumeTrans, m_kMeshScale, m_kResolutions;
    private float m_fVolumeDiv, m_fVolumeMult;
    private BoxBV  m_kBoundingBox = null;
    private Vector3f m_kMinBB, m_kMaxBB;

    /**
     * Add a new geodesic component to the surface.
     * @param kNew the new geodesic component.
     * @param iGroup the display group to add to.
     */
    public void AddGeodesic( Geometry kMesh, int iGroup )
    {

        kMesh.AttachGlobalState(m_kMaterial);  
        kMesh.UpdateRS();
            
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
        m_kScene.UpdateRS();
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Blend(float)
     */
    public void Blend( float fValue )
    {
        m_fBlend = fValue;
        m_kLightShader.Blend(fValue);
        m_kLightShaderTransparent.Blend(fValue);
        m_kVolumePreShader.Blend(fValue);
        m_kVolumePreShaderTransparent.Blend(fValue);
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
            
            meshToVolumeCoords( kPos0 );
            meshToVolumeCoords( kPos1 );
            meshToVolumeCoords( kPos2 );            
            
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
            vx = ((kPos0.Y * kPos1.Z) - (kPos1.Y * kPos0.Z) + (kPos1.Y * kPos2.Z) - (kPos2.Y * kPos1.Z) +
                  (kPos2.Y * kPos0.Z) - (kPos0.Y * kPos2.Z));
            vy = ((kPos0.Z * kPos1.X) - (kPos1.Z * kPos0.X) + (kPos1.Z * kPos2.X) - (kPos2.Z * kPos1.X) +
                  (kPos2.Z * kPos0.X) - (kPos0.Z * kPos2.X));
            vz = ((kPos0.X * kPos1.Y) - (kPos1.X * kPos0.Y) + (kPos1.X * kPos2.Y) - (kPos2.X * kPos1.Y) +
                  (kPos2.X * kPos0.Y) - (kPos0.X * kPos2.Y));
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

        return (fSum * m_kResolutions.X * m_kResolutions.Y * m_kResolutions.Z);
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
        int iTriangleQuantity = m_kMesh.GetTriangleQuantity();
        int[] aiConnect = m_kMesh.IBuffer.GetData();
        Vector3f kPos0 = new Vector3f();
        Vector3f kPos1 = new Vector3f();
        Vector3f kPos2 = new Vector3f();
        for (int iT = 0; iT < iTriangleQuantity; iT++) {

            // get indices to triangle vertices
            int iV0 = aiConnect[iT * 3 + 0];
            int iV1 = aiConnect[iT * 3 + 1];
            int iV2 = aiConnect[iT * 3 + 2];

            // get vertices
            m_kMesh.VBuffer.GetPosition3(iV0, kPos0);
            m_kMesh.VBuffer.GetPosition3(iV1, kPos1);
            m_kMesh.VBuffer.GetPosition3(iV2, kPos2);
            
            meshToVolumeCoords( kPos0 );
            meshToVolumeCoords( kPos1 );
            meshToVolumeCoords( kPos2 );
            
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
            float fProd = (kPos0.X * ((kPos1.Y * kPos2.Z) - (kPos1.Z * kPos2.Y))) +
                          (kPos0.Y * ((kPos1.Z * kPos2.X) - (kPos1.X * kPos2.Z))) +
                          (kPos0.Z * ((kPos1.X * kPos2.Y) - (kPos1.Y * kPos2.X)));

            fSum += fProd;
        }
        fSum = (Math.abs(fSum / 6.0f) * m_kResolutions.X * m_kResolutions.Y * m_kResolutions.Z);
        return fSum;
    }

    /** Delete local memory. */
    public void dispose(Renderer kRenderer)
    {
        if ( m_kVolumePreShader != null )
        {
        	kRenderer.ReleaseResources(m_kVolumePreShader);
            m_kVolumePreShader.dispose();
            m_kVolumePreShader = null;
        }
        if ( m_kVolumePreShaderTransparent != null )
        {
        	kRenderer.ReleaseResources(m_kVolumePreShaderTransparent);
            m_kVolumePreShaderTransparent.dispose();
            m_kVolumePreShaderTransparent = null;
        }
        if ( m_kMesh != null )
        {
        	kRenderer.ReleaseVBuffer(m_kMesh.VBuffer);
        	kRenderer.ReleaseIBuffer(m_kMesh.IBuffer);        	
            m_kMesh.dispose();
            m_kMesh = null;
        }
        if ( m_kMaterial != null )
        {
            m_kMaterial.dispose();
            m_kMaterial = null;
        }
        if ( m_kLightShader != null )
        {
        	kRenderer.ReleaseResources(m_kLightShader);
            m_kLightShader.dispose();
            m_kLightShader = null;
        }
        if ( m_kLightShaderTransparent != null )
        {
        	kRenderer.ReleaseResources(m_kLightShaderTransparent);
            m_kLightShaderTransparent.dispose();
            m_kLightShaderTransparent = null;
        }
        
        m_kCenter = null;
        for ( int i = 0; i < m_akBackupColor.length; i++ )
        {
        	m_akBackupColor[i] = null;
        }
        m_akBackupColor = null;    
        if ( m_kSurfaceState != null )
        {
        	m_kSurfaceState.dispose();
        }
        
        if ( m_akGeodesicNodes != null )
        {
        	for ( int i = 0; i < m_akGeodesicNodes.length; i++ )
        	{
        		if ( m_akGeodesicNodes[i] != null )
        		{
        			kRenderer.ReleaseAllResources( m_akGeodesicNodes[i] );
        			m_akGeodesicNodes[i].dispose();
        			m_akGeodesicNodes[i] = null;
        		}
        	}	
        	m_akGeodesicNodes = null;
        }
        super.dispose(kRenderer);
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Dropper(WildMagic.LibGraphics.Collision.PickRecord, WildMagic.LibFoundation.Mathematics.ColorRGBA, WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public void Dropper(PickRecord kRecord, ColorRGBA rkDropperColor, Vector3f rkPickPoint )
    {
        m_kMesh.VBuffer.GetPosition3(kRecord.iV0, rkPickPoint );
        rkPickPoint.Mult( new Vector3f( 1.0f/m_fX, 1.0f/m_fY, 1.0f/m_fZ ));
        int[] iExtents = m_kVolumeImageA.GetImage().getExtents();
        rkPickPoint.Mult( new Vector3f( iExtents[0], iExtents[1], iExtents[2] ));
        
        m_kMesh.VBuffer.GetColor4(0, kRecord.iV0, rkDropperColor );
        if ( m_bTextureOn )
        {
            Vector3f kTexCoord = new Vector3f();
            m_kMesh.VBuffer.GetTCoord3( 0, kRecord.iV0, kTexCoord );
            m_kLightShader.Dropper( kTexCoord, rkDropperColor);
            m_kLightShaderTransparent.Dropper( kTexCoord, rkDropperColor);
        }
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Erase(WildMagic.LibGraphics.Rendering.Renderer, WildMagic.LibGraphics.Collision.PickRecord, int)
     */
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
                kDiff.Sub( kPos1, kPos2 );
                if ( kDiff.Length() < (iBrushSize/500.0f) )
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
            m_kMesh.VBuffer.LoadSub( iMin, iMax );
            /*
            float[] afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(m_kMesh.VBuffer.GetAttributes(),
                                                                           iMin, iMax );
            FloatBuffer kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, iMin*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );
*/
        }
        else
        {
            m_kMesh.VBuffer.LoadSub( kRecord.iV0, kRecord.iV0 );
            m_kMesh.VBuffer.LoadSub( kRecord.iV1, kRecord.iV1 );
            m_kMesh.VBuffer.LoadSub( kRecord.iV2, kRecord.iV2 );
            /*
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
            */
        }
    }


    /**
     * Erase all paint.
     * @param kRenderer renderer.
     */
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

        m_kMesh.VBuffer.LoadSub( iMin, iMax );
        /*
        float[] afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(m_kMesh.VBuffer.GetAttributes(),
                iMin, iMax );
        FloatBuffer kData = FloatBuffer.wrap(afCompatible);
        kData.rewind();
        kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, iMin*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );
*/
    }
    
    /**
     * Return center of the surface.
     * @return center of the surface.
     */
    public Vector3f GetCenter()
    {
        if ( m_kCenter == null )
        {
            ComputeCenter();
        }
        return m_kCenter;
    }    
    /**
     * Return the surface material properties.
     * @return surface material properties.
     */
    public MaterialState GetMaterial()
    {
        return m_kMaterial;
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#GetMesh()
     */
    public TriMesh GetMesh()
    {
        return m_kMesh; 
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#GetName()
     */
    public String GetName()
    {
        return m_kMesh.GetName();
    }

    /**
     * Return the surface opacity value.
     * @return surface opacity value.
     */
    public float GetOpacity() {
    	return m_kOpacity;
    }
    
    /**
     * Return the surface area.
     * @return surface area.
     */
    public float GetSurfaceArea()
    {
        if ( m_fSurfaceArea == 0 )
        {
            m_fSurfaceArea = ComputeSurfaceArea();
        }
        return m_fSurfaceArea;
    }  
    
    /**
     * Return the surface area.
     * @return surface area.
     */
    public String GetSurfaceAreaString()
    {
        if ( m_fSurfaceArea == 0 )
        {
            m_fSurfaceArea = ComputeSurfaceArea();
        }
        return new String ( m_fSurfaceArea + " " + m_akUnitsLabel[0] + " x " + m_akUnitsLabel[1] );
    }  
    
    /**
     * Return the volume of this surface.
     * @return volume of this surface.
     */
    public float GetVolume()
    {
        if ( m_fVolume == 0 )
        {
            m_fVolume = ComputeVolume();
        }
        
        return m_fVolume;
    } 
    
    /**
     * Return the volume of this surface.
     * @return volume of this surface.
     */
    public String GetVolumeString()
    {
        if ( m_fVolume == 0 )
        {
            m_fVolume = ComputeVolume();
        }
        return new String ( (m_fVolume) + " " + m_akUnitsLabel[0] + " x " + m_akUnitsLabel[1] + " x " + m_akUnitsLabel[2] );
    } 

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Paint(WildMagic.LibGraphics.Rendering.Renderer, WildMagic.LibGraphics.Collision.PickRecord, WildMagic.LibFoundation.Mathematics.ColorRGBA, int)
     */
    public void Paint( Renderer kRenderer, PickRecord kRecord, ColorRGBA kPaintColor, int iBrushSize )
    {
        m_bPainted = true;
        //System.err.println( "Painting: " + m_kMesh.VBuffer.GetAttributes().GetCChannels(0) );
        m_kMesh.VBuffer.SetColor4(0, kRecord.iV0, kPaintColor.R, kPaintColor.G, kPaintColor.B, kPaintColor.A );
        m_kMesh.VBuffer.SetColor4(0, kRecord.iV1, kPaintColor.R, kPaintColor.G, kPaintColor.B, kPaintColor.A );
        m_kMesh.VBuffer.SetColor4(0, kRecord.iV2, kPaintColor.R, kPaintColor.G, kPaintColor.B, kPaintColor.A );


        Attributes kIAttr = kRenderer.GetVBufferInputAttributes( m_kMesh.VBuffer );
        if ( kIAttr == null )
        {
            kIAttr = m_kMesh.VBuffer.GetAttributes();
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
                kDiff.Sub( kPos1, kPos2);
                if ( kDiff.Length() < (iBrushSize/500.0f) )
                {
                    m_kMesh.VBuffer.SetColor4(0, i, kPaintColor.R, kPaintColor.G, kPaintColor.B, kPaintColor.A );
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
            m_kMesh.VBuffer.LoadSub( iMin, iMax );
            /*
            float[] afCompatible = m_kMesh.VBuffer.BuildCompatibleSubArray(kIAttr,
                                                                           iMin, iMax );
            FloatBuffer kData = FloatBuffer.wrap(afCompatible);
            kData.rewind();
            kRenderer.LoadSubVBuffer(m_kMesh.VBuffer, iMin*m_kMesh.VBuffer.GetVertexSize(), afCompatible.length, kData );
        */
        }
        else
        {
            m_kMesh.VBuffer.LoadSub( kRecord.iV0, kRecord.iV0 );
            m_kMesh.VBuffer.LoadSub( kRecord.iV1, kRecord.iV1 );
            m_kMesh.VBuffer.LoadSub( kRecord.iV2, kRecord.iV2 );
            /*
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
            */
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
        for ( int i = 0; i < m_kScene.GetQuantity(); i++ )
        {
            m_kScene.GetChild(i).DetachAllEffects();
            m_kScene.GetChild(i).AttachEffect( m_kVolumePreShader );
        }

        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }
      */
    /**
     * Removes all geodesic components from the given surface. */
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

  
    /**
     * Removes the specific geodesic component from the given surface.
     * @param iNode
     * @param iGroup
     */    
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

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#Render(WildMagic.LibGraphics.Rendering.Renderer, WildMagic.LibGraphics.SceneGraph.Culler)
     */
    public void Render( Renderer kRenderer, Culler kCuller, boolean bPreRender, boolean bSolid )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        for ( int i = 0; i < m_kScene.GetQuantity(); i++ )
        {
            //Spatial kObj = m_kScene.DetachChildAt(i);
            //m_kDisplayObjs.add(  );
            m_kScene.GetChild(i).DetachAllEffects();
            if ( bSolid && (m_fBlend >= 1.0f) )
            {
                if ( bPreRender )
                {
                    m_kScene.GetChild(i).AttachEffect( m_kVolumePreShader );
                }
                else
                {
                    m_kScene.GetChild(i).AttachEffect( m_kLightShader );
                }
            }
            else if ( !bSolid && (m_fBlend > 0) && (m_fBlend < 1.0) )
            {
                if ( bPreRender )
                {
                    m_kScene.GetChild(i).AttachEffect( m_kVolumePreShaderTransparent );
                }
                else
                {
                    m_kScene.GetChild(i).AttachEffect( m_kLightShaderTransparent );
                }
            }
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
        }
        m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }
    

    /**
     * Render the object.
     * @param kRenderer the OpenGLRenderer object.
     * @param kCuller the Culler object.
     */
    public void Render( Renderer kRenderer, Culler kCuller, Effect kEffect )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        for ( int i = 0; i < m_kScene.GetQuantity(); i++ )
        {
            m_kScene.GetChild(i).DetachAllEffects();
            m_kScene.GetChild(i).AttachEffect( kEffect );
        }
        m_kScene.UpdateGS();
        kCuller.ComputeVisibleSet(m_kScene);
        kRenderer.DrawScene(kCuller.GetVisibleSet());
    }

    /**
     * When the Geodesic object cuts the mesh along an open curve, the old mesh changes, but does not need to be deleted
     * and no new mesh needs to be added. This function allows the Geodesic object to replace the original mesh with the
     * sliced mesh in the surface renderer. ReplaceMesh is also used to undo cutting operations.
     *
     * @param  kNew  TriMesh new surface mesh
     */
    public void ReplaceGeodesic( TriMesh kMesh )
    {       
        //kMesh.Local.SetTranslate(m_kTranslate);
        kMesh.UpdateMS();

        kMesh.AttachGlobalState(m_kMaterial);
        //kMesh.UpdateGS();
        kMesh.AttachEffect(m_kLightShader);
        kMesh.UpdateRS();
        
        m_kScene.SetChild(0, kMesh);
        //m_kScene.UpdateGS();
        m_kScene.UpdateRS();
        
        m_kMesh = null;
        m_kMesh = kMesh;
    }
    
    
    /** Sets axis-aligned clipping for the VolumeShaderEffect.
     * @param afClip the clipping parameters for axis-aligned clipping.
     */
    public void SetClip( int iWhich, float data, boolean bEnable)
    {
        m_kLightShader.SetClip(iWhich, data, bEnable);
        m_kLightShaderTransparent.SetClip(iWhich, data, bEnable);
    }


    /** Sets arbitrary clipping for the VolumeShaderEffect.
     * @param afEquation the arbitrary-clip plane equation.
     */
    public void SetClipArb( float[] afEquation, boolean bEnable )
    {
        m_kLightShader.SetClipArb(afEquation, bEnable);
        m_kLightShaderTransparent.SetClipArb(afEquation, bEnable);
    }

    
    /** Sets eye clipping for the VolumeShaderEffect.
     * @param afEquation the eye clipping equation.
     */
    public void SetClipEye( float[] afEquation, boolean bEnable )
    {
        m_kLightShader.SetClipEye(afEquation, bEnable);
        m_kLightShaderTransparent.SetClipEye(afEquation, bEnable);
    }
    /** Sets inverse-eye clipping for the VolumeShaderEffect.
     * @param afEquation the inverse-eye clipping equation.
     */
    public void SetClipEyeInv( float[] afEquation, boolean bEnable )
    {
        m_kLightShader.SetClipEyeInv(afEquation, bEnable);
        m_kLightShaderTransparent.SetClipEyeInv(afEquation, bEnable);
    }


    /**
     * Enables/disables surface clipping for the per-pixel shader.
     * @param bClip surface clipping on/off.
     */
    public void SetClipping( boolean bClip )
    {
        m_kLightShader.SetClipping(bClip);
        m_kLightShaderTransparent.SetClipping(bClip);
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#SetColor(WildMagic.LibFoundation.Mathematics.ColorRGB)
     */
    public void SetColor( ColorRGB kColor, boolean bUpdate )
    {
        m_kMaterial.Diffuse.Copy(kColor);
        //m_kMaterial.Ambient.Copy( ColorRGB.BLACK );
        //m_kMaterial.Specular.Set( 0.5f,0.5f,0.5f );
        //m_kMaterial.Emissive.Copy( ColorRGB.BLACK );
        //m_kMaterial.Shininess = 5f;
        if ( bUpdate )
        {
            for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
            {
                m_kMesh.VBuffer.SetColor3( 0, i, kColor );
            }
            m_kMesh.VBuffer.Release();
        }
    }


    /**
     * Sets the ModelImage to use as an alternative to the volume ModelImage for surface texturing.
     * @param kImage the alternate ModelImage to use for the surface texture.
     */
    public void SetImageNew( ModelImage kImage )
    {
        m_kLightShader.SetImageNew(kImage);
        m_kLightShaderTransparent.SetImageNew(kImage);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject#SetLight(java.lang.String, float[])
     */
    public void SetLight( String kLightType, float[] afType )
    {
        if ( m_kLightShader != null )
        {
            m_kLightShader.SetLight(kLightType, afType);
            m_kLightShaderTransparent.SetLight(kLightType, afType);
        }
    }

    /**
     * Sets the LUT to use as an alternative to the volume lut for surface texturing.
     * @param kLUT the new LUT.
     * @param kRGBT the new ModelRGB (for color images).
     */
    public void SetLUTNew( ModelLUT kLUT, ModelRGB kRGBT )
    {
        m_kLightShader.SetLUTNew(kLUT, kRGBT);
        m_kLightShaderTransparent.SetLUTNew(kLUT, kRGBT);
    }

    /**
     * Set the surface material properties
     * @param kMaterial surface material properties.
     */
    public void SetMaterial( MaterialState kMaterial, boolean bUpdate)
    {
        m_kMaterial.Ambient.Copy( kMaterial.Ambient );
        m_kMaterial.Diffuse.Copy( kMaterial.Diffuse );
        m_kMaterial.Specular.Copy( kMaterial.Specular );
        m_kMaterial.Emissive.Copy( kMaterial.Emissive );
        m_kMaterial.Shininess = kMaterial.Shininess;
        if ( bUpdate )
        {
            for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
            {
                m_kMesh.VBuffer.SetColor3( 0, i, kMaterial.Diffuse );
            }
            m_kMesh.VBuffer.Release();
            m_kScene.UpdateGS();
        }
    }

    /**
     * Set the surface opacity properties
     * @param kMaterial surface opacity value.
     */
    public void SetOpacity( float opacity) {
    	m_kOpacity = opacity;
    }

    /**
     * Sets the lighting shader to be per-pixel or per-vertex.
     * @param kRenderer OpenGL renderer.
     * @param bOn turns per-pixel lighting on/off.
     */
    public void SetPerPixelLighting( Renderer kRenderer, boolean bOn )
    {
        if ( m_kLightShader != null )
        {
            m_kLightShader.SetPerPixelLighting(kRenderer, bOn);
            m_kLightShaderTransparent.SetPerPixelLighting(kRenderer, bOn);
        }
    }
    
    /**
     * Sets the surface texture on/off.
     * @param bTextureOn texture on/off
     * @param bUseNewImage indicates which volume to use as the texture.
     * @param bUseNewLUT indicates which LUT to use.
     */
    public void SetSurfaceTexture( boolean bOn, boolean bUseNewImage, boolean bUseNewLUT )
    {
        m_kLightShader.SetSurfaceTexture(bOn, bUseNewImage, bUseNewLUT);
        m_kLightShaderTransparent.SetSurfaceTexture(bOn, bUseNewImage, bUseNewLUT);
        m_bTextureOn = bOn;
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

        for ( int k = 0; k < iteration; k++ ) {
           scaleMesh( lambda, connections );
           scaleMesh( mu, connections );
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

        HashMap<Edge,Integer> kEMap = new HashMap<Edge,Integer>();
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
     * Switches between different ways of displaying the geodesic path (Euclidean, Geodesic, or Mesh).
     * @param iWhich the type of display.
     */
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
    
    
    /**
     * Creates a BitSet mask that is the volume enclosed by the triangle mesh.
     * This function is not accurate if the mesh is not closed, however it will still return a mask.
     * @return
     */
    private BitSet createMask( )
    {
    	final int iDimX = m_kVolumeImageA.GetImage().getExtents()[0];
    	final int iDimY = m_kVolumeImageA.GetImage().getExtents()[1];
    	
    	//final ModelImage kOutputImage = new ModelImage( ModelStorageBase.INTEGER, m_kVolumeImageA.GetImage().getExtents(), "SurfaceMask" );
    	if ( m_kBoundingBox == null )
    	{
    		initBoundingBox();
    	}
            	
    	final BitSet mask = new BitSet();
    	
    	final Vector3f[] directions = new Vector3f[5];
    	directions[0] = new Vector3f( (float)Math.random(), (float)Math.random(), (float)Math.random() );
    	directions[1] = new Vector3f( -(float)Math.random(), (float)Math.random(), (float)Math.random() );
    	directions[2] = new Vector3f( (float)Math.random(), -(float)Math.random(), (float)Math.random() );
    	directions[3] = new Vector3f( (float)Math.random(), (float)Math.random(), -(float)Math.random() );
    	directions[4] = new Vector3f( -(float)Math.random(), (float)Math.random(), -(float)Math.random() );
    	for ( int i = 0; i < directions.length; i++ )
    	{
    		directions[i].Normalize();
    	}
        long startTime = System.currentTimeMillis();
    	

		int xMin = (int)Math.floor(m_kMinBB.X);
		int yMin = (int)Math.floor(m_kMinBB.Y);
		int zMin = (int)Math.floor(m_kMinBB.Z);
		int xMax = (int)Math.ceil(m_kMaxBB.X);
		int yMax = (int)Math.ceil(m_kMaxBB.Y);
		int zMax = (int)Math.ceil(m_kMaxBB.Z);

        if (Preferences.isMultiThreadingEnabled())
        {
        	int nthreads = ThreadUtil.getAvailableCores();
        	int intervalX = xMax - xMin;
        	int intervalY = yMax - yMin;
        	int intervalZ = zMax - zMin;
            final CountDownLatch doneSignal = new CountDownLatch(nthreads);
            float stepX = 0;
            if ( intervalX > intervalY && intervalX > intervalZ )
            {
            	stepX = (float)intervalX / (float)nthreads;
            }
            float stepY = 0;
            if ( intervalY > intervalX && intervalY > intervalZ )
            {
            	stepY = (float)intervalY / (float)nthreads;
            }
            float stepZ = 0;
            if ( intervalZ > intervalX && intervalZ > intervalY )
            {
            	stepZ = (float)intervalZ / (float)nthreads;
            }
            if ( stepX == 0 && stepY == 0 && stepZ == 0 )
            {
            	stepZ = (float)intervalZ / (float)nthreads;			            	
            }

            for (int i = 0; i < nthreads; i++) {
                final int startX = stepX == 0 ? xMin : (int) (xMin + (    i * stepX));
                final int   endX = stepX == 0 ? xMax : (int) (xMin + ((i+1) * stepX));
                final int startY = stepY == 0 ? yMin : (int) (yMin + (    i * stepY));
                final int   endY = stepY == 0 ? yMax : (int) (yMin + ((i+1) * stepY));
                final int startZ = stepZ == 0 ? zMin : (int) (zMin + (    i * stepZ));
                final int   endZ = stepZ == 0 ? zMax : (int) (zMin + ((i+1) * stepZ));
                System.err.println( startX + " " + endX + "      " + startY + " " + endY + "      " + startZ + "  " + endZ );
                final Runnable task = new Runnable() {
                    public void run() {
                        calcMask( mask, null, directions, startX, endX, startY, endY, startZ, endZ,
                        		iDimX, iDimY );
                        doneSignal.countDown();
                    }
                };

                ThreadUtil.mipavThreadPool.execute(task);
            }
            try {
                doneSignal.await();
            } catch (final InterruptedException e) {
                e.printStackTrace();
            }
        }
        else
        {
            calcMask( mask, null, directions, xMin, xMax, yMin, yMax, zMin, zMax,
            		iDimX, iDimY );
        }
    	
    	
        
        long now = System.currentTimeMillis();
        double elapsedTime = (double) (now - startTime);

        // if elasedTime is invalid, then set it to 0
        if (elapsedTime <= 0) {
            elapsedTime = (double) 0.0;
        }

        double timeinSec =  (double) (elapsedTime / 1000.0); // return in seconds!!
        
        System.err.println( "Elapsed time: " + timeinSec );
		//kOutputImage.calcMinMax();
		//new ViewJFrameImage( kOutputImage );
		return mask;
    }


    private void calcMask( final BitSet mask, final ModelImage kOutputImage, final Vector3f[] directions, 
    		final int xMin, final int xMax,
    		final int yMin, final int yMax, 
    		final int zMin, final int zMax,
    		final int iDimX, final int iDimY )
    {
    	Vector3f kTest = new Vector3f();
    	Line3f[] akLines = new Line3f[directions.length];
		for ( int i = 0; i < directions.length; i++ )
		{
			akLines[i] = new Line3f( Vector3f.ONE, directions[i] );
		}
    	
    	
    	for ( int z = zMin; z <= zMax; z++ )
    	{
    		for ( int y = yMin; y <= yMax; y++ )
    		{
    			for ( int x = xMin; x <= xMax; x++ )
    			{
    				kTest.Set(x,y,z);
    				// convert to 'mesh' coords...
    				volumeToMeshCoords(kTest);

    				if ( m_kBoundingBox.Contains( kTest ) )
    				{
    					for ( int i = 0; i < directions.length; i++ )
    					{
    						akLines[i].Origin = kTest;
    					}
    					if ( testIntersections( kTest, akLines ) )
    					{
    						mask.set( z * iDimX * iDimY + y * iDimX + x );
    						if ( kOutputImage != null )
    						{
    							kOutputImage.set( z * iDimX * iDimY + y * iDimX + x, 50 );
    						}
    					}
    				}
    			}
    		}
    	}
    }

	/**
	 * Initializes the bounding box for the mesh. The volume bounding box is in volume-index coordinats.
	 */
	private void initBoundingBox()
	{
		m_kBoundingBox = new BoxBV();
		m_kBoundingBox.ComputeFromData( m_kMesh.VBuffer );
    	
    	Vector3f[] kBoxCorners = new Vector3f[8];
    	m_kBoundingBox.GetBox().ComputeVertices( kBoxCorners );
    	m_kMaxBB = new Vector3f( -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE );
    	m_kMinBB = new Vector3f(  Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE );
    	for ( int i = 0; i < kBoxCorners.length; i++ )
    	{
        	meshToVolumeCoords(kBoxCorners[i]);
    		m_kMaxBB.Max( kBoxCorners[i] );
    		m_kMinBB.Min( kBoxCorners[i] );
    	}
	}
	
	/**
	 * Returns the min/max bounding box coordinates for the mesh in volume-index coordinates.
	 * @return the min/max bounding box coordinates for the mesh in volume-index coordinates.
	 */
	public Vector3f[] getMinMax()
	{
    	if ( m_kBoundingBox == null )
    	{
    		initBoundingBox();
    	}
    	return new Vector3f[]{ new Vector3f( m_kMinBB ), new Vector3f( m_kMaxBB ) };
	}
    
    
    /**
     * Test if the input point is inside the triangle mesh.
     * @param kP0 input point.
     * @return true if the point is inside the mesh, false otherwise.
     */
    public boolean testIntersection( Vector3f kP0, boolean bUseMask )
    {
    	if ( bUseMask )
    	{
    		return testIntersectionMask(kP0);
    	}
    	if ( m_kBoundingBox == null )
    	{
    		initBoundingBox();
    	}    	

    	// convert to 'mesh' coords...
    	Vector3f kP0Mesh = new Vector3f(kP0);    	
    	volumeToMeshCoords(kP0Mesh);

    	if ( m_kBoundingBox.Contains( kP0Mesh ) )
    	{
    		Vector3f[] directions = new Vector3f[5];
    		directions[0] = new Vector3f( (float)Math.random(), (float)Math.random(), (float)Math.random() );
    		directions[1] = new Vector3f( -(float)Math.random(), (float)Math.random(), (float)Math.random() );
    		directions[2] = new Vector3f( (float)Math.random(), -(float)Math.random(), (float)Math.random() );
    		directions[3] = new Vector3f( (float)Math.random(), (float)Math.random(), -(float)Math.random() );
    		directions[4] = new Vector3f( -(float)Math.random(), (float)Math.random(), -(float)Math.random() );
    		Line3f[] akLines = new Line3f[directions.length];
    		for ( int i = 0; i < directions.length; i++ )
    		{
    			directions[i].Normalize();
            	akLines[i] = new Line3f(kP0Mesh,directions[i]);
    		}
    		if ( testIntersections( kP0Mesh, akLines ) )
    		{
    			return true;
    		}
    	}
    	return false;
    }
    
    /**
     * Tests if the line segment specified by the two input points intersects the mesh.
     * @param kP0 first end-point of the line sement in volume-index coordinates.
     * @param kP1 second end-point of the line sement in volume-index coordinates.
     * @param bPrint flag to print debugging printouts.
     * @return true if the line segment intersects the mesh, false otherwise.
     */
    public boolean testIntersection( Vector3f kP0, Vector3f kP1, boolean bUseMask, boolean bPrint )
    {        
    	if ( m_kBoundingBox == null )
    	{
    		initBoundingBox();
    	}
    	
    	// convert to 'mesh' coords...
    	Vector3f kP0Mesh = new Vector3f(kP0);
    	Vector3f kP1Mesh = new Vector3f(kP1);
    	
    	volumeToMeshCoords(kP0Mesh);
    	volumeToMeshCoords(kP1Mesh);
    	
		if ( bPrint )
		{
			System.err.println( kP0Mesh + " inside BB? " + m_kBoundingBox.Contains( kP0Mesh ) );
			System.err.println( kP1Mesh + " inside BB? " + m_kBoundingBox.Contains( kP1Mesh ) );
		}

		if ( testIntersections( kP0Mesh, kP1Mesh ) )
		{
			return true;
		}
		else if ( bPrint )
		{
			System.err.println( "testIntersections " + kP0Mesh + " " + kP1Mesh + "  FAILED " );
		}
		/*
		if ( testIntersection( kP0, bUseMask ) && testIntersection( kP1, bUseMask ) )
		{
			return true;
		}
		else if ( bPrint )
		{
			System.err.println( "testIntersection " + kP0 + " " + testIntersection( kP0, bUseMask ) );
			System.err.println( "testIntersection " + kP1 + " " + testIntersection( kP1, bUseMask ) );
		}
		*/
		return false;
    }
    
    private BitSet m_kMask = null;
    private boolean testIntersectionMask( Vector3f kPos )
    {
    	if ( m_kMask == null )
    	{
    		m_kMask = createMask();
    	}
    	final int iDimX = m_kVolumeImageA.GetImage().getExtents()[0];
    	final int iDimY = m_kVolumeImageA.GetImage().getExtents()[1];
		return m_kMask.get( (int) (kPos.Z * iDimX * iDimY + kPos.Y * iDimX + kPos.X) );
    }
    
    
    /**
     * Converts the input point from volume-index coordinates into local mesh coordinates used to display the surface in the volume renderer.
     * The input position is overwritten in the process.
     * @param kVolume the input position.
     */
    private void volumeToMeshCoords(Vector3f kVolume)
    {
    	kVolume.Mult( m_kVolumeScale );
    	kVolume.Sub( m_kVolumeTrans );	
    	kVolume.Scale( m_fVolumeDiv );			
    }

    /**
     * Converts the input point from local mesh coordinates used to display the surface in the volume renderer into volume-index coordinates.
     * The input position is overwritten in the process.
     * @param kVolume the input position.
     */
    private void meshToVolumeCoordsA(Vector3f kMesh)
    {
    	kMesh.Scale( m_fVolumeMult );	
    	kMesh.Add( m_kVolumeTrans );			
    	kMesh.Scale( .5f );
    }

    /**
     * Converts the input point from local mesh coordinates used to display the surface in the volume renderer into volume-index coordinates.
     * The input position is overwritten in the process.
     * @param kVolume the input position.
     */
    private void meshToVolumeCoords(Vector3f kMesh)
    {
    	kMesh.Scale( m_fVolumeMult );	
    	kMesh.Add( m_kVolumeTrans );			
    	kMesh.Mult( m_kMeshScale );
    }


    /**
     * Converts the input point from local mesh coordinates used to display the surface in scanner coordinates.
     * The input position is overwritten in the process.
     * @param kVolume the input position.
     */
    private void meshToScannerCoords(Vector3f kMesh)
    {
    	meshToVolumeCoords(kMesh);    	
    	Vector3f kScanner = new Vector3f();
    	MipavCoordinateSystems.fileToScanner( kMesh, kScanner, m_kVolumeImageA.GetImage() );
    	kMesh.Copy(kScanner);
    }
    
    /**
     * Builds a list of cross-references, so that connections[i] contains all the vertices that are connected to vertex
     * at i.
     */
    @SuppressWarnings("unchecked")
    private HashSet[] buildConnections() {
        Iterator iter;
        int index;
        boolean addT1, addT2, addT3;

        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        HashSet<Integer>[] connections = new HashSet[iVQuantity];

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
                connections[iV0] = new HashSet<Integer>();
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
                connections[iV1] = new HashSet<Integer>();
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
                connections[iV2] = new HashSet<Integer>();
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
     * Compute the center of the surface.
     */
    private void ComputeCenter()
    {
        m_kCenter = new Vector3f();
        m_kCenterScanner = new Vector3f();
        Vector3f kPos = new Vector3f();
        for ( int i = 0; i < m_kMesh.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kMesh.VBuffer.GetPosition3( i, kPos );
            m_kCenter.Add( kPos );
            meshToScannerCoords(kPos);
            m_kCenterScanner.Add( kPos );
        }
        m_kCenter.Scale( 1.0f / m_kMesh.VBuffer.GetVertexQuantity() );
        m_kCenterScanner.Scale( 1.0f / m_kMesh.VBuffer.GetVertexQuantity() );
    }
    /**
     * Compute the average length of all the edges in the triangle mesh.
     */
    private float computeMeanEdgeLength( HashMap kEMap )
    {
        float fMeanEdgeLength = 0.0f;

        Iterator kEIter = kEMap.entrySet().iterator();
        Map.Entry kEntry = null;
        Vector3f kEdge = new Vector3f();
        while (kEIter.hasNext()) {
            kEntry = (Map.Entry) kEIter.next();

            Edge kE = (Edge) kEntry.getKey();

            Vector3f kV0 = m_kMesh.VBuffer.GetPosition3(kE.m_iV0);
            Vector3f kV1 = m_kMesh.VBuffer.GetPosition3(kE.m_iV1);
            kEdge.Sub( kV1, kV0 );
            fMeanEdgeLength += kEdge.Length();
        }

        fMeanEdgeLength /= kEMap.size();
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
            akVMean[i].Set(0.0f, 0.0f, 0.0f);
        }

        Vector3f kS = new Vector3f();
        
        for (int i = 0; i < iVertexQuantitaty; i++) {

            // compute the mean of the vertex neighbors
            // Point3f kMean = m_akVMean[i];
            UnorderedSetInt kAdj = akAdjacent[i];

            for (int j = 0; j < kAdj.getQuantity(); j++) {
                Vector3f kV0 = m_kMesh.VBuffer.GetPosition3(kAdj.get(j));
                akVMean[i].Add(kV0);
            }

            akVMean[i].Scale(1.0f / kAdj.getQuantity());

            // compute the normal and tangential components of mean-vertex
            Vector3f kV0 = m_kMesh.VBuffer.GetPosition3(i);
            kS.Sub( akVMean[i], kV0 );
            akSNormal[i].Scale( kS.Dot(akVNormal[i]), akVNormal[i] );
            akSTangent[i].Sub( kS, akSNormal[i] );

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
            akVNormal[i].Set(0.0f, 0.0f, 0.0f);
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
            kEdge1.Sub( kV1, kV0 );
            kEdge2.Sub( kV2, kV0 );
            kNormal.Cross( kEdge1, kEdge2 );

            // the triangle normal partially contributes to each vertex normal
            akVNormal[iV0].Add(kNormal);
            akVNormal[iV1].Add(kNormal);
            akVNormal[iV2].Add(kNormal);
        }

        for (int i = 0; i < iVertexQuantity; i++) {
            akVNormal[i].Normalize();
        }
    }
    /** Creates the scene graph. */
    private void CreateScene ( )
    {
        m_kVolumePreShader = new VolumePreRenderEffect(true, true, false);
        m_kVolumePreShaderTransparent = new VolumePreRenderEffect(true, true, true);
        m_kScene = new Node();

        m_kCull = new CullState();
        m_kScene.AttachGlobalState(m_kCull);

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kScene.AttachGlobalState(m_kAlpha);

        m_kWireframe = new WireframeState();
        m_kScene.AttachGlobalState(m_kWireframe);
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

            kSum.Set(0f,0f,0f);
            num = 0;
            m_kMesh.VBuffer.GetPosition3(i, kOriginalPos);

            // get all the verticies that are connected to this one (at i)
            for (Iterator iter = connections[i].iterator(); iter.hasNext();) {
                int index = ((Integer) iter.next()).intValue();

                m_kMesh.VBuffer.GetPosition3(index, kConnectionPos);

                // Sum of (xj - xi) where j ranges over all the points connected to xi
                // xj = m_kV2; xi = m_kV3
                kConnectionPos.Sub( kOriginalPos );
                kSum.Add( kConnectionPos );
                num++;
            }
            // xi+1 = xi + (alpha)*(sum of(points xi is connected to - xi))

            if (num > 1) {
                kSum.Scale( 1.0f / num );
            }

            kSum.Scale( fValue );
            kOriginalPos.Add( kSum );
            kVBuffer.SetPosition3(i, kOriginalPos);
        }

        for (int i = 0; i < iVQuantity; i++) {
            m_kMesh.VBuffer.SetPosition3(i, kVBuffer.GetPosition3(i) );
        }

        kVBuffer.dispose();
        kVBuffer = null;
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
        Vector3f kT = new Vector3f();
        Vector3f kS = new Vector3f();
        for (int i = 0; i < iVertexQuantity; i++) {
            Vector3f kV = m_kMesh.VBuffer.GetPosition3(i);

            // tangential update
            kT.Scale( 0.5f, akSTangent[i] );
            kV.Add( kT );

            // normal update
            float fUpdate2 = update2(i, afCurvature, fStiffness, afParams);

            kS.Scale(  fUpdate2, akSNormal[i] );
            kV.Add( kS );
            m_kMesh.VBuffer.SetPosition3(i, kV);
        }
    }   
    
    /**
     * Test if the input point is inside the mesh.
     * @param origin point to test for inside/outside mesh.
     * @param directions set of randomised directions for counting mesh-intersections (odd = inside, even = outside).
     * @return true when the point is inside the mesh, false otherwise.
     */
    private boolean testIntersections( Vector3f origin, Line3f[] akLines )
    {
    	int[] lineIntersectionCount = new int[akLines.length]; 
    	
    	
        // Compute intersections with the model-space triangles.
		Triangle3f kTriangle = new Triangle3f();
        int iTQuantity = m_kMesh.GetTriangleQuantity();
    	IntrLine3Triangle3f kIntr = new IntrLine3Triangle3f();
        
        int iV0, iV1, iV2;
        int[] aiTris = new int[3];
        
        for (int i = 0; i < iTQuantity; i++)
        {
            if (!m_kMesh.GetTriangle(i,aiTris) )
            {
                continue;
            }

            iV0 = aiTris[0];
            iV1 = aiTris[1];
            iV2 = aiTris[2];

    		m_kMesh.VBuffer.GetPosition3(iV0, kTriangle.V[0]);
    		m_kMesh.VBuffer.GetPosition3(iV1, kTriangle.V[1]);
    		m_kMesh.VBuffer.GetPosition3(iV2, kTriangle.V[2]);
            
            for ( int j = 0; j < akLines.length; j++ )
            {
            	kIntr.Line = akLines[j];
            	kIntr.Triangle = kTriangle;
            	if (kIntr.Find() && 0 <= kIntr.GetLineT() &&  kIntr.GetLineT() <= Float.MAX_VALUE )
            	{
            		lineIntersectionCount[j]++;
            	}
            }  	
        }
        int oddCount = 0;
        for ( int j = 0; j < akLines.length; j++ )
        {
        	if ( (lineIntersectionCount[j]%2) == 1 )
        	{
        		oddCount++;
        	}
        }
    	return ( oddCount >= (1 + akLines.length/2) );
    }

    /**
     * Tests if the line segment determined by the two input end-points intersects the mesh.
     * @param kP0 end-point of the line segment.
     * @param kP1 end-point of the line segment.
     * @return true if the line segment intersects the mesh.
     */
    private boolean testIntersections( final Vector3f kP0, final Vector3f kP1 )
    {
    	// Compute intersections with the model-space triangles.
    	int iTQuantity = m_kMesh.GetTriangleQuantity();
    	// Compute intersections with the model-space triangles.
		Triangle3f kTriangle = new Triangle3f();
		IntrSegment3Triangle3f kIntr = new IntrSegment3Triangle3f();
		Segment3f kSegment = new Segment3f(kP0, kP1);
		kIntr.Segment = kSegment;
		
		int iV0, iV1, iV2;
		int[] aiTris = new int[3];
    	for (int i = 0; i < iTQuantity; i++)
    	{
    		if (!m_kMesh.GetTriangle(i,aiTris) )
    		{
    			continue;
    		}

    		iV0 = aiTris[0]; 
    		iV1 = aiTris[1];
    		iV2 = aiTris[2];

    		m_kMesh.VBuffer.GetPosition3(iV0, kTriangle.V[0]);
    		m_kMesh.VBuffer.GetPosition3(iV1, kTriangle.V[1]);
    		m_kMesh.VBuffer.GetPosition3(iV2, kTriangle.V[2]);

    		kIntr.Triangle = kTriangle;
    		if ( kIntr.Test() )
    		{
    			return true;
    		}
    	}
    	return false;
    }
    
    
}
