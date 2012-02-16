package gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import WildMagic.LibFoundation.Intersection.IntrRay2Segment2f;
import WildMagic.LibFoundation.Intersection.IntrSegment2Circle2f;
import WildMagic.LibFoundation.Mathematics.Circle2f;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Polynomial1f;
import WildMagic.LibFoundation.Mathematics.Ray2f;
import WildMagic.LibFoundation.Mathematics.Segment2f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Meshes.BasicMesh;
import WildMagic.LibFoundation.Meshes.ConformalMap;
import WildMagic.LibFoundation.Meshes.EdgeKey;
import WildMagic.LibFoundation.Meshes.MeshCurvature;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public class MjCorticalMesh_WM {

    /**
     * DOCUMENT ME!
     */
    public static class Polylines {

        /** Array of mesh points. */
        VertexBuffer kMVertex;

        /** Array of plane points. */
        VertexBuffer kPVertex;

        /** Array of sphere points. */
        VertexBuffer kSVertex;
    }

    /**
     * support for point-in-triangle tests; The akVertex array must have length 3.
     *
     * @param   akVertex  DOCUMENT ME!
     * @param   kP        DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static boolean contains(Vector2f[] akVertex, Vector2f kP) {
        /* assert:  <V0,V1,V2> is clockwise ordered */

        final float fEpsilon = 1e-05f;
        Vector2f kV1mV0 = new Vector2f();
        Vector2f kInnerNormal = new Vector2f();
        Vector2f kDiff = new Vector2f();

        for (int i0 = 2, i1 = 0; i1 < 3; i0 = i1++) {
        	kV1mV0.Sub( akVertex[i1], akVertex[i0] );
            kInnerNormal.Perp(kV1mV0);
            kDiff.Sub( kP, akVertex[i0] );
            kInnerNormal.Normalize();
            kDiff.Normalize();

            float fCos = kInnerNormal.Dot(kDiff);

            if (fCos < -fEpsilon) {
                return false;
            }
        }

        return true;
    }

    /** DOCUMENT ME! */
    private float[] m_afAvrConvexity;

    /** DOCUMENT ME! */
    private float[] m_afMeanCurvature;

    /** projection of sphere onto cylinder. */
    private Vector3f[] m_akCylinder = null;

    /** DOCUMENT ME! */
    private BasicMesh.Edge[] m_akEdge = null;

    /** Conformal mapping to a plane. The (u,v) points correspond to the (x,y,z) mesh points. */
    private Vector2f[] m_akPlane = null;

    /** Conformal mapping to a sphere. rho is the radius of the stereographic sphere. */
    private Vector3f[] m_akSphere = null;

    /** DOCUMENT ME! */
    private BasicMesh.Triangle[] m_akTriangle = null;

    /** DOCUMENT ME! */
    private BasicMesh.Vertex[] m_akVertex = null;

    /** DOCUMENT ME! */
    private float m_fMaxAvrConvexity;

    /** DOCUMENT ME! */
    private float m_fMaxDistance;

    /** DOCUMENT ME! */
    private float m_fMaxMeanCurvature;

    /** DOCUMENT ME! */
    private float m_fMinAvrConvexity;

    /** DOCUMENT ME! */
    private float m_fMinDistance;

    /** DOCUMENT ME! */
    private float m_fMinMeanCurvature;

    /** DOCUMENT ME! */
    private float m_fRho;

    /** DOCUMENT ME! */
    private float m_fSurfaceArea;

    /** DOCUMENT ME! */
    private int m_iEQuantity;

    /** The index of the puncture triangle for computing the conformal mapping:. */
    private int m_iPunctureTri = 0;
    /** vertex-vertex distances measured along edge paths. */
    private HashMap<EdgeKey, Float> m_kDistance = new HashMap<EdgeKey, Float>(); /* map MjEdgeKey to Float */
    
    /** surface inflation. */
    private HashMap<EdgeKey, Float> m_kInitDistance = new HashMap<EdgeKey, Float>(); /* map MjEdgeKey to Float */
    private TriMesh m_kMesh;
    private BasicMesh m_kBasicMesh;
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    private Vector3f[] m_akPoint;
    
    private TriMesh m_kSphereMesh = null;

    private TriMesh m_kCylinderMesh = null;

    /**
     * Creates a new MjCorticalMesh object.
     */
    public MjCorticalMesh_WM( TriMesh kMesh, Vector3f kCenter)
    {
        m_kMesh = kMesh;
        int iVQuantity = kMesh.VBuffer.GetVertexQuantity();
        int iTQuantity = kMesh.GetTriangleQuantity();
        m_akPoint = new Vector3f[iVQuantity];
        for ( int i = 0; i < iVQuantity; i++ )
        {
            m_akPoint[i] = new Vector3f();
            m_kMesh.VBuffer.GetPosition3(i, m_akPoint[i] );
        }
        m_kBasicMesh = new BasicMesh(iVQuantity,m_akPoint,iTQuantity,m_kMesh.IBuffer.GetData());

        m_iEQuantity = m_kBasicMesh.GetEQuantity();
        m_akEdge = m_kBasicMesh.GetEdges();
        m_akTriangle = m_kBasicMesh.GetTriangles();
        m_akVertex = m_kBasicMesh.GetVertices();
        
        m_fMinMeanCurvature = 0.0f;
        m_fMaxMeanCurvature = 0.0f;
        m_afMeanCurvature = null;
        m_akPlane = null;
        m_akSphere = null;
        m_akCylinder = null;
        m_fMinAvrConvexity = 0.0f;
        m_fMaxAvrConvexity = 0.0f;
        m_afAvrConvexity = null;
    }
    
    
   
    public boolean CheckManifold()
    {
    	if ( !m_kBasicMesh.IsValid() )
    	{
    		return false;
    	}
        for (int i = 0; i < m_kBasicMesh.GetEQuantity(); i++) {
            if ( m_akEdge[i].T[1] == -1 )
            {
            	return false;
            }
        }
        return true;
    }

    /**
     * conformal mapping of mesh to plane and sphere, projection to cylinder.
     */
    public void computeConformalMapping()
    {
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        ConformalMap kConformalMap = new ConformalMap ( m_kBasicMesh, iVQuantity, m_akPoint,
                m_kMesh.GetTriangleQuantity(), m_iPunctureTri );
        m_akSphere = kConformalMap.GetSphereCoordinates();
        m_akPlane = kConformalMap.GetPlaneCoordinates();
        m_fRho = kConformalMap.GetSphereRadius();

        /* Project the sphere onto a cylinder.  The cylinder is centered at the */
        /* origin and has axis direction (0,0,1).  The radius of the cylinder is */
        /* the radius of the sphere, 1.  The height of the cylinder is 2 since z */
        /* varies from -1 to 1. */
        m_akCylinder = new Vector3f[iVQuantity];
        for (int i = 0; i < iVQuantity; i++) {
            m_akCylinder[i] = new Vector3f();
            m_akCylinder[i].X = -(float) Math.atan2(m_akSphere[i].Y, m_akSphere[i].X);
            m_akCylinder[i].Y = m_akSphere[i].Z;
            m_akCylinder[i].Z = 0.0f;
        }
        if ( m_kSphereMesh != null )
        {
            m_kSphereMesh.dispose();
            m_kSphereMesh = null;
        }
        if ( m_kCylinderMesh != null )
        {
            m_kCylinderMesh.dispose();
            m_kCylinderMesh = null;
        }
    }

    /**
     * Computes the distances of vertex pairs <V0,V1> for which an edge-path exists with at most iSize edges.
     *
     * @param  iSize  DOCUMENT ME!
     */
    public void computeDistances(int iSize) {
        assert (iSize >= 1);

        if (iSize < 1) {
            return;
        }

        m_kDistance.clear();
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        for (int iSource = 0; iSource < iVQuantity; iSource++) {
            computeDistance(iSize, iSource, false);
        }

        computeExtremes();
    }

    /**
     * mean curvature.
     */
    public void computeMeanCurvature( ) {
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        m_afMeanCurvature = new float[iVQuantity];
        int iTQuantity = m_kMesh.GetTriangleQuantity(); 
        int[] aiConnect = m_kMesh.IBuffer.GetData();
        MeshCurvature kMG = new MeshCurvature(iVQuantity, m_akPoint, iTQuantity, aiConnect);

        float[] afMinCurv = kMG.GetMinCurvatures();
        float[] afMaxCurv = kMG.GetMaxCurvatures();
        m_fMinMeanCurvature = afMinCurv[0] + afMaxCurv[0];
        m_fMaxMeanCurvature = m_fMinMeanCurvature;

        for (int i = 0; i < iVQuantity; i++) {
            m_afMeanCurvature[i] = afMinCurv[i] + afMaxCurv[i];

            if (m_afMeanCurvature[i] < m_fMinMeanCurvature) {
                m_fMinMeanCurvature = m_afMeanCurvature[i];
            } else if (m_afMeanCurvature[i] > m_fMaxMeanCurvature) {
                m_fMaxMeanCurvature = m_afMeanCurvature[i];
            }
            //System.err.println( m_afMeanCurvature[i] + " " + afMinCurv[i] + " " + afMaxCurv[i] );
        }
    }

    /**
     * compute vertex normals as averages of triangle normals.
     */
    public void computeNormals() {
        m_kMesh.UpdateMS();
    }

    /**
     * surface area of the mesh (input mesh is closed, manifold).
     */
    public void computeSurfaceArea() {
        m_fSurfaceArea = 0.0f;

        Vector3f kE0 = new Vector3f();
        Vector3f kE1 = new Vector3f();
        Vector3f kCross = new Vector3f();

        int[] aiConnect = m_kMesh.IBuffer.GetData();
        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++) {

            Vector3f kV0 = m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * i) + 0] );
            Vector3f kV1 = m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * i) + 1] );
            Vector3f kV2 = m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * i) + 2] );
            kE0.Sub( kV1, kV0 );
            kE1.Sub( kV2, kV0 );
            kCross.Cross( kE0, kE1 );
            m_fSurfaceArea += 0.5f * kCross.Length();
        }
    }
    

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        for (int i = 0; i < iVQuantity; i++) {

            if (m_akPlane != null) {
                m_akPlane[i] = null;
            }

            if (m_akSphere != null) {
                m_akSphere[i] = null;
            }

            if (m_akCylinder != null) {
                m_akCylinder[i] = null;
            }
        }
        if (m_akPlane != null) {
            m_akPlane = null;
        }

        if (m_akSphere != null) {
            m_akSphere = null;
        }

        if (m_akCylinder != null) {
            m_akCylinder = null;
        }

        if (m_akVertex != null) {

            for (int i = 0; i < m_akVertex.length; i++) {
                m_akVertex[i] = null;
            }

            m_akVertex = null;
        }

        if (m_akEdge != null) {

            for (int i = 0; i < m_akEdge.length; i++) {
                m_akEdge[i] = null;
            }

            m_akEdge = null;
        }

        if (m_akTriangle != null) {

            for (int i = 0; i < m_akTriangle.length; i++) {
                m_akTriangle[i] = null;
            }

            m_akTriangle = null;
        }
    }

    /**
     * surface inflation operation 0 = initialize 1 = inflate 2 = terminate.
     *
     * @param   iOperation  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float doInflation(int iOperation) {
        float fError = 0.0f;
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        switch (iOperation) {
            
            case 0:
                m_afAvrConvexity = new float[iVQuantity];
                for (int i = 0; i < iVQuantity; i++) {
                    m_afAvrConvexity[i] = 0.0f;
                }

                computeDistances(1);
                m_kInitDistance = m_kDistance;
                computeNormals();
                fError = doInflationStep();
                break;

            case 1:
                fError = doInflationStep();
                break;

            case 2:
                if (null != m_afAvrConvexity) {
                    m_fMinAvrConvexity = m_afAvrConvexity[0];
                    m_fMaxAvrConvexity = m_fMinAvrConvexity;

                    for (int i = 1; i < iVQuantity; i++) {

                        if (m_afAvrConvexity[i] < m_fMinAvrConvexity) {
                            m_fMinAvrConvexity = m_afAvrConvexity[i];
                        } else if (m_afAvrConvexity[i] > m_fMaxAvrConvexity) {
                            m_fMaxAvrConvexity = m_afAvrConvexity[i];
                        }
                    }
                }

                fError = 0.0f;
                break;

            default:
                assert (false);
                break;
        }

        return fError;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getAvrConvexity() {
        return m_afAvrConvexity;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public TriMesh getCylinder() {
        if ( m_kCylinderMesh == null )
        {
            Attributes kAttr = new Attributes();
            kAttr.SetPChannels( 3 );
            kAttr.SetCChannels( 0, 4 );
            kAttr.SetNChannels( 3 );
            int iVQuantity = m_akCylinder.length;
            VertexBuffer kVBuffer = new VertexBuffer( kAttr, iVQuantity );
            for ( int i = 0; i < iVQuantity; i++ )
            {
                kVBuffer.SetPosition3( i, m_akCylinder[i] );
                kVBuffer.SetColor4( 0, i, m_kMesh.VBuffer.GetColor4(0, i) );
            }
            IndexBuffer kIBuffer = new IndexBuffer( m_kMesh.IBuffer );
            m_kCylinderMesh = new TriMesh( kVBuffer, kIBuffer );
            m_kCylinderMesh.SetName( m_kMesh.GetName() + "Cylinder" );
        }
        return m_kCylinderMesh;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f[] getCylinderCoordinates() {
        return m_akCylinder;
    }

    /**
     * Vertex-vertex distance measured along the shortest edge-path connecting the two vertices.
     *
     * @param   iV0  DOCUMENT ME!
     * @param   iV1  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getDistance(int iV0, int iV1) {
        Float kDistance = m_kDistance.get(new EdgeKey(iV0, iV1));

        if (null != kDistance) {
            return kDistance.floatValue();
        }

        /* vertices not within each others neighborhood; */
        /* return invalid distance */
        return -1.0f;
    }


    /**
     * Produce polylines that are superimposed on the input cortical mesh, sphere, and plane. The znormal value
     * indicates the z-slice on the unit sphere that is mapped to the polyline on the mesh. This value is in (-1,1). The
     * angle value indicates the theta-slice on the unit sphere, theta measured as a positive angle counterclockwise
     * from the x-axis towards the y-axis. This value is in [0,2*pi]. The bias value adds a small amount to the polyline
     * to raise it above the mesh surface to avoid z-buffer fighting.
     *
     * @param   fZNormal  DOCUMENT ME!
     * @param   fMBias    DOCUMENT ME!
     * @param   fSBias    DOCUMENT ME!
     * @param   fPBias    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Polylines getLatitude(float fZNormal, float fMBias, float fSBias, float fPBias) {

        /* Latitude circle in complex plane.  The radius is chosen so that the */
        /* latitude circles are uniformly spaced on the stereographic sphere in */
        /* the z-direction. */
        Circle2f kLatitude = new Circle2f();
        kLatitude.Center.Copy(Vector2f.ZERO);
        kLatitude.Radius = (float) Math.sqrt((1.0f + fZNormal) / (1.0f - fZNormal)) * getStereographicRadius();
        /* sorted t-values for intersections of circle */
        /* with triangle mesh edges */
        TreeMap<Float, Vector3f> kIntrMesh = new TreeMap<Float, Vector3f>(); /* map<float,Point3f> */
        TreeMap<Float, Vector3f> kIntrSphere = new TreeMap<Float, Vector3f>(); /* map<float,Point3f> */

        Segment2f kEdge = new Segment2f();
        Vector3f kU1mU0 = new Vector3f();
        Vector3f kU2mU0 = new Vector3f();
        Vector3f kN0 = new Vector3f();
        Vector3f kN1 = new Vector3f();
        Vector3f kNAvr = new Vector3f();
        Vector3f kNormal = new Vector3f();
        Vector2f kDiff = new Vector2f();

        int[] aiConnect = m_kMesh.IBuffer.GetData();
        for (int i = 0; i < m_iEQuantity; i++) {

            /* line segment representing the edge */
            int iV0 = m_akEdge[i].V[0];
            int iV1 = m_akEdge[i].V[1];
            Vector2f kV0 = m_akPlane[iV0];
            Vector2f kV1 = m_akPlane[iV1];
            kEdge.Origin.Copy(kV0);
            kEdge.Direction.Sub( kV1, kV0 );

            /* compute intersection of ray and segment */
            Vector2f[] akP = new Vector2f[]{ new Vector2f(), new Vector2f() };
            int iCount = IntrSegment2Circle2f.FindIntersection(kEdge, kLatitude, akP);

            if (iCount > 0) {

                /* get normal for sharing triangle */
                int iTriangle = m_akEdge[i].T[0];               
                kN0 = m_kMesh.VBuffer.GetNormal3(aiConnect[(3 * iTriangle) + 0]);
                kN0.Add(m_kMesh.VBuffer.GetNormal3(aiConnect[(3 * iTriangle) + 1]));
                kN0.Add(m_kMesh.VBuffer.GetNormal3(aiConnect[(3 * iTriangle) + 2]));
                kN0.Normalize();
                
                /* get normal for sharing triangle */
                iTriangle = m_akEdge[i].T[1];
                kN1 = m_kMesh.VBuffer.GetNormal3(aiConnect[(3 * iTriangle) + 0]);
                kN1.Add(m_kMesh.VBuffer.GetNormal3(aiConnect[(3 * iTriangle) + 1]));
                kN1.Add(m_kMesh.VBuffer.GetNormal3(aiConnect[(3 * iTriangle) + 2]));
                kN1.Normalize();
                
                /* average normal */
                kNAvr.Add( kN0, kN1 );
                kNAvr.Normalize();

                Vector3f kPos0 = new Vector3f();
                Vector3f kPos1 = new Vector3f();
                for (int j = 0; j < iCount; j++) {

                    /* Use the angle formed by the intersection point with the */
                    /* positive x-axis as the map key for sorting. */
                    float fAngle = (float) Math.atan2(akP[j].Y, akP[j].X);

                    /* determine the edge parameter at the intersection */
                    kDiff.Sub( akP[j], kV0 );

                    float fNumer = kDiff.Dot(kEdge.Direction);
                    float fDenom = kEdge.Direction.SquaredLength();
                    float fS = fNumer / fDenom;

                    m_kMesh.VBuffer.GetPosition3( iV0, kPos0 );
                    m_kMesh.VBuffer.GetPosition3( iV1, kPos1 );

                    /* construct the edge point on the original mesh */
                    Vector3f kQ = new Vector3f();
                    kQ.Sub( kPos1, kPos0 );
                    kQ.Scale(fS);
                    kQ.Add(kPos0);
                    
                    /* To avoid z-buffer biasing problems, lift the point in the */
                    /* direction of the average of the normals for the triangles */
                    /* sharing the edge. */
                    kQ.ScaleAdd(fMBias, kNAvr, kQ);

                    /* save the point in an ordered map */
                    kIntrMesh.put(new Float(fAngle), kQ);    
                    
                    /* construct the edge point on the sphere */
                    kQ = new Vector3f();
                    kQ.Sub( m_akSphere[iV1], m_akSphere[iV0] );
                    kQ.Scale(fS);
                    kQ.Add(m_akSphere[iV0]);

                    /* To avoid z-buffer biasing problems, lift the point in the */
                    /* direction of sphere normal. */
                    kNormal.Copy(kQ);
                    kNormal.Normalize();
                    kQ.ScaleAdd(fSBias, kNormal, kNormal);
                    
                    /* save the point in an ordered map */
                    kIntrSphere.put(new Float(fAngle), kQ);
                }
            }
        }
        
        Polylines kPolylines = new Polylines();
        assert (kIntrMesh.size() > 0);
        
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0, 3);
        kPolylines.kMVertex = new VertexBuffer( kAttr, kIntrMesh.size());
        kPolylines.kSVertex = new VertexBuffer( kAttr, kIntrSphere.size());

        Iterator kIM = kIntrMesh.entrySet().iterator();
        Iterator kIS = kIntrSphere.entrySet().iterator();

        for (int i = 0; kIM.hasNext(); i++) {
            kPolylines.kMVertex.SetPosition3( i, (Vector3f) (((Map.Entry) kIM.next()).getValue()) );
            kPolylines.kMVertex.SetColor3( 0, i, 1.0f, 1.0f, 1.0f );
            
            kPolylines.kSVertex.SetPosition3( i, (Vector3f) (((Map.Entry) kIS.next()).getValue()) );
            kPolylines.kSVertex.SetColor3( 0, i, 1.0f, 1.0f, 1.0f );
        }

        kPolylines.kPVertex = new VertexBuffer( kAttr, 2);
        kPolylines.kPVertex.SetPosition3( 0, new Vector3f(+(float) Math.PI, fZNormal, -fPBias) );
        kPolylines.kPVertex.SetColor3( 0, 0, 1.0f, 1.0f, 1.0f );
        kPolylines.kPVertex.SetPosition3( 1, new Vector3f(-(float) Math.PI, fZNormal, -fPBias) );
        kPolylines.kPVertex.SetColor3( 0, 1, 1.0f, 1.0f, 1.0f );

        return kPolylines;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   fAngle  DOCUMENT ME!
     * @param   fMBias  DOCUMENT ME!
     * @param   fSBias  DOCUMENT ME!
     * @param   fPBias  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Polylines getLongitude(float fAngle, float fMBias, float fSBias, float fPBias) {
        int[] aiConnect = m_kMesh.IBuffer.GetData();
        
        final Vector2f kPoint2fZero = new Vector2f(0.0f, 0.0f);

        /* longitude ray in the complex plane */
        Ray2f kLongitude = new Ray2f();
        kLongitude.Origin.Copy(kPoint2fZero);
        kLongitude.Direction.X = (float) Math.cos(fAngle);
        kLongitude.Direction.Y = (float) Math.sin(fAngle);

        /* sorted t-values for intersections of ray with edges */
        TreeMap<Float, Vector3f> kIntrMesh = new TreeMap<Float, Vector3f>(); /* map<float,Point3f> */
        TreeMap<Float, Vector3f> kIntrSphere = new TreeMap<Float, Vector3f>(); /* map<float,Point3f> */

        /* ***** add the ray origin to the polyline ***** */
        int iContain = getContainingTriangle(kPoint2fZero, 0);
        assert (iContain > 0);

        Vector3f kBary = getBarycentric(kPoint2fZero, iContain);
        int iV0 = aiConnect[(3 * iContain) + 0];
        int iV1 = aiConnect[(3 * iContain) + 1];
        int iV2 = aiConnect[(3 * iContain) + 2];
        Vector3f kU1mU0 = new Vector3f();
        Vector3f kU2mU0 = new Vector3f();
        Vector3f kNormal = new Vector3f();

        /* get normal for containing triangle */
        Vector3f kU0 = m_kMesh.VBuffer.GetPosition3(iV0);
        Vector3f kU1 = m_kMesh.VBuffer.GetPosition3(iV1);
        Vector3f kU2 = m_kMesh.VBuffer.GetPosition3(iV2);
        kU1mU0.Sub( kU1, kU0 );
        kU2mU0.Sub( kU2, kU0 );
        kNormal.UnitCross( kU1mU0, kU2mU0 );

        kU0.Scale(kBary.X);
        kU1.Scale(kBary.Y);
        kU2.Scale(kBary.Z);
        /* compute point on original mesh */
        Vector3f kQ = new Vector3f();
        kQ.Add( kU0, kU1 );
        kQ.Add( kU2 );

        /* lift slightly off the surface */
        kNormal.Scale(fMBias);
        kQ.Add( kNormal );
        kIntrMesh.put(new Float(0.0f), kQ);

        /* repeat calculations for sphere */
        kU0 = m_akSphere[iV0];
        kU1 = m_akSphere[iV1];
        kU2 = m_akSphere[iV2];      
        kU0.Scale(kBary.X);
        kU1.Scale(kBary.Y);
        kU2.Scale(kBary.Z);
        kQ.Copy( Vector3f.ZERO );
        kQ.Add( kU0, kU1 );
        kQ.Add( kU2 );
        kNormal.Copy(kQ);
        kNormal.Normalize();
        kNormal.Scale(fMBias);
        kQ.Add( kNormal );
        kIntrSphere.put(new Float(0.0f), kQ);
        /* ***** end add ray origin ***** */

        /* ***** add the ray infinity to the polyline ***** */
        iContain = 0; /* TO DO: if puncture triangle is changed, change this */

        /* get normal for containing triangle */
        m_kMesh.VBuffer.GetPosition3( iV0, kU0 );
        m_kMesh.VBuffer.GetPosition3( iV1, kU1 );
        m_kMesh.VBuffer.GetPosition3( iV2, kU2 );
        kU1mU0.Sub( kU1, kU0 );
        kU2mU0.Sub( kU2, kU0 );
        kNormal.UnitCross( kU1mU0, kU2mU0 );

        /* compute point on original mesh */
        kQ.Copy( Vector3f.ZERO );
        kQ.Add(kU0);
        kQ.Add(kU1);
        kQ.Add(kU2);
        kQ.Scale(1.0f / 3.0f);

        /* lift slightly off the surface */
        kNormal.Scale(fMBias);
        kQ.Add( kNormal );
        kIntrMesh.put(new Float(Float.MAX_VALUE), kQ);

        /* repeat calculations for sphere */
        kU0 = m_akSphere[iV0];
        kU1 = m_akSphere[iV1];
        kU2 = m_akSphere[iV2];
        kQ.Copy( Vector3f.ZERO );
        kQ.Add(kU0);
        kQ.Add(kU1);
        kQ.Add(kU2);
        kQ.Scale(1.0f / 3.0f);
        kNormal.Copy(kQ);
        kNormal.Normalize();
        kNormal.Scale(fSBias);
        kQ.Add( kNormal );
        kIntrSphere.put(new Float(Float.MAX_VALUE), kQ);
        /* ***** end add ray origin ***** */

        float[] afT = new float[2];
        Vector3f kNormal0 = new Vector3f();
        Vector3f kNormal1 = new Vector3f();
        Vector2f kDiff = new Vector2f();

        for (int i = 0; i < m_iEQuantity; i++) {

            /* line segment representing the edge */
            iV0 = m_akEdge[i].V[0];
            iV1 = m_akEdge[i].V[1];

            Vector2f kV0 = m_akPlane[iV0];
            Vector2f kV1 = m_akPlane[iV1];
            Segment2f kEdge = new Segment2f();
            kEdge.Origin.Copy(kV0);
            kEdge.Direction.Sub( kV1, kV0 );

            /* compute intersection of ray and segment */
            int iCount = IntrRay2Segment2f.FindIntersection(kLongitude, kEdge, afT);

            if (iCount > 0) {

                /* get normal for sharing triangle */
                int iTriangle = m_akEdge[i].T[0];
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 0], kU0 );
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 1], kU1 );
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 2], kU2 );
                kU1mU0.Sub( kU1, kU0 );
                kU2mU0.Sub( kU2, kU0 );
                kNormal0.UnitCross( kU1mU0, kU2mU0 );

                /* get normal for sharing triangle */
                iTriangle = m_akEdge[i].T[1];
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 0], kU0 );
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 1], kU1 );
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 2], kU2 );
                kU1mU0.Sub( kU1, kU0 );
                kU2mU0.Sub( kU2, kU0 );
                kNormal1.UnitCross( kU1mU0, kU2mU0 );

                kNormal.Add( kNormal0, kNormal1 );
                kNormal.Normalize();

                Vector3f kPos0 = new Vector3f();
                Vector3f kPos1 = new Vector3f();
                for (int j = 0; j < iCount; j++) {

                    /* afT[j] is the ray parameter, need to compute the edge */
                    /* parameter */
                    kDiff.Copy(kLongitude.Direction);
                    kDiff.Scale(afT[j]);
                    kDiff.Sub(kV0);

                    float fNumer = kDiff.Dot(kEdge.Direction);
                    float fDenom = kEdge.Direction.SquaredLength();
                    float fS = fNumer / fDenom;

                    m_kMesh.VBuffer.GetPosition3( iV0, kPos0 );
                    m_kMesh.VBuffer.GetPosition3( iV1, kPos1 );

                    /* construct the edge point on the original mesh */
                    kQ = new Vector3f();
                    kQ.Copy( Vector3f.ZERO );
                    kQ.Sub( kPos1, kPos0 );
                    kQ.Scale(fS);
                    kQ.Add(kPos0);

                    /* To avoid z-buffer biasing problems, lift the point in the */
                    /* direction of the average of the normals for the triangles */
                    /* sharing the edge. */
                    kNormal.Scale(fMBias);
                    kQ.Add( kNormal );

                    kIntrMesh.put(new Float(afT[j]), kQ);

                    /* repeat the construction for sphere */
                    kQ = new Vector3f();
                    kQ.Copy( Vector3f.ZERO );
                    kQ.Sub( m_akSphere[iV1], m_akSphere[iV0] );
                    kQ.Scale(fS);
                    kQ.Add(m_akSphere[iV0]);
                    kNormal.Copy(kQ);
                    kNormal.Normalize();
                    kNormal.Scale(fSBias);
                    kQ.Add( kNormal );
                    kIntrSphere.put(new Float(afT[j]), kQ);
                }
            }
        }

        Polylines kPolylines = new Polylines();
        assert (kIntrMesh.size() > 0);
        
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0, 3);
        kPolylines.kMVertex = new VertexBuffer( kAttr, kIntrMesh.size());
        kPolylines.kSVertex = new VertexBuffer( kAttr, kIntrSphere.size());

        Iterator kIM = kIntrMesh.entrySet().iterator();
        Iterator kIS = kIntrSphere.entrySet().iterator();

        for (int i = 0; kIM.hasNext(); i++) {
            kPolylines.kMVertex.SetPosition3( i, (Vector3f) (((Map.Entry) kIM.next()).getValue()) );
            kPolylines.kMVertex.SetColor3( 0, i, 1.0f, 1.0f, 1.0f );
            
            kPolylines.kSVertex.SetPosition3( i, (Vector3f) (((Map.Entry) kIS.next()).getValue()) );
            kPolylines.kSVertex.SetColor3( 0, i, 1.0f, 1.0f, 1.0f );
        }

        fAngle -= Math.PI;
        kPolylines.kPVertex = new VertexBuffer( kAttr, 2);
        kPolylines.kPVertex.SetPosition3( 0, new Vector3f(fAngle, -1.0f, -fPBias) );
        kPolylines.kPVertex.SetColor3( 0, 0, 1.0f, 1.0f, 1.0f );
        kPolylines.kPVertex.SetPosition3( 1, new Vector3f(fAngle, +1.0f, -fPBias) );
        kPolylines.kPVertex.SetColor3( 0, 1, 1.0f, 1.0f, 1.0f );

        return kPolylines;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxMeanCurvature() {
        return m_fMaxMeanCurvature;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getMeanCurvature() {
        return m_afMeanCurvature;
    }

    public TriMesh getMesh()
    {
        return m_kMesh;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMinMeanCurvature() {
        return m_fMinMeanCurvature;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public TriMesh getSphere() {
        if ( m_kSphereMesh == null )
        {
            Attributes kAttr = new Attributes();
            kAttr.SetPChannels( 3 );
            kAttr.SetCChannels( 0, 4 );
            kAttr.SetNChannels( 3 );
            int iVQuantity = m_akSphere.length;
            VertexBuffer kVBuffer = new VertexBuffer( kAttr, iVQuantity );
            for ( int i = 0; i < iVQuantity; i++ )
            {
                kVBuffer.SetPosition3( i, m_akSphere[i] );
                kVBuffer.SetColor4( 0, i, m_kMesh.VBuffer.GetColor4(0, i) );
            }
            IndexBuffer kIBuffer = new IndexBuffer( m_kMesh.IBuffer );
            m_kSphereMesh = new TriMesh( kVBuffer, kIBuffer );
            m_kSphereMesh.SetName( m_kMesh.GetName() + "Sphere" );
        }
        return m_kSphereMesh;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getSurfaceArea() {
        return m_fSurfaceArea;
    }

    
    public int getVQuantity()
    {
        return m_kMesh.VBuffer.GetVertexQuantity();
    }


    public void setColor( int i, float fRed, float fGreen, float fBlue )
    {
        m_kMesh.VBuffer.SetColor4( 0, i, fRed, fGreen, fBlue, 1.0f );
    }

    /**
     * Set the index of the triangle used to "puncture" the mesh before the conformal mapping is computed. The triangle
     * is found based on the three index values that describe the triangle:
     *
     * @param  aiIndex  DOCUMENT ME!
     */
    public void setPunctureTriangle(int[] aiIndex) {
        m_iPunctureTri = 0;

        /* If aiIndex is not set, use the default value of 0 for the puncture
         * triangle */
        if (aiIndex == null) {
            return;
        }

        /* Search through the triangle list, find the triangle that matches
         * the aiIndex input: */
        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++) {
            boolean bfound = true;

            for (int j = 0; j < aiIndex.length; j++) {

                if (m_akTriangle[i].V[j] != aiIndex[j]) {
                    bfound = false;

                    break;
                }
            }

            if (bfound) {
                m_iPunctureTri = i;
            }
        }
    }

    public void updateMesh( boolean bUpdateNormals )
    {
        if ( bUpdateNormals )
        {
            m_kMesh.UpdateMS();
        }
        m_kMesh.VBuffer.Release();
    }

    /**
     * compute distances within specified neighborhood size (size >= 1).
     *
     * @param  iSize       DOCUMENT ME!
     * @param  iSource     DOCUMENT ME!
     * @param  bInitiator  DOCUMENT ME!
     */
    private void computeDistance(int iSize, int iSource, boolean bInitiator) {

        if (bInitiator) {
            m_fMinDistance = -1.0f;
            m_fMaxDistance = -1.0f;
            m_kDistance.clear();
        }

        /* start distance calculations from source vertex */
        Set<Integer> kInterior = new HashSet<Integer>(); /* set of int */
        kInterior.add(new Integer(iSource));

        /* compute distances to all 1-neighbors */
        BasicMesh.Vertex kSource = m_akVertex[iSource];
        int i, iNbr;
        Set<Integer> kBoundary = new HashSet<Integer>(); /* set of int */
        Vector3f kDiff = new Vector3f();
        for (i = 0; i < kSource.VQuantity; i++) {
            iNbr = kSource.V[i];
            kBoundary.add(new Integer(iNbr));

            EdgeKey kKey = new EdgeKey(iSource, iNbr);

            if (!m_kDistance.containsKey(kKey)) {
            	kDiff.Sub( m_kMesh.VBuffer.GetPosition3(iNbr),
            			   m_kMesh.VBuffer.GetPosition3(iSource) );
                float fDist = kDiff.Length();
                m_kDistance.put(kKey, new Float(fDist));
            }
        }

        /* Compute distances to farther neighbors using Dijkstra's algorithm. */
        /* TO DO.  A set of boundary points is used to support the breadth-first */
        /* search.  If the neighborhoods are very large and performance is an */
        /* issue, the sets should be replaced by a priority queue. */

        for (int j = 2; j <= iSize; j++) {
            Set<Integer> kExterior = new HashSet<Integer>(); /* set of int */

            Iterator kIter = kBoundary.iterator();

            while (kIter.hasNext()) {

                /* current boundary point to process */
                int iCenter = ((Integer) (kIter.next())).intValue();
                BasicMesh.Vertex kCenter = m_akVertex[iCenter];

                /* distance of path <source,center> */
                float fPathLength = m_kDistance.get(new EdgeKey(iSource, iCenter)).floatValue();

                /* search immediate, exterior neighbors */
                for (i = 0; i < kCenter.VQuantity; i++) {
                    iNbr = kCenter.V[i];

                    Integer kNbr = new Integer(iNbr);

                    if (!kInterior.contains(kNbr) && !kBoundary.contains(kNbr)) {

                        /* get distance of edge <center,nbr> */
                        Float kDist = m_kDistance.get(new EdgeKey(iCenter, iNbr));
                        kDiff.Sub( m_kMesh.VBuffer.GetPosition3(iNbr),
                        	       m_kMesh.VBuffer.GetPosition3(iCenter) );
                        float fDist = (null != kDist) ? kDist.floatValue()
                                                      : kDiff.Length();

                        /* get distance of <source,nbr> along current path */
                        float fTotalLength = fPathLength + fDist;

                        /* update current distance to shorter one */
                        /* (if necessary) */
                        EdgeKey kKey = new EdgeKey(iSource, iNbr);
                        kDist = m_kDistance.get(kKey);

                        if (null != kDist) {

                            if (fTotalLength < kDist.floatValue()) {
                                m_kDistance.put(kKey, new Float(fTotalLength));
                            }
                        } else {
                            m_kDistance.put(kKey, new Float(fTotalLength));
                        }

                        kExterior.add(new Integer(iNbr));
                    }
                }
            }

            /* boundary points processed, move to interior */
            kInterior.addAll(kBoundary);

            /* exterior points are next in line to be processed */
            kBoundary = kExterior;
        }

        if (bInitiator) {
            computeExtremes();
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void computeExtremes() {
        m_fMinDistance = Float.MAX_VALUE;
        m_fMaxDistance = 0.0f;

        Iterator kIter = m_kDistance.entrySet().iterator();

        while (kIter.hasNext()) {
            Map.Entry kMapEntry = (Map.Entry) (kIter.next());
            float fValue = ((Float) kMapEntry.getValue()).floatValue();

            if (fValue < m_fMinDistance) {
                m_fMinDistance = fValue;
            }

            if (fValue > m_fMaxDistance) {
                m_fMaxDistance = fValue;
            }
        }
    }

    /**
     * surface inflation.
     *
     * @return  DOCUMENT ME!
     */
    private float doInflationStep() {
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        
        Vector3f kVDiff = new Vector3f();
        Vector3f kDDiff = new Vector3f();

        float fWeight = 0.1f;

        /* compute the gradient of the energy function */
        Vector3f[] akDJDX = new Vector3f[iVQuantity];
        Vector3f kSumSqr = new Vector3f(Vector3f.ZERO);
        float fError = 0.0f;

        Vector3f kPos0 = new Vector3f();
        Vector3f kPos1 = new Vector3f();
        for (int i0 = 0; i0 < iVQuantity; i0++) {
            akDJDX[i0] = new Vector3f(Vector3f.ZERO);

            BasicMesh.Vertex kVertex = m_akVertex[i0];

            m_kMesh.VBuffer.GetPosition3( i0, kPos0 );

            for (int j = 0; j < kVertex.VQuantity; j++) {
                int i1 = kVertex.V[j];
                m_kMesh.VBuffer.GetPosition3( i1, kPos1 );
                kVDiff.Sub( kPos0, kPos1 );

                float fC = kVDiff.SquaredLength();

                EdgeKey kKey = new EdgeKey(i0, i1);
                assert (m_kInitDistance.containsKey(kKey));

                Float kValue = m_kInitDistance.get(kKey);
                assert (null != kValue);

                float fValue = kValue.floatValue();
                float fTmp0 = fC - (fValue * fValue);
                float fTmp1 = 1.0f + (fWeight * fTmp0);

                fError += (2.0f * fC) + (fWeight * fTmp0 * fTmp0);
                kVDiff.Scale(fTmp1);
                akDJDX[i0].Add( kVDiff );
            }

            kSumSqr.Add(akDJDX[i0]);

            /* update average convexity */
            m_afAvrConvexity[i0] += m_kMesh.VBuffer.GetNormal3(i0).Dot(akDJDX[i0]);
        }

        float fInvQuantity = 1.0f / iVQuantity;
        fError *= fInvQuantity;

        float fGradLength = kSumSqr.Length();

        if (fGradLength < Mathf.EPSILON) {
            return fError;
        }

        if (fGradLength > 1.0f) {
            float fInvGradLength = 1.0f / fGradLength;

            for (int i = 0; i < iVQuantity; i++) {
                akDJDX[i].Scale(fInvGradLength);
            }
        }

        /* Compute the energy function along the line X+t*D where X is the */
        /* total vector of vertex positions and D is the normalized gradient */
        /* vector. */
        Polynomial1f kPoly = new Polynomial1f(4);
        kPoly.SetCoeff(0, fError);

        for (int j = 1; j <= 4; j++) {
            kPoly.SetCoeff(j, 0.0f);
        }

        for (int i0 = 0; i0 < iVQuantity; i0++) {
            BasicMesh.Vertex kVertex = m_akVertex[i0];
            m_kMesh.VBuffer.GetPosition3( i0, kPos0 );
            
            for (int j = 0; j < kVertex.VQuantity; j++) {
                int i1 = kVertex.V[j];
                m_kMesh.VBuffer.GetPosition3( i1, kPos1 );
                kVDiff.Sub( kPos0, kPos1 );
                kDDiff.Sub( akDJDX[i0], akDJDX[i1] );

                float fA = kDDiff.SquaredLength();
                float fB = kDDiff.Dot(kVDiff);
                float fC = kVDiff.SquaredLength();

                EdgeKey kKey = new EdgeKey(i0, i1);
                assert (m_kInitDistance.containsKey(kKey));

                Float kValue = m_kInitDistance.get(kKey);
                assert (null != kValue);

                float fValue = kValue.floatValue();
                float fTmp0 = fC - (fValue * fValue);
                float fTmp1 = 1.0f + (fWeight * fTmp0);

                kPoly.SetCoeff(1, kPoly.GetCoeff(1) + (4.0f * fB * fTmp1));
                kPoly.SetCoeff(2, kPoly.GetCoeff(2) + (4.0f * fWeight * fB * fB) + (2.0f * fA * fTmp1));
                kPoly.SetCoeff(3, kPoly.GetCoeff(3) + (4.0f * fWeight * fA * fB));
                kPoly.SetCoeff(4, kPoly.GetCoeff(4) + (fWeight * fA * fA));
            }
        }

        for (int j = 1; j <= 4; j++) {
            kPoly.SetCoeff(j, kPoly.GetCoeff(j) * fInvQuantity);
        }

        Polynomial1f kDPoly = kPoly.GetDerivative();

        float[] afRoot = new float[3];
        int iCount = kDPoly.RootsDegree3(afRoot);
        assert (iCount > 0);

        int iMin = -1;

        for (int i = 0; i < iCount; i++) {
            float fTest = kDPoly.Eval(afRoot[i]);
            float fValue = kPoly.Eval(afRoot[i]);
            assert (fValue >= 0.0f);

            if (fValue < fError) {
                fError = fValue;
                iMin = i;
            }
        }
        int iChanged = 0;
        if (iMin >= 0) {
            float fH = afRoot[iMin];

            Vector3f kPos = new Vector3f();
            Vector3f kPos2 = new Vector3f();
            Vector3f kScale = new Vector3f();
            for (int i = 0; i < iVQuantity; i++) {
                m_kMesh.VBuffer.GetPosition3( i, kPos );
                m_kMesh.VBuffer.GetPosition3( i, kPos2 );
                kScale.Scale( fH, akDJDX[i] );
                kPos.Add( kScale );
                m_kMesh.VBuffer.SetPosition3( i, kPos );
                
                if ( !kPos.IsEqual( kPos2 ) )
                {
                    iChanged++;
                }
            }
            System.err.println( "changed: " + iChanged + " " + fH );
        }
        
        return fError;
    }

    /**
     * Support for conformal mapping to planar triangle mesh. Once the containing triangle i is found, compute the
     * barycentric coordinates of P relative to the triangle vertices V0, V1, and V2.
     *
     * @param   kP  DOCUMENT ME!
     * @param   i   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector3f getBarycentric(Vector2f kP, int i) {

        /* get barycentric coordinates */
        BasicMesh.Triangle kT = m_akTriangle[i];
        Vector2f kV0 = m_akPlane[kT.V[0]];
        Vector2f kV1 = m_akPlane[kT.V[1]];
        Vector2f kV2 = m_akPlane[kT.V[2]];
        Vector2f kE10 = new Vector2f();
        kE10.Sub( kV1, kV0 );

        Vector2f kE20 = new Vector2f();
        kE20.Sub( kV2, kV0 );

        Vector2f kDiff = new Vector2f();
        kDiff.Sub( kP, kV0 );

        float fA00 = kE10.Dot(kE10);
        float fA01 = kE10.Dot(kE20);
        float fA11 = kE20.Dot(kE20);
        float fInvDet = 1.0f / ((fA00 * fA11) - (fA01 * fA01));
        float fB0 = kE10.Dot(kDiff);
        float fB1 = kE20.Dot(kDiff);

        Vector3f kBary = new Vector3f();
        kBary.Y = ((fA11 * fB0) - (fA01 * fB1)) * fInvDet;
        kBary.Z = ((fA00 * fB1) - (fA01 * fB0)) * fInvDet;
        kBary.X = 1.0f - kBary.Y - kBary.Z;

        return kBary;
    }

    /**
     * Support for conformal mapping to planar triangle mesh. Use a linear walk to locate a triangle containing P. The
     * index of such a triangle is returned, unless there is no containing triangle in which case the return value is
     * -1. The "start" index is the suggested starting triangle. When testing a lot of points that are ordered in some
     * spatial manner, the chance that the triangle containing the previous test point will contain the next test point
     * (or a neighbor might contain it). Taking advantage of spatial coherence should speed up the search in "batch
     * mode".
     *
     * @param   kP      DOCUMENT ME!
     * @param   iStart  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int getContainingTriangle(Vector2f kP, int iStart) {

        /* The search should never iterate over more than all the triangles... */
        Ray2f kRay = new Ray2f();
        Segment2f kSeg = new Segment2f();
        Vector2f[] akVertex = new Vector2f[3];

        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++) {

            /* get vertices of current triangle */
            BasicMesh.Triangle kT = m_akTriangle[iStart];
            akVertex[0] = m_akPlane[kT.V[0]];
            akVertex[1] = m_akPlane[kT.V[1]];
            akVertex[2] = m_akPlane[kT.V[2]];

            /* test if P is inside the triangle */
            if (contains(akVertex, kP)) {
                return iStart;
            }

            /* P is outside the triangle.  Search an adjacent triangle through a */
            /* shared edge that is intersected by C+t*D where C is the current */
            /* triangle center and D is the direction (P-C)/|P-C|. */
            kRay.Origin.Set(0.0f, 0.0f);
            kRay.Origin.Add(akVertex[0]);
            kRay.Origin.Add(akVertex[1]);
            kRay.Origin.Add(akVertex[2]);
            kRay.Origin.Scale(1.0f / 3.0f);
            kRay.Direction.Sub( kP, kRay.Origin );
            kRay.Direction.Normalize();

            int i0, i1;

            for (i0 = 2, i1 = 0; i1 < 3; i0 = i1++) {
                kSeg.Origin.Copy(akVertex[i0]);
                kSeg.Direction.Sub( akVertex[i1], akVertex[i0] );

                float[] afT = new float[2]; /* T[0] = ray, T[1] = segment */
                int iQuantity = IntrRay2Segment2f.FindIntersection(kRay, kSeg, afT);

                /* intersects? */
                if (0 != iQuantity) {
                    BasicMesh.Edge kE = m_akEdge[kT.E[i0]];
                    iStart = ((kE.T[0] == iStart) ? kE.T[1] : kE.T[0]);

                    break;
                }
            }

            assert (i1 < 3);
        }

        /* point not located in triangle mesh */
        return -1;
    }

    /**
     * support for unfolding.
     *
     * @return  DOCUMENT ME!
     */
    private float getStereographicRadius() {
        return m_fRho;
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    @Override
	protected void finalize() throws Throwable {
        disposeLocal();
    }
}
