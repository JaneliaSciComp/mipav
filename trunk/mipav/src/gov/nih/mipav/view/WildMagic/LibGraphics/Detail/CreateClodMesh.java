package gov.nih.mipav.view.WildMagic.LibGraphics.Detail;

import java.util.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;


/**
 * A triangle decimator that is used for continuous level of detail of a triangle mesh. The algorithm is based on edge
 * collapses of the mesh. A detailed discussion of the algorithm is found in <a href=../../../LevelSetExtraction.pdf>
 * Level Set Extraction</a>
 */

public class CreateClodMesh {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** map of <Edge,EdgeAttribute>. */
    protected HashMap m_kEMap;

    /** map of <Triangle,TriangleAttribute>. */
    protected HashMap m_kTMap;

    /** map of <Vertex,VertexAttribute>. */
    protected HashMap m_kVMap;
    
    /** DOCUMENT ME! */
    Vector m_kEDelete; // vector of <ModelCollapseRecord>

    /** DOCUMENT ME! */
    HashSet m_kVDelete; // set of <Vertex>

    /** DOCUMENT ME! */
    private int[] m_aiConnect; // array of 3*m_iTQuantity vertex indices

    /** DOCUMENT ME! */
    private int[] m_aiIndex;

    /** DOCUMENT ME! */
    private int[] m_aiNewConnect;

    /** DOCUMENT ME! */
    private int[] m_aiVOrdered;

    /** DOCUMENT ME! */
    private int[] m_aiVPermute;

    /** DOCUMENT ME! */
    private HeapRecord[] m_akHeap;

    /** DOCUMENT ME! */
    private VertexBuffer m_kNewVBuffer;

    /** the incremental changes representing the decimation. */
    private CollapseRecordArray m_akRecord;

    /** DOCUMENT ME! */
    private VertexBuffer m_kVBuffer;
    /** DOCUMENT ME! */
    private IndexBuffer m_kIBuffer;

    /** DOCUMENT ME! */
    private boolean m_bCollapsing;

    /** collapse support. */
    private int m_iHQuantity;

    /** DOCUMENT ME! */
    private int m_iTCurrent;

    /** DOCUMENT ME! */
    private int m_iTQuantity; // number of triangles

    /** for reordering vertices and triangles. */
    private int m_iVCurrent;

    /** triangle mesh to be decimated. */
    private int m_iVQuantity; // number of vertices/normals

    /** temporary variables to avoid 'new' calls. */
    private Vector3f m_kE0, m_kE1, m_kN0, m_kN1, m_kCross, m_kDiff;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The decimator constructed by this method is designed to be reused, if necessary, for decimating multiple meshes.
     * If used this way, the input parameters measure the maximum quantities over all the meshes.
     *
     * @param  iVQuantity  the maximum number of vertices for all meshes to be decimated by this object
     * @param  iTQuantity  the maximum number of triangles for all meshes to be decimated by this object
     */
    public CreateClodMesh(VertexBuffer pkVBuffer, IndexBuffer pkIBuffer) {

        m_kVMap = new HashMap();
        m_kEMap = new HashMap();
        m_kTMap = new HashMap();
        
        m_kVBuffer = pkVBuffer;
        m_kIBuffer = pkIBuffer;
        
        int iVQuantity = pkVBuffer.GetVertexQuantity();
        int iIndexQuantity = pkIBuffer.GetIndexQuantity();
        m_aiVOrdered = new int[iVQuantity];
        m_aiVPermute = new int[iVQuantity];
        m_aiNewConnect = new int[iIndexQuantity];
        m_kVDelete = new HashSet();
        m_kEDelete = new Vector();
        m_akHeap = new HeapRecord[iIndexQuantity];

        m_kNewVBuffer = new VertexBuffer(pkVBuffer);

        // temporary variables to avoid 'new' calls
        m_kE0 = new Vector3f();
        m_kE1 = new Vector3f();
        m_kN0 = new Vector3f();
        m_kN1 = new Vector3f();
        m_kCross = new Vector3f();
        m_kDiff = new Vector3f();
        m_aiIndex = new int[iIndexQuantity];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * A triangle decimator for a triangle mesh. After decimation, the vertex and triangle connectivity arrays have been
     * reordered and should be used as input in creating an ModelClodMesh object. The collapse records constructed in
     * this function can be accessed via getRecords().
     *
     * @param  akVertex     array of vertices in the mesh
     * @param  aiConnect    Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                      triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     */
    public void decimate()
    {

        // clear out topology if decimator used previously
        m_kVMap.clear();
        m_kEMap.clear();
        m_kTMap.clear();
        m_kVDelete.clear();
        m_kEDelete.clear();

        // Hang onto these to avoid having to pass them through member
        // function calls.
        m_iVQuantity = m_kVBuffer.GetVertexQuantity();
        m_iTQuantity = m_kIBuffer.GetIndexQuantity() / 3;
        m_aiConnect = m_kIBuffer.GetData();

        // for reordering vertices and triangles
        m_iVCurrent = m_iVQuantity - 1;
        m_iTCurrent = m_iTQuantity - 1;

        // Insert the triangles into the mesh.  The triangle indices are
        // attached as extra data.
        m_bCollapsing = false;

        for (int i = 0; i < m_iTQuantity; i++) {
            Triangle kT = new Triangle(m_aiConnect[3 * i], m_aiConnect[(3 * i) + 1], m_aiConnect[(3 * i) + 2]);
            insertTriangle(kT);
            setData(kT, new Integer(i));
        }

        // set up the heap for storing information needed by edge collapses
        initializeHeap();

        // do the edge collapses
        m_bCollapsing = true;

        while (m_iHQuantity > 0) {

            if (m_akHeap[0].m_fMetric == Float.POSITIVE_INFINITY) {

                // all remaining heap elements have infinite weight
                flushVertices();
                flushTriangles();

                break;
            }

            doCollapse();
        }

        m_bCollapsing = false;

        // Permute the vertices and triangle connectivity so that the last
        // vertex/triangle in the array is the first vertex/triangle to be
        // removed.
        reorder();

        // The collapse records store the incremental changes that are used for
        // dynamic LOD changes in the caller of this constructor.
        computeRecords();

        // DEBUGGING.  Uncomment this line to verify the correctness of the
        // edge collapses.
        // boolean bIsValid = validate();
        // END DEBUGGING.
    }
    
    /**
     * Get the attribute data associated with the specified triangle.
     *
     * @param   kT  the triangle whose attribute data will be retrieved
     *
     * @return  the attribute data
     */
    public Object getData(Triangle kT) {
        TriangleAttribute kTAttr = (TriangleAttribute) m_kTMap.get(kT);

        return ((kTAttr != null) ? kTAttr.m_kData : null);
    }


    /**
     * The collapse records that have been created by the constructor for ModelSurfaceDecimator. After construction, the
     * application uses the vertex and triangle connectivity arrays that were passed to the constructor (arrays are now
     * reordered) and should call this function to access the collapse records. All three data structures are then used
     * to create an ModelClodMesh object.
     *
     * @return  the array of collapse records for the decimation
     */
    public CollapseRecordArray getRecords() {
        return m_akRecord;
    }

    /**
     * An override of the base class ModelSurfaceTopology member function. This callback is used to keep track of heap
     * changes due to edge collapses.
     *
     * @param  kE       the edge that was attempted to be inserted into the map
     * @param  bCreate  true if the edge did not exist in the map before insertion (it is a brand new edge), false if
     *                  the edge already existed.
     * @param  kEAttr   the attribute for the edge
     */
    public void OnEdgeInsert(Edge kE, boolean bCreate, EdgeAttribute kEAttr) {

        if (bCreate) {
            kEAttr.m_kData = new HeapRecord();

            if (m_bCollapsing) {
                m_akHeap[m_iHQuantity] = (HeapRecord) kEAttr.m_kData;
                m_akHeap[m_iHQuantity].m_kEdge = kE;
                m_akHeap[m_iHQuantity].m_iHIndex = m_iHQuantity;
                add(getMetric(kE, kEAttr));
            }
        } else {

            if (m_bCollapsing) {
                HeapRecord kRecord = (HeapRecord) kEAttr.m_kData;

                if (kRecord.m_iHIndex >= 0) {
                    update(kRecord.m_iHIndex, getMetric(kE, kEAttr));
                } else {
                    kRecord.m_iHIndex = m_iHQuantity;
                    add(getMetric(kE, kEAttr));
                }
            }
        }
    }

    /**
     * An override of the base class ModelSurfaceTopology member function. This callback is used to keep track of heap
     * changes due to edge collapses.
     *
     * @param  kE        the edge that was attempted to be removed from the map
     * @param  bDestroy  true if the edge did exist in the map before the attempted removal and no other mesh components
     *                   are referencing the edge, false if the edge does exist in the map, but other mesh components
     *                   are referencing it.
     * @param  kEAttr    the attribute for the edge
     */
    public void OnEdgeRemove(Edge kE, boolean bDestroy, EdgeAttribute kEAttr) {
        // Remove the edge from the heap.  The metric of the edge is set to -INFINITY so that it has the minimum value
        // of all edges.  The update call bubbles the edge to the root of the heap.  The edge is then removed from the
        // root.

        if (bDestroy) {
            HeapRecord kRecord = (HeapRecord) kEAttr.m_kData;

            if (kRecord.m_iHIndex >= 0) {
                update(kRecord.m_iHIndex, Float.NEGATIVE_INFINITY);
                remove();
            }

            kEAttr.m_kData = null;
        }
    }

    /**
     * An override of the base class ModelSurfaceTopology member function. This callback is used to assign integer
     * indices to triangles in the mesh.
     *
     * @param  kT       the triangle that was attempted to be inserted into the map
     * @param  bCreate  true if the triangle did not exist in the map before insertion (it is a brand new triangle),
     *                  false if the triangle already existed.
     * @param  kTAttr   the attribute for the triangle
     */
    public void OnTriangleInsert(Triangle kT, boolean bCreate, TriangleAttribute kTAttr) {

        if (bCreate) {
            kTAttr.m_kData = new Integer(-1);
        }
    }

    /**
     * An override of the base class ModelSurfaceTopology member function. This callback is used to assign integer
     * indices to triangles in the mesh.
     *
     * @param  kT        the triangle that was attempted to be removed from the map
     * @param  bDestroy  true if the triangle did exist in the map before the attempted removal and no other mesh
     *                   components are referencing the triangle, false if the triangle does exist in the map, but other
     *                   mesh components are referencing it.
     * @param  kTAttr    the attribute for the triangle
     */
    public void OnTriangleRemove(Triangle kT, boolean bDestroy, TriangleAttribute kTAttr) {

        if (bDestroy) {
            kTAttr.m_kData = null;
        }
    }

    /**
     * An override of the base class ModelSurfaceTopology member function. This callback is used to keep track of
     * vertices deleted during an edge collapse.
     *
     * @param  kV       the vertex that was attempted to be inserted into the map
     * @param  bCreate  true if the vertex did not exist in the map before insertion (it is a brand new vertex), false
     *                  if the vertex already existed.
     * @param  kVAttr   the attribute for the vertex
     */
    public void OnVertexInsert(Vertex kV, boolean bCreate, VertexAttribute kVAttr) {

        // It is possible that a 'keep' vertex was removed because the
        // triangles sharing the collapse edge were removed first, but then
        // the insertion of a modified triangle reinserts the 'keep' vertex.
        if (bCreate && m_bCollapsing) {
            m_kVDelete.remove(kV);
        }
    }

    /**
     * An override of the base class ModelSurfaceTopology member function. This callback is used to keep track of
     * vertices deleted during an edge collapse.
     *
     * @param  kV        the vertex that was attempted to be removed from the map
     * @param  bDestroy  true if the vertex did exist in the map before the attempted removal and no other mesh
     *                   components are referencing the vertex, false if the vertex does exist in the map, but other
     *                   mesh components are referencing it.
     * @param  kVAttr    the attribute for the vertex
     */
    public void OnVertexRemove(Vertex kV, boolean bDestroy, VertexAttribute kVAttr) {

        // Keep track of vertices removed during the edge collapse.
        if (bDestroy && m_bCollapsing) {
            m_kVDelete.add(kV);
        }
    }
    /**
     * Convenience function for inserting triangles into the mesh.
     *
     * @param  kT  the triangle to be inserted
     */
    public void insertTriangle(Triangle kT) {
        insertTriangle(kT.m_iV0, kT.m_iV1, kT.m_iV2);
    }

    /**
     * Insert triangle <V0,V1,V2> into the mesh. The ordering of the indices is relevant. Triangle <V0,V2,V1> is
     * considered to be a different triangle (reversed ordering from <V0,V1,V2>).
     *
     * @param  iV0  index of triangle
     * @param  iV1  index of triangle
     * @param  iV2  index of triangle
     */
    public void insertTriangle(int iV0, int iV1, int iV2) {

        // create vertices, edges, and triangle
        Vertex kV0 = new Vertex(iV0);
        Vertex kV1 = new Vertex(iV1);
        Vertex kV2 = new Vertex(iV2);
        Edge kE0 = new Edge(iV0, iV1);
        Edge kE1 = new Edge(iV1, iV2);
        Edge kE2 = new Edge(iV2, iV0);
        Triangle kT = new Triangle(iV0, iV1, iV2);

        // insert triangle
        TriangleAttribute kTAttr = (TriangleAttribute) m_kTMap.get(kT);
        boolean bTCreated = (kTAttr == null);

        if (bTCreated) {
            kTAttr = new TriangleAttribute();
            m_kTMap.put(kT, kTAttr);
        }

        // insert vertices
        VertexAttribute kV0Attr = (VertexAttribute) m_kVMap.get(kV0);
        boolean bV0Created = (kV0Attr == null);

        if (bV0Created) {
            kV0Attr = new VertexAttribute();
            m_kVMap.put(kV0, kV0Attr);
        }

        kV0Attr.m_kESet.add(kE0);
        kV0Attr.m_kESet.add(kE2);
        kV0Attr.m_kTSet.add(kT);

        VertexAttribute kV1Attr = (VertexAttribute) m_kVMap.get(kV1);
        boolean bV1Created = (kV1Attr == null);

        if (bV1Created) {
            kV1Attr = new VertexAttribute();
            m_kVMap.put(kV1, kV1Attr);
        }

        kV1Attr.m_kESet.add(kE0);
        kV1Attr.m_kESet.add(kE1);
        kV1Attr.m_kTSet.add(kT);

        VertexAttribute kV2Attr = (VertexAttribute) m_kVMap.get(kV2);
        boolean bV2Created = (kV2Attr == null);

        if (bV2Created) {
            kV2Attr = new VertexAttribute();
            m_kVMap.put(kV2, kV2Attr);
        }

        kV2Attr.m_kESet.add(kE1);
        kV2Attr.m_kESet.add(kE2);
        kV2Attr.m_kTSet.add(kT);

        // insert edges
        EdgeAttribute kE0Attr = (EdgeAttribute) m_kEMap.get(kE0);
        boolean bE0Created = (kE0Attr == null);

        if (bE0Created) {
            kE0Attr = new EdgeAttribute();
            m_kEMap.put(kE0, kE0Attr);
        }

        kE0Attr.m_kTSet.add(kT);

        EdgeAttribute kE1Attr = (EdgeAttribute) m_kEMap.get(kE1);
        boolean bE1Created = (kE1Attr == null);

        if (bE1Created) {
            kE1Attr = new EdgeAttribute();
            m_kEMap.put(kE1, kE1Attr);
        }

        kE1Attr.m_kTSet.add(kT);

        EdgeAttribute kE2Attr = (EdgeAttribute) m_kEMap.get(kE2);
        boolean bE2Created = (kE2Attr == null);

        if (bE2Created) {
            kE2Attr = new EdgeAttribute();
            m_kEMap.put(kE2, kE2Attr);
        }

        kE2Attr.m_kTSet.add(kT);

        // Notify derived classes that mesh components have been inserted.
        // The notification occurs here to make sure the derived classes have
        // access to the current state of the mesh after the triangle
        // insertion.

        OnVertexInsert(kV0, bV0Created, kV0Attr);
        OnVertexInsert(kV1, bV1Created, kV1Attr);
        OnVertexInsert(kV2, bV2Created, kV2Attr);
        OnEdgeInsert(kE0, bE0Created, kE0Attr);
        OnEdgeInsert(kE1, bE1Created, kE1Attr);
        OnEdgeInsert(kE2, bE2Created, kE2Attr);
        OnTriangleInsert(kT, bTCreated, kTAttr);
    }

    
    /**
     * An override of the ModelSurfaceTopology member function. This function keeps track of the order of triangle
     * removal to support reordering of the connectivity array. Once done, then the base class function is called to
     * remove the triangle from the mesh.
     *
     * @param  kT  the triangle to be removed
     */
    public void removeTriangle(Triangle kT) {

        // If the triangle is an original one, reorder the connectivity array
        // so that the triangle occurs at the end.
        Integer kTIndex = (Integer) getData(kT);
        int iTIndex = kTIndex.intValue();

        if (iTIndex >= 0) {
            m_aiNewConnect[3 * m_iTCurrent] = m_aiConnect[3 * iTIndex];
            m_aiNewConnect[(3 * m_iTCurrent) + 1] = m_aiConnect[(3 * iTIndex) + 1];
            m_aiNewConnect[(3 * m_iTCurrent) + 2] = m_aiConnect[(3 * iTIndex) + 2];
            m_iTCurrent--;
        }

        superRemoveTriangle(kT);
    }
    
    /**
     * Convenience function for removing triangles from the mesh.
     *
     * @param  kT  the triangle to be removed
     */
    public void superRemoveTriangle(Triangle kT) {
        removeTriangle(kT.m_iV0, kT.m_iV1, kT.m_iV2);
    }

    /**
     * Remove triangle <V0,V1,V2> from the mesh. The ordering of the indices is relevant. Triangle <V0,V2,V1> is
     * considered to be a different triangle (reversed ordering from <V0,V1,V2>).
     *
     * @param  iV0  index of triangle
     * @param  iV1  index of triangle
     * @param  iV2  index of triangle
     */
    public void removeTriangle(int iV0, int iV1, int iV2) {

        // remove triangle
        Triangle kT = new Triangle(iV0, iV1, iV2);
        TriangleAttribute kTAttr = (TriangleAttribute) m_kTMap.get(kT);

        if (kTAttr == null) {

            // triangle does not exist, nothing to do
            return;
        }

        // update edges
        Edge kE0 = new Edge(iV0, iV1);
        EdgeAttribute kE0Attr = (EdgeAttribute) m_kEMap.get(kE0);
        kE0Attr.m_kTSet.remove(kT);

        Edge kE1 = new Edge(iV1, iV2);
        EdgeAttribute kE1Attr = (EdgeAttribute) m_kEMap.get(kE1);
        kE1Attr.m_kTSet.remove(kT);

        Edge kE2 = new Edge(iV2, iV0);
        EdgeAttribute kE2Attr = (EdgeAttribute) m_kEMap.get(kE2);
        kE2Attr.m_kTSet.remove(kT);

        // update vertices
        Vertex kV0 = new Vertex(iV0);
        VertexAttribute kV0Attr = (VertexAttribute) m_kVMap.get(kV0);
        kV0Attr.m_kTSet.remove(kT);

        Vertex kV1 = new Vertex(iV1);
        VertexAttribute kV1Attr = (VertexAttribute) m_kVMap.get(kV1);
        kV1Attr.m_kTSet.remove(kT);

        Vertex kV2 = new Vertex(iV2);
        VertexAttribute kV2Attr = (VertexAttribute) m_kVMap.get(kV2);
        kV2Attr.m_kTSet.remove(kT);

        if (kE0Attr.m_kTSet.isEmpty()) {
            kV0Attr.m_kESet.remove(kE0);
            kV1Attr.m_kESet.remove(kE0);
        }

        if (kE1Attr.m_kTSet.isEmpty()) {
            kV1Attr.m_kESet.remove(kE1);
            kV2Attr.m_kESet.remove(kE1);
        }

        if (kE2Attr.m_kTSet.isEmpty()) {
            kV0Attr.m_kESet.remove(kE2);
            kV2Attr.m_kESet.remove(kE2);
        }

        // Notify derived classes that mesh components are about to be
        // destroyed.  The notification occurs here to make sure the derived
        // classes have access to the current state of the mesh before the
        // triangle removal.

        boolean bDestroyed = kV0Attr.isEmpty();
        OnVertexRemove(kV0, bDestroyed, kV0Attr);

        if (bDestroyed) {
            m_kVMap.remove(kV0);
        }

        bDestroyed = kV1Attr.isEmpty();
        OnVertexRemove(kV1, bDestroyed, kV1Attr);

        if (bDestroyed) {
            m_kVMap.remove(kV1);
        }

        bDestroyed = kV2Attr.isEmpty();
        OnVertexRemove(kV2, bDestroyed, kV2Attr);

        if (bDestroyed) {
            m_kVMap.remove(kV2);
        }

        bDestroyed = kE0Attr.isEmpty();
        OnEdgeRemove(kE0, bDestroyed, kE0Attr);

        if (bDestroyed) {
            m_kEMap.remove(kE0);
        }

        bDestroyed = kE1Attr.isEmpty();
        OnEdgeRemove(kE1, bDestroyed, kE1Attr);

        if (bDestroyed) {
            m_kEMap.remove(kE1);
        }

        bDestroyed = kE2Attr.isEmpty();
        OnEdgeRemove(kE2, bDestroyed, kE2Attr);

        if (bDestroyed) {
            m_kEMap.remove(kE2);
        }

        OnTriangleRemove(kT, true, kTAttr);
        m_kTMap.remove(kT);
    }

    /**
     * Set the attribute data associated with the specified triangle.
     *
     * @param  kT     the triangle whose attribute data will be set
     * @param  kData  the attribute data
     */
    public void setData(Triangle kT, Object kData) {
        TriangleAttribute kTAttr = (TriangleAttribute) m_kTMap.get(kT);

        if (kTAttr != null) {
            kTAttr.m_kData = kData;
        }
    }



    /**
     * New edges that occur because of modified triangles being added to the mesh must be added to the heap and the heap
     * must be updated. This is an O(log N) process for N edges.
     *
     * @param  fMetric  The new edge weight to be added to the heap. The other heap element information was set up by
     *                  the OnEdgeInsert callback.
     */
    private void add(float fMetric) {

        // Under normal heap operations, you would have to make sure that the
        // heap storage grows if necessary.  Increased storage demand will not
        // happen in this application.  The creation of the heap record itself
        // is done in OnEdgeCreate.
        m_iHQuantity++;

        int iCh = m_iHQuantity - 1;
        HeapRecord kRecord = m_akHeap[iCh];

        while (iCh > 0) {
            int iPa = (iCh - 1) / 2;

            if (m_akHeap[iPa].m_fMetric <= fMetric) {
                break;
            }

            m_akHeap[iPa].m_iHIndex = iCh;
            m_akHeap[iCh] = m_akHeap[iPa];
            kRecord.m_iHIndex = iPa;
            kRecord.m_fMetric = fMetric;
            m_akHeap[iPa] = kRecord;
            iCh = iPa;
        }

        m_akHeap[iCh].m_fMetric = fMetric;
    }

    /**
     * A look-ahead test to determine if the requested edge collapse will cause the mesh to fold over itself.
     *
     * @param   iVKeep   the vertex to keep in the edge collapse
     * @param   iVThrow  the vertex to be thrown away in the edge collapse
     *
     * @return  true iff the edge collapse will cause the mesh to fold over
     */
    private boolean collapseCausesFolding(int iVKeep, int iVThrow) {
        VertexAttribute kVTAttr = (VertexAttribute) m_kVMap.get(new Vertex(iVThrow));

        Edge kCollapse = new Edge(iVKeep, iVThrow);

        for (int j = 0; j < kVTAttr.m_kTSet.size(); j++) {
            Triangle kT = (Triangle) kVTAttr.m_kTSet.get(j);

            if ((kCollapse == new Edge(kT.m_iV0, kT.m_iV1)) || (kCollapse == new Edge(kT.m_iV1, kT.m_iV2)) ||
                    (kCollapse == new Edge(kT.m_iV2, kT.m_iV0))) {

                // This triangle would be removed in a collapse, so it does
                // not contribute to any folding.
                continue;
            }

            for (int i = 0; i < 3; i++) {

                // Test if potential replacement triangle (either ordering)
                // is in the mesh.
                int iV0 = iVKeep, iV1 = 0, iV2 = 0;

                switch (i) {

                    case 0:
                        if (kT.m_iV0 == iVThrow) {
                            iV1 = kT.m_iV1;
                            iV2 = kT.m_iV2;
                        }

                        break;

                    case 1:
                        if (kT.m_iV1 == iVThrow) {
                            iV1 = kT.m_iV2;
                            iV2 = kT.m_iV0;
                        }

                        break;

                    case 2:
                        if (kT.m_iV2 == iVThrow) {
                            iV1 = kT.m_iV0;
                            iV2 = kT.m_iV1;
                        }

                        break;
                }

                if ((m_kTMap.get(new Triangle(iV0, iV1, iV2)) != null) ||
                        (m_kTMap.get(new Triangle(iV0, iV2, iV1)) != null)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * The internal edge collapse function. This is called as long as the suggested edge does not cause folding.
     *
     * @param  iVKeep   the vertex to keep in the edge collapse
     * @param  iVThrow  the vertex to be thrown away in the edge collapse
     */
    private void collapseEdge(int iVKeep, int iVThrow) {

        // find the edge to collapse
        Edge kCollapse = new Edge(iVKeep, iVThrow);
        EdgeAttribute kCollapseAttr = (EdgeAttribute) m_kEMap.get(kCollapse);

        // Keep track of vertices that are deleted in the collapse.
        m_kVDelete.clear();

        // Remove the collapse-edge-shared triangles.  Using a clone of the
        // triangle set from the collapse edge is required since removal of
        // the last triangle sharing the collapse edge will remove that edge
        // from the edge map, thereby invalidating any iterator that points to
        // data in the collapse edge.
        ModelSet kTSet = new ModelSet(kCollapseAttr.m_kTSet);
        int iTDeletions = kTSet.size();
        int j;

        for (j = 0; j < kTSet.size(); j++) {
            Triangle kT = (Triangle) kTSet.get(j);
            removeTriangle(kT);
        }

        // Replace 'throw' vertices by 'keep' vertices in the remaining
        // triangles at the 'throw' vertex.  The old triangles are removed
        // and the modified triangles are inserted.
        Vertex kVThrow = new Vertex(iVThrow);
        VertexAttribute kVThrowAttr = (VertexAttribute) m_kVMap.get(kVThrow);

        if (kVThrowAttr != null) {
            kTSet = new ModelSet(kVThrowAttr.m_kTSet);

            for (j = 0; j < kTSet.size(); j++) {
                Triangle kT = (Triangle) kTSet.get(j);
                modifyTriangle(kT, iVKeep, iVThrow);
            }
        }

        // The set of potentially modified edges consists of all those edges
        // that are shared by the triangles containing the 'keep' vertex.
        // Modify these metrics and update the heap.
        HashSet kModified = new HashSet(); // set of <Edge>
        Vertex kVKeep = new Vertex(iVKeep);
        VertexAttribute kVKeepAttr = (VertexAttribute) m_kVMap.get(kVKeep);

        if (kVKeepAttr != null) {
            kTSet = new ModelSet(kVKeepAttr.m_kTSet);

            for (j = 0; j < kTSet.size(); j++) {
                Triangle kT = (Triangle) kTSet.get(j);
                kModified.add(new Edge(kT.m_iV0, kT.m_iV1));
                kModified.add(new Edge(kT.m_iV1, kT.m_iV2));
                kModified.add(new Edge(kT.m_iV2, kT.m_iV0));
            }

            Iterator kEIter = kModified.iterator();

            while (kEIter.hasNext()) {
                Edge kE = (Edge) kEIter.next();
                EdgeAttribute kEAttr = (EdgeAttribute) m_kEMap.get(kE);
                HeapRecord kRecord = (HeapRecord) kEAttr.m_kData;
                float fMetric = getMetric(kE, kEAttr);
                update(kRecord.m_iHIndex, fMetric);
            }
        }

        // save vertex reordering information
        Iterator kVIter = m_kVDelete.iterator();

        while (kVIter.hasNext()) {
            Vertex kV = (Vertex) kVIter.next();
            m_aiVOrdered[m_iVCurrent] = kV.m_iV;
            m_aiVPermute[kV.m_iV] = m_iVCurrent;
            m_iVCurrent--;
        }

        // Save the collapse information for use in constructing the final
        // collapse records for the caller of the constructor of this class.
        CollapseRecord kCR = new CollapseRecord(iVKeep, iVThrow, m_kVDelete.size(), iTDeletions);
        m_kEDelete.add(kCR);
    }

    /**
     * All collapse information is known when this is called. What remains to be done is to determine which indices in
     * the triangle connectivity must be changed when a collapse record is applied to the mesh. This is done to make the
     * index changes fast in the application. Without the look-up tables in the collapse record, the application would
     * have to constantly search the connectivity array and do the work in the current function <b>every time level of
     * detail is changed</b>.
     */
    private void computeRecords() {

        // build the collapse records for the caller
        int iCQuantity = m_kEDelete.size() + 1;
        CollapseRecord[] akRecord = new CollapseRecord[iCQuantity];

        int i;

        for (i = 0; i < iCQuantity; i++) {
            akRecord[i] = new CollapseRecord();
        }

        m_akRecord = new CollapseRecordArray( iCQuantity, akRecord );
        
        // initial record only stores initial vertex and triangle quantities
        akRecord[0].VQuantity = m_iVQuantity;
        akRecord[0].TQuantity = m_iTQuantity;

        // construct the replacement arrays
        int iVQuantity = m_iVQuantity, iTQuantity = m_iTQuantity;
        int iR;
        int length = m_kEDelete.size();

        for (iR = 0; iR < length; iR++) {
            CollapseRecord kERecord = (CollapseRecord) m_kEDelete.get(iR);
            CollapseRecord kRecord = akRecord[iR + 1];

            iVQuantity -= kERecord.VQuantity;
            iTQuantity -= kERecord.TQuantity;

            kRecord.VKeep = kERecord.VKeep;
            kRecord.VThrow = kERecord.VThrow;
            kRecord.VQuantity = iVQuantity;
            kRecord.TQuantity = iTQuantity;
            kRecord.IQuantity = 0;

            if (iTQuantity > 0) {
                int iIMax = 3 * iTQuantity;

                for (i = 0; i < iIMax; i++) {

                    if (m_aiConnect[i] == kRecord.VThrow) {
                        m_aiConnect[i] = kRecord.VKeep;
                        m_aiIndex[kRecord.IQuantity++] = i;
                    }
                }

                if (kRecord.IQuantity > 0) {
                    kRecord.Index = new int[kRecord.IQuantity];

                    for (i = 0; i < kRecord.IQuantity; i++) {
                        kRecord.Index[i] = m_aiIndex[i];
                    }
                }
            } else {
                kRecord.Index = null;
            }
        }

        // expand mesh back to original
        for (iR = iCQuantity - 1; iR > 0; iR--) {

            // restore indices in connectivity array
            CollapseRecord kRecord = akRecord[iR];

            for (i = 0; i < kRecord.IQuantity; i++) {
                int iC = kRecord.Index[i];
                m_aiConnect[iC] = kRecord.VThrow;
            }
        }
    }

    /**
     * The top-level edge collapse operation.
     */
    private void doCollapse() {
        // Define a 2-edge to be an edge that has exactly two triangles
        // sharing it.  An edge is collapsible if it is a 2-edge and has at
        // least one end point whose sharing edges are all 2-edges.  In this
        // case, such an end point will be the 'throw' vertex.  This keeps
        // the boundary and junction edges from changing geometry and helps
        // preserve the shape of the mesh.  The topology is always guaranteed
        // not to change.

        Edge kEdge = m_akHeap[0].m_kEdge;

        // test end points to see if either has only 2-edges sharing it
        int i;

        for (i = 0; i < 2; i++) {
            int iV = ((i == 0) ? kEdge.m_iV0 : kEdge.m_iV1);
            VertexAttribute kVAttr = (VertexAttribute) m_kVMap.get(new Vertex(iV));
            int j;

            for (j = 0; j < kVAttr.m_kESet.size(); j++) {
                Edge kEAdj = (Edge) kVAttr.m_kESet.get(j);
                EdgeAttribute kEAdjAttr = (EdgeAttribute) m_kEMap.get(kEAdj);

                if (kEAdjAttr.m_kTSet.size() != 2) {
                    break;
                }
            }

            if (j == kVAttr.m_kESet.size()) {

                // all edges sharing this end point are 2-edges
                break;
            }
        }

        if (i < 2) {
            int iVThrow, iVKeep;

            if (i == 0) {
                iVThrow = kEdge.m_iV0;
                iVKeep = kEdge.m_iV1;
            } else {
                iVThrow = kEdge.m_iV1;
                iVKeep = kEdge.m_iV0;
            }

            if (!collapseCausesFolding(iVKeep, iVThrow)) {
                remove();
                collapseEdge(iVKeep, iVThrow);

                return;
            }
        }

        // edge not collapsible, assign it infinite weight and update heap
        update(0, Float.POSITIVE_INFINITY);
    }

    /**
     * When all edges have infinite weight, no further edge collapses are allowed. At this point the remaining triangles
     * in the map are written to the reordering array and the edge collapse process is finished.
     */
    private void flushTriangles() {
        Iterator kTIter = m_kTMap.entrySet().iterator();

        while (kTIter.hasNext()) {
            Map.Entry kEntry = (Map.Entry) kTIter.next();
            TriangleAttribute kTAttr = (TriangleAttribute) kEntry.getValue();
            Integer iTInteger = (Integer) kTAttr.m_kData;
            int iTIndex = iTInteger.intValue();

            if (iTIndex >= 0) {
                m_aiNewConnect[3 * m_iTCurrent] = m_aiConnect[3 * iTIndex];
                m_aiNewConnect[(3 * m_iTCurrent) + 1] = m_aiConnect[(3 * iTIndex) + 1];
                m_aiNewConnect[(3 * m_iTCurrent) + 2] = m_aiConnect[(3 * iTIndex) + 2];
                m_iTCurrent--;
            }
        }
    }

    /**
     * When all edges have infinite weight, no further edge collapses are allowed. At this point the remaining vertices
     * in the map are written to the reordering/permutation arrays and the edge collapse process is finished.
     */
    private void flushVertices() {
        Iterator kVIter = m_kVMap.entrySet().iterator();

        while (kVIter.hasNext()) {
            Map.Entry kEntry = (Map.Entry) kVIter.next();
            Vertex kV = (Vertex) kEntry.getKey();
            int iV = kV.m_iV;
            m_aiVOrdered[m_iVCurrent] = iV;
            m_aiVPermute[iV] = m_iVCurrent;
            m_iVCurrent--;
        }
    }

    /**
     * Computes the weight associated with an edge. The weight is based on the length of the edge, the area of the
     * adjacent triangles, and the angle between the triangles. The smaller any of these quantites are, the more likely
     * the edge should be removed. That is, attempt to collapse short edges whose triangles have small area and are
     * nearly coplanar. Only manifold edges have finite weight. Other edges are assigned infinite weight to preserve
     * topology of the mesh (boundary edges and junction edges should not be collapsed).
     *
     * @param   kE      the edge to be weighted
     * @param   kEAttr  the attribute associated with the edge
     *
     * @return  the new weight of the edge
     */
    private float getMetric(Edge kE, EdgeAttribute kEAttr) {

        // user-modifiable parameters
        float fLengthWeight = 10.0f;
        float fAngleWeight = 1.0f;

        // compute the metric for the edge
        if (kEAttr.m_kTSet.size() == 2) {

            // length contribution
            Vector3f kEnd0 = m_kVBuffer.GetPosition3(kE.m_iV0);
            Vector3f kEnd1 = m_kVBuffer.GetPosition3(kE.m_iV1);
            kEnd1.sub(kEnd0, m_kDiff);

            float fMetric = fLengthWeight * m_kDiff.Length();

            // angle/area contribution
            Triangle kT = (Triangle) kEAttr.m_kTSet.get(0);
            Vector3f kV0 = m_kVBuffer.GetPosition3(kT.m_iV0);
            Vector3f kV1 = m_kVBuffer.GetPosition3(kT.m_iV1);
            Vector3f kV2 = m_kVBuffer.GetPosition3(kT.m_iV2);
            kV1.sub(kV0, m_kE0);
            kV2.sub(kV0, m_kE1);
            m_kE0.Cross(m_kE1, m_kN0);

            kT = (Triangle) kEAttr.m_kTSet.get(1);
            kV0 = m_kVBuffer.GetPosition3(kT.m_iV0);
            kV1 = m_kVBuffer.GetPosition3(kT.m_iV1);
            kV2 = m_kVBuffer.GetPosition3(kT.m_iV2);
            kV1.sub(kV0, m_kE0);
            kV2.sub(kV0, m_kE1);
            m_kE0.Cross(m_kE1, m_kN1);

            m_kN0.Cross(m_kN1, m_kCross);
            fMetric += fAngleWeight * m_kCross.Length();

            return fMetric;
        }

        return Float.POSITIVE_INFINITY;
    }

    /**
     * Allocate the heap and initialize all the heap elements. This is an O(N) process for N edges.
     */
    private void initializeHeap() {
        // It is possible that during an edge collapse, the number of
        // temporary edges is larger than the original number of edges in the
        // mesh.  To make sure there is enough heap space, allocate two times
        // the number of original edges.

        m_iHQuantity = m_kEMap.size();

        int iHIndex = 0;
        Iterator kIter = m_kEMap.entrySet().iterator();

        while (kIter.hasNext()) {
            Map.Entry kEntry = (Map.Entry) kIter.next();
            Edge kE = (Edge) kEntry.getKey();
            EdgeAttribute kEAttr = (EdgeAttribute) kEntry.getValue();
            m_akHeap[iHIndex] = (HeapRecord) kEAttr.m_kData;
            m_akHeap[iHIndex].m_kEdge = kE;
            m_akHeap[iHIndex].m_iHIndex = iHIndex;
            m_akHeap[iHIndex].m_fMetric = getMetric(kE, kEAttr);
            iHIndex++;
        }

        sort();
    }

    /**
     * When an edge is collapsed, all triangles sharing the edge have been removed. All remaining triangles that contain
     * VThrow as a vertex must have VThrow replaced by VKeep. The old triangles are removed from the mesh and the
     * modified triangles are inserted.
     *
     * @param  kT       the triangle to modify
     * @param  iVKeep   the vertex to keep in the edge collapse
     * @param  iVThrow  the vertex to be thrown away in the edge collapse
     */
    private void modifyTriangle(Triangle kT, int iVKeep, int iVThrow) {

        // save the old triangle
        int iV0 = kT.m_iV0;
        int iV1 = kT.m_iV1;
        int iV2 = kT.m_iV2;

        // Get the index of the pre-modified triangle, then remove the
        // triangle from the mesh.
        Integer kTIndex = (Integer) getData(kT);
        superRemoveTriangle(kT);

        // replace 'throw' by 'keep'
        if (iV0 == iVThrow) {
            iV0 = iVKeep;
        } else if (iV1 == iVThrow) {
            iV1 = iVKeep;
        } else { // iV2 == iVThrow must be the case
            iV2 = iVKeep;
        }

        // Indices on modified triangles are the same as the indices on the
        // pre-modified triangles.
        Triangle kTSave = new Triangle(iV0, iV1, iV2);
        insertTriangle(kTSave);
        setData(kTSave, kTIndex);
    }

    /**
     * The only remove operation from a heap occurs at the root of the heap. Once removed, the heap must be updated.
     * This is an O(log N) process for N edges.
     */
    private void remove() {
        HeapRecord kRoot = m_akHeap[0];

        int iLast = m_iHQuantity - 1;
        HeapRecord kRecord = m_akHeap[iLast];
        int iPa = 0, iCh = 1;

        while (iCh <= iLast) {

            if (iCh < iLast) {
                int iChP = iCh + 1;

                if (m_akHeap[iCh].m_fMetric > m_akHeap[iChP].m_fMetric) {
                    iCh = iChP;
                }
            }

            if (m_akHeap[iCh].m_fMetric >= kRecord.m_fMetric) {
                break;
            }

            m_akHeap[iCh].m_iHIndex = iPa;
            m_akHeap[iPa] = m_akHeap[iCh];
            iPa = iCh;
            iCh = (2 * iCh) + 1;
        }

        kRecord.m_iHIndex = iPa;
        m_akHeap[iPa] = kRecord;
        m_iHQuantity--;

        // To notify OnEdgeDestroy that this edge was already removed from the
        // heap, but the object must be deleted by that callback.
        kRoot.m_iHIndex = -1;
    }

    /**
     * After edge collapses are finished, the actual vertex locations, the triangle connectivity array, and the collapse
     * record information must be reordered to support fast dynamic change in level of detail.
     */
    private void reorder() {

        // permute the vertices and copy to the original array
        int i;

        for (i = 0; i < m_iVQuantity; i++) {
            m_kNewVBuffer.SetVertex(i, m_kVBuffer.GetVertex(m_aiVOrdered[i]));
        }

        for (i = 0; i < m_iVQuantity; i++) {
            m_kVBuffer.SetVertex(i, m_kNewVBuffer.GetVertex(i));
        }

        // permute the connectivity array and copy to the original array
        for (i = 0; i < (3 * m_iTQuantity); i++) {
            m_aiConnect[i] = m_aiVPermute[m_aiNewConnect[i]];
        }

        // permute the keep/throw pairs
        for (i = 0; i < m_kEDelete.size(); i++) {
            CollapseRecord kCR = (CollapseRecord) m_kEDelete.get(i);
            kCR.VKeep = m_aiVPermute[kCR.VKeep];
            kCR.VThrow = m_aiVPermute[kCR.VThrow];
        }
    }

    /**
     * After the heap is initialized, it must be sorted. This is an O(N log N) process for N edges.
     */
    private void sort() {
        int iLast = m_iHQuantity - 1;

        for (int iLeft = iLast / 2; iLeft >= 0; iLeft--) {
            HeapRecord kRecord = m_akHeap[iLeft];
            int iPa = iLeft, iCh = (2 * iLeft) + 1;

            while (iCh <= iLast) {

                if (iCh < iLast) {
                    int iChP = iCh + 1;

                    if (m_akHeap[iCh].m_fMetric > m_akHeap[iChP].m_fMetric) {
                        iCh = iChP;
                    }
                }

                if (m_akHeap[iCh].m_fMetric >= kRecord.m_fMetric) {
                    break;
                }

                m_akHeap[iCh].m_iHIndex = iPa;
                m_akHeap[iPa] = m_akHeap[iCh];
                iPa = iCh;
                iCh = (2 * iCh) + 1;
            }

            kRecord.m_iHIndex = iPa;
            m_akHeap[iPa] = kRecord;
        }
    }

    /**
     * If an edge weight changes for an edge whose heap element is interior to the heap tree, the heap must be updated.
     * This is an O(log N) process for N edges.
     *
     * @param  iHIndex  the index of the heap element corresponding to the modified edge (the heap is a binary tree but
     *                  is stored in an array)
     * @param  fMetric  the modified edge weight
     */
    private void update(int iHIndex, float fMetric) {
        HeapRecord kRecord = m_akHeap[iHIndex];
        int iPa, iCh, iChP, iMaxCh;

        if (fMetric > kRecord.m_fMetric) {
            kRecord.m_fMetric = fMetric;

            // new weight larger than old, propagate it towards the leaves
            iPa = iHIndex;
            iCh = (2 * iPa) + 1;

            while (iCh < m_iHQuantity) {

                // at least one child exists
                if (iCh < (m_iHQuantity - 1)) {

                    // two children exist
                    iChP = iCh + 1;

                    if (m_akHeap[iCh].m_fMetric <= m_akHeap[iChP].m_fMetric) {
                        iMaxCh = iCh;
                    } else {
                        iMaxCh = iChP;
                    }
                } else {

                    // one child exists
                    iMaxCh = iCh;
                }

                if (m_akHeap[iMaxCh].m_fMetric >= fMetric) {
                    break;
                }

                m_akHeap[iMaxCh].m_iHIndex = iPa;
                m_akHeap[iPa] = m_akHeap[iMaxCh];
                kRecord.m_iHIndex = iMaxCh;
                m_akHeap[iMaxCh] = kRecord;
                iPa = iMaxCh;
                iCh = (2 * iPa) + 1;
            }
        } else if (fMetric < kRecord.m_fMetric) {
            kRecord.m_fMetric = fMetric;

            // new weight smaller than old, propagate it towards the root
            iCh = iHIndex;

            while (iCh > 0) {

                // a parent exists
                iPa = (iCh - 1) / 2;

                if (m_akHeap[iPa].m_fMetric <= fMetric) {
                    break;
                }

                m_akHeap[iPa].m_iHIndex = iCh;
                m_akHeap[iCh] = m_akHeap[iPa];
                kRecord.m_iHIndex = iPa;
                kRecord.m_fMetric = fMetric;
                m_akHeap[iPa] = kRecord;
                iCh = iPa;
            }
        }
    }

    /**
     * DEBUGGING. After a decimation, the collapse records are applied from highest to lowest resolution to test if the
     * vertex indices for the triangles are within range.
     *
     * @return  DOCUMENT ME!
     */
    private boolean validate() {
        CollapseRecord[] akRecord = m_akRecord.GetData();
        for (int j = 0; j < akRecord.length; j++) {

            // replace indices in connectivity array
            CollapseRecord kRecord = akRecord[j];
            int i;

            for (i = 0; i < kRecord.IQuantity; i++) {
                m_aiConnect[kRecord.Index[i]] = kRecord.VKeep;
            }

            // reduce vertex count
            int iVQuantity = kRecord.VQuantity;

            // reduce triangle count
            int iTQuantity = kRecord.TQuantity;

            // test for validity of indices
            for (i = 0; i < (3 * iTQuantity); i++) {

                if (m_aiConnect[i] >= iVQuantity) {
                    /*
                    Preferences.debug("record " + j + " is invalid\n");
                    Preferences.debug("  vq = ");
                    Preferences.debug(iVQuantity + "");
                    Preferences.debug("  tq = ");
                    Preferences.debug(iTQuantity + "");
                    Preferences.debug("  idx = ");
                    Preferences.debug(m_aiConnect[i] + "\n");
*/
                    return false;
                }
            }
        }

        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * A representation of a triangle for the vertex-edge-triangle table. This class stores the triple of vertex indices
     * for the vertices of the triangle. The triangles <V0,V1,V2> and <V0,V2,V1> are considered to be distinct
     * triangles. The class extends Object to obtain support for hashing into a map of triangles.
     */
    public class Triangle extends Object {

        /** DOCUMENT ME! */
        public int m_iV0, m_iV1, m_iV2;

        /**
         * Constructs a triangle in the table.
         *
         * @param  iV0  a vertex index for a triangle vertex
         * @param  iV1  a vertex index for a triangle vertex
         * @param  iV2  a vertex index for a triangle vertex
         */
        public Triangle(int iV0, int iV1, int iV2) {

            if (iV0 < iV1) {

                if (iV0 < iV2) {

                    // V0 is minimum
                    m_iV0 = iV0;
                    m_iV1 = iV1;
                    m_iV2 = iV2;
                } else {

                    // V2 is minimum
                    m_iV0 = iV2;
                    m_iV1 = iV0;
                    m_iV2 = iV1;
                }
            } else {

                if (iV1 < iV2) {

                    // V1 is minimum
                    m_iV0 = iV1;
                    m_iV1 = iV2;
                    m_iV2 = iV0;
                } else {

                    // V2 is minimum
                    m_iV0 = iV2;
                    m_iV1 = iV0;
                    m_iV2 = iV1;
                }
            }
        }

        /**
         * Support for hashing into a map of triangles.
         *
         * @param   kObject  a triangle for comparison to the current one
         *
         * @return  true iff the triangles are identical
         */
        public boolean equals(Object kObject) {
            Triangle kT = (Triangle) kObject;

            return (m_iV0 == kT.m_iV0) && (m_iV1 == kT.m_iV1) && (m_iV2 == kT.m_iV2);
        }

        /**
         * Support for hashing into a map of triangles.
         *
         * @return  the hash key for the triangle
         */
        public int hashCode() {
            int iCmp;

            if (m_iV1 < m_iV2) {
                iCmp = (m_iV1 << 8) ^ ((m_iV0 << 16) | m_iV2);
            } else {
                iCmp = (m_iV2 << 8) ^ ((m_iV0 << 16) | m_iV1);
            }

            return iCmp;
        }


    }

    /**
     * A representation of a vertex for the vertex-edge-triangle table. This class just stores the index of the vertex
     * but is unconcerned about the actually vertex 3D coordinate. The class extends Object to obtain support for
     * hashing into a map of vertices.
     */
    public class Vertex extends Object {

        /** DOCUMENT ME! */
        public int m_iV;

        /**
         * Constructs a vertex in the table.
         *
         * @param  iV  the index of the vertex
         */
        public Vertex(int iV) {
            m_iV = iV;
        }

        /**
         * Support for hashing into a map of vertices.
         *
         * @param   kObject  a vertex for comparison to the current one
         *
         * @return  true iff the vertices have the same index
         */
        public boolean equals(Object kObject) {
            Vertex kV = (Vertex) kObject;

            return m_iV == kV.m_iV;
        }

        /**
         * Support for hashing into a map of vertices.
         *
         * @return  the hash key for the vertex
         */
        public int hashCode() {
            return m_iV;
        }
    }

    /**
     * The attributes associated with an edge. These include the set of triangles sharing the edge. The data member
     * m_kData allows an application to attach whatever data it wants associated with the edge. The
     * attachment/detachment is performed by the application in the OnEdgeInsert/OnEdgeRemove callbacks.
     */
    protected class EdgeAttribute {

        /** support for application-specific data. */
        public Object m_kData;

        /** set of <Triangle>. */
        public ModelSet m_kTSet;

        /**
         * Construct an attribute for an edge. The set of triangles is initially empty.
         */
        public EdgeAttribute() {
            m_kData = null;
            m_kTSet = new ModelSet(4, 4);
        }

        /**
         * Test if the set of adjacent triangles is empty. The function is used to determine the bCreate/bDestroy
         * parameters in the OnEdgeInsert/OnEdgeRemove callbacks.
         *
         * @return  true iff the edge has no adjacent triangles
         */
        public boolean isEmpty() {
            return m_kTSet.isEmpty();
        }
    }

    /**
     * The attributes associated with a triangle. The triangle does not keep track of any adjacencies (these can be
     * indirectly determined from other mesh components). The data member m_kData allows an application to attach
     * whatever data it wants associated with the triangle. The attachment/detachment is performed by the application in
     * the OnTriangleInsert/OnTriangleRemove callbacks.
     */
    protected class TriangleAttribute {

        /** support for application-specific data. */
        public Object m_kData;

        /**
         * Construct an attribute for a triangle.
         */
        public TriangleAttribute() {
            m_kData = null;
        }
    }

    /**
     * The attributes associated with a vertex. These include the set of edges sharing the vertex and the set of
     * triangles sharing the vertex. The data member m_kData allows an application to attach whatever data it wants
     * associated with the vertex. The attachment/detachment is performed by the application in the
     * OnVertexInsert/OnVertexRemove callbacks.
     */
    protected class VertexAttribute {

        /** support for application-specific data. */
        public Object m_kData;

        /** set of <Edge>. */
        public ModelSet m_kESet;

        /** set of <Triangle>. */
        public ModelSet m_kTSet;

        /**
         * Construct an attribute for a vertex. The sets of edges and triangles are initially empty.
         */
        public VertexAttribute() {
            m_kData = null;
            m_kESet = new ModelSet(8, 8);
            m_kTSet = new ModelSet(8, 8);
        }

        /**
         * Test if the sets of adjacent edges and triangles are empty. The function is used to determine the
         * bCreate/bDestroy parameters in the OnVertexInsert/OnVertexRemove callbacks.
         *
         * @return  true iff the vertex has no adjacent edges and triangles
         */
        public boolean isEmpty() {
            return m_kESet.isEmpty() && m_kTSet.isEmpty();
        }
    }
    
    /**
     * The representation of a heap element. The important quantity for heap updates is the metric value. However, the
     * HIndex value is used for O(1) lookups of an edge whose weight needs to be modified as a result of an edge
     * collapse.
     */
    private class HeapRecord {

        /** DOCUMENT ME! */
        public float m_fMetric;

        /** DOCUMENT ME! */
        public int m_iHIndex;

        /** DOCUMENT ME! */
        public Edge m_kEdge;

        /**
         * Creates a new HeapRecord object.
         */
        public HeapRecord() {
            m_kEdge = new Edge(-1, -1);
            m_iHIndex = -1;
            m_fMetric = -1.0f;
        }
    }
}
