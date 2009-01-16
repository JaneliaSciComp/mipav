package gov.nih.mipav.view.renderer.J3D.model.structures;


import gov.nih.mipav.view.*;

import java.util.*;

import javax.vecmath.*;


/**
 * A triangle decimator that is used for continuous level of detail of a triangle mesh. The algorithm is based on edge
 * collapses of the mesh. A detailed discussion of the algorithm is found in <a href=../../../LevelSetExtraction.pdf>
 * Level Set Extraction</a>
 */

public class ModelSurfaceDecimator extends ModelSurfaceTopology {

    //~ Instance fields ------------------------------------------------------------------------------------------------

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
    private Point3f[] m_akNewVertex;

    /** the incremental changes representing the decimation. */
    private ModelCollapseRecord[] m_akRecord;

    /** DOCUMENT ME! */
    private Point3f[] m_akVertex;

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
    public ModelSurfaceDecimator(int iVQuantity, int iTQuantity) {
        m_aiVOrdered = new int[iVQuantity];
        m_aiVPermute = new int[iVQuantity];
        m_aiNewConnect = new int[3 * iTQuantity];
        m_kVDelete = new HashSet();
        m_kEDelete = new Vector();
        m_akHeap = new HeapRecord[3 * iTQuantity];

        m_akNewVertex = new Point3f[iVQuantity];

        int i;

        for (i = 0; i < iVQuantity; i++) {
            m_akNewVertex[i] = new Point3f();
        }

        // temporary variables to avoid 'new' calls
        m_kE0 = new Vector3f();
        m_kE1 = new Vector3f();
        m_kN0 = new Vector3f();
        m_kN1 = new Vector3f();
        m_kCross = new Vector3f();
        m_kDiff = new Vector3f();
        m_aiIndex = new int[3 * iTQuantity];
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
     * @param  progressBar  DOCUMENT ME!
     * @param  added        DOCUMENT ME!
     * @param  total        DOCUMENT ME!
     */
    public void decimate(Point3f[] akVertex, int[] aiConnect, ViewJProgressBar progressBar, int added, int total) {

        // clear out topology if decimator used previously
        m_kVMap.clear();
        m_kEMap.clear();
        m_kTMap.clear();
        m_kVDelete.clear();
        m_kEDelete.clear();

        // Hang onto these to avoid having to pass them through member
        // function calls.
        m_iVQuantity = akVertex.length;
        m_akVertex = akVertex;
        m_iTQuantity = aiConnect.length / 3;
        m_aiConnect = aiConnect;

        // for reordering vertices and triangles
        m_iVCurrent = m_iVQuantity - 1;
        m_iTCurrent = m_iTQuantity - 1;

        // Insert the triangles into the mesh.  The triangle indices are
        // attached as extra data.
        m_bCollapsing = false;

        for (int i = 0; i < m_iTQuantity; i++) {
            Triangle kT = new Triangle(aiConnect[3 * i], aiConnect[(3 * i) + 1], aiConnect[(3 * i) + 2]);
            insertTriangle(kT);
            setData(kT, new Integer(i));
        }

        // set up the heap for storing information needed by edge collapses
        initializeHeap();

        // do the edge collapses
        m_bCollapsing = true;

        progressBar.updateValueImmed(added + (25 / total));

        while (m_iHQuantity > 0) {

            if (m_akHeap[0].m_fMetric == Float.POSITIVE_INFINITY) {

                // all remaining heap elements have infinite weight
                flushVertices();
                flushTriangles();

                break;
            }

            doCollapse();
        }

        progressBar.updateValueImmed(added + (50 / total));
        m_bCollapsing = false;

        // Permute the vertices and triangle connectivity so that the last
        // vertex/triangle in the array is the first vertex/triangle to be
        // removed.
        reorder();
        progressBar.updateValueImmed(added + (75 / total));

        // The collapse records store the incremental changes that are used for
        // dynamic LOD changes in the caller of this constructor.
        computeRecords();

        // DEBUGGING.  Uncomment this line to verify the correctness of the
        // edge collapses.
        // boolean bIsValid = validate();
        // END DEBUGGING.
        progressBar.updateValueImmed(added + (100 / total));
    }

    /**
     * The collapse records that have been created by the constructor for ModelSurfaceDecimator. After construction, the
     * application uses the vertex and triangle connectivity arrays that were passed to the constructor (arrays are now
     * reordered) and should call this function to access the collapse records. All three data structures are then used
     * to create an ModelClodMesh object.
     *
     * @return  the array of collapse records for the decimation
     */
    public ModelCollapseRecord[] getRecords() {
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

        super.removeTriangle(kT);
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
        ModelCollapseRecord kCR = new ModelCollapseRecord(iVKeep, iVThrow, m_kVDelete.size(), iTDeletions);
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
        m_akRecord = new ModelCollapseRecord[iCQuantity];

        int i;

        for (i = 0; i < iCQuantity; i++) {
            m_akRecord[i] = new ModelCollapseRecord();
        }

        // initial record only stores initial vertex and triangle quantities
        m_akRecord[0].m_iVQuantity = m_iVQuantity;
        m_akRecord[0].m_iTQuantity = m_iTQuantity;

        // construct the replacement arrays
        int iVQuantity = m_iVQuantity, iTQuantity = m_iTQuantity;
        int iR;
        int length = m_kEDelete.size();

        for (iR = 0; iR < length; iR++) {
            ModelCollapseRecord kERecord = (ModelCollapseRecord) m_kEDelete.get(iR);
            ModelCollapseRecord kRecord = m_akRecord[iR + 1];

            iVQuantity -= kERecord.m_iVQuantity;
            iTQuantity -= kERecord.m_iTQuantity;

            kRecord.m_iVKeep = kERecord.m_iVKeep;
            kRecord.m_iVThrow = kERecord.m_iVThrow;
            kRecord.m_iVQuantity = iVQuantity;
            kRecord.m_iTQuantity = iTQuantity;
            kRecord.m_iIQuantity = 0;

            if (iTQuantity > 0) {
                int iIMax = 3 * iTQuantity;

                for (i = 0; i < iIMax; i++) {

                    if (m_aiConnect[i] == kRecord.m_iVThrow) {
                        m_aiConnect[i] = kRecord.m_iVKeep;
                        m_aiIndex[kRecord.m_iIQuantity++] = i;
                    }
                }

                if (kRecord.m_iIQuantity > 0) {
                    kRecord.m_aiIndex = new int[kRecord.m_iIQuantity];

                    for (i = 0; i < kRecord.m_iIQuantity; i++) {
                        kRecord.m_aiIndex[i] = m_aiIndex[i];
                    }
                }
            } else {
                kRecord.m_aiIndex = null;
            }
        }

        // expand mesh back to original
        for (iR = iCQuantity - 1; iR > 0; iR--) {

            // restore indices in connectivity array
            ModelCollapseRecord kRecord = m_akRecord[iR];

            for (i = 0; i < kRecord.m_iIQuantity; i++) {
                int iC = kRecord.m_aiIndex[i];
                m_aiConnect[iC] = kRecord.m_iVThrow;
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
            Point3f kEnd0 = m_akVertex[kE.m_iV0];
            Point3f kEnd1 = m_akVertex[kE.m_iV1];
            m_kDiff.sub(kEnd1, kEnd0);

            float fMetric = fLengthWeight * m_kDiff.length();

            // angle/area contribution
            Triangle kT = (Triangle) kEAttr.m_kTSet.get(0);
            Point3f kV0 = m_akVertex[kT.m_iV0];
            Point3f kV1 = m_akVertex[kT.m_iV1];
            Point3f kV2 = m_akVertex[kT.m_iV2];
            m_kE0.sub(kV1, kV0);
            m_kE1.sub(kV2, kV0);
            m_kN0.cross(m_kE0, m_kE1);

            kT = (Triangle) kEAttr.m_kTSet.get(1);
            kV0 = m_akVertex[kT.m_iV0];
            kV1 = m_akVertex[kT.m_iV1];
            kV2 = m_akVertex[kT.m_iV2];
            m_kE0.sub(kV1, kV0);
            m_kE1.sub(kV2, kV0);
            m_kN1.cross(m_kE0, m_kE1);

            m_kCross.cross(m_kN0, m_kN1);
            fMetric += fAngleWeight * m_kCross.length();

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
        super.removeTriangle(kT);

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
            m_akNewVertex[i].set(m_akVertex[m_aiVOrdered[i]]);
        }

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVertex[i].set(m_akNewVertex[i]);
        }

        // permute the connectivity array and copy to the original array
        for (i = 0; i < (3 * m_iTQuantity); i++) {
            m_aiConnect[i] = m_aiVPermute[m_aiNewConnect[i]];
        }

        // permute the keep/throw pairs
        for (i = 0; i < m_kEDelete.size(); i++) {
            ModelCollapseRecord kCR = (ModelCollapseRecord) m_kEDelete.get(i);
            kCR.m_iVKeep = m_aiVPermute[kCR.m_iVKeep];
            kCR.m_iVThrow = m_aiVPermute[kCR.m_iVThrow];
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

        for (int j = 0; j < m_akRecord.length; j++) {

            // replace indices in connectivity array
            ModelCollapseRecord kRecord = m_akRecord[j];
            int i;

            for (i = 0; i < kRecord.m_iIQuantity; i++) {
                m_aiConnect[kRecord.m_aiIndex[i]] = kRecord.m_iVKeep;
            }

            // reduce vertex count
            int iVQuantity = kRecord.m_iVQuantity;

            // reduce triangle count
            int iTQuantity = kRecord.m_iTQuantity;

            // test for validity of indices
            for (i = 0; i < (3 * iTQuantity); i++) {

                if (m_aiConnect[i] >= iVQuantity) {
                    Preferences.debug("record " + j + " is invalid\n");
                    Preferences.debug("  vq = ");
                    Preferences.debug(iVQuantity + "");
                    Preferences.debug("  tq = ");
                    Preferences.debug(iTQuantity + "");
                    Preferences.debug("  idx = ");
                    Preferences.debug(m_aiConnect[i] + "\n");

                    return false;
                }
            }
        }

        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

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
