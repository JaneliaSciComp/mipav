package gov.nih.mipav.view.renderer.J3D.model.structures;


import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * This is an implementation of a vertex-edge-triangle table. It uses hash maps for storing the mesh components. Basic
 * operations are supported including insertion and removal of triangles and accessing the mesh components and their
 * adjacent components. More complex operations are also supported including determination if the mesh is connected,
 * closed, or manifold, and allows for computing the connected components of a mesh. With additional computation time,
 * the connected components can be created so that the triangles within each component are consistely ordered. See the
 * document <a href=../../../LevelSetExtraction.pdf>Level Set Extraction</a> for more details on vertex-edge-triangle
 * tables.
 */

public class ModelSurfaceTopology {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** map of <Edge,EdgeAttribute>. */
    protected HashMap m_kEMap;

    /** map of <Triangle,TriangleAttribute>. */
    protected HashMap m_kTMap;

    /** map of <Vertex,VertexAttribute>. */
    protected HashMap m_kVMap;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The default constructor whose job is solely to allocate the hashmaps for the vertex-edge-triangle table.
     */
    public ModelSurfaceTopology() {
        m_kVMap = new HashMap();
        m_kEMap = new HashMap();
        m_kTMap = new HashMap();
    }

    /**
     * Creates a new ModelSurfaceTopology object.
     *
     * @param  iVCapacity  the initial number of buckets in the vertex hashmap
     * @param  fVLoad      the load factor for the vertex hash map, in (0,1)
     * @param  iECapacity  the initial number of buckets in the edge hash map
     * @param  fELoad      the load factor for the edge hash map, in (0,1)
     * @param  iTCapacity  the initial number of buckets in the triangle hashmap
     * @param  fTLoad      the load factor for the triangle hash map, in (0,1)
     */
    public ModelSurfaceTopology(int iVCapacity, float fVLoad, int iECapacity, float fELoad, int iTCapacity,
                                float fTLoad) {
        m_kVMap = new HashMap(iVCapacity, fVLoad);
        m_kEMap = new HashMap(iECapacity, fELoad);
        m_kTMap = new HashMap(iTCapacity, fTLoad);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * A factory that is used for operations that create new meshes from the current one. This allows derived class
     * construction within the base class operations.
     *
     * @return  a new vertex-edge-triangle table of the correct derived-class type
     */
    public ModelSurfaceTopology Create() {
        return new ModelSurfaceTopology();
    }

    /**
     * Construct the connected components of the mesh. The triangle ordering within a single component is not guaranteed
     * to be consistent. That is, it is possible for a triangle to be counterclockwise ordered, but an adjacent triangle
     * to be clockwise ordered.
     *
     * @return  an array of components, each entry of type ModelSurfaceTopology
     */
    public Vector getComponents() {

        // Do a depth-first search of the mesh to find connected components.
        int iTSize = m_kTMap.size();

        if (iTSize == 0) {
            return null;
        }

        // for marking visited triangles during the traversal
        HashMap kVisitedMap = new HashMap(); // map of <Triangle,Boolean>
        Iterator kTIter = m_kTMap.entrySet().iterator();
        Map.Entry kEntry;

        while (kTIter.hasNext()) {
            kEntry = (Map.Entry) kTIter.next();
            kVisitedMap.put(kEntry.getKey(), Boolean.FALSE);
        }

        Vector kComponents = new Vector(); // vector of <ModelSurfaceTopology>

        while (iTSize > 0) {

            // find an unvisited triangle in the mesh
            Stack kStack = new Stack(); // stack of <Triangle>
            Iterator kVIter = kVisitedMap.entrySet().iterator();

            while (kVIter.hasNext()) {
                kEntry = (Map.Entry) kVIter.next();

                if (kEntry.getValue() == Boolean.FALSE) {
                    kStack.push(kEntry.getKey());
                    kEntry.setValue(Boolean.TRUE);
                    iTSize--;

                    break;
                }
            }

            // traverse the connected component of the starting triangle
            ModelSurfaceTopology kComponent = Create();

            while (!kStack.empty()) {

                // start at the current triangle
                Triangle kT = (Triangle) kStack.pop();
                kComponent.insertTriangle(kT);

                int iV0 = 0, iV1 = 0;

                for (int i = 0; i < 3; i++) {

                    switch (i) {

                        case 0:
                            iV0 = kT.m_iV0;
                            iV1 = kT.m_iV1;
                            break;

                        case 1:
                            iV0 = kT.m_iV1;
                            iV1 = kT.m_iV2;
                            break;

                        case 2:
                            iV0 = kT.m_iV2;
                            iV1 = kT.m_iV0;
                            break;
                    }

                    // get an edge of the current triangle
                    Edge kE = new Edge(iV0, iV1);
                    EdgeAttribute kEAttr = (EdgeAttribute) m_kEMap.get(kE);

                    // visit each adjacent triangle
                    for (int j = 0; j < kEAttr.m_kTSet.size(); j++) {
                        Triangle kTAdj = (Triangle) kEAttr.m_kTSet.get(j);
                        Boolean kVisited = (Boolean) kVisitedMap.get(kTAdj);

                        if (kVisited == Boolean.FALSE) {

                            // this adjacent triangle not yet visited
                            kStack.push(kTAdj);
                            kVisitedMap.put(kTAdj, Boolean.TRUE);
                            iTSize--;
                        }
                    }
                }
            }

            kComponents.add(kComponent);
        }

        return kComponents;
    }

    /**
     * Construct the connected components of the mesh by factoring the connectivity array into disjoint subarrays. The
     * triangle ordering within a single component is not guaranteed to be consistent. That is, it is possible for a
     * triangle to be counterclockwise ordered, but an adjacent triangle to be clockwise ordered.
     *
     * @param   kIndex  - An array whose entries indicate the starting point of each subarray in the returned integer
     *                  array. Each entry is of type Integer. kIndex[0] always has value 0 since the first subarray
     *                  always starts at the beginning. The next subarray is at kIndex[1]. The number of elements in the
     *                  first subarray is kIndex[1]-kIndex[0]. The last entry kIndex[kIndex.size()-1] stores the total
     *                  length of the returned array, so there are only kIndex.size()-1 subarrays.
     *
     * @return  An array of indices that contain the disjoint subarrays in contiguous order.
     */
    public int[] getComponents(Vector kIndex) {
        kIndex.clear();

        // Do a depth-first search of the mesh to find connected components.
        int iTSize = m_kTMap.size();

        if (iTSize == 0) {
            return null;
        }

        int iIQuantity = 3 * iTSize;
        int iIndex = 0;
        int[] aiConnect = new int[iIQuantity];

        // for marking visited triangles during the traversal
        HashMap kVisitedMap = new HashMap(); // map of <Triangle,Boolean>
        Iterator kTIter = m_kTMap.entrySet().iterator();
        Map.Entry kEntry;

        while (kTIter.hasNext()) {
            kEntry = (Map.Entry) kTIter.next();
            kVisitedMap.put(kEntry.getKey(), Boolean.FALSE);
        }

        while (iTSize > 0) {

            // find an unvisited triangle in the mesh
            Stack kStack = new Stack(); // stack of <Triangle>
            Iterator kVIter = kVisitedMap.entrySet().iterator();

            while (kVIter.hasNext()) {
                kEntry = (Map.Entry) kVIter.next();

                if (kEntry.getValue() == Boolean.FALSE) {
                    kStack.push(kEntry.getKey());
                    kEntry.setValue(Boolean.TRUE);
                    iTSize--;

                    break;
                }
            }

            // traverse the connected component of the starting triangle
            ModelSurfaceTopology kComponent = Create();

            while (!kStack.empty()) {

                // start at the current triangle
                Triangle kT = (Triangle) kStack.pop();
                kComponent.insertTriangle(kT);

                int iV0 = 0, iV1 = 0;

                for (int i = 0; i < 3; i++) {

                    switch (i) {

                        case 0:
                            iV0 = kT.m_iV0;
                            iV1 = kT.m_iV1;
                            break;

                        case 1:
                            iV0 = kT.m_iV1;
                            iV1 = kT.m_iV2;
                            break;

                        case 2:
                            iV0 = kT.m_iV2;
                            iV1 = kT.m_iV0;
                            break;
                    }

                    // get an edge of the current triangle
                    Edge kE = new Edge(iV0, iV1);
                    EdgeAttribute kEAttr = (EdgeAttribute) m_kEMap.get(kE);

                    // visit each adjacent triangle
                    for (int j = 0; j < kEAttr.m_kTSet.size(); j++) {
                        Triangle kTAdj = (Triangle) kEAttr.m_kTSet.get(j);
                        Boolean kVisited = (Boolean) kVisitedMap.get(kTAdj);

                        if (kVisited == Boolean.FALSE) {

                            // this adjacent triangle not yet visited
                            kStack.push(kTAdj);
                            kVisitedMap.put(kTAdj, Boolean.TRUE);
                            iTSize--;
                        }
                    }
                }
            }

            // store the connectivity information for this component
            kIndex.add(new Integer(iIndex));
            kTIter = kComponent.m_kTMap.entrySet().iterator();

            while (kTIter.hasNext()) {
                kEntry = (Map.Entry) kTIter.next();

                Triangle kT = (Triangle) kEntry.getKey();
                aiConnect[iIndex++] = kT.m_iV0;
                aiConnect[iIndex++] = kT.m_iV1;
                aiConnect[iIndex++] = kT.m_iV2;
            }
        }

        kIndex.add(new Integer(iIQuantity));

        return aiConnect;
    }

    /**
     * Construct the connected components of the mesh. The triangle ordering within a single component is guaranteed to
     * be consistent. That is, all triangles in the component are counterclockwise ordered, or all are clockwise
     * ordered. It is not possible to make ordering between two components consistent since this requires geometric
     * information about how the components are placed in space relative to each other (requires specification of an eye
     * point).
     *
     * @return  an array of components (triangles), each entry of type ModelSurfaceTopology
     */
    public Vector getConsistentComponents() {

        // Do a depth-first search of the mesh to find connected components.
        int iTSize = m_kTMap.size();

        if (iTSize == 0) {
            return null;
        }

        // for marking visited triangles during the traversal
        HashMap kVisitedMap = new HashMap(); // map of <Triangle,Boolean>
        Iterator kTIter = m_kTMap.entrySet().iterator();
        Map.Entry kEntry;

        while (kTIter.hasNext()) {
            kEntry = (Map.Entry) kTIter.next();
            kVisitedMap.put(kEntry.getKey(), Boolean.FALSE);
        }

        Vector kComponents = new Vector(); // vector of <ModelSurfaceTopology>

        while (iTSize > 0) {

            // Find an unvisited triangle in the mesh.  Any triangle pushed
            // onto the stack is considered to have a consistent ordering.
            Stack kStack = new Stack(); // stack of <Triangle>
            Iterator kVIter = kVisitedMap.entrySet().iterator();

            while (kVIter.hasNext()) {
                kEntry = (Map.Entry) kVIter.next();

                if (kEntry.getValue() == Boolean.FALSE) {
                    kStack.push(kEntry.getKey());
                    kEntry.setValue(Boolean.TRUE);
                    iTSize--;

                    break;
                }
            }

            // traverse the connected component of the starting triangle
            ModelSurfaceTopology kComponent = Create();

            while (!kStack.empty()) {

                // start at the current triangle
                Triangle kT = (Triangle) kStack.pop();
                kComponent.insertTriangle(kT);

                int iV0 = 0, iV1 = 0, iV2 = 0;

                for (int i = 0; i < 3; i++) {

                    switch (i) {

                        case 0:
                            iV0 = kT.m_iV0;
                            iV1 = kT.m_iV1;
                            break;

                        case 1:
                            iV0 = kT.m_iV1;
                            iV1 = kT.m_iV2;
                            break;

                        case 2:
                            iV0 = kT.m_iV2;
                            iV1 = kT.m_iV0;
                            break;
                    }

                    // get an edge of the current triangle
                    Edge kE = new Edge(iV0, iV1);
                    EdgeAttribute kEAttr = (EdgeAttribute) m_kEMap.get(kE);

                    if (kEAttr.m_kTSet.size() == 2) {

                        // get the adjacent triangle to the current one
                        Triangle kTAdj = (Triangle) kEAttr.m_kTSet.get(0);

                        if (kTAdj == kT) {
                            kTAdj = (Triangle) kEAttr.m_kTSet.get(1);
                        }

                        Boolean kVisited = (Boolean) kVisitedMap.get(kTAdj);

                        if (kVisited == Boolean.FALSE) {

                            // adjacent triangle not yet visited
                            if (((kTAdj.m_iV0 == iV0) && (kTAdj.m_iV1 == iV1)) ||
                                    ((kTAdj.m_iV1 == iV0) && (kTAdj.m_iV2 == iV1)) ||
                                    ((kTAdj.m_iV2 == iV0) && (kTAdj.m_iV0 == iV1))) {

                                // adjacent triangle must be reordered
                                iV0 = kTAdj.m_iV0;
                                iV1 = kTAdj.m_iV1;
                                iV2 = kTAdj.m_iV2;
                                kVisitedMap.remove(kTAdj);
                                removeTriangle(iV0, iV1, iV2);
                                insertTriangle(iV1, iV0, iV2);
                                kVisitedMap.put(new Triangle(iV1, iV0, iV2), Boolean.FALSE);

                                // refresh the iterators since maps changed
                                kEAttr = (EdgeAttribute) m_kEMap.get(kE);
                                kTAdj = (Triangle) kEAttr.m_kTSet.get(0);

                                if (kTAdj == kT) {
                                    kTAdj = (Triangle) kEAttr.m_kTSet.get(1);
                                }

                                kVisited = (Boolean) kVisitedMap.get(kTAdj);
                            }

                            kStack.push(kTAdj);
                            kVisitedMap.put(kTAdj, Boolean.TRUE);
                            iTSize--;
                        }
                    }
                }
            }

            kComponents.add(kComponent);
        }

        return kComponents;
    }

    /**
     * Get the attribute data associated with the specified vertex.
     *
     * @param   kV  the vertex whose attribute data will be retrieved
     *
     * @return  the attribute data
     */
    public Object getData(Vertex kV) {
        VertexAttribute kVAttr = (VertexAttribute) m_kVMap.get(kV);

        return ((kVAttr != null) ? kVAttr.m_kData : null);
    }

    /**
     * Get the attribute data associated with the specified edge.
     *
     * @param   kE  the edge whose attribute data will be retrieved
     *
     * @return  the attribute data
     */
    public Object getData(Edge kE) {
        EdgeAttribute kEAttr = (EdgeAttribute) m_kEMap.get(kE);

        return ((kEAttr != null) ? kEAttr.m_kData : null);
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
     * Access the hash map of edges. This is useful for allowing an application to iterate over the edge/attribute
     * pairs.
     *
     * @return  the hash map of edges
     */
    public HashMap getEdgeMap() {
        return m_kEMap;
    }

    /**
     * The number of edges currently in the table.
     *
     * @return  the number of edges
     */
    public int getEdgeQuantity() {
        return m_kEMap.size();
    }

    /**
     * Reverse the order of each triangle in the mesh. If a triangle in the mesh is <V0,V1,V2>, the returned
     * vertex-edge-table contains a corresponding triangle <V0,V2,V1>.
     *
     * @return  the vertex-edge-table containing the reversed order triangles
     */
    public ModelSurfaceTopology getReversedOrderMesh() {
        ModelSurfaceTopology kReversed = Create();

        Iterator kIter = m_kTMap.keySet().iterator();

        while (kIter.hasNext()) {
            Triangle kT = (Triangle) kIter.next();
            kReversed.insertTriangle(kT.m_iV0, kT.m_iV2, kT.m_iV1);
        }

        return kReversed;
    }

    /**
     * Access the hash map of triangles. This is useful for allowing an application to iterate over the
     * triangle/attribute pairs.
     *
     * @return  the hash map of triangles
     */
    public HashMap getTriangleMap() {
        return m_kTMap;
    }

    /**
     * The number of triangles currently in the table.
     *
     * @return  the number of triangles
     */
    public int getTriangleQuantity() {
        return m_kTMap.size();
    }

    /**
     * Access the hash map of vertices. This is useful for allowing an application to iterate over the vertex/attribute
     * pairs.
     *
     * @return  the hash map of vertices
     */
    public HashMap getVertexMap() {
        return m_kVMap;
    }

    /**
     * The number of vertices currently in the table.
     *
     * @return  the number of vertices
     */
    public int getVertexQuantity() {
        return m_kVMap.size();
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
     * Determine if the triangle mesh is closed. Each edge must have exactly two triangles sharing it
     *
     * @return  true iff the mesh is closed
     */
    public boolean isClosed() {
        Iterator kIter = m_kEMap.values().iterator();

        while (kIter.hasNext()) {
            EdgeAttribute kEAttr = (EdgeAttribute) kIter.next();

            if (kEAttr.m_kTSet.size() != 2) {
                return false;
            }
        }

        return true;
    }

    /**
     * Determine if the triangle mesh is connected.
     *
     * @return  true iff the mesh is connected
     */
    public boolean isConnected() {
        // Do a depth-first search of the mesh.  It is connected if and only
        // if all of the triangles are visited on a single search.

        int iTSize = m_kTMap.size();

        if (iTSize == 0) {
            return true;
        }

        // for marking visited triangles during the traversal
        HashMap kVisitedMap = new HashMap(); // map of <Triangle,Boolean>
        Iterator kTIter = m_kTMap.entrySet().iterator();
        Map.Entry kEntry = null;

        while (kTIter.hasNext()) {
            kEntry = (Map.Entry) kTIter.next();
            kVisitedMap.put(kEntry.getKey(), Boolean.FALSE);
        }

        // Start the traversal at any triangle in the mesh.  The kEntry object
        // is not null as a side effect of the previous iteration.
        Stack kStack = new Stack(); // stack of <Triangle>
        Triangle kT = (Triangle) m_kTMap.keySet().iterator().next();
        kStack.push(kT);
        kVisitedMap.put(kEntry.getKey(), Boolean.TRUE);
        iTSize--;

        while (!kStack.empty()) {

            // start at the current triangle
            kT = (Triangle) kStack.pop();

            int iV0 = 0, iV1 = 0;

            for (int i = 0; i < 3; i++) {

                switch (i) {

                    case 0:
                        iV0 = kT.m_iV0;
                        iV1 = kT.m_iV1;
                        break;

                    case 1:
                        iV0 = kT.m_iV1;
                        iV1 = kT.m_iV2;
                        break;

                    case 2:
                        iV0 = kT.m_iV2;
                        iV1 = kT.m_iV0;
                        break;
                }

                // get an edge of the current triangle
                Edge kE = new Edge(iV0, iV1);
                EdgeAttribute kEAttr = (EdgeAttribute) m_kEMap.get(kE);

                // visit each adjacent triangle
                for (int j = 0; j < kEAttr.m_kTSet.size(); j++) {
                    Triangle kTAdj = (Triangle) kEAttr.m_kTSet.get(j);
                    Boolean kVisited = (Boolean) kVisitedMap.get(kTAdj);

                    if (kVisited == Boolean.FALSE) {

                        // this adjacent triangle not yet visited
                        kStack.push(kTAdj);
                        kVisitedMap.put(kTAdj, Boolean.TRUE);
                        kVisited = (Boolean) kVisitedMap.get(kTAdj);
                        iTSize--;
                    }
                }
            }
        }

        return iTSize == 0;
    }

    /**
     * Determine if the triangle mesh is manifold. An edge is a manifold edge if it has at most two triangles sharing
     * it.
     *
     * @return  true iff the mesh is manifold
     */
    public boolean isManifold() {
        Iterator kIter = m_kEMap.values().iterator();

        while (kIter.hasNext()) {
            EdgeAttribute kEAttr = (EdgeAttribute) kIter.next();

            if (kEAttr.m_kTSet.size() > 2) {
                return false;
            }
        }

        return true;
    }

    /**
     * Stub provided for overriding in derived classes. Each time an edge is attempted to be inserted into a map, this
     * function is called so that the derived class can perform actions specific to the application.
     *
     * @param  kE       the edge that was attempted to be inserted into the map
     * @param  bCreate  true if the edge did not exist in the map before insertion (it is a brand new edge), false if
     *                  the edge already existed.
     * @param  kEAttr   the attribute for the edge
     */
    public void OnEdgeInsert(Edge kE, boolean bCreate, EdgeAttribute kEAttr) {
        // nothing to do
    }

    /**
     * Stub provided for overriding in derived classes. Each time an edge is attempted to be removed from a map, this
     * function is called so that the derived class can perform actions specific to the application.
     *
     * @param  kE        the edge that was attempted to be removed from the map
     * @param  bDestroy  true if the edge did exist in the map before the attempted removal and no other mesh components
     *                   are referencing the edge, false if the edge does exist in the map, but other mesh components
     *                   are referencing it.
     * @param  kEAttr    the attribute for the edge
     */
    public void OnEdgeRemove(Edge kE, boolean bDestroy, EdgeAttribute kEAttr) {
        // nothing to do
    }

    /**
     * Stub provided for overriding in derived classes. Each time a triangle is attempted to be inserted into a map,
     * this function is called so that the derived class can perform actions specific to the application.
     *
     * @param  kT       the triangle that was attempted to be inserted into the map
     * @param  bCreate  true if the triangle did not exist in the map before insertion (it is a brand new triangle),
     *                  false if the triangle already existed.
     * @param  kTAttr   the attribute for the triangle
     */
    public void OnTriangleInsert(Triangle kT, boolean bCreate, TriangleAttribute kTAttr) {
        // nothing to do
    }

    /**
     * Stub provided for overriding in derived classes. Each time a triangle is attempted to be removed from a map, this
     * function is called so that the derived class can perform actions specific to the application.
     *
     * @param  kT        the triangle that was attempted to be removed from the map
     * @param  bDestroy  true if the triangle did exist in the map before the attempted removal and no other mesh
     *                   components are referencing the triangle, false if the triangle does exist in the map, but other
     *                   mesh components are referencing it.
     * @param  kTAttr    the attribute for the triangle
     */
    public void OnTriangleRemove(Triangle kT, boolean bDestroy, TriangleAttribute kTAttr) {
        // nothing to do
    }

    /**
     * Stub provided for overriding in derived classes. Each time a vertex is attempted to be inserted into a map, this
     * function is called so that the derived class can perform actions specific to the application.
     *
     * @param  kV       the vertex that was attempted to be inserted into the map
     * @param  bCreate  true if the vertex did not exist in the map before insertion (it is a brand new vertex), false
     *                  if the vertex already existed.
     * @param  kVAttr   the attribute for the vertex
     */
    public void OnVertexInsert(Vertex kV, boolean bCreate, VertexAttribute kVAttr) {
        // nothing to do
    }

    /**
     * Stub provided for overriding in derived classes. Each time a vertex is attempted to be removed from a map, this
     * function is called so that the derived class can perform actions specific to the application.
     *
     * @param  kV        the vertex that was attempted to be removed from the map
     * @param  bDestroy  true if the vertex did exist in the map before the attempted removal and no other mesh
     *                   components are referencing the vertex, false if the vertex does exist in the map, but other
     *                   mesh components are referencing it.
     * @param  kVAttr    the attribute for the vertex
     */
    public void OnVertexRemove(Vertex kV, boolean bDestroy, VertexAttribute kVAttr) {
        // nothing to do
    }

    /**
     * Print the vertex-edge-triangle table to a text file. This function is provided for debugging support and is not
     * needed by the triangle decimation scheme or surface visualizer.
     *
     * @param      kFilename  the name of the file to which the table should be printed
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public void print(String kFilename) throws IOException {

        try {
            PrintWriter kOut = new PrintWriter(new FileWriter(kFilename));

            int i;
            Vertex kV;
            VertexAttribute kVAttr;
            Iterator kVIter;
            Edge kE;
            EdgeAttribute kEAttr;
            Iterator kEIter;
            Triangle kT;
            Iterator kTIter;
            Map.Entry kEntry;

            // print vertices
            kOut.print("vertex quantity = ");
            kOut.println(m_kVMap.size());
            kVIter = m_kVMap.entrySet().iterator();

            while (kVIter.hasNext()) {
                kEntry = (Map.Entry) kVIter.next();
                kV = (Vertex) kEntry.getKey();
                kVAttr = (VertexAttribute) kEntry.getValue();

                kOut.print("v<");
                kOut.print(kV.m_iV);
                kOut.print("> : e ");

                for (i = 0; i < kVAttr.m_kESet.size(); i++) {
                    kE = (Edge) kVAttr.m_kESet.get(i);
                    kOut.print("<");
                    kOut.print(kE.m_iV0);
                    kOut.print(",");
                    kOut.print(kE.m_iV1);
                    kOut.print("> ");
                }

                kOut.print(": t");

                for (i = 0; i < kVAttr.m_kTSet.size(); i++) {
                    kT = (Triangle) kVAttr.m_kTSet.get(i);
                    kOut.print("<");
                    kOut.print(kT.m_iV0);
                    kOut.print(",");
                    kOut.print(kT.m_iV1);
                    kOut.print(",");
                    kOut.print(kT.m_iV2);
                    kOut.print(">");
                }

                kOut.println();
            }

            kOut.println();

            // print edges
            kOut.print("edge quantity = ");
            kOut.println(m_kEMap.size());
            kEIter = m_kEMap.entrySet().iterator();

            while (kEIter.hasNext()) {
                kEntry = (Map.Entry) kEIter.next();
                kE = (Edge) kEntry.getKey();
                kEAttr = (EdgeAttribute) kEntry.getValue();

                kOut.print("e<");
                kOut.print(kE.m_iV0);
                kOut.print(",");
                kOut.print(kE.m_iV1);
                kOut.print("> : t ");

                for (i = 0; i < kEAttr.m_kTSet.size(); i++) {
                    kT = (Triangle) kEAttr.m_kTSet.get(i);
                    kOut.print("<");
                    kOut.print(kT.m_iV0);
                    kOut.print(",");
                    kOut.print(kT.m_iV1);
                    kOut.print(",");
                    kOut.print(kT.m_iV2);
                    kOut.print("> ");
                }

                kOut.println();
            }

            kOut.println();

            // print triangles
            kOut.print("triangle quantity = ");
            kOut.println(m_kTMap.size());
            kTIter = m_kTMap.keySet().iterator();

            while (kTIter.hasNext()) {
                kT = (Triangle) kTIter.next();
                kOut.print("t<");
                kOut.print(kT.m_iV0);
                kOut.print(",");
                kOut.print(kT.m_iV1);
                kOut.print(",");
                kOut.print(kT.m_iV2);
                kOut.println(">");
            }

            kOut.println();
            kOut.close();
        } catch (IOException e) { }
    }

    /**
     * Extract a connected component from the mesh and remove all the triangles of the component from the mesh. This is
     * useful for computing the components in a very large mesh that uses a lot of memory. The intention is that the
     * function is called until all components are found. The typical code is
     *
     * <p>ModelSurfaceTopology kTopo = <some mesh topology>; Vector kIndex = new Vector(); // vector of <Integer>
     * kIndex.add(new Integer(0)); int iITotalQuantity = 3*kTopo.getTriangleQuantity(); int[] aiConnect = new
     * int[iITotalQuantity]; int iNumComponents = 0; for (int iIQuantity = 0; iIQuantity < iITotalQuantity; ) { int
     * iComponentIQuantity = kTopo.removeComponent(iIQuantity,aiConnect); iIQuantity += iComponentIQuantity;
     * kIndex.add(new Integer(iQuantity)); iNumComponents++; }</p>
     *
     * <p>The components are stored contiguously in aiConnect. The starting index for component C is stored in the
     * kIndex[C] for 0 <= C <= iNumComponents. kIndex[iNumComponents+1] stores the length of the connectivity array.</p>
     *
     * @param   iOffset    the starting index in aiConnect where the next component is to be stored
     * @param   aiConnect  the array of connectivity indices that will store all the components
     *
     * @return  the number of indices in the just-extracted component
     */
    public int removeComponent(int iOffset, int[] aiConnect) {

        // Do a depth-first search of the mesh to find connected components.
        // The input array is assumed to be large enough to hold the
        // component.
        int iComponentIQuantity = 0;

        int iTSize = m_kTMap.size();

        if (iTSize == 0) {
            return 0;
        }

        // Find the connected component containing the first triangle in the
        // mesh.  A set is used instead of a stack to avoid having a
        // large-memory 'visited' map.
        HashSet kVisited = new HashSet(); // set of <Triangle>
        Iterator kTIter = m_kTMap.entrySet().iterator();
        Map.Entry kEntry = (Map.Entry) kTIter.next();
        kVisited.add(kEntry.getKey());

        // traverse the connected component
        while (!kVisited.isEmpty()) {

            // start at the current triangle
            Triangle kT = (Triangle) kVisited.iterator().next();

            // add adjacent triangles to the set for recursive processing
            int iV0 = 0, iV1 = 0;

            for (int i = 0; i < 3; i++) {

                switch (i) {

                    case 0:
                        iV0 = kT.m_iV0;
                        iV1 = kT.m_iV1;
                        break;

                    case 1:
                        iV0 = kT.m_iV1;
                        iV1 = kT.m_iV2;
                        break;

                    case 2:
                        iV0 = kT.m_iV2;
                        iV1 = kT.m_iV0;
                        break;
                }

                // get an edge of the current triangle
                Edge kE = new Edge(iV0, iV1);
                EdgeAttribute kEAttr = (EdgeAttribute) m_kEMap.get(kE);

                // visit each adjacent triangle
                for (int j = 0; j < kEAttr.m_kTSet.size(); j++) {
                    Triangle kTAdj = (Triangle) kEAttr.m_kTSet.get(j);

                    if (kTAdj != kT) {
                        kVisited.add(kTAdj);
                    }
                }
            }

            // add triangle to connectivity array
            aiConnect[iOffset + iComponentIQuantity++] = kT.m_iV0;
            aiConnect[iOffset + iComponentIQuantity++] = kT.m_iV1;
            aiConnect[iOffset + iComponentIQuantity++] = kT.m_iV2;

            // remove the current triangle (visited, no longer needed)
            kVisited.remove(kT);
            removeTriangle(kT);
        }

        return iComponentIQuantity;
    }

    /**
     * Convenience function for removing triangles from the mesh.
     *
     * @param  kT  the triangle to be removed
     */
    public void removeTriangle(Triangle kT) {
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
     * Set the attribute data associated with the specified vertex.
     *
     * @param  kV     the vertex whose attribute data will be set
     * @param  kData  the attribute data
     */
    public void setData(Vertex kV, Object kData) {
        VertexAttribute kVAttr = (VertexAttribute) m_kVMap.get(kV);

        if (kVAttr != null) {
            kVAttr.m_kData = kData;
        }
    }

    /**
     * Set the attribute data associated with the specified edge.
     *
     * @param  kE     the edge whose attribute data will be set
     * @param  kData  the attribute data
     */
    public void setData(Edge kE, Object kData) {
        EdgeAttribute kEAttr = (EdgeAttribute) m_kEMap.get(kE);

        if (kEAttr != null) {
            kEAttr.m_kData = kData;
        }
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

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * For debugging and testing. A simple class for storing information about the mesh including the number of vertices
     * in the mesh, the number of edges in the mesh, number of triangles in the mesh, the average number of edges per
     * vertex, the average number of triangles per vertex, the average number of triangles per edge, the maximum number
     * of edges for a vertex, the maximum number of triangles for a vertex, and the maximum number of triangles for an
     * edge.
     */
    public static class Statistics {

        /** self-explanatory statistics. */
        public float m_fAverageEdgesPerVertex;

        /** DOCUMENT ME! */
        public float m_fAverageTrianglesPerEdge;

        /** DOCUMENT ME! */
        public float m_fAverageTrianglesPerVertex;

        /** DOCUMENT ME! */
        public int m_iEQuantity;

        /** DOCUMENT ME! */
        public int m_iMaximumEdgesPerVertex;

        /** DOCUMENT ME! */
        public int m_iMaximumTrianglesPerEdge;

        /** DOCUMENT ME! */
        public int m_iMaximumTrianglesPerVertex;

        /** DOCUMENT ME! */
        public int m_iTQuantity;

        /** number of vertices, edge, and triangles in the mesh. */
        public int m_iVQuantity;

        /**
         * The constructor and only member function for the class. The statistics gathering is all done by the
         * constructor.
         *
         * @param  kTopo  the mesh topology object whose statistics need to be calculated
         */
        public Statistics(ModelSurfaceTopology kTopo) {
            m_iVQuantity = kTopo.m_kVMap.size();
            m_iEQuantity = kTopo.m_kEMap.size();
            m_iTQuantity = kTopo.m_kTMap.size();

            int iESumForV = 0;
            int iTSumForV = 0;
            m_iMaximumEdgesPerVertex = 0;
            m_iMaximumTrianglesPerVertex = 0;

            int iESize, iTSize;
            Map.Entry kEntry;

            Iterator kVIter = kTopo.m_kVMap.entrySet().iterator();

            while (kVIter.hasNext()) {
                kEntry = (Map.Entry) kVIter.next();

                VertexAttribute kVAttr = (VertexAttribute) kEntry.getValue();
                iESize = kVAttr.m_kESet.size();
                iTSize = kVAttr.m_kTSet.size();
                iESumForV += iESize;
                iTSumForV += iTSize;

                if (iESize > m_iMaximumEdgesPerVertex) {
                    m_iMaximumEdgesPerVertex = iESize;
                }

                if (iTSize > m_iMaximumTrianglesPerVertex) {
                    m_iMaximumTrianglesPerVertex = iTSize;
                }
            }

            int iTSumForE = 0;
            m_iMaximumTrianglesPerEdge = 0;

            Iterator kEIter = kTopo.m_kEMap.entrySet().iterator();

            while (kEIter.hasNext()) {
                kEntry = (Map.Entry) kEIter.next();

                EdgeAttribute kEAttr = (EdgeAttribute) kEntry.getValue();
                iTSize = kEAttr.m_kTSet.size();
                iTSumForE += iTSize;

                if (iTSize > m_iMaximumTrianglesPerEdge) {
                    m_iMaximumTrianglesPerEdge = iTSize;
                }
            }

            m_fAverageEdgesPerVertex = ((float) iESumForV) / m_iVQuantity;
            m_fAverageTrianglesPerVertex = ((float) iTSumForV) / m_iVQuantity;
            m_fAverageTrianglesPerEdge = ((float) iTSumForE) / m_iEQuantity;
        }

        /**
         * Print the statistics information to System.out.
         */
        public void print() {
            Preferences.debug("vertex quantity = ");
            Preferences.debug(m_iVQuantity + "\n");
            Preferences.debug("edge quantity = ");
            Preferences.debug(m_iEQuantity + "\n");
            Preferences.debug("triangle quantity = ");
            Preferences.debug(m_iTQuantity + "\n");
            Preferences.debug("average edges per vertex = ");
            Preferences.debug(m_fAverageEdgesPerVertex + "\n");
            Preferences.debug("average triangles per vertex = ");
            Preferences.debug(m_fAverageTrianglesPerVertex + "\n");
            Preferences.debug("average triangles per edge = ");
            Preferences.debug(m_fAverageTrianglesPerEdge + "\n");
            Preferences.debug("maximum edges per vertex = ");
            Preferences.debug(m_iMaximumEdgesPerVertex + "\n");
            Preferences.debug("maximum triangles per vertex = ");
            Preferences.debug(m_iMaximumTrianglesPerVertex + "\n");
            Preferences.debug("maximum triangles per edge = ");
            Preferences.debug(m_iMaximumTrianglesPerEdge + "\n");
        }
    }

    /**
     * A representation of an edge for the vertex-edge-triangle table. This class stores the pair of vertex indices for
     * the end points of the edge. The edges <V0,V1> and <V1,V0> are considered to be identical. To simplify
     * comparisons, the class stores the ordered indices. The class extends Object to obtain support for hashing into a
     * map of edges.
     */
    public class Edge extends Object {

        /** DOCUMENT ME! */
        public int m_iV0, m_iV1;

        /**
         * Constructs an edge in the table.
         *
         * @param  iV0  a vertex index for an end point
         * @param  iV1  a vertex index for an end point
         */
        public Edge(int iV0, int iV1) {

            if (iV0 < iV1) {

                // V0 is minimum
                m_iV0 = iV0;
                m_iV1 = iV1;
            } else {

                // V1 is minimum
                m_iV0 = iV1;
                m_iV1 = iV0;
            }
        }

        /**
         * Support for hashing into a map of edges.
         *
         * @param   kObject  an edge for comparison to the current one
         *
         * @return  true iff the edges are identical. Because the class stores ordered indices, it is not necessary to
         *          use the more expensive test (V0 == other.V0 && V1 == other.V1) || (V0 == other.V1 && V1 ==
         *          other.V0).
         */
        public boolean equals(Object kObject) {
            Edge kE = (Edge) kObject;

            return (m_iV0 == kE.m_iV0) && (m_iV1 == kE.m_iV1);
        }

        /**
         * Support for hashing into a map of edges.
         *
         * @return  the hash key for the edge
         */
        public int hashCode() {
            return (m_iV0 << 16) | m_iV1;
        }
    }

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
}
